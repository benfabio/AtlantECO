
##### ATLANTECO SCRIPT 7.1 ----------------------------------------------------------------------------------------------------------------------------
##### 20/01/2023: R Script to reformat the Biometric Data Tables from NMFS-COPEPOD (COPEPEDIA) © Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Read the date tables you copy/pasted into excel from the COPEPEDIA website and reformat it to add factors such as: species names, C conversion parameters (PL vs. TL. vs. DW vs. DM etc.) - use stringr
# - Perform the WoRMS taxonomic check to add taxonomic classification (might be useful for higher group level conversions or average conversion rates)
# - Make some plots to show data distrbution (n) per groups
# - In the next scripts (7.2 etc.), we will use these reformatted tables to convert individual counts (#/m3) into biomass estimates (mg C/m3)

### Original data: T. O'Brien & S. Ramos
# https://www.st.nmfs.noaa.gov/copepedia/taxa/T4022992/index.html 
# https://www.st.nmfs.noaa.gov/copepedia/taxa/T4022992/html/biometricframe.html

### Latest update: 26/01/22

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("lubridate")
library("viridis")
library("readxl")
library("xlsx")

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the excel sheets (1 per type of biometric info) and reformat them

### 1.A) Length-Weight equations
# ?read_excel
lwe <- data.frame(read_excel("NMFS-COPEPOD_tables_biometry_zooplankton.xlsx", sheet = 1))
dim(lwe)
str(lwe)
head(lwe)

# Examine each cols values and then decide how to reformat them
unique(lwe$Taxonomic.identity)
# From this, derive: 
# - OrigScientificName = Taxonomic.identity without the first two elements
# - ScientificName (i.e., modified 'OrigScientificName')
# - SEX (Male vs. FEMALE)
# - LifeStage (CII-CVI, CI-CVI, CII-CIII etc.)

### ¡¡¡ --> First manually reformatted on excel because very complicated otherwise !!!

unique(lwe$Length.Weight.Equations)
# To be trimmed with trimws()
#lwe$Length.Weight.Equations <- base::trimws(x = as.character(lwe$Length.Weight.Equations), which = "both")
head(lwe)
# Derive: 
# - Resp var = DW, WW, AFDW, log C, log DW, log N, log AFDW, log WW, ln C, ln DW
lwe[grepl("DW",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "DW"
lwe[grepl("WW",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "WW"
lwe[grepl("AFDW",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "AFDW"
lwe[grepl("log C",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "logC"
lwe[grepl("log DW",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "logDW"
lwe[grepl("log N",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "logN"
lwe[grepl("log AFDW",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "logAFDW"
lwe[grepl("ln C",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "logC"
lwe[grepl("ln DW",lwe$Length.Weight.Equations, fixed = T),"Biomass_variable"] <- "logDW"
unique(lwe$Biomass_variable) # No NaN

# - Resp var unit = [mg], [ug]
lwe[grepl("[mg]",lwe$Length.Weight.Equations, fixed = T),"unit"] <- "mg"
lwe[grepl("[ug]",lwe$Length.Weight.Equations, fixed = T),"unit"] <- "µg"
unique(lwe$unit) 

# - Expln. var = L, ML, CL, PL, TL
lwe[grepl("L",lwe$Length.Weight.Equations, fixed = T),"L_variable"] <- "L" # length
lwe[grepl("ML",lwe$Length.Weight.Equations, fixed = T),"L_variable"] <- "ML" # mean leangth??
lwe[grepl("CL",lwe$Length.Weight.Equations, fixed = T),"L_variable"] <- "CL" # cephalothorax length
lwe[grepl("TL",lwe$Length.Weight.Equations, fixed = T),"L_variable"] <- "TL" # total length
lwe[grepl("PL",lwe$Length.Weight.Equations, fixed = T),"L_variable"] <- "PL" # prosome length
unique(lwe$L_variable) 

# - Expln. var unit = mm, um
lwe[grepl("[mm]",lwe$Length.Weight.Equations, fixed = T),"L_variable_unit"] <- "mm"
lwe[grepl("[um]",lwe$Length.Weight.Equations, fixed = T),"L_variable_unit"] <- "µm"
unique(lwe$L_variable_unit) 

unique(lwe$Reference) # Leave as is
unique(lwe$Compilation) # Leave as is

### 1.B) Add taxonomic classif
library("worms")
#keys.worms <- wormsbynames( unique(lwe$ScientificName) )
# 4 levels to manually adjust
# Eucalanus pseudoattenuatus                           no match         
lwe[lwe$ScientificName == "Eucalanus pseudoattenuatus","ScientificName"] <- "Pareucalanus attenuatus"                        
# Pareuchaeta antarctica                               no match   
lwe[lwe$ScientificName == "Pareuchaeta antarctica","ScientificName"] <- "Paraeuchaeta antarctica"                                                      
# Pleosis polyphemoides                                no match   
lwe[lwe$ScientificName == "Pleosis polyphemoides","ScientificName"] <- "Pleopis polyphemoides"                                                      
# Oikopleura vanhoeffeni                               no match
lwe[lwe$ScientificName == "Oikopleura vanhoeffeni","ScientificName"] <- "Oikopleura (Vexillaria) vanhoeffeni"                        

keys.worms <- wormsbynames( unique(lwe$ScientificName) )
keys.worms$ScientificName <- unique(lwe$ScientificName)

# Add WoRMS_status field
lwe <-  add_column(lwe, TaxonRank = NA, .after = "ScientificName")
lwe <-  add_column(lwe, AphiaID = NA, .after = "TaxonRank")
lwe <-  add_column(lwe, Status = NA, .after = "AphiaID")
colnames(lwe)

# For testing the functions below:
s <- unique(lwe$ScientificName)[1] ; s

require("parallel")
res <- mclapply( unique(lwe$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- lwe[lwe$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$AphiaID <- "No match found in WoRMS"
                subset$Status <- "No match found in WoRMS"
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$AphiaID <- as.character(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
                subset$TaxonRank <- keys.worms[keys.worms$ScientificName == s,"rank"]
                subset[,c("Kingdom","Phylum","Class","Order","Family","Genus")] <- keys.worms[keys.worms$ScientificName == s,c("kingdom","phylum","class","order","family","genus")]
            
            } # eo 1st if else loop - if species is actually found in 'w'
        
            ### 2nd if else loop to add species name if subset$TaxonRank == "Species"
            if( keys.worms[keys.worms$ScientificName == s,"rank"] == "Species" & !is.na(keys.worms[keys.worms$ScientificName == s,"rank"]) ) {
            
                subset$Species <- keys.worms[keys.worms$ScientificName == s,"valid_name"]
            
            } else {
            
                subset$Species <- NA
            
            } # eo 2nd if else loop - if rank == "Species" 
        
            ### 3rd if else loop to add subset$WoRMS_status
            if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
             
                statuses <- melt( keys.worms[keys.worms$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                subset$Status <- status2add
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 2 

) # eo mclapply - s in taxa_names

### Rbind
ddf <- bind_rows(res)
dim(ddf) # 
ddf[1:20,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf[is.na(ddf$Species),]
unique(ddf$AphiaID) 
unique(ddf$Status) # good
### All good
summary(factor(ddf$Class))

write.xlsx(ddf, file = "NMFS-COPEPEDIA_table_biometry_zooplankton_L-W_equ_26_01_22.xlsx", sheetName = "Length-Weight equations", append = FALSE)

### ----------------------------------------------------------

### B.1) Individual size
indS <- data.frame(read_excel("NMFS-COPEPOD_tables_biometry_zooplankton.xlsx", sheet = 2))
dim(indS) # 7660    4
str(indS)
head(indS)
unique(indS$Taxonomic.identity)
### Those sp names that have brackets for subgenera --> 
# head( str_split_fixed(unique(indS$Taxonomic.identity), " ", n = 7) )
terms <- as.data.frame(str_split_fixed(indS$Taxonomic.identity, " ", n = 6))[,c(3:6)]
head(terms) ; dim(terms) ; unique(terms[,4])
# Col 3 and 4, remove "" by NA
terms[terms$V5 == "","V5"] <- NA
terms[terms$V6 == "","V6"] <- NA
colnames(terms) <- c("N1","N2","N3","N4")
# unique(terms$N2) # terms[c(5651,6292,6859),]
# unique(terms$N3)
# unique(terms$N4)

# terms[terms$N2 == "male","N2"] ; terms[terms$N2 == "female","N2"]

### In indS, need to add: 
# - ScientificName (N1+N2+N3 if brackets present in N2, otherwise N1+N2) - clean after
# - LifeStage: in N3 or N4
# - Sex: in N3 and N4
indS$ScientificName <- NA
indS$LifeStage <- NA
indS$Sex <- NA
# i <- 2

for(i in c(1:nrow(indS)) ) {
    
    message(paste(i, sep = ""))
    
    ### If else loop for ScientificName ----------------------------------------------------------------
    if( grepl("(", terms[i,"N2"], fixed = T) ) {
        
        name <- paste(terms[i,"N1"], terms[i,"N2"], terms[i,"N3"], sep = " ")
        indS[i,"ScientificName"] <- name
        
    } else {
        
        name <- paste(terms[i,"N1"], terms[i,"N2"], sep = " ")
        indS[i,"ScientificName"] <- name
        
    } # eo if else loop 
    
    
    ### If else loop for Sex ----------------------------------------------------------------
    if( grepl("male", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"], sep = " "), fixed = T) ) {
        indS[i,"Sex"] <- "male"
    }
    
    if( grepl("female", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"], sep = " "), fixed = T) ) {
        indS[i,"Sex"] <- "female"
    } 
    
    
    ### If else loop for LifeStage ----------------------------------------------------------------
    if( terms[i,"N3"] %in% c("NI","NII","NIII","NIV","NV","NVI","CI","CII","CIII","CIV","CV") ) {
        
        indS[i,"LifeStage"] <- terms[i,"N3"]
        
    } else if ( terms[i,"N4"] %in% c("CI","CII","CIII","CIV","CV") ) {
        
        indS[i,"LifeStage"] <- terms[i,"N4"]
        
    } else {
        
        indS[i,"LifeStage"] <- NA
        
    }
    
} # eo for loop - i in c(1:nrow(indS))

# Check indS$Sex
unique(indS$Sex) 
indS[1:50,] # Worked

# Check indS$LifeStage
unique(indS$LifeStage) 
indS[3829,] # Worked

### How to reformat 'Individual.Size' ? What do we extract from this? 
unique(indS$Individual.Size) # tricky...
# - Type of length var (TL, L, PL)
# head( as.data.frame(str_split_fixed(indS$Individual.Size, ":", n = 2)) )
unique( as.data.frame(str_split_fixed(indS$Individual.Size, ":", n = 2))[,1] ) # good :)
indS$Size_variable <- as.data.frame(str_split_fixed(indS$Individual.Size, ":", n = 2))[,1] # done
# - Check whether all rows have an average size value inside (are they all in mm)
# for(i in c(1:nrow(indS)) ) {
        # message(paste(i," : ", grepl("avg", indS[i,"Individual.Size"], fixed = T), sep = "" ) )
        # message(paste(i," : ", grepl("mm", indS[i,"Individual.Size"], fixed = T), sep = "" ) )
# } # eo for loop
### --> Extract the average length in mm: the value that is AFTER the "avg"    
# ?parse_number    
dat <- parse_number(x = unique(indS$Individual.Size), trim_ws = T)
dat[1:50] # Does it match?
unique(indS$Individual.Size)[1:50]
# Interesting: it works well when no range is given. When the mean length is given in brackets, you shoud extract the string in brackets first before applying the parse_number() function
test <- str_extract_all(unique(indS$Individual.Size)[10], "\\([^()]+\\)")[[1]]
test <- substring(test, 2, nchar(test)-1)
parse_number(x = test, trim_ws = T)
### Ok, so, for each row, test if brackets are present, if yes: do the 3 lines above, if not do the parse_number() directly
colnames(indS)
indS$MeanSize_mm <- NA
# i <- 2
for(i in c(1:nrow(indS)) ) {
        
        message(indS[i,"Individual.Size"])
        
        # if else loop: if yes: do the 3 lines above, if not do the parse_number() directly
        if( grepl("(", indS[i,"Individual.Size"], fixed = T) ) {
            
            size <- str_extract_all(indS[i,"Individual.Size"], "\\([^()]+\\)")[[1]]
            size <- substring(size, 2, nchar(size)-1)
            size <- parse_number(x = size, trim_ws = T)
            size <- as.numeric(size) # size
            indS[i,"MeanSize_mm"] <- size
            
        } else {
            
            size <- parse_number(x = indS[i,"Individual.Size"], trim_ws = T)
            size <- as.numeric(size) # size
            indS[i,"MeanSize_mm"] <- size
            
        } # eo if else loop 
        
} # eo for loop
# Check with a summary?
summary(indS$MeanSize_mm)
# indS[1:100,c("Individual.Size","MeanSize_mm")] # looks like it worked
# indS[is.na(indS$MeanSize_mm),]
### Worked!!


### B.2) Add taxonomic classif

# Check ScientificName
unique(indS$ScientificName) 
# To do : 
# - Remove " male" and " female"
# - Remove " sp."
indS$ScientificName <- str_replace_all(indS$ScientificName, " female", "")
indS$ScientificName <- str_replace_all(indS$ScientificName, " male", "")
# indS$ScientificName <- str_replace_all(indS$ScientificName, " sp.", "")
# - Use wormsbynames to check names quickly
# keys <- wormsbynames( unique(indS$ScientificName), marine_only = "false" )
# Acartia (Odontacartia) lilljeborgi                   no match
indS[indS$ScientificName == "Acartia (Odontacartia) lilljeborgi","ScientificName"] <- "Acartia (Odontacartia) lilljeborgii"
# Acartia sp.                                          no match
indS[indS$ScientificName == "Acartia sp.","ScientificName"] <- "Acartia"
# Centropages kroeyeri                                 no match
indS[indS$ScientificName == "Centropages kroeyeri","ScientificName"] <- "Centropages kroyeri"
# Clausidiidae A                                       no match
indS[indS$ScientificName == "Clausidiidae A","ScientificName"] <- "Clausidiidae"
# Clausocalanus sp.                                    no match
indS[indS$ScientificName == "Clausocalanus sp.","ScientificName"] <- "Clausocalanus"
# Corycaeus sp.                                        no match
indS[indS$ScientificName == "Corycaeus sp.","ScientificName"] <- "Corycaeus"
# Diacyclops sp.                                       no match
indS[indS$ScientificName == "Diacyclops sp.","ScientificName"] <- "Diacyclops"
# Epischura sp.                                        no match
indS[indS$ScientificName == "Epischura sp.","ScientificName"] <- "Epischura"
# Euaugaptilus palumboi                                no match
indS[indS$ScientificName == "Euaugaptilus palumboi","ScientificName"] <- "Euaugaptilus palumboii"
# Euchirella sp.                                       no match
indS[indS$ScientificName == "Euchirella sp.","ScientificName"] <- "Euchirella"
# Eucyclops sp.                                        no match
indS[indS$ScientificName == "Eucyclops sp.","ScientificName"] <- "Eucyclops"
# Eurytemora sp.                                       no match
indS[indS$ScientificName == "Eurytemora sp.","ScientificName"] <- "Eurytemora"
# Macrocyclops sp.                                     no match
indS[indS$ScientificName == "Macrocyclops sp.","ScientificName"] <- "Macrocyclops"
# Mesocyclops sp.                                      no match
indS[indS$ScientificName == "Mesocyclops sp.","ScientificName"] <- "Mesocyclops"
# Onchocorycaeus agilis                                no match
indS[indS$ScientificName == "Onchocorycaeus agilis","ScientificName"] <- "Onychocorycaeus agilis"
# Onchocorycaeus catus                                 no match
indS[indS$ScientificName == "Onchocorycaeus catus","ScientificName"] <- "Onychocorycaeus catus"
# Onchocorycaeus giesbrechti                           no match
indS[indS$ScientificName == "Onchocorycaeus giesbrechti","ScientificName"] <- "Onychocorycaeus giesbrechti"
# Onchocorycaeus latus                                 no match
indS[indS$ScientificName == "Onchocorycaeus latus","ScientificName"] <- "Onychocorycaeus latus"
# Onchocorycaeus ovalis                                no match
indS[indS$ScientificName == "Onchocorycaeus ovalis","ScientificName"] <- "Onychocorycaeus ovalis"
# Onchocorycaeus pacificus                             no match
indS[indS$ScientificName == "Onchocorycaeus pacificus","ScientificName"] <- "Onychocorycaeus pacificus"
# Onchocorycaeus pumilus                               no match
indS[indS$ScientificName == "Onchocorycaeus pumilus","ScientificName"] <- "Onychocorycaeus pumilus"
# Orthocyclops sp.                                     no match
indS[indS$ScientificName == "Orthocyclops sp.","ScientificName"] <- "Orthocyclops"
# Paracalanus sp.                                      no match
indS[indS$ScientificName == "Paracalanus sp.","ScientificName"] <- "Paracalanus"
# Paracyclops sp.                                      no match
indS[indS$ScientificName == "Paracyclops sp.","ScientificName"] <- "Paracyclops"
# Pontella sp.                                         no match
indS[indS$ScientificName == "Pontella sp.","ScientificName"] <- "Pontella"
# Pontella C                                           no match
indS[indS$ScientificName == "Pontella C","ScientificName"] <- "Pontella"
# Pseudocalanus minutus-elongatus                      no match
indS[indS$ScientificName == "Pseudocalanus minutus-elongatus","ScientificName"] <- "Pseudocalanus"
# Ridgewayia C                                         no match
indS[indS$ScientificName == "Ridgewayia C","ScientificName"] <- "Ridgewayia"
# Skistodiaptomus sp.                                  no match
indS[indS$ScientificName == "Skistodiaptomus sp.","ScientificName"] <- "Skistodiaptomus"
# Stephos C                                            no match
indS[indS$ScientificName == "Stephos C","ScientificName"] <- "Stephos"
# Tropocyclops sp.                                     no match
indS[indS$ScientificName == "Tropocyclops sp.","ScientificName"] <- "Tropocyclops"
# Alona sp.                                            no match
indS[indS$ScientificName == "Alona sp.","ScientificName"] <- "Alona"
# Alonella sp.                                         no match
indS[indS$ScientificName == "Alonella sp.","ScientificName"] <- "Alonella"
# Camptocercus sp.                                     no match
indS[indS$ScientificName == "Camptocercus sp.","ScientificName"] <- "Camptocercus"
# Chydorus sp.                                         no match
indS[indS$ScientificName == "Chydorus sp.","ScientificName"] <- "Chydorus"
# Daphnia sp.                                          no match
indS[indS$ScientificName == "Daphnia sp.","ScientificName"] <- "Daphnia"
# Diaphanosoma sp.                                     no match
indS[indS$ScientificName == "Diaphanosoma sp.","ScientificName"] <- "Diaphanosoma"
# Eurycercus sp.                                       no match
indS[indS$ScientificName == "Eurycercus sp.","ScientificName"] <- "Eurycercus"
# Holopedium sp.                                       no match
indS[indS$ScientificName == "Holopedium sp.","ScientificName"] <- "Holopedium"
# Ilyocryptus sp.                                      no match
indS[indS$ScientificName == "Ilyocryptus sp.","ScientificName"] <- "Ilyocryptus"
# Latona sp.                                           no match
indS[indS$ScientificName == "Latona sp.","ScientificName"] <- "Donax (Latona)"
# Leydigia sp.                                         no match
indS[indS$ScientificName == "Leydigia sp.","ScientificName"] <- "Leydigia"
# Macrothrix sp.                                       no match
indS[indS$ScientificName == "Macrothrix sp.","ScientificName"] <- "Macrothrix"
# Moina sp.                                            no match
indS[indS$ScientificName == "Moina sp.","ScientificName"] <- "Moina"
# Ophryoxus sp.                                        no match
indS[indS$ScientificName == "Ophryoxus sp.","ScientificName"] <- "Ophryoxus"
# Pleuroxus sp.                                        no match
indS[indS$ScientificName == "Pleuroxus sp.","ScientificName"] <- "Pleuroxus"
# Podon sp.                                            no match
indS[indS$ScientificName == "Podon sp.","ScientificName"] <- "Podon"
# Polyphemus sp.                                       no match
indS[indS$ScientificName == "Polyphemus sp.","ScientificName"] <- "Polyphemus"
# Pseudochydorus sp.                                   no match
indS[indS$ScientificName == "Pseudochydorus sp.","ScientificName"] <- "Pseudochydorus"
# Scapholeberis sp.                                    no match
indS[indS$ScientificName == "Scapholeberis sp.","ScientificName"] <- "Scapholeberis"
# Sida sp.                                             no match
indS[indS$ScientificName == "Sida sp.","ScientificName"] <- "Sida"
# Simocephalus sp.                                     no match
indS[indS$ScientificName == "Simocephalus sp.","ScientificName"] <- "Simocephalus"

### Check again
keys <- wormsbynames( unique(indS$ScientificName), marine_only = "false")
keys$ScientificName <- unique(indS$ScientificName)

# Add WoRMS_status field
indS <-  add_column(indS, TaxonRank = NA, .after = "ScientificName")
indS <-  add_column(indS, AphiaID = NA, .after = "TaxonRank")
indS <-  add_column(indS, Status = NA, .after = "AphiaID")
colnames(indS)

# For testing the functions below:
s <- unique(indS$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(indS$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- indS[indS$ScientificName == s,]
            # head(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys[keys$ScientificName == s,"scientificname"]) ) {
            
                subset$AphiaID <- "No match found in WoRMS"
                subset$Status <- "No match found in WoRMS"
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys[keys$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$AphiaID <- as.character(keys[keys$ScientificName == s,"valid_AphiaID"])
                subset$TaxonRank <- keys[keys$ScientificName == s,"rank"]
                subset[,c("Kingdom","Phylum","Class","Order","Family","Genus")] <- keys[keys$ScientificName == s,c("kingdom","phylum","class","order","family","genus")]
            
            } # eo 1st if else loop - if species is actually found in 'w'
        
            ### 2nd if else loop to add species name if subset$TaxonRank == "Species"
            if( keys[keys$ScientificName == s,"rank"] == "Species" & !is.na(keys[keys$ScientificName == s,"rank"]) ) {
            
                subset$Species <- keys[keys$ScientificName == s,"valid_name"]
            
            } else {
            
                subset$Species <- NA
            
            } # eo 2nd if else loop - if rank == "Species" 
        
            ### 3rd if else loop to add subset$WoRMS_status
            if( !is.na(keys[keys$ScientificName == s,"valid_AphiaID"]) ) {
             
                statuses <- melt( keys[keys$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                subset$Status <- status2add
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 2 

) # eo mclapply - s in taxa_names

### Rbind
ddf2 <- bind_rows(res)
dim(ddf2) 
ddf2[1:20,]
unique(ddf2$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf2[is.na(ddf2$Species),]
unique(ddf2$AphiaID) 
unique(ddf2$Status) # good
### All good
summary(factor(ddf2$Family))

write.xlsx(ddf2, file = "NMFS-COPEPEDIA_table_biometry_zooplankton_IndSize_26_01_22.xlsx", sheetName = "Individual sizes", append = FALSE)


### ----------------------------------------------------------

### C) Individual mass (first reformatted on excel because easier)

### C.1) Reformat and add LifeStage and sex

indM <- data.frame(read_excel("NMFS-COPEPOD_tables_biometry_zooplankton.xlsx", sheet = 3))
dim(indM) # 1642    9
str(indM)
head(indM)
unique(indM$ScientificName)
indM$ScientificName <- trimws(indM$ScientificName, "both")

terms <- as.data.frame(str_split_fixed(indM$Taxonomic.identity, " ", n = 7))[,c(3:7)]
# head(terms) ; dim(terms)
# unique(terms[,"V3"])
# Col 3 and 4, remove "" by NA
terms[terms$V4 == "","V4"] <- NA
terms[terms$V5 == "","V5"] <- NA
terms[terms$V6 == "","V6"] <- NA
terms[terms$V7 == "","V7"] <- NA
colnames(terms) <- c("N1","N2","N3","N4","N5")

# Check content: are LfStages and sexes present in N2, N3, N4, N5
#"female" %in% unique(terms$N2) # sexes but also: CIV, CV, naup
#"male" %in% unique(terms$N3) # sp names but also lifestages and sexes
#"male" %in% unique(terms$N4) # only sexes and LifeStages
# unique(terms$N5) # --> only lifestages: "(NIII)","(CIII)"

### In indM, need to add: 
# - LifeStage: in N3 or N4
# - Sex: in N3 and N4
indM$LifeStage <- NA
indM$Sex <- NA
# For testing:
# i <- 44 ; terms[i,]

for(i in c(1:nrow(indM)) ) {
    
    message(paste(i, sep = ""))    
    
    ### If else loop for Sex ----------------------------------------------------------------
    if( grepl("male", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"Sex"] <- "male"
    }
    
    if( grepl("female", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"Sex"] <- "female"
    } 
    
    
    ### If else loop for LifeStage ----------------------------------------------------------------
    if( grepl("adult", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "adult"
    }
    
    if( grepl("naup", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "Nauplius"
    }
    
    if( grepl("cop", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "Copepodites"
    }
    
    if( grepl("NI", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "NI"
    }
    
    if( grepl("NII", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "NII"
    }
    
    if( grepl("NIII", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "NIII"
    }
    
    if( grepl("NIV", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "NIV"
    }
    
    if( grepl("NV", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "NV"
    }
    
    if( grepl("NVI", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "NVI"
    }
    
    if( grepl("CI", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "CI"
    }
    
    if( grepl("CII", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "CII"
    }
    
    if( grepl("CIII", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "CIII"
    }
    
    if( grepl("CIV", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "CIV"
    }
    
    if( grepl("CV", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "CV"
    }
    
    if( grepl("CVI", paste(terms[i,"N2"],terms[i,"N3"],terms[i,"N4"],terms[i,"N5"], sep = " "), fixed = T) ) {
        indM[i,"LifeStage"] <- "CVI"
    }
    
} # eo for loop - i in c(1:nrow(indS))

# Check indS$Sex LifeStage
unique(indM$Sex) 
indM[1:50,c("Taxonomic.identity","Sex","LifeStage")] # Worked
indM[,c("Taxonomic.identity","Sex","LifeStage")] 
# Say all NaN of LifeStage are "Assumed adult"
indM[is.na(indM$LifeStage),'LifeStage'] <- "Assumed adult"
### Worked! 


### 26/01/2022: Add 'C_variable' and 'Unit'
unique(indM$Mass)
# unique( as.data.frame(str_split_fixed(indM$Mass, ":", n = 2))[,1] ) # good :)
indM$C_variable <- as.data.frame(str_split_fixed(indM$Mass, ":", n = 2))[,1] # done
# Unit too
# "mg" %in% unique(indM$Mass) # only µg. Good.
indM$Unit <- "µg"
# Next, ho to extract average mass in µg
# ?parse_number
#test <- str_extract_all(unique(indM$Mass)[1076], "\\([^()]+\\)")[[1]]
#test <- substring(test, 2, nchar(test)-1)
#parse_number(x = test, trim_ws = T)

### Ok, so, for each row, test if brackets are present, if yes: do the 3 lines above, if not do the parse_number() directly
colnames(indM)
indM <-  add_column(indM, MeanMass_ug = NA, .after = "Unit")

# i <- 2
# i <- 1615
for(i in c(1:nrow(indM)) ) {
        
        message(indM[i,"Mass"])
        
        # if else loop: if yes: do the 3 lines above, if not do the parse_number() directly
        if( grepl("(", indM[i,"Mass"], fixed = T) ) {
            
            mass <- str_extract_all(indM[i,"Mass"], "\\([^()]+\\)")[[1]]
            mass <- substring(mass, 2, nchar(mass)-1)
            mass <- parse_number(x = mass, trim_ws = T)
            mass <- as.numeric(mass) # size
            indM[i,"MeanMass_ug"] <- mass
            
        } else {
            
            mass <- as.numeric( str_extract(indM[i,"Mass"], "\\d+\\.*\\d*") )
            #mass <- parse_number(x = indM[i,"Mass"], trim_ws = T)
            #mass <- as.numeric(mass) # size
            indM[i,"MeanMass_ug"] <- mass
            
        } # eo if else loop 
        
} # eo for loop
# Check with a summary?
summary(indM$MeanMass_ug)
# Check why so many NaN
head(indM[is.na(indM$MeanMass_ug),c(5,8)])
head(indM[!is.na(indM$MeanMass_ug),c(5,8)])
indM[100:150,c(5,8)] # Worked!


### C.2) Add taxonomic classif
library("worms")
unique(indM$ScientificName)
# Worms check
# keys <- wormsbynames( unique(indM$ScientificName), marine_only = "false")
# Just one to modify: Paraeuchaeta rubra female
indM[indM$ScientificName == "Paraeuchaeta rubra female","ScientificName"] <- "Paraeuchaeta rubra"
# Re-check quickly
keys <- wormsbynames( unique(indM$ScientificName), marine_only = "false")
keys$ScientificName <- unique(indM$ScientificName)
# Add classif and WoRMS_status field
indM <- add_column(indM, TaxonRank = NA, .after = "ScientificName")
indM <- add_column(indM, AphiaID = NA, .after = "TaxonRank")
indM <- add_column(indM, Status = NA, .after = "AphiaID")
colnames(indM)

# For testing the functions below:
s <- unique(indM$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(indM$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- indM[indM$ScientificName == s,]
            # head(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys[keys$ScientificName == s,"scientificname"]) ) {
            
                subset$AphiaID <- "No match found in WoRMS"
                subset$Status <- "No match found in WoRMS"
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys[keys$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$AphiaID <- as.character(keys[keys$ScientificName == s,"valid_AphiaID"])
                subset$TaxonRank <- keys[keys$ScientificName == s,"rank"]
                subset[,c("Kingdom","Phylum","Class","Order","Family","Genus")] <- keys[keys$ScientificName == s,c("kingdom","phylum","class","order","family","genus")]
            
            } # eo 1st if else loop - if species is actually found in 'w'
        
            ### 2nd if else loop to add species name if subset$TaxonRank == "Species"
            if( keys[keys$ScientificName == s,"rank"] == "Species" & !is.na(keys[keys$ScientificName == s,"rank"]) ) {
            
                subset$Species <- keys[keys$ScientificName == s,"valid_name"]
            
            } else {
            
                subset$Species <- NA
            
            } # eo 2nd if else loop - if rank == "Species" 
        
            ### 3rd if else loop to add subset$WoRMS_status
            if( !is.na(keys[keys$ScientificName == s,"valid_AphiaID"]) ) {
             
                statuses <- melt( keys[keys$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                subset$Status <- status2add
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 2 

) # eo mclapply - s in taxa_names

### Rbind
ddf2 <- bind_rows(res)
dim(ddf2) 
ddf2[1:30,]
unique(ddf2$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf2[is.na(ddf2$Species),"ScientificName"]
unique(ddf2$AphiaID) 
unique(ddf2$Status) # good
### All good
summary(factor(ddf2$Family))

write.xlsx(ddf2, file = "NMFS-COPEPEDIA_table_biometry_zooplankton_IndMass_26_01_22.xlsx", sheetName = "Individual masses", append = FALSE)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------