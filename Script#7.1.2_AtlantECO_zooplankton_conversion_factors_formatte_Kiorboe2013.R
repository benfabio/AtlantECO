
##### ATLANTECO SCRIPT 7.1.2 ----------------------------------------------------------------------------------------------------------------------------
##### 20/01/2023: R Script to clean and reformat TableA1 of Kiorboe 2013 (L&O) © Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Read the date tables you copy/pasted into excel 
# - Perform the WoRMS taxonomic check to add taxonomic classification (might be useful for higher group level conversions or average conversion rates)
# - Make some plots to show data distribution (n) per groups

### Original data tables accessible here: 
# https://aslopubs.onlinelibrary.wiley.com/doi/abs/10.4319/lo.2013.58.5.1843
# --> https://aslopubs.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.4319%2Flo.2013.58.5.1843&file=1843a.html 

### Latest update: 24/06/22

library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("lubridate")
library("viridis")
library("readxl")
library("xlsx")

substrRight <- function(x, n) { substr(x, nchar(x)-n+1, nchar(x)) }

### ----------------------------------------------------------------------------------------------------------------------------

setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Carbon_Conversion_Factors/Kiorboe2013")

### 1°) Read the table
d <- read.csv("TableA1_ZooplanktonBodyComposition_Kiorboe2013.csv", h = T, sep = ";")
dim(d)
head(d)
str(d)
# Check columns' content
unique(d$Group) ; summary(factor(d$Group))
unique(d$OrigScientificName)
unique(d$WetMass_mg)
unique(d$DryMass_mg)
unique(d$Ash_mg)
unique(d$CarbonMass_mg)

### 1.A) Clean dataset: curate ScientificName and LifeForms and then clean the mass measurements which you could not load as numeric variables because the way T. Kiorboe provided the data in the appendices of the original paper

### For cleaning WetMass_mg/DryMass_mg/Ash_mg/CarbonMass_mg --> make a function that converts to numerics and manages to treat the '\xd00X' as x10-X (x0.001 when X = 3 for instance)

# Test with 'CarbonMass_mg' first, sicne this is your biomass of interest 
vals <- unique(d$CarbonMass_mg)
# For testing function
v <- ""
v <- "7,32E\xd002"
v <- "7,12"

fOO <- function(v) {
    
    if( v == "" ) {
        
        num.val <- NA
        
    } else {
        
        # Identify if val has 'E\xd00' inside
        if( grepl("E\xd00", v) ) {
            
            # Then need to split the character in two based on the 'E'
            t <- as.data.frame(do.call(rbind, str_split(v, "E")))
            # Convert furst part to numeric
            t[,1] <- as.numeric(sub(",", ".", t[,1], fixed = T))
            
            # And multiply it by 10^(-X), X being the lst number of t[,2]
            x <- as.numeric(str_sub(t[,2], -1, -1))
            
            num.val <- t[,1]*10^(-x)
            
        } else {
            
            num.val <- as.numeric(sub(",", ".", v, fixed = T))
            
        } 
        
    }
    
    message(paste("Old char was: ",v," ; is now: ", num.val, sep = ""))
    message("                                                       ")
    
    return(num.val)
    
} # eo fOO

# Test in sapply
new.vals <- sapply(d$CarbonMass_mg, fOO)
str(new.vals)
d$CarbonMass_mg[1:15]
new.vals[1:15]

# Seems to work fine, apply to your columns
d2 <- d # save 'd' aside just in case
colnames(d2)

d2$WetMass_mg <- sapply(d$WetMass_mg, fOO)
d2$DryMass_mg <- sapply(d$DryMass_mg, fOO)
d2$Ash_mg <- sapply(d$Ash_mg, fOO)
d2$CarbonMass_mg <- sapply(d$CarbonMass_mg, fOO)
d2$NitrogenMass_mg <- sapply(d$NitrogenMass_mg, fOO)
# Check
summary(d2) # Looks ok
# Compare visually to original table 
d2[,c("OrigScientificName","CarbonMass_mg")]
# Good! 


### Clean the OrigScientificName, provide LifeForms
# First, identify LiefForms (sex, lifestages etc.)
unique(d2$ScientificName)
# " female"
# " male"
# " CV"
# " V+VI "
# " CVI "
# " VI "
# " V"
# " CIV"
# " VII"
# " VIII "
# " larva"
# " larvae"
# " zoea"
# " nauplius"
# "Meterythrops microphthalma juv" --> juvenile
# "aggregate"
# "solitary"

# For each row, extract the ScientificName and its LifeForm based on the factors identified above

# Separate Sex from Lifestages 
d2 <- add_column(d2, Sex = NA, .after = "LifeForm")

for(i in c(1:nrow(d2))) {
    
    name <- d2[i,"OrigScientificName"] #; name
    #message(name)
    # Use if loop statements to extract LifeForm
    if( grepl(" male",name) ) { d2[i,"Sex"] <- "Male" }
    if( grepl("female",name) ) { d2[i,"Sex"] <- "Female" }

}
# Check
summary(factor(d2$Sex)) # ; d2[,c("OrigScientificName","Sex")]
# Good. Same, but for Ligestages now

#i <- 632
for(i in c(1:nrow(d2))) {
    
    name <- d2[i,"OrigScientificName"] #; name
    message(name)
    # Use if loop statements to extract LifeForm
    if( grepl(" CV",name) ) { d2[i,"LifeForm"] <- "CV" }
    if( grepl(" V+VI ",name) ) { d2[i,"LifeForm"] <- "V+VI" }
    if( grepl(" CVI ",name) ) { d2[i,"LifeForm"] <- "CVI" }
    if( grepl(" VI ",name) ) { d2[i,"LifeForm"] <- "VI" }
    if( grepl(" V",name) ) { d2[i,"LifeForm"] <- "V" }
    if( grepl(" CIV",name) ) { d2[i,"LifeForm"] <- "CIV" }
    if( grepl(" VII",name) ) { d2[i,"LifeForm"] <- "VII" }
    if( grepl(" VIII ",name) ) { d2[i,"LifeForm"] <- "VIII" }
    if( grepl(" larva",name) ) { d2[i,"LifeForm"] <- "Larva" }
    if( grepl(" larvae",name) ) { d2[i,"LifeForm"] <- "Larvae" }
    if( grepl(" zoea",name) ) { d2[i,"LifeForm"] <- "Zoea larva" }
    if( grepl(" nauplius",name) ) { d2[i,"LifeForm"] <- "Nauplius larva" }
    if( grepl("Meterythrops microphthalma juv",name) ) { d2[i,"LifeForm"] <- "Juvenile" }
    if( grepl("aggregate",name) ) { d2[i,"LifeForm"] <- "Aggregate" }
    if( grepl("solitary",name) ) { d2[i,"LifeForm"] <- "Solitary" }    
    
}
# Check
summary(factor(d2$LifeForm)) #
d2[,c("OrigScientificName","LifeForm","Sex")]
# Good. Time to clean taxonomy now
d3 <- d2

### -------------------------------------------------------

### 1.B) Add taxonomic classif
library("worms")
unique(d3$ScientificName)
# Remove sp., male, female etc.
d3$ScientificName <- str_replace_all(d3$ScientificName, " sp.", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " female", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " male", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " aggregate", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " solitary", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " larva", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " larvae", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " zoea", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " V+VI", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " VIII", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " VII", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " VI", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " VI", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " V", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " CV", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " CIV", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, " juv", "")
d3$ScientificName <- str_replace_all(d3$ScientificName, "Pseudocalanus minutus+VI", "Pseudocalanus minutus")
d3$ScientificName <- str_replace_all(d3$ScientificName, "Neocalanus plumchrus+VI", "Neocalanus plumchrus")

# First check
keys.worms <- wormsbynames( unique(d3$ScientificName), marine_only = "false")
keys.worms$ScientificName <- unique(d3$ScientificName) 
head(keys.worms)
# First automatic round of corrections
sp2correct <- keys.worms[keys.worms$status == "unaccepted" & !is.na(keys.worms$status),c("ScientificName")]
# For testing: 
# sp <- sp2correct[1]
for(sp in sp2correct) {
    
        # Get valid name
        valid <- keys.worms[keys.worms$ScientificName == sp,c("valid_name")]
        message(paste("Changing ",sp," to ",valid, sep = ""))
        d3[d3$ScientificName == sp,"ScientificName"] <- valid
    
} # eo for loop 
# Re-run wormsbynames
# ?wormsbynames
keys.worms2 <- wormsbynames( unique(d3$ScientificName), marine_only = "false", match = F) # 
### Stuff to correct manually:
# Viblia antarctica                                    no match
d3[d3$ScientificName == "Viblia antarctica","ScientificName"] <- "Vibilia antarctica"
# Cyphocatris challengeri                              no match
d3[d3$ScientificName == "Cyphocatris challengeri","ScientificName"] <- "Cyphocaris challengeri"
# Primno menevillei                                    no match
d3[d3$ScientificName == "Primno menevillei","ScientificName"] <- "Primno latreillei"
# Bivalvee                                             no match
d3[d3$ScientificName == "Bivalvee","ScientificName"] <- "Bivalvia"
# Eukronia fowleri                                     no match
d3[d3$ScientificName == "Eukronia fowleri","ScientificName"] <- "Eukrohnia fowleri"
# Disseta palumbi                                      no match
d3[d3$ScientificName == "Disseta palumbi","ScientificName"] <- "Disseta palumbii"
# Candacia aetiopica                                   no match
d3[d3$ScientificName == "Candacia aetiopica","ScientificName"] <- "Candacia ethiopica"
# Centropages marinus                                  no match
d3[d3$ScientificName == "Centropages marinus","ScientificName"] <- "Centropages typicus"
### synonym
# Calanus hyperborius                                  no match
d3[d3$ScientificName == "Calanus hyperborius","ScientificName"] <- "Calanus hyperboreus"
# Neocalanus plumchrus+VI                              no match
d3[d3$ScientificName == "Neocalanus plumchrus+VI","ScientificName"] <- "Neocalanus plumchrus"
# Pseudocalanus minutus+VI                             no match
d3[d3$ScientificName == "Pseudocalanus minutus+VI","ScientificName"] <- "Pseudocalanus minutus"
# Gaidius variabilisI                                  no match
d3[d3$ScientificName == "Gaidius variabilisI","ScientificName"] <- "Gaetanus minutus"
# Paraeuchaeta japanica                                no match
d3[d3$ScientificName == "Paraeuchaeta japanica","ScientificName"] <- "Paraeuchaeta elongata"
# Eucalanus bungi                                      no match
d3[d3$ScientificName == "Eucalanus bungi","ScientificName"] <- "Eucalanus bungii"
# Mnemiopsis leidy                                     no match
d3[d3$ScientificName == "Mnemiopsis leidy","ScientificName"] <- "Mnemiopsis leidyi"
# Lucifer reynaudii                                    no match
d3[d3$ScientificName == "Lucifer reynaudii","ScientificName"] <- "Lucifer typus"
# Macruran mysis                                       no match
d3[d3$ScientificName == "Macruran mysis","ScientificName"] <- "Mysis larva"
# Macruran                                             no match
d3[d3$ScientificName == "Macruran","ScientificName"] <- "Macrura"
# Brachyuran                                           no match
d3[d3$ScientificName == "Brachyuran","ScientificName"] <- "Brachyura"
# Balanus nauplius                                     no match
d3[d3$ScientificName == "Balanus nauplius","ScientificName"] <- "Balanus"
# Anomuran                                             no match
d3[d3$ScientificName == "Anomuran","ScientificName"] <- "Anomura"
# Euphausia tricantha                                  no match
d3[d3$ScientificName == "Euphausia tricantha","ScientificName"] <- "Euphausia triacantha"
# Tessarabrachion oculatus                             no match
d3[d3$ScientificName == "Tessarabrachion oculatus","ScientificName"] <- "Tessarabrachion oculatum"
# Euphausia superbae                                   no match
d3[d3$ScientificName == "Euphausia superbae","ScientificName"] <- "Euphausia superba"
# Calyptotise                                          no match
d3[d3$ScientificName == "Calyptotise","ScientificName"] <- "Calyptosis larva"
# Cyrtopiae                                            no match
d3[d3$ScientificName == "Cyrtopiae","ScientificName"] <- "Cyrtopia"
# Cardiapoda sublaeviss                                no match
d3[d3$ScientificName == "Cardiapoda sublaeviss","ScientificName"] <- "Cardiapoda placenta"
# Clionina                                             no match
d3[d3$ScientificName == "Clionina","ScientificName"] <- "Clionidae"
# Idothea metallica                                    no match
d3[d3$ScientificName == "Idothea metallica","ScientificName"] <- "Idotea metallica"
# Meterythrops microphthakma                           no match
d3[d3$ScientificName == "Meterythrops microphthakma","ScientificName"] <- "Meterythrops microphthalmus"
# Achantomysis                                         no match
d3[d3$ScientificName == "Achantomysis","ScientificName"] <- "Acanthomysis"
# Conchoecia pseudodiscophora                          no match
d3[d3$ScientificName == "Conchoecia pseudodiscophora","ScientificName"] <- "Porroecia pseudoparthenoda"
# Larvae                                               no match
#d3[d3$ScientificName == "","ScientificName"] <- ""
# Polychaet                                            no match
d3[d3$ScientificName == "Polychaet","ScientificName"] <- "Polychaeta"
# Noctilluca scintillans                               no match
d3[d3$ScientificName == "Noctilluca scintillans","ScientificName"] <- "Noctiluca scintillans"
# Strobilidiumralis                                    no match
d3[d3$ScientificName == "Strobilidiumralis","ScientificName"] <- "Pelagostrobilidium spirale"
# Coroniderichthuse                                    no match
d3[d3$ScientificName == "Coroniderichthuse","ScientificName"] <- "Coroniderichthus larva"
# Alimae                                               no match
d3[d3$ScientificName == "Alimae","ScientificName"] <- "Alima"
# Oikopleura dioica                                    no match
d3[d3$ScientificName == "Oikopleura dioica","ScientificName"] <- "Oikopleura (Vexillaria) dioica"

# last worms check
keys.worms3 <- wormsbynames( unique(d3$ScientificName), marine_only = "false", match = F) # 
keys.worms3$ScientificName <- unique(d3$ScientificName) 

# Add WoRMS_status field
colnames(d3)
d3 <-  add_column(d3, TaxonRank = NA, .after = "ScientificName")
d3 <-  add_column(d3, AphiaID = NA, .after = "TaxonRank")
d3 <-  add_column(d3, Status = NA, .after = "AphiaID")

# For testing the function below:
s <- unique(d3$ScientificName)[1] ; s

require("parallel")
res <- mclapply( unique(d3$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- d3[d3$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms3[keys.worms3$ScientificName == s,"scientificname"]) ) {
            
                subset$AphiaID <- "No match found in WoRMS"
                subset$Status <- "No match found in WoRMS"
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$AphiaID <- as.character(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"])
                subset$TaxonRank <- keys.worms3[keys.worms3$ScientificName == s,"rank"]
                subset[,c("Kingdom","Phylum","Class","Order","Family","Genus")] <- keys.worms3[keys.worms3$ScientificName == s,c("kingdom","phylum","class","order","family","genus")]
            
            } # eo 1st if else loop - if species is actually found in 'w'
        
            ### 2nd if else loop to add species name if subset$TaxonRank == "Species"
            if( keys.worms3[keys.worms3$ScientificName == s,"rank"] == "Species" & !is.na(keys.worms3[keys.worms3$ScientificName == s,"rank"]) ) {
            
                subset$Species <- keys.worms3[keys.worms3$ScientificName == s,"valid_name"]
            
            } else {
            
                subset$Species <- NA
            
            } # eo 2nd if else loop - if rank == "Species" 
        
            ### 3rd if else loop to add subset$WoRMS_status
            if( !is.na(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"]) ) {
             
                statuses <- melt( keys.worms3[keys.worms3$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                subset$Status <- status2add
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 2 

) # eo mclapply - s in taxa_names

### Rbind
ddf <- bind_rows(res)
dim(ddf) # 725  21
# Check random lines
ddf[700:725,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf[is.na(ddf$Species),]
unique(ddf$AphiaID) 
unique(ddf$Status) # good

# Save as .xlsx
write.xlsx(ddf, file = "TableA1_ZooplanktonBodyComposition_Kiorboe2013_cleaned_17_06_22.xlsx", sheetName = "Data", append = FALSE)

### ----------------------------------------------------------------------------------------------------------------------------

### 24/06/2022: Clena taxonomy of the Suppl. 4 of Lucas et al. (2014)
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Carbon_Conversion_Factors/JedI")
d <- read.csv("Lucas2014_SupplTable_A4_toreformat.csv", sep = ";", dec = ".", h = T)
dim(d)
str(d)
# Add ScientificName
d$ScientificName <- paste(d$Genus, d$Species, sep = " ")
unique(d$ScientificName)
# Add OrigScientificName for traceability
d <- add_column(d, OrigScientificName = d$ScientificName, .before = "ScientificName")

# First WoRMS check
library("worms")
keys.worms <- wormsbynames( unique(d$ScientificName), marine_only = "false")
keys.worms$ScientificName <- unique(d$ScientificName) 
head(keys.worms)
# First automatic round of corrections
sp2correct <- keys.worms[keys.worms$status == "unaccepted" & !is.na(keys.worms$status),c("ScientificName")]
# For testing: 
# sp <- sp2correct[1]
for(sp in sp2correct) {
        # Get valid name
        valid <- keys.worms[keys.worms$ScientificName == sp,c("valid_name")]
        message(paste("Changing ",sp," to ",valid, sep = ""))
        d[d$ScientificName == sp,"ScientificName"] <- valid
} # eo for loop 
# wormsbyname again
keys.worms <- wormsbynames( unique(d$ScientificName), marine_only = "false")
### Stuff to correct manually:
# Pryosoma atlanticum                                  no match
d[d$ScientificName == "Pryosoma atlanticum","ScientificName"] <- "Pyrosoma atlanticum"
# Cyclocana welshi                                     no match
d[d$ScientificName == "Cyclocana welshi","ScientificName"] <- "Cyclocanna producta"
# Laodicia undulata                                    no match
d[d$ScientificName == "Laodicia undulata","ScientificName"] <- "Laodicea undulata"
# Bougainvillia princpis                               no match
d[d$ScientificName == "Bougainvillia princpis","ScientificName"] <- "Bougainvillia principis"
# Aglaura hemistom                                     no match
d[d$ScientificName == "Aglaura hemistom","ScientificName"] <- "Aglaura hemistoma"
# Amphinemum rugosum                                   no match
d[d$ScientificName == "Amphinemum rugosum","ScientificName"] <- "Amphinema rugosum"
# Bougainvillea niobe                                  no match
d[d$ScientificName == "Bougainvillea niobe","ScientificName"] <- "Bougainvillia niobe"
# Bougainvillea superciliaris                          no match
d[d$ScientificName == "Bougainvillea superciliaris","ScientificName"] <- "Bougainvillia superciliaris"
# Hybodcon pendulus                                    no match
d[d$ScientificName == "Hybodcon pendulus","ScientificName"] <- "Corymorpha pendula"
# Solmaris lenticulata                                 no match
d[d$ScientificName == "Solmaris lenticulata","ScientificName"] <- "Solmaris lenticula"
# Bougainvillea fondosa                                no match
d[d$ScientificName == "Bougainvillea fondosa","ScientificName"] <- "Bougainvillia frondosa"
# Aequorea pencilis                                    no match
d[d$ScientificName == "Aequorea pencilis","ScientificName"] <- "Aequorea pensilis"
# Ptychogena gracilis                                  no match
#d[d$ScientificName == "Ptychogena gracilis","ScientificName"] <- "" # Calycella gracilis?
# Tima bairdi                                          no match
d[d$ScientificName == "Tima bairdi","ScientificName"] <- "Tima bairdii"
# Eurhamphaea vexillegra                               no match
d[d$ScientificName == "Eurhamphaea vexillegra","ScientificName"] <- "Eurhamphaea vexilligera"
# Ocyropsis spp.                                       no match
d[d$ScientificName == "Ocyropsis spp.","ScientificName"] <- "Ocyropsis"

# Last check
keys.worms <- wormsbynames( unique(d$ScientificName), marine_only = "false")
keys.worms$ScientificName <- unique(d$ScientificName) 

# Good.

d <-  add_column(d, Status = NA, .after = "WoRMS_ID")

require("parallel")
#s <- unique(d$ScientificName)[13] ; s
res <- mclapply( unique(d$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- d[d$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- "No match found in WoRMS"
                subset$Status <- "No match found in WoRMS"
                #subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- as.character(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
                #subset$TaxonRank <- keys.worms3[keys.worms3$ScientificName == s,"rank"]
                subset[,c("Phylum","Class","Order","Family","Genus")] <- keys.worms[keys.worms$ScientificName == s,c("phylum","class","order","family","genus")]
            
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
dim(ddf)
summary(ddf$mass_mgC.ind.1)
colnames(ddf)
unique(ddf$WoRMS_ID)
unique(ddf$ScientificName)
unique(ddf$Order)
unique(ddf$Class)

# Move Family to before genus
ddf <- ddf %>% relocate(Family, .after = Order)

# Save
save(ddf, file = "Lucas2014_Suppl.Table_A4_WoRMScheck+reformat_24_06_22.RData")

### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------