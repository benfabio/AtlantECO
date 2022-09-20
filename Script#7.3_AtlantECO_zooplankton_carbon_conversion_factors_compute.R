
##### ATLANTECO SCRIPT 7.3 ----------------------------------------------------------------------------------------------------------------------------
##### 22/06/2022: R Script to compute Min/Max/Mean/Stdev individual Carbon mass (not wet weight or dry weight) for various zooplankton species/groups/lifestages based on the data available in: Moriarty et al. (2013), Lucas et al. (2014), Kiørboe (2013) and COPEPEDIA biometric values ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load needed excel sheets depending on group of interest (Kiørboe (2013) for groups other than copepods, Lucas et al. (2014) for jellyfish, etc.)
# - Derive Min/Max/Mean/Stdev C mass for the names given in 'table_tally_ScientificNames_abund_zooplankton_20_06_22.xlsx', at the appropriate taxonomic level (finest whenever possible)
# - Report Min/Max/Mean/stdev C mass

### Latest update: 08/07/2022

library("tidyverse")
library("reshape2")
library("readxl")
library("xlsx")

### ----------------------------------------------------------------------------------------------------------------------------

### Groups for which to define specific C conversion factors for
# Appendicularia - Moriarty et al. (2013) - DONE manually 
# Chaetognatha - Kiorboe (2013) + Moriarty et al. (2013) - DONE
# Euphausiacea - Kiorboe (2013) + Moriarty et al. (2013) - DONE
# Cnidaria+Ctenophora - Kiorboe (2013) + Moriarty et al. (2013) + Lucas et al. (2014) - DONE
# Copepoda - COPEPEDIA biometric tables - DONE

### ----------------------------------------------------------------------------------------------------------------------------

### A°) Appendicularia --> Done directly on the excel sheet based on Moriarty et al. (2013) - Table 2. 

### ----------------------------------------------------------

### B°) Chaetognatha: Kiorboe (2013) + Moriarty et al. (2013)
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Carbon_Conversion_Factors/Kiorboe2013") 
# dir()
kio <- as.data.frame(read_excel("TableA1_ZooplanktonBodyComposition_Kiorboe2013_cleaned_17_06_22.xlsx", sheet = 1))
dim(kio)
str(kio) # Good.
unique(kio$Group)
# Show Chaetognatha data
kio[kio$Group == "Chaetognatha",] 
dim(kio[kio$Group == "Chaetognatha",] ) # 17 measurements
summary(factor(kio[kio$Group == "Chaetognatha","ScientificName"]))

# Group-level data
summary(kio[kio$Group == "Chaetognatha","CarbonMass_mg"]) # sd(kio[kio$Group == "Chaetognatha","CarbonMass_mg"])

# Family-level data
summary(kio[kio$Family == "Sagittidae" & !is.na(kio$Family),"CarbonMass_mg"])
sd(kio[kio$Family == "Sagittidae" & !is.na(kio$Family),"CarbonMass_mg"])

# Genus-level data
summary( kio[kio$Genus == "Flaccisagitta" & !is.na(kio$Genus),"CarbonMass_mg"] )
sd(kio[kio$Genus == "Flaccisagitta" & !is.na(kio$Genus),"CarbonMass_mg"])

# Species-level data
summary( kio[kio$ScientificName == "Flaccisagitta enflata" & !is.na(kio$Genus),"CarbonMass_mg"] )
sd(kio[kio$ScientificName == "Flaccisagitta enflata" & !is.na(kio$Genus),"CarbonMass_mg"])

summary( kio[kio$ScientificName == "Parasagitta elegans" & !is.na(kio$Genus),"CarbonMass_mg"] )
sd(kio[kio$ScientificName == "Parasagitta elegans" & !is.na(kio$Genus),"CarbonMass_mg"])

### ----------------------------------------------------------

### C°) Euphausiacea - Kiorboe (2013) + Moriarty et al. (2013)
# Show Euphausiacea data
kio[kio$Group == "Euphausiacea" & !is.na(kio$Group),] 
unique(kio[kio$Group == "Euphausiacea" & !is.na(kio$Group),"ScientificName"]) 
dim(kio[kio$Group == "Euphausiacea" & !is.na(kio$Group),]) # 117 measurements
summary(factor(kio[kio$Group == "Euphausiacea" & !is.na(kio$Group),"ScientificName"]))
summary(kio[kio$Group == "Euphausiacea" & !is.na(kio$Group),"CarbonMass_mg"])
sd(kio[kio$Group == "Euphausiacea" & !is.na(kio$Group),"CarbonMass_mg"], na.rm = T)

# Thysanoessa-level values -> Moriarty et al. (2013) 

# Euphausia-level values 
summary(kio[kio$Genus == "Euphausia" & !is.na(kio$Genus),"CarbonMass_mg"])
sd(kio[kio$Genus == "Euphausia" & !is.na(kio$Genus),"CarbonMass_mg"])

# E. triacantha
kio[kio$Species == "Euphausia triacantha" & !is.na(kio$Genus),"CarbonMass_mg"]

# E. triacantha
kio[kio$Species == "Euphausia triacantha" & !is.na(kio$Genus),"CarbonMass_mg"]

# Euphausiidae-level
summary(kio[kio$Family == "Euphausiidae" & !is.na(kio$Family),"CarbonMass_mg"])
sd(kio[kio$Family == "Euphausiidae" & !is.na(kio$Family),"CarbonMass_mg"], na.rm = T)

# Euphausia lucens
kio[kio$Species == "Euphausia lucens" & !is.na(kio$Genus),"CarbonMass_mg"]
summary(kio[kio$Species == "Euphausia lucens" & !is.na(kio$Family),"CarbonMass_mg"])
sd(kio[kio$Species == "Euphausia lucens" & !is.na(kio$Family),"CarbonMass_mg"], na.rm = T)

# E. recurva
kio[kio$Species == "Euphausia recurva" & !is.na(kio$Genus),"CarbonMass_mg"]

# E. crystallorophias
summary(kio[kio$Species == "Euphausia crystallorophias" & !is.na(kio$Species),"CarbonMass_mg"])
sd(kio[kio$Species == "Euphausia crystallorophias" & !is.na(kio$Species),"CarbonMass_mg"], na.rm = T)

# T. inermis
kio[kio$Species == "Thysanoessa inermis" & !is.na(kio$Species),"CarbonMass_mg"]

# T. longipes
kio[kio$Species == "Thysanoessa longipes" & !is.na(kio$Species),"CarbonMass_mg"]

# Tessarabrachion oculatum
kio[kio$Species == "Tessarabrachion oculatum" & !is.na(kio$Species),"CarbonMass_mg"]

# Meganyctiphanes norvegica
kio[kio$Species == "Meganyctiphanes norvegica" & !is.na(kio$Species),"CarbonMass_mg"]

### ----------------------------------------------------------

### D°) Jellyfish - Kiorboe (2013) + Moriarty et al. (2013) + Lucas et al. (2014)

### Note: Moriarty et al. (2013) has 11 species:
# 2 Ctenophora: Bolionopsis infundibulum & Pleurobrachia pileus
# 2 Scyphozoa: Aurelia aurita & Cyaena capillata
# 7 Hydrozoa: Aglantha digitale, Eutonina indicans, Philalidium gregarium, Sarsia princeps, Sarsia tubulosa, Stomotoca atra, Agalma elegans

### For Lucas et al. (2014) (Suppl. Table 4 originally called 'geb12169-sup-0004-as4.xls'):
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Carbon_Conversion_Factors/JeDi")
lucas <- get(load("Lucas2014_Suppl.Table_A4_WoRMScheck+reformat_24_06_22.RData"))
dim(lucas)
dim(lucas[lucas$Phylum == "Cnidaria" & !is.na(lucas$Phylum),]) # 85 measurements
dim(lucas[lucas$Phylum == "Ctenophora" & !is.na(lucas$Phylum),]) # 16 measurements

dim(kio[kio$Group == "Cnidaria" & !is.na(kio$Group),]) #  53 measurements
dim(kio[kio$Group == "Ctenophora" & !is.na(kio$Group),]) # 132 measurements


### It gets quite complicated to find species names from 'tally' that match those in the 3 carbon mass data sources
### Need to quickly identify which ScientificNames in 'tally' have a direct counterpart in one of the 3 datasets
moriarty.spp <- c("Bolionopsis infundibulum","Pleurobrachia pileus","Aurelia aurita","Cyaena capillata","Aglantha digitale",
                "Eutonina indicans","Philalidium gregarium","Sarsia princeps","Sarsia tubulosa","Stomotoca atra","Agalma elegans")

lucas.spp <- unique(lucas$ScientificName)
kio.spp <- unique(kio$ScientificName)
all.spp <- c(moriarty.spp,lucas.spp,kio.spp)

setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Carbon_Conversion_Factors/")
tally <- as.data.frame(read_excel("table_tally_ScientificNames_abund_zooplankton_20_06_22.xlsx", sheet = 1))
dim(tally)
str(tally)
names2find <- unique(tally[tally$Group == "Jellyfish" & !is.na(tally$Group),"ScientificName"])
# Find commons : ?base::intersect
commons <- base::intersect(names2find, all.spp) # 24 out of 114
# sp <- commons[13]
for(sp in commons) {
    
    if( sp %in% moriarty.spp ) {
        message(paste(sp, " is in Moriarty et al. (2013)", sep = ""))
    } else if( sp %in% lucas.spp ) {
        message(paste(sp, " is in Lucas et al. (2014)", sep = ""))
    } else if( sp %in% kio.spp ) {
        message(paste(sp, " is in Kiorboe (2013)", sep = ""))
    } # eo if else loop 
    
}


# Solmundella bitentaculata not in there --> Narcomedusae level

lucas[lucas$ScientificName == "Periphylla periphylla",]
mean(kio[kio$ScientificName == "Beroe","CarbonMass_mg"]) ; sd(kio[kio$ScientificName == "Beroe","CarbonMass_mg"])


# To find Genus
unique(lucas[lucas$Genus == "Halistemma" & !is.na(lucas$Genus),"ScientificName"])
unique(kio[kio$Genus == "Halistemma" & !is.na(kio$Genus),"ScientificName"])

# To find Family
unique(lucas[lucas$Family == "Agalmatidae" & !is.na(lucas$Family),"ScientificName"])
unique(kio[kio$Family == "Agalmatidae" & !is.na(kio$Family),"ScientificName"])
# Show rows
lucas[lucas$Family == "Halicreatidae" & !is.na(lucas$Family),]
kio[kio$Family == "Halicreatidae" & !is.na(kio$Family),]

# To find Order
unique(lucas[lucas$Order == "Cydippida" & !is.na(lucas$Order),"ScientificName"])
unique(kio[kio$Order == "Cydippida" & !is.na(kio$Order),"ScientificName"])

# To find Class
unique(lucas[lucas$Class == "Scyphozoa" & !is.na(lucas$Class),"ScientificName"]) 
dim( lucas[lucas$Class == "Scyphozoa" & !is.na(lucas$Class),] ) # 

unique(kio[kio$Class == "Scyphozoa" & !is.na(kio$Class),"ScientificName"])
dim( kio[kio$Class == "Scyphozoa" & !is.na(kio$Class),] ) # 

summary(lucas[lucas$Class == "Scyphozoa" & !is.na(lucas$Class),"mass_mgC.ind.1"])
summary(kio[kio$Class == "Scyphozoa" & !is.na(kio$Class),"CarbonMass_mg"])

hydro.masses <- c(kio[kio$Class == "Hydrozoa" & !is.na(kio$Class),"CarbonMass_mg"],lucas[lucas$Class == "Hydrozoa" & !is.na(lucas$Class),"mass_mgC.ind.1"])
summary(hydro.masses)
sd(hydro.masses)

narco.masses <- lucas[lucas$Order == "Narcomedusae" & !is.na(lucas$Order),"mass_mgC.ind.1"]
summary(narco.masses) ; sd(narco.masses)

trachy.masses <- c(kio[kio$Order == "Trachymedusae" & !is.na(kio$Order),"CarbonMass_mg"],lucas[lucas$Order == "Trachymedusae" & !is.na(lucas$Order),"mass_mgC.ind.1"])
summary(trachy.masses) ; sd(trachy.masses)

cydi.masses <- c(kio[kio$Order == "Cydippida" & !is.na(kio$Order),"CarbonMass_mg"],lucas[lucas$Order == "Cydippida" & !is.na(lucas$Order),"mass_mgC.ind.1"])
summary(cydi.masses) ; sd(cydi.masses)

antho.masses <- lucas[lucas$Order == "Anthoathecata" & !is.na(lucas$Order),"mass_mgC.ind.1"]
summary(antho.masses) ; sd(antho.masses)

lobata.masses <- lucas[lucas$Order == "Lobata" & !is.na(lucas$Order),"mass_mgC.ind.1"]
summary(lobata.masses) ; sd(lobata.masses)

scyphozoa.masses <- lucas[lucas$Class == "Scyphozoa" & !is.na(lucas$Class),"mass_mgC.ind.1"]
summary(scyphozoa.masses) ; sd(scyphozoa.masses)


### For Siphonophorae: combine Kiorboe (2013) with A. elegans values from Moriarty et al. (2013)
# 9.35+0.18+2.78/3 = 10.45667 mgC.ind-1
# From Moriarty et al. (2013) in mgC instead of µM: 0.006005, 0.055246, 0.019216 (0.016814)
mean(c(9.35,0.18,2.78,0.019216))
sd(c(9.35,0.18,2.78,0.019216))

# Phylum level: Cnidaria
cnid.masses <- c(kio[kio$Phylum == "Cnidaria" & !is.na(kio$Phylum),"CarbonMass_mg"],lucas[lucas$Phylum == "Cnidaria" & !is.na(lucas$Phylum),"mass_mgC.ind.1"])
cnid.masses <- na.omit(cnid.masses)
summary(cnid.masses)
sd(cnid.masses)


### ----------------------------------------------------------

### E°) Copepoda - COPEPEDIA tables (© Todd O'Brien, NOAA)
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Carbon_Conversion_Factors/") 
tally <- as.data.frame(read_excel("table_tally_ScientificNames_abund_Hexanauplia_06_07_22.xlsx", sheet = 1))
dim(tally)
str(tally)
head(tally)
colnames(tally)
unique(tally$LifeForm)

setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Carbon_Conversion_Factors/COPEPEDIA Tables") 
cops <- as.data.frame(read_excel("NMFS-COPEPEDIA_table_biometry_zooplankton_IndMass_26_01_22.xlsx", sheet = 1))
str(cops)
# Check some columns
summary(cops$MeanMass) # mixes dry weights and C masses
cops.mass <- cops[cops$C_variable == "C mass",] # we do not need dry weights as it incorporates other elements than carbon
# Subset copepods only: Class = Hexanauplia
cops.mass <- cops.mass[cops.mass$Class == "Hexanauplia",]
dim(cops.mass) # 290 measurements
summary(cops.mass$MeanMass) 
# Min.    1st Qu.   Median  Mean     3rd Qu.  Max. 
# 0.018    1.740    5.535  157.376  158.550 3412.000 

colnames(cops.mass)
# Check scientificNames and classif
unique(cops.mass$ScientificName)
unique(cops.mass$Genus)
unique(cops.mass$Family)
unique(cops.mass$Order)
# Check LifeStages and Sex indicators
unique(cops.mass$LifeStage)
unique(cops.mass$Sex)
### --> LifeStages & Sex columns in the abundance data (output of Script #5.3) should have the same values --> modify Script #7.2 (DONE)


### Identify common columns
# intersect(colnames(tally), colnames(cops.mass))

### What is the bets strategy to find the closest measurements of individual C mass in 'cops.mass' for each ScientificName x LifeForm combination of 'tally' (i.e. the names in the abundance dataset) ? 
tally <- add_column(tally, combination = paste(tally$ScientificName, tally$LifeForm, sep = " ; "), .before = "Min Carbon Mass") 
head(tally)
unique(tally$combination)

### For each value 'c' of unique(tally$combination): identify the closest ScientificName in 'cops.mass', its LifeForm (if not NA), or the closest observations within higher order taxonomic groups if 'c' has no counterpart in 'cops.mass' (e.g., if no Acartia clausi in 'cops', use the Min/Max/Mean values based on all Acartia values).

### OK, for observations that correspond to Copepodites or Nauplius stages BUT that have no exact counterpart in cops.mass, use these ranges of values:
sub.copepodites <- cops.mass[(cops.mass$LifeStage %in% c("Copepodites","CI","CIII","CV","CIV")),]
sub.nauplii <- cops.mass[(cops.mass$LifeStage %in% c("Nauplius","NIII","NIV","NII","NI")),]
copepodites.vals <- data.frame(Min = min(sub.copepodites$MeanMass), Max = max(sub.copepodites$MeanMass), Mean = mean(sub.copepodites$MeanMass), Stdev = sd(sub.copepodites$MeanMass), Nobs = nrow(sub.copepodites) )
nauplii.vals <- data.frame(Min = min(sub.nauplii$MeanMass), Max = max(sub.nauplii$MeanMass), Mean = mean(sub.nauplii$MeanMass), Stdev = sd(sub.nauplii$MeanMass), Nobs = nrow(sub.nauplii))


### For testing long function below - various cases that returned an issue
# c <- unique(tally$combination)[3738] ; c # for a non matching element
# c <- unique(tally$combination)[1] ; c # for a matching element
# c <- unique(tally$combination)[37] ; c # for a matching element
# c <- unique(tally$combination)[373] ; c
c <- unique(tally$combination)[3] ; c
# c <- "Metridia longa ; CV ; female"
# c <- "Calanoida ; Small"
# c <- unique(tally$combination)[156] ; c
# c <- "Calanus pacificus ; CVI ; min-size= 3. mm"
# c <- "Pleuromamma gracilis ; NA"
# c <- "Calanus pacificus ; CVI ; female"
# c <- "Temora longicornis ; NA"
# c <- "Temora stylifera ; male"
# c <- "Temora turbinata ; adult"

res <- lapply(unique(tally$combination), function(c) {
    
            # Get index of c in unique(tally$combination)
            ind <- match(c, unique(tally$combination))
            
            # Get the corresponding ScientificName, taxonomic rank and LifeForm of 'c'
            sname <- tally[tally$combination == c,"ScientificName"]
            form <- tally[tally$combination == c,"LifeForm"]
            rank <- tally[tally$combination == c,"TaxonRank"]
            # sname; form; rank
            
            # Check if sname is already in 'cops.mass'
            if( sname %in% unique(cops.mass$ScientificName) ) {
                
                    message(paste(ind," | ",sname," is present in the COPEPEDIA IndMass table - fetching corresponding C mass values", sep = ""))
                
                    # sname does have a counterpart in cops.mass already
                    # Based on 'rank', subset the right part of 'cops.mass'
                    if( rank %in% c("Species","Subspecies") ) {
                    
                        subset <- cops.mass[cops.mass$ScientificName == sname,]
                    
                    } else if( rank %in% c("Subgenus","Genus") ) {
                    
                        subset <- cops.mass[cops.mass$Genus == sname,]
                    
                    } else if( rank %in% c("Family","Subfamily") ) {
                    
                        subset <- cops.mass[cops.mass$Family == sname,]
                    
                    } else if( rank %in% c("Order","Suborder") ) {
                    
                        subset <- cops.mass[cops.mass$Order == sname,]
                    
                    } else if( rank %in% c("Class","Subclass") ) {
                    
                        subset <- cops.mass[cops.mass$Class == "Hexanauplia",]
                    
                    } # 2nd if else loop

                    # OK, you've got the appropriate subset of cops.names, check whether you can go further based on LifeForm
                    
                    # First, check if 'form' is of use: is it something reported in 'cops.mass'
                    forms2checkagainst <- c(unique(cops.mass$LifeStage), unique(cops.mass$Sex))
                
                    if( TRUE %in% str_detect(form, forms2checkagainst, negate = F) ) {
                      # 'form' contains at least element of 'forms2checkagainst', keep it that way
                      form <- form
                    } else {
                        # 'form' does not contain any of the element in 'forms2checkagainst', transform it to NA (e.g., forms like a size or 'small')
                        form <- "NA"
                    } # eo if else loop - is form in 'forms2checkagainst'                    
                    
                    
                    # If 'form' is not NA, then find whether it has a counterpart in 'LifeStage' and 'Sex' of 'subset'
                    if( form != "NA" ) {
                    
                        # Check if adult
                        if( grepl("adult", form) | grepl("male", form) | grepl("female", form) ) {
                        
                            # Subset adult measurements only (if it says 'male' or 'female' but not 'adult', then still assume it's an adult as late stages of copepodites (CIV, CV or CVI) are already quite close to adult stages in terms of C content)
                            
                            # Subset adult measurements
                            if( TRUE %in% grepl("adult", unique(subset$LifeStage), fixed = T) ) {
                                    subset2 <- subset[grepl("adult", subset$LifeStage),]
                            } else if( TRUE %in% grepl("male", unique(subset$Sex), fixed = T) ) {
                                    subset2 <- subset[grepl("adult", subset$Sex),]
                            } else if( TRUE %in% grepl("female", unique(subset$Sex), fixed = T) ) {    
                                    subset2 <- subset[grepl("female", subset$Sex),]
                            } else {
                                    subset2 <- cops.mass[cops.mass$Order == ord,]
                                    subset2 <- subset2[grepl("adult", subset2$LifeStage),]
                            } # eo if else loop - are there adult/male/female measurements in subset?
                        
                            # And try to refine based on male/female measurements (female copepods tend to be larger and thus 'heavier')
                            if( grepl(" male", form) & "male" %in% subset2$Sex ) {
                            
                                    # Subset male measurements in subset2
                                    subset3 <- subset2[subset2$Sex == "male",]
                            
                                    # No sex-specific measurements in subset2, just use range of adult measurements 
                                    n <- nrow(subset3)
                            
                                    if(n == 1) {
                                
                                        min <- NA
                                        max <- NA
                                        mean <- subset3$MeanMass
                                        sd <- NA
                                        # identify references
                                        ref <- paste(unique(subset3$Reference), sep = ";")
                                
                                    } else if(n == 2) {
                                
                                        min <- min(subset3$MeanMass)
                                        max <- max(subset3$MeanMass)
                                        mean <- mean(subset3$MeanMass)
                                        sd <- NA
                                        # identify references
                                        ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                                    } else if(n >= 3) {
                             
                                        min <- min(subset3$MeanMass)
                                        max <- max(subset3$MeanMass)
                                        mean <- mean(subset3$MeanMass)
                                        sd <- sd(subset3$MeanMass)
                                        # identify references
                                        ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                                    } # eo if else loop
                            
                                    ### Return these values in a ddf
                                    data2return <- data.frame(combination = c, Nobs = n,
                                        Min = min, Max = max, Mean = mean, Stdev = sd, 
                                        unit = "µgC.ind-1", Reference = ref, Note = paste("Adult male values of ",sname, sep = "")
                                    ) # eo ddf
                                           
                            } else if( grepl("female", form) & "female" %in% subset2$Sex ) {
                            
                                    # Subset female measurements in subset2
                                    subset3 <- subset2[subset2$Sex == "female",]
                            
                                    # No sex-specific measurements in subset2, just use range of adult measurements 
                                    n <- nrow(subset3)
                            
                                    if(n == 1) {
                                
                                        min <- NA
                                        max <- NA
                                        mean <- subset3$MeanMass
                                        sd <- NA
                                        # identify references
                                        ref <- paste(unique(subset3$Reference), sep = ";")
                                
                                    } else if(n == 2) {
                                
                                        min <- min(subset3$MeanMass)
                                        max <- max(subset3$MeanMass)
                                        mean <- mean(subset3$MeanMass)
                                        sd <- NA
                                        # identify references
                                        ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                                    } else if(n >= 3) {
                             
                                        min <- min(subset3$MeanMass)
                                        max <- max(subset3$MeanMass)
                                        mean <- mean(subset3$MeanMass)
                                        sd <- sd(subset3$MeanMass)
                                        # identify references
                                        ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                                    } # eo if else loop
                            
                                    data2return <- data.frame(combination = c, Nobs = n,
                                        Min = min, Max = max, Mean = mean, Stdev = sd, 
                                        unit = "µgC.ind-1", Reference = ref, Note = paste("Adult female values of ",sname, sep = "")
                                    ) # eo ddf
                            
                            } else {
                            
                                    # No sex-specific measurements in subset2, just use range of adult measurements 
                                    n <- nrow(subset2)
                            
                                    if(n == 1) {
                                
                                        min <- NA
                                        max <- NA
                                        mean <- subset2$MeanMass
                                        sd <- NA
                                        # identify references
                                        ref <- paste(unique(subset2$Reference), sep = ";")
                                
                                    } else if(n == 2) {
                                
                                        min <- min(subset2$MeanMass)
                                        max <- max(subset2$MeanMass)
                                        mean <- mean(subset2$MeanMass)
                                        sd <- NA
                                        # identify references
                                        ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                                    } else if(n >= 3) {
                             
                                        min <- min(subset2$MeanMass)
                                        max <- max(subset2$MeanMass)
                                        mean <- mean(subset2$MeanMass)
                                        sd <- sd(subset2$MeanMass)
                                        # identify references
                                        ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                                    } # eo if else loop
                            
                                    data2return <- data.frame(combination = c, Nobs = n,
                                        Min = min, Max = max, Mean = mean, Stdev = sd, 
                                        unit = "µgC.ind-1", Reference = ref, Note = paste("Non sex-specific adult values of ",sname, sep = "")
                                    ) # eo ddf
       
                            } # eo if loop based on adult sex-specific measurement
                        
                    } else {
                        
                        # Subset NON adult measurements
                        subset2 <- subset[!grepl("adult", subset$LifeStage),]
                        
                        # Go further if nauplius or copepodite lifestage is in 'subset2' (meaning: length of subset2 is >= 1)
                        if( nrow(subset2) >= 1 ) {
                            
                            # How many mesurements? (if less than 3, cannot compute stdev, if less than 2 cannot compute Min/Max/Mean)
                            n <- nrow(subset2)
                            
                            if(n == 1) {
                                
                                min <- NA
                                max <- NA
                                mean <- subset2$MeanMass
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference), sep = ";")
                                
                            } else if(n == 2) {
                                
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } else if(n >= 3) {
                             
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- sd(subset2$MeanMass)
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } # eo if else loop
                            
                            data2return <- data.frame(combination = c, Nobs = n,
                                Min = min, Max = max, Mean = mean, Stdev = sd, 
                                unit = "µgC.ind-1", Reference = ref, Note = paste(form ," values of ",sname, sep = "")
                            ) # eo ddf
                            
                        } else {
                            
                            # If no lifestage specific values in 'subset2', use range of copepodites or nauplis values
                            if( TRUE %in% str_detect(string = form, pattern = c("Copepodites","CI","CII","CIII","CIV","CV","CVI")) ) {
                                                                                                
                                # If 'form' indicates at least one copepodite stage, use 'copepodites.vals'
                                data2return <- data.frame(combination = c, Nobs = copepodites.vals$Nobs,
                                    Min = copepodites.vals$Min, Max = copepodites.vals$Max, Mean = copepodites.vals$Mean, Stdev = copepodites.vals$Stdev, 
                                    unit = "µgC.ind-1", Reference = NA, Note = paste("Copepodites stages values (n = 39)", sep = "")
                                ) # eo ddf
                       
                                
                            } else if( TRUE %in% str_detect(string = form, pattern = c("Nauplius","NI","NII","NIII","NIV","NV","NVI")) ) {
                                
                                # If 'form' indicates at least one nauplius stage, use 'nauplii.vals'
                                data2return <- data.frame(combination = c, Nobs = nauplii.vals$Nobs,
                                    Min = nauplii.vals$Min, Max = nauplii.vals$Max, Mean = nauplii.vals$Mean, Stdev = nauplii.vals$Stdev, 
                                    unit = "µgC.ind-1", Reference = NA, Note = paste("Nauplius stages values (n = 19)", sep = "")
                                ) # eo ddf
                                
                            } else {
                                
                                # If no 'form' is found anywhere, then return NA 
                                data2return <- data.frame(combination = c, Nobs = NA,
                                    Min = NA, Max = NA, Mean = NA, Stdev = NA, 
                                    unit = NA, Reference = NA, Note = paste("No matching values in the COPEPEDIA IndMass table", sep = "")
                                ) # eo ddf
                                
                            } # eo if else loop - is form in 'subset' or should you use copepodites-level or nauplius-level values                            
                                
                        } # eo if else loop based on LifeStages
                    
                     } # eo if else loop -    
                        
                    } else if ( form == "NA" ) {
                    
                        # If form == "NA", the observation of level 'c' is noether lifestage-specific nor sex-specific...so use the range of adult values in subset (we assume that lifestage == 'adult' when form == 'NA')
                        
                        if( TRUE %in% grepl("adult", unique(subset$LifeStage), fixed = T) ) {
                            
                            subset2 <- subset[grepl("adult", subset$LifeStage),]
                            
                            n <- nrow(subset2)
                        
                            if(n == 1) {
                            
                                min <- NA
                                max <- NA
                                mean <- subset2$MeanMass
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference), sep = ";")
                            
                            } else if(n == 2) {
                            
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                            
                            } else if(n >= 3) {
                         
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- sd(subset2$MeanMass)
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                            
                            } # eo if else loop
                        
                            data2return <- data.frame(combination = c, Nobs = n,
                                Min = min, Max = max, Mean = mean, Stdev = sd, 
                                unit = "µgC.ind-1", Reference = ref, Note = paste("Adult values of ",sname," (lifestage assumed to be adult)", sep = "")
                            ) # eo ddf
                            
                        } else {
                            
                            subset2 <- cops.mass[cops.mass$Order == ord,]
                            subset2 <- subset2[grepl("adult", subset2$LifeStage),]
                            
                            n <- nrow(subset2)
                        
                            if(n == 1) {
                            
                                min <- NA
                                max <- NA
                                mean <- subset2$MeanMass
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference), sep = ";")
                            
                            } else if(n == 2) {
                            
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                            
                            } else if(n >= 3) {
                         
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- sd(subset2$MeanMass)
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                            
                            } # eo if else loop
                        
                            data2return <- data.frame(combination = c, Nobs = n,
                                Min = min, Max = max, Mean = mean, Stdev = sd, 
                                unit = "µgC.ind-1", Reference = ref, Note = paste("Adult values of ",ord," (lifestage assumed to be adult and COPEPEDIA table only had larval stages)", sep = "")
                            ) # eo ddf
                            
                        }
                    
                    } # eo if loop for 'form': is it 'NA' or not?
                    
                    
                } else {
                
                #########################################################################################################
                
                message(paste(ind," | ",sname," IS NOT present in the COPEPEDIA IndMass table - fetching C mass values of closest relatives", sep = ""))
                
                # sname has a NO counterpart in cops.mass --> need to find the closest match based on taxonomic classif  
                # If rank == "Species", if the genus of the species in cops.mass? If rank == "Genus", is the Family there etc.
                gen <- unique(tally[tally$combination == c,"Genus"])
                fam <- unique(tally[tally$combination == c,"Family"])
                ord <- unique(tally[tally$combination == c,"Order"])
                # gen; fam; ord
                
                if( rank %in% c("Species","Subspecies") ) {
                       
                        if( gen %in% unique(cops.mass$Genus) ) {
                
                            # If genus of sname is in 'cops.mass', retrieve corresponding measurements
                            subset <- cops.mass[cops.mass$Genus == gen,]
                            level <- "Genus-level"
                        
                        } else if(fam %in% unique(cops.mass$Family)) {
                        
                            subset <- cops.mass[cops.mass$Family == fam,]
                            level <- "Family-level"
                                   
                        } else if(ord %in% unique(cops.mass$Order)) {
                            
                            subset <- cops.mass[cops.mass$Order == ord,]
                            level <- "Order-level"
                            
                        } else {
                                
                            # And if neither of those levels work, then use the Min/Max/Mean/Stdev values of Calanoida+Cyclopoida+Harpacticoida        
                            subset <- cops.mass[cops.mass$Order %in% c("Calanoida","Cyclopoida","Harpacticoida"),]
                            level <- "Class-level"    
                                
                        } # eo if else loop 
                    
                        
                } else if ( rank %in% c("Genus","Subgenus") ) {
                        
                        if(fam %in% unique(cops.mass$Family)) {
                    
                            subset <- cops.mass[cops.mass$Family == fam,]
                            level <- "Family-level"
                               
                        } else if(ord %in% unique(cops.mass$Order)) {
                        
                            subset <- cops.mass[cops.mass$Order == ord,]
                            level <- "Order-level"
                        
                        } else {
                            
                            # And if neither of those levels work, then use the Min/Max/Mean/Stdev values of Calanoida+Cyclopoida+Harpacticoida        
                            subset <- cops.mass[cops.mass$Order %in% c("Calanoida","Cyclopoida","Harpacticoida"),]
                            level <- "Class-level"  
                            
                        } # eo if else loop 
                        
                    
                } else if( rank %in% c("Family","Subfamily") ) {
                    
                        if(ord %in% unique(cops.mass$Order)) {
                    
                            subset <- cops.mass[cops.mass$Order == ord,]
                            level <- "Order-level"
                    
                        } else {
                        
                            # And if 'c' is of Order-level and not even in the ScientificNames of 'cops.mass' in the first place, then use values of Calanoida+Cyclopoida+Harpacticoida 
                            subset <- cops.mass[cops.mass$Order %in% c("Calanoida","Cyclopoida","Harpacticoida"),]
                            level <- "Class-level"  
                        
                        } # eo if else loop 
                        
                } else if( rank == "Order" ) {
                    
                            if( ord %in% unique(cops.mass$Order) ) {
                    
                                subset <- cops.mass[cops.mass$Order == ord,]
                                level <- "Order-level"
                    
                            } else {
                        
                                # And if 'c' is of Order-level and not even in the ScientificNames of 'cops.mass' in the first place, then use values of Calanoida+Cyclopoida+Harpacticoida 
                                subset <- cops.mass[cops.mass$Order %in% c("Calanoida","Cyclopoida","Harpacticoida"),]
                                level <- "Class-level"  
                        
                            } # eo if else loop
                                
                } else {
                    
                    # If rank is above Genus and Family or Order, and not even in the ScientificNames of 'cops.mass' in the first place, then use values of Calanoida+Cyclopoida+Harpacticoida
                    subset <- cops.mass[cops.mass$Order %in% c("Calanoida","Cyclopoida","Harpacticoida"),]
                    level <- "Class-level"
                    
                } # eo if else loop to find subset of closest relatives
                
                
                
                ### Now that you've identified the right subset, check whether you have lifestage-specific or sex-specific measurements 
                # First, check if 'form' is of use: is it something reported in 'cops.mass'
                forms2checkagainst <- c(unique(cops.mass$LifeStage), unique(cops.mass$Sex))
                
                if( TRUE %in% str_detect(form, forms2checkagainst, negate = F) ) {
                  # 'form' contains at least element of 'forms2checkagainst', keep it that way
                  form <- form
                } else {
                    # 'form' does not contain any of the element in 'forms2checkagainst', transform it to 'NA'
                    form <- "NA"
                } # eo if else loop - is form in 'forms2checkagainst'
                
                
                # If form is not NA....
                if( form != "NA" ) {
                    
                    # Check if adult
                    if( grepl("adult", form) | grepl("male", form) | grepl("female", form) ) {
                        
                        # Subset adult measurements
                        if( TRUE %in% grepl("adult", unique(subset$LifeStage), fixed = T) ) {
                             subset2 <- subset[grepl("adult", subset$LifeStage),]
                        } else if( TRUE %in% grepl("male", unique(subset$Sex), fixed = T) ) {
                             subset2 <- subset[grepl("adult", subset$Sex),]
                        } else if( TRUE %in% grepl("female", unique(subset$Sex), fixed = T) ) {     
                            subset2 <- subset[grepl("female", subset$Sex),]
                        } else {
                            subset2 <- cops.mass[cops.mass$Order == ord,]
                            subset2 <- subset2[grepl("adult", subset2$LifeStage),]
                        } # eo if else loop 
                        
                        # And try to refine based on male/female measurements (female copepods tend to be larger and thus 'heavier')
                        if( grepl(" male", form) & "male" %in% unique(subset2$Sex) ) {
                            
                            # Subset male measurements in subset2
                            subset3 <- subset2[subset2$Sex == "male",]
                            
                            # No sex-specific measurements in subset2, just use range of adult measurements 
                            n <- nrow(subset3)
                            
                            if(n == 1) {
                                
                                min <- NA
                                max <- NA
                                mean <- subset3$MeanMass
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset3$Reference), sep = ";")
                                
                            } else if(n == 2) {
                                
                                min <- min(subset3$MeanMass)
                                max <- max(subset3$MeanMass)
                                mean <- mean(subset3$MeanMass)
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } else if(n >= 3) {
                             
                                min <- min(subset3$MeanMass)
                                max <- max(subset3$MeanMass)
                                mean <- mean(subset3$MeanMass)
                                sd <- sd(subset3$MeanMass)
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } # eo if else loop
                            
                            ### Retrun these values in a ddf
                            data2return <- data.frame(combination = c, Nobs = n,
                                Min = min, Max = max, Mean = mean, Stdev = sd, 
                                unit = "µgC.ind-1", Reference = ref, Note = paste(level," adult male values", sep = "")
                            ) # eo ddf
                                           
                        } else if( grepl("female", form) & "female" %in% unique(subset2$Sex) ) {
                            
                            # Subset female measurements in subset2
                            subset3 <- subset2[subset2$Sex == "female",]
                            
                            # No sex-specific measurements in subset2, just use range of adult measurements 
                            n <- nrow(subset3)
                            
                            if(n == 1) {
                                
                                min <- NA
                                max <- NA
                                mean <- subset3$MeanMass
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset3$Reference), sep = ";")
                                
                            } else if(n == 2) {
                                
                                min <- min(subset3$MeanMass)
                                max <- max(subset3$MeanMass)
                                mean <- mean(subset3$MeanMass)
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } else if(n >= 3) {
                             
                                min <- min(subset3$MeanMass)
                                max <- max(subset3$MeanMass)
                                mean <- mean(subset3$MeanMass)
                                sd <- sd(subset3$MeanMass)
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } # eo if else loop
                            
                            data2return <- data.frame(combination = c, Nobs = n,
                                Min = min, Max = max, Mean = mean, Stdev = sd, 
                                unit = "µgC.ind-1", Reference = ref, Note = paste(level," adult female values", sep = "")
                            ) # eo ddf
                            
                        } else {
                            
                            # No sex-specific measurements in subset2, just use range of adult measurements 
                            n <- nrow(subset2)
                            
                            if(n == 1) {
                                
                                min <- NA
                                max <- NA
                                mean <- subset2$MeanMass
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference), sep = ";")
                                
                            } else if(n == 2) {
                                
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } else if(n >= 3) {
                             
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- sd(subset2$MeanMass)
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } # eo if else loop
                            
                            data2return <- data.frame(combination = c, Nobs = n,
                                Min = min, Max = max, Mean = mean, Stdev = sd, 
                                unit = "µgC.ind-1", Reference = ref, Note = paste(level," non sex-specific adult values", sep = "")
                            ) # eo ddf
       
                        } # eo if loop based on adult sex-specific measurement
                        
                    } else if ( !grepl("adult", form) & !grepl(" male", form) & !grepl("female", form) ) {
                        
                        # Subset NON adult measurements
                        subset2 <- subset[!grepl("adult", subset$LifeStage),]
                        
                        # Go further if nauplius or copepodite lifestage is in 'subset2'

                        if( TRUE %in% grepl(form, unique(subset2$LifeStage)) ) {
                            
                            # Subset lifestage specific measurement measurements and derive Min/Max/Mean/Stdev values
                            subset2 <- subset[grepl(form, subset$LifeStage),]
                            
                            # How many mesurements? (if less than 3, cannot compute stdev, if less than 2 cannot compute Min/Max/Mean)
                            n <- nrow(subset2)
                            
                            if(n == 1) {
                                
                                min <- NA
                                max <- NA
                                mean <- subset2$MeanMass
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference), sep = ";")
                                
                            } else if(n == 2) {
                                
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- NA
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } else if(n >= 3) {
                             
                                min <- min(subset2$MeanMass)
                                max <- max(subset2$MeanMass)
                                mean <- mean(subset2$MeanMass)
                                sd <- sd(subset2$MeanMass)
                                # identify references
                                ref <- paste(unique(subset2$Reference)[1], sep = ";")
                                
                            } # eo if else loop
                            
                            data2return <- data.frame(combination = c, Nobs = n,
                                Min = min, Max = max, Mean = mean, Stdev = sd, 
                                unit = "µgC.ind-1", Reference = ref, Note = paste(form ," values of ",c, sep = "")
                            ) # eo ddf
                            
                        } else {
                            
                            # If no lifestage specific values in 'subset2', use range of copepodites or nauplius values
                            if( TRUE %in% str_detect(string = form, pattern = c("Copepodites","CI","CII","CIII","CIV","CV","CVI")) ) {
                                
                                # If 'form' indicates at least one copepodite stage, use 'copepodites.vals'
                                data2return <- data.frame(combination = c, Nobs = copepodites.vals$Nobs,
                                    Min = copepodites.vals$Min, Max = copepodites.vals$Max, Mean = copepodites.vals$Mean, Stdev = copepodites.vals$Stdev, 
                                    unit = "µgC.ind-1", Reference = NA, Note = paste("Copepodites stages values (n = 39)", sep = "")
                                ) # eo ddf
                       
                                
                            } else if( TRUE %in% str_detect(string = form, pattern = c("Nauplius","NI","NII","NIII","NIV","NV","NVI")) ) {
                                
                                # If 'form' indicates at least one nauplius stage, use 'nauplii.vals'
                                data2return <- data.frame(combination = c, Nobs = nauplii.vals$Nobs,
                                    Min = nauplii.vals$Min, Max = nauplii.vals$Max, Mean = nauplii.vals$Mean, Stdev = nauplii.vals$Stdev, 
                                    unit = "µgC.ind-1", Reference = NA, Note = paste("Nauplius stages values (n = 19)", sep = "")
                                ) # eo ddf
                                
                            } else {
                                
                                # If no 'form' is found anywhere, then return NA 
                                data2return <- data.frame(combination = c, Nobs = NA,
                                    Min = NA, Max = NA, Mean = NA, Stdev = NA, 
                                    unit = NA, Reference = NA, Note = paste("No matching values in the COPEPEDIA IndMass table", sep = "")
                                ) # eo ddf
                                
                            } # eo if else loop for copepodites.vals & nauplii.vals
                                
                      } # 
                        
                        
                    } else if( form == "NA" ) {
                    
                        # If form == "NA", it means that the observation of level 'c' is not lifestage or sex specific...so use the range of adult values in subset
                        
                        subset2 <- subset[grepl("adult", subset$LifeStage),]
                        n <- nrow(subset2)
                        
                        if(n == 1) {
                            
                            min <- NA
                            max <- NA
                            mean <- subset2$MeanMass
                            sd <- NA
                            # identify references
                            ref <- paste(unique(subset2$Reference), sep = ";")
                            
                        } else if(n == 2) {
                            
                            min <- min(subset2$MeanMass)
                            max <- max(subset2$MeanMass)
                            mean <- mean(subset2$MeanMass)
                            sd <- NA
                            # identify references
                            ref <- paste(unique(subset2$Reference)[1], sep = ";")
                            
                        } else if(n >= 3) {
                         
                            min <- min(subset2$MeanMass)
                            max <- max(subset2$MeanMass)
                            mean <- mean(subset2$MeanMass)
                            sd <- sd(subset2$MeanMass)
                            # identify references
                            ref <- paste(unique(subset2$Reference)[1], sep = ";")
                            
                        } # eo if else loop
                        
                        data2return <- data.frame(combination = c, Nobs = n,
                            Min = min, Max = max, Mean = mean, Stdev = sd, 
                            unit = "µgC.ind-1", Reference = ref, Note = paste(level, " adult values (lifestage assumed to be adult)", sep = "")
                        ) # eo ddf
                        
                } # eo if loop for 'form'
                
            } # eo 1st if else loop - if sname is in the ScientificNames of 'cops.mass'
        
        }
        
            return(data2return)
    
    } # eo fun
    
) # eo lapply - c in unique(tally$combination)


### Rbind & check
table <- dplyr::bind_rows(res)
dim(table) # 5096    9
colnames(table)
#str(table)
head(table)
summary(table)
# There were 316 combinations for which no data could be associated for Min/Max computation
head(table[is.na(table$Min),])
table[is.na(table$Min),'combination']

# Remove potential duplicates
table2 <- table %>% distinct()
dim(table2)

### Save
write.table(x = table2, file = "table_ind_c_masses_complete_Hexanauplia_08_07_22.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------