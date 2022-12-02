
##### ATLANTECO SCRIPT 7.2 ----------------------------------------------------------------------------------------------------------------------------
##### 15/06/2022: R Script to tally the ScientificNamexTaxonRankxLifeForm combinations of each zooplankton PFT from the AtlantECO-BASE1 abundance datasets ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load abundance datasets for zooplankton PFTs
# - Tally ScientificName x TaxonRank x LifeForm combinations 
# - Make some plots
# - Output excel sheet

### Latest update: 06/07/2022

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("lubridate")
library("viridis")

world <- map_data("world")  # for maps

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/") # When on kryo
WD <- getwd() # Main dir
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance")
# dir()

### ----------------------------------------------------------------------------------------------------------------------------

### List the groups of interest, from smaller to larger
# Appendicularia (Larvacea)
# Chaetognatha
# Jellyfish (Cnidaria+Ctenophora)
# Euphausiacea (krill)
# Copepoda (Hexanauplia)

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Appendicularia
app <- get(load("AtlantECO-BASEv1_dataset_Appendicularia_abundances_28_04_22.RData"))
dim(app) # 218'159 obs
colnames(app)
# Check ScientificName, WoRMS_ID, WoRMS_status, TaxonRank & LifeForm
unique(app$ScientificName) # summary(factor(app$ScientificName))
unique(app$WoRMS_ID) # No NA, all good
unique(app$WoRMS_status) # OK
unique(app$TaxonRank)
unique(app$LifeForm) # summary(factor(app$LifeForm))
# 98.7% of the obs have NA in LifeForm. Life stage-specific or Size-based conversion to biomass seem irrelevant then

tall.app <- data.frame(app %>% group_by(WoRMS_ID,Class,Order,Family,Genus,ScientificName,TaxonRank,WoRMS_status) %>% summarize(n = n()) )
dim(tall.app) # 15 rows
tall.app <- tall.app[order(tall.app$n, decreasing = T),]
# Add % after n
tall.app <- add_column(tall.app, perc. = tall.app$n/sum(tall.app$n), .after = "n")

# keep for later
tall.app$LifeForm <- NA
tall.app$Group <- "Appendicularia"

### ----------------------------------------------------------------------------------------------------------------------------

### 2°) Chaetognatha
chaeto <- get(load("AtlantECO-BASEv1_dataset_Chaetognatha_abundances_28_04_22.RData"))
dim(chaeto) # 383'151
colnames(chaeto)
# Check ScientificName, WoRMS_ID, WoRMS_status, TaxonRank & LifeForm
unique(chaeto$ScientificName) # summary(factor(chaeto$ScientificName))
unique(chaeto$WoRMS_ID) # No NA, all good
unique(chaeto$WoRMS_status) # OK
unique(chaeto$TaxonRank) # summary(factor(chaeto$TaxonRank))
unique(chaeto$LifeForm) # summary(factor(chaeto$LifeForm))
# 98.6% of the obs have NA in LifeForm. Life stage-specific or Size-based conversion to biomass seem irrelevant then

tall.chaeto <- data.frame(chaeto %>% group_by(WoRMS_ID,Class,Order,Family,Genus,ScientificName,TaxonRank,WoRMS_status) %>% summarize(n = n()) )
dim(tall.chaeto)
tall.chaeto <- tall.chaeto[order(tall.chaeto$n, decreasing = T),]
# Add % after n
tall.chaeto <- add_column(tall.chaeto, perc. = tall.chaeto$n/sum(tall.chaeto$n), .after = "n")

# keep for later
tall.chaeto$LifeForm <- NA
tall.chaeto$Group <- "Chaetognatha"

### ----------------------------------------------------------------------------------------------------------------------------

### 3°) Euphausiacea
kri <- get(load("AtlantECO-BASEv1_dataset_Euphausiacea_abundances_13_04_22.RData"))
dim(kri) # 1'071'450
colnames(kri)
# Check ScientificName, WoRMS_ID, WoRMS_status, TaxonRank & LifeForm
unique(kri$ScientificName) # summary(factor(kri$ScientificName))
unique(kri$WoRMS_ID) # No NA, all good
unique(kri$WoRMS_status) # OK
unique(kri$TaxonRank) # summary(factor(kri$TaxonRank))
unique(kri$LifeForm) # summary(factor(kri$LifeForm))
# 83.7% of the obs have NA in LifeForm. Life stage-specific or size-based conversion to biomass seem important here! 

# Check which lifeform correspond to KRILLBASE
unique(kri$BiblioCitation)
unique(kri[kri$BiblioCitation == "Atkinson et al. (2017). KRILLBASE: a circumpolar database of Antarctic krill and salp numerical densities, 1926–2016. Earth Syst. Sci. Data, 9(1), 193-210.","LifeForm"])
# "Postlarval krill, >19mm"
dim(kri[kri$BiblioCitation == "Atkinson et al. (2017). KRILLBASE: a circumpolar database of Antarctic krill and salp numerical densities, 1926–2016. Earth Syst. Sci. Data, 9(1), 193-210.",])
# 11387 == n lines with LifeForm == "Postlarval krill, >19mm" in 'kri'
# For the rawKRILLBASE database the mean body mass of Euphausia superba was estimated as 140 mg dry mass ind.−1 (A. Atkinson, personal communication, 2007) and was calculated from a length frequency distribution dataset containing 535 581 length measurements of Euphausia superba recovered from scientific hauls between October and April from 1926–1939 and 1976–2006 (Atkinson et al., 2009). A dry mass to carbon conversion of 0.45 (Pakhomov et al., 2002 and references therein) was used to convert dry mass to carbon per individual. 
# --> For KRILLBASE data, multiply #/m3 by (140*0.45=) 63 mgC/ind to obtain mean biomass of krill in mgC/m3

tall.kri <- data.frame(kri %>% group_by(WoRMS_ID,Class,Order,Family,Genus,ScientificName,TaxonRank,WoRMS_status,LifeForm) %>% summarize(n = n()) )
dim(tall.kri) # 267 here
tall.kri <- tall.kri[order(tall.kri$n, decreasing = T),]
# Add % after n
tall.kri <- add_column(tall.kri, perc. = tall.kri$n/sum(tall.kri$n), .after = "n")

# keep for later
tall.kri$Group <- "Euphausiacea"

### ----------------------------------------------------------------------------------------------------------------------------

### 4°) Jellyfish
jelly <- get(load("AtlantECO-BASEv1_dataset_Jellyfish_abundances_22_04_22.RData"))
dim(jelly) # 590'371
colnames(jelly)
# Check ScientificName, WoRMS_ID, WoRMS_status, TaxonRank & LifeForm
unique(jelly$ScientificName) # summary(factor(jelly$ScientificName))
unique(jelly$WoRMS_ID) # 5712 "No match found in WoRMS"
# unique(jelly[jelly$WoRMS_ID == "No match found in WoRMS","ScientificName"]) # "Jellyfishes"
unique(jelly$WoRMS_status) # OK
unique(jelly$TaxonRank) # summary(factor(jelly$TaxonRank))
unique(jelly$LifeForm) # summary(factor(jelly$LifeForm))
# 87.8% of the obs have NA in LifeForm. 8?7% say 'Nectophore' = The swimming bell in (the colony of) siphonophores
# Life stage-specific or size-based conversion to biomass seem important here! 

tall.jelly <- data.frame(jelly %>% group_by(WoRMS_ID,Class,Order,Family,Genus,ScientificName,TaxonRank,WoRMS_status,LifeForm) %>% summarize(n = n()) )
dim(tall.jelly) # 218 here
tall.jelly <- tall.jelly[order(tall.jelly$n, decreasing = T),]
# Add % after n
tall.jelly <- add_column(tall.jelly, perc. = tall.jelly$n/sum(tall.jelly$n), .after = "n")

# keep for later
tall.jelly$Group <- "Jellyfish"

### ----------------------------------------------------------------------------------------------------------------------------

### 5°) Copepoda
cops <- get(load("AtlantECO-BASEv1_dataset_Hexanauplia_abundances_12_04_22.RData"))
dim(cops) # 9'080'989
colnames(cops)
# Check ScientificName, WoRMS_ID, WoRMS_status, TaxonRank & LifeForm
unique(cops$ScientificName) # summary(factor(cops$ScientificName))
unique(cops$WoRMS_ID) # No NA, good
unique(cops$WoRMS_status) # some 'isBrackish'
unique(cops$TaxonRank) # summary(factor(cops$TaxonRank))
unique(cops$LifeForm) # summary(factor(cops$LifeForm))
# 76.7% of the obs have NA in LifeForm. >16% just say Male or Female
# Life stage-specific or size-based conversion to biomass seem important here! 

tall.cops <- data.frame(cops %>% group_by(WoRMS_ID,Class,Order,Family,Genus,ScientificName,TaxonRank,WoRMS_status,LifeForm) %>% summarize(n = n()) )
dim(tall.cops) # 5324
tall.cops <- tall.cops[order(tall.cops$n, decreasing = T),]

# Add % after n
tall.cops <- add_column(tall.cops, perc. = tall.cops$n/sum(tall.cops$n), .after = "n")
tall.cops$Group <- "Copepoda"


### Rbind all tally datasets 
all.tally <- rbind(tall.app,tall.chaeto,tall.kri,tall.jelly,tall.cops)
dim(all.tally) # 5856
(summary(factor(all.tally$Group))/5856)*100
(summary(factor(all.tally$TaxonRank))/5856)*100

# Print
setwd(WD)
write.table(all.tally, "table_tally_ScientificNames_abund_zooplankton_20_06_22.txt", sep = "\t")


### 06/07/22: Re-tally copepod ScientificNames and LifeForms after you modified them on the abundance datasets so they match the values in COPEPEDIA tables  
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
cops <- get(load("AtlantECO-BASEv1_dataset_Hexanauplia_abundances_05_07_22.RData"))
dim(cops) # 9'080'989
colnames(cops)

tall.cops <- data.frame(cops %>% group_by(WoRMS_ID,Class,Order,Family,Genus,ScientificName,TaxonRank,WoRMS_status,LifeForm) %>% summarize(n = n()) )
dim(tall.cops) # 5324
tall.cops <- tall.cops[order(tall.cops$n, decreasing = T),]

# Add % after n
tall.cops <- add_column(tall.cops, perc. = tall.cops$n/sum(tall.cops$n), .after = "n")
tall.cops$Group <- "Copepoda"
head(tall.cops)
summary(tall.cops)

# Print
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
write.table(tall.cops, "table_tally_ScientificNames_abund_Hexanauplia_06_07_22.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------