
##### ATLANTECO SCRIPT 6.1 ----------------------------------------------------------------------------------------------------------------------------
# ©Fabio Benedetti, ETH Zürich, IBP, UP Group.
##### 13/12/2021: R Script to:  

# - Examine distribution of data per sampling parameters (e.g., mesh size, depths, tow type etc.), within 1x1 monthly cells (sites) for Copepods (Hexanauplia)
# - Examine distribution of data per taxonomic levels (e.g., TaxonRank and main ScientificNames covered), within 1x1 monthly cells (sites)
# - Decide on aggregation levels (200m depth integral, separate per tow types?)

# - 13/12/21: For J. of Bio. manuscript (Benedetti, Wydler & Vogt, in prep.), examine abundance data distribution across the 11 FGs defined in the study

### Latest update: 12/01/22

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("lubridate")
library("viridis")

world <- map_data("world")  # for maps

substrRight <- function(x,n) { substr(x, nchar(x)-n+1, nchar(x)) }

### ----------------------------------------------------------------------------------------------------------------------------

### 13/12/2021: Part 1°) is dedicated to the copepod abundances data for J. of Bio. manuscript (Discussions on PFGs importance)

### 1°) Load and examine the Hexanauplia abund data
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/")
data <- get(load("AtlantECO_WP2_Hexanauplia_data_COPEPOD+CPRs+Cornils.2018_16_12_21.Rdata"))
dim(data) # 12'390'410  57
str(data)
colnames(data)
summary(data$MeasurementValue) # 99833 NA
data <- data[!is.na(data$MeasurementValue),]

# Check ScientificNames
data %>% count(ScientificName, sort = T)
data %>% count(SamplingProtocol, sort = T) # unique(data$SamplingProtocol)
data %>% count(Dataset, sort = T)

# Inform mesh size and tow type as a suppl. variable (important for modelling and aggregating data), from 
data$MeshSize <- NA
data$TowType <- NA
data[data$Dataset %in% c("SO-CPR","AusCPR","CPR"),"MeshSize"] <- 270      
data[data$Dataset %in% c("SO-CPR","AusCPR","CPR"),"TowType"] <- "horizontal"      
# Use SamplingProtocol to derive mesh size and tow type for those obs of dataset != c("SO-CPR","AusCPR","CPR")
# unique(data$SamplingProtocol)
# unique( data[!(data$Dataset %in% c("SO-CPR","AusCPR","CPR")),"SamplingProtocol"] )
# substrRight(x = unique(data[!(data$Dataset %in% c("SO-CPR","AusCPR","CPR")),"SamplingProtocol"]), 4) # Good start for mesh size
# test.types <- do.call(rbind, strsplit(unique(data[!(data$Dataset %in% c("SO-CPR","AusCPR","CPR")),"SamplingProtocol"]), split = ";"))[,1]
# word(test.types,-1) # Works ! 

# Inform mesh size for NMFS COPEPOD datasets
data[!(data$Dataset %in% c("SO-CPR","AusCPR","CPR","Cornils&al.2018")),"MeshSize"] <- substrRight(x = data[!(data$Dataset %in% c("SO-CPR","AusCPR","CPR","Cornils&al.2018")),"SamplingProtocol"], 4)
unique(data$MeshSize) 
# Remove "= " and trimsw
data$MeshSize <- str_replace_all(data$MeshSize,"= ","")
data$MeshSize <- trimws(data$MeshSize, "both")
# Remove "-999"
data[data$MeshSize == "-999","MeshSize"] <- NA

### 16/12/21: Inform MeshSize and TowType for Cornils&al.2018. Go back to the actual paper to properly inform methodology
# unique(data[data$Dataset == "Cornils&al.2018","basisOfRecord"])
# unique(data[data$Dataset == "Cornils&al.2018","SamplingProtocol"])
data[data$SamplingProtocol == "Nansen net, 250 \xe5\xb5m" & !is.na(data$SamplingProtocol),"MeshSize"] <- "250"
data[data$SamplingProtocol == "Nansen net, 250 \xe5\xb5m" & !is.na(data$SamplingProtocol),"TowType"] <- "vertical"
data[data$SamplingProtocol == "Nansen net 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"MeshSize"] <- "200"
data[data$SamplingProtocol == "Nansen net 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"TowType"] <- "vertical"
data[data$SamplingProtocol == "net 1, Nansen 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"MeshSize"] <- "200"
data[data$SamplingProtocol == "net 1, Nansen 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"TowType"] <- "vertical"
data[data$SamplingProtocol == "net 2, Nansen 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"MeshSize"] <- "200"
data[data$SamplingProtocol == "net 2, Nansen 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"TowType"] <- "vertical"
data[data$SamplingProtocol == "net 3, Nansen 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"MeshSize"] <- "200"
data[data$SamplingProtocol == "net 3, Nansen 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"TowType"] <- "vertical"
data[data$SamplingProtocol == "net 4, Nansen 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"MeshSize"] <- "200"
data[data$SamplingProtocol == "net 4, Nansen 200 \xe5\xb5m" & !is.na(data$SamplingProtocol),"TowType"] <- "vertical"
data[data$SamplingProtocol == "Bongo, 300 \xe5\xb5m" & !is.na(data$SamplingProtocol),"MeshSize"] <- "300"
data[data$SamplingProtocol == "Bongo, 300 \xe5\xb5m" & !is.na(data$SamplingProtocol),"TowType"] <- "vertical"
data[data$SamplingProtocol == "Bongo, 500 \xe5\xb5m" & !is.na(data$SamplingProtocol),"MeshSize"] <- "500"
data[data$SamplingProtocol == "Bongo, 500 \xe5\xb5m" & !is.na(data$SamplingProtocol),"TowType"] <- "vertical"
# split char string for MN samples? 
# test.types <- do.call(rbind, strsplit(unique(data[data$Dataset == "Cornils&al.2018" & is.na(data$MeshSize),"SamplingProtocol"]), split = " "))
# Or use grepl instead? grepl(value, chars, fixed = TRUE)
# unique(data[data$Dataset == "Cornils&al.2018" & is.na(data$MeshSize),"SamplingProtocol"])
# head( data[grepl("MN midi 55", data$SamplingProtocol, fixed = T),] ) # this seems to work
data[grepl("MN midi 200", data$SamplingProtocol, fixed = T),"MeshSize"] <- 200
data[grepl("MN midi 100", data$SamplingProtocol, fixed = T),"MeshSize"] <- 100
data[grepl("MN midi 55", data$SamplingProtocol, fixed = T),"MeshSize"] <- 55
data[grepl("MN midi, 100", data$SamplingProtocol, fixed = T),"MeshSize"] <- 100
# unique(data[data$Dataset == "Cornils&al.2018" & is.na(data$MeshSize),"SamplingProtocol"]) # should return 0 integer

# Convert to numeric
data$MeshSize <- as.numeric(data$MeshSize)
summary(data$MeshSize)
data[data$MeshSize == "-999","MeshSize"] <- NA
# Remove samples with NA mesh size
data <- data[!is.na(data$MeshSize),]
summary(data$MeshSize)
data %>% count(MeshSize, sort = T)

# Inform tow type
types <- do.call(rbind, strsplit(data[!(data$Dataset %in% c("SO-CPR","AusCPR","CPR","Cornils&al.2018")),"SamplingProtocol"], split = ";"))[,1] ; length(types)
# word(types,-1) # unique(word(types,-1))
data[!(data$Dataset %in% c("SO-CPR","AusCPR","CPR","Cornils&al.2018")),"TowType"] <- word(types,-1) 
data[data$TowType == "NA" & !is.na(data$TowType),"TowType"] <- NA
# unique(data$TowType)
data %>% count(TowType, sort = T)
 
# Add an id for sites (1x1 monthly cells) 
data$x_1d <- round(data$decimalLongitude, 0.1) # unique(data$x_1d)
data$y_1d <- round(data$decimalLatitude, 0.1) # unique(data$y_1d)
data$cell_id <- paste(data$x_1d,data$y_1d,data$Month, sep = "_")
# length(unique(data$cell_id))

# Add an id for sampling basis (TowTypexMeshSize)
data$Sampling_id <- paste(data$MeshSize, data$TowType, sep = "_")
#unique(data$Sampling_id)
data %>% count(Sampling_id, sort = T)

# Examine depth data again
summary(data[,c("Depth","MinDepth","MaxDepth")])
summary(data[data$Dataset == "Cornils&al.2018",c("Depth","MinDepth","MaxDepth")])
# Examine some random rows
# data[1202966:1202986,c("Depth","MinDepth","MaxDepth")]
data <- data[!is.na(data$MaxDepth),]
data <- data[!is.na(data$MinDepth),]
### Retain those obs whose MaxDepth <= 250 
data <- data[data$MaxDepth <= 250,]
dim(data) # 12127602  

# Examine TaxonRank
data %>% count(TaxonRank, sort = T)
data[data$TaxonRank == "Family",] %>% count(ScientificName, sort = T)
data[data$TaxonRank == "Subclass",] %>% count(ScientificName, sort = T)

# Check Cornils data
# unique(data[data$Dataset == "Cornils&al.2018",c("Family","Order","Genus","Species")])

# Remove obs at Family/Order/Subclass/Suborder/Subfamily (tto broad to be associated one of the 11 FGs of Benedetti et al.)
data <- data[!is.na(data$TaxonRank),]
data <- data[!(data$TaxonRank %in% c("Family","Order","Subclass","Suborder","Subfamily")),]
dim(data) # 11523148

# Examine distrib of abund data per datasets
summary(data[data$Dataset == "NMFS-COPEPOD","MeasurementValue"]) # includes zeros
summary(data[data$Dataset == "AusCPR","MeasurementValue"]) # lots of zeroes
summary(data[data$Dataset == "SO-CPR","MeasurementValue"]) # lots of zeroes
summary(data[data$Dataset == "Cornils&al.2018","MeasurementValue"]) # zeroes present
summary(data[data$Dataset == "CPR","MeasurementValue"]) # No zeroes though...meaning average values per sites will be biased towards higher values
data %>% count(Dataset, sort = T)
data2 <- data[data$MeasurementValue != 0,]
# dim(data2) # 1188250/11523148 # 10.3% of the data were retained
data2 %>% count(Dataset, sort = T)


### 14/12/21: Id ScientificNames to be associated with one of the 11 copepod FGs
### Load the species_FGs table
groups <- get(load("Copepod_species_FG_Benedetti_et_al_14_12_21.Rdata"))
# dim(groups); head(groups)
groups$Species <- str_replace_all(groups$Species, "_", " ")
require("worms")
groups[groups$Species == "Acartia (Odontacartia) lilljeborgi","Species"] <- "Acartia (Odontacartia) lilljeborgii"
keys.worms <- wormsbynames(groups$Species, marine_only = "false")
keys.worms$SpeciesName <- groups$Species
# head(keys.worms)
keys.worms$valid_AphiaID # no missing values good

# Add AphiaID and taxonoic classif from worms to 'groups'
groups <-  add_column(groups, Genus = NA, .before = "Species")
groups <-  add_column(groups, Family = NA, .before = "Genus")
groups <-  add_column(groups, Order = NA, .before = "Family")
# groups <-  add_column(groups, Superorder = NA, .before = "Order") # not available in key.worms ...
groups <-  add_column(groups, Class = NA, .before = "Order")
groups <-  add_column(groups, AphiaID = NA, .before = "Class")
# head(groups)

detach("package:worms", unload = TRUE)
detach("package:marmap", unload = TRUE)
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
require("dplyr")

for(sp in groups$Species) {
    message(paste(sp, sep = ""))
    groups[groups$Species == sp,c("AphiaID","Class","Order","Family","Genus")] <- keys.worms[keys.worms$SpeciesName == sp,c("valid_AphiaID","class","order","family","genus")]
} # eo for loop - sp in groups$Species
# Check
groups # OK, check taxonomic content of groups

### Wrong numbers compared to the manuscript
groups$FG <- NA
groups[groups$FG_f11 == 1,"FG"] <- "FG8"
groups[groups$FG_f11 == 2,"FG"] <- "FG3"
groups[groups$FG_f11 == 3,"FG"] <- "FG5"
groups[groups$FG_f11 == 4,"FG"] <- "FG2"
groups[groups$FG_f11 == 5,"FG"] <- "FG9"
groups[groups$FG_f11 == 6,"FG"] <- "FG4"
groups[groups$FG_f11 == 7,"FG"] <- "FG10"
groups[groups$FG_f11 == 8,"FG"] <- "FG1"
groups[groups$FG_f11 == 9,"FG"] <- "FG7"
groups[groups$FG_f11 == 10,"FG"] <- "FG6"
groups[groups$FG_f11 == 11,"FG"] <- "FG11"
# groups[groups$FG == "FG1","Species"]
summary(factor(groups[groups$FG == "FG1","Genus"])) # Clausocalanus + Macrosetella gracilis
summary(factor(groups[groups$FG == "FG2","Genus"])) # Oncaeidae + Euterpina acutifrons + Monothula subtilis 
summary(factor(groups[groups$FG == "FG3","Genus"])) # diverse
summary(factor(groups[groups$FG == "FG4","Genus"])) # Agetus+Corycaeus+
summary(factor(groups[groups$FG == "FG5","Genus"])) # Copilia     Euchaeta Paraeuchaeta   Sapphirina  Undeuchaeta 
summary(factor(groups[groups$FG == "FG6","Genus"])) # 
summary(factor(groups[groups$FG == "FG7","Genus"])) # 
# !!! overlapping genera between FG6 and FG7
summary(factor(groups[groups$FG == "FG8","Genus"])) # diverse
summary(factor(groups[groups$FG == "FG9","Genus"])) # 
summary(factor(groups[groups$FG == "FG10","Genus"])) # Oithonidae only
summary(factor(groups[groups$FG == "FG11","Genus"])) # Acartia Centropages Isias

### 14/12/21: How to assign copepod FGs in "data"
# Clausocalanus + Macrosetella gracilis                     --> FG1
# Oncaeidae + Euterpina acutifrons + Monothula subtilis     --> FG2
# Corycaeidae + Vettoria                                    --> FG4
# Copilia+Euchaeta+Paraeuchaeta+Sapphirina+Undeuchaeta      --> FG5
# Oithonidae                                                --> FG10
# Acartia Centropages Isias                                 --> FG11
# Use AphiaIDs for FGs: 3,6,7,8,9

### And then check the obs that are left in 'data2' without a FG associated 
data2$FG <- NA
# g <- "FG3" # for testing
for(g in c("FG3","FG6","FG7","FG8","FG9")) {
    message(paste("Attributing groups for AphiaIDs of ",g, sep = ""))
    ids <- groups[groups$FG == g,"AphiaID"] # ids
    data2[data2$WoRMS_ID %in% ids & !is.na(data2$WoRMS_ID),"FG"] <- g
} # eo for loop

# FG1
data2[data2$Genus == "Clausocalanus" & !is.na(data2$Genus),"FG"] <- "FG1"
data2[data2$Species == "Macrosetella gracilis" & !is.na(data2$Species),"FG"] <- "FG1"
# FG2
data2[data2$Family == "Oncaeidae" & !is.na(data2$Family),"FG"] <- "FG2"
data2[data2$Species == "Euterpina acutifrons" & !is.na(data2$Species),"FG"] <- "FG2"
data2[data2$Species == "Monothula subtilis" & !is.na(data2$Species),"FG"] <- "FG2"
# FG4
data2[data2$Family == "Corycaeidae" & !is.na(data2$Family),"FG"] <- "FG4"
data2[data2$Genus == "Vettoria" & !is.na(data2$Genus),"FG"] <- "FG4"
# FG5
data2[data2$Genus %in% c("Copilia","Euchaeta","Paraeuchaeta","Sapphirina","Undeuchaeta") & !is.na(data2$Genus),"FG"] <- "FG5"
# FG10
data2[data2$Family == "Oithonidae" & !is.na(data2$Family),"FG"] <- "FG10"
# FG11
data2[data2$Genus %in% c("Acartia","Centropages","Isias") & !is.na(data2$Genus),"FG"] <- "FG11"

### Check
data2 %>% count(FG, sort = T) # 153724 NA to complete?
unique(data2[is.na(data2$FG),"ScientificName"])

# Sort categories to attribute
data2[is.na(data2$FG),] %>% count(Family, sort = T) # Too broad !
data2[is.na(data2$FG),] %>% count(Genus, sort = T) 
#                 Genus      n
# 1                <NA> 124438
### Has to remain NA

# 2             Calanus  63222 --> could be FG 6 OR 7
groups[groups$Genus == "Calanus",] %>% count(FG, sort = T) # 6 OR 7 Depends on location! Map those
# data2[data2$Dataset == "CPR",] %>% count(ScientificName, sort = T) 

# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
#         data = data2[data2$ScientificName == "Calanus",], alpha = 0.75) +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") + scale_colour_discrete(name = "Data source") +
#     theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
#         panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom")
#
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
#         data = data2[is.na(data2$FG) & data2$ScientificName == "Calanus",], alpha = 0.75) +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") + scale_colour_discrete(name = "Data source") +
#     theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
#         panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom")

# HAs to remain NA...too hard to decide !!!

# 3        Ctenocalanus   9697
groups[groups$Genus == "Ctenocalanus",] %>% count(FG, sort = T) # FG7
data2[is.na(data2$FG) & data2$Genus == "Ctenocalanus","FG"] <- "FG7"
# 4         Pleuromamma   8376
groups[groups$Genus == "Pleuromamma",] %>% count(FG, sort = T) # FG9
data2[is.na(data2$FG) & data2$Genus == "Pleuromamma","FG"] <- "FG9"
# 5         Calocalanus   7628
groups[groups$Genus == "Calocalanus",] %>% count(FG, sort = T) # FG7 
data2[is.na(data2$FG) & data2$Genus == "Calocalanus","FG"] <- "FG7"
# 6          Eurytemora   5292
### Not covered by study - Has to remain NA
# 7            Metridia   4873
groups[groups$Genus == "Metridia",] %>% count(FG, sort = T) # FG3
data2[is.na(data2$FG) & data2$Genus == "Metridia","FG"] <- "FG3"
# 8            Candacia   4717
groups[groups$Genus == "Candacia",] %>% count(FG, sort = T) # FG8
data2[is.na(data2$FG) & data2$Genus == "Candacia","FG"] <- "FG8"
# 9           Eucalanus   4385
groups[groups$Genus == "Eucalanus",] %>% count(FG, sort = T) # FG6
data2[is.na(data2$FG) & data2$Genus == "Eucalanus","FG"] <- "FG6"
# 10        Paracalanus   4115
groups[groups$Genus == "Paracalanus",] %>% count(FG, sort = T) # FG7
data2[is.na(data2$FG) & data2$Genus == "Paracalanus","FG"] <- "FG7"
# 11   Scolecithricella   2786
groups[groups$Genus == "Scolecithricella",] %>% count(FG, sort = T) # FG3
data2[is.na(data2$FG) & data2$Genus == "Scolecithricella","FG"] <- "FG3"
# 12         Euchirella   2063
groups[groups$Genus == "Euchirella",] %>% count(FG, sort = T) # FG3
data2[is.na(data2$FG) & data2$Genus == "Euchirella","FG"] <- "FG3"
# 13       Parvocalanus   2052
groups[groups$Genus == "Parvocalanus",] %>% count(FG, sort = T) # FG7
data2[is.na(data2$FG) & data2$Genus == "Parvocalanus","FG"] <- "FG7"
# 14        Acrocalanus   1961
groups[groups$Genus == "Acrocalanus",] %>% count(FG, sort = T) # FG7
data2[is.na(data2$FG) & data2$Genus == "Acrocalanus","FG"] <- "FG7"
# 15         Drepanopus   1942
#groups[groups$Genus == "Drepanopus",] %>% count(FG, sort = T)
### Has to remain NA
# 16       Limnocalanus   1893
#groups[groups$Genus == "Limnocalanus",] %>% count(FG, sort = T) 
### Has to remain NA
# 17          Lucicutia   1701
groups[groups$Genus == "Lucicutia",] %>% count(FG, sort = T) # FG9
data2[is.na(data2$FG) & data2$Genus == "Lucicutia","FG"] <- "FG9"
# 18            Cyclops   1518
### Not covered by study - Has to remain NA
# 19       Clytemnestra   1467
#groups[groups$Genus == "Clytemnestra",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 20             Temora   1369
#groups[groups$Genus == "Temora",] %>% count(FG, sort = T) 
### Has to remain NA
# 21       Microsetella   1359
groups[groups$Genus == "Microsetella",] %>% count(FG, sort = T) # FG3
data2[is.na(data2$FG) & data2$Genus == "Microsetella","FG"] <- "FG3"
# 22      Heterorhabdus   1314
groups[groups$Genus == "Heterorhabdus",] %>% count(FG, sort = T) # FG8
data2[is.na(data2$FG) & data2$Genus == "Heterorhabdus","FG"] <- "FG8"
# 23      Pseudocalanus   1305
groups[groups$Genus == "Pseudocalanus",] %>% count(FG, sort = T) # FG7
data2[is.na(data2$FG) & data2$Genus == "Pseudocalanus","FG"] <- "FG7"
# 24         Haloptilus   1141
groups[groups$Genus == "Haloptilus",] %>% count(FG, sort = T) # FG8
data2[is.na(data2$FG) & data2$Genus == "Haloptilus","FG"] <- "FG8"
# 25       Subeucalanus    942
groups[groups$Genus == "Subeucalanus",] %>% count(FG, sort = T) # FG7
data2[is.na(data2$FG) & data2$Genus == "Subeucalanus","FG"] <- "FG7"
# 26          Lubbockia    914
#groups[groups$Genus == "Lubbockia",] %>% count(FG, sort = T) # 
### Not covered by study - Has to remain NA
# 27         Labidocera    775
groups[groups$Genus == "Labidocera",] %>% count(FG, sort = T) # FG9
data2[is.na(data2$FG) & data2$Genus == "Labidocera","FG"] <- "FG9"
# 28      Scaphocalanus    720
groups[groups$Genus == "Scaphocalanus",] %>% count(FG, sort = T) # FG3
data2[is.na(data2$FG) & data2$Genus == "Scaphocalanus","FG"] <- "FG3"
# 29            Miracia    632
#groups[groups$Genus == "Miracia",] %>% count(FG, sort = T) 
### Has to remain NA
# 30          Temoropia    613
#groups[groups$Genus == "Temoropia",] %>% count(FG, sort = T) 
### Has to remain NA
# 31           Aetideus    429
groups[groups$Genus == "Aetideus",] %>% count(FG, sort = T) # FG8
data2[is.na(data2$FG) & data2$Genus == "Aetideus","FG"] <- "FG8"
# 32       Scolecithrix    369
groups[groups$Genus == "Scolecithrix",] %>% count(FG, sort = T) # FG3 
data2[is.na(data2$FG) & data2$Genus == "Scolecithrix","FG"] <- "FG3"
# 33         Mormonilla    363
#groups[groups$Genus == "Mormonilla",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 34       Microcalanus    343
groups[groups$Genus == "Microcalanus",] %>% count(FG, sort = T) # FG9
data2[is.na(data2$FG) & data2$Genus == "Microcalanus","FG"] <- "FG9"
# 35    Pseudodiaptomus    315
#groups[groups$Genus == "Pseudodiaptomus",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 36        Rhincalanus    311
#groups[groups$Genus == "Rhincalanus",] %>% count(FG, sort = T) 
# Has to remain NA
# 37      Scottocalanus    262
#groups[groups$Genus == "Scottocalanus",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 38         Mecynocera    259
groups[groups$Genus == "Mecynocera",] %>% count(FG, sort = T) # FG7
data2[is.na(data2$FG) & data2$Genus == "Mecynocera","FG"] <- "FG7"
# 39         Calanoides    252
#groups[groups$Genus == "Calanoides",] %>% count(FG, sort = T) 
# Has to remain NA
# 40            Phaenna    236
#groups[groups$Genus == "Phaenna",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 41  Scolecitrichopsis    203
#groups[groups$Genus == "Scolecitrichopsis",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 42           Pontella    175
#groups[groups$Genus == "Pontella",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 43        Distioculus    167
#groups[groups$Genus == "Distioculus",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 44        Amallothrix    165
groups[groups$Genus == "Amallothrix",] %>% count(FG, sort = T) # FG3
data2[is.na(data2$FG) & data2$Genus == "Amallothrix","FG"] <- "FG3"

# 45         Neocalanus    163
# groups[groups$Genus == "Neocalanus",] %>% count(FG, sort = T) # IDK, like Calanus, need to map
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#          data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#      geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
#          data = data2[is.na(data2$FG) & data2$ScientificName == "Neocalanus",], alpha = 0.75) +
#      coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") + scale_colour_discrete(name = "Data source") +
#      theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
#          panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom")
#
### The AusCPR Neocalanus sp. is likely N. tonsus (https://obis.org/taxon/344701; see also maps in https://copepodes.obs-banyuls.fr/en/nom.php?nom=Neocalanus&Submit=Send) --> FG7

data2[is.na(data2$FG) & data2$Genus == "Neocalanus" & data2$Dataset == "AusCPR","FG"] <- "FG7"

# 46         Undinopsis    154
groups[groups$Genus == "Undinopsis",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 47        Harpacticus    146
groups[groups$Genus == "Harpacticus",] %>% count(FG, sort = T) 
### Not covered by study - Has to remain NA
# 48       Spinocalanus    144
groups[groups$Genus == "Spinocalanus",] %>% count(FG, sort = T) # FG3
data2[is.na(data2$FG) & data2$Genus == "Spinocalanus","FG"] <- "FG3"
# 49           Tortanus    127
groups[groups$Genus == "Tortanus",] %>% count(FG, sort = T) # FG3
data2[is.na(data2$FG) & data2$Genus == "Tortanus","FG"] <- "FG3"
# 50           Gaetanus    121
groups[groups$Genus == "Gaetanus",] %>% count(FG, sort = T) # FG9
data2[is.na(data2$FG) & data2$Genus == "Gaetanus","FG"] <- "FG9"
# 51          Calanopia    109
groups[groups$Genus == "Calanopia",] %>% count(FG, sort = T) # FG9
data2[is.na(data2$FG) & data2$Genus == "Calanopia","FG"] <- "FG9"

### Check status of FG in Cornils et al. 2018 data?
unique(data2[data2$Dataset == "Cornils&al.2018","FG"]) # 9 FGs detected
unique(data2[is.na(data2$FG) & data2$Dataset == "Cornils&al.2018","Genus"]) # Taxa not covered by FGs - can discard


### Check again and compare to the orginal 147395 NA
data2 %>% count(FG, sort = T) # 85081 NAs now
data2[is.na(data2$FG),] %>% count(ScientificName, sort = T) 
# Main problem remains 'Calanus' and then: Eurytemora, Limnocalanus macrurus, Drepanopus pectinatus, Cyclops and Temora

# Subset
data3 <- data2[!is.na(data2$FG),]
# dim(data3) # Map sampling effort?
data3$cell_id2 <- paste(data3$x_1d, data3$y_1d, sep = "_")

spatial.effort <- data.frame(data3 %>% group_by(cell_id2) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
# dim(spatial.effort) ; summary(spatial.effort) # 6939 grid cells
ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 
# colnames(data3)
data3 %>% count(Sampling_id, sort = T) # 70% is CPR, which is not that much after all :-D
summary(data3$MeshSize)

### To evaluate how many spatial points (i.e. grid cells) are covered by each SamplingID
contribs <- data.frame(data3 %>% group_by(Sampling_id) %>% summarize(n = length(unique(cell_id))))
contribs[order(contribs$n, decreasing = T),]
sum(contribs$n)
# Check some maps of sampling ids
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = data3[data3$Sampling_id == "168_NA",]) +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")

### Keep all, check distribution of abund per FG
ggplot(data = data3, aes(x = factor(FG), y = MeasurementValue, fill = factor(FG))) + geom_boxplot(colour = "black") + theme_minimal()
### And without outliers?
m <- mean(data3$MeasurementValue)
sd <- sd(data3$MeasurementValue)
out.u <- m+2*sd
# no.out <- subset(data3, data3$MeasurementValue < out.u)
posix <- c("FG1","FG2","FG3","FG4","FG5","FG6","FG7","FG8","FG9","FG10","FG11")
ggplot(data = subset(data3, data3$MeasurementValue < out.u), aes(x = factor(FG), y = log(MeasurementValue), fill = factor(FG))) +
    geom_boxplot(colour = "black") + theme_minimal() + ylab("Abundance - log (#/m3)") + xlab("") + scale_x_discrete(limits = posix)   
### Interesting...Look at that again but without outliers and by computing medians per cell_ids 

data4 <- subset(data3, data3$MeasurementValue < out.u)
# head(data4)
avg.data4 <- data.frame(data4 %>% group_by(cell_id,FG) %>% summarize(n = n(), mean.abund = median(MeasurementValue)))
summary(avg.data4)
# Remove cells that were only visited less than 3 times
ggplot(data = subset(avg.data4, avg.data4$n >= 3), aes(x = factor(FG), y = log(mean.abund), fill = factor(FG))) +
    geom_boxplot(colour = "black") + theme_minimal() + ylab("Mean abundance - log (#/m3)") + xlab("") +
    scale_x_discrete(limits = posix)   
    
### Nice! To explore further...tomorrow!
save(data3, file = "AtlantECO_WP2_table_Hexanauplia_NMFS-COPEPOD+CPRs+Cornils.2018_no_null_abund_CopepodFunctionalGroups_16_12_21.Rdata")


### ----------------------------------------------------------------------------------------------------------------------------


### 16/12/21: Keep examining FGs abund distribution, this time per sampling protocol/months/basins etc.

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("viridis")
world <- map_data("world")  # for maps
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/OVERSEE/manuscript#3_copepods_funct_groups/data")

data <- get(load("AtlantECO_WP2_table_Hexanauplia_NMFS-COPEPOD+CPRs+Cornils.2018_no_null_abund_CopepodFunctionalGroups_16_12_21.Rdata"))
dim(data)

### To evaluate how many spatial points (i.e. grid cells) are covered by each SamplingID
# Contribution in terms of cell ids
ncells <- length(unique(data$cell_id)); ncells
contribs <- data.frame( data %>% group_by(Sampling_id) %>% summarize(n = length(unique(cell_id)), perc = (n/ncells)*100) )
contribs <- contribs[order(contribs$perc, decreasing = T),]
contribs$cumsum <- cumsum(contribs$perc)
contribs
# Contribution in terms of n obs
contribs <- data.frame( data %>% group_by(Dataset,Sampling_id) %>% summarize(n = n(), perc = (n/nrow(data))*100) )
contribs <- contribs[order(contribs$perc, decreasing = T),]
contribs$cumsum <- cumsum(contribs$perc)
# Identify top contributors
tops <- contribs[contribs$cumsum < 99,"Sampling_id"]
tops

### To evaluate sampling effort with only the top contributing sampling devices
spatial.effort <- data.frame(data[data$Sampling_id %in% tops,] %>% group_by(cell_id2) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
dim(spatial.effort) ; summary(spatial.effort) # 6939 grid cells
ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 

# Map the distribution of the top contributing sampling devices
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
#         data = data, alpha = .5) + scale_colour_discrete(name = "Source dataset") +
#     geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180),
#         labels = c("180°","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
#         panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom")
        
map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(MeshSize)),
        data = data[data$Sampling_id %in% tops,], alpha = .5) + scale_colour_discrete(name = "Mesh size\n(µm)") +
    geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) + 
    coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180),
        labels = c("180°","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°"), expand = c(0,0)) + 
    scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom") 
# Save this one
ggsave(plot = map, filename = "map_copepods_FGs_abund_sampling_meshes.png", dpi = 300, width = 12, height = 10)

# Same but with Datasets
map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
        data = data[data$Sampling_id %in% tops,], alpha = .5) + scale_colour_discrete(name = "Source dataset") +
    geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) + 
    coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180),
        labels = c("180°","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°"), expand = c(0,0)) + 
    scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom") 
# Save this one
ggsave(plot = map, filename = "map_copepods_FGs_abund_datasets.png", dpi = 300, width = 12, height = 10)


# # Better? Map average mesh size
# mesh.effort <- data.frame(data[data$Sampling_id %in% tops,] %>% group_by(cell_id2) %>% summarize(x = unique(x_1d), y = unique(y_1d), mesh = mean(MeshSize)))
# dim(mesh.effort) ; summary(mesh.effort) # 6939 grid cells
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_tile(aes(x = x, y = y, fill = mesh), data = mesh.effort) + scale_fill_viridis(name = "Mean mesh\nsize (µm)", option = "B") +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# # Not great

### Decisions as to which sampling protocols to keep: 
# - discard the 55µm mesh samples from Cornils et al. 2018 (too fine and very localized) 
# - discard 100 vertical samples from NMFS COPEPOD (relatively fine mesh and only the Baltic sea only)
# - discard mesh size > 300µm from NMFS COPEPOD (relatively coarse mesh and too few points)
# - discard Sampling_id == 168_NA (coastal Arctic only)
dim(data) # 1103169 
sub <- data[-which(data$MeshSize == 55),] ; dim(sub) # 1098273 (99.55% of 'data')
sub <- sub[-which(sub$MeshSize == 100 & sub$Dataset == "NMFS-COPEPOD"),] ; dim(sub) # 1083602 (98.2%)
sub <- sub[-which(sub$MeshSize > 280),] ; dim(sub) # 1057286 (95.8%)
sub <- sub[-which(sub$MeshSize == 168),] ; dim(sub) # 1030472 (93.4%)
dim(sub)

# Re-map, just to make sure
map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(MeshSize)),
        data = sub[sub$Sampling_id %in% tops,], alpha = .5) + scale_colour_discrete(name = "Mesh size\n(µm)") +
    geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) + 
    coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180),
        labels = c("180°","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°"), expand = c(0,0)) + 
    scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom") 

ggsave(plot = map, filename = "map_copepods_FGs_abund_sampling_meshes_subset.png", dpi = 300, width = 12, height = 10)

### Examining mean FGs abundance by splitting the data into the following methdological subsets: 
# - 125+145 µm samples: Atlantic Ocean
summary(sub[sub$MeshSize %in% c(125,145),c("x_1d","y_1d")]) # Remove the samples with longitude > 30
# - 112µm (all) + 125 + 145 with longitude > -10 : Mediterranean and Indian Ocean
summary(sub[sub$MeshSize %in% c(112,125,145),c("x_1d","y_1d")]) # Remove the samples with longitude > 30
# - 100+150+200+270 with Latitude < -40 : Southern seas
summary(sub[sub$MeshSize %in% c(100,150,200,270),c("x_1d","y_1d")]) # Remove the samples with Latitude > -40
# - NAtl and NPac CPR: 270µm with Latitude > 10°


### Subset1: 125+145 µm samples: Atlantic Ocean; Remove the samples with longitude > 30
sub1 <- sub[which(sub$MeshSize %in% c(125,145)),]
sub1 <- sub1[which(sub1$decimalLongitude < 30),]
summary(sub1[,c("x_1d","y_1d")])

### Subset2: 112µm (all) + 125 + 145 with longitude > -10 : Mediterranean and Indian Ocean
sub2 <- sub[which(sub$MeshSize %in% c(112,125,145)),]
sub2 <- sub2[!(sub2$MeshSize %in% c(125,145) & sub2$decimalLongitude < 30),]

### Subset3: 100+150+200+270 with Latitude < -40 : Southern seas
sub3 <- sub[which(sub$MeshSize %in% c(100,150,200,270)),]
sub3 <- sub3[sub3$decimalLatitude < -40,]

### Subset4: VPR above 10° Latitude
sub4 <- sub[which(sub$MeshSize == 270 & sub$decimalLatitude > 10),]
summary(sub4[,c("x_1d","y_1d")])

posix <- c("FG1","FG2","FG3","FG4","FG5","FG6","FG7","FG8","FG9","FG10","FG11")

### Limit scales
# summary(log(sub$MeasurementValue))
for(s in c("sub1","sub2","sub3","sub4")) {
    
    message(paste("Plotting inter-FGs distribution for subset = ",s, sep = ""))
    
    if(s == "sub1") {
         d <- sub1
         title <- "Copepod functional groups abundance distribution\n(Atlantic Ocean; 46°S-53°N)"
    } else if(s == "sub2") {
         d <- sub2
         title <- "Copepod functional groups abundance distribution\n(Indian Ocean and Mediterranean Sea)"
    } else if(s == "sub3") {
         d <- sub3
         title <- "Copepod functional groups abundance distribution\n(Southern seas; >40°S)"
    } else if(s == "sub4") {
         d <- sub4
         title <- "Copepod functional groups abundance distribution\n(North Atlantic and North Pacific Oceans; >48°N)"
    } # eo if else loop 
     
    m <- mean(d$MeasurementValue)
    sd <- sd(d$MeasurementValue)
    out.u <- m+2*sd
    # no.out <- subset(data3, data3$MeasurementValue < out.u)
    
    d <- subset(d, d$MeasurementValue < out.u)
    # head(d)
    
    avg.d <- data.frame(d %>% group_by(cell_id,FG) %>% summarize(n = n(), mean.abund = mean(MeasurementValue)))
    group.counts <- data.frame(d %>% group_by(FG) %>% summarize(n = n()))
    group.counts
    # summary(avg.d)
    
    # Remove cells that were only visited less than 3 times
    plot <- ggplot(data = subset(avg.d, avg.d$n >= 3), aes(x = factor(FG), y = log(mean.abund), fill = factor(FG))) +
        geom_violin(colour = "black") + geom_boxplot(colour = "black", fill = "white", width = .2) + 
        theme_minimal() + ylab("Mean abundance - log (#/m3)") + xlab("") +
        scale_x_discrete(limits = posix) + scale_y_continuous(limits = c(-5,5)) +
        theme(legend.position = "none") + ggtitle(title)
    #
    ggsave(plot = plot, filename = paste("boxplot_FG_abund_",s,".png", sep = ""), dpi = 300, width = 6, height = 6)
    
} # eo for loop

### Same as above, but for relative contribution to total abundnance
s <- "sub2" # For testing
for(s in c("sub1","sub2","sub3","sub4")) {
    
    message(paste("Plotting inter-FGs distribution for subset = ",s, sep = ""))
    
    if(s == "sub1") {
         d <- sub1
         title <- "Copepod functional groups relative abundance distribution\n(Atlantic Ocean; 46°S-53°N; 125µm mesh)"
    } else if(s == "sub2") {
         d <- sub2
         title <- "Copepod functional groups relative abundance distribution\n(Indian Ocean and Mediterranean Sea; 112µm)"
    } else if(s == "sub3") {
         d <- sub3
         title <- "Copepod functional groups relative abundance distribution\n(Southern seas; >40°S; 100-270µm meshes)"
    } else if(s == "sub4") {
         d <- sub4
         title <- "Copepod functional groups relative abundance distribution\n(North Atlantic and North Pacific Oceans; >48°N; CPR)"
    } # eo if else loop 
     
    m <- mean(d$MeasurementValue)
    sd <- sd(d$MeasurementValue)
    out.u <- m+2*sd
        
    d <- subset(d, d$MeasurementValue < out.u)
    # head(d)
    
    avg.d <- data.frame(d %>% group_by(cell_id2,FG) %>% summarize(x = unique(x_1d), y = unique(y_1d), n = n(), mean.abund = mean(MeasurementValue)))
    # summary(avg.d) 
    avg.d$total.abund <- NA
    for(i in unique(avg.d$cell_id2)) {
        message(paste(i, sep = ""))
        tot <- sum(avg.d[avg.d$cell_id2 == i,"mean.abund"])
        avg.d[avg.d$cell_id2 == i,"total.abund"] <- tot
    } # eo for loop - i in cell_id2
    avg.d$perc <- (avg.d$mean.abund/avg.d$total.abund)*100
    
    # Remove cells that were only visited less than 5 times
    plot <- ggplot(data = subset(avg.d, avg.d$n >= 5), aes(x = factor(FG), y = perc, fill = factor(FG))) +
        geom_violin(colour = "black") + geom_boxplot(colour = "black", fill = "white", width = .2) + 
        theme_minimal() + ylab("Relative contribution to total mean abundance (%)") + xlab("") +
        scale_x_discrete(limits = posix) + #+ scale_y_continuous(limits = c(0,100)) +
        theme(legend.position = "none") + ggtitle(title)
    #
    ggsave(plot = plot, filename = paste("boxplot_FG_relat_abund_",s,".png", sep = ""), dpi = 300, width = 6, height = 6)
    
} # eo for loop


### And global? 
title <- "Copepod functional groups abundance distribution\n(Global, CPR surveys only)"
glob.cpr <- sub[which(sub$MeshSize == 270),]
m <- mean(glob.cpr$MeasurementValue)
sd <- sd(glob.cpr$MeasurementValue)
out.u <- m+2*sd
# no.out <- subset(data3, data3$MeasurementValue < out.u)
d <- subset(glob.cpr, glob.cpr$MeasurementValue < out.u)
head(d)
avg.d <- data.frame(d %>% group_by(cell_id2,FG) %>% summarize(n = n(), mean.abund = mean(MeasurementValue)))
summary(avg.d)
# Remove cells that were only visited less than 3 times
plot <- ggplot(data = subset(avg.d, avg.d$n >= 5), aes(x = factor(FG), y = log(mean.abund), fill = factor(FG))) +
    geom_violin(colour = "black") + geom_boxplot(colour = "black", fill = "white", width = .2) + 
    theme_minimal() + ylab("Mean abundance - log (#/m3)") + xlab("") +
    scale_x_discrete(limits = posix) + theme(legend.position = "none") + ggtitle(title)
ggsave(plot = plot, filename = paste("boxplot_FG_abund_global_CPRs.png", sep = ""), dpi = 300, width = 6, height = 5)

### Look at this per 10° or 30° lat bands?
# unique(round(sub.glob$decimalLatitude, -1))
d <- subset(glob.cpr, glob.cpr$MeasurementValue < out.u)
d$band <- factor(round(d$decimalLatitude,-1))
posix2 <- factor(seq(-90,90,10))
# Test latitudinal trends with boxplots
avg.d <- data.frame(d %>% group_by(cell_id2,band,FG) %>% summarize(n = n(), mean.abund = mean(MeasurementValue)))
summary(avg.d) ; dim(avg.d)
# Remove cells that were only visited less than 3 times
avg.d$FGord <- factor(avg.d$FG, levels = posix)
plots <- ggplot(data = avg.d[avg.d$n >= 5,], aes(x = factor(band), y = log(mean.abund))) +
    geom_boxplot(colour = "black", fill = "grey60") + 
    theme_minimal() + ylab("Abundance - log(#/m3)") + xlab("") +
    scale_x_discrete(limits = posix2) + theme(legend.position = "none") + 
    ggtitle(title) + coord_flip() + facet_wrap(.~factor(FGord))
#
ggsave(plot = plots, filename = paste("boxplot_FG_abund_global_CPRs_bands.png", sep = ""), dpi = 300, width = 10, height = 10)


### 17/12/21: Look at patterns of relative abundance instead (CPR data still)
# First, remove outliers as always
d <- subset(glob.cpr, glob.cpr$MeasurementValue < out.u)
head(d)
# Compute relative contribution of FGs to total abundance (%) per sites
avg.d <- data.frame(d %>% group_by(cell_id2,FG) %>% summarize(x = unique(x_1d), y = unique(y_1d), n = n(), mean.abund = mean(MeasurementValue)))
summary(avg.d)
# Add sum of mean abund per cell_id2
avg.d$total.abund <- NA
for(i in unique(avg.d$cell_id2)) {
    message(paste(i, sep = ""))
    tot <- sum(avg.d[avg.d$cell_id2 == i,"mean.abund"])
    avg.d[avg.d$cell_id2 == i,"total.abund"] <- tot
} # eo for loop - i in cell_id2
summary(avg.d)
head(avg.d)
### Derive FG's relative contributions to total mean abund
avg.d$perc <- (avg.d$mean.abund/avg.d$total.abund)*100

### Plot while discarding sites with n < 5
ggplot(data = subset(avg.d, avg.d$n >= 5), aes(x = factor(FG), y = perc, fill = factor(FG))) +
    geom_violin(colour = "black") + geom_boxplot(colour = "black", fill = "white", width = .2) + 
    theme_minimal() + ylab("Relative contribution to total mean abundance (#/m3)") + xlab("") +
    scale_x_discrete(limits = posix) + theme(legend.position = "none") + ggtitle(title)

### And per lat bands:
d$band <- factor(round(d$decimalLatitude,-1))
posix2 <- factor(seq(-90,90,10))
# Test latitudinal trends with boxplots
avg.d <- data.frame(d %>% group_by(cell_id2,band,FG) %>% summarize(x = unique(x_1d), y = unique(y_1d),
        n = n(), mean.abund = mean(MeasurementValue)))
summary(avg.d) 
avg.d$total.abund <- NA
for(i in unique(avg.d$cell_id2)) {
    message(paste(i, sep = ""))
    tot <- sum(avg.d[avg.d$cell_id2 == i,"mean.abund"])
    avg.d[avg.d$cell_id2 == i,"total.abund"] <- tot
} # eo for loop - i in cell_id2
avg.d$perc <- (avg.d$mean.abund/avg.d$total.abund)*100

# Remove cells that were only visited less than 5 times
avg.d$FGord <- factor(avg.d$FG, levels = posix)
title <- "Relative contribution of copepod functional groups to mean annual abundance \n(Global, CPR surveys only)"
plot <- ggplot(data = avg.d[avg.d$n >= 5,], aes(x = factor(band), y = perc)) +
    geom_boxplot(colour = "black", fill = "grey60") + 
    theme_minimal() + ylab("Relative contribution to total mean abundnance (%)") + xlab("") +
    scale_x_discrete(limits = posix2) + theme(legend.position = "none") + 
    ggtitle(title) + coord_flip() + facet_wrap(.~factor(FGord))

ggsave(plot = plot, filename = paste("boxplot_FG_relat_abund_global_CPRs_bands.png", sep = ""), dpi = 300, width = 10, height = 10)


### ----------------------------------------------------------------------------------------------------------------------------

### 11/01/22: Look at patterns of relative abundance globally through bar charts (suggestion from M.V.)
# http://www.sthda.com/french/wiki/ggplot2-barplots-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees 
# https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html 

### First, discard outliers (twice the stdev)
m <- mean(sub$MeasurementValue)
sd <- sd(sub$MeasurementValue)
out.u <- m+2*sd
# no.out <- subset(data3, data3$MeasurementValue < out.u)
d <- subset(sub, sub$MeasurementValue < out.u)
d <- d[d$MeshSize %in% c(100,112,125,145,149,150,200,270),]
head(d); dim(d) # 98.5% of 'sub'

# Compute relative contribution of FGs to total abundance (%) per sites and lat bands
d$band <- factor(round(d$decimalLatitude,-1))
# Will need to inform N obs bands
bands.n <- data.frame( d %>% group_by(band) %>% summarize(n = n()) ) ; bands.n
sum(bands.n$n)

# Test latitudinal trends with boxplots
avg.d <- data.frame(d %>% group_by(band,FG) %>% summarize(n = n(), mean.abund = mean(MeasurementValue)))
dim(avg.d)
avg.d$total.abund <- NA
for(i in unique(avg.d$band)) {
    message(paste(i, sep = ""))
    tot <- sum(avg.d[avg.d$band == i,"mean.abund"])
    avg.d[avg.d$band == i,"total.abund"] <- tot
} # eo for loop - i in cell_id2
avg.d$perc <- (avg.d$mean.abund/avg.d$total.abund)*100
summary(avg.d)

posix <- c("FG1","FG2","FG3","FG4","FG5","FG6","FG7","FG8","FG9","FG10","FG11")
posix2 <- factor(seq(-90,90,10))

### OK? now try the double barchart
avg.d$FGord <- factor(avg.d$FG, levels = posix)
# vector of colors used for the functional dendrogram (FG1)
# colors <- c('#8b4513','#008000','#4682b4','#4b0082','#ff0000','#ffd700','#00ff00','#00ffff','#0000ff','#ff1493','#ffe4b5')
# Check

p1 <- ggplot(avg.d, aes(x = factor(band), y = perc, fill = factor(FGord))) + geom_bar(stat = "identity") +
     scale_fill_brewer(palette = "Paired", name = "FG") + scale_x_discrete(limits = posix2) + 
     scale_y_reverse(breaks = seq(from = 0, to = 100, by = 10), labels = seq(from = 0, to = 100, by = 10)) + #scale_y_reverse() + 
     xlab("Latitudinal band (10°)") + ylab("Relative contribution (%) to total mean annual abundance (#/m3)") +
     theme(legend.position = "bottom") + coord_flip()

### 12/01/22: Map points and color them per FG (Meike's comment)
d$FGord <- factor(d$FG, levels = posix)
test.map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(FGord)),
       data = d, alpha = .5) + scale_colour_brewer(palette = "Paired", name = "FG") +
   geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) + 
   coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180),
       labels = c("180°","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°"), expand = c(0,0)) + 
   scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
       labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
   theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
       panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom") 

ggsave(plot = test.map, filename = "Fig.S16a_v2.png", dpi = 300, width = 10, height = 7)

### And by panelling per FGord?
test.map.panel <- ggplot(data = d) + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(FGord)), data = d) +
    scale_colour_brewer(palette = "Paired", name = "FG") +
    geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) + 
    coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180),
       labels = c("180°","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°"), expand = c(0,0)) + 
    scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
       labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
       panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom") +
    facet_wrap(.~FGord, ncol = 2)   

ggsave(plot = test.map.panel, filename = "Fig.S16a_v3.png", dpi = 300, width = 12, height = 20)


### Need to add the % contrib of mesh sizes too
unique(d$MeshSize)
avg.meshes <- data.frame(d %>% group_by(band,MeshSize) %>% summarize(n = n()))
head(avg.meshes); dim(avg.meshes)
# Add total n per cell id and derive % contrib of meshes
avg.meshes$total <- NA
for(i in unique(avg.meshes$band)) {
    message(paste(i, sep = ""))
    tot <- sum(avg.d[avg.d$band == i,"n"])
    avg.meshes[avg.meshes$band == i,"total"] <- tot
} # eo for loop - i in cell_id2
avg.meshes$perc <- (avg.meshes$n/avg.meshes$total)*100
summary(avg.meshes)

### OK, now same as above but for meshes
p2 <- ggplot(avg.meshes, aes(x = factor(band), y = perc, fill = factor(MeshSize))) + geom_bar(stat = "identity") + 
     scale_fill_brewer(palette = "Spectral", name = "Mesh size\n(µm)") + scale_x_discrete(limits = posix2) +
     scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10), labels = seq(from = 0, to = 100, by = 10)) + 
     xlab("Latitudinal band (10°)") + ylab("Relative contribution (%) to total number of observations") +
      theme(legend.position = "bottom") + coord_flip()

### Assemble in panel
library("ggpubr")
panel <- ggarrange(p1, p2, labels = c("a)","b)"), align = "hv", ncol = 2)
ggsave(plot = panel, filename = "Fig.S16.1.png", dpi = 300, width = 10, height = 10)

# In case a map is demanded as well
map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(MeshSize)),
       data = d, alpha = .5) + scale_colour_brewer(palette = "Spectral", name = "Mesh size\n(µm)") +
   geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) + 
   coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-150,-120,-90,-60,-30,0,30,60,90,120,150,180),
       labels = c("180°","150°W","120°W","90°W","60°W","30°W","0°","30°E","60°E","90°E","120°E","150°E","180°"), expand = c(0,0)) + 
   scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
       labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
   theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
       panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom") 
#
ggsave(plot = map, filename = "Fig.S16.2.png", dpi = 300, width = 10, height = 7)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------