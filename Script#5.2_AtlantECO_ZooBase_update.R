
##### ATLANTECO SCRIPT 5.2 ----------------------------------------------------------------------------------------------------------------------------
##### 01/04/2022: R Script to update the ZooBase dataset (Benedetti et al., 2021) with AtlantECO WP2 datsets from the 'Traditional data' silo ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted ZooBase dataset, examine main sources and their identifiers
# - Add AtlantECO datasets (CPR surveys, MALASPINA, JedI)

### Latest update: 08/09/2022

library("marmap")
library("raster")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("lubridate")
library("viridis")
library("here")

world <- map_data("world")  # for maps

### ----------------------------------------------------------------------------------------------------------------------------

### List the dataset part of ZooBase v1:
# OBIS
# GBIF
# Cornils et al. 2018

### List the datasets to add to ZooBase v1 to create v2:
# Updated CPR x
# Updated SO-CPR x
# Updated AusCPR x
# MALASPINA gelatinous zooplankton x
# JeDI (occ only + abundances only) x
# KRILLBASE x
# Burridge et al. 2016 x
# Becker at al. 2021 x
# Additional Forams and Pteropods dataset from R. Schiebel x

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Load ZooBase datasets (the 3 main sources: OBIS, GBIF, Cornils et al. 2018)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021") ; dir()

obis <- get(load("ZOObase_OBIS_merged_reformated+WoRMScheck_04_06_2021.RData"))  
gbif <- get(load("ZOObase_GBIF_merged_reformated+WoRMScheck_04_06_2021.RData"))  
cornils <- get(load("ZOObase_Cornils&al._2018_SO_Copepods_reformated+WoRMScheck_04_06_2021.RData"))
# dim(obis); dim(gbif); dim(cornils)
# Need to convert WoRMS_ID to factor
# unique(gbif$WoRMS_ID)
gbif$WoRMS_ID <- factor(gbif$WoRMS_ID)

# Check colnames
# colnames(obis)
# colnames(gbif)
# colnames(cornils)
#
# # Check orig datasets keys
# unique(gbif$DatasetKey) # --> can be used to trace gbif datasets with rgbif::datasets()
# unique(obis$DatasetKey)
# unique(gbif$gbifID)  # occurrence ID?
# unique(obis$obisID) # occurrence ID?
# unique(gbif$orig_occurrenceID)
# unique(obis$orig_occurrenceID)
# # And for Cornils?
# unique(cornils$DatasetKey)  # OK NA
# unique(cornils$obisID)  # OK NA
# unique(cornils$orig_occurrenceID)

# Like for PhytoBase v2 (Script #5.1) --> merge all 3 datasets by giving everyone GBIF.key and OBIS.key columns and drop: gbifID and obisID
obis <- obis %>% add_column(GBIF.key = NA, .after = "DatasetKey")
obis <- obis %>% add_column(OBIS.key = obis$DatasetKey, .after = "GBIF.key")
gbif <- gbif %>% add_column(GBIF.key = gbif$DatasetKey, .after = "DatasetKey")
gbif <- gbif %>% add_column(OBIS.key = NA, .after = "GBIF.key")
cornils <- cornils %>% add_column(GBIF.key = NA, .after = "DatasetKey")
cornils <- cornils %>% add_column(OBIS.key = NA, .after = "GBIF.key")
# Drop
obis <- select(obis, - obisID)
gbif <- select(gbif, - gbifID)
cornils <- select(cornils, - obisID)

# Add flag to map new data over older one in the end
obis$version <- "ZooBase v1"
gbif$version <- "ZooBase v1"
cornils$version <- "ZooBase v1"

# Rbind all 3
zoo <- rbind(obis,gbif,cornils)
dim(zoo) # 3'472'405      71
zoo$BiblioCitation <- "Benedetti, F., Vogt, M., Elizondo, U. H., Righetti, D., Zimmermann, N. E., & Gruber, N. (2021). Nature Communications, 12(1), 5226. doi:10.1038/s41467-021-25385-x"

summary(zoo[,c("Day","Month","Year")])

unique(zoo$WoRMS_ID)
unique(zoo[zoo$WoRMS_ID %in% c("To_add_at_the_end","No match found in WoRMS"),"ScientificName"])
nrow(zoo[zoo$WoRMS_ID %in% c("To_add_at_the_end","No match found in WoRMS"),]) / nrow(zoo)

# Check ID columns: source archive, institution code etc. to identify CPR data
#unique(zoo$ParentEventID) # Nope - useless here
#unique(zoo$EventID) # Nope - useless here
#unique(zoo$InstitutionCode) # "SAHFOS" 
counts <- data.frame(zoo %>% group_by(InstitutionCode) %>% summarize(n = n()) )
counts[order(counts$n, decreasing = T),]

# Identify the top contributors and map their associated data
tops <- counts[order(counts$n, decreasing = T),][1:15,"InstitutionCode"]
# tops

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(InstitutionCode)),
#         data = zoo[zoo$InstitutionCode %in% tops,]) +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#                data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#             labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#             labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") +
#     facet_wrap(.~factor(InstitutionCode))

### Check some spatial and taxonomic overlaps between:
# "JODC" x "" #
sub1 <- zoo[zoo$InstitutionCode == "" & !is.na(zoo$InstitutionCode),] 
sub2 <- zoo[zoo$InstitutionCode == "JODC" & !is.na(zoo$InstitutionCode),]
sub3 <- zoo[is.na(zoo$InstitutionCode),]
dim(sub1) ; dim(sub2) ; dim(sub3)
# Theoretically: sub3 > sub1 > sub2
# Compare species lists
# unique(sub1$ScientificName) # everything BUT copepods
# unique(sub2$ScientificName) # --> mix of different groups
# unique(sub3$ScientificName) # --> only copepods!


### Rbind, give occurrence ID, remove duplicated, give back to zoobase
# Give occurrendID
sub1$occurrenceID <- factor(paste(round(sub1$decimalLongitude,2), round(sub1$decimalLatitude,2), sub1$Day, sub1$Month, sub1$Year, sub1$ScientificName, sep = "_"))
sub2$occurrenceID <- factor(paste(round(sub2$decimalLongitude,2), round(sub2$decimalLatitude,2), sub2$Day, sub2$Month, sub2$Year, sub2$ScientificName, sep = "_"))
sub3$occurrenceID <- factor(paste(round(sub3$decimalLongitude,2), round(sub3$decimalLatitude,2), sub3$Day, sub3$Month, sub3$Year, sub3$ScientificName, sep = "_"))
# Rbind
subs2clear <- rbind(sub1,sub2,sub3)
# dim(subs2clear) # 758192
zoo2 <- zoo[!(zoo$InstitutionCode %in% c(NA,"","JODC")),]
# dim(zoo2) # 2714213
# 2714213+758192 == 3472405
subs2clear <- subs2clear[!duplicated(subs2clear$occurrenceID),]
# dim(subs2clear) kept (178434/758192)*100 = 23.5% of the data. Good.
### Re-supply filtered data to zoo2
zoo3 <- rbind(zoo2,subs2clear)
zoo3$occurrenceID <- NA
# dim(zoo3) # 2892647 occ (83% of the data)

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(InstitutionCode)),
#         data = zoo3[zoo3$InstitutionCode %in% c("JODC","",NA),]) +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#                data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#             labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#             labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") +
#     facet_wrap(.~factor(InstitutionCode))

### Based on InstitutionCodes:
# - remove "SAHFOS" (classic CPR)
# - remove 'AADC' (SO-CPR)
# - remove 'CSIRO Oceans and Atmosphere, Australia (CMAR)' (AusCPR) ; 'CSIRO, Australia' corresponds to other datasets (older)

test <- subset(zoo, InstitutionCode != "SAHFOS")
test <- subset(test, InstitutionCode != "AADC")
test <- subset(test, InstitutionCode != "CSIRO Oceans and Atmosphere, Australia (CMAR)")
dim(zoo) ; dim(test)

zoo4 <- subset(zoo3, InstitutionCode != "SAHFOS")
zoo4 <- subset(zoo4, InstitutionCode != "AADC")
zoo4 <- subset(zoo4, InstitutionCode != "CSIRO Oceans and Atmosphere, Australia (CMAR)")
# How many obs were discarded
# (nrow(zoo4)/nrow(zoo3))*100 # Retained 57.4% of the occurrences. Not so bad.

# Map to further check CPR data?
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(SourceArchive)), data = zoo4) +
#      coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#                 data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#      scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#              labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#      scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#              labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#              panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") +
#      facet_wrap(.~factor(SourceArchive))

# Remove occurrences overlapping between OBIS & GBIF
# summary(zoo4$Depth) # No NA! Use for occurrence ID
zoo4$occurrenceID <- NA
zoo4$occurrenceID <- factor(paste(round(zoo4$decimalLongitude,2), round(zoo4$decimalLatitude,2), zoo4$Day, zoo4$Month, zoo4$Year, zoo4$Depth, zoo4$ScientificName, sep = "_"))
# length(unique(zoo4$occurrenceID)) # 784'839 unique occurrences
dim(zoo4) # but 1663232
### --> 47% overlap! (which makes sense considering the map above)
# Remove these overlapping occurrences
zoo5 <- zoo4[!duplicated(zoo4$occurrenceID),]
# dim(zoo5) # 784'839

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(SourceArchive)), data = zoo5) +
#      coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#                 data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#      scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#              labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#      scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#              labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#              panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") +
#      facet_wrap(.~factor(SourceArchive))

### OK, next, add updated CPR surveys etc.

save(zoo5, file = "tempo_file_ZooBasev1_to_update_07_04_22.RData")



### ------------------------------------------------------

### 08/04/22: 2°) Re-load the temporary file and add 'new' AtlantECO datasets 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021") 
zoo <- get(load("tempo_file_ZooBasev1_to_update_07_04_22.RData"))
dim(zoo) # 784'839 occurrences
# str(zoo)
unique(zoo$WoRMS_ID)
unique(zoo[zoo$WoRMS_ID %in% c("To_add_at_the_end","No match found in WoRMS"),"ScientificName"])
nrow(zoo[zoo$WoRMS_ID %in% c("To_add_at_the_end","No match found in WoRMS"),]) / nrow(zoo)


# Check whether there are zeroes (absences) in the quantitative measurements 
# numbers <- readr::parse_number(unique(zoo[zoo$MeasurementValue != "Presence" & !is.na(zoo$MeasurementValue),"MeasurementValue"]))
# min(numbers) ; summary(numbers)
# Some absences
# Tally
# data.frame(zoo[zoo$MeasurementValue != "Presence" & !is.na(zoo$MeasurementValue),] %>% group_by(factor(MeasurementValue)) %>% summarize(n = n()) )
zoo[zoo$MeasurementValue == "0" & !is.na(zoo$MeasurementValue),"MeasurementValue"] <- "Absence"
zoo[zoo$MeasurementValue != "Absence" & !is.na(zoo$MeasurementValue),"MeasurementValue"] <- "Presence"
# summary(factor(zoo$MeasurementValue)) # Good
zoo$MeasurementUnit <- NA


### 2.A) Updated NAtl & NPac CPR survey (converted to 1/0)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2") ; dir() 
cpr <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
# dim(cpr) # 712720
# Find unmatching colnames
# setdiff(colnames(zoo), colnames(cpr))
# setdiff(colnames(cpr), colnames(zoo))
# Drop obisID from 'cpr'
cpr <- select(cpr, -obisID)
# Add "GBIF.key" "OBIS.key" "version" to 'cpr'
cpr$version <- "ZooBase v2"
cpr <- cpr %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
cpr <- cpr %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")

### Check MeasurementValues
summary(cpr$MeasurementValue) ; min(cpr$MeasurementValue, na.rm = T)
### --> presence only
# Drop na
cpr <- cpr %>% filter(!is.na(MeasurementValue)) # dim(cpr)
cpr$MeasurementType <- "Occurrence"
cpr$MeasurementValue <- "Presence"
cpr$MeasurementUnit <- NA
dim(cpr)


# Rbind and check map
zoo2 <- rbind(zoo,cpr)
dim(zoo2) # 1391972 occurrences

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo2, alpha = .5) +
#       coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#           data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#       scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#           labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#       scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#           labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#       theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#           panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

rm(zoo,cpr) ; gc()


### 2.B) Updated Australian CPR survey (converted to 1/0)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir() 
aus.cpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
# dim(aus.cpr) # 7373520
# Check colnames again
# setdiff(colnames(zoo2), colnames(aus.cpr))
# setdiff(colnames(aus.cpr), colnames(zoo2))
# Same above
aus.cpr <- select(aus.cpr, -obisID)
# Add "GBIF.key" "OBIS.key" "version" to 'cpr'
aus.cpr$version <- "ZooBase v2"
aus.cpr <- aus.cpr %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
aus.cpr <- aus.cpr %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")

# Check MeasurementValues
summary(aus.cpr$MeasurementValue) ; min(aus.cpr$MeasurementValue, na.rm = T)
aus.cpr <- aus.cpr %>% filter(!is.na(MeasurementValue)) # Remove the 3 NAs
# Convert to 1/0 and then Presence/Absence
aus.cpr$MeasurementType <- "Occurrence"
aus.cpr$MeasurementUnit <- NA
aus.cpr$MeasurementValue <- as.integer( aus.cpr$MeasurementValue > 0.0 )
aus.cpr$MeasurementValue <- as.character(aus.cpr$MeasurementValue)
aus.cpr[aus.cpr$MeasurementValue == "1","MeasurementValue"] <- "Presence"
aus.cpr[aus.cpr$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# summary(factor(aus.cpr$MeasurementValue))
dim(aus.cpr)

# Looks good, rbind
zoo3 <- rbind(zoo2,aus.cpr)
dim(zoo3) # 8765489

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo3, alpha = .5) +
#        coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#            data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#        scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#        scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#            labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#            panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

rm(zoo2,aus.cpr) ; gc()

# Check list of scientififc names
# unique(zoo3$ScientificName) # 3869
# unique(zoo3$Genus) # 888 genera
# unique(zoo3$Kingdom) # "Chromista"? NA?
# dim(zoo3[zoo3$Kingdom == "Chromista",]) # 364'126
# unique(zoo3[zoo3$Kingdom == "Chromista","Phylum"])
# # Remove: "Ciliophora"   "Ochrophyta"   "Myzozoa"
# unique(zoo3[zoo3$Phylum == "Ciliophora","Order"]) # Keep Ciliophorans --> microzooplankton
# unique(zoo3[zoo3$Phylum == "Ochrophyta","Class"]) # Bacillariophyceae --> Diatoms
# unique(zoo3[zoo3$Phylum == "Myzozoa","Class"]) # Dinophyceae --> Dinos
# dim(zoo3)
zoo3 <- zoo3[!(zoo3$Phylum == "Ochrophyta"),]
zoo3 <- zoo3[!(zoo3$Phylum == "Myzozoa"),]
# NAs in Kingdom?
# head( zoo3[is.na(zoo3$Kingdom),] )
# Drop 
zoo3 <- zoo3 %>% filter(!is.na(Kingdom))
dim(zoo3) # 8582033


### 2.C) Updated Southern Ocean CPR survey (converted to 1/0)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir() 
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
dim(so.cpr) # 15'204'798
# Check colnames
# setdiff(colnames(zoo3), colnames(so.cpr))
# setdiff(colnames(so.cpr), colnames(zoo3))
# Drop: [1] "obisID"              "Temperature_Celsius" "Salinity"           
# [4] "PAR_microE_m2_s1"    "Fluorescence"        "PCI" 
so.cpr <- select(so.cpr, -c(obisID,Temperature_Celsius,Salinity,PAR_microE_m2_s1,Fluorescence,PCI))
# Add missing cols
so.cpr$version <- "ZooBase v2"
so.cpr <- so.cpr %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
so.cpr <- so.cpr %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")
# Check MeasurementValues
# summary(so.cpr$MeasurementValue) # No NA
# Convert to 1/0 and then Presence/Absence
so.cpr$MeasurementType <- "Occurrence"
so.cpr$MeasurementUnit <- NA
so.cpr$MeasurementValue <- as.integer( so.cpr$MeasurementValue > 0.0 )
so.cpr$MeasurementValue <- as.character(so.cpr$MeasurementValue)
so.cpr[so.cpr$MeasurementValue == "1","MeasurementValue"] <- "Presence"
so.cpr[so.cpr$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# summary(factor(so.cpr$MeasurementValue))
dim(so.cpr)

# Looks good, rbind
zoo4 <- rbind(zoo3,so.cpr)
# dim(zoo4) # 23786831
# rm(zoo3,so.cpr) ; gc()

# Check list of scientififc names
unique(zoo4$ScientificName) # 
unique(zoo4$Kingdom) # 
unique(zoo4$Phylum) # NA?
unique(zoo4[is.na(zoo4$Phylum),"ScientificName"])
# Discard them
names2rm <- unique(zoo4[is.na(zoo4$Phylum),"ScientificName"])   # nrow(zoo4[(zoo4$ScientificName %in% names2rm),])
names2rm
dim(zoo4[(zoo4$ScientificName %in% names2rm),])
zoo4 <- zoo4[!(zoo4$ScientificName %in% names2rm),]
# Check again
unique(zoo4[zoo4$Kingdom == "Chromista","Phylum"]) # "Ochrophyta"   "Myzozoa" again...
unique(zoo4[zoo4$Phylum == "Ochrophyta","Order"])
unique(zoo4[zoo4$Phylum == "Myzozoa","InstitutionCode"])
unique(zoo4[zoo4$Phylum == "Myzozoa","ScientificName"])
### --> Noctiluca scintillans obs in the SO-CPR
zoo4 <- zoo4[-which(zoo4$ScientificName == "Noctiluca scintillans"),]

# head( zoo4[zoo4$ScientificName == "Ctenophora",] )
### --> CLASSIF TO CORRECT!
levels(zoo4$WoRMS_ID)[levels(zoo4$WoRMS_ID) == "163921"] <- "1248"
zoo4$WoRMS_status <- as.character(zoo4$WoRMS_status)
zoo4[zoo4$ScientificName == "Ctenophora",'WoRMS_status'] <- "isMarine+isBrackish"
zoo4[zoo4$ScientificName == "Ctenophora","TaxonRank"] <- "Phylum"
zoo4[zoo4$ScientificName == "Ctenophora","Kingdom"] <- "Animalia"
zoo4[zoo4$ScientificName == "Ctenophora","Phylum"] <- "Ctenophora"
zoo4[zoo4$ScientificName == "Ctenophora",c("Class","Order","Family","Genus","Species","Subspecies")] <- NA
dim(zoo4) # 23424812 
### (23786831-23424812) # --> 362 019 were removed

# Quick map
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo4, alpha = .5) +
        coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
            data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
        scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
        scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
            labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
            panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

### OK, save temporary file and then add next AtlantECO datasets: KRILLBASE, JeDI, MALASPINA etc.
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
save(zoo4, file = "tempo_file_ZooBasev1+CPRsurveys_to_update_08_04_22.RData")


### ------------------------------------------------------

### Re-load tempo_file_ZooBasev1+CPRsurveys
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
zoo <- get(load("tempo_file_ZooBasev1+CPRsurveys_to_update_08_04_22.RData"))
dim(zoo) # 23'424'812
unique(zoo$WoRMS_ID)

### 3°) Re-load the temporary file and add the remaining AtlantECO datasets (MALASPINA, JeDI, KRILLBASE, Burridge, Becker, Schiebel) 

### 3.A) MALASPINA gelatinous zooplankton 1/0
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/MALASPINA_Villarino_2017")
mala <- get(load("MALASPINA_Villarinoetal._2017_gelatinous_zooplankton_reformated+WoRMScheck_01_06_2021.Rdata"))
# dim(mala) # 671  70
# Check colnames
# setdiff(colnames(zoo), colnames(mala)) # Add "GBIF.key" "OBIS.key" "version" 
# setdiff(colnames(mala), colnames(zoo))

mala$version <- "ZooBase v2"
mala <- mala %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
mala <- mala %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")

# Check MeasurementValues
# unique(mala$MeasurementValue) # Good
# OK, rbind

zoo2 <- rbind(zoo,mala)
dim(zoo2) # 23 425 483

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo2, alpha = .5) +
#         coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#             data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#         scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#             labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#         scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#             labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#         theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")


### 3.B) KRILLBASE (E. superba + S. maxima)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/KRILLBASE") ; dir()
krillb <- get(load("KRILLBASE_Atkinson2017_reformatted+WoRMScheck_21_10_21.Rdata"))
# dim(krillb) # 29086
# Check colnames
# setdiff(colnames(zoo2), colnames(krillb)) # Add "GBIF.key" "OBIS.key" "version" 
# setdiff(colnames(krillb), colnames(zoo2)) # Remove obisID
krillb <- select(krillb, -obisID)
krillb$version <- "ZooBase v2"
krillb <- krillb %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
krillb <- krillb %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")
# Check MeasurementValues
summary(krillb$MeasurementValue) ; min(krillb$MeasurementValue, na.rm = T)
# Absences + NA

krillb <- krillb %>% filter(!is.na(MeasurementValue)) # Remove the 3 NAs
# Convert to 1/0 and then Presence/Absence
krillb$MeasurementType <- "Occurrence"
krillb$MeasurementUnit <- NA
krillb$MeasurementValue <- as.integer( krillb$MeasurementValue > 0.0 )
krillb$MeasurementValue <- as.character(krillb$MeasurementValue)
krillb[krillb$MeasurementValue == "1","MeasurementValue"] <- "Presence"
krillb[krillb$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# summary(factor(krillb$MeasurementValue))

# Check ScientificName and dates
# unique(krillb$ScientificName)
# str(krillb$eventDate)

# Looks good, rbind
zoo3 <- rbind(zoo2,krillb)
# dim(zoo3) # 23446433
rm(zoo,krillb,mala) ; gc()

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo3, alpha = .5) +
#          coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#              data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#          scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#              labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#          scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#              labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#          theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#              panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")


### 3.C) JeDI 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database") ; dir()
### 3.C.1) JeDI occurrences
jedi.occ <- get(load("JeDi_Lucas&al._2014_occurrences_reformated+WoRMScheck_18_06_2021.Rdata"))
# dim(jedi.occ) # 298969
# Check colnames
# setdiff(colnames(zoo3), colnames(jedi.occ)) # Add "GBIF.key" "OBIS.key" "version" 
# setdiff(colnames(jedi.occ), colnames(zoo3)) # Remove obisID
jedi.occ <- select(jedi.occ, -obisID)
jedi.occ$version <- "ZooBase v2"
jedi.occ <- jedi.occ %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
jedi.occ <- jedi.occ %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")
# Check MeasurementValues
# unique(jedi.occ$MeasurementValue)  # Re-write with capitals
jedi.occ$MeasurementType <- "Occurrence"
jedi.occ$MeasurementUnit <- NA
jedi.occ[jedi.occ$MeasurementValue == "present","MeasurementValue"] <- "Presence"
jedi.occ[jedi.occ$MeasurementValue == "absent","MeasurementValue"] <- "Absence"

# Check ScientificName and dates
# unique(jedi.occ$ScientificName)
# summary(jedi.occ[,c("Day","Month","Year")])
# Add eventDate to jedi.occ based on these
dates <- lubridate::dmy(paste(jedi.occ$Day, jedi.occ$Month, jedi.occ$Year, sep = "-")) 
# unique(dates)
jedi.occ$eventDate <- dates

# Looks good, rbind
zoo4 <- rbind(zoo3,jedi.occ)
# dim(zoo4) # 23'745'402
rm(zoo2, jedi.occ) ; gc()

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo4, alpha = .5) +
#           coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#               data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#           scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#               labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#           scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#               labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#           theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#               panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

### --> Loads of overlap. Will need to remove some

### 3.C.2) JeDI abund
jedi.abund <- get(load("JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_03_12_2021.Rdata"))
# dim(jedi.abund) # 238083
# Check colnames
# setdiff(colnames(zoo4), colnames(jedi.abund)) # Add "GBIF.key" "OBIS.key" "version" 
# setdiff(colnames(jedi.abund), colnames(zoo4)) # Remove obisID
jedi.abund <- select(jedi.abund, -obisID)
jedi.abund$version <- "ZooBase v2"
jedi.abund <- jedi.abund %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
jedi.abund <- jedi.abund %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")

# Check MeasurementValues
# summary(jedi.abund$MeasurementValue) # unique(jedi.abund$MeasurementValue)
# min(jedi.abund$MeasurementValue) # discard the negative abund
# unique(jedi.abund$MeasurementUnit)
# nrow(jedi.abund[jedi.abund$MeasurementValue < 0,])
jedi.abund <- jedi.abund %>% filter(MeasurementValue != min(jedi.abund$MeasurementValue))
jedi.abund$MeasurementType <- "Occurrence"
jedi.abund$MeasurementUnit <- NA
jedi.abund$MeasurementValue <- as.integer( jedi.abund$MeasurementValue > 0.0 )
jedi.abund$MeasurementValue <- as.character(jedi.abund$MeasurementValue)
jedi.abund[jedi.abund$MeasurementValue == "1","MeasurementValue"] <- "Presence"
jedi.abund[jedi.abund$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# summary(factor(jedi.abund$MeasurementValue))

# Check ScientificName and dates
#unique(jedi.abund$ScientificName)
#summary(jedi.abund[,c("Day","Month","Year")])
# Add eventDate to jedi.occ based on these
dates <- lubridate::dmy(paste(jedi.abund$Day, jedi.abund$Month, jedi.abund$Year, sep = "-")) 
# unique(dates)
jedi.abund$eventDate <- dates

# Looks good, rbind
zoo5 <- rbind(zoo4,jedi.abund)
# dim(zoo5) # 23'983'484
rm(zoo3, jedi.abund) ; gc()

### Identify duplicate occurrences like you did above and discard occurrences overlapping between basic ZooBase and JeDI
### Can you use "Depth","MinDepth","MaxDepth" as headers for the occurrenceID
summary(zoo5[,c("Depth","MinDepth","MaxDepth")]) # No NA! Use for occurrence ID
# First, discard what does not have Depth or MaxDepth
# dim(zoo5 %>% filter(!if_all(c(Depth, MaxDepth), is.na)) ) 
# dim(zoo5[with(zoo5, is.na(Depth) & is.na(MinDepth) & is.na(MaxDepth)),] ) # 296 040 to remove
zoo5 <- zoo5[!with(zoo5, is.na(Depth) & is.na(MinDepth) & is.na(MaxDepth)),] 
# zoo5 <- zoo5 %>% filter(!if_all(c(Depth, MaxDepth), is.na)) 
# Check
#dim(zoo5) # 23687444
#summary(zoo5[is.na(zoo5$Depth),c("Depth","MinDepth","MaxDepth")])
#summary(zoo5[is.na(zoo5$MaxDepth),c("Depth","MinDepth","MaxDepth")])
#summary(zoo5[is.na(zoo5$MinDepth),c("Depth","MinDepth","MaxDepth")])

### Assess rate of duplicate occurrences
summary(factor(zoo5$MeasurementValue))
zoo5$occurrenceID <- factor(paste(round(zoo5$decimalLongitude,2), round(zoo5$decimalLatitude,2), zoo5$Day, zoo5$Month, zoo5$Year, zoo5$ScientificName, zoo5$MeasurementValue, sep = "_"))

# How many unique occ
length(unique(zoo5$occurrenceID)) # 16'604'195 unique occurrences. 
(length(unique(zoo5$occurrenceID))/nrow(zoo5))*100 
### --> 30% overlap! 

### Remove these overlapping occurrences
zoo6 <- zoo5[!duplicated(zoo5$occurrenceID),]
# dim(zoo6) # 16'604'195

# Quickmap
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo6, alpha = .5) +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#                data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#                labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#                labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#                panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

### Still that WODC Plankton Database --> should not be species level though (to discard for SDMs)
counts <- data.frame(zoo6 %>% group_by(TaxonRank) %>% summarize(n = n()))
counts[order(counts$n, decreasing = T),]

# clean some stuff
# str(zoo6$TaxonRank) # is a character string, good
zoo6[which(zoo6$TaxonRank == "SPECIES"),"TaxonRank"] <- "Species"
# Remove  <NA>; Variety and 
unique(zoo6[which(zoo6$TaxonRank == "Forma"),"ScientificName"])
unique(zoo6[which(zoo6$TaxonRank == "Forma"),"Genus"])
unique(zoo6[which(zoo6$TaxonRank == "Forma"),"WoRMS_ID"])
# Forma --> Variety
zoo6[which(zoo6$TaxonRank == "Forma"),"TaxonRank"] <- "Variety"
zoo6[which(zoo6$ScientificName == "Clio pyramidata f. lanceolata"),"Species"] <- "Clio pyramidata"
zoo6[which(zoo6$ScientificName == "Clio pyramidata f. sulcata"),"Species"] <- "Clio pyramidata"
zoo6[which(zoo6$ScientificName == "Cyclosalpa quadriluminis f. parallela"),"Species"] <- "Cyclosalpa quadriluminis"
# head(zoo6[which(zoo6$ScientificName == "Clio pyramidata f. lanceolata"),])
dim(zoo6) # 16 604 195

### Save temporary file and continue this afternoon
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
save(zoo6, file = "tempo_file_ZooBasev1+CPRsurveys+MALASPINA+KRILLBASE+JeDI_to_update_11_04_22.RData")


### ------------------------------------------------------

### Re-load "tempo_file_ZooBasev1+CPRsurveys+MALASPINA+KRILLBASE+JeDI_to_update_11_04_22.RData"
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
zoo <- get(load("tempo_file_ZooBasev1+CPRsurveys+MALASPINA+KRILLBASE+JeDI_to_update_11_04_22.RData"))
dim(zoo) # 16 604 195

# Reconvert the occurrenceID to NA
zoo$occurrenceID <- NA

### 3.D) Burridge et al. (2016) --> Prepared by Nielka Knecht on 'meso' (not kryo)
setwd("/net/meso/work/nknecht/Masterarbeit/Data/final_single_files_pteropods") ; dir()
burr <- read.table("AMT_24_Burridge2016_PteropodsHeteropods_18_10_21.txt", h = T)
# dim(burr) # 1953
#str(burr)

# Check colnames
# setdiff(colnames(zoo), colnames(burr)) # Add "GBIF.key" "OBIS.key" "version" 
# setdiff(colnames(burr), colnames(zoo)) # Remove obisID
burr <- select(burr, -obisID)
burr$version <- "ZooBase v2"
burr <- burr %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
burr <- burr %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")
# Check MeasurementValues
summary(burr$MeasurementValue)  # Re-code to integer and then P/A
burr$MeasurementType <- "Occurrence"
burr$MeasurementUnit <- NA
burr$MeasurementValue <- as.integer(burr$MeasurementValue > 0.0)
burr$MeasurementValue <- as.character(burr$MeasurementValue)
burr[burr$MeasurementValue == "1","MeasurementValue"] <- "Presence"
burr[burr$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# summary(factor(burr$MeasurementValue))

# Check ScientificName and dates
# unique(burr$ScientificName)
# unique(burr$WoRMS_ID) # Good
# unique(burr$WoRMS_status) # Good
# unique(burr$eventDate)

# Need to convert WoRMS_ID to factor
burr$WoRMS_ID <- factor(burr$WoRMS_ID)

# Looks good, rbind
zoo2 <- rbind(zoo,burr)
dim(zoo2) # 16 606 148
rm(burr) ; gc()

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo2, alpha = .5) +
#            coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#                data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#            scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#                labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#            scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#                labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#            theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#                panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")


### 3.E) Becker et al. (2021)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/Copepod_species_SAO_Becker&al._2021") ; dir()
becker <- get(load("table_Becker2021_PiO_copepods_abund_SAO_reformatted+WoRMScheck_14_02_22.Rdata"))
dim(becker) # 8732

# Check colnames
# setdiff(colnames(zoo2), colnames(becker)) # Add "GBIF.key" "OBIS.key" "version" 
# setdiff(colnames(becker), colnames(zoo2)) # Remove obisID
becker <- select(becker, -obisID)
becker$version <- "ZooBase v2"
becker <- becker %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
becker <- becker %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")
# Check MeasurementValues
summary(becker$MeasurementValue)  # Re-code to integer and then P/A
becker$MeasurementType <- "Occurrence"
becker$MeasurementUnit <- NA
becker$MeasurementValue <- as.integer(becker$MeasurementValue > 0.0)
becker$MeasurementValue <- as.character(becker$MeasurementValue)
becker[becker$MeasurementValue == "1","MeasurementValue"] <- "Presence"
becker[becker$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# summary(factor(becker$MeasurementValue))

# Check ScientificName and dates etc.
#unique(becker$ScientificName)
#unique(becker$WoRMS_ID) # Good
#unique(becker$WoRMS_status) # Good
#unique(becker$eventDate) # Good
# just remove the "Copepoda all"
becker <- becker[-which(becker$ScientificName == "Copepoda all"),] # dim(becker)

# Need to convert WoRMS_ID to factor
becker$WoRMS_ID <- factor(becker$WoRMS_ID)

# Rbind
zoo3 <- rbind(zoo2,becker)
dim(zoo3) # 16614843
rm(zoo,becker) ; gc()


### 3.F) Additional datasets from R. Schiebel --> Prepared by R. Schiebel
### 3.F.1) Forams: /net/meso/work/nknecht/Masterarbeit/Data/final_single_files_forams
setwd("/net/meso/work/nknecht/Masterarbeit/Data/final_single_files_forams") ; dir()
forams <- read.csv("Schiebel_foraminifera_AtlantECO_nknecht_fb_09_12_21.csv", h = T)
# dim(forams) # 259 900
# head(forams)
unique(forams$BiblioCitation)
unique(forams$CitationDOI)

# Check colnames
# setdiff(colnames(zoo3), colnames(forams)) # Add "GBIF.key" "OBIS.key" "version" 
# setdiff(colnames(forams), colnames(zoo3)) # Remove obisID + "Surf_temperature" "Surf_salinity"    "MeshSize"
forams <- select(forams, -c(obisID,Surf_temperature,Surf_salinity,MeshSize))
forams$version <- "ZooBase v2"
forams <- forams %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
forams <- forams %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")

# Check MeasurementValues
summary(forams$MeasurementValue)  # Re-code to integer and then P/A
forams$MeasurementType <- "Occurrence"
forams$MeasurementUnit <- NA
forams$MeasurementValue <- as.integer(forams$MeasurementValue > 0.0)
forams$MeasurementValue <- as.character(forams$MeasurementValue)
forams[forams$MeasurementValue == "1","MeasurementValue"] <- "Presence"
forams[forams$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# summary(factor(forams$MeasurementValue))

# Check ScientificName and dates etc.
# unique(forams$ScientificName)
# unique(forams$WoRMS_ID) # Good
# unique(forams$WoRMS_status) # Good
# unique(forams$eventDate) # Good

# Convert 'WoRMS_ID' to factor()
forams$WoRMS_ID <- factor(forams$WoRMS_ID) # unique(forams$WoRMS_ID)

# Rbind
zoo4 <- rbind(zoo3,forams)
dim(zoo4) # 16874743
rm(zoo2, forams) ; gc()


### 3.F.2) Pteropods: /net/meso/work/nknecht/Masterarbeit/Data/final_single_files_pteropods 
setwd("/net/meso/work/nknecht/Masterarbeit/Data/final_single_files_pteropods") ; dir()
ptero <- read.csv("Schiebel_pteropods_AtlantECO_nknecht_22-01-19.csv", h = T)
# dim(ptero) # 48378
#unique(ptero$BiblioCitation)
#unique(ptero$CitationDOI)

# Check colnames
# setdiff(colnames(zoo4), colnames(ptero)) # Add "GBIF.key" "OBIS.key" "version" 
# setdiff(colnames(ptero), colnames(zoo4)) # Remove obisID + "Surf_temperature" "Surf_salinity"    "MeshSize"
ptero <- select(ptero, -c(obisID,Surf_temperature,Surf_salinity,MeshSize))
ptero$version <- "ZooBase v2"
ptero <- ptero %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
ptero <- ptero %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")

# Check MeasurementValues
# summary(ptero$MeasurementValue)  # Re-code to integer and then P/A
ptero$MeasurementType <- "Occurrence"
ptero$MeasurementUnit <- NA
ptero$MeasurementValue <- as.integer(ptero$MeasurementValue > 0.0)
ptero$MeasurementValue <- as.character(ptero$MeasurementValue)
ptero[ptero$MeasurementValue == "1","MeasurementValue"] <- "Presence"
ptero[ptero$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# summary(factor(ptero$MeasurementValue))

# Check ScientificName and dates etc.
unique(ptero$ScientificName)
unique(ptero$WoRMS_ID) # Good
unique(ptero$WoRMS_status) # Good
str(ptero$eventDate) # To convert to date
str(ptero$WoRMS_ID) # To convert to factor
ptero$eventDate <- as.Date(ptero$eventDate)
ptero$WoRMS_ID <- factor(ptero$WoRMS_ID) # unique(ptero$WoRMS_ID)

# Rbind
zoo5 <- rbind(zoo4,ptero)
dim(zoo5) # 16923121
rm(zoo3,ptero) ; gc()

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo5, alpha = .5) +
#             coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#                 data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#             scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#                 labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#             scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#                 labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#             theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#                 panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

### Some final checks: depth levels, duplicates
summary(zoo5[,c("Depth","MinDepth","MaxDepth")])
# Check occ that have no Depth levels?
# dim(zoo5[with(zoo5, is.na(Depth) & is.na(MinDepth) & is.na(MaxDepth)),]) # 0
# dim(zoo5[with(zoo5, is.na(Depth) & is.na(MaxDepth)),]) # 153'103 too
#
# sub.na.depth <- zoo5[with(zoo5, is.na(Depth) & is.na(MaxDepth)),]
# dim(sub.na.depth)
# summary(sub.na.depth[,c("Depth","MinDepth","MaxDepth")])
# unique(sub.na.depth$CitationDOI) # Only JeDI/precision
# # Check taxonomic resolution
# summary(factor(sub.na.depth$TaxonRank)) # mostly species
# length( unique(sub.na.depth$ScientificName) )
#
# # Quickmap
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = sub.na.depth, alpha = .5) +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

# I would opt to discard them:
# zoo6 <- zoo5[!with(zoo5, is.na(Depth) & is.na(MaxDepth)),]
# dim(zoo5)
# dim(zoo6) # Retained 99.3% of occs

### Check for duplicates again
zoo6$occurrenceID <- factor(paste(zoo6$decimalLongitude, zoo6$decimalLatitude, zoo6$Day, zoo6$Month, zoo6$Year, 
                        zoo6$Depth, zoo6$MinDepth, zoo6$MaxDepth, zoo6$ScientificName, zoo6$MeasurementValue, sep = "_"))

# How many unique occ?
length(unique(zoo6$occurrenceID)) #  unique occurrences. 
(length(unique(zoo6$occurrenceID))/nrow(zoo6))*100 
### --> 

zoo7 <- zoo6[!duplicated(zoo6$occurrenceID),]
#dim(zoo7) # 16 670 316 Good
#nrow(zoo6)-nrow(zoo7) #2 52805

### Save tempo file
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
save(zoo7, file = "tempo_file_ZooBasev1+CPRsurveys+MALASPINA+KRILLBASE+JeDI+others_11_04_22.RData")


### ------------------------------------------------------

### To finish: add bathymetry and Longhurst provinces (lke for Script #5.2)
### Re-load tempo file
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
zoo2 <- get(load("tempo_file_ZooBasev1+CPRsurveys+MALASPINA+KRILLBASE+JeDI+others_11_04_22.RData"))
dim(zoo2) # 16670316

### Need to add 'Bathymetry' and 'LonghurstProvince'
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15)
ras.bathy <- as.xyz(bathy)
colnames(ras.bathy) <- c("x","y","z")
ras.bathy <- rasterFromXYZ(ras.bathy)
# plot(ras.bathy)
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors/Longhurst_world_v4_2010")
BGCP.Longhurst <- raster::raster("Longhurst_world_v4_2010_04_04_22.grd")
# BGCP.Longhurst ; plot(BGCP.Longhurst)

# Go back to proper dir
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()

### Add bathymetry
unique(zoo2$BathySource) # Wait, does not apply to all data
summary(zoo2$Bathymetry) # Only apply below to NA
zoo2[is.na(zoo2$Bathymetry),"BathySource"] <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
zoo2[is.na(zoo2$Bathymetry),"Bathymetry"] <- raster::extract(x = ras.bathy, y = zoo2[is.na(zoo2$Bathymetry),c("decimalLongitude","decimalLatitude")])

### Add Longhurst provinces: since the raster only has the provinces' indices, first extract those (maybe as characters, easier for replacement after) and then convert them back to the actual provinces' names using the levels of the raster layer:
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
zoo2$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = zoo2[,c("decimalLongitude","decimalLatitude")]))
unique(zoo2$LonghurstProvince) ; summary(factor(zoo2$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
# i <- "46"  # for testing 
for(i in unique(na.omit(zoo2$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    zoo2[zoo2$LonghurstProvince == i & !is.na(zoo2$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(zoo2$LonghurstProvince))
# 342724 NAs (too coastal points) --> (342724/nrow(zoo2))*100 = 2.05% of the data 

### Do some final checkings
summary(factor(zoo2$MeasurementValue))
# 89.5% are absences (because of the SO-CPR and the AusCPR)
# Tally taxonrank
counts <- data.frame( zoo2 %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(zoo2)) )
counts[order(counts$n, decreasing = T),]

# - 58% are species-level
# - 25% are genus-level
# - 5.3% are family-level
# - 3.5% are order-level


### Drop useless cols
colnames(zoo2)
class(zoo2)
zoo2 <- dplyr::select(zoo2,-c(Biomass_mgCm3,BiomassConvFactor))

save(zoo2, file = "AtlantECO-BASEv1_dataset_ZooBase2_occurrences_08_09_22.RData")
write.table(zoo2, file = "AtlantECO-BASEv1_dataset_ZooBase2_occurrences_08_09_22.csv", sep = ";")

### Print a map of occ distrib by colouring as a fun of ZooBase version
# map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = zoo2, alpha = .5) +
#     scale_colour_manual(name = "Version", values = c("#3288bd","#d53e4f")) +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#                data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#             labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#             labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
# ggsave(plot = map, filename = "map_ZooBasev2_occurrences_08_09_22.jpg", dpi = 300, width = 15, height = 12)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------