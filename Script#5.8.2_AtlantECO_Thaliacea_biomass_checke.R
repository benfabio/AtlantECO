
##### ATLANTECO SCRIPT 5.8.2 ----------------------------------------------------------------------------------------------------------------------------
##### 04/07/2022: R Script to check and finalize the Thaliaceans (Salps and Doliolids) biomass data prepared by Corentin Clerc (ENS-MLD) ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted dataset, examine the format etc.
# - Add bathymetry & longhurst provinces

### Latest update: 04/07/2022

library("marmap")
library("raster")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("lubridate")
library("viridis")

world <- map_data("world")  # for maps


### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Load Corentin's dataset, check colnames and columns order
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/3_biomass/Thaliaceans"); dir()
biom <- read.csv("AtlantECO-BASEv1_dataset_Thaliaceans_abund+biomass_29_06_22.csv", h = T)
dim(biom) # 491529     65
str(biom)
colnames(biom)
head(biom)
unique(biom$MeasurementUnit)
biom$MeasurementUnit <- "#/m3"

### Check colnames against those of the abundance data
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance")
abund <- get(load("AtlantECO-BASEv1_dataset_Thaliacea_abundances_29_04_22.RData"))
dim(abund) # 494623     71
colnames(abund)
head(abund)

# setdiff
base::setdiff(x = colnames(abund), y = colnames(biom))
base::setdiff(x = colnames(biom), y = colnames(abund))
# Remove 'X' and 'Dataset' from 'biom'
biom2 <- select(biom,-X)
biom2 <- select(biom2,-Dataset)
colnames(biom2)
unique(abund$MeshSize)
unique(abund$Flag)

# Add:
# "ProjectID"     "ProjectWP"     "DataSilo"      "ContactName"  "ContactAdress" "occurrenceID" -> before 'orig_occurrenceID'
# Add "MeshSize" after 'SamplingProtocol'  
# Add "Flag" after 'Note'

biom2 <- add_column(biom2, ProjectID = "AtlantECO_H2020_GA#862923", .before = "orig_occurrenceID")
biom2 <- add_column(biom2, ProjectWP = "WP2", .before = "orig_occurrenceID")
biom2 <- add_column(biom2, DataSilo = "Trad_microscopy", .before = "orig_occurrenceID")
biom2 <- add_column(biom2, ContactName = "Corentin_Clerc;Fabio_Benedetti", .before = "orig_occurrenceID")
biom2 <- add_column(biom2, ContactAdress = "cclerc@lmd.ipsl.fr;fabio.benedetti@usys.ethz.ch", .before = "orig_occurrenceID")
biom2 <- add_column(biom2, occurrenceID = NA, .before = "orig_occurrenceID") # To fill
# For MeshSize & Flag, need to find the common cells first
biom2 <- add_column(biom2, MeshSize = NA, .after = "SamplingProtocol") # To fill
biom2 <- add_column(biom2, Flag = NA, .after = "Note") # To fill
head(biom2)
# Quick check again
#base::setdiff(x = colnames(abund), y = colnames(biom2))
#base::setdiff(x = colnames(biom2), y = colnames(abund))


### OK, both have same columns, just need to specify MeshSize (Flag is actually all NA in 'abund', so just MeshSize)
### Create an occurrenceID for both abund and biom2 and find matching MeshSize based on it
biom2$occurrenceID <- paste(biom2$decimalLatitude,biom2$decimalLongitude,biom2$Day,biom2$Month,biom2$Year,
                            biom2$MinDepth,biom2$MaxDepth,biom2$ScientificName,biom2$MeasurementValue, sep = "_")
                            
abund$occurrenceID <- paste(abund$decimalLatitude,abund$decimalLongitude,abund$Day,abund$Month,abund$Year,
                            abund$MinDepth,abund$MaxDepth,abund$ScientificName,abund$MeasurementValue, sep = "_")
#
# length(unique(abund$occurrenceID)) ; length(unique(biom2$occurrenceID))
commons <- intersect(unique(biom2$occurrenceID), unique(abund$occurrenceID))
# length(commons) 433193 unique IDs
# c <- commons[13]
res <- mclapply(commons, function(c) {
    
        message(c)
        sub <- biom2[biom2$occurrenceID == c,]
        mesh <- unique(abund[abund$occurrenceID == c,"MeshSize"])
        sub$MeshSize <- mesh
        return(sub)
        
    }, mc.cores = 25
    
) # eo mclapply
# Rbind
biom3 <- dplyr::bind_rows(res)
unique(biom3$MeshSize)
# Convert MeshSize to numerics
abund$MeshSize <- as.numeric(abund$MeshSize)
biom3$MeshSize <- as.numeric(biom3$MeshSize)
# summary(abund$MeshSize); summary(biom3$MeshSize)


### ----------------------------------------------------------

### 2°) Add bathymetry & Longhurst's provinces

bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15)
ras.bathy <- as.xyz(bathy)
colnames(ras.bathy) <- c("x","y","z")
ras.bathy <- rasterFromXYZ(ras.bathy)
# plot(ras.bathy)
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors/Longhurst_world_v4_2010")
BGCP.Longhurst <- raster::raster("Longhurst_world_v4_2010_04_04_22.grd")

### Add bathymetry
# unique(biom3$Bathymetry)
# unique(biom3$BathySource)
biom3$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
biom3$Bathymetry <- raster::extract(x = ras.bathy, y = biom3[,c("decimalLongitude","decimalLatitude")])
summary(biom3$Bathymetry)

### Add Longhurst provinces
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
biom3$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = biom3[,c("decimalLongitude","decimalLatitude")]))
unique(biom3$LonghurstProvince) ; summary(factor(biom3$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(biom3$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    biom3[biom3$LonghurstProvince == i & !is.na(biom3$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(biom3$LonghurstProvince))
# 7354 NAs (too coastal points) --> (7354/nrow(biom3))*100 = 1.52% of the data 

### Do some final checks
summary(biom3$MeasurementValue)
summary(biom3$Biomass_mgCm3)
summary(biom3$BiomassConvFactor)

# Check names one last time
unique(biom3$ScientificName) 
unique(biom3$WoRMS_ID) 
unique(biom3$TaxonRank) 

### Check coordinates etc.
summary(biom3[,c("decimalLatitude","decimalLongitude","MinDepth","MaxDepth","Day","Month","Year")])

# Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/3_biomass/Thaliaceans")
save(biom3, file = "AtlantECO-BASEv1_dataset_Thaliaceans_abund+biomass_04_07_22.RData")
write.table(biom3, file = "AtlantECO-BASEv1_dataset_Thaliaceans_abund+biomass_04_07_22.csv", sep = ";")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------