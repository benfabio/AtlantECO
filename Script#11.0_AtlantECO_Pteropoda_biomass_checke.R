
##### ATLANTECO SCRIPT 11.0 ----------------------------------------------------------------------------------------------------------------------------
##### 15/07/2022: R Script to check and finalize the Pteropoda biomass data prepared by Nielja Knecht ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load re-formatted dataset, examine the format etc.
# - Add bathymetry & longhurst provinces

### Latest update: 15/07/2022

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

### 1°) Load Nielja's final pteropoda dataset, check colnames and columns order
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/3_biomass/Pteropoda"); dir()

biom <- read.csv("pteropoda_complete_quality_controlled_2022-07-14.csv", h = T)
dim(biom) # 841239    167 (because still has all the predictors )
colnames(biom)
# Remove col 1:3, 75:87, 89:158, put 'MeshSize' before 'SamplingProtocol'
unique(biom$BiomassConvFactorLiterature) # put BiomassConvFactorLiterature and LengthValLiterature before basisOfRecord
biom2 <- biom[,c(4:74)]
biom2 <- add_column(biom2, MeshSize = biom$MeshSize, .before = "SamplingProtocol")
biom2 <- add_column(biom2,BiomassConvFactorLiterature  = biom$BiomassConvFactorLiterature, .before = "basisOfRecord")
biom2 <- add_column(biom2, LengthValLiterature = biom$LengthValLiterature, .before = "basisOfRecord")
colnames(biom2)
# Good.


### Create an occurrenceID 
biom2$occurrenceID <- paste(biom2$decimalLatitude,biom2$decimalLongitude,biom2$Day,biom2$Month,biom2$Year,
                            biom2$MinDepth,biom2$MaxDepth,biom2$ScientificName,biom2$MeasurementValue, sep = "_")
#
length(unique(biom2$occurrenceID))                   

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
# unique(biom2$Bathymetry)
# unique(biom2$BathySource)
biom2$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
biom2$Bathymetry <- raster::extract(x = ras.bathy, y = biom2[,c("decimalLongitude","decimalLatitude")])
summary(biom2$Bathymetry)

### Add Longhurst provinces
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
biom2$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = biom2[,c("decimalLongitude","decimalLatitude")]))
unique(biom2$LonghurstProvince) ; summary(factor(biom2$LonghurstProvince))

# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(biom2$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    biom2[biom2$LonghurstProvince == i & !is.na(biom2$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(biom2$LonghurstProvince))
# 12323 NAs (too coastal points) --> (12323/nrow(biom2))*100 = 1.46% of the data 

### Do some final checks
summary(biom2$MeasurementValue)
summary(biom2$Biomass_mgCm3)
unique(biom2$BiomassConvFactor)

# Check names one last time
unique(biom2$ScientificName) 
unique(biom2$WoRMS_ID) 
unique(biom2$TaxonRank) 

### Check coordinates etc.
summary(biom2[,c("decimalLatitude","decimalLongitude","MinDepth","MaxDepth","Day","Month","Year")])

# Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/3_biomass/Pteropoda")
save(biom2, file = "AtlantECO-BASEv1_dataset_Pteropoda_abund+biomass_15_07_22.RData")
write.table(biom2, file = "AtlantECO-BASEv1_dataset_Pteropoda_abund+biomass_15_07_22.csv", sep = ";")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------