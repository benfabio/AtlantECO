
##### ATLANTECO SCRIPT 5.4 ----------------------------------------------------------------------------------------------------------------------------
##### 12/04/2022: R Script to create the AtlantECO-BASE1 abundance only dataset for krill (Euphausiids) ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted datasets, examine their format (should be the same for everyone)
# - Combine them, and clean dataset (units, data sources, ducplicates, add bathymetry and Longhurst provinces)

### Latest update: 13/04/2022

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

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/")
#setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/MAREDAT files/Leblanc&al._2012_ESSD_PANGAEA.777384")

### ----------------------------------------------------------------------------------------------------------------------------

### List the datasets to be merged to create the abundance data compilation for: Copepods
# NMFS-COPEPOD x
# SO-CPR x
# AusCPR x
# KRILLBASE x

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) NMFS-COPEPOD
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA/") ; dir()
kri <- get(load("COPEPOD-NOAA_Euphausiacea_4218000_reformatted+WoRMScheck_11_11_21.Rdata"))
# dim(kri) # 37'450
# Make sure you only have krill (Order == "Euphausiacea")
# unique(kri$Order) ; unique(kri$Family) # NA family? probably and Order-level obs
# head(kri[is.na(kri$Family),]) ; unique(kri[is.na(kri$Family),"ScientificName"]) # 
# All good

# Check distribution of Measurement
#summary(kri$MeasurementValue) # 2791 NAs to remove (probably from KRILLBASE)
#summary(factor(kri$MeasurementUnit)) # #/m3
kri <- kri[!is.na(kri$MeasurementValue),]
dim(kri) # 34'659

# Check origin of data (to identify old CPR version and remove it - should be in the top contributors to total N so near the top of the lists below)
# kri %>% count(basisOfRecord, sort = T)
# kri %>% count(SamplingProtocol, sort = T)
# #kri %>% count(InstitutionCode, sort = T)
# kri %>% count(basisOfRecord, sort = T)
# unique(kri$basisOfRecord)
#
# ### Remove CPR observations
# kri2 <- kri[!(kri$basisOfRecord %in% c("Tow type= horizontal; with: Continuous Plankton Recorder (CPR) MBA/SAHFOS","Tow type= horizontal; with: Continuous Plankton Sampler (model not specified)") ),]
# dim(kri2) # 30'541
# # unique(kri2$basisOfRecord)
# gc()

# Quickmap
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#          data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#      geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(basisOfRecord)),
#          data = kri, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") +
#      coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
#          panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# Good. No more CPR

### Check colnames and some if necessary
colnames(kri)
# head(cope3)
# unique(kri2$obisID) # to remove
# unique(kri2$DatasetKey) # keep
kri <- select(kri, -obisID)

### Add mesh (from 'SamplingProtocol') size after 'SamplingProtocol'
head(do.call(rbind, strsplit(x = as.character(unique(kri$SamplingProtocol)), split = "; ", fixed = F)))
meshes <- do.call(rbind, strsplit(x = kri$SamplingProtocol, split = "; ", fixed = F))[,3]
# length(meshes); unique(meshes) # length(meshes) == nrow(kri) # should be TRUE
# Remove 'mesh= ' and '1/2m opening @ ' and 'um)'
meshes <- str_replace_all(meshes, "mesh= ", "")
meshes <- str_replace_all(meshes, "1/2m opening @ ", "")
meshes[meshes == "333um)"] <- "333"
# unique(meshes)
# summary(factor(meshes))  # Looks ok
kri <- kri %>% add_column(MeshSize = meshes, .after = "SamplingProtocol")

# Check ScientificNames and WoRMS_ID and some other headers
unique(kri$ScientificName)
unique(kri$WoRMS_ID) # str(kri2$WoRMS_ID)  # should be character string throughout all files
unique(kri$LifeForm) # important for C biomass conversion
# Tally taxonranks
kri %>% count(TaxonRank, sort = T) # Ok good


### ----------------------------------------------------------

### 2°) NAtl+NPac CPR
# setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2") ; dir()
# cpr <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
# dim(cpr) # 712'720
# # Subset Euphausiacea
# unique(cpr$Order) ; unique(cpr$Class)
# ### --> NO KRILL!
# rm(cpr) ; gc()

# # Check in raw CPR data maybe
# raw.cpr <- get(load("dwca_CPRv1.2_occ+event_zoo_27_04_2021.Rdata"))
# dim(raw.cpr)
# head(raw.cpr)
# unique(raw.cpr$scientificName)

### Krill obs from CPR not provided by the default submission of David Johns' team. Proceed to 2 other CPR surveys.

### CANCEL LINE 57 ABOVE (keep old CPR obs) 

### ----------------------------------------------------------

### 3°) SO-CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
# dim(so.cpr) # 15'204'798
# Subset Euphausiacea
# unique(so.cpr$Order)
so.cpr <- so.cpr[so.cpr$Order == "Euphausiacea",]
dim(so.cpr) # 4'447'662

# Check colnames, measured values etc.
#setdiff(colnames(kri2), colnames(so.cpr)) # missing MeshSize
#setdiff(colnames(so.cpr), colnames(kri2)) # obisID to rm in cpr
so.cpr <- dplyr::select(so.cpr, -c(obisID,Temperature_Celsius,Salinity,PAR_microE_m2_s1,Fluorescence,PCI))
# Add MeshSize unique(cpr$basisOfRecord)
so.cpr <- so.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
summary(so.cpr$MeasurementValue) # 1189491 NaNs to remove and clear outliers 
so.cpr <- so.cpr[!is.na(so.cpr$MeasurementValue),]
dim(so.cpr) # 3'258'171

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
# str(so.cpr) # Looks ok
# Check ScientificName
# unique(so.cpr$ScientificName) # OK

# Rbind
kri2 <- rbind(kri, so.cpr)
dim(kri2) # 3'292'830
rm(so.cpr) ; gc()


### ----------------------------------------------------------

### 4°) AusCPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
aus.cpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
# dim(aus.cpr) # 7'373'520
# Subset Euphausiacea 
unique(aus.cpr$Order)
aus.cpr <- aus.cpr[aus.cpr$Order == "Euphausiacea" & !is.na(aus.cpr$Order),]
dim(aus.cpr) # 451'584

# Check colnames, measured values etc.
#setdiff(colnames(kri3), colnames(aus.cpr)) # missing MeshSize
#setdiff(colnames(aus.cpr), colnames(kri3)) # obisID to rm in cpr
aus.cpr <- dplyr::select(aus.cpr, -obisID)
# Add MeshSize unique(aus.cpr$basisOfRecord)
aus.cpr <- aus.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of MeasurementValue
#summary(aus.cpr$MeasurementValue) # 1 NA to remove and clear outliers 
aus.cpr <- aus.cpr[!is.na(aus.cpr$MeasurementValue),]
dim(aus.cpr) # 451583

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
# str(aus.cpr) # Looks ok
# Check ScientificName
# unique(aus.cpr$ScientificName) # OK

# Rbind
kri3 <- rbind(kri2, aus.cpr)
dim(kri3) # 3'744'413
rm(kri,aus.cpr) ; gc()


### ----------------------------------------------------------

### 5°) KRILLBASE
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/KRILLBASE") ; dir()
kbase <- get(load("KRILLBASE_Atkinson2017_reformatted+WoRMScheck_21_10_21.Rdata"))
dim(kbase) # 29'086
# Subset Euphausiacea 
unique(kbase$Order)
kbase <- kbase[kbase$Order == "Euphausiacea",]
dim(kbase) # 14'543

# Check colnames, measured values etc.
#setdiff(colnames(kri3), colnames(kbase)) # missing MeshSize
#setdiff(colnames(kbase), colnames(kri3)) # obisID to rm in cpr
kbase <- dplyr::select(kbase, -obisID)
# Add MeshSize? 
# unique(kbase$SamplingProtocol)
# Not in there...did you retain the actual info from the raw data?
# krikri <- read.csv("KRILLBASE_full_query_cleaned_28_04_2021.csv", h = T, sep = ";", dec = ",")
# dim(krikri)
# colnames(krikri)
### Actually not given --> NA
kbase <- kbase %>% add_column(MeshSize = NA, .after = "SamplingProtocol")

# Check distribution of MeasurementValue
summary(kbase$MeasurementValue) # 2950 NA to remove and clear outliers 
unique(kbase$MeasurementUnit) # ind/m2 - not m3
kbase <- kbase[!is.na(kbase$MeasurementValue),]
# dim(kbase) # 11'593

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
#str(kbase) # Looks ok
# Check ScientificName
#unique(kbase$ScientificName) # OK

# Rbind
kri4 <- rbind(kri3, kbase)
dim(kri4) # 3'756'006
rm(kri2,kbase) ; gc()

# Quickmap to check
# unique(kri4$BiblioCitation)
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(BiblioCitation)), data = kri4, alpha = .5) +
   scale_colour_brewer(name = "Source", palette = "Paired") + 
   coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group), 
       data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
   scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
       labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
   scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
       labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
   theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
       panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")


### ----------------------------------------------------------

### 6°) Final steps: check for duplicates, add bathymetry & longhurst provinces, make map
### Need to add 'Bathymetry' and 'LonghurstProvince'
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15)
ras.bathy <- as.xyz(bathy)
colnames(ras.bathy) <- c("x","y","z")
ras.bathy <- rasterFromXYZ(ras.bathy)
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors/Longhurst_world_v4_2010")
BGCP.Longhurst <- raster::raster("Longhurst_world_v4_2010_04_04_22.grd")
# Go back to proper dir
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance")

### Add bathymetry
unique(kri4$BathySource) # Wait, does not apply to all data
summary(kri4$Bathymetry) # Only apply below to NA
kri4[is.na(kri4$Bathymetry),"BathySource"] <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
kri4[is.na(kri4$Bathymetry),"Bathymetry"] <- raster::extract(x = ras.bathy, y = kri4[is.na(kri4$Bathymetry),c("decimalLongitude","decimalLatitude")])

### Add Longhurst provinces: since the raster only has the provinces' indices, first extract those (maybe as characters, easier for replacement after) and then convert them back to the actual provinces' names using the levels of the raster layer:
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
kri4$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = kri4[,c("decimalLongitude","decimalLatitude")]))
# Check
unique(kri4$LonghurstProvince) ; summary(factor(kri4$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(kri4$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    kri4[kri4$LonghurstProvince == i & !is.na(kri4$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(kri4$LonghurstProvince))
# 32055 NAs (too coastal points) --> (32055/nrow(kri4))*100 = 0.85% of the data 

### Do some final checks
# summary(kri4$MeasurementValue) # No NA
# unique(kri4$MeasurementUnit) ### beware of krillbase obs here --> standardized m2
# kri4$MeasurementUnit <- "#/m3"

# Tally taxonrank
counts <- data.frame( kri4 %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(kri4)) )
counts[order(counts$n, decreasing = T),]
# Good
# - 85% are species level

### Check sampling depth headers
summary(kri4[,c("Depth","MinDepth","MaxDepth")])
# Only 2-5 obs have NA in "MinDepth","MaxDepth
kri4[is.na(kri4$MaxDepth),] # OK to rm
kri4 <- kri4[!is.na(kri4$MinDepth),]
kri4 <- kri4[!is.na(kri4$MaxDepth),]
# dim(kri4)

### Attribute an occurrenceID? (for duplicates) --> put MeasurementUnit in there to make sure to separate KRILLBASE from the rest
kri4$occurrenceID <- factor(paste(round(kri4$decimalLongitude,2), round(kri4$decimalLatitude,2), kri4$Day, kri4$Month, kri4$Year, kri4$MaxDepth, kri4$ScientificName, kri4$MeasurementValue, kri4$MeasurementUnit, sep = "_"))
# How many unique obs?
length(unique(kri4$occurrenceID)) # 1'071'450 unique obs. 
(length(unique(kri4$occurrenceID))/nrow(kri4))*100 
### --> 71.5%% overlap 

kri5 <- kri4[!duplicated(kri4$occurrenceID),]
dim(kri5) # 1'071'450

### Drop useless cols
# colnames(kri5)
kri5 <- dplyr::select(kri5,-c(ProjectID,ProjectWP,DataSilo))
# Save
save(kri5, file = "AtlantECO-BASEv1_dataset_Euphausiacea_abundances_13_04_22.RData")
write.table(kri5, file = "AtlantECO-BASEv1_dataset_Euphausiacea_abundances_13_04_22.csv", sep = ";")

# Quickmap to check
# unique(kri4$BiblioCitation)
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(BiblioCitation)), data = kri5, alpha = .5) +
   scale_colour_brewer(name = "Source", palette = "Paired") + 
   coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group), 
       data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
   scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
       labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
   scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
       labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
   theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
       panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------