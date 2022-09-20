
##### ATLANTECO SCRIPT 5.10 ----------------------------------------------------------------------------------------------------------------------------
##### 11/07/2022: R Script to create the AtlantECO-BASE1 abundance only dataset for Ostracoda ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted datasets, examine their format (should be the same for everyone)
# - Combine them, and clean dataset (units, data sources, ducplicates, add bathymetry and Longhurst provinces)

### Latest update: 11/07/2022

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

### ----------------------------------------------------------------------------------------------------------------------------

### List the datasets to be merged to create the abundance data compilation for: Ostracoda
# NMFS-COPEPOD x
# SO-CPR x
# AusCPR x

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) NMFS-COPEPOD
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA/") ; dir()
ostra <- get(load("COPEPOD-NOAA_Ostracoda_4211000_reformatted+WoRMScheck_15_11_21.Rdata"))
dim(ostra) # 26735

# Check if only Class == Ostracoda
unique(ostra$Phylum) 
unique(ostra$Class)  
unique(ostra$Order) # has NA
# What's inside the NA
unique( ostra[which(is.na(ostra$Order)),"ScientificName"] ) # "Alacia minor" (https://www.marinespecies.org/aphia.php?p=taxdetails&id=172451)
# Keep
# unique(ostra$Family)
# unique(ostra$Genus)

# Fix classif of Alacia minor
ostra[ostra$ScientificName == "Alacia minor","Species"] <- "Alacia minor"
ostra[ostra$ScientificName == "Alacia minor","Genus"] <- "Alacia"
ostra[ostra$ScientificName == "Alacia minor","Family"] <- "Halocyprididae"
ostra[ostra$ScientificName == "Alacia minor","Order"] <- "Halocyprida"
ostra[ostra$ScientificName == "Alacia minor","Class"] <- "Ostracoda"
ostra[ostra$ScientificName == "Alacia minor","Phylum"] <- "Arthropoda"
ostra[ostra$ScientificName == "Alacia minor","WoRMS_ID"] <- "172451"
ostra[ostra$ScientificName == "Alacia minor","WoRMS_status"] <- "isMarine"
ostra[ostra$ScientificName == "Alacia minor","TaxonRank"] <- "Species"

# Check distribution of MeasurementValue
summary(ostra$MeasurementValue) # 1362 NaNs to remove
ostra2 <- ostra[!is.na(ostra$MeasurementValue),]
dim(ostra2) # 25'373

# Check origin of data 
ostra2 %>% count(ostra2$OrigCollectionID, sort = T) # mostly YugNIRO and Pelagic Ecosystems of the Tropical Atlantic
ostra2 %>% count(ostra2$SamplingProtocol, sort = T) 

# Quickmap
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#              data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#          geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(OrigCollectionID)),
#              data = ostra2, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") +
#          coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#          theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
#              panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# # Good.

### Check colnames and some if necessary
colnames(ostra2)
ostra2 <- select(ostra2, -obisID)

### Add mesh (from 'SamplingProtocol') size after 'SamplingProtocol'
# head(do.call(rbind, strsplit(x = as.character(unique(cope2$SamplingProtocol)), split = "; ", fixed = F)))
meshes <- do.call(rbind, strsplit(x = ostra2$SamplingProtocol, split = "; ", fixed = F))[,3]
# unique(meshes)
# length(meshes) == nrow(ostra2)
# Remove 'mesh= ' and '1/2m opening @ ' and 'um)'
meshes <- str_replace_all(meshes, "mesh= ", "")
meshes <- str_replace_all(meshes, "1/2m opening @ ", "")
meshes[meshes == "333um)"] <- "333"
meshes[meshes == "NA"] <- NA
# unique(meshes)
ostra2 <- ostra2 %>% add_column(MeshSize = meshes, .after = "SamplingProtocol")

# Check ScientificNames and WoRMS_ID and some other headers
unique(ostra2$ScientificName) # Looks good
unique(ostra2$WoRMS_ID) # str(cope2$WoRMS_ID)  # should be character string throughout all files
unique(ostra2$WoRMS_status) 
unique(ostra2$LifeForm) # important for C biomass conversion

# Tally taxonranks
ostra2 %>% count(TaxonRank, sort = T) # Ok good
#    TaxonRank     n
#1     Class    24279
#2     Genus      846
#3   Species      248

### OK, add the CPR surveys 

### ----------------------------------------------------------

### 2°) NAtl+NPac CPR
#setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2") ; dir()
#cpr <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
#dim(cpr) # 712'720
# Subset Ostracoda
#unique(cpr$Order)
#unique(cpr$Class)
#unique(cpr$Family)
# intersect(unique(ostra2$Order), unique(cpr$Order))
### --> No ostrapoda...
# rm(cpr) ; gc()

### ----------------------------------------------------------

### 3°) SO-CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))

# Subset Class Ostracoda
unique(so.cpr$Class)
so.cpr <- so.cpr[so.cpr$Class == "Ostracoda" & !is.na(so.cpr$Class),]
unique(so.cpr$Class)
unique(so.cpr$Order)
unique(so.cpr$Genus)
unique(so.cpr$ScientificName) # OK, only class-level info
unique(so.cpr$TaxonRank) 
dim(so.cpr) # 51'717

# Check colnames, measured values etc.
# setdiff(colnames(ostra2), colnames(so.cpr)) # missing MeshSize
# setdiff(colnames(so.cpr), colnames(ostra2)) # obisID to rm in cpr
so.cpr <- dplyr::select(so.cpr, -c(obisID,Temperature_Celsius,Salinity,PAR_microE_m2_s1,Fluorescence,PCI))
# Add MeshSize unique(cpr$basisOfRecord)
so.cpr <- so.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(so.cpr$MeasurementValue) # No NAs

# Check eventDate and WoRMS_ID before rbinding with 'ostra2'
# str(so.cpr) # Looks ok

# Rbind
ostra3 <- rbind(ostra2, so.cpr)
dim(ostra3) # 77'090
rm(ostra,so.cpr) ; gc()


### ----------------------------------------------------------

### 4°) AusCPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
aus.cpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))

# Subset Ostracoda
unique(aus.cpr$Class) 
aus.cpr <- aus.cpr[aus.cpr$Class == "Ostracoda" & !is.na(aus.cpr$Class),]
unique(aus.cpr$Class)
unique(aus.cpr$Order)
unique(aus.cpr$Genus)
unique(aus.cpr$ScientificName) # OK
dim(aus.cpr) # 56'448

# Check colnames, measured values etc.
# setdiff(colnames(cope3), colnames(aus.cpr)) # missing MeshSize
# setdiff(colnames(aus.cpr), colnames(cope3)) # obisID to rm in cpr
aus.cpr <- dplyr::select(aus.cpr, -obisID)
# Add MeshSize unique(aus.cpr$basisOfRecord)
aus.cpr <- aus.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(aus.cpr$MeasurementValue) # 1 NA to remove 
# aus.cpr <- aus.cpr[!is.na(aus.cpr$MeasurementValue),]

# Rbind
ostra4 <- rbind(ostra3, aus.cpr)
dim(ostra4) # 133'537
rm(ostra2,aus.cpr) ; gc()


### ----------------------------------------------------------

### 5°) Final checkings, add bathymetry and longhurst provinces

### Add bathymetry where needed and Longhurst provinces
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15)
ras.bathy <- as.xyz(bathy)
colnames(ras.bathy) <- c("x","y","z")
ras.bathy <- rasterFromXYZ(ras.bathy)
# plot(ras.bathy)
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors/Longhurst_world_v4_2010")
BGCP.Longhurst <- raster::raster("Longhurst_world_v4_2010_04_04_22.grd")
# Go back to proper dir
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") 

### Add bathymetry
# unique(ostra4$BathySource)
# unique(ostra4$Bathymetry)
ostra4$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
ostra4$Bathymetry <- raster::extract(x = ras.bathy, y = ostra4[,c("decimalLongitude","decimalLatitude")])
summary(ostra4$Bathymetry)

### Add Longhurst provinces
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
ostra4$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = ostra4[,c("decimalLongitude","decimalLatitude")]))
unique(ostra4$LonghurstProvince) ; summary(factor(ostra4$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(ostra4$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    ostra4[ostra4$LonghurstProvince == i & !is.na(ostra4$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(ostra4$LonghurstProvince))
# 4280 NAs (too coastal points) --> (4280/nrow(ostra4))*100 = 3.20% of the data 

### Do some final checks
# summary(ostra4$MeasurementValue)
# unique(ostra4$MeasurementUnit)
ostra4$MeasurementUnit <- "#/m3"

unique(ostra4$WoRMS_status)
unique(ostra4$ScientificName)
unique(ostra4$LifeForm)

# Tally taxonrank
counts <- data.frame( ostra4 %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(ostra4)) )
counts[order(counts$n, decreasing = T),]
#   TaxonRank     n      perc
#1     Class   90107  0.6747718
#2     Genus   29070  0.2176925
#3   Species   14360  0.1075357

### Check coordinates etc.
summary(ostra4[,c("decimalLatitude","decimalLongitude","Depth","MinDepth","MaxDepth","Day","Month","Year")])
# Discard the 4 NAs in MaxDepth 
ostra4 <- ostra4[!is.na(ostra4$MaxDepth),]

### Drop useless cols
# colnames(cope4)
ostra4 <- dplyr::select(ostra4,-c(ProjectID,ProjectWP,DataSilo))
# Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
save(ostra4, file = "AtlantECO-BASEv1_dataset_Ostracoda_abundances_11_07_22.RData")
write.table(ostra4, file = "AtlantECO-BASEv1_dataset_Ostracoda_abundances_11_07_22.csv", sep = ";")

# dim(get(load("AtlantECO-BASEv1_dataset_Ostracoda_abundances_11_07_22.RData")))

### Print a map of occ distrib by colouring as a fun of datasets (to check for )
unique(ostra4$BiblioCitation)
ostra4$Dataset <- NA
ostra4[ostra4$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD"
ostra4[ostra4$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR"
ostra4[ostra4$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR"

summary(factor(ostra4$Dataset))

map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)), data = ostra4, alpha = .5) +
    scale_colour_brewer(name = "Source", palette = "Paired") + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group), 
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")   

ggsave(plot = map, filename = "map_Ostracoda_abundances_11_07_22.jpg", dpi = 300, width = 15, height = 12)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------