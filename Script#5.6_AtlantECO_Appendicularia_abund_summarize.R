
##### ATLANTECO SCRIPT 5.6 ----------------------------------------------------------------------------------------------------------------------------
##### 28/04/2022: R Script to create the AtlantECO-BASE1 abundance only dataset for Appendicularia/Larvacea ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted datasets, examine their format (should be the same for everyone)
# - Combine them, and clean dataset (units, data sources, duplicates, add bathymetry and Longhurst provinces)

### Latest update: 28/04/2022

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

### List the datasets to be merged to create the abundance data compilation for: Copepods
# NMFS-COPEPOD 
# SO-CPR 
# AusCPR 

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) NMFS-COPEPOD
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA/") ; dir()
cope <- get(load("COPEPOD-NOAA_Urochordata_4350000_reformatted+WoRMScheck_12_11_21.Rdata"))
dim(cope) # 57'958 obs

# Subset Appendicularia
unique(cope$Class) 
unique(cope[is.na(cope$Class),'ScientificName'])
#unique(cope$Order) 
#unique(cope[is.na(cope$Order),'ScientificName'])
cope <- cope[!is.na(cope$Class) & cope$Class == "Appendicularia",]
dim(cope) # 29'893 obs

# Check distribution of MeasurementValue
summary(cope$MeasurementValue) # 2160 NaNs to remove and clear outliers 
cope2 <- cope[!is.na(cope$MeasurementValue),]
dim(cope2) # 27'733

# Check origin of data 
cope2 %>% count(basisOfRecord, sort = T)
cope2 %>% count(SamplingProtocol, sort = T) 
cope2 %>% count(basisOfRecord, sort = T) 

# Quickmap
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#           data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#       geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(basisOfRecord)),
#           data = cope2, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") +
#       coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#       theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
#           panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# # Good.

### Check colnames and some if necessary
colnames(cope2)
cope2 <- select(cope2, -obisID)

### Add mesh (from 'SamplingProtocol') size after 'SamplingProtocol'
head(do.call(rbind, strsplit(x = as.character(unique(cope2$SamplingProtocol)), split = "; ", fixed = F)))
meshes <- do.call(rbind, strsplit(x = cope2$SamplingProtocol, split = "; ", fixed = F))[,3]
# unique(meshes)
# length(meshes) == nrow(cope2)
# Remove 'mesh= ' and '1/2m opening @ ' and 'um)'
meshes <- str_replace_all(meshes, "mesh= ", "")
meshes <- str_replace_all(meshes, "1/2m opening @ ", "")
meshes[meshes == "333um)"] <- "333"
unique(meshes)
meshes[meshes == "-999"] <- NA
# unique(meshes)
# summary(factor(meshes))  # Looks ok
cope2 <- cope2 %>% add_column(MeshSize = meshes, .after = "SamplingProtocol")

# Check ScientificNames and WoRMS_ID and some other headers
unique(cope2$ScientificName) # Looks good
unique(cope2$WoRMS_ID) # str(cope2$WoRMS_ID)  # should be character string throughout all files
unique(cope2$WoRMS_status) 
unique(cope2$LifeForm) # important for C biomass conversion
# Copepodites stages?
unique(cope2[cope2$LifeForm == "c1-3","ScientificName"]) # dim( cope2[cope2$LifeForm == "c1-3" & !is.na(cope2$LifeForm),] )
unique(cope2[cope2$LifeForm == "copepodite (unspecified)","ScientificName"])
# dim( cope2[cope2$LifeForm == "copepodite (unspecified)" & !is.na(cope2$LifeForm),] )

# Tally taxonranks
cope2 %>% count(TaxonRank, sort = T) # Ok good

### OK, add the 2 CPR surveys available

### ----------------------------------------------------------

### 2°) SO-CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))

# Subset Class Appendicularia
unique(so.cpr$Phylum)
unique(so.cpr$Class)
so.cpr <- so.cpr[so.cpr$Class == "Appendicularia" & !is.na(so.cpr$Class),]
unique(so.cpr$Class)
unique(so.cpr$Order)
unique(so.cpr$ScientificName) # OK
dim(so.cpr) # 155'151

# Check colnames, measured values etc.
# setdiff(colnames(cope2), colnames(so.cpr)) # missing MeshSize
# setdiff(colnames(so.cpr), colnames(cope2)) # obisID to rm in cpr
so.cpr <- dplyr::select(so.cpr, -c(obisID,Temperature_Celsius,Salinity,PAR_microE_m2_s1,Fluorescence,PCI))
# Add MeshSize unique(cpr$basisOfRecord)
so.cpr <- so.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(so.cpr$MeasurementValue) # No NAs

# Check eventDate and WoRMS_ID before rbinding with 'cope2'
# str(so.cpr) # Looks ok

# Rbind
cope3 <- rbind(cope2, so.cpr)
# dim(cope3) # 182'884
rm(cope,so.cpr) ; gc()


### ----------------------------------------------------------

### 3°) AusCPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
aus.cpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))

# Subset Appendicularia
unique(aus.cpr$Phylum) 
unique(aus.cpr$Class) 
aus.cpr <- aus.cpr[aus.cpr$Class == "Appendicularia" & !is.na(aus.cpr$Class),]
unique(aus.cpr$Class)
unique(aus.cpr$Order)
unique(aus.cpr$ScientificName) # OK
dim(aus.cpr) # 35'280

# Check colnames, measured values etc.
# setdiff(colnames(cope3), colnames(aus.cpr)) # missing MeshSize
# setdiff(colnames(aus.cpr), colnames(cope3)) # obisID to rm in cpr
aus.cpr <- dplyr::select(aus.cpr, -obisID)
# Add MeshSize unique(aus.cpr$basisOfRecord)
aus.cpr <- aus.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
summary(aus.cpr$MeasurementValue) # 1 NA to remove 
aus.cpr <- aus.cpr[!is.na(aus.cpr$MeasurementValue),]

# Rbind
cope4 <- rbind(cope3, aus.cpr)
dim(cope4) # 218'163
rm(cope2,aus.cpr) ; gc()


### ----------------------------------------------------------

### 4°) Final checkings, add bathymetry & longhurst provinces

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
cope4$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
cope4$Bathymetry <- raster::extract(x = ras.bathy, y = cope4[,c("decimalLongitude","decimalLatitude")])
# summary(cope4$Bathymetry)

### Add Longhurst provinces
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
cope4$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = cope4[,c("decimalLongitude","decimalLatitude")]))
unique(cope4$LonghurstProvince) ; summary(factor(cope4$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(cope4$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    cope4[cope4$LonghurstProvince == i & !is.na(cope4$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(cope4$LonghurstProvince))
# 3246 NAs (too coastal points) --> (3246/nrow(cope4))*100 = 1.49% of the data 

### Do some final checks
# summary(cope4$MeasurementValue)
# unique(cope4$MeasurementUnit)
# cope4$MeasurementUnit <- "#/m3"

# Tally taxonrank
counts <- data.frame( cope4 %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(cope4)) )
counts[order(counts$n, decreasing = T),]
# 2     Genus 193503 0.886965251
# 1    Family  14437 0.066175291
# 4   Species   8318 0.038127455
# 3     Order   1905 0.008732003
# unique(cope4$ScientificName)

### Check coordinates etc.
summary(cope4[,c("decimalLatitude","decimalLongitude","Depth","MaxDepth","Day","Month","Year")])
# Discard the 4 NAs in MaxDepth 
cope4 <- cope4[!is.na(cope4$MaxDepth),]

### Drop useless cols
# colnames(cope4)
cope4 <- dplyr::select(cope4,-c(ProjectID,ProjectWP,DataSilo))
# Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
save(cope4, file = "AtlantECO-BASEv1_dataset_Appendicularia_abundances_28_04_22.RData")
write.table(cope4, file = "AtlantECO-BASEv1_dataset_Appendicularia_abundances_28_04_22.csv", sep = ";")

# dim(get(load("AtlantECO-BASEv1_dataset_Appendicularia_abundances_28_04_22.RData")))

### Print a map of occ distrib by colouring as a fun of datasets (to check for )
unique(cope4$BiblioCitation)
cope4$Dataset <- NA
cope4[cope4$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD"
cope4[cope4$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR"
cope4[cope4$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR"
summary(factor(cope4$Dataset))
# AusCPR     NMFS-COPEPOD   SO-CPR 
#  35279        27729       155151 

map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)), data = cope4, alpha = .5) +
    scale_colour_brewer(name = "Source", palette = "Paired") + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group), 
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") #+
    #facet_wrap(.~ factor(Dataset))    

ggsave(plot = map, filename = "map_Appendicularia_abundances_28_04_22.jpg", dpi = 300, width = 15, height = 12)

### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------