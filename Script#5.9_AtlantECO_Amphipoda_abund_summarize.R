
##### ATLANTECO SCRIPT 5.9 ----------------------------------------------------------------------------------------------------------------------------
##### 08/07/2022: R Script to create the AtlantECO-BASE1 abundance only dataset for Amphipoda ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted datasets, examine their format (should be the same for everyone)
# - Combine them, and clean dataset (units, data sources, ducplicates, add bathymetry and Longhurst provinces)

### Latest update: 08/07/2022

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

### List the datasets to be merged to create the abundance data compilation for: Amphipoda
# NMFS-COPEPOD x
# SO-CPR x
# AusCPR x

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) NMFS-COPEPOD
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA/") ; dir()
amphi <- get(load("COPEPOD-NOAA_Amphipoda_4217000_reformatted+WoRMScheck_15_11_21.Rdata"))
dim(amphi) # 27'140

# Check if only Chaetognatha
unique(amphi$Phylum) 
unique(amphi$Class)  
unique(amphi$Order) # has Isopoda too
unique(amphi$Family)
unique(amphi$Genus) 
unique(amphi[is.na(amphi$Phylum),'ScientificName'])
### Opunorchestia rectimana is a terrestrial species. Remove
amphi <- amphi[!is.na(amphi$Phylum),]
amphi <- amphi[-which(amphi$Order == "Isopoda"),]
#dim(amphi) 27123

# Check distribution of MeasurementValue
summary(amphi$MeasurementValue) # 2116 NaNs to remove and clear outliers 
amphi2 <- amphi[!is.na(amphi$MeasurementValue),]
dim(amphi2) # 25'007

# Check origin of data 
amphi2 %>% count(amphi2$OrigCollectionID, sort = T)
amphi2 %>% count(amphi2$SamplingProtocol, sort = T) 


# Quickmap
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#             data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#         geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(OrigCollectionID)),
#             data = amphi2, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") +
#         coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#         theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# # # Good.

### Check colnames and some if necessary
colnames(amphi2)
amphi2 <- select(amphi2, -obisID)
amphi2 <- select(amphi2, -WoRMS_status.1)

### Add mesh (from 'SamplingProtocol') size after 'SamplingProtocol'
# head(do.call(rbind, strsplit(x = as.character(unique(cope2$SamplingProtocol)), split = "; ", fixed = F)))
meshes <- do.call(rbind, strsplit(x = amphi2$SamplingProtocol, split = "; ", fixed = F))[,3]
# unique(meshes)
# length(meshes) == nrow(cope2)
# Remove 'mesh= ' and '1/2m opening @ ' and 'um)'
meshes <- str_replace_all(meshes, "mesh= ", "")
meshes <- str_replace_all(meshes, "1/2m opening @ ", "")
meshes[meshes == "333um)"] <- "333"
# unique(meshes)
meshes[meshes == "NA"] <- NA
# summary(factor(meshes))  # Looks ok
amphi2 <- amphi2 %>% add_column(MeshSize = meshes, .after = "SamplingProtocol")

# Check ScientificNames and WoRMS_ID and some other headers
unique(amphi2$ScientificName) # Looks good
unique(amphi2$WoRMS_ID) # str(cope2$WoRMS_ID)  # should be character string throughout all files
unique(amphi2$WoRMS_status) 
unique(amphi2$LifeForm) # important for C biomass conversion

# Tally taxonranks
amphi2 %>% count(TaxonRank, sort = T) # Ok good

### OK, add the CPR surveys 

### ----------------------------------------------------------

### 2°) NAtl+NPac CPR
#setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2") ; dir()
#cpr <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
#dim(cpr) # 712'720
# Subset Amphipoda
#unique(cpr$Order)
#unique(cpr$Class)
#unique(cpr$Family)
# intersect(unique(amphi2$Order), unique(cpr$Order))
### --> No amphipoda...
# rm(cpr) ; gc()

### ----------------------------------------------------------

### 3°) SO-CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))

# Subset Class Chaetognatha
unique(so.cpr$Order)
so.cpr <- so.cpr[so.cpr$Order == "Amphipoda" & !is.na(so.cpr$Order),]
unique(so.cpr$Class)
unique(so.cpr$Order)
unique(so.cpr$Genus)
unique(so.cpr$ScientificName) # OK
dim(so.cpr) # 1'241'208

# Check colnames, measured values etc.
# setdiff(colnames(amphi2), colnames(so.cpr)) # missing MeshSize
# setdiff(colnames(so.cpr), colnames(amphi2)) # obisID to rm in cpr
so.cpr <- dplyr::select(so.cpr, -c(obisID,Temperature_Celsius,Salinity,PAR_microE_m2_s1,Fluorescence,PCI))
# Add MeshSize unique(cpr$basisOfRecord)
so.cpr <- so.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(so.cpr$MeasurementValue) # No NAs

# Check eventDate and WoRMS_ID before rbinding with 'amphi2'
# str(so.cpr) # Looks ok

# Rbind
amphi3 <- rbind(amphi2, so.cpr)
dim(amphi3) # 1'266'215
rm(amphi,so.cpr) ; gc()


### ----------------------------------------------------------

### 4°) AusCPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
aus.cpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))

# Subset Chaetognatha
unique(aus.cpr$Order) 
aus.cpr <- aus.cpr[aus.cpr$Order == "Amphipoda" & !is.na(aus.cpr$Order),]
unique(aus.cpr$Class)
unique(aus.cpr$Order)
unique(aus.cpr$Genus)
unique(aus.cpr$ScientificName) # OK
dim(aus.cpr) # 183'456

# Check colnames, measured values etc.
# setdiff(colnames(cope3), colnames(aus.cpr)) # missing MeshSize
# setdiff(colnames(aus.cpr), colnames(cope3)) # obisID to rm in cpr
aus.cpr <- dplyr::select(aus.cpr, -obisID)
# Add MeshSize unique(aus.cpr$basisOfRecord)
aus.cpr <- aus.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(aus.cpr$MeasurementValue) # No NA to remove 
# aus.cpr <- aus.cpr[!is.na(aus.cpr$MeasurementValue),]

# Rbind
amphi4 <- rbind(amphi3, aus.cpr)
dim(amphi4) # 1'449'671
rm(amphi2,aus.cpr) ; gc()


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
# unique(amphi4$BathySource)
# unique(amphi4$Bathymetry)
amphi4$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
amphi4$Bathymetry <- raster::extract(x = ras.bathy, y = amphi4[,c("decimalLongitude","decimalLatitude")])
summary(amphi4$Bathymetry)

### Add Longhurst provinces
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
amphi4$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = amphi4[,c("decimalLongitude","decimalLatitude")]))
unique(amphi4$LonghurstProvince) ; summary(factor(amphi4$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(amphi4$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    amphi4[amphi4$LonghurstProvince == i & !is.na(amphi4$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(amphi4$LonghurstProvince))
# 13312 NAs (too coastal points) --> (13312/nrow(amphi4))*100 = 0.9% of the data 

### Do some final checks
# summary(amphi4$MeasurementValue)
# unique(amphi4$MeasurementUnit)
amphi4$MeasurementUnit <- "#/m3"

unique(amphi4$WoRMS_status)
unique(amphi4$ScientificName)
unique(amphi4$LifeForm)

# Tally taxonrank
counts <- data.frame( amphi4 %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(amphi4)) )
counts[order(counts$n, decreasing = T),]
# 5     Species 724142 0.499521616
# 2       Genus 484975 0.334541424
# 4       Order  84508 0.058294606
# 6    Suborder  53594 0.036969768
# 7 Superfamily  51717 0.035674991
# 1      Family  36387 0.025100178
# 3  Infraorder  14348 0.009897418

### Check coordinates etc.
summary(amphi4[,c("decimalLatitude","decimalLongitude","Depth","MinDepth","MaxDepth","Day","Month","Year")])
# Discard the 5 NAs in MaxDepth 
amphi4 <- amphi4[!is.na(amphi4$MaxDepth),]

### Drop useless cols
# colnames(cope4)
amphi4 <- dplyr::select(amphi4,-c(ProjectID,ProjectWP,DataSilo))
# Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
save(amphi4, file = "AtlantECO-BASEv1_dataset_Amphipoda_abundances_08_07_22.RData")
write.table(amphi4, file = "AtlantECO-BASEv1_dataset_Amphipoda_abundances_08_07_22.csv", sep = ";")

# dim(get(load("AtlantECO-BASEv1_dataset_Amphipoda_abundances_08_07_22.RData")))

### Print a map of occ distrib by colouring as a fun of datasets (to check for )
unique(amphi4$BiblioCitation)
amphi4$Dataset <- NA
amphi4[amphi4$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD"
amphi4[amphi4$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR"
amphi4[amphi4$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR"

summary(factor(amphi4$Dataset))

map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)), data = amphi4, alpha = .5) +
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

ggsave(plot = map, filename = "map_Amphipoda_abundances_08_07_22.jpg", dpi = 300, width = 15, height = 12)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------