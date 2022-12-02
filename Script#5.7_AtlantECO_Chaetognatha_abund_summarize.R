
##### ATLANTECO SCRIPT 5.6 ----------------------------------------------------------------------------------------------------------------------------
##### 19/04/2022: R Script to create the AtlantECO-BASE1 abundance only dataset for Chaetognatha ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted datasets, examine their format (should be the same for everyone)
# - Combine them, and clean dataset (units, data sources, ducplicates, add bathymetry and Longhurst provinces)

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
# NMFS-COPEPOD x
# SO-CPR x
# AusCPR x
# Brandao, Benedetti et al. (2021) x

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) NMFS-COPEPOD
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA/") ; dir()
cope <- get(load("COPEPOD-NOAA_Chaetognatha_4320000_reformatted+WoRMScheck_15_11_21.Rdata"))
dim(cope) # 24'945

# Check if only Chaetognatha
unique(cope$Phylum) 
unique(cope$Class)  
unique(cope$Genus) 
unique(cope[is.na(cope$Class),'ScientificName'])
### OK. No need to subset

# Check distribution of MeasurementValue
summary(cope$MeasurementValue) # 1873 NaNs to remove and clear outliers 
cope2 <- cope[!is.na(cope$MeasurementValue),]
dim(cope2) # 23'072

# Check origin of data 
cope2 %>% count(basisOfRecord, sort = T)
cope2 %>% count(SamplingProtocol, sort = T) 
cope2 %>% count(basisOfRecord, sort = T) 

# Quickmap
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#            data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#        geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(basisOfRecord)),
#            data = cope2, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") +
#        coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#        theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
#            panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# # Good.

### Check colnames and some if necessary
colnames(cope2)
cope2 <- select(cope2, -obisID)

### Add mesh (from 'SamplingProtocol') size after 'SamplingProtocol'
# head(do.call(rbind, strsplit(x = as.character(unique(cope2$SamplingProtocol)), split = "; ", fixed = F)))
meshes <- do.call(rbind, strsplit(x = cope2$SamplingProtocol, split = "; ", fixed = F))[,3]
# unique(meshes)
# length(meshes) == nrow(cope2)
# Remove 'mesh= ' and '1/2m opening @ ' and 'um)'
meshes <- str_replace_all(meshes, "mesh= ", "")
meshes <- str_replace_all(meshes, "1/2m opening @ ", "")
meshes[meshes == "333um)"] <- "333"
# unique(meshes)
meshes[meshes == "NA"] <- NA
# summary(factor(meshes))  # Looks ok
cope2 <- cope2 %>% add_column(MeshSize = meshes, .after = "SamplingProtocol")

# Check ScientificNames and WoRMS_ID and some other headers
unique(cope2$ScientificName) # Looks good
unique(cope2$WoRMS_ID) # str(cope2$WoRMS_ID)  # should be character string throughout all files
unique(cope2$WoRMS_status) 
unique(cope2$LifeForm) # important for C biomass conversion

# Tally taxonranks
cope2 %>% count(TaxonRank, sort = T) # Ok good

### OK, add the 2 CPR surveys + Tara Oceans

### ----------------------------------------------------------

### 2°) SO-CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))

# Subset Class Chaetognatha
unique(so.cpr$Phylum)
so.cpr <- so.cpr[so.cpr$Phylum == "Chaetognatha" & !is.na(so.cpr$Phylum),]
unique(so.cpr$Class)
unique(so.cpr$Order)
unique(so.cpr$ScientificName) # OK
dim(so.cpr) # 310'302

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
dim(cope3) # 333'374
rm(cope,so.cpr) ; gc()


### ----------------------------------------------------------

### 3°) AusCPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
aus.cpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))

# Subset Chaetognatha
unique(aus.cpr$Phylum) 
aus.cpr <- aus.cpr[aus.cpr$Phylum == "Chaetognatha" & !is.na(aus.cpr$Phylum),]
unique(aus.cpr$Class)
unique(aus.cpr$Order)
unique(aus.cpr$ScientificName) # OK
dim(aus.cpr) # 49'392

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
cope4 <- rbind(cope3, aus.cpr)
dim(cope4) # 382'766
rm(cope2,aus.cpr) ; gc()


### ----------------------------------------------------------

### 4°) TARA Oceans (Brandao, Benedetti et al., 2021)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Plankton_Imaging/") ; dir()
tara <- get(load("TARA_Oceans_plankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports_26_07_21.RData"))
dim(tara) # 15'908
str(tara)

# Subset copepods
# unique(tara$ScientificName)
# unique(tara$Phylum)
tara <- tara[tara$Phylum == "Chaetognatha" & !is.na(tara$Phylum),]
dim(tara) # 388

# Check colnames etc.
# setdiff(colnames(cope2), colnames(tara)) # missing MeshSize
# setdiff(colnames(tara), colnames(cope2)) # obisID to rm
tara <- dplyr::select(tara, -obisID)
# Add MeshSize after 'SamplingProtocol'
tara <- tara %>% add_column(MeshSize = NA, .after = "SamplingProtocol")
# unique(tara$basisOfRecord)
tara[tara$basisOfRecord == "Bongo+300micron mesh","MeshSize"] <- "300"
tara[tara$basisOfRecord == "Régent+680micron mesh","MeshSize"] <- "680"
tara[tara$basisOfRecord == "WP2+200micron mesh","MeshSize"] <- "200"
# Check
summary(factor(tara$MeshSize))

# Check distribution of Measurement
summary(tara$MeasurementValue) # No NAs

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
#str(tara) # Looks ok unique(tara$WoRMS_ID)
# Check ScientificName
#unique(tara$ScientificName) # OK

# Rbind
cope5 <- rbind(cope4, tara)
dim(cope5) # 383'154
rm(cope3,tara) ; gc()


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
#unique(cope5$BathySource)
#unique(cope5$Bathymetry)
cope5$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
cope5$Bathymetry <- raster::extract(x = ras.bathy, y = cope5[,c("decimalLongitude","decimalLatitude")])
summary(cope5$Bathymetry)

### Add Longhurst provinces
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
cope5$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = cope5[,c("decimalLongitude","decimalLatitude")]))
unique(cope5$LonghurstProvince) ; summary(factor(cope5$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(cope5$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    cope5[cope5$LonghurstProvince == i & !is.na(cope5$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(cope5$LonghurstProvince))
# 3823 NAs (too coastal points) --> (3823/nrow(cope5))*100 = 1% of the data 

### Do some final checks
#summary(cope5$MeasurementValue)
#unique(cope5$MeasurementUnit)
cope5$MeasurementUnit <- "#/m3"

# Tally taxonrank
counts <- data.frame( cope5 %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(cope5)) )
counts[order(counts$n, decreasing = T),]
# 5   Species 195658 0.5106511
# 4    Phylum  71439 0.1864498
# 3     Genus  62502 0.1631250
# 2    Family  51760 0.1350893
# 1     Class   1795 0.0046848

### Check coordinates etc.
summary(cope5[,c("decimalLatitude","decimalLongitude","Depth","MaxDepth","Day","Month","Year")])
# Discard the 3 NAs in MaxDepth 
cope5 <- cope5[!is.na(cope5$MaxDepth),]

### Drop useless cols
# colnames(cope4)
cope5 <- dplyr::select(cope5,-c(ProjectID,ProjectWP,DataSilo))
# Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
save(cope5, file = "AtlantECO-BASEv1_dataset_Chaetognatha_abundances_28_04_22.RData")
write.table(cope5, file = "AtlantECO-BASEv1_dataset_Chaetognatha_abundances_28_04_22.csv", sep = ";")

# dim(get(load("AtlantECO-BASEv1_dataset_Chaetognatha_abundances_28_04_22.RData")))

### Print a map of occ distrib by colouring as a fun of datasets (to check for )
unique(cope5$BiblioCitation)
cope5$Dataset <- NA
cope5[cope5$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD"
cope5[cope5$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR"
cope5[cope5$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR"
cope5[cope5$BiblioCitation == "Brandao,Benedetti et al. (2021) - Scientific Reports","Dataset"] <- "TARA Oceans"

summary(factor(cope5$Dataset))
# AusCPR     NMFS-COPEPOD  SO-CPR       TARA Oceans 
#  49392        23069       310302          388 

map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)), data = cope5, alpha = .5) +
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

ggsave(plot = map, filename = "map_Chaetognatha_abundances_28_04_22.jpg", dpi = 300, width = 15, height = 12)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------