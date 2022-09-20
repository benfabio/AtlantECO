
##### ATLANTECO SCRIPT 5.3 ----------------------------------------------------------------------------------------------------------------------------
##### 12/04/2022: R Script to create the AtlantECO-BASE1 abundance only dataset for Copepods (Hexanauplia) ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted datasets, examine their format (should be the same for everyone)
# - Combine them, and clean dataset (units, data sources, ducplicates, add bathymetry and Longhurst provinces)

### Latest update: 05/07/2022

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
# CPR x
# SO-CPR x
# AusCPR x
# Cornils et al. 2018 x
# Brandao, Benedetti et al. 2021 x 
# Becker at al. 2021 x

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) NMFS-COPEPOD
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA/") ; dir()
cope <- get(load("COPEPOD-NOAA_Copepoda_4212000_reformatted+WoRMScheck_19_11_21.Rdata"))
dim(cope) # 588'895
# Subset Copepoda
# unique(cope$Class) # NA?
# head(cope[is.na(cope$Class),]) ; dim(cope[is.na(cope$Class),])
# Non existing species. Remove.
cope <- cope[-which(cope$ScientificName == "Macrosetella sulcata"),]
# dim(cope) 588 760

# Check distribution of Measurement
# summary(cope$MeasurementValue) # 81 943 NaNs to remove and clear outliers 
# summary(factor(cope$MeasurementUnit)) # #/m3
cope2 <- cope[!is.na(cope$MeasurementValue),]
# dim(cope2) # 506'817

# Check origin of data (to identify old CPR version and remove it - should be in the top contributors to total N so near the top of the lists below)
cope2 %>% count(basisOfRecord, sort = T)
cope2 %>% count(SamplingProtocol, sort = T) 
cope2 %>% count(InstitutionCode, sort = T) 
cope2 %>% count(basisOfRecord, sort = T) 

### Remove CPR observations
cope3 <- cope2[!(cope2$basisOfRecord %in% c("Tow type= horizontal; with: Continuous Plankton Recorder (CPR) MBA/SAHFOS","Tow type= horizontal; with: Continuous Plankton Sampler (model not specified)") ),]
dim(cope3) # 463 977
nrow(cope2)-nrow(cope3) # 42 840
rm(cope) ; gc()

# Quickmap
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(basisOfRecord)),
#         data = cope3, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# Good. 

### Check colnames and some if necessary
colnames(cope3)
# head(cope3)
# unique(cope3$obisID) # to remove
# unique(cope3$DatasetKey) # keep
cope3 <- select(cope3, -obisID)

### Add mesh (from 'SamplingProtocol') size after 'SamplingProtocol'
#head(do.call(rbind, strsplit(x = as.character(unique(cope3$SamplingProtocol)), split = "; ", fixed = F)))
meshes <- do.call(rbind, strsplit(x = cope3$SamplingProtocol, split = "; ", fixed = F))[,3]
# length(meshes); unique(meshes) # length(meshes) == nrow(cope3)
# Remove 'mesh= ' and '1/2m opening @ ' and 'um)'
meshes <- str_replace_all(meshes, "mesh= ", "")
meshes <- str_replace_all(meshes, "1/2m opening @ ", "")
meshes[meshes == "333um)"] <- "333"
meshes[meshes == "-999"] <- NA
# unique(meshes)
# summary(factor(meshes))  # Looks ok
cope3 <- cope3 %>% add_column(MeshSize = meshes, .after = "SamplingProtocol")
# dim(cope3)

# Check ScientificNames and WoRMS_ID and some other headers
unique(cope3$ScientificName)
unique(cope3$WoRMS_ID) # str(cope3$WoRMS_ID)  # should be character string throughout all files
unique(cope3$LifeForm) # important for C biomass conversion
# Tally taxonranks
cope3 %>% count(TaxonRank, sort = T) # Ok good
# OK. Add CPR suveys now.

### ----------------------------------------------------------

### 2°) NAtl+NPac CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2") ; dir()
cpr <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
# dim(cpr) # 712'720
# Subset Hexanauplia
cpr <- cpr[cpr$Class == "Hexanauplia",]
# dim(cpr) # 665'921

# Check colnames, measured values etc.
# setdiff(colnames(cope3), colnames(cpr)) # missing MeshSize
# setdiff(colnames(cpr), colnames(cope3)) # obisID to rm in cpr
cpr <- select(cpr, -obisID)
# Add MeshSize unique(cpr$basisOfRecord)
cpr <- cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
summary(cpr$MeasurementValue) # 99 833 NaNs to remove and clear outliers 
cpr <- cpr[!is.na(cpr$MeasurementValue),]
# dim(cpr) # 566'088

# Check eventDate and WoRMS_ID before rbinding with 'cope3'
# str(cpr) # Looks ok

# Rbind
cope4 <- rbind(cope3, cpr)
# dim(cope4) # 1'030'065

# Quick map with all datasets
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope3, alpha = 0.2, colour = "grey50") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cpr, alpha = 0.3, colour = "red") +
#     geom_polygon(aes(x = long, y = lat, group = group),
#             data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")

### Looks OK. Proceed to 2 other CPR surveys.

### ----------------------------------------------------------

### 3°) SO-CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
# dim(so.cpr) # 15'204'798
# Subset Hexanauplia
# unique(so.cpr$Class)
so.cpr <- so.cpr[so.cpr$Class == "Hexanauplia",]
# dim(so.cpr) # 6'568'059

# Check colnames, measured values etc.
setdiff(colnames(cope4), colnames(so.cpr)) # missing MeshSize
setdiff(colnames(so.cpr), colnames(cope4)) # obisID to rm in cpr
so.cpr <- dplyr::select(so.cpr, -c(obisID,Temperature_Celsius,Salinity,PAR_microE_m2_s1,Fluorescence,PCI))
# Add MeshSize unique(cpr$basisOfRecord)
so.cpr <- so.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
summary(so.cpr$MeasurementValue) # 620604 NaNs to remove and clear outliers 
so.cpr <- so.cpr[!is.na(so.cpr$MeasurementValue),]
# dim(so.cpr) # 5'947'455

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
# str(so.cpr) # Looks ok
# Check ScientificName
# unique(so.cpr$ScientificName)  #OK

# Rbind
cope5 <- rbind(cope4, so.cpr)
dim(cope5) # 6'977'520
rm(cope2,cope3,so.cpr) ; gc()


### ----------------------------------------------------------

### 4°) AusCPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
aus.cpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
# dim(aus.cpr) # 7'373'520
# Subset Hexanauplia 
unique(aus.cpr$Class) # NA?
head(aus.cpr[is.na(aus.cpr$Class),]) ; dim(aus.cpr[is.na(aus.cpr$Class),])
aus.cpr <- aus.cpr[aus.cpr$Class == "Hexanauplia",]
# dim(aus.cpr) # 5'390'784

# Check colnames, measured values etc.
setdiff(colnames(cope5), colnames(aus.cpr)) # missing MeshSize
setdiff(colnames(aus.cpr), colnames(cope5)) # obisID to rm in cpr
aus.cpr <- dplyr::select(aus.cpr, -obisID)
# Add MeshSize unique(aus.cpr$basisOfRecord)
aus.cpr <- aus.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
summary(aus.cpr$MeasurementValue) # 620604 NaNs to remove and clear outliers 
aus.cpr <- aus.cpr[!is.na(aus.cpr$MeasurementValue),]
dim(aus.cpr) # 5'052'096

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
# str(aus.cpr) # Looks ok
# Check ScientificName
# unique(aus.cpr$ScientificName)  #OK

# Rbind
cope6 <- rbind(cope5, aus.cpr)
dim(cope6) # 12'029'616
rm(cope4,aus.cpr) ; gc()

# Save tempo file 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
save(cope6, file = "tempo_file_Hexanauplia_NMFS-COPEPOD+CPRsurveys_toupdate_12_04_22.Rdata")


### ----------------------------------------------------------

### 12/04/22: Re-load temporary file and continue
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
cope <- get(load("tempo_file_Hexanauplia_NMFS-COPEPOD+CPRsurveys_toupdate_12_04_22.Rdata"))
# dim(cope) # 12'029'616
# str(cope)

### 5°) Cornils et al. 2018
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021") ; dir()
so <- get(load("ZOObase_Cornils&al._2018_SO_Copepods_reformated+WoRMScheck_04_06_2021.RData"))
# dim(so) # 260 826
# str(so)
# Only has copepods inside

# Check colnames, measured values etc.
setdiff(colnames(cope), colnames(so)) # missing MeshSize
setdiff(colnames(so), colnames(cope)) # obisID to rm 
so <- dplyr::select(so, -obisID)
# Add MeshSize after 'SamplingProtocol'
# unique(so$SamplingProtocol)
# Problem is that string actually has several numerics so parse_number() might not be ideal. But mesh size can be either: 300, 500, 100, 200, 250 or 55
# So maybe use a grepl()
so <- so %>% add_column(MeshSize = NA, .after = "SamplingProtocol")
# Fill it in a for loop
# i <- unique(so$SamplingProtocol)[1]
for(i in unique(so$SamplingProtocol)) {
    
    if( grepl('55',i) ) {
        so[so$SamplingProtocol == i,"MeshSize"] <- "55"
    } else if(grepl('100',i)) {
        so[so$SamplingProtocol == i,"MeshSize"] <- "100"
    } else if(grepl('200',i)) {
        so[so$SamplingProtocol == i,"MeshSize"] <- "200"
    } else if(grepl('250',i)) {
        so[so$SamplingProtocol == i,"MeshSize"] <- "250"
    } else if(grepl('300',i)) {
        so[so$SamplingProtocol == i,"MeshSize"] <- "300"
    } else if(grepl('500',i)) {
        so[so$SamplingProtocol == i,"MeshSize"] <- "500"
    }
    
} # eo for loop - i in SamplingProtocol
# Check
# summary(factor(so$MeshSize))
# Looks like it worked

# Check distribution of Measurement
# summary(so$MeasurementValue) # No NAs

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
str(so) # Looks ok unique(so$WoRMS_ID)
so$WoRMS_ID <- as.character(so$WoRMS_ID)
# Check ScientificName
unique(so$ScientificName) # OK

# Rbind
cope2 <- rbind(cope, so)
dim(cope2) # 12'290'442
rm(so) ; gc()


### ----------------------------------------------------------

### 6°) Brandāo, Benedetti et al. 2021 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Plankton_Imaging/") ; dir()
tara <- get(load("TARA_Oceans_plankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports_26_07_21.RData"))
dim(tara) # 15'908
str(tara)

# Subset copepods
# unique(tara$ScientificName)
# unique(tara$Class)
tara <- tara[tara$Class == "Hexanauplia" & !is.na(tara$Class),]
dim(tara) # 10'088

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
# summary(factor(tara$MeshSize))

# Check distribution of Measurement
# summary(tara$MeasurementValue) # No NAs

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
str(tara) # Looks ok unique(tara$WoRMS_ID)
# Check ScientificName
unique(tara$ScientificName) # OK
### ! Remember: Categories are nested here ! (Copepoda = summ of all other sub groups, it is NOT a separate group of unidentified copepods)

# Rbind
cope3 <- rbind(cope2, tara)
dim(cope3) # 12'300'530
rm(cope,tara) ; gc()

### ----------------------------------------------------------

### 7°) Becker at al. 2021 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/Copepod_species_SAO_Becker&al._2021") ; dir()
beckie <- get(load("table_Becker2021_PiO_copepods_abund_SAO_reformatted+WoRMScheck_14_02_22.Rdata"))
dim(beckie) # 8'732
str(beckie)
# unique(beckie$Class) # NA?
# head(beckie[is.na(beckie$Class),]) # 'Copepoda all'
beckie <- beckie[beckie$Class == "Hexanauplia" & !is.na(beckie$Class),]
# 8'695 rows

# Check colnames etc.
# setdiff(colnames(cope3), colnames(beckie)) # missing MeshSize
# setdiff(colnames(beckie), colnames(cope3)) # obisID to rm 
beckie <- dplyr::select(beckie, -obisID)
# Add MeshSize after 'SamplingProtocol'
unique(beckie$basisOfRecord) # All are Vertical WP2 net tow (200µm mesh)
beckie <- beckie %>% add_column(MeshSize = "200", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(beckie$MeasurementValue) # No NAs
# Check ScientificName
# unique(beckie$ScientificName) # OK

# Rbind
cope4 <- rbind(cope3, beckie)
dim(cope4) # 
rm(cope2,beckie) ; gc()

# Quickmap of data sources
# unique(cope4$BiblioCitation)
ggplot() + geom_polygon(aes(x = long, y = lat, group = group), 
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
     geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(BiblioCitation)),
        data = cope4, alpha = 0.25) + scale_colour_discrete(name = "Sources") +
     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
     theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# Good. 

# Save tempo file 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
save(cope4, file = "tempo_file_Hexanauplia_all_tofinish_12_04_22.Rdata")
# cope4 <- get(load("tempo_file_Hexanauplia_all_tofinish_12_04_22.Rdata"))
#  

### ----------------------------------------------------------

### 8°) Final check - add bathymetry and Longhurst provinces
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
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()

### Add bathymetry
unique(cope4$BathySource) # Wait, does not apply to all data
summary(cope4$Bathymetry) # Only apply below to NA
cope4[is.na(cope4$Bathymetry),"BathySource"] <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
cope4[is.na(cope4$Bathymetry),"Bathymetry"] <- raster::extract(x = ras.bathy, y = cope4[is.na(cope4$Bathymetry),c("decimalLongitude","decimalLatitude")])

### Add Longhurst provinces: since the raster only has the provinces' indices, first extract those (maybe as characters, easier for replacement after) and then convert them back to the actual provinces' names using the levels of the raster layer:
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
cope4$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = cope4[,c("decimalLongitude","decimalLatitude")]))
unique(cope4$LonghurstProvince) ; summary(factor(cope4$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
# i <- "46"  # for testing 
for(i in unique(na.omit(cope4$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    cope4[cope4$LonghurstProvince == i & !is.na(cope4$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(cope4$LonghurstProvince))
# 346887 NAs (too coastal points) --> (346887/nrow(cope4))*100 = 2.82% of the data 

### Do some final checks
summary(cope4$MeasurementValue)
# unique(cope4$MeasurementUnit)
cope4$MeasurementUnit <- "#/m3"

# Tally taxonrank
counts <- data.frame( cope4 %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(cope4)) )
counts[order(counts$n, decreasing = T),]
# Good
# - 66% are species-level
# - 27% are genus-level
# - 1.8% are family-level
# - 1.7% are order-level

### 13/04/22: Forgot to check the sampling depth fields and assess potential duplicates yesterday
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance")
cope4 <- get(load("AtlantECO-BASEv1_dataset_Hexanauplia_abundances_12_04_22.RData"))
summary(cope4[,c("Depth","MinDepth","MaxDepth")])
dim(cope4) # 12309225
# Only 2-5 obs have NA in "MinDepth","MaxDepth
# cope4[is.na(cope4$MaxDepth),] # OK to rm
cope4 <- cope4[!is.na(cope4$MinDepth),]
cope4 <- cope4[!is.na(cope4$MaxDepth),]
dim(cope4) # 12309220

### Attribute an occurrenceID? (for duplicates)
cope4$occurrenceID <- factor(paste(round(cope4$decimalLongitude,2), round(cope4$decimalLatitude,2), cope4$Day, cope4$Month, cope4$Year, cope4$MaxDepth, cope4$ScientificName, cope4$MeasurementValue, sep = "_"))
# How many unique obs?
length(unique(cope4$occurrenceID)) # 9'080'989 unique obs. 
(length(unique(cope4$occurrenceID))/nrow(cope4))*100 
### --> 26% overlap 

cope4 <- cope4[!duplicated(cope4$occurrenceID),]
dim(cope4) # 9080989 Good


### Drop useless cols
# colnames(cope4)
cope4 <- dplyr::select(cope4,-c(ProjectID,ProjectWP,DataSilo))
# Save
save(cope4, file = "AtlantECO-BASEv1_dataset_Hexanauplia_abundances_12_04_22.RData")
write.table(cope4, file = "AtlantECO-BASEv1_dataset_Hexanauplia_abundances_12_04_22.csv", sep = ";")

### Print a map of occ distrib by colouring as a fun of datasets (don't save in the actual table though, just for the plot)
#str(cope4$BiblioCitation)
#unique(cope4$BiblioCitation)
cope4$Dataset <- NA
cope4[cope4$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD"
cope4[cope4$BiblioCitation == "Johns D, Broughton D (2019): The CPR Survey. v1.2. Marine Biological Association. Dataset/Samplingevent. https://doi.org/10.17031/1629","Dataset"] <- "CPR"
cope4[cope4$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR"
cope4[cope4$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR"
cope4[cope4$BiblioCitation == "Cornils&al._2018_ESSD_doi.org/10.5194/essd-2018-36","Dataset"] <- "Cornils & al. (2018)"
cope4[cope4$BiblioCitation == "Brandao,Benedetti et al. (2021) - Scientific Reports","Dataset"] <- "Brandao & al. (2021)"
cope4[cope4$BiblioCitation == "Becker, \x83. C., Mazzocchi, M. G., de Macedo-Soares, L. C. P., Costa Brand\x8bo, M., & Santarosa Freire, A. (2021). Latitudinal gradient of copepod functional diversity in the South Atlantic Ocean. Progress in Oceanography, 199, 102710. doi:https://doi.org/10.1016/j.pocean.2021.102710","Dataset"] <- "Becker et al. (2021)"
summary(factor(cope4$Dataset))

map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)), data = cope4, alpha = .5) +
    scale_colour_brewer(name = "Source", palette = "Paired") + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group), 
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

ggsave(plot = map, filename = "map_Hexanauplia_abundances_13_04_22.jpg", dpi = 300, width = 15, height = 12)


### ----------------------------------------------------------------------------------------------------------------------------

### 05/07/22: Modify LifeStage values so they match the value sin COPEPEDIA tables --> facilitates identification of C mass conversion factor

### 'Sex' codes should be: "female" or "male"  
### 'LifeStage' codes should be:
# [1] "adult"         "Copepodites"   "Assumed adult" "NIII"         
# [5] "CIII"          "CV"            "Nauplius"      "CIV"          
# [9] "CI"            "NI"            "NII"           "NIV"          

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
cops <- get(load("AtlantECO-BASEv1_dataset_Hexanauplia_abundances_12_04_22.RData"))
dim(cops) # 9'080'989
colnames(cops)
# Check LifeForm values
unique(cops$LifeForm)

### List of terms to modify in LifeForm so it matches LifeStage & Sex in COPEPEDIA tables: 
cops2 <- cops
# nauplii --> "Nauplius"
cops2[grepl("nauplii",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("nauplii",cops2$LifeForm),"LifeForm"], "nauplii", "Nauplius")
# copepodites C1-C4 --> "Copepodites"
cops2[cops2$LifeForm == "copepodites C1-C4" & !is.na(cops2$LifeForm),"LifeForm"] <- "Copepodites"
# copepodites --> "Copepodites"
cops2[grepl("copepodites",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("copepodites",cops2$LifeForm),"LifeForm"], "copepodites", "Copepodites")
# copepodite --> "Copepodites"
cops2[grepl("copepodite",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("copepodite",cops2$LifeForm),"LifeForm"], "copepodite", "Copepodites")
# juvenile --> "Copepodites"
cops2[grepl("juvenile",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("juvenile",cops2$LifeForm),"LifeForm"], "juvenile", "Copepodites")
# Juvenile --> "Copepodites"
cops2[grepl("Juvenile",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("Juvenile",cops2$LifeForm),"LifeForm"], "Juvenile", "Copepodites")
# immature --> "Copepodites"
cops2[cops2$LifeForm == "immature" & !is.na(cops2$LifeForm),"LifeForm"] <- "Copepodites"
# Nauplius larva --> "Nauplius"
cops2[cops2$LifeForm == "Nauplius larva" & !is.na(cops2$LifeForm),"LifeForm"] <- "Nauplius"
# larva --> "Nauplius"
cops2[cops2$LifeForm == "larva" & !is.na(cops2$LifeForm),"LifeForm"] <- "Nauplius"
# CIV stage --> "CIV"
cops2[cops2$LifeForm == "CIV stage" & !is.na(cops2$LifeForm),"LifeForm"] <- "CIV"
# CV stage --> "CV"                                                                                             
cops2[cops2$LifeForm == "CV stage" & !is.na(cops2$LifeForm),"LifeForm"] <- "CV"

# c1-2 --> "CI-CII"       
cops2[cops2$LifeForm == "c1-2" & !is.na(cops2$LifeForm),"LifeForm"] <- "CI-CII" 
cops2[grepl("c1-2",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c1-2",cops2$LifeForm),"LifeForm"], "c1-2", "CI-CII")
                                    
# c2-3 --> "CII-CIII"
cops2[cops2$LifeForm == "c2-3" & !is.na(cops2$LifeForm),"LifeForm"] <- "CII-CIII"
cops2[grepl("c2-3",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c2-3",cops2$LifeForm),"LifeForm"], "c2-3", "CII-CIII")

# c4-5 --> "CIV-CV"
cops2[cops2$LifeForm == "c4-5" & !is.na(cops2$LifeForm),"LifeForm"] <- "CIV-CV"
cops2[grepl("c4-5",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c4-5",cops2$LifeForm),"LifeForm"], "c4-5", "CIV-CV")

# c1-4 --> "CI-CIV"
cops2[cops2$LifeForm == "c1-4" & !is.na(cops2$LifeForm),"LifeForm"] <- "CI-CIV"
cops2[grepl("c1-4",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c1-4",cops2$LifeForm),"LifeForm"], "c1-4", "CI-CIV")

# c1-3 --> "CI-CIII"
cops2[cops2$LifeForm == "c1-3" & !is.na(cops2$LifeForm),"LifeForm"] <- "CI-CIII"
cops2[grepl("c1-3",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c1-3",cops2$LifeForm),"LifeForm"], "c1-3", "CI-CIII")

# c3-4 --> "CIII-CIV"
cops2[cops2$LifeForm == "c3-4" & !is.na(cops2$LifeForm),"LifeForm"] <- "CIII-CIV"
cops2[grepl("c3-4",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c3-4",cops2$LifeForm),"LifeForm"], "c3-4", "CIII-CIV")

# c1-5 --> "Copepodites"
cops2[cops2$LifeForm == "c1-5" & !is.na(cops2$LifeForm),"LifeForm"] <- "Copepodites"
cops2[grepl("c1-5",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c1-5",cops2$LifeForm),"LifeForm"], "c1-5", "Copepodites")

# c3-6 --> "CIII-CVI"
cops2[cops2$LifeForm == "c3-6" & !is.na(cops2$LifeForm),"LifeForm"] <- "CIII-CVI"
cops2[grepl("c3-6",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c3-6",cops2$LifeForm),"LifeForm"], "c3-6", "CIII-CVI")

# c5-6 --> "CV-CVI"
cops2[cops2$LifeForm == "c5-6" & !is.na(cops2$LifeForm),"LifeForm"] <- "CV-CVI"
cops2[grepl("c5-6",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c5-6",cops2$LifeForm),"LifeForm"], "c5-6", "CV-CVI")

# c4-6 --> "CIV-CVI"
cops2[cops2$LifeForm == "c4-6" & !is.na(cops2$LifeForm),"LifeForm"] <- "CIV-CVI"
cops2[grepl("c4-6",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c4-6",cops2$LifeForm),"LifeForm"], "c4-6", "CIV-CVI")

# c1+c4 --> "CI-CIV"
cops2[cops2$LifeForm == "c1+c4" & !is.na(cops2$LifeForm),"LifeForm"] <- "CI-CIV"
cops2[grepl("c1+c4",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c1+c4",cops2$LifeForm),"LifeForm"], "c1+c4", "CI-CIV")

# n3-4 --> "NIII-NIV"
cops2[cops2$LifeForm == "n3-4" & !is.na(cops2$LifeForm),"LifeForm"] <- "NIII-NIV"   
cops2[grepl("n3-4",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n3-4",cops2$LifeForm),"LifeForm"], "n3-4", "NIII-NIV")
                            
# n4-5 --> "NIV-NV"                                  
cops2[cops2$LifeForm == "n4-5" & !is.na(cops2$LifeForm),"LifeForm"] <- "NIV-NV"
cops2[grepl("n4-5",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n4-5",cops2$LifeForm),"LifeForm"], "n4-5", "NIV-NV")

# n5-6 --> "NV-NVI"
cops2[cops2$LifeForm == "n5-6" & !is.na(cops2$LifeForm),"LifeForm"] <- "NV-NVI"
cops2[grepl("n5-6",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n5-6",cops2$LifeForm),"LifeForm"], "n5-6", "NV-NVI")

# c5 --> "CV"
cops2[cops2$LifeForm == "c5" & !is.na(cops2$LifeForm),"LifeForm"] <- "CV"
cops2[grepl("c5",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c5",cops2$LifeForm),"LifeForm"], "c5", "CV")

# c2 --> "CII"
cops2[cops2$LifeForm == "c2" & !is.na(cops2$LifeForm),"LifeForm"] <- "CII"
cops2[grepl("c2",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c2",cops2$LifeForm),"LifeForm"], "c2", "CII")

# c4 --> "CIV"
cops2[cops2$LifeForm == "c4" & !is.na(cops2$LifeForm),"LifeForm"] <- "CIV"
cops2[grepl("c4",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c4",cops2$LifeForm),"LifeForm"], "c4", "CIV")

# c3 --> "CIII"
cops2[cops2$LifeForm == "c3" & !is.na(cops2$LifeForm),"LifeForm"] <- "CIII"
cops2[grepl("c3",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c3",cops2$LifeForm),"LifeForm"], "c3", "CIII")

# c1 --> "CI"
cops2[cops2$LifeForm == "c1" & !is.na(cops2$LifeForm),"LifeForm"] <- "CI"
cops2[grepl("c1",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c1",cops2$LifeForm),"LifeForm"], "c1", "CI")

# c6 --> "CVI"
cops2[cops2$LifeForm == "c6" & !is.na(cops2$LifeForm),"LifeForm"] <- "CVI"
cops2[grepl("c6",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("c6",cops2$LifeForm),"LifeForm"], "c6", "CVI")

# n1 --> "NI"
cops2[cops2$LifeForm == "n1" & !is.na(cops2$LifeForm),"LifeForm"] <- "NI"
cops2[grepl("n1",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n1",cops2$LifeForm),"LifeForm"], "n1", "NI")

# n2 --> "NII"                   
cops2[cops2$LifeForm == "n2" & !is.na(cops2$LifeForm),"LifeForm"] <- "NII"  
cops2[grepl("n2",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n2",cops2$LifeForm),"LifeForm"], "n2", "NII")
                 
# n3 --> "NIII"                                      
cops2[cops2$LifeForm == "n3" & !is.na(cops2$LifeForm),"LifeForm"] <- "NIII"
cops2[grepl("n3",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n3",cops2$LifeForm),"LifeForm"], "n3", "NIII")

# n4 --> "NIV"
cops2[cops2$LifeForm == "n4" & !is.na(cops2$LifeForm),"LifeForm"] <- "NIV"
cops2[grepl("n4",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n4",cops2$LifeForm),"LifeForm"], "n4", "NIV")

# n5 --> "NV"
cops2[cops2$LifeForm == "n5" & !is.na(cops2$LifeForm),"LifeForm"] <- "NV"
cops2[grepl("n5",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n5",cops2$LifeForm),"LifeForm"], "n5", "NV")

# n6 --> "NVI"
cops2[cops2$LifeForm == "n6" & !is.na(cops2$LifeForm),"LifeForm"] <- "NVI"
cops2[grepl("n6",cops2$LifeForm),"LifeForm"] <- str_replace_all(cops2[grepl("n6",cops2$LifeForm),"LifeForm"], "n6", "NVI")

# Female --> "female"
cops2[cops2$LifeForm == "Female" & !is.na(cops2$LifeForm),"LifeForm"] <- "female"
# Male --> "male"
cops2[cops2$LifeForm == "Male" & !is.na(cops2$LifeForm),"LifeForm"] <- "male"
# female (assumed adult) --> "female"          
cops2[cops2$LifeForm == "female (assumed adult)" & !is.na(cops2$LifeForm),"LifeForm"] <- "female"       
# male (assumed adult) --> "male"
cops2[cops2$LifeForm == "male (assumed adult)" & !is.na(cops2$LifeForm),"LifeForm"] <- "male"
# Flat or point head_female --> "Flat or point head; female"
cops2[cops2$LifeForm == "Flat or point head_female" & !is.na(cops2$LifeForm),"LifeForm"] <- "Flat or point head; female"
# 0.4mm_male --> "0.4mm; male"
cops2[cops2$LifeForm == "0.4mm_male" & !is.na(cops2$LifeForm),"LifeForm"] <- "0.4mm; male"                                 
# 0.7mm_male --> "0.7mm; male" 
cops2[cops2$LifeForm == "0.7mm_male" & !is.na(cops2$LifeForm),"LifeForm"] <- "0.7mm, male"                                     
# 0.4+0.7mm_male --> "0.4+0.7mm; male"
cops2[cops2$LifeForm == "0.4+0.7mm_male" & !is.na(cops2$LifeForm),"LifeForm"] <- "0.4+0.7mm; male"

# Check
unique(cops2$LifeForm)

# Good, save 
#dim(cops2)
save(cops2, file = "AtlantECO-BASEv1_dataset_Hexanauplia_abundances_05_07_22.RData")
write.table(cops2, file = "AtlantECO-BASEv1_dataset_Hexanauplia_abundances_05_07_22.csv", sep = ";")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------