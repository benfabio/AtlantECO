
##### ATLANTECO SCRIPT 5.5 ----------------------------------------------------------------------------------------------------------------------------
##### 19/04/2022: R Script to create the AtlantECO-BASE1 abundance only dataset for Cnidaria+Ctenophora (Jellyfish) ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted datasets, examine their format (should be the same for everyone)
# - Combine them, and clean dataset (units, data sources, ducplicates, add bathymetry and Longhurst provinces)

### Latest update: 15/09/2022 (forgot NMFS-COPEPOD's Ctenophora data....)

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
# JeDI (abund only) x
# SO-CPR x
# AusCPR x
# Brandao, Benedetti et al. 2021 x

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) NMFS-COPEPOD

### A) Cnidaria
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA/") ; dir()
cope <- get(load("COPEPOD-NOAA_Cnidarian_4030000_reformatted+WoRMScheck_12_11_21.Rdata"))
dim(cope) # 54'594
# Subset Cnidaria
unique(cope$Phylum) 
unique(cope[is.na(cope$Phylum),'ScientificName'])
unique(cope$Class) 
unique(cope[is.na(cope$Class),'ScientificName'])
unique(cope$Order) 
unique(cope[is.na(cope$Order),'ScientificName'])
# NA are OK in Class, need to remove Ochrophyta (Phylum) and Anthozoa (Class)
cope <- cope[-which(cope$Phylum == "Ochrophyta"),]
cope <- cope[-which(cope$Class == "Anthozoa"),]
dim(cope) # 51'590

# Check distribution of MeasurementValue
summary(cope$MeasurementValue) # 4286 NaNs to remove and clear outliers 
cope2 <- cope[!is.na(cope$MeasurementValue),]
# dim(cope2) # 47'304

# Check origin of data 
cope2 %>% count(basisOfRecord, sort = T)
cope2 %>% count(SamplingProtocol, sort = T) 
cope2 %>% count(basisOfRecord, sort = T) 

# Quickmap
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#          data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#      geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(basisOfRecord)),
#          data = cope2, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") +
#      coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
#          panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# # Good.

### Check colnames and some if necessary
colnames(cope2)
# head(cope2)
# unique(cope2$obisID) # to remove
# unique(cope2$DatasetKey) # keep
cope2 <- select(cope2, -obisID)

### Add mesh (from 'SamplingProtocol') size after 'SamplingProtocol'
# head(do.call(rbind, strsplit(x = as.character(unique(cope2$SamplingProtocol)), split = "; ", fixed = F)))
meshes <- do.call(rbind, strsplit(x = cope2$SamplingProtocol, split = "; ", fixed = F))[,3]
# length(meshes); unique(meshes) # length(meshes) == nrow(cope2)
# Remove 'mesh= ' and '1/2m opening @ ' and 'um)'
meshes <- str_replace_all(meshes, "mesh= ", "")
meshes <- str_replace_all(meshes, "1/2m opening @ ", "")
meshes[meshes == "333um)"] <- "333"
meshes[meshes == "NA"] <- NA
# unique(meshes)
# summary(factor(meshes))  # Looks ok
cope2 <- cope2 %>% add_column(MeshSize = meshes, .after = "SamplingProtocol")

# Check ScientificNames and WoRMS_ID and some other headers
unique(cope2$ScientificName)
unique(cope2$WoRMS_ID) # str(cope2$WoRMS_ID)  # should be character string throughout all files
unique(cope2[cope2$WoRMS_ID == "No match found in WoRMS","ScientificName"])
cope2[cope2$ScientificName == "Hydromedusae","ScientificName"] <- "Hydrozoa"
cope2[cope2$ScientificName == "Hydrozoa","TaxonRank"] <- "Class"
cope2[cope2$ScientificName == "Hydrozoa","WoRMS_ID"] <- "1337"
cope2[cope2$ScientificName == "Hydrozoa","Kingdom"] <- "Animalia"
cope2[cope2$ScientificName == "Hydrozoa","Phylum"] <- "Cnidaria"
cope2[cope2$ScientificName == "Hydrozoa","WoRMS_status"] <- "isMarine+isBrackish+isFreshwater"
# dim(cope2)

#unique(cope2$LifeForm) # important for C biomass conversion
# Tally taxonranks
#cope2 %>% count(TaxonRank, sort = T) # Ok good

### OK, add JeDI and be careful to remove duplicates

### ----------------------------------------------------------

### 2°) JeDI
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database/") ; dir()
jedi <- get(load("JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_03_12_2021.Rdata"))
# dim(jedi) # 238'083
# Subset Cnidaria+Ctenophora
# unique(jedi$Phylum)
# unique(jedi$Class)
# unique(jedi$ScientificName)
jedi <- jedi[jedi$Phylum %in% c("Ctenophora","Cnidaria") & !is.na(jedi$Phylum),]
# dim(jedi) # 198'746
# unique(jedi$WoRMS_ID)

# Check colnames, measured values etc.
# setdiff(colnames(cope2), colnames(jedi)) # missing MeshSize in JeDI
# setdiff(colnames(jedi), colnames(cope2)) # obisID to rm in jedi
jedi <- select(jedi, -obisID)
# Add MeshSize: from which column though
# unique(jedi$basisOfRecord) # not this one
# unique(jedi$SamplingProtocol) # --> this one 

jedi <- jedi %>% add_column(MeshSize = NA, .after = "SamplingProtocol")
# head(do.call(rbind, strsplit(x = as.character(unique(jedi$SamplingProtocol)), split = "; ", fixed = F)))
meshes <- do.call(rbind, strsplit(x = as.character(jedi$SamplingProtocol), split = "; ", fixed = F))[,3]
# unique(meshes)
meshes <- readr::parse_number(meshes, na = c("NA"), trim_ws = TRUE)
jedi$MeshSize <- meshes # unique(jedi$MeshSize)

# Check distribution of MeasurementValue
# summary(jedi$MeasurementValue) # No NaNs
#min(jedi$MeasurementValue) # --> NA
#dim(jedi[jedi$MeasurementValue >= 0,])
jedi <- jedi[jedi$MeasurementValue >= 0,]

# summary(factor(jedi$MeasurementUnit)) # #/m3!

### Check original datasets --> is NMFS-COPEPOD already in there?
# jedi %>% count(ParentEventID)
# jedi %>% count(InstitutionCode)

# Check eventDate and WoRMS_ID before rbinding with 'cope3'
#str(jedi) # WoRMS_ID --> character and eventDate should be date
# str(cope2) 
jedi$WoRMS_ID <- as.character(jedi$WoRMS_ID)
#summary(jedi[,c("Day","Month","Year")])
#unique(jedi[is.na(jedi$Day),"eventDate"])
jedi$eventDate <- lubridate::dmy(paste(jedi$Day, jedi$Month, jedi$Year, sep = "-"))
# unique(jedi$eventDate)

# Check depth layers
summary(cope2[,c("Depth","MinDepth","MaxDepth")])
cope2 <- cope2[!is.na(cope2$MaxDepth),] # 6 records removed 

summary(jedi[,c("Depth","MinDepth","MaxDepth")]) 
# dim( jedi[!is.na(jedi$MaxDepth),] )
jedi$na.count <- rowSums(is.na(jedi[,c("Depth","MinDepth","MaxDepth")]))
# summary(factor(jedi$na.count)) # 140357/198745 (70.6%) obs have NA in all 3
jedi2 <- jedi[jedi$na.count < 3,]
dim(jedi2) # 58'388
# Drop 'na.count'
jedi2 <- select(jedi2, -na.count)
dim(jedi2) # 58 388

# Rbind
cope3 <- rbind(cope2, jedi2)
dim(cope3) # 105'686
rm(cope,jedi) ; gc()

# Quick map with all datasets
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope2, alpha = 0.2, colour = "grey50") +
     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = jedi2, alpha = 0.3, colour = "red") +
     geom_polygon(aes(x = long, y = lat, group = group),
             data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")

### Looks like there is A LOT of overlap. Try to identify duplicates and rm 
# Check if you can use depth levels in the occurrenceID?
summary(cope3[,c("Depth","MinDepth","MaxDepth")])
# Going to need to remove all NA here

cope3$occurrenceID <- factor(paste(round(cope3$decimalLongitude,2), round(cope3$decimalLatitude,2), cope3$Day, cope3$Month, cope3$Year, cope3$MaxDepth, cope3$ScientificName, cope3$MeasurementValue, sep = "_"))
# How many unique obs?
# length(unique(cope3$occurrenceID)) # 79 788 unique obs. 
# 100 - (length(unique(cope3$occurrenceID))/nrow(cope3))*100 
### --> 24.5% overlap 

cope4 <- cope3[!duplicated(cope3$occurrenceID),]
# dim(cope4) # 79788
nrow(cope3) - nrow(cope4)

ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope2, alpha = 0.2, colour = "grey50") +
     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope4, alpha = 0.3, colour = "red") +
     geom_polygon(aes(x = long, y = lat, group = group),
             data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")
# OK continue

### ----------------------------------------------------------

### 3°) SO-CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
# dim(so.cpr) # 15'204'798
# Subset Cnidaria+Ctenophora
# unique(so.cpr$Phylum)
so.cpr <- so.cpr[so.cpr$Phylum == "Cnidaria" & !is.na(so.cpr$Phylum),]
# unique(so.cpr$Class)
# unique(so.cpr$Order)
# unique(so.cpr$ScientificName) # OK
dim(so.cpr) # 362'019

# Check colnames, measured values etc.
# setdiff(colnames(cope4), colnames(so.cpr)) # missing MeshSize
# setdiff(colnames(so.cpr), colnames(cope4)) # obisID to rm in cpr
so.cpr <- dplyr::select(so.cpr, -c(obisID,Temperature_Celsius,Salinity,PAR_microE_m2_s1,Fluorescence,PCI))
# Add MeshSize unique(cpr$basisOfRecord)
so.cpr <- so.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(so.cpr$MeasurementValue) # No NAs, but very very low values

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
# str(so.cpr) # Looks ok

# Rbind
cope5 <- rbind(cope4, so.cpr)
# dim(cope5) # 441'807
rm(cope2,so.cpr) ; gc()

# Quickmap
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope4, alpha = 0.2, colour = "grey50") +
#       geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = so.cpr, alpha = 0.3, colour = "red") +
#       geom_polygon(aes(x = long, y = lat, group = group),
#               data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#       coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#       theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#           panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")
#
# OK, add AusCPR

### ----------------------------------------------------------

### 4°) AusCPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
aus.cpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
dim(aus.cpr) # 7'373'520
# Subset Cnidaria+Ctenophora 
# unique(aus.cpr$Phylum) # NA?
# head(aus.cpr[is.na(aus.cpr$Class),]) ; dim(aus.cpr[is.na(aus.cpr$Class),])
aus.cpr <- aus.cpr[aus.cpr$Phylum == "Cnidaria" & !is.na(aus.cpr$Phylum),]
# unique(aus.cpr$Class)
# unique(aus.cpr$Order)
# unique(aus.cpr$ScientificName) # OK
dim(aus.cpr) # 148'176

# Check colnames, measured values etc.
# setdiff(colnames(cope5), colnames(aus.cpr)) # missing MeshSize
# setdiff(colnames(aus.cpr), colnames(cope5)) # obisID to rm in cpr
aus.cpr <- dplyr::select(aus.cpr, -obisID)
# Add MeshSize unique(aus.cpr$basisOfRecord)
aus.cpr <- aus.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(aus.cpr$MeasurementValue) # No NAS to remove byt very very low values (because CPR aperture)

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
# str(aus.cpr) # Looks ok

# Rbind
cope6 <- rbind(cope5, aus.cpr)
dim(cope6) # 589'983
rm(cope4,aus.cpr) ; gc()


### ----------------------------------------------------------

### 5°) Brandao et al. (2021)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Plankton_Imaging/") ; dir()
tara <- get(load("TARA_Oceans_plankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports_26_07_21.RData"))
# dim(tara) # 15'908
# str(tara)

# Subset Cnidaria
unique(tara$Phylum)
tara <- tara[tara$Phylum == "Cnidaria" & !is.na(tara$Phylum),]
dim(tara) # 388

# Check colnames etc.
#setdiff(colnames(cope6), colnames(tara)) # missing MeshSize
#setdiff(colnames(tara), colnames(cope6)) # obisID to rm
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
#unique(tara$ScientificName) # OK

# Rbind
cope7 <- rbind(cope6, tara)
dim(cope7) # 590'371
rm(cope5,cope3,tara) ; gc()

# Save tempo file ?
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
save(cope7, file = "tempo_file_Jellyfish_NMFS-COPEPOD+JeDI+CPRsurveys+TARA_tofinish_19_04_22.Rdata")


### ----------------------------------------------------------

### 6°) Final checkings, add bathymetry & longhurst provinces
# Re-load temp file
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
data <- get(load("tempo_file_Jellyfish_NMFS-COPEPOD+JeDI+CPRsurveys+TARA_tofinish_19_04_22.Rdata"))
# dim(data) # 590371

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
data$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
data$Bathymetry <- raster::extract(x = ras.bathy, y = data[,c("decimalLongitude","decimalLatitude")])
# summary(data$Bathymetry)

### Add Longhurst provinces
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
data$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = data[,c("decimalLongitude","decimalLatitude")]))
unique(data$LonghurstProvince) ; summary(factor(data$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(data$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    data[data$LonghurstProvince == i & !is.na(data$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(data$LonghurstProvince))
# 25787 NAs (too coastal points) --> (25787/nrow(data))*100 = 4.36% of the data 

### Do some final checks
summary(data$MeasurementValue)
unique(data$MeasurementUnit)
data$MeasurementUnit <- "#/m3"

# Tally taxonrank
counts <- data.frame( data %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(data)) )
counts[order(counts$n, decreasing = T),]
#   TaxonRank      n        perc
# 4     Order 153143 0.259401292
# 3     Genus 147401 0.249675204
# 6   Species  99342 0.168270460
# 5    Phylum  79541 0.134730534
# 2    Family  66615 0.112835827
# 7  Subclass  14939 0.025304427
# 8  Suborder  14154 0.023974755
# 1     Class   9524 0.016132229
# 9      <NA>   5712 0.009675272
# unique(data[is.na(data$TaxonRank),"ScientificName"]) # Of course
# nrow(data[is.na(data$TaxonRank),])/nrow(data) # <1% of the obs

### Check coordinates etc.
# summary(data[,c("decimalLatitude","decimalLongitude","Depth","MaxDepth","Day","Month","Year")])

### Drop useless cols
# colnames(data)
data <- dplyr::select(data,-c(ProjectID,ProjectWP,DataSilo))
# Save
save(data, file = "AtlantECO-BASEv1_dataset_Jellyfish_abundances_22_04_22.RData")
write.table(data, file = "AtlantECO-BASEv1_dataset_Jellyfish_abundances_22_04_22.csv", sep = ";")

# dim(get(load("AtlantECO-BASEv1_dataset_Jellyfish_abundances_22_04_22.RData")))

### Print a map of occ distrib by colouring as a fun of datasets (to check for )
# unique(data$BiblioCitation)
data$Dataset <- NA
data[data$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD"
data[data$BiblioCitation == "Lucas et al. (2014) - Gelatinous zooplankton biomass in the global oceans: geographic variation and environmental drivers","Dataset"] <- "JeDI"
data[data$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR"
data[data$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR"
data[data$BiblioCitation == "Brandao,Benedetti et al. (2021) - Scientific Reports","Dataset"] <- "TARA Oceans"
summary(factor(data$Dataset))
#       AusCPR        JeDI      NMFS-COPEPOD   SO-CPR       TARA Oceans 
#       148176        37877        41911       362019          388 

map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)), data = data, alpha = .5) +
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

ggsave(plot = map, filename = "map_Jellyfish_abundances_22_04_22.jpg", dpi = 300, width = 15, height = 12)


### Might still be some overlap between JeDI and COPEPOD...check overlap of ScientificNames and TaxonRank
### But you've already discarded duplicates based on occurrenceID so the overlap in space could be due to different ScientificNames
# summary(factor(data[data$Dataset == "NMFS-COPEPOD","TaxonRank"]))
# summary(factor(data[data$Dataset == "JeDI","TaxonRank"]))
# ### Much better taxonomic classif in JeDI
# # Non matching names
# length( unique(data[data$Dataset == "NMFS-COPEPOD","ScientificName"]) ) # 80
# length( unique(data[data$Dataset == "JeDI","ScientificName"]) ) # 53 names
#
# base::setdiff(unique(data[data$Dataset == "NMFS-COPEPOD","ScientificName"]), unique(data[data$Dataset == "JeDI","ScientificName"]))
# base::setdiff(unique(data[data$Dataset == "JeDI","ScientificName"]), unique(data[data$Dataset == "NMFS-COPEPOD","ScientificName"]))
# # Matching names
# base::intersect(unique(data[data$Dataset == "NMFS-COPEPOD","ScientificName"]), unique(data[data$Dataset == "JeDI","ScientificName"]))


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------