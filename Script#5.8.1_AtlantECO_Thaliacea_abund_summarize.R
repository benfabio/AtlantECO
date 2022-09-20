
##### ATLANTECO SCRIPT 5.8.1 ----------------------------------------------------------------------------------------------------------------------------
##### 28/04/2022: R Script to create the AtlantECO-BASE1 abundance only dataset for Thaliaceans (Salps and Doliolids) ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted datasets, examine their format (should be the same for everyone)
# - Combine them, and clean dataset (units, data sources, ducplicates, add bathymetry and Longhurst provinces)

### Latest update: 29/04/2022

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
# KRILLBASE x

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) NMFS-COPEPOD
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA/") ; dir()
cope <- get(load("COPEPOD-NOAA_Urochordata_4350000_reformatted+WoRMScheck_12_11_21.Rdata"))
# dim(cope) # 57'958
# Subset salps and doliolids
unique(cope$Phylum) # NA?
# unique(cope[is.na(cope$Phylum),'ScientificName'])
# dim(cope[is.na(cope$Phylum),]) # 27 records only - discard
unique(cope$Class) # Subset Thaliacea here
# unique(cope[cope$Class == "Ascidiacea",'ScientificName'])
cope <- cope[!is.na(cope$Class) & cope$Class == "Thaliacea",]
# dim(cope) # 24'316
unique(cope$ScientificName) # Wrong names in there left --> will need to correct some names before adding JeDI

### Clean taxonomy (again...)
require("worms") 
keys.worms <- wormsbynames( unique(cope$ScientificName) )
keys.worms$ScientificName <- unique(cope$ScientificName)

### Show unaccpted ones and their accepted names 
sp2correct <- keys.worms[keys.worms$status == "unaccepted" & !is.na(keys.worms$status),c("ScientificName")]
sp2correct

# sp <- sp2correct[1]
for(sp in sp2correct) {
        # Get valid name
        valid <- keys.worms[keys.worms$ScientificName == sp,c("valid_name")]
        message(paste("Changing ",sp," to ",valid, sep = ""))
        cope[cope$ScientificName == sp,"ScientificName"] <- valid
} # eo for loop 
# colnames(cope)

# For testing the functions below:
# s <- unique(cope$ScientificName)[3] ; s
require("parallel")
res <- mclapply( unique(cope$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- cope[cope$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- "No match found in WoRMS"
                subset$WoRMS_status <- "No match found in WoRMS"
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- as.character(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
                subset$TaxonRank <- keys.worms[keys.worms$ScientificName == s,"rank"]
                subset[,c("Kingdom","Phylum","Class","Order","Family","Genus")] <- keys.worms[keys.worms$ScientificName == s,c("kingdom","phylum","class","order","family","genus")]
            
            } # eo 1st if else loop - if species is actually found in 'w'
        
            ### 2nd if else loop to add species name if subset$TaxonRank == "Species"
            if( keys.worms[keys.worms$ScientificName == s,"rank"] == "Species" & !is.na(keys.worms[keys.worms$ScientificName == s,"rank"]) ) {
            
                subset$Species <- keys.worms[keys.worms$ScientificName == s,"valid_name"]
            
            } else {
            
                subset$Species <- NA
            
            } # eo 2nd if else loop - if rank == "Species" 
        
            ### 3rd if else loop to add subset$WoRMS_status
            if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
             
                statuses <- melt( keys.worms[keys.worms$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                subset$WoRMS_status <- status2add
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 2 
        
) # eo mclapply - s in taxa_names
cope2 <- dplyr::bind_rows(res)
rm(res); gc()

# unique(cope2$ScientificName)
# unique(cope2$WoRMS_status)
# unique(cope2$WoRMS_ID)
### Good now.

# Check distribution of MeasurementValue
# summary(cope2$MeasurementValue) # 862 NaNs to remove and clear outliers 
cope2 <- cope2[!is.na(cope2$MeasurementValue),]
dim(cope2) # 23454

# Check origin of data 
cope2 %>% dplyr::count(basisOfRecord, sort = T)
cope2 %>% dplyr::count(SamplingProtocol, sort = T) 
cope2 %>% dplyr::count(basisOfRecord, sort = T) 

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
# head(cope2)
# unique(cope2$obisID) # to remove
# unique(cope2$DatasetKey) # keep
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
unique(meshes) 
meshes[meshes == "-999"] <- NA
# unique(meshes)
# summary(factor(meshes))  # Looks ok
cope2 <- cope2 %>% add_column(MeshSize = meshes, .after = "SamplingProtocol")

# Check LifeForms
cope2 %>% dplyr::count(LifeForm, sort = T)
unique(cope2$LifeForm) # important for C biomass conversions!!
# But 97.7% of records here have NA anyway...

# Tally taxonranks
cope2 %>% dplyr::count(TaxonRank, sort = T) # Ok good
#   TaxonRank    n
# 1    Family 8052
# 2     Class 5708
# 3     Genus 4741
# 4     Order 4398
# 5   Species  555

### OK, add JeDI and be careful to remove duplicates


### ----------------------------------------------------------

### 2°) JeDI
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database/") ; dir()
jedi <- get(load("JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_03_12_2021.Rdata"))
# dim(jedi) # 238'083
# Subset Thaliacea: Class == Thaliacea
# unique(jedi$Phylum)
# unique(jedi$Class)
jedi <- jedi[jedi$Class == "Thaliacea" & !is.na(jedi$Class),]
# unique(jedi$Order)
# unique(jedi$Family)
# unique(jedi$ScientificName)
# unique(jedi$WoRMS_ID)
jedi$WoRMS_ID <- as.character(jedi$WoRMS_ID)
# dim(jedi) # 19'169

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
unique(meshes)
meshes <- readr::parse_number(meshes, na = c("NA"), trim_ws = TRUE)
jedi$MeshSize <- meshes # unique(jedi$MeshSize)

# Check distribution of MeasurementValue
# summary(jedi$MeasurementValue) # No NaNs
# min(jedi$MeasurementValue) # --> NA
# jedi <- jedi[jedi$MeasurementValue >= 0,]

# summary(factor(jedi$MeasurementUnit)) # #/m3!

### Check original datasets --> is NMFS-COPEPOD already in there?
# jedi %>% count(ParentEventID)
# jedi %>% count(InstitutionCode)

# Check eventDate 
# str(cope2) 
# summary(jedi[,c("Day","Month","Year")])
#unique(jedi[is.na(jedi$Day),"eventDate"])
jedi$eventDate <- lubridate::dmy(paste(jedi$Day, jedi$Month, jedi$Year, sep = "-"))
# unique(jedi$eventDate)

# Check depth layers
summary(cope2[,c("Depth","MinDepth","MaxDepth")])
cope2 <- cope2[!is.na(cope2$MaxDepth),]

summary(jedi[,c("Depth","MinDepth","MaxDepth")]) 
# dim( jedi[!is.na(jedi$MaxDepth),] )
jedi$na.count <- rowSums(is.na(jedi[,c("Depth","MinDepth","MaxDepth")]))
summary(factor(jedi$na.count)) # 140357/198745 (70.6%) obs have NA in all 3
jedi2 <- jedi[jedi$na.count < 3,]
dim(jedi2) # 18'090
# Drop 'na.count'
jedi2 <- select(jedi2, -na.count)

# Rbind
cope3 <- rbind(cope2, jedi2)
dim(cope3) # 41'540
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
# Going to need to remove the 1058 NAs in MaxDepth to create a proper record ID to discard duplicates
cope3 <- cope3[!is.na(cope3$MaxDepth),]

cope3$occurrenceID <- factor(paste(round(cope3$decimalLongitude,2), round(cope3$decimalLatitude,2), cope3$Day, cope3$Month, cope3$Year, cope3$MaxDepth, cope3$ScientificName, cope3$MeasurementValue, sep = "_"))
# How many unique obs?
length(unique(cope3$occurrenceID)) # 31519 unique obs. 
(length(unique(cope3$occurrenceID))/nrow(cope3))*100 
### --> ~22% overlap 

cope4 <- cope3[!duplicated(cope3$occurrenceID),]
dim(cope4) # 31519

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope2, alpha = 0.2, colour = "grey50") +
#      geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope4, alpha = 0.3, colour = "red") +
#      geom_polygon(aes(x = long, y = lat, group = group),
#              data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#      coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#          panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")
# OK continue to CPR surveys


### ----------------------------------------------------------

### 3°) SO-CPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
# dim(so.cpr) # 15'204'798
# Subset Thaliacea
# unique(so.cpr$Class)
so.cpr <- so.cpr[so.cpr$Class == "Thaliacea" & !is.na(so.cpr$Class),]
# unique(so.cpr$Order)
# unique(so.cpr$Family)
# unique(so.cpr$Genus)
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
# dim(cope5) # 393'538
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
# Subset Thaliacea
# unique(aus.cpr$Class) # NA?
# head(aus.cpr[is.na(aus.cpr$Class),]) ; dim(aus.cpr[is.na(aus.cpr$Class),])
aus.cpr <- aus.cpr[aus.cpr$Class == "Thaliacea" & !is.na(aus.cpr$Class),]
# unique(aus.cpr$Class)
# unique(aus.cpr$Order)
# unique(aus.cpr$ScientificName) # OK
dim(aus.cpr) # 91'728

# Check colnames, measured values etc.
# setdiff(colnames(cope5), colnames(aus.cpr)) # missing MeshSize
# setdiff(colnames(aus.cpr), colnames(cope5)) # obisID to rm in cpr
aus.cpr <- dplyr::select(aus.cpr, -obisID)
# Add MeshSize unique(aus.cpr$basisOfRecord)
aus.cpr <- aus.cpr %>% add_column(MeshSize = "270", .after = "SamplingProtocol")

# Check distribution of Measurement
# summary(aus.cpr$MeasurementValue) # No NAS to remove

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
# str(aus.cpr) # Looks ok

# Rbind
cope6 <- rbind(cope5, aus.cpr)
# dim(cope6) # 485'266
rm(cope4,cope3,aus.cpr) ; gc()

### ----------------------------------------------------------

### 5°) KRILLBASE
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/KRILLBASE") ; dir()
kbase <- get(load("KRILLBASE_Atkinson2017_reformatted+WoRMScheck_21_10_21.Rdata"))
# dim(kbase) # 29'086
# Subset Salps 
unique(kbase$Order)
kbase <- kbase[kbase$Order == "Salpida",]
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
summary(kbase$MeasurementValue) # 5186 NA to remove and clear outliers 
unique(kbase$MeasurementUnit) # ind/m2 - not m3
kbase <- kbase[!is.na(kbase$MeasurementValue),]
# dim(kbase) # 9'357

# Check eventDate and WoRMS_ID before rbinding with 'cope4'
# str(kbase) # Looks ok
# Check ScientificName
# unique(kbase$ScientificName) # OK

# Rbind
cope7 <- rbind(cope6, kbase)
# dim(cope7) # 494'623
rm(cope5, kbase) ; gc()

# Quickmap to check
# unique(kri4$BiblioCitation)
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude,
#       colour = factor(BiblioCitation)), data = cope7, alpha = .5) +
#    scale_colour_brewer(name = "Source", palette = "Paired") +
#    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#

### Save tempo file
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
save(cope7, file = "tempo_file_Thaliacea_NMFS-COPEPOD+JeDI+CPRsurveys+KRILLBASE_tofinish_28_04_22.Rdata")


### ----------------------------------------------------------

### 6°) Final checkings, add bathymetry & longhurst provinces
# Re-load temp file
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance") ; dir()
data <- get(load("tempo_file_Thaliacea_NMFS-COPEPOD+JeDI+CPRsurveys+KRILLBASE_tofinish_28_04_22.Rdata"))
dim(data) # 494'623

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
# summary(data$Bathymetry)
# unique(data$BathySource)
data[is.na(data$Bathymetry),"BathySource"] <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(zoo2[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
data[is.na(data$Bathymetry),"Bathymetry"] <- raster::extract(x = ras.bathy, y = data[is.na(data$Bathymetry),c("decimalLongitude","decimalLatitude")])
summary(data$Bathymetry)

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
# 7309 NAs (too coastal points) --> (7309/nrow(data))*100 = 1.48% of the data 

### Do some final checks
summary(data$MeasurementValue)
unique(data$MeasurementUnit)
data[data$MeasurementUnit != "#/m2","MeasurementUnit"] <- "#/m3"
### Can convert #/m2 to #/m3 later

# Tally taxonrank
counts <- data.frame( data %>% group_by(TaxonRank) %>% summarize(n = n(), perc = n/nrow(data)) )
counts[order(counts$n, decreasing = T),]
#   TaxonRank      n       perc
# 5   Species 191662 0.38749108
# 2    Family 187608 0.37929494
# 3     Genus  92963 0.18794718
# 1     Class  16506 0.03337087
# 4     Order   5884 0.01189593

# Check names one last time
unique(data$ScientificName) 

### Check coordinates etc.
# summary(data[,c("decimalLatitude","decimalLongitude","Depth","MaxDepth","Day","Month","Year")])

### Drop useless cols
# colnames(data)
data <- dplyr::select(data,-c(ProjectID,ProjectWP,DataSilo))
# Save
save(data, file = "AtlantECO-BASEv1_dataset_Thaliacea_abundances_29_04_22.RData")
write.table(data, file = "AtlantECO-BASEv1_dataset_Thaliacea_abundances_29_04_22.csv", sep = ";")


### Print a map of occ distrib by colouring as a fun of datasets (to check for )
# unique(data$BiblioCitation)
data$Dataset <- NA
data[data$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD"
data[data$BiblioCitation == "Lucas et al. (2014) - Gelatinous zooplankton biomass in the global oceans: geographic variation and environmental drivers","Dataset"] <- "JeDI"
data[data$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR"
data[data$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR"
data[data$BiblioCitation == "Atkinson et al. (2017). KRILLBASE: a circumpolar database of Antarctic krill and salp numerical densities, 1926–2016. Earth Syst. Sci. Data, 9(1), 193-210.","Dataset"] <- "KRILLBASE"
summary(factor(data$Dataset))
#       AusCPR       JeDI       KRILLBASE  NMFS-COPEPOD    SO-CPR 
#       91728        10666         9357        20853       362019

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

ggsave(plot = map, filename = "map_Thaliacea_abundances_29_04_22.jpg", dpi = 300, width = 15, height = 12)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------