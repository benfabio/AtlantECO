
##### ATLANTECO SCRIPT 1.6.2 ----------------------------------------------------------------------------------------------------------------------------
##### 13/04/2022: R Script to reformat the MAREDAT Diatoms data published by Leblanc et al. (2012) - ESSD © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Aims to:
#  - Read the excel sheet and reformat to AtlantECO WP2 template
#  - Provide AphiaID and classification using WoRMS' R package 'worms'

### Latest update: 13/07/2022

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("readxl")
library("lubridate")

world <- map_data("world")  # for maps

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the data and reshape
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/MAREDAT files/Leblanc&al._2012_ESSD_PANGAEA.777384") ; dir() 
# Use the first sheet of the '.xlsx' file - the .nc only has standard feilds of biomasses (no metadata) 
diato <- read.csv("Leblanc2012_diatom_database_corr_06_12_2016.csv", h = T, sep = ";", dec = ",")
# dim(diato) # 91'704
# str(diato)
# Convert Date to actual date
#summary(diato[,c('Day','Month','Year')])
diato$Date <- lubridate::dmy(paste(diato$Day, diato$Month, diato$Year, sep = "-"))

# Check coordinates, depth etc.
# summary(diato[,c("decimalLongitude","decimalLatitude")]) # Looks OK
# summary(diato$Depth) # Looks OK

# Quick map
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = diato) + coord_quickmap() +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")

### The data.frame is already kind of following a long table format. Just need to complete those additional AtlantECO headers
colnames(diato)

# Check cols content to assess where to ut them in the AtlantECO format
# unique(diato$Other) # --> LifeForm
# diato %>% count(Other)

# length(diato[diato$Other == "-","Other"])
diato[diato$Other == "-","Other"] <- NA
#unique(diato$Orig_Database) # --> InstitutionCode
diato[diato$Orig_Database == "","Orig_Database"] <- NA
#unique(diato$ProjectID) # --> ParentEventID
diato[diato$ProjectID == "","ProjectID"] <- NA
#unique(diato$Cruise_station_ID) # --> EventID ; needs trimws
diato$Cruise_station_ID <- trimws(diato$Cruise_station_ID, which = "both")
#unique(diato$Gear) # --> basisOfRecord or SamplingProtocol
diato[diato$Gear == "","Gear"] <- NA
#unique(diato$Counting_method) # 
diato[diato$Counting_method == "","Counting_method"] <- NA
# unique(diato$Reference) # Don't care
#unique(diato$Contact) # --> DeterminedBy
diato[diato$Contact == "","Contact"] <- NA
#unique(diato$Preservative)
diato[diato$Preservative == "","Preservative"] <- NA

#unique(diato$Weblink) # Don't care

colnames(diato)

### Reformat to WP2 format
diato2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", DatasetKey = NA,
                decimalLatitude = diato$decimalLatitude, decimalLongitude = diato$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = diato$Date,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = diato$Year, Month = diato$Month, Day = diato$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA (15min)", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = diato$Depth, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = NA, MaxDepth = NA, ParentEventID = diato$ProjectID, EventID = diato$Cruise_station_ID, InstitutionCode = diato$Orig_Database,
                SourceArchive = "PANGAEA", OrigCollectionCode = "777384", OrigCollectionID = "https://doi.org/10.1594/PANGAEA.777384",
                BiblioCitation = "Leblanc et al. (2021) - ESSD", CitationDOI = "doi:10.5194/essd-4-149-2012", DateDataAccess = "23-05-2021",
                OrigScientificName = diato$ScientificName, ScientificName = diato$ScientificName,
                WoRMS_ID = diato$WoRMS, TaxonRank = 'To_add_at_the_end', 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = diato$Genus, Species = diato$Species,
                Subspecies = NA, LifeForm = paste(diato$Other,diato$Group, sep = "; Group = "),
                AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Cell concentration", MeasurementTypeID = "To_define", MeasurementValue = diato$Abundance_cell.L,
                MeasurementUnit = "cell/L", MeasurementAcurracy = NA, MeasurementValueID = "To_define",
                MinBiomass = diato$tot_min_mgCm3, MaxBiomass = diato$tot_max_mgCm3, MeanBiomass = diato$avg_tot_mgCm3,
                BiomassConvFactor = "See original .xslx sheet - Average total biomass are given in mgC.m-3 in the MeanBiomass column",
                basisOfRecord = diato$Gear, SamplingProtocol = paste(diato$Counting_method,diato$Preservative, sep = "; "),
                SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = diato$Contact, DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
# head(diato2)
dim(diato2) # 91'704, good
str(diato2)
summary(diato2[,c("MinBiomass","MaxBiomass","MeanBiomass")])

### Check scientific names used
unique(diato2$ScientificName) #; str(diato2$ScientificName)
# Needs some trimming
diato2$ScientificName <- trimws(diato2$ScientificName, which = "both")
# Remove spp. and sp. before running the 'worms' routine
diato2$ScientificName <- gsub(" spp.", "", diato2$ScientificName, fixed = T)
diato2$ScientificName <- gsub(" sp.", "", diato2$ScientificName, fixed = T)
diato2$ScientificName <- gsub(" cf. ", " ", diato2$ScientificName, fixed = T)

# Run worms a firts time
require("worms") # install.packages("worms")
keys.worms <- wormsbynames( unique(diato2$ScientificName), marine_only = "false") # 
#head(keys.worms)
keys.worms$ScientificName <- unique(diato2$ScientificName)

### Show unaccpted ones and their accepted names 
# keys.worms[keys.worms$status == "unaccepted" & !is.na(keys.worms$status),c("ScientificName","valid_name")]
# use this to correct the names in a for loop
sp2correct <- keys.worms[keys.worms$status == "unaccepted" & !is.na(keys.worms$status),c("ScientificName")]
# For testing: 
# sp <- sp2correct[1]
diato3 <- diato2 # save in case it goes wrong
for(sp in sp2correct) {
        # Get valid name
        valid <- keys.worms[keys.worms$ScientificName == sp,c("valid_name")]
        message(paste("Changing ",sp," to ",valid, sep = ""))
        diato3[diato3$ScientificName == sp,"ScientificName"] <- valid
} # eo for loop 
# Re-run wormsbynames
# ?wormsbynames
keys.worms2 <- wormsbynames( unique(diato3$ScientificName), marine_only = "false", match = F) # 
# head(keys.worms2)

### Stuff to manually correct:
# Pennate                                              no match
# unique(diato3[diato3$ScientificName == "Pennate","LifeForm"])
diato3[diato3$ScientificName == "Pennate","ScientificName"] <- "Bacillariophyceae"
# Centric                                              no match
diato3[diato3$ScientificName == "Centric","ScientificName"] <- "Bacillariophyceae"
# Chaetoceros whigamii                                 no match keys.worms2[keys.worms2$ScientificName == "Chaetoceros whigamii",]
#diato3[diato3$ScientificName == "","ScientificName"] <- ""
### EXISTS! Fill info manually after
# Diatom                                               no match
diato3[diato3$ScientificName == "Diatom","ScientificName"] <- "Bacillariophyceae"
# Raphoneis                                            no match
diato3[diato3$ScientificName == "Raphoneis","ScientificName"] <- "Rhaphoneis"
# Chaetoceros phaeoceros                               no match
diato3[diato3$ScientificName == "Chaetoceros phaeoceros","ScientificName"] <- "Chaetoceros (Phaeoceros)"
# Chaetoceros hyalochaetae                             no match
diato3[diato3$ScientificName == "Chaetoceros hyalochaetae","ScientificName"] <- "Chaetoceros (Hyalochaete)"
# Helicotheca tamensis                                 no match
# diato3[diato3$ScientificName == "","ScientificName"] <- ""
### EXISTS! Fill info manually after
# Nanoneis haslea                                      no match
diato3[diato3$ScientificName == "Nanoneis haslea","ScientificName"] <- "Nanoneis hasleae"
# Chaetoceros atlanticus var. neopolitanus             no match
diato3[diato3$ScientificName == "Chaetoceros atlanticus var. neopolitanus","ScientificName"] <- "Chaetoceros atlanticus var. neapolitanus"
# Rhizosolenia cf.striata                              no match
diato3[diato3$ScientificName == "Rhizosolenia cf.striata","ScientificName"] <- "Rhizosolenia striata"
# Rhizosolenia longirostrum                            no match
#diato3[diato3$ScientificName == "Rhizosolenia longirostrum","ScientificName"] <- ""
###  24 obs - no equivalent
# Proboscia gracillima                                 no match
diato3[diato3$ScientificName == "Proboscia gracillima","ScientificName"] <- "Proboscia alata"
# Diatoma  vulgare                                     no match
diato3[diato3$ScientificName == "Diatoma  vulgare","ScientificName"] <- "Diatoma vulgaris"
# Navicula transitans var. deresa f. delicatula        no match
diato3[diato3$ScientificName == "Navicula transitans var. deresa f. delicatula","ScientificName"] <- "Navicula transitans"
# Navicula transitans var. deresa                      no match
diato3[diato3$ScientificName == "Navicula transitans var. deresa","ScientificName"] <- "Navicula transitans"
# Thalassiosira levanderi var. oestrupii               no match
diato3[diato3$ScientificName == "Thalassiosira levanderi var. oestrupii","ScientificName"] <- "Thalassiosira levanderi"
# Gyrosigma balticum var. silimis                      no match
diato3[diato3$ScientificName == "Gyrosigma balticum var. silimis","ScientificName"] <- "Pleurosigma simile"
# Proboscia subantarctica                              no match
# diato3[diato3$ScientificName == "Proboscia subantarctica","ScientificName"] <- ""
### EXISTS! Fill info manually after
# Pleurosigma atlanticus                               no match
diato3[diato3$ScientificName == "Pleurosigma atlanticus","ScientificName"] <- "Pleurosigma atlanticum"
# Diploneis coffeaeformis                              no match             
diato3[diato3$ScientificName == "Diploneis coffeaeformis","ScientificName"] <- "Diploneis coffaeiformis"
# Bacteriosira
diato3[diato3$ScientificName == "Bacteriosira","ScientificName"] <- "Bacterosira"


### OK re-re-run wormsbyname
keys.worms3 <- wormsbynames( unique(diato3$ScientificName), marine_only = "false", match = F, verbose = F) # 
# keys.worms3[keys.worms3$status == "unaccepted" & !is.na(keys.worms3$status),c("valid_name")]
keys.worms3$ScientificName <- unique(diato3$ScientificName)

# Add WoRMS_status field
diato3 <-  add_column(diato3, WoRMS_status = NA, .after = "WoRMS_ID")
# colnames(diato3)

# For testing:
# s <- unique(diato3$ScientificName)[13] ; s
require("parallel")
res <- mclapply( unique(diato3$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- diato3[diato3$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms3[keys.worms3$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS; unassessed AlgaeBase entry")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- "Species" # because all are actual species names, except those 2 Chaetoceros subgenera
            
            } else if( !is.na(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"])
                subset$TaxonRank <- keys.worms3[keys.worms3$ScientificName == s,"rank"]
                subset[,c("Kingdom","Phylum","Class","Order","Family","Genus")] <- keys.worms3[keys.worms3$ScientificName == s,c("kingdom","phylum","class","order","family","genus")]
            
            } # eo 1st if else loop - if species is actually found in 'w'
        
            ### 2nd if else loop to add species name if subset$TaxonRank == "Species"
            if( keys.worms3[keys.worms3$ScientificName == s,"rank"] == "Species" & !is.na(keys.worms3[keys.worms3$ScientificName == s,"rank"]) ) {
            
                subset$Species <- keys.worms3[keys.worms3$ScientificName == s,"valid_name"]
            
            } else {
            
                subset$Species <- NA
            
            } # eo 2nd if else loop - if rank == "Species" 
        
            ### 3rd if else loop to add subset$WoRMS_status
            if( !is.na(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"]) ) {
             
                statuses <- melt( keys.worms3[keys.worms3$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            return(subset)

        }, mc.cores = 25

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
# dim(ddf) # 91'704        70
rm(res) ; gc()
# Check random lines
# ddf[54545:54555,]

unique(ddf$Species)
unique(ddf$WoRMS_ID)
unique(ddf$WoRMS_status)

unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS; unassessed AlgaeBase entry","ScientificName"])

### 18/04/22: Classification to fill manually after: 
# - Chaetoceros whigamii
# - Helicotheca tamensis
# - Proboscia subantarctica
# (Rhizosolenia longirostrum & Nitzschia crotonensis were not found anywhere)
#head(ddf[ddf$ScientificName == "Chaetoceros whigamii",])
ddf[ddf$ScientificName == "Chaetoceros whigamii","WoRMS_ID"] <- factor("160524")
ddf[ddf$ScientificName == "Chaetoceros whigamii","WoRMS_status"] <- factor("isMarine")
ddf[ddf$ScientificName == "Chaetoceros whigamii","Kingdom"] <- "Chromista"
ddf[ddf$ScientificName == "Chaetoceros whigamii","Phylum"] <- "Ochrophyta"
ddf[ddf$ScientificName == "Chaetoceros whigamii","Class"] <- "Bacillariophyceae"
ddf[ddf$ScientificName == "Chaetoceros whigamii","Order"] <- "Chaetocerotanae incertae sedis"
ddf[ddf$ScientificName == "Chaetoceros whigamii","Family"] <- "Chaetocerotaceae"
ddf[ddf$ScientificName == "Chaetoceros whigamii","Genus"] <- "Chaetoceros"
ddf[ddf$ScientificName == "Chaetoceros whigamii","Species"] <- "Chaetoceros whigamii"

#head(ddf[ddf$ScientificName == "Helicotheca tamensis",])
ddf[ddf$ScientificName == "Helicotheca tamensis","WoRMS_ID"] <- factor("157440")
ddf[ddf$ScientificName == "Helicotheca tamensis","WoRMS_status"] <- factor("isMarine")
ddf[ddf$ScientificName == "Helicotheca tamensis","Kingdom"] <- "Chromista"
ddf[ddf$ScientificName == "Helicotheca tamensis","Phylum"] <- "Ochrophyta"
ddf[ddf$ScientificName == "Helicotheca tamensis","Class"] <- "Bacillariophyceae"
ddf[ddf$ScientificName == "Helicotheca tamensis","Order"] <- "Lithodesmiales"
ddf[ddf$ScientificName == "Helicotheca tamensis","Family"] <- "Lithodesmiaceae"
ddf[ddf$ScientificName == "Helicotheca tamensis","Genus"] <- "Helicotheca"
ddf[ddf$ScientificName == "Helicotheca tamensis","Species"] <- "Helicotheca tamensis"

#head(ddf[ddf$ScientificName == "Proboscia subantarctica",])
ddf[ddf$ScientificName == "Proboscia subantarctica","WoRMS_ID"] <- factor("573542")
ddf[ddf$ScientificName == "Proboscia subantarctica","WoRMS_status"] <- factor("isMarine")
ddf[ddf$ScientificName == "Proboscia subantarctica","Kingdom"] <- "Chromista"
ddf[ddf$ScientificName == "Proboscia subantarctica","Phylum"] <- "Ochrophyta"
ddf[ddf$ScientificName == "Proboscia subantarctica","Class"] <- "Bacillariophyceae"
ddf[ddf$ScientificName == "Proboscia subantarctica","Order"] <- "Rhizosoleniales"
ddf[ddf$ScientificName == "Proboscia subantarctica","Family"] <- "Rhizosoleniaceae"
ddf[ddf$ScientificName == "Proboscia subantarctica","Genus"] <- "Proboscia"
ddf[ddf$ScientificName == "Proboscia subantarctica","Species"] <- "Proboscia subantarctica"

### Check other taxnomic levels
ddf$Kingdom <- "Chromista"
unique(ddf$Phylum) # Platyhelminthes, NA, Mollusca, Foraminifera
unique(ddf$Order) 
### Fix manually

# A) the NA (will be some left)
# unique(ddf[is.na(ddf$Phylum),"ScientificName"])
# dim(ddf[ddf$ScientificName == "Pseudo-eunotia",]) # Unaccepted but only 2 obs
ddf[ddf$ScientificName == "Pseudo-eunotia","TaxonRank"] <- "Genus"
ddf[ddf$ScientificName == "Pseudo-eunotia","WoRMS_status"] <- NA
ddf[ddf$ScientificName == "Pseudo-eunotia","Phylum"] <- "Ochrophyta"
ddf[ddf$ScientificName == "Pseudo-eunotia","Class"] <- "Bacillariophyceae"
ddf[ddf$ScientificName == "Pseudo-eunotia","Order"] <- "Bacillariales"
ddf[ddf$ScientificName == "Pseudo-eunotia","Family"] <- "Bacillariaceae"

# B) the Platyhelminthes
unique(ddf[ddf$Phylum == "Platyhelminthes" & !is.na(ddf$Phylum),"ScientificName"]) #  Nitzschia?? --> wronf AphiaID
# unique(ddf[ddf$ScientificName == "Nitzschia","Phylum"]) 
ddf[ddf$ScientificName == "Nitzschia","WoRMS_ID"] <- "149045"
ddf[ddf$ScientificName == "Nitzschia","Phylum"] <- "Ochrophyta"
ddf[ddf$ScientificName == "Nitzschia","Class"] <- "Bacillariophyceae"
ddf[ddf$ScientificName == "Nitzschia","Order"] <- "Bacillariales"
ddf[ddf$ScientificName == "Nitzschia","Family"] <- "Bacillariaceae"

# C) the Mollusca "Actinocyclus" "Trichotoxon"
# C.1) "Trichotoxon" (sounds funny)
# unique(ddf[ddf$Phylum == "Mollusca" & !is.na(ddf$Phylum),"ScientificName"])
ddf[ddf$ScientificName == "Trichotoxon","WoRMS_ID"] <- "149645"
ddf[ddf$ScientificName == "Trichotoxon","WoRMS_status"] <- factor("isMarine")
ddf[ddf$ScientificName == "Trichotoxon","Kingdom"] <- "Chromista"
ddf[ddf$ScientificName == "Trichotoxon","Phylum"] <- "Ochrophyta"
ddf[ddf$ScientificName == "Trichotoxon","Class"] <- "Bacillariophyceae"
ddf[ddf$ScientificName == "Trichotoxon","Order"] <- "Thalassionematales"
ddf[ddf$ScientificName == "Trichotoxon","Family"] <- "Thalassionemataceae"

# C.2) "Actinocyclus" 
ddf[ddf$ScientificName == "Actinocyclus","WoRMS_ID"] <- "148944"
ddf[ddf$ScientificName == "Actinocyclus","WoRMS_status"] <- factor("isMarine+isFreshwater")
ddf[ddf$ScientificName == "Actinocyclus","Kingdom"] <- "Chromista"
ddf[ddf$ScientificName == "Actinocyclus","Phylum"] <- "Ochrophyta"
ddf[ddf$ScientificName == "Actinocyclus","Class"] <- "Bacillariophyceae"
ddf[ddf$ScientificName == "Actinocyclus","Order"] <- "Coscinodiscales"
ddf[ddf$ScientificName == "Actinocyclus","Family"] <- "Hemidiscaceae"

# D) the Foraminifera: 'Hemidiscus'
# unique(ddf[ddf$Phylum == "Foraminifera" & !is.na(ddf$Phylum),"ScientificName"])
ddf[ddf$ScientificName == "Hemidiscus","WoRMS_ID"] <- "180366"
ddf[ddf$ScientificName == "Hemidiscus","WoRMS_status"] <- factor("isMarine")
ddf[ddf$ScientificName == "Hemidiscus","Kingdom"] <- "Chromista"
ddf[ddf$ScientificName == "Hemidiscus","Phylum"] <- "Ochrophyta"
ddf[ddf$ScientificName == "Hemidiscus","Class"] <- "Bacillariophyceae"
ddf[ddf$ScientificName == "Hemidiscus","Order"] <- "Coscinodiscales"
ddf[ddf$ScientificName == "Hemidiscus","Family"] <- "Hemidiscaceae"

### Good.
unique(ddf$WoRMS_status)
# unique(ddf[ddf$WoRMS_status == "","ScientificName"])   
# unique(ddf[ddf$WoRMS_status == "","Order"]) # From various orders.. 

# summary(ddf$MeasurementValue) # No zeroes...
# summary(ddf$Biomass_mgCm3) # but a zeo here?
min(ddf$MeasurementValue) ; min(ddf$MeanBiomass, na.rm = T)
# Weird!
ddf[is.na(ddf$MeanBiomass),"MeasurementValue"] # not very low cell concentrations
ddf[is.na(ddf$MeanBiomass),"ScientificName"] # Ah ok, class-level obs, makes sense! No proper C conv factor


### 18/04/22: Save new file
save(ddf, file = "AtlantECO_WP2_MAREDAT_Diatoms_Leblanc2012_reformat+WoRMScheck_13_07_22.RData")
write.table(x = ddf, file = "AtlantECO_WP2_MAREDAT_Diatoms_Leblanc2012_reformat+WoRMScheck_13_07_22.txt", sep = "\t")
gc()

### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
