
##### ATLANTECO SCRIPT 1.1.1 ----------------------------------------------------------------------------------------------------------------------------
##### 04/06/2021: R Script to format the copepod abundance data from Cornils et al. (2018) following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#### All ran on local machine, not kryo

### For each large zooplankton group, aims to:
#	- Load the dataset combining all the data tables from Cornils et al. (2018): "antarctic_out_AC.txt"
#   - Examine str
#   - Re-format to ATLANTECO WP2 template, and load on kryo

### Latest update: 04/06/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")
library("worms")

### ----------------------------------------------------------------------------------------------------------------------------

### 04/06/2021: Load the data and examine content
data <- read.table("antarctic_out_AC.txt", sep = "\t", h = T, fill = NA)
dim(data) # 1729  491
str(data)
colnames(data)
summary(data[,c("y","x")])

# Before melting the dataset, replace double dots by "_"
colnames(data)[11:490] <- gsub("...", "___", as.character(colnames(data)[11:490]), fixed = TRUE)
colnames(data)[11:490] <- gsub("..", "__", as.character(colnames(data)[11:490]), fixed = TRUE)
colnames(data)[11:490] <- gsub(".", "_", as.character(colnames(data)[11:490]), fixed = TRUE)

# Melt, with following colnames ad id.vars: event, date, Date_Time_of_event, y, x, elevation, depth, mindepth, maxdepth, volume, comment, and DOI
m.data <- melt(data, id.vars = c("event","date","Date_Time_of_event","y","x","elevation","depth","mindepth","maxdepth","volume","comment","DOI"))
m.data <- m.data %>% select(-Date_Time_of_event)
# dim(m.data) # 828'191     13
head(m.data)
unique(m.data$event)
unique(m.data$date)
unique(m.data$comment)
colnames(m.data)[12] <- "OrigScientificName"

# Derive "ScientificName" from "OrigScientificName"
# Derive ParentEventID (cruise) and EventID (station) from 'event'
# use comment to provide SamplingProtocol
# use volume to inform SampleAmount and SampleAmountUnit (m3)
# use OrigScientificName to inform LifeForm (gender, lifestage)

### For ParentEventID/ EventID
#dim(do.call(rbind, strsplit(x = as.character(m.data$event), split = "/")) ) # works
#unique(do.call(rbind, strsplit(x = as.character(m.data$event), split = "/"))[,1] )
#unique(do.call(rbind, strsplit(x = as.character(m.data$event), split = "/"))[,2] )
m.data$ParentEventID <- do.call(rbind, strsplit(x = as.character(m.data$event), split = "/"))[,1]
m.data$EventID <- do.call(rbind, strsplit(x = as.character(m.data$event), split = "/"))[,2]

### Derive ScientificName and LifeForm from OrigScientificName using "_" as split 
unique(m.data$OrigScientificName)
# test
#dim(do.call(rbind, strsplit(x = as.character(m.data$OrigScientificName), split = "__", fixed = T)) ) 
# head(do.call(rbind, strsplit(x = as.character(m.data$OrigScientificName), split = "__")) ) 
#unique( do.call(rbind, strsplit(x = as.character(m.data$OrigScientificName), split = "__", fixed = T))[,1] ) # OK, that's a first version of ScientificName
#unique( do.call(rbind, strsplit(x = as.character(m.data$OrigScientificName), split = "__", fixed = T))[,2] ) # and that's a first version of LifeForm
# unique( do.call(rbind, strsplit(x = as.character(m.data$OrigScientificName), split = "__", fixed = T))[,3] )

m.data$ScientificName <- do.call(rbind, strsplit(x = as.character(m.data$OrigScientificName), split = "__", fixed = T))[,1]
m.data$LifeForm <- do.call(rbind, strsplit(x = as.character(m.data$OrigScientificName), split = "__", fixed = T))[,2]
# Check
# unique(m.data$ScientificName)

### Remove: "_spp", "_sp" and then replace underscores by spaces
m.data$ScientificName <- str_replace_all(string = as.character(m.data$ScientificName), pattern = "_spp", replacement = "")
m.data$ScientificName <- str_replace_all(string = as.character(m.data$ScientificName), pattern = "_sp", replacement = "")
m.data$ScientificName <- str_replace_all(string = as.character(m.data$ScientificName), pattern = "_", replacement = " ")
unique(m.data$ScientificName)
# Good, ready for WoRMS cleaner
# Just need to fix 2 values: Calanoida indeterminata & Pseudochirellaectabilis
m.data[m.data$ScientificName == "Calanoida indeterminata","ScientificName"] <- "Calanoida"
m.data[m.data$ScientificName == "Pseudochirellaectabilis","ScientificName"] <- "Pseudochirella spectabilis"


### Clean LifeForm
# unique(m.data$LifeForm) 
# Convert to NA the following:
labs2remove <-  c("Oithonidae","Oncaeidae","Mormonillidae","Harpacticoida","Lubbockiidae","Ratania_atlantica","Mormonilloida",
        "Monstrilloida","Drescheriella_spp_","Ectinosoma_antarcticum","Harpacticus_furcifer","_Idomene_antarctica_","Microsetella_sp_",
           "_not_identified_","Pseudocyclopina_belgicae")
m.data[m.data$LifeForm %in% labs2remove & !is.na(m.data$LifeForm),"LifeForm"] <- NA
# Replace "_c1_c4" by ""
m.data[m.data$LifeForm == "_c1_c4" & !is.na(m.data$LifeForm),"LifeForm"] <- "c1+c4"
# unique(m.data$LifeForm) 
# Remove "°"
m.data$LifeForm <- str_replace_all(string = as.character(m.data$LifeForm), pattern = "_", replacement = "")
# Good.
colnames(m.data)

### Add WoRMS_ID & WoRMS_status
m.data[,c("WoRMS_ID","WoRMS_status")] <- NA

### And now use mclapply() to provide classif etc. to 'test2'
keys.worms <- wormsbynames( unique(m.data$ScientificName) )
keys.worms$ScientificName <- unique(m.data$ScientificName)

### OK, look clean enough, apply fun to get WoRMS' AphiaIDs 
# s <- unique(m.data$ScientificName)[3] # for testing
res <- mclapply(unique(keys.worms$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- m.data[m.data$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 25

) # eo mclapply - s in taxa_names
# Rbind
ddf <- dplyr::bind_rows(res)
# dim(ddf) # same dims as m.data, good
ddf[56150:56250,]
unique(ddf$Species) ; unique(ddf$WoRMS_ID)
rm(res) ; gc()

### And now, convert to AtlantECO template
colnames(ddf)
# Derive Year/Month/day based on lubridate
require("lubridate")
ddf$Year <- year(ddf$date)
ddf$Month <- month(ddf$date)
ddf$Day <- day(ddf$date)
# head(ddf) ; tail(ddf)
# summary(ddf[,c("Year","Month","Day")])
summary(ddf[,c("mindepth","maxdepth")])

summary(ddf$value) # drop those NAs
ddf <- ddf %>% drop_na(value)
dim(ddf)

table <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Astrid_Cornils",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;Astrid.Cornils@awi.de", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "None given", obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = ddf$y, decimalLongitude = ddf$x, geodeticDatum = "Assumed_WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = ddf$date,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = ddf$Year, Month = ddf$Month, Day = ddf$Day,
                Bathymetry = ddf$elevation, BathySource = "Values given in Cornils&al._2018", HabitatType = "water_column", LonghurstProvince = NA,
                Depth = ddf$depth, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = ddf$mindepth, MaxDepth = ddf$maxdepth, 
                ParentEventID = ddf$ParentEventID, EventID = ddf$EventID, InstitutionCode = "AWI", SourceArchive = "PANGAEA", 
                OrigCollectionCode = "PANGAEA.884619", OrigCollectionID = "See CitationDOI",
                BiblioCitation = "Cornils&al._2018_ESSD_doi.org/10.5194/essd-2018-36", CitationDOI = ddf$DOI, DateDataAccess = '2018-04-23',
                OrigScientificName = ddf$OrigScientificName, ScientificName = ddf$ScientificName,
                WoRMS_ID = ddf$WoRMS_ID, WoRMS_status = ddf$WoRMS_status, TaxonRank = ddf$TaxonRank, 
                Kingdom = "Animalia", Phylum = ddf$Phylum, Class = ddf$Class, Order = ddf$Order,
                Family = ddf$Family, Genus = ddf$Genus, Species = ddf$Species,
                Subspecies = NA, LifeForm = ddf$LifeForm, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Zooplankton_abundance", MeasurementTypeID = "To_define", MeasurementValue = ddf$value,
                MeasurementUnit = "ind.m3", MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = "Plankton net", SamplingProtocol = ddf$comment, SampleAmount = ddf$volume, SampleAmountUnit = "m3", SampleEffort = NA,
                DeterminedBy = NA, DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf

dim(table) # 260'826     71
str(table)
summary(table)
table[1:500,]

### saving new file
save(table, file = "ZOObase_Cornils&al._2018_SO_Copepods_reformated+WoRMScheck_04_06_2021.RData")
gc()



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
