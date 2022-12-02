
##### ATLANTECO SCRIPT 1.9 ----------------------------------------------------------------------------------------------------------------------------
##### 26/07/2021: R Script to reformat the TARA Oceans ZooScan imaging data publihsed by Brandao, Benedetti et al. (2021) - Scientific Reports © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Aims to:
#  - Read the excel sheet and reformat to AtlantECO WP2 template
#  - Provide AphiaID using WoRMS

### Latest update: 26/07/2021

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("readxl")
library("lubridate")

world <- map_data("world") 

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the data and reshape
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Plankton_Imaging") ; 
dir() 
data <- read_excel("TARA_Oceans_Zooplankton_Abundance_Brandaoetal.2021.Sc.Reports_to_reformat_20_07_21.xlsx", sheet = 1)
dim(data); str(data)

### Couple of things before melting the data.frame: convert 'Time' to 'Date' object
?as.Date
data$Time <- as.Date(data$Time)
str(data) ; summary(data$Time)
# Looks ok - melt
colnames(data)
m.data <- melt(data, id.vars = colnames(data)[c(1:12)])
head(m.data)
colnames(m.data)
summary(m.data)

unique(m.data$variable)
m.data$scientificName <- m.data$variable

### OK, can reformat to AtlantECO WP2 template :-) 
m.data$Year <- lubridate::year(m.data$Time)
m.data$Month <- lubridate::month(m.data$Time)
m.data$Day <- lubridate::day(m.data$Time)

tara <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Plankton_imaging", ContactName = "Fabio_Benedetti;Fabien_Lombard",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;fabien.lombard@imev-mer.fr", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = m.data$decimalLatitude, decimalLongitude = m.data$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m.data$Time,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = m.data$Year, Month = m.data$Month, Day = m.data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = m.data$DepthRange,
                MinDepth = m.data$MinDepth, MaxDepth = m.data$MaxDepth, 
                ParentEventID = m.data$Station, EventID = m.data$Tara_Sample_ID, InstitutionCode = "SU/CNRS-IMEV-LOV (UMR7093)", SourceArchive = "Not_applicable", 
                OrigCollectionCode = "Not_applicable", OrigCollectionID = "Not_applicable",
                BiblioCitation = "Brandao,Benedetti et al. (2021) - Scientific Reports", CitationDOI = "To provide later", DateDataAccess = "Not_applicable",
                OrigScientificName = m.data$scientificName, ScientificName = m.data$scientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = m.data$value,
                MeasurementUnit = "#/m3", MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = m.data[,"Plankton net"], SamplingProtocol = m.data$Protocol, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = NA, DeterminedDate = NA, Note = m.data[,"Size fraction"], Flag = NA 
) # eo ddf
head(tara)
dim(tara) # 15'908
str(tara)

# Check scientific names used
unique(tara$ScientificName) ; str(tara$ScientificName)
# Modify some levels
levels(tara$ScientificName)[levels(tara$ScientificName) == "Zooplankton"] <- "Total zooplankton"
levels(tara$ScientificName)[levels(tara$ScientificName) == "Other_large"] <- "Other large unidentified zooplankton"
levels(tara$ScientificName)[levels(tara$ScientificName) == "Other_small"] <- "Other small unidentified zooplankton"
levels(tara$ScientificName)[levels(tara$ScientificName) == "Other_unidentified"] <- "Unidentified plankton"
levels(tara$ScientificName)[levels(tara$ScientificName) == "Small_grazers"] <- "Ostracoda+Cladocera"
levels(tara$ScientificName)[levels(tara$ScientificName) == "Diatoms"] <- "Bacillariophyceae"
levels(tara$ScientificName)[levels(tara$ScientificName) == "Dinoflagelates"] <- "Dinophyceae"
levels(tara$ScientificName)[levels(tara$ScientificName) == "Calanoida_unid"] <- "Unidentified Calanoida"
levels(tara$ScientificName)[levels(tara$ScientificName) == "Copepoda_unid"] <- "Unidentified Copepoda"
# Conver to char for wormsbynames
tara$ScientificName <- as.character(tara$ScientificName)

### And finally, provide AphiaID using 'worms' functions
require("worms")
keys.worms <- wormsbynames( unique(tara$ScientificName) )
keys.worms$ScientificName <- unique(tara$ScientificName)

# Add WoRMS_status field
tara <-  add_column(tara, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(tara)

# For testing:
s <- unique(tara$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(tara$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- tara[tara$ScientificName == s,]
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

### Rbind
ddf <- bind_rows(res)
dim(ddf) # 15908
ddf[100:125,]
unique(ddf$Species)
unique(ddf$WoRMS_ID)
unique(ddf$WoRMS_status)

### Saving new file
save(ddf, file = "TARA_Oceans_plankton_zooscan_abundance_Brandao_et_al.2021.Sc.Reports_26_07_21.RData")
gc()
# And also as .txt for excel
write.table(x = ddf, file = "TARA_Oceans_plankton_zooscan_abundance_Brandao_et_al.2021.Sc.Reports_26_07_21.txt", sep = "\t")

### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
