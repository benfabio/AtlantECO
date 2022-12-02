
##### ATLANTECO SCRIPT 1.2.1 ----------------------------------------------------------------------------------------------------------------------------
##### 03/05/2021: R Script to format MALASPINA bacteria and viral lysis data following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#   - Read MALASPINA viral lysis, and bacteria abundance (+ metadata) datasets sent by Marta Royo-Llonch
#   - Reformat to AtlantECO template

module load R/4.0.3 # To load latest R version on kryo

### Latest update: 04/05/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")
library("lubridate")

world <- map_data("world") 

### ----------------------------------------------------------------------------------------------------------------------------

### Test if you manage to read the .xlsx file  
library("xlsx")
library("readxl")

lysis_data <- read_excel("MALASPINA_LYSIS_LYSOGENY_DATA_20210412.xlsx", sheet = 1)
dim(lysis_data) ; str(lysis_data)
colnames(lysis_data)
# Gut! Melt by keeping the following cols as id.vars: c("STATION","LAYER","METHOD","CAST","DEPTH")
m_lysis_data <- melt(lysis_data, id.vars = c("STATION","LAYER","METHOD","CAST","DEPTH"))
dim(m_lysis_data) ; str(m_lysis_data)
m_lysis_data$CAST <- as.numeric(m_lysis_data$CAST)
summary(m_lysis_data$CAST)

### Ok, good start, now fetch the whole metadata to complete this dataset with the needed headers
meta <- data.frame(read_excel("MALASPINA_METADATA_20210409.xlsx", sheet = 1))
dim(meta) ; str(meta)
colnames(meta)
meta$DATE # needed to extract Year/Month/Day
meta$DATE <- as.Date(meta$DATE)
# lubridate::year(meta$DATE); lubridate::month(meta$DATE); lubridate::day(meta$DATE)
### Add YEAR/MONTH/DAY
meta$Year <- lubridate::year(meta$DATE)
meta$Month <- lubridate::month(meta$DATE)
meta$Day <- lubridate::day(meta$DATE)
#meta$DATE <- factor(meta$DATE)
meta$STATION <- as.integer(meta$STATION)
meta$DEPTH <- as.integer(meta$DEPTH)
meta$CAST <- as.integer(meta$CAST)
meta$MAX_Z <- as.numeric(meta$MAX_Z)
summary(meta)

summary(m_lysis_data)

# ### Identify common depth and stations
# common.stations <- intersect(unique(meta$STATION), unique(m_lysis_data$STATION))
# #common.stations
# common.depths <- intersect(unique(meta$DEPTH), unique(m_lysis_data$DEPTH))
# #common.depths

### Columns to add from 'meta' and 'm_lysis_data'?
cols2add <- setdiff(colnames(meta), colnames(m_lysis_data) ) ; cols2add
#m_lysis_data[,cols2add] <- NA
#head(m_lysis_data)
#str(m_lysis_data)

# With a double for loop, fill those
#st <- 13
#z <- 10

m_lysis_data$id <- paste(m_lysis_data$STATION, m_lysis_data$DEPTH, sep = "_") ; m_lysis_data$id
meta$id <- paste(meta$STATION, meta$DEPTH, sep = "_") ; meta$id
commons <- intersect(unique(m_lysis_data$id), unique(meta$id)) ; commons

# Cbind with lapply
c <- commons[10]
res <- lapply(commons, function(c) {
        sub.m_lysis_data <- m_lysis_data[m_lysis_data$id == c,]
        sub.m_lysis_data[,cols2add] <- unique(meta[meta$id == c,cols2add])[1,]
        return(sub.m_lysis_data)
    } # eo lapply - c in commons
) # eo lapply
# Rbind
m_lysis_data2 <- bind_rows(res)
summary(m_lysis_data2)
head(m_lysis_data2)
rm(res) ; gc()


### Good, re-format to AtlantECO WP2 template
colnames(m_lysis_data2)

m_lysis_data2$CAST <- as.numeric(m_lysis_data2$CAST)
m_lysis_data2$DATE
m_lysis_data2$LEG
m_lysis_data2$MAX_Z <- as.numeric(m_lysis_data2$MAX_Z)

lysis_refrmat <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy",
                ContactName = "Marta_Royo_Llonch;Silvia_Acinas", ContactAdress = "royo@icm.csic.es;sacinas@icm.csic.es",
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = NA,
                decimalLatitude = m_lysis_data2$LATITUDE, decimalLongitude = m_lysis_data2$LONGITUDE, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m_lysis_data2$DATE,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = m_lysis_data2$Year, Month = m_lysis_data2$Month, Day = m_lysis_data2$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = m_lysis_data2$LONGHURST,
                Depth = m_lysis_data2$DEPTH, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = NA, MaxDepth = m_lysis_data2$MAX_Z, 
                ParentEventID = m_lysis_data2$LEG, EventID = m_lysis_data2$STATION, InstitutionCode = "CSIC", SourceArchive = NA, 
                OrigCollectionCode = "MALASPINA viral lysis and lysogeny data", OrigCollectionID = NA,
                BiblioCitation = "Lara, E et al. (2017): Unveiling the role and life strategies of viruses from the surface to the dark ocean. Science Advances, Vol. 3, no. 9, e1602565",
                CitationDOI = "DOI:10.1126/sciadv.1602565", DateDataAccess = NA,
                OrigScientificName = "Does_not_apply", ScientificName = "Does_not_apply",
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "Does_not_apply", 
                Kingdom = "Does_not_apply", Phylum = "Does_not_apply", Class = "Does_not_apply", Order = "Does_not_apply",
                Family = "Does_not_apply", Genus = "Does_not_apply", Species = "Does_not_apply", Subspecies = "Does_not_apply", LifeForm = "Does_not_apply", AssocTaxa = "Does_not_apply",
                MeasurementID = "To_define", MeasurementType = m_lysis_data2$variable, MeasurementTypeID = "To_define",
                MeasurementValue = m_lysis_data2$value, MeasurementUnit = NA, MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = m_lysis_data2$METHOD, SamplingProtocol = NA, SampleAmount = m_lysis_data2$CAST, SampleAmountUnit = "Number of CTD or Bottle casts", 
                SampleEffort = NA, DeterminedBy = NA, DeterminedDate = NA, Note = "ParentEventID = MALASPINA leg", Flag = NA,
                Temp_CTD_celsius = m_lysis_data2$TEMPERATURE, Cond_CTD_S.m = m_lysis_data2$CONDUCTIVITY, Oxygen_CTD_V = m_lysis_data2$OXYGEN_VOLTAGE,
                Fluor_CTD = m_lysis_data2$FLUORESCENCE, PAR_CTD = m_lysis_data2$PAR, SPAR_CTD = m_lysis_data2$SPAR,
                Turbid_CTD_FTU = m_lysis_data2$TURBIDITY, Salinity_CTD = m_lysis_data2$SALINITY, Oxygen_CTD_mL = m_lysis_data2$OXYGEN_CONCENTRATION,
                NO3_WOA13 = m_lysis_data2$NO3_WOA13, PO4_WOA13 = m_lysis_data2$PO4_WOA13, SiO4_WOA13 = m_lysis_data2$SiO4_WOA13 
) # eo ddf

# check
summary(lysis_refrmat)
str(lysis_refrmat)

# Save as .txt file
write.table(lysis_refrmat, file = "MALASPINA_Lara_et_al._2017_lysis_data_reformated_04_05_2021.txt", sep = "\t")

# Open on excel and adjust manually
unique(lysis_refrmat$MeasurementType)
lysis_refrmat[lysis_refrmat$MeasurementType == "VMM_PERCENTAGE","MeasurementValue"]

### ----------------------------------------------------------------------------------------------------------------------------

### Do the same for the other measurements (bacteria)
data <- data.frame(read_excel("MALASPINA_TRADITIONAL_DATA_20210409.xlsx", sheet = 2))
# dim(data) # 1817   27
# str(data)
# colnames(data)
# class(data)
data2 <- data.frame(bind_rows(lapply(data, function(x) as.numeric(as.character(x)))))
# 
data[1:20,12]
data2[1:20,12]
rm(data)

# Gut! Melt by keeping the following cols as id.vars: c("STATION","LAYER","METHOD","CAST","DEPTH")
m_data <- melt(data2, id.vars = c("STATION","CAST","DEPTH"))
dim(m_data)
str(m_data)

cols2add <- setdiff(colnames(meta), colnames(m_data) ) ; cols2add

# With a double for loop, fill those
#st <- 13
#z <- 10

m_data$id <- paste(m_data$STATION, m_data$CAST, m_data$DEPTH, sep = "_") ; unique(m_data$id)
meta$id <- paste(meta$STATION, meta$CAST, meta$DEPTH, sep = "_") ; unique(meta$id)
commons <- intersect(unique(m_data$id), unique(meta$id)) ; commons

# Cbind with lapply
c <- commons[1000]
res <- lapply(commons, function(c) {
        sub <- m_data[m_data$id == c,]
        sub[,cols2add] <- unique( meta[meta$id == c,cols2add] )
        return(sub)
    } # eo lapply - c in commons
) # eo lapply
# Rbind
m_data2 <- bind_rows(res)
summary(m_data2)
head(m_data2) ; dim(m_data2)
str(m_data2)

### Looks OK, reformat to AtlantECO WP2 template
colnames(m_data2)

trad_refrmat <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy",
                ContactName = "Marta_Royo_Llonch;Silvia_Acinas", ContactAdress = "royo@icm.csic.es;sacinas@icm.csic.es",
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = NA,
                decimalLatitude = m_data2$LATITUDE, decimalLongitude = m_data2$LONGITUDE, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m_data2$DATE,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = m_data2$Year, Month = m_data2$Month, Day = m_data2$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = m_data2$LONGHURST,
                Depth = m_data2$DEPTH, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = NA, MaxDepth = m_data2$MAX_Z, 
                ParentEventID = m_data2$LEG, EventID = m_data2$STATION, InstitutionCode = "CSIC", SourceArchive = NA, 
                OrigCollectionCode = "To_be_added", OrigCollectionID = NA, BiblioCitation = "To_be_added", CitationDOI = "To_be_added", DateDataAccess = NA,
                OrigScientificName = "Does_not_apply", ScientificName = "Does_not_apply",
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "Does_not_apply", 
                Kingdom = "Does_not_apply", Phylum = "Does_not_apply", Class = "Does_not_apply", Order = "Does_not_apply",
                Family = "Does_not_apply", Genus = "Does_not_apply", Species = "Does_not_apply", Subspecies = "Does_not_apply", LifeForm = "Does_not_apply", AssocTaxa = "Does_not_apply",
                MeasurementID = "To_define", MeasurementType = m_data2$variable, MeasurementTypeID = "To_define",
                MeasurementValue = m_data2$value, MeasurementUnit = NA, MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = m_data2$METHOD, SamplingProtocol = NA, SampleAmount = m_data2$CAST, SampleAmountUnit = "Number of CTD or Bottle casts", 
                SampleEffort = NA, DeterminedBy = NA, DeterminedDate = NA, Note = "ParentEventID = MALASPINA leg", Flag = NA,
                Temp_CTD_celsius = m_data2$TEMPERATURE, Cond_CTD_S.m = m_data2$CONDUCTIVITY, Oxygen_CTD_V = m_data2$OXYGEN_VOLTAGE,
                Fluor_CTD = m_data2$FLUORESCENCE, PAR_CTD = m_data2$PAR, SPAR_CTD = m_data2$SPAR,
                Turbid_CTD_FTU = m_data2$TURBIDITY, Salinity_CTD = m_data2$SALINITY, Oxygen_CTD_mL = m_data2$OXYGEN_CONCENTRATION,
                NO3_WOA13 = m_data2$NO3_WOA13, PO4_WOA13 = m_data2$PO4_WOA13, SiO4_WOA13 = m_data2$SiO4_WOA13 
) # eo ddf

# check
summary(trad_refrmat)
str(trad_refrmat)

### To prevent excel from automatically changing numerics to dates...add a ' to numerics
trad_refrmat2 <- trad_refrmat
nums <- unlist(lapply(trad_refrmat2, is.numeric))  
nums <- names(nums[nums == TRUE])
nums
for(i in nums) {
    message(paste("Converting ",i," to char", sep = ""))
    trad_refrmat2[,i] <- as.character(trad_refrmat2[,i])
} # eo for loop - i in nums

head(trad_refrmat2[,nums])
str(trad_refrmat2)

# Save as .txt file
write.table(trad_refrmat, file = "MALASPINA_all_data_reformated_04_05_2021.txt", sep = "\t")



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
