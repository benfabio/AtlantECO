
##### ATLANTECO SCRIPT 1.1.2 ----------------------------------------------------------------------------------------------------------------------------
##### 07/06/2021: R Script to format PhytoBase following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### For each large zooplankton group, aims to:
#	- Read and examine PhytoBase 
#   - Re-format to ATLANTECO WP2 template like you did for ZOObase
#   - Harmonize taxonomic classification & names with Script#1.4 (WoRMS AphiaID)

# module load R/4.0.3 # To load latest R version on kryo

### Latest update: 07/06/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/PHYTObase")

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the dataset by Damiano and examine content 
phy <- read.csv("PHYTObase_final_Righettietal._22_02_2021.csv", h = T)
dim(phy) # 1'360'621  36
str(phy)
colnames(phy)

### Check some fields
unique(phy$scientificName) # 1710 levels
unique(phy$occurrenceStatus)
unique(phy$basisOfRecord)
unique(phy$institutionCode)
unique(phy$sourceArchive)
# Here, need to correct a bit: Sal --> Sal&al._2013 ; VILLAR --> Villar&al._2015
phy[phy$sourceArchive == "SAL","sourceArchive"] <- "Sal&al._2013_doi.org/10.1890/13-0236.1"
phy[phy$sourceArchive == "VILLAR","sourceArchive"] <- "Villar&al._2015_doi.org/10.1126/science.1261447"

unique(phy$datasetKey_gbif) # Nice, useful for identifying CPR
unique(phy$publishingOrgKey_gbif) # unecessary 
unique(phy$collectionCode_obis)
unique(phy$resname_obis)
unique(phy$resourceID_obis) # to be used with datasetKey_gbif

### Important for tracing original datasets: where's the dataset key for non GBIF data?
head( phy[is.na(phy$datasetKey_gbif),] )
# The parameters “datasetKey_gbif” and “resourceID_obis” are keys to access metadata of original datasets in GBIF and OBIS via API, including information on sampling methods
unique(phy[is.na(phy$datasetKey_gbif),"resourceID_obis"])
unique(phy[is.na(phy$resourceID_obis),"datasetKey_gbif"])
# Are there some observtaions that have both? 
#summary( phy[!is.na(phy$datasetKey_gbif),"resourceID_obis"] )
#head( phy[!is.na(phy$datasetKey_gbif) & !is.na(phy$resourceID_obis),] )

phy[540:840,c("datasetKey_gbif","resourceID_obis")]

# Check quality flags
unique(phy$flag) # ? re-name more clearly 
phy[phy$flag == "S","flag"] <- "Records collected from sediment core or trap"
phy[phy$flag == "D","flag"] <- "Unrealistic original day value"
phy[phy$flag == "Y","flag"] <- "Unrealistic original year value"
phy[phy$flag == "N","flag"] <- "Original depth value was negative"
phy[phy$flag == "NS","flag"] <- "Original depth value was negative+Records collected from sediment core or trap"
phy[phy$flag == "NONE","flag"] <- NA

# And the quantitative ones?
summary(na.omit(phy$organismQuantity))
min(na.omit(phy$organismQuantity)) # all values > 0
min(na.omit(phy$individualCount)) # all values > 0
max(na.omit(phy$organismQuantity)) # 757185000
max(na.omit(phy$individualCount)) # 108702000

# Check which datasets are associated with quanti data
dim(phy[!is.na(phy$organismQuantity),]) # 7.7% of the full dataset
unique(phy[!is.na(phy$organismQuantity),"sourceArchive"]) # mixture of datasets, but basically MAREDAT data + Sal et al. 2013
unique(phy[!is.na(phy$organismQuantity),"originDatabase_maredat"])

unique(phy$organismQuantityType)
summary(factor(phy$organismQuantityType))

summary(factor(phy$individualCount)) # 1272100 NAs
summary(factor(phy$organismQuantity)) # 1255379 NAs
summary(phy$individualCount)

### What's the difference between individualCount & organismQuantityType? 
# - individualCount --> quanti vars from OBIS & GBIF 
unique(phy[phy$individualCount > 0,"sourceArchive"]) # Yep
# - organismQuantity --> data from MareDat and Sal et al. (2013)
unique(phy[phy$organismQuantity > 0,"sourceArchive"]) # Yep

### --> separate pure occ data from the quantitative ones
### Are there data with both?
# summary(phy[phy$individualCount > 0,"organismQuantity"]) # nope
# summary(phy[phy$organismQuantity > 0,"individualCount"]) # nope
# head(phy[phy$individualCount > 0 & !is.na(phy$individualCount),])
# head(phy[phy$organismQuantity > 0 & !is.na(phy$organismQuantity),])

### Separate the dataset in 3 pieces:
# - presence-only (no individualCount nor organismQuantity)
# - individualCount > 0 (quanti from OBIS+GBIF)
# - organismQuantity > 0 (quanti from Sal + MAREDAT)

# presence-only
part1 <- phy[is.na(phy$individualCount) & is.na(phy$organismQuantity),]
dim(part1) # 1'166'858
head(part1)

# individualCount > 0
part2 <- phy[!is.na(phy$individualCount),]
dim(part2) # 88'521
head(part2)

# organismQuantity > 0
part3 <- phy[!is.na(phy$organismQuantity),]
dim(part3) # 105'242
head(part3)

### --------------------------------------------------------------

### 2°) Re-format the 3 parts to AtlantECO WP2 template
colnames(part1) ; summary(part1)

rfmt.phy.p1 <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy", 
                ContactName = "Fabio_Benedetti;Meike_Vogt", ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", 
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = NA,
                decimalLatitude = part1$decimalLatitude, decimalLongitude = part1$decimalLongitude, geodeticDatum = "Assumed_WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = NA,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = part1$year, Month = part1$month, Day = part1$day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA",
                HabitatType = "Water column", LonghurstProvince = NA, Depth = part1$depth, DepthAccuracy = part1$depthAccuracy,
                DepthIntegral = NA, MinDepth = NA, MaxDepth = NA, 
                ParentEventID = NA, EventID = NA, InstitutionCode = part1$institutionCode, SourceArchive = part1$sourceArchive, 
                OrigCollectionCode = NA, OrigCollectionID = NA,
                BiblioCitation = "Righetti&al._2020_ESSD", CitationDOI = "doi.org/10.5194/essd-12-907-2020", DateDataAccess = '2021-02-22',
                OrigScientificName = part1$scientificName, ScientificName = part1$scientificName, WoRMS_ID = "To_add_at_the_end", TaxonRank = part1$taxonRank, 
                Kingdom = "Plantae", Phylum = part1$phylum, Class = part1$class, Order = NA, Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Occurrence", MeasurementTypeID = "To_define", MeasurementValue = "Presence",
                MeasurementUnit = NA, MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = part1$basisOfRecord, SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA, DeterminedBy = NA, DeterminedDate = NA, 
                Note = NA, Flag = part1$flag
) # eo ddf
### For rfmt.phy.p1$DatasetKey: paste together datasetKey_gbif & resourceID_obis
keys <- factor(paste("GBIF__",part1$datasetKey_gbif,"; OBIS__",part1$resourceID_obis, sep = ""))
head(unique(keys))
rfmt.phy.p1$DatasetKey <- keys

dim(rfmt.phy.p1) # 1'166'858  69
str(rfmt.phy.p1)

# Reformat part2 (individualCount)
colnames(part2)
summary(part2$individualCount)
unique(part2$organismQuantityType)

rfmt.phy.p2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy", 
                ContactName = "Fabio_Benedetti;Meike_Vogt", ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", 
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = NA,
                decimalLatitude = part2$decimalLatitude, decimalLongitude = part2$decimalLongitude, geodeticDatum = "Assumed_WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = NA,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = part2$year, Month = part2$month, Day = part2$day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA",
                HabitatType = "Water column", LonghurstProvince = NA, Depth = part2$depth, DepthAccuracy = part2$depthAccuracy,
                DepthIntegral = NA, MinDepth = NA, MaxDepth = NA, 
                ParentEventID = NA, EventID = NA, InstitutionCode = part2$institutionCode, SourceArchive = part2$sourceArchive, 
                OrigCollectionCode = NA, OrigCollectionID = NA,
                BiblioCitation = "Righetti&al._2020_ESSD", CitationDOI = "doi.org/10.5194/essd-12-907-2020", DateDataAccess = '2021-02-22',
                OrigScientificName = part2$scientificName, ScientificName = part2$scientificName, WoRMS_ID = "To_add_at_the_end", TaxonRank = part2$taxonRank, 
                Kingdom = "Plantae", Phylum = part2$phylum, Class = part2$class, Order = NA, Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = part2$organismQuantityType, MeasurementTypeID = "To_define", MeasurementValue = part2$individualCount,
                MeasurementUnit = part2$organismQuantityType, MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = part2$basisOfRecord, SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA, DeterminedBy = NA, DeterminedDate = NA, 
                Note = NA, Flag = part2$flag
) # eo ddf
dim(rfmt.phy.p2) # 88'521
str(rfmt.phy.p2)
keys <- factor(paste("GBIF__",part2$datasetKey_gbif,"; OBIS__",part2$resourceID_obis, sep = ""))
unique(keys)
rfmt.phy.p2$DatasetKey <- keys

# Reformat part3 (organismQuantity)
rfmt.phy.p3 <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy", 
                ContactName = "Fabio_Benedetti;Meike_Vogt", ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", 
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = NA,
                decimalLatitude = part3$decimalLatitude, decimalLongitude = part3$decimalLongitude, geodeticDatum = "Assumed_WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = NA,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = part3$year, Month = part3$month, Day = part3$day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA",
                HabitatType = "Water column", LonghurstProvince = NA, Depth = part3$depth, DepthAccuracy = part3$depthAccuracy,
                DepthIntegral = NA, MinDepth = NA, MaxDepth = NA, 
                ParentEventID = NA, EventID = NA, InstitutionCode = part3$institutionCode, SourceArchive = part3$sourceArchive, 
                OrigCollectionCode = NA, OrigCollectionID = NA,
                BiblioCitation = "Righetti&al._2020_ESSD", CitationDOI = "doi.org/10.5194/essd-12-907-2020", DateDataAccess = '2021-02-22',
                OrigScientificName = part3$scientificName, ScientificName = part3$scientificName, WoRMS_ID = "To_add_at_the_end", TaxonRank = part3$taxonRank, 
                Kingdom = "Plantae", Phylum = part3$phylum, Class = part3$class, Order = NA, Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = part3$organismQuantityType, MeasurementTypeID = "To_define", MeasurementValue = part3$organismQuantity,
                MeasurementUnit = part3$organismQuantityType, MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = part3$basisOfRecord, SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA, DeterminedBy = NA, DeterminedDate = NA, 
                Note = NA, Flag = part3$flag
) # eo ddf
dim(rfmt.phy.p3) # 105'242
str(rfmt.phy.p3)

keys <- factor(paste("GBIF__",part3$datasetKey_gbif,"; OBIS__",part3$resourceID_obis, sep = ""))
unique(keys)
rfmt.phy.p3$DatasetKey <- keys

### And rbind them
rfmt.phy <- rbind(rfmt.phy.p1, rfmt.phy.p2, rfmt.phy.p3)
dim(rfmt.phy)
str(rfmt.phy)

unique(rfmt.phy$DatasetKey)

### Save
save(rfmt.phy, file = "PhytoBase_reformated_07_06_2021.Rdata")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
