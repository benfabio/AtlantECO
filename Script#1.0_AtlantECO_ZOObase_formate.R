
##### ATLANTECO SCRIPT 1.0 ----------------------------------------------------------------------------------------------------------------------------
##### 21/04/2021: R Script to format ZOObase datasets following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### For each large zooplankton group, aims to:
#	- Copy/Paste ZOObase files (v2) from the OVERSEE dir to the AtlantECO dir
#   - Re-format all OBIS+GBIF files to ATLANTECO WP2 template
#   - Remove CPR dataset from both and re-add updated CPR data once 
#   - Finalize details later


module load R/4.0.3 # To load latest R version on kryo

### Latest update: 27/04/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Copy/Paste ZOObase files (v2) from the OVERSEE dir to the AtlantECO dir

setwd("/net/kryo/work/fabioben/OVERSEE/data/biology/occurence_data_groups/v2")
# dir()
files <- dir()[grep(".Rdata",dir())] ; files
for(f in files) {
    setwd("/net/kryo/work/fabioben/OVERSEE/data/biology/occurence_data_groups/v2")
    message(paste("Loading & saving: ",f, sep = ""))
    d <- get(load(f))
    setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021")
    save(d, file = f)
}

### And for Copepoda OBIS: separate folder
setwd("/net/kryo/work/fabioben/OVERSEE/data/biology/occurence_data_groups/v2/Copepoda_OBIS_23_04_18")
files <- dir()[grep(".Rdata",dir())] ; files

for(f in files) {
    d <- get(load(f))
    message(paste(f," has           Ncol = ",length(d), sep = ""))
    message(paste("", sep = ""))
}

# Choose colnames to retain from each file:
# - load and return vectors of colnames
# - use intersect to find largest common denominators (largest number of common headers across all OBIS copepod species file)
res.names <- mclapply(files, function(f) {
                    message(paste("Loading & saving: ",f, sep = ""))
                    d <- get(load(f))
                    return(colnames(d))
    }, mc.cores = 30
) # eo mclapply
#length(res.names)
# ?Reduce
# Identify common elements
common.headers <- Reduce(intersect, res.names)     

### Find the longets element of a list
nbs <- mclapply(c(1:length(res.names)), function(i) {
            l <- length(res.names[[i]])
            return(data.frame(i = i, length = l))
    }, mc.cores = 30
) # eo mclapply
nb <- bind_rows(nbs)
#summary(nb)
# max is 75 headers ! 
nb[nb$length == 75,]
# species #425 
#files[425] # Oithona similis...
d <- get(load(files[425]))
#colnames(d) # identify all non matching colnames between these & common.headers
UNcommon.headers <- setdiff(colnames(d), common.headers)

### OK, now rbinf all Copepoda files by filling all those missing variables with their values when they have one
setwd("/net/kryo/work/fabioben/OVERSEE/data/biology/occurence_data_groups/v2/Copepoda_OBIS_23_04_18")
files <- dir()[grep(".Rdata",dir())] ; files
#f <- files[100] # for testing
res <- mclapply(files, function(f) {
                    
                    message(paste("Loading & saving: ",f, sep = ""))
                    d <- get(load(f))
                    # Subset based on 'common.headers'
                    dd <- d[,common.headers]
                    
                    for(c in UNcommon.headers) {
                        
                        if(c %in% colnames(d)) {
                            dd[,c] <- d[,c]
                        } else {
                            dd[,c] <- NA
                        } # eo else if loop
                        
                    } # eo for loop
                    
                    return(dd)
                    
    }, mc.cores = 30
    
) # eo mclapply
# Rbind
table <- bind_rows(res)
str(table) ; dim(table) # 1 779 596 occurrences
summary(table)
rm(res) ; gc()

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021")
save(table, file = "Copepoda_OBIS_23_04_18_merged.Rdata")


### ----------------------------------------------------------------------------------------------------------------------------

### 2°) Re-format all files to ATLANTECO WP2 template
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
template <- read.csv("AtlantECO_data_template_WP2_21_04_21.csv", header = TRUE, sep = ";")
#class(template) ; str(template)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021")
dir()
# Check the colnames of the GBIF files & then the OBIS ones
files.gbif <- dir()[grep("_GBIF_",dir())] ; files.gbif
# files.obis <- dir()[grep("_OBIS_",dir())] ; files.obis
# Do all files have same length? 
for(f in files.gbif) {
    d <- get(load(f))
    message(paste(f," has           Ncol = ",length(d), sep = ""))
    message(paste(f," has           Latitudes as ",class(d$decimallatitude), sep = ""))
    message(paste(f," has           Longitudes as ",class(d$decimallongitude), sep = ""))
    message(paste("", sep = ""))
}

res.names <- mclapply(files.gbif, function(f) {
                message(paste("Loading & saving: ",f, sep = ""))
                d <- get(load(f))
                return(colnames(d))
    }, mc.cores = 15
) # eo mclapply
# Identify common elements
common.headers <- Reduce(intersect, res.names)     
length(common.headers)
# Looks like they all match! Nice. SO forst merge all GBIF together and try to adjust to template
colnames(template)
d <- get(load(files.gbif[1]))
names <- colnames(d)

### Now rbind all GBIF files
# f <- "Hydrozoa_GBIF_23_04_18.Rdata"
res <- mclapply(files.gbif, function(f) {
                
                message(paste("Loading & saving: ",f, sep = ""))
                d <- get(load(f))
                
                if(f == "Hydrozoa_GBIF_23_04_18.Rdata") {
                    d$decimallatitude <- as.numeric(as.character(d$decimallatitude))
                    d$decimallongitude <- as.numeric(as.character(d$decimallatitude))
                    
                    d$coordinateuncertaintyinmeters <- as.numeric(as.character(d$coordinateuncertaintyinmeters))
                    d$coordinateprecision <- as.numeric(as.character(d$coordinateprecision))
                    d$elevationaccuracy <- as.numeric(as.character(d$elevationaccuracy))
                    
                    d$depthaccuracy <- as.numeric(as.character(d$depthaccuracy))
                    d$day <- as.numeric(as.character(d$day))
                    #d$ <- as.numeric(as.character(d$))
                    
                    # message(paste(f," has           Latitudes as ",class(d$decimallatitude), sep = ""))
                    # message(paste(f," has           Longitudes as ",class(d$decimallongitude), sep = ""))
                }
                
                return(d[,names])
                
    }, mc.cores = 15
) # eo mclapply
# Rbind
#length(res) ; str(res)
table.gbif <- dplyr::bind_rows(res)
dim(table.gbif)
head(table.gbif)
rm(res) ; gc()
### Trouble shoot
#str(table.gbif)
#summary(table.gbif)
#unique(table.gbif$scientificname)
# Quickly fix this one: add uderscores are retrieve first 2 elements per row
table.gbif$species_name <- str_replace_all(table.gbif$scientificname," ","_")
#unique(table.gbif$species_name)[2149]
#table.gbif[2149,]
#head( do.call(rbind, strsplit(x = table.gbif$species_name, split = "_")) )
terms <- as.data.frame(do.call(rbind, strsplit(x = table.gbif$species_name, split = "_")))
#head(terms); dim(terms)
#unique(terms$V1) ; unique(terms$V2)
table.gbif$species_name <- paste(terms$V1, terms$V2, sep = "_")
#unique(table.gbif$species_name)
table.gbif[1:100,c("scientificname","species_name")]
# OK.

### Reformat merged GBIF data to AtlantECO template
colnames(table.gbif)
colnames(template)
unique(table.gbif$infraspecificepithet)
merged.gbif <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy", 
                ContactName = "Fabio_Benedetti;Meike_Vogt", ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", 
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = table.gbif$occurrenceid, gbifID = table.gbif$gbifid, DatasetKey = table.gbif$datasetkey,
                decimalLatitude = table.gbif$decimallatitude, decimalLongitude = table.gbif$decimallongitude, geodeticDatum = "Assumed_WGS84", 
                CoordUncertainty = table.gbif$coordinateuncertaintyinmeters, CountryCode = table.gbif$countrycode, eventDate = table.gbif$eventdate,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = table.gbif$year, Month = table.gbif$month, Day = table.gbif$day, Bathymetry = NA, BathySource = "ETOPO1-NOAA",
                HabitatType = "Water column", LonghurstProvince = NA, Depth = table.gbif$depth, DepthAccuracy = table.gbif$depthaccuracy, DepthIntegral = NA, MinDepth = NA, MaxDepth = NA, 
                ParentEventID = NA, EventID = NA, InstitutionCode = table.gbif$institutioncode, SourceArchive = "GBIF", 
                OrigCollectionCode = table.gbif$collectioncode, OrigCollectionID = table.gbif$occurrenceid,
                BiblioCitation = "Benedetti_et_al._(subm. to Nat. Comms)", CitationDOI = NA, DateDataAccess = '2018-04-23',
                OrigScientificName = table.gbif$scientificname, ScientificName = table.gbif$species, WoRMS_ID = "To_add_at_the_end", TaxonRank = table.gbif$taxonrank, 
                Kingdom = "Animalia", Phylum = table.gbif$phylum, Class = table.gbif$class, Order = table.gbif$order,
                Family = table.gbif$family, Genus = table.gbif$genus, Species = table.gbif$species,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Occurrence", MeasurementTypeID = "To_define", MeasurementValue = "Presence",
                MeasurementUnit = NA, MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA, basisOfRecord = table.gbif$basisofrecord, 
                SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA, DeterminedBy = table.gbif$identifiedby, DeterminedDate = NA, 
                Note = table.gbif$issue, Flag = NA 
) # eo ddf

dim(merged.gbif) # 926,597 & 70
str(merged.gbif)

### Save
save(merged.gbif, file = "ZOObase_GBIF_merged_reformated_23_04_2021.Rdata")



### 27/04/2021: OBIS files now 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
template <- read.csv("AtlantECO_data_template_WP2_21_04_21.csv", header = TRUE, sep = ";")
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021")
files.obis <- dir()[grep("_OBIS_",dir())] ; files.obis

### Check if headers are common
res.names <- mclapply(files.obis, function(f) {
                message(paste("Loading & saving: ",f, sep = ""))
                d <- get(load(f))
                return(colnames(d))
    }, mc.cores = 10
) # eo mclapply
# Identify common elements
common.headers <- Reduce(intersect, res.names)     
length(common.headers)
### Identify if those contain all the headers you need (compare to headers of Copepoda_OBIS_23_04_18_merged.Rdata)
test <- get(load("Copepoda_OBIS_23_04_18_merged.Rdata"))
UNcommon.headers <- setdiff(colnames(test), common.headers)

# WHo's missing these fields?
for(f in files.obis) {
    d <- get(load(f))
    message(paste(f," has           Ncol = ",length(d), sep = ""))
    message(paste("", sep = ""))
}
### Most datasets have >140 headers except Copepoda_merged (limiting dataset)
res.names <- mclapply(files.obis[c(1,2,4:21)], function(f) {
                message(paste("Loading & saving: ",f, sep = ""))
                d <- get(load(f))
                return(colnames(d))
    }, mc.cores = 10
) # eo mclapply
# Identify common elements
common.headers2 <- Reduce(intersect, res.names) 
length(common.headers2) # 136
UNcommon.headers2 <- setdiff(common.headers2, common.headers)
### Not that important anyway. Merge based on 'common.headers' 

res <- mclapply(files.obis, function(f) {
                
                message(paste("Loading & saving: ",f, sep = ""))
                d <- get(load(f))
                
               # message(paste(f," has           recordNumber as ",class(d$recordNumber), sep = ""))
                
                if(class(d$eventID) != "factor") {
                    d$eventID <- as.factor(d$eventID)
                } # eo if loop
                
                if(class(d$coordinateUncertaintyInMeters) != "numeric") {
                    d$coordinateUncertaintyInMeters <- as.numeric(as.character(d$coordinateUncertaintyInMeters))
                } # eo if loop
                
                if(class(d$taxonID) != "factor") {
                    d$taxonID <- as.factor(d$taxonID)
                } # eo if loop
                
                if(class(d$locationID) != "factor") {
                    d$locationID <- as.factor(d$locationID)
                } # eo if loop
                
                if(class(d$recordNumber) != "factor") {
                    d$recordNumber <- as.factor(d$recordNumber)
                } # eo if loop
                
                return(d[,common.headers])
                
    }, mc.cores = 20
) # eo mclapply
table.obis <- dplyr::bind_rows(res)
dim(table.obis) # 2'288'615 , 66 cols
head(table.obis)
colnames(table.obis)
str(table.obis)

### Need to extarct day, month and year from eventDate
require("lubridate")
# unique(table.obis$eventDate)
table.obis$eventDate <- as.Date(table.obis$eventDate)
summary(table.obis$eventDate)
head(table.obis$eventDate)
head(lubridate::year(table.obis$eventDate))
head(lubridate::month(table.obis$eventDate))
head(lubridate::day(table.obis$eventDate))

table.obis$Year <- lubridate::year(table.obis$eventDate)
table.obis$Month <- lubridate::month(table.obis$eventDate)
table.obis$Day <- lubridate::day(table.obis$eventDate)

summary(table.obis$Year)
summary(table.obis$Month)
summary(table.obis$Day)
# Schön :-) 

colnames(table.obis)
str(table.obis$associatedReferences)
unique(table.obis$occurrenceRemarks)
unique(table.obis$scientificName)
na.omit(table.obis[table.obis$occurrenceStatus == "absent",])

### Re-format to AtlantECO template
merged.obis <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = table.obis$occurrenceID, obisID = table.obis$id, DatasetKey = table.obis$datasetID,
                decimalLatitude = table.obis$decimalLatitude, decimalLongitude = table.obis$decimalLongitude, geodeticDatum = "Assumed_WGS84", 
                CoordUncertainty = table.obis$coordinateUncertaintyInMeters, CountryCode = table.obis$county, eventDate = table.obis$eventDate,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = table.obis$Year, Month = table.obis$Month, Day = table.obis$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = table.obis$habitat, LonghurstProvince = NA,
                Depth = table.obis$depth, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = table.obis$minimumDepthInMeters, MaxDepth = table.obis$maximumDepthInMeters, 
                ParentEventID = table.obis$eventID, EventID = table.obis$fieldNumber, InstitutionCode = table.obis$institutionCode, SourceArchive = "OBIS", 
                OrigCollectionCode = table.obis$collectionCode, OrigCollectionID = table.obis$collectionID,
                BiblioCitation = "Benedetti_et_al._(subm. to Nat. Comms)", CitationDOI = table.obis$references, DateDataAccess = '2018-04-23',
                OrigScientificName = table.obis$scientificName, ScientificName = table.obis$scientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = table.obis$taxonRank, 
                Kingdom = "Animalia", Phylum = table.obis$phylum, Class = table.obis$class, Order = table.obis$order,
                Family = table.obis$family, Genus = table.obis$genus, Species = table.obis$species,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Occurrence", MeasurementTypeID = "To_define", MeasurementValue = "Presence",
                MeasurementUnit = NA, MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = table.obis$basisOfRecord, SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = table.obis$identifiedBy, DeterminedDate = table.obis$dateIdentified, Note = table.obis$occurrenceRemarks, Flag = NA 
) # eo ddf

dim(merged.obis) # 2,288,615 & 70
str(merged.obis)
summary(merged.obis)
merged.obis[1:1000,]

merged.obis$Depth <- abs(merged.obis$Depth)
merged.obis$MinDepth <- abs(merged.obis$MinDepth)
merged.obis$MaxDepth <- abs(merged.obis$MaxDepth)


### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021")
save(merged.obis , file = "ZOObase_OBIS_merged_reformated_27_04_2021.Rdata")


### ----------------------------------------------------------------------------------------------------------------------------

