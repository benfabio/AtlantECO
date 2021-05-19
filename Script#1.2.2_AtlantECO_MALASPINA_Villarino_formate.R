
##### ATLANTECO SCRIPT 1.2 ----------------------------------------------------------------------------------------------------------------------------
##### 29/04/2021: R Script to format MALASPINA occurrence datasets following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#   - Read MALASPINA species occurrence dataset found on PANGAEA (data from Villarino et al., 2017, Nat Comms)
#   - Reformat to AtlantECO template

module load R/4.0.3 # To load latest R version on kryo

### Latest update: 29/04/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")

world <- map_data("world") 

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) List .tab data to be imported in AtlantECO WP2 data synthesis:
# Malaspina_cocco.tab
# Malaspina_diatoms.tab
# Malaspina_dino.tab
# Malaspina_gel_zooplank.tab
# Rest of the files are OTUs...

### Examine strcuture of tab files
cocco <- read.table("Malaspina_cocco.tab", h = T, sep = "\t", skip = 199)
dim(cocco) ; head(cocco)
summary(cocco)
colnames(cocco) # Keep cols # 1:39
# Melt
m.cocco <- melt(cocco[,c(1:39)], id.vars = colnames(cocco)[c(1:8)] )
head(m.cocco) # Gut
unique(m.cocco$variable) ; str(m.cocco)
colnames(m.cocco)[c(9,10)] <- c("OrigScientificName","MeasurementValue")  # unique(m.cocco$OrigScientificName)
# Change species names (un-abbreviate)
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "A..quattrospina"] <- "Acanthoica quattrospina"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "A..robusta"] <- "Algirosphaera robusta"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "B..bigelowii"] <- "Braarudosphaera bigelowii"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "C..leptoporus"] <- "Calcidiscus leptoporus"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "C..brasiliensis"] <- "Calciosolenia brasiliensis"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "C..murrayi"] <- "Calciosolenia murrayi "
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "C..rigidus"] <- "Calciopappus rigidus"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "C..cristatus"] <- "Ceratolithus cristatus"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "D..tubifer"] <- "Discosphaera tubifer "
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "H..carteri"] <- "Helicosphaera carteri"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "Helicosphaera.sp...1.suave."] <- "Helicosphaera sp."
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "M..adriaticus"] <- "Michaelsarsia adriaticus"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "M..elegans"] <- "Michaelsarsia elegans"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "Oolithotus.spp."] <- "Oolithotus spp."
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "O..hydroideus"] <- "Ophiaster hydroideus"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "Palusphaera.spp."] <- "Palusphaera spp."
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "Papposphaera.sp."] <- "Papposphaera sp."
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "P..aurisinae"] <- "Poricalyptra aurisinae"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "P..syracusana"] <- "Pontosphaera syracusana"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "R..sessilis..sobre.Thalassiosira."] <- "Reticulofenestra sessilis"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "R..clavigera"] <- "Rhabdosphaera clavigera"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "S..apsteinii"] <- "Scyphosphaera apsteinii"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "S..pulchra..HET."] <- "Syracosphaera pulchra"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "S..pulchra..HOL."] <- "Syracosphaera pulchra"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "S..pirus"] <- "Syracosphaera pirus"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "S..prolongata"] <- "Syracosphaera prolongata"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "Syracosphaera.sp...peque_a.redonda."] <- "Syracosphaera sp."
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "Syracosphaera.spp."] <- "Syracosphaera spp."
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "U..irregularis"] <- "Umbellosphaera irregularis"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "U..annulus"] <- "Umbilicosphaera annulus"
levels(m.cocco$OrigScientificName)[levels(m.cocco$OrigScientificName) == "U..sibogae"] <- "Umbilicosphaera sibogae"

### Test map
ggplot() + geom_point(aes(x = Longitude, y = Latitude, fill = factor(MeasurementValue)),
            data = m.cocco[m.cocco$OrigScientificName == "Syracosphaera pulchra",], pch = 21, colour = "black") +
            scale_fill_manual(name = "Occurrence", values = c("red","blue")) + 
        geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
        coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
        scale_y_continuous(name = "", labels = c("90°S","60°S","30°S","Eq","30°N","60°N","90°N"), limits = c(-90,90), 
            breaks = c(-90,-60,-30,0,30,60,90), expand = c(0,0)) +
        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
            panel.grid.major = element_line(colour = "grey70",linetype = "dashed"), legend.position = "bottom") 
# Nice.

### Re-format cocco data to AtlantECO template: add columns progressively
colnames(m.cocco)
colnames(m.cocco)[1:8] <- c("ParentEventID","EventID","decimalLatitude","decimalLongitude","eventDate","Depth","MinDepth","MaxDepth")
# Add stuff progressively 
# m.cocco$DatasetKey <- "PANGAEA.874646"
# Extract date
require("lubridate")
m.cocco$eventDate <- as.Date(m.cocco$eventDate)
head(m.cocco$eventDate)
m.cocco$Year <- lubridate::year(m.cocco$eventDate)
m.cocco$Month <- lubridate::month(m.cocco$eventDate)
m.cocco$Day <- lubridate::day(m.cocco$eventDate)

# Convert 1/0 to "presence" "absence" in m.cocco$MeasurementValue
str(m.cocco$MeasurementValue)
m.cocco$MeasurementValue <- factor(m.cocco$MeasurementValue)
levels(m.cocco$MeasurementValue)[levels(m.cocco$MeasurementValue) == "0"] <- "Absence"
levels(m.cocco$MeasurementValue)[levels(m.cocco$MeasurementValue) == "1"] <- "Presence"


cocco_refrmat <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy",
                ContactName = "Fabio_Benedetti;Meike_Vogt", ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch",
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = "PANGAEA.874646",
                decimalLatitude = m.cocco$decimalLatitude, decimalLongitude = m.cocco$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m.cocco$eventDate,
                eventDateInterval = 7, eventDateIntervalUnit = "months", Year = m.cocco$Year, Month = m.cocco$Month, Day = m.cocco$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = m.cocco$Depth, DepthAccuracy = NA, DepthIntegral = 160,
                MinDepth = m.cocco$MinDepth, MaxDepth = m.cocco$MaxDepth, 
                ParentEventID = m.cocco$ParentEventID, EventID = m.cocco$EventID, InstitutionCode = "AZTI", SourceArchive = "PANGAEA", 
                OrigCollectionCode = NA, OrigCollectionID = NA,
                BiblioCitation = "Villarino, E et al. (2018): Large-scale ocean connectivity and planktonic body size. Nature Communications, 9(1)",
                CitationDOI = "https://doi.org/10.1038/s41467-017-02535-8", DateDataAccess = '2021-02-22',
                OrigScientificName = m.cocco$OrigScientificName, ScientificName = m.cocco$OrigScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "species", 
                Kingdom = NA, Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA, Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Occurrence", MeasurementTypeID = "To_define",
                MeasurementValue = m.cocco$MeasurementValue, MeasurementUnit = NA, MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = NA, SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = "Ernesto_Villarino", DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
# Check
dim(cocco_refrmat)
head(cocco_refrmat)
str(cocco_refrmat)

# Save
save(cocco_refrmat , file = "ZOObase_MALASPINA_Villarinoetal._2017_coccolithophores_reformated_29_04_2021.Rdata")


### Do the same for the 3 other groups

### ---------------------------------------------------------------------------------------------------------------------------- 

### 2°) Diatoms
diat <- read.table("Malaspina_diatoms.tab", h = T, sep = "\t", skip = 220)
dim(diat) ; head(diat)
summary(diat)
colnames(diat) # Keep cols # 1:71
# Save as.csv and change species names (un-abbreviate) on excel! 
write.table(diat, file = "Malaspina_diatoms_temporary.txt")

# Re-load the v2 version
diat2 <- read.csv("Malaspina_diatoms_temporary_v2.csv", h = T, sep = ";")

colnames(diat)
colnames(diat2)
str(diat)
str(diat2)

diat2[,c(1:8)] <- diat[,c(1:8)]
summary(diat2)

# Melt
m.diat <- melt(diat2, id.vars = colnames(diat2)[c(1:8)] )
head(m.diat) # Gut
unique(m.diat$variable) ; str(m.diat)
colnames(m.diat)[c(9,10)] <- c("OrigScientificName","MeasurementValue")  # unique(m.diat$OrigScientificName)
summary(m.diat)

### Test map
ggplot() + geom_point(aes(x = Longitude, y = Latitude, fill = factor(MeasurementValue)),
            data = m.diat[m.diat$OrigScientificName == "Asterionellopsis_glacialis",], pch = 21, colour = "black") +
            scale_fill_manual(name = "Occurrence", values = c("red","blue")) + 
        geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
        coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
        scale_y_continuous(name = "", labels = c("90°S","60°S","30°S","Eq","30°N","60°N","90°N"), limits = c(-90,90), 
            breaks = c(-90,-60,-30,0,30,60,90), expand = c(0,0)) +
        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
            panel.grid.major = element_line(colour = "grey70",linetype = "dashed"), legend.position = "bottom") 

### Re-format diat data to AtlantECO template: add columns progressively
colnames(m.diat)
colnames(m.diat)[1:8] <- c("ParentEventID","EventID","decimalLatitude","decimalLongitude","eventDate","Depth","MinDepth","MaxDepth")
# Add stuff progressively 
# m.diat$DatasetKey <- "PANGAEA.874646"
# Extract date
require("lubridate")
m.diat$eventDate <- as.Date(m.diat$eventDate)
m.diat$Year <- lubridate::year(m.diat$eventDate)
m.diat$Month <- lubridate::month(m.diat$eventDate)
m.diat$Day <- lubridate::day(m.diat$eventDate)

# Convert 1/0 to "presence" "absence" in m.diat$MeasurementValue
str(m.diat$MeasurementValue)
m.diat$MeasurementValue <- factor(m.diat$MeasurementValue)
levels(m.diat$MeasurementValue)[levels(m.diat$MeasurementValue) == "0"] <- "Absence"
levels(m.diat$MeasurementValue)[levels(m.diat$MeasurementValue) == "1"] <- "Presence"

summary(m.diat)

# Define 'm.diat$ScientificName' by removing underscores in m.diat$OrigScientificName
unique(m.diat$OrigScientificName)
m.diat$ScientificName <- str_replace_all(m.diat$OrigScientificName, "_", " ")
unique(m.diat$ScientificName)
# Fix some labels
m.diat[m.diat$ScientificName == "Chaetoceros sp. Solitary","ScientificName"] <- "Chaetoceros sp. (solitary)"
m.diat[m.diat$ScientificName == "Chaetoceros spp. ..20micron.","ScientificName"] <- "Chaetoceros spp. (>20micron)"
m.diat[m.diat$ScientificName == "Chaetoceros spp. ..20micron..1","ScientificName"] <- "Chaetoceros spp. (<20micron)"
m.diat[m.diat$ScientificName == "Chaetoceros spp. .spores.","ScientificName"] <- "Chaetoceros spp. (spores)"
m.diat[m.diat$ScientificName == "Pseudosolenia calcar.avis","ScientificName"] <- "Pseudosolenia calcar-avis"
m.diat[m.diat$ScientificName == "Rhizosolenia sp. .styliformis type.","ScientificName"] <- "Rhizosolenia sp. (styliformis type)"


diat_refrmat <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy",
                ContactName = "Fabio_Benedetti;Meike_Vogt", ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch",
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = "PANGAEA.874647",
                decimalLatitude = m.diat$decimalLatitude, decimalLongitude = m.diat$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m.diat$eventDate,
                eventDateInterval = 7, eventDateIntervalUnit = "months", Year = m.diat$Year, Month = m.diat$Month, Day = m.diat$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = m.diat$Depth, DepthAccuracy = NA, DepthIntegral = 160,
                MinDepth = m.diat$MinDepth, MaxDepth = m.diat$MaxDepth, 
                ParentEventID = m.diat$ParentEventID, EventID = m.diat$EventID, InstitutionCode = "AZTI", SourceArchive = "PANGAEA", 
                OrigCollectionCode = NA, OrigCollectionID = NA,
                BiblioCitation = "Villarino, E et al. (2018): Large-scale ocean connectivity and planktonic body size. Nature Communications, 9(1)",
                CitationDOI = "https://doi.org/10.1038/s41467-017-02535-8", DateDataAccess = '2021-02-22',
                OrigScientificName = m.diat$OrigScientificName, ScientificName = m.diat$ScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "species", 
                Kingdom = "Plantae", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA, Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Occurrence", MeasurementTypeID = "To_define",
                MeasurementValue = m.diat$MeasurementValue, MeasurementUnit = NA, MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = NA, SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = "Ernesto_Villarino", DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
# Check
dim(diat_refrmat)
head(diat_refrmat)
str(diat_refrmat)
summary(diat_refrmat)

# Save
save(diat_refrmat , file = "ZOObase_MALASPINA_Villarinoetal._2017_diatoms_reformated_29_04_2021.Rdata")


### ---------------------------------------------------------------------------------------------------------------------------- 

### 3°) Dinos

dino <- read.table("Malaspina_dino.tab", h = T, sep = "\t", skip = 388)
dim(dino) ; head(dino)
summary(dino)
str(dino)
colnames(dino)[1:8] # Keep cols # 1:39

# Like diatoms, save as.csv and change species names (un-abbreviate) on excel! 
# write.table(dino, file = "Malaspina_dinos_temporary.txt")
# dino$T..robustum

# Re-load the v2 version
dino2 <- read.csv("Malaspina_dinos_temporary_v2.csv", h = T, sep = ";")
colnames(dino2)
dino2[,c(1:8)] <- dino[,c(1:8)]
summary(dino2)
str(dino2)

m.dino <- melt(dino2, id.vars = colnames(dino2)[c(1:8)] )
head(m.dino) # Gut
unique(m.dino$variable) ; str(m.dino)
colnames(m.dino)[c(9,10)] <- c("OrigScientificName","MeasurementValue")
# unique(m.dino$OrigScientificName)
summary(m.dino)

### Test map
ggplot() + geom_point(aes(x = Longitude, y = Latitude, fill = factor(MeasurementValue)),
            data = m.dino[m.dino$OrigScientificName == "Protoceratium_areolatum",], pch = 21, colour = "black") +
            scale_fill_manual(name = "Occurrence", values = c("red","blue")) + 
        geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
        coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
        scale_y_continuous(name = "", labels = c("90°S","60°S","30°S","Eq","30°N","60°N","90°N"), limits = c(-90,90), 
            breaks = c(-90,-60,-30,0,30,60,90), expand = c(0,0)) +
        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
            panel.grid.major = element_line(colour = "grey70",linetype = "dashed"), legend.position = "bottom") 

### Re-format dino data to AtlantECO template: add columns progressively
colnames(m.dino)
colnames(m.dino)[1:8] <- c("ParentEventID","EventID","decimalLatitude","decimalLongitude","eventDate","Depth","MinDepth","MaxDepth")
# Add stuff progressively 
# m.dino$DatasetKey <- "PANGAEA.874646"
# Extract date
require("lubridate")
m.dino$eventDate <- as.Date(m.dino$eventDate)
head(m.dino$eventDate)
m.dino$Year <- lubridate::year(m.dino$eventDate)
m.dino$Month <- lubridate::month(m.dino$eventDate)
m.dino$Day <- lubridate::day(m.dino$eventDate)

# Convert 1/0 to "presence" "absence" in m.dino$MeasurementValue
str(m.dino$MeasurementValue)
m.dino$MeasurementValue <- factor(m.dino$MeasurementValue)
levels(m.dino$MeasurementValue)[levels(m.dino$MeasurementValue) == "0"] <- "Absence"
levels(m.dino$MeasurementValue)[levels(m.dino$MeasurementValue) == "1"] <- "Presence"
# summary(m.dino$MeasurementValue)

# Adjust ScientificName
m.dino$ScientificName <- str_replace_all(m.dino$OrigScientificName, "_", " ")
unique(m.dino$ScientificName)
# Fix some labels
m.dino[m.dino$ScientificName == "Gymnodinium spp. ..40micron.","ScientificName"] <- "Gymnodinium spp. (>40micron)"
m.dino[m.dino$ScientificName == "Gymnodinium spp. ..40micron..1","ScientificName"] <- "Gymnodinium spp. (<40micron)"
m.dino[m.dino$ScientificName == "Blepharocysta splendor.maris","ScientificName"] <- "Blepharocysta splendor-maris"


dino_refrmat <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy",
                ContactName = "Fabio_Benedetti;Meike_Vogt", ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch",
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = "PANGAEA.874650",
                decimalLatitude = m.dino$decimalLatitude, decimalLongitude = m.dino$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m.dino$eventDate,
                eventDateInterval = 7, eventDateIntervalUnit = "months", Year = m.dino$Year, Month = m.dino$Month, Day = m.dino$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = m.dino$Depth, DepthAccuracy = NA, DepthIntegral = 160,
                MinDepth = m.dino$MinDepth, MaxDepth = m.dino$MaxDepth, 
                ParentEventID = m.dino$ParentEventID, EventID = m.dino$EventID, InstitutionCode = "AZTI", SourceArchive = "PANGAEA", 
                OrigCollectionCode = NA, OrigCollectionID = NA,
                BiblioCitation = "Villarino, E et al. (2018): Large-scale ocean connectivity and planktonic body size. Nature Communications, 9(1)",
                CitationDOI = "https://doi.org/10.1038/s41467-017-02535-8", DateDataAccess = '2021-02-22',
                OrigScientificName = m.dino$OrigScientificName, ScientificName = m.dino$ScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "species", 
                Kingdom = "Plantae", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA, Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Occurrence", MeasurementTypeID = "To_define",
                MeasurementValue = m.dino$MeasurementValue, MeasurementUnit = NA, MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = NA, SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = "Ernesto_Villarino", DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
# Check
dim(dino_refrmat)
head(dino_refrmat)
str(dino_refrmat)

# Save
save(dino_refrmat , file = "MALASPINA_Villarinoetal._2017_dinoflagellates_reformated_29_04_2021.Rdata")


### ---------------------------------------------------------------------------------------------------------------------------- 

### 4°) gel_zooplank
### Examine strcuture of tab files
gel <- read.table("Malaspina_gel_zooplank.tab", h = T, sep = "\t", skip = 89)
dim(gel) ; head(gel)
summary(gel)
colnames(gel) 
# Melt
m.gel <- melt(gel, id.vars = colnames(gel)[c(1:6)] )
head(m.gel) # Gut
unique(m.gel$variable) ; str(m.gel)
colnames(m.gel)[c(7,8)] <- c("OrigScientificName","MeasurementValue")  # unique(m.gel$OrigScientificName)
# Change species names (un-abbreviate)
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "C..affinis"] <- "Cyclosalpa affinis"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "P..confederata"] <- "Pegea confederata"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "T..vagina"] <- "Thetys vagina"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "S..fusiformis"] <- "Salpa fusiformis"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "I..zonaria"] <- "Iasis zonaria"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "T..rhomboides"] <- "Thalia rhomboides"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "S..thompsoni"] <- "Salpa thompsoni"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "P..porpita"] <- "Porpita porpita"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "V..velella"] <- "Velella velella"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "T..democratica"] <- "Thalia democratica"
levels(m.gel$OrigScientificName)[levels(m.gel$OrigScientificName) == "I..racoviatzi"] <- "Ihela racoviatzi"

### Test map
ggplot() + geom_point(aes(x = Longitude, y = Latitude, fill = factor(MeasurementValue)),
            data = m.gel[m.gel$OrigScientificName == "Thalia democratica",], pch = 21, colour = "black") +
            scale_fill_manual(name = "Occurrence", values = c("red","blue")) + 
        geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
        coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
        scale_y_continuous(name = "", labels = c("90°S","60°S","30°S","Eq","30°N","60°N","90°N"), limits = c(-90,90), 
            breaks = c(-90,-60,-30,0,30,60,90), expand = c(0,0)) +
        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
            panel.grid.major = element_line(colour = "grey70",linetype = "dashed"), legend.position = "bottom") 

### Re-format gel data to AtlantECO template: add columns progressively
colnames(m.gel)
colnames(m.gel)[1:6] <- c("ParentEventID","decimalLatitude","decimalLongitude","eventDate","EventID","Depth")
# Add stuff progressively 
# m.gel$DatasetKey <- "PANGAEA.874646"
# Extract date
require("lubridate")
m.gel$eventDate <- as.Date(m.gel$eventDate)
head(m.gel$eventDate)
m.gel$Year <- lubridate::year(m.gel$eventDate)
m.gel$Month <- lubridate::month(m.gel$eventDate)
m.gel$Day <- lubridate::day(m.gel$eventDate)

# Convert 1/0 to "presence" "absence" in m.gel$MeasurementValue
str(m.gel$MeasurementValue)
m.gel$MeasurementValue <- factor(m.gel$MeasurementValue)
levels(m.gel$MeasurementValue)[levels(m.gel$MeasurementValue) == "0"] <- "Absence"
levels(m.gel$MeasurementValue)[levels(m.gel$MeasurementValue) == "1"] <- "Presence"


gel_refrmat <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy",
                ContactName = "Fabio_Benedetti;Meike_Vogt", ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch",
                occurrenceID = "To_define_within_AtlantECO", orig_occurrenceID = NA, DatasetKey = "PANGAEA.874651",
                decimalLatitude = m.gel$decimalLatitude, decimalLongitude = m.gel$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m.gel$eventDate,
                eventDateInterval = 7, eventDateIntervalUnit = "months", Year = m.gel$Year, Month = m.gel$Month, Day = m.gel$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = m.gel$Depth, DepthAccuracy = NA, DepthIntegral = 0,
                MinDepth = 0, MaxDepth = 0, 
                ParentEventID = m.gel$ParentEventID, EventID = m.gel$EventID, InstitutionCode = "AZTI", SourceArchive = "PANGAEA", 
                OrigCollectionCode = NA, OrigCollectionID = NA,
                BiblioCitation = "Villarino, E et al. (2018): Large-scale ocean connectivity and planktonic body size. Nature Communications, 9(1)",
                CitationDOI = "https://doi.org/10.1038/s41467-017-02535-8", DateDataAccess = '2021-02-22',
                OrigScientificName = m.gel$OrigScientificName, ScientificName = m.gel$OrigScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "species", 
                Kingdom = NA, Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA, Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Occurrence", MeasurementTypeID = "To_define",
                MeasurementValue = m.gel$MeasurementValue, MeasurementUnit = NA, MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = NA, SamplingProtocol = NA, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = "Ernesto_Villarino", DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
# Check
dim(gel_refrmat)
head(gel_refrmat)
str(gel_refrmat)

# Save
save(gel_refrmat , file = "ZOObase_MALASPINA_Villarinoetal._2017_gelatinous_zooplankton_reformated_29_04_2021.Rdata")

### Save as .csv?
for(f in dir()[grep("reformated_29_04_2021.Rdata",dir())] ) {
    
    d <- get(load(f)) # f <- "MALASPINA_Villarinoetal._2017_diatoms_reformated_29_04_2021.Rdata"
    write.table(d, file = str_replace_all(f,'.Rdata','.txt'), sep = "\t")
    
} # eo for loop


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------

