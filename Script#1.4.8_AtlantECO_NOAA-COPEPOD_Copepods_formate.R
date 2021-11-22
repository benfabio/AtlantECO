
##### ATLANTECO SCRIPT 1.4.8 ----------------------------------------------------------------------------------------------------------------------------
##### 19/11/2021: R Script to examine and format of COPEPOD-NOAA Copepods dataset and reformat it following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Read the .csv file that you had to manually prepare on excel because of the copy-pasting issues
# - Examine strcuture, clean some values
# - Reformat to ATLANTECO WP2 template
# - Check and correct taxonomy with WoRMS

# Check: https://www.st.nmfs.noaa.gov/copepod/documentation/short-format_description.html 
# For format description 

### Latest update: 19/11/2021

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("lubridate")
library("viridis")

### In case you need them: load the Gear codes and Life stage codes table
# Gear codes
gears <- read.csv("copecode-biohmeta.gear.csv", h = T, sep = ",")[,c(1:2)]
head(gears) ; str(gears) ; unique(gears[,2])
gears[,2] <- trimws(x = gears[,2], which = "both")
# Life stages code
stages <- read.csv("copecode-taxameta.life_stage.csv", h = T, sep = ",")[,c(1:2)]
head(stages) ; str(stages) ; unique(stages[,2])
stages[,2] <- trimws(x = stages[,2], which = "both")
# Sex group table
sex <- read.csv("copecode-taxameta.sex.csv", h = T, sep = ",")[,c(1:2)]
head(sex) ; str(sex) ; unique(sex[,2])
sex[,2] <- trimws(x = sex[,2], which = "both")
# Modifiers table
mods <- read.csv("copecode-taxameta.modifier.csv", h = T, sep = ",")[,c(1:2)]
head(mods) ; str(mods) ; unique(mods[,2])
mods[,2] <- trimws(x = mods[,2], which = "both")

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the merged Gastropoda data
data <- read.csv("COPEPOD_copepod.csv", h = F, dec = ".", sep = ",", skip = 2)
dim(data) # 588'898     46
str(data)
which(rowSums(is.na(data)) == ncol(data)) # should return 'integer(0)'

colnames(data) # OK, colnames to provide and first three lines to be discarded
#head(data[-c(1:3),])
data <- data[-c(1:3),]

# (01) SHP-CRUISE   -   NODC ship code + COPEPOD internal cruise identifier - TO KEEP                           - TO KEEP
# (02) YEAR   -   year of the tow or sample                                                                     - TO KEEP
# (03) MONTH   -   month of the tow or sample                                                                   - TO KEEP
# (04) DAY   -   GMT-based day of the tow or sample                                                             - TO KEEP
# (05) TIMEgmt   -   GMT time of the tow or sample ( in decimal hours )                                         - TO KEEP BUT DON'T REALLY CARE
# (06) TIMEloc   -   LOCAL time of the tow or sample ( in decimal hours )                                       - TO KEEP BUT DON'T REALLY CARE
# (07) LATITUDE   -   latitude of the tow or sample ( + = North, - = South )                                    - TO KEEP
# (08) LONGITDE   -   longitude of the tow or sample ( + = East, - = West )                                     - TO KEEP
# (09) UPPER_Z   -   upper depth of the tow or bottle-                                                          - TO KEEP
# (10) LOWER_Z   -   lower depth of the tow or bottle                                                           - TO KEEP
# (11) T   -   tow type   ( Vertical, Horizontal, Oblique )                                                     - TO KEEP
# (12) GEAR   -   towing gear used ( COPEPOD Gear Table )                                                       - TO KEEP
# (13) MESH   -   net mesh size ( in micrometers )                                                              - TO KEEP
# (14) NMFS_PGC   -   COPEPOD Plankton Grouping Code (PGC) for the observation                                  - NOT TO KEEP
# (15) ITIS_TSN   -   ITIS Taxonomic Serial Number (TSN) or COPEPOD identifier (iff < 0)                        - NOT TO KEEP
# (16) MOD   -   taxonomic modifier, such as "sp.", "spp.", or "unknown". ( COPEPOD Taxonomic Modifier Table )  - TO KEEP
# (17) LIF   -   plankton lifestage, such as "adult", "nauplius", or "veliger". ( COPEPOD Lifestage Table )     - TO KEEP
# (18) PSC   -   COPEPOD Plankton [Life] Staging Code for the observation                                       - TO KEEP
#     0 = unspecified
#     1 = adult or sub-adult
#     2 = juvenile or larvae
#     3 = nauplius-like 4 = eggs 5 = incomplete body fragments
# (19) SEX   -   plankton sex, if provided ( 1=male, 2=female, 3+ = COPEPOD Sex Table )                         - TO KEEP

# (20) V   -   Type of (V)alue measurement in the Value fields
#     c = Number Count (e.g., number of species "X")
#     r = Relative Abundance code (e.g., "absent", "present", "common")
#     b = Total Net-haul Biomass Value
#     t = Invidual Taxa biomass or biovolume (value for that specific group or species)

# (21) Water Strained   -   volume of water sampled by bottle or flow meter ( in cubic meters ).                - TO KEEP
# (22) Original-VALUE   -   this is the original measured value ( in units provided below )                     - NOT TO KEEP
# (23) Orig-UNITS   -   this is the original units for the original value provided above                        - NOT TO KEEP
# (24) VALUE-per-volu[me]   -   standardized "per-water-volume" count or biomass measurement of the observation - TO KEEP
# (25) UNITS   -   Units for the VALUE-per-volu[me] above ( "#/m3" for zooplankton, "#/mL" for phytoplankton )  - TO KEEP

# (26) F1   -   COPEPOD-2010 global-annual range flag for the per-volu[me] value
# (27) F2   -   COPEPOD-2010 basin-annual range flag for the per-volu[me] value
# (28) F3   -   COPEPOD-2010 basin-seasonal range flag for the per-volu[me] value
# (29) F4   -   COPEPOD-2010 basin-monthly range flag for the per-volu[me] value

# (30) VALUE-per-area   -   standardized "per-surface-area" count or biomass measurement of the observation     - NOT TO KEEP
# These calculated "per area" values are currently in a testing state.   While the calculations appear sound,
# range flagging has not been run on these values.   Use these values at your own risk, with caution!.
# (31) UNITS   -   Units of the count or biomass measurement
# (32) F1   -   place-holder for future global-annual range flag for the per-area value
# (33) F2   -   place-holder for future basin-annual range flag for the per-area value
# (34) F3   -   place-holder for future basin-seasonal range flag for the per-area value
# (35) F4   -   place-holder for future basin-monthly range flag for the per-area value

# (36) SCIENTIFIC NAME -[ descriptive modifiers ]-   test description of the observation                        - TO KEEP
# (37) RECORD-ID   -   COPEPOD unique record identifier                                                         - TO KEEP
# (38) DATASET-ID   -   COPEPOD data set identifier                                                             - TO KEEP
# (39) SHIP   -   sampling ship (vessel) identifier ( COPEPOD Ship Table )                                      - TO KEEP
# (40) PROJ   -   associated project identifier ( COPEPOD Project Table )                                       - TO KEEP
# (41) INSTITUTE   -   associated institute identifier ( COPEPOD Institute Table )                              - TO KEEP

### Provide proper colnames: 
colnames(data)[c(1:46)] <- c("CRUISE","Year","Month","Day","TIMEgmt","TIMEloc","decimalLatitude","decimalLongitude","MinDepth","MaxDepth",
                            "Tow_type","Gear_code","Mesh","NMFS_PGC","ITIS_TSN","MOD","LIF","PSC","SEX","V",
                            "Water_strained","OrigValue","OrigUnits","ValPerVolume","Units","F1","F2","F3","F4","ValPerArea",
                            "UNITS","F1.1","F2.1","F3.1","F4.1","OrigScientificNameMod","RECORD.ID","DATASET.ID","SHIP","PROJ",
                            "INST","Unknown1","Unknown2","OrigScientificName","Modifiers","Unknown3")

### Then, examine, trim, discard and clean them: 
str(data)
unique(data$Unknown3) # To discard
unique(data$Unknown2) # to discard
unique(data$Unknown1) # to discard
# head(subset(data, select = -c(Unknown1,Unknown2,Unknown3)))
data <- subset(data, select = -c(Unknown1,Unknown2,Unknown3))
# Need to trim: CRUISE/Year/Month/Day/TIMEgmt/TIMEloc/decimalLatitude/decimalLongitude/MinDepth/MaxDepth/Tow_type/Gear_code/Mesh
# Water_strained/OrigValue/OrigUnits/ValPerVolume/Units/OrigScientificName/Modifiers
data$CRUISE <- trimws(data$CRUISE, which = c("both") )
data$Year <- trimws(data$Year, which = c("both") )
data$Month <- trimws(data$Month, which = c("both") )
data$Day <- trimws(data$Day, which = c("both") )
data$TIMEgmt <- trimws(data$TIMEgmt, which = c("both") )
data$TIMEloc <- trimws(data$TIMEloc, which = c("both") )
data$decimalLatitude <- trimws(data$decimalLatitude, which = c("both") )
data$decimalLongitude <- trimws(data$decimalLongitude, which = c("both") )
data$MinDepth <- trimws(data$MinDepth, which = c("both") )
data$MaxDepth <- trimws(data$MaxDepth, which = c("both") )
data$Tow_type <- trimws(data$Tow_type, which = c("both") )
data$Gear_code <- trimws(data$Gear_code, which = c("both") )
data$Mesh <- trimws(data$Mesh, which = c("both") )
data$Water_strained <- trimws(data$Water_strained, which = c("both") )
data$OrigValue <- trimws(data$OrigValue, which = c("both") )
data$OrigUnits <- trimws(data$OrigUnits, which = c("both") )
data$ValPerVolume <- trimws(data$ValPerVolume, which = c("both") )
data$Units <- trimws(data$Units, which = c("both") )
# Check
unique(data$CRUISE) # Leave as is
unique(data$Mesh) # Leave as is
unique(data$OrigUnits) # Leave as is
unique(data$Gear_code) # Leave as is

unique(data$Year) # convert to numerics
data$Year <- as.numeric(data$Year) ; summary(data$Year) # Good
unique(data$Month) # convert to numerics
data$Month <- as.numeric(data$Month) ; summary(data$Month) # Good
unique(data$Day) # convert to numerics
data$Day <- as.numeric(data$Day) ; summary(data$Day) # Good
unique(data$TIMEgmt) # convert to numerics
data$TIMEgmt <- as.numeric(data$TIMEgmt) ; summary(data$TIMEgmt) # Need to clean
# Replace -99.00 and 99.99 by NA
data[data$TIMEgmt == -99,"TIMEgmt"] <- NA
data[data$TIMEgmt == 99.99 & !is.na(data$TIMEgmt),"TIMEgmt"] <- NA
data[data$TIMEgmt < -5 & !is.na(data$TIMEgmt),"TIMEgmt"] <- NA

unique(data$TIMEloc) # convert to numerics
data$TIMEloc <- as.numeric(data$TIMEloc) ; summary(data$TIMEloc) # Need to clean
data[data$TIMEloc == -99 & !is.na(data$TIMEloc),"TIMEloc"] <- NA

unique(data$decimalLatitude) # convert to numerics
data$decimalLatitude <- as.numeric(data$decimalLatitude) ; summary(data$decimalLatitude)
unique(data$decimalLongitude) # convert to numerics
data$decimalLongitude <- as.numeric(data$decimalLongitude) ; summary(data$decimalLongitude)
unique(data$MinDepth) # convert to numerics
data$MinDepth <- as.numeric(data$MinDepth) ; summary(data$MinDepth) # Need to clean
unique(data$MaxDepth) # convert to numerics
data$MaxDepth <- as.numeric(data$MaxDepth) ; summary(data$MaxDepth) # Need to clean
data[data$MaxDepth == -999.9 & !is.na(data$MaxDepth),"MaxDepth"] <- NA
data[data$MinDepth == -999.9 & !is.na(data$MinDepth),"MinDepth"] <- NA

unique(data$Tow_type) # Replace "-" by NA
data[data$Tow_type == "-" & !is.na(data$Tow_type),"Tow_type"] <- NA

unique(data$Water_strained) # Replace "null" by NA and 
data[data$Water_strained == "null" & !is.na(data$Water_strained),"Water_strained"] <- NA

unique(data$OrigValue) # Replace "" by NA
data[data$OrigValue == "" & !is.na(data$OrigValue),"OrigValue"] <- NA

unique(data$ValPerVolume) # Replace "n/a" and "null" by NA and convert to numerics
data[data$ValPerVolume == "n/a" & !is.na(data$ValPerVolume),"ValPerVolume"] <- NA
data[data$ValPerVolume == "null" & !is.na(data$ValPerVolume),"ValPerVolume"] <- NA
# Check if you can do a as.numeric directly
data[58885:58895,"ValPerVolume"]
as.numeric(data[58885:58895,"ValPerVolume"])
# Looks OK...
data$ValPerVolumeNum <- as.numeric(data$ValPerVolume)
summary(data$ValPerVolumeNum) 
# Check some rows again to make sure the conversion occurred cleanly
data[data$ValPerVolumeNum == 17990002 & !is.na(data$ValPerVolume),c("ValPerVolume","ValPerVolumeNum")]
data[400000:400050,c("ValPerVolume","ValPerVolumeNum")]
# Guet

unique(data$Units) # Replace "----" and "-----" by NA
data[data$Units == "----" & !is.na(data$Units),"Units"] <- NA
data[data$Units == "-----" & !is.na(data$Units),"Units"] <- NA

### First, check spatial coordinates and dates, add a date column
summary(data[,c("Year","Month","Day","TIMEgmt","TIMEloc","decimalLatitude","decimalLongitude","MinDepth","MaxDepth")])

# Add date
data$Date <- ymd(paste(data$Year,data$Month,data$Day, sep = "-"))
str(data$Date)
summary(data$Date) # Nice

# Check other columns one by one
unique(data$Tow_type)
data[data$Tow_type == "H" & !is.na(data$Tow_type),"Tow_type"] <- "horizontal"
data[data$Tow_type == "V" & !is.na(data$Tow_type),"Tow_type"] <- "vertical"
data[data$Tow_type == "O" & !is.na(data$Tow_type),"Tow_type"] <- "oblique"
data[data$Tow_type == "S" & !is.na(data$Tow_type),"Tow_type"] <- "surface"

# Gear codes
unique(data$Gear_code) # https://www.st.nmfs.noaa.gov/copepod/codes/copecode-biohmeta.gear.html
# Use the COPEPOD Gear Code tabme to trace sampling protocol back
unique(data$Mesh) # in micrometers
unique(data$MOD) # 0 = unspecified; 1 = sp., 2 = spp., 6-7-8 = sp., 34 = other, 35 = unidentified
unique(data$LIF) # Use the COPEPOD Life Stage Code tabme to inform this
# 0 = unspecified, 1 = eggs, 2 = nauplii, 5 = veliger, 6 = larva, 7 = juvenile, 81 = CI-II-III
unique(data$PSC) # COPEPOD Plankton [Life] Staging Code for the observation
# 0 = unspecified, 1 = adult, 2 = juvenile or larva, 3 = nauplius, 4 = eggs, 5 = incomplete body fragments 
unique(data$SEX) # 0 = unspecified
unique(data$V) # V   -   Type of (V)alue measurement in the Value fields
# c = Number Count (e.g., number of species "X")
# r = Relative Abundance code (e.g., "absent", "present", "common")
# b = Total Net-haul Biomass Value
# t = Invidual Taxa biomass or biovolume (value for that specific group or species)

# Now, examine OrigScientificName
unique(data$OrigScientificNameMod)
unique(data$OrigScientificName) # 638 names...sounds like fun
unique(data$Modifiers) # several ancillary info in there
# Are all Modifiers in OrigScientifcNameWithMods already?
data[20000:20020,c("OrigScientificNameMod","Modifiers")]
# Looks like it, use ScientificName as OrigScientificName then. 

### Looks alright...reformat to AtlantECO WP2 template
colnames(data)
# Use 'gears' to inform actual gear names instead of code
unique(data$Gear_code)  # Make sure no -99 is inside 
data[data$Gear_code == -99 & !is.na(data$Gear_code),"Gear_code"] <- NA
data$gear <- NA
for(code in unique(na.omit(data$Gear_code)) ) {
        message(paste(code,"  =  ",gears[gears$Gear.Code == code,"Gear.Description"], sep = ""))
        g <- unique(gears[gears$Gear.Code == code,"Gear.Description"])
        # Provide to data$gear
        data[data$Gear_code == code & !is.na(data$Gear_code),"gear"] <- g        
} # eo for loop 
unique(data$gear)

# unique(paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = ""))
# summary(factor(paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = "")))
# unique(paste("Tow type= ",data$Tow_type,"; with: ",data$gear,"; mesh= ",data$Mesh, sep = ""))

data2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@env.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = data$RECORD.ID, obisID = "Not_applicable", DatasetKey = "4212000",
                decimalLatitude = data$decimalLatitude, decimalLongitude = data$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = data$Date,
                eventDateInterval = data$TIMEgmt, eventDateIntervalUnit = "GMT time of sampling (in decimal hours)",
                Year = data$Year, Month = data$Month, Day = data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = data$MinDepth, MaxDepth = data$MaxDepth, ParentEventID = paste("Cruise code="," ",data$CRUISE, sep = ""),
                EventID = NA, InstitutionCode = paste("Institute code="," ",data$INST, sep = ""),
                SourceArchive = "https://www.st.nmfs.noaa.gov/copepod/atlas/html/taxatlas_4212000.html",
                OrigCollectionCode = NA, OrigCollectionID = data$DATASET.ID,
                BiblioCitation = "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.",
                CitationDOI = NA, DateDataAccess = "15-10-2021",
                OrigScientificName = data$OrigScientificNameMod, ScientificName = data$OrigScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "To_add_at_the_end", 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = data$Modifiers, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = data$ValPerVolumeNum,
                MeasurementUnit = data$Units, MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = ""),
                SamplingProtocol = paste("Tow type= ",data$Tow_type,"; with: ",data$gear,"; mesh= ",data$Mesh, sep = ""),
                SampleAmount = data$Water_strained,
                SampleAmountUnit = "See SampleAmount", SampleEffort = NA,
                DeterminedBy = NA,
                DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
dim(data2) # 588'895     70
str(data2)
head(data2)

unique(data2$basisOfRecord)
unique(data2$SamplingProtocol)

# Check scientific names used
unique(data2$ScientificName) #; str(data2$ScientificName)
data2$ScientificName <- str_replace_all(data2$ScientificName, " sp. A", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " sp. B", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " sp. C", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " spp.", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " sp.", "")
data2[data2$ScientificName == "Copepoda TOTAL (total count)" & !is.na(data2$ScientificName),"ScientificName"] <- "Copepoda"
data2[data2$ScientificName == "Calanus TOTAL (total count)" & !is.na(data2$ScientificName),"ScientificName"] <- "Calanus"
data2[data2$ScientificName == "Harpacticoida TOTAL (total count)" & !is.na(data2$ScientificName),"ScientificName"] <- "Harpacticoida"
data2[data2$ScientificName == "Calanus finmarchicus (aff)" & !is.na(data2$ScientificName),"ScientificName"] <- "Calanus finmarchicus"
data2[data2$ScientificName == "Microcalanids parts/fragments" & !is.na(data2$ScientificName),"ScientificName"] <- "Microcalanus"

### Now, give worms::wormsbynames a first try and manually correct the labels that won't fit
require("worms") 
keys.worms <- wormsbynames( unique(data2$ScientificName), marine_only = "false" )
# To correct manually
# Diaxis (sars 1903)                                   no match
data2[data2$ScientificName == "Diaxis (sars 1903)","ScientificName"] <- "Diaixis"
# P-cal & parac & claus & pseud & cten copepodids      no match
data2[data2$ScientificName == "P-cal & parac & claus & pseud & cten copepodids","ScientificName"] <- "Calanoida"
# Cyclopina litoralis                                  no match
data2[data2$ScientificName == "Cyclopina litoralis","ScientificName"] <- "Cyclopinoides littoralis"
# Harpacticoida unidentified                           no match
data2[data2$ScientificName == "Harpacticoida unidentified","ScientificName"] <- "Harpacticoida"
# Calanoida unidentified                               no match
data2[data2$ScientificName == "Calanoida unidentified","ScientificName"] <- "Calanoida"
# Cyclops oithonoides ; g o                            no match
data2[data2$ScientificName == "Cyclops oithonoides ; g o","ScientificName"] <- "Thermocyclops oithonoides"
# Copepoda unidentified                                no match
data2[data2$ScientificName == "Copepoda unidentified","ScientificName"] <- "Copepoda"
# Cyclopoida unidentified                              no match
data2[data2$ScientificName == "Cyclopoida unidentified","ScientificName"] <- "Cyclopoida"
# Copepoda parts/fragments                             no match
data2[data2$ScientificName == "Copepoda parts/fragments","ScientificName"] <- "Copepoda"
# Pleuromamma robusta f antarctica                     no match
data2[data2$ScientificName == "Pleuromamma robusta f antarctica","ScientificName"] <- "Pleuromamma robusta antarctica"
# Pseudochirellactabilis                               no match
data2[data2$ScientificName == "Pseudochirellactabilis","ScientificName"] <- "Pseudochirella spectabilis"
# Zausnatus                                            no match
data2[data2$ScientificName == "Zausnatus","ScientificName"] <- "Zaus spinatus spinatus"
# Eudiaptomus gracilloides                             no match
data2[data2$ScientificName == "Eudiaptomus gracilloides","ScientificName"] <- "Eudiaptomus graciloides"
# Augaptilusnifrons                                    no match
data2[data2$ScientificName == "Augaptilusnifrons","ScientificName"] <- "Augaptilus spinifrons"
# Heterorhabdusnifrons                                 no match
data2[data2$ScientificName == "Heterorhabdusnifrons","ScientificName"] <- "Heterorhabdus spinifrons"
# Corycaeusciosus                                      no match
data2[data2$ScientificName == "Corycaeusciosus","ScientificName"] <- "Corycaeus speciosus"
# Euaugaptilus palumboi                                no match
data2[data2$ScientificName == "Euaugaptilus palumboi","ScientificName"] <- "Euaugaptilus palumboii"
# Phaennanifera                                        no match
data2[data2$ScientificName == "Phaennanifera","ScientificName"] <- "Phaenna spinifera"
# Euchaetanosa                                         no match
data2[data2$ScientificName == "Euchaetanosa","ScientificName"] <- "Euchaeta spinosa"
# Spinocalanusnosus                                    no match
data2[data2$ScientificName == "Spinocalanusnosus","ScientificName"] <- "Spinocalanus spinosus"
# Haloptilusniceps                                     no match
data2[data2$ScientificName == "Haloptilusniceps","ScientificName"] <- "Haloptilus spiniceps"
# Scaphocalanus minutus                                no match
data2[data2$ScientificName == "Scaphocalanus minutus","ScientificName"] <- "Scaphocalanus brevicornis"
# Acartianata                                          no match
data2[data2$ScientificName == "Acartianata","ScientificName"] <- "Acartia (Acanthacartia) spinata"
# Scolecithrichopsis ctenopus                          no match
data2[data2$ScientificName == "Scolecithrichopsis ctenopus","ScientificName"] <- "Scolecitrichopsis ctenopus"
# Heterorhabdusnifer                                   no match
data2[data2$ScientificName == "Heterorhabdusnifer","ScientificName"] <- "Heterorhabdus spinifer"
# Amallothrixnata                                      no match
data2[data2$ScientificName == "Amallothrixnata","ScientificName"] <- "Amallothrix spinata"
# Euchirellaendens                                     no match
data2[data2$ScientificName == "Euchirellaendens","ScientificName"] <- "Euchirella splendens"
# Arietellus armata                                    no match
data2[data2$ScientificName == "Arietellus armata","ScientificName"] <- "Arietellus armatus"
# Centropages elongata                                 no match
data2[data2$ScientificName == "Centropages elongata","ScientificName"] <- "Centropages elongatus"
# Pseudaugaptilus longiremus                           no match
data2[data2$ScientificName == "Pseudaugaptilus longiremus","ScientificName"] <- "Pseudaugaptilus longiremis"
# Heterorhabdusnosus                                   no match
data2[data2$ScientificName == "Heterorhabdusnosus","ScientificName"] <- "Heterorhabdus spinosus"
# Calocalanusnosus                                     no match
data2[data2$ScientificName == "Calocalanusnosus","ScientificName"] <- "Calocalanus spinosus"
# Centropages kroeyeri                                 no match
data2[data2$ScientificName == "Centropages kroeyeri","ScientificName"] <- "Centropages kroyeri"
# Diaixis pigmaea                                      no match
data2[data2$ScientificName == "Diaixis pigmaea","ScientificName"] <- "Diaixis pygmaea"
# Pareuchaeta antarctica                               no match
data2[data2$ScientificName == "Pareuchaeta antarctica","ScientificName"] <- "Paraeuchaeta antarctica"
# Euchirella mesinensis                                no match
data2[data2$ScientificName == "Euchirella mesinensis","ScientificName"] <- "Euchirella messinensis messinensis"
# Acartianicauda                                       no match
data2[data2$ScientificName == "Acartianicauda","ScientificName"] <- "Acartia (Odontacartia) spinicauda"
# Eucalanus pseudoattenuatus                           no match
data2[data2$ScientificName == "Eucalanus pseudoattenuatus","ScientificName"] <- "Pareucalanus attenuatus"
# Scolecithricella gracialis                           no match
data2[data2$ScientificName == "Scolecithricella gracialis","ScientificName"] <- "Scolecithricella minor minor"
# Clausocalanus breuipes                               no match
data2[data2$ScientificName == "Clausocalanus breuipes","ScientificName"] <- "Clausocalanus brevipes"
# Pareuchaeta erebi                                    no match
data2[data2$ScientificName == "Pareuchaeta erebi","ScientificName"] <- "Paraeuchaeta erebi"
# Pareuchaeta biloba                                   no match
data2[data2$ScientificName == "Pareuchaeta biloba","ScientificName"] <- "Paraeuchaeta biloba"
# Clausocalanus major                                  no match
data2[data2$ScientificName == "Clausocalanus major","ScientificName"] <- "Clausocalanus arcuicornis major"
# Pareuchaeta rasa                                     no match
data2[data2$ScientificName == "Pareuchaeta rasa","ScientificName"] <- "Paraeuchaeta rasa"
# Hetermalla dubia                                     no match
data2[data2$ScientificName == "Hetermalla dubia","ScientificName"] <- "Scopalatum dubia"
# Pareuchaeta grandiremis                              no match
data2[data2$ScientificName == "Pareuchaeta grandiremis","ScientificName"] <- "Paraeuchaeta grandiremis"
# Pareuchaeta incisa                                   no match
data2[data2$ScientificName == "Pareuchaeta incisa","ScientificName"] <- "Paraeuchaeta incisa"
# Pareuchaeta tonsa                                    no match
data2[data2$ScientificName == "Pareuchaeta tonsa","ScientificName"] <- "Paraeuchaeta tonsa"
# Pareuchaeta tumidula                                 no match
data2[data2$ScientificName == "Pareuchaeta tumidula","ScientificName"] <- "Paraeuchaeta tumidula"
# Scolecithricellanacantha                             no match
data2[data2$ScientificName == "Scolecithricellanacantha","ScientificName"] <- "Scolecithricella spinacantha"
# Aegisthusnulosus                                     no match
data2[data2$ScientificName == "Aegisthusnulosus","ScientificName"] <- "Aegisthus spinulosus"
# Corycaeus ilaccus                                    no match
data2[data2$ScientificName == "Corycaeus ilaccus","ScientificName"] <- "Agetus flaccus"
# Oithonanirostris                                     no match
data2[data2$ScientificName == "Oithonanirostris","ScientificName"] <- "Oithona setigera setigera"
# Oncaea notopa                                        no match
data2[data2$ScientificName == "Oncaea notopa","ScientificName"] <- "Oncaea notopus"
# Chiridiella abussalis                                no match
data2[data2$ScientificName == "Chiridiella abussalis","ScientificName"] <- "Chiridiella abyssalis"
# Copepoda other                                       no match
data2[data2$ScientificName == "Copepoda other","ScientificName"] <- "Copepoda"
# Copepoda miscellaneous                               no match
data2[data2$ScientificName == "Copepoda miscellaneous","ScientificName"] <- "Copepoda"
# Pareuchaeta japonica                                 no match
data2[data2$ScientificName == "Pareuchaeta japonica","ScientificName"] <- "Paraeuchaeta elongata"
# Microcalanids                                        no match
data2[data2$ScientificName == "Microcalanids","ScientificName"] <- "Microcalanus"
# Oithona setiger                                 no match
data2[data2$ScientificName == "Oithona setiger","ScientificName"] <- "Oithona setigera setigera"
# Calanus CPR-TOTAL "traverse"                         no match
data2[grepl(pattern = "Calanus CPR-TOTAL", x = data2$ScientificName),"ScientificName"] <- "Calanus"
# Metridia CPR-TOTAL "traverse"                        no match
data2[grepl(pattern = "Metridia CPR-TOTAL", x = data2$ScientificName),"ScientificName"] <- "Metridia"
# Centropages chierchiae CPR-TOTAL "eyecount"          no match
data2[grepl(pattern = "Centropages chierchiae CPR-TOTAL", x = data2$ScientificName),"ScientificName"] <- "Centropages chierchiae"

# Macrosetella sulcata                                 no match
data2[data2$ScientificName == "Macrosetella sulcata","ScientificName"] <- "Macrosetella sulcata" ### NO MATCH WHATSOEVER

# Do it again now
keys.worms <- wormsbynames( unique(data2$ScientificName), marine_only = "false" ) # The 9 that are left should be the "no.matchies"
keys.worms$ScientificName <- unique(data2$ScientificName)

# Add WoRMS_status field
data2 <-  add_column(data2, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(data2)

# For testing the functions below:
s <- unique(data2$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(data2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- data2[data2$ScientificName == s,]
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
rm(res); gc()
dim(ddf) # 588'895, same as before.
ddf[513735:513755,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
unique(ddf[is.na(ddf$Species),'ScientificName']) # unique(ddf[,"ScientificName"])
unique(ddf$WoRMS_ID)  
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS",'ScientificName']) 
# Good

### Check lifeforms
unique(ddf$LifeForm) # Ok, need to remove all those brackets etc.
# test <- gsub("\\[|\\]", "", unique(ddf$LifeForm)) # Good.
# gsub("- ", "", test)
# gsub(" -", "", test)
# OK, do all 3 in a row
ddf$LifeForm <- gsub("\\[|\\]", "", ddf$LifeForm)
ddf$LifeForm <- gsub("- ", "", ddf$LifeForm)
ddf$LifeForm <- gsub(" -", "", ddf$LifeForm)
ddf$LifeForm <- trimws(ddf$LifeForm, which = c("both") )
# Final check
unique(ddf$LifeForm)
# Replace "-" by NA
ddf[ddf$LifeForm == "-" & !is.na(ddf$LifeForm),"LifeForm"] <- NA


# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[588875:588895,c("OrigScientificName","ScientificName","LifeForm","TaxonRank","Genus")]
# Looks ganz OK :-)

### Last, make a map of sampling effort in space and then maybe a Hövmoller plot
d.effort <- ddf
d.effort$x_1d <- round(d.effort$decimalLongitude)
d.effort$y_1d <- round(d.effort$decimalLatitude)
d.effort$cell_id <- factor(paste(d.effort$x_1d, d.effort$y_1d, sep = "_"))
detach("package:worms", unload = TRUE)
detach("package:marmap", unload = TRUE) ; detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
require("dplyr")
spatial.effort <- data.frame(d.effort %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
dim(spatial.effort) ; summary(spatial.effort) # 4375 grid cells

# Map sampling effort
world <- map_data("world")  # for maps
ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "COPEPOD-NOAA_Copepoda_4212000_reformatted+WoRMScheck_19_11_21.Rdata")
write.table(ddf, file = "COPEPOD-NOAA_Copepoda_4212000_reformatted+WoRMScheck_19_11_21.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
