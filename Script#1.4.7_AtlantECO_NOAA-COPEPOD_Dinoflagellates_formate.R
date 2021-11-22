
##### ATLANTECO SCRIPT 1.4.7 ----------------------------------------------------------------------------------------------------------------------------
##### 16/11/2021: R Script to examine and format of COPEPOD-NOAA Dinoflagellates dataset and reformat it following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Read the .csv file that you had to manually prepare on excel because of the copy-pasting issues
# - Examine strcuture, clean some values
# - Reformat to ATLANTECO WP2 template
# - Check and correct taxonomy with WoRMS

# Check: https://www.st.nmfs.noaa.gov/copepod/documentation/short-format_description.html 
# For format description 

### Latest update: 18/11/2021

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
data <- read.csv("COPEPOD_Dinoflagellates_2070000_09_11_21.csv", sep = ";", dec = ",", h = T)
dim(data) # 49691
str(data)
which(rowSums(is.na(data)) == ncol(data)) # should return 'integer(0)'

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

colnames(data)

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
dim(data[data$Tow_type == "B" & !is.na(data$Tow_type),]) # 13590 B tows? What does B correspond to? Bananas?

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

### Check #/m3
summary(data$ValPerVolume)

unique(data$Units); unique(data$OrigUnits)
data$Units <- trimws(data$UNITS, which = c("both") )
data$OrigUnits <- trimws(data$OrigUnits, which = c("both") )
unique(data$OrigUnits)

# Check strained water volume
#unique(data$Water_strained) # Need to trimws this
data$Water_strained <- trimws(data$Water_strained, which = c("both") )
unique(data$Water_strained)
# Good, leave like this for now...

# Now, examine OrigScientificName
unique(data$OrigScientificNameMod)
unique(data$OrigScientificName) # 638 names...sounds like fun
unique(data$Modifiers) # several ancillary info in there
# Are all Modifiers in OrigScientifcNameWithMods already?
data[20000:20020,c("OrigScientificNameMod","Modifiers")]
# Looks like it, use ScientificName as OrigScientificName then. 

### Looks alright...reformat to AtlantECO WP2 template
colnames(data)
unique(data$Tow_type)
# SamplingProtocol should be a combination of: gear+mesh+tow type. MESH and Tow_type are already in the right format, but gear codes...
# Use 'gears' to inform actual gear names instead of code
unique(data$Gear_code)  # Make sure no -99 is inside 
# data[data$Gear_code == -99 & !is.na(data$Gear_code),"Gear_code"] <- NA
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
                orig_occurrenceID = data$RECORD.ID, obisID = "Not_applicable", DatasetKey = "2070000",
                decimalLatitude = data$decimalLatitude, decimalLongitude = data$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = data$Date,
                eventDateInterval = data$TIMEgmt, eventDateIntervalUnit = "GMT time of sampling (in decimal hours)",
                Year = data$Year, Month = data$Month, Day = data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = data$MinDepth, MaxDepth = data$MaxDepth, ParentEventID = paste("Cruise code="," ",data$CRUISE, sep = ""),
                EventID = NA, InstitutionCode = paste("Institute code="," ",data$INST, sep = ""),
                SourceArchive = "https://www.st.nmfs.noaa.gov/copepod/atlas/html/taxatlas_2070000.html",
                OrigCollectionCode = NA, OrigCollectionID = data$DATASET.ID,
                BiblioCitation = "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.",
                CitationDOI = NA, DateDataAccess = "15-10-2021",
                OrigScientificName = data$OrigScientificNameMod, ScientificName = data$OrigScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "To_add_at_the_end", 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = data$Modifiers, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = data$ValPerVolume,
                MeasurementUnit = data$Units, MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = ""),
                SamplingProtocol = paste("Tow type= ",data$Tow_type,"; with: ",data$gear,"; mesh= ",data$Mesh, sep = ""),
                SampleAmount = data$Water_strained,
                SampleAmountUnit = "See SampleAmount", SampleEffort = NA,
                DeterminedBy = NA,
                DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
dim(data2) # 49'691  70
str(data2)
head(data2)

unique(data2$basisOfRecord)
unique(data2$SamplingProtocol)

# Check scientific names used
unique(data2$ScientificName) #; str(data2$ScientificName)
data2$ScientificName <- str_replace_all(data2$ScientificName, " spp.", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " sp.", "")

### Now, give worms::wormsbynames a first try and manually correct the labels that won't fit
require("worms") 
keys.worms <- wormsbynames( unique(data2$ScientificName), marine_only = "false" )
# To correct manually

# Ceratium inflatum                                  
data2[data2$ScientificName == "Ceratium inflatum","ScientificName"] <- "Tripos inflatus"
# Ceratium inflexum                                 
data2[data2$ScientificName == "Ceratium inflexum","ScientificName"] <- "Tripos inflexus"
# Prorocentrum schilleri                                  
data2[data2$ScientificName == "Prorocentrum schilleri","ScientificName"] <- "Prorocentrum micans"
# Peridinium marielebourae                             no match
data2[data2$ScientificName == "Peridinium marielebourae","ScientificName"] <- "Protoperidinium marielebouriae"
# Prorocentrum marina                                  no match
data2[data2$ScientificName == "Prorocentrum marina","ScientificName"] <- "Prorocentrum lima"
# Podolampasnifer                                      no match
data2[data2$ScientificName == "Podolampasnifer","ScientificName"] <- "Podolampas spinifer"
# Dinophysis caudata tripos                            no match
data2[data2$ScientificName == "Dinophysis caudata tripos","ScientificName"] <- "Dinophysis tripos"
# Histioneis voucki                                    no match
data2[data2$ScientificName == "Histioneis voucki","ScientificName"] <- "Histioneis vouckii"
# Pavillardinium intermedia                            no match
data2[data2$ScientificName == "Pavillardinium intermedia","ScientificName"] <- "Centrodinium pavillardii"
# Ceratium macroceros gallicum                         no match
data2[data2$ScientificName == "Ceratium macroceros gallicum","ScientificName"] <- "Ceratium macroceros var. gallicum"
# Gonyaulax kofoidi                                    no match
data2[data2$ScientificName == "Gonyaulax kofoidi","ScientificName"] <- "Gonyaulax kofoidii"
# Peridinium pendunculatum                             no match
data2[data2$ScientificName == "Peridinium pendunculatum","ScientificName"] <- "Protoperidinium pedunculatum"
# Ceratium evarcuatum                                  no match
data2[data2$ScientificName == "Ceratium evarcuatum","ScientificName"] <- "Tripos euarcuatus"
# Ornithocercus triclavatus                            no match
data2[data2$ScientificName == "Ornithocercus triclavatus","ScientificName"] <- "Ornithocercus heteroporus"
# Dinophysis micronatum                                no match
data2[data2$ScientificName == "Dinophysis micronatum","ScientificName"] <- "Dinophysis mucronata"
# Dinophysis okamurai                                  no match
data2[data2$ScientificName == "Dinophysis okamurai","ScientificName"] <- "Dinophysis okamurae"
# Ceratium horridum molle                              no match
data2[data2$ScientificName == "Ceratium horridum molle","ScientificName"] <- "Tripos mollis"
# Dinophysisaerica                                     no match
data2[data2$ScientificName == "Dinophysisaerica","ScientificName"] <- "Dinophysis sphaerica"
# Oxytoxum cribrosum                                   no match
data2[data2$ScientificName == "Oxytoxum cribrosum","ScientificName"] <- "Oxytoxum cribosum"
# Histioneis jorgenseni                                no match
data2[data2$ScientificName == "Histioneis jorgenseni","ScientificName"] <- "Histioneis joergensenii"
# Pronoctilucanifera                                   no match
data2[data2$ScientificName == "Pronoctilucanifera","ScientificName"] <- "Pronoctiluca spinifera"
# Amphidinium hyalinum                                 no match
data2[data2$ScientificName == "Amphidinium hyalinum","ScientificName"] <- "Prosoaulax lacustris" # According to AlgaeBase
# Corythodinium tessellatum                            no match
data2[data2$ScientificName == "Corythodinium tessellatum","ScientificName"] <- "Corythodinium tesselatum"
# Dinophysis bipartitum                                no match
data2[data2$ScientificName == "Dinophysis bipartitum","ScientificName"] <- "Dinophysis bipartita"
# Gonyaulax monocantha                                 no match
data2[data2$ScientificName == "Gonyaulax monocantha","ScientificName"] <- "Gonyaulax monacantha"
# Gymnodiniumaeroideum                                 no match
data2[data2$ScientificName == "Gymnodiniumaeroideum","ScientificName"] <- "Gymnodinium sphaeroideum"
# Dinophysis jourdani                                  no match
data2[data2$ScientificName == "Dinophysis jourdani","ScientificName"] <- "Ceratocorys horrida"
# Peridinium bipes                                     no match
data2[data2$ScientificName == "Peridinium bipes","ScientificName"] <- "Protoperidinium bipes"
# Ceratium bigelowi                                    no match
data2[data2$ScientificName == "Ceratium bigelowi","ScientificName"] <- "Tripos bigelowii"
# Lingulodinium polyedrum                              no match
data2[data2$ScientificName == "Lingulodinium polyedrum","ScientificName"] <- "Lingulodinium polyedra"
# Ornithocercus steini                                 no match
data2[data2$ScientificName == "Ornithocercus steini","ScientificName"] <- "Ornithocercus steinii"
# Pyrocystis biconica                                  no match
data2[data2$ScientificName == "Pyrocystis biconica","ScientificName"] <- "Pyrocystis fusiformis f. biconica"
# Protoperidiniumniferum                               no match
data2[data2$ScientificName == "Protoperidiniumniferum","ScientificName"] <- "Protoperidinium solidicorne"
# Peridinium obtusum                                   no match
data2[data2$ScientificName == "Peridinium obtusum","ScientificName"] <- "Protoperidinium obtusum"
# Goniodomaaericum                                     no match
data2[data2$ScientificName == "Goniodomaaericum","ScientificName"] <- "Goniodoma sphaericum"
# Gonyaulaxnifera                                      no match
data2[data2$ScientificName == "Gonyaulaxnifera","ScientificName"] <- "Gonyaulax spinifera"
# Heterodinium hindmarchi                              no match
data2[data2$ScientificName == "Heterodinium hindmarchi","ScientificName"] <- "Heterodinium hindmarchii"
# Heterodinium blackmani                               no match
data2[data2$ScientificName == "Heterodinium blackmani","ScientificName"] <- "Heterodinium blackmanii"
# Histioneis pietschmanii                              no match
data2[data2$ScientificName == "Histioneis pietschmanii","ScientificName"] <- "Histioneis pietschmannii"
# Peridinium gatunense                                 no match
data2[data2$ScientificName == "Peridinium gatunense","ScientificName"] <- "Peridinium gatunense var. carinatum"
# Dinophysis favus                                     no match
data2[data2$ScientificName == "Dinophysis favus","ScientificName"] <- "Dinophysis fava"
# Amphisolenia acutissimum                             no match
data2[data2$ScientificName == "Amphisolenia acutissimum","ScientificName"] <- "Amphisolenia acuta"
# Ceratium tripos tripodioides                         no match
data2[data2$ScientificName == "Ceratium tripos tripodioides","ScientificName"] <- "Tripos muelleri"
# Amphisolenia lemmermanni                             no match
data2[data2$ScientificName == "Amphisolenia lemmermanni","ScientificName"] <- "Amphisolenia lemmermannii"
# Amphisolenia schauinslandi                           no match
data2[data2$ScientificName == "Amphisolenia schauinslandi","ScientificName"] <- "Amphisolenia schauinslandii"
# Diplopsalis rotundata                                no match
data2[data2$ScientificName == "Diplopsalis rotundata","ScientificName"] <- "Diplopsalis caspica"
# Peridiniumaericum                                    no match
data2[data2$ScientificName == "Peridiniumaericum","ScientificName"] <- "Protoperidinium sphaericum"
# Heterodinium australe                                no match
data2[data2$ScientificName == "Heterodinium australe","ScientificName"] <- "Heterodinium australiae"
# Peridinium striolatum                                no match
data2[data2$ScientificName == "Peridinium striolatum","ScientificName"] <- "Peridinium willei" # FRESHWATER
# Diplopsalisaerica                                    no match
data2[data2$ScientificName == "Diplopsalisaerica","ScientificName"] <- "Diplopsalis sphaerica"
# Glenodinium gymnodinium                              no match
data2[data2$ScientificName == "Glenodinium gymnodinium","ScientificName"] <- "Naiadinium polonicum"
# Goniodoma polygramma                                 no match
data2[data2$ScientificName == "Goniodoma polygramma","ScientificName"] <- "Triadinium polyedricum"
# Peridinium tuba                                      no match
data2[data2$ScientificName == "Peridinium tuba","ScientificName"] <- "Protoperidinium tuba"
# Dinophysis obtusidens                                no match
data2[data2$ScientificName == "Dinophysis obtusidens","ScientificName"] <- "Dinophysis islandica"
# Peridinium cinctum                                   no match
data2[data2$ScientificName == "Peridinium cinctum","ScientificName"] <- "Peridinium cinctum" # FRESHWATER
# Oxytoxumaeroideum                                    no match
data2[data2$ScientificName == "Oxytoxumaeroideum","ScientificName"] <- "Oxytoxum sphaeroideum"
# Pavillardiniumnosum                                  no match
data2[data2$ScientificName == "Pavillardiniumnosum","ScientificName"] <- "Pavillardinium spinosum"
# Warnowia violacea                                    no match
data2[data2$ScientificName == "Warnowia violacea","ScientificName"] <- "Warnowia violescens"
# Gymnodiniumendens                                    no match
data2[data2$ScientificName == "Gymnodiniumendens","ScientificName"] <- "Akashiwo sanguinea"
# Peridinium okamurai                                  no match
data2[data2$ScientificName == "Peridinium okamurai","ScientificName"] <- "Peridinium okamurae"
# Amphidinium kesslitzi                                no match
data2[data2$ScientificName == "Amphidinium kesslitzi","ScientificName"] <- "Gymnodinium kesslitzii"
# Blepharocystaendormaris                              no match
data2[data2$ScientificName == "Blepharocystaendormaris","ScientificName"] <- "Blepharocysta splendor-maris"
# Gyrodinium stratissimum                              no match
data2[data2$ScientificName == "Gyrodinium stratissimum","ScientificName"] <- "Gyrodinium striatissimum"
# Gymnodinium rotundatum                               no match
data2[data2$ScientificName == "Gymnodinium rotundatum","ScientificName"] <- "Cystodinium unicorne"
# Gyrodiniumrale                                       no match
data2[data2$ScientificName == "Gyrodiniumrale","ScientificName"] <- "Gyrodinium spirale"
# Protoceratium cuerlatus                              no match
data2[data2$ScientificName == "Protoceratium cuerlatus","ScientificName"] <- "Protoceratium aculeatum"
# Ceratium fusus schutii                               no match
data2[data2$ScientificName == "Ceratium fusus schutii","ScientificName"] <- "Tripos fusus var. schuettii"
# Ceratium fusus seta                                  no match
data2[data2$ScientificName == "Ceratium fusus seta","ScientificName"] <- "Tripos seta"
# Dinophyceae other                                    no match
data2[data2$ScientificName == "Dinophyceae other","ScientificName"] <- "Dinophyceae"
# Dinophyceae TOTAL (total count)                      no match
data2[data2$ScientificName == "Dinophyceae TOTAL (total count)","ScientificName"] <- "Dinophyceae"
# Ornithocercusendidus                                 no match
data2[data2$ScientificName == "Ornithocercusendidus","ScientificName"] <- "Ornithocercus splendidus"
# Amphidinium lacustre                                 no match
data2[data2$ScientificName == "Amphidinium lacustre","ScientificName"] <- "Prosoaulax lacustris" # FRESHWATER
# Amphidiniumenoides                                   no match
data2[data2$ScientificName == "Amphidiniumenoides","ScientificName"] <- "Amphidinium sphenoides"
# Gyrodinium lacryna                                   no match
data2[data2$ScientificName == "Gyrodinium lacryna","ScientificName"] <- "Gyrodinium lachryma"
# Gymnodinium vitiligo                                 no match
data2[data2$ScientificName == "Gymnodinium vitiligo","ScientificName"] <- "Karlodinium vitiligo"  
# Protoperidinium monocanthum                          no match
data2[data2$ScientificName == "Protoperidinium monocanthum","ScientificName"] <- "Protoperidinium monacanthum"
# Adenoides kofoidi                                    no match
data2[data2$ScientificName == "Adenoides kofoidi","ScientificName"] <- "Pseudadenoides kofoidii"
# Gymnodinium hyalinum                                 no match
data2[data2$ScientificName == "Gymnodinium hyalinum","ScientificName"] <- "Gymnodinium hyalinum" # FRESHWATER
# Gymnodinium lohmanni                                 no match
data2[data2$ScientificName == "Gymnodinium lohmanni","ScientificName"] <- "Gymnodinium gracile"
# Cochlodinium brandti                                 no match
data2[data2$ScientificName == "Cochlodinium brandti","ScientificName"] <- "Cochlodinium brandtii"
# Pyrrophycophyta                                      no match
data2[data2$ScientificName == "Pyrrophycophyta","ScientificName"] <- "Dinophyceae" # https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=9873#null
# Nitzschia tenuirostris                               no match
data2[data2$ScientificName == "Nitzschia tenuirostris","ScientificName"] <- "Nitzschia tenuirostris" # Diatom - not dino - unchecked algaebase entry
# Phalacromaaeroideum                                  no match
data2[data2$ScientificName == "Phalacromaaeroideum","ScientificName"] <- "Phalacroma sphaeroideum"
# Ceratium tripos atlanticum                           no match
data2[data2$ScientificName == "Ceratium tripos atlanticum","ScientificName"] <- "Tripos muelleri"
# Prorocentrum sigmoides                               no match
data2[data2$ScientificName == "Prorocentrum sigmoides","ScientificName"] <- "Prorocentrum gracile"
# Gyrodinium adriaticum                                no match
data2[data2$ScientificName == "Gyrodinium adriaticum","ScientificName"] <- "Cochlodinium adriaticum"
# Ceratium buceros leptosomum                          no match
data2[data2$ScientificName == "Ceratium buceros leptosomum","ScientificName"] <- "Tripos buceros"
# Ceratium trichoceros contrarium                      no match
data2[data2$ScientificName == "Ceratium trichoceros contrarium","ScientificName"] <- "Tripos trichoceros"
# Ornithocercus calolinae                              no match
data2[data2$ScientificName == "Ornithocercus calolinae","ScientificName"] <- "Ornithocercus carolinae"
# Peridinium steinii v mediterranea                    no match
data2[data2$ScientificName == "Peridinium steinii v mediterranea","ScientificName"] <- "Protoperidinium mediterraneum"
# Gyrodinium pusillum                                  no match
data2[data2$ScientificName == "Gyrodinium pusillum","ScientificName"] <- "Gyrodinium pusillum" # FRESHWATER
# Phalacroma incinctum                                 no match
data2[data2$ScientificName == "Phalacroma incinctum","ScientificName"] <- "Phalacroma circumcinctum"
# Ceratium buceros tenue                               no match
data2[data2$ScientificName == "Ceratium buceros tenue","ScientificName"] <- "Tripos tenuis" # Uncertain still
# Amphisolenia retangulata                             no match
data2[data2$ScientificName == "Amphisolenia retangulata","ScientificName"] <- "Amphisolenia rectangulata"
# Oxytoxumaeroideum v steinii                          no match
data2[data2$ScientificName == "Oxytoxumaeroideum v steinii","ScientificName"] <- "Oxytoxum sphaeroideum"
# Amphisolenianulosa                                   no match
data2[data2$ScientificName == "Amphisolenianulosa","ScientificName"] <- "Amphisolenia spinulosa"
# Ceratium furca eugrammum                             no match
data2[data2$ScientificName == "Ceratium furca eugrammum","ScientificName"] <- "Tripos eugrammus"
# Ceratium karstenii robustum                          no match
data2[data2$ScientificName == "Ceratium karstenii robustum","ScientificName"] <- "Tripos karstenii"
# Cystodinium steinii                                  no match
data2[data2$ScientificName == "Cystodinium steinii","ScientificName"] <- "Cystodinium cornifax"
# Ceratium pentagonum robustum                         no match
data2[data2$ScientificName == "Ceratium pentagonum robustum","ScientificName"] <- "Tripos pentagonus"
# Prorocentrum ostenfeldii?                            no match
data2[data2$ScientificName == "Prorocentrum ostenfeldii?","ScientificName"] <- "Alexandrium ostenfeldii"
# Peridinium parvum                                    no match
data2[data2$ScientificName == "Peridinium parvum","ScientificName"] <- "Protoperidinium cruciferum"
# Ceratium contortum v saltans                         no match
data2[data2$ScientificName == "Ceratium contortum v saltans","ScientificName"] <- "Tripos saltans"
# Ceratium gibberum v sinistrum                        no match
data2[data2$ScientificName == "Ceratium gibberum v sinistrum","ScientificName"] <- "Tripos gibberus"
# Glenodinium rotundum                                 no match
data2[data2$ScientificName == "Glenodinium rotundum","ScientificName"] <- "Oblea rotunda"
# Histioneis paulseni                                  no match
data2[data2$ScientificName == "Histioneis paulseni","ScientificName"] <- "Histioneis paulsenii"
# Peridinium nipponicum                                no match
data2[data2$ScientificName == "Peridinium nipponicum","ScientificName"] <- "Protoperidinium ovum"
# Peridinium minimus                                   no match
data2[data2$ScientificName == "Peridinium minimus","ScientificName"] <- "Parvodinium umbonatum"
# Cystodinium unicorne                                 no match
data2[data2$ScientificName == "Cystodinium unicorne","ScientificName"] <- "Cystodinium unicorne" # FRESHWATER
# Ceratium pulchellum semipulchellum                   no match
data2[data2$ScientificName == "Ceratium pulchellum semipulchellum","ScientificName"] <- "Tripos pulchellus"
# Ceratium furca v bergii                              no match
data2[data2$ScientificName == "Ceratium furca v bergii","ScientificName"] <- "Tripos furca"
# Pyrophacus horologicum v steinii                     no match
data2[data2$ScientificName == "Pyrophacus horologicum v steinii","ScientificName"] <- "Pyrophacus horologium"
# Ceratium candelabrum commune                         no match
data2[data2$ScientificName == "Ceratium candelabrum commune","ScientificName"] <- "Tripos candelabrum"
# Ceratium candelabrum curvatum                        no match
data2[data2$ScientificName == "Ceratium candelabrum curvatum","ScientificName"] <- "Tripos candelabrum"
# Gymnodinium varians                                  no match
data2[data2$ScientificName == "Gymnodinium varians","ScientificName"] <- "Gymnodinium varians" # FRESHWATER
# Gymnodinium paradoxum                                no match
data2[data2$ScientificName == "Gymnodinium paradoxum","ScientificName"] <- "Gymnodinium paradoxum" # FRESHWATER
# Ceratium kofoidi                                     no match
data2[data2$ScientificName == "Ceratium kofoidi","ScientificName"] <- "Tripos kofoidii"
# Noctulica & other dinoflagellata                     no match
data2[data2$ScientificName == "Noctulica & other dinoflagellata","ScientificName"] <- "Dinoflagellata"
# Ornithocercus gladiolus                              no match
data2[data2$ScientificName == "Ornithocercus gladiolus","ScientificName"] <- "Oxytoxum gladiolus" # assumed that O. gladiolus wrongly attributed to Ornithocercus genus
# Ornithocercus gracile                                no match
data2[data2$ScientificName == "Ornithocercus gracile","ScientificName"] <- "Oxytoxum gracile" # Assumed because of species name 
# Dinophysis jolliffei                                 no match
data2[data2$ScientificName == "Dinophysis jolliffei","ScientificName"] <- "Spiraulax jolliffei" # Assumed because of species name 
# Ornithocercus longiceps                              no match
data2[data2$ScientificName == "Ornithocercus longiceps","ScientificName"] <- "Oxytoxum longiceps" # Assumed because of species name 
# Ornithocercus laticeps                               no match
data2[data2$ScientificName == "Ornithocercus laticeps","ScientificName"] <- "Oxytoxum laticeps" # Assumed because of species name 
# Gonyaulax gravidus                                   no match
data2[data2$ScientificName == "Gonyaulax gravidus","ScientificName"] <- "Tripos gravidus" # Assumed because of species name
# Protoperidinium belgicum                             no match
data2[data2$ScientificName == "Protoperidinium belgicum","ScientificName"] <- "Peridinium belgicum" # Uncertain: https://www.marinespecies.org/aphia.php?p=taxdetails&id=630730
# Prorocentrum pelagica                                no match
data2[data2$ScientificName == "Prorocentrum pelagica","ScientificName"] <- "Prorocentrum micans" # According to algaebase (https://www.algaebase.org/search/species/detail/?species_id=55124)
# Gymnodinium nudum                                    no match
data2[data2$ScientificName == "Gymnodinium nudum","ScientificName"] <- "Protoperidinium nudum" # Assumed because of species name
# Dinophysis quadratus                                 no match
data2[data2$ScientificName == "Dinophysis quadratus","ScientificName"] <- "Ornithocercus quadratus" # Assumed because Dinophysis really resembles Ornithocercus
# Glenodinium globulus                                 no match
data2[data2$ScientificName == "Glenodinium globulus","ScientificName"] <- "Protoperidinium globulus" # Assumed because of species name
# Podolampas globulus                                  no match
data2[data2$ScientificName == "Podolampas globulus","ScientificName"] <- "Protoperidinium globulus" # Assumed because of species name
# Podolampas grande                                    no match
data2[data2$ScientificName == "Podolampas grande","ScientificName"] <- "Protoperidinium grande"  # Assumed because of species name
# Podolampas grani                                     no match
data2[data2$ScientificName == "Podolampas grani","ScientificName"] <- "Protoperidinium granii"  # Assumed because of species name
# Cladopyxis lieuhater                                 no match
data2[data2$ScientificName == "Cladopyxis lieuhater","ScientificName"] <- "Cladopyxis lieuhater"    ### NO MATCH FOUND ANYWHERE !
# Gyrodinium primus                                    no match
data2[data2$ScientificName == "Gyrodinium primus","ScientificName"] <- "Gyrodinium primus"  ### NO MATCH FOUND ANYWHERE !
# Heterodinium auspole                                 no match
data2[data2$ScientificName == "Heterodinium auspole","ScientificName"] <- "Heterodinium auspole"    ### NO MATCH FOUND ANYWHERE !
# Protoceratium angulatum                              no match
data2[data2$ScientificName == "Protoceratium angulatum","ScientificName"] <- "Protoceratium angulatum" ### NO MATCH FOUND ANYWHERE !
# Gymnodinium robusta                                  no match
data2[data2$ScientificName == "Gymnodinium robusta","ScientificName"] <- "Gymnodinium robusta"  ### NO MATCH FOUND ANYWHERE !
# Amphisolenia turbo                                   no match
data2[data2$ScientificName == "Amphisolenia turbo","ScientificName"] <- "Amphisolenia turbo"    ### NO MATCH FOUND ANYWHERE !
# Ceratocorys magnifica                                no match
data2[data2$ScientificName == "Ceratocorys magnifica","ScientificName"] <- "Ceratocorys magnifica"  ### NO MATCH FOUND ANYWHERE !
# Ceratocorys complanata                               no match
data2[data2$ScientificName == "Ceratocorys complanata","ScientificName"] <- "Ceratocorys complanata" ### NO MATCH FOUND ANYWHERE !
# Gymnaster striatum                                   no match
data2[data2$ScientificName == "Gymnaster striatum","ScientificName"] <- "Gymnaster striatum"    ### NO MATCH FOUND ANYWHERE !

### 18/11/2021: Make a list of those species with no match whatsoever (useful for later)
no.matchies <- c("Cladopyxis lieuhater","Gyrodinium primus","Heterodinium auspole","Protoceratium angulatum",
                "Gymnodinium robusta","Amphisolenia turbo","Ceratocorys magnifica","Ceratocorys complanata","Gymnaster striatum")

# Check again:
#unique(data2$ScientificName)

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
dim(ddf) # 49'691, same as before.
ddf[1000:1020,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
unique(ddf[is.na(ddf$Species),'ScientificName']) # unique(ddf[,"ScientificName"])
unique(ddf$WoRMS_ID)  

unique(ddf[ddf$WoRMS_ID == "To_add_at_the_end",'ScientificName']) # Why? 
# "Ceratium inflatum" --> Tripos inflatus
# "Tripos mollis" --> Uncertain 
ddf[ddf$ScientificName == "Tripos mollis",'WoRMS_ID'] <- "Uncertain"
# "Ceratium inflexum" --> Tripos inflexus
# "Tripos seta" --> Uncertain
ddf[ddf$ScientificName == "Tripos seta",'WoRMS_ID'] <- "Uncertain"
# "Peridinium belgicum" --> Uncertain
ddf[ddf$ScientificName == "Peridinium belgicum",'WoRMS_ID'] <- "Uncertain"
# "Tripos tenuis" --> Uncertain
ddf[ddf$ScientificName == "Tripos tenuis",'WoRMS_ID'] <- "Uncertain"
# "Prorocentrum antarcticum" --> Uncertain
ddf[ddf$ScientificName == "Prorocentrum antarcticum",'WoRMS_ID'] <- "Uncertain"

unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS",'ScientificName']) # The 9 no.matchies, good.

### Check lifeforms
unique(ddf$LifeForm) # Ok, easy
ddf$LifeForm <- trimws(ddf$LifeForm, which = c("both") )

# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[35000:35010,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
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

save(ddf, file = "COPEPOD-NOAA_Dinoflagellates_2070000_reformatted+WoRMScheck_18_11_21.Rdata")
write.table(ddf, file = "COPEPOD-NOAA_Dinoflagellates_2070000_reformatted+WoRMScheck_18_11_21.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
