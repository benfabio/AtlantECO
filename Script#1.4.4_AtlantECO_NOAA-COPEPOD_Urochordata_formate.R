
##### ATLANTECO SCRIPT 1.4.4 ----------------------------------------------------------------------------------------------------------------------------
##### 12/11/2021: R Script to examine and format of COPEPOD-NOAA Urochordata+Cnidaria+Ctenophora dataset and reformat it following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Read the .csv file that you had to manually prepare on excel because of the copy-pasting issues
# - Examine strcuture, clean some values
# - Reformat to ATLANTECO WP2 template
# - Check and correct taxonomy with WoRMS

# Check: https://www.st.nmfs.noaa.gov/copepod/documentation/short-format_description.html 
# For format description 

### Latest update: 15/11/2021

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
data <- read.csv("COPEPOD_Urochordata_4350000_12_11_21.csv", sep = ";", dec = ",", h = T)
dim(data) # 57'958 records
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

# Same with TIMEloc and TIMEgmt
summary(data[,c("TIMEloc","TIMEgmt")])
unique(data$TIMEgmt)[order(unique(data$TIMEgmt))]

# Check other columns one by one
summary(factor(data$Tow_type)) # Good, spell those out. I don't what 'B' stands for though
unique(data$Tow_type)
data[data$Tow_type == "H" & !is.na(data$Tow_type),"Tow_type"] <- "horizontal"
data[data$Tow_type == "V"& !is.na(data$Tow_type),"Tow_type"] <- "vertical"
data[data$Tow_type == "O"& !is.na(data$Tow_type),"Tow_type"] <- "oblique"
data[data$Tow_type == "S"& !is.na(data$Tow_type),"Tow_type"] <- "surface"

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
data$Units <- trimws(data$Units, which = c("both") )
data$OrigUnits <- trimws(data$OrigUnits, which = c("both") )
unique(data$OrigUnits)

# Check strained water volume
unique(data$Water_strained) # Need to trimws this
data$Water_strained <- trimws(data$Water_strained, which = c("both") )
unique(data$Water_strained)
# Good, leave like this for now...

# Now, examine OrigScientificName
unique(data$OrigScientificNameMod)
unique(data$OrigScientificName)
unique(data$Modifiers) # several ancillary info in there
# Are all Modifiers in OrigScientifcNameWithMods already?
data[1000:1010,c("OrigScientificNameMod","Modifiers")]
# Looks like it, use ScientificName as OrigScientificName then. 


### Looks alright...reformat to AtlantECO WP2 template
colnames(data)
unique(data$Tow_type)
# SamplingProtocol should be a combination of: gear+mesh+tow type. MESH and Tow_type are already in the right format, but gear codes...
# Use 'gears' to inform actual gear names instead of code
unique(data$Gear_code)  # Make sure no -99 is inside 

data[data$Gear_code == -99 & !is.na(data$Gear_code),"Gear_code"] <- NA

data$gear <- NA
# code <- "1003" # For testing

for(code in unique(na.omit(data$Gear_code)) ) {
    
        message(paste(code,"  =  ",gears[gears$Gear.Code == code,"Gear.Description"], sep = ""))
        g <- unique(gears[gears$Gear.Code == code,"Gear.Description"])
        # Provide to data$gear
        data[data$Gear_code == code & !is.na(data$Gear_code),"gear"] <- g
        
} # eo for loop 
# Check:
unique(data$gear)
# unique(paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = ""))
# summary(factor(paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = "")))
# unique(paste("Tow type= ",data$Tow_type,"; with: ",data$gear,"; mesh= ",data$Mesh, sep = ""))

data2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@env.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = data$RECORD.ID, obisID = "Not_applicable", DatasetKey = "4350000",
                decimalLatitude = data$decimalLatitude, decimalLongitude = data$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = data$Date,
                eventDateInterval = data$TIMEgmt, eventDateIntervalUnit = "GMT time of sampling (in decimal hours)",
                Year = data$Year, Month = data$Month, Day = data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = data$MinDepth, MaxDepth = data$MaxDepth, ParentEventID = paste("Cruise code="," ",data$CRUISE, sep = ""),
                EventID = NA, InstitutionCode = paste("Institute code="," ",data$INST, sep = ""),
                SourceArchive = "https://www.st.nmfs.noaa.gov/copepod/atlas/html/taxatlas_4350000.html",
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
dim(data2) # 57'958  70
str(data2)
head(data2)

unique(data2$basisOfRecord)
unique(data2$SamplingProtocol)

# Check scientific names used
unique(data2$ScientificName); str(data2$ScientificName)

### Now, give worms::wormsbynames a first try and manually correct the labels that won't fit
require("worms") 
keys.worms <- wormsbynames( unique(data2$ScientificName) )
# Fritillaridae                                        no match
# Pyrosoma parts/fragments                             no match
# Oikopleura sp.                                       no match
# Doliolum sp.                                         no match
# Oikopleura labradoriensis                            no match
# Oikopleura vanhoeffeni                               no match
# Appendicularia sp.                                   no match
# Fritillaria sp.                                      no match
# Salps & doliolids                                    no match
# Pyrosomida                                           no match
# Oikopleura parva                                     no match
# Protochordate                                        no match
# Appendicularia spp.                                  no match
# Oikopleura spp.                                      no match
# Fritillaria spp.                                     no match
# Salpa spp.                                           no match
# Doliolum spp.                                        no match
# Oikopleura dioica                                    no match
# Oikopleura longicauda                                no match
# Pyrosoma spp.                                        no match
# Doliolides spp.                                      no match
# Ihlea spp.                                           no match
# Salpa sp.                                            no match
# Tunicata miscellaneous                               no match
# Tunicata other/unidentified                          no match
# Pyrosomidae (syn)                                    no match
# Thaliacea cyclomyaria                                no match
# Thaliacea hemimyaria                                 no match

# To correct manually
data2[data2$ScientificName == "Fritillaridae","ScientificName"] <- "Fritillariidae"
data2[data2$ScientificName == "Pyrosoma parts/fragments","ScientificName"] <- "Pyrosoma"
data2[data2$ScientificName == "Oikopleura labradoriensis","ScientificName"] <- "Oikopleura (Vexillaria) labradoriensis"
data2[data2$ScientificName == "Oikopleura vanhoeffeni","ScientificName"] <- "Oikopleura (Vexillaria) vanhoeffeni"
data2[data2$ScientificName == "Salps & doliolids","ScientificName"] <- "Thaliacea"
data2[data2$ScientificName == "Pyrosomida","ScientificName"] <- "Pyrosomatida"
data2[data2$ScientificName == "Oikopleura parva","ScientificName"] <- "Oikopleura (Vexillaria) parva"
data2[data2$ScientificName == "Protochordate","ScientificName"] <- "Protochordata"
data2[data2$ScientificName == "Oikopleura dioica","ScientificName"] <- "Oikopleura (Vexillaria) dioica"
data2[data2$ScientificName == "Oikopleura longicauda","ScientificName"] <- "Oikopleura (Coecaria) longicauda"
data2[data2$ScientificName == "Tunicata miscellaneous","ScientificName"] <- "Tunicata"
data2[data2$ScientificName == "Tunicata other/unidentified","ScientificName"] <- "Tunicata"
data2[data2$ScientificName == "Pyrosomidae (syn)","ScientificName"] <- "Pyrosomatidae"
data2[data2$ScientificName == "Thaliacea cyclomyaria","ScientificName"] <- "Doliolida"
data2[data2$ScientificName == "Thaliacea hemimyaria","ScientificName"] <- "Salpida"
data2$ScientificName <- str_replace_all(data2$ScientificName, " spp.", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " sp.", "")
data2[data2$ScientificName == "Doliolides","ScientificName"] <- "Dolioloides"

# Check again:
unique(data2$ScientificName)

# Do it again now
keys.worms <- wormsbynames( unique(data2$ScientificName) )
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
dim(ddf) # 57'958, same as before.
ddf[1:50,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
unique(ddf[is.na(ddf$Species),'ScientificName'])
unique(ddf$WoRMS_ID)  # "Protochordates" is causing the missing value
unique(ddf$WoRMS_status) # good

### Check lifeforms
unique(ddf$LifeForm) # Ok, easy
ddf$LifeForm <- trimws(ddf$LifeForm, which = c("both") )

# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[57000:57010,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
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

save(ddf, file = "COPEPOD-NOAA_Urochordata_4350000_reformatted+WoRMScheck_12_11_21.Rdata")
write.table(ddf, file = "COPEPOD-NOAA_Urochordata_4350000_reformatted+WoRMScheck_12_11_21.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------

### 12/11/2021: Cnidaria 
data <- read.csv("COPEPOD_Cnidarian_4030000_11_11_21.csv", sep = ";", dec = ",", h = T)
dim(data) # 54'594 records
str(data)
which(rowSums(is.na(data)) == ncol(data)) # should return 'integer(0)'

colnames(data)

### First, check spatial coordinates and dates, add a date column
summary(data[,c("Year","Month","Day","TIMEgmt","TIMEloc","decimalLatitude","decimalLongitude","MinDepth","MaxDepth")])

# Add date
data$Date <- ymd(paste(data$Year,data$Month,data$Day, sep = "-"))
str(data$Date)
summary(data$Date) # Nice

# Same with TIMEloc and TIMEgmt
summary(data[,c("TIMEloc","TIMEgmt")])

# Check other columns one by one
summary(factor(data$Tow_type)) # Good, spell those out. I don't what 'B' stands for though
unique(data$Tow_type)
data[data$Tow_type == "H" & !is.na(data$Tow_type),"Tow_type"] <- "horizontal"
data[data$Tow_type == "V"& !is.na(data$Tow_type),"Tow_type"] <- "vertical"
data[data$Tow_type == "O"& !is.na(data$Tow_type),"Tow_type"] <- "oblique"
data[data$Tow_type == "S"& !is.na(data$Tow_type),"Tow_type"] <- "surface"

### Check #/m3
summary(data$ValPerVolume)

unique(data$Units); unique(data$OrigUnits)
data$Units <- trimws(data$Units, which = c("both") )
data$OrigUnits <- trimws(data$OrigUnits, which = c("both") )

# Check strained water volume
unique(data$Water_strained) # Need to trimws this
data$Water_strained <- trimws(data$Water_strained, which = c("both") )
# Good, leave like this for now...

# Now, examine OrigScientificName
unique(data$OrigScientificNameMod)
unique(data$OrigScientificName)
unique(data$Modifiers) # several ancillary info in there
# Are all Modifiers in OrigScientifcNameWithMods already?
data[1000:1010,c("OrigScientificNameMod","Modifiers")]
# Looks like it, use ScientificName as OrigScientificName then. 

### Looks alright...reformat to AtlantECO WP2 template
colnames(data)
unique(data$Tow_type)
# SamplingProtocol should be a combination of: gear+mesh+tow type. MESH and Tow_type are already in the right format, but gear codes...
# Use 'gears' to inform actual gear names instead of code
unique(data$Gear_code)  # Make sure no -99 is inside 

if( -99 %in% unique(data$Gear_code) ) {
    data[data$Gear_code == -99 & !is.na(data$Gear_code),"Gear_code"] <- NA
} # eo if loop

data$gear <- NA

for(code in unique(na.omit(data$Gear_code)) ) {
    
        message(paste(code,"  =  ",gears[gears$Gear.Code == code,"Gear.Description"], sep = ""))
        g <- unique(gears[gears$Gear.Code == code,"Gear.Description"])
        # Provide to data$gear
        data[data$Gear_code == code & !is.na(data$Gear_code),"gear"] <- g
        
} # eo for loop 
# Check:
unique(data$gear)
# unique(paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = ""))
# summary(factor(paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = "")))
# unique(paste("Tow type= ",data$Tow_type,"; with: ",data$gear,"; mesh= ",data$Mesh, sep = ""))

data2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@env.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = data$RECORD.ID, obisID = "Not_applicable", DatasetKey = "4030000",
                decimalLatitude = data$decimalLatitude, decimalLongitude = data$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = data$Date,
                eventDateInterval = data$TIMEgmt, eventDateIntervalUnit = "GMT time of sampling (in decimal hours)",
                Year = data$Year, Month = data$Month, Day = data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = data$MinDepth, MaxDepth = data$MaxDepth, ParentEventID = paste("Cruise code="," ",data$CRUISE, sep = ""),
                EventID = NA, InstitutionCode = paste("Institute code="," ",data$INST, sep = ""),
                SourceArchive = "https://www.st.nmfs.noaa.gov/copepod/atlas/html/taxatlas_4030000.html",
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
dim(data2) # 54'594  70
str(data2)
head(data2)

unique(data2$basisOfRecord)
unique(data2$SamplingProtocol)

# Check scientific names used
unique(data2$ScientificName); str(data2$ScientificName)
data2$ScientificName <- str_replace_all(data2$ScientificName, " spp.", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " sp.", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " (syn)", "")

### Now, give worms::wormsbynames a first try and manually correct the labels that won't fit
require("worms") 
keys.worms <- wormsbynames( unique(data2$ScientificName) )
# Jellyfish not classified                             no match
# Jellyfish                                            no match
# Cnidaria unidentified                                no match
# Jellyfishes (except solmundella bitentaculata)       no match
# Hydromedusae                                         no match
# Jellyfish other                                      no match
# Hydromedusae other                                   no match
# Siphonophorae parts/fragments                        no match
# Hydroidea                                            no match
# Eudoxoidesralis                                      no match
# Muggiaea kochi                                       no match
# Siphonophora (syn)                                   no match
# Siphonophorae other                                  no match

# To correct manually
data2[data2$ScientificName == "Jellyfish not classified","ScientificName"] <- "Jellyfish"
data2[data2$ScientificName == "Cnidaria unidentified","ScientificName"] <- "Cnidaria"
data2[data2$ScientificName == "Jellyfish other","ScientificName"] <- "Jellyfish"
data2[data2$ScientificName == "Hydromedusae other","ScientificName"] <- "Hydromedusae"
data2[data2$ScientificName == "Siphonophorae parts/fragments","ScientificName"] <- "Siphonophorae"
data2[data2$ScientificName == "Hydroidea","ScientificName"] <- "Hydroidolina"
data2[data2$ScientificName == "Eudoxoidesralis","ScientificName"] <- "Eudoxoides spiralis"
data2[data2$ScientificName == "Muggiaea kochi","ScientificName"] <- "Muggiaea kochii"
data2[data2$ScientificName == "Siphonophora (syn)","ScientificName"] <- "Siphonophorae"
data2[data2$ScientificName == "Siphonophorae other","ScientificName"] <- "Siphonophorae"

# Check again:
unique(data2$ScientificName)

# Do it again now
keys.worms <- wormsbynames( unique(data2$ScientificName) )
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
rm(res) ; gc()
dim(ddf) # 54'594, same as before.
ddf[1:50,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
unique(ddf[is.na(ddf$Species),'ScientificName'])
unique(ddf$WoRMS_ID)  # "Protochordates" is causing the missing value
unique(ddf$WoRMS_status) # good

### Check lifeforms
unique(ddf$LifeForm) # Ok, easy
ddf$LifeForm <- trimws(ddf$LifeForm, which = c("both") )
# copepodite?
unique(ddf[ddf$LifeForm == "copepodite (unspecified)","OrigScientificName"]) # Metridiidae, a copepod

# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[1100:1110,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
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

save(ddf, file = "COPEPOD-NOAA_Cnidarian_4030000_reformatted+WoRMScheck_12_11_21.Rdata")
write.table(ddf, file = "COPEPOD-NOAA_Cnidarian_4030000_reformatted+WoRMScheck_12_11_21.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------

### 15/11/21: Ctenophora now
data <- read.csv("COPEPOD_Ctenophora_4040000_11_11_21.csv", sep = ";", dec = ",", h = T)
dim(data) # 8949 records
str(data)
which(rowSums(is.na(data)) == ncol(data)) # should return 'integer(0)'

colnames(data)

### First, check spatial coordinates and dates, add a date column
summary(data[,c("Year","Month","Day","TIMEgmt","TIMEloc","decimalLatitude","decimalLongitude","MinDepth","MaxDepth")])

# Add date
data$Date <- ymd(paste(data$Year,data$Month,data$Day, sep = "-"))
str(data$Date)
summary(data$Date) # Nice

# Same with TIMEloc and TIMEgmt
summary(data[,c("TIMEloc","TIMEgmt")])

# Check other columns one by one
summary(factor(data$Tow_type)) # Good, spell those out. I don't what 'B' stands for though
unique(data$Tow_type)
data[data$Tow_type == "H" & !is.na(data$Tow_type),"Tow_type"] <- "horizontal"
data[data$Tow_type == "V"& !is.na(data$Tow_type),"Tow_type"] <- "vertical"
data[data$Tow_type == "O"& !is.na(data$Tow_type),"Tow_type"] <- "oblique"
data[data$Tow_type == "S"& !is.na(data$Tow_type),"Tow_type"] <- "surface"

### Check #/m3
summary(data$ValPerVolume)

unique(data$Units); unique(data$OrigUnits)
data$Units <- trimws(data$Units, which = c("both") )
data$OrigUnits <- trimws(data$OrigUnits, which = c("both") )

# Check strained water volume
unique(data$Water_strained) # Need to trimws this
data$Water_strained <- trimws(data$Water_strained, which = c("both") )
# Good, leave like this for now...

# Now, examine OrigScientificName
unique(data$OrigScientificNameMod)
unique(data$OrigScientificName)
unique(data$Modifiers) # several ancillary info in there
data[50:75,c("OrigScientificNameMod","Modifiers")]
# Looks like it, use ScientificName as OrigScientificName then. 

### Looks alright...reformat to AtlantECO WP2 template
colnames(data)
# SamplingProtocol should be a combination of: gear+mesh+tow type. MESH and Tow_type are already in the right format, but gear codes...
# Use 'gears' to inform actual gear names instead of code
unique(data$Gear_code)  # Make sure no -99 is inside 

if( -99 %in% unique(data$Gear_code) ) {
    data[data$Gear_code == -99 & !is.na(data$Gear_code),"Gear_code"] <- NA
} # eo if loop

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
                orig_occurrenceID = data$RECORD.ID, obisID = "Not_applicable", DatasetKey = "4040000",
                decimalLatitude = data$decimalLatitude, decimalLongitude = data$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = data$Date,
                eventDateInterval = data$TIMEgmt, eventDateIntervalUnit = "GMT time of sampling (in decimal hours)",
                Year = data$Year, Month = data$Month, Day = data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = data$MinDepth, MaxDepth = data$MaxDepth, ParentEventID = paste("Cruise code="," ",data$CRUISE, sep = ""),
                EventID = NA, InstitutionCode = paste("Institute code="," ",data$INST, sep = ""),
                SourceArchive = "https://www.st.nmfs.noaa.gov/copepod/atlas/html/taxatlas_4040000.html",
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
dim(data2) # 8'949 70
str(data2)
head(data2)

unique(data2$basisOfRecord)
unique(data2$SamplingProtocol)

# Check scientific names used
unique(data2$ScientificName); str(data2$ScientificName)
data2$ScientificName <- str_replace_all(data2$ScientificName, " spp.", "")
data2$ScientificName <- str_replace_all(data2$ScientificName, " sp.", "")

### Now, give worms::wormsbynames a first try and manually correct the labels that won't fit
require("worms") 
keys.worms <- wormsbynames( unique(data2$ScientificName) )
# To correct manually
data2[data2$ScientificName == "Ctenophora other","ScientificName"] <- "Ctenophora"
data2[data2$ScientificName == "Pleurobrachia & mertensia","ScientificName"] <- "Cydippida"
# Check again:
unique(data2$ScientificName)

# Do it again now
keys.worms <- wormsbynames( unique(data2$ScientificName) )
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
rm(res) ; gc()
dim(ddf) # 8949, same as before.
ddf[1:20,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
unique(ddf[is.na(ddf$Species),'ScientificName'])
unique(ddf$WoRMS_ID) 
unique(ddf$WoRMS_status) # good

### Check lifeforms
unique(ddf$LifeForm) # Ok, easy
ddf$LifeForm <- trimws(ddf$LifeForm, which = c("both") )

# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[75:100,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
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

save(ddf, file = "COPEPOD-NOAA_Ctenophora_4040000_reformatted+WoRMScheck_15_11_21.Rdata")
write.table(ddf, file = "COPEPOD-NOAA_Ctenophora_4040000_reformatted+WoRMScheck_15_11_21.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
