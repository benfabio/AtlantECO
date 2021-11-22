
##### ATLANTECO SCRIPT 1.4.1 ----------------------------------------------------------------------------------------------------------------------------
##### 18/10/2021: R Script to examine and format of COPEPOD-NOAA datasets and reformat them following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Read the .csv file that you had to manually prepare on excel (part1+part2) because of the copy-pasting
# - Examine strcuture, clean some values
# - Reformat to ATLANTECO WP2 template
# - Check and correct taxonomy with WoRMS

# Check: https://www.st.nmfs.noaa.gov/copepod/documentation/short-format_description.html 
# For format description 

### Latest update: 19/10/2021

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
data <- read.csv("COPEPOD-NOAA_Gastropoda_18_10_21.csv", sep = ";", dec = ",", h = T)
dim(data) # 47'792 records
str(data)
# which(rowSums(is.na(data)) == ncol(data)) # should return 'integer(0)'

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
summary(data[,c("YEAR","MONTH","DAY","TIMEgmt","TIMEloc","decimalLatitude","decimalLongitude","UPPER_Z","LOWER_Z")])
# Quick map
# world <- map_data("world")
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude, alpha = .5), data = data) + coord_quickmap() +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")

# Add date
data$Date <- ymd(paste(data$YEAR,data$MONTH,data$DAY, sep = "-"))
str(data$Date)
summary(data$Date) # Nice

# Check other columns one by one
summary(factor(data$Tow_type)) # Good, spell those out. I don't what 'B' stands for though
# unique(data$Tow_type)
data[data$Tow_type == "H" & !is.na(data$Tow_type),"Tow_type"] <- "horizontal"
data[data$Tow_type == "V"& !is.na(data$Tow_type),"Tow_type"] <- "vertical"
data[data$Tow_type == "O"& !is.na(data$Tow_type),"Tow_type"] <- "oblique"
data[data$Tow_type == "S"& !is.na(data$Tow_type),"Tow_type"] <- "surface"

# Gear codes
unique(data$GEAR) # https://www.st.nmfs.noaa.gov/copepod/codes/copecode-biohmeta.gear.html
# Use the COPEPOD Gear Code tabme to trace sampling protocol back
unique(data$MESH) # in micrometers
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
unique(data$ValuePerVolume)
#           NA 
data[data$ValuePerVolume == "          NA "& !is.na(data$ValuePerVolume),"ValuePerVolume"] <- NA
data[data$ValuePerVolume == "            NA" & !is.na(data$ValuePerVolume),"ValuePerVolume"] <- NA
# Convert to numeric with ',' as decimal separator
summary( as.numeric(sub(",", ".", data$ValuePerVolume, fixed = T)) )
data$ValuePerVolume <- as.numeric(sub(",", ".", data$ValuePerVolume, fixed = T))
# summary(data$ValuePerVolume)

# Check strained water volume
unique(data$Water_strained) # Need to trimws this
data$Water_strained <- trimws(data$Water_strained, which = c("both") )
# Convert NA to NA
data[data$Water_strained == "NA" & !is.na(data$Water_strained),"Water_strained"] <- NA
# Good, leave like this for now...

# Now, examine OrigScientificName
colnames(data)[36] <- "OrigScientifcNameWithMods"
unique(data$OrigScientifcNameWithMods)
unique(data$ScientificName)
unique(data$Modifiers)
# Are all Modifiers in OrigScientifcNameWithMods already?
data[45:90,c("OrigScientifcNameWithMods","Modifiers")]
# Looks like it, use ScientificName as OrigScientificName then. 

# Clean some ScientificName quickly: 
data[data$ScientificName == "Gastropoda unidentified","ScientificName"] <- "Gastropoda"
data[data$ScientificName == "Pteropods gymnosomes","ScientificName"] <- "Gymnosomata"
data[data$ScientificName == "Pteropods thecosomes","ScientificName"] <- "Thecosomata"
data[data$ScientificName == "Heteropoda & pteropoda","ScientificName"] <- "Heteropoda+Pteropoda"
# dim(data[data$ScientificName == "Heteropoda & pteropoda",]) # 1455...
data$ScientificName <- gsub(" spp.", replacement = "", data$ScientificName) # unique(gsub(" spp.", replacement = "", data$ScientificName))
data$ScientificName <- gsub(" sp.", replacement = "", data$ScientificName) # unique(gsub(" spp.", replacement = "", data$ScientificName))
data[data$ScientificName == "Heteropods --> heteropoda (syn) --> carinariidae","ScientificName"] <- "Carinariidae"
data[data$ScientificName == "Carinariidae other","ScientificName"] <- "Carinariidae"
data[data$ScientificName == "Heteropoda (syn)","ScientificName"] <- "Heteropoda"
data[data$ScientificName == "Thecosomata A or 3","ScientificName"] <- "Thecosomata"
data[data$ScientificName == "Thecosomata B or 4","ScientificName"] <- "Thecosomata"
data[data$ScientificName == "Thecosomata C or 5","ScientificName"] <- "Thecosomata"
unique(data$ScientificName)

### Looks alright...reformat to AtlantECO WP2 template
colnames(data)
unique(data$Tow_type)
# SamplingProtocol should be a combination of: gear+mesh+tow type. MESH and Tow_type are already in the right format, but gear codes...
# Use 'gears' to inform actual gear names instead of code
data$gear <- NA
# code <- "1003" # For testing
for(code in unique(na.omit(data$GEAR)) ) {
    
        message(paste(code,"  =  ",gears[gears$Gear.Code == code,"Gear.Description"], sep = ""))
        g <- unique(gears[gears$Gear.Code == code,"Gear.Description"])
        # Provide to data$gear
        data[data$GEAR == code & !is.na(data$GEAR),"gear"] <- g
        
} # eo for loop 
# Check:
unique(data$gear)
# unique(paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = ""))
# summary(factor(paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = "")))
# unique(paste("Tow type= ",data$Tow_type,"; with: ",data$gear,"; mesh= ",data$MESH, sep = ""))

data2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@env.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = data$recordID, obisID = "Not_applicable", DatasetKey = "4262500",
                decimalLatitude = data$decimalLatitude, decimalLongitude = data$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = data$Date,
                eventDateInterval = data$TIMEgmt, eventDateIntervalUnit = "GMT time of sampling (in decimal hours)",
                Year = data$YEAR, Month = data$MONTH, Day = data$DAY,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = data$UPPER_Z, MaxDepth = data$LOWER_Z, ParentEventID = paste("Cruise code="," ",data$SHP.CRUISE, sep = ""),
                EventID = NA, InstitutionCode = paste("Institute code="," ",data$INSTITUTE, sep = ""),
                SourceArchive = "https://www.st.nmfs.noaa.gov/copepod/atlas/html/taxatlas_4262500.html",
                OrigCollectionCode = NA, OrigCollectionID = data$DatasetID,
                BiblioCitation = "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.",
                CitationDOI = NA, DateDataAccess = "15-10-2021",
                OrigScientificName = data$OrigScientifcNameWithMods, ScientificName = data$ScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "To_add_at_the_end", 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = data$Modifiers, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = data$ValuePerVolume,
                MeasurementUnit = "#/m3", MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = paste("Tow type= ",data$Tow_type,"; with: ",data$gear, sep = ""),
                SamplingProtocol = paste("Tow type= ",data$Tow_type,"; with: ",data$gear,"; mesh= ",data$MESH, sep = ""),
                SampleAmount = data$Water_strained,
                SampleAmountUnit = "m3", SampleEffort = NA,
                DeterminedBy = NA,
                DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
dim(data2) # 47'792     70
str(data2)
head(data2)

# Check scientific names used
unique(data2$ScientificName); str(data2$ScientificName)

### Now, give worms::wormsbynames a first try and manually correct the labels that won't fit
require("worms") 
keys.worms <- wormsbynames( unique(data2$ScientificName) )
# Charcotia bifrons ?? Charcotia is an amphipod genus, but Charcotia bifrons (Castracane) M.Peragallo 1921 is a Diatom! (https://www.algaebase.org/search/species/detail/?tc=accept&species_id=160781)
data2[data2$ScientificName == "Charcotia bifrons",] # To be removed!

data3 <- data2[!(data2$ScientificName == "Charcotia bifrons"),]
dim(data2); dim(data3)
# keys.worms <- wormsbynames( unique(data3$ScientificName) )

### !!! https://www.marinespecies.org/aphia.php?p=taxdetails&id=13703 !!!
# Thecosomata is officially an abandoned concept, including Euthecosomata and Pseudothecosomata)
# Replace by 'Pteropoda'
data3[data3$ScientificName == "Thecosomata","ScientificName"] <- "Pteropoda"

# OK, ready for WoRMS script 
keys.worms <- wormsbynames( unique(data3$ScientificName) )
keys.worms$ScientificName <- unique(data3$ScientificName)

# Add WoRMS_status field
data3 <-  add_column(data3, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(data3)

# For testing the functions below:
s <- unique(data3$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(data3$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- data3[data3$ScientificName == s,]
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
dim(ddf) # 47727, same as before.
ddf[1:50,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
head(ddf[is.na(ddf$Species),])
unique(ddf$WoRMS_ID) 
# What do the 'To_add_at_the_end' correspond to?
unique(ddf[ddf$WoRMS_ID == "To_add_at_the_end","ScientificName"])
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS","ScientificName"])
# Makes sense
unique(ddf$WoRMS_status) # good

### Check lifeforms
unique(ddf$LifeForm)
# Clean it by removing those brackets etc.
# gsub("-[ ]-", replacement = NA, unique(ddf$LifeForm), fixed = T)
# gsub("-[]-", replacement = NA, unique(ddf$LifeForm), fixed = T)
ddf$LifeForm <- gsub("-[ ]-", replacement = NA, ddf$LifeForm, fixed = T)
ddf$LifeForm <- gsub("-[]-", replacement = NA, ddf$LifeForm, fixed = T)
unique(ddf$LifeForm)
ddf[ddf$LifeForm == "-[ larva ]-" & !is.na(ddf$LifeForm),"LifeForm"] <- "larva"
ddf[ddf$LifeForm == "-[ eggs ]-" & !is.na(ddf$LifeForm),"LifeForm"] <- "eggs"
ddf[ddf$LifeForm == "-[ veliger ]-" & !is.na(ddf$LifeForm),"LifeForm"] <- "Veliger larva"
ddf[ddf$LifeForm == "-[ nauplii ]-" & !is.na(ddf$LifeForm),"LifeForm"] <- "Nauplius larva"
ddf[ddf$LifeForm == "-[ c1-3 ]-" & !is.na(ddf$LifeForm),"LifeForm"] <- "CI-III stage"

# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[47707:47727,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[35020:35080,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
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

### And for only those calcifying pteropod species
unique(d.effort$Order)
dim(na.omit(d.effort[d.effort$Order == "Pteropoda",c("Month","Year")])) # 25'308 obs
summary(na.omit(d.effort[d.effort$Order == "Pteropoda",c("Month","Year")]))

spatial.effort <- na.omit(data.frame(d.effort[d.effort$Order == "Pteropoda",] %>% 
                    group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n())) )
# 579 grid cells

ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = spatial.effort) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "COPEPOD-NOAA_Gastropoda_4262500_reformatted+WoRMScheck_19_10_21.Rdata")
write.table(ddf, file = "COPEPOD-NOAA_Gastropoda_4262500_reformatted+WoRMScheck_19_10_21.txt", sep = "\t")



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
