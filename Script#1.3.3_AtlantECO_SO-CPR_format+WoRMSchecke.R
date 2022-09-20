
##### ATLANTECO SCRIPT 1.3.3 ----------------------------------------------------------------------------------------------------------------------------
##### 15/10/2021: R Script to reformat the Southern Ocean (SO) CPR data shared by John Kitchnere (AAD) © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### All data and material are available on: https://data.aad.gov.au/aadc/cpr/index.cfm 

### Aims to:
# - Read the file
# - Reformat to AtlantECO WP2 standards. Use Richardson et al. (2006) - https://doi.org/10.1016/j.pocean.2005.09.011 and Pinkerton et al. (2020) - https://doi.org/10.1016/j.dsr.2020.103303 
# - Check and correct the taxonomic classification based on WoRMS

### Latest update: 17/10/2021

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("readxl")
library("lubridate")

world <- map_data("world")  # for maps

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") # working dir
dir()

### ------------------------------------------------------------------------------------------------------------------------------------------------------

### 1°) Zooplankton (starting with zooplankto instead of phytoplankton for Nielja)

# What does 'cpr_xform.csv' correspond to?
#xform <- read.csv("cpr_xform.csv", h = T, sep = ",", dec = ".")
#dim(xform) # 51'466 ; 292 taxa
#colnames(xform)
#str(xform)
#head(xform)
#summary(xform)
### This is the data, in already pretty good shape

# Read the other data table 'AADC-00099_28June2021.csv'
cpr <- read.csv("AADC-00099_28June2021.csv", h = T, sep = ",", dec = ".") 
dim(cpr) # 51'717 observations of 292 taxa. SO more data here apparently. 
#colnames(cpr)
#str(cpr[,1:15])
#summary(cpr)
# This is the same but with more rows --> keep this one
#rm(xform) ; gc()

# Values in the columns represent counts --> need to convert to #/m3 after the melt()

summary(cpr)

# Month is not numeric?
#unique(cpr$Month) # char..
# Convert to numerics
cpr$months <- NA
cpr[cpr$Month == "January","months"] <- 01
cpr[cpr$Month == "March","months"] <- 03
cpr[cpr$Month == "February","months"] <- 02
cpr[cpr$Month == "August","months"] <- 08
cpr[cpr$Month == "April","months"] <- 04
cpr[cpr$Month == "September","months"] <- 11
cpr[cpr$Month == "October","months"] <- 10
cpr[cpr$Month == "November","months"] <- 11
cpr[cpr$Month == "December","months"] <- 12
cpr[cpr$Month == "May","months"] <- 05
cpr[cpr$Month == "July","months"] <- 07

# Lacking a 'Day' column...extract from 'Date' using lubridate:day
# unique(cpr$Date)
# unique(lubridate::day(x = ymd(cpr$Date)))
# Try to convert SampleDateUTC to vector of class vector
# str(cpr$Date) ; unique(cpr$Date)
# str(as.Date(cpr$Date, "%d-%b-%Y")) ; unique(as.Date(cpr$Date, format = "%d-%b-%Y")) 
# Sometimes retuns NA, why? 
#dates <- data.frame(v1 = cpr$Date, v2 = as.Date(cpr$Date, format = "%d-%h-%Y"))
#summary(dates) ; str(dates)
#dates[is.na(dates$v2),][3050:3070,]
#cpr$Date <- as.Date(cpr$Date, "%d-%b-%Y")
### For some unknown reason, the conversion to a 'Date' vectopr does not work. leave it as charecter for now
# Do it the hardcore way: strsplit...cpr$Date
# head( do.call(rbind, strsplit(x = as.character(unique(cpr$Date)), split = "-")) )
# do.call(rbind, strsplit(x = as.character(unique(cpr$Date)), split = "-"))[1:33,]
# summary(as.numeric(do.call(rbind, strsplit(x = as.character(unique(cpr$Date)), split = "-"))[,1]))

cpr$Day <- as.numeric(do.call(rbind, strsplit(x = as.character(cpr$Date), split = "-"))[,1])
# summary(cpr$Day) # Worked. Now, re-create a date vector from Day-months and Year
cpr$Datev2 <- ymd(paste(cpr$Year,cpr$months,cpr$Day, sep = "-"))
# head(cpr$Datev2) ; str(cpr$Datev2) ; unique(cpr$Datev2)
# Nice

### Before melting, converting to #/m3 and reformatting to AtlantECO data, modify the taxa colnames to avoid the extra dots etc.
cpr2 <- cpr
# colnames(cpr2)[c(12:305)]
# For those, replace: 
# '.indet' by ''
# '..' by ' '
# '.' by ' '
# Will generate empty blanks at the end of the char vector so : https://stat.ethz.ch/R-manual/R-devel/library/base/html/trimws.html 
colnames(cpr2)[c(12:305)] <- gsub(".indet", "", colnames(cpr2)[c(12:305)], fixed = T)
colnames(cpr2)[c(12:305)] <- gsub("..", " ", colnames(cpr2)[c(12:305)], fixed = T)
colnames(cpr2)[c(12:305)] <- gsub(".", " ", colnames(cpr2)[c(12:305)], fixed = T)
colnames(cpr2)[c(12:305)] <- trimws(colnames(cpr2)[c(12:305)], which = c("right") )
colnames(cpr2)[c(12:305)]


### Melt
colnames(cpr2)
m.zoo <- melt(cpr2[,c(1:3,6,8:313)], id.vars = c("Tow_Number","Ship_Code","Time","Datev2","Year","months","Day",
        "Latitude","Longitude","Segment_No.","Segment_Length","Phytoplankton_Colour_Index","Fluorescence","Salinity","Water_Temperature","Photosynthetically_Active_Radiation"))
head(m.zoo) ; dim(m.zoo) # 15'204'798 observations now
#summary(m.zoo$value)
#unique(m.zoo$variable)
colnames(m.zoo)[c(17,18)] <- c("OrigScientificName","Count")

### Convert those counts to abundances! 
### Strategy: A 5 nautical segment is equivalent to 1.49 cubic metres
unique(m.zoo$Segment_Length)
### If 5 = 1.49 m^3, then use ((m.zoo$Segment_Length)*1.49)/5 to derive volume of seawater filtered

m.zoo$VolFiltered <- ((m.zoo$Segment_Length)*1.49)/5
#summary(m.zoo$VolFiltered)
# Use that to convert Counts to Abund in #/m3
m.zoo$Abund <- m.zoo$Count/m.zoo$VolFiltered
summary(m.zoo$Count) ; unique(m.zoo$Count)
summary(m.zoo$Abund)

### Format to WP2 standards
colnames(m.zoo)
# head(paste(m.zoo$Segment_Length, " n.m; 5 n.m assumed = 1.49m3 of filtered seawater", sep = ""))
# unique(m.zoo$Ship_Code)
colnames(m.zoo)[10] <- "Segment_No"

m.zoo2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;John_Kitchener",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;John.Kitchener@awe.gov.au", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "AADC-00099",
                decimalLatitude = m.zoo$Latitude, decimalLongitude = m.zoo$Longitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m.zoo$Datev2,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = m.zoo$Year, Month = m.zoo$months, Day = m.zoo$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = 10, DepthAccuracy = NA, DepthIntegral = "5-10", MinDepth = 5, MaxDepth = 10,
                ParentEventID = paste(m.zoo$Ship_Code, m.zoo$Tow_Number, sep = "_"),
                EventID = paste(m.zoo$Ship_Code, m.zoo$Tow_Number, m.zoo$Segment_No, sep = "_"),
                InstitutionCode = "Australian Antarctic Data Centre", SourceArchive = "https://data.aad.gov.au/metadata/records/AADC-00099",
                OrigCollectionCode = "AADC-00099", OrigCollectionID = "Not_applicable",
                BiblioCitation = "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC",
                CitationDOI = "doi:10.26179/ksds-s610", DateDataAccess = "14-10-2021",
                OrigScientificName = m.zoo$OrigScientificName, ScientificName = m.zoo$OrigScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = m.zoo$Abund,
                MeasurementUnit = "#/m3", MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = "Methods described in Pinkerton et al. 2020 (https://doi.org/10.1016/j.dsr.2020.103303)",
                SamplingProtocol = "CPR is towed behind ships and collects samples from 10m depth; 1.27cm2 aperture; water speed 0.2 m/s; silk mesh of 270 μm; collecting sink covered and rolled into a preservation tank containing formaldehyde",
                SampleAmount = paste(m.zoo$Segment_Length, " n.m; 5 n.m = 1.49m3 filtered seawater", sep = ""),
                SampleAmountUnit = "nautical miles and m3", SampleEffort = m.zoo$VolFiltered,
                DeterminedBy = NA, DeterminedDate = NA, Note = "ParentEventID = Ship_Code+Tow_Number; EventID = ParentEventID+Segment_No", Flag = NA,
                Temperature_Celsius = m.zoo$Water_Temperature, Salinity = m.zoo$Salinity, PAR_microE_m2_s1 = m.zoo$Photosynthetically_Active_Radiation, 
                Fluorescence = m.zoo$Fluorescence, PCI = m.zoo$Phytoplankton_Colour_Index  
) # eo ddf

head(m.zoo2)
dim(m.zoo2) # 15'204'798       75
str(m.zoo2)
unique(m.zoo2$PCI) # "-" "0" "1" "2" "3" is a factor
# Convert Temp, Sal, PAR & Fluo to numerics
m.zoo2[m.zoo2$Temperature_Celsius == "-","Temperature_Celsius"] <- NA
m.zoo2[m.zoo2$Salinity == "-","Salinity"] <- NA
m.zoo2[m.zoo2$PAR_microE_m2_s1 == "-","PAR_microE_m2_s1"] <- NA
m.zoo2[m.zoo2$Fluorescence == "-","Fluorescence"] <- NA
# Convert to num
m.zoo2$Temperature_Celsius <- as.numeric(m.zoo2$Temperature_Celsius)
m.zoo2$Salinity <- as.numeric(m.zoo2$Salinity)
m.zoo2$PAR_microE_m2_s1 <- as.numeric(m.zoo2$PAR_microE_m2_s1)
m.zoo2$Fluorescence <- as.numeric(m.zoo2$Fluorescence)
summary(m.zoo2[,c(71:75)])

# Check scientific names used
unique(m.zoo2$ScientificName); str(m.zoo2$ScientificName)
# Is a factor, convert to char:
m.zoo2$OrigScientificName <- as.character(m.zoo2$OrigScientificName)
m.zoo2$ScientificName <- as.character(m.zoo2$ScientificName)

### Need to modify most names manually because they will cause problems (extra blank spaces, typos in names, 8 instead of brackets etc...)
### We can keep the original labels in 'OrigScientificName' and use them to inform 'LifeForm' (juveniles, larvae etc.) in the "LifeForm" column

### First, in 'ScientificName', start removing the life stages and sex indicators listed below: 
# " sp", " larvae" , " small", " cyprid", " nauplius", " megalopa", " phyllosoma", " zoea", " natant juv", 
# " calyptopis", " furcilia", " C1", " C2", " C3", " F1", " F2", " F3", " F4", " F5", " F6", " nectophore", " metanauplius"

m.zoo2$ScientificName <- gsub(" larvae", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" small", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" cyprid", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" nauplius", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" megalopa", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" phyllosoma", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" zoea", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" natant juv", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" calyptopis", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" furcilia", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" C1", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" C2", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" C3", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" F1", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" F2", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" F3", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" F4", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" F5", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" F6", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" nectophore", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" metanauplius", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(" sp", "", as.character(m.zoo2$ScientificName), fixed = T)
# Check
unique(m.zoo2$ScientificName)


### Now, give worms::wormsbynames a first try and manually correct the labels that won't fit
### You'll use the OrigScientifcName to give proper lifestages etc.
require("worms") 
keys.worms <- wormsbynames( unique(m.zoo2$ScientificName) )

# Only a few labels to correct manually...
m.zoo2[m.zoo2$ScientificName == "Acartia Acartia danae","ScientificName"] <- "Acartia (Acartia) danae"
m.zoo2[m.zoo2$ScientificName == "Acartia Acartiura tranteri","ScientificName"] <- "Acartia (Acartiura) tranteri"
m.zoo2[m.zoo2$ScientificName == "Acartia Odontacartia pacifica","ScientificName"] <- "Acartia (Odontacartia) pacifica"
m.zoo2[m.zoo2$ScientificName == "Euphausia similis var armata","ScientificName"] <- "Euphausia similis"
m.zoo2[m.zoo2$ScientificName == "Euphausiainifera","ScientificName"] <- "Euphausia spinifera"
m.zoo2[m.zoo2$ScientificName == "Heterorhabdusinifrons","ScientificName"] <- "Heterorhabdus spinifrons"
m.zoo2[m.zoo2$ScientificName == "Hyperiainigera","ScientificName"] <- "Hyperia spinigera"

# Check again: 
keys.worms <- wormsbynames( unique(m.zoo2$ScientificName) )
keys.worms$ScientificName <- unique(m.zoo2$ScientificName)

# Add WoRMS_status field
m.zoo2 <-  add_column(m.zoo2, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(m.zoo2)

# For testing the functions below:
s <- unique(m.zoo2$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(m.zoo2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- m.zoo2[m.zoo2$ScientificName == s,]
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

        }, mc.cores = 20 # >2 when on kryo

) # eo mclapply - s in taxa_names

### Rbind
ddf <- bind_rows(res)
dim(ddf) # 15'204'798, same as before. Check some random lines.
ddf[1:50,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf[is.na(ddf$Species),][1:50,]
unique(ddf$WoRMS_ID) 
# What do the 'To_add_at_the_end' correspond to?
unique(ddf[ddf$WoRMS_ID == "To_add_at_the_end","ScientificName"])
# Gammaridea == PARAPHYLETIC!
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS","ScientificName"])
# Makes sense
unique(ddf$WoRMS_status) # good

### Finally, use OrigScientificName to provide lifestages info. All NA in 'LifeForm' will be assumed mature adults specimen. One can also use the mesh size of the plankton net used for sampling (if info is given) to derive a size class. For instance, a WP2 net should only capture organisms > 200µm

# To do so, identify those OrigScientificName levels that contain LifeForm info (sex, life stage, qualitative indicator of anything). Identify those indicators and report them in the LifeForm column based on grep()
unique(ddf$OrigScientificName)

# LifeForm indicators from OrigScientificName
# " larvae" , " small", " cyprid", " nauplius", " megalopa", " phyllosoma", " zoea", " natant juv", 
# " calyptopis", " furcilia", " C1", " C2", " C3", " F1", " F2", " F3", " F4", " F5", " F6", " nectophore", " metanauplius"


# o <- unique(ddf$OrigScientificName)[10] # For testing for loop below
for(o in unique(ddf$OrigScientificName)) {
    
    message(paste(o, sep = ""))
    
    if( grepl("small", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Small"
    } else if( grepl(" larvae", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Larva"
    } else if( grepl(" cyprid", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Cypris larva"
    } else if( grepl(" nauplius", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Nauplius larva"
    } else if( grepl(" megalopa", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Megalopa larva"
    } else if( grepl("phyllosoma", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Phyllosoma larva"
    } else if( grepl(" zoea", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Zoea larva"
    } else if( grepl(" natant juv", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Juvenile natantia"
    } else if( grepl("calyptopis", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Calyptopis larva"
    } else if( grepl(" furcilia", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia larva"
    } else if( grepl(" C1", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "C1 stage"
    } else if( grepl(" C2", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "C2 stage"
    } else if( grepl(" C3", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "C3 stage"
    } else if( grepl(" F1", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "F1 stage"
    } else if( grepl(" F2", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "F2 stage"
    } else if( grepl(" F3", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "F3 stage"
    } else if( grepl(" F4", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "F4 stage"
    } else if( grepl(" F5", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "F5 stage"
    } else if( grepl(" F6", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "F6 stage"
    } else if( grepl(" nectophore", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Nectophore"
    } else if( grepl(" metanauplius", x = o, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Metanauplius larva"
    } 
    
} # eo for loop

### Check: 
unique(ddf$LifeForm)

# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[70000:70050,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[35020:35080,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[373520:373580,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[1520000:1520050,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
# Ganz OK :-)

### Last, make a map of sampling effort in space
d.effort <- ddf
d.effort$x_1d <- round(d.effort$decimalLongitude)
d.effort$y_1d <- round(d.effort$decimalLatitude)
d.effort$cell_id <- factor(paste(d.effort$x_1d, d.effort$y_1d, sep = "_"))
require("dplyr")#; detach("package:plyr", unload = TRUE)
### Issue related to the loading of the 'worms' R pakage! detach worms and then plyr
detach("package:worms", unload = TRUE)
detach("package:marmap", unload = TRUE)
detach("package:reshape2", unload = TRUE)
spatial.effort <- data.frame(d.effort %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
dim(spatial.effort) ; summary(spatial.effort) # 2136    4 

# Map sampling effort
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180 & world$lat <= -35 & world$lat >= -80,],
        fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### And for only those calcifying pteropod species
unique(d.effort$Order)
dim(na.omit(d.effort[d.effort$Order == "Pteropoda",c("Month","Year")])) # 362'019 obs
summary(na.omit(d.effort[d.effort$Order == "Pteropoda",c("Month","Year")]))

spatial.effort <- na.omit(data.frame(d.effort[d.effort$Order == "Pteropoda",] %>% 
                    group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n())) )
# 2136 grid cells

ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180 & world$lat <= -35 & world$lat >= -80,],
        fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = spatial.effort) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata")
write.table(ddf, file = "SO-CPR_reformatted+WoRMScheck_17_10_21.txt", sep = "\t")

### Check distrbution of Pteropoda counts data
so.data <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
# unique(so.data$Order)
so.data <- so.data[so.data$Order == "Pteropoda" & !is.na(so.data$Order),]
unique(so.data$ScientificName)
head(so.data)

### Check distrbution of data 
summary(so.data$MeasurementValue) ; unique(so.data$MeasurementValue)
ggplot(so.data, aes(x = log1p(MeasurementValue))) + geom_histogram(binwidth = .1, colour="black", fill="white") +
    xlab("Pteropoda counts") + ylab("Count - raw data - SO")

### Compute monthly clims
so.data$x <- round(so.data$decimalLongitude, .1)
so.data$y <- round(so.data$decimalLatitude, .1)
unique(so.data$x)
unique(so.data$y)
so.data$id <- factor(paste(so.data$x, so.data$y, so.data$Month, sep = "_"))
# Derive monthly clims
clims <- data.frame(so.data %>% group_by(id) %>% 
    summarize(x = unique(x), y = unique(y), month = unique(Month), Nevents = length(unique(EventID)), sum = sum(MeasurementValue), mean = sum/Nevents ) 
)

summary(clims)

unique(clims$mean)

ggplot(clims, aes(x = log1p(mean))) + geom_histogram(binwidth = .1, colour="black", fill="white") +
    xlab("Euthecosomata counts (log(x+1))") + ylab("Count - monthly climatologies") + facet_wrap(.~ factor(month), ncol = 4)


### ------------------------------------------------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------------------------------------------------
