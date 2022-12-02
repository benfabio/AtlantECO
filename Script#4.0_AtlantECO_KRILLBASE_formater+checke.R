
##### ATLANTECO SCRIPT 4.0 ----------------------------------------------------------------------------------------------------------------------------
##### 21/10/2021: R Script to check the data of Burridge et al. (2016) sent by Katja Peijnenburg () © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Original dataset available at: Atkinson et al. (2019)

### Aims to:
# - Read the file that you prepared on the 28/04/21
# - Reformat to AtlantECO standards - WoRMS check

### Latest update: 21/10/2021

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("readxl")
library("lubridate")

world <- map_data("world")  # for maps

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/KRILLBASE")
dir()

### ------------------------------------------------------------------------------------------------------------------------------------------------------

### 1°) Read the data: Burridge_etal2016_PteropodsHeteropods_TableS1_AtlantECO_nknecht.csv
krikri <- read.csv("KRILLBASE_full_query_cleaned_28_04_2021.csv", h = T, sep = ";", dec = ",")
dim(krikri) # 14543    29
str(krikri) # Looks ganz OK
head(krikri)
summary(krikri)
colnames(krikri)

### Description of original headers: 
# Station:	Unique identifier for each record (row). The first three letters identify the source of the data (starting letters of the name of the individual, national program, or country which provided the data). The next 4 numbers identify the season of sampling (e.g. 1926 spans Oct 1925 to Sept 1926). The next 3 letters provide additional sample information, often referring either to the net type used or the name of the sampling survey. Additional characters at the end list the station numbers etc. These are, as far as possible, the same as used in the original sources, with British Antarctic Survey and Palmer LTER cruise station numbers being replaced by cruise-unique “event numbers”. Records are typically resolved to station but see RECORD_TYPE for more information on resolution.
unique(krikri$Station)
# RecordType:	This is an important field that will need screening before any use of the database. Records labelled “haul” are the usual situation meaning that the record refers to a single net haul. “Survey mean” represents a record where the krill or salp density represents an arithmetic mean of a group of stations whose central position and sampling point are thus provided in the database with less accuracy then the other records. Survey means are given only when it was not possible to obtain station-specific data. “Stratified haul” represents a haul, usually within the top 200 m, which forms part of a stratified series (e.g. 0-50m, 50-100m, 100-200m). “Stratified pooled haul” represents a record that integrates these respective stratified hauls, whereby the krill or salp densities from the component nets have been summed (in this example into an equivalent 0-200m haul). Thus to avoid double counting any use of the data should sift out either stratified hauls or stratified pooled hauls.
unique(krikri$RecordType)
# Nb_stations: the number of stations that have been averaged to provide the krill or salp density values.
unique(krikri$Nb_stations)
# Nb_nets: This refers to the number of sequentially fished nets included in the estimate (e.g. the value would be 3 for a stratified pooled haul consisting of a stratified series sampling 0-50m, 50-100m and 100-200m, and it would be 32 for a survey mean which averages 32 hauls). A LHPR haul counts as one net despite multiple gauzes being cut. This value is also 1 for a paired Bongo haul (2 nets fished concurrently).
unique(krikri$Nb_nets)
# decimalLatitude: Units are decimal degrees
# decimalLongitude: Units are decimal degrees
summary(krikri[,c("decimalLatitude","decimalLongitude")]) # ganz OK
# Season: This is the austral “summer” season of sampling. For example the 1926 season spans all data from 1 Oct 1925 through to 30 Sept 1926.
# Date: The date of sampling, based on the dates provided to us (see “DATE ACCURACY” column).
unique(krikri$Date)
### !!! Issue here: too many different formats for dates --> need to clean

# Days_since_1Oct	This is the day of sampling during the austral season. Therefore 1 Oct is DAYS_FROM_1ST_OCT =1. The value for dates after 28 February vary depending on whether they occur during a leap year.
# DateAccuracy: “D” means the exact day of sampling is known. “M” means that we have been provided only with the month in which samples were taken, so the record’s DATE value is entered as the middle of the month. “Y” means only the year of sampling is known, so the date is recorded as 1st January (this affects one record only)
unique(krikri$DateAccuracy) # OK but will need to explicit the acronyms

# NetTime: This is the time of the haul: Either the start, midpoint or end times of hauls were used, as provided to us. Absent data means no net time information was available, or it was not entered into the database because the station was already classified as either day or night (Discovery data net times are recorded in their published “Station Lists” but not entered in KRILLBASE). Net times for Stratified pooled hauls represent that of the shallowest net of the series.
unique(krikri$NetTime) # Looks OK

# GMT_or_local: Information on whether the time in the previous column is GMT (labelled “GMT”). Data which were provided as local times with a stated offset to GMT have been converted to GMT. Data which were provided as local times with no offset have not been converted and are labelled “local”. Absent data means there was no net time information
unique(krikri$GMT_or_local) # Replace blan char by NA
krikri[krikri$GMT_or_local == "","GMT_or_local"] <- NA

# Day_or_night: This field indicates whether the net was hauled in daylight (labelled “day”) or night time (labelled “night”) and was used in the calculation of standardised krill densities. See DAY_NIGHT_METHOD for information on the source of these data.
unique(krikri$Day_or_night)
krikri[krikri$Day_or_night == "","Day_or_night"] <- NA

# DayNightMethod: Method used to determine whether the net was hauled in daylight or at night time, which depends on the time information available: 1 - DAY_NIGHT is based on calculated solar elevation determined using NET_TIME; 2 - DAY_NIGHT is as recorded in the ship’s log; 3 - no DAY_NIGHT information was available, and standardised krill densities were adjusted for the probability that the haul was conducted in daylight.
unique(krikri$DayNightMethod) # don't care about this
# NetType: This is a brief name for the sampling net used.
unique(krikri$NetType)

# Area_net_m2: This is a nominal mouth area of the net calculated from the net dimensions. It is typically the simple linear area of the mouth, but for RMT8 and 1 it is assigned as value of 8 and 1 respectively. Bongo nets are assigned as an area of both openings combined and LHPR is given as maximum net diameter – both of these are used to crudely compensate for the lack of towing bridles and wire/release gear directly in front of the net, as compared to the standard ring nets often of similar net dimensions.
summary(krikri$Area_net_m2)

# Vol_filtered_m3: Volume of water (m3) filtered by the net. This value is provided only when the value is provided with the density data.
summary(krikri$Vol_filtered_m3) # 66% of the data missing this information

# TopSamplingDepth_m: Shallowest sampling depth (m)
# BottomSamplingDepth_m	Deepest sampling depth (m). Note that whilst most hauls were oblique, double oblique or vertical, a small minority were nearly horizontal, as shown by similar top and bottom depths. These would need to be screened out of nearly all analyses as they provide little information on numerical densities (no. m-2).
summary(krikri[,c("TopSamplingDepth_m","BottomSamplingDepth_m")]) # OK

# KrillDensity_<1m2	Numerical density, N, of numbers of postlarval krill under 1 m2 (or, where based on a length frequency distribution as in the Discovery Investigations, it is krill >19mm in length). Where the numbers of krill n were provided per m3 filtered, the density of krill was calculated based on top sampling depth t and bottom sampling depth b in metres as N = n *(b-t)
# Stdev_MeanKrill	The standard deviation of the krill densities extracted from the publications where the survey mean value of krill density is provided (see column RECORD_TYPE)
summary(krikri[,c("KrillDensity_.1m2","Stdev_MeanKrill")]) # 99.9% of the data lack the Stdev_MeanKrill

# Stand_KrillDensity_.1m2:	Standardised numerical density of postlarval krill. To reduce possible artefacts arising from differences in sampling method in KRILLBASE, this column presents krill density according to a single sampling method. This method is a 0-200 m night-time RMT8 haul on 1 January, following the standardisation method in Atkinson et al. (2008).
# SalpDensity_.1m2:	The numerical density of salps, calculated as for krill. All individuals are counted, irrespective of which salp species or whether they are the solitaries of components of aggregate chains. Standardised salp densities have not been calculated.
summary(krikri[,c("Stand_KrillDensity_.1m2","SalpDensity_.1m2")]) # 2950 NAs there?  

# N_S_PF.: Position (North or South) relative to the Antarctic polar front as published by Orsi et al. (1995): Oceanographic Research Papers, 42(5), pp.641-673
summary(factor(krikri[,"N_S_PF."]))
#  N     S 
# 765 13778

# MeanWaterDepth_10km	Mean water depth within a 10 km radius. In South Polar Stereographic projection, the stations were superimposed on the Gebco bathymetry (IOC et al. 2002: http://www.gebco.net/data_and_products/gebco_digital_atlas/) and all pixels within a 10 km radius of the station were extracted. After removing data above sea level, the remaining pixel value for water depth was averaged.
summary(krikri$MeanWaterDepth_10km)
# WaterDepthRange_10km	Depth range within a 10 km radius. In the procedure above, having removed pixels above sea level, the range in water depth was calculated as the difference between the shallowest and the deepest pixel. This will provide an index of even-ness of bathymetry (e.g. proximity to seamounts, canyons, continental slope).
summary(krikri$WaterDepthRange_10km)

# Temperature_clim: Long term average February sea surface temperature for the sampling locale. This is not the actual sea temperature at the time of sampling but a climatological mean sea-surface value for February, averaged over the years 1979 to 2014, based on data downloaded July 2016 from http://apps.ecmwf.int/datasets/data/interim-full-moda/levtype=sfc/ . Data were provided on a 0.75 degree by 0.75degree grid and we extracted mean values using the same 10 km buffer method used for the bathymetry. These values may indicate a relative thermal regime as a basis for station characterisation
# Caveats: Any issues which might require particular caution when using the data (e.g. potential inaccuracies in estimated date or day/night or sampling depths outside of the normal range) are listed here. Default is blank.
unique(krikri$Caveats)

# Sources: Information about the source of the data, including a citable reference where available.
unique(krikri$Sources)


### ------------------------------------------------------------------

### OK, first, just correct the dates and convert to YMD with lubridate and extract Day/Month/Year values
krikri$Dates2 <- krikri$Date
str(krikri$Dates2) # chars, good one can just use gsub()
unique(krikri$Dates2)
# To be replaced: 
# "-DEC-"
krikri$Dates2 <- str_replace_all(krikri$Dates2, "-DEC-", ".12.")
# "-FEB-"
krikri$Dates2 <- str_replace_all(krikri$Dates2, "-FEB-", ".02.")
# "-APR-"
krikri$Dates2 <- str_replace_all(krikri$Dates2, "-APR-", ".04.")
# "-AUG-"
krikri$Dates2 <- str_replace_all(krikri$Dates2, "-AUG-", ".08.")
# "-MAY-"
krikri$Dates2 <- str_replace_all(krikri$Dates2, "-MAY-", ".05.")

# Convert to dmy: summary(lubridate::dmy(krikri$Dates2))
krikri$Dates2 <- lubridate::dmy(krikri$Dates2)
krikri$Day <- lubridate::day(krikri$Dates2)
krikri$Month <- lubridate::month(krikri$Dates2)
krikri$Year <- lubridate::year(krikri$Dates2)
summary(krikri[,c("Day","Month","Year")]) # Alles guet

### Reformat to AtlantECO template.
colnames(krikri)
# For sampling protocol: combine RecordType, NetType and Area_net_m2
# But first, need to melt to have salps and krill densities as rows (not two separate columns)
m.krikri <- melt(krikri, id.vars = colnames(krikri)[c(1:21,24:33)])
head(m.krikri); dim(m.krikri)
summary(m.krikri[,c("variable","value")])
colnames(m.krikri)[c(32:33)] <- c("OrigScientificName","Density")

krikrikri <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@env.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "",
                decimalLatitude = m.krikri$decimalLatitude, decimalLongitude = m.krikri$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m.krikri$Dates2,
                eventDateInterval = m.krikri$DateAccuracy, eventDateIntervalUnit = m.krikri$DateAccuracy,
                Year = m.krikri$Year, Month = m.krikri$Month, Day = m.krikri$Day,
                Bathymetry = m.krikri$MeanWaterDepth_10km, BathySource = "Mean water depth within a 10 km radius-GEBCO",
                HabitatType = "Water_column", LonghurstProvince = NA, Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = m.krikri$TopSamplingDepth_m, MaxDepth = m.krikri$BottomSamplingDepth_m,
                ParentEventID = NA, EventID = m.krikri$Station, InstitutionCode = NA,
                SourceArchive = "https://data.bas.ac.uk/full-record.php?id=GB/NERC/BAS/PDC/00915",
                OrigCollectionCode = "Not_applicable", OrigCollectionID = "Not_applicable",
                BiblioCitation = "Atkinson et al. (2017). KRILLBASE: a circumpolar database of Antarctic krill and salp numerical densities, 1926–2016. Earth Syst. Sci. Data, 9(1), 193-210.",
                CitationDOI = "doi:10.5194/essd-9-193-2017", DateDataAccess = "05-05-2021",
                OrigScientificName = m.krikri$OrigScientificName, ScientificName = NA,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = "To_add_at_the_end", 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms density", MeasurementTypeID = "To_define",
                MeasurementValue = m.krikri$Density,
                MeasurementUnit = "#/m2", MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = m.krikri$RecordType,
                SamplingProtocol = paste("Tow type= ",m.krikri$RecordType,"; with: ",m.krikri$NetType,"; area= ",m.krikri$Area_net_m2, sep = ""),
                SampleAmount = m.krikri$Vol_filtered_m3,
                SampleAmountUnit = "m3",
                SampleEffort = paste("N stations= ",m.krikri$Nb_stations,"; N nets= ",m.krikri$Nb_nets, sep = ""),
                DeterminedBy = m.krikri$Sources_cleaned,
                DeterminedDate = NA, Note = m.krikri$Caveats, Flag = NA 
) # eo ddf
dim(krikrikri) # 29'086    70
head(krikrikri)
unique(krikrikri$OrigScientificName)
### Fill in the following columns based on 'OrigScientificName':
# "MeasurementAcurracy"
krikrikri[krikrikri$OrigScientificName == "Stand_KrillDensity_.1m2","MeasurementAcurracy"] <- "Standardized density, see Atkinson et al. (2008)"
krikrikri[krikrikri$OrigScientificName == "SalpDensity_.1m2","MeasurementAcurracy"] <- "Unstandardized density"

# "ScientificName"
krikrikri[krikrikri$OrigScientificName == "Stand_KrillDensity_.1m2","ScientificName"] <- "Euphausia superba"
krikrikri[krikrikri$OrigScientificName == "SalpDensity_.1m2","ScientificName"] <- "Salpa"

# "LifeForm" (Postlarval krill, >19mm); irrespective of which salp species or whether they are the solitaries or components of aggregate chains
krikrikri[krikrikri$OrigScientificName == "Stand_KrillDensity_.1m2","LifeForm"] <- "Postlarval krill, >19mm"
krikrikri[krikrikri$OrigScientificName == "SalpDensity_.1m2","LifeForm"] <- "Mostly Salpa thompsoni; solitaries or components of aggregate chains"

### OK, WoRMS check (simple):
library("worms") 
keys.worms <- wormsbynames( unique(krikrikri$ScientificName) )
keys.worms$ScientificName <- unique(krikrikri$ScientificName)

# Add WoRMS_status field
krikrikri <-  add_column(krikrikri, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(krikrikri)
# head(krikrikri)

# For testing the functions below:
s <- unique(krikrikri$ScientificName)[2] ; s

require("parallel")
res <- mclapply( unique(krikrikri$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- krikrikri[krikrikri$ScientificName == s,]
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
dim(ddf) # 29'086 (half Antarctic krill, half Salps)
ddf[1:50,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf[is.na(ddf$Species),][1:50,]
unique(ddf$WoRMS_ID) 
unique(ddf$WoRMS_status) # good

### Make map of sampling effort and save

### Last, make a map of sampling effort in space and then maybe a Hövmoller plot
d.effort <- ddf
d.effort$x_1d <- round(d.effort$decimalLongitude)
d.effort$y_1d <- round(d.effort$decimalLatitude)
d.effort$cell_id <- factor(paste(d.effort$x_1d, d.effort$y_1d, sep = "_"))
require("dplyr")
detach("package:worms", unload = TRUE)
detach("package:marmap", unload = TRUE)
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
spatial.effort <- data.frame(d.effort %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
dim(spatial.effort) ; summary(spatial.effort)

# Map sampling effort
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180 & world$lat <= -30 & world$lat >= -80,],
        fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Save
save(ddf, file = "KRILLBASE_Atkinson2017_reformatted+WoRMScheck_21_10_21.Rdata")
write.table(ddf, file = "KRILLBASE_Atkinson2017_reformatted+WoRMScheck_21_10_21.txt", sep = "\t")

### 01/11/2021: Make a map of sampling effort for krill and salps data with polar coordinates
# library("devtools"); library("remotes")
# remotes::install_github("AustralianAntarcticDivision/SOmap")
# library("SOmap")
# library("raster")
# my_cmap <- colorRampPalette(c("#4D4140", "#596F7E", "#168B98", "#ED5B67","#E27766", "#DAAD50", "#EAC3A6"))(51)
# base_map <- SOmap()
# plot(base_map)

ddf <- get(load("KRILLBASE_Atkinson2017_reformatted+WoRMScheck_21_10_21.Rdata"))
dim(ddf)
str(ddf)
unique(ddf$ScientificName) # "Euphausia superba" "Salpa"    
# But no need to split in two parts
d.effort <- ddf
d.effort$x_1d <- round(d.effort$decimalLongitude)
d.effort$y_1d <- round(d.effort$decimalLatitude)
d.effort$cell_id <- factor(paste(d.effort$x_1d, d.effort$y_1d, sep = "_"))
spatial.effort <- data.frame(d.effort %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
dim(spatial.effort) ; summary(spatial.effort)

# Map with orthogonal proj
map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
  scale_y_continuous(name = "", labels = NULL) + scale_x_continuous(name = "", labels = NULL) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) +
  scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
  theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
      panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")   
# Save
ggsave(plot = map, filename = "map_sampling_effort_KRILLBASE_01_11_21.jpg", dpi = 300, width = 6, height = 6)

# Mean year, depth? etc.
summary(ddf$Year)
mean(ddf$Year); sd(ddf$Year) 
summary(ddf[,c("Depth","MinDepth","MaxDepth")])
sd(ddf$MaxDepth)
#unique(ddf$Depth) # 
unique(ddf$MeasurementType) # "density"
unique(ddf$MeasurementUnit) # #/m2
summary(ddf$MeasurementValue)


### ------------------------------------------------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------------------------------------------------
