
##### ATLANTECO SCRIPT 1.3.4 ----------------------------------------------------------------------------------------------------------------------------
##### 25/10/2021: R Script to format CPR abundance data sent by David Johns (NPac & NAtl abunda ce data of Coccos, Forams and Thecosomata) following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#   - Read CPR occurrence dataset sent by David Johns et al. on the 22/10/21
#   - Combine the 3 .txt files: occurrence, event and extendedmeasurementorfact.txt
#	- Split CPR occurrence dataset into phyto vs. zoo (first for PHYTObase update, second for ZOObase update)
#   - Remove occurrences not at genus of species level (can be done at same time as above)
#   - Re-format to AtlantECO WP2 template? Maybe later

### Latest update: 25/10/2021

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")
library("lubridate")
library("viridis")
library("worms")

world <- map_data("world")  # for maps

### ----------------------------------------------------------------------------------------------------------------------------

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR")
dir()

### 1°) Read the the file, examine and adjust if necessary
cpr <- read.table("Copie de CPR_foram_theco_cocco-1.csv", sep = ";", dec = ",", h = T)
dim(cpr) # 250'620     10 
str(cpr)
head(cpr)
summary(cpr)
# cpr[50620:50640,c("Thecosomata_Pacific","Thecosomata_NAtlantic")]
# Quickly melt to have both 
m.cpr <- melt(cpr, id.vars = c("Sample_Id","decimalLatitude","decimalLongitude","Midpoint_Date_Local","Year","Month"))
str(m.cpr) ; head(m.cpr)
summary(factor(m.cpr$variable)) ; summary(m.cpr$value)
colnames(m.cpr)[c(7,8)] <- c("OrigScientificName","Abund")
m.cpr$OrigScientificName <- as.character(m.cpr$OrigScientificName)
unique(m.cpr$OrigScientificName)
m.cpr[m.cpr$OrigScientificName %in% c("Thecosomata_NAtlantic","Thecosomata_Pacific"),"OrigScientificName"] <- "Euthecosomata"
# Remove NA
m.cpr <- m.cpr %>% drop_na("Abund")
#dim(m.cpr) ; summary(m.cpr); str(m.cpr)

### Convert Midpoint_Date_Local to 'Date' object?
# head( as.Date(m.cpr$Midpoint_Date_Local) )
# head( lubridate::dmy(m.cpr$Midpoint_Date_Local) )
# m.cpr$Midpoint_Date_Local <- as.Date(m.cpr$Midpoint_Date_Local)
# Split actual date (d.m.y) from local time using " " as strsplit
#head( do.call(rbind, strsplit(x = unique(m.cpr$Midpoint_Date_Local), split = " ")) ) # 1st col for date, 2nd for local time (GMT?)
m.cpr$Date <- do.call(rbind, strsplit(x = m.cpr$Midpoint_Date_Local, split = " "))[,1]
m.cpr$Time <- do.call(rbind, strsplit(x = m.cpr$Midpoint_Date_Local, split = " "))[,2]
# head(m.cpr)
head( lubridate::dmy(m.cpr$Date) ) # str( lubridate::dmy(m.cpr$Date) ) # Good. Extract Day
m.cpr$Date <- lubridate::dmy(m.cpr$Date)
m.cpr$Day <- lubridate::day(m.cpr$Date)
head(m.cpr)
# Nice.

### Reformat to AtlantECO WP2 standards
colnames(m.cpr)
unique(m.cpr$Sample_Id) # 

m.cpr2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;David_Johns",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;djoh@MBA.ac.uk", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = m.cpr$decimalLatitude, decimalLongitude = m.cpr$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = "Comparison between the calculated position and data from vessels where a GPS record was available suggests the position assigned to CPR samples is accurate to within 10–20 nautical miles.",
                CountryCode = NA, eventDate = m.cpr$Date,
                eventDateInterval = m.cpr$Time, eventDateIntervalUnit = "Assumed GMT", Year = m.cpr$Year, Month = m.cpr$Month, Day = m.cpr$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = 7, DepthAccuracy = "2-3m", DepthIntegral = NA,
                MinDepth = 5, MaxDepth = 10, ParentEventID = "CPR", EventID = m.cpr$Sample_Id,
                InstitutionCode = "Marine Biological Association of the UK (MBA)", SourceArchive = "https://www.dassh.ac.uk/doitool/data/1763",
                OrigCollectionCode = "Not_applicable", OrigCollectionID = "Not_applicable",
                BiblioCitation = "David Johns Marine Biological Association of the UK (MBA) (2021): Continuous Plankton Recorder data for all coccolithophores, foraminifera and thecosomata - all areas. The Archive for Marine Species and Habitats Data (DASSH). https://doi.org/10.17031/1763",
                CitationDOI = "10.17031/1763",
                DateDataAccess = "22-10-2021",
                OrigScientificName = m.cpr$OrigScientificName, ScientificName = m.cpr$OrigScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = NA, Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = m.cpr$Abund,
                MeasurementUnit = "#/m3", MeasurementAcurracy = "",
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = "CPR survey - Methods described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011)",
                SamplingProtocol = "Described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011)",
                SampleAmount = "The volume of water filtered for each 10 nautical mile sample is about 3m3 (mean = 3.27 m3, SD = 0.71 m3, n = 1723, Jonas et al., 2004).",
                SampleAmountUnit = "nautical miles and m3", SampleEffort = "volume of water filtered for each 10 nautical mile sample is about 3m3",
                DeterminedBy = "SAHFOS",
                DeterminedDate = NA, Note = "Concentration values of 0.001 indicate presence only", Flag = NA 
) # eo ddf
head(m.cpr2)
dim(m.cpr2) # 816'806    70
str(m.cpr2)


# Check: 
#keys.worms <- wormsbynames( unique(m.cpr2$ScientificName) )
# No match for coccolithophores...change ScientificName to Prymnesiophyceae
m.cpr2[m.cpr2$ScientificName == "Coccolithophores","ScientificName"] <- "Prymnesiophyceae"
keys.worms <- wormsbynames( unique(m.cpr2$ScientificName) )
keys.worms$ScientificName <- unique(m.cpr2$ScientificName)

# Add WoRMS_status field
m.cpr2 <-  add_column(m.cpr2, WoRMS_status = NA, .after = "WoRMS_ID")
#colnames(m.cpr2)

# For testing the functions below:
s <- unique(m.cpr2$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(m.cpr2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- m.cpr2[m.cpr2$ScientificName == s,]
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

        }, mc.cores = 2 # >2 when on kryo

) # eo mclapply - s in taxa_names

### Rbind
ddf <- bind_rows(res)
dim(ddf) # 816'806, same as before. Check some random lines.
ddf[1:50,]
unique(ddf$Species) # Good. No species-level obs
ddf[is.na(ddf$Species),][1:50,]
unique(ddf$WoRMS_ID) # No NA
unique(ddf$WoRMS_status) # good

### Counts were ACTUALLY in 3/m3, not #/m3 (pers. comm. David Johns). Need to divide by 3
# colnames(ddf)
summary(ddf$MeasurementValue) ; unique(ddf$MeasurementValue)
ddf$MeasurementValue <- (ddf$MeasurementValue)/3 ; summary(ddf$MeasurementValue)
unique(ddf$MeasurementValue)


### Last, make a map of sampling effort in space and then maybe a Hövmoller plot
d.effort <- ddf
d.effort$x_1d <- round(d.effort$decimalLongitude)
d.effort$y_1d <- round(d.effort$decimalLatitude)
d.effort$cell_id <- factor(paste(d.effort$x_1d, d.effort$y_1d, sep = "_"))
require("dplyr")
detach("package:worms", unload = T)
detach("package:marmap", unload = T)
detach("package:reshape2", unload = T)
detach("package:plyr", unload = T)

spatial.effort <- data.frame(d.effort %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
dim(spatial.effort) ; summary(spatial.effort)

# Map sampling effort
world <- map_data("world")  # for maps

ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "CPR_calcifiers_reformatted+WoRMScheck_25_10_21.Rdata")
write.table(ddf, file = "CPR_calcifiers_reformatted+WoRMScheck_25_10_21.txt", sep = "\t")

# d <- get(load("CPR_calcifiers_reformatted+WoRMScheck_25_10_21.Rdata"))
# unique(d$MeasurementValue)

### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
