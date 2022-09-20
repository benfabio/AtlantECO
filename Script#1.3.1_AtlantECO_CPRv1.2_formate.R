
##### ATLANTECO SCRIPT 1.3.1 ----------------------------------------------------------------------------------------------------------------------------
##### 27/04/2021: R Script to format CPR occurrence datasets following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#   - Read CPR occurrence dataset sent by David Johns et al. on the 20/04/21
#   - Combine the 3 .txt files: occurrence, event and extendedmeasurementorfact.txt
#	- Split CPR occurrence dataset into phyto vs. zoo (first for PHYTObase update, second for ZOObase update)
#   - Remove occurrences not at genus of species level (can be done at same time as above)
#   - Re-format to AtlantECO WP2 template: zoo first and then phytoplankton

### Latest update: 30/11/2021

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

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2")
dir()

### 1°) Read the 3 .txt files and merge 
occ <- read.table("occurrence.txt", sep = "\t", dec = ".", h = T)
dim(occ)
head(occ)
summary(occ)
unique(occ$individualCount)
# unique(occ$occurrenceStatus) # 'present' only...but then what about individualCount?
length(unique(occ$scientificName))
unique(occ$scientificName)
grepl(pattern = "Eutheco", x = as.character(unique(occ$scientificName)), fixed = T)
grepl(pattern = "Cocco", x = as.character(unique(occ$scientificName)), fixed = T)
grepl(pattern = "Foram", x = as.character(unique(occ$scientificName)), fixed = T)
grepl(pattern = "Creseis", x = as.character(unique(occ$scientificName)), fixed = T)

### ...

event <- read.table("event.txt", sep = "\t", dec = ".", h = T)
dim(event)
head(event)
summary(event)
unique(event$sampleSizeValue)

meta <- read.table("extendedmeasurementorfact.txt", sep = "\t", dec = ".", h = T)
dim(meta)
head(meta)
summary(meta)

colnames(occ) ; colnames(event) ; colnames(meta)

meta[1:50,]

### All 3 files seem to be linked throuhg the "id" header
length(unique(occ$id)) == length(unique(event$id)) # is TRUE
length(unique(occ$id)) == length(unique(meta$id)) # is TRUE too! 
# Use 'id' to cbind ! 

ids <- unique(occ$id) ; head(ids)
# i <- ids[5800]

occ[occ$id == i,]
meta[meta$id == i,]
event[event$id == i,]

sub.occ <- occ[occ$id == i,]
sub.occ[,colnames(event)[c(2:19)]] <- event[event$id == i,c(2:19)]
head(sub.occ)

# Trying to match obs in occ+event with meta based on occurrenceID
commons <- intersect(unique(sub.occ$occurrenceID), unique(meta[meta$id == i,"occurrenceID"]))
oid <- commons[1]
sub.occ[sub.occ$occurrenceID == oid,]
sub.meta <- meta[meta$occurrenceID == oid,]
# Reshape 'sub.meta' based on the different "measurementType" and "measurementValue"
#dcast(sub.meta, id+occurrenceID+measurementID+measurementTypeID+measurementValueID+measurementUnit+measurementUnitID ~ measurementType, value.var = "measurementValue")



res <- mclapply(ids, function(i) {
            message(paste(i, sep = ""))   
            sub.occ <- occ[occ$id == i,]
            # dim(sub.occ) ; dim(event[event$id == i,]) ; dim( meta[meta$id == i,] )
            sub.occ[,colnames(event)[c(2:19)]] <- event[event$id == i,c(2:19)]
            return(sub.occ)
    }, mc.cores = 30
) # eo mclapply
# Rbind
cpr.merged <- bind_rows(res)
head(cpr.merged) ; dim(cpr.merged) # 2'112'490
summary(cpr.merged)
rm(res) ; gc()

# Quickly save
save(cpr.merged, file = "dwca_CPRv1.2_occ+event_27_04_2021.Rdata")
### For extendedmeasurementorfact.txt, ask the CPR guys
#
# ### 22/06/2022: Checking data for le S. Ramondenc
# cpr <- get(load("dwca_CPRv1.2_occ+event_27_04_2021.Rdata"))
# dim(cpr)
# str(cpr)
# # Check what wa caught between May & July 2013 in the following bounding box:
# cpr$Date <- do.call(rbind, strsplit(x = cpr$eventDate, split = "T"))[,1]
# #head( lubridate::ymd(cpr$Date) ) # str( lubridate::dmy(m.cpr$Date) ) # Good. Extract Day
# cpr$Date <- lubridate::ymd(cpr$Date)
# cpr$Day <- lubridate::day(cpr$Date)
# cpr$Month <- lubridate::month(cpr$Date)
# cpr$Year <- lubridate::year(cpr$Date)
# subset <- cpr[cpr$Year == 2013,]
# subset <- subset[subset$Month >= 5,]
# subset <- subset[subset$Month <= 7,]
# subset <- subset[subset$Month <= 7,]
# subset <- subset[subset$decimalLongitude >= -18,]
# subset <- subset[subset$decimalLongitude <= -15,]
# subset <- subset[subset$decimalLatitude >= 47,]
# subset <- subset[subset$decimalLatitude <= 50,]
# dim(subset)
#
# write.table(subset, "subset_4leS_dwca_CPRv1.2_occ+event_27_04_2021.txt", sep = "\t")

### 2°) Split into phyto vs. zoo
names <- unique(cpr.merged$scientificName)
phyto.ind <- c(2:6,13,21,22,25:28,31:37,39,41,42,44:47,50:54,56:59,61,64:71,80,83,87:92,95:98,100:101,108,110:112,118:120,130:133,136,
            139,141,146,159,161,162:167,169,171,172,177:179,186:188,191,193:195,200:204,206,209:213,215,217,218,220,222,223,225:231,
            242,245,248:249,252,258:265,268,271,273,276,278:282,284,287,289,292,295,303,307)
phyto <- names[phyto.ind]
zoo <- setdiff(unique(cpr.merged$scientificName),phyto)

### And re-sub set zoo (remove non species level occ)
zoo.ind <- c(4,6,8:12,14,17:22,25:26,29:34,36:37,39:43,45:48,50:58,61:64,68:72,74:82,84:85,87:91,93:107,110:116,
            118:137,139:142,144:164)
zoo2 <- zoo[zoo.ind]

# !! NOTES:
# 137 = Zoothamnium pelagicum = Ciliates that form branching colonies

cpr.merged.phyto <- cpr.merged[cpr.merged$scientificName %in% phyto,]
cpr.merged.zoo <- cpr.merged[cpr.merged$scientificName %in% zoo2,]
dim(cpr.merged) ; dim(cpr.merged.phyto) ; dim(cpr.merged.zoo)

save(cpr.merged.phyto, file = "dwca_CPRv1.2_occ+event_phyto_27_04_2021.Rdata")
save(cpr.merged.zoo, file = "dwca_CPRv1.2_occ+event_zoo_27_04_2021.Rdata")

### How easy is it to merge it with PHYTObase?
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/PHYTObase")
phytobase <- read.csv("PHYTObase_final_Righettietal._22_02_2021.csv", h = T, sep = ",")
dim(phytobase)
summary(phytobase)
str(phytobase)
colnames(phytobase)

### Identify the key that identifies CPR data
unique(phytobase$collectionCode_obis) # 14 = CPR
unique(phytobase$resname_obis) # "Continuous Plankton Recorder Dataset (SAHFOS) - Pacific Phytoplankton" & "Continuous Plankton Recorder (Phytoplankton)"  

unique(phytobase$organismQuantityType)
summary(phytobase$organismQuantity)
summary(phytobase$individualCount)

### ----------------------------------------------------------------------------------------------------------------------------

### 25/10/21: Go back to the "dwca_CPRv1.2_occ+event_zoo_27_04_2021.Rdata" file and reformat to AtlantECO standards
cpr <- get(load("dwca_CPRv1.2_occ+event_zoo_27_04_2021.Rdata"))
dim(cpr) # 712'720
head(cpr)
str(cpr)

unique(cpr$eventDate)

### Convert dynamicProperties or eventDate to Date
#head(lubridate::ymd(cpr$dynamicProperties))
# cpr$Date <- lubridate::ymd(cpr$)
head( do.call(rbind, strsplit(x = unique(cpr$eventDate), split = "T")) ) # 1st col for date, 2nd for local time (GMT?)
unique(do.call(rbind, strsplit(x = unique(cpr$eventDate), split = "T"))[,1])
cpr$Date <- do.call(rbind, strsplit(x = cpr$eventDate, split = "T"))[,1]
#head( lubridate::ymd(cpr$Date) ) # str( lubridate::dmy(m.cpr$Date) ) # Good. Extract Day
cpr$Date <- lubridate::ymd(cpr$Date)
str(cpr$Date)
cpr$Day <- lubridate::day(cpr$Date)
cpr$Month <- lubridate::month(cpr$Date)
cpr$Year <- lubridate::year(cpr$Date) 
head(cpr); summary(cpr) ; summary(cpr$Year)
# Good

### Check OrigScientificName
unique(cpr$scientificName)
# OK

### Reformat to AtlantECO standards
colnames(cpr)
head(cpr)
# unique(cpr$sex) # Always NA
unique(cpr$lifeStage) # What do S114 and S1116 correspond to?
cpr[cpr$lifeStage == "","lifeStage"] <- NA
unique(cpr$occurrenceStatus) # "present" only
unique(cpr$individualCount) # in #/m3? for sure?

cpr2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;David_Johns",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;djoh@MBA.ac.uk", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = cpr$occurrenceID, obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = cpr$decimalLatitude, decimalLongitude = cpr$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = "Comparison between the calculated position and data from vessels where a GPS record was available suggests the position assigned to CPR samples is accurate to within 10–20 nautical miles.",
                CountryCode = NA, eventDate = cpr$Date,
                eventDateInterval = cpr$eventDate, eventDateIntervalUnit = NA, Year = cpr$Year, Month = cpr$Month, Day = cpr$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = 7, DepthAccuracy = "5-10m", DepthIntegral = NA,
                MinDepth = cpr$minimumDepthInMeters, MaxDepth = cpr$maximumDepthInMeters, ParentEventID = "CPR", EventID = cpr$eventID,
                InstitutionCode = "Marine Biological Association of the UK (MBA)", SourceArchive = "https://www.dassh.ac.uk/ipt/resource?r=cpr_public",
                OrigCollectionCode = "Not_applicable", OrigCollectionID = "Not_applicable",
                BiblioCitation = "Johns D, Broughton D (2019): The CPR Survey. v1.2. Marine Biological Association. Dataset/Samplingevent. https://doi.org/10.17031/1629",
                CitationDOI = "https://doi.org/10.17031/1629",
                DateDataAccess = "27-04-2021",
                OrigScientificName = cpr$scientificName, ScientificName = cpr$scientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = NA, Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = cpr$lifeStage, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = cpr$individualCount,
                MeasurementUnit = "#/m3", MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = "PreservedSpecimen from CPR survey - Methods described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011)",
                SamplingProtocol = "Described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011)",
                SampleAmount = "The volume of water filtered for each 10 nautical mile sample is about 3m3 (mean = 3.27 m3, SD = 0.71 m3, n = 1723, Jonas et al., 2004).",
                SampleAmountUnit = "nautical miles and m3", SampleEffort = "volume of water filtered for each 10 nautical mile sample is about 3m3",
                DeterminedBy = "SAHFOS",
                DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
head(cpr2)
dim(cpr2) # 712'720   70
str(cpr2)

# Check: 
library("worms")
#keys.worms <- wormsbynames( unique(cpr2$ScientificName) )
# Looks good, just have to remove the spp.
# str_replace_all(unique(cpr2$ScientificName), " spp.", "") 
cpr2$ScientificName <- str_replace_all(cpr2$ScientificName, " spp.", "") 
keys.worms <- wormsbynames( unique(cpr2$ScientificName) )
keys.worms$ScientificName <- unique(cpr2$ScientificName)

# Add WoRMS_status field
cpr2 <-  add_column(cpr2, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(cpr2)

# For testing the functions below:
s <- unique(cpr2$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(cpr2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- cpr2[cpr2$ScientificName == s,]
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
dim(ddf) # 712'720, same as before. Check some random lines.
rm(res); gc()
ddf[45000:45020,]
unique(ddf$Species) # Good. 
ddf[is.na(ddf$Species),][1:50,]
unique(ddf$WoRMS_ID) # No NA
unique(ddf$WoRMS_status) # good

### Counts were ACTUALLY in 3/m3, not #/m3 (pers. comm. David Johns). Need to divide by 3
# colnames(ddf)
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


ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata")
write.table(ddf, file = "CPR_zooplankton_reformatted+WoRMScheck_29_10_21.txt", sep = "\t")

### 30/11/21: Following David Johns' email, correct the unknown LifeForm codes based on the BODC parameter semantics 
# https://vocab.seadatanet.org/v_bodc_vocab_v2/browse.asp?order=conceptid&formname=search&screen=0&lib=s11&v0_0=S114&v1_0=conceptid%2Cpreflabel%2Caltlabel%2Cdefinition%2Cmodified&v2_0=0&v0_1=&v1_1=conceptid&v2_1=3&v0_2=&v1_2=preflabel&v2_2=3&v0_3=&v1_3=altlabel&v2_3=3&v0_4=&v1_4=modified&v2_4=9&v0_5=&v1_5=modified&v2_5=10&x=34&y=24&v1_6=&v2_6=&v1_7=&v2_7 

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2")
dir()

cpr <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
dim(cpr)
unique(cpr$LifeForm)  # str(cpr$LifeForm)
# - S114 == copepodites C1-C4
# - S1116 == adult
cpr[cpr$LifeForm == "S114" & !is.na(cpr$LifeForm),"LifeForm"] <- "copepodites C1-C4"
cpr[cpr$LifeForm == "S1116" & !is.na(cpr$LifeForm),"LifeForm"] <- "adult"
# Save again 
save(cpr, file = "CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata")
write.table(cpr, file = "CPR_zooplankton_reformatted+WoRMScheck_29_10_21.txt", sep = "\t")

### No need for phytoplankton though

### ----------------------------------------------------------------------------------------------------------------------------

### 26/10/2021: Same as above, but for phytoplankton taxa
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2")
dir()

# Load the phyto data frame
cpr <- get(load("dwca_CPRv1.2_occ+event_phyto_27_04_2021.Rdata"))
dim(cpr) # 755'168 obs
head(cpr)
str(cpr)

### Convert dynamicProperties or eventDate to Date
# head( do.call(rbind, strsplit(x = unique(cpr$dynamicProperties), split = " ")) ) # 1st col for date, 2nd for local time (GMT?)
# unique(do.call(rbind, strsplit(x = unique(cpr$dynamicProperties), split = " "))[,1])
cpr$Date <- do.call(rbind, strsplit(x = cpr$eventDate, split = "T"))[,1]
#head( lubridate::ymd(cpr$Date) ) # str( lubridate::dmy(m.cpr$Date) ) # Good. Extract Day
cpr$Date <- lubridate::ymd(cpr$Date)
str(cpr$Date)
cpr$Day <- lubridate::day(cpr$Date)
cpr$Month <- lubridate::month(cpr$Date)
cpr$Year <- lubridate::year(cpr$Date) 
head(cpr); summary(cpr) ; summary(cpr$Year)
# Good

### Check OrigScientificName
unique(cpr$scientificName)
# Looks OK

### Reformat to AtlantECO standards
colnames(cpr)
head(cpr)
# unique(cpr$sex) # Always NA
unique(cpr$lifeStage) # NA only
cpr[cpr$lifeStage == "","lifeStage"] <- NA
unique(cpr$occurrenceStatus) # "present" only

cpr2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;David_Johns",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;djoh@MBA.ac.uk", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = cpr$occurrenceID, obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = cpr$decimalLatitude, decimalLongitude = cpr$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = "Comparison between the calculated position and data from vessels where a GPS record was available suggests the position assigned to CPR samples is accurate to within 10–20 nautical miles.",
                CountryCode = NA, eventDate = cpr$Date,
                eventDateInterval = cpr$eventDate, eventDateIntervalUnit = NA, Year = cpr$Year, Month = cpr$Month, Day = cpr$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = 7, DepthAccuracy = "5-10m", DepthIntegral = NA,
                MinDepth = cpr$minimumDepthInMeters, MaxDepth = cpr$maximumDepthInMeters, ParentEventID = "CPR", EventID = cpr$eventID,
                InstitutionCode = "Marine Biological Association of the UK (MBA)", SourceArchive = "https://www.dassh.ac.uk/ipt/resource?r=cpr_public",
                OrigCollectionCode = "Not_applicable", OrigCollectionID = "Not_applicable",
                BiblioCitation = "Johns D, Broughton D (2019): The CPR Survey. v1.2. Marine Biological Association. Dataset/Samplingevent. https://doi.org/10.17031/1629",
                CitationDOI = "https://doi.org/10.17031/1629",
                DateDataAccess = "27-04-2021",
                OrigScientificName = cpr$scientificName, ScientificName = cpr$scientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = NA, Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = cpr$lifeStage, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = cpr$individualCount,
                MeasurementUnit = "#/m3", MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = "PreservedSpecimen from CPR survey - Methods described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011)",
                SamplingProtocol = "Described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011)",
                SampleAmount = "The volume of water filtered for each 10 nautical mile sample is about 3m3 (mean = 3.27 m3, SD = 0.71 m3, n = 1723, Jonas et al., 2004).",
                SampleAmountUnit = "nautical miles and m3", SampleEffort = "volume of water filtered for each 10 nautical mile sample is about 3m3",
                DeterminedBy = "SAHFOS",
                DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
head(cpr2)
dim(cpr2) # 755'168   70
str(cpr2)

### Check taxonomy
keys.worms <- wormsbynames( unique(cpr2$ScientificName) )
# Looks good, just have to remove the spp. and correct 'Chaetoceros(Hyalochaete)' and 'Nitzschia sigma rigida'
# str_replace_all(unique(cpr2$ScientificName), " spp.", "") 
### Nitzschia sigma rigida is now represented as Amphipleura rigida, though still unclear (https://www.marinespecies.org/aphia.php?p=taxdetails&id=611547)
cpr2$ScientificName <- str_replace_all(cpr2$ScientificName, " spp.", "") 
cpr2[cpr2$ScientificName == "Chaetoceros(Hyalochaete)","ScientificName"] <- "Chaetoceros (Hyalochaete)"
cpr2[cpr2$ScientificName == "Ceratium inflatum","ScientificName"] <- "Tripos inflatus"

cpr2$ScientificName <- str_replace_all(cpr2$ScientificName, "Nitzschia sigma rigida", "Amphipleura rigida") 
unique(cpr2$ScientificName)
# Check again
keys.worms <- wormsbynames( unique(cpr2$ScientificName) )
# If OK
keys.worms$ScientificName <- unique(cpr2$ScientificName)

# Add WoRMS_status field
cpr2 <-  add_column(cpr2, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(cpr2)

# For testing the functions below:
s <- unique(cpr2$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(cpr2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- cpr2[cpr2$ScientificName == s,]
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

        }, mc.cores = 15 # >2 when on kryo

) # eo mclapply - s in taxa_names

### Rbind
ddf <- bind_rows(res)
dim(ddf) # 755'168, same as before. Check some random lines.
rm(res); gc()
ddf[45000:45020,]
unique(ddf$Species) # Good. 
ddf[is.na(ddf$Species),][1:20,]
unique(ddf$WoRMS_ID) # Some missing values still
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS","ScientificName"]) # OK, unassessed species
unique(ddf$WoRMS_status) # good

### Counts were ACTUALLY in 3/m3, not #/m3 (pers. comm. David Johns). Need to divide by 3
summary(ddf$MeasurementValue) ; unique(ddf$MeasurementValue)
ddf$MeasurementValue <- (ddf$MeasurementValue)/3
# summary(ddf$MeasurementValue)
# unique(ddf$MeasurementValue)


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

ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "CPR_phytoplankton_reformatted+WoRMScheck_29_10_21.Rdata")
write.table(ddf, file = "CPR_phytoplankton_reformatted+WoRMScheck_29_10_21.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
