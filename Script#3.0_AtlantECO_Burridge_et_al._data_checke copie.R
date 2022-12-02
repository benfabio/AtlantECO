
##### ATLANTECO SCRIPT 3.0 ----------------------------------------------------------------------------------------------------------------------------
##### 18/10/2021: R Script to check the data of Burridge et al. (2016) sent by Katja Peijnenburg () © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### All data and material are available on: https://data.aad.gov.au/aadc/cpr/index.cfm 

### Aims to:
# - Read the file prepared by Nielja 
# - Correct some headers, if needed, and perform the taxonomy checking using 'worms'

### Latest update: 18/10/2021

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("readxl")
library("lubridate")

world <- map_data("world")  # for maps

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AMT") # working dir
dir()

### ------------------------------------------------------------------------------------------------------------------------------------------------------

### 1°) Read the data: Burridge_etal2016_PteropodsHeteropods_TableS1_AtlantECO_nknecht.csv
data <- read.csv("Burridge_etal2016_PteropodsHeteropods_TableS1_AtlantECO_nknecht.csv", h = T)
dim(data) # 1953   71
# Remove the X column
data <- data[,c(2:71)]
str(data)
colnames(data)
### Check coordinates and dates etc.
summary(data[,c(11,12,16:21,26:30)])
# Gut, gut

# Check some headers individually
unique(data$SamplingProtocol) # Gut
unique(data$InstitutionCode) 
data$InstitutionCode <- "Naturalis Biodiversity Center"
data$SourceArchive <- "Not_applicable"
data$OrigCollectionCode <- "Not_applicable"
data$OrigCollectionID <- "Not_applicable"

# Check the taxonomic classif
data[1:10,c(40:52)] # data[1000:1500,c(40:52)]
unique(data$ScientificName) # To correct a bit
# "Cuvierina sp" 
# "Creseis "
# "Corolla sp"
data[data$ScientificName == "Cuvierina sp","ScientificName"] <- "Cuvierina"
data[data$ScientificName == "Creseis ","ScientificName"] <- "Creseis"
data[data$ScientificName == "Corolla sp","ScientificName"] <- "Corolla"

### Check the taxonomic classification
require("worms") 
keys.worms <- wormsbynames( unique(data$ScientificName) )
# All names check out.
keys.worms$ScientificName <- unique(data$ScientificName)

# Add WoRMS_status field
data <-  add_column(data, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(data)

# For testing the functions below:
s <- unique(data$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(data$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- data[data$ScientificName == s,]
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
dim(ddf) # 1'953 rows
ddf[1:50,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf[is.na(ddf$Species),][1:10,]
unique(ddf$WoRMS_ID) # All filled, nice
unique(ddf$WoRMS_status) # good

# Check OrigScientificName and LifeForm
unique(ddf$OrigScientificName)
unique(ddf$LifeForm)

# Quick map
summary(ddf)
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 0 & world$long >= -50 & world$lat <= 50 & world$lat >= -50,],
        fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = ddf) + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'
# Save
save(ddf, file = "AMT_24_Burridge2016_PteropodsHeteropods_18_10_21.Rdata")
write.table(ddf, file = "AMT_24_Burridge2016_PteropodsHeteropods_18_10_21.txt", sep = "\t")


### ------------------------------------------------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------------------------------------------------
