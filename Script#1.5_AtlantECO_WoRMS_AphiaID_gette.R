
##### ATLANTECO SCRIPT 1.5 ----------------------------------------------------------------------------------------------------------------------------
##### 06/05/2021: R Script to retrieve WORMS AphiaID code for any biological occurrence/abundance observation © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### For each dataset with taxon-level observations (occurrences/ abund/ biomass whatever):
#	- extract the vector containing the scientific name associated with the observation/measurement
#   - clean it if necessary (remove underscore...retain genus name only etc.)
#   - use functions from R package 'worms' to retrieve WoRMS' accepted species name and AphiaID

# module load R/4.0.3 # To load latest R version on kryo

### Latest update: 02/07/2021

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("worms")

# If not already there:
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional")
WD <- getwd()

### ----------------------------------------------------------------------------------------------------------------------------

### List the datasets on which to apply the function you'll define to retrive the alphaID from worms
# x MALASPINA (Villarino species counts) - DONE !!! (01/06/21)
# x ZOObase - DONE !!! (04/06/21), including: OBIS+GBIF+Cornils et al. 2018
# x PhytoBase - DONE !!! (08/06/21)
# x COPEPOD - zoo - DONE !!! (01/06/21)
# x COPEPOD - phyto - DONE !!! (01/06/21)
# x JeDi (occurrences and integrated abundances) - DONE !!! (22/06/21)

# - AMT? (when available)
# - CPR? (when absences made available)
# - TARA (microscopy and/or metagenomics?) when available - not before 2022

#install.packages("worms")
library("worms")
# ?worms
# The worms package provides two kinds of functions:
# a) retrieving taxonomic information using WoRMS' RESTful
#   Webservice by using taxon name search, fuzzy matching, or Aphia ID
#   search implementing methods documented at <URL: http://www.marinespecies.org/rest/>

# b) functions that parse the data for synonyms in order to complete
#   the dataset so that for every taxon in the dataset the respective
#   taxon with status 'accepted' exists as well. Constructed
#   references to the respective taxon with status 'accepted' help
#   aggregating biodiversity data without the use of synonyms,
#   alternative representations, and common misspellings leading to errors.

?wormsaccepted
?wormsbymatchnames
?wormsbynames
# Match: taxon_names that could not retrieved will be retried with ‘wormsbymatchnames’. Implies "id=TRUE"
?wormsconsolidate

# Run some examples
## start with IDs that are no longer up to date 
# get the Aphia information
u <- wormsbyid(c(410749))
dim(u) ; class(u) # data.frame with 1 row and:
str(u)

# Recursively retrieve information on the taxa they refer to
v <- wormsconsolidate(u)
dim(v); class(v)
str(v)

# what are the currently correct "accepted" taxa? Answer: "accepted_id".
w <- wormsaccepted(v)
w

# test wormsbynames()
taxon_names <- c("Westwodilla caecula","Abra alba","Chaetozone cf. setosa","Algae",
                "Calanus_helgolandicus","Calanus helgolandicus","Calanus_sp.","Calanus sp.", "Calanus")
w <- wormsbynames(taxon_names)
w
## print unrecognized returns
failed_species <- rownames(w[is.na(w[,1]),])

## try again with fuzzy matching turned on
w <- wormsbynames(taxon_names, match = TRUE)

### OK, seems like we must use wormsbynames()


### ----------------------------------------------------------------------------------------------------------------------------

### 1°) First dataset (smallest): MALASPINA species data from Villarino et al. (2017)
setwd(paste(WD,"/","MALASPINA_Villarino_2017", sep = ""))
files <- dir()[grep("reformated_29_04_2021",dir())]
f <- files[1]
test <- get(load(f))
#dim(test)
#str(test)

### So, goal is to fill WoRMS_ID column based on 'ScientificName'
# Test first: 
unique(test$ScientificName) ; str(test$ScientificName) # has to be chr string
taxon_names <- as.character(unique(test$ScientificName))
taxon_names <- str_replace_all(taxon_names, " spp.", "")
taxon_names <- str_replace_all(taxon_names, " sp.", "")
taxon_names <- str_replace_all("\\.", "", taxon_names)
# Remove trailing white spaces with trimws() ?base::trimws
taxon_names <- base::trimws(x = taxon_names, which = "right")
taxon_names

w <- wormsbynames(taxon_names)
# dim(w)
# str(w)
#colnames(w)
#which(is.na())
ind.failed <- which(is.na(w[,1]))
taxon_names[ind.failed]

consol.w <- wormsconsolidate(w)
consol.w[consol.w$status == "unaccepted",]

w$ScientificName <- taxon_names

### Columns to retrieve from 'consol.w' and to add into the data table
colnames(w)
head(w)
# lsid
# valid_name
# [13] "kingdom"           "phylum"            "class"            
# [16] "order"             "family"            "genus"
# And maybe: "isMarine","isBrackish","isFreshwater","isTerrestrial","isExtinct"
# Maybe add a column that says: isMarine and if not 'yes' return isBrackish/isFreshwater/isExtinct/isTerrestrial
# And for complete NA (e.g. Ihela racoviatzi), return 'no match in WoRMS' in 'WoRMS_ID'



### Now, find the bets strategy to add these fields to the 'reformated_29_04_2021' R files most efficiently
# Re-load test data again 
f <- files[1]
test <- get(load(f))
#head(test)
#colnames(w)
#colnames(test) # Fill cols 40:49
# WoRMS_ID --> valid_AphiaID
# TaxonRank --> rank
# 42:47 --> higher classif (Kingdom to Genus)
# Species --> valid_name IF TaxonRank == species

# s <- unique(test$ScientificName)[1] # For testing mclapply below
taxa_names <- as.character(test$ScientificName)
taxa_names <- str_replace_all(taxa_names, " spp.", "")
taxa_names <- str_replace_all(taxa_names, " sp.", "")
taxa_names <- str_replace_all("\\.", "", taxa_names)
taxa_names <- base::trimws(x = taxa_names, which = "right")
unique(taxa_names)

test$ScientificName <- taxa_names

#w <- wormsbynames(taxa_names)
#ind.failed <- which(is.na(w[,1]))
#taxa_names[ind.failed]
#consol.w <- wormsconsolidate(w)
#consol.w[consol.w$status == "unaccepted",]
#w$ScientificName <- taxa_names

test2 <- add_column(test, WoRMS_status = NA, .after = "WoRMS_ID")

### And now use mclapply() to provide classif etc. to 'test2'
# For testing;
s <- unique(taxa_names)[1] # example when species name exists
s <- unique(taxa_names)[17] # example when species name does not exist

keys.worms <- wormsbynames(unique(taxa_names))
keys.worms$ScientificName <- unique(taxa_names)
# str(keys.worms)

res <- mclapply(unique(taxa_names), function(s) {
    
            # Message
            message(paste(s, sep = ""))
            subset <- test2[test2$ScientificName == s,] # dim(subset)
            
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
                
                subset$WoRMS_ID <- factor("No match found in WoRMS")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- NA
                
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
            
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                 
                # w[w$ScientificName == s,]
                statuses <- melt( keys.worms[keys.worms$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                
                # virtual statuses case study
                # virtual.statuses <- data.frame(variable = c("isMarine","isBrackish","isFreshwater","isTerrestrial"), value = c(1,0,1,NA))
                # paste(virtual.statuses[virtual.statuses$value == 1 ,'variable'], collapse = "+")
                
                subset$WoRMS_status <- factor(status2add)
                
            } # eo 3rd for loop - for WoRMS_status
            
            # head(subset)
            return(subset)
    
        }, mc.cores = 20
 
) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
dim(ddf)
summary(ddf) # Nice
ddf[4000:4100,]

### ----------------------------------------------------------------------------------------------------------------------------

### 01/06/2021: Good, now that you've figured out how to do this, apply to all MALASPINA datasets

### -----------------------------------------------------------

### First, need to check if current species labels are fit to be used 
setwd(paste(WD,"/","MALASPINA_Villarino_2017", sep = ""))
files <- dir()[grep("reformated_29_04_2021",dir())]

# Coccos
f <- files[1] # for testing
data <- get(load(f))
unique(data$ScientificName) # fine

# Diatoms
f <- files[2] # for testing
data <- get(load(f))
unique(data$ScientificName)
# Need to curate labels!
# "Chaetoceros sp. (solitary)"
data[data$ScientificName == "Chaetoceros sp. (solitary)","Note"] <- "Solitary Chaetoceros cells"
data[data$ScientificName == "Chaetoceros sp. (solitary)","ScientificName"] <- "Chaetoceros sp."
# "Chaetoceros spp. (>20micron)"
data[data$ScientificName == "Chaetoceros spp. (>20micron)","Note"] <- ">20micron Chaetoceros cells"
data[data$ScientificName == "Chaetoceros spp. (>20micron)","ScientificName"] <- "Chaetoceros sp."

# "Chaetoceros spp. (<20micron)"
data[data$ScientificName == "Chaetoceros spp. (<20micron)","Note"] <- "<20micron Chaetoceros cells"
data[data$ScientificName == "Chaetoceros spp. (<20micron)","ScientificName"] <- "Chaetoceros spp."

# "Chaetoceros spp. (spores)"
data[data$ScientificName == "Chaetoceros spp. (spores)","Note"] <- "Chaetoceros spores"
data[data$ScientificName == "Chaetoceros spp. (spores)","ScientificName"] <- "Chaetoceros spp."

# "Rhizosolenia sp. (styliformis type)"
data[data$ScientificName == "Rhizosolenia sp. (styliformis type)","Note"] <- "Rhizosolenia styliformis type"
data[data$ScientificName == "Rhizosolenia sp. (styliformis type)","ScientificName"] <- "Rhizosolenia sp."

unique(data$ScientificName) ; unique(data$Note)
save(data, file = "MALASPINA_Villarinoetal._2017_diatoms_reformated_01_06_2021.Rdata")


# Dinoflagellates
f <- files[3] # for testing
data <- get(load(f))
unique(data$ScientificName)
# Need to curate labels. Is the 'cf.' handled by worms?
# wormsbynames("Prorocentrum cf. donghaiense") ; wormsbynames("Prorocentrum donghaiense")
### --> NO.
data$ScientificName <- str_replace_all(string = data$ScientificName, pattern = c(" cf. "), replacement = " ")
unique(data$ScientificName)

# "Dinophysis sp..1"      "Dinophysis sp..2"         "Dinophysis sp..3"
data$ScientificName <- str_replace_all(string = data$ScientificName, pattern = c("sp..1"), replacement = "sp.")
data$ScientificName <- str_replace_all(string = data$ScientificName, pattern = c("sp..2"), replacement = "sp.")
data$ScientificName <- str_replace_all(string = data$ScientificName, pattern = c("sp..3"), replacement = "sp.")
data$ScientificName <- str_replace_all(string = data$ScientificName, pattern = c("spp..1"), replacement = "spp.")
unique(data$ScientificName)

# "Gymnodinium spp. (>40micron)"  "Gymnodinium spp. (<40micron)"
data[data$ScientificName == "Gymnodinium spp. (>40micron)","Note"] <- ">40micron Gymnodinium"
data[data$ScientificName == "Gymnodinium spp. (>40micron)","ScientificName"] <- "Gymnodinium spp."
data[data$ScientificName == "Gymnodinium spp. (<40micron)","Note"] <- "<40micron Gymnodinium"
data[data$ScientificName == "Gymnodinium spp. (<40micron)","ScientificName"] <- "Gymnodinium spp."
unique(data$ScientificName)
# Save
save(data, file = "MALASPINA_Villarinoetal._2017_dinoflagellates_reformated_01_06_2021.Rdata")


# Gelatinous_zooplankton
f <- files[4] # for testing
data <- get(load(f))
unique(data$ScientificName) # fine


### -----------------------------------------------------------

setwd(paste(WD,"/","MALASPINA_Villarino_2017", sep = ""))
files <- dir()[grep("reformated",dir())]
files <- files[c(1,2,4,6)] ; files

f <- files[2] # for testing
data <- get(load(f))
unique(data$ScientificName)

for(f in files) {
    
    message(paste(f, " -------------------------------------------------------------------------------- ", sep = ""))
    
    data <- get(load(f))
    
    taxa_names <- as.character(data$ScientificName)
    # unique(taxa_names)
    
    ### detect presence or absence of certain characters in string (e.g. 'sp.' or 'spp.')
    if( TRUE %in% unique(str_detect(string = taxa_names, pattern = " spp.")) ) {
        taxa_names <- str_replace_all(taxa_names, " spp\\.", "")
    } 
    
    if( TRUE %in% unique( str_detect(string = taxa_names, pattern = " sp.") ) ) {
        taxa_names <- str_replace_all(taxa_names, " sp\\.", "")
    }
    
    # unique(taxa_names)
    if( TRUE %in% unique(str_detect(string = taxa_names, pattern = "\\.")) ) {
        taxa_names <- str_replace_all("\\.", "", taxa_names)
    }
    # taxa_names <- str_replace_all("\\.", "", taxa_names)
    taxa_names <- base::trimws(x = taxa_names, which = "right")
    # unique(taxa_names)

    data$ScientificName <- taxa_names

    data2 <- add_column(data, WoRMS_status = NA, .after = "WoRMS_ID")

    ### And now use mclapply() to provide classif etc. to 'test2'
    keys.worms <- wormsbynames(unique(taxa_names))
    keys.worms$ScientificName <- unique(taxa_names)
    
    ### Fill in with mclapply
    res <- mclapply(unique(taxa_names), function(s) {
    
                # Message
                message(paste(s, sep = ""))
                subset <- data2[data2$ScientificName == s,] # dim(subset)
            
                ### 1st if else loop to check whether the species is actually found in 'w'
                if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
                
                    subset$WoRMS_ID <- factor("No match found in WoRMS")
                    subset$WoRMS_status <- factor("No match found in WoRMS")
                    subset$TaxonRank <- NA
                
                } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
            
                    subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                 
                    # w[w$ScientificName == s,]
                    statuses <- melt( keys.worms[keys.worms$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                    status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                    subset$WoRMS_status <- factor(status2add)
                
                } # eo 3rd for loop - for WoRMS_status
            
                # head(subset)
                return(subset)
    
            }, mc.cores = 20
 
    ) # eo mclapply - s in taxa_names
    # Rbind
    ddf <- bind_rows(res)
    # dim(ddf)
    # ddf[550:610,]
    
    ### saving new 'f'
    save(ddf, file = str_replace_all(f,"reformated","reformated+WoRMScheck") )
    
    message(paste("  ", sep = ""))
    message(paste(" -------------------------------------------------------------------------------- ", sep = ""))
    message(paste("  ", sep = ""))
    
} # eo main for loop - f in files


### ----------------------------------------------------------------------------------------------------------------------------

### 01/06/2021: Same as above but for COPEPOD data
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA")
files <- dir()[grep("05_2021",dir())]

### 1°) PHYTO + MICROZOO
copepod.noaa.phyto <- read.table(files[1], h = TRUE)
dim(copepod.noaa.phyto) # 193,696     39
str(copepod.noaa.phyto)
unique(copepod.noaa.phyto$OrigScientificName)

### Add 'ScientificName' which would a cleanr version of 'OrigScientificName', and that is compatible with the worms::wormsbynames() FUNCTION
# Replace underscores by spaces
copepod.noaa.phyto$ScientificName <- str_replace_all(string = copepod.noaa.phyto$OrigScientificName, pattern = "_", replacement = " ")
#unique(copepod.noaa.phyto$ScientificName)

# Remove the 'v' (e.g. 'Rhizosolenia styliformis v longissima') or the 'var.' (Amphiprora kjellmanii var. kjellmanii)
copepod.noaa.phyto$ScientificName <- str_replace_all(string = copepod.noaa.phyto$ScientificName, pattern = " v ", replacement = " ")

# Correct Amphiprora kjellmanii var. kjellmanii
copepod.noaa.phyto[copepod.noaa.phyto$ScientificName == "Amphiprora kjellmanii var. kjellmanii","ScientificName"] <- "Amphiprora kjellmanii"
copepod.noaa.phyto[copepod.noaa.phyto$ScientificName == "Calciosolenia granii var. cylindrotheciformis","ScientificName"] <- "Calciosolenia granii"
copepod.noaa.phyto[copepod.noaa.phyto$ScientificName == "Hemiella or heimiella excentrica","ScientificName"] <- "Heimiella"

# Remove spp. and sp. and right empty spaces
copepod.noaa.phyto$ScientificName <- str_replace_all(string = copepod.noaa.phyto$ScientificName, pattern = " spp\\.", replacement = " ")
copepod.noaa.phyto$ScientificName <- str_replace_all(string = copepod.noaa.phyto$ScientificName, pattern = " sp\\.", replacement = " ")
copepod.noaa.phyto$ScientificName <- base::trimws(x = copepod.noaa.phyto$ScientificName, which = "right")
unique(copepod.noaa.phyto$ScientificName)

# Test "Chaetoceros (Hyalochaete)" & "Phaeoceros"
# wormsbynames("Chaetoceros (Phaeoceros)")
# Fine, both work
# wormsbynames("Thalassiothrix longissima antarctica")

### Find a way to identify those ScientificName hat have 3 names (subspecies) and keep the first 2 only
# First, need to identify those char strings with >2 words: new col that you'll remove later
copepod.noaa.phyto$CountChar <- sapply(strsplit(copepod.noaa.phyto$ScientificName, " "), length)
# summary(copepod.noaa.phyto$CountChar) # OK
names2change <- unique(copepod.noaa.phyto[copepod.noaa.phyto$CountChar > 2,"ScientificName"])[c(1:10,12,14:53)]
names2change
# n <- names2change[25]
for(n in names2change) {
    message(paste(n, sep = ""))
    new.name <- word(string = n, start = 1, end = 2)
    copepod.noaa.phyto[copepod.noaa.phyto$ScientificName == n,"ScientificName"] <- new.name
} # eo for loop
# Check : unique(copepod.noaa.phyto$ScientificName)
# Looks clean, time to apply that function
# Drop CountChar col
copepod.noaa.phyto <- select(copepod.noaa.phyto, -CountChar)

### Add WoRMS_ID & WoRMS_status
copepod.noaa.phyto[,c("WoRMS_ID","WoRMS_status")] <- NA

### And now use mclapply() to provide classif etc. to 'test2'
keys.worms <- wormsbynames( unique(copepod.noaa.phyto$ScientificName) )
keys.worms$ScientificName <- unique(copepod.noaa.phyto$ScientificName)


### Fill in with mclapply 
# s <- unique(copepod.noaa.phyto$ScientificName)[33] # for testing
s <- "Cyttarocylis eucecryphalus"

res <- mclapply(unique(copepod.noaa.phyto$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- copepod.noaa.phyto[copepod.noaa.phyto$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
             
                # w[w$ScientificName == s,]
                statuses <- melt( keys.worms[keys.worms$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 25

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
# dim(ddf)
#ddf[8696:8896,]
unique(ddf$Species) ; unique(ddf$WoRMS_ID)
# ddf[ddf$Species == "Cyttarocylis ampulla f. eucecryphalus",]

rm(res) ; gc()

### saving new file
save(ddf, file = "COPEPOD_NOAA_all_phytoplankton_microzooplankton_modified+WoRMScheck_01_06_2021.RData")


### ----------------------------------------------------------------------------------------------------------------------------

### 2°) ZOOPLANKTON - part I
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA")
copepod.noaa.zoo1 <- read.table("COPEPOD_NOAA_all_zooplankton_modified_04_05_2021_part1.txt", h = T, sep = "\t")
# dim(copepod.noaa.zoo1) # 167'705     39
#str(copepod.noaa.zoo1)
#unique(copepod.noaa.zoo1$OrigScientificName)
summary(factor(copepod.noaa.zoo1$OrigScientificName)) # 128 NAs to drop

copepod.noaa.zoo1 <- copepod.noaa.zoo1 %>% drop_na(OrigScientificName)

### Add 'ScientificName' which would a cleanr version of 'OrigScientificName', and that is compatible with the worms::wormsbynames() FUNCTION
# Replace underscores by spaces
copepod.noaa.zoo1$ScientificName <- str_replace_all(string = copepod.noaa.zoo1$OrigScientificName, pattern = "_", replacement = " ")
unique(copepod.noaa.zoo1$ScientificName)

# Remove spp. and sp. and right empty spaces
copepod.noaa.zoo1$ScientificName <- str_replace_all(string = copepod.noaa.zoo1$ScientificName, pattern = " spp\\.", replacement = " ")
copepod.noaa.zoo1$ScientificName <- str_replace_all(string = copepod.noaa.zoo1$ScientificName, pattern = " sp\\.", replacement = " ")
copepod.noaa.zoo1$ScientificName <- base::trimws(x = copepod.noaa.zoo1$ScientificName, which = "right")
unique(copepod.noaa.zoo1$ScientificName)

### Correct the Oikopleura (Vexillaria)
copepod.noaa.zoo1[copepod.noaa.zoo1$ScientificName == "Oikopleura vanhoeffeni","ScientificName"] <- "Oikopleura (Vexillaria) vanhoeffeni"
copepod.noaa.zoo1[copepod.noaa.zoo1$ScientificName == "Oikopleura labradoriensis","ScientificName"] <- "Oikopleura (Vexillaria) labradoriensis"
copepod.noaa.zoo1[copepod.noaa.zoo1$ScientificName == "Oikopleura parva","ScientificName"] <- "Oikopleura (Vexillaria) parva"
# Correct Diaxis
copepod.noaa.zoo1[copepod.noaa.zoo1$ScientificName == "Diaxis","ScientificName"] <- "Diaixis"
# Correct Onychoteuthis borealijaponicus
copepod.noaa.zoo1[copepod.noaa.zoo1$ScientificName == "Onychoteuthis borealijaponicus","ScientificName"] <- "Onychoteuthis borealijaponica"
# Correct Liocranchia reinhardti
copepod.noaa.zoo1[copepod.noaa.zoo1$ScientificName == "Liocranchia reinhardti","ScientificName"] <- "Liocranchia reinhardtii"

### Add WoRMS_ID & WoRMS_status
copepod.noaa.zoo1[,c("WoRMS_ID","WoRMS_status")] <- NA

### And now use mclapply() to provide classif etc. to 'test2'
keys.worms <- wormsbynames( unique(copepod.noaa.zoo1$ScientificName) )
keys.worms$ScientificName <- unique(copepod.noaa.zoo1$ScientificName)

### OK, look clean enough, apply fun to get WoRMS' AphiaIDs 
# s <- unique(copepod.noaa.zoo1$ScientificName)[3] # for testing

res <- mclapply(unique(copepod.noaa.zoo1$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- copepod.noaa.zoo1[copepod.noaa.zoo1$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 25

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
# dim(ddf)
# ddf[77577:77777,]
#unique(ddf$Species) ; unique(ddf$WoRMS_ID)
# ddf[ddf$Species == "Cyttarocylis ampulla f. eucecryphalus",]
rm(res) ; gc()

### saving new file
save(ddf, file = "COPEPOD_NOAA_all_zooplankton_modified+WoRMScheck_part1_01_06_2021.RData")
gc()


### ----------------------------------------------------------------------------------------------------------------------------

### 01/06/2021: Zooplankton - part II ! 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA") ; dir()
copepod.noaa.zoo2 <- read.table("COPEPOD_NOAA_all_zooplankton_modified_05_05_2021_part2.txt", h = T, sep = "\t")
# dim(copepod.noaa.zoo2) # 56'950    39
# str(copepod.noaa.zoo2)
# unique(copepod.noaa.zoo2$OrigScientificName)
# Just in case: 
copepod.noaa.zoo2 <- copepod.noaa.zoo2 %>% drop_na(OrigScientificName)

### Add 'ScientificName' which would a cleanr version of 'OrigScientificName', and that is compatible with the worms::wormsbynames() FUNCTION
# Replace underscores by spaces
copepod.noaa.zoo2$ScientificName <- str_replace_all(string = copepod.noaa.zoo2$OrigScientificName, pattern = "_", replacement = " ")
# unique(copepod.noaa.zoo2$ScientificName)

# Remove spp. and sp. and right empty spaces
copepod.noaa.zoo2$ScientificName <- str_replace_all(string = copepod.noaa.zoo2$ScientificName, pattern = " spp\\.", replacement = " ")
copepod.noaa.zoo2$ScientificName <- str_replace_all(string = copepod.noaa.zoo2$ScientificName, pattern = " sp\\.", replacement = " ")
copepod.noaa.zoo2$ScientificName <- base::trimws(x = copepod.noaa.zoo2$ScientificName, which = "right")
# unique(copepod.noaa.zoo2$ScientificName)

# Correct manually some names
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Oikopleura vanhoeffeni","ScientificName"] <- "Oikopleura (Vexillaria) vanhoeffeni"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Oikopleura labradoriensis","ScientificName"] <- "Oikopleura (Vexillaria) labradoriensis"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Oikopleura parva","ScientificName"] <- "Oikopleura (Vexillaria) parva"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Pseudocalanus minutus+elongatus","ScientificName"] <- "Pseudocalanus"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Tessarabrachion oculatus","ScientificName"] <- "Tessarabrachion oculatum"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Poebius meseres","ScientificName"] <- "Poeobius meseres"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Euprimno rectimannus","ScientificName"] <- "Primno"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Rhynchonerella angelina","ScientificName"] <- "Rhynchonereella angelini"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Bosmina coregoni","ScientificName"] <- "Bosmina (Eubosmina) coregoni"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Bosmina coregoni coregoni","ScientificName"] <- "Bosmina (Eubosmina) coregoni"
copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == "Eubosmina maritima","ScientificName"] <- "Bosmina (Eubosmina) coregoni"

### Add WoRMS_ID & WoRMS_status
copepod.noaa.zoo2[,c("WoRMS_ID","WoRMS_status")] <- NA

### And now use mclapply() to provide classif etc. to 'test2'
keys.worms <- wormsbynames( unique(copepod.noaa.zoo2$ScientificName) )
keys.worms$ScientificName <- unique(copepod.noaa.zoo2$ScientificName)

### OK, look clean enough, apply fun to get WoRMS' AphiaIDs 
# s <- unique(copepod.noaa.zoo2$ScientificName)[80] # for testing
res <- mclapply(unique(copepod.noaa.zoo2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- copepod.noaa.zoo2[copepod.noaa.zoo2$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 25

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
# dim(ddf)
 ddf[56150:56250,]
#unique(ddf$Species) ; unique(ddf$WoRMS_ID)
# ddf[ddf$Species == "Cyttarocylis ampulla f. eucecryphalus",]
rm(res) ; gc()

### saving new file
save(ddf, file = "COPEPOD_NOAA_all_zooplankton_modified+WoRMScheck_part2_01_06_2021.RData")
gc()


### ----------------------------------------------------------------------------------------------------------------------------

### 04/06/2021: Get WoRMS' AphiaID andclassif for ZOObase reformated datasets (GBIF & OBIS)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021") ; dir()

### A°) OBIS merged
zoobase.obis <- get(load("ZOObase_OBIS_merged_reformated_27_04_2021.Rdata"))
dim(zoobase.obis) # 2'288'615 x 70
colnames(zoobase.obis)
head(zoobase.obis)
unique(zoobase.obis$ScientificName)

### Check if the 'worms' package accepts 'f.' and brackets
wormsbynames("Oikopleura (Vexillaria) villafrancae") # yes
wormsbynames("Tomopteris (Johnstonella) dunckeri") # yes
wormsbynames("Tomopteris krampi") # yes
wormsbynames("Doliolina (Doliolina) undulata") # yes
wormsbynames("Euphilomedes sinister pentathrix") # yes!
wormsbynames("Oncaea media var. major") # NO
wormsbynames("Clio pyramidata f. lanceolata") # yes
wormsbynames("Euaugaptilus palumboii") # yes

### ScientificName that do not work:
# Fritillaria borealis f. typica
# Fritillaria borealis f. sargassi
zoobase.obis[zoobase.obis$ScientificName %in% c("Fritillaria borealis f. typica","Fritillaria borealis f. sargassi"),"ScientificName"] <- "Fritillaria borealis"
# Oikopleura labradoriensis
zoobase.obis[zoobase.obis$ScientificName == "Oikopleura labradoriensis","ScientificName"] <- "Oikopleura (Vexillaria) labradoriensis"
# Oikopleura dioica
zoobase.obis[zoobase.obis$ScientificName == "Oikopleura dioica","ScientificName"] <- "Oikopleura (Vexillaria) dioica"
# Fritillaria borealis f. intermedia
zoobase.obis[zoobase.obis$ScientificName == "Fritillaria borealis f. intermedia","ScientificName"] <- "Fritillaria borealis"
# Calanus finmarchicus glacialis
zoobase.obis[zoobase.obis$ScientificName == "Calanus finmarchicus glacialis","ScientificName"] <- "Calanus finmarchicus"
# Euaugaptilus palumboi
zoobase.obis[zoobase.obis$ScientificName == "Euaugaptilus palumboi","ScientificName"] <- "Euaugaptilus palumboii"
# Oncaea venusta robusta
# Oncaea venusta medium
zoobase.obis[zoobase.obis$ScientificName %in% c("Oncaea venusta robusta","Oncaea venusta medium"),"ScientificName"] <- "Oncaea venusta"
# Sapphirina nigromaculata scarlata
zoobase.obis[zoobase.obis$ScientificName == "Sapphirina nigromaculata scarlata","ScientificName"] <- "Sapphirina nigromaculata"
# Sapphirina opalina darwini
zoobase.obis[zoobase.obis$ScientificName == "Sapphirina opalina darwini","ScientificName"] <- "Sapphirina opalina"
# Beroe cucumis ovata
zoobase.obis[zoobase.obis$ScientificName == "Beroe cucumis ovata","ScientificName"] <- "Beroe cucumis"
# Pleurobrachia pileus bachei
zoobase.obis[zoobase.obis$ScientificName == "Pleurobrachia pileus bachei","ScientificName"] <- "Pleurobrachia pileus"
# Carybdea alata grandis           
zoobase.obis[zoobase.obis$ScientificName == "Carybdea alata grandis","ScientificName"] <- "Carybdea alata"
# Thysanopoda microphthalma acutifrons
zoobase.obis[zoobase.obis$ScientificName == "Thysanopoda microphthalma acutifrons","ScientificName"] <- "Thysanopoda microphthalma"
# Neogloboquadrina pachyderma sinistralis
# Neogloboquadrina pachyderma dextralis
zoobase.obis[zoobase.obis$ScientificName %in% c("Neogloboquadrina pachyderma sinistralis","Neogloboquadrina pachyderma dextralis"),"ScientificName"] <- "Neogloboquadrina pachyderma"
# Streptochilus globigerum
zoobase.obis[zoobase.obis$ScientificName == "Streptochilus globigerum","ScientificName"] <- "Streptochilus globigerus"
# Clione limacina gracilis
zoobase.obis[zoobase.obis$ScientificName == "Clione limacina gracilis","ScientificName"] <- "Clione limacina"
# Agalma okeni
zoobase.obis[zoobase.obis$ScientificName == "Agalma okeni","ScientificName"] <- "Agalma okenii"
# Lensia grimaldi
zoobase.obis[zoobase.obis$ScientificName == "Lensia grimaldi","ScientificName"] <- "Lensia grimaldii"
# Muggiaea kochi
zoobase.obis[zoobase.obis$ScientificName == "Muggiaea kochi","ScientificName"] <- "Muggiaea kochii"
# Ceratocymba leuckarti
zoobase.obis[zoobase.obis$ScientificName == "Agalma okeni","ScientificName"] <- "Agalma okenii"
# Abylopsis eschscholtzi
zoobase.obis[zoobase.obis$ScientificName == "Abylopsis eschscholtzi","ScientificName"] <- "Abylopsis eschscholtzii"
# Lensia pannikari
zoobase.obis[zoobase.obis$ScientificName == "Lensia pannikari","ScientificName"] <- "Lensia panikkari"
# Forskalia edwardsi
zoobase.obis[zoobase.obis$ScientificName == "Forskalia edwardsi","ScientificName"] <- "Forskalia edwardsii"
# Stephanomia amphitrides
zoobase.obis[zoobase.obis$ScientificName == "Stephanomia amphitrides","ScientificName"] <- "Stephanomia amphytridis"
# Crossota brunnea norvegica
zoobase.obis[zoobase.obis$ScientificName == "Crossota brunnea norvegica","ScientificName"] <- "Crossota brunnea"
# Aglantha digitale intermedia
zoobase.obis[zoobase.obis$ScientificName == "Aglantha digitale intermedia","ScientificName"] <- "Aglantha digitale"
# Rhizophysa eysenhardti
zoobase.obis[zoobase.obis$ScientificName == "Rhizophysa eysenhardti","ScientificName"] <- "Rhizophysa eysenhardtii"
# Cordagalma ordinata
zoobase.obis[zoobase.obis$ScientificName == "Cordagalma ordinata","ScientificName"] <- "Cordagalma ordinatum"
# Margelopsis hartlaubi
zoobase.obis[zoobase.obis$ScientificName == "Margelopsis hartlaubi","ScientificName"] <- "Margelopsis hartlaubii"
# Hyperia medusarum histrix
zoobase.obis[zoobase.obis$ScientificName == "Hyperia medusarum histrix","ScientificName"] <- "Hyperia medusarum"
# Hyperoche martinezi
zoobase.obis[zoobase.obis$ScientificName == "Hyperoche martinezi","ScientificName"] <- "Hyperoche martinezii"
# Lanceola clausi pirloti
zoobase.obis[zoobase.obis$ScientificName == "Lanceola clausi pirloti","ScientificName"] <- "Lanceola clausi"
# Vibilia jeangerardi
zoobase.obis[zoobase.obis$ScientificName == "Vibilia jeangerardi","ScientificName"] <- "Vibilia jeangerardii"
# Prolopadorrhynchus henseni
zoobase.obis[zoobase.obis$ScientificName == "Prolopadorrhynchus henseni","ScientificName"] <- "Lopadorrhynchus henseni"
# Siriellinae thompsonii
zoobase.obis[zoobase.obis$ScientificName == "Siriellinae thompsonii","ScientificName"] <- "Siriella thompsonii"
# Amblyops abbreviata
zoobase.obis[zoobase.obis$ScientificName == "Amblyops abbreviata","ScientificName"] <- "Amblyops abbreviatus"
# Heteromysis beetoni
zoobase.obis[zoobase.obis$ScientificName == "Heteromysis beetoni","ScientificName"] <- "Heteromysis (Olivemysis) beetoni"
# Leptomysis truncata sarolica
zoobase.obis[zoobase.obis$ScientificName == "Leptomysis truncata sarolica","ScientificName"] <- "Leptomysis truncata"
# Mysidium gracile
zoobase.obis[zoobase.obis$ScientificName == "Mysidium gracile","ScientificName"] <- "Mysidium (Mysidium) gracile"
# Mysidium columbiae
zoobase.obis[zoobase.obis$ScientificName == "Mysidium columbiae","ScientificName"] <- "Mysidium (Orientomysidium) columbiae"
# Paramysis pontica
zoobase.obis[zoobase.obis$ScientificName == "Paramysis pontica","ScientificName"] <- "Paramysis (Pseudoparamysis) pontica"
# Paramysis kroyeri
zoobase.obis[zoobase.obis$ScientificName == "Paramysis kroyeri","ScientificName"] <- "Paramysis (Longidentia) kroyeri"
# Amblyops tenuicauda
zoobase.obis[zoobase.obis$ScientificName == "Amblyops tenuicauda","ScientificName"] <- "Amblyops tenuicaudus"
# Pleopis polyphaemoides
zoobase.obis[zoobase.obis$ScientificName == "Pleopis polyphaemoides","ScientificName"] <- "Pleopis polyphemoides"
# Sagitta zetesios
zoobase.obis[zoobase.obis$ScientificName == "Sagitta zetesios","ScientificName"] <- "Solidosagitta zetesios"
# Aidanosagitta crassa naikaiensis
zoobase.obis[zoobase.obis$ScientificName == "Aidanosagitta crassa naikaiensis","ScientificName"] <- "Aidanosagitta crassa"
# Cephea cephea coerulea
zoobase.obis[zoobase.obis$ScientificName == "Cephea cephea coerulea","ScientificName"] <- "Cephea cephea"
# Atolla wyvillei alexandri
zoobase.obis[zoobase.obis$ScientificName == "Atolla wyvillei alexandri","ScientificName"] <- "Atolla wyvillei"
# Mastigias papua sibogae
zoobase.obis[zoobase.obis$ScientificName == "Mastigias papua sibogae","ScientificName"] <- "Mastigias papua"
# Lobonema smithi
zoobase.obis[zoobase.obis$ScientificName == "Lobonema smithi","ScientificName"] <- "Lobonema smithii"
# Cyanea capillata fulva
zoobase.obis[zoobase.obis$ScientificName == "Cyanea capillata fulva","ScientificName"] <- "Cyanea capillata"
# Nauphantopsis diomedeae
zoobase.obis[zoobase.obis$ScientificName == "Nauphantopsis diomedeae","ScientificName"] <- "Nauphanthopsis diomedeae"
# Salpa fusiformis aspera
# Salpa fusiformis runcinata
zoobase.obis[zoobase.obis$ScientificName %in% c("Salpa fusiformis aspera","Salpa fusiformis runcinata"),"ScientificName"] <- "Salpa fusiformis"
# Pyrosoma atlanticum giganteum
# Pyrosoma atlanticum elegans
zoobase.obis[zoobase.obis$ScientificName %in% c("Pyrosoma atlanticum elegans","Pyrosoma atlanticum giganteum"),"ScientificName"] <- "Pyrosoma atlanticum"
# Pegea confederata bicaudata
zoobase.obis[zoobase.obis$ScientificName == "Pegea confederata bicaudata","ScientificName"] <- "Pegea confoederata"
# Dolioletta gegenbauri tritonis
zoobase.obis[zoobase.obis$ScientificName == "Dolioletta gegenbauri tritonis","ScientificName"] <- "Dolioletta gegenbauri"
# Cyclosalpa pinnata var. polae                 
zoobase.obis[zoobase.obis$ScientificName == "Cyclosalpa pinnata var. polae","ScientificName"] <- "Cyclosalpa pinnata"
# Oncaea media var. major
zoobase.obis[zoobase.obis$ScientificName == "Oncaea media var. major","ScientificName"] <- "Oncaea media"
# Ceratocymba leuckarti                             
zoobase.obis[zoobase.obis$ScientificName == "Ceratocymba leuckarti","ScientificName"] <- "Ceratocymba leuckartii"     
# Lanceola clausi
zoobase.obis[zoobase.obis$ScientificName == "Lanceola clausi","ScientificName"] <- "Lanceola clausii"     


### Insert the 'WoRMS_status' field after 'WoRMS_ID'
zoobase.obis <-  add_column(zoobase.obis, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(zoobase.obis)

### And now use mclapply() to provide classif etc. to 'test2'
keys.worms <- wormsbynames( unique(zoobase.obis$ScientificName) )
keys.worms$ScientificName <- unique(zoobase.obis$ScientificName)

# OK, let's go then
s <- unique(zoobase.obis$ScientificName)[33] ; s
res <- mclapply(unique(zoobase.obis$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- zoobase.obis[zoobase.obis$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- NA
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 30

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
# dim(ddf) # 2'288'615 same as zoobase obis
 ddf[56150:56250,]
unique(ddf$Species) ; unique(ddf$WoRMS_ID)
rm(res) ; gc()

### saving new file
save(ddf, file = "ZOObase_OBIS_merged_reformated+WoRMScheck_04_06_2021.RData")
gc()


### --------------------------------------------------------

### B°) GBIF merged
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021") ; dir()
zoo.gbif <- get(load("ZOObase_GBIF_merged_reformated_23_04_2021.Rdata"))
dim(zoo.gbif) # 922'964
colnames(zoo.gbif)
# unique(zoo.gbif$ScientificName)

# Some still have '_'
zoo.gbif$ScientificName <- str_replace_all(zoo.gbif$ScientificName, "_", " ")

### Check those balebls that need correcting, like OBIS data above
# keys.worms <- wormsbynames( unique(zoo.gbif$ScientificName) )

# Oikopleura subgenera:
vexis <- c("Oikopleura labradoriensis","Oikopleura albicans","Oikopleura dioica","Oikopleura cophocerca","Oikopleura rufescens",
            "Oikopleura gaussica","Oikopleura vanhoeffeni","Oikopleura caudaornata","Oikopleura inflata","Oikopleura villafrancae")      
coers <- c("Oikopleura fusiformis","Oikopleura longicauda")                    
# use str_replace_all
zoo.gbif[zoo.gbif$ScientificName %in% vexis,"ScientificName"] <- str_replace_all(zoo.gbif[zoo.gbif$ScientificName %in% vexis,"ScientificName"], "Oikopleura ", "Oikopleura (Vexillaria) ")
zoo.gbif[zoo.gbif$ScientificName %in% coers,"ScientificName"] <- str_replace_all(zoo.gbif[zoo.gbif$ScientificName %in% coers,"ScientificName"], "Oikopleura ", "Oikopleura (Coecaria) ")

# Acartia lilljeborgi                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Acartia lilljeborgi","ScientificName"] <- "Acartia (Odontacartia) lilljeborgii"
# Pontostratiotes acanthoferans                        no match
zoo.gbif[zoo.gbif$ScientificName == "Pontostratiotes acanthoferans","ScientificName"] <- "Pontostratiotes acanthoferens"
# Gaetanus microcanthus                                no match
zoo.gbif[zoo.gbif$ScientificName == "Gaetanus microcanthus","ScientificName"] <- "Gaetanus microacanthus"
# Euaugaptilus palumboi                                no match
zoo.gbif[zoo.gbif$ScientificName == "Euaugaptilus palumboi","ScientificName"] <- "Euaugaptilus palumboii"
# Pachyptilus eurygnatha                               no match
zoo.gbif[zoo.gbif$ScientificName == "Pachyptilus eurygnatha","ScientificName"] <- "Pseudhaloptilus eurygnathus"
# Pseudaugaptilus longiremus                           no match
zoo.gbif[zoo.gbif$ScientificName == "Pseudaugaptilus longiremus","ScientificName"] <- "Pseudaugaptilus longiremis"
# Boeckella gracilis                                   no match
zoo.gbif[zoo.gbif$ScientificName == "Boeckella gracilis","ScientificName"] <- "Boeckella gracilis"
# Pseudocalanus moultani                               no match
zoo.gbif[zoo.gbif$ScientificName == "Pseudocalanus moultani","ScientificName"] <- "Pseudocalanus moultoni"
# Halectinosoma chrystalii                             no match
zoo.gbif[zoo.gbif$ScientificName == "Halectinosoma chrystalii","ScientificName"] <- "Halectinosoma chrystali"
# Arenosetella unisetella                              no match
zoo.gbif[zoo.gbif$ScientificName == "Arenosetella unisetella","ScientificName"] <- "Glabrotelson unisetosa"
# Halectinosoma sarsi                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Halectinosoma sarsi","ScientificName"] <- "Halectinosoma sarsii"
# Haloschizopera bulbifera                             no match
zoo.gbif[zoo.gbif$ScientificName == "Haloschizopera bulbifera","ScientificName"] <- "Haloschizopera bulbifer"
# Heterocope appendiculata                             no match
zoo.gbif[zoo.gbif$ScientificName == "Heterocope appendiculata","ScientificName"] <- "Heterocope appendiculata"
# Heterocope saliens                                   no match
zoo.gbif[zoo.gbif$ScientificName == "Heterocope saliens","ScientificName"] <- "Heterocope saliens" 
# Euphausia krohni                                     no match
zoo.gbif[zoo.gbif$ScientificName == "Euphausia krohni","ScientificName"] <- "Euphausia krohnii"
# Globigerina trilobata                                no match
zoo.gbif[zoo.gbif$ScientificName == "Globigerina trilobata","ScientificName"] <- "Trilobatus trilobus"
# Globigerina decoraperta                              no match
zoo.gbif[zoo.gbif$ScientificName == "Globigerina decoraperta","ScientificName"] <- "Globoturborotalita decoraperta"
# Globigerinoides immaturus                            no match
zoo.gbif[zoo.gbif$ScientificName == "Globigerinoides immaturus","ScientificName"] <- "Trilobatus immaturus"
# Globigerinoidesella fistulosus                       no match
zoo.gbif[zoo.gbif$ScientificName == "Globigerinoidesella fistulosus","ScientificName"] <- "Globigerinoidesella fistulosa"
# Errinia lowei                                        no match
zoo.gbif[zoo.gbif$ScientificName == "Errinia lowei","ScientificName"] <- "Inferiolabiata lowei"
# Parerenna emilya                                     no match
zoo.gbif[zoo.gbif$ScientificName == "Parerenna emilya","ScientificName"] <- "Parerenna emilyae"
# Vampyrcrossota childressi                            no match
zoo.gbif[zoo.gbif$ScientificName == "Vampyrcrossota childressi","ScientificName"] <- "Vampyrocrossota childressi"
# Errinia fascicularis                                 no match
zoo.gbif[zoo.gbif$ScientificName == "Errinia fascicularis","ScientificName"] <- "Lepidotheca fascicularis"
# Errinia cheilopora                                   no match
zoo.gbif[zoo.gbif$ScientificName == "Errinia cheilopora","ScientificName"] <- "Errina cheilopora"
# Plaeophysa agassizi                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Plaeophysa agassizi","ScientificName"] <- "Athorybia rosacea"
# Errinia boschmai                                     no match
zoo.gbif[zoo.gbif$ScientificName == "Errinia boschmai","ScientificName"] <- "Errina boschmai"
# Chuniphyes moseri                                    no match
zoo.gbif[zoo.gbif$ScientificName == "Chuniphyes moseri","ScientificName"] <- "Chuniphyes moserae"
# Craseoa latitheca                                    no match
zoo.gbif[zoo.gbif$ScientificName == "Craseoa latitheca","ScientificName"] <- "Craseoa lathetica"
# Hyperoche martinezi                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Hyperoche martinezi","ScientificName"] <- "Hyperoche martinezii"
# Lanceola clausi                                      no match
zoo.gbif[zoo.gbif$ScientificName == "Lanceola clausi","ScientificName"] <- "Lanceola clausii"
# Scina damasii                                        no match
zoo.gbif[zoo.gbif$ScientificName == "Scina damasii","ScientificName"] <- "Scina damasi"
# Tullbergella cuspidata                               no match
zoo.gbif[zoo.gbif$ScientificName == "Tullbergella cuspidata","ScientificName"] <- "Tullbergella cuspidatus"
# Cycloleberis christei                                no match
zoo.gbif[zoo.gbif$ScientificName == "Cycloleberis christei","ScientificName"] <- "Cycloleberis christiei"
# Vargula bex                                          no match
zoo.gbif[zoo.gbif$ScientificName == "Vargula bex","ScientificName"] <- "Vargula hex"
# Anisomysis laticauda                                 no match
zoo.gbif[zoo.gbif$ScientificName == "Anisomysis laticauda","ScientificName"] <- "Anisomysis (Anisomysis) laticauda"
# Heteromysis formosa                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis formosa","ScientificName"] <- "Heteromysis (Heteromysis) formosa"
# Heteromysis harpax                                   no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis harpax","ScientificName"] <- "Heteromysis (Gnathomysis) harpax"
# Amblyops abbreviata                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Amblyops abbreviata","ScientificName"] <- "Amblyops abbreviatus"
# Mysidium columbiae                                   no match
zoo.gbif[zoo.gbif$ScientificName == "Mysidium columbiae","ScientificName"] <- "Mysidium (Orientomysidium) columbiae"
# Amblyops antarctica                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Amblyops antarctica","ScientificName"] <- "Amblyops antarcticus"
# Heteromysis beetoni                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis beetoni","ScientificName"] <- "Heteromysis (Olivemysis) beetoni"
# Anisomysis xenops                                    no match
zoo.gbif[zoo.gbif$ScientificName == "Anisomysis xenops","ScientificName"] <- "Anisomysis (Carnegieomysis) xenops"
# Heteromysis panamaensis                              no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis panamaensis","ScientificName"] <- "Heteromysis (Olivemysis) panamaensis"
# Anisomysis chessi                                    no match
zoo.gbif[zoo.gbif$ScientificName == "Anisomysis chessi","ScientificName"] <- "Anisomysis (Anisomysis) chessi"
# Heteromysis tuberculospina                           no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis tuberculospina","ScientificName"] <- "Heteromysis (Olivemysis) tuberculospina"
# Heteromysis guitarti                                 no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis guitarti","ScientificName"] <- "Heteromysis (Olivemysis) guitarti"
# Paramysis helleri                                    no match
zoo.gbif[zoo.gbif$ScientificName == "Paramysis helleri","ScientificName"] <- "Paramysis (Longidentia) helleri"
# Mysidium gracile                                     no match
zoo.gbif[zoo.gbif$ScientificName == "Mysidium gracile","ScientificName"] <- "Mysidium (Mysidium) gracile"
# Heteromysis mayana                                   no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis mayana","ScientificName"] <- "Heteromysis (Olivemysis) mayana"
# Limnomysis benedeni                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Limnomysis benedeni","ScientificName"] <- "Limnomysis benedeni"
# Heteromysis bermudensis                              no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis bermudensis","ScientificName"] <- "Heteromysis (Olivemysis) bermudensis"
# Heteromysis tasmanica                                no match
zoo.gbif[zoo.gbif$ScientificName == "Heteromysis tasmanica","ScientificName"] <- "Heteromysis (Heteromysis) tasmanica"
# Mysidopsis sudafricana                               no match
zoo.gbif[zoo.gbif$ScientificName == "Mysidopsis sudafricana","ScientificName"] <- "Mysidopsis suedafrikana"
# Ommatocepheus ocellatus                              no match
zoo.gbif[zoo.gbif$ScientificName == "Ommatocepheus ocellatus","ScientificName"] <- "Ommatocepheus clavatus"
# Nauphantopsis diomedeae                              no match
zoo.gbif[zoo.gbif$ScientificName == "Nauphantopsis diomedeae","ScientificName"] <- "Nauphanthopsis diomedeae"
# Diacavolinia deshayesi van-der                       no match
zoo.gbif[zoo.gbif$ScientificName == "Diacavolinia deshayesi van-der","ScientificName"] <- "Diacavolinia deshayesi"
# Diacavolinia vanutrechti van-der                     no match
zoo.gbif[zoo.gbif$ScientificName == "Diacavolinia vanutrechti van-der","ScientificName"] <- "Diacavolinia vanutrechti"
# Diacavolinia aspina van-der                          no match
zoo.gbif[zoo.gbif$ScientificName == "Diacavolinia aspina van-der","ScientificName"] <- "Diacavolinia aspina"
# Cavolina uncinata                                    no match
zoo.gbif[zoo.gbif$ScientificName == "Cavolina uncinata","ScientificName"] <- "Cavolinia uncinata"
# Cavolina longirostris                                no match
zoo.gbif[zoo.gbif$ScientificName == "Cavolina longirostris","ScientificName"] <- "Diacavolinia longirostris"
# Cavolina tridentata                                  no match
zoo.gbif[zoo.gbif$ScientificName == "Cavolina tridentata","ScientificName"] <- "Cavolinia tridentata"
# Cavolina globulosa                                   no match
zoo.gbif[zoo.gbif$ScientificName == "Cavolina globulosa","ScientificName"] <- "Cavolinia globulosa"
# Cavolina inflexa                                     no match
zoo.gbif[zoo.gbif$ScientificName == "Cavolina inflexa","ScientificName"] <- "Cavolinia inflexa"
# Cavolinia quadridentata                              no match
zoo.gbif[zoo.gbif$ScientificName == "Cavolinia quadridentata","ScientificName"] <- "Cavolinia tridentata"
                                      
### Get WoRMS' keys
keys.worms <- wormsbynames( unique(zoo.gbif$ScientificName) )
keys.worms$ScientificName <- unique(zoo.gbif$ScientificName)

### Add WoRMS_status field
zoo.gbif <-  add_column(zoo.gbif, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(zoo.gbif)

# For testing:
# s <- unique(zoo.gbif$ScientificName)[153] ; s

res <- mclapply( unique(zoo.gbif$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- zoo.gbif[zoo.gbif$ScientificName == s,]
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

        }, mc.cores = 5

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
# dim(ddf) # 922'964
# ddf[56150:56250,]
unique(ddf$Species) ; unique(ddf$WoRMS_ID) ; unique(ddf$WoRMS_status)
ddf[ddf$WoRMS_ID == "To_add_at_the_end",]
rm(res) ; gc()

### 01/07/2021: Add the WoRMS_status and the AphiaID of freshwater/fossil species
# Boeckella gracilis
ddf[ddf$ScientificName == "Boeckella gracilis","WoRMS_status"] <- "isFreshwater"
ddf[ddf$ScientificName == "Boeckella gracilis","WoRMS_ID"] <- "349230"
ddf[ddf$ScientificName == "Boeckella gracilis","TaxonRank"] <- "Species"
ddf[ddf$ScientificName == "Boeckella gracilis","Species"] <- "Boeckella gracilis"
# Heterocope appendiculata
ddf[ddf$ScientificName == "Heterocope appendiculata","WoRMS_status"] <- "isFreshwater"
ddf[ddf$ScientificName == "Heterocope appendiculata","WoRMS_ID"] <- "352338"
ddf[ddf$ScientificName == "Heterocope appendiculata","TaxonRank"] <- "Species"
ddf[ddf$ScientificName == "Heterocope appendiculata","Species"] <- "Heterocope appendiculata"
# Heterocope saliens
ddf[ddf$ScientificName == "Heterocope saliens","WoRMS_status"] <- "isFreshwater"
ddf[ddf$ScientificName == "Heterocope saliens","WoRMS_ID"] <- "358800"
ddf[ddf$ScientificName == "Heterocope saliens","TaxonRank"] <- "Species"
ddf[ddf$ScientificName == "Heterocope saliens","Species"] <- "Heterocope saliens"
# Limnomysis benedeni
ddf[ddf$ScientificName == "Limnomysis benedeni","WoRMS_status"] <- "isFreshwater"
ddf[ddf$ScientificName == "Limnomysis benedeni","WoRMS_ID"] <- "120062"
ddf[ddf$ScientificName == "Limnomysis benedeni","TaxonRank"] <- "Species"
ddf[ddf$ScientificName == "Limnomysis benedeni","Species"] <- "Limnomysis benedeni"
# Ommatocepheus clavatus
ddf[ddf$ScientificName == "Ommatocepheus clavatus","WoRMS_status"] <- "isTerrestrial"
ddf[ddf$ScientificName == "Ommatocepheus clavatus","WoRMS_ID"] <- "583162"
ddf[ddf$ScientificName == "Ommatocepheus clavatus","TaxonRank"] <- "Species"
ddf[ddf$ScientificName == "Ommatocepheus clavatus","Species"] <- "Ommatocepheus clavatus"


### saving new file
save(ddf, file = "ZOObase_GBIF_merged_reformated+WoRMScheck_04_06_2021.RData")
gc()


### ----------------------------------------------------------------------------------------------------------------------------

### 08/06/2021: PhytoBase (Righetti et al., 2020)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/PHYTObase") ; dir()
phy <- get(load("PhytoBase_reformated_07_06_2021.Rdata"))
dim(phy) # 1'360'621      69
colnames(phy)
length( unique(phy$ScientificName) ) # 1710 names
unique(phy$ScientificName) # looks good

### Check those balebls that need correcting, like OBIS data above
keys.worms <- wormsbynames( unique(phy$ScientificName), marine_only = T)
keys.worms$ScientificName <- unique(phy$ScientificName)
# Return those non accepted labels
#keys.worms[is.na(keys.worms$status),"ScientificName"]
#keys.worms[keys.worms$ScientificName == "Anthosphaera periperforata",]

# 149 labels to adjust to WoRMS. If you really cn' find a synonym or a matching name in WoRMS, leave as is and comment in WoRMS_ID column after you get the APhiaID (e.g. Alisphaera spatula, Anacanthoica cidaris)

### Searches were also manually carried out on: https://www.algaebase.org/search/genus/detail/?genus_id=45112&sk=0
# 'C' indicates a name that is accepted taxonomically; 'S' a homotypic or heterotypic synonym; 'U' indicates a name of uncertain taxonomic status, but which has been subjected to some verification nomenclaturally; 'P' indicates a preliminary AlgaeBase entry that has not been subjected to any kind of verification. 

# To test some of the species names corrected below
wormsbynames("Pyramimonas janetae")

# Pyramimonas janetiae                                 no match
phy[phy$ScientificName == "Pyramimonas janetiae","ScientificName"] <- "Pyramimonas janetae"
# Anthosphaera periperforata                           no match
phy[phy$ScientificName == "Anthosphaera periperforata","ScientificName"] <- "Calcidiscus leptoporus"  
# Hayaster perplexus                                   no match
phy[phy$ScientificName == "Hayaster perplexus","ScientificName"] <- "Discoaster perplexus"
# Helicosphaera ampliaperta                            no match
phy[phy$ScientificName == "Helicosphaera ampliaperta","ScientificName"] <- "Helicopontosphaera ampliaperta"
# Helicosphaera granulata                              no match
phy[phy$ScientificName == "Helicosphaera granulata","ScientificName"] <- "Helicosphaera carteri"
# Helladosphaera arethusae                             no match
phy[phy$ScientificName == "Helladosphaera arethusae","ScientificName"] <- "Syracosphaera arethusae"
# Homozygosphaera arethusae                            no match
phy[phy$ScientificName == "Homozygosphaera arethusae","ScientificName"] <- "Syracosphaera arethusae"
# Rhabdosphaera stylifera                              no match
phy[phy$ScientificName == "Rhabdosphaera stylifera","ScientificName"] <- "Rhabdolithes claviger"
# Diplopsalopsis minor                                 no match
phy[phy$ScientificName == "Diplopsalopsis minor","ScientificName"] <- "Preperidinium meunieri"
# Kofoidinium lebouriae                                no match
phy[phy$ScientificName == "Kofoidinium lebouriae","ScientificName"] <- "Kofoidinium lebourae"
# Oxytoxum challangeroides                             no match
phy[phy$ScientificName == "Oxytoxum challangeroides","ScientificName"] <- "Oxytoxum challengeroides"
# Oxytoxum cribrosum                                   no match
phy[phy$ScientificName == "Oxytoxum cribrosum","ScientificName"] <- "Oxytoxum cribosum"
# Peridinium triqueta                                  no match
phy[phy$ScientificName == "Peridinium triqueta","ScientificName"] <- "Kryptoperidinium triquetrum"
# Protoperidinium marukawae                            no match
phy[phy$ScientificName == "Protoperidinium marukawae","ScientificName"] <- "Protoperidinium marukawai"
# Trinovantidinium applanatum                          no match
phy[phy$ScientificName == "Trinovantidinium applanatum","ScientificName"] <- "Protoperidinium shanghaiense"
# Tripos porrectus                                     no match
phy[phy$ScientificName == "Tripos porrectus","ScientificName"] <- "Ceratium porrectum"
# Chaetoceros laevis                                   no match
phy[phy$ScientificName == "Chaetoceros laevis","ScientificName"] <- "Chaetoceros leve"
# Chaetoceros pseudodichaetus                          no match
phy[phy$ScientificName == "Chaetoceros pseudodichaetus","ScientificName"] <- "Chaetoceros pseudodichaeta"
# Chaetoceros salsugineus                              no match
phy[phy$ScientificName == "Chaetoceros salsugineus","ScientificName"] <- "Chaetoceros salsugineum"
# Chaetoceros setoensis                                no match
phy[phy$ScientificName == "Chaetoceros setoensis","ScientificName"] <- "Chaetoceros setoense"
# Chaetoceros seychellarum                             no match
phy[phy$ScientificName == "Chaetoceros seychellarum","ScientificName"] <- "Chaetoceros seychellarus"
# Chaetoceros siamensis                                no match
phy[phy$ScientificName == "Chaetoceros siamensis","ScientificName"] <- "Chaetoceros siamense"
# Cyclotella meneghiniana                              no match
phy[phy$ScientificName == "Cyclotella meneghiniana","ScientificName"] <- "Cyclotella meneghiniana"
# Diatoma elongata                                     no match
phy[phy$ScientificName == "Diatoma elongata","ScientificName"] <- "Diatoma tenue"
# Dimeregramma minus                                   no match
phy[phy$ScientificName == "Dimeregramma minus","ScientificName"] <- "Plagiogramma minus"
# Diploneis constricta                                 no match
phy[phy$ScientificName == "Diploneis constricta","ScientificName"] <- "Navicula constricta"
# Ellerbeckia sol                                      no match
phy[phy$ScientificName == "Ellerbeckia sol","ScientificName"] <- "Gaillonella sol"
# Licmophora flabellata                                no match
phy[phy$ScientificName == "Licmophora flabellata","ScientificName"] <- "Exilaria flabellata"
# Licmophora paradoxa                                  no match
phy[phy$ScientificName == "Licmophora paradoxa","ScientificName"] <- "Echinella paradoxa"
# Lyrella lyra                                         no match
phy[phy$ScientificName == "Lyrella lyra","ScientificName"] <- "Navicula lyra"
# Navicula pagophila                                   no match
phy[phy$ScientificName == "Navicula pagophila","ScientificName"] <- "Navicula tuscula"
# Paralia polaris                                      no match
phy[phy$ScientificName == "Paralia polaris","ScientificName"] <- "Melosira polaris"
# Psammodictyon constrictum                            no match
phy[phy$ScientificName == "Psammodictyon constrictum","ScientificName"] <- "Tryblionella constricta"
# Rhizosolenia styIiformis                             no match
phy[phy$ScientificName == "Rhizosolenia styIiformis","ScientificName"] <- "Rhizosolenia styliformis"
# Surirella febigeri                                   no match
phy[phy$ScientificName == "Surirella febigeri","ScientificName"] <- "Surirella febigerii"
# Synedra gailonii                                     no match
phy[phy$ScientificName == "Synedra gailonii","ScientificName"] <- "Synedra gallionii"
# Thalassiosira angustelineata                         no match
phy[phy$ScientificName == "Thalassiosira angustelineata","ScientificName"] <- "Coscinodiscus angustelineatus"
# Tryblionella marginulata                             no match
phy[phy$ScientificName == "Tryblionella marginulata","ScientificName"] <- "Nitzschia marginulata"
# Diploneis obliqua                                    no match
phy[phy$ScientificName == "Diploneis obliqua","ScientificName"] <- "Navicula didyma"
# Gyrosigma sciotoense                                 no match
phy[phy$ScientificName == "Gyrosigma sciotoense","ScientificName"] <- "Pleurosigma sciotense"
# Halamphora coffeiformis                              no match
phy[phy$ScientificName == "Halamphora coffeiformis","ScientificName"] <- "Halamphora coffeaeformis"
# Lyrella exsul                                        no match
phy[phy$ScientificName == "Lyrella exsul","ScientificName"] <- "Navicula exsul"
# Navicula oahuensis                                   no match
phy[phy$ScientificName == "Navicula oahuensis","ScientificName"] <- "Cymbella oahuensis"
# Prestauroneis protractoides                          no match
phy[phy$ScientificName == "Prestauroneis protractoides","ScientificName"] <- "Prestauroneis protracta"
# Chaetoceros hyalochaetae                             no match
phy[phy$ScientificName == "Chaetoceros hyalochaetae","ScientificName"] <- "Chaetoceros (Hyalochaete)"
# Chaetoceros phaeoceros                               no match
phy[phy$ScientificName == "Chaetoceros phaeoceros","ScientificName"] <- "Chaetoceros (Phaeoceros)"


### Re-check
keys.worms <- wormsbynames( unique(phy$ScientificName), marine_only = T)
keys.worms$ScientificName <- unique(phy$ScientificName)


### Retrieve AphiaID for all 
# Add WoRMS_status field
phy <-  add_column(phy, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(phy)

# For testing:
s <- unique(phy$ScientificName)[13] ; s
s <- "Syracosphaera ossa" # keys.worms[keys.worms$ScientificName == s,]

res <- mclapply( unique(phy$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- phy[phy$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS; unassessed AlgaeBase entry")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- "Species" # because all are actual species names, except those 2 Chaetoceros subgenera
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 25

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
dim(ddf) # 1'360'621        70
rm(res) ; gc()
#ddf[360621:360821,]
unique(ddf$Species) ; unique(ddf$WoRMS_ID)

unique(ddf[is.na(ddf$Species),"ScientificName"])

### One weird value left that is not NA nor a proper WoRMS ID
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS; unassessed AlgaeBase entry","ScientificName"])
length(unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS; unassessed AlgaeBase entry","ScientificName"])) / length(unique(ddf$ScientificName))
# 7.5% of ScientificName

# Good. Before, you finish, need to trouble shoot some "To_add_at_the_end" that were left
#unique(ddf[ddf$WoRMS_ID == "To_add_at_the_end","ScientificName"])
sp2correct <- unique( ddf[ddf$WoRMS_ID == "To_add_at_the_end","ScientificName"] ) ; sp2correct
# head(ddf[ddf$WoRMS_ID == "To_add_at_the_end",])
#keys.worms[keys.worms$ScientificName == "Tripos geniculatus",]
#s <- sp2correct[1]
for(s in sp2correct) {
    
    message(paste("Correcting ", s, sep = ""))
    
    ddf[ddf$ScientificName == s,"WoRMS_ID"] <- keys.worms[keys.worms$ScientificName == s,"AphiaID"]
    ddf[ddf$ScientificName == s,"TaxonRank"] <- "Species"
    ddf[ddf$ScientificName == s,"Species"] <- s
    statuses <- melt( keys.worms[keys.worms$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
    status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
    ddf[ddf$ScientificName == s,"WoRMS_status"] <- factor(status2add)
    
} # for s in sp2correct
# Check again
unique(ddf$WoRMS_ID)
unique(ddf$WoRMS_status)
unique(ddf$Species)

# And adjust 'Species' for those Species == NA although ScientificName is a species
sp2correct2 <- unique(ddf[is.na(ddf$Species) & ddf$TaxonRank == "Species","ScientificName"])
for(s in sp2correct2) {
    message(paste("Correcting ", s, sep = ""))
    ddf[ddf$ScientificName == s,"Species"] <- s
} # for s in sp2correct
unique(ddf[is.na(ddf$Species),"ScientificName"]) ; unique(ddf[is.na(ddf$Species),"Genus"])
# SHould be aonly Genus names

### List of fossil taxa to correct AFTER the AphiaID getter
str(ddf$WoRMS_status)
# Convert factor to char string
ddf$WoRMS_status <- as.character(ddf$WoRMS_status)
ddf[ddf$WoRMS_status == "" & !is.na(ddf$WoRMS_status),"WoRMS_status"] <- NA

# Ceratolithus tricorniculatus  
# Coccolithus doronicoides
# Coccolithus subpertusus 
# Helicopontosphaera ampliaperta
# Helicosphaera burkei 
# Alisocysta circumtabulata
# Leonella granifera
fossils <- c("Ceratolithus tricorniculatus","Coccolithus doronicoides","Coccolithus subpertusus",
            "Helicopontosphaera ampliaperta","Helicosphaera burkei","Alisocysta circumtabulata","Leonella granifera")

ddf[ddf$ScientificName %in% fossils,"WoRMS_status"] <- "isExtinct"

### List if freshwater taxa: 
# Aulacoseira granulata  
# Cyclotella meneghiniana
# Cyclotella stylorum
# Neodelphineis silenda
# Tabularia investiens
# Tryblionella apiculata
# Tryblionella debilis
# Tryblionella granulata
freshies <- c("Aulacoseira granulata","Cyclotella meneghiniana","Cyclotella stylorum","Neodelphineis silenda",
                "Tabularia investiens","Tryblionella apiculata","Tryblionella debilis","Tryblionella granulata")
#
ddf[ddf$ScientificName %in% freshies,"WoRMS_status"] <- "isFreshwater"

### Remove Picoeukaryotes
dim(ddf[which(ddf$ScientificName == "Picoeukaryotes"),]) # 27'537 Picoeukaryotes occurrences
ddf2 <- ddf[-which(ddf$ScientificName == "Picoeukaryotes"),]
dim(ddf) ; dim(ddf2)

### Save PhytoBase
save(ddf2, file = "PhytoBase_reformated+WoRMScheck_08_06_2021.Rdata")

# # Check one last thing
# str(ddf2$Species)
# dim(ddf2[ddf2$Species == "Carteria marina",])
# head(ddf2[ddf2$Species == "Carteria marina",])
# subset <- ddf2[ddf2$Species == "Carteria marina",c(39:49)] %>% drop_na(ScientificName)
# ddf2[501000:501100,c(31:35,39:49)]


### ----------------------------------------------------------------------------------------------------------------------------

### 18/06/2021: JeDi (Lucas et al., 2014) 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database")

### Part A: Occurrences only (presence-absence)
jedi <- get(load("JeDi_Lucas&al._2014_occurrences_reformated_18_06_2021.Rdata"))
dim(jedi)
head(jedi)
# Check n spp
length( unique(jedi$ScientificName) ) # 1020 names
unique(jedi$ScientificName) # Still ned to remove the:
# - " sp"
# - " spp."
jedi$ScientificName <- str_replace_all(jedi$ScientificName, " spp.", "")
jedi$ScientificName <- str_replace_all(jedi$ScientificName, " sp", "")
jedi$ScientificName <- gsub(pattern = "\\.", replacement = "", x = jedi$ScientificName)
length( unique(jedi$ScientificName) ) # 937 names, lost 83 names

# Do a first check with wormsbynames()
# keys.worms <- wormsbynames( unique(jedi$ScientificName), marine_only = T)

### Labels to manually correct otherwise no match in WoRMS
# Aeginopsis laurenti                                  no match
jedi[jedi$ScientificName == "Aeginopsis laurenti","ScientificName"] <- "Aeginopsis laurentii"
# Liriope  tetraphylla                                 no match
jedi[jedi$ScientificName == "Liriope  tetraphylla","ScientificName"] <- "Liriope tetraphylla"
# Oikopleura vanhoeffeni                               no match
jedi[jedi$ScientificName == "Oikopleura vanhoeffeni","ScientificName"] <- "Oikopleura (Vexillaria) vanhoeffeni"
# Oikopleura parva                                     no match
jedi[jedi$ScientificName == "Oikopleura parva","ScientificName"] <- "Oikopleura (Vexillaria) parva"
# Eudoxoidesiralis                                     no match
jedi[jedi$ScientificName == "Eudoxoidesiralis","ScientificName"] <- "Eudoxoides spiralis"
# Oikopleura diocia                                    no match
jedi[jedi$ScientificName == "Oikopleura diocia","ScientificName"] <- "Oikopleura (Vexillaria) dioica"
# Celophyes appendiculata                              no match
jedi[jedi$ScientificName == "Celophyes appendiculata","ScientificName"] <- "Chelophyes appendiculata"
# Celophyes                                            no match
jedi[jedi$ScientificName == "Celophyes","ScientificName"] <- "Chelophyes"
# Muggiaea kochi                                       no match
jedi[jedi$ScientificName == "Muggiaea kochi","ScientificName"] <- "Muggiaea kochii"
# Oikopleura longicauda                                no match
jedi[jedi$ScientificName == "Oikopleura longicauda","ScientificName"] <- "Oikopleura (Coecaria) longicauda"
# Oikopleura labradoriensis                            no match
jedi[jedi$ScientificName == "Oikopleura labradoriensis","ScientificName"] <- "Oikopleura (Vexillaria) labradoriensis"
# Turritopis nutricula                                 no match
jedi[jedi$ScientificName == "Turritopis nutricula","ScientificName"] <- "Turritopsis nutricula"
# Tima bairdi                                          no match
jedi[jedi$ScientificName == "Tima bairdi","ScientificName"] <- "Tima bairdii"
# Rathkea octopunktata                                 no match
jedi[jedi$ScientificName == "Rathkea octopunktata","ScientificName"] <- "Rathkea octopunctata"
# Sarsia prolifer                                      no match
jedi[jedi$ScientificName == "Sarsia prolifer","ScientificName"] <- "Codonium proliferum"
# Steenstrupia natans                                  no match
jedi[jedi$ScientificName == "Steenstrupia natans","ScientificName"] <- "Corymorpha nutans"
# Euphysa auata                                        no match
jedi[jedi$ScientificName == "Euphysa auata","ScientificName"] <- "Euphysa aurata"
# Studiosarsia producta                                no match
jedi[jedi$ScientificName == "Studiosarsia producta","ScientificName"] <- "Sarsia hargitti"
# octopuncatata                                        no match
jedi[jedi$ScientificName == "octopuncatata","ScientificName"] <- "Rathkea octopunctata"
# Aequores vitrina                                     no match
jedi[jedi$ScientificName == "Aequores vitrina","ScientificName"] <- "Aequorea vitrina"
# Eutoninia indicans                                   no match
jedi[jedi$ScientificName == "Eutoninia indicans","ScientificName"] <- "Eutonina indicans"
# Eucheilota muclata                                   no match
jedi[jedi$ScientificName == "Eucheilota muclata","ScientificName"] <- "Eucheilota maculata"
# Tiaropsis multicarrata                               no match
jedi[jedi$ScientificName == "Tiaropsis multicarrata","ScientificName"] <- "Tiaropsis multicirrata"
# Muggia                                               no match
jedi[jedi$ScientificName == "Muggia","ScientificName"] <- "Muggiaea"
# Eudoxides mitra                                      no match
jedi[jedi$ScientificName == "Eudoxides mitra","ScientificName"] <- "Eudoxoides mitra"
# Eudoxidesiralis                                      no match
jedi[jedi$ScientificName == "Eudoxidesiralis","ScientificName"] <- "Eudoxoides spiralis"
# Amphycarion acaule                                   no match
jedi[jedi$ScientificName == "Amphycarion acaule","ScientificName"] <- "Amphicaryon acaule"
# Vogtiainosa                                          no match
jedi[jedi$ScientificName == "Vogtiainosa","ScientificName"] <- "Vogtia spinosa"
# Ceratocymba leuckarti                                no match
jedi[jedi$ScientificName == "Ceratocymba leuckarti","ScientificName"] <- "Ceratocymba leuckartii"
# Agalma okeni                                         no match
jedi[jedi$ScientificName == "Agalma okeni","ScientificName"] <- "Agalma okenii"
# Lensia grimaldi                                      no match
jedi[jedi$ScientificName == "Lensia grimaldi","ScientificName"] <- "Lensia grimaldii"
# Forskalia ?contorta                                  no match
jedi[jedi$ScientificName == "Forskalia ?contorta","ScientificName"] <- "Forskalia contorta"
# Crossota brunea                                      no match
jedi[jedi$ScientificName == "Crossota brunea","ScientificName"] <- "Crossota brunnea"
# Nausithoe goblifera                                  no match
jedi[jedi$ScientificName == "Nausithoe goblifera","ScientificName"] <- "Nausithoe globifera"
# Craspedacusta sowerbii                               no match
jedi[jedi$ScientificName == "Craspedacusta sowerbii","ScientificName"] <- "Craspedacusta sowerbii"
# Carybdea rastoni                                     no match
jedi[jedi$ScientificName == "Carybdea rastoni","ScientificName"] <- "Carybdea rastonii"
# Limnocnida tanganyicae                               no match
jedi[jedi$ScientificName == "Limnocnida tanganyicae","ScientificName"] <- "Limnocnida tanganjicae"
# Clytia lineares                                      no match
jedi[jedi$ScientificName == "Clytia lineares","ScientificName"] <- "Clytia linearis"
# Aequorea pensile                                     no match
jedi[jedi$ScientificName == "Aequorea pensile","ScientificName"] <- "Aequorea pensilis"
# Aeginura grimaldi                                    no match
jedi[jedi$ScientificName == "Aeginura grimaldi","ScientificName"] <- "Aeginura grimaldii"
# Phacellophora camtshatica                            no match
jedi[jedi$ScientificName == "Phacellophora camtshatica","ScientificName"] <- "Phacellophora camtschatica"
# Modeeria rotundata                                   no match
jedi[jedi$ScientificName == "Modeeria rotundata","ScientificName"] <- "Modeeria rotunda"
# Atorella octogonas                                   no match
jedi[jedi$ScientificName == "Atorella octogonas","ScientificName"] <- "Atorella octogonos"
# Halimedusa pauper                                    no match
jedi[jedi$ScientificName == "Halimedusa pauper","ScientificName"] <- "Halimedusa typus"
# Calycopsis borchgrewinki                             no match
jedi[jedi$ScientificName == "Calycopsis borchgrewinki","ScientificName"] <- "Calycopsis borchgrevinki"
# Mitrocomella figida                                  no match
jedi[jedi$ScientificName == "Mitrocomella figida","ScientificName"] <- "Mitrocomella frigida"
# Forskalia edwardsi                                   no match
jedi[jedi$ScientificName == "Forskalia edwardsi","ScientificName"] <- "Forskalia edwardsii"
# Abylopsis eschscholtzi                               no match
jedi[jedi$ScientificName == "Abylopsis eschscholtzi","ScientificName"] <- "Abylopsis eschscholtzii"
# Leuckartiara octonae                                 no match
jedi[jedi$ScientificName == "Leuckartiara octonae","ScientificName"] <- "Leuckartiara octona"
# Halicreas minum                                      no match
jedi[jedi$ScientificName == "Halicreas minum","ScientificName"] <- "Halicreas minimum"
# Chrysaora helova                                     no match
jedi[jedi$ScientificName == "Chrysaora helova","ScientificName"] <- "Chrysaora fuscescens"
# Phialidium hemisphericum                             no match
jedi[jedi$ScientificName == "Phialidium hemisphericum","ScientificName"] <- "Clytia hemisphaerica"
# Rhopalonema vetalum                                  no match
jedi[jedi$ScientificName == "Rhopalonema vetalum","ScientificName"] <- "Rhopalonema velatum"
# Muggiaeairalis                                       no match
jedi[jedi$ScientificName == "Muggiaeairalis","ScientificName"] <- "Eudoxoides spiralis"
# Porpita linnaeana                                    no match
jedi[jedi$ScientificName == "Porpita linnaeana","ScientificName"] <- "Porpita linneana"
# Atolla verrilli                                      no match
jedi[jedi$ScientificName == "Atolla verrilli","ScientificName"] <- "Atolla verrillii"
# Pyrosomainosum                                       no match
jedi[jedi$ScientificName == "Pyrosomainosum","ScientificName"] <- "Pyrostremma spinosum"
# Tamoya tamoya                                        no match
jedi[jedi$ScientificName == "Tamoya tamoya","ScientificName"] <- "Tamoya"
# Atolla bairdi                                        no match
jedi[jedi$ScientificName == "Atolla bairdi","ScientificName"] <- "Atolla bairdii"
# Abyla leuckarti                                      no match
jedi[jedi$ScientificName == "Abyla leuckarti","ScientificName"] <- "Ceratocymba leuckartii"
# Catablema vesicaria                                  no match
jedi[jedi$ScientificName == "Catablema vesicaria","ScientificName"] <- "Catablema vesicarium"
# Hybocodon pendula                                    no match
jedi[jedi$ScientificName == "Hybocodon pendula","ScientificName"] <- "Corymorpha pendula"
# Diphyesiralis                                        no match
jedi[jedi$ScientificName == "Diphyesiralis","ScientificName"] <- "Eudoxoides spiralis"
# Diphyes acuta                                        no match
jedi[jedi$ScientificName == "Diphyes acuta","ScientificName"] <- "Chelophyes appendiculata"
# Clausophyes galatea                                  no match
jedi[jedi$ScientificName == "Clausophyes galatea","ScientificName"] <- "Clausophyes galeata"
# Praya cymbiformis                                    no match
jedi[jedi$ScientificName == "Praya cymbiformis","ScientificName"] <- "Rosacea cymbiformis"
# Beroe forskali                                       no match
jedi[jedi$ScientificName == "Beroe forskali","ScientificName"] <- "Beroe forskalii"
# Mastigias ocellata                                   no match
jedi[jedi$ScientificName == "Mastigias ocellata","ScientificName"] <- "Mastigias ocellatus"
# Salpa hexagona                                       no match
jedi[jedi$ScientificName == "Salpa hexagona","ScientificName"] <- "Metcalfina hexagona"
# Periphylla dodecabostrycha                           no match
jedi[jedi$ScientificName == "Periphylla dodecabostrycha","ScientificName"] <- "Periphylla periphylla"
# Thamnostoma alexanderi                               no match
jedi[jedi$ScientificName == "Thamnostoma alexanderi","ScientificName"] <- "Lymnorea alexandri"
# Pyrosoma paradoxum                                   no match
jedi[jedi$ScientificName == "Pyrosoma paradoxum","ScientificName"] <- "Pyrosoma atlanticum"
# Bougainvillia mertensi                               no match
jedi[jedi$ScientificName == "Bougainvillia mertensi","ScientificName"] <- "Bougainvillia Mertensii "
# Rhizophysa eysenhardti                               no match
jedi[jedi$ScientificName == "Rhizophysa eysenhardti","ScientificName"] <- "Rhizophysa eysenhardtii"
# Stephanomia amphitrides                              no match
jedi[jedi$ScientificName == "Stephanomia amphitrides","ScientificName"] <- "Stephanomia amphytridis"
# Olindias periphylla                                  no match
jedi[jedi$ScientificName == "Olindias periphylla","ScientificName"] <- "Periphylla periphylla"
# Doliolina undulatum                                  no match
jedi[jedi$ScientificName == "Doliolina undulatum","ScientificName"] <- "Doliolina (Doliolina) undulata"
# Doliolenetta                                         no match
jedi[jedi$ScientificName == "Doliolenetta","ScientificName"] <- "Doliolina (Doliolinetta)"
# Limnocnida congoensis                                no match
#jedi[jedi$ScientificName == "Limnocnida congoensis","ScientificName"] <- "Limnocnida congoensis"
### IsFreshwater
# Aglauropsis aeroa                                    no match
jedi[jedi$ScientificName == "Aglauropsis aeroa","ScientificName"] <- "Aglauropsis aeora"
# Nauphantopsis diomedeae                              no match
jedi[jedi$ScientificName == "Nauphantopsis diomedeae","ScientificName"] <- "Nauphanthopsis diomedeae"
# Atorella vanhoffeni                                  no match
jedi[jedi$ScientificName == "Atorella vanhoffeni","ScientificName"] <- "Atorella vanhoeffeni"
# Lobonema smithi                                      no match
jedi[jedi$ScientificName == "Lobonema smithi","ScientificName"] <- "Lobonema smithii"
# Aglantha igitale                                     no match
jedi[jedi$ScientificName == "Aglantha igitale","ScientificName"] <- "Aglantha digitale"
# Abylap                                               no match
jedi[jedi$ScientificName == "Abylap","ScientificName"] <- "Abyla"
# Salpida unid                                         no match
jedi[jedi$ScientificName == "Salpida unid","ScientificName"] <- NA
jedi <- jedi %>% drop_na(ScientificName)
# Hormiphora pulmosa                                   no match
jedi[jedi$ScientificName == "Hormiphora pulmosa","ScientificName"] <- "Hormiphora plumosa"
# Tinerfe lactea                                       no match
jedi[jedi$ScientificName == "Tinerfe lactea","ScientificName"] <- "Tinerfe cyanea"
# Corollaectabilis                                     no match
jedi[jedi$ScientificName == "Corollaectabilis","ScientificName"] <- "Corolla spectabilis"
# Anthomedusaep                                        no match
jedi[jedi$ScientificName == "Anthomedusaep","ScientificName"] <- NA
jedi <- jedi %>% drop_na(ScientificName)
# Callianira bilata                                    no match
jedi[jedi$ScientificName == "Callianira bilata","ScientificName"] <- "Callianira bialata"
# Ocyropsis maculata fusca                             no match
jedi[jedi$ScientificName == "Ocyropsis maculata fusca","ScientificName"] <- "Ocyropsis maculata"
# Stomolophus Stomolophus                              no match
jedi[jedi$ScientificName == "Stomolophus Stomolophus","ScientificName"] <- "Stomolophus"
# Pleurbrachia bachei                                  no match
jedi[jedi$ScientificName == "Pleurbrachia bachei","ScientificName"] <- "Pleurobrachia bachei"
# Corynidae Sarsia                                     no match
jedi[jedi$ScientificName == "Corynidae Sarsia","ScientificName"] <- "Sarsia"
# Clytia gregarium                                     no match
jedi[jedi$ScientificName == "Clytia gregarium","ScientificName"] <- "Clytia gregaria"
# Pleurbrachia                                         no match
jedi[jedi$ScientificName == "Pleurbrachia","ScientificName"] <- "Pleurobrachia"
# Microcoma                                            no match
jedi[jedi$ScientificName == "Microcoma","ScientificName"] <- "Microcomatula mortenseni"
# Pleurbrachia pileus                                  no match
jedi[jedi$ScientificName == "Pleurbrachia pileus","ScientificName"] <- "Pleurobrachia pileus"
# Microcoma cellularia                                 no match
jedi[jedi$ScientificName == "Microcoma cellularia","ScientificName"] <- "Microcomatula mortenseni"
# Polyorchidae Scrippsia                               no match
jedi[jedi$ScientificName == "Polyorchidae Scrippsia","ScientificName"] <- "Scrippsia"
# Polyochis                                            no match
jedi[jedi$ScientificName == "Polyochis","ScientificName"] <- "Polyorchis penicillatus"
# Aequorea forkalea                                    no match
jedi[jedi$ScientificName == "Aequorea forkalea","ScientificName"] <- "Aequorea forskalea"
# Ulmaridae Aurelia                                    no match
jedi[jedi$ScientificName == "Ulmaridae Aurelia","ScientificName"] <- "Aurelia"
# Polyochis penicillatus                               no match
jedi[jedi$ScientificName == "Polyochis penicillatus","ScientificName"] <- "Polyorchis penicillatus"
# Chirosalmus quadrumanus                              no match
jedi[jedi$ScientificName == "Chirosalmus quadrumanus","ScientificName"] <- "Chiropsalmus quadrumanus"
#  fusiformis                                          no match
jedi[jedi$ScientificName == " fusiformis","ScientificName"] <- "Salpa fusiformis"
# Aequorea aequeora                                    no match
jedi[jedi$ScientificName == "Aequorea aequeora","ScientificName"] <- "Aequorea forskalea"
# Margelopsis hartlaubi                                no match
jedi[jedi$ScientificName == "Margelopsis hartlaubi","ScientificName"] <- "Margelopsis hartlaubii"
# Lasis zonaria                                        no match
jedi[jedi$ScientificName == "Lasis zonaria","ScientificName"] <- "Soestia zonaria"
# Leucothea milticornis                                no match
jedi[jedi$ScientificName == "Leucothea milticornis","ScientificName"] <- "Leucothea multicornis"
# Aequorea australia                                   no match
jedi[jedi$ScientificName == "Aequorea australia","ScientificName"] <- "Aequorea australis"
# Staurdiscus                                          no match
jedi[jedi$ScientificName == "Staurdiscus","ScientificName"] <- "Staurodiscus"
# Melicertessa clavigera                               no match
jedi[jedi$ScientificName == "Melicertessa clavigera","ScientificName"] <- "Melicertissa clavigera"
# Cyclosalpa sewalli                                   no match
jedi[jedi$ScientificName == "Cyclosalpa sewalli","ScientificName"] <- "Cyclosalpa sewelli"
# Ritteriella cylindrica                               no match
jedi[jedi$ScientificName == "Ritteriella cylindrica","ScientificName"] <- "Iasis cylindrica"
# Ritteriella maxima                                   no match
jedi[jedi$ScientificName == "Ritteriella maxima","ScientificName"] <- "Salpa maxima"

### Do a re-check
length( unique(jedi$ScientificName) ) # 872 now, lost since 
keys.worms <- wormsbynames( unique(jedi$ScientificName), marine_only = T)
# look OK.
keys.worms$ScientificName <- unique(jedi$ScientificName)

# Add WoRMS_status field
jedi <-  add_column(jedi, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(jedi)

# s %in% unique(keys.worms$scientificname)

# For testing:
s <- unique(jedi$ScientificName)[13] ; s
s <- "Cotylorhiza pacifica"
res <- mclapply( unique(jedi$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- jedi[jedi$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- "Species" # because all are actual species names, except those 2 Chaetoceros subgenera
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 20

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
dim(ddf) # 298'969       71
rm(res) ; gc()
ddf[10000:10100,]
unique(ddf$Species) ; unique(ddf$WoRMS_ID)
# one "To_add_at_the_end" left
ddf[ddf$WoRMS_ID == "To_add_at_the_end",]
# Fix 
ddf[ddf$ScientificName == s,"WoRMS_ID"] <- "287182"
unique(ddf$WoRMS_status)
unique(ddf$Species)
length(unique(ddf$Species)) ; length(unique(ddf$ScientificName)) # Mispmatch since a lot of "ScientificName" are actually genera
unique(ddf$TaxonRank)
ddf[is.na(ddf$TaxonRank),"TaxonRank"] <- "Species"

### Save
save(ddf, file = "JeDi_Lucas&al._2014_occurrences_reformated+WoRMScheck_18_06_2021.Rdata")


### ---------------------------------------------------------

### Part B: Abundances only (integrated density)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database")

### Part A: Occurrences only (presence-absence)
jedi <- get(load("JeDi_Lucas&al._2014_abundances_reformated_03_12_2021.Rdata"))
dim(jedi) ; head(jedi)
# Check n spp
length( unique(jedi$ScientificName) ) # 191 names
unique(jedi$ScientificName) # Still ned to remove the:
# - " sp"
# - " spp."
jedi$ScientificName <- str_replace_all(jedi$ScientificName, " spp.", "")
jedi$ScientificName <- str_replace_all(jedi$ScientificName, " sp", "")
jedi$ScientificName <- gsub(pattern = "\\.", replacement = "", x = jedi$ScientificName)
length( unique(jedi$ScientificName) ) # 186 names

# Do a first check with wormsbynames()
require("worms")
keys.worms <- wormsbynames( unique(jedi$ScientificName), marine_only = F)

### Labels to manually correct otherwise no match in WoRMS
# Aeginopsis laurenti                                  no match
jedi[jedi$ScientificName == "Aeginopsis laurenti","ScientificName"] <- "Aeginopsis laurentii"
# Fritillaridae                                        no match
jedi[jedi$ScientificName == "Fritillaridae","ScientificName"] <- "Fritillariidae"
# Liriope  tetraphylla                                 no match
jedi[jedi$ScientificName == "Liriope  tetraphylla","ScientificName"] <- "Liriope tetraphylla"
# Oikopleura vanhoeffeni                               no match
jedi[jedi$ScientificName == "Oikopleura vanhoeffeni","ScientificName"] <- "Oikopleura (Vexillaria) vanhoeffeni"
# Oikopleura parva                                     no match
jedi[jedi$ScientificName == "Oikopleura parva","ScientificName"] <- "Oikopleura (Vexillaria) parva"
# Hyrdrozoa                                            no match
jedi[jedi$ScientificName == "Hyrdrozoa","ScientificName"] <- "Hydrozoa"
# Eudoxoidesiralis                                     no match
jedi[jedi$ScientificName == "Eudoxoidesiralis","ScientificName"] <- "Eudoxoides spiralis"
# Oikopleura diocia                                    no match
jedi[jedi$ScientificName == "Oikopleura diocia","ScientificName"] <- "Oikopleura (Vexillaria) dioica"
# Celophyes appendiculata                              no match
jedi[jedi$ScientificName == "Celophyes appendiculata","ScientificName"] <- "Chelophyes appendiculata"
# Celophyes                                            no match
jedi[jedi$ScientificName == "Celophyes","ScientificName"] <- "Chelophyes"
# Muggiaea kochi                                       no match
jedi[jedi$ScientificName == "Muggiaea kochi","ScientificName"] <- "Muggiaea kochii"
# Oikopleura longicauda                                no match
jedi[jedi$ScientificName == "Oikopleura longicauda","ScientificName"] <- "Oikopleura (Coecaria) longicauda"
# Oikopleura labradoriensis                            no match
jedi[jedi$ScientificName == "Oikopleura labradoriensis","ScientificName"] <- "Oikopleura (Vexillaria) labradoriensis"
# Sarsia prolifer                                      no match
jedi[jedi$ScientificName == "Sarsia prolifer","ScientificName"] <- "Codonium proliferum"
# Steenstrupia natans                                  no match
jedi[jedi$ScientificName == "Steenstrupia natans","ScientificName"] <- "Corymorpha nutans"
# Euphysa auata                                        no match
jedi[jedi$ScientificName == "Euphysa auata","ScientificName"] <- "Euphysa aurata"
# Studiosarsia producta                                no match
jedi[jedi$ScientificName == "Studiosarsia producta","ScientificName"] <- "Stauridiosarsia producta"
# octopuncatata                                        no match
jedi[jedi$ScientificName == "octopuncatata","ScientificName"] <- "Rathkea octopunctata"
# Aequores vitrina                                     no match
jedi[jedi$ScientificName == "Aequores vitrina","ScientificName"] <- "Aequorea vitrina"
# Eutoninia indicans                                   no match
jedi[jedi$ScientificName == "Eutoninia indicans","ScientificName"] <- "Eutonina indicans"
# Tima bairdi                                          no match
jedi[jedi$ScientificName == "Tima bairdi","ScientificName"] <- "Tima bairdii"
# Eucheilota muclata                                   no match
jedi[jedi$ScientificName == "Eucheilota muclata","ScientificName"] <- "Eucheilota maculata"
# Tiaropsis multicarrata                               no match
jedi[jedi$ScientificName == "Tiaropsis multicarrata","ScientificName"] <- "Tiaropsis multicirrata"
# Muggia                                               no match
jedi[jedi$ScientificName == "Muggia","ScientificName"] <- "Muggiaea"
# Eudoxides mitra                                      no match
jedi[jedi$ScientificName == "Eudoxides mitra","ScientificName"] <- "Eudoxoides mitra"
# Eudoxidesiralis                                      no match
jedi[jedi$ScientificName == "Eudoxidesiralis","ScientificName"] <- "Eudoxoides spiralis"
# Amphycarion acaule                                   no match
jedi[jedi$ScientificName == "Amphycarion acaule","ScientificName"] <- "Amphicaryon acaule"
# Margelopsis hartlaubi                                no match
jedi[jedi$ScientificName == "Margelopsis hartlaubi","ScientificName"] <- "Margelopsis hartlaubii"
# Lasis zonaria                                        no match
jedi[jedi$ScientificName == "Lasis zonaria","ScientificName"] <- "Soestia zonaria"


### Do a re-check
length( unique(jedi$ScientificName) ) # 178
keys.worms <- wormsbynames( unique(jedi$ScientificName), marine_only = F)
keys.worms$ScientificName <- unique(jedi$ScientificName)

# Add WoRMS_status field
jedi <-  add_column(jedi, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(jedi)

res <- mclapply( unique(jedi$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- jedi[jedi$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms[keys.worms$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- "Species" # because all are actual species names, except those 2 Chaetoceros subgenera
            
            } else if( !is.na(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms[keys.worms$ScientificName == s,"valid_AphiaID"])
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
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            # head(subset)
            return(subset)

        }, mc.cores = 20

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
dim(ddf) # 238083   71
rm(res) ; gc()
ddf[6000:6100,]
unique(ddf$Species) ; unique(ddf$WoRMS_ID)
unique(ddf$WoRMS_status)
length(unique(ddf$Species)) ; length(unique(ddf$ScientificName)) # 
unique(ddf$TaxonRank)

### Save
save(ddf, file = "JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_03_12_2021.Rdata")

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



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------