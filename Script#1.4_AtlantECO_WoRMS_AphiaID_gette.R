
##### ATLANTECO SCRIPT 1.4 ----------------------------------------------------------------------------------------------------------------------------
##### 06/05/2021: R Script to retrieve WORMS AphiaID code for any biological occurrence/abundance observation © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### For each dataset with taxon-level observations (occurrences/ abund/ biomass whatever):
#	- extract the vector containing the scientific name associated with the observation/measurement
#   - clean it if necessary (remove underscore...retain genus name only etc.)
#   - use functions from R package 'worms' to retrieve WoRMS' accepted species name and AphiaID

module load R/4.0.3 # To load latest R version on kryo

### Latest update: 06/05/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")

# If not already there:
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional")
WD <- getwd()

### ----------------------------------------------------------------------------------------------------------------------------

### List the datasets on which to apply the function you'll define to retrive the alphaID from worms
# - ZOObase
# - PHYTObase
# - COPEPOD - zoo
# - COPEPOD - phyto
# - AMT? (when available)
# - CPR? (when absences made available)
# - MALASPINA (Villarino species counts)
# - TARA (microscopy and/or metagenomics) when available

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
#

### OK, seems like we must use wormsbynames()


### ----------------------------------------------------------------------------------------------------------------------------

### 1°) First dataset (smallest): MALASPINA species data from Villarino et al. (2017)
setwd(paste(WD,"/","MALASPINA_Villarino_2017", sep = ""))
files <- dir()[grep("reformated_29_04_2021",dir())]
f <- files[4]
test <- get(load(f))
dim(test)
str(test)
# So, goal is to fill WoRMS_ID column based on 'ScientificName'
unique(test$ScientificName) ; str(test$ScientificName) # has to be chr string
taxon_names <- as.character(unique(test$ScientificName))
taxon_names
w <- wormsbynames(taxon_names)
dim(w)
str(w)
colnames(w)
which(is.na())
ind.failed <- which(is.na(w[,1]))
taxon_names[ind.failed]

consol.w <- wormsconsolidate(w)
consol.w[consol.w$status == "unaccepted",]

w$ScientificName <- taxon_names

### Columns to retrieve from 'consol.w' and to add into the data table
colnames(w)
# lsid
# valid_name
# [13] "kingdom"           "phylum"            "class"            
# [16] "order"             "family"            "genus"
# And maybe: "isMarine" "isBrackish" "isFreshwater" "isTerrestrial" "isExtinct"
# Maybe add a column that says: isMarine and if not 'yes' return isBrackish/isFreshwater/isExtinct/isTerrestrial
# And for complete NA (e.g. Ihela racoviatzi), return 'no match in WoRMS'

# 

### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
