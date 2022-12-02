
##### ATLANTECO SCRIPT 1.1 ----------------------------------------------------------------------------------------------------------------------------
##### 27/04/2021: R Script to format CPR occurrence datasets following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#   - Read CPR occurrence dataset sent by David Johns et al. on the 20/04/21
#   - Combine the 3 .txt files: occurrence, event and extendedmeasurementorfact.txt
#	- Split CPR occurrence dataset into phyto vs. zoo (first for PHYTObase update, second for ZOObase update)
#   - Remove occurrences not at genus of species level (can be done at same time as above)
#   - Re-format to AtlantECO WP2 template? Maybe later

module load R/4.0.3 # To load latest R version on kryo

### Latest update: 28/04/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the 3 .txt files and merge 
occ <- read.table("occurrence.txt", sep = "\t", dec = ".", h = T)
dim(occ)
head(occ)
summary(occ)
# unique(occ$occurrenceStatus) # 'present' only...but then what about individualCount?

event <- read.table("event.txt", sep = "\t", dec = ".", h = T)
dim(event)
head(event)
summary(event)

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



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
