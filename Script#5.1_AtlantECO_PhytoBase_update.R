
##### ATLANTECO SCRIPT 5.1 ----------------------------------------------------------------------------------------------------------------------------
##### 01/04/2022: R Script to update the PhytoBase dataset (Righetti et al., 2020) with AtlantECO WP2 datasets from the 'Traditional data' silo ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load reformatted PhytoBase dataset, examine main sources and their identifiers
# - Re-clean GBIF data sources (lots of drilling holes data and surface sediment samples)
# - Add AtlantECO datasets (CPR surveys, MALASPINA etc.)
# - Add bathymetry and Longhurst provinces
# - Check, merge, homogenize, remove duplicates (?)

### Latest update: 08/04/2022

library("marmap")
library("raster")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("lubridate")
library("viridis")
library("here")

world <- map_data("world")  # for maps

### ----------------------------------------------------------------------------------------------------------------------------

### List the dataset to add to PhytoBase v1:
# Main CPR survey (Atl+Pac)
# Australian CPR
# Villarino et al. (2018) - phytoplankton 1/0
# 08/04/22: N. scintillans occurrences from SO-CPR

### ----------------------------------------------------------------------------------------------------------------------------


### 1°) Load PhytoBase v1
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/PHYTObase") ; dir()
# Load dataset
phyto <- get(load("PhytoBase_reformated+WoRMScheck_08_06_2021.Rdata"))
# dim(phyto) # 1'333'084 rows
# str(phyto)
# colnames(phyto)
# Add flag to map new data over older one in the end
phyto$version <- "PhytoBase v1"

# unique(phyto$WoRMS_ID)
# unique(phyto[phyto$WoRMS_ID == "No match found in WoRMS; unassessed AlgaeBase entry","ScientificName"])
# unique(phyto$ScientificName)
# nrow(phyto[phyto$WoRMS_ID == "No match found in WoRMS; unassessed AlgaeBase entry",]) / nrow(phyto)

# Map
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = phyto, colour = "red", alpha = .5) + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# Check sources
# unique(phyto$InstitutionCode)
# unique(phyto$DatasetKey) 
# unique(phyto$SourceArchive) 
# unique(phyto$MeasurementValue) # mix of quanti and quali
# summary(phyto[,c("Day","Month","Year")]) # Not even clean yet...crap

# Identify CPR data (SHAFOS & CSIRO) on the map
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude),
        data = phyto[phyto$InstitutionCode != "SAHFOS",], colour = "gray", alpha = .5) + 
    geom_point(aes(x = decimalLongitude, y = decimalLatitude),
        data = phyto[phyto$InstitutionCode %in% c("SAHFOS","CSIRO Marine and Atmospheric Research, Australia (CMAR)"),], colour = "red", alpha = .5) + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "#4d4d4d", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

# Check if CPR data are quantitative?
# unique(phyto[phyto$InstitutionCode %in% c("SAHFOS","CSIRO Marine and Atmospheric Research, Australia (CMAR)"),"MeasurementValue"]) # mix...
# unique(phyto[phyto$InstitutionCode %in% c("CSIRO Marine and Atmospheric Research, Australia (CMAR)"),"MeasurementValue"])
# unique(phyto[phyto$InstitutionCode %in% c("SAHFOS"),"MeasurementValue"]) # Classic CPR: presence only#
# unique(phyto[phyto$InstitutionCode %in% c("CSIRO Marine and Atmospheric Research, Australia (CMAR)"),"MeasurementValue"])
### Map above matches maps from Righetti et al. (2020) - Fig. 7

### --> Remove c("SAHFOS","CSIRO Marine and Atmospheric Research, Australia (CMAR)") data (to be replaced with updated ones)


### Use 'DatasetKey' of GBIF and OBIS to inform dataset type (title, citation)
keys <- phyto$DatasetKey
ddf.keys <- as.data.frame(do.call(rbind, str_split(string = keys, pattern = "; ")))
# Adjust format etc.
colnames(ddf.keys) <- c("GBIF","OBIS")
# Remove 'GBIF__' and 'OBIS__'
ddf.keys$GBIF <- str_replace_all(ddf.keys$GBIF, "GBIF__", "")
ddf.keys$OBIS <- str_replace_all(ddf.keys$OBIS, "OBIS__", "")
# OK, just convert some NAs to actual NA
ddf.keys[ddf.keys == "NA"] <- NA
# unique(ddf.keys$GBIF)
# unique(ddf.keys$OBIS)
ddf.keys$orig.key <- keys
# Add GBIF.key and OBIS.key as new columns in phytobase
phyto <- phyto %>% add_column(GBIF.key = ddf.keys$GBIF, .after = "DatasetKey")
phyto <- phyto %>% add_column(OBIS.key = ddf.keys$OBIS, .after = "GBIF.key")
# colnames(phyto)

### Identify OBIS.key to discard because they correspond to CPR data
#unique(phyto$OBIS.key)
#contribs <- data.frame(phyto %>% group_by(OBIS.key) %>% summarize(n = n()) )
#contribs[order(contribs$n, decreasing = T),] # Main level is NA...great...
# Top 2: 2546, 2547
#
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude),
#             data = phyto[phyto$OBIS.key == "2546",], colour = "red", alpha = .5) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude),
#             data = phyto[phyto$OBIS.key == "2505",], colour = "blue", alpha = .5) +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "#4d4d4d", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

# 2547: --> CPR, to remove !
# 2546: --> mix of various stuff...examine

# unique(phyto[phyto$OBIS.key == "2546","InstitutionCode"])
# NODC, Pangaea & NA...what's in the NA...
# NODC WOD01 Plankton Database 
# But is also in the 2546 with InstitutionCode == "NODC"? ....

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude),
#             data = phyto[phyto$OBIS.key == "2546" & is.na(phyto$InstitutionCode),], colour = "red", alpha = .5) +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "#4d4d4d", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
# ### OK, identify the bounding box of the CPR samples from dataset #2546 (clearly CPR tracks too)
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude),
#             data = phyto[phyto$OBIS.key == "2546" & is.na(phyto$InstitutionCode),], colour = "red", alpha = .5) +
#     geom_rect(aes(xmax = 3.5, xmin = -75, ymax = 67, ymin = 30), fill = "#b2df8a", alpha = .5, colour = "black") +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "#4d4d4d", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

### --> Just leave the NODC dataset in there - it will laso be in GBIF anyway - will use date and coord and WoRMS_ID to discard duplicates
### --> Just keep '2546'

### From GBIF keys: use rgbif::datasets() to identify datasets that correspond to CPR + drilling holes (still too many in there)
require("parallel")
require("rgbif")
require("robis")

res <- mclapply(unique(na.omit(ddf.keys$GBIF)), function(i) {
    
            message(paste(i, sep = ""))
            dataset <- rgbif::datasets(data = "all", uuid = i)
            title <- dataset$data$title
            citation <- dataset$data$citation$text
            project <- dataset$data$project$title
    
            # Use if else loop with is.null
            if( is.null(title) ) {
                title <- 'No title found'
            }
            if( is.null(citation) ) {
                citation <- 'No citation found'
            }
            if( is.null(project) ) {
                project <- 'No project found'
            }
            
            # Return
            return( data.frame(key = i, title = title, citation = citation, project = project) )
    
    }, mc.cores = 25
)    
gbif.datasets <- bind_rows(res)
head(gbif.datasets) ; dim(gbif.datasets)
rm(res) ; gc()

### OK, now identify the keys of the datasets to remove
unique(gbif.datasets$title)

keywords.cpr <- c("Continuous Plankton Recorder","AusCPR")

keywords.fossils <- c("ODP Hole","DSDP Hole","Hole 81","Hole 84","surface sediment samples",
            "ODP Site","Site 175-1075","Hole 302","palynological","nannofossils","sediment core"," Hole ","fossil","site GIK23414",
            "surface sediments of the Arabian Sea","site GIK2325","surface sediments","NU2_trap","DSDP Site","site GIK23259",
            "Core SO82","Sediment components","deep-sea sediments","site GIK23414","Trap data","sediment trap","palynomorpha",
            "surface sediments of cores","Site 177-1092","mooring MST-8","mooring MST-9","palinológica","dinoflagellate cysts of multicorer",
            "cysts of surface sediments","multicorer surface","cysts from surface sediments","sediment trap MST-9","Terrestrial and Limnetic",
            "trap WR2","Dinoflagellate cyst species composition of recent surface samples","sediment profile PS1730",
            "grain fraction > 20 µm","grain fraction < 20 µm")
            
# Vector of datasets' title to remove
keys2remove.cpr <- gbif.datasets$title[grepl(paste(keywords.cpr, collapse = "|" ), gbif.datasets$title)] # length(keys2remove.cpr) 
keys2remove.foss <- gbif.datasets$title[grepl(paste(keywords.fossils, collapse = "|" ), gbif.datasets$title)] # length(keys2remove.foss) 
keys2remove <- c(keys2remove.cpr, keys2remove.foss)

# To check those that are NOT to remove
gbif.datasets$title[!(gbif.datasets$title %in% keys2remove)]
length(c(keys2remove.cpr,keys2remove.foss)) # 713 keys
length(gbif.datasets$title) # 1136
# 710/1136 --> 62.5% of all GBIF datasets are actually sediment data (cores from drilling hole or sediment trap)...

### Check datasets in 'gbif.datasets'
# gbif.datasets[gbif.datasets$title %in% keys2remove,"project"]
# gbif.datasets[!(gbif.datasets$title %in% keys2remove),"project"]
# gbif.datasets[!(gbif.datasets$title %in% keys2remove),"citation"]

# Add flag: to remove or to keep?
gbif.datasets$keep <- TRUE
gbif.datasets[gbif.datasets$title %in% keys2remove,"keep"] <- FALSE
# summary(gbif.datasets$keep)

# Define keys to keep and map them
keys2keep <- gbif.datasets[gbif.datasets$keep == T,"key"]
keys2rm <- gbif.datasets[gbif.datasets$keep == F,"key"]

### 05/04/22: Use 'gbif.datasets' (title & citation) to provide original data source info to PhytoBase
str(phyto)
unique(phyto$OrigCollectionCode) # Only NaN --> 'title'
unique(phyto$OrigCollectionID) # Only NaN --> 'citation'

### Provide to PhytoBase in a simple for loop 
# For testing:
# k <- unique(na.omit(ddf.keys$GBIF))[3] ; k
for(k in unique(na.omit(ddf.keys$GBIF)) ) {
        message(paste(k, sep = ""))
        code <- gbif.datasets[gbif.datasets$key == k,"title"]
        cit <- gbif.datasets[gbif.datasets$key == k,"citation"]
        # Provide to PhytoBase
        phyto[phyto$GBIF.key == k & !is.na(phyto$GBIF.key),"OrigCollectionCode"] <- code
        phyto[phyto$GBIF.key == k & !is.na(phyto$GBIF.key),"OrigCollectionID"] <- cit
}
# Check
unique(phyto$OrigCollectionCode) 
unique(phyto$OrigCollectionID) 
# Worked fine.

### Instead of removing "Records collected from sediment core or trap", add the latter 'Flag' level to PhytoBase
# unique(phyto[phyto$OrigCollectionCode %in% keys2remove.foss,"Flag"]) # Some were actually arealdy marked down as sediment trap data
# dim(phyto[phyto$OrigCollectionCode %in% keys2remove.foss,])
phyto[phyto$OrigCollectionCode %in% keys2remove.foss,"Flag"] <- "Records collected from sediment core or trap"

### Still looks like some CPR remains in there
contribs <- data.frame(phyto[phyto$GBIF.key %in% keys2keep,] %>% group_by(GBIF.key) %>% summarize(n = n()))
contribs[order(contribs$n, decreasing = T),] # Main level is NA...great...

### Check
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude),
            data = phyto[phyto$GBIF.key == "838e2626-f762-11e1-a439-00145eb45e9a",], colour = "red", alpha = .5) + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "#4d4d4d", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude),
            data = phyto[phyto$OBIS.key == "2546",], colour = "blue", alpha = .5) + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "#4d4d4d", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

# gbif.datasets[gbif.datasets$key == "838e2626-f762-11e1-a439-00145eb45e9a",]
# --> NODC WOD01 Plankton Database
### !!! is also in OBIS --> overlap...
dim(phyto[phyto$GBIF.key == "838e2626-f762-11e1-a439-00145eb45e9a",]) # 710 203 occurrences
dim(phyto[phyto$OBIS.key == "2546",]) # 930 904 occurrences...

ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude),
            data = phyto[phyto$OBIS.key == "2546",], colour = "red", alpha = .5) + # OBIS = RED
    geom_point(aes(x = decimalLongitude, y = decimalLatitude),
            data = phyto[phyto$GBIF.key == "838e2626-f762-11e1-a439-00145eb45e9a",], colour = "blue", alpha = .5) + # GBIF = BLUE
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "#4d4d4d", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# OK, just remove what's in blue then (GBIF.key == "838e2626-f762-11e1-a439-00145eb45e9a")

### Compare species list
sub1 <- phyto[phyto$OBIS.key == "2546" & !is.na(phyto$OBIS.key),] # to keep - should have more species
sub2 <- phyto[phyto$GBIF.key == "838e2626-f762-11e1-a439-00145eb45e9a" & !is.na(phyto$GBIF.key),] # to remove
dim(phyto[which(phyto$GBIF.key == "838e2626-f762-11e1-a439-00145eb45e9a"),])
# Assess temporal overlap 
#summary(sub1[,c("Month","Year")]) # longer
#summary(sub2[,c("Month","Year")])

# test length of species list (WoRMS_ID)
# length(unique(sub1$WoRMS_ID)) # 863
# length(unique(sub2$WoRMS_ID)) # 760
# length(unique(sub1$WoRMS_ID)) - length(unique(sub2$WoRMS_ID)) # 103 more in sub1
# # Check species lists
# unique(sub1$ScientificName) # 884 ScientificName
# unique(sub2$ScientificName) # 793 ScientificName
# # Assess % of overlap!
# intersect(unique(sub2$ScientificName),unique(sub1$ScientificName)) # 722 matching names
# setdiff(unique(sub1$ScientificName), unique(sub2$ScientificName)) # Returns those sub1 that are NOT in sub2 (should be > 150)
# setdiff(unique(sub2$ScientificName), unique(sub1$ScientificName)) # Returns those sub2 that are NOT in sub1 # should be fewer

### Species in sub2 that were not observed in sub1
sub22keep <- sub2[sub2$ScientificName %in% setdiff(unique(sub2$ScientificName), unique(sub1$ScientificName)),]
# dim(sub22keep) # 17340 only
rm(sub1);gc()

### 05/04/22: Action plan to update (and further clean) PhytoBase

### Use 'gbif.datasets' data.frame to complete 'phyto' (provide 'title' and 'citation' in one of the empty columns) - DONE above (line 171 and onwards)
### Add sediment/trap data Flag based on that (1183202,  don't discard the corresponding observations though) - DONE above (line 266 and onwards)

### Remove observations from the GBIF version of the WODC Plankton Database (GBIF.key == "838e2626-f762-11e1-a439-00145eb45e9a") but retain 'sub22keep' (ScientificNames that are not in the NODC from OBIS)
phyto2 <- phyto[-which(phyto$GBIF.key == "838e2626-f762-11e1-a439-00145eb45e9a"),]
phyto2 <- rbind(phyto2,sub22keep)
dim(phyto2) # 1 183 202

### Remove c("SAHFOS","CSIRO Marine and Atmospheric Research, Australia (CMAR)") data using 'InstitutionCode'
# unique(phyto2$InstitutionCode)
phyto3 <- phyto2[-which(phyto2$InstitutionCode %in% c("SAHFOS","CSIRO Marine and Atmospheric Research, Australia (CMAR)")),]
dim(phyto3) # 897 479 (removed 285723, 75.8% of previous phyto)

### Remove previous CPR data using 'keys2remove.cpr' (based on 'OrigCollectionCode')
# dim(phyto3[phyto3$OrigCollectionCode %in% keys2remove.cpr,]) # still 226'265
phyto4 <- phyto3[-which(phyto3$OrigCollectionCode %in% keys2remove.cpr),]
dim(phyto4) # 671 214 --> further removed 226 265; 74.7% of remaining data
# Quickmap to check 
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = phyto4, colour = "red", alpha = .5) + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

### Effectively removed CPR data, except the few that are still part of of the WODC Plankton Database

# Check MeasurementType again
unique(phyto4$MeasurementType) # 
unique(phyto4$MeasurementValue)[order(unique(phyto4$MeasurementValue))]
### Next: need to convert this numerics and then 1/0...
#dim(phyto4[phyto4$MeasurementValue == "0",])
#dim(phyto4[phyto4$MeasurementValue == "Presence",])
### Check whether there are zeroes (absences) in the quantitative measurements 
numbers <- readr::parse_number(unique(phyto4[phyto4$MeasurementValue != "Presence" & !is.na(phyto4$MeasurementValue),"MeasurementValue"]))
min(numbers) # 0.01? but not 0
 
phyto4$MeasurementType <- "Occurrence"
phyto4$MeasurementValue <- "Presence"

# Save temporary file
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
save(phyto4, file = "tempo_file_PhytoBasev2_toupdate_05_04_22.RData")
# dim(phyto4) # 671 214 occs

### ---------------------------------------------------------

### 05/04/22: Add updated CPR curveys
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2") ; dir()
cpr.update <- get(load("CPR_phytoplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
dim(cpr.update) # 755168
str(cpr.update)
unique(cpr.update$ScientificName)

### Add version column and have matching colnames between the 2 datasets
cpr.update$version <- "PhytoBase v2"
# Add GBIF.key and OBIS.key as new columns to match phytobase
cpr.update <- cpr.update %>% add_column(GBIF.key = NA, .after = "DatasetKey")
cpr.update <- cpr.update %>% add_column(OBIS.key = NA, .after = "GBIF.key")
# setdiff(colnames(cpr.update), colnames(phyto4))
# Drop obisID from 'cpr.update'
cpr.update <- select(cpr.update, -obisID)

### Check range of values
#summary(cpr.update$MeasurementValue) # 27.9% are NaN
#head(cpr.update[is.na(cpr.update$MeasurementValue),])
cpr.update <- cpr.update[!is.na(cpr.update$MeasurementValue),] # Remove missing values in 'MeasurementValue'
# dim(cpr.update)

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cpr.update, colour = "red", alpha = .5) +
#     coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

cpr.update$MeasurementType <- "Occurrence"
cpr.update$MeasurementValue <- "Presence"

### Rbind with phyto4?
phyto5 <- rbind(phyto4,cpr.update)
dim(phyto5) # 1'215'445 occurrences

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = phyto5, alpha = .5) +
#      coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#          data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#      scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#          labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#      scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#          labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#          panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

### ---------------------------------------------------------

### 05/04/22: Add updated AusCPR
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
auscpr.update <- get(load("AusCPR_phyto_reformatted+WoRMScheck_27_10_21.Rdata"))
dim(auscpr.update) # 3'863'540
str(auscpr.update)
unique(auscpr.update$OrigScientificName) # unique(auscpr.update$ScientificName)

auscpr.update$version <- "PhytoBase v2"
# Add GBIF.key and OBIS.key as new columns to match phytobase
auscpr.update <- auscpr.update %>% add_column(GBIF.key = NA, .after = "DatasetKey")
auscpr.update <- auscpr.update %>% add_column(OBIS.key = NA, .after = "GBIF.key")
auscpr.update <- select(auscpr.update, -obisID)
# setdiff(colnames(auscpr.update), colnames(phyto4))

### Check range of values
# summary(auscpr.update$MeasurementValue)  # No NaN or -999
# min(auscpr.update$MeasurementValue)
# summary(as.integer( auscpr.update$MeasurementValue > 0.0 ))
# Convert to integers (1/0) and then 'Presence/Absence'
auscpr.update$MeasurementType <- "Occurrence"
auscpr.update$MeasurementValue <- as.integer( auscpr.update$MeasurementValue > 0.0 )
auscpr.update$MeasurementValue <- as.character(auscpr.update$MeasurementValue)
# summary(factor(auscpr.update$MeasurementValue))
auscpr.update[auscpr.update$MeasurementValue == "1","MeasurementValue"] <- "Presence"
auscpr.update[auscpr.update$MeasurementValue == "0","MeasurementValue"] <- "Absence"
# Good, add to phyto5

phyto6 <- rbind(phyto5,auscpr.update)
dim(phyto6) # 5'078'985

 # ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = phyto6, alpha = .5) +
 #      coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
 #          data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
 #      scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
 #          labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
 #      scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
 #          labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
 #      theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
 #          panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

# Make room
rm(phyto,phyto2,phyto3,phyto4,sub2,sub22keep,ddf.keys,ddf,cpr.update,auscpr.update,gbif.datasets); gc()

### ---------------------------------------------------------

### 06/04/22: Add MALASPINA
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/MALASPINA_Villarino_2017") ; dir()
mala.diato <- get(load("MALASPINA_Villarinoetal._2017_diatoms_reformated+WoRMScheck_01_06_2021.Rdata"))
mala.dino <- get(load("MALASPINA_Villarinoetal._2017_dinoflagellates_reformated+WoRMScheck_01_06_2021.Rdata"))
mala.coccos <- get(load("MALASPINA_Villarinoetal._2017_coccolithophores_reformated+WoRMScheck_01_06_2021.Rdata"))
# dim(mala.diato); dim(mala.dino); dim(mala.coccos)
# Rbind
mala.phyto <- rbind(mala.diato, mala.dino, mala.coccos) 
rm(mala.diato,mala.dino,mala.coccos) ; gc()
dim(mala.phyto) # 36'922
str(mala.phyto)

# Have matching coluln headers
mala.phyto$version <- "PhytoBase v2"
mala.phyto <- mala.phyto %>% add_column(GBIF.key = NA, .after = "DatasetKey")
mala.phyto <- mala.phyto %>% add_column(OBIS.key = NA, .after = "GBIF.key")
#mala.phyto <- select(auscpr.update, -obisID)
#setdiff(colnames(mala.phyto), colnames(phyto6))

# Check measurement values
unique(mala.phyto$MeasurementType)
unique(mala.phyto$MeasurementValue)
# Seems good already 
unique(mala.phyto$OrigScientificName)
unique(mala.phyto$LifeForm)

### Rbind with phyto6 already
phyto7 <- rbind(phyto6,mala.phyto)
dim(phyto7) # 5'115'907

# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = phyto7, alpha = .5) +
#       coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#           data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#       scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#           labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#       scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#           labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#       theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#           panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

### Save temporary file (finish tomorrow)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
save(phyto7, file = "tempo_file_PhytoBasev2_toupdate_05_04_22.RData")
rm(phyto6,mala.phyto) ; gc()

# Check some stuff
str(phyto7)
unique(phyto7$SourceArchive)
unique(phyto7$eventDate) # To add based on c("Day","Month","Year")
summary(phyto7[,c("Day","Month","Year")]) # To correct! (missing day, wrong years...)


### ---------------------------------------------------------


### 06/04/22: Add bathymetry & Longhurst provinces (from the newly made .grd)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()
phyto <- get(load("tempo_file_PhytoBasev2_toupdate_05_04_22.RData"))
# dim(phyto) # 5'115'907 occurrences
# colnames(phyto)

### Need to add 'Bathymetry' and 'LonghurstProvince'
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15)
ras.bathy <- as.xyz(bathy)
colnames(ras.bathy) <- c("x","y","z")
ras.bathy <- rasterFromXYZ(ras.bathy)
# plot(ras.bathy)
# To get Longhurst provinces: 
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors/Longhurst_world_v4_2010")
BGCP.Longhurst <- raster::raster("Longhurst_world_v4_2010_04_04_22.grd")
# BGCP.Longhurst ; plot(BGCP.Longhurst)
### Change the values in the raster to actual names?
values(BGCP.Longhurst)[values(BGCP.Longhurst) == 3 & !is.na(values(BGCP.Longhurst))] = factor("Polar - Atlantic Subarctic Province")

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()

### Add bathymetry
unique(phyto$BathySource)
phyto$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(phyto[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
phyto$Bathymetry <- raster::extract(x = ras.bathy, y = phyto[,c("decimalLongitude","decimalLatitude")])
# Check
# summary(phyto$Bathymetry)
# Some points in hiugh altitude apparently 
# nrow(phyto[phyto$Bathymetry > 0,]) # 150052 points
# Check precision of the coordinates (too high or too low?)
# summary(phyto[phyto$Bathymetry > 0,c("decimalLongitude","decimalLatitude")])
# unique(phyto[phyto$Bathymetry > 0,c("decimalLongitude")])
# unique(phyto[phyto$Bathymetry > 0,c("decimalLatitude")])

# # Check their position
# ggplot() + coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#            data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = phyto[phyto$Bathymetry > 0,], alpha = .5, colour = "red") +
#        scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#        scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#            labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#            panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")

### --> very coastal points ; the high precision of the spatial coordinates interfered with the lower resolution of the bathymetry product

### Add Longhurst provinces: since the raster only has the provinces' indices, first extract those (maybe as characters, easier for replacement after) and then convert them back to the actual provinces' names using the levels of the raster layer:
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
# str(bgcp)
phyto$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = phyto[,c("decimalLongitude","decimalLatitude")]))
# unique(phyto$LonghurstProvince) # summary(factor(phyto$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
# i <- "46"  # for testing 
for(i in unique(na.omit(phyto$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    phyto[phyto$LonghurstProvince == i & !is.na(phyto$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
# summary(factor(phyto$LonghurstProvince))
# 231291 NaN (probably too coastal points) --> (231291/nrow(phyto))*100 = 4.52% of the data only
# Check their position 
# ggplot() + coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
#            data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = phyto[is.na(phyto$LonghurstProvince),], alpha = .5, colour = "red") +
#        scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#        scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#            labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#            panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "none")
# --> same as bathymetry: coordinates too precise and close to coast compared to the BGCPs product's resolution 
# Good. Finish it up

### Clean stuff: check coords, dates etc.
summary(phyto[,c("Day","Month","Year")]) 
# Attribute NaN to day < 0 and year < 1000
phyto[phyto$Day < 1 & !is.na(phyto$Day),"Day"] <- NA
phyto[phyto$Year < 1000 & !is.na(phyto$Year),"Year"] <- NA
# dim(phyto)
# Drop occs with NA in Year
phyto <- phyto %>% drop_na(Year)

# unique(phyto$eventDate) 
# Convert to NA; For occs with D-M-Y -> create date
dates <- lubridate::dmy(paste(phyto$Day, phyto$Month, phyto$Year, sep = "-")) 
# unique(dates)
phyto$eventDate <- dates # str(phyto$eventDate)
# Good

# Check depths
# summary(phyto[,c("Depth","DepthAccuracy","DepthIntegral","MinDepth","MaxDepth")]) 
# All iz OK

### Drop unecessary columns?
colnames(phyto)
phyto <- select(phyto,-c(Biomass_mgCm3,BiomassConvFactor))
phyto <- select(phyto,-c(ProjectID,ProjectWP,DataSilo))

### Attribute an occurrenceID? Let's see that latter...for now save
save(phyto, file = "AtlantECO-BASEv1_dataset_PhytoBasev2_occurrences_06_04_22.RData")
write.table(phyto, file = "AtlantECO-BASEv1_dataset_PhytoBasev2_occurrences_06_04_22.csv", sep = ";")

### Print a map of occ distrib by colouring as a fun of PhytoBase version
map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = phyto, alpha = .5) +
    scale_colour_manual(name = "Version", values = c("#3288bd","#d53e4f")) + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
               data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
            labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
            panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
ggsave(plot = map, filename = "map_PhytoBasev2_occurrences_08_04_22.jpg", dpi = 300, width = 15, height = 12)



### ---------------------------------------------------------


### 08/04/22: Add the N. scintillans occurrences from the SO-CPR!
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence")
phyto <- get(load("AtlantECO-BASEv1_dataset_PhytoBasev2_occurrences_06_04_22.RData"))
# dim(phyto) # 5115565 occurrences
# Load SO-CPR data
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") #; dir() 
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
so.cpr.sub <- so.cpr[so.cpr$ScientificName == "Noctiluca scintillans",]
dim(so.cpr.sub) # 51717
# Check colnames
setdiff(colnames(phyto), colnames(so.cpr.sub))
setdiff(colnames(so.cpr.sub), colnames(phyto))

so.cpr.sub <- so.cpr.sub %>% add_column(version = "PhytoBase v2", .after = "Flag")
so.cpr.sub <- so.cpr %>% add_column(GBIF.key = "Not_applicable", .after = "DatasetKey")
so.cpr.sub <- so.cpr.sub %>% add_column(OBIS.key = "Not_applicable", .after = "GBIF.key")

# Drop: [1] "obisID"              "Temperature_Celsius" "Salinity"           
# [4] "PAR_microE_m2_s1"    "Fluorescence"        "PCI" 
so.cpr.sub <- select(so.cpr.sub, -c(obisID,Temperature_Celsius,Salinity,PAR_microE_m2_s1,Fluorescence,PCI,ProjectID,ProjectWP,DataSilo,Biomass_mgCm3,BiomassConvFactor))
# Add missing cols
so.cpr.sub$MeasurementType <- "Occurrence"
so.cpr.sub$MeasurementUnit <- NA
so.cpr.sub$MeasurementValue <- as.integer( so.cpr.sub$MeasurementValue > 0.0 )
so.cpr.sub$MeasurementValue <- as.character(so.cpr.sub$MeasurementValue)
so.cpr.sub[so.cpr.sub$MeasurementValue == "1","MeasurementValue"] <- "Presence"
so.cpr.sub[so.cpr.sub$MeasurementValue == "0","MeasurementValue"] <- "Absence"

# Rbind?
phyto2 <- rbind(phyto,so.cpr.sub)
# dim(phyto2) # 5167282
# summary(phyto2[,c("Day","Month","Year")]) 

### Need to add 'Bathymetry' and 'LonghurstProvince'
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15)
ras.bathy <- as.xyz(bathy)
colnames(ras.bathy) <- c("x","y","z")
ras.bathy <- rasterFromXYZ(ras.bathy)
# plot(ras.bathy)
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors/Longhurst_world_v4_2010")
BGCP.Longhurst <- raster::raster("Longhurst_world_v4_2010_04_04_22.grd")
# BGCP.Longhurst ; plot(BGCP.Longhurst)

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence") ; dir()

### Add bathymetry
unique(phyto2$BathySource)
phyto2$BathySource <- "ETOPO1-NOAA (15min)"
# ?raster::extract
# summary(phyto[,c("decimalLongitude","decimalLatitude")]) # All coords check out. use extract()
# raster::extract(x = ras.bathy, y = phyto[1:100,c("decimalLongitude","decimalLatitude")])
phyto2$Bathymetry <- raster::extract(x = ras.bathy, y = phyto2[,c("decimalLongitude","decimalLatitude")])

### Add Longhurst provinces: since the raster only has the provinces' indices, first extract those (maybe as characters, easier for replacement after) and then convert them back to the actual provinces' names using the levels of the raster layer:
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
# str(bgcp)
phyto2$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = phyto2[,c("decimalLongitude","decimalLatitude")]))
# unique(phyto2$LonghurstProvince) ; summary(factor(phyto2$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
# i <- "46"  # for testing 
for(i in unique(na.omit(phyto2$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    phyto2[phyto2$LonghurstProvince == i & !is.na(phyto2$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
# summary(factor(phyto2$LonghurstProvince))
# 231335 NAs (too coastal points) --> (231335/nrow(phyto2))*100 = 4.47% of the data 

### Attribute an occurrenceID? Let's see that latter...for now save
save(phyto2, file = "AtlantECO-BASEv1_dataset_PhytoBasev2_occurrences_08_04_22.RData")
write.table(phyto2, file = "AtlantECO-BASEv1_dataset_PhytoBasev2_occurrences_08_04_22.csv", sep = ";")

### Print a map of occ distrib by colouring as a fun of PhytoBase version
map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(version)), data = phyto2, alpha = .5) +
    scale_colour_manual(name = "Version", values = c("#3288bd","#d53e4f")) + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group),
               data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
            labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
            panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
ggsave(plot = map, filename = "map_PhytoBasev2_occurrences_08_04_22.jpg", dpi = 300, width = 15, height = 12)



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------