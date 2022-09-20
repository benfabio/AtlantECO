
##### ATLANTECO SCRIPT 1.9.2 ----------------------------------------------------------------------------------------------------------------------------
##### 02/05/2022: R Script to reformat the TARA Oceans ZooScan imaging data publihsed by Soviadan et al. (2021) - Progress in Oceanography © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Aims to:
#  - Read the excel sheet and reformat to AtlantECO WP2 template
#  - Provide AphiaID using WoRMS

### Latest update: 09/05/2022

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("readxl")
library("lubridate")

world <- map_data("world") 

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the data and reshape
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Plankton_Imaging") ; 
# dir() 
meta <- as.data.frame(read_excel("Metadata_Zooplankton_Article1-for_Fabio.xls", sheet = 3))
abund <- as.data.frame(read_excel("Metadata_Zooplankton_Article1-for_Fabio.xls", sheet = 1))
biom <- as.data.frame(read_excel("Metadata_Zooplankton_Article1-for_Fabio.xls", sheet = 2))
# dim(meta)
# dim(abund)
# dim(biom)
# All have same n rows, good.

### Couple of things before melting the data.frame: convert 'Time' to 'Date' in 'meta'
# str(meta) ; colnames(meta)
# Need to convert to numerics
meta[,c(2:5,12:13)] <- apply(meta[,c(2:5,12:13)], 2, as.numeric)
# str(abund) ; colnames(abund)
abund[,c(2:length(abund))] <- apply(abund[,c(2:length(abund))], 2, as.numeric)
# str(biom) ; colnames(biom)
biom[,c(2:length(biom))] <- apply(biom[,c(2:length(biom))], 2, as.numeric)

### Convert Date to date
meta$Date <- as.Date(meta$Date)
# str(meta$Date) ; summary(meta$Date)

### Cbind metadata to both 'abund' & 'biom'
# setdiff(colnames(meta), colnames(abund))
# Add those to 'abund' & 'biom'
abund[,setdiff(colnames(meta), colnames(abund))] <- meta[,setdiff(colnames(meta), colnames(abund))]
biom[,setdiff(colnames(meta), colnames(biom))] <- meta[,setdiff(colnames(meta), colnames(biom))]
# Make sure they follow the same order
head(abund[,c(1:5)]) # tail(abund[,c(1:5)])
head(biom[,c(1:5)]) # tail(abund[,c(1:5)])

# OK melt
# colnames(abund)
# colnames(biom)
names <- colnames(abund)[c(1:5,25:33)] # id.vars for the melt
m.abund <- melt(abund, id.vars = names)
m.biom <- melt(biom, id.vars = names)
# head(m.abund) ; head(m.biom)
colnames(m.abund)[15] <- "ScientificName"
colnames(m.biom)[15] <- "ScientificName"
# unique(m.abund$ScientificName) ; unique(m.biom$ScientificName)
# head(m.biom)

### OK, can reformat to AtlantECO WP2 template :-) 
m.abund$Year <- lubridate::year(m.abund$Date)
m.abund$Month <- lubridate::month(m.abund$Date)
m.abund$Day <- lubridate::day(m.abund$Date)
m.biom$Year <- lubridate::year(m.biom$Date)
m.biom$Month <- lubridate::month(m.biom$Date)
m.biom$Day <- lubridate::day(m.biom$Date)

### Reshape to AtlantECO WP2 data format
colnames(m.abund)
tara <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Plankton_imaging", ContactName = "Fabio_Benedetti;Lars_Stemmann",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;lars.stemmann@imev-mer.fr", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = m.abund$Latitude, decimalLongitude = m.abund$Longitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = m.abund$Date,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = m.abund$Year, Month = m.abund$Month, Day = m.abund$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA (15min)", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = m.abund$Zmin, MaxDepth = m.abund$Zmax, 
                ParentEventID = m.abund$Station_Nets, EventID = m.abund$SampleID, InstitutionCode = "SU/CNRS-IMEV-LOV (UMR7093)", SourceArchive = "Not_applicable", 
                OrigCollectionCode = "Not_applicable", OrigCollectionID = "Not_applicable",
                BiblioCitation = "Soviadan et al. (2021) - Progress in Oceanography", CitationDOI = "https://doi.org/10.1016/j.pocean.2021.102717", DateDataAccess = "Not_applicable",
                OrigScientificName = m.abund$ScientificName, ScientificName = m.abund$ScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = NA, Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = m.abund$value,
                MeasurementUnit = "#/m3", MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = m.biom$value,
                BiomassConvFactor = "a*S^(b); S = surface in mm2; Lehette & Hernandez-Leon (2009) https://doi.org/10.4319/lom.2009.7.304",
                basisOfRecord = "Depth stratified Hydrobios Multinet tow; 300µm; O.25m2",
                SamplingProtocol = "Hydrobios Multinet; buffered formaldehyde-Borax solution (4%); MOTODA box split; ZooScan imaging system", 
                MeshSize = 300, SampleAmount = m.abund$Volume, SampleAmountUnit = "m3", SampleEffort = NA,
                DeterminedBy = "Quantitative imaging platform of Villefranche (PIQv) - https://lov.imev-mer.fr/web/facilities/piqv/",
                DeterminedDate = NA, Note = m.abund$Sample_comments, Flag = "Biomasses in microgramC/m3 not mgC/m3 !" 
) # eo ddf
# head(tara)
dim(tara) # 5415
str(tara)

# Check scientific names used
tara$ScientificName <- as.character(tara$ScientificName)
# unique(tara$ScientificName) 
tara[tara$ScientificName == "Metrinidae","ScientificName"] <- "Metridinidae"
tara[tara$ScientificName == "Other Copepoda","ScientificName"] <- "Copepoda"
tara[tara$ScientificName == "Crustacean larvae","LifeForm"] <- "Nauplius larvae"
tara[tara$ScientificName == "Cladocera","ScientificName"] <- "Diplostraca"

### And finally, provide AphiaID using 'worms' functions
require("worms")
keys.worms <- wormsbynames( unique(tara$ScientificName), marine_only = "false")
keys.worms$ScientificName <- unique(tara$ScientificName)

# Add WoRMS_status field
tara <-  add_column(tara, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(tara)

# For testing:
s <- unique(tara$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(tara$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- tara[tara$ScientificName == s,]
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
dim(ddf) # 5415
ddf[3489:3499,]

unique(ddf$WoRMS_ID)
unique(ddf$WoRMS_status)
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS","ScientificName"]) # "Protista" & "Crustacean larvae"

### Add Bathymetry and Longhurst provinces
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15)
ras.bathy <- as.xyz(bathy)
colnames(ras.bathy) <- c("x","y","z")
ras.bathy <- rasterFromXYZ(ras.bathy)
# plot(ras.bathy)
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Longhurst_world_v4_2010")
BGCP.Longhurst <- raster::raster("Longhurst_world_v4_2010_04_04_22.grd")
# Go back to proper dir
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Plankton_Imaging/") 
# Add bathymetry
ddf$Bathymetry <-  raster::extract(x = ras.bathy, y = ddf[,c("decimalLongitude","decimalLatitude")])
summary(ddf$Bathymetry)

# Add Longhurst provinces
bgcp <- as.data.frame(levels(BGCP.Longhurst))
bgcp[c(1:3)] <- sapply(bgcp[c(1:3)], as.character)
ddf$LonghurstProvince <- as.character(raster::extract(x = BGCP.Longhurst, y = ddf[,c("decimalLongitude","decimalLatitude")]))
unique(ddf$LonghurstProvince) ; summary(factor(ddf$LonghurstProvince))
# Use 'bgcp' to give BGCPs their actual names
for(i in unique(na.omit(ddf$LonghurstProvince)) ) {
    message(paste(i, sep = ""))
    name <- bgcp[bgcp$ID == i,"ProvDescr"]
    ddf[ddf$LonghurstProvince == i & !is.na(ddf$LonghurstProvince),"LonghurstProvince"] <- name
} # eo for loop - i in LonghurstProvince
# Check
summary(factor(ddf$LonghurstProvince))
# 95 NAs (too coastal points) --> (95/nrow(ddf))*100 = 1.75% of the data 


### Saving new file
save(ddf, file = "TARA_Oceans_multinet_zooscan_abundance+biom_Soviadan_et_al.2021.PiO_09_05_22.RData")
write.table(x = ddf, file = "TARA_Oceans_multinet_zooscan_abundance+biom_Soviadan_et_al.2021.PiO_09_05_22.txt", sep = "\t")
gc()

# Quick map
ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = ddf, alpha = .5) +
    scale_colour_brewer(name = "Source", palette = "Paired") + 
    coord_quickmap() + geom_polygon(aes(x = long, y = lat, group = group), 
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
