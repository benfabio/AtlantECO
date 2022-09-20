
##### ATLANTECO SCRIPT 1.6.1 ----------------------------------------------------------------------------------------------------------------------------
##### 08/10/2021: R Script to reformat the MAREDAT data published by Berdnaczek et al. (2012) - ESSD © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Aims to:
#  - Read the excel sheet (or the .csv file) and reformat to AtlantECO WP2 template
#  - Provide AphiaID and classification using WoRMS' R package 'worms'

### Latest update: 08/10/2021

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("readxl")
library("lubridate")

world <- map_data("world")  # for maps

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the data and reshape
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Traditional/MAREDAT files/Berdnaczek_Pteropods_PANGAEA.777387_(PDI-1447)") ; 
dir() 
data <- read_excel("MAREDAT_Pteropoda_Berdnaczek2012_data2rfrmt.xlsx", sheet = 1)
dim(data); str(data)

### Or the .csv: 
data <- read.csv("MAREDAT_Pteropoda_Berdnaczek2012_data2rfrmt.csv", h = T, sep = ";", dec = ",")
dim(data); str(data)

### Check data.frame
dim(data) # 8814 points
head(data)
str(data)
summary(data) 
data[data$decimalLatitude > 100,]
# 4 points from Rogachev et al. (2008) have a latitude of 138.12 
# Manually correct by checking: https://www.sciencedirect.com/science/article/abs/pii/S0278434308001854?via%3Dihub 

# Quick map
ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = DecimalLongitude, y = decimalLatitude), data = data) + coord_quickmap() +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 
### The Arctic Ocean has never been so high

### Fill in the 'Date' column for those obs with Day-Month-Year, and convert to 'Date' vector
data$Date <- NA
data[!is.na(data$Day),"Date"] <- paste(data[!is.na(data$Day),"Day"], data[!is.na(data$Day),"Month"], data[!is.na(data$Day),"Year"], sep = "-")
unique(data$Date)
# ?as.Date
as.Date(x = data$Date, tryFormats = "%d-%m-%Y")[1000:1100] ; data$Date[1000:1100] # Cool works fine
data$Date <- as.Date(x = data$Date, tryFormats = "%d-%m-%Y")
str(data) ; summary(data$Date)
# Does it work without the Day though? 
# as.Date(paste(data[!is.na(data$Day),"Month"], data[!is.na(data$Day),"Year"], sep = "-"), tryFormats = "%m-%Y") # Nope, well I tried

### The data.frame is already kinda following a long table format. Just need to complete those additional AtlantECO headers
colnames(data)

# Check those Additional_information to see in what column they'll fall in
unique(data$Additional_information) # OK...It's a mix of sampling effort...sampling protocols etc. This should go into the 'SamplingProtocol' column
# First conevrt thta empty char by NA
data[data$Additional_information == "","Additional_information"] <- NA

mare <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = data$decimalLatitude, decimalLongitude = data$DecimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = data$Date,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = data$Year, Month = data$Month, Day = data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = data$Sampling_depth, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = NA, MaxDepth = NA, ParentEventID = NA, EventID = NA, InstitutionCode = "Not_applicable", SourceArchive = "PANGAEA", 
                OrigCollectionCode = "777387_(PDI-1447)", OrigCollectionID = "Not_applicable",
                BiblioCitation = "Berdnazcek et al. (2021) - ESSD", CitationDOI = "doi:10.5194/essd-4-167-2012", DateDataAccess = "23-05-2021",
                OrigScientificName = data$OrigScientificName, ScientificName = data$OrigScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = data$Abundance_ind.m3,
                MeasurementUnit = "#/m3", MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = data$Biomass_mg.m3, BiomassConvFactor = "To be added",
                basisOfRecord = "See SamplingProtocol", SamplingProtocol = data$Additional_information, SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = NA, DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
head(mare)
dim(mare) # 8'814
str(mare)

# Check scientific names used
unique(mare$ScientificName) ; str(mare$ScientificName)

### Need to modify most names manually because they will cause problems (extra blank spaces, typos in names, 8 instead of brackets etc...)
### !!! We can keep the original labels in 'OrigScientificName' and use them to inform 'LifeForm' (juveniles, larvae etc.) in the "LifeForm" column
# Remove "  " and "      " in characters using str_replace
unique( str_replace_all(string = mare$ScientificName, pattern = "  ", replacement = "") ) # Looks ok
mare$ScientificName <- str_replace_all(string = mare$ScientificName, pattern = "  ", replacement = "")
mare$ScientificName <- str_replace_all(string = mare$ScientificName, pattern = " spp.", replacement = "")

unique(mare$ScientificName)

### Manually correct based on WoRMS
mare[mare$ScientificName == "L. helicina antarctica","ScientificName"] <- "Limacina helicina rangii"
mare[mare$ScientificName == "Clione limacina veliger","ScientificName"] <- "Clione limacina"
mare[mare$ScientificName == "L. helicina veliger","ScientificName"] <- "Limacina helicina"
mare[mare$ScientificName == "Limacina helicina ","ScientificName"] <- "Limacina helicina"
mare[mare$ScientificName == "Clione antarctica","ScientificName"] <- "Clione limacina antarctica"
mare[mare$ScientificName == "L. trochiformis","ScientificName"] <- "Limacina trochiformis"
mare[mare$ScientificName == "L. inflata","ScientificName"] <- "Heliconoides inflatus"
mare[mare$ScientificName == "S. subula","ScientificName"] <- "Styliola subula"
mare[mare$ScientificName == "L.inflata","ScientificName"] <- "Heliconoides inflatus"
mare[mare$ScientificName == "L.trochiformis","ScientificName"] <- "Limacina trochiformis"
mare[mare$ScientificName == "S.subula","ScientificName"] <- "Styliola subula"
mare[mare$ScientificName == "Pteropods","ScientificName"] <- "Pteropoda"
mare[mare$ScientificName == "Clione limacina (polytrochous larvae)","ScientificName"] <- "Clione limacina"
mare[mare$ScientificName == "Limacina helicina (adults)","ScientificName"] <- "Limacina helicina"
mare[mare$ScientificName == "Limacina helicina (juveniles and larvae)","ScientificName"] <- "Limacina helicina"
mare[mare$ScientificName == "Clione limacina(veliger)","ScientificName"] <- "Clione limacina"
mare[mare$ScientificName == "Limacina helicina (larvae)","ScientificName"] <- "Limacina helicina"
mare[mare$ScientificName == "Clione limacina (veliger)","ScientificName"] <- "Clione limacina"
mare[mare$ScientificName == "Pteropoda larvae","ScientificName"] <- "Pteropoda"
mare[mare$ScientificName == "Clio cuspidata juveniles","ScientificName"] <- "Clio cuspidata"
mare[mare$ScientificName == "Clio pyramidata f. lanceolata juveniles","ScientificName"] <- "Clio pyramidata"
mare[mare$ScientificName == "Cavolinia longirostris f.. Longirostris","ScientificName"] <- "Diacavolinia longirostris"
mare[mare$ScientificName == "Diacria rampali","ScientificName"] <- "Diacria rampalae"
mare[mare$ScientificName == "Cavolinia group. juveniles","ScientificName"] <- "Cavolinia"
mare[mare$ScientificName == "Clio convexa juveniles","ScientificName"] <- "Clio convexa" 
mare[mare$ScientificName == "Diacria quadridentata group juveniles","ScientificName"] <- "Telodiacria quadridentata" 
mare[mare$ScientificName == "Cavolinia longirostris f. strangulata","ScientificName"] <- "Diacavolinia longirostris" 
mare[mare$ScientificName == "Cavolinia unicata unicata f. pusilla","ScientificName"] <- "Cavolinia uncinata" 
mare[mare$ScientificName == "Diacria quadridentata","ScientificName"] <- "Telodiacria quadridentata" 
mare[mare$ScientificName == "Clio pyramidata f. lanceolata","ScientificName"] <- "Clio pyramidata" 
mare[mare$ScientificName == "Diacria danae","ScientificName"] <- "Telodiacria danae" 
mare[mare$ScientificName == "Cavolinia longirostris f.. angulosa","ScientificName"] <- "Diacavolinia longirostris" 
mare[mare$ScientificName == "Cavolinia longirostris f.. longirostris","ScientificName"] <- "Diacavolinia longirostris" 
mare[mare$ScientificName == "Cavolinia longirostris f. angulosa","ScientificName"] <- "Diacavolinia longirostris" 
mare[mare$ScientificName == "Diacria costata","ScientificName"] <- "Telodiacria costata" 
mare[mare$ScientificName == "Limacina inflata","ScientificName"] <- "Heliconoides inflatus" 
mare[mare$ScientificName == "Pteropods (total)","ScientificName"] <- "Pteropoda" 
mare[mare$ScientificName == "C. uncinata","ScientificName"] <- "Cavolinia uncinata" 
mare[mare$ScientificName == "Cavolinia longirostris","ScientificName"] <- "Diacavolinia longirostris" 
mare[mare$ScientificName == "Cu. c. juveniles","ScientificName"] <- "Cuvierina columnella" # Assumed C. colummnella ALTHOUGH there are two other Cuvierina species whose species name starts with 'c' (Cuvierina curryi and Cuvierina cancapae)
mare[mare$ScientificName == "C. virgula virgula","ScientificName"] <- "Creseis virgula" 
mare[mare$ScientificName == "Cavolinia l. juveniles","ScientificName"] <- "Diacavolinia longirostris" # Again, here I assumed it was Cavolinia longirostris and not another Cavolinia species based on the previous observations present in the dataset 
mare[mare$ScientificName == "C. virgula conica","ScientificName"] <- "Creseis conica" 
mare[mare$ScientificName == "Diacria q. juveniles","ScientificName"] <- "Telodiacria quadridentata" 
mare[mare$ScientificName == "C. virgula constricta","ScientificName"] <- "Boasia chierchiae" # That's a species I've never seen before 
mare[mare$ScientificName == "Hyalocylix striata","ScientificName"] <- "Hyalocylis striata" # Classic typo
mare[mare$ScientificName == "Clio pyramidata juveniles","ScientificName"] <- "Clio pyramidata" 
mare[mare$ScientificName == "C. unicata total","ScientificName"] <- "Cavolinia uncinata" 
mare[mare$ScientificName == "C. virgula virgula (total)","ScientificName"] <- "Creseis virgula" 
mare[mare$ScientificName == "Cuvierina columnella (total)","ScientificName"] <- "Cuvierina columnella" 
mare[mare$ScientificName == "C. virgula conica (total)","ScientificName"] <- "Creseis conica"
mare[mare$ScientificName == "C. pyramidata juveniles (total)","ScientificName"] <- "Clio pyramidata"
mare[mare$ScientificName == "Hyalocylix striata (total)","ScientificName"] <- "Hyalocylis striata"
mare[mare$ScientificName == "L. inflata (toal)","ScientificName"] <- "Heliconoides inflatus"  # "(toal)", aka when the t is spilled (...)
mare[mare$ScientificName == "C. virgula constricta (total)","ScientificName"] <- "Creseis conica"
mare[mare$ScientificName == "Diacria quadridentata 8total)","ScientificName"] <- "Telodiacria quadridentata"
mare[mare$ScientificName == "Cavolinia (total)","ScientificName"] <- "Cavolinia"
mare[mare$ScientificName == "Limacina bulimoides (total)","ScientificName"] <- "Limacina bulimoides"
mare[mare$ScientificName == "L. trochiformis (total)","ScientificName"] <- "Limacina trochiformis"
mare[mare$ScientificName == "Creseis acicula (total)","ScientificName"] <- "Creseis acicula"
mare[mare$ScientificName == "Clione sp.","ScientificName"] <- "Clione"
mare[mare$ScientificName == "Clione Limacina","ScientificName"] <- "Clione limacina"
mare[mare$ScientificName == "Diacria quadrientata","ScientificName"] <- "Telodiacria quadridentata"
mare[mare$ScientificName == "Styliola n. sp.","ScientificName"] <- "Styliola" # There is no Styliola species whose name starts with 'n' so I hope that 'n' stands for number
mare[mare$ScientificName == "Spiratella inflata","ScientificName"] <- "Heliconoides inflatus"  # Ahahal the whole Spiratella genus has been cancelled (https://www.marinespecies.org/aphia.php?p=taxdetails&id=585853)
mare[mare$ScientificName == "Spiratella lesueuri","ScientificName"] <- "Limacina lesueurii"
mare[mare$ScientificName == "Spiratella bulimoides","ScientificName"] <- "Limacina bulimoides"
mare[mare$ScientificName == "Spiratella trochiformis","ScientificName"] <- "Limacina trochiformis"
mare[mare$ScientificName == "Creseis virgula virgula","ScientificName"] <- "Creseis virgula"
mare[mare$ScientificName == "Creseis virgula conica","ScientificName"] <- "Creseis conica"
mare[mare$ScientificName == "TOTAL (euthecosomatous pteropods)","ScientificName"] <- "Euthecosomata"
mare[mare$ScientificName == "Limacinahelicina ","ScientificName"] <- "Limacina helicina"
mare[mare$ScientificName == "Cavolinia immatures","ScientificName"] <- "Cavolinia" # Oh, that's French for 'immature' as in 'juveniles'. Either that or these pteropods were really behaving like children

mare[mare$ScientificName == "L. lesueuri","ScientificName"] <- "Limacina lesueurii"
mare[mare$ScientificName == "L. retroversa","ScientificName"] <- "Limacina retroversa"
mare[mare$ScientificName == "L. bulimoides","ScientificName"] <- "Limacina bulimoides"
mare[mare$ScientificName == "Cresesis acicula","ScientificName"] <- "Creseis acicula"
mare[mare$ScientificName == "Creseis vir. Virgula","ScientificName"] <- "Creseis virgula"

### Check again: 
unique(mare$ScientificName) # Looks goof now. We'll use the OrigScientificName column to inform life stages after

### Now, provide the AphiaID from the WoRMS using 'worms' functions
require("worms") # install.packages("worms")
keys.worms <- wormsbynames( unique(mare$ScientificName) )
keys.worms$ScientificName <- unique(mare$ScientificName)
# All is good for the next steps, everything has been cleaned properly above

# Add WoRMS_status field
mare <-  add_column(mare, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(mare)

# For testing the functions below:
s <- unique(mare$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(mare$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- mare[mare$ScientificName == s,]
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
dim(ddf) # 8'814 same as before of course. Check some random lines
ddf[1:50,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf[is.na(ddf$Species),][1:50,]
unique(ddf$WoRMS_ID) # No NA, excellent
unique(ddf$WoRMS_status) # no freshwater taxa, good

### Finally, use OrigScientificName to provide lifestages info. All NA in 'LifeForm' will be assumed mature adults specimen. One can also use the mesh size of the plankton net used for sampling (if info is given) to derive a size class. For instance, a WP2 net should only capture organisms > 200µm

# To do so, identify those OrigScientificName levels with juveniles, larvae, veliger (=larvae) or immatures in it
l <- unique(ddf$OrigScientificName)[grep("larva",unique(ddf$OrigScientificName))]
veli <- unique(ddf$OrigScientificName)[grep("velige",unique(ddf$OrigScientificName))]
juve <- unique(ddf$OrigScientificName)[grep("juveni",unique(ddf$OrigScientificName))]
imma <- unique(ddf$OrigScientificName)[grep("immat",unique(ddf$OrigScientificName))]
larvae <- c(l,veli)
juveniles <- c(juve,imma)

# Use of else loops to inform LifeForm
for(o in unique(ddf$OrigScientificName)) {
    
        message(paste(o, sep = ""))
        
        if(o %in% juveniles) {
            
            ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Juveniles"
            
        } else if(o %in% larvae) {
            
            ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Larvae"
            
        }
    
} # eo for loop - o in unique(ddf$OrigScientificName)
# Check
unique(ddf$LifeForm) # Seems to have worked..
summary(factor(ddf$LifeForm)) # Compare to nrow ddf
nrow(ddf[ddf$OrigScientificName %in% juveniles,]) # 179 rows
nrow(ddf[ddf$OrigScientificName %in% larvae,]) # 107 rows
# Correct.

head(ddf); str(ddf)
ddf[4000:4050,]

### Saving new file
save(ddf, file = "AtlantECO_WP2_MAREDAT_pteropoda_Bernadczek2012_reformat+WoRMScheck_08_10_21.RData")
gc()
# And also as .txt for excel
write.table(x = ddf, file = "AtlantECO_WP2_MAREDAT_pteropoda_Bernadczek2012_reformat+WoRMScheck_08_10_21.txt", sep = "\t")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
