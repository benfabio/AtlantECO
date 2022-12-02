
##### ATLANTECO SCRIPT 1.3.5 ----------------------------------------------------------------------------------------------------------------------------
##### 25/10/2021: R Script to format the copepod species counts and concentrations data made available by E. Becker on researchGate, following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#  - Read the excel spreadsheet you prepared from the various sheets made available on RG by Erica Becker et al.
#  - Re-format to AtlantECO WP2 template
#  - Perform WoRMS taxonomic check
#  - Don't forget to convert counts to concentrations

### Latest update: 14/02/2022

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("lubridate")
library("viridis")
library("worms")
library("xlsx")

world <- map_data("world")  # coastline for maps

### ----------------------------------------------------------------------------------------------------------------------------

setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Traditional/Dataset1_Copepod_species_SAO")

### 1°) Read the the file, examine and adjust if necessary
# data <- read.xlsx("table_data_Becker2021_PiO_copepod_counts_SAO_11_02_22.xlsx", 1, header = T)
data <- read.csv("table_data_Becker2021_PiO_copepod_counts_SAO_11_02_22.csv", h = T, sep = ";", dec = ",")
colnames(data)
dim(data) # 
str(data)
head(data)
summary(data)

# Quickly melt to put species as rows instead of colnames
id.names <- colnames(data)[c(1:13,251:length(data))] ; id.names
m.data <- melt(data, id.vars = id.names)
str(m.data) ; head(m.data) ; dim(m.data)
colnames(m.data)[c(30,31)] <- c("OrigScientificName","value")
m.data$OrigScientificName <- as.character(m.data$OrigScientificName)
unique(m.data$OrigScientificName)
summary(m.data$value)

### Convert Date to 'Date' vector and extract D/M/Y from it
head(lubridate::dmy(m.data$Date)) ; head(m.data$Date) ; str(m.data$Date)
# Seems to work. use lubridate package to derive D/M/Y headers from Date
m.data$Date <- lubridate::dmy(m.data$Date)
m.data$Day <- lubridate::day(m.data$Date)
m.data$Month <- lubridate::month(m.data$Date)
m.data$Year <- lubridate::year(m.data$Date)
# summary(m.data[,c("Day","Month","Year")])
# Good.

### Convert 'value' (raw counts) into concentrations in #/m3
m.data$abund <- NA
m.data$abund <- ((m.data$value * m.data$Fraction_concentration_mL)/m.data$Aliquot_mL) * (m.data$Motoda.fraction/m.data$Vol_filtered_m3)
# summary(m.data$abund) # NaN?
### !! Different formula necessary for Fraction_concentration_mL == 0 (no dilution needed likely because few organisms in sample)!
### In this case: Abund = counts*(Motoda/Volume filtered)
m.data[m.data$Fraction_concentration_mL == 0,"abund"] <- (m.data[m.data$Fraction_concentration_mL == 0,"value"])*(m.data[m.data$Fraction_concentration_mL == 0,"Motoda.fraction"]/m.data[m.data$Fraction_concentration_mL == 0,"Vol_filtered_m3"])
# summary(m.data$abund)
# To verify: m.data$abund of Copepoda_.all. should be == m.data$value of Copepoda_.all._abundance
m.data[m.data$OrigScientificName == "Copepoda_.all._abundance", "value"][1:10]
m.data[m.data$OrigScientificName == "Copepoda_.all.", "abund"][1:10]
### Checks out :)

### Reformat to AtlantECO WP2 standards
colnames(m.data)

### For defining sampling protocol
# head( paste("Vol filtered = ",m.data$Vol_filtered_m3," (m3); ","Fraction = ",m.data$Fraction_concentration_mL," (mL); Aliquot = ",m.data$Aliquot_mL," (mL); Motoda fraction = ",m.data$Motoda.fraction, sep = "") )

m.data2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = m.data$decimalLatitude, decimalLongitude = m.data$decimalLongitude, geodeticDatum = "WGS84", 
                CoordUncertainty = "Comparison between the calculated position and data from vessels where a GPS record was available suggests the position assigned to CPR samples is accurate to within 10–20 nautical miles.",
                CountryCode = NA, eventDate = m.data$Date,
                eventDateInterval = m.data$Time, eventDateIntervalUnit = "Assumed GMT", Year = m.data$Year, Month = m.data$Month, Day = m.data$Day,
                Bathymetry = m.data$Bathymetry, BathySource = "Given in dataset", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = NA, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = m.data$MinDepth, MaxDepth = m.data$MaxDepth, ParentEventID = m.data$ParentEventID, EventID = m.data$EventID,
                InstitutionCode = "Universidade Federal de Santa Catarina (UFSC)", SourceArchive = "https://www.vliz.be/en/imis?dasid=6440&doiid=459",
                OrigCollectionCode = "https://doi.org/10.14284/458", OrigCollectionID = "Not_applicable",
                BiblioCitation = m.data$Reference, CitationDOI = m.data$CitationDOI, DateDataAccess = "11-02-2022",
                OrigScientificName = m.data$OrigScientificName, ScientificName = m.data$OrigScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = NA, Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = m.data$abund,
                MeasurementUnit = "#/m3", MeasurementAcurracy = NA,
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = "Vertical WP2 net tow (200µm mesh) from the TARA Oceans and MCT-II Cruzeiro do Sul expeditions",
                SamplingProtocol =  paste("Vol filtered = ",m.data$Vol_filtered_m3," (m3); ","Fraction = ",m.data$Fraction_concentration_mL," (mL); Aliquot = ",m.data$Aliquot_mL," (mL); Motoda fraction = ",m.data$Motoda.fraction, sep = ""),
                SampleAmount = m.data$Vol_filtered_m3,
                SampleAmountUnit = "Volume filtered (m3)", SampleEffort = "At least 100 copepod individuals were sorted per sample",
                DeterminedBy = "Becker et al. 2021",
                DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
head(m.data2)
dim(m.data2) # 8769   70
str(m.data2)

### Modify 'OrigScientificName' into ScientificName
unique(m.data2$OrigScientificName) 
# Remove the '.' and leave the '_' 
m.data2$OrigScientificName <- gsub(".", "", unique(m.data2$OrigScientificName), fixed = T)
# delete observations from "Copepoda_.all._abundance" 
m.data2 <- m.data2[!(m.data2$OrigScientificName == "Copepoda_all_abundance"),] # m.data2[(m.data2$OrigScientificName == "Copepoda_all_abundance"),] # should return interget 0

m.data2[m.data2$OrigScientificName == "Copepoda_all","Note"] <- "Values here correspond to total copepod community abundance"

### Compare to older version
unique(m.data2$OrigScientificName) 
m.data2$ScientificName <- m.data2$OrigScientificName
### OK, don't touch OrigScientificName anylonger here for the sak of tracability

### Use the OrigScientificName (_M/_F/_J) to inform: LifeForm (M/J/Juvenile)
# unique(m.data2[grepl("_M",x = m.data2$OrigScientificName),"OrigScientificName"])
# unique(m.data2[grepl("_J",x = m.data2$OrigScientificName),"OrigScientificName"])
m.data2[grepl("_M",x = m.data2$ScientificName),"LifeForm"] <- "male (assumed adult)"
m.data2[grepl("_F",x = m.data2$ScientificName),"LifeForm"] <- "female (assumed adult)"
m.data2[grepl("_J",x = m.data2$ScientificName),"LifeForm"] <- "juvenile"
m.data2[grepl("_CII",x = m.data2$ScientificName),"LifeForm"] <- "juvenile CII"
m.data2[grepl("_CIII",x = m.data2$ScientificName),"LifeForm"] <- "juvenile CIII"
m.data2[grepl("_J",x = m.data2$ScientificName),"LifeForm"] <- "juvenile"
summary(factor(m.data2$LifeForm)) # NA?
m.data2[is.na(m.data2$LifeForm),'ScientificName'] # Makes sence

### Modify ScientificName so it matches WoRMS expectations
# unique(m.data2$ScientificName) 
m.data2$ScientificName <- str_replace_all(m.data2$ScientificName, "_M", "")
m.data2$ScientificName <- str_replace_all(m.data2$ScientificName, "_F", "")
m.data2$ScientificName <- str_replace_all(m.data2$ScientificName, "_J", "")
m.data2$ScientificName <- str_replace_all(m.data2$ScientificName, "_spp", "")
m.data2$ScientificName <- str_replace_all(m.data2$ScientificName, "_CIII", "")
m.data2$ScientificName <- str_replace_all(m.data2$ScientificName, "_CII", "")
# Replace underscores by spaces
m.data2$ScientificName <- str_replace_all(m.data2$ScientificName, "_", " ")
unique(m.data2$ScientificName) 

### Good, test key.worms
keys.worms <- wormsbynames( unique(m.data2$ScientificName) )
# Manually correct:
# Acartia Acartia danae                                no match
m.data2[m.data2$ScientificName == "Acartia Acartia danae","ScientificName"] <- "Acartia (Acartia) danae"
# Acartia Acartiura longiremis                         no match
m.data2[m.data2$ScientificName == "Acartia Acartiura longiremis","ScientificName"] <- "Acartia (Acartiura) longiremis"
# Acartia Acartia negligens                            no match
m.data2[m.data2$ScientificName == "Acartia Acartia negligens","ScientificName"] <- "Acartia (Acartia) negligens"
# Acartia Acanthacartia tonsa                          no match
m.data2[m.data2$ScientificName == "Acartia Acanthacartia tonsa","ScientificName"] <- "Acartia (Acanthacartia) tonsa"
# Corycaeus Corycaeus speciosus                        no match
m.data2[m.data2$ScientificName == "Corycaeus Corycaeus speciosus","ScientificName"] <- "Corycaeus speciosus"
# Corycaeus Agetus flaccus                             no match
m.data2[m.data2$ScientificName == "Corycaeus Agetus flaccus","ScientificName"] <- "Agetus flaccus"
# Corycaeus Agetus typicus                             no match
m.data2[m.data2$ScientificName == "Corycaeus Agetus typicus","ScientificName"] <- "Agetus typicus"
# Corycaeus Onychocorycaeus giesbrechti                no match
m.data2[m.data2$ScientificName == "Corycaeus Onychocorycaeus giesbrechti","ScientificName"] <- "Corycaeus (Onchocorycaeus) giesbrechti"
# Corycaeus Urocorycaeus lautus                        no match
m.data2[m.data2$ScientificName == "Corycaeus Urocorycaeus lautus","ScientificName"] <- "Corycaeus (Urocorycaeus) lautus"
### Leave 'Copepoda all' as is
# Do it again now
keys.worms <- wormsbynames( unique(m.data2$ScientificName) )
keys.worms$ScientificName <- unique(m.data2$ScientificName)

# Add WoRMS_status field
m.data2 <-  add_column(m.data2, WoRMS_status = NA, .after = "WoRMS_ID")
# colnames(m.data2)

# For testing the functions below:
s <- unique(m.data2$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(m.data2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- m.data2[m.data2$ScientificName == s,]
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
dim(ddf) # 8732 same as before, check random lines
ddf[5000:5020,]
unique(ddf$Species) # Good. 
ddf[is.na(ddf$Species),][1:10,]
unique(ddf$WoRMS_ID) # No NA
unique(ddf$WoRMS_status) # good

### Last, make a map of sampling effort in space 
# Map sampling effort
world <- map_data("world")  # for maps
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$lat <= -10 & world$long <= -20,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = ddf) + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "table_Becker2021_PiO_copepods_abund_SAO_reformatted+WoRMScheck_14_02_22.Rdata")
write.table(ddf, file = "table_Becker2021_PiO_copepods_abund_SAO_reformatted+WoRMScheck_14_02_22.txt", sep = "\t")



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
