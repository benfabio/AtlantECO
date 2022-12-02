
##### ATLANTECO SCRIPT 1.6.3 ----------------------------------------------------------------------------------------------------------------------------
##### 13/04/2022: R Script to reformat the MAREDAT Coccolithophores data published by O'Brien et al. (2012) - ESSD © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Aims to:
#  - Read the excel sheet and reformat to AtlantECO WP2 template
#  - Provide AphiaID and classification using WoRMS' R package 'worms'

### Latest update: 19/04/2022

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
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/MAREDAT files/OBrien_Coccos_PANGAEA.785092_(PDI-2316)") ; dir() 
cocco <- read.csv("OBrien_Coccos_biomass_MAREDAT_2013.csv", h = T, sep = ";", dec = ",")
# dim(cocco) # 58'384
# str(cocco)
# colnames(cocco)

# Check spatial coordinates
# summary(cocco[,c('decimalLongitutude','decimalLatitude')]) # OK good

# Add Date 
# summary(cocco[,c('Day','Month','Year')]) # Day == 0?
cocco[cocco$Day == 0,"Day"] <- NA
cocco$Date <- lubridate::dmy(paste(cocco$Day, cocco$Month, cocco$Year, sep = "-"))

# Check depth levels
# summary(cocco[,c('Depth_m','Total_Depth_m')]) # Total_Depth_m --> depth of seafloor

# Quick map
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
#      geom_point(aes(x = decimalLongitutude, y = decimalLatitude), data = cocco) + coord_quickmap() +
#      scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#          labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#      scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#          labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#          panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")

### The dataframe is already kind of following a long table format. Just need to complete those additional AtlantECO headers
colnames(cocco)

# unique(cocco$Database) # --> InstitutionCode? No..
# cocco[cocco$Database == "" &!is.na(cocco$Database),"Database"] <- NA

# unique(cocco$Investigator) # --> DeterminedBy
cocco[cocco$Investigator == "" &!is.na(cocco$Investigator),"Investigator"] <- NA
# unique(cocco$Institute) # --> InstitutionCode!
cocco[cocco$Institute == "" &!is.na(cocco$Institute),"Institute"] <- NA
# unique(cocco$Project) # --> ParentEventID (combine with 'Cruise')
cocco[cocco$Project == "" &!is.na(cocco$Project),"Project"] <- NA
cocco$Project <- trimws(cocco$Project, "both")
# unique(cocco$Cruise) # --> ParentEventID
cocco[cocco$Cruise == "" &!is.na(cocco$Cruise),"Cruise"] <- NA
cocco[cocco$Cruise == "               " &!is.na(cocco$Cruise),"Cruise"] <- NA
cocco$Cruise <- trimws(cocco$Cruise, "both")
# unique(cocco$StationCode) # --> EventID, trimws()
cocco[cocco$StationCode == "" &!is.na(cocco$StationCode),"StationCode"] <- NA
cocco$StationCode <- trimws(cocco$StationCode, "both")

# unique(cocco$CruiseNumbers) # --> Don't care
# unique(cocco$BODC_SampleID) # --> DatasetKey (with a paste())

# unique(cocco$Biovolume_Category) # names...not sure what to do with this
# unique(cocco$Assumed_Cytoplasm_Diameter_micron) #
# unique(cocco$Coccosphere_Diameter) # # --> LifeForm

# Check range of concentration and biomass
summary(cocco[,c("Concentration_cell.L","Biomass_microg.L","MeanBM","MeanBM.L")])
# Discard the 205 NA and keep MeanBM.L (in µgC/L)
cocco <- cocco[!is.na(cocco$Concentration_cell.L),]       
# unique(cocco$Preservative) # Protocol?
cocco[cocco$Preservative == "" &!is.na(cocco$Preservative),"Preservative"] <- NA
# unique(cocco$Sampling_method) # SamplingProtocol
cocco[cocco$Sampling_method == "" &!is.na(cocco$Sampling_method),"Sampling_method"] <- NA
# unique(cocco$InstrumentCode) # Replace by actual value thanks to the excel sheet
cocco$InstrumentCode <- as.character(cocco$InstrumentCode)
cocco[cocco$InstrumentCode == 1,"InstrumentCode"] <- "Light microscopy"
cocco[cocco$InstrumentCode == 2,"InstrumentCode"] <- "SEM"
cocco[cocco$InstrumentCode == 3,"InstrumentCode"] <- "Unknown"
cocco[cocco$InstrumentCode == 4,"InstrumentCode"] <- "Flow cytometry"

# unique(cocco$Reference) # do not care right now
# cocco[cocco$Reference == "" &!is.na(cocco$Reference),"Reference"] <- NA

# unique(cocco$Flag) # Replace by actual value thanks to the excel sheet
cocco$Flag <- as.character(cocco$Flag)
cocco[cocco$Flag == "0","Flag"] <- NA
cocco[cocco$Flag == "1" & !is.na(cocco$Flag),"Flag"] <- "Thoracosphaera heimii"
cocco[cocco$Flag == "2" & !is.na(cocco$Flag),"Flag"] <- "Biomass estimates only (no abundance data)"
cocco[cocco$Flag == "3" & !is.na(cocco$Flag),"Flag"] <- "No depth provided"
cocco[cocco$Flag == "4" & !is.na(cocco$Flag),"Flag"] <- "Statistical outlier detected by Chauvenet analysis"

# unique(cocco$ID_Flag) # do not care

### And finally check ScientificName
# unique(cocco$ScientificName)  # a real mess...
cocco$ScientificName <- trimws(cocco$ScientificName)

### Reformat to WP2 format
cocco2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", DatasetKey = paste("BODC_SampleID =", cocco$BODC_SampleID, sep = " "),
                decimalLatitude = cocco$decimalLatitude, decimalLongitude = cocco$decimalLongitutude, geodeticDatum = "WGS84", 
                CoordUncertainty = NA, CountryCode = NA, eventDate = cocco$Date,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = cocco$Year, Month = cocco$Month, Day = cocco$Day,
                Bathymetry = cocco$Total_Depth_m, BathySource = "Given in original dataset", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = cocco$Depth_m, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = NA, MaxDepth = NA, ParentEventID = paste(cocco$Project, cocco$Cruise, sep = "; "),
                EventID = cocco$StationCode,InstitutionCode = cocco$Institute,
                SourceArchive = "PANGAEA", OrigCollectionCode = "785092", OrigCollectionID = "https://doi.pangaea.de/10.1594/PANGAEA.785092",
                BiblioCitation = "O'Brien et al. (2013) - ESSD", CitationDOI = "doi:10.5194/essd-5-259-2013", DateDataAccess = "23-05-2021",
                OrigScientificName = cocco$ScientificName, ScientificName = cocco$ScientificName,
                WoRMS_ID = 'To_add_at_the_end', TaxonRank = 'To_add_at_the_end', 
                Kingdom = NA, Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = cocco$Coccosphere_Diameter, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Cell concentration", MeasurementTypeID = "To_define", MeasurementValue = cocco$Concentration_cell.L,
                MeasurementUnit = "cell/L", MeasurementAcurracy = NA, MeasurementValueID = "To_define",
                MinBiomass = cocco$MinBM.L, MaxBiomass = cocco$MaxBM.L, MeanBiomass = cocco$MeanBM.L,
                BiomassConvFactor = "See original .xslx sheet - Average biomasses (Min/Max/Mean) are given in µgC.L-1",
                basisOfRecord = cocco$Sampling_method,
                SamplingProtocol = paste(cocco$Sampling_method, cocco$InstrumentCode, cocco$Preservative, sep = "; "),
                SampleAmount = NA, SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = cocco$Investigator, DeterminedDate = NA, Note = NA, Flag = cocco$Flag 
) # eo ddf
# head(cocco2)
# dim(cocco2) # 58179, good
# str(cocco2)

### Check scientific names used
unique(cocco2$ScientificName) 

### List of ScientificName containing lifeforms:
cocco2$LifeForm <- as.character(cocco2$LifeForm)
# 10-25um Coccospheres
cocco2[cocco2$ScientificName == "10-25um Coccospheres","LifeForm"] <- "10-25 µm"
# Holococcolithophorid 10microns
cocco2[cocco2$ScientificName == "Holococcolithophorid 10microns","LifeForm"] <- "10 µm"
# Holococcolithophorid 15microns
cocco2[cocco2$ScientificName == "Holococcolithophorid 15microns","LifeForm"] <- "15 µm"
# Holococcolithophorid 5microns
cocco2[cocco2$ScientificName == "Holococcolithophorid 5microns","LifeForm"] <- "5 µm"
# Syracosphaera 10microns
cocco2[cocco2$ScientificName == "Syracosphaera 10microns","LifeForm"] <- "10 µm"
# Syracosphaera 15microns
cocco2[cocco2$ScientificName == "Syracosphaera 15microns","LifeForm"] <- "15 µm"
# Syracosphaera 20microns
cocco2[cocco2$ScientificName == "Syracosphaera 20microns","LifeForm"] <- "20 µm"
# Coccosphaerales (Stage: holococcolithophorid Size: 10um)
cocco2[cocco2$ScientificName == "Coccosphaerales (Stage: holococcolithophorid Size: 10um)","LifeForm"] <- "10 µm"
# Coccolithophorids <10 um
cocco2[cocco2$ScientificName == "Coccolithophorids <10 um","LifeForm"] <- "<10 µm"
# Coccolithophorids <10 um (5 - 10)
cocco2[cocco2$ScientificName == "Coccolithophorids <10 um (5 - 10)","LifeForm"] <- "5-10 µm"
# Syracosphaera (Size: 10um)
cocco2[cocco2$ScientificName == "Syracosphaera (Size: 10um)","LifeForm"] <- "10 µm"
# Syracosphaera (Size: 20um)
cocco2[cocco2$ScientificName == "Syracosphaera (Size: 20um)","LifeForm"] <- "20 µm"
# Coccosphaerales (Stage: holococcolithophorid Size: 14um)
cocco2[cocco2$ScientificName == "Coccosphaerales (Stage: holococcolithophorid Size: 14um)","LifeForm"] <- "14 µm"
# Coccosphaerales (Stage: holococcolithophorid Size: 5um)
cocco2[cocco2$ScientificName == "Coccosphaerales (Stage: holococcolithophorid Size: 5um)","LifeForm"] <- "5 µm"
# Coccosphaerales (Size: 5um Morphology: paired-cell Subgroup: coccolithophore)
cocco2[cocco2$ScientificName == "Coccosphaerales (Size: 5um Morphology: paired-cell Subgroup: coccolithophore)","LifeForm"] <- "5 µm"
# Holococcolithophorid 14microns
cocco2[cocco2$ScientificName == "Holococcolithophorid 14microns","LifeForm"] <- "14 µm"
# Holococcolithophorid 8microns
cocco2[cocco2$ScientificName == "Holococcolithophorid 8microns","LifeForm"] <- "8 µm"
# Coccolithus fragilis -[ spherical/coccoid ; radius= 5.600 um ]-
cocco2[cocco2$ScientificName == "Coccolithus fragilis -[ spherical/coccoid ; radius= 5.600 um ]-","LifeForm"] <- "5.6 µm"
# Coccolithus huxleyi -[ spherical/coccoid ; radius= 3.65 um ]-
cocco2[cocco2$ScientificName == "Coccolithus huxleyi -[ spherical/coccoid ; radius= 3.65 um ]-","LifeForm"] <- "3.65 µm"
# Coccolithus pelagicus -[ spherical/coccoid ; radius= 8.400 um ]-
cocco2[cocco2$ScientificName == "Coccolithus pelagicus -[ spherical/coccoid ; radius= 8.400 um ]-","LifeForm"] <- "8.4 µm"
### Check
# cocco2 %>% dplyr::count(LifeForm) # OK

# Remove spp. and sp. before running the 'worms' routine
cocco2$ScientificName <- gsub(" spp.", "", cocco2$ScientificName, fixed = T)
cocco2$ScientificName <- gsub(" sp.", "", cocco2$ScientificName, fixed = T)
cocco2$ScientificName <- gsub(" cf. ", " ", cocco2$ScientificName, fixed = T)
cocco2$ScientificName <- gsub(" v ", " ", cocco2$ScientificName, fixed = T)

# Run wormsbynames a first time 
require("worms") 
keys.worms <- wormsbynames( unique(cocco2$ScientificName), marine_only = "false") # 
keys.worms$ScientificName <- unique(cocco2$ScientificName)

### Show unaccpted ones and their accepted names 
# keys.worms[keys.worms$status == "unaccepted" & !is.na(keys.worms$status),c("ScientificName","valid_name")]
sp2correct <- keys.worms[keys.worms$status == "unaccepted" & !is.na(keys.worms$status),c("ScientificName")]
sp2correct
# sp <- sp2correct[1]
cocco3 <- cocco2 # save in case it goes wrong
for(sp in sp2correct) {
        # Get valid name
        valid <- keys.worms[keys.worms$ScientificName == sp,c("valid_name")]
        message(paste("Changing ",sp," to ",valid, sep = ""))
        cocco3[cocco3$ScientificName == sp,"ScientificName"] <- valid
} # eo for loop 

# Re-run wormsbynames
# keys.worms2 <- wormsbynames( unique(cocco3$ScientificName), marine_only = "false")  
# keys.worms2[keys.worms2$status == "unaccepted" & !is.na(keys.worms2$status),c("ScientificName")]

### Stuff to manually correct...and identify those labels that provide LifeForms too
# Acanthoica jancheni                                  no match
cocco3[cocco3$ScientificName == "Acanthoica jancheni","ScientificName"] <- "Acanthoica janchenii"
# Acanthoica quattraspina                              no match
cocco3[cocco3$ScientificName == "Acanthoica quattraspina","ScientificName"] <- "Acanthoica quattrospina"
# Algirosphaera robsta                                 no match
cocco3[cocco3$ScientificName == "Algirosphaera robsta","ScientificName"] <- "Algirosphaera robusta"
# Anthosphaera SP.                                     no match
cocco3[cocco3$ScientificName == "Anthosphaera SP.","ScientificName"] <- "Anthosphaera"
# Emiliania huxleyi/Gephyrocapsa oceania               no match
cocco3[cocco3$ScientificName == "Emiliania huxleyi/Gephyrocapsa oceania","ScientificName"] <- "Emiliania huxleyi+Gephyrocapsa oceanica"
# Emiliania huxleyi, Gephyrocapsa + 5um Coccospheres       no match
cocco3[cocco3$ScientificName == "Emiliania huxleyi, Gephyrocapsa + 5um Coccospheres","ScientificName"] <- "Prymnesiophyceae"
# Braarudosphaera bigelowi                             no match
cocco3[cocco3$ScientificName == "Braarudosphaera bigelowi","ScientificName"] <- "Braarudosphaera bigelowii"
# Calcidiscus leptoporus (inc. Coccolithus pelagicus)       no match
cocco3[cocco3$ScientificName == "Calcidiscus leptoporus (inc. Coccolithus pelagicus)","ScientificName"] <- "Calcidiscus leptoporus+Coccolithus pelagicus"
# Crystallolithus cf rigidus                           no match
cocco3[cocco3$ScientificName == "Crystallolithus cf rigidus","ScientificName"] <- "Calcidiscus leptoporus f. rigidus"
# Calcidiscus leptoporus (MURRA                        no match
cocco3[cocco3$ScientificName == "Calcidiscus leptoporus (MURRA","ScientificName"] <- "Calcidiscus leptoporus"
# Calcidiscus leptoporus (small + intermediate)        no match
cocco3[cocco3$ScientificName == "Calcidiscus leptoporus (small + intermediate)","ScientificName"] <- "Calcidiscus leptoporus"
# Calcidiscus leptoporus intermediate                  no match
cocco3[cocco3$ScientificName == "Calcidiscus leptoporus intermediate","ScientificName"] <- "Calcidiscus leptoporus"
# Calcidiscus leptopora                                no match
cocco3[cocco3$ScientificName == "Calcidiscus leptopora","ScientificName"] <- "Calcidiscus leptoporus"
# Calciopappus (Young) softus                          no match
cocco3[cocco3$ScientificName == "Calciopappus (Young) softus","ScientificName"] <- "Calciopappus"
# Calciosoleniceae                                     no match
cocco3[cocco3$ScientificName == "Calciosoleniceae","ScientificName"] <- "Calciosoleniaceae"
# Anoplosolenia braziliensis                           no match
cocco3[cocco3$ScientificName == "Anoplosolenia braziliensis","ScientificName"] <- "Calciosolenia brasiliensis"
# Calciosolenia granii var closterium                  no match
cocco3[cocco3$ScientificName == "Calciosolenia granii var closterium","ScientificName"] <- "Calciosolenia granii var. closterium"
# Syracosphaera brasiliensis                           no match
cocco3[cocco3$ScientificName == "Syracosphaera brasiliensis","ScientificName"] <- "Calciosolenia brasiliensis"
# Calciosolenia granii var cylindrothecaf              no match
cocco3[cocco3$ScientificName == "Calciosolenia granii var cylindrothecaf","ScientificName"] <- "Calciosolenia granii var. cylindrotheciformis"
# Calciosolenia siniosa                                no match
cocco3[cocco3$ScientificName == "Calciosolenia siniosa","ScientificName"] <- "Calciosolenia murrayi"
# Calciosolenia granii var cylindrothecaforma          no match
cocco3[cocco3$ScientificName == "Calciosolenia granii var cylindrothecaforma","ScientificName"] <- "Calciosolenia granii var. cylindrotheciformis"
# Calciosolenia granii var cylindrothecaeiformis       no match
cocco3[cocco3$ScientificName == "Calciosolenia granii var cylindrothecaeiformis","ScientificName"] <- "Calciosolenia granii var. cylindrotheciformis"
# Calciosolenia murray                                 no match
cocco3[cocco3$ScientificName == "Calciosolenia murray","ScientificName"] <- "Calciosolenia murrayi"
# Calyptrosphaera A                                    no match
cocco3[cocco3$ScientificName == "Calyptrosphaera A","ScientificName"] <- "Calyptrosphaera"
# Calyptrosphaera B                                    no match
cocco3[cocco3$ScientificName == "Calyptrosphaera B","ScientificName"] <- "Calyptrosphaera"
# Calyptrosphaera circu                                no match
cocco3[cocco3$ScientificName == "Calyptrosphaera circu","ScientificName"] <- "Calyptrosphaera"
# Canistrolithus 1 HET                                 no match
cocco3[cocco3$ScientificName == "Canistrolithus 1 HET","ScientificName"] <- "Canistrolithus"
# Ceratolithus lyramultiformis                         no match
# cocco3[cocco3$ScientificName == "Ceratolithus lyramultiformis","ScientificName"] <- ""
# Coccolithus metiori                                  no match
cocco3[cocco3$ScientificName == "Coccolithus metiori","ScientificName"] <- "Coccolithus meteorii"
# Couldn't find any accepted names for those above
# Crystallolithus hyalina                              no match
cocco3[cocco3$ScientificName == "Crystallolithus hyalina","ScientificName"] <- "Coccolithus pelagicus f. hyalinus"
# cf. Corisphaera sp                                   no match
cocco3[cocco3$ScientificName == "cf. Corisphaera sp","ScientificName"] <- "Corisphaera"
# Cribrosphaera ehrenbergii                            no match
cocco3[cocco3$ScientificName == "Cribrosphaera ehrenbergii","ScientificName"] <- "Cribrosphaerella ehrenbergii"
### ! IS A FOSSIL
# Cyrtosphaera acuelata                                no match
cocco3[cocco3$ScientificName == "Cyrtosphaera acuelata","ScientificName"] <- "Cyrtosphaera aculeata"
# Discosphaera  tubifer (inc. Papposphaera.lepida)       no match
cocco3[cocco3$ScientificName == "Discosphaera  tubifer (inc. Papposphaera.lepida)","ScientificName"] <- "Discosphaera tubifera+Papposphaera lepida"
# Discophaera tubifer                                  no match
cocco3[cocco3$ScientificName == "Discophaera tubifer","ScientificName"] <- "Discosphaera tubifera"
# Discosphaera  tubifer                                no match
cocco3[cocco3$ScientificName == "Discosphaera  tubifer","ScientificName"] <- "Discosphaera tubifera"
# Discosphaera  tubifera                               no match
cocco3[cocco3$ScientificName == "Discosphaera  tubifera","ScientificName"] <- "Discosphaera tubifera"
# Discosphaera  thomsoni                               no match
cocco3[cocco3$ScientificName == "Discosphaera  thomsoni","ScientificName"] <- "Discosphaera tubifera"
# Rhabdosphaera tubifer                                no match
cocco3[cocco3$ScientificName == "Rhabdosphaera tubifer","ScientificName"] <- "Discosphaera tubifera"
# Emiliania huxleyi var. huxleyi                       no match
cocco3[cocco3$ScientificName == "Emiliania huxleyi var. huxleyi","ScientificName"] <- "Emiliania huxleyi"
# Emiliana huxleyi                                     no match
cocco3[cocco3$ScientificName == "Emiliana huxleyi","ScientificName"] <- "Emiliania huxleyi"
# Emiliania huxleyi A1                                 no match
cocco3[cocco3$ScientificName == "Emiliania huxleyi A1","ScientificName"] <- "Emiliania huxleyi"
# Emiliania huxleyi A2                                 no match
cocco3[cocco3$ScientificName == "Emiliania huxleyi A2","ScientificName"] <- "Emiliania huxleyi"
# Emiliania huxleyi A3                                 no match
cocco3[cocco3$ScientificName == "Emiliania huxleyi A3","ScientificName"] <- "Emiliania huxleyi"
# Emiliania huxleyi C                                  no match
cocco3[cocco3$ScientificName == "Emiliania huxleyi C","ScientificName"] <- "Emiliania huxleyi"
# Emiliania huxleyi Indet.                             no match
cocco3[cocco3$ScientificName == "Emiliania huxleyi Indet.","ScientificName"] <- "Emiliania huxleyi"
# Coccolithus huxley                                   no match
cocco3[cocco3$ScientificName == "Coccolithus huxley","ScientificName"] <- "Emiliania huxleyi"
# Florisphaera profunda var. profunda                  no match
cocco3[cocco3$ScientificName == "Florisphaera profunda var. profunda","ScientificName"] <- "Florisphaera profunda"
# Gephryocapsa                                         no match
cocco3[cocco3$ScientificName == "Gephryocapsa","ScientificName"] <- "Gephyrocapsa"
# Gephyracapsa                                         no match
cocco3[cocco3$ScientificName == "Gephyracapsa","ScientificName"] <- "Gephyrocapsa"
# Gephyrocapsa sp A                                    no match
cocco3[cocco3$ScientificName == "Gephyrocapsa sp A","ScientificName"] <- "Gephyrocapsa"
# Unid coccolithophore - 'Spike' (cf. G.ornata)        no match
cocco3[cocco3$ScientificName == "Unid coccolithophore - 'Spike' (cf. G.ornata)","ScientificName"] <- "Gephyrocapsa ornata"
# Gephyrocapsids: Emiliania + Gephyrocapsa             no match
cocco3[cocco3$ScientificName == "Gephyrocapsids: Emiliania + Gephyrocapsa","ScientificName"] <- "Emiliania+Gephyrocapsa"
# cf Thorosphaera flabellata                           no match
cocco3[cocco3$ScientificName == "cf Thorosphaera flabellata","ScientificName"] <- "Gladiolithus flabellatus"
# Halopappus (inc. Calciopappus + Michaelsarsia)       no match
cocco3[cocco3$ScientificName == "Halopappus (inc. Calciopappus + Michaelsarsia)","ScientificName"] <- "Halopappus+Calciopappus+Michaelsarsia"
# Hallopappus                                          no match
cocco3[cocco3$ScientificName == "Hallopappus","ScientificName"] <- "Halopappus"
# Halopappinae                                         no match
cocco3[cocco3$ScientificName == "Halopappinae","ScientificName"] <- "Halopappaceae"
# Halopappus vahseli                                   no match
cocco3[cocco3$ScientificName == "Halopappus vahseli","ScientificName"] <- "Halopappus vahselii"
# Hemiella or heimiella excentrica                     no match
cocco3[cocco3$ScientificName == "Hemiella or heimiella excentrica","ScientificName"] <- "Heimiella excentrica"
# Helicosphaera (inc. H.carteri + hyalina)             no match
cocco3[cocco3$ScientificName == "Helicosphaera (inc. H.carteri + hyalina)","ScientificName"] <- "Helicosphaera"
# Helicosphaera carteri var. hyalina                   no match
cocco3[cocco3$ScientificName == "Helicosphaera carteri var. hyalina","ScientificName"] <- "Helicosphaera carteri"
# Lohmanosphaera                                       no match
cocco3[cocco3$ScientificName == "Lohmanosphaera","ScientificName"] <- "Lohmannosphaera"
# Lomanosphaera                                        no match
cocco3[cocco3$ScientificName == "Lomanosphaera","ScientificName"] <- "Lohmannosphaera"
# Lohmanosphaera paucoscyphos                          no match
cocco3[cocco3$ScientificName == "Lohmanosphaera paucoscyphos","ScientificName"] <- "Lohmannosphaera paucoscyphos"
# 10-25um Coccospheres                                 no match
cocco3[cocco3$ScientificName == "10-25um Coccospheres","ScientificName"] <- "Prymnesiophyceae"
### --> Life form
# Holococcolithophorid 10microns                       no match
cocco3[cocco3$ScientificName == "Holococcolithophorid 10microns","ScientificName"] <- "Prymnesiophyceae"
### --> Life form
# Holococcolithophorid 15microns                       no match
cocco3[cocco3$ScientificName == "Holococcolithophorid 15microns","ScientificName"] <- "Prymnesiophyceae"
# Holococcolithophorid 5microns                        no match
cocco3[cocco3$ScientificName == "Holococcolithophorid 5microns","ScientificName"] <- "Prymnesiophyceae"
# Syracosphaera 10microns                              no match
cocco3[cocco3$ScientificName == "Syracosphaera 10microns","ScientificName"] <- "Syracosphaera"
# Syracosphaera 15microns                              no match
cocco3[cocco3$ScientificName == "Syracosphaera 15microns","ScientificName"] <- "Syracosphaera"
# Syracosphaera 20microns                              no match
cocco3[cocco3$ScientificName == "Syracosphaera 20microns","ScientificName"] <- "Syracosphaera"
# Coccosphaerales (Stage: heterococcolithophorid Size: small)       no match
cocco3[cocco3$ScientificName == "Coccosphaerales (Stage: heterococcolithophorid Size: small)","ScientificName"] <- "Coccosphaerales"
# Coccosphaerales (Stage: holococcolithophorid Size: small)       no match
cocco3[cocco3$ScientificName == "Coccosphaerales (Stage: holococcolithophorid Size: small)","ScientificName"] <- "Coccosphaerales"
# Coccosphaerales (Stage: holococcolithophorid)        no match
cocco3[cocco3$ScientificName == "Coccosphaerales (Stage: holococcolithophorid)","ScientificName"] <- "Coccosphaerales"
# Coccosphaerales (Subgroup: coccolithophore)          no match
cocco3[cocco3$ScientificName == "Coccosphaerales (Subgroup: coccolithophore)","ScientificName"] <- "Coccosphaerales"
# Coccosphaerales (Stage: holococcolithophorid Size: 10um)       no match
cocco3[cocco3$ScientificName == "Coccosphaerales (Stage: holococcolithophorid Size: 10um)","ScientificName"] <- "Coccosphaerales"
# Coccolithophorids <10 um                             no match
cocco3[cocco3$ScientificName == "Coccolithophorids <10 um","ScientificName"] <- "Coccosphaerales"
# Coccolithophorids <10 um (5 - 10)                    no match
cocco3[cocco3$ScientificName == "Coccolithophorids <10 um (5 - 10)","ScientificName"] <- "Coccosphaerales"
# Syracosphaera (Size: 10um)                           no match
cocco3[cocco3$ScientificName == "Syracosphaera (Size: 10um)","ScientificName"] <- "Syracosphaera"
# Syracosphaera (Size: 20um)                           no match
cocco3[cocco3$ScientificName == "Syracosphaera (Size: 20um)","ScientificName"] <- "Syracosphaera"
# Coccosphaerales (Stage: holococcolithophorid Size: 14um)       no match
cocco3[cocco3$ScientificName == "Coccosphaerales (Stage: holococcolithophorid Size: 14um)","ScientificName"] <- "Coccosphaerales"
# Coccosphaerales (Stage: holococcolithophorid Size: 5um)       no match
cocco3[cocco3$ScientificName == "Coccosphaerales (Stage: holococcolithophorid Size: 5um)","ScientificName"] <- "Coccosphaerales"
# Coccosphaerales (Size: 5um Morphology: paired-cell Subgroup: coccolithophore)       no match
cocco3[cocco3$ScientificName == "Coccosphaerales (Size: 5um Morphology: paired-cell Subgroup: coccolithophore)","ScientificName"] <- "Coccosphaerales"
# Holococcolithophorid 14microns                       no match
cocco3[cocco3$ScientificName == "Holococcolithophorid 14microns","ScientificName"] <- "Prymnesiophyceae"
# Holococcolithophorid 8microns                        no match
cocco3[cocco3$ScientificName == "Holococcolithophorid 8microns","ScientificName"] <- "Prymnesiophyceae"
# Coccolithus fragilis -[ spherical/coccoid ; radius= 5.600 um ]-       no match
cocco3[cocco3$ScientificName == "Coccolithus fragilis -[ spherical/coccoid ; radius= 5.600 um ]-","ScientificName"] <- "Oolithotus fragilis"
# Coccolithus huxleyi -[ spherical/coccoid ; radius= 3.65 um ]-       no match
cocco3[cocco3$ScientificName == "Coccolithus huxleyi -[ spherical/coccoid ; radius= 3.65 um ]-","ScientificName"] <- "Emiliania huxleyi"
# Coccolithus pelagicus -[ spherical/coccoid ; radius= 8.400 um ]-       no match
cocco3[cocco3$ScientificName == "Coccolithus pelagicus -[ spherical/coccoid ; radius= 8.400 um ]-","ScientificName"] <- "Coccolithus pelagicus f. hyalinus"
# Michaelsarsia adriaticus                             no match
cocco3[cocco3$ScientificName == "Michaelsarsia adriaticus","ScientificName"] <- "Michaelsarsia adriatica"
# Syracosphaera corri                                  no match
cocco3[cocco3$ScientificName == "Syracosphaera corri","ScientificName"] <- "Syracosphaera molischii"
# Michelsarsia elegans                                 no match
cocco3[cocco3$ScientificName == "Michelsarsia elegans","ScientificName"] <- "Michaelsarsia elegans"
# Halopappus splendens                                 no match
# cocco3[cocco3$ScientificName == "Halopappus splendens","ScientificName"] <- ""
# Couldn't find any accepted species names
# Navilithus altivelum                                 no match
# cocco3[cocco3$ScientificName == "Navilithus altivelum","ScientificName"] <- ""
# deep-photic coccolithophore, from the Coccolithales Order (seems legit) - Family = Incertae sedis for now
# Oolithus                                             no match
cocco3[cocco3$ScientificName == "Oolithus","ScientificName"] <- "Oolithotus"
# Oolithus per                                         no match
cocco3[cocco3$ScientificName == "Oolithus per","ScientificName"] <- "Oolithotus"
# Oolithus cf fragilis                                 no match
cocco3[cocco3$ScientificName == "Oolithus cf fragilis","ScientificName"] <- "Oolithotus fragilis"
# Oolithus fragilis                                    no match
cocco3[cocco3$ScientificName == "Oolithus fragilis","ScientificName"] <- "Oolithotus fragilis"
# Oolithotus fragilis var. fragilis                    no match
cocco3[cocco3$ScientificName == "Oolithotus fragilis var. fragilis","ScientificName"] <- "Oolithotus fragilis"
# Ophiaster Sp.                                        no match
cocco3[cocco3$ScientificName == "Ophiaster Sp.","ScientificName"] <- "Ophiaster"
# Ophiaster hydroides                                  no match
cocco3[cocco3$ScientificName == "Ophiaster hydroides","ScientificName"] <- "Ophiaster hydroideus"
# Ophiaster hydroideuss                                no match
cocco3[cocco3$ScientificName == "Ophiaster hydroideuss","ScientificName"] <- "Ophiaster hydroideus"
# Pappomonas  type 2                                   no match
cocco3[cocco3$ScientificName == "Pappomonas  type 2","ScientificName"] <- "Pappomonas"
# Pappomonas type 3                                    no match
cocco3[cocco3$ScientificName == "Pappomonas type 3","ScientificName"] <- "Pappomonas"
# Pappomonas A                                         no match
cocco3[cocco3$ScientificName == "Pappomonas A","ScientificName"] <- "Pappomonas"
# Pappomonas B                                         no match
cocco3[cocco3$ScientificName == "Pappomonas B","ScientificName"] <- "Pappomonas"
# Pappomonas C                                         no match
cocco3[cocco3$ScientificName == "Pappomonas C","ScientificName"] <- "Pappomonas"
# Papposphaera type 4                                  no match
cocco3[cocco3$ScientificName == "Papposphaera type 4","ScientificName"] <- "Papposphaera"
# Papposphaera type 6                                  no match
cocco3[cocco3$ScientificName == "Papposphaera type 6","ScientificName"] <- "Papposphaera"
# Pontosphaera robusta                                 no match
# cocco3[cocco3$ScientificName == "Pontosphaera robusta","ScientificName"] <- ""
# Couldn't find any accepted name
# Pontosphaera discophora                              no match
cocco3[cocco3$ScientificName == "Pontosphaera discophora","ScientificName"] <- "Pontosphaera discopora"
# Pontosphaera haeckelli                               no match
cocco3[cocco3$ScientificName == "Pontosphaera haeckelli","ScientificName"] <- "Pontosphaera haeckelii"
# Ponotosphaera (P.syracusana)                         no match
cocco3[cocco3$ScientificName == "Ponotosphaera (P.syracusana)","ScientificName"] <- "Pontosphaera syracusana"
# Pontosphaera syracusana Lohmann                      no match
cocco3[cocco3$ScientificName == "Pontosphaera syracusana Lohmann","ScientificName"] <- "Pontosphaera syracusana"
# Poricalyptra gaarderiae                              no match
cocco3[cocco3$ScientificName == "Poricalyptra gaarderiae","ScientificName"] <- "Poricalyptra gaarderae"
# Poricalyptra magnaghi                                no match
cocco3[cocco3$ScientificName == "Poricalyptra magnaghi","ScientificName"] <- "Poricalyptra magnaghii"
# Poritectolithus poritectus                           no match
cocco3[cocco3$ScientificName == "Poritectolithus poritectus","ScientificName"] <- "Poritectolithus poritectum"
# Rhabdosphaera ampullacea                             no match
cocco3[cocco3$ScientificName == "Rhabdosphaera ampullacea","ScientificName"] <- "Rhabdolithes claviger"
# Rhabdosphaera  claviger (inc. var. stylifera)        no match
cocco3[cocco3$ScientificName == "Rhabdosphaera  claviger (inc. var. stylifera)","ScientificName"] <- "Rhabdolithes claviger"
# Rhabdosphaera claviger                               no match
cocco3[cocco3$ScientificName == "Rhabdosphaera claviger","ScientificName"] <- "Rhabdolithes claviger"
# Rhabdosphaera clavigera var. clavigera               no match
cocco3[cocco3$ScientificName == "Rhabdosphaera clavigera var. clavigera","ScientificName"] <- "Rhabdolithes claviger"
# Scyphospaera apsteinii                               no match
cocco3[cocco3$ScientificName == "Scyphospaera apsteinii","ScientificName"] <- "Scyphosphaera apsteinii"
# Scyphosphaera apstenii                               no match
cocco3[cocco3$ScientificName == "Scyphosphaera apstenii","ScientificName"] <- "Scyphosphaera apsteinii"
# Saturnulus                                           no match
cocco3[cocco3$ScientificName == "Saturnulus","ScientificName"] <- "Solisphaera"
# Sarturnulus blagnacensis                             no match
cocco3[cocco3$ScientificName == "Sarturnulus blagnacensis","ScientificName"] <- "Solisphaera blagnacensis"
# Sarturnulus emidasius                                no match
cocco3[cocco3$ScientificName == "Sarturnulus emidasius","ScientificName"] <- "Solisphaera emidasia"
# Syracosphaera hentsheli                              no match
cocco3[cocco3$ScientificName == "Syracosphaera hentsheli","ScientificName"] <- "Syracosphaera hentschelii"
### Couldn't find any accepted species name
# Syracosphaera quadridentata                          no match
cocco3[cocco3$ScientificName == "Syracosphaera quadridentata","ScientificName"] <- "Sphaerocalyptra quadridentata"
# Syr.                                                 no match
cocco3[cocco3$ScientificName == "Syr.","ScientificName"] <- "Syracosphaera"
# Syracosphaera type D                                 no match
cocco3[cocco3$ScientificName == "Syracosphaera type D","ScientificName"] <- "Syracosphaera"
# Syracosphaera type K                                 no match
cocco3[cocco3$ScientificName == "Syracosphaera type K","ScientificName"] <- "Syracosphaera"
# Syracosphaera dilatata varia                         no match
cocco3[cocco3$ScientificName == "Syracosphaera dilatata varia","ScientificName"] <- "Syracosphaera dilatata"
# Syracosphaera halldalii. f. dilatata                 no match
cocco3[cocco3$ScientificName == "Syracosphaera halldalii. f. dilatata","ScientificName"] <- "Syracosphaera halldalii"
# Syracosphaera S. epigrosa                            no match
cocco3[cocco3$ScientificName == "Syracosphaera S. epigrosa","ScientificName"] <- "Syracosphaera epigrosa"
# Syracosphaera haldalii                               no match
cocco3[cocco3$ScientificName == "Syracosphaera haldalii","ScientificName"] <- "Syracosphaera halldalii"
# Syracosphaera haldalii. f. haldalii.                 no match
cocco3[cocco3$ScientificName == "Syracosphaera haldalii. f. haldalii.","ScientificName"] <- "Syracosphaera halldalii"
# Syracopshaera histrica                               no match
cocco3[cocco3$ScientificName == "Syracopshaera histrica","ScientificName"] <- "Syracosphaera histrica"
# Syraacosphaera histrica                              no match
cocco3[cocco3$ScientificName == "Syraacosphaera histrica","ScientificName"] <- "Syracosphaera histrica"
# Syracosphaera marginaporata                          no match
cocco3[cocco3$ScientificName == "Syracosphaera marginaporata","ScientificName"] <- "Syracosphaera marginiporata"
# Caneosphaera molischii and similar                   no match
cocco3[cocco3$ScientificName == "Caneosphaera molischii and similar","ScientificName"] <- "Syracosphaera molischii"
# Syracosphaera molischii s.l.                         no match
cocco3[cocco3$ScientificName == "Syracosphaera molischii s.l.","ScientificName"] <- "Syracosphaera molischii"
# Syracosphaera noriotica                              no match
cocco3[cocco3$ScientificName == "Syracosphaera noriotica","ScientificName"] <- "Syracosphaera noroitica"
# Syracosphaera cf pirus                               no match
cocco3[cocco3$ScientificName == "Syracosphaera cf pirus","ScientificName"] <- "Syracosphaera pirus"
# Syracosphaera prolongata (inc. S.pirus)              no match
cocco3[cocco3$ScientificName == "Syracosphaera prolongata (inc. S.pirus)","ScientificName"] <- "Syracosphaera"
# Syrachosphaera pulchra                               no match
cocco3[cocco3$ScientificName == "Syrachosphaera pulchra","ScientificName"] <- "Syracosphaera pulchra"
# Syracosphaera oblonga                                no match
cocco3[cocco3$ScientificName == "Syracosphaera oblonga","ScientificName"] <- "Syracosphaera pulchra" # oblonga = type of pulchra
# Umbellosphaera sp                                    no match
cocco3[cocco3$ScientificName == "Umbellosphaera sp","ScientificName"] <- "Umbellosphaera"
# Umbellosphaera irregularis + tenuis                  no match
cocco3[cocco3$ScientificName == "Umbellosphaera irregularis + tenuis","ScientificName"] <- "Umbellosphaera"
# Umbellocosphaera                                     no match
cocco3[cocco3$ScientificName == "Umbellocosphaera","ScientificName"] <- "Umbellosphaera"
# Umbilicosphaera sibogae foliosa                      no match
cocco3[cocco3$ScientificName == "Umbilicosphaera sibogae foliosa","ScientificName"] <- "Umbilicosphaera foliosa"
# FOSSIL 
# Umbillicosphaera ?hulburtiana                        no match
cocco3[cocco3$ScientificName == "Umbillicosphaera ?hulburtiana","ScientificName"] <- "Umbilicosphaera hulburtiana"
# Umbellosphaera hulburtiana                           no match
cocco3[cocco3$ScientificName == "Umbellosphaera hulburtiana","ScientificName"] <- "Umbilicosphaera hulburtiana"
# Umbilicosphaera (U.sibogae)                          no match
cocco3[cocco3$ScientificName == "Umbilicosphaera (U.sibogae)","ScientificName"] <- "Umbilicosphaera sibogae"
# Umbillicosphaera sibogae                             no match
cocco3[cocco3$ScientificName == "Umbillicosphaera sibogae","ScientificName"] <- "Umbilicosphaera sibogae"
# Umbilicosphaera sibogae (Weber-van-Bosse) Gaarder       no match
cocco3[cocco3$ScientificName == "Umbilicosphaera sibogae (Weber-van-Bosse) Gaarder","ScientificName"] <- "Umbilicosphaera sibogae"
# Umbilicosphaera sibogae var. sibogae                 no match
cocco3[cocco3$ScientificName == "Umbilicosphaera sibogae var. sibogae","ScientificName"] <- "Umbilicosphaera sibogae"
# Umbilicosphaera sibogae sibogae                      no match
cocco3[cocco3$ScientificName == "Umbilicosphaera sibogae sibogae","ScientificName"] <- "Umbilicosphaera sibogae"
# Umbellosphaera sibogae                               no match
cocco3[cocco3$ScientificName == "Umbellosphaera sibogae","ScientificName"] <- "Umbilicosphaera sibogae"
# Unid coccolithophore - 'Heart'                       no match
cocco3[cocco3$ScientificName == "Unid coccolithophore - 'Heart'","ScientificName"] <- "Prymnesiophyceae"
# Coccospheres                                         no match
cocco3[cocco3$ScientificName == "Coccospheres","ScientificName"] <- "Prymnesiophyceae"
# Undet Cocco.                                         no match
cocco3[cocco3$ScientificName == "Undet Cocco.","ScientificName"] <- "Prymnesiophyceae"
# COCCOLITHOPHORES                                     no match
cocco3[cocco3$ScientificName == "COCCOLITHOPHORES","ScientificName"] <- "Prymnesiophyceae"
# Coccolithophoridae                                   no match
cocco3[cocco3$ScientificName == "Coccolithophoridae","ScientificName"] <- "Coccosphaerales"
# Other coccolithophores                               no match
cocco3[cocco3$ScientificName == "Other coccolithophores","ScientificName"] <- "Prymnesiophyceae"
# COCCOLITHOPHORID                                     no match
cocco3[cocco3$ScientificName == "COCCOLITHOPHORID","ScientificName"] <- "Coccosphaerales"
# coccolith                                            no match
cocco3[cocco3$ScientificName == "coccolith","ScientificName"] <- "Prymnesiophyceae"
# Coccolithophorida                                    no match
cocco3[cocco3$ScientificName == "Coccolithophorida","ScientificName"] <- "Coccosphaerales"
# Coccolithophoridae, others                           no match
cocco3[cocco3$ScientificName == "Coccolithophoridae, others","ScientificName"] <- "Coccosphaerales"
# Holococcoliths                                       no match
cocco3[cocco3$ScientificName == "Holococcoliths","ScientificName"] <- "Prymnesiophyceae"
# Heterococcoliths                                     no match
cocco3[cocco3$ScientificName == "Heterococcoliths","ScientificName"] <- "Prymnesiophyceae"
# Coccolithinae                                        no match
cocco3[cocco3$ScientificName == "Coccolithinae","ScientificName"] <- "Coccolithaceae"
# Coccolithophores                                     no match
cocco3[cocco3$ScientificName == "Coccolithophores","ScientificName"] <- "Prymnesiophyceae"
# Holococco                                            no match
cocco3[cocco3$ScientificName == "Holococco","ScientificName"] <- "Prymnesiophyceae"
# Undescr. het.cocco A sensu Young et al. 2003         no match
cocco3[cocco3$ScientificName == "Undescr. het.cocco A sensu Young et al. 2003","ScientificName"] <- "Prymnesiophyceae"
# Unident. Coccolithophorid                            no match
cocco3[cocco3$ScientificName == "Unident. Coccolithophorid","ScientificName"] <- "Prymnesiophyceae"
# mixed coccolithophorids                              no match
cocco3[cocco3$ScientificName == "mixed coccolithophorids","ScientificName"] <- "Prymnesiophyceae"
# coccolithoph sexton                                  no match
cocco3[cocco3$ScientificName == "coccolithoph sexton","ScientificName"] <- "Prymnesiophyceae"
# coccolithophorid                                     no match
cocco3[cocco3$ScientificName == "coccolithophorid","ScientificName"] <- "Coccosphaerales"
# Coccolithophorids                                    no match
cocco3[cocco3$ScientificName == "Coccolithophorids","ScientificName"] <- "Coccosphaerales"
# holococcolithophores                                 no match
cocco3[cocco3$ScientificName == "holococcolithophores","ScientificName"] <- "Prymnesiophyceae"
# miscellaneous coccolithophores                       no match
cocco3[cocco3$ScientificName == "miscellaneous coccolithophores","ScientificName"] <- "Prymnesiophyceae"
# Coccolithus pelagicus
cocco3[cocco3$ScientificName == "Coccolithus pelagicus","ScientificName"] <- "Coccolithus pelagicus f. hyalinus"
# Rhabdosphaera clavigera var. stylifera
cocco3[cocco3$ScientificName == "Rhabdosphaera clavigera var. stylifera","ScientificName"] <- "Rhabdolithes claviger"
# Rhabdosphaera tignifer
cocco3[cocco3$ScientificName == "Rhabdosphaera tignifer","ScientificName"] <- "Rhabdolithes claviger"
# ACANTHOICA QUATTROSPINA
cocco3[cocco3$ScientificName == "ACANTHOICA QUATTROSPINA","ScientificName"] <- "Acanthoica quattrospina"
# Papposphaera borealis f. sagittifera
cocco3[cocco3$ScientificName == "Papposphaera borealis f. sagittifera","ScientificName"] <- "Papposphaera borealis"


# Re-re-run wormsbynames
keys.worms3 <- wormsbynames( unique(cocco3$ScientificName), marine_only = "false")  
keys.worms3$ScientificName <- unique(cocco3$ScientificName)
# keys.worms3[keys.worms3$status == "unaccepted" & !is.na(keys.worms3$status),c("ScientificName")]
keys.worms3[keys.worms3$status == "quarantined" & !is.na(keys.worms3$status),c("ScientificName")]
# quarantined species names

# Add WoRMS_status field
cocco3 <-  add_column(cocco3, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(cocco3)

# For testing:
# s <- unique(cocco3$ScientificName)[13] ; s
require("parallel")
res <- mclapply( unique(cocco3$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- cocco3[cocco3$ScientificName == s,]
            # dim(subset)
        
            ### 1st if else loop to check whether the species is actually found in 'w'
            if( is.na(keys.worms3[keys.worms3$ScientificName == s,"scientificname"]) ) {
            
                subset$WoRMS_ID <- factor("No match found in WoRMS; unassessed AlgaeBase entry")
                subset$WoRMS_status <- factor("No match found in WoRMS")
                subset$TaxonRank <- "Species" # because all are actual species names, except those 2 Chaetoceros subgenera
            
            } else if( !is.na(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"]) ) {
        
                subset$WoRMS_ID <- factor(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"])
                subset$TaxonRank <- keys.worms3[keys.worms3$ScientificName == s,"rank"]
                subset[,c("Kingdom","Phylum","Class","Order","Family","Genus")] <- keys.worms3[keys.worms3$ScientificName == s,c("kingdom","phylum","class","order","family","genus")]
            
            } # eo 1st if else loop - if species is actually found in 'w'
        
            ### 2nd if else loop to add species name if subset$TaxonRank == "Species"
            if( keys.worms3[keys.worms3$ScientificName == s,"rank"] == "Species" & !is.na(keys.worms3[keys.worms3$ScientificName == s,"rank"]) ) {
            
                subset$Species <- keys.worms3[keys.worms3$ScientificName == s,"valid_name"]
            
            } else {
            
                subset$Species <- NA
            
            } # eo 2nd if else loop - if rank == "Species" 
        
            ### 3rd if else loop to add subset$WoRMS_status
            if( !is.na(keys.worms3[keys.worms3$ScientificName == s,"valid_AphiaID"]) ) {
             
                statuses <- melt( keys.worms3[keys.worms3$ScientificName == s,c('ScientificName','isMarine','isBrackish','isFreshwater','isTerrestrial','isExtinct')], id.var = "ScientificName" )
                status2add <- paste(na.omit(statuses[statuses$value == 1 ,'variable']), collapse = "+")
                subset$WoRMS_status <- factor(status2add)
            
            } # eo 3rd for loop - for WoRMS_status
        
            return(subset)

        }, mc.cores = 25

) # eo mclapply - s in taxa_names
# Rbind
ddf <- bind_rows(res)
# dim(ddf) # 58'179      70
rm(res) ; gc()
# Check random lines
# ddf[5545:5555,]

### Check 
# unique(ddf$WoRMS_ID)
# unique(ddf[ddf$WoRMS_ID == "To_add_at_the_end","ScientificName"])
ddf[ddf$ScientificName == "Syracosphaera ossa","WoRMS_ID"] <- "627879"
ddf[ddf$ScientificName == "Syracosphaera ossa","WoRMS_status"] <- "isMarine"
ddf[ddf$ScientificName == "Syracosphaera ossa","TaxonRank"] <- "Species"
ddf[ddf$ScientificName == "Syracosphaera ossa","Kingdom"] <- "Chromista"
ddf[ddf$ScientificName == "Syracosphaera ossa","Phylum"] <- "Haptophyta"
ddf[ddf$ScientificName == "Syracosphaera ossa","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Syracosphaera ossa","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Syracosphaera ossa","Family"] <- "Syracosphaeraceae"
ddf[ddf$ScientificName == "Syracosphaera ossa","Genus"] <- "Syracosphaera" 
ddf[ddf$ScientificName == "Syracosphaera ossa","Species"] <- "Syracosphaera ossa" 


### Check those that have No match found in WoRMS - usuammy need to add their taxonomy manually - TaxonRank is usually OK
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS; unassessed AlgaeBase entry","ScientificName"])
### Correspond to those 'quarantined' species names ! Leave as is

# Add Kingdom+Phylum to all 
ddf$Kingdom <- "Chromista"
ddf$Phylum <- "Haptophyta"

#  [1] "Acanthoica ornata"
ddf[ddf$ScientificName == "Acanthoica ornata","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Acanthoica ornata","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Acanthoica ornata","Family"] <- "Rhabdosphaeraceae"
ddf[ddf$ScientificName == "Acanthoica ornata","Genus"] <- "Acanthoica" 
#  [2] "Alisphaera spatula"
ddf[ddf$ScientificName == "Alisphaera spatula","Class"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Alisphaera spatula","Order"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Alisphaera spatula","Family"] <- "Alisphaeraceae"
ddf[ddf$ScientificName == "Alisphaera spatula","Genus"] <- "Alisphaera" 
#  [3] "Alveosphaera bimurata"
ddf[ddf$ScientificName == "Alveosphaera bimurata","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Alveosphaera bimurata","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Alveosphaera bimurata","Family"] <- "Calciosoleniaceae"
ddf[ddf$ScientificName == "Alveosphaera bimurata","Genus"] <- "Alveosphaera" 
#  [4] "Anacanthoica cidaris"
ddf[ddf$ScientificName == "Anacanthoica cidaris","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Anacanthoica cidaris","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Anacanthoica cidaris","Family"] <- "Rhabdosphaeraceae"
ddf[ddf$ScientificName == "Anacanthoica cidaris","Genus"] <- "Anacanthoica" 
#  [5] "Anthosphaera periperforata"
ddf[ddf$ScientificName == "Anthosphaera periperforata","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Anthosphaera periperforata","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Anthosphaera periperforata","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Anthosphaera periperforata","Genus"] <- "Anthosphaera" 

#  [6] "Anthosphaera lafourcadii"
ddf[ddf$ScientificName == "Anthosphaera lafourcadii","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Anthosphaera lafourcadii","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Anthosphaera lafourcadii","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Anthosphaera lafourcadii","Genus"] <- "Anthosphaera" 

#  [7] "Emiliania huxleyi+Gephyrocapsa oceanica"
ddf[ddf$ScientificName == "Emiliania huxleyi+Gephyrocapsa oceanica","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Emiliania huxleyi+Gephyrocapsa oceanica","Order"] <- "Isochrysidales"
ddf[ddf$ScientificName == "Emiliania huxleyi+Gephyrocapsa oceanica","Family"] <- "Noelaerhabdaceae"
ddf[ddf$ScientificName == "Emiliania huxleyi+Gephyrocapsa oceanica","Genus"] <- "Emiliania+Gephyrocapsa" 

#  [8] "Calcidiscus leptoporus+Coccolithus pelagicus"
ddf[ddf$ScientificName == "Calcidiscus leptoporus+Coccolithus pelagicus","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Calcidiscus leptoporus+Coccolithus pelagicus","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Calcidiscus leptoporus+Coccolithus pelagicus","Family"] <- "Calcidiscaceae+Coccolithaceae"
ddf[ddf$ScientificName == "Calcidiscus leptoporus+Coccolithus pelagicus","Genus"] <- "Calcidiscus+Coccolithus" 

#  [9] "Calyptrolithina wettsteinii"
ddf[ddf$ScientificName == "Calyptrolithina wettsteinii","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Calyptrolithina wettsteinii","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Calyptrolithina wettsteinii","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Calyptrolithina wettsteinii","Genus"] <- "Calyptrolithina"

# [10] "Canistrolithus valliformis"
ddf[ddf$ScientificName == "Canistrolithus valliformis","Class"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Canistrolithus valliformis","Order"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Canistrolithus valliformis","Family"] <- "Alisphaeraceae"
ddf[ddf$ScientificName == "Canistrolithus valliformis","Genus"] <- "Canistrolithus"

# [11] "Ceratolithus lyramultiformis"
ddf[ddf$ScientificName == "Ceratolithus lyramultiformis","Class"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Ceratolithus lyramultiformis","Order"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Ceratolithus lyramultiformis","Family"] <- "Ceratolithaceae"
ddf[ddf$ScientificName == "Ceratolithus lyramultiformis","Genus"] <- "Ceratolithus"

# [12] "Coccolithus meteorii"
ddf[ddf$ScientificName == "Coccolithus meteorii","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Coccolithus meteorii","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Coccolithus meteorii","Family"] <- "Coccolithaceae"
ddf[ddf$ScientificName == "Coccolithus meteorii","Genus"] <- "Coccolithus"

# [13] "Corisphaera strigilis"
ddf[ddf$ScientificName == "Corisphaera strigilis","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Corisphaera strigilis","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Corisphaera strigilis","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Corisphaera strigilis","Genus"] <- "Corisphaera"

# [14] "Cyrtosphaera lecaliae"
ddf[ddf$ScientificName == "Cyrtosphaera lecaliae","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Cyrtosphaera lecaliae","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Cyrtosphaera lecaliae","Family"] <- "Rhabdosphaeraceae"
ddf[ddf$ScientificName == "Cyrtosphaera lecaliae","Genus"] <- "Cyrtosphaera"

# [15] "Discosphaera tubifera+Papposphaera lepida"
ddf[ddf$ScientificName == "Discosphaera tubifera+Papposphaera lepida","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Discosphaera tubifera+Papposphaera lepida","Order"] <- "Syracosphaerales+Coccosphaerales"
ddf[ddf$ScientificName == "Discosphaera tubifera+Papposphaera lepida","Family"] <- "Rhabdosphaeraceae+Papposphaeraceae"
ddf[ddf$ScientificName == "Discosphaera tubifera+Papposphaera lepida","Genus"] <- "Discosphaera+Papposphaera"

# [17] "Emiliania+Gephyrocapsa"
ddf[ddf$ScientificName == "Emiliania+Gephyrocapsa","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Emiliania+Gephyrocapsa","Order"] <- "Isochrysidales"
ddf[ddf$ScientificName == "Emiliania+Gephyrocapsa","Family"] <- "Noelaerhabdaceae"
ddf[ddf$ScientificName == "Emiliania+Gephyrocapsa","Genus"] <- "Emiliania+Gephyrocapsa"

# [18] "Halopappus+Calciopappus+Michaelsarsia"
ddf[ddf$ScientificName == "Halopappus+Calciopappus+Michaelsarsia","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Halopappus+Calciopappus+Michaelsarsia","Order"] <- "Coccosphaerales+Syracosphaerales"
ddf[ddf$ScientificName == "Halopappus+Calciopappus+Michaelsarsia","Family"] <- "Halopappaceae+Syracosphaeraceae"
ddf[ddf$ScientificName == "Halopappus+Calciopappus+Michaelsarsia","Genus"] <- "Halopappus+Calciopappus+Michaelsarsia"

# [19] "Helicosphaera pavimentum"
ddf[ddf$ScientificName == "Helicosphaera pavimentum","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Helicosphaera pavimentum","Order"] <- "Zygodiscales"
ddf[ddf$ScientificName == "Helicosphaera pavimentum","Family"] <- "Helicosphaeraceae"
ddf[ddf$ScientificName == "Helicosphaera pavimentum","Genus"] <- "Helicosphaera"

# [20] "Helladosphaera pienaarii"
ddf[ddf$ScientificName == "Helladosphaera pienaarii","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Helladosphaera pienaarii","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Helladosphaera pienaarii","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Helladosphaera pienaarii","Genus"] <- "Helladosphaera"

# [21] "Homozygosphaera arethusae"
ddf[ddf$ScientificName == "Homozygosphaera arethusae","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Homozygosphaera arethusae","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Homozygosphaera arethusae","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Homozygosphaera arethusae","Genus"] <- "Homozygosphaera"

# [22] "Halopappus splendens"
ddf[ddf$ScientificName == "Halopappus splendens","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Halopappus splendens","Order"] <- "Coccosphaerales"
ddf[ddf$ScientificName == "Halopappus splendens","Family"] <- "Halopappaceae"
ddf[ddf$ScientificName == "Halopappus splendens","Genus"] <- "Halopappus"

# [23] "Navilithus altivelum"
ddf[ddf$ScientificName == "Navilithus altivelum","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Navilithus altivelum","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Navilithus altivelum","Family"] <- "Incertae sedis"
ddf[ddf$ScientificName == "Navilithus altivelum","Genus"] <- "Navilithus"

# [24] "Syracosphaera confusa"
ddf[ddf$ScientificName == "Syracosphaera confusa","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Syracosphaera confusa","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Syracosphaera confusa","Family"] <- "Syracosphaeraceae"
ddf[ddf$ScientificName == "Syracosphaera confusa","Genus"] <- "Syracosphaera"

# [26] "Picarola margalefii"
ddf[ddf$ScientificName == "Picarola margalefii","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Picarola margalefii","Order"] <- "Coccosphaerales"
ddf[ddf$ScientificName == "Picarola margalefii","Family"] <- "Papposphaeraceae"
ddf[ddf$ScientificName == "Picarola margalefii","Genus"] <- "Picarola"

# [27] "Pontosphaera robusta"
ddf[ddf$ScientificName == "Pontosphaera robusta","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Pontosphaera robusta","Order"] <- "Zygodiscales"
ddf[ddf$ScientificName == "Pontosphaera robusta","Family"] <- "Pontosphaeraceae"
ddf[ddf$ScientificName == "Pontosphaera robusta","Genus"] <- "Pontosphaera"

# [28] "Poritectolithus maximus"
ddf[ddf$ScientificName == "Poritectolithus maximus","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Poritectolithus maximus","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Poritectolithus maximus","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Poritectolithus maximus","Genus"] <- "Poritectolithus"

# [29] "Sphaerocalyptra adenensis"
ddf[ddf$ScientificName == "Sphaerocalyptra adenensis","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Sphaerocalyptra adenensis","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Sphaerocalyptra adenensis","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Sphaerocalyptra adenensis","Genus"] <- "Sphaerocalyptra"

# [30] "Syracosphaera coronata"
ddf[ddf$ScientificName == "Syracosphaera coronata","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Syracosphaera coronata","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Syracosphaera coronata","Family"] <- "Syracosphaeraceae"
ddf[ddf$ScientificName == "Syracosphaera coronata","Genus"] <- "Syracosphaera"

# [31] "Syracosphaera hentschelii"
ddf[ddf$ScientificName == "Syracosphaera hentschelii","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Syracosphaera hentschelii","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Syracosphaera hentschelii","Family"] <- "Syracosphaeraceae"
ddf[ddf$ScientificName == "Syracosphaera hentschelii","Genus"] <- "Syracosphaera"

# [32] "Syracosphaera bannockii"
ddf[ddf$ScientificName == "Syracosphaera bannockii","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Syracosphaera bannockii","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Syracosphaera bannockii","Family"] <- "Syracosphaeraceae"
ddf[ddf$ScientificName == "Syracosphaera bannockii","Genus"] <- "Syracosphaera"

# [33] "Syracosphaera exigua"
ddf[ddf$ScientificName == "Syracosphaera exigua","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Syracosphaera exigua","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Syracosphaera exigua","Family"] <- "Syracosphaeraceae"
ddf[ddf$ScientificName == "Syracosphaera exigua","Genus"] <- "Syracosphaera"

# [34] "Syracosphaera spinosa"
ddf[ddf$ScientificName == "Syracosphaera spinosa","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Syracosphaera spinosa","Order"] <- "Syracosphaerales"
ddf[ddf$ScientificName == "Syracosphaera spinosa","Family"] <- "Syracosphaeraceae"
ddf[ddf$ScientificName == "Syracosphaera spinosa","Genus"] <- "Syracosphaera"

# [35] "Tetralithoides quadrilaminata"
ddf[ddf$ScientificName == "Tetralithoides quadrilaminata","Class"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Tetralithoides quadrilaminata","Order"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Tetralithoides quadrilaminata","Family"] <- "Haptophyta incertae sedis"
ddf[ddf$ScientificName == "Tetralithoides quadrilaminata","Genus"] <- "Tetralithoides"

# [36] "Hayaster perplexus"
ddf[ddf$ScientificName == "Hayaster perplexus","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Hayaster perplexus","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Hayaster perplexus","Family"] <- "Calcidiscaceae"
ddf[ddf$ScientificName == "Hayaster perplexus","Genus"] <- "Hayaster"

# [37] "Zygosphaera debilis"
ddf[ddf$ScientificName == "Zygosphaera debilis","Class"] <- "Prymnesiophyceae"
ddf[ddf$ScientificName == "Zygosphaera debilis","Order"] <- "Coccolithales"
ddf[ddf$ScientificName == "Zygosphaera debilis","Family"] <- "Calyptrosphaeraceae"
ddf[ddf$ScientificName == "Zygosphaera debilis","Genus"] <- "Zygosphaera"


# unique(ddf$WoRMS_status)
# unique(ddf[ddf$WoRMS_status == "","ScientificName"]) # same ones as above

### Check basal taxono (all should be Haptophyta or Prymnesiophyceae)
unique(ddf$Class) # Dinophyceae + some NA
unique(ddf[ddf$Class == "Dinophyceae" & !is.na(ddf$Class),"ScientificName"])
# "Thoracosphaera heimii" "Thoracosphaera"
### --> Actual Dinos! Remove
ddf <- ddf[-which(ddf$Class == "Dinophyceae"),]

### Check the NAs in 'Class'
# unique( ddf[is.na(ddf$Class),"ScientificName"] )
# unique( ddf[is.na(ddf$Order),"ScientificName"] )
ddf[ddf$Genus == "Alisphaera" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Calcioconus" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Acanthosolenia" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Canistrolithus" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Cribrosphaera" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Cribrosphaerella" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Gladiolithus" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Heimiella" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Polycrater" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Turrilithus" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Umbellosphaera" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"
ddf[ddf$Genus == "Ceratolithus" & !is.na(ddf$Genus),"Order"] <- "Haptophyta incertae sedis"

# And Family
# unique( ddf[is.na(ddf$Family),"ScientificName"] )
# Good. those names don't belong to a particular Family, directly from Order --> Genus

# And Genus
# unique( ddf[is.na(ddf$Genus),"ScientificName"] )

### Check the MeasurementValues
#summary(ddf$MeasurementValue) # No zeroes...
#summary(ddf$Biomass_mgCm3) # but a zeo here?
#min(ddf$MeasurementValue) ; min(ddf$Biomass_mgCm3, na.rm = T)
# Weird!
#ddf[is.na(ddf$Biomass_mgCm3),"MeasurementValue"] # not very low cell concentrations
#ddf[is.na(ddf$Biomass_mgCm3),"ScientificName"] # Ah ok, class-level obs, makes sense


### 19/04/22: Save new file
save(ddf, file = "AtlantECO_WP2_MAREDAT_Coccolithophores_OBrien2013_reformat+WoRMScheck_13_07_22.RData")
write.table(x = ddf, file = "AtlantECO_WP2_MAREDAT_Coccolithophores_OBrien2013_reformat+WoRMScheck_13_07_22.txt", sep = "\t")
gc()


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------