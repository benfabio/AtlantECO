
##### ATLANTECO SCRIPT 1.4.0 ----------------------------------------------------------------------------------------------------------------------------
##### 05/05/2021: R Script to examine and format the phyto & zooplankton abundances from COPEPOD-NOAA following the ATLANTECO WP2 template © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#   - Read the part 1 of COPEPOD_NOAA_all_zooplankton, fix the issue with ScientificNames
#   - Examine the str() and class()...
#   - Re-format to AtlantECO WP2 data template

module load R/4.0.3 # To load latest R version on kryo

### Latest update: 05/05/2021

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

### 1°) Read zooplankton abund data part 1
zoo <- read.csv("COPEPOD_NOAA_all_zooplankton_modified_28_04_2021_part1.csv", h = T, sep = ";")
dim(zoo)
head(zoo)
str(zoo)
summary(zoo)
colnames(zoo)

# Fix some issues: FilteredSeawater, OrigValue, OrigValueUnit, CountOrBiomassStandPerSurfAreaUnit, CountOrBiomassPerVolumeUnit
unique(zoo$FilteredSeawater)
zoo$FilteredSeawater <- str_replace_all(zoo$FilteredSeawater, " ", "")
zoo$FilteredSeawater <- str_replace_all(zoo$FilteredSeawater, "m3", " m3")

unique(zoo$OrigValue)
zoo$OrigValue <- str_replace_all(zoo$OrigValue, " ", "")

unique(zoo$OrigValueUnit)
zoo$OrigValueUnit <- str_replace_all(zoo$OrigValueUnit, " ", "")

unique(zoo$CountOrBiomassStandPerSurfAreaUnit)
zoo$CountOrBiomassStandPerSurfAreaUnit <- str_replace_all(zoo$CountOrBiomassStandPerSurfAreaUnit, " ", "")
zoo[zoo$CountOrBiomassStandPerSurfAreaUnit %in% c("----","-----") ,"CountOrBiomassStandPerSurfAreaUnit"] <- NA

unique(zoo$CountOrBiomassPerVolumeUnit)
zoo$CountOrBiomassPerVolumeUnit <- str_replace_all(zoo$CountOrBiomassPerVolumeUnit, " ", "")
zoo[zoo$CountOrBiomassPerVolumeUnit %in% c("----","-----") ,"CountOrBiomassPerVolumeUnit"] <- NA


### Now, fix OrigScientificName! based on OrigScientificNameWithMod
unique(zoo$OrigScientificNameWithMod)
unique(zoo$OrigScientificName)

# Strategy: split zoo$OrigScientificNameWithMod with '-'
newnames <- do.call(rbind, strsplit(x = as.character(zoo$OrigScientificNameWithMod), split = "-"))[,1]
newnames <- str_replace_all(newnames, "spp. ", "spp.")
newnames <- str_replace_all(newnames, "sp. ", "sp.")
newnames <- str_replace_all(newnames, " ", "_")
# unique( stringr::str_extract(newnames, "[^_]*_[^_]*") )
newnames <- stringr::str_extract(newnames, "[^_]*_[^_]*")
newnames <- str_replace_all(newnames, "_(except", "")
unique(newnames)

# Last thing: need to remove the underscore for stuff like 'Hyas_'...adjust manually
newnames[newnames == "Cladocera_"] <- "Cladocera"
newnames[newnames == "Copepoda_"] <- "Copepoda"
newnames[newnames == "Chaetognatha_"] <- "Chaetognatha"
newnames[newnames == "Appendicularia_"] <- "Appendicularia"
newnames[newnames == "Euphausiacea_"] <- "Euphausiacea"
newnames[newnames == "Siphonophorae_"] <- "Siphonophorae"
newnames[newnames == "Turbellaria_"] <- "Turbellaria"
newnames[newnames == "Crustacea_"] <- "Crustacea"
newnames[newnames == "Amphipoda_"] <- "Amphipoda"
newnames[newnames == "Thecosomata_"] <- "Thecosomata"
newnames[newnames == "Enteropneusta_"] <- "Enteropneusta"
newnames[newnames == "Echinodermata_"] <- "Echinodermata"
newnames[newnames == "Desmomyaria_"] <- "Desmomyaria"
newnames[newnames == "Fritillaridae_"] <- "Fritillaridae"
newnames[newnames == "Oikopleuridae_"] <- "Oikopleuridae"
newnames[newnames == "Ostracoda_"] <- "Ostracoda"
newnames[newnames == "Bivalvia_"] <- "Bivalvia"
newnames[newnames == "Ctenophora_"] <- "Ctenophora"
newnames[newnames == "Ascidiacea_"] <- "Ascidiacea"
newnames[newnames == "Polychaeta_"] <- "Polychaeta"
newnames[newnames == "Cyclomyaria_"] <- "Cyclomyaria"
newnames[newnames == "Ectoprocta_"] <- "Ectoprocta"
newnames[newnames == "Beroe_"] <- "Beroe"
newnames[newnames == "Cirripedia_"] <- "Cirripedia"
newnames[newnames == "Branchiostoma_"] <- "Branchiostoma"
newnames[newnames == "Aegisthus_"] <- "Aegisthus"
newnames[newnames == "Actinia_"] <- "Actinia_"
newnames[newnames == "Rotaria_"] <- "Rotaria"
newnames[newnames == "Gaidius_&"] <- "Gaidius_sp."
newnames[newnames == "Balanidae_"] <- "Balanidae"
newnames[newnames == "Bopyridae_"] <- "Bopyridae"
newnames[newnames == "Gammaridae_"] <- "Gammaridae"
newnames[newnames == "Cnidaria_"] <- "Cnidaria"
newnames[newnames == "Pleurobrachia_&"] <- "Pleurobrachia"
newnames[newnames == "Gammaridae_unidentified"] <- "Gammaridae"
newnames[newnames == "Decapoda_"] <- "Decapoda"
newnames[newnames == "Jellyfishes_(except"] <- "Jellyfish"
newnames[newnames == "Cumacea_"] <- "Cumacea"
newnames[newnames == "Diaxis_(sars"] <- "Diaxis_sp."
newnames[newnames == "Metridia_&"] <- "Metridia_sp."
newnames[newnames == "Lophogastrida_"] <- "Lophogastrida"
newnames[newnames == "Isopoda_"] <- "Isopoda"
newnames[newnames == "Polychaeta_(except"] <- "Polychaeta"
newnames[newnames == "Gastropoda_"] <- "Gastropoda"
newnames[newnames == "Phoronida_"] <- "Phoronida"
newnames[newnames == "Harpacticoida_"] <- "Harpacticoida"
newnames[newnames == "Mysidae_"] <- "Mysidae"
newnames[newnames == "Harpacticoida_unidentified"] <- "Harpacticoida"
newnames[newnames == "Polychaeta_unidentified"] <- "Polychaeta"
newnames[newnames == "Bivalvia_unidentified"] <- "Bivalvia"
newnames[newnames == "Gastropoda_unidentified"] <- "Gastropoda"
newnames[newnames == "Cladocera_unidentified"] <- "Cladocera"
newnames[newnames == "Calanoida_unidentified"] <- "Calanoida"
newnames[newnames == "Zooplankton_unidentified"] <- "Zooplankton"
newnames[newnames == "Rotifera_unidentified"] <- "Rotifera"
newnames[newnames == "Copepoda_unidentified"] <- "Copepoda"
newnames[newnames == "Cyclopoida_unidentified"] <- "Cyclopoida"
newnames[newnames == "Mysidae_unidentified"] <- "Mysidae"
newnames[newnames == "Salpa_"] <- "Salpa"
newnames[newnames == "Shrimp_"] <- "Shrimp_unidentified"
newnames[newnames == "Cephalopoda_spp."] <- "Cephalopoda"
newnames[newnames == "Porifera_"] <- "Porifera"
newnames[newnames == "Jellyfish_"] <- "Jellyfish"
newnames[newnames == "Hydrozoa_"] <- "Hydrozoa"
newnames[newnames == "Scyphozoa_"] <- "Scyphozoa"
newnames[newnames == "Tetraplatia_"] <- "Tetraplatia"
newnames[newnames == "Anthozoa_"] <- "Anthozoa"
newnames[newnames == "Platyhelminthes_"] <- "Platyhelminthes"
newnames[newnames == "Nemertea_"] <- "Nemertea"
newnames[newnames == "Nemata_"] <- "Nemata"
newnames[newnames == "Insecta_"] <- "Insecta"
newnames[newnames == "Stomatopoda_"] <- "Stomatopoda"
newnames[newnames == "Hyperiidae_"] <- "Hyperiidae"
newnames[newnames == "Caprellidae_"] <- "Caprellidae"
newnames[newnames == "Anomura_"] <- "Anomura"
newnames[newnames == "Brachyura_"] <- "Brachyura"
newnames[newnames == "Lucifer_"] <- "Photonectes"
newnames[newnames == "Penaeidae_"] <- "Penaeidae"
newnames[newnames == "Scyllaridea_"] <- "Scyllaridea"
newnames[newnames == "Sergestidae_"] <- "Sergestidae"
newnames[newnames == "Halacaridae_"] <- "Halacaridae"
newnames[newnames == "Cephalopoda_"] <- "Cephalopoda"
newnames[newnames == "Brachiopoda_"] <- "Brachiopoda"
newnames[newnames == "Hemichordata_"] <- "Hemichordata"
newnames[newnames == "Pyrosomatidae_"] <- "Pyrosomatidae"
newnames[newnames == "Thaliacea_"] <- "Thaliacea"
newnames[newnames == "Doliolidae"] <- "Doliolidae"
newnames[newnames == "Salpidae_"] <- "Salpidae"
newnames[newnames == "Porcellanidae_"] <- "Porcellanidae"
newnames[newnames == "Arachnida_"] <- "Arachnida"
newnames[newnames == "Oligochaeta_"] <- "Oligochaeta"
newnames[newnames == "Paguroidea_"] <- "Paguroidea"
newnames[newnames == "Hydromedusae_"] <- "Hydromedusae"
newnames[newnames == "Euphausiidae_"] <- "Euphausiidae"
newnames[newnames == "Decapoda_other"] <- "Decapoda"
newnames[newnames == "Palinuridae_"] <- "Palinuridae"
newnames[newnames == "Pteropods_gymnosomes"] <- "Gymnosomata"
newnames[newnames == "Pteropods_thecosomes"] <- "Thecosomata"
newnames[newnames == "Salpida_"] <- "Salpida"
newnames[newnames == "Zooplankton_TOTAL"] <- "Zooplankton"
newnames[newnames == "Salps_&"] <- "Salpida"
newnames[newnames == "Lucifer_sp."] <- "Photonectes_sp."
newnames[newnames == "Sergestidae_other"] <- "Sergestidae"
newnames[newnames == "Calyconectae_"] <- "Calyconectae"
newnames[newnames == "Sagittidea_"] <- "Sagittoidea"
newnames[newnames == "Anthoathecatae_"] <- "Anthoathecatae"
newnames[newnames == "Alima_"] <- "Alima"
newnames[newnames == "Eupagurus_"] <- "Eupagurus"
newnames[newnames == "Chaetognatha_unidentified"] <- "Chaetognatha"
newnames[newnames == "Euphausiacea_&"] <- "Euphausiacea"
newnames[newnames == "Heteropoda_&"] <- "Heteropoda"
newnames[newnames == "Sagitta_"] <- "Sagitta"
newnames[newnames == "Doliolum_"] <- "Doliolum"
newnames[newnames == "Oikopleura_"] <- "Oikopleura"
newnames[newnames == "Squid_"] <- "Squid_unidentified"
newnames[newnames == "Caridea_"] <- "Caridea"
newnames[newnames == "Luciferidae_"] <- "Luciferidae"
newnames[newnames == "Atlantidae_"] <- "Atlantidae"
newnames[newnames == "Mollusca_other"] <- "Mollusca"
newnames[newnames == "Pyrosomida_"] <- "Pyrosomida"
newnames[newnames == "Doliolida_"] <- "Doliolida"
newnames[newnames == "Portunidae_"] <- "Portunidae"
newnames[newnames == "Xanthidae_"] <- "Xanthidae"
newnames[newnames == "Grapsidae_"] <- "Grapsidae"
newnames[newnames == "Cranchiidae_"] <- "Cranchiidae"
newnames[newnames == "Jellyfish_other"] <- "Jellyfish"
newnames[newnames == "Calanoida_"] <- "Calanoida"
newnames[newnames == "Cyclopoida_"] <- "Cyclopoida"
newnames[newnames == "Thalia_"] <- "Thalia_sp."
newnames[newnames == "Hydroidolina_"] <- "Hydroidolina"
newnames[newnames == "Zooplankton_miscellaneous"] <- "Zooplankton"
newnames[newnames == "Sagittoidea_"] <- "Sagittoidea"
newnames[newnames == "Tunicata_"] <- "Tunicata"
newnames[newnames == "Gymnosomata_"] <- "Gymnosomata"
newnames[newnames == "Mollusca_"] <- "Mollusca"
newnames[newnames == "Annelida_"] <- "Annelida"
newnames[newnames == "Scaphopoda_"] <- "Scaphopoda"
newnames[newnames == "Mollusca_other/unidentified"] <- "Mollusca"
newnames[newnames == "Amphipoda_spp."] <- "Amphipoda"
newnames[newnames == "Decapoda_spp."] <- "Decapoda"
newnames[newnames == "Calanus_"] <- "Calanus_sp."
newnames[newnames == "Scinidae_spp."] <- "Scinidae"
newnames[newnames == "Mysidae_spp."] <- "Mysidae"
newnames[newnames == "Calanoida_spp."] <- "Calanoida"
newnames[newnames == "Siphonophorae_spp"] <- "Siphonophorae"
newnames[newnames == "Ctenophora_spp."] <- "Ctenophora"
newnames[newnames == "Euphausiacea_spp."] <- "Euphausiacea"
newnames[newnames == "Hydromedusae_other"] <- "Hydromedusae"
newnames[newnames == "Nudibranchia_"] <- "Nudibranchia"
newnames[newnames == "Protochordate_"] <- "Protochordata"
newnames[newnames == "Zooplankton_spp."] <- "Zooplankton"
newnames[newnames == "Asteroidea_"] <- "Asteroidea"
newnames[newnames == "Scyphozoa_spp."] <- "Scyphozoa"
newnames[newnames == "Chaetognatha_spp."] <- "Chaetognatha"
newnames[newnames == "Appendicularia_spp."] <- "Appendicularia"
newnames[newnames == "Ostracoda_spp."] <- "Ostracoda"
newnames[newnames == "Hexacorallia_spp."] <- "Hexacorallia"
newnames[newnames == "Acartia_"] <- "Acartia_sp."
newnames[newnames == "Carinariidae_"] <- "Carinariidae"
newnames[newnames == "Hyas_"] <- "Hyas_spp."
newnames[newnames == "Zooplankton_"] <- "Zooplankton"
newnames[newnames == "Zooplankton_sp.A"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.B"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.C"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.D"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.E"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.F"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.G"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.H"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.I"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.J"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.K"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.L"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_sp.M"] <- "Zooplankton_sp."
newnames[newnames == "Zooplankton_other"] <- "Zooplankton"

unique(newnames)
length(newnames) ; dim(zoo)

zoo$OrigScientificName <- newnames
# zoo[16700:16750,c("OrigScientificNameWithMod","OrigScientificName")]

write.table(zoo, file = "COPEPOD_NOAA_all_zooplankton_modified_04_05_2021_part1.txt", sep = "\t")
# Check
test <- read.table("COPEPOD_NOAA_all_zooplankton_modified_04_05_2021_part1.txt", h = T, sep = "\t")
dim(test)
str(test)
colnames(test)


### ------------------------------------------------------------

### Like above, but for zooplankton part 2
zoo2 <- read.csv("COPEPOD_NOAA_all_zooplankton_modified_28_04_2021_part2.csv", h = T, sep = ";", dec = ",")
head(zoo2) ; dim(zoo2)
colnames(zoo2)
str(zoo2)
summary(zoo2)
unique(zoo2$decimalLatitude) ; unique(zoo2$decimalLongitude)

unique(zoo2$FilteredSeawater)
zoo2$FilteredSeawater <- str_replace_all(zoo2$FilteredSeawater, " ", "")
zoo2$FilteredSeawater <- str_replace_all(zoo2$FilteredSeawater, "m3", " m3")
zoo2[zoo2$FilteredSeawater == "","FilteredSeawater"] <- NA

unique(zoo2$OrigValueUnit)
zoo2$OrigValueUnit <- str_replace_all(zoo2$OrigValueUnit, " ", "")

unique(zoo2$CountOrBiomassStandPerSurfAreaUnit)
zoo2$CountOrBiomassStandPerSurfAreaUnit <- str_replace_all(zoo2$CountOrBiomassStandPerSurfAreaUnit, " ", "")
zoo2[zoo2$CountOrBiomassStandPerSurfAreaUnit %in% c("----","-----") ,"CountOrBiomassStandPerSurfAreaUnit"] <- NA

unique(zoo2$CountOrBiomassPerVolumeUnit)
zoo2$CountOrBiomassPerVolumeUnit <- str_replace_all(zoo2$CountOrBiomassPerVolumeUnit, " ", "")
zoo2[zoo2$CountOrBiomassPerVolumeUnit %in% c("----","-----") ,"CountOrBiomassPerVolumeUnit"] <- NA


### Now, fix OrigScientificName! based on OrigScientificNameWithMod
unique(zoo2$OrigScientificNameWithMod)
unique(zoo2$OrigScientificName)

newnames <- str_replace_all(zoo2$OrigScientificName, " ", "_")
unique(newnames)

### Last thing...adjust manually
newnames[newnames == "Eucalanus"] <- "Eucalanus_sp."
newnames[newnames == "Metridia"] <- "Metridia_sp."
newnames[newnames == "Pseudocalanus"] <- "Pseudocalanus_sp."
newnames[newnames == "Scolecithricella"] <- "Scolecithricella_sp."
newnames[newnames == "Oithona"] <- "Oithona_sp."
newnames[newnames == "Racovitzanus"] <- "Racovitzanus_sp."
newnames[newnames == "Euchaeta"] <- "Euchaeta_sp."
newnames[newnames == "Scaphocalanus"] <- "Scaphocalanus_sp."
newnames[newnames == "Acartia"] <- "Acartia_sp."
newnames[newnames == "Chiridius"] <- "Chiridius_sp."
newnames[newnames == "Aetideus"] <- "Aetideus_sp."
newnames[newnames == "Oncaea"] <- "Oncaea_sp."
newnames[newnames == "Candacia"] <- "Candacia_sp."
newnames[newnames == "Gaetanus"] <- "Gaetanus_sp."
newnames[newnames == "Spinocalanus"] <- "Spinocalanus_sp."
newnames[newnames == "Gaidius"] <- "Gaetanus_sp."
newnames[newnames == "Microcalanus"] <- "Microcalanus_sp."
newnames[newnames == "Metazoa_(sponges;_cnidarians;_ctenophores)"] <- "Metazoa_(sponges_cnidarians_ctenophores)"
newnames[newnames == "Bosmina_coregoni_maritima"] <- "Bosmina_coregoni"
newnames[newnames == "Pseudocalanus_minutus_elongatus"] <- "Pseudocalanus_minutus+elongatus"
newnames[newnames == "Daphnia_cristata_cristata"] <- "Daphnia_cristata"
newnames[newnames == "Daphnia_cucullata_cucullata"] <- "Daphnia_cucullata"
newnames[newnames == "Cyclops_oithonoides_;_g_o"] <- "Mesocyclops_oithonoides"
newnames[newnames == "Harpacticoida_unidentified"] <- "Harpacticoida"
newnames[newnames == "Cyclopoida_unidentified"] <- "Cyclopoida"

unique(newnames)

length(newnames) ; dim(zoo2)

zoo2$OrigScientificName <- newnames
# zoo[16700:16750,c("OrigScientificNameWithMod","OrigScientificName")]

write.table(zoo2, file = "COPEPOD_NOAA_all_zooplankton_modified_05_05_2021_part2.txt", sep = "\t")
# Check
test <- read.table("COPEPOD_NOAA_all_zooplankton_modified_05_05_2021_part2.txt", h = T, sep = "\t")
dim(test)
str(test)

### 12/10/2021: Checking something for Nielja Knecht (MSc ETHZ)
zoo1 <- get(load("COPEPOD_NOAA_all_zooplankton_modified+WoRMScheck_part1_01_06_2021.RData"))
zoo2 <- get(load("COPEPOD_NOAA_all_zooplankton_modified+WoRMScheck_part2_01_06_2021.RData"))

dim(zoo1) # 167'577
str(zoo1)
dim(zoo2) # 56'950
str(zoo2)

# Rbind
zoo <- rbind(zoo1, zoo2)
dim(zoo)

### Was not formatted to AtlantECO WP2 stanrds yet! Oops...
unique(zoo$OrigScientificName)
summary(factor(zoo$ScientificName))

# Sub pteropod data
sub <- zoo[zoo$OrigScientificName %in% c("Limacina_retroversa","Limacina_spp.","Thecosomata","Limacina_helicina","Clione_limacina","Heteropoda","Gastropoda","Clio_sp.","Clione_spp."),]
dim(sub) # 7603
# With actual counts: sub[!is.na(sub$CountOrBiomassPerVolume),]
dim(sub[!is.na(sub$CountOrBiomassPerVolume),]) # 7395
colnames(sub)
summary(sub)

# Quick map
world <- map_data("world")
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = sub[!is.na(sub$CountOrBiomassPerVolume),]) + coord_quickmap() +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 



### ------------------------------------------------------------

### And for phytoplankton abund now:
phyto <- read.csv("COPEPOD_NOAA_all_phytoplankton_modified_28_04_2021.csv", h = T, sep = ";", dec = ",")
head(phyto) ; dim(phyto)
colnames(phyto)
str(phyto)
summary(phyto)

# Fix some issues: FilteredSeawater, OrigValue, OrigValueUnit, CountOrBiomassStandPerSurfAreaUnit, CountOrBiomassPerVolumeUnit
unique(phyto$FilteredSeawater)
phyto$FilteredSeawater <- str_replace_all(phyto$FilteredSeawater, " ", "")
phyto$FilteredSeawater <- str_replace_all(phyto$FilteredSeawater, "m3", " m3")
phyto[phyto$FilteredSeawater == "null","FilteredSeawater"] <- NA

unique(phyto$OrigValue)
phyto$OrigValue <- str_replace_all(phyto$OrigValue, " ", "")

unique(phyto$OrigValueUnit)
phyto$OrigValueUnit <- str_replace_all(phyto$OrigValueUnit, " ", "")

unique(phyto$CountOrBiomassStandPerSurfAreaUnit)
phyto$CountOrBiomassStandPerSurfAreaUnit <- str_replace_all(phyto$CountOrBiomassStandPerSurfAreaUnit, " ", "")
phyto[phyto$CountOrBiomassStandPerSurfAreaUnit %in% c("----","-----") ,"CountOrBiomassStandPerSurfAreaUnit"] <- NA

unique(phyto$CountOrBiomassPerVolumeUnit)
phyto$CountOrBiomassPerVolumeUnit <- str_replace_all(phyto$CountOrBiomassPerVolumeUnit, " ", "")
phyto[phyto$CountOrBiomassPerVolumeUnit %in% c("----","-----") ,"CountOrBiomassPerVolumeUnit"] <- NA


### Now, fix OrigScientificName! based on OrigScientificNameWithMod
# unique(phyto$OrigScientificNameWithMod)
# unique(phyto$OrigScientificName)
# phyto[1:50,c("OrigScientificNameWithMod","OrigScientificName")]

newnames <- str_replace_all(phyto$OrigScientificName, " ", "_")
unique(newnames)

newnames[newnames == "Phytoplankton_TOTAL_(total_count)"] <- "Phytoplankton_total"
newnames[newnames == "Peridinium"] <- "Peridinium_sp."
newnames[newnames == "Prorocentrum"] <- "Prorocentrum_sp."
newnames[newnames == "Amphidinium"] <- "Amphidinium_sp."
newnames[newnames == "Pronoctiluca"] <- "Pronoctiluca_sp."
newnames[newnames == "Thalassiosira"] <- "Thalassiosira_sp."
newnames[newnames == "Dinophysis"] <- "Dinophysis_sp."
newnames[newnames == "Rhizosolenia"] <- "Rhizosolenia_sp."
newnames[newnames == "Chaetoceros"] <- "Chaetoceros_sp."
newnames[newnames == "Pennales"] <- "Pennates"
newnames[newnames == "Vorticella"] <- "Vorticella_sp."
newnames[newnames == "Sarcodina"] <- "Sarcomastigota"
newnames[newnames == "Chaetoceros_(hyalochaetae)_spp."] <- "Chaetoceros_(Hyalochaete)_spp."
newnames[newnames == "Dinophyceae_other"] <- "Dinophyceae"
newnames[newnames == "Dinophyceae_TOTAL_(total_count)"] <- "Dinophyceae_total"
newnames[newnames == "Pennales_other"] <- "Pennates"
newnames[newnames == "Protozoa_other"] <- "Protozoa"
newnames[newnames == "Dictyocha_messanensis_forma_spinosa"] <- "Dictyocha_messanensis_spinosa"
newnames[newnames == "Radiolaria_spp."] <- "Radiolaria"
newnames[newnames == "Alexandrium"] <- "Alexandrium_sp."
newnames[newnames == "Hemiselmis"] <- "Hemiselmis_sp."
newnames[newnames == "Gomphonema"] <- "Gomphonema_sp."
newnames[newnames == "Adenoides_kofoidi"] <- "Adenoides_kofoidii"
newnames[newnames == "Calciosolenia_granii_v_cylindrothecaf"] <- "Calciosolenia_granii_var._cylindrotheciformis"
newnames[newnames == "Pennales_sp."] <- "Pennates"
newnames[newnames == "Microzooplankton_(_<_200_um_)"] <- "Microzooplankton"
newnames[newnames == "Chaetoceros_(phaeoceros)_spp."] <- "Chaetoceros_(Phaeoceros)_spp."
newnames[newnames == "Ceratium"] <- "Ceratium_sp."
newnames[newnames == "Amphorella_quadrilineata/minor"] <- "Amphorides_quadrilineata"
newnames[newnames == "Dadayiella_ganymedes/bulbosa"] <- "Dadayiella_ganymedes"
newnames[newnames == "Helicostomella_longa_(syn)"] <- "Helicostomella_longa"
newnames[newnames == "Steenstrupiella_steenstrupii/robusta"] <- "Steenstrupiella_steenstrupii"
newnames[newnames == "Fragilariopsis_f_bouvet"] <- "Fragilariopsis_sp."
newnames[newnames == "Noctiluca"] <- "Noctiluca_sp."
newnames[newnames == "Coscinodiscus"] <- "Coscinodiscus_sp."
newnames[newnames == "Pleurosigma"] <- "Pleurosigma_sp."
newnames[newnames == "Navicula"] <- "Navicula_sp."
newnames[newnames == "Dinoflagellida"] <- "Dinophyceae"
newnames[newnames == "Noctulica_&_other_dinoflagellata"] <- "Dinoflagellata"
newnames[newnames == "Pyrocystis"] <- "Pyrocystis_sp."
newnames[newnames == "Thalassiothrix"] <- "Thalassiothrix_sp."
newnames[newnames == "Hemidiscus"] <- "Hemidiscus_sp."
newnames[newnames == "Nitzschia"] <- "Nitzschia_sp."
newnames[newnames == "Isthmia"] <- "Isthmia_sp."
newnames[newnames == "Amphiprora_kjellmanii_striolata"] <- "Amphiprora_kjellmanii_var._kjellmanii"
newnames[newnames == "Actinocyclus_normanii_subsala"] <- "Coscinodiscus_subsalsus"
newnames[newnames == "Chaetoceros_atlanticum_neapolitanum"] <- "Chaetoceros_atlanticum"
newnames[newnames == "Pseudo-nitzschia_seriata_seriata"] <- "Pseudo-nitzschia_seriata"
newnames[newnames == "Thalassiothrix_mediterranea_pacifica"] <- "Thalassiothrix_mediterranea"

# Check results
unique(newnames)

length(newnames) ; dim(phyto)

phyto$OrigScientificName <- newnames
# phyto[5000:5030,c("OrigScientificNameWithMod","OrigScientificName")]

write.table(phyto, file = "COPEPOD_NOAA_all_phytoplankton_microzooplankton_modified_05_05_2021.txt", sep = "\t")
# Check
dim(phyto[92920,])
test <- read.table("COPEPOD_NOAA_all_phytoplankton_microzooplankton_modified_05_05_2021.txt", h = T)
dim(test)
str(test)



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
