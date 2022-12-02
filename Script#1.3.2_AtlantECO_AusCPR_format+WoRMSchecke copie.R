
##### ATLANTECO SCRIPT 1.3.2 ----------------------------------------------------------------------------------------------------------------------------
##### 12/10/2021: R Script to reformat the Australian CPR data shared by Claire Davies (CSIRO) and Anthony J. Richardson (CSIRO) © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### All data available on GitHub: https://github.com/PlanktonTeam/IMOS_Toolbox/tree/master/Plankton/Output
### Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure Strategy (NCRIS).

### Aims to:
# - Read the .csv files for Phyto and Zooplankton
# - Reformat to AtlantECO WP2 standards. Use Richardson et al. (2006) - https://doi.org/10.1016/j.pocean.2005.09.011 and the catalogues kindly forwaded by Claire Davies to inform metadata: 
# Phytoplankton: https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/catalog.search#/metadata/c1344979-f701-0916-e044-00144f7bc0f4 
# Zooplankton: https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/catalog.search#/metadata/c1344e70-480e-0993-e044-00144f7bc0f4
# - Check and correct the taxonomic classification based on WoRMS

### Latest update: 27/10/2021

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("readxl")
library("lubridate")
library("worms")

world <- map_data("world")  # for maps

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") # working dir
dir()

### ------------------------------------------------------------------------------------------------------------------------------------------------------

### 1°) Zooplankton (starting with zooplankto instead of phytoplankton for Nielja)
zoo <- read.csv("AusCPR_Zoop_RawMat.csv", h = T, sep = ",", dec = ".") # come separated, decimals indicated by dots
dim(zoo) # 7056 observations of 1045 taxa (8 metadata columns)
colnames(zoo)
str(zoo[,1:10])
summary(zoo)
# Values in counts represent #/m3
### IMPORTANT: The data are made available in binned products for ease of use. As taxonomy changes over time and the ability of analysts improves with training more and more species are able to be identified over time. The products account for these changes and for real absences from the samples. A 0 abundance means that the species was looked for and not seen, -999 means that the species was not looked for in that sample.

# Coordinates, dates etc. look quite OK.
# Just convert SampleDateUTC to vector of class vector
#str(zoo$SampleDateUTC)
#str(as.Date(zoo$SampleDateUTC))
zoo$SampleDateUTC <- as.Date(zoo$SampleDateUTC)

### Before melting and reformatting to AtlantECO data, modify the taxa colnames to avoid the extra dots etc.
# gsub("...", "_", colnames(zoo), fixed = TRUE)
colnames(zoo) <- gsub("...", "_", colnames(zoo), fixed = T)
colnames(zoo) <- gsub("..", "_", colnames(zoo), fixed = T)
# colnames(zoo)
# Let's clean those labels later (durig the WoRMS ID check)

### Melt
m.zoo <- melt(zoo, id.vars = c("TripCode","Latitude","Longitude","SampleDateUTC","Year","Month","Day","Time_24hr"))
head(m.zoo) ; dim(m.zoo) # 7'373'520 observations now
# summary(m.zoo) # Check range of concentrations values
# unique(m.zoo$value)[order(unique(m.zoo$value))]

### Format to WP2 standards
# colnames(m.zoo)

m.zoo2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Claire_Davies",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;imos-plankton@csiro.au", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = m.zoo$Latitude, decimalLongitude = m.zoo$Longitude, geodeticDatum = "WGS84", 
                CoordUncertainty = "Comparison between the calculated position and data from vessels where a GPS record was available suggests the position assigned to CPR samples is accurate to within 10–20 nautical miles.",
                CountryCode = NA, eventDate = m.zoo$SampleDateUTC,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = m.zoo$Year, Month = m.zoo$Month, Day = m.zoo$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = 7, DepthAccuracy = "2-3m", DepthIntegral = NA,
                MinDepth = 5, MaxDepth = 10, ParentEventID = "AusCPR", EventID = m.zoo$TripCode, InstitutionCode = "Australia’s Integrated Marine Observing System (IMOS)",
                SourceArchive = "https://github.com/PlanktonTeam/IMOS_Toolbox/blob/master/Plankton/Output/CPR_Zoop_RawMat.csv",
                OrigCollectionCode = "c1344e70-480e-0993-e044-00144f7bc0f4", OrigCollectionID = "IMOS (http://imos.aodn.org.au/webportal/)",
                BiblioCitation = "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)",
                CitationDOI = "https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/catalog.search#/metadata/c1344e70-480e-0993-e044-00144f7bc0f4",
                DateDataAccess = "12-10-2021",
                OrigScientificName = m.zoo$variable, ScientificName = m.zoo$variable,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = m.zoo$value,
                MeasurementUnit = "#/m3", MeasurementAcurracy = "A 0 abundance = species was looked for and not seen, -999 = species was not looked for in that sample.",
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = "Australian CPR survey - Methods described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011)",
                SamplingProtocol = "The zooplankton analysis is conducted differently to that described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011) as it is counted off the 270µm silk in a Bogorov tray. This is accomplished by rinsing the silks in water and straining through a 10 micron mesh sieve. The collected plankton is transferred to a bogorov tray and counted under a dissecting scope. This is done to retain the phytoplankton. After counting the zooplankton and phytoplankton are stored with the silk segment in PGP preservative.",
                SampleAmount = "The volume of water filtered for each 10 nautical mile sample is about 3m3 (mean = 3.27 m3, SD = 0.71 m3, n = 1723, Jonas et al., 2004). But AusCPR does 5 nautical miles instead of 10",
                SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = "Servicing and maintenance of the CPRs and analysis of the samples for the AusCPR survey will be carried out by staff based at the O&A CSIRO laboratories in Queensland and Hobart and at the AAD in Hobart.",
                DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
head(m.zoo2)
dim(m.zoo2) # 7'373'520     70
str(m.zoo2)

# Check scientific names used
unique(m.zoo2$ScientificName); str(m.zoo2$ScientificName)

### Need to modify most names manually because they will cause problems (extra blank spaces, typos in names, 8 instead of brackets etc...)
### We can keep the original labels in 'OrigScientificName' and use them to inform 'LifeForm' (juveniles, larvae etc.) in the "LifeForm" column

### First, in 'ScientificName', start removing the life stages and sex indicators listed below: 
# .spp., .f, .m, .j, _f, _m, _j, .CIV, .CV, _small., .gravid, _1.00mm., _1.00mm.x.1.2mm., _1.2mm., .damaged_unidentified, _0.4mm
# _0.7mm, .0.4_0.7mm, .hump.f, .no.hump.f, .agg, .sol, .adult, .FI, .FII, .FIII, .FIV, .FV, .FVI, .CII, .CIII, 

m.zoo2$ScientificName <- gsub(".CIII", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".CII", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".FVI", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".FV", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".FIV", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".FIII", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".FII", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".FI", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".adult", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".sol", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".agg", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".no.hump.f", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".hump.f", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".0.4_0.7mm", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_0.7mm", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_0.4mm", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".damaged_unidentified", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_1.2mm.", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_1.00mm.x.1.2mm.", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_1.00mm.", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".gravid", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_small.", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".CV", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".CIV", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_j", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_i", "", as.character(m.zoo2$ScientificName), fixed = T) # Acrocalanus.spp_i ?? i stands for...?
m.zoo2$ScientificName <- gsub("_f", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub("_m", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".j", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".m", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".f", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".spp.", "", as.character(m.zoo2$ScientificName), fixed = T)
# unique(m.zoo2$ScientificName)

### Remove '.spp' and add " " for '.'
m.zoo2$ScientificName <- gsub(".spp", "", as.character(m.zoo2$ScientificName), fixed = T)
m.zoo2$ScientificName <- gsub(".", " ", as.character(m.zoo2$ScientificName), fixed = T)
# unique(m.zoo2$ScientificName)

### Now, give worms::wormsbynames a first try and manually correct the labels that won't fit
### You'll use the OrigScientifcName to give proper lifestages etc.
require("worms") 
keys.worms <- wormsbynames( unique(m.zoo2$ScientificName) )

# Time to correct all of this...
# str(m.zoo2$ScientificName) # chr not factor

m.zoo2[m.zoo2$ScientificName == "Acartia_Acanthacartiaossae","ScientificName"] <- "Acartia (Acanthacartia) fossae"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acanthacartia_pietschmani","ScientificName"] <- "Acartia (Acanthacartia) pietschmani"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acanthacartia_sinjiensis","ScientificName"] <- "Acartia (Acanthacartia) sinjiensis"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acanthacartia_tonsa","ScientificName"] <- "Acartia (Acanthacartia) tonsa"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acartia_danae","ScientificName"] <- "Acartia (Acartia) danae"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acartia_negligens","ScientificName"] <- "Acartia (Acartia) negligens"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acartiura_clausi","ScientificName"] <- "Acartia (Acartiura) clausi"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acartiura_simplex","ScientificName"] <- "Acartia (Acartiura) simplex"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acartiura_spp","ScientificName"] <- "Acartia (Acartiura)"
m.zoo2[m.zoo2$ScientificName == "Acartia_Acartiura_tranteri","ScientificName"] <- "Acartia (Acartiura) tranteri"
m.zoo2[m.zoo2$ScientificName == "Acartia_Odontacartia_amboinensis","ScientificName"] <- "Acartia (Odontacartia) amboinensis"
m.zoo2[m.zoo2$ScientificName == "Acartia_Odontacartia_australis","ScientificName"] <- "Acartia (Odontacartia) australis"
m.zoo2[m.zoo2$ScientificName == "Acartia_Odontacartia_bispinosa","ScientificName"] <- "Acartia (Odontacartia) bispinosa"
m.zoo2[m.zoo2$ScientificName == "Acartia_Odontacartia_pacifica","ScientificName"] <- "Acartia (Odontacartia) pacifica"
m.zoo2[m.zoo2$ScientificName == "Acartia_Odontacartia_pacificaertoni","ScientificName"] <- "Acartia (Odontacartia) pacifica"
m.zoo2[m.zoo2$ScientificName == "Acrocalanusonachus","ScientificName"] <- "Acrocalanus monachus"
m.zoo2[m.zoo2$ScientificName == "Agetuslaccus","ScientificName"] <- "Agetus flaccus"
m.zoo2[m.zoo2$ScientificName == "Alpheid zoea","ScientificName"] <- "Alpheidae"
m.zoo2[m.zoo2$ScientificName == "Amphipod","ScientificName"] <- "Amphipoda"
m.zoo2[m.zoo2$ScientificName == "Anomuran zoea","ScientificName"] <- "Anomura"
m.zoo2[m.zoo2$ScientificName == "Asteroidea larvae","ScientificName"] <- "Asteroidea"
m.zoo2[m.zoo2$ScientificName == "Barnacle cyprid","ScientificName"] <- "Cirripedia"
m.zoo2[m.zoo2$ScientificName == "Barnacle nauplii","ScientificName"] <- "Cirripedia"
m.zoo2[m.zoo2$ScientificName == "Bivalve","ScientificName"] <- "Bivalvia"
m.zoo2[m.zoo2$ScientificName == "Bivalve larvae","ScientificName"] <- "Bivalvia"
m.zoo2[m.zoo2$ScientificName == "Bothidae larvae","ScientificName"] <- "Bothidae"
m.zoo2[m.zoo2$ScientificName == "Brachiopoda larvae","ScientificName"] <- "Brachiopoda"
m.zoo2[m.zoo2$ScientificName == "Brachyuranegalopa","ScientificName"] <- "Brachyura"
m.zoo2[m.zoo2$ScientificName == "Brachyuran zoea","ScientificName"] <- "Brachyura"
m.zoo2[m.zoo2$ScientificName == "Bryozoan_Cyphonaute larvae ","ScientificName"] <- "Bryozoa"
m.zoo2[m.zoo2$ScientificName == "Calanid","ScientificName"] <- "Calanidae"
m.zoo2[m.zoo2$ScientificName == "Calanoid copepods","ScientificName"] <- "Calanoida"
m.zoo2[m.zoo2$ScientificName == "Calanoid","ScientificName"] <- "Calanoida"
m.zoo2[m.zoo2$ScientificName == "Calanoides breivcornis","ScientificName"] <- "Calanoides brevicornis"
m.zoo2[m.zoo2$ScientificName == "Calanoidesacrocarinatus","ScientificName"] <- "Calanoides carinatus"
m.zoo2[m.zoo2$ScientificName == "Calanoides_CIV","ScientificName"] <- "Calanoides"
m.zoo2[m.zoo2$ScientificName == "Calanoides_CV","ScientificName"] <- "Calanoides"
m.zoo2[m.zoo2$ScientificName == "Calanopiainor","ScientificName"] <- "Calanopia minor"
m.zoo2[m.zoo2$ScientificName == "Callianassidae zoea","ScientificName"] <- "Callianassidae"
m.zoo2[m.zoo2$ScientificName == "Calycophoran","ScientificName"] <- "Calycophorae"
m.zoo2[m.zoo2$ScientificName == "Candaciaalcifera","ScientificName"] <- "Candacia falcifera"
m.zoo2[m.zoo2$ScientificName == "Candaciaaxima","ScientificName"] <- "Candacia maxima"
m.zoo2[m.zoo2$ScientificName == "Carideaegalopa larvae","ScientificName"] <- "Caridea"
m.zoo2[m.zoo2$ScientificName == "Caridea zoea","ScientificName"] <- "Caridea"
m.zoo2[m.zoo2$ScientificName == "Cavoliniidae body","ScientificName"] <- "Cavoliniidae"
m.zoo2[m.zoo2$ScientificName == "Cavoliniidae larvae","ScientificName"] <- "Cavoliniidae"
m.zoo2[m.zoo2$ScientificName == "Cavoliniidae shell","ScientificName"] <- "Cavoliniidae"
m.zoo2[m.zoo2$ScientificName == "Centropagesurcatus","ScientificName"] <- "Centropages furcatus"
m.zoo2[m.zoo2$ScientificName == "Cladoceran","ScientificName"] <- "Diplostraca"
m.zoo2[m.zoo2$ScientificName == "Clausocalanusarrani","ScientificName"] <- "Clausocalanus farrani"
m.zoo2[m.zoo2$ScientificName == "Clausocalanusarraniobei","ScientificName"] <- "Clausocalanus"
m.zoo2[m.zoo2$ScientificName == "Clausocalanusurcatus","ScientificName"] <- "Clausocalanus furcatus"
m.zoo2[m.zoo2$ScientificName == "Clausocalanusobei","ScientificName"] <- "Clausocalanus jobei"
m.zoo2[m.zoo2$ScientificName == "Clausocalanusastigophorus","ScientificName"] <- "Clausocalanus mastigophorus"
m.zoo2[m.zoo2$ScientificName == "Clausocalanusinor","ScientificName"] <- "Clausocalanus minor"
m.zoo2[m.zoo2$ScientificName == "Clytemnestraarrani","ScientificName"] <- "Clytemnestra farrani"
m.zoo2[m.zoo2$ScientificName == "Cnidarian larvae","ScientificName"] <- "Cnidaria"
m.zoo2[m.zoo2$ScientificName == "Codonellopsisorchella","ScientificName"] <- "Codonellopsis morchella"
m.zoo2[m.zoo2$ScientificName == "Colonial ascidian larvae","ScientificName"] <- "Ascidia"
m.zoo2[m.zoo2$ScientificName == "Copepod","ScientificName"] <- "Copepoda"
m.zoo2[m.zoo2$ScientificName == "Copiliairabilis","ScientificName"] <- "Copilia mirabilis"
m.zoo2[m.zoo2$ScientificName == "Ctenophore","ScientificName"] <- "Ctenophora"
m.zoo2[m.zoo2$ScientificName == "Cyttarocylis ampulla_cassis","ScientificName"] <- "Cyttarocylis"
m.zoo2[m.zoo2$ScientificName == "Decapod larvae","ScientificName"] <- "Decapoda"
m.zoo2[m.zoo2$ScientificName == "Dendrobranchiata larvae","ScientificName"] <- "Dendrobranchiata"
m.zoo2[m.zoo2$ScientificName == "Ditrichocorycaeusinimus","ScientificName"] <- "Ditrichocorycaeus minimus"
m.zoo2[m.zoo2$ScientificName == "Doliolum oozoid","ScientificName"] <- "Doliolum"
m.zoo2[m.zoo2$ScientificName == "Echinoderm larvae","ScientificName"] <- "Echinodermata"
m.zoo2[m.zoo2$ScientificName == "Echinopluteus","ScientificName"] <- "Echinodermata"
m.zoo2[m.zoo2$ScientificName == "Echinospira larva","ScientificName"] <- "Echinospira"
m.zoo2[m.zoo2$ScientificName == "Eggass","ScientificName"] <- "Egg mass"
m.zoo2[m.zoo2$ScientificName == "Ephyra larva","ScientificName"] <- "Cnidaria"
m.zoo2[m.zoo2$ScientificName == "Euchaetaarina gp","ScientificName"] <- "Euchaeta marina"
m.zoo2[m.zoo2$ScientificName == "Euchaetaarina","ScientificName"] <- "Euchaeta marina"
m.zoo2[m.zoo2$ScientificName == "Euchaetaarinella","ScientificName"] <- "Euchaeta marinella"
m.zoo2[m.zoo2$ScientificName == "Euchaetaedia","ScientificName"] <- "Euchaeta media"
m.zoo2[m.zoo2$ScientificName == "Euphausiarigida","ScientificName"] <- "Euphausia frigida"
m.zoo2[m.zoo2$ScientificName == "Euphausiarigida calyptopis","ScientificName"] <- "Euphausia frigida"
m.zoo2[m.zoo2$ScientificName == "Euphausiarigidaurcilia","ScientificName"] <- "Euphausia frigida"
m.zoo2[m.zoo2$ScientificName == "Euphausia longirostris calyptopis","ScientificName"] <- "Euphausia longirostris"
m.zoo2[m.zoo2$ScientificName == "Euphausia longirostrisurcilia","ScientificName"] <- "Euphausia longirostris"
m.zoo2[m.zoo2$ScientificName == "Euphausia similis var_armata","ScientificName"] <- "Euphausia similis"
m.zoo2[m.zoo2$ScientificName == "Euphausia spinifera calyptopis","ScientificName"] <- "Euphausia spinifera"
m.zoo2[m.zoo2$ScientificName == "Euphausia spiniferaurcilia","ScientificName"] <- "Euphausia spinifera"
m.zoo2[m.zoo2$ScientificName == "Euphausia triacantha calyptopis","ScientificName"] <- "Euphausia triacantha"
m.zoo2[m.zoo2$ScientificName == "Euphausia vallentini calyptopis","ScientificName"] <- "Euphausia vallentini"
m.zoo2[m.zoo2$ScientificName == "Euphausia vallentiniurcilia","ScientificName"] <- "Euphausia vallentini"
m.zoo2[m.zoo2$ScientificName == "Euphausid larvae","ScientificName"] <- "Euphausiidae"
m.zoo2[m.zoo2$ScientificName == "Euphausiid","ScientificName"] <- "Euphausiidae"
m.zoo2[m.zoo2$ScientificName == "Euphausiid calyptope","ScientificName"] <- "Euphausiidae"
m.zoo2[m.zoo2$ScientificName == "Euphausiid calyptope CI","ScientificName"] <- "Euphausiidae"
m.zoo2[m.zoo2$ScientificName == "Euphausiidurcilia","ScientificName"] <- "Euphausiidae"
m.zoo2[m.zoo2$ScientificName == "Euphausiidurcilia larvae","ScientificName"] <- "Euphausiidae"
m.zoo2[m.zoo2$ScientificName == "Euphausiidaeetanauplii","ScientificName"] <- "Euphausiidae"
m.zoo2[m.zoo2$ScientificName == "Facetotecta cyprid","ScientificName"] <- "Facetotecta"
m.zoo2[m.zoo2$ScientificName == "Facetotecta nauplii","ScientificName"] <- "Facetotecta"
m.zoo2[m.zoo2$ScientificName == "Foram A","ScientificName"] <- "Foraminifera"
m.zoo2[m.zoo2$ScientificName == "Foram C","ScientificName"] <- "Foraminifera"
m.zoo2[m.zoo2$ScientificName == "Gammarid amphipod","ScientificName"] <- "Gammarida"
m.zoo2[m.zoo2$ScientificName == "Gastropod","ScientificName"] <- "Gastropoda"
m.zoo2[m.zoo2$ScientificName == "Gastropod operculum","ScientificName"] <- "Gastropoda"
m.zoo2[m.zoo2$ScientificName == "Gastropod veliger","ScientificName"] <- "Gastropoda"
m.zoo2[m.zoo2$ScientificName == "Harpacticoid","ScientificName"] <- "Harpacticoida"
m.zoo2[m.zoo2$ScientificName == "Hydroid","ScientificName"] <- "Hydroidolina"
m.zoo2[m.zoo2$ScientificName == "Hydromedusae","ScientificName"] <- "Hydroidolina"
m.zoo2[m.zoo2$ScientificName == "Hyperiellaacronyx","ScientificName"] <- "Hyperiella macronyx"
m.zoo2[m.zoo2$ScientificName == "Hyperiid amphipod","ScientificName"] <- "Hyperiidae"
m.zoo2[m.zoo2$ScientificName == "Hyperiid","ScientificName"] <- "Hyperiidae"
m.zoo2[m.zoo2$ScientificName == "Isopod","ScientificName"] <- "Isopoda"
m.zoo2[m.zoo2$ScientificName == "Kuhnia_cyst","ScientificName"] <- "Kuhnia"
m.zoo2[m.zoo2$ScientificName == "Labidoceraarrani","ScientificName"] <- "Labidocera farrani"
m.zoo2[m.zoo2$ScientificName == "Labidocerainuta","ScientificName"] <- "Labidocera minuta"
m.zoo2[m.zoo2$ScientificName == "Labidoceraoretoni","ScientificName"] <- "Labidocera moretoni"
m.zoo2[m.zoo2$ScientificName == "Lucicutialavicornis","ScientificName"] <- "Lucicutia flavicornis"
m.zoo2[m.zoo2$ScientificName == "Luciferysis","ScientificName"] <- "Lucifer"
m.zoo2[m.zoo2$ScientificName == "Lucifer_protozoea","ScientificName"] <- "Lucifer"
m.zoo2[m.zoo2$ScientificName == "Megalope","ScientificName"] <- "Brachyura"
m.zoo2[m.zoo2$ScientificName == "Mesocalanus_CV","ScientificName"] <- "Mesocalanus"
m.zoo2[m.zoo2$ScientificName == "Mollusc","ScientificName"] <- "Mollusca"
m.zoo2[m.zoo2$ScientificName == "Mollusc veliger","ScientificName"] <- "Mollusca"
m.zoo2[m.zoo2$ScientificName == "Monacanthidae_leatherjacket_larvae","ScientificName"] <- "Monacanthidae"
m.zoo2[m.zoo2$ScientificName == "Myctophid","ScientificName"] <- "Myctophidae"
m.zoo2[m.zoo2$ScientificName == "Mysid","ScientificName"] <- "Mysidae"
m.zoo2[m.zoo2$ScientificName == "Nannocalanusinor","ScientificName"] <- "Nannocalanus minor"
m.zoo2[m.zoo2$ScientificName == "Nauplii calanoid","ScientificName"] <- "Calanoida"
m.zoo2[m.zoo2$ScientificName == "Nauplii calanoid_Rhincalanus_","ScientificName"] <- "Rhincalanus"
m.zoo2[m.zoo2$ScientificName == "Nauplii copepod","ScientificName"] <- "Copepoda"
m.zoo2[m.zoo2$ScientificName == "Nauplii crustracean","ScientificName"] <- "Crustacea"
m.zoo2[m.zoo2$ScientificName == "Nauplii euphausiid","ScientificName"] <- "Euphausiidae"
m.zoo2[m.zoo2$ScientificName == "Nauplii harpacticoid","ScientificName"] <- "Harpacticoida"
m.zoo2[m.zoo2$ScientificName == "Nauplii poecilostomatoida_cyclopoid","ScientificName"] <- "Ergasilida"
m.zoo2[m.zoo2$ScientificName == "Nauplii Rhincalanus gigas","ScientificName"] <- "Rhincalanus gigas"
m.zoo2[m.zoo2$ScientificName == "Nauplii zooplankton","ScientificName"] <- "Crustacea"
m.zoo2[m.zoo2$ScientificName == "Nematoscelisegalops","ScientificName"] <- "Nematoscelis"
m.zoo2[m.zoo2$ScientificName == "Neocalanus_CIV","ScientificName"] <- "Neocalanus"
m.zoo2[m.zoo2$ScientificName == "Neocalanus_CV","ScientificName"] <- "Neocalanus"
m.zoo2[m.zoo2$ScientificName == "Nyctiphanes australis calyptopis","ScientificName"] <- "Nyctiphanes australis"
m.zoo2[m.zoo2$ScientificName == "Nyctiphanes australisurcilia","ScientificName"] <- "Nyctiphanes australis"
m.zoo2[m.zoo2$ScientificName == "Oithona attenuata typical","ScientificName"] <- "Oithona attenuata"
m.zoo2[m.zoo2$ScientificName == "Oithona decipiens similis","ScientificName"] <- "Oithona"
m.zoo2[m.zoo2$ScientificName == "Oithona Grp1lat head","ScientificName"] <- "Oithona"
m.zoo2[m.zoo2$ScientificName == "Oithona Grp3_pointy head","ScientificName"] <- "Oithona"
m.zoo2[m.zoo2$ScientificName == "Oithona Grp5_orangeurcae","ScientificName"] <- "Oithona"
m.zoo2[m.zoo2$ScientificName == "Oithona_large","ScientificName"] <- "Oithona"
m.zoo2[m.zoo2$ScientificName == "Oithonam","ScientificName"] <- "Oithona"
m.zoo2[m.zoo2$ScientificName == "Oncaea atlantica complex","ScientificName"] <- "Oncaea atlantica"
m.zoo2[m.zoo2$ScientificName == "Oncaea clevei complex","ScientificName"] <- "Oncaea clevei"
m.zoo2[m.zoo2$ScientificName == "Oncaeaedia complex","ScientificName"] <- "Oncaea media"
m.zoo2[m.zoo2$ScientificName == "Oncaeaedia","ScientificName"] <- "Oncaea media"
m.zoo2[m.zoo2$ScientificName == "Oncaeaediterranea broad","ScientificName"] <- "Oncaea mediterranea mediterranea"
m.zoo2[m.zoo2$ScientificName == "Oncaeaediterranea complex","ScientificName"] <- "Oncaea mediterranea mediterranea"
m.zoo2[m.zoo2$ScientificName == "Oncaeaediterranea slender","ScientificName"] <- "Oncaea mediterranea mediterranea"
m.zoo2[m.zoo2$ScientificName == "Oncaea venusta complex","ScientificName"] <- "Oncaea venusta"
m.zoo2[m.zoo2$ScientificName == "Oncaea venustaedium","ScientificName"] <- "Oncaea venusta"
m.zoo2[m.zoo2$ScientificName == "Oncaea zernovi complex","ScientificName"] <- "Oncaea zernovi"
m.zoo2[m.zoo2$ScientificName == "Ophiopluteus","ScientificName"] <- "Ophiura"
m.zoo2[m.zoo2$ScientificName == "Ostracod","ScientificName"] <- "Ostracoda"
m.zoo2[m.zoo2$ScientificName == "Paracalanus indicus i","ScientificName"] <- "Paracalanus indicus"
m.zoo2[m.zoo2$ScientificName == "Parvocalanus cf_serratipes","ScientificName"] <- "Parvocalanus serratipes"
m.zoo2[m.zoo2$ScientificName == "Pentacrinoid larvae","ScientificName"] <- "Crinoidea"
m.zoo2[m.zoo2$ScientificName == "Phyllosoma larvae","ScientificName"] <- "Achelata"
m.zoo2[m.zoo2$ScientificName == "Pluteus larva","ScientificName"] <- "Echinidea"
m.zoo2[m.zoo2$ScientificName == "Poecilostomatoida_Cyclopoid","ScientificName"] <- "Ergasilida"
m.zoo2[m.zoo2$ScientificName == "Polychaete","ScientificName"] <- "Polychaeta"
m.zoo2[m.zoo2$ScientificName == "Polychaete larvae","ScientificName"] <- "Polychaeta"
m.zoo2[m.zoo2$ScientificName == "Pontellinaorii","ScientificName"] <- "Pontellina morii"
m.zoo2[m.zoo2$ScientificName == "Porcelain crab zoea","ScientificName"] <- "Brachyura"
m.zoo2[m.zoo2$ScientificName == "Primnoacropa","ScientificName"] <- "Primno macropa"
m.zoo2[m.zoo2$ScientificName == "Prosobranch","ScientificName"] <- "Prosobranchia"
m.zoo2[m.zoo2$ScientificName == "Protozoea decapod larvae","ScientificName"] <- "Decapoda"
m.zoo2[m.zoo2$ScientificName == "Pseudodiaptomusertoni","ScientificName"] <- "Pseudodiaptomus mertoni"
m.zoo2[m.zoo2$ScientificName == "Pteropod","ScientificName"] <- "Pteropoda"
m.zoo2[m.zoo2$ScientificName == "Pterotracheid","ScientificName"] <- "Pterotracheidae"
m.zoo2[m.zoo2$ScientificName == "Pterotracheoidea_atlantid","ScientificName"] <- "Atlantidae"
m.zoo2[m.zoo2$ScientificName == "Pyrosome","ScientificName"] <- "Pyrosoma"
m.zoo2[m.zoo2$ScientificName == "Radiolarian A","ScientificName"] <- "Radiozoa"
m.zoo2[m.zoo2$ScientificName == "Radiolarian B","ScientificName"] <- "Radiozoa"
m.zoo2[m.zoo2$ScientificName == "Radiolarian D","ScientificName"] <- "Radiozoa"
m.zoo2[m.zoo2$ScientificName == "Radiolarian G","ScientificName"] <- "Radiozoa"
m.zoo2[m.zoo2$ScientificName == "Radiolarian J","ScientificName"] <- "Radiozoa"
m.zoo2[m.zoo2$ScientificName == "Radiolarian L","ScientificName"] <- "Radiozoa"
m.zoo2[m.zoo2$ScientificName == "Sapphirina cf_nigromaculata","ScientificName"] <- "Sapphirina nigromaculata"
m.zoo2[m.zoo2$ScientificName == "Sapphirinaetallina","ScientificName"] <- "Sapphirina metallina"
m.zoo2[m.zoo2$ScientificName == "Scolecithricellainor","ScientificName"] <- "Scolecithricella minor"
m.zoo2[m.zoo2$ScientificName == "Sergestid protozoea","ScientificName"] <- "Sergestidae"
m.zoo2[m.zoo2$ScientificName == "Sergestid zoea","ScientificName"] <- "Sergestidae"
m.zoo2[m.zoo2$ScientificName == "Siphonophore","ScientificName"] <- "Siphonophorae"
m.zoo2[m.zoo2$ScientificName == "Siphonophore_nectophore","ScientificName"] <- "Siphonophorae"
m.zoo2[m.zoo2$ScientificName == "Siphonophore abylidae","ScientificName"] <- "Abylidae"
m.zoo2[m.zoo2$ScientificName == "Siphonophore diphyidae","ScientificName"] <- "Diphyidae"
m.zoo2[m.zoo2$ScientificName == "Sipunculid worm larvae","ScientificName"] <- "Sipunculidea"
m.zoo2[m.zoo2$ScientificName == "Spongodiscidae_Trigonal ","ScientificName"] <- "Spongodiscidae"
m.zoo2[m.zoo2$ScientificName == "Stomatopod","ScientificName"] <- "Stomatopoda"
m.zoo2[m.zoo2$ScientificName == "Subeucalanusonachus","ScientificName"] <- "Subeucalanus monachus"
m.zoo2[m.zoo2$ScientificName == "Subeucalanusucronatus","ScientificName"] <- "Subeucalanus mucronatus"
m.zoo2[m.zoo2$ScientificName == "Subeucalanus subcrassus i","ScientificName"] <- "Subeucalanus subcrassus"
m.zoo2[m.zoo2$ScientificName == "Syngnathid","ScientificName"] <- "Syngnathidae"
m.zoo2[m.zoo2$ScientificName == "Temoropiaayumbaensis","ScientificName"] <- "Temoropia mayumbaensis"
m.zoo2[m.zoo2$ScientificName == "Thaliacean","ScientificName"] <- "Thaliacea"
m.zoo2[m.zoo2$ScientificName == "Thysanoessa gregariaurcilia","ScientificName"] <- "Thysanoessa gregaria"
m.zoo2[m.zoo2$ScientificName == "Thysanoessaacrura","ScientificName"] <- "Thysanoessa macrura"
m.zoo2[m.zoo2$ScientificName == "Thysanoessaacruraurcilia","ScientificName"] <- "Thysanoessa macrura"
m.zoo2[m.zoo2$ScientificName == "Thysanoessa vicinaacrura","ScientificName"] <- "Thysanoessa"
m.zoo2[m.zoo2$ScientificName == "Thysanoessa vicinaacrura calyptopis","ScientificName"] <- "Thysanoessa"
m.zoo2[m.zoo2$ScientificName == "Thysanoessa vicinaacruraurcilia","ScientificName"] <- "Thysanoessa vicina"
m.zoo2[m.zoo2$ScientificName == "Tortanus_Tortanus_barbatus","ScientificName"] <- "Tortanus (Tortanus) barbatus"
m.zoo2[m.zoo2$ScientificName == "Tortanus_Tortanus_gracilis","ScientificName"] <- "Tortanus (Tortanus) gracilis"
m.zoo2[m.zoo2$ScientificName == "Triconia conifera complex","ScientificName"] <- "Triconia conifera"
m.zoo2[m.zoo2$ScientificName == "Trochophore larvae","ScientificName"] <- ""
m.zoo2[m.zoo2$ScientificName == "Urochordate","ScientificName"] <- "Tunicata"
m.zoo2[m.zoo2$ScientificName == "Urocorycaeusurcifer","ScientificName"] <- "Urocorycaeus furcifer"
m.zoo2[m.zoo2$ScientificName == "Medusa","ScientificName"] <- "Cnidaria"
m.zoo2[m.zoo2$ScientificName == "Opisthobranchia","ScientificName"] <- "Heterobranchia"

### Final check
#unique(m.zoo2$ScientificName)
#unique(m.zoo2[m.zoo2$ScientificName == "","OrigScientificName"])
m.zoo2[m.zoo2$ScientificName == "","ScientificName"] <- "Trochophore larvae"

# Check again: 
keys.worms <- wormsbynames( unique(m.zoo2$ScientificName) )
keys.worms$ScientificName <- unique(m.zoo2$ScientificName)

# Add WoRMS_status field
m.zoo2 <-  add_column(m.zoo2, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(m.zoo2)

# For testing the functions below:
s <- unique(m.zoo2$ScientificName)[3] ; s

require("parallel")
res <- mclapply( unique(m.zoo2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- m.zoo2[m.zoo2$ScientificName == s,]
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

        }, mc.cores = 15 # >2 when on kryo

) # eo mclapply - s in taxa_names

### Rbind
ddf <- bind_rows(res)
dim(ddf) # 7'373'520, same as before. Check some random lines.
ddf[1:50,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
ddf[is.na(ddf$Species),][1:50,]
unique(ddf$WoRMS_ID) 
# What do the 'To_add_at_the_end' correspond to?
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS","ScientificName"])
# Makes sense
unique(ddf$WoRMS_status) # good

### Finally, use OrigScientificName to provide lifestages info. All NA in 'LifeForm' will be assumed mature adults specimen. One can also use the mesh size of the plankton net used for sampling (if info is given) to derive a size class. For instance, a WP2 net should only capture organisms > 200µm

# To do so, identify those OrigScientificName levels that contain LifeForm info (sex, life stage, qualitative indicator of anything). Identify those indicators and report them in the LifeForm column based on grep()
unique(ddf$OrigScientificName)

### A potentially interesting strategy would be to identify the last 3 or 5 characters for each of OrigScientificName labels andbased on that use a if else loop to inform LifeForm? 
substrRight <- function(x,n) { substr(x, nchar(x)-n+1, nchar(x)) }
# x <- "hello there" ; substrRight(x, n = 3)
unique(substrRight(as.character(ddf$OrigScientificName), n = 7)) # Choose n = 7
# OK, looks like this works
# o <- unique(ddf$OrigScientificName)[10] # For tetsing for loop belo
for(o in unique(ddf$OrigScientificName)) {
    
    message(paste(o, sep = ""))
    chara <- substrRight(as.character(o), n = 7)
    
    if( grepl(".m", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Male"
    } else if( grepl(".f", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Female"
    } else if( grepl(".j", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Juvenile"
    } else if( grepl("_spp_f", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Female"
    } else if( grepl("spp_f", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Female"
    } else if( grepl("spp_j", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Juvenile"
    } else if( grepl("spp_m", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Male"
    } else if( grepl(".zoea", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Zoea larva"
    } else if( grepl(".larvae", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Larva"
    } else if( grepl(".cyprid", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Cyprid larva"
    } else if( grepl("nauplii", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Nauplius larva"
    } else if( grepl("larvae.", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Larva"
    } else if( grepl("egalopa", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Megalopa larva"
    } else if( grepl("egalope", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Megalopa larva"
    } else if( grepl("CIV", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "CIV stage"
    } else if( grepl("CV", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "CV stage"
    } else if( grepl("_small.", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Small"
    } else if( grepl(".body", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Body part"
    } else if( grepl(".shell", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Shell"
    } else if( grepl(".gravid", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Gravid"
    } else if( grepl("1.00mm.", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "1.00m"
    } else if( grepl(".1.2mm.", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "1.0x1.2mm"
    } else if( grepl("_1.2mm.", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "1.2mm"
    } else if( grepl(".larva", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Larva"
    } else if( grepl(".eggs", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Eggs"
    } else if( grepl("yptopis", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Calyptopis larva"
    } else if( grepl(".adult", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Adult"
    } else if( grepl("lyptope", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Calyptopis larva"
    } else if( grepl("tope.CI", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Calyptopis CI"
    } else if( grepl("ope.CII", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Calyptopis CII"
    } else if( grepl("pe.CIII", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Calyptopis CIII"
    } else if( grepl("ilia.FI", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia FI"
    } else if( grepl("lia.FII", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia FII"
    } else if( grepl("ia.FIII", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia FIII"
    } else if( grepl("lia.FIV", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia FIV"
    } else if( grepl("ilia.FV", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia FV"
    } else if( grepl("lia.FVI", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia FVI"
    } else if( grepl("erculum", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Operculum"
    } else if( grepl("veliger", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Veliger larva"
    } else if( grepl("_cyst", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Cysts"
    } else if( grepl("_larvae", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Larva"
    } else if( grepl("_mysis", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Mysis larva"
    } else if( grepl("otozoea", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Protozoea larva"
    } else if( grepl("egalops", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Megalopa larva"
    } else if( grepl("alops.f", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Megalopa larva"
    } else if( grepl("pical.f", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Typical female"
    } else if( grepl(".head_f", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Flat or point head_female"
    } else if( grepl("urcae_f", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Orange furcae female"
    } else if( grepl("large.m", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Large_male"
    } else if( grepl("small.m", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Small_male"
    } else if( grepl("m_0.4mm", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "0.4mm_male"
    } else if( grepl("m_0.7mm", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "0.7mm_male"
    } else if( grepl("4_0.7mm", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "0.4+0.7mm_male"
    } else if( grepl("n.spp_f", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Female"
    } else if( grepl("oni.agg", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Aggregates"
    } else if( grepl("oni.sol", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Solitary"
    } else if( grepl("dae.agg", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Aggregates"
    } else if( grepl("dae.sol", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Solitary"
    } else if( grepl("ica.agg", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Aggregates"
    } else if( grepl("ica.sol", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Solitary"
    } else if( grepl("a.adult", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Adult"
    } else if( grepl("ura.CII", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "CII stage"
    } else if( grepl("ra.CIII", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "CIII stage"
    } else if( grepl("rura.FI", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia FI"
    } else if( grepl("ra.FIII", x = chara, fixed = T) ) {
        ddf[ddf$OrigScientificName == o,"LifeForm"] <- "Furcilia FIII"
    } # eof if else loops 
    
} # eo for loop

### Check: 
unique(ddf$LifeForm)

# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[70000:70050,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[35020:35080,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[373520:373580,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[623000:623050,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
# Looks ganz OK :-)

### Last, make a map of sampling effort in space and then maybe a Hövmoller plot
d.effort <- ddf
d.effort$x_1d <- round(d.effort$decimalLongitude)
d.effort$y_1d <- round(d.effort$decimalLatitude)
d.effort$cell_id <- factor(paste(d.effort$x_1d, d.effort$y_1d, sep = "_"))
require("dplyr")#; detach("package:plyr", unload = TRUE)
### Issue related to the loading of the 'worms' R pakage! detach worms and then plyr
detach("package:worms", unload = TRUE)
detach("package:marmap", unload = TRUE)
detach("package:reshape2", unload = TRUE)
spatial.effort <- data.frame(d.effort %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
dim(spatial.effort) ; summary(spatial.effort)

# Map sampling effort
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 177 & world$long >= 80 & world$lat <= -10 & world$lat >= -67,],
        fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### And for only those calcifying pteropod species
unique(d.effort$Order)
dim(na.omit(d.effort[d.effort$Order == "Pteropoda",c("Month","Year")])) # 70'560 obs
summary(na.omit(d.effort[d.effort$Order == "Pteropoda",c("Month","Year")]))

spatial.effort <- na.omit(data.frame(d.effort[d.effort$Order == "Pteropoda",] %>% 
                    group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n())) )
# 579 grid cells

ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 177 & world$long >= 80 & world$lat <= -10 & world$lat >= -67,],
        fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = spatial.effort) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "AusCPR_reformatted+WoRMScheck_13_10_21.Rdata")
write.table(ddf, file = "AusCPR_reformatted+WoRMScheck_13_10_21.txt", sep = "\t")

### ------------------------------------------------------------------------------------------------------------------------------------------------------

### 26/10/21: 2°) Phytoplankton 
### File is called 'v2' because I had to manually replace the 'µ' by a 'm' in Excel so R could read the headers (R struggles to read special characters in colnames)

# Make sure to read this again: https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/catalog.search#/metadata/c1344979-f701-0916-e044-00144f7bc0f4 

phy <- read.csv("AusCPR_Phyto_RawMatv2.csv", h = T, sep = ",", dec = ".")
dim(phy) # 8399 observations of 460 taxa (8 metadata columns)
colnames(phy)
str(phy[,1:10])
summary(phy)
head(phy)
# Values in counts represent #/m3
### IMPORTANT: The data are made available in binned products for ease of use. As taxonomy changes over time and the ability of analysts improves with training more and more species are able to be identified over time. The products account for these changes and for real absences from the samples. A 0 abundance means that the species was looked for and not seen, -999 means that the species was not looked for in that sample.

unique(phy$Sample)

# Coordinates, dates etc. look quite OK.
# Just convert SampleDateUTC to vector of class vector
#str(phy$SampleDateUTC) # charact
#str(as.Date(phy$SampleDateUTC)) # as Date
phy$SampleDateUTC <- as.Date(phy$SampleDateUTC)

### Before melting and reformatting to AtlantECO data, modify the taxa colnames to avoid the extra dots etc.
colnames(phy)
# Let's clean those labels later (durig the WoRMS ID check)

### Melt
m.phy <- melt(phy, id.vars = c("Sample","Latitude","Longitude","SampleDateUTC","Year","Month","Day","Time_24hr"))
head(m.phy) ; dim(m.phy) # 3'863'540      10
summary(m.phy) # Check range of concentrations values
unique(m.phy$value)[order(unique(m.phy$value))]

### Format to WP2 standards
colnames(m.phy)

m.phy2 <- data.frame(ProjectID = "AtlantECO_H2020_GA#862923", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Claire_Davies",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;imos-plankton@csiro.au", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "Not_applicable", obisID = "Not_applicable", DatasetKey = "Not_applicable",
                decimalLatitude = m.phy$Latitude, decimalLongitude = m.phy$Longitude, geodeticDatum = "WGS84", 
                CoordUncertainty = "Comparison between the calculated position and data from vessels where a GPS record was available suggests the position assigned to CPR samples is accurate to within 10–20 nautical miles.",
                CountryCode = NA, eventDate = m.phy$SampleDateUTC,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = m.phy$Year, Month = m.phy$Month, Day = m.phy$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Water_column", LonghurstProvince = NA,
                Depth = 7, DepthAccuracy = "2-3m", DepthIntegral = NA,
                MinDepth = 5, MaxDepth = 10, ParentEventID = "AusCPR", EventID = m.phy$Sample, InstitutionCode = "Australia’s Integrated Marine Observing System (IMOS)",
                SourceArchive = "https://github.com/PlanktonTeam/IMOS_Toolbox/blob/master/Plankton/Output/CPR_Phyto_RawMat.csv",
                OrigCollectionCode = "c1344979-f701-0916-e044-00144f7bc0f4", OrigCollectionID = "IMOS (http://imos.aodn.org.au/webportal/)",
                BiblioCitation = "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)",
                CitationDOI = "https://catalogue-imos.aodn.org.au/geonetwork/srv/eng/catalog.search#/metadata/c1344e70-480e-0993-e044-00144f7bc0f4",
                DateDataAccess = "12-10-2021",
                OrigScientificName = m.phy$variable, ScientificName = m.phy$variable,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = 'To_add_at_the_end', 
                Kingdom = "Animalia", Phylum = NA, Class = NA, Order = NA,
                Family = NA, Genus = NA, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Organisms concentration", MeasurementTypeID = "To_define", MeasurementValue = m.phy$value,
                MeasurementUnit = "#/m3", MeasurementAcurracy = "A 0 abundance = species was looked for and not seen, -999 = species was not looked for in that sample.",
                MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = "Australian CPR survey - Methods described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011)",
                SamplingProtocol = "The plankton analysis is conducted differently to that described in Richardson et al. 2006 (https://doi.org/10.1016/j.pocean.2005.09.011) as it is counted off the 270µm silk in a Bogorov tray. This is accomplished by rinsing the silks in water and straining through a 10 micron mesh sieve. The collected plankton is transferred to a bogorov tray and counted under a dissecting scope. This is done to retain the phytoplankton. After counting the zooplankton and phytoplankton are stored with the silk segment in PGP preservative.",
                SampleAmount = "The volume of water filtered for each 10 nautical mile sample is about 3m3 (mean = 3.27 m3, SD = 0.71 m3, n = 1723, Jonas et al., 2004). But AusCPR does 5 nautical miles instead of 10",
                SampleAmountUnit = NA, SampleEffort = NA,
                DeterminedBy = "Servicing and maintenance of the CPRs and analysis of the samples for the AusCPR survey will be carried out by staff based at the O&A CSIRO laboratories in Queensland and Hobart and at the AAD in Hobart.",
                DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
head(m.phy2)
dim(m.phy2) # 3'863'540
str(m.phy2)

# Check scientific names used
str(m.phy2$ScientificName) # factor

# First, convert to char (better for adjusting levels)
m.phy2$ScientificName <- as.character(m.phy2$ScientificName)
unique(m.phy2$ScientificName)

### Start adjusting this manually...
m.phy2[m.phy2$ScientificName == "Acanthostomella_cf._conicoides","ScientificName"] <- "Acanthostomella conicoides"
m.phy2[m.phy2$ScientificName == "Amphidinium_spp._.30_mm_cell_height","ScientificName"] <- "Amphidinium"
m.phy2[m.phy2$ScientificName == "Centric_diatom","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Centric_diatom_._10_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Centric_diatom_10.20_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Centric_diatom_20.30_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Centric_diatom_30.40_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Centric_diatom_40.50_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Chaetoceros_.Hyalochaete.","ScientificName"] <- "Chaetoceros (Hyalochaete)"
m.phy2[m.phy2$ScientificName == "Chaetoceros_.Phaeoceros.","ScientificName"] <- "Chaetoceros (Phaeoceros)"
m.phy2[m.phy2$ScientificName == "Chaetoceros_cf._affinis","ScientificName"] <- "Chaetoceros affinis"
m.phy2[m.phy2$ScientificName == "Chaetoceros_decipiens_._15_mm_cell_width","ScientificName"] <- "Chaetoceros decipiens"
m.phy2[m.phy2$ScientificName == "Chaetoceros_peruvianus_._40_mm_cell_width","ScientificName"] <- "Chaetoceros peruvianus"
m.phy2[m.phy2$ScientificName == "Chaetoceros_peruvianus_._40_mm_cell_width.1","ScientificName"] <- "Chaetoceros peruvianus"
m.phy2[m.phy2$ScientificName == "Chaetoceros_spp._._10_mm_cell_width","ScientificName"] <- "Chaetoceros"
m.phy2[m.phy2$ScientificName == "Ciliate_.Inc._tintinnid.","ScientificName"] <- "Ciliates including Tintinnids"
m.phy2[m.phy2$ScientificName == "Ciliate_30.40_mm","ScientificName"] <- "Ciliates 30-40mm"
m.phy2[m.phy2$ScientificName == "Coccolithophore_cf.","ScientificName"] <- "Coccolithophore"
m.phy2[m.phy2$ScientificName == "Coscinodiscus_spp._._80_mm","ScientificName"] <- "Coscinodiscus"
m.phy2[m.phy2$ScientificName == "Coscinodiscus_spp._100.150_mm","ScientificName"] <- "Coscinodiscus"
m.phy2[m.phy2$ScientificName == "Coscinodiscus_spp._150.200_mm","ScientificName"] <- "Coscinodiscus"
m.phy2[m.phy2$ScientificName == "Coscinodiscus_spp._200.300_mm","ScientificName"] <- "Coscinodiscus"
m.phy2[m.phy2$ScientificName == "Coscinodiscus_spp._300.400_mm","ScientificName"] <- "Coscinodiscus"
m.phy2[m.phy2$ScientificName == "Coscinodiscus_spp._400.500_mm","ScientificName"] <- "Coscinodiscus"
m.phy2[m.phy2$ScientificName == "Coscinodiscus_spp._60.80_mm","ScientificName"] <- "Coscinodiscus"
m.phy2[m.phy2$ScientificName == "Cyanobacteria_coccoid_chain","ScientificName"] <- "Cyanobacteria"
m.phy2[m.phy2$ScientificName == "Dactyliosolen_cf._spp.","ScientificName"] <- "Dactyliosolen"
m.phy2[m.phy2$ScientificName == "Dictyocysta_elegans_var._lepida","ScientificName"] <- "Dictyocysta elegans"
m.phy2[m.phy2$ScientificName == "Dinoflagellate_cyst","ScientificName"] <- "Dinoflagellata"
m.phy2[m.phy2$ScientificName == "Ditylum_brightwellii_._40_mm_width","ScientificName"] <- "Ditylum brightwellii"
m.phy2[m.phy2$ScientificName == "Eucampia_spp._.100_mm_cell_length","ScientificName"] <- "Eucampia"
m.phy2[m.phy2$ScientificName == "Eutintinnus_lusus.undae","ScientificName"] <- "Eutintinnus lusus-undae"
m.phy2[m.phy2$ScientificName == "Filamentous_algae_.branching.","ScientificName"] <- "Filamentous algae (branching)"
m.phy2[m.phy2$ScientificName == "Filamentous_algae_.non_branching.","ScientificName"] <- "Filamentous algae (non branching)"
m.phy2[m.phy2$ScientificName == "Flagellate","ScientificName"] <- "Protozoa"
m.phy2[m.phy2$ScientificName == "Flagellate_.10_mm_fusiform","ScientificName"] <- "Protozoa"
m.phy2[m.phy2$ScientificName == "Fragilaria_._Fragilariopsis","ScientificName"] <- "Fragilariopsis"
m.phy2[m.phy2$ScientificName == "Fragilariopsis_separanda.rhombica","ScientificName"] <- "Diatoma rhombica"
m.phy2[m.phy2$ScientificName == "Guinardia_cf._tubiformis","ScientificName"] <- "Rhizosolenia tubiformis"
m.phy2[m.phy2$ScientificName == "Gymnodinioid_dinoflagellate_10.20_mm","ScientificName"] <- "Gymnodiniaceae"
m.phy2[m.phy2$ScientificName == "Gyrodinium_spp._20.40_mm","ScientificName"] <- "Gyrodinium"
m.phy2[m.phy2$ScientificName == "Leptocylindrus_cf._spp.","ScientificName"] <- "Leptocylindrus"
m.phy2[m.phy2$ScientificName == "Leptocylindrus_mediterraneus_.no_flagellates.","ScientificName"] <- "Leptocylindrus mediterraneus"
m.phy2[m.phy2$ScientificName == "Leptocylindrus_mediterraneus_.with_flagellates.","ScientificName"] <- "Leptocylindrus mediterraneus"
m.phy2[m.phy2$ScientificName == "Lioloma_cf._spp.","ScientificName"] <- "Lioloma"
m.phy2[m.phy2$ScientificName == "Melosira_cf._spp.","ScientificName"] <- "Melosira"
m.phy2[m.phy2$ScientificName == "Mesodinium_cf._spp.","ScientificName"] <- "Mesodinium"
m.phy2[m.phy2$ScientificName == "Microzooplankton_unid","ScientificName"] <- "Microzooplankton (unidentified)"
m.phy2[m.phy2$ScientificName == "Navicula_._shaped","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._.20_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._100.120_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._120.140_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._140.160_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._20.40_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._200.250_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._300.350_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._40.60_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._60.80_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Navicula_cf._80.100_mm_length","ScientificName"] <- "Navicula"
m.phy2[m.phy2$ScientificName == "Nitzschia_cf._bicapitata","ScientificName"] <- "Nitzschia bicapitata"
m.phy2[m.phy2$ScientificName == "Pennate_diatom","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Pennate_diatom_._10_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Pennate_diatom_._10_mm.1","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Pennate_diatom_._100_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Pennate_diatom_10_._25_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Pennate_diatom_25_._50_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Pennate_diatom_50_._75_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Pennate_diatom_75_._100_mm","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Peridinium_cf._spp.","ScientificName"] <- "Peridinium"
m.phy2[m.phy2$ScientificName == "Phaeocystis_cf._spp._.colony_.50_um.","ScientificName"] <- "Phaeocystis"
m.phy2[m.phy2$ScientificName == "Phaeocystis_cf._spp._.colony_.50_um..1","ScientificName"] <- "Phaeocystis"
m.phy2[m.phy2$ScientificName == "Phaeocystis_spp._.colony.","ScientificName"] <- "Phaeocystis"
m.phy2[m.phy2$ScientificName == "Pleurosigma_._Gyrosigma_spp.","ScientificName"] <- "Pleurosigmataceae"
m.phy2[m.phy2$ScientificName == "Pleurosigma_spp._._150_mm","ScientificName"] <- "Pleurosigma"
m.phy2[m.phy2$ScientificName == "Pinus_pollen","ScientificName"] <- "Pollen"
m.phy2[m.phy2$ScientificName == "Prorocentrum_cf_rostratum","ScientificName"] <- "Prorocentrum rostratum"
m.phy2[m.phy2$ScientificName == "Protoperidinium_oceanicum.oblongum","ScientificName"] <- "Protoperidinium oceanicum"
m.phy2[m.phy2$ScientificName == "Psammodictyon_cf._spp.","ScientificName"] <- "Psammodictyon"
m.phy2[m.phy2$ScientificName == "Pseudo.nitzschia_cf._heimii","ScientificName"] <- "Pseudo-nitzschia heimii"
m.phy2[m.phy2$ScientificName == "Pseudo.nitzschia_delicatissima_complex_..3_mm","ScientificName"] <- "Pseudo-nitzschia delicatissima"
m.phy2[m.phy2$ScientificName == "Pseudo.nitzschia_seriata_complex_.3_mm","ScientificName"] <- "Pseudo-nitzschia seriata"
m.phy2[m.phy2$ScientificName == "Pseudo.nitzschia_spp.","ScientificName"] <- "Pseudo-nitzschia"
m.phy2[m.phy2$ScientificName == "Pseudosolenia_calcar.avis","ScientificName"] <- "Pseudosolenia calcar-avis"
m.phy2[m.phy2$ScientificName == "Pyrophacus_vancampoae_cyst","ScientificName"] <- "Pyrophacus vancampoae"
m.phy2[m.phy2$ScientificName == "Radiolarian","ScientificName"] <- "Radiozoa"
m.phy2[m.phy2$ScientificName == "Rhizosolenia_antennata_f._semispina","ScientificName"] <- "Rhizosolenia semispina"
m.phy2[m.phy2$ScientificName == "Rhizosolenia_hebetata_f._semispina","ScientificName"] <- "Rhizosolenia hebetata"
m.phy2[m.phy2$ScientificName == "Rhizosolenia_imbricata_.with_Richelia.","ScientificName"] <- "Rhizosolenia imbricata"
m.phy2[m.phy2$ScientificName == "Rhizosolenia_imbricata_group","ScientificName"] <- "Rhizosolenia imbricata"
m.phy2[m.phy2$ScientificName == "Rhizosolenia_polydactyla_f._polydactyla","ScientificName"] <- "Rhizosolenia styliformis f. polydactyla"
m.phy2[m.phy2$ScientificName == "Rhizosolenia_setigera_group","ScientificName"] <- "Rhizosolenia setigera"
m.phy2[m.phy2$ScientificName == "Rhizosolenia_sima_f._sima","ScientificName"] <- "Rhizosolenia sima f. sima"
m.phy2[m.phy2$ScientificName == "Silicoflagellate","ScientificName"] <- "Dictyochales"
m.phy2[m.phy2$ScientificName == "Silicoflagellate_4_points","ScientificName"] <- "Dictyochales"
m.phy2[m.phy2$ScientificName == "Silicoflagellate_6_points","ScientificName"] <- "Dictyochales"
m.phy2[m.phy2$ScientificName == "Steenstrupiella_cf._steenstrupii","ScientificName"] <- "Steenstrupiella steenstrupii"
m.phy2[m.phy2$ScientificName == "Thalassionema_spp._._100_mm_length","ScientificName"] <- "Thalassionema"
m.phy2[m.phy2$ScientificName == "Thalassionema_spp._._100_mm_length.1","ScientificName"] <- "Thalassionema"
m.phy2[m.phy2$ScientificName == "Thalassiosira_gracilis_var._gracilis","ScientificName"] <- "Thalassiosira gracilis var. gracilis"
m.phy2[m.phy2$ScientificName == "Thalassiosira_spp._.10_mm","ScientificName"] <- "Thalassiosira"
m.phy2[m.phy2$ScientificName == "Thalassiosira_spp._10.20_mm","ScientificName"] <- "Thalassiosira"
m.phy2[m.phy2$ScientificName == "Thalassiosira_spp._20.40_mm","ScientificName"] <- "Thalassiosira"
m.phy2[m.phy2$ScientificName == "Thalassiosira_spp._40.60_mm","ScientificName"] <- "Thalassiosira"
m.phy2[m.phy2$ScientificName == "Thalassiosira_spp._60.80_mm","ScientificName"] <- "Thalassiosira"
m.phy2[m.phy2$ScientificName == "Thalassiosira_spp._80.100_mm","ScientificName"] <- "Thalassiosira"
m.phy2[m.phy2$ScientificName == "Thalassiothrix_cf._spp.","ScientificName"] <- "Thalassiothrix"
m.phy2[m.phy2$ScientificName == "Tripos_contortus_var._subcontortus","ScientificName"] <- "Tripos hexacanthus"
m.phy2[m.phy2$ScientificName == "Tripos_furca_type","ScientificName"] <- "Tripos furca"
m.phy2[m.phy2$ScientificName == "Tripos_fusus_type","ScientificName"] <- "Tripos fusus"
m.phy2[m.phy2$ScientificName == "Tripos_fusus.extensus","ScientificName"] <- "Tripos extensus"
m.phy2[m.phy2$ScientificName == "Tripos_gravidus_type","ScientificName"] <- "Tripos gravidus"
m.phy2[m.phy2$ScientificName == "Tripos_lineatus.pentagonus_complex","ScientificName"] <- "Tripos"
m.phy2[m.phy2$ScientificName == "Tripos_macroceros_type","ScientificName"] <- "Tripos macroceros"
m.phy2[m.phy2$ScientificName == "Tripos_muelleri_type","ScientificName"] <- "Tripos muelleri"
m.phy2[m.phy2$ScientificName == "Unid_autotroph_10.20_mm","ScientificName"] <- "Autotroph (unidentified) 10-20mm"
m.phy2[m.phy2$ScientificName == "Unid_colony.forming_autotroph_10.20_mm","ScientificName"] <- "Colony-forming Autotroph (unidentified) 10-20mm"
m.phy2[m.phy2$ScientificName == "Unid_diatom","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Unid_dinoflagellate_._10_mm","ScientificName"] <- "Dinoflagellata"
m.phy2[m.phy2$ScientificName == "Unid_dinoflagellate_._10_mm.1","ScientificName"] <- "Dinoflagellata"
m.phy2[m.phy2$ScientificName == "Unid_dinoflagellate_._50_mm","ScientificName"] <- "Dinoflagellata"
m.phy2[m.phy2$ScientificName == "Unid_dinoflagellate_10_._20_mm","ScientificName"] <- "Dinoflagellata"
m.phy2[m.phy2$ScientificName == "Unid_dinoflagellate_10_._30_mm","ScientificName"] <- "Dinoflagellata"
m.phy2[m.phy2$ScientificName == "Unid_dinoflagellate_30_._50_mm","ScientificName"] <- "Dinoflagellata"
m.phy2[m.phy2$ScientificName == "Codonellopsis gausii","ScientificName"] <- "Codonellopsis gaussi"
m.phy2[m.phy2$ScientificName == "Cylindrical diatom","ScientificName"] <- "Bacillariophyceae"
m.phy2[m.phy2$ScientificName == "Ephemera planamembranaceae","ScientificName"] <- "Ephemera planamembranacea"
m.phy2[m.phy2$ScientificName == "Globotoralia","ScientificName"] <- "Globorotalia"
m.phy2[m.phy2$ScientificName == "Rhizosolenia styliformis f. polydactyla","ScientificName"] <- "Rhizosolenia styliformis"
m.phy2[m.phy2$ScientificName == "Tripos schoeteri","ScientificName"] <- "Tripos schroeteri"
m.phy2[m.phy2$ScientificName == "Undella claparedi","ScientificName"] <- "Undella claparedei"
m.phy2[m.phy2$ScientificName == "Chaetoceros resting cyst","ScientificName"] <- "Chaetoceros"
m.phy2[m.phy2$OrigScientificName == "Unid_diatom","ScientificName"] <- "Bacillariophyceae"

# And finally remove the "_spp." and the "_"
m.phy2$ScientificName <- gsub("_spp.", "", m.phy2$ScientificName, fixed = T)
m.phy2$ScientificName <- gsub("_", " ", m.phy2$ScientificName, fixed = T)
# Check again before performing WoRMS check
unique(m.phy2$ScientificName)

### Do a first worms check
keys.worms <- wormsbynames( unique(m.phy2$ScientificName) )
keys.worms$ScientificName <- unique(m.phy2$ScientificName)

# Add WoRMS_status field
m.phy2 <-  add_column(m.phy2, WoRMS_status = NA, .after = "WoRMS_ID")
colnames(m.phy2)

# For testing the functions below:
#s <- unique(m.phy2$ScientificName)[3] ; s
s <- "Prorocentrum antarcticum"

require("parallel")
res <- mclapply( unique(m.phy2$ScientificName), function(s) {

            # Message
            message(paste(s, sep = ""))
            subset <- m.phy2[m.phy2$ScientificName == s,]
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

        }, mc.cores = 15 # >2 when on kryo

) # eo mclapply - s in taxa_names

### Rbind
ddf <- bind_rows(res)
rm(res); gc()
dim(ddf) # 3'863'540, same as before. Check some random lines.
# ddf[1000:1010,]
unique(ddf$Species) # Good. Those NA values should be the Genus-level and Order-level observations
unique(ddf[is.na(ddf$Species),"ScientificName"])
unique(ddf$WoRMS_ID) 
# What do the 'To_add_at_the_end' correspond to?
unique(ddf[ddf$WoRMS_ID == "To_add_at_the_end","ScientificName"]) # 8 dinoflagellates returned no ID though they are correct
# head(ddf[ddf$ScientificName == "Prorocentrum antarcticum",])
# keys.worms[keys.worms$ScientificName == "Prorocentrum antarcticum",]

# Manually correct those:
species2correct <- unique(ddf[ddf$WoRMS_ID == "To_add_at_the_end","ScientificName"]) ; species2correct

for(s in species2correct) {
    
    # keys.worms[keys.worms$ScientificName == s,]
    message(paste(s, sep = ""))
    ddf[ddf$ScientificName == s,"WoRMS_ID"] <- unique(keys.worms[keys.worms$ScientificName == s,"AphiaID"])
    ddf[ddf$ScientificName == s,"WoRMS_status"] <- "isMarine"
    ddf[ddf$ScientificName == s,"TaxonRank"] <- unique(keys.worms[keys.worms$ScientificName == s,"rank"])
    ddf[ddf$ScientificName == s,"Kingdom"] <- unique(keys.worms[keys.worms$ScientificName == s,"kingdom"])
    ddf[ddf$ScientificName == s,"Phylum"] <- unique(keys.worms[keys.worms$ScientificName == s,"phylum"])
    ddf[ddf$ScientificName == s,"Class"] <- unique(keys.worms[keys.worms$ScientificName == s,"class"])
    ddf[ddf$ScientificName == s,"Order"] <- unique(keys.worms[keys.worms$ScientificName == s,"order"])
    ddf[ddf$ScientificName == s,"Family"] <- unique(keys.worms[keys.worms$ScientificName == s,"family"])
    ddf[ddf$ScientificName == s,"Genus"] <- unique(keys.worms[keys.worms$ScientificName == s,"genus"])
    ddf[ddf$ScientificName == s,"Species"] <- s
    ddf[ddf$ScientificName == s,"Subspecies"] <- NA
    
} # eo for loop 

# Check again
unique(ddf$WoRMS_ID) 
# And check the ones witout any match
unique(ddf[ddf$WoRMS_ID == "No match found in WoRMS","ScientificName"]) 

### 27/10/2021: OK, good. Final step, inform LifeStafe from OrigScientificName...
unique(ddf$OrigScientificName)
str(ddf)

ddf[ddf$OrigScientificName == "Amphidinium_spp._.30_mm_cell_height","LifeForm"] <- "Cell height < 30mm"
ddf[ddf$OrigScientificName == "Centric_diatom","LifeForm"] <- "Centric"
ddf[ddf$OrigScientificName == "Centric_diatom_._10_mm","LifeForm"] <- "Centric <10mm"
ddf[ddf$OrigScientificName == "Centric_diatom_10.20_mm","LifeForm"] <- "Centric 10-20mm"
ddf[ddf$OrigScientificName == "Centric_diatom_20.30_mm","LifeForm"] <- "Centric 20-30mm"
ddf[ddf$OrigScientificName == "Centric_diatom_30.40_mm","LifeForm"] <- "Centric 30-40mm"
ddf[ddf$OrigScientificName == "Centric_diatom_40.50_mm","LifeForm"] <- "Centric 40-50mm"
ddf[ddf$OrigScientificName == "Pennate_diatom","LifeForm"] <- "Pennate"
ddf[ddf$OrigScientificName == "Pennate_diatom_._10_mm","LifeForm"] <- "Pennate <10mm"
ddf[ddf$OrigScientificName == "Pennate_diatom_._10_mm.1","LifeForm"] <- "Pennate >10mm"
ddf[ddf$OrigScientificName == "Pennate_diatom_._100_mm","LifeForm"] <- "Pennate >100mm"
ddf[ddf$OrigScientificName == "Pennate_diatom_10_._25_mm","LifeForm"] <- "Pennate 10-25mm"
ddf[ddf$OrigScientificName == "Pennate_diatom_25_._50_mm","LifeForm"] <- "Pennate 25-50mm"
ddf[ddf$OrigScientificName == "Pennate_diatom_50_._75_mm","LifeForm"] <- "Pennate 50-75mm"
ddf[ddf$OrigScientificName == "Pennate_diatom_75_._100_mm","LifeForm"] <- "Pennate 75-100mm"
ddf[ddf$OrigScientificName == "Chaetoceros_decipiens_._15_mm_cell_width","LifeForm"] <- "Cell width <15mm"
ddf[ddf$OrigScientificName == "Chaetoceros_peruvianus_._40_mm_cell_width","LifeForm"] <- "Cell width <40mm"
ddf[ddf$OrigScientificName == "Chaetoceros_peruvianus_._40_mm_cell_width.1","LifeForm"] <- "Cell width >40mm"
ddf[ddf$OrigScientificName == "Chaetoceros_resting_cyst","LifeForm"] <- "Resting cysts"
ddf[ddf$OrigScientificName == "Chaetoceros_spp._._10_mm_cell_width","LifeForm"] <- "Cell width <10mm"
ddf[ddf$OrigScientificName == "Ciliate_30.40_mm","LifeForm"] <- "30-40mm"
ddf[ddf$OrigScientificName == "Coscinodiscus_spp._._80_mm","LifeForm"] <- ">80mm"
ddf[ddf$OrigScientificName == "Coscinodiscus_spp._100.150_mm","LifeForm"] <- "100-150mm"
ddf[ddf$OrigScientificName == "Coscinodiscus_spp._150.200_mm","LifeForm"] <- "150-200mm"
ddf[ddf$OrigScientificName == "Coscinodiscus_spp._200.300_mm","LifeForm"] <- "200-300mm"
ddf[ddf$OrigScientificName == "Coscinodiscus_spp._300.400_mm","LifeForm"] <- "300-400mm"
ddf[ddf$OrigScientificName == "Coscinodiscus_spp._400.500_mm","LifeForm"] <- "400-500mm"
ddf[ddf$OrigScientificName == "Coscinodiscus_spp._60.80_mm","LifeForm"] <- "60-80mm"
ddf[ddf$OrigScientificName == "Cyanobacteria_coccoid_chain","LifeForm"] <- "Coccoid chain"
ddf[ddf$OrigScientificName == "Cylindrical_diatom","LifeForm"] <- "Cylindrical cell"
ddf[ddf$OrigScientificName == "Dinoflagellate_cyst","LifeForm"] <- "Dinocysts"
ddf[ddf$OrigScientificName == "Unid_dinoflagellate_._10_mm","LifeForm"] <- "<10mm"
ddf[ddf$OrigScientificName == "Unid_dinoflagellate_._10_mm.1","LifeForm"] <- ">10mm"
ddf[ddf$OrigScientificName == "Unid_dinoflagellate_._50_mm","LifeForm"] <- ">50mm"
ddf[ddf$OrigScientificName == "Unid_dinoflagellate_10_._20_mm","LifeForm"] <- "10-20mm"
ddf[ddf$OrigScientificName == "Unid_dinoflagellate_10_._30_mm","LifeForm"] <- "10-30mm"
ddf[ddf$OrigScientificName == "Unid_dinoflagellate_30_._50_mm","LifeForm"] <- "30-50mm"
ddf[ddf$OrigScientificName == "Ditylum_brightwellii_._40_mm_width","LifeForm"] <- "Cell width <40mm"
ddf[ddf$OrigScientificName == "Eucampia_spp._.100_mm_cell_length","LifeForm"] <- "Cell length <100mm"
ddf[ddf$OrigScientificName == "Flagellate_.10_mm_fusiform","LifeForm"] <- "Fusiform <10mm"
ddf[ddf$OrigScientificName == "Gymnodinioid_dinoflagellate_10.20_mm","LifeForm"] <- "10-20mm"
ddf[ddf$OrigScientificName == "Gyrodinium_spp._20.40_mm","LifeForm"] <- "20-40mm"
ddf[ddf$OrigScientificName == "Leptocylindrus_mediterraneus_.no_flagellates.","LifeForm"] <- "No flagellates"
ddf[ddf$OrigScientificName == "Leptocylindrus_mediterraneus_.with_flagellates.","LifeForm"] <- "With flagellates"
ddf[ddf$OrigScientificName == "Navicula_._shaped","LifeForm"] <- "Shaped like Navicula spp."
ddf[ddf$OrigScientificName == "Navicula_cf._.20_mm_length","LifeForm"] <- "Cell length <20mm"
ddf[ddf$OrigScientificName == "Navicula_cf._100.120_mm_length","LifeForm"] <- "Cell length 100-120mm"
ddf[ddf$OrigScientificName == "Navicula_cf._120.140_mm_length","LifeForm"] <- "Cell length 120-140mm"
ddf[ddf$OrigScientificName == "Navicula_cf._140.160_mm_length","LifeForm"] <- "Cell length 140-160mm"
ddf[ddf$OrigScientificName == "Navicula_cf._20.40_mm_length","LifeForm"] <- "Cell length 20-40mm"
ddf[ddf$OrigScientificName == "Navicula_cf._200.250_mm_length","LifeForm"] <- "Cell length 200-250mm"
ddf[ddf$OrigScientificName == "Navicula_cf._300.350_mm_length","LifeForm"] <- "Cell length 300-350mm"
ddf[ddf$OrigScientificName == "Navicula_cf._40.60_mm_length","LifeForm"] <- "Cell length 40-60mm"
ddf[ddf$OrigScientificName == "Navicula_cf._60.80_mm_length","LifeForm"] <- "Cell length 60-80mm"
ddf[ddf$OrigScientificName == "Navicula_cf._80.100_mm_length","LifeForm"] <- "Cell length 80-100mm"
ddf[ddf$OrigScientificName == "Phaeocystis_cf._spp._.colony_.50_um.","LifeForm"] <- "Phaeocystis colony <50mm"
ddf[ddf$OrigScientificName == "Phaeocystis_cf._spp._.colony_.50_um..1","LifeForm"] <- "Phaeocystis colony >50mm"
ddf[ddf$OrigScientificName == "Phaeocystis_spp._.colony.","LifeForm"] <- "Phaeocystis colony"
ddf[ddf$OrigScientificName == "leurosigma_._Gyrosigma_spp.","LifeForm"] <- "Pleurosigma+Gyrosigma spp."
ddf[ddf$OrigScientificName == "Pleurosigma_spp._._150_mm","LifeForm"] <- "<150mm"
ddf[ddf$OrigScientificName == "Pseudo.nitzschia_delicatissima_complex_..3_mm","LifeForm"] <- "delicatissima complex (<= 3mm)"
ddf[ddf$OrigScientificName == "Pseudo.nitzschia_seriata_complex_.3_mm","LifeForm"] <- "seriata complex (>3mm)"
ddf[ddf$OrigScientificName == "Pyrophacus_vancampoae_cyst","LifeForm"] <- "Dinocysts"
ddf[ddf$OrigScientificName == "Rhizosolenia_imbricata_.with_Richelia.","LifeForm"] <- "With Richelia inside"
ddf[ddf$OrigScientificName == "Silicoflagellate_4_points","LifeForm"] <- "Silicoflagellates with 4 points"
ddf[ddf$OrigScientificName == "Silicoflagellate_6_points","LifeForm"] <- "Silicoflagellates with 6 points"
ddf[ddf$OrigScientificName == "Thalassionema_spp._._100_mm_length","LifeForm"] <- "Cell length <100mm"
ddf[ddf$OrigScientificName == "Thalassionema_spp._._100_mm_length.1","LifeForm"] <- "Cell length >100mm"
ddf[ddf$OrigScientificName == "Thalassiosira_spp._.10_mm","LifeForm"] <- "<10mm"
ddf[ddf$OrigScientificName == "Thalassiosira_spp._10.20_mm","LifeForm"] <- "10-20mm"
ddf[ddf$OrigScientificName == "Thalassiosira_spp._20.40_mm","LifeForm"] <- "20-40mm"
ddf[ddf$OrigScientificName == "Thalassiosira_spp._40.60_mm","LifeForm"] <- "40-60mm"
ddf[ddf$OrigScientificName == "Thalassiosira_spp._60.80_mm","LifeForm"] <- "60-80mm"
ddf[ddf$OrigScientificName == "Thalassiosira_spp._80.100_mm","LifeForm"] <- "80-100mm"
ddf[ddf$OrigScientificName == "Tripos_lineatus.pentagonus_complex","LifeForm"] <- "lineatus pentagonus complex"
ddf[ddf$OrigScientificName == "Unid_autotroph_10.20_mm","LifeForm"] <- "10-20mm"
ddf[ddf$OrigScientificName == "Unid_colony.forming_autotroph_10.20_mm","LifeForm"] <- "10-20mm"
ddf[ddf$OrigScientificName == "Unid_heterotroph_10.20_mm","LifeForm"] <- "10-20mm"
ddf[ddf$OrigScientificName == "Unid_heterotroph_20.30_mm","LifeForm"] <- "20-30mm"
ddf[ddf$OrigScientificName == "Unid_heterotroph_30.40_mm","LifeForm"] <- "30-40mm"
ddf[ddf$OrigScientificName == "Unid_round_green_object_._5_mm","LifeForm"] <- "<5mm"
# ddf[ddf$OrigScientificName == "","LifeForm"] <- ""

### Check: 
unique(ddf$LifeForm) ; str(ddf$LifeForm)
# Check those with NA to see if you've missed something...
unique( ddf[is.na(ddf$LifeForm),"OrigScientificName"] ) # OK

# And check some random rows to make sure the formatting went all across OrigScientificName, ScientificName and LifeForm
# colnames(ddf)
ddf[10457:10477,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[35020:35040,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[373520:373540,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
ddf[623000:623020,c("OrigScientificName","ScientificName","LifeForm","TaxonRank")]
# Looks ganz OK :-)

### Last, make a map of sampling effort in space and then maybe a Hövmoller plot
d.effort <- ddf
d.effort$x_1d <- round(d.effort$decimalLongitude)
d.effort$y_1d <- round(d.effort$decimalLatitude)
d.effort$cell_id <- factor(paste(d.effort$x_1d, d.effort$y_1d, sep = "_"))
detach("package:worms", unload = TRUE)
detach("package:marmap", unload = TRUE)
detach("package:reshape2", unload = TRUE)
detach("package:plyr", unload = TRUE)
require("dplyr")

spatial.effort <- data.frame(d.effort %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n() ))
dim(spatial.effort) ; summary(spatial.effort)

# Map sampling effort
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 177 & world$long >= 80 & world$lat <= -10 & world$lat >= -67,],
        fill = "grey85", colour = "black", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Looks OK. Save on kryo. Make sure there are no rows with only NA values
which(rowSums(is.na(ddf)) == ncol(ddf)) # should return 'integer(0)'

save(ddf, file = "AusCPR_phyto_reformatted+WoRMScheck_27_10_21.Rdata")
write.table(ddf, file = "AusCPR_phyto_reformatted+WoRMScheck_27_10_21.txt", sep = "\t")


### ------------------------------------------------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------------------------------------------------
### ------------------------------------------------------------------------------------------------------------------------------------------------------
