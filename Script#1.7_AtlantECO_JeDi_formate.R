
##### ATLANTECO SCRIPT 1.7 ----------------------------------------------------------------------------------------------------------------------------
##### 17/06/2021: R Script to examine the JeDI jellyfish biomass database of Lucas et al. (2014) © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Aims to:
#  - Read the JeDi.nc file, fugure out structure etc. 
#  - Re-format to AtlantECO WP2 template
#  - Retrieve AphiaID from WoRMS --> Script#1.5

### Latest update: 18/06/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("ncdf4")

world <- map_data("world") # coastline for maps

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the data and figure out structure!
# Open a connection to the file
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database")
nc <- nc_open("JeDI.netcdf")
class(nc) # ncdf4
# str(nc)
print(nc)
#      37 variables (excluding dimension variables):
#         char project_title[maxlen_,unlimited]
#         char sub_project_title[maxlen_,unlimited]
#         char owner_dataset[maxlen_,unlimited]
#         char contact[maxlen_,unlimited]
#         char location_name[maxlen_,unlimited]
#         char date[maxlen_,unlimited]
#         double year[unlimited]
#         char month[maxlen_,unlimited]
#         char day[maxlen_,unlimited]
#         char time_local[maxlen_,unlimited]
#         double lat[unlimited]
#         double lon[unlimited]
#         char taxon[maxlen_,unlimited]
#         char rank_phylum[maxlen_,unlimited]
#         char rank_class[maxlen_,unlimited]
#         char rank_order[maxlen_,unlimited]
#         char rank_family[maxlen_,unlimited]
#         char rank_genus[maxlen_,unlimited]
#         char rank_species[maxlen_,unlimited]
#         char data_type[maxlen_,unlimited]
#         char collection_method[maxlen_,unlimited]
#         char net_opening[maxlen_,unlimited]
#         char net_mesh[maxlen_,unlimited]
#         char depth[maxlen_,unlimited]
#         char depth_upper[maxlen_,unlimited]
#         char depth_lower[maxlen_,unlimited]
#         char count_actual[maxlen_,unlimited]
#         char density[maxlen_,unlimited]
#         char density_integrated[maxlen_,unlimited]
#         char biovolume[maxlen_,unlimited]
#         char biovolume_integrated[maxlen_,unlimited]
#         char weight_wet[maxlen_,unlimited]
#         char weight_dry[maxlen_,unlimited]
#         char presence_absence[maxlen_,unlimited]
#         char study_type[maxlen_,unlimited]
#         char accompanying_ancillary_data[maxlen_,unlimited]
#         char catch_per_effort[maxlen_,unlimited]
#
#      2 dimensions:
#         unlimited  Size:429462   *** is unlimited ***
# [1] "vobjtovarid4: **** WARNING **** I was asked to get a varid for dimension named unlimited BUT this dimension HAS NO DIMVAR! Code will probably fail at this point"
#         maxlen_  Size:513
# [1] "vobjtovarid4: **** WARNING **** I was asked to get a varid for dimension named maxlen_ BUT this dimension HAS NO DIMVAR! Code will probably fail at this point"

### Seems like a lot of nice headers already...encouraging. Check attributes of .nc file
attributes(nc)
attributes(nc)$names

### Get a list of the nc variable names.
attributes(nc$var)$names # Nice

# Retrieve a tets variable with ncatt_get() ?
# ncatt_get(nc, attributes(nc$var)$names[7] )

### Retrieve data with:
# ?ncvar_get
# Retrieve test data using the ncvar_get function:
years <- ncvar_get(nc, attributes(nc$var)$names[7]) # 7 = year
# Print the data's dimensions
dim(years) ; class(years) ; head(years) ; summary(years) 
rm(years) ; gc()
# Test another one 
taxon <- ncvar_get(nc, attributes(nc$var)$names[13]) # 13 = taxon
class(taxon) ; dim(taxon) ; head(taxon) ; unique(taxon)
# Species ? 
taxon <- ncvar_get(nc, attributes(nc$var)$names[19]) # 19 = rank_species
class(taxon) ; dim(taxon) ; unique(taxon) # To be combined with Genus
# genus
genus <- ncvar_get(nc, attributes(nc$var)$names[18]) 
dim(genus) ; unique(genus)
# OK, you will use those to generate the OrigScientificName and 

# Check various oher fields
unique( ncvar_get(nc, attributes(nc$var)$names[31]) ) 

### All of this look actually quite good! Cbind all data together and reformat :-) 
# Use mclapply() to retrieve all data
# require("parallel")
# # v <- "contact"
# res <- mclapply(attributes(nc$var)$names, function(v) {
#
#         message(paste("Loading  ", v, sep = ""))
#         data <- data.frame(v = ncvar_get(nc,v)) # dim(data) ; summary(data) ; unique(data) ; length(sum(is.na(data[,v])))
#         colnames(data) <- v
#
#         # There are a lot of "" in the data, replace by NA. use if loop
#         if( "" %in% data[,v] ) {
#             data[,v] <- na_if(x = data[,v], y = "")
#         } # eo if loop
#
#         return(data)
#
#     }, mc.cores = 25
# ) # eo mclapply - varnames
# # Cbind ?bind_cols
# str(res)
# # Check if values are correct
# unique(res[[7]]) # Keeps having problem
# table <- bind_cols(cbind, res)
# head(table) ; dim(table)
# str(table)
# rm(res) ; gc()
# ### Check some fields
# unique(table$project_title) # Issues in the way the data was extracted
# summary(table$lat) # issue
# summary(table$lon) # issue
# summary(table$year) # issue

# ### Try less elegant manual extraction
# table <- data.frame(project_title = ncvar_get(nc,"project_title"), sub_project_title = ncvar_get(nc,"sub_project_title"),
#             owner = ncvar_get(nc,"owner_dataset"), location = ncvar_get(nc,"location_name"), data = ncvar_get(nc,"date"),
#             Year = ncvar_get(nc,"year"), Month = ncvar_get(nc,"month"), Day = ncvar_get(nc,"day"),
#             decimalLatitude = ncvar_get(nc,"lat"), decimalLongitude = ncvar_get(nc,"lon")
# ) # eo data.frame
# head(table)
# summary(table)
# That works :D dirty but works...

### Cbind manually
table <- data.frame(project_title = ncvar_get(nc,"project_title"), sub_project_title = ncvar_get(nc,"sub_project_title"),
        owner = ncvar_get(nc,"owner_dataset"), location = ncvar_get(nc,"location_name"), date = ncvar_get(nc,"date"), 
        Year = ncvar_get(nc,"year"), Month = ncvar_get(nc,"month"), Day = ncvar_get(nc,"day"), Time = ncvar_get(nc,"time_local"),
        decimalLatitude = ncvar_get(nc,"lat"), decimalLongitude = ncvar_get(nc,"lon"), Depth = ncvar_get(nc,"depth"), 
        MinDepth = ncvar_get(nc,"depth_upper"), MaxDepth = ncvar_get(nc,"depth_lower"), data_type = ncvar_get(nc,"data_type"),
        Method = ncvar_get(nc,"collection_method"), NetOpening = ncvar_get(nc,"net_opening"), NetMesh = ncvar_get(nc,"net_mesh"),
        study_type = ncvar_get(nc,"study_type"), Kingdom = "Animalia", Phylum = ncvar_get(nc,"rank_phylum"), Class = ncvar_get(nc,"rank_class"),  
        Order = ncvar_get(nc,"rank_order"), Family = ncvar_get(nc,"rank_family"), Genus = ncvar_get(nc,"rank_genus"),
        Species = ncvar_get(nc,"rank_species"), Occurrence = ncvar_get(nc,"presence_absence"), Count = ncvar_get(nc,"count_actual"), 
        Density = ncvar_get(nc,"density"), IntDensity = ncvar_get(nc,"density_integrated"), Biovolume = ncvar_get(nc,"biovolume"),
        IntBiovolume = ncvar_get(nc,"biovolume_integrated"), WetWeight = ncvar_get(nc,"weight_wet"), DryWeight = ncvar_get(nc,"weight_dry"), 
        CPUE = ncvar_get(nc,"catch_per_effort"), Note = ncvar_get(nc,"accompanying_ancillary_data")
) # eo data.frame  
# Check
colnames(table) ; dim(table)
str(table)
summary(table)
# head(table)

### Correct some str: 
# Months & Days
summary(factor(table$Month))
table$Month <- as.numeric(as.character(table$Month)) ; summary(table$Month)
summary(factor(table$Day))
table$Day <- as.numeric(as.character(table$Day)) ; summary(table$Day)
# Depths 
summary(factor(table$Depth))
table$Depth <- as.numeric(as.character(table$Depth)) ; summary(table$Depth)
# Are some Depth levels == -999 ? 
#table[table$Depth == -999 & !is.na(table$Depth),"Depth"]
# Ned to convert neg values to absolute
table$Depth <- abs(table$Depth)

summary(factor(table$MinDepth))
table$MinDepth <- as.numeric(as.character(table$MinDepth)) ; summary(table$MinDepth)
summary(factor(table$MaxDepth))
table$MaxDepth <- as.numeric(as.character(table$MaxDepth)) ; summary(table$MaxDepth)
# Convert MinDepth and MaxDepth to NA
table$MinDepth <- na_if(x = table$MinDepth, y = -999)
table$MaxDepth <- na_if(x = table$MaxDepth, y = -999)

### Check further columns to correct (wrong class usually)
str(table)
unique(table$owner)

### Replace "" by NaNs for: 
# owner
# location
# date
# Time
# NetOpening
# NetMesh
# study_type
table$owner <- na_if(x = table$owner, y = "")
table$location <- na_if(x = table$location, y = "")
table$date <- na_if(x = table$date, y = "")
table$Time <- na_if(x = table$Time, y = "")
table$NetOpening <- na_if(x = table$NetOpening, y = "")
table$NetMesh <- na_if(x = table$NetMesh, y = "")
table$study_type <- na_if(x = table$study_type, y = "")

### Convert numerics to actual numerics: Count to CPUE
table[,c(28:35)] <- sapply(table[c(28:35)], as.numeric)
summary(table) ; str(table) ; dim(table)
head( na.omit(table[,c(28:35)]) )

# Check if "absent" actually corresponds to null counts/density
summary(factor(table$Occurrence)) # no NA here
summary(table[table$Occurrence == "present",c(27:35)])
table[table$Occurrence == "present",c(27:35)][1:1000,]

### Add OrigScientificName and ScientificName
head(table[,c("Genus","Species")])
table$OrigScientificName <- factor(paste(table$Genus, table$Species, sep = "_"))
length(unique(table$OrigScientificName)) # 1067 taxa
unique(table$OrigScientificName)
table$OrigScientificName <- na_if(x = table$OrigScientificName, y = "_")
# Create ScientificName and replace _ by spaces
table$ScientificName <- str_replace_all(string = as.character(table$OrigScientificName), "_", " ")
unique(table$ScientificName)
dim(table[table$ScientificName == "obelia ",])

# Remove the trailing blank spaces
table$ScientificName <- trimws(table$ScientificName, which = c("right"))
table$OrigScientificName <- table$ScientificName 
table[10000:10500,]

head( table[!is.na(table$WetWeight),] )

# OK, save this table as temporary file before re-formatting to AtlantECO WP2 template
save(table, file = "JeDi_Lucas&al._2014_temporary_to_rfrmt_17_06_2021.Rdata")


### ----------------------------------------------------------------------------------------------------------------------------

### 18/06/21: Picking up the temporary file and re-format to AtlantECO WP2 template
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database")
table <- get(load("JeDi_Lucas&al._2014_temporary_to_rfrmt_17_06_2021.Rdata"))
dim(table) ; head(table)
str(table)

### Need to figure out how to treat the avrious quantitative data: Occurrence/Count/Density/IntDensity/WetWeight/DryWeight...
# GO HERE FOR DESCRIPTION OF FIELDS: https://www.bco-dmo.org/dataset/526852 
# - mesh size is in mm
# - count : Raw counts from respective survey (dimensionless)
# - density : unknown (avoid using)
# - intdensity: Depth integrated density (abundance, ind/m2) 
# - biovolume: Displacement volume of sample (mm/m3)
# - wet weight: Sample wet weight (g/m3) - biomass
# - dry weight: sample dry weight (g/m3)
### Those weight values DO NOT seem to be the ones used in the 2014 GEB paper by Lucas et al.
# - CPUE: Fisheries unit: an indirect measure of the abundance of a target species; also known as catch rate (kg/hectare)

# http://people.uncw.edu/condonr/JeDI/JeDI.html 
# https://www.ices.dk/sites/pub/CM%20Doccuments/CM-2014/Theme%20Session%20A%20contributions/A4414.pdf
#  a  total  of  91,765  quantitative  numerical  abundance  data  of  gelatinous taxa in the upper 200 m between the years 1934 and 2011 were extracted from the database and converted into carbon biomass (mg C m-3) using species, family or group-specific length–mass or mass–mass linear and logistic regression equations (Lucas et al. 2011).
# Try to identify those 90,000 abudn data
dim(table[!is.na(table$Density),]) # 257'600
dim(table[!is.na(table$IntDensity),]) # 76'146
dim(table[!is.na(table$Depth) & table$Depth <= 200,]) # 45'562
dim(table[!is.na(table$MaxDepth) & table$MaxDepth <= 200,]) # 102'198

### Based on this information, and considering we have to do our own conversion to biomass for AtlantECO WP2 (MAREDAT update), just keep 
### occurrence values --> for ZOObase update and integrated density (abundance)
colnames(table)
summary(table[,c("decimalLongitude","decimalLatitude","Year","Month","Day","Depth","MinDepth","MaxDepth")])
# Check those lines that don't have month data (3.7% of data)
# table[is.na(table$Month),][1:100,] ; summary( table[is.na(table$Month),"Year"] ) # Median year is 1991, so not that old
table <- table %>% drop_na(Month)
# > 60% of data lack depth value ! 
# summary( table[is.na(table$Depth),"Year"] ) # from a large year span

# Check Occurrence status
summary(factor(table$Occurrence))
# OK, keep all for P-A data

# Check intdensity status
summary(table$IntDensity) # 81% of the data are NAs
dim(table %>% drop_na(IntDensity))
# Those non values should have depth valeus though
dim(table[!is.na(table$IntDensity),c("decimalLongitude","decimalLatitude","Year","Month","Day","Depth","MinDepth","MaxDepth")])
# 63328 rows (~19% of data)
summary(table[!is.na(table$IntDensity),c("decimalLongitude","decimalLatitude","Year","Month","Day","Depth","MinDepth","MaxDepth")])

### And check the completion of the taxonomic id
unique(table$ScientificName)
dim(table[is.na(table$ScientificName),]) # 114257 rows though == 27% of data
# Need to complete those NA values
head(table[is.na(table$ScientificName),c("Phylum","Class","Order","Family")])

### For P/A data --> discard all the stuff that have no value in ScientificName
### For IntDensity data (abundance) data --> inform at leats one level (Family/Order/Class )

occ.data <- table[,c(1:28,32,37:38)] # keep IntBiovolume #32
occ.data <- occ.data %>% drop_na(ScientificName)
dim(occ.data) # 299'018
unique(occ.data$ScientificName)

abund.data <- table[!is.na(table$IntDensity),]
dim(abund.data) # 63'328
# How many points with missing ScientificName
dim(abund.data[is.na(abund.data$ScientificName),]) # 44290 == 69% of data
# Need to give those a Family/Order/Class

# Progressively add Family, if, not add Order and so on
# Make sure to replace "" by NAs first though
abund.data$Family <- na_if(x = abund.data$Family, y = "")
abund.data$Order <- na_if(x = abund.data$Order, y = "")
abund.data$Class <- na_if(x = abund.data$Class, y = "")
abund.data$Phylum <- na_if(x = abund.data$Phylum, y = "")
# Family
abund.data[is.na(abund.data$ScientificName),"ScientificName"] <- abund.data[is.na(abund.data$ScientificName),"Family"]
unique(abund.data$ScientificName)
dim(abund.data[is.na(abund.data$ScientificName),]) # 35617/63328 == 56% of data
# Order
abund.data[is.na(abund.data$ScientificName),"ScientificName"] <- abund.data[is.na(abund.data$ScientificName),"Order"]
dim(abund.data[is.na(abund.data$ScientificName),]) # 18275/63328 == 29% of the data
# Class
abund.data[is.na(abund.data$ScientificName),"ScientificName"] <- abund.data[is.na(abund.data$ScientificName),"Class"]
dim(abund.data[is.na(abund.data$ScientificName),]) # 7439/63328 == 12% of the data
# Phylum
abund.data[is.na(abund.data$ScientificName),"ScientificName"] <- abund.data[is.na(abund.data$ScientificName),"Phylum"]
unique(abund.data$ScientificName)
# OK, no more NAs

### Re-format to AtlantECO template ! 

### A) Occ data only (Genus- and species level)
colnames(occ.data)
unique(occ.data$location)

occ.data.rfrmt <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "None given", obisID = "Not applicable", DatasetKey = "Not applicable",
                decimalLatitude = occ.data$decimalLatitude, decimalLongitude = occ.data$decimalLongitude, geodeticDatum = "Assumed_WGS84", 
                CoordUncertainty = NA, CountryCode = occ.data$location, eventDate = occ.data$date,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = occ.data$Year, Month = occ.data$Month, Day = occ.data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Assumed water column", LonghurstProvince = NA,
                Depth = occ.data$Depth, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = occ.data$MinDepth, MaxDepth = occ.data$MaxDepth, 
                ParentEventID = occ.data$sub_project_title, EventID = "None given", InstitutionCode = occ.data$owner, SourceArchive = "bco-dmo.org", 
                OrigCollectionCode = "Dataset#526852", OrigCollectionID = "Dataset version: 2015-01-08",
                BiblioCitation = "Lucas et al. (2014) - Gelatinous zooplankton biomass in the global oceans: geographic variation and environmental drivers",
                CitationDOI = "doi:10.1111/geb.12169", DateDataAccess = '2021-05-06',
                OrigScientificName = occ.data$OrigScientificName, ScientificName = occ.data$ScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = NA, 
                Kingdom = "Animalia", Phylum = occ.data$Phylum, Class = occ.data$Class, Order = occ.data$Order,
                Family = occ.data$Family, Genus = occ.data$Genus, Species = NA,
                Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Occurrence", MeasurementTypeID = "To_define", MeasurementValue = occ.data$Occurrence,
                MeasurementUnit = "P/A", MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = factor(paste(occ.data$Method, occ.data$study_type, sep = ";")),
                SamplingProtocol = factor(paste(occ.data$Method,";",occ.data$study_type,"; NetOpening (m):",occ.data$NetOpening,"; NetMesh (mm):",occ.data$NetMesh, sep = "")),
                SampleAmount = occ.data$IntBiovolume, SampleAmountUnit = "Integrated displacement volume of sample (mm/m2)", SampleEffort = NA,
                DeterminedBy = occ.data$owner, DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
### Check before saving
dim(occ.data.rfrmt) # 299'018      70
str(occ.data.rfrmt)
summary(occ.data.rfrmt)
occ.data.rfrmt[1:100,]

### Save
save(occ.data.rfrmt, file = "JeDi_Lucas&al._2014_occurrences_reformated_18_06_2021.Rdata")


### B) Abund data
colnames(abund.data)

ab.data.rfrmt <- data.frame(ProjectID = "AtlantECO_H2020_GA#210591007", ProjectWP = "WP2", DataSilo = "Trad_microscopy", ContactName = "Fabio_Benedetti;Meike_Vogt",
                ContactAdress = "fabio.benedetti@usys.ethz.ch;meike.vogt@usys.ethz.ch", occurrenceID = "To_define_within_AtlantECO",
                orig_occurrenceID = "None given", obisID = "Not applicable", DatasetKey = "Not applicable",
                decimalLatitude = abund.data$decimalLatitude, decimalLongitude = abund.data$decimalLongitude, geodeticDatum = "Assumed_WGS84", 
                CoordUncertainty = NA, CountryCode = abund.data$location, eventDate = abund.data$date,
                eventDateInterval = NA, eventDateIntervalUnit = NA, Year = abund.data$Year, Month = abund.data$Month, Day = abund.data$Day,
                Bathymetry = NA, BathySource = "ETOPO1-NOAA", HabitatType = "Assumed water column", LonghurstProvince = NA,
                Depth = abund.data$Depth, DepthAccuracy = NA, DepthIntegral = NA,
                MinDepth = abund.data$MinDepth, MaxDepth = abund.data$MaxDepth, 
                ParentEventID = abund.data$sub_project_title, EventID = "None given", InstitutionCode = abund.data$owner, SourceArchive = "bco-dmo.org", 
                OrigCollectionCode = "Dataset#526852", OrigCollectionID = "Dataset version: 2015-01-08",
                BiblioCitation = "Lucas et al. (2014) - Gelatinous zooplankton biomass in the global oceans: geographic variation and environmental drivers",
                CitationDOI = "doi:10.1111/geb.12169", DateDataAccess = '2021-05-06',
                OrigScientificName = abund.data$OrigScientificName, ScientificName = abund.data$ScientificName,
                WoRMS_ID = "To_add_at_the_end", TaxonRank = NA, 
                Kingdom = "Animalia", Phylum = abund.data$Phylum, Class = abund.data$Class, Order = abund.data$Order,
                Family = abund.data$Family, Genus = abund.data$Genus, Species = NA, Subspecies = NA, LifeForm = NA, AssocTaxa = NA,
                MeasurementID = "To_define", MeasurementType = "Depth integrated abundance", MeasurementTypeID = "To_define", MeasurementValue = abund.data$IntDensity,
                MeasurementUnit = "ind/m2", MeasurementAcurracy = NA, MeasurementValueID = "To_define", Biomass_mgCm3 = NA, BiomassConvFactor = NA,
                basisOfRecord = factor(paste(abund.data$Method, abund.data$study_type, sep = ";")),
                SamplingProtocol = factor(paste(abund.data$Method,";",abund.data$study_type,"; NetOpening (m):",abund.data$NetOpening,"; NetMesh (mm):",abund.data$NetMesh, sep = "")),
                SampleAmount = abund.data$IntBiovolume, SampleAmountUnit = "Integrated displacement volume of sample (mm/m2)", SampleEffort = NA,
                DeterminedBy = abund.data$owner, DeterminedDate = NA, Note = NA, Flag = NA 
) # eo ddf
### Check before saving
dim(ab.data.rfrmt) # 63'328            70
str(ab.data.rfrmt)
summary(ab.data.rfrmt)
ab.data.rfrmt[1:1000,]

### Save
save(ab.data.rfrmt, file = "JeDi_Lucas&al._2014_abundances_reformated_18_06_2021.Rdata")
  
### Good. Now go to Script#1.5 to provide AphiaIDs from WoRMS

###
colnames(jedi)
summary(factor(jedi[,c('Order')]))
dim(jedi[jedi$Order == "Salpida",]) # 18'015 
salps <- jedi[jedi$Order == "Salpida",]
dim(salps)
head(salps)

i <- which(rowSums(is.na(salps)) == ncol(salps)) # should return 'integer(0)'


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
