
##### ATLANTECO SCRIPT 2.1.2 ----------------------------------------------------------------------------------------------------------------------------
##### 29/10/2021: R Script to aggregate and map the sampling effort of some pf the AtlantECO W2 datasets for the General Assembly meeting (those not mapped in Script #2.1.1).  
##### © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### For the followong datasets:
#	- all CPR surveys combined
#   - AMT transects
#   - COPEPOD (NOAA) - copepods only for now
#   - KRILLBASE

### Latest update: 01/11/2021

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")
library("viridis")
library("lubridate")

# Load coastline for mapping
world <- map_data("world")

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/") ; dir()

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) All CPR surveys

### Go to the directory containing your data
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/") ; dir()
cpr.calci <- get(load("CPR_calcifiers_reformatted+WoRMScheck_25_10_21.Rdata"))
summary(cpr.calci$Year)

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2") ; dir()
cpr.zoo <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
cpr.phyto <- get(load("CPR_phytoplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
dim(cpr.calci); dim(cpr.zoo); dim(cpr.phyto) 
# [1] 816806     72
# [1] 712720     72
# [1] 755168     72

cpr.calci$Survey <- NA
cpr.zoo$Survey <- NA
cpr.phyto$Survey <- NA
# Separate between N Pacific and N Atlantic
summary(cpr.zoo[,"decimalLongitude"]); summary(cpr.phyto[,"decimalLongitude"]); summary(cpr.calci[,"decimalLongitude"])
# decimalLongitude < -90° or > 120° --> "North Pacific CPR"
cpr.zoo[cpr.zoo$decimalLongitude < -90 | cpr.zoo$decimalLongitude > 120,"Survey"] <- "North Pacific CPR"
cpr.phyto[cpr.phyto$decimalLongitude < -90 | cpr.phyto$decimalLongitude > 120,"Survey"] <- "North Pacific CPR"
cpr.calci[cpr.calci$decimalLongitude < -90 | cpr.calci$decimalLongitude > 120,"Survey"] <- "North Pacific CPR"
# N Atlantic
cpr.zoo[is.na(cpr.zoo$Survey),"Survey"] <- "North Atlantic CPR"
cpr.phyto[is.na(cpr.phyto$Survey),"Survey"] <- "North Atlantic CPR"
cpr.calci[is.na(cpr.calci$Survey),"Survey"] <- "North Atlantic CPR"
# summary(factor(cpr.zoo$Survey))
# Quick map to check 
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
#        fill = "grey85", colour = "grey50", size = 0.3) +
#    geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Survey)), data = cpr.zoo, alpha = .5) +
#    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
# Change those few NPac points from the Arctic
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
#        fill = "grey85", colour = "grey50", size = 0.3) +
#    geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Survey)),
#         data = cpr.zoo[cpr.zoo$Survey == "North Pacific CPR" & cpr.zoo$decimalLongitude > -118 & cpr.zoo$decimalLongitude < 0,], alpha = .5) +
#    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#dim(cpr.zoo[cpr.zoo$Survey == "North Pacific CPR" & cpr.zoo$decimalLongitude > -118 & cpr.zoo$decimalLongitude < 0,])
cpr.zoo[cpr.zoo$Survey == "North Pacific CPR" & cpr.zoo$decimalLongitude > -118 & cpr.zoo$decimalLongitude < 0,"Survey"] <- "North Atlantic CPR"
cpr.phyto[cpr.phyto$Survey == "North Pacific CPR" & cpr.phyto$decimalLongitude > -118 & cpr.phyto$decimalLongitude < 0,"Survey"] <- "North Atlantic CPR"
cpr.calci[cpr.calci$Survey == "North Pacific CPR" & cpr.calci$decimalLongitude > -118 & cpr.calci$decimalLongitude < 0,"Survey"] <- "North Atlantic CPR"


setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/SO-CPR") ; dir()
so.cpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
dim(so.cpr)
so.cpr$Survey <- "Southern Ocean CPR"
# Sub colnames(so.cpr)[1:71,77]

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/AusCPR") ; dir()
aus.cpr.phyto <- get(load("AusCPR_phyto_reformatted+WoRMScheck_27_10_21.Rdata"))
aus.cpr.zoo <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
dim(aus.cpr.phyto); dim(aus.cpr.zoo)
aus.cpr.phyto$Survey <- "Australian CPR"
aus.cpr.zoo$Survey <- "Australian CPR"


### Rbind all
cpr <- rbind(cpr.calci,cpr.zoo,cpr.phyto,so.cpr[,c(1:71,77)],aus.cpr.phyto,aus.cpr.zoo)
dim(cpr) # 28'726'552
str(cpr)
summary(cpr)
# Check if spatial coordinates follow CRS WGS84
summary(cpr[,c("decimalLatitude","decimalLongitude","Year")])

### Aggregate N observations (= proxy of sampling effort) per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
cpr$x_1d <- round(cpr$decimalLongitude)
cpr$y_1d <- round(cpr$decimalLatitude)
cpr$cell_id <- factor(paste(cpr$x_1d, cpr$y_1d, sep = "_"))

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(cpr %>% group_by(cell_id,Survey) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)

map1 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = factor(Survey)), data = na.omit(spatial.effort) ) +
    scale_fill_manual(name = "Survey", values = c("#ff7f00","#1f78b4","#33a02c","#e31a1c")) + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

# Map sampling effort
map2 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 

### Save both maps
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/")
ggsave(plot = map1, filename = "panel_plots_all_CPR_surveys_29_10_21.jpg", dpi = 300, height = 7, width = 10)
ggsave(plot = map2, filename = "panel_plots_sampling_effort_all_CPR_surveys_29_10_21.jpg", dpi = 300, height = 7, width = 10)
 

### Now, indicate the characteristics of each survey:
# N records
# temporal range
# N taxa
# Type of data
# Null abundances?

head(cpr.zoo[is.na(cpr.zoo$MeasurementValue),])

### A°) North Atlantic CPR
nrow(cpr[cpr$Survey == "North Atlantic CPR",])

summary(cpr.zoo[cpr.zoo$Survey == "North Atlantic CPR","Year"])
summary(cpr.phyto[cpr.phyto$Survey == "North Atlantic CPR","Year"])
summary(cpr.calci[cpr.calci$Survey == "North Atlantic CPR","Year"])
# Are there differences between Pteropods/Coccos/Forams? unique(cpr.calci$OrigScientificName)
summary(cpr.calci[cpr.calci$OrigScientificName == "Coccolithophores","Year"])
summary(cpr.calci[cpr.calci$OrigScientificName == "Foraminifera","Year"])
summary(cpr.calci[cpr.calci$OrigScientificName == "Euthecosomata","Year"])
summary(cpr.calci[cpr.calci$Survey == "North Atlantic CPR","MeasurementValue"])

length(unique(cpr.zoo[cpr.zoo$Survey == "North Atlantic CPR","ScientificName"]))
length(unique(cpr.phyto[cpr.phyto$Survey == "North Atlantic CPR","ScientificName"]))

summary(cpr.zoo[cpr.zoo$Survey == "North Atlantic CPR","MeasurementValue"])
summary(cpr.phyto[cpr.phyto$Survey == "North Atlantic CPR","MeasurementValue"])

### B°) North Pac CPR
nrow(cpr[cpr$Survey == "North Pacific CPR",])
summary(cpr[cpr$Survey == "North Pacific CPR","Year"])
length(unique(cpr.zoo[cpr.zoo$Survey == "North Pacific CPR","ScientificName"]))
length(unique(cpr.phyto[cpr.phyto$Survey == "North Pacific CPR","ScientificName"]))
summary(cpr.zoo[cpr.zoo$Survey == "North Pacific CPR","MeasurementValue"])
summary(cpr.phyto[cpr.phyto$Survey == "North Pacific CPR","MeasurementValue"])

### C°) Southern Ocean
nrow(cpr[cpr$Survey == "Southern Ocean CPR",])
summary(cpr[cpr$Survey == "Southern Ocean CPR","Year"])
length(unique(so.cpr$ScientificName))
summary(so.cpr$MeasurementValue)

### D°) Australian CPR
nrow(cpr[cpr$Survey == "Australian CPR",])
summary(cpr[cpr$Survey == "Australian CPR","Year"])
length(unique(aus.cpr.phyto$ScientificName))
length(unique(aus.cpr.zoo$ScientificName))
summary(aus.cpr.phyto$MeasurementValue)
summary(aus.cpr.zoo$MeasurementValue)

### ----------------------------------------------------------------------------------------------------------------------------

### 2°) PhytoBase & ZooBase
# PhytoBase
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/PHYTObase"); dir()
phy <- get(load("PhytoBase_reformated+WoRMScheck_08_06_2021.Rdata"))
str(phy)
summary(phy$Year) # 6??
dim(phy[phy$Year < 1000,]) # remove 
phy <- phy[!(phy$Year < 1000),]
summary(phy$Year) 
mean(phy$Year); sd(phy$Year)
nrow(phy)
length( unique(phy$ScientificName) )
summary(phy$Depth); mean(phy$Depth, na.rm = T); sd(phy$Depth, na.rm = T)

# Relative proportion of groups
summary(factor(phy$Phylum))
# Diatoms = 699097/1332742
# Myzozoa = 527293/1332742
# Haptophytes = 47183/1332742

# ZooBase
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase"); dir()
zoo <- get(load("ZooBase_merged_temp_for_Zenodo_06_07_21.Rdata"))
nrow(zoo) # 1453665
summary(zoo$Year) ; mean(zoo$Year); sd(zoo$Year)
summary(zoo$Depth); mean(zoo$Depth, na.rm = T); sd(zoo$Depth, na.rm = T)
length( unique(zoo$ScientificName) )
# Relative proportion of groups
summary(factor(zoo$Phylum))
summary(factor(zoo$Class))
summary(factor(zoo$Order))

# Copepods = 1070092/1453665
# Jellyfish = (63320+8687)/1453665
# Thaliacea = 10238/1453665
# Chaetognatha = 79033/1453665
# Pteropods = 70654/1453665 
# Malacostraca = 83942/1453665
# Forams = 20947/1453665


### ----------------------------------------------------------------------------------------------------------------------------

### 3°) JeDI
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database"); dir()
abund <- get(load("JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_22_06_2021.Rdata"))
occs <- get(load("JeDi_Lucas&al._2014_occurrences_reformated+WoRMScheck_18_06_2021.Rdata"))
dim(abund); dim(occs)
abund$Type <- "Abund" ; occs$Type <- "Occ"
# Rbind?
jedi <- rbind(abund,occs)
dim(jedi) # 362297
summary(factor(jedi$Type))/nrow(jedi)
summary(jedi$Year) ; mean(jedi$Year); sd(jedi$Year)
summary(jedi$Depth); mean(jedi$Depth, na.rm = T); sd(jedi$Depth, na.rm = T) # 221635/362297
summary(jedi$MinDepth)
summary(jedi$MaxDepth)

length( unique(occs$ScientificName) )
length( unique(abund$ScientificName) )

unique(jedi$MeasurementType)
unique(jedi$MeasurementUnit)
unique(occs$MeasurementValue)
summary(abund$MeasurementValue)

(summary(factor(occs$Phylum))/nrow(occs))*100

summary(factor(abund$Phylum))/nrow(abund)
# Chordata   Cnidaria 
#  27975     31205


### ----------------------------------------------------------------------------------------------------------------------------

### 4°) MALASPINA (Villarino et al. 2018)
gela <- get(load("MALASPINA_Villarinoetal._2017_gelatinous_zooplankton_reformated+WoRMScheck_01_06_2021.Rdata"))
cocco <- get(load("MALASPINA_Villarinoetal._2017_coccolithophores_reformated+WoRMScheck_01_06_2021.Rdata"))
dino <- get(load("MALASPINA_Villarinoetal._2017_dinoflagellates_reformated+WoRMScheck_01_06_2021.Rdata"))
diato <- get(load("MALASPINA_Villarinoetal._2017_diatoms_reformated+WoRMScheck_01_06_2021.Rdata"))
# dim(gela); dim(cocco); dim(dino); dim(diato)
nrow(gela)+nrow(cocco)+nrow(dino)+nrow(diato)
length(unique(cocco$ScientificName))
length(unique(dino$ScientificName))
length(unique(diato$ScientificName))
length(unique(gela$ScientificName))
unique(gela$Phylum)


### ----------------------------------------------------------------------------------------------------------------------------

### 5°) COPEPOD-NOAA (v2014) - copepods as an example
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA"); dir()
cops <- read.csv("COPEPOD_copepod.csv", h = F, dec = ".", sep = ",", skip = 2) 
dim(cops) # 588'898
head(cops)
str(cops)
# colnames(cops)
copsv2 <- cops[c(3:588896),]
dim(copsv2)
head(copsv2)
str(copsv2)
colnames(copsv2)
# V7 and V8 are LATITUDE and LONGITDE
copsv2$decimalLatitude <- as.numeric(copsv2$V7)
copsv2$decimalLongitude <- as.numeric(copsv2$V8)
summary(copsv2[,c("decimalLatitude","decimalLongitude")])
# Good :-) Quick map (no time to clean everythong for AtlantECO's General Assembly)

copsv2$x_1d <- round(copsv2$decimalLongitude)
copsv2$y_1d <- round(copsv2$decimalLatitude)
copsv2$cell_id <- factor(paste(copsv2$x_1d, copsv2$y_1d, sep = "_"))

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(copsv2 %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)

map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 
        
ggsave(plot = map, filename = "map_sampling_effort_copepods_COPEPOD_01_11_21.jpg", dpi = 300, height = 7, width = 10)

### Try to make the panel like for AtlantECO's interim report
# Plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
# Find year and month: V2-V3-V4: year, mon, day
unique(copsv2$V2) # Good, years
unique(copsv2$V3) # Trim this and convert
unique(copsv2$V4) # Trim this and convert
?trimws
copsv2$Year <- as.numeric(copsv2$V2) ; summary(copsv2$Year); sd(copsv2$Year)
copsv2$Month <- as.numeric(trimws(copsv2$V3, "both")) ; summary(copsv2$Month)
copsv2$Day <- as.numeric(trimws(copsv2$V4, "both")) ; summary(copsv2$Day)

hovmuller <- data.frame(copsv2 %>% group_by(Year,Month) %>% summarize(N = n()))
# dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
# Where are the depth levels?
# V9 = Upper depth, V10 = lower depth
unique(copsv2$V9) 
unique(copsv2$V10)
copsv2$MinDepth <- as.numeric(trimws(copsv2$V9, "both")) ; summary(copsv2$MinDepth)
copsv2$MaxDepth <- as.numeric(trimws(copsv2$V10, "both")) ; summary(copsv2$MaxDepth)
# Convert -999 to NA
copsv2[copsv2$MinDepth < -99,"MinDepth"] <- NA
copsv2[copsv2$MaxDepth < -99,"MaxDepth"] <- NA

# Range of MaxDepth
summary(copsv2$MaxDepth); sd(copsv2$MaxDepth, na.rm = T)

copsv2$DepthLevels <- NA
sub <- copsv2 %>% drop_na(MaxDepth) # dim(sub) --> 588771 rows
sub2 <- copsv2[is.na(copsv2$MaxDepth),]  # dim(sub2) --> 123 rows only - discard
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$MaxDepth, breaks = seq(from = min(sub$MaxDepth), to = max(sub$MaxDepth), by = 10)))
# unique(sub$DepthLevels) ; str(sub$DepthLevels)
# Then bin manually? 
sub[sub$MaxDepth >= 500 & sub$MaxDepth < 600,"DepthLevels"] <- "(500,600]"
sub[sub$MaxDepth >= 600 & sub$MaxDepth < 700,"DepthLevels"] <- "(600,700]"
sub[sub$MaxDepth >= 700 & sub$MaxDepth < 800,"DepthLevels"] <- "(700,800]"
sub[sub$MaxDepth >= 800 & sub$MaxDepth < 900,"DepthLevels"] <- "(800,900]"
sub[sub$MaxDepth >= 900 & sub$MaxDepth < 1000,"DepthLevels"] <- "(900,1000]"
sub[sub$MaxDepth >= 1000 & sub$MaxDepth < 2000,"DepthLevels"] <- "(1000,2000]"
sub[sub$MaxDepth >= 2000 & sub$MaxDepth < 3000,"DepthLevels"] <- "(2000,3000]"
sub[sub$MaxDepth >= 3000 & sub$MaxDepth < 4000,"DepthLevels"] <- "(3000,4000]"
sub[sub$MaxDepth >= 4000 & sub$MaxDepth < 5000,"DepthLevels"] <- "(4000,5000]"
sub[sub$MaxDepth >= 5000,"DepthLevels"] <- ">5000"
sub[sub$MaxDepth == 0,"DepthLevels"] <- "(0,10]"
### And finally, remove brackets and comas from the levels 
sub$DepthLevels <- str_replace_all(string = sub$DepthLevels, pattern = "\\(", replacement = "")
sub$DepthLevels <- str_replace_all(string = sub$DepthLevels, pattern = "]", replacement = "")
sub$DepthLevels <- str_replace_all(string = sub$DepthLevels, pattern = ",", replacement = "-")
sub$DepthLevels <- factor(sub$DepthLevels)
# levels(sub$DepthLevels)
d <- rbind(sub,sub2)
# summary(d$DepthLevels) ; str(d$DepthLevels)
# OK, will have to re-order this 
depth.levels <- c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100",
                "100-110","110-120","120-130","130-140","140-150","150-160","160-170","170-180","180-190","190-200",
                "200-210","210-220","220-230","230-240","240-250","250-260","260-270","270-280","280-290","290-300",
                "300-310","310-320","320-330","330-340","340-350","350-360","360-370","370-380","380-390","390-400",
                "400-410","410-420","420-430","430-440","440-450","450-460","460-470","470-480","480-490","490-500",
                "500-600","600-700","700-800","800-900","900-1000","1000-2000","2000-3000","3000-4000","4000-5000",">5000")
# Latitude
p1 <- ggplot(d, aes(x = decimalLatitude)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(decimalLatitude)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Latitude (1° bins)", y = "N records") + theme_classic()
# Depth - Re-order for DepthLevels first
d$DepthLevels <- factor(d$DepthLevels, levels = depth.levels)
p2 <- ggplot(d, aes(x = DepthLevels)) + geom_histogram(fill = "black", stat = "count") +
    geom_vline(aes(xintercept = median(MaxDepth, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()
# Years
p4 <- ggplot(d, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("ggpubr")
library("patchwork")

patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = "COPEPOD (T. O'Brien, 2014)", 
            subtitle = 'Copepod abundance data')
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_copepods_COPEPOD_01_11_21.jpg", dpi = 300, height = 14, width = 11)
 
### What kind of units do we have here? 
unique(copsv2$V23)
unique(copsv2$V25); summary(factor(copsv2$V25))

### ----------------------------------------------------------------------------------------------------------------------------

### 6°) AMT data (check R script #1.8)
ptero <- read.table("AMT_24_Burridge2016_PteropodsHeteropods_18_10_21.txt", h = T)
dim(ptero); str(ptero)
head(ptero)
unique(ptero$Order)
unique(ptero$SamplingProtocol)

ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 29 & world$long >= -80 & world$lat >= -70 & world$lat <= 70,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = ptero, pch = 21, colour = "black", fill = "blue") +
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

# Combine with the other?
setwd(paste(getwd(),"/AMT_Long_Datasets", sep = ""))
d <- read.table("AMT01-29_CTD.dat", sep = "", skip = 2, dec = ".")
dim(d) ; str(d)
# And manually add colnames
colnames(d) <- c("Date","Time_GMT","decimalLatitude","decimalLongitude","Depth","Temp_degC","Salinity","Oxygen_µM","Fluorescence_mg/m3")
summary(d)

map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 29 & world$long >= -80 & world$lat >= -70 & world$lat <= 70,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = d, colour = "#b3cde3", alpha = 0.65) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = ptero, pch = 21, colour = "black", fill = "#377eb8", size = 2.5) +
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

ggsave(plot = map, filename = "map_AMT_pteropods+CTD_data_01_11_21.jpg", dpi = 300, height = 10, width = 6)

### Convert "Date" to actual Date vector
lubridate::ymd(str_replace_all(string = as.character(unique(d$Date)), "/", "-"))
d$eventDate <- lubridate::ymd(str_replace_all(string = as.character(d$Date), "/", "-"))
d$Year <- lubridate::year(d$eventDate)
d$Month <- lubridate::month(d$eventDate)
d$Day <- lubridate::day(d$eventDate)
summary(d[,c("Year","Month","Day")]) ; sd(d$Year)

### ----------------------------------------------------------------------------------------------------------------------------

### 7°) Map EcoTaxa projects
library("xlsx")
file <- read.xlsx("Atlantic_projects_EcoTaxa_summary_19_10_21_FB.xlsx", sheetIndex = 1)
dim(file) # 993     30
str(file)
unique(file$license)
# Subset those CC BY datasets
sub <- file[file$license %in% c("CC BY 4.0","CC BY-NC 4.0","CC0 1.0"),c(1:15)]
dim(sub) # 102  30
summary(sub)
# Remove those that have missing coordinates
sub <- sub[sub$lat_min != 0,]


### ----------------------------------------------------------------------------------------------------------------------------

### 08/11/2021: Quick plastipshere data mapping for AtlantECO yearly summary
library("xlsx")
library("tidyverse")
library("RColorBrewer")

world <- map_data("world")

file <- read.xlsx("AtlantECO_Plastisphere_GA_summary.xlsx", sheetIndex = 1)
dim(file) ; str(file) ; summary(file) 

map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 25 & world$long >= -85 & world$lat >= -70 & world$lat <= 85,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = Longitude, y = Latitude, fill = factor(Label)), data = file, colour = "black", pch = 21) +
    scale_fill_manual(name = "Source", values = c("#1f78b4","#e31a1c","#33a02c")) + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

ggsave(plot = map, filename = "map_plastisphere_data_Linda_08_11_21.jpg", dpi = 300, height = 10, width = 8)



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
