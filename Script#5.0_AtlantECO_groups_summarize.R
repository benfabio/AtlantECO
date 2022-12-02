
##### ATLANTECO SCRIPT 5.0 ----------------------------------------------------------------------------------------------------------------------------
##### 30/11/2021: R Script to gather and bind the observations at the Plankton Functional Type (PFT) level or taxonomic group level ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Read and subset the data per PFG/ taxonomic group
# - Examine distribution of data
# - Examine relative contribution of sampling parameters such as MinDepth/MaxDepth, tow type etc.
# - Draw plots of panels and give summarizing statistics for non crustacean zooplankton groups (DRIFT proposal)

### Latest update: 26/01/2022

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("parallel")
library("lubridate")
library("viridis")

world <- map_data("world")  # for maps

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/") # When on kryo
# setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Traditional/") # When on personal station
# dir()
WD <- getwd() # Main dir

### ----------------------------------------------------------------------------------------------------------------------------

### List the groups of interest:
# Diatoms 
# Dinoflagellates --> Noémy
# Coccolithophores --> MAREDAT+AusCPR+CPR
# Copepods --> NMFS-COPEPOD+AusCPR+CPR+SOCPR+Cornils&al. compilation
# Euphausiids
# Thaliacea --> collab with Corentin Clerc
# Cnidaria+Ctenophora
# Pteropods --> Nielja
# Foraminifera --> Nielja
# Chaetognatha
# Amphipods
# Decapods
# Mysiids

### ----------------------------------------------------------------------------------------------------------------------------

### 30/11/2021: Thaliacea (for Corentin Clerc)

### A) NMFS-COPEPOD 
setwd(paste(WD,"/COPEPOD-NOAA/", sep = "")) ; dir()
#setwd(paste(WD,"/NOAA_COPEPOD/", sep = "")) 
cope <- get(load("COPEPOD-NOAA_Urochordata_4350000_reformatted+WoRMScheck_12_11_21.Rdata"))
# Subset Thaliacea
unique(cope$Class) # NA?
unique(cope$Order)
head(cope[is.na(cope$Class),c("ScientificName","OrigScientificName")])
subset1 <- cope[cope$Class == "Thaliacea" & !is.na(cope$Class),]
dim(subset1)
head(subset1)
colnames(subset1)
# Check distribution of Measurement
summary(subset1$MeasurementValue) # 862 NaNs to remove and clear outliers 
summary(factor(subset1$MeasurementUnit)) # #/m3

subset1 <- subset1[!is.na(subset1$MeasurementValue),]

# Check origin of data
subset1 %>% count(basisOfRecord, sort = T)
subset1 %>% count(SamplingProtocol, sort = T) 
subset1 %>% count(InstitutionCode, sort = T) # Map this for comparison to JeDI

m <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(InstitutionCode)),
        data = subset1, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 

ggsave(m, filename = "map_Institutes_NMFS-COPEPOD.png", dpi = 300, width = 10, height = 10)


### B) JeDI
setwd(paste(WD,"/JeDI_jellyfish_database/", sep = "")) ; dir()
jedi <- get(load("JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_03_12_2021.Rdata"))
dim(jedi)
#head(jedi)
# Subset Thaliacea
unique(jedi$Class)
subset2 <- jedi[jedi$Class == "Thaliacea" & !is.na(jedi$Class),]
dim(subset2) # 19169
head(subset2)
colnames(subset2)
summary(subset2$MeasurementValue) # No NaNs
summary(factor(subset2$MeasurementUnit)) # #/m3!

### 03/12/21: Check orig datasets
unique(subset2$ParentEventID)
unique(subset2$InstitutionCode) # NOAA_NMFS here
summary(factor(subset2$ParentEventID))
summary(factor(subset2$InstitutionCode))

# Map obs by changing the points' colour as a fun of ParentEventID & then InstitutionCode
m1 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(InstitutionCode)), data = subset2, alpha = 0.75) +
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") + scale_colour_discrete(name = "JeDI\ndata source") + 
    theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom") 

m2 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(ParentEventID)), data = subset2, alpha = 0.75) +
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") + scale_colour_discrete(name = "JeDI\ndata source") + 
    theme(panel.background = element_rect(fill = "white"), legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white", linetype = "dashed"), legend.position = "bottom") 

ggsave(m1, filename = "map_Institutes_JeDI.png", dpi = 300, width = 12, height = 10)
ggsave(m2, filename = "map_ParentEventID_JeDI.png", dpi = 300, width = 12, height = 10)

# ### !!! Evaluate overlap between JeDI & NMSF-COPEPOD
# unique(subset2$ParentEventID)
# unique(subset2$orig_occurrenceID)
#
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset1, alpha = 0.2, colour = "grey50") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset2, alpha = 0.3, colour = "red") +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")
#

### C) CPR surveys
setwd(paste(WD,"/CPR/dwca-cpr_public-v1.2", sep = "")) ; dir()

### a) North Atlantic + North Pacific CPR
# cpr <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
# unique(cpr$ScientificName)
# unique(cpr$Order)
# ### OK, no Thaliacea actually
# rm(cpr); gc()


### b) SO-CPR
setwd(paste(WD,"/CPR/SO-CPR", sep = "")) ; dir()
socpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
unique(socpr$ScientificName)
unique(socpr$Order)
unique(socpr$Class) # NA?
unique(socpr[is.na(socpr$Class),c("ScientificName","OrigScientificName")])
# OK. Subset 
subset3 <- socpr[socpr$Class == "Thaliacea" & !is.na(socpr$Class),]
dim(subset3) # 362'019
summary(subset3$MeasurementValue) # No NaNs
summary(factor(subset3$MeasurementUnit)) # Only #/m3
rm(socpr); gc()

# Quick map with all datasets
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset1, alpha = 0.2, colour = "grey50") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset2, alpha = 0.3, colour = "red") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset3, alpha = 0.3, colour = "blue") +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")

### c) AuSCPR
setwd(paste(WD,"/CPR/AusCPR", sep = "")) ; dir()
auscpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
# head(auscpr)
unique(auscpr$ScientificName)
unique(auscpr$Class) # NA?
subset4 <- auscpr[auscpr$Class == "Thaliacea" & !is.na(auscpr$Class),]
dim(subset4) # 91728 obs
# unique(subset4$ScientificName)
summary(subset4$MeasurementValue) # No NaNs
summary(factor(subset4$MeasurementUnit)) # Only #/m3
rm(auscpr); gc()

# Quick map with all datasets
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset1, alpha = 0.2, colour = "grey50") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset2, alpha = 0.3, colour = "#e31a1c") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset3, alpha = 0.3, colour = "#1f78b4") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset4, alpha = 0.3, colour = "#33a02c") +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")

### D) KRILLBASE
setwd(paste(WD,"/KRILLBASE/", sep = "")) ; dir()
krill <- get(load("KRILLBASE_Atkinson2017_reformatted+WoRMScheck_21_10_21.Rdata"))
unique(krill$ScientificName)
unique(krill$Class) # NA?
subset5 <- krill[krill$Class == "Thaliacea" & !is.na(krill$Class),]
dim(subset5) # 14543 obs - 5186
# unique(subset4$ScientificName)
summary(subset5$MeasurementValue) # 
subset5 <- subset5[!is.na(subset5$MeasurementValue),]
summary(factor(subset5$MeasurementUnit)) # Only #/m2
rm(krill); gc()

# Quick map with all datasets
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
#         data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset1, alpha = 0.3, colour = "grey50") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset2, alpha = 0.2, colour = "#e31a1c") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset3, alpha = 0.2, colour = "#1f78b4") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset4, alpha = 0.2, colour = "#33a02c") +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = subset5, alpha = 0.2, colour = "#a6cee3") +
#     coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right")


### REMOVE NMFS-COPEPOD from JeDI before rbinding! level = 'NOAA_NMFS' & 'NA'
unique(subset2$InstitutionCode) # NOAA_NMFS here
subset2 <- subset2[-which(subset2$InstitutionCode == "NOAA_NMFS" | is.na(subset2$InstitutionCode)),]
dim(subset2) # 4971/19169 --> 25.9

subset1$Dataset <- "NMFS-COPEPOD"
subset2$Dataset <- "JeDI"
subset3$Dataset <- "SO-CPR"
subset4$Dataset <- "AusCPR"
subset5$Dataset <- "KRILLBASE"
# Choose colnames()
names <- colnames(subset1)[c(7,9:14,18:20,25:70,72)]
# Check dates str are OK
salps.all <- rbind(subset1[,names],subset2[,names],subset3[,names],subset4[,names],subset5[,names])
dim(salps.all) # 491529 (but includes)
head(salps.all)
summary(salps.all)

# NA values in MeasurementValue? 
summary(salps.all$MeasurementValue)
salps.all <- salps.all[!is.na(salps.all$MeasurementValue),]

mean(salps.all$MaxDepth, na.rm = T) ; sd(salps.all$MaxDepth, na.rm = T)
mean(salps.all$Year, na.rm = T) ; sd(salps.all$Year, na.rm = T)

# Without CPR:
mean(salps.all[salps.all$Dataset %in% c("NMFS-COPEPOD","JeDI","KRILLBASE"),"MaxDepth"], na.rm = T) 
sd(salps.all[salps.all$Dataset %in% c("NMFS-COPEPOD","JeDI","KRILLBASE"),"MaxDepth"], na.rm = T)

### Data sets contributions
round(summary(factor(salps.all$Dataset))/nrow(salps.all), 2)

mean(salps.all$MeasurementValue, na.rm = T) ; sd(salps.all$MeasurementValue, na.rm = T)
mean(salps.all[salps.all$Dataset %in% c("NMFS-COPEPOD","JeDI","KRILLBASE"),"MeasurementValue"], na.rm = T) 
sd(salps.all[salps.all$Dataset %in% c("NMFS-COPEPOD","JeDI","KRILLBASE"),"MeasurementValue"], na.rm = T)

### Taxonomic resoution?  
round(summary(factor(salps.all$TaxonRank))/nrow(salps.all), 2)
counts <- data.frame(salps.all %>% group_by(ScientificName) %>% summarize(n = n() , p = n/nrow(salps.all)))
counts[order(counts$n, decreasing = T),]

### 26/01/2022: Save first version for Corentin C.
setwd(WD) ; getwd()
write.table(x = salps.all, file = "table_AtlantECO_data_Thaliacea_abundance_for_C.Clerc_26_01_2022.txt", sep = "\t")

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
test.map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
        data = salps.all, alpha = 0.5, size = 1.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.2) +    
    scale_colour_brewer(name = "Dataset", palette = "Paired") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
ggsave(plot = test.map, filename = "map_Thaliacea_data_AtlantECO_no_overlap.png", dpi = 300, width = 12, height = 10)


### 03/12/21: Map sampling effort 
d <- salps.all 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id)) # 6926 cells

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)
# Map sampling effort
map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 

### Now, plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
hovmuller <- data.frame(d %>% group_by(Year,Month) %>% summarize(N = n()))
dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
# colnames(d)
summary(d[,c("MinDepth","MaxDepth")])
### Convert to 0.075m depth to 0m depth --> to match other plots' scales
d[d$MinDepth <= 0.1 & !is.na(d$MinDepth),"MinDepth"] <- 0
d[d$MaxDepth <= 0.1 & !is.na(d$MaxDepth),"MaxDepth"] <- 0
d$DepthLevels <- NA
sub <- d %>% drop_na(MaxDepth) # dim(sub) --> 11265 rows
sub2 <- d[is.na(d$MaxDepth),]  # dim(sub2) --> 1892 rows
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$MaxDepth, breaks = seq(from = min(sub$MaxDepth), to = max(sub$MaxDepth), by = 10)))
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
summary(d$DepthLevels)
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
    geom_vline(aes(xintercept = 100), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()
# Years
p4 <- ggplot(d, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("patchwork")
str(d) ; unique(d$measurementUnit)

patch <- test.map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Thaliacea data (©Fabio Benedetti)', 
            subtitle = 'Thaliacea abundance data (#/m3 or #/m2 for KRILLBASE) collected for AtlantECO WP2')
### Save
setwd(WD)
ggsave(plot = patch, filename = "panel_plots_sampling_effort_Thaliacea_AtlantECO-WP2_25_01_22.jpg", dpi = 300, height = 14, width = 11)

### And plot contribution of TaxonRank and ScientificNames (donut charts)
# Tally 
dim(d)
tally <- data.frame(d %>% group_by(factor(TaxonRank)) %>% summarize(N = n(), Prop = (N/nrow(d))*100) )
tally[order(tally$Prop, decreasing = T),]
colnames(tally)[1] <- "Rank"
d1 <- ggplot(tally, aes(x = 2, y = Prop, fill = factor(Rank))) + 
    geom_bar(stat = "identity", color = "black") + coord_polar(theta = "y", start = 0) +
    scale_fill_discrete(name = "Rank\n(%N > 1%)") +
    theme_void() + xlim(0.5,2.5) + theme(legend.position = "bottom")

# Tally ScientificNames
tally <- data.frame(d %>% group_by(factor(ScientificName)) %>% summarize(N = n(), Prop = (N/nrow(d))*100) )
tally[order(tally$Prop, decreasing = T),]
colnames(tally)[1] <- "Name"
d2 <- ggplot(tally, aes(x = 2, y = Prop, fill = factor(Name))) + 
    geom_bar(stat = "identity", color = "black") + coord_polar(theta = "y", start = 0) +
    scale_fill_discrete(name = "Taxon name\n(%N > 1%)") +
    theme_void() + xlim(0.5,2.5) + theme(legend.position = "bottom")

require("ggpubr")
patch2 <- ggarrange(d1,d2, ncol = 1, align = "hv")
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch2, filename = "panel_donuts_taxo_coverage_Thaliacea_AtlantECO-WP2_03_12_21.png", dpi = 300, height = 10, width = 10)



### ----------------------------------------------------------------------------------------------------------------------------


### 06/12/21: Copepoda (for J. of Biogeography paper?)

### A) NMFS-COPEPOD 
setwd(paste(WD,"/COPEPOD-NOAA/", sep = "")) ; dir()
cope <- get(load("COPEPOD-NOAA_Copepoda_4212000_reformatted+WoRMScheck_19_11_21.Rdata"))
# Subset Copepoda
unique(cope$Class) # NA?
unique(cope$Order)
head(cope[is.na(cope$Class),c("ScientificName","OrigScientificName")])
unique(cope[is.na(cope$Class),c("ScientificName")])
dim(cope[is.na(cope$Class),])
dim(cope)
# Check distribution of Measurement
summary(cope$MeasurementValue) # 81943 NaNs to remove and clear outliers 
summary(factor(cope$MeasurementUnit)) # #/m3

cope2 <- cope[!is.na(cope$MeasurementValue),]
dim(cope2) # 506952

# Check origin of data
cope2 %>% count(basisOfRecord, sort = T)
cope2 %>% count(SamplingProtocol, sort = T) 
cope2 %>% count(InstitutionCode, sort = T) 
cope2 %>% count(basisOfRecord, sort = T) 
sampels2map <- unique(data.frame(cope2 %>% count(basisOfRecord, sort = T))[1:20,1]) 

ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(InstitutionCode)),
        data = cope2, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 
#
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(basisOfRecord)),
        data = cope2[cope2$basisOfRecord %in% sampels2map,], alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\nbasis of record") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 

### Remove CPR data, basisOfRecord = 
# "Tow type= horizontal; with: Continuous Plankton Recorder (CPR) MBA/SAHFOS"
# "Tow type= horizontal; with: Continuous Plankton Sampler (model not specified)"

cope3 <- cope2[!(cope2$basisOfRecord %in% c("Tow type= horizontal; with: Continuous Plankton Recorder (CPR) MBA/SAHFOS","Tow type= horizontal; with: Continuous Plankton Sampler (model not specified)") ),]
dim(cope3) # 464112

ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(basisOfRecord)),
        data = cope3, alpha = 0.25) + scale_colour_discrete(name = "NMFS-COPEPOD\ndata source") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 


### B) CPR surveys
setwd(paste(WD,"/CPR/dwca-cpr_public-v1.2", sep = "")) ; dir()

### a) North Atlantic + North Pacific CPR
cpr <- get(load("CPR_zooplankton_reformatted+WoRMScheck_29_10_21.Rdata"))
unique(cpr$ScientificName)
unique(cpr$Order)
unique(cpr$Class)
# Subset Hexanauplia
cpr <- cpr[cpr$Class == "Hexanauplia",]
dim(cpr)

### b) SO-CPR
setwd(paste(WD,"/CPR/SO-CPR", sep = "")) ; dir()
socpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
unique(socpr$Order)
unique(socpr$Class) # NA?
unique(socpr[is.na(socpr$Class),c("ScientificName","OrigScientificName")])
# OK. Subset 
socpr2 <- socpr[socpr$Class == "Hexanauplia" & !is.na(socpr$Class),]
dim(socpr2) # 5'947'455
head(socpr2)
unique(socpr2$ScientificName)

summary(socpr2$MeasurementValue) # No NaNs
summary(factor(socpr2$MeasurementUnit)) # Only #/m3
rm(socpr); gc()

# Quick map with all datasets
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope3, alpha = 0.2, colour = "grey50") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cpr, alpha = 0.3, colour = "red") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = socpr2, alpha = 0.3, colour = "blue") +
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### c) AuSCPR
setwd(paste(WD,"/CPR/AusCPR", sep = "")) ; dir()
auscpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
unique(auscpr$ScientificName)
unique(auscpr$Class) # NA?
auscpr2 <- auscpr[auscpr$Class == "Hexanauplia" & !is.na(auscpr$Class),]
dim(auscpr2) # 5'052'096
unique(auscpr2$ScientificName)
summary(auscpr2$MeasurementValue) # No NaNs
summary(factor(auscpr2$MeasurementUnit)) # Only #/m3
rm(auscpr); gc()

# Quick map with all datasets
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope3, alpha = 0.2, colour = "grey50") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cpr, alpha = 0.3, colour = "#e31a1c") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = socpr2, alpha = 0.3, colour = "#1f78b4") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = auscpr2, alpha = 0.3, colour = "#33a02c") +
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 


### 16/12/21: Cornils & al. 2018 - ESSD - PANGAEA submissions
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021/")
cornils <- get(load("ZOObase_Cornils&al._2018_SO_Copepods_reformated+WoRMScheck_04_06_2021.RData"))
dim(cornils) ; head(cornils)
# Check taxonomic content like for the others
unique(cornils$ScientificName)
unique(cornils$Class) # perfect

# Quick map with all datasets
ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cope3, alpha = 0.2, colour = "grey50") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cpr, alpha = 0.3, colour = "#e31a1c") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = socpr2, alpha = 0.3, colour = "#1f78b4") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = auscpr2, alpha = 0.3, colour = "#33a02c") +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = cornils, alpha = 0.3, colour = "#6a3d9a") +
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") 

### Rbind?
cope3$Dataset <- "NMFS-COPEPOD"
cpr$Dataset <- "CPR"
socpr2$Dataset <- "SO-CPR"
auscpr2$Dataset <- "AusCPR"
cornils$Dataset <- "Cornils&al.2018"

# Choose colnames()
names <- colnames(cope3)[c(7,9:14,18:20,25:70,72)]
# Check dates str are OK
cops.all <- rbind(cope3[,names],cpr[,names],socpr2[,names],auscpr2[,names],cornils[,names])
dim(cops.all) # 12'390'410

### Check TaxonRank before saving
cops.all %>% count(TaxonRank, sort = T)

test.map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
        data = cops.all, alpha = 0.5, size = 1.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.2) +    
    scale_colour_brewer(name = "Dataset", palette = "Paired") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")

ggsave(plot = test.map, filename = "map_Hexanauplia_data_AtlantECO_WP2_06_12_21.png", dpi = 300, width = 15, height = 12)


### 03/12/21: Map sampling effort 
d <- cops.all 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id)) # 6926 cells

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort); summary(spatial.effort)
# Map sampling effort
map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) +
    scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 

### Save 
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/")
save(cops.all, file = "AtlantECO_WP2_Hexanauplia_data_COPEPOD+CPRs+Cornils.2018_16_12_21.Rdata")        


### ----------------------------------------------------------------------------------------------------------------------------

### 25/01/2022: Larvacea (for DRIFT proposal)

### A) NMFS-COPEPOD 
setwd(paste(WD,"/COPEPOD-NOAA/", sep = "")) ; dir()
#setwd(paste(WD,"/NOAA_COPEPOD/", sep = "")) 
cope <- get(load("COPEPOD-NOAA_Urochordata_4350000_reformatted+WoRMScheck_12_11_21.Rdata"))
# Subset Thaliacea
unique(cope$Class) # NA?
subset1 <- cope[cope$Class == "Appendicularia" & !is.na(cope$Class),]
dim(subset1)
head(subset1)
colnames(subset1)
# Check distribution of Measurement
summary(subset1$MeasurementValue) # 862 NaNs to remove and clear outliers 
summary(factor(subset1$MeasurementUnit)) # #/m3

subset1 <- subset1[!is.na(subset1$MeasurementValue),]

# Check origin of data
subset1 %>% count(basisOfRecord, sort = T)
subset1 %>% count(SamplingProtocol, sort = T) 
subset1 %>% count(InstitutionCode, sort = T) # Map this for comparison to JeDI


### B) JeDI
setwd(paste(WD,"/JeDI_jellyfish_database/", sep = "")) ; dir()
jedi <- get(load("JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_03_12_2021.Rdata"))
head(jedi)
# Subset Thaliacea
unique(jedi$Class)
subset2 <- jedi[jedi$Class == "Appendicularia" & !is.na(jedi$Class),]
dim(subset2) # 14612
head(subset2)
colnames(subset2)
summary(subset2$MeasurementValue) # No NaNs
summary(factor(subset2$MeasurementUnit)) # #/m3!


### b) SO-CPR
setwd(paste(WD,"/CPR/SO-CPR", sep = "")) ; dir()
socpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
unique(socpr$ScientificName)
unique(socpr$Order)
unique(socpr$Class) # NA?
unique(socpr[is.na(socpr$Class),c("ScientificName","OrigScientificName")])
# OK. Subset 
subset3 <- socpr[socpr$Class == "Appendicularia" & !is.na(socpr$Class),]
dim(subset3) # 155'151
summary(subset3$MeasurementValue) # No NaNs
summary(factor(subset3$MeasurementUnit)) # Only #/m3
rm(socpr); gc()


### c) AuSCPR
setwd(paste(WD,"/CPR/AusCPR", sep = "")) ; dir()
auscpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
unique(auscpr$ScientificName)
unique(auscpr$Class) # NA?
subset4 <- auscpr[auscpr$Class == "Appendicularia" & !is.na(auscpr$Class),]
dim(subset4) # 35280 obs
# unique(subset4$ScientificName)
summary(subset4$MeasurementValue) # No NaNs
summary(factor(subset4$MeasurementUnit)) # Only #/m3
rm(auscpr); gc()

### REMOVE NMFS-COPEPOD from JeDI before rbinding! level = 'NOAA_NMFS' & 'NA'
unique(subset2$InstitutionCode) # NOAA_NMFS here and CPR_Atl
subset2 <- subset2[-which(subset2$InstitutionCode == "NOAA_NMFS" | is.na(subset2$InstitutionCode)),]
subset2 <- subset2[-which(subset2$InstitutionCode == "CPR_Atl" | is.na(subset2$InstitutionCode)),]

dim(subset2) # 2063

subset1$Dataset <- "NMFS-COPEPOD (#/m3)"
subset2$Dataset <- "JeDI (#/m3)"
subset3$Dataset <- "SO-CPR (#/m3)"
subset4$Dataset <- "AusCPR (#/m3)"

# Choose colnames()
names <- colnames(subset1)[c(7,9:14,18:20,25:70,72)]
# Check dates str are OK
append.all <- rbind(subset1[,names],subset2[,names],subset3[,names],subset4[,names])
dim(append.all) #  220227

test.map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
        data = append.all, alpha = 0.5, size = 1.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.2) +    
    scale_colour_brewer(name = "Dataset", palette = "Paired") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
setwd(WD)
ggsave(plot = test.map, filename = "map_Appendicularia_data_AtlantECO_WP2_no_overlap_25_01_22.png", dpi = 300, width = 12, height = 10)

### 03/12/21: Map sampling effort 
d <- append.all 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id)) # 6267 cells

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)

### Now, plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
hovmuller <- data.frame(d %>% group_by(Year,Month) %>% summarize(N = n()))
dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
# colnames(d)
summary(d[,c("MinDepth","MaxDepth")])
### Convert to 0.075m depth to 0m depth --> to match other plots' scales
d[d$MinDepth <= 0.1 & !is.na(d$MinDepth),"MinDepth"] <- 0
d[d$MaxDepth <= 0.1 & !is.na(d$MaxDepth),"MaxDepth"] <- 0
d$DepthLevels <- NA
sub <- d %>% drop_na(MaxDepth) # dim(sub) --> 11265 rows
sub2 <- d[is.na(d$MaxDepth),]  # dim(sub2) --> 1892 rows
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$MaxDepth, breaks = seq(from = min(sub$MaxDepth), to = max(sub$MaxDepth), by = 10)))
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
summary(d$DepthLevels)
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
    geom_vline(aes(xintercept = 100), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()
# Years
p4 <- ggplot(d, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("patchwork")
str(d) ; unique(d$measurementUnit)

patch <- test.map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Appendicularia data (©Fabio Benedetti)', 
            subtitle = 'Appendicularia abundance data (#/m3) collected for AtlantECO WP2')
### Save
setwd(WD)
ggsave(plot = patch, filename = "panel_plots_sampling_effort_Appendicularia_AtlantECO-WP2_25_01_22.jpg", dpi = 300, height = 14, width = 11)

### And plot contribution of TaxonRank and ScientificNames (donut charts)
# Tally 
dim(d)
length( unique(d$Species) )

tally <- data.frame(d %>% group_by(factor(TaxonRank)) %>% summarize(N = n(), Prop = (N/nrow(d))*100) )
tally[order(tally$Prop, decreasing = T),]


### ----------------------------------------------------------------------------------------------------------------------------

### 25/01/2022: Chaetognatha (for DRIFT proposal)

### A) NMFS-COPEPOD 
setwd(paste(WD,"/COPEPOD-NOAA/", sep = "")) ; dir()
#setwd(paste(WD,"/NOAA_COPEPOD/", sep = "")) 
cope <- get(load("COPEPOD-NOAA_Chaetognatha_4320000_reformatted+WoRMScheck_15_11_21.Rdata"))
# Subset ?
unique(cope$Class) 
unique(cope$ScientificName) 
subset1 <- cope
dim(subset1) # 24945 rows
head(subset1)
# Check distribution of Measurement
summary(subset1$MeasurementValue) # 862 NaNs to remove and clear outliers 
summary(factor(subset1$MeasurementUnit)) # #/m3
subset1 <- subset1[!is.na(subset1$MeasurementValue),]
rm(cope) ; gc()


### b) SO-CPR
setwd(paste(WD,"/CPR/SO-CPR", sep = "")) ; dir()
socpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
unique(socpr$ScientificName)
unique(socpr$Order)
unique(socpr$Phylum) 
unique(socpr[is.na(socpr$Phylum),c("ScientificName","OrigScientificName")]) # Ok makes sense
# OK. Subset 
subset3 <- socpr[socpr$Phylum == "Chaetognatha" & !is.na(socpr$Phylum),]
dim(subset3) # 310'302
summary(subset3$MeasurementValue) # No NaNs
summary(factor(subset3$MeasurementUnit)) # Only #/m3
rm(socpr); gc()


### c) AuSCPR
setwd(paste(WD,"/CPR/AusCPR", sep = "")) ; dir()
auscpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
unique(auscpr$ScientificName)
unique(auscpr$Class) # NA?
unique(auscpr$Phylum) 
subset4 <- auscpr[auscpr$Phylum == "Chaetognatha" & !is.na(auscpr$Phylum),]
dim(subset4) # 49'392 obs

# unique(subset4$ScientificName)
summary(subset4$MeasurementValue) # No NaNs
summary(factor(subset4$MeasurementUnit)) # Only #/m3
rm(auscpr); gc()

subset1$Dataset <- "NMFS-COPEPOD (#/m3)"
subset3$Dataset <- "SO-CPR (#/m3)"
subset4$Dataset <- "AusCPR (#/m3)"

# Choose colnames()
names <- colnames(subset1)[c(7,9:14,18:20,25:70,72)]
# Check dates str are OK
chaeto.all <- rbind( subset1[,names],subset3[,names],subset4[,names]) 
dim(chaeto.all) #  382'766

test.map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
        data = chaeto.all, alpha = 0.5, size = 1.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.2) +    
    scale_colour_brewer(name = "Dataset", palette = "Paired") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
setwd(WD)
ggsave(plot = test.map, filename = "map_Chaetognatha_data_AtlantECO_WP2_no_overlap_25_01_22.png", dpi = 300, width = 12, height = 10)

### Map sampling effort 
d <- chaeto.all 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id)) # 3298 cells

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)
# Map sampling effort
map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 

### Now, plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
hovmuller <- data.frame(d %>% group_by(Year,Month) %>% summarize(N = n()))
dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
# colnames(d)
summary(d[,c("MinDepth","MaxDepth")])
### Convert to 0.075m depth to 0m depth --> to match other plots' scales
d[d$MinDepth <= 0.1 & !is.na(d$MinDepth),"MinDepth"] <- 0
d[d$MaxDepth <= 0.1 & !is.na(d$MaxDepth),"MaxDepth"] <- 0
d$DepthLevels <- NA
sub <- d %>% drop_na(MaxDepth) # dim(sub) --> 11265 rows
sub2 <- d[is.na(d$MaxDepth),]  # dim(sub2) --> 1892 rows
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$MaxDepth, breaks = seq(from = min(sub$MaxDepth), to = max(sub$MaxDepth), by = 10)))
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
summary(d$DepthLevels)
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
    geom_vline(aes(xintercept = 100), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()
# Years
p4 <- ggplot(d, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("patchwork")
str(d) ; unique(d$measurementUnit)

patch <- test.map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Chaetognatha data (©Fabio Benedetti)', 
            subtitle = 'Chaetognatha abundance data (#/m3) collected for AtlantECO WP2')
### Save
setwd(WD)
ggsave(plot = patch, filename = "panel_plots_sampling_effort_Chaetognatha_AtlantECO-WP2_25_01_22.jpg", dpi = 300, height = 14, width = 11)

### And plot contribution of TaxonRank and ScientificNames (donut charts)
# Tally 
dim(d)
length( unique(d$Species) )

tally <- data.frame(d %>% group_by(factor(TaxonRank)) %>% summarize(N = n(), Prop = (N/nrow(d))*100) )
tally[order(tally$Prop, decreasing = T),]


### ----------------------------------------------------------------------------------------------------------------------------

### 25/01/2022: Cnidaria+Ctenophora (for DRIFT proposal)

### A) NMFS-COPEPOD 
setwd(paste(WD,"/COPEPOD-NOAA/", sep = "")) ; dir()

cope1 <- get(load("COPEPOD-NOAA_Cnidarian_4030000_reformatted+WoRMScheck_12_11_21.Rdata"))
cope2 <- get(load("COPEPOD-NOAA_Ctenophora_4040000_reformatted+WoRMScheck_15_11_21.Rdata"))
cope <- rbind(cope1, cope2)
dim(cope) ; rm(cope1,cope2) ; gc()

# Subset Thaliacea
unique(cope$Phylum) # NA?
subset1 <- cope[cope$Phylum %in% c("Cnidaria","Ctenophora") & !is.na(cope$Phylum),]
dim(subset1) # 48085
colnames(subset1)
# Check distribution of Measurement
summary(subset1$MeasurementValue) # 862 NaNs to remove and clear outliers 
summary(factor(subset1$MeasurementUnit)) # #/m3

subset1 <- subset1[!is.na(subset1$MeasurementValue),]

# Check origin of data
subset1 %>% count(basisOfRecord, sort = T)
subset1 %>% count(SamplingProtocol, sort = T) 
subset1 %>% count(InstitutionCode, sort = T) # Map this for comparison to JeDI


### B) JeDI
setwd(paste(WD,"/JeDI_jellyfish_database/", sep = "")) ; dir()
jedi <- get(load("JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_03_12_2021.Rdata"))
head(jedi)
# Subset Thaliacea
unique(jedi$Class)
unique(jedi$Phylum)
# Subset
subset2 <- jedi[jedi$Phylum %in% c("Ctenophora","Cnidaria") & !is.na(jedi$Class),]
dim(subset2) # 191957
head(subset2)
colnames(subset2)
summary(subset2$MeasurementValue) # No NaNs
summary(factor(subset2$MeasurementUnit)) # #/m3!
### Remove negative abund (makes no sense!)
subset2 <- subset2[subset2$MeasurementValue >= 0,] # only 1 row though


### b) SO-CPR
setwd(paste(WD,"/CPR/SO-CPR", sep = "")) ; dir()
socpr <- get(load("SO-CPR_reformatted+WoRMScheck_17_10_21.Rdata"))
unique(socpr$ScientificName)
unique(socpr$Class) 
unique(socpr$Phylum) 
# Subset 
subset3 <- socpr[socpr$Phylum %in% c("Cnidaria","Ctenophora") & !is.na(socpr$Phylum),]
dim(subset3) # 362019
summary(subset3$MeasurementValue) # No NaNs
summary(factor(subset3$MeasurementUnit)) # Only #/m3
rm(socpr); gc()
# unique(subset3$ScientificName)


### c) AuSCPR
setwd(paste(WD,"/CPR/AusCPR", sep = "")) ; dir()
auscpr <- get(load("AusCPR_zoo_reformatted+WoRMScheck_13_10_21.Rdata"))
unique(auscpr$ScientificName)
unique(auscpr$Class) 
unique(auscpr$Phylum) 
subset4 <- auscpr[auscpr$Phylum %in% c("Cnidaria","Ctenophora") & !is.na(auscpr$Phylum),]
dim(subset4) # 148176 obs
# unique(subset4$ScientificName)
summary(subset4$MeasurementValue) # No NaNs
summary(factor(subset4$MeasurementUnit)) # Only #/m3
rm(auscpr); gc()

### REMOVE NMFS-COPEPOD from JeDI before rbinding! level = 'NOAA_NMFS' & 'NA'
unique(subset2$InstitutionCode) # NOAA_NMFS here and CPR_Atl
subset2 <- subset2[-which(subset2$InstitutionCode == "NOAA_NMFS" | is.na(subset2$InstitutionCode)),]
subset2 <- subset2[-which(subset2$InstitutionCode == "CPR_Atl" | is.na(subset2$InstitutionCode)),]
dim(subset2) # 168064

subset1$Dataset <- "NMFS-COPEPOD (#/m3)"
subset2$Dataset <- "JeDI (#/m3)"
subset3$Dataset <- "SO-CPR (#/m3)"
subset4$Dataset <- "AusCPR (#/m3)"

# Choose colnames()
names <- colnames(subset1)[c(7,9:14,18:20,25:70,72)]
# Check dates str are OK
jelly.all <- rbind(subset1[,names],subset2[,names],subset3[,names],subset4[,names])
dim(jelly.all) #  721'968

test.map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
        data = append.all, alpha = 0.5, size = 1.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.2) +    
    scale_colour_brewer(name = "Dataset", palette = "Paired") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
setwd(WD)
ggsave(plot = test.map, filename = "map_Jellyfish_data_AtlantECO_WP2_no_overlap_25_01_22.png", dpi = 300, width = 12, height = 10)

### 03/12/21: Map sampling effort 
d <- jelly.all 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id)) # 6267 cells

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)

### Now, plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
hovmuller <- data.frame(d %>% group_by(Year,Month) %>% summarize(N = n()))
dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
# colnames(d)
summary(d[,c("MinDepth","MaxDepth")])
### Convert to 0.075m depth to 0m depth --> to match other plots' scales
d[d$MinDepth <= 0.1 & !is.na(d$MinDepth),"MinDepth"] <- 0
d[d$MaxDepth <= 0.1 & !is.na(d$MaxDepth),"MaxDepth"] <- 0
d$DepthLevels <- NA
sub <- d %>% drop_na(MaxDepth) # dim(sub) --> 11265 rows
sub2 <- d[is.na(d$MaxDepth),]  # dim(sub2) --> 1892 rows
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$MaxDepth, breaks = seq(from = min(sub$MaxDepth), to = max(sub$MaxDepth), by = 10)))
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
summary(d$DepthLevels)
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
    geom_vline(aes(xintercept = 100), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()
# Years
p4 <- ggplot(d, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("patchwork")
str(d) ; unique(d$measurementUnit)

patch <- test.map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Jellyfish data (©Fabio Benedetti)', 
            subtitle = 'Cnidaria+Ctenophora abundance data (#/m3) collected for AtlantECO WP2')
### Save
setwd(WD)
ggsave(plot = patch, filename = "panel_plots_sampling_effort_Jellyfish_AtlantECO-WP2_25_01_22.jpg", dpi = 300, height = 14, width = 11)

### And plot contribution of TaxonRank and ScientificNames (donut charts)
# Tally 
dim(d)
length( unique(d$Species) )

tally <- data.frame(d %>% group_by(factor(TaxonRank)) %>% summarize(N = n(), Prop = (N/nrow(d))*100) )
tally[order(tally$Prop, decreasing = T),]


### ----------------------------------------------------------------------------------------------------------------------------

### 26/01/21: Pteropods and Forams (on Nielja's )

### A) Pteropods
setwd("/net/meso/work/nknecht/Masterarbeit/Data/final_joint_files_pteropods") ; dir()
# pteropod_complete_2022-01-19.csv
d <- read.csv("pteropod_complete_2022-01-19.csv", h = T, sep = ",", dec = ".")
dim(d) # 781'369
colnames(d)
head(d)

### Need to define a 'Dataset' column...which columns can we use?
unique(d$BiblioCitation) # Nice. What is in "" though?
head(d[d$BiblioCitation == "",]) # 1302 obs
unique(d[d$BiblioCitation == "","CitationDOI"])
unique(d[d$BiblioCitation == "","ParentEventID"])
# AMT27. OK.  
d[d$BiblioCitation == "","BiblioCitation"] <- "Peijnenburg et al. (unpublished data) - AMT27"

d$Dataset <- NA
d[d$BiblioCitation == "David Johns Marine Biological Association of the UK (MBA) (2021): Continuous Plankton Recorder data for all coccolithophores, foraminifera and thecosomata - all areas. The Archive for Marine Species and Habitats Data (DASSH). https://doi.org/10.17031/1763","Dataset"] <- "CPR (#/m3)"
d[d$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR (#/m3)"
d[d$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR (#/m3)"
d[d$BiblioCitation == "Burridge et al. (2016)","Dataset"] <- "AMT (#/m3)"
d[d$BiblioCitation == "Peijnenburg et al. (unpublished data) - AMT27","Dataset"] <- "AMT (#/m3)"
d[d$BiblioCitation == "Berdnazcek et al. (2021) - ESSD","Dataset"] <- "MAREDAT (#/m3)"
d[d$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD (#/m3)"
d[d$BiblioCitation == "Unpublished data from R. Schiebel (pers. comm.)","Dataset"] <- "Data from R. Schiebel (#/m3)"
d[d$BiblioCitation == "Brandao,Benedetti et al. (2021) Macroscale patterns of oceanic zooplankton composition and size structure. Scientific Reports. 11, 15714.","Dataset"] <- "Tara Oceans (#/m3)"
# Check
unique(d$Dataset)
summary(factor(d$Dataset))
# Weird points (4 rows) here: d <- d[d$decimalLatitude > 100,]
d <- d[d$decimalLatitude < 100,]

test.map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
        data = d, alpha = 0.5, size = 1.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.2) +    
    scale_colour_brewer(name = "Dataset", palette = "Paired") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/")
ggsave(plot = test.map, filename = "map_Pteropods_data_AtlantECO_WP2_no_overlap_26_01_22.png", dpi = 300, width = 12, height = 10)

d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id)) # 8795 cells

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)

### Now, plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
hovmuller <- data.frame(d %>% group_by(Year,Month) %>% summarize(N = n()))
dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
# colnames(d)
summary(d[,c("MinDepth","MaxDepth")])
### Convert to 0.075m depth to 0m depth --> to match other plots' scales
d[d$MinDepth <= 0.1 & !is.na(d$MinDepth),"MinDepth"] <- 0
d[d$MaxDepth <= 0.1 & !is.na(d$MaxDepth),"MaxDepth"] <- 0
d$DepthLevels <- NA
sub <- d %>% drop_na(MaxDepth) # dim(sub) --> 11265 rows
sub2 <- d[is.na(d$MaxDepth),]  # dim(sub2) --> 1892 rows
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$MaxDepth, breaks = seq(from = min(sub$MaxDepth), to = max(sub$MaxDepth), by = 10)))
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
summary(d$DepthLevels)
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
    geom_vline(aes(xintercept = 100), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()
# Years
p4 <- ggplot(d, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("patchwork")
str(d) ; unique(d$measurementUnit)

patch <- test.map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Pteropods data (©Nielja Knecht)', 
            subtitle = 'Pteropod abundance data (#/m3) collected for AtlantECO WP2')
### Save
ggsave(plot = patch, filename = "panel_plots_sampling_effort_Pteropods_AtlantECO-WP2_26_01_22.jpg", dpi = 300, height = 14, width = 11)

### And plot contribution of TaxonRank and ScientificNames (donut charts)
# Tally 
dim(d)
unique(d$Species)

tally <- data.frame(d %>% group_by(factor(TaxonRank)) %>% summarize(N = n(), Prop = (N/nrow(d))*100) )
tally[order(tally$Prop, decreasing = T),]


### B) Foraminifera
setwd("/net/meso/work/nknecht/Masterarbeit/Data/final_joint_files_forams") ; dir()
# forams_complete_2021-12-09.csv
d <- read.csv("forams_complete_2022-01-26.csv", h = T, sep = ",", dec = ".")
dim(d) # 1'033'991
colnames(d)
head(d)

### Need to define a 'Dataset' column...which columns can we use?
unique(d$BiblioCitation) # Nice. What is in "" though?
head(d[d$BiblioCitation == "",]) # 1302 obs
unique(d[d$BiblioCitation == "","CitationDOI"])
unique(d[d$BiblioCitation == "","ParentEventID"])
# AMT27. OK.  
d[d$BiblioCitation == "","BiblioCitation"] <- "Peijnenburg et al. (unpublished data) - AMT27"

d$Dataset <- NA
d[d$BiblioCitation == "David Johns Marine Biological Association of the UK (MBA) (2021): Continuous Plankton Recorder data for all coccolithophores, foraminifera and thecosomata - all areas. The Archive for Marine Species and Habitats Data (DASSH). https://doi.org/10.17031/1763","Dataset"] <- "CPR (#/m3)"
d[d$BiblioCitation == "Hosie, G. (2021) Southern Ocean Continuous Plankton Recorder Zooplankton Records, V9, AADC","Dataset"] <- "SO-CPR (#/m3)"
d[d$BiblioCitation == "Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure strategy (NCRIS)","Dataset"] <- "AusCPR (#/m3)"
d[d$BiblioCitation == "O’Brien, T.D. (2014). COPEPOD: The Global Plankton Database. An overview of the 2014 database contents, processing methods, and access interface. US Dep. Commerce, NOAA Tech. Memo NMFS-F/ST-38, 28 pp.","Dataset"] <- "NMFS-COPEPOD (#/m3)"
d[d$BiblioCitation == "Jentzen et al. (2018)","Dataset"] <- "Jentzen et al. (2018) (#/m3)"
d[d$BiblioCitation %in% c("Schiebel et al. (1995)","Schiebel (2002)","Schiebel et al. (2001)","Schiebel et al. (2000)","Schiebel et al. (2002)","Schiebel et al. (2004)"),"Dataset"] <- "Schiebel et al. papers (#/m3)"

# Check
unique(d$Dataset)
summary(factor(d$Dataset))

test.map <- ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Dataset)),
        data = d, alpha = 0.5, size = 1.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "black", size = 0.2) +    
    scale_colour_brewer(name = "Dataset", palette = "Paired") + 
    coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_blank(),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")
#
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/")
ggsave(plot = test.map, filename = "map_Foraminifera_data_AtlantECO_WP2_no_overlap_26_01_22.png", dpi = 300, width = 12, height = 10)

d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id)) # 7129 cells

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)

### Now, plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
hovmuller <- data.frame(d %>% group_by(Year,Month) %>% summarize(N = n()))
dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
# colnames(d)
summary(d[,c("MinDepth","MaxDepth")])
### Convert to 0.075m depth to 0m depth --> to match other plots' scales
d[d$MinDepth <= 0.1 & !is.na(d$MinDepth),"MinDepth"] <- 0
d[d$MaxDepth <= 0.1 & !is.na(d$MaxDepth),"MaxDepth"] <- 0
d$DepthLevels <- NA
sub <- d %>% drop_na(MaxDepth) # dim(sub) --> 11265 rows
sub2 <- d[is.na(d$MaxDepth),]  # dim(sub2) --> 1892 rows
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$MaxDepth, breaks = seq(from = min(sub$MaxDepth), to = max(sub$MaxDepth), by = 10)))
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
summary(d$DepthLevels)
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
    geom_vline(aes(xintercept = 100), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()
# Years
p4 <- ggplot(d, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("patchwork")
str(d) ; unique(d$measurementUnit)

patch <- test.map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Foraminifera data (©Nielja Knecht)', 
            subtitle = 'Foraminifera abundance data (#/m3) collected for AtlantECO WP2')
### Save
ggsave(plot = patch, filename = "panel_plots_sampling_effort_Foraminifera_AtlantECO-WP2_26_01_22.jpg", dpi = 300, height = 14, width = 11)

### And plot contribution of TaxonRank and ScientificNames (donut charts)
# Tally 
dim(d)
unique(d$Species)

tally <- data.frame(d %>% group_by(factor(TaxonRank)) %>% summarize(N = n(), Prop = (N/nrow(d))*100) )
tally[order(tally$Prop, decreasing = T),]


### ----------------------------------------------------------------------------------------------------------------------------

### 03/06/22: 

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA") #; dir()
files <- dir()[grep("WoRMScheck_",dir())]
files <- files[grep("Rdata",files)] 
files

for(f in files) {
    
    d <- get(load(f))
    n <- nrow(d)
    
    message(paste("", , , sep = ""))
    
} # eo for loop


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------