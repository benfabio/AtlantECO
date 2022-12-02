
##### ATLANTECO SCRIPT 2.1 ----------------------------------------------------------------------------------------------------------------------------
##### 28/06/2021: R Script to aggregate and map the sampling effort of the various datasets synthetized within Work Package (WP) 2 of AtlantECO.  
##### © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### For dataset currently available in WP2:
#	- Load the data as data.frame, examine str()/summary() of the data.frame, make sure everything is in the right format (numerics, factors, date etc.)
#   - Aggregate N observations per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
#   - Compute N points (effort) per 1°x1° cell grid, map. Same with sampling effort in time with Hovmoller plot (Month ~ Year). 
#   - For some datasets, one could provide additional statistics: N species/genera/clades recorded 

### Latest update: 01/07/2021

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("geosphere")
library("parallel")
library("viridis")

# Load coastline for mapping
world <- map_data("world")

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) PhytoBase - Global open ocean phytoplankton species occurrences

### Go to the directory containing your data
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/PHYTObase")
d <- get(load("PhytoBase_reformated+WoRMScheck_08_06_2021.Rdata"))
dim(d) ; str(d)
summary(d)
# Check if spatial coordinates follow CRS WGS84
summary(d[,c("decimalLatitude","decimalLongitude")])
# If not, manually rotate your coordinates to match the WGS84
d <- d[d$Year > 1800,]

### Aggregate N observations (= proxy of sampling effort) per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
# dim(spatial.effort) ; summary(spatial.effort)
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
# dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
d$DepthLevels <- NA
sub <- d %>% drop_na(Depth) # dim(sub) --> 1275842 rows
sub2 <- d[is.na(d$Depth),]  # dim(sub2) --> 56900 rows
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$Depth, breaks = seq(from = min(sub$Depth), to = max(sub$Depth), by = 10)))
# unique(sub$DepthLevels) ; str(sub$DepthLevels)
# Then bin manually? 
sub[sub$Depth >= 500 & sub$Depth < 600,"DepthLevels"] <- "(500,600]"
sub[sub$Depth >= 600 & sub$Depth < 700,"DepthLevels"] <- "(600,700]"
sub[sub$Depth >= 700 & sub$Depth < 800,"DepthLevels"] <- "(700,800]"
sub[sub$Depth >= 800 & sub$Depth < 900,"DepthLevels"] <- "(800,900]"
sub[sub$Depth >= 900 & sub$Depth < 1000,"DepthLevels"] <- "(900,1000]"
sub[sub$Depth >= 1000 & sub$Depth < 2000,"DepthLevels"] <- "(1000,2000]"
sub[sub$Depth >= 2000 & sub$Depth < 3000,"DepthLevels"] <- "(2000,3000]"
sub[sub$Depth >= 3000 & sub$Depth < 4000,"DepthLevels"] <- "(3000,4000]"
sub[sub$Depth >= 4000 & sub$Depth < 5000,"DepthLevels"] <- "(4000,5000]"
sub[sub$Depth >= 5000,"DepthLevels"] <- ">5000"
sub[sub$Depth == 0,"DepthLevels"] <- "(0,10]"
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
    geom_vline(aes(xintercept = median(Depth, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
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
            plot_annotation(title = 'PhytoBase (Righetti et al., 2020)', 
            subtitle = 'Phytoplankton species occurrences')
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_PhytoBase_AtlantECO-WP2_report_28_06_21.jpg", dpi = 300, height = 14, width = 11)
 


### ----------------------------------------------------------------------------------------------------------------------------

### 2°) ZooBase - Global zooplankton species occurrences
### Go to the directory containing your data
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/ZOObase/v2_files_21_04_2021")

### First, load both obis and then gbif, add an identifier based on 0.01° grid cell + month_year_species and remove duplicates with x[!duplicated(x)]
obis <- get(load("ZOObase_OBIS_merged_reformated+WoRMScheck_04_06_2021.RData"))
gbif <- get(load("ZOObase_GBIF_merged_reformated+WoRMScheck_04_06_2021.RData"))
cornils <- get(load("ZOObase_Cornils&al._2018_SO_Copepods_reformated+WoRMScheck_04_06_2021.RData"))
dim(obis) ; dim(gbif) ; dim(cornils)
### All have same dimensions, good.

### Add 0.01°x0.01° cell id
# head(round(obis$decimalLatitude,1)) ; head( round(obis$decimalLatitude,2) )
obis$x <- round(obis$decimalLongitude,2) ; obis$y <- round(obis$decimalLatitude,2)
gbif$x <- round(gbif$decimalLongitude,2) ; gbif$y <- round(gbif$decimalLatitude,2)
cornils$x <- round(cornils$decimalLongitude,2) ; cornils$y <- round(cornils$decimalLatitude,2)
# Check coords
summary(obis[,c("x","y")])
summary(gbif[,c("x","y")])
gbif <- gbif %>% drop_na(x)
summary(cornils[,c("x","y")])

# Add id
obis$id <- factor(paste(obis$x, obis$y, sep = "_"))
gbif$id <- factor(paste(gbif$x, gbif$y, sep = "_"))
cornils$id <- factor(paste(cornils$x, cornils$y, sep = "_"))
# Need to rbind - check colnames and choose proper colnames
names <- colnames(obis)[c(10:74)]
# Rbind
d <- rbind(obis[,names], gbif[,names], cornils[,names])
dim(d) # 3'472'405
# str(d) ; summary(d)
summary(d[,c("Year","decimalLatitude","decimalLongitude")])

### Add ID based on 0.01° grid cell + month_year_species and remove duplicates with x[!duplicated(x)]
# unique(d$WoRMS_ID)
d$ID.duplicates <- paste(d$id,d$Month,d$Year,d$WoRMS_ID,sep = "_")
length(unique(d$ID.duplicates)) # 1'208'082

d2 <- d[!duplicated(d$ID.duplicates),]
dim(d2) # 1208082
str(d2) ; summary(d2)
# unique(d2$SourceArchive)

### Aggregate N observations (= proxy of sampling effort) per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
d2$x_1d <- round(d2$decimalLongitude)
d2$y_1d <- round(d2$decimalLatitude)
d2$cell_id <- factor(paste(d2$x_1d, d2$y_1d, sep = "_"))
# length(unique(d2$cell_id))

### !!! SEE COMMENTED LINES BELOW: ISSUE WITH COORDINATES WITH GBIF'S HYDROZOA
d2 <- d2[-which(d2$Class == "Hydrozoa" & d2$SourceArchive == "GBIF"),]
dim(d2)

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(d2 %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)
# Map sampling effort
map <- ggplot() + geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + 
        geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
            fill = "grey85", colour = "grey50", size = 0.3) +
        scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
        coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
            labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
        scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
            labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
            panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 

### Weird diagonal pattern across the globe...identify the dataset causing that
#dim(d2[which(d2$decimalLongitude < 2 & d2$decimalLongitude > -2 & d2$decimalLatitude > -2 & d2$decimalLatitude < 2),])
# dim(d2[which(d2$decimalLongitude < 5 & d2$decimalLongitude > -5 & d2$decimalLatitude > -5 & d2$decimalLatitude < 5),])
# #head(d2[which(d2$decimalLongitude < 2 & d2$decimalLongitude > -2 & d2$decimalLatitude > -2 & d2$decimalLatitude < 2),])
# #unique(d2[which(d2$decimalLongitude < 2 & d2$decimalLongitude > -2 & d2$decimalLatitude > -2 & d2$decimalLatitude < 2),"SourceArchive"])
# #unique(d2[which(d2$decimalLongitude < 2 & d2$decimalLongitude > -2 & d2$decimalLatitude > -2 & d2$decimalLatitude < 2),"OrigCollectionCode"])
# unique(d2[which(d2$decimalLongitude < 5 & d2$decimalLongitude > -5 & d2$decimalLatitude > -5 & d2$decimalLatitude < 5),"SourceArchive"])
# unique(d2[which(d2$decimalLongitude < 5 & d2$decimalLongitude > -5 & d2$decimalLatitude > -5 & d2$decimalLatitude < 5),"OrigCollectionCode"])
# unique(d2[which(d2$decimalLongitude < 5 & d2$decimalLongitude > -5 & d2$decimalLatitude > -5 & d2$decimalLatitude < 5),"InstitutionCode"])
#
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = Depth),
#             data = d2[which(d2$x_1d < 5 & d2$x_1d > -5 & d2$y_1d > -5 & d2$y_1d < 5),], alpha = 0.5) +
#         geom_polygon(aes(x = long, y = lat, group = group),
#             data = world[which(world$long < 10 & world$long > -10 & world$lat > -10 & world$lat < 10),],
#             fill = "grey85", colour = "grey50", size = 0.3) +
#         coord_quickmap() + theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")
# ### Issue seems related to:
# # "Invertebrate Zoology"
# dim(d2[d2$OrigCollectionCode == "Invertebrate Zoology",]) # not the problem global
# # "Marine Invertebrates"
# dim(d2[d2$OrigCollectionCode == "Marine Invertebrates",]) # problem in the Atlantic
# # "ZOO"
# dim(d2[d2$OrigCollectionCode == "ZOO",]) # issue but not all points seem to be wrong, like the two datasets above
# # "Diveboard"
# dim(d2[d2$OrigCollectionCode == "Diveboard",]) # most are good
# ### And when looking at "InstitutionCode" header, issue seems related to:
# # "USNM" # most records seem OK...
# # "TCWC" # many records are actually ok
# # "NHMUK" # same
# ### --> so not an isseu related to OrigCollectionCode or InstitutionCode - probably related to GBIF data strcuture
#
# ### Is problem related to a particular zooplankton group?
# unique(d2[which(d2$decimalLongitude < 5 & d2$decimalLongitude > -5 & d2$decimalLatitude > -5 & d2$decimalLatitude < 5),"Class"])
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude, colour = factor(Class)),
#             data = d2[which(d2$x_1d < 5 & d2$x_1d > -5 & d2$y_1d > -5 & d2$y_1d < 5),], alpha = 0.5) +
#         geom_polygon(aes(x = long, y = lat, group = group),
#             data = world[which(world$long < 10 & world$long > -10 & world$lat > -10 & world$lat < 10),],
#             fill = "grey85", colour = "grey50", size = 0.3) +
#         coord_quickmap() + theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")
# # Map them
# ggplot() + geom_point(aes(x = decimalLongitude, y = decimalLatitude),
#             data = d2[d2$Class == "Hydrozoa" & d2$SourceArchive == "GBIF",] ) +
#         geom_polygon(aes(x = long, y = lat, group = group), data = world[which(world$long < 180),],
#             fill = "grey85", colour = "grey50", size = 0.3) +
#         coord_quickmap() + theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")

### !!! REMOVE ALL HYDROZOA FROM GBIF 


### Now, plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
hovmuller <- data.frame(d %>% group_by(Year,Month) %>% summarize(N = n()))
dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
d2$DepthLevels <- NA
# First cut: every 10m to 500
d2$DepthLevels <- as.character(base::cut(x = d2$Depth, breaks = seq(from = min(d2$Depth), to = max(d2$Depth), by = 10)))
# unique(d2$DepthLevels) ; str(d2$DepthLevels)
# Then bin manually? 
d2[d2$Depth >= 500 & d2$Depth < 600,"DepthLevels"] <- "(500,600]"
d2[d2$Depth >= 600 & d2$Depth < 700,"DepthLevels"] <- "(600,700]"
d2[d2$Depth >= 700 & d2$Depth < 800,"DepthLevels"] <- "(700,800]"
d2[d2$Depth >= 800 & d2$Depth < 900,"DepthLevels"] <- "(800,900]"
d2[d2$Depth >= 900 & d2$Depth < 1000,"DepthLevels"] <- "(900,1000]"
d2[d2$Depth >= 1000 & d2$Depth < 2000,"DepthLevels"] <- "(1000,2000]"
d2[d2$Depth >= 2000 & d2$Depth < 3000,"DepthLevels"] <- "(2000,3000]"
d2[d2$Depth >= 3000 & d2$Depth < 4000,"DepthLevels"] <- "(3000,4000]"
d2[d2$Depth >= 4000 & d2$Depth < 5000,"DepthLevels"] <- "(4000,5000]"
d2[d2$Depth >= 5000,"DepthLevels"] <- ">5000"
d2[d2$Depth == 0,"DepthLevels"] <- "(0,10]"
### And finally, remove brackets and comas from the levels 
d2$DepthLevels <- str_replace_all(string = d2$DepthLevels, pattern = "\\(", replacement = "")
d2$DepthLevels <- str_replace_all(string = d2$DepthLevels, pattern = "]", replacement = "")
d2$DepthLevels <- str_replace_all(string = d2$DepthLevels, pattern = ",", replacement = "-")
d2$DepthLevels <- factor(d2$DepthLevels)
# levels(d2$DepthLevels)
# OK, will have to re-order this 
depth.levels <- c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100",
                "100-110","110-120","120-130","130-140","140-150","150-160","160-170","170-180","180-190","190-200",
                "200-210","210-220","220-230","230-240","240-250","250-260","260-270","270-280","280-290","290-300",
                "300-310","310-320","320-330","330-340","340-350","350-360","360-370","370-380","380-390","390-400",
                "400-410","410-420","420-430","430-440","440-450","450-460","460-470","470-480","480-490","490-500",
                "500-600","600-700","700-800","800-900","900-1000","1000-2000","2000-3000","3000-4000","4000-5000",">5000")
# Latitude
p1 <- ggplot(d2, aes(x = decimalLatitude)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(decimalLatitude)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Latitude (1° bins)", y = "N records") + theme_classic()
# Depth - Re-order for DepthLevels first
d2$DepthLevels <- factor(d2$DepthLevels, levels = depth.levels)
p2 <- ggplot(d2, aes(x = DepthLevels)) + geom_histogram(fill = "black", stat = "count") +
    geom_vline(aes(xintercept = median(Depth, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))
# Months
p3 <- ggplot(d2, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()
# Years
p4 <- ggplot(d2, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("ggpubr")
library("patchwork")

patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'ZooBase (Benedetti et al., accepted in Nat. Comms.)', 
            subtitle = 'Zooplankton species occurrences')
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_ZooBase_AtlantECO-WP2_report_28_06_21.jpg", dpi = 300, height = 14, width = 11)



### ----------------------------------------------------------------------------------------------------------------------------

### 3°) Continuous Plankton Recorder (CPR) - plankton abundances in the N Atlantic
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/CPR/dwca-cpr_public-v1.2")
d <- get(load("dwca_CPRv1.2_occ+event_27_04_2021.Rdata"))
dim(d); str(d) # 2'112'490
summary(d)
# summary(factor(d$occurrenceStatus)) # only presences
# (370016/ 2112490)*100 --> 17% of data have missing individual count

### Aggregate N observations (= proxy of sampling effort) per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id))

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
### Need to add Month/Year first 
require("lubridate")
d$Month <- lubridate::month(d$eventDate)
d$Year <- lubridate::year(d$eventDate)
# summary(d[,c("Month","Year")])
hovmuller <- data.frame(d %>% group_by(Year,Month) %>% summarize(N = n()))
# dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

### Bin per depth ranges of increasing width: 10m width in the first 
summary(d[,c("minimumDepthInMeters","maximumDepthInMeters")])
### No need to add depth levels, all CPR is between 5 and 10m depth

# Latitude
p1 <- ggplot(d, aes(x = decimalLatitude)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(decimalLatitude)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Latitude (1° bins)", y = "N records") + theme_classic()
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

patch <- map / hov.plot / p1 / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Continuous Plankton Recorder (CPR)', 
            subtitle = 'Plankton abundances (5-10m depth) - absences (abundance = 0) are not provided yet')
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_CPR_AtlantECO-WP2_report_28_06_21.jpg", dpi = 300, height = 14, width = 11)
 


### ----------------------------------------------------------------------------------------------------------------------------

### 4°) Jellyfish Database Initiative (JeDi) - gelatinous zooplankton occurrences and abundance
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/JeDI_jellyfish_database")
# Load Occ + Abund and rbind
d1 <- get(load("JeDi_Lucas&al._2014_abundances_reformated+WoRMScheck_22_06_2021.Rdata"))
d2 <- get(load("JeDi_Lucas&al._2014_occurrences_reformated+WoRMScheck_18_06_2021.Rdata"))
dim(d1) ; dim(d2) # 63328 (17.5%) ; 298969 ; 362297 in total
d <- rbind(d1,d2)
dim(d) ; str(d)
summary(d)
# Check if spatial coordinates follow CRS WGS84
summary(d[,c("decimalLatitude","decimalLongitude")])

### Aggregate N observations (= proxy of sampling effort) per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id))

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
#colnames(d)
summary(d[,c("Depth","MinDepth","MaxDepth")])
d$DepthLevels <- NA
sub <- d %>% drop_na(MaxDepth) # dim(sub) --> 1275842 rows
sub2 <- d[is.na(d$MaxDepth),]  # dim(sub2) --> 56900 rows
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
    geom_vline(aes(xintercept = median(Depth, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
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
summary(factor(d$TaxonRank))
patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Jellyfish Database Initiative (JeDi; Lucas et al., 2014)', 
            subtitle = 'Gelatinous zooplankton occurrences and abundance (17.5% of the date) - ~60% resolved to species level')
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_JeDi_AtlantECO-WP2_report_28_06_21.jpg", dpi = 300, height = 14, width = 11)
 

### ----------------------------------------------------------------------------------------------------------------------------

### 4°) Jellyfish Database Initiative (JeDi) - gelatinous zooplankton occurrences and abundance
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/COPEPOD-NOAA")
# Load data for all groups together: 3 files
d1 <- get(load("COPEPOD_NOAA_all_zooplankton_modified+WoRMScheck_part1_01_06_2021.RData"))
d2 <- get(load("COPEPOD_NOAA_all_zooplankton_modified+WoRMScheck_part2_01_06_2021.RData"))
d3 <- get(load("COPEPOD_NOAA_all_phytoplankton_microzooplankton_modified+WoRMScheck_01_06_2021.RData"))
dim(d1) ; dim(d2) ; dim(d3)
d <- rbind(d1,d2,d3)
dim(d) ; str(d)
summary(d)
# Check if spatial coordinates follow CRS WGS84
summary(d[,c("decimalLatitude","decimalLongitude")])

### Aggregate N observations (= proxy of sampling effort) per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id))

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
d$DepthLevels <- NA
sub <- d %>% drop_na(MaxDepth) # dim(sub) --> 413960 rows
sub2 <- d[is.na(d$MaxDepth),]  # dim(sub2) --> 4263 rows
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
    geom_vline(aes(xintercept = 100), color = "#d53e4f", linetype = "dashed") +
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
summary(factor(d$TaxonRank)) # 181348/418223
patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'COPEPOD (NOAA)', 
            subtitle = 'Plankton abundances - 43% resolved to species level')
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_COPEPOD-NOAA_AtlantECO-WP2_report_28_06_21.jpg", dpi = 300, height = 14, width = 11)



### ----------------------------------------------------------------------------------------------------------------------------

### 5°) MALASPINA species occurrences. Villarino et al., 2018
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/MALASPINA_Villarino_2017")
dir()[grep("WoRMScheck",dir())]

### Merge all 4 fields with WoRMScheck
d1 <- get(load("MALASPINA_Villarinoetal._2017_coccolithophores_reformated+WoRMScheck_01_06_2021.Rdata"))
d2 <- get(load("MALASPINA_Villarinoetal._2017_diatoms_reformated+WoRMScheck_01_06_2021.Rdata"))
d3 <- get(load("MALASPINA_Villarinoetal._2017_dinoflagellates_reformated+WoRMScheck_01_06_2021.Rdata"))
d4 <- get(load("MALASPINA_Villarinoetal._2017_gelatinous_zooplankton_reformated+WoRMScheck_01_06_2021.Rdata"))
# Check dimensions
dim(d1); dim(d2); dim(d3); dim(d4) # Looks ok...
d <- rbind(d1,d2,d3,d4)
dim(d) ; str(d)
summary(d)
### No need to make depth plot, all samples between 0 and 160m depth (0m only for gelatinous zooplankton)
### No need to draw Hovmoller plot or Year distribution ! Just one cruise after all

### Just plot the MALASPINA stations - use ggrepel to lable stations
# require("ggrepel") # If you want to add stations names
map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = d, colour = "black", pch = 21, fill = "#d53e4f") +
    #geom_text_repel(aes(x = decimalLongitude, y = decimalLatitude, label = EventID), data = d) + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 

### Density distributions of Latitudes and Month sampled
# Latitude
p1 <- ggplot(d, aes(x = decimalLatitude)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(decimalLatitude)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Latitude (1° bins)", y = "N records") + theme_classic()
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
library("patchwork")
patch <- map / (p1 + p3) + plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
    plot_annotation(title = 'MALASPINA expedition (Villarino et al., 2018)',
    subtitle = 'Species occurrences of Diatoms, Dinoflagellates, Coccolithophores and floating gelatinous zooplankton; 0-160m depth - from 2010-12-16 to 2011-07-11')
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_MALASPINA_Villarinoetal._2018_AtlantECO-WP2_report_28_06_21.jpg", dpi = 300, height = 14, width = 11)


### ----------------------------------------------------------------------------------------------------------------------------

### 6°) MALASPINA species occurrences. Villarino et al., 2018
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Plastics")# ; dir()
d <- read.csv("WP2_SmallPlastics_UU_11_06_21.csv", sep = ";", h = T, dec = ",")
str(d) ; dim(d) # 13'159 records
summary(d[,c("MinDepth","MaxDepth")])
summary(d[,c("decimalLongitude","decimalLatitude")])

### !! issue: some longitudes are > 180
d[d$decimalLongitude > 180,] # only 2 records - ok- delete though
d <- d[d$decimalLongitude <= 180,]

### Aggregate N observations (= proxy of sampling effort) per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
length(unique(d$cell_id)) # 3'408

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
patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
            plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
            plot_annotation(title = 'Microplastics data (E. van Sebille & S. Schmiz)', 
            subtitle = 'Microplastics density (#/m2 or #/m3) collected near the surface')
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_microplastics_AtlantECO-WP2_report_29_06_21.jpg", dpi = 300, height = 14, width = 11)


### ----------------------------------------------------------------------------------------------------------------------------

### 7°) Zooplankton Imaging data from Tara Oceans (Zooscan data from Brandao, Benedetti et al., submitted to Sc. Reports)
### Load data from working station - no need to go on kryo. Read the untransformed abundance data: "table_TARA_imaging_Abundance_revisions2_v1_06_04_21.txt"

d <- read.table("table_TARA_imaging_Abundance_revisions2_v1_06_04_21.txt", sep = "\t", h = T)
dim(d); str(d)
colnames(d)
summary(d)
unique(d[,4])
d[d$Depth..nominal == "0-800",]
summary(factor(d$Depth..nominal))

map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = Longitude, y = Latitude), data = d, colour = "black", pch = 21, fill = "#d53e4f") +
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 

### Density distributions of Latitudes and Month sampled
### !!! Event dates NOT provided in Manoela's files: need to read the registry of all of Tara's sampling stations
setwd("/Users/fabiobenedetti/Desktop/")
# dir()
tab <- read.table("TARA_SAMPLES_CONTEXT_ENV-WATERCOLUMN_2.tab", sep = "\t", skip = 2573, h = F) # skip = 2573
dim(tab)
str(tab)
unique(tab$V6)
unique(tab$V9)
unique(tab$V5)
require("lubridate")
# lubridate::month(tab$V9) ; lubridate::day(tab$V9); lubridate::year(tab$V9)
head(tab)
### Convert tab$V6 to integer
head(as.numeric(str_replace_all(tab$V6, "TARA_", "")))
tab$Station <- str_replace_all(tab$V6, "TARA_", "")
tab$Station <- str_replace_all(tab$Station, "a", "")
tab$Station <- str_replace_all(tab$Station, "b", "")
# Replace the unknown by NA
tab[tab$Station == "unknown","Station"] <- NA
tab$Station <- as.numeric(tab$Station)
summary(tab$Station)

### Find common stations
commons <- intersect(d$Station, tab$Station) ; commons # OK
# Provide EventDate (tab$V9) to d based on "Station" 
d$Month <- NA
d$Year <- NA
# c <- 175  ;  tab[tab$Station == c,"V9"]
for(c in commons) {
    
    message(c)
    
    ### Error in the registry: station #86 is '2011-01-09' 
    if(c == 86) {
        d[d$Station == c,"Month"] <- 01
        d[d$Station == c,"Year"] <- 2011
    } else if(c == 111) {
        d[d$Station == c,"Month"] <- 06
        d[d$Station == c,"Year"] <- 2011
    } else if(c == 123) {
        d[d$Station == c,"Month"] <- 08
        d[d$Station == c,"Year"] <- 2011
    } else if(c == 131) {
        d[d$Station == c,"Month"] <- 10
        d[d$Station == c,"Year"] <- 2011
    } else if(c == 149) {
        d[d$Station == c,"Month"] <- 03
        d[d$Station == c,"Year"] <- 2012
    } else if(c == 175) {
        d[d$Station == c,"Month"] <- 07
        d[d$Station == c,"Year"] <- 2012
    } else {
        m <- na.omit(unique(lubridate::month(tab[tab$Station == c,"V9"])))
        y <- na.omit(unique(lubridate::year(tab[tab$Station == c,"V9"])))
        d[d$Station == c,"Month"] <- m
        d[d$Station == c,"Year"] <- y   
    }
     
} # eo for loop c in commons

summary(d$Month)
summary(d$Year)

# Latitude
p1 <- ggplot(d, aes(x = Latitude)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Latitude)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Latitude (1° bins)", y = "N records") + theme_classic()
# Months
p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()

### Arrange the 6 plots in panel 
require("patchwork")
patch <- map / (p1 + p3) + plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
    plot_annotation(title = 'TARA Oceans expedition - Zooplankton imaging (Zooscan) data (Brandao, Benedetti et al., subm. to Scientific Reports)',
    subtitle = 'Organisms concentrations for 36 zooplankton groups and 3 size fractions: >200µm (WP2 net); >300µm (Bongo net); >680µm (Régent net)\n0-100m or 0-500m depth - from 2009-09-07 to 2013-12-06')

### Save
setwd("/Users/fabiobenedetti/Desktop/")
ggsave(plot = patch, filename = "panel_plots_sampling_effort_TARA_imaging_Brandao,Benedetti_et_al._AtlantECO-WP2_report_01_07_21.jpg", dpi = 300, height = 14, width = 11)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
