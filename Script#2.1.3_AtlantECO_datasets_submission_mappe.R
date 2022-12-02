
##### ATLANTECO SCRIPT 2.1.3 ----------------------------------------------------------------------------------------------------------------------------
##### 29/10/2021: R Script to map the sampling effort of the AtlantECO-BASE v1 datasets prior to the their submission on Zenodo
##### © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### For the followong datasets:
#	- PhytoBase v2
#   - ZooBase v2
#   - Copepoda abundance/biomass dataset

### Latest update: 05/09/2022

library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("lubridate")
library("ggpubr")
library("patchwork")

# Load coastline for mapping
world <- map_data("world")

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Phytoplankton species occurrences (PhytoBase v2)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence")
dir()

data <- get(load("AtlantECO-BASE-v1_microbiome_traditional_phytoplankton_species_occurrences_20220408.RData"))
dim(data) # 5'167'282
head(data)
str(data)
colnames(data)

### 05/09/2022: MISSING the occurrenceID ! 
data$occurrenceID <- paste(data$decimalLatitude, data$decimalLongitude, data$Day, data$Month, data$Year,
                    data$Depth, data$MinDepth, data$MaxDepth, data$ScientificName, data$MeasurementValue, sep = "_")
# length(unique(data$occurrenceID))
### Also missing columns: ProjectID, ProjectWP and DataSilo (all before 'ContactName')
data <- data %>% add_column(ProjectID = "AtlantECO_H2020_GA#862923", .before = "ContactName")
data <- data %>% add_column(ProjectWP = "WP2", .before = "ContactName")
data <- data %>% add_column(DataSilo = "Traditional_microscopy", .before = "ContactName")

### Round coordinates 
data$x_1d <- round(data$decimalLongitude)
data$y_1d <- round(data$decimalLatitude)
data$cell_id <- factor(paste(data$x_1d, data$y_1d, sep = "_"))
#summary(data[,c("x_1d","y_1d")])

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(data %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
# dim(spatial.effort) ; summary(spatial.effort)

map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 181,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 
        
ggsave(plot = map, filename = "map_sampling_effort_PhytoBasev2_05_09_22.jpg", dpi = 300, height = 7, width = 10)

### Try to make the panel like for AtlantECO's interim report
# Plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
# Find year and month: V2-V3-V4: year, mon, day

hovmuller <- data.frame(data %>% group_by(Year,Month) %>% summarize(N = n()))
# dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller plot
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))


# Range of Depths
# summary(data$Depth); sd(data$Depth, na.rm = T)
# summary(data$MaxDepth); sd(data$MaxDepth, na.rm = T)

data$DepthLevels <- NA
sub <- data %>% drop_na(Depth) # dim(sub) --> 588771 rows
sub2 <- data[is.na(data$Depth),]  # dim(sub2) --> 123 rows only - discard
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
patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
    plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
    plot_annotation(title = "Phytoplankton species occurrence (presence/absence) data", 
        subtitle = 'PhytoBase v2')
        
### Save
ggsave(plot = patch, filename = "panel_plots_sampling_effort_PhytoBasev2_05_09_2022.jpg", dpi = 300, height = 14, width = 11)

# Save because had not savec version with occurrenceID....
data <- select(data, -cell_id)
data <- select(data, -x_1d)
data <- select(data, -y_1d)
data <- select(data, -DepthLevels)
colnames(data)

### Save final file
save(data, "AtlantECO-BASE-v1_microbiome_traditional_phytoplankton_species_occurrences_20220905.RData")
write.csv(data, file = "AtlantECO-BASE-v1_microbiome_traditional_phytoplankton_species_occurrences_20220905.csv", sep = ";")


### ----------------------------------------------------------------------------------------------------------------------------

### 2°) Zooplankton species occurrences (ZooBase v2)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence"); dir()
data <- get(load("AtlantECO-BASE-v1_microbiome_traditional_zooplankton_species_occurrences_20220908.RData"))
dim(data) # 16 670 316
head(data)
str(data)

unique(data$WoRMS_ID)

### MISSING the occurrenceID ! 
# data$occurrenceID <- paste(data$decimalLatitude, data$decimalLongitude, data$Day, data$Month, data$Year,
#                     data$Depth, data$MinDepth, data$MaxDepth, data$ScientificName, data$MeasurementValue, sep = "_")
# length(unique(data$occurrenceID)) # 100%
# data <- data %>% add_column(ProjectID = "AtlantECO_H2020_GA#862923", .before = "ContactName")
# data <- data %>% add_column(ProjectWP = "WP2", .before = "ContactName")
# data <- data %>% add_column(DataSilo = "Traditional_microscopy", .before = "ContactName")

data$x_1d <- round(data$decimalLongitude)
data$y_1d <- round(data$decimalLatitude)
data$cell_id <- factor(paste(data$x_1d, data$y_1d, sep = "_"))

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(data %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)

ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 181,],
         fill = "grey85", colour = "grey50", size = 0.3) +
     geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") +
     coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")
  
### Still too many land points..identify them
# summary(data$Bathymetry)
# nrow(data[data$Bathymetry >= 1000,])
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 181,],
#         fill = "grey85", colour = "grey50", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = data[data$Bathymetry >= 1000,], colour = "red") +
#     coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#         labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#     scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#         labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")
  
### Can't use bathymetry product --> use NOAA's landmask (not precise enough IMO) or a recent SST climatology (1/4°, better resolution) 
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors/"); dir()  
mask <- raster::raster("woa18_decav_t00_04.nc") 
# mask; plot(mask)
data$mask <- raster::extract(x = mask, y = data[,c("decimalLongitude","decimalLatitude")])
summary(data$mask)  # lots of points still...
  
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 181,],
#          fill = "grey85", colour = "grey50", size = 0.3) +
#      geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = data[is.na(data$mask),], colour = "red") +
#      coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#          labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#      scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#          labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#          panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")

### Still a mix of land and ocean points. can't remove those just like that. Combine mask with bathymetry filter
# nrow(data[is.na(data$mask) & data$Bathymetry >= 200,])
# (nrow(data[is.na(data$mask) & data$Bathymetry >= 200,])/nrow(data))*100
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 181,],
#          fill = "grey85", colour = "grey50", size = 0.3) +
#      geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = data[is.na(data$mask) & data$Bathymetry >= 300,], colour = "red") +
#      coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
#          labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
#      scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
#          labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
#      theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#          panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left")
# OK, remove those 7441 occurrences (0.044% of the data)
data2 <- data[-which(is.na(data$mask) & data$Bathymetry >= 200),]
# dim(data2)
### And finally remove those points in the Amazon rain forest & the Arab Peninsula & the USA & Antarctica
data2 <- data2[!(data2$x_1d > -80 & data2$x_1d < -60 & data2$y_1d < 0 & data2$y_1d > -10),]
data2 <- data2[!(data2$x_1d > 47 & data2$x_1d < 57 & data2$y_1d < 25 & data2$y_1d > 17),]
data2 <- data2[!(data2$x_1d > -90 & data2$x_1d < -82 & data2$y_1d < 50 & data2$y_1d > 32),]
data2 <- data2[!(data2$x_1d > -150 & data2$x_1d < -100 & data2$y_1d > -89 & data2$y_1d < -75),]
# dim(data2) # 16662867
# nrow(data) - nrow(data2) # 7449

spatial.effort <- data.frame(data2 %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))

map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 181,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) + scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence")        
ggsave(plot = map, filename = "map_sampling_effort_ZooBasev2_09_09_22.jpg", dpi = 300, height = 7, width = 10)


# Use geom_tile (heatmap) to generate Hovmoller plot
hovmuller <- data.frame(data2 %>% group_by(Year,Month) %>% summarize(N = n()))
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

# Range of Depths
#summary(data$Depth)#; sd(data$Depth, na.rm = T)
#summary(data$MinDepth)
#summary(data$MaxDepth)#; sd(data$MaxDepth, na.rm = T)

data2$DepthLevels <- NA
sub <- data2 %>% drop_na(Depth) # dim(sub) --> 588771 rows
sub2 <- data2[is.na(data2$Depth),]  # dim(sub2) --> 123 rows only - discard
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
patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
    plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
    plot_annotation(title = "Zooplankton species occurrence (presence/absence) data",
    subtitle = 'ZooBase v2')
    
### Save
ggsave(plot = patch, filename = "panel_plots_sampling_effort_ZooBasev2_09_09_2022.jpg", dpi = 300, height = 14, width = 11)

### Save files  
colnames(data2)
data2 <- dplyr::select(data2, -cell_id)
data2 <- dplyr::select(data2, -x_1d)
data2 <- dplyr::select(data2, -y_1d)
data2 <- dplyr::select(data2, -DepthLevels)
data2 <- dplyr::select(data2, -mask)

setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/1_presence-absence")
# dim(data2) # 16 662 867
# unique(data2$WoRMS_ID); length(unique(data2$WoRMS_ID))
save(x = data2, file = "AtlantECO-BASE-v1_microbiome_traditional_zooplankton_species_occurrences_20220909.RData")
write.csv(data2, file = "AtlantECO-BASE-v1_microbiome_traditional_zooplankton_species_occurrences_20220909.csv", sep = ";")


### ----------------------------------------------------------------------------------------------------------------------------

### 14/09/22: Zooplankton abund/biomass datasets prepared by Ruby Bader
setwd("/net/meso/work/rubader/Zooplankton/from_scratch_3/out")
dir()

data <- read_fst("AtlantECO-BASEv1_dataset_Hexanauplia_abundances_05_07_22_biomass_depthlevel_2022-08-31.fst")
dim(data) # 9 088 140
str(data)
colnames(data)
head(data)

unique(data$BiblioCitation)
unique(data$SourceArchive)

### How many accepted names 
unique(data$WoRMS_ID); length(unique(data$WoRMS_ID))
(summary(factor(data$TaxonRank))/nrow(data))*100 

### 05/09/2022: MISSING the occurrenceID ! 
data$occurrenceID <- paste(data$decimalLatitude, data$decimalLongitude, data$Day, data$Month, data$Year,
                    data$Depth, data$MinDepth, data$MaxDepth, data$ScientificName, data$MeasurementValue, sep = "_")
# length(unique(data$occurrenceID))
### Also missing columns: ProjectID, ProjectWP and DataSilo (all before 'ContactName')
data <- data %>% add_column(ProjectID = "AtlantECO_H2020_GA#862923", .before = "ContactName")
data <- data %>% add_column(ProjectWP = "WP2", .before = "ContactName")
data <- data %>% add_column(DataSilo = "Traditional_microscopy", .before = "ContactName")

### Round coordinates 
data$x_1d <- round(data$decimalLongitude)
data$y_1d <- round(data$decimalLatitude)
data$cell_id <- factor(paste(data$x_1d, data$y_1d, sep = "_"))
#summary(data[,c("x_1d","y_1d")])

### Use dplyr to compute sampling effort per 1°x1° cell
spatial.effort <- data.frame(data %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
dim(spatial.effort) ; summary(spatial.effort)

map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 181,],
        fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = na.omit(spatial.effort)) +
    scale_fill_viridis(name = "N records\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 
        
#ggsave(plot = map, filename = "map_sampling_effort_Copepoda_abund_biomass_rawdata_14_09_22.jpg", dpi = 300, height = 7, width = 10)

### Try to make the panel like for AtlantECO's interim report
# Plot sampling effort in time: N obs per MonthxYear (Hovmoller plot) 
# Find year and month: V2-V3-V4: year, mon, day

hovmuller <- data.frame(data %>% group_by(Year,Month) %>% summarize(N = n()))
# dim(hovmuller) ; summary(hovmuller)
# Use geom_tile (heatmap) to generate Hovmoller plot
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = na.omit(hovmuller)) + 
    scale_fill_viridis(name = "N records\n(log10) ", option = "B") + xlab("Year") + ylab("Month") + 
    theme_bw() + theme(legend.position = "left") +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))


# Range of Depths
summary(data$Depth); sd(data$Depth, na.rm = T)
summary(data$MaxDepth); sd(data$MaxDepth, na.rm = T)
summary(data$MeanDepth); sd(data$MeanDepth, na.rm = T)
unique(data$DepthLevel)

data$DepthLevels <- NA
sub <- data %>% drop_na(MaxDepth) # dim(sub) --> 588771 rows
sub2 <- data[is.na(data$MaxDepth),]  # dim(sub2) --> 123 rows only - discard
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
patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
    plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
    plot_annotation(title = "Copepoda abundance (ind.m-3) and biomass concentration (mgC.m-3) data")
        
### Save
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/3_biomass/")
dir()
ggsave(plot = patch, filename = "panel_plots_sampling_effort_Copepoda_abund_biom_14_09_2022.jpg", dpi = 300, height = 14, width = 11)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
