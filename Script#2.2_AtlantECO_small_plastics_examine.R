
##### ATLANTECO SCRIPT 2.2 ----------------------------------------------------------------------------------------------------------------------------
##### 31/05/2021: R Script to examine the microplatics count data sent by UU (Erik Van Sebille) on the 31/05/21 © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Load and examine microplastics data: 
# - Map observations in geographical space
# - Map observations in time (hovmoller plot)
# - Show % contribution of various metadata 

# module load R/4.0.3 # To load latest R version on kryo

### Latest update: 31/05/2021

library("raster")
library("rgeos")
library("rgdal")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")

setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Plastics")
world <- map_data("world") # coastline for maps

### ----------------------------------------------------------------------------------------------------------------------------

data <- read.csv("WP2_SmallPlastics_UU_31_05_21.csv", h = T, sep = ";", dec = ",")
dim(data) ; head(data)
str(data)
summary(data)
colnames(data)
unique(data$measurementUnit)

### Note: (543/13159)*100 = 4.13% of observations have missing month and day metadata

# First map
ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = data, colour = "black", size = 1.5, pch = 21, fill = "#d53e4f") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "grey70",linetype = "dashed"), legend.position = "left") 

# Too many data! Need to aggregate n obs per 1°x1° grid cell
#unique( round(data$decimalLongitude) )
#unique( round(data$decimalLatitude) )
data$x_1d <- round(data$decimalLongitude)
data$y_1d <- round(data$decimalLatitude)
data$cell_id <- factor(paste(data$x_1d, data$y_1d, sep = "_"))
length(unique(data$cell_id)) # 3410 unique 1° cells

# Map effort per cell
effort <- data.frame(data %>% group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d), N = n()))
# dim(effort) ; summary(effort)
map <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(N)), data = effort) + scale_fill_viridis(name = "N obs\n(log10)", option = "B") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 

# Nice. Hövmoller plot
hovmuller <- data.frame(data %>% group_by(Year,Month) %>% summarize(Year = unique(Year), Month = unique(Month), N = n()))
# dim(hovmuller) ; summary(hovmuller)
hov.plot <- ggplot() + geom_tile(aes(x = factor(Year), y = factor(Month), fill = log10(N)), data = hovmuller) + 
    scale_fill_viridis(name = "N obs\n(log10) ", option = "B") + 
    xlab("Year") + ylab("Month") + theme_linedraw() + 
    theme(legend.position = "bottom") 

ggsave(plot = hov.plot, filename = "hovmuller_efforts_microplastics.jpg", dpi = 300, width = 12, height = 4)



### Map mean small plastics 
summary(factor(data$measurementUnit))
# 1860 are m/3 (14% of the data)
# Map effort per cell
effort2 <- data.frame(data[data$measurementUnit == "m-2",] %>% 
            group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d),
                mean = mean(measurementValue, na.rm = T), sd = sd(measurementValue, na.rm = T))
) # eo ddf
dim(effort2) ; summary(effort2)

map.mean.abund <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(mean)), data = effort2) +
    scale_fill_viridis(name = "Mean abundance\nlog10(m/2)", option = "B", na.value = "grey25") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 
#
map.sd.abund <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(sd)), data = effort2) +
    scale_fill_viridis(name = "Stdev abundance\nlog10(m/2)", option = "B", na.value = "grey25") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 

# And for m/3
effort3 <- data.frame(data[data$measurementUnit == "m-3",] %>% 
            group_by(cell_id) %>% summarize(x = unique(x_1d), y = unique(y_1d),
                mean = mean(measurementValue, na.rm = T), sd = sd(measurementValue, na.rm = T))
) # eo ddf
dim(effort3) ; summary(effort3)

map.mean.abundv2 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(mean)), data = effort3) +
    scale_fill_viridis(name = "Mean abundance\nlog10(m/3)", option = "B", na.value = "grey25") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 
#
map.sd.abundv2 <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey85", colour = "grey50", size = 0.3) +
    geom_tile(aes(x = x, y = y, fill = log10(sd)), data = effort3) +
    scale_fill_viridis(name = "Stdev abundance\nlog10(m/3)", option = "B", na.value = "grey25") + 
    coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") 

### Arrange in panel
require("ggpubr")
panel <- ggarrange(map.mean.abund, map.sd.abund, map.mean.abundv2, map.sd.abundv2, map, align = "hv", ncol = 2, nrow = 3)
ggsave(plot = panel, filename = "panel_maps_efforts_microplastics.jpg", dpi = 300, width = 8.5, height = 8.5)

### And now plot % contribution of various metadata
colnames(data)
unique(data$Depth)
unique(data$ParentEventID)
unique(data$Note)
unique(data$SamplingProtocol)

# Tally depth factors
tally.depth <- data.frame(data %>% group_by(factor(Depth)) %>% summarize(N = n(), Prop = (N/nrow(data))*100) )
tally.depth[order(tally.depth$Prop, decreasing = T),]
# ggplot(tally.depth[tally.depth$Psp > 1,], aes(x = 2, y = Psp, fill = factor(Family))) +
#  geom_bar(stat = "identity", color = "black") + coord_polar(theta = "y", start = 0) +
#  theme_void() + xlim(0.5,2.5)

# Tally ParentEventID
tally.ID <- data.frame(data %>% group_by(factor(ParentEventID)) %>% summarize(N = n(), Prop = (N/nrow(data))*100) )
tally.ID[order(tally.ID$Prop, decreasing = T),]
colnames(tally.ID) <- c("ID","N","Prop")
p1 <- ggplot(tally.ID[tally.ID$Prop > 1,], aes(x = 2, y = Prop, fill = factor(ID))) +
  geom_bar(stat = "identity", color = "black") + coord_polar(theta = "y", start = 0) +
  theme_void() + xlim(0.5,2.5) + scale_fill_brewer(palette = "Paired", name = "Source\n(%N > 1%)") +
  theme(legend.position = "bottom") 

# Tally Note
tally.notes <- data.frame(data %>% group_by(factor(Note)) %>% summarize(N = n(), Prop = (N/nrow(data))*100) )
tally.notes[order(tally.notes$Prop, decreasing = T),]
colnames(tally.notes) <- c("Note","N","Prop")
p2 <- ggplot(tally.notes[tally.notes$Prop > 1,], aes(x = 2, y = Prop, fill = factor(Note))) +
  geom_bar(stat = "identity", color = "black") + coord_polar(theta = "y", start = 0) +
  theme_void() + xlim(0.5,2.5) + scale_fill_brewer(palette = "Paired", name = "Notes\n(%N > 1%)") + 
  theme(legend.position = "bottom") 

# Tally Protocol
tally.protocol <- data.frame(data %>% group_by(factor(SamplingProtocol)) %>% summarize(N = n(), Prop = (N/nrow(data))*100) )
tally.protocol[order(tally.protocol$Prop, decreasing = T),]
colnames(tally.protocol) <- c("Protocol","N","Prop")
p3 <- ggplot(tally.protocol[tally.protocol$Prop > 1,], aes(x = 2, y = Prop, fill = factor(Protocol))) +
  geom_bar(stat = "identity", color = "black") + coord_polar(theta = "y", start = 0) +
  theme_void() + xlim(0.5,2.5) + scale_fill_brewer(palette = "Paired", name = "Protocol\n(%N > 1%)") + 
  theme(legend.position = "bottom") 

### Arrange in panel & save
panel <- ggarrange(p1,p3,p2, align = "hv", ncol = 1, nrow = 3)
ggsave(plot = panel, filename = "panel_donuts_sources.jpg", dpi = 300, width = 13, height = 10)



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------