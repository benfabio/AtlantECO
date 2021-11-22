
##### ATLANTECO SCRIPT 2.0 ----------------------------------------------------------------------------------------------------------------------------
##### 24/06/2021: R Script to aggregate and map the sampling effort of the various datasets synthetized within Work Package (WP) 2 of AtlantECO.  
##### © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### For dataset currently available in WP2:
#	- Load the data as data.frame, examine str()/summary() of the data.frame, make sure everything is in the right format (numerics, factors, date etc.)
#   - Aggregate N observations per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
#   - Compute N points (effort) per 1°x1° cell grid, map. Same with sampling effort in time with Hovmoller plot (Month ~ Year). 
#   - For some datasets, one could provide additional statistics: N species/genera/clades recorded 

### Latest update: 28/06/2021

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

### A°) Load the data as data.frame (ggplot2 only works with objects of class 'data.frame')

# Go to the directory containing your data
# setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/PHYTObase")
# Adapt the function used to read the data based on your file extension (read.csv(), read.table() etc.)
d <- get(load("PhytoBase_reformated+WoRMScheck_08_06_2021.Rdata"))
# Check object dimension, structure, 
dim(d)
str(d)
summary(d)
# Check if spatial coordinates follow CRS WGS84
summary(d[,c("decimalLatitude","decimalLongitude")])
# If not, manually rotate your coordinates to match the WGS84

### Aggregate N observations (= proxy of sampling effort) per 1°x1° cell grid (make sure spatial coordinates follow CRS WGS84: -180°/+180° longitude) 
### Otherwise, there are usually too many data for single points/sampling stations
d$x_1d <- round(d$decimalLongitude)
d$y_1d <- round(d$decimalLatitude)
d$cell_id <- factor(paste(d$x_1d, d$y_1d, sep = "_"))
# length(unique(d$cell_id)) # returns the number of unique 1x1 grid cells covered by the dataset at hand 

### Basic for plotting sampling stations only OR records
### !!! Use fill and/or colour arguments in the geom_point below to colour points/stations as a function of the cruise/project etc.
map.occ.only <- ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
    geom_point(aes(x = x_1d, y = y_1d), data = d) + coord_quickmap() +
    scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
    scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
    theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "left") 


### B°) Use dplyr to compute sampling effort per 1°x1° cell
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

# ### And for Latitude x depth plot: round 'Depth' column to the nearest 10m (try 10m depth levels)
# d$Depth_10m <- round(d$Depth, -2)
# # Check if this works
# summary(d$Depth_10m) ; summary(factor(d$Depth_10m))
# # Compute effort
# depth.effort <- data.frame(d %>% group_by(y_1d,Depth_10m) %>% summarize(N = n()))
# dim(depth.effort) ; summary(depth.effort)
# # And use that to plot samplig effort across depths and latitude
# depth.plot <- ggplot() + geom_tile(aes(x = factor(y_1d), y = Depth_10m, fill = log10(N)), data = na.omit(depth.effort)) +
#         scale_fill_viridis(name = "N obs\n(log10) ", option = "B") + xlab("") + ylab("") + theme_bw() +
#         theme(legend.position = "left") + scale_y_reverse() + ylab("Depth (10m bins)") +
#         theme(axis.text.x = element_text(size = 5.5, angle = 90, vjust = 0.5, hjust = 1))

### 25/06/21: LatxDepth plot was not optimal for purpose of report. Go for the following laternative option: 
# - bin records per world ocean atlas (WOA,https://www.nodc.noaa.gov/OC5/woa13/) dstandard depth levels 
# - plot records density per: lon, lat, month, year, depth
# - assemble abovementioned plots in a 5 figure panel and try to combine with Hovmoller (Month~Year)

### Bin per depth ranges of increasing width: 10m width in the first 
summary(d$Depth) # max is ~4600m
d$DepthLevels <- NA
# Use cut() and seq() to derive bins from 'Depth'
# From 0 to 500 -> by 10; From 500 to 1000: by 100; From 1000 to 9000: by 10000

sub <- d %>% drop_na(Depth) # dim(sub) --> 1275842 rows
sub2 <- d[is.na(d$Depth),]  # dim(sub2) --> 56900 rows
sub2$DepthLevels <- NA
# First cut: every 10m to 500
sub$DepthLevels <- as.character(base::cut(x = sub$Depth, breaks = seq(from = min(sub$Depth), to = max(sub$Depth), by = 10)))
unique(sub$DepthLevels) ; str(sub$DepthLevels)
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
levels(sub$DepthLevels)
d <- rbind(sub,sub2)
summary(d$DepthLevels)
str(d$DepthLevels)
# OK, will have to re-order this 
depth.levels <- c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100",
                "100-110","110-120","120-130","130-140","140-150","150-160","160-170","170-180","180-190","190-200",
                "200-210","210-220","220-230","230-240","240-250","250-260","260-270","270-280","280-290","290-300",
                "300-310","310-320","320-330","330-340","340-350","350-360","360-370","370-380","380-390","390-400",
                "400-410","410-420","420-430","430-440","440-450","450-460","460-470","470-480","480-490","490-500",
                "500-600","600-700","700-800","800-900","900-1000","1000-2000","2000-3000","3000-4000","4000-5000",">5000")

### Now, Plot density of records per LON/LAT/DEPTH/MONTH/YEAR
# ggplot(d, aes(x = decimalLatitude)) + geom_density(fill = "gray") +
#     geom_vline(aes(xintercept = mean(decimalLatitude)), color = "#d53e4f", linetype = "dashed") +
#     labs(title = "Records density across latitude", x = "Latitude", y = "Density") + theme_classic()

p1 <- ggplot(d, aes(x = decimalLatitude)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(decimalLatitude)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Latitude (1° bins)", y = "N records") + theme_classic()

# Re-order for DepthLevels
d$DepthLevels <- factor(d$DepthLevels, levels = depth.levels)

p2 <- ggplot(d, aes(x = DepthLevels)) + geom_histogram(fill = "black", stat = "count") +
    geom_vline(aes(xintercept = median(Depth, na.rm = T)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Depth (m)", y = "N records") + theme_classic() +
    theme(axis.text.x = element_text(size = 4.5, angle = 90, vjust = 0.5, hjust = 1))

p3 <- ggplot(d, aes(x = Month)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Month)), color = "#d53e4f", linetype = "dashed") +
    scale_x_continuous(breaks = c(1:12), labels = c(1:12)) + 
    labs(x = "Month", y = "N records") + theme_classic()

p4 <- ggplot(d, aes(x = Year)) + geom_histogram(fill = "black", binwidth = 1) +
    geom_vline(aes(xintercept = median(Year)), color = "#d53e4f", linetype = "dashed") +
    labs(x = "Year", y = "N records") + theme_classic()

### Arrange in panel based on various lay outs
# One can use: 
library("ggpubr") # ?ggarrange --> Not optimal for uneven plots, very good when all plots are simialr and even numbers
#library("grid") ; library("gridExtra")
# grid.arrange(map, hov.plot, arrangeGrob(p1, p2, p3, p4, ncol = 2, nrow = 2), ncol=2, widths=c(1,1.2))

### And try the brand new 'patchwork' package
# library("devtools")
# devtools::install_github("thomasp85/patchwork")
library("patchwork") # https://patchwork.data-imaginist.com/
# ?patchwork
#p1 + (p2 / p3 / p4) ; p1 + p2 + p3 + p4

### Use plot_layout() to make sure plots are well arranged (relative displays)
# ?plot_layout
# map / hov.plot / (p1 + p2) / (p3 + p4) + plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3.5,2.5,1,1) )
patch <- map / hov.plot / (p1 + p2) / (p3 + p4) +
        plot_layout(nrow = 4, widths = c(3,3,1,1), heights = c(3,2.25,2,2) ) +
        plot_annotation(title = 'PhytoBase (Righetti et al., 2020)', 
            subtitle = 'Phytoplankton species occurrences')

### Save
ggsave(plot = patch, filename = "test_patchwork.jpg", dpi = 300, height = 14, width = 11)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
