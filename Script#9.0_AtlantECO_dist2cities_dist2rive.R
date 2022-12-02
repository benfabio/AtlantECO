
##### ATLANTECO SCRIPT 9.0 ----------------------------------------------------------------------------------------------------------------------------
##### 15/03/2022: R Script to compute distance to closest lareg city and distance to closest large river network for AtlantECO WP2 (might be useful for modelling surface plastic concentrations).  
##### ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

### R script below aims to:
#	- Load the datasets required: a) global bathymetry (ETOPOv1) to identify ocean grid cells, b) 'world.cities' dataset from maps R package, and c) major rivers (based on drainage area) dataset derived from Wikipedia list (https://en.wikipedia.org/wiki/List_of_rivers_by_length).
# - Use 'geosphere' and 'marmap' R packages to compute distance (in km) of each ocean grid cells to closest cities (maybe add threshold of pop size?) and closest major river network
# - Explore alternative river dataset from the GRDC (https://www.bafg.de/GRDC/EN/02_srvcs/22_gslrs/221_MRB/riverbasins.html?nn=201570#doc2731742bodyText4) 
# - Apply parallel computing to compute the Haversine distances

### Latest update: 18/03/2022

library("tidyverse")
library("ggrepel")
library("rgeos")
library("rgdal")
library("raster")
library("maps")
library("marmap")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("geosphere")
library("parallel")

# Load coastline for mapping
world <- map_data("world")

### ----------------------------------------------------------------------------------------------------------------------------

### Choose to perform computations on your kryo wd:
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors/dist2coast")
wd <- getwd()

### A) Load bathymetry data from marmap
# ?getNOAA.bathy
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15)
# resolution = 15 mins --> 1/4° resolution
ras.bathy <- as.xyz(bathy)
colnames(ras.bathy) <- c("x","y","z")
# head(ras)
# summary(ras$z)
# Add a boolean vector desribing what is ocean and what is not
ras.bathy$land <- NA
ras.bathy[ras.bathy$z <= -1,"land"] <- FALSE # marine cells basically
ras.bathy[ras.bathy$z > -1,"land"] <- TRUE # cells that are coastline or positive altitude
# Add cell id
ras.bathy$id_cell <- paste(ras.bathy$x, ras.bathy$y, sep = "_")
marine_ids <- unique(ras.bathy[ras.bathy$land == FALSE, "id_cell"])
land_ids <- unique(ras.bathy[ras.bathy$land == TRUE, "id_cell"])
rownames(ras.bathy) <- ras.bathy$id_cell

# Quick maps to check
# autoplot(bathy, geom = c("r","c"), colour="white", size = 0.1) + scale_fill_etopo()
# ggplot() + geom_tile(aes(x = x, y = y, fill = factor(land)), data = ras.bathy) + coord_quickmap()
rm(bathy) ; gc()


### B) Get world cities datasets
# data(world.cities)
# head(world.cities)
# str(world.cities)
# summary(world.cities) # population ranges from 0 to 15'017'783
# unique(world.cities$name) # 41'074 cities...
# Quick distribution map
# world.cities %>% ggplot() + geom_point(aes(x=long, y=lat, size=pop, fill=pop, color=pop), alpha=.1, show.legend = FALSE) + theme_void() + coord_equal() 
# OK, clearly too many cities. Restrict to cities > threshold
# "A principal city of a metropolitan area, with the city having a population greater than or equal to 250,000" (https://nces.ed.gov/programs/edge/docs/locale_classifications.pdf)

# The UNICEF defines metropolitan areas as follows: "A formal local government area comprising the urban area as a whole and its primary commuter areas, typically formed around a city with a large concentration of people (i.e., a population of at least 100,000). In addition to the city proper, a metropolitan area includes both the surrounding territory with urban levels of residential density and some additional lower-density areas that are adjacent to and linked to the city (e.g., through frequent transport, road linkages or commuting facilities)."

# Let's use the UNICEF definition of a "metropolitan area" (not a "city")
# t = 100000
# dim(world.cities[world.cities$pop >= t,]) # 4'251 cities
# world.cities[world.cities$pop >= t,] %>% ggplot() + geom_point(aes(x=long, y=lat, size=pop, fill=pop, color=pop), alpha=.1, show.legend = FALSE) + theme_void() + coord_equal() 
# cities <- world.cities[world.cities$pop >= t,]

# dim(cities) 4251 cities
# summary(cities)
# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
#             fill = "grey85", colour = "black", size = 0.3) +
#         geom_point(aes(x = long, y = lat, size = log10(pop), fill = log10(pop)), data = cities, pch = 21, colour = "black") +
#         scale_fill_viridis(name = "Population (log10)") + coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#         theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#             panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") +
#         guides(size = 'none')


### Alternative updated (better) version from opendatasoft https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/information/?flg=fr&disjunctive.cou_name_en&sort=name
cities <- read.csv("geonames-all-cities-with-a-population-1000.csv", h = T, sep = ";")
# dim(cities)
# str(cities)
### Extract long & lat from 'Coordinates' vector
coords <- as.data.frame(do.call(rbind, str_split(string = cities$Coordinates, pattern = ",")))
colnames(coords) <- c("lat","long")
coords$long <- as.numeric(coords$long)
coords$lat <- as.numeric(coords$lat)
# summary(coords)
cities$decimalLongitude <- coords$long
cities$decimalLatitude <- coords$lat
# summary(cities$Population)
t = 100000
# dim(cities[cities$Population > t,])

# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
#               fill = "grey85", colour = "black", size = 0.3) +
#           geom_point(aes(x = decimalLongitude, y = decimalLatitude, size = log10(Population), fill = log10(Population)),
#               data = cities[cities$Population >= t,], pch = 21, colour = "black") +
#           scale_fill_viridis(name = "Population (log10)") + coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#           theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#               panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") +
#           guides(size = 'none')

### Restrict to metropolitan areas: 
cities <- cities[cities$Population > t,]
# dim(cities) # 4629 cities


### C) Get the river dataset (read from .xlsx sheet) - READ SHEET #2
riv <- read.xlsx("list_rivers_wikipedia_15_03_22.xlsx", sheetIndex = 2)
# dim(riv) # 168  12
# colnames(riv)
# head(riv)
# unique(riv$River_name) 
# str(riv)
# Convert coordinates, drainage and outflow estimates to numerics: 
riv$decimalLongitude <- as.numeric(riv$decimalLongitude)
riv$decimalLatitude <- as.numeric(riv$decimalLatitude)
riv$Drainage_km2 <- as.numeric(riv$Drainage_km2)
riv$Outflow_m3.s <- as.numeric(riv$Outflow_m3.s)
# summary(riv[riv$Sea_connection == T,])

# ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world[world$long <= 180,],
#         fill = "grey85", colour = "black", size = 0.3) +
#     geom_point(aes(x = decimalLongitude, y = decimalLatitude, size = Drainage_km2, fill = Length_km),
#         data = riv[riv$Sea_connection == T,], pch = 21, colour = "black") +
#     scale_fill_viridis(name = "Drainage surface\n(km2)") + coord_quickmap() + ylab("Latitude (°N)") + xlab("Longitude (°W)") +
#     theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
#         panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "right") +
#     geom_text_repel(aes(x = decimalLongitude, y = decimalLatitude, label = River_name), data = riv[riv$Sea_connection == T,])

# OK. But could be too incomplete (not enough rivers)

### Read 'Major rivers' shapefile from the GRDC
# The “Major Rivers” layer contains blue lines of 977 named rivers which represent 520 river basins considered “major” in basin size but also in their hydro-political importance or interest. The rivers lines are extracted from HydroRIVERS dataset (WWF 2019) and attributed with: basin identifier, basin name and name of the river which forms part of the river network. 
# Note on GRDC Major Rivers: The river lines of edition 2007 displayed 687 rivers associated with the river basin polygons of edition 2007. Among other attributes, a long-term average discharge value along the river course is provided, calculated from generated mean discharge 1961-1990 as computed with WaterGAP 2.1 (University of Frankfurt, 2007) at a spatial resolution of 0.5 degree. An update of the calculated discharge using a new version of WaterGAP model is intended but not yet ready to update the river discharge attribute. For the time being, the rivers layer of edition 2007 with the calculated discharge values is still valid and will be provided on request. Please note, that the river lines as of 2007 (delineationh based on Hydro1K dataset) in some cases do not fit with river basin polygons 2020 which are generated using the HydroSHEDS dataset. 
# setwd(paste(wd,"/","mrb_shp_zip", sep = ""))
# # Read the major river shapefile
# # ?readOGR
# shape <- readOGR(dsn = ".", layer = "mrb_rivers")
# class(shape) # class sp
# plot(shape)
# Try to convert to data.frame or a raster
# str(shape)
# str(shape@lines)
# str(shape@data)
# length(unique(shape@data$MRBID)) #  495 MRBID: ID of Major River Basin !
# length(unique(shape@data$RIVERBASIN)) #  508 basins
# length(shape@data$RIVERBASIN)
# length(unique(shape@data$RIVER)) # 959 rivers
# length(shape@data$RIVER) # 983 OK
# length(shape@data$Shape_Leng) # 983 values of Shape_Leng (981 are unique)

### So each Shape_Leng
# length(shape@lines)
# summary((shape@lines[[100]])) # x = long, y = lat

### In a mclapply: extract for each river point the coordinates and the river's name + basin
# i <- 13
# require("parallel")
# res.rivers <- mclapply(c(1:length(shape@lines)), function(i) {
#
#              message(paste(i, sep = ""))
#              name <- shape@data$RIVER[i] # name
#              basin <- shape@data$RIVERBASIN[i]
#              coords <- shape@lines[[i]]@Lines[[1]]@coords
#              MRBID <- shape@data$MRBID[i]
#              ocean <- shape@data$OCEAN[i]
#              cont <- shape@data$CONTINENT[i]
#
#              # Gather in ddf
#              d <- data.frame(River = name, Basin = basin, Continent = cont, Ocean = ocean, long = coords[,1], lat = coords[,2])
#
#              # Return
#              return(d)
#
#          }, mc.cores = 2
#
# ) # eo mclapply
# Rbind
# require("dplyr")
# rivers <- bind_rows(res.rivers)
# summary(rivers)
# dim(rivers) # 643'069
# # Quickmap
# ggplot(data = rivers, aes(x = long, y = lat)) + geom_point(colour = "black", size = 1) + coord_quickmap() + xlab("Longitude") + ylab("Latitude")
# ### OK, good start but needs to be refined: remove points that are too far away from the sea or that have too high altitude
# # Use 'ras' again
# summary(ras)
# # Convert to raster and extarct z values at each location of 'rivers':
#bathy <- rasterFromXYZ(ras.bathy[,c(1:3)])  # Convert first two columns as lon-lat and third as value
#bathy ; plot(bathy)
# Use extract to attribute a z value to rivers
# # ?extract
#rivers$elevation <- raster::extract(x = bathy, y = rivers[,c("long","lat")])
#summary(rivers)
# unique(rivers$River) ; unique(rivers$Basin)
# # Re-plot with only points whose elevation is < 50
# dim(rivers[rivers$elevation < 50,]) # 80'344 points instead of 643'069
# dim(rivers[rivers$elevation < 20,]) # 33447 fewer points
#
# ggplot(data = rivers[rivers$elevation < 50,], aes(x = long, y = lat)) +
#      geom_point(colour = "black", size = 1) + coord_quickmap() +
#      xlab("Longitude") + ylab("Latitude")

# quartz()
# ggplot(data = rivers[rivers$elevation < 20,], aes(x = long, y = lat)) +
#     geom_point(colour = "black", size = 1) + coord_quickmap() +
#     xlab("Longitude") + ylab("Latitude")
#setwd(wd)
#save(x = rivers, file = "table_mrb_rivers_GRDC+ETOPOv1_15_03_22.Rdata")

### Load the dataset you made from the code commented above
rivers <- get(load("table_mrb_rivers_GRDC+ETOPOv1_15_03_22.Rdata"))
summary(rivers) # dim(rivers)

### Remove those river points too far away from the coast based on elevation 
rivers <- rivers[rivers$elevation < 50,]

### ----------------------------------------------------------------------------------------------------------------------------

### Datasets to use: 
# - ras.bathy 
# - cities
# - rivers 
# - riv (not essential?)

### D°) Compute Haversine distances (in km) between ocean grid cells and closest cities and rivers
# For testing functions below:
# i <- "-11.1327310632384_68.2197496522949"

### D.1) Distance to closest city
require("parallel")
distances <- mclapply(marine_ids, function(i) {
					
                    # Get id coords
					message(paste(i, spe = ""))
					xy <- ras.bathy[ras.bathy$id_cell == i,c("x","y")]
					
                    distances <- t(distm(xy, cities[,c("decimalLongitude","decimalLatitude")], fun = distHaversine)/1000) # /1000 to get dist in km
                    
                    # Identify closest city based on index of distances
                    ind <- which(distances %in% min(distances))
					c <- unique(cities[ind,c("Name")])[1]
                    pop <- unique(cities[ind,c("Population")])[1]
                    country <- unique(cities[ind,c("Country.name.EN")])[1]
                    
                    return(data.frame(id = i, long = xy$x, lat = xy$y, dist.km = min(distances), Closest = c, Population = pop, Country = country) )
                    
}, mc.cores = 20 ) # eo mclapply
dist2cities <- bind_rows(distances)
# dim(dist2cities)
# head(dist2cities)
# str(dist2cities)


### Save and make room for next distances
save(dist2cities, file = "dist2cities_15min_17_03_22.Rdata")
write.table(dist2cities, file = "dist2cities_15min_17_03_22.txt", sep = "\t")
rm(distances, dist2cities) ; gc()


### D.2) Distance to closest river network
distances <- mclapply(marine_ids, function(i) {
					
                    # Get id coords
					message(paste(i, spe = ""))
					xy <- ras.bathy[ras.bathy$id_cell == i,c("x","y")]
					
                    distances <- t(distm(xy, rivers[,c("long","lat")], fun = distHaversine)/1000) # /1000 to get dist in km
                    
                    # Identify closest city based on index of distances
                    ind <- which(distances %in% min(distances))
					r <- unique(rivers[ind,c("River")])[1]
                    basin <- unique(rivers[ind,c("Basin")])[1]
                    continent <- unique(rivers[ind,c("Continent")])[1]
					
                    return( data.frame(id = i, long = xy$x, lat = xy$y, distkm = min(distances), River = r, Basin = basin, Continent = continent) )
                    
}, mc.cores = 20 ) # eo mclapply
dist2riv <- bind_rows(distances)
# dim(dist2riv)
# head(dist2riv)

### Save and leave
save(dist2riv, file = "dist2rivers_GRDC_mrb_15min_17_03_22.Rdata")
write.table(dist2riv, file = "dist2rivers_GRDC_mrb_15min_17_03_22.txt", sep = "\t")

rm(distances, dist2riv) ; gc()

### Check results (mapping outputs)
d1 <- get(load("dist2cities_15min_17_03_22.Rdata"))
dim(d1)
str(d1)

ggplot() + geom_tile(aes(x = long, y = lat, fill = log10(Population)), data = d1) + coord_quickmap() +
        geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
        scale_fill_viridis(name = "Population (log10)") + 
        scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
        scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------