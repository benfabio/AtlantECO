
##### ATLANTECO SCRIPT 1.8 ----------------------------------------------------------------------------------------------------------------------------
##### 30/06/2021: R Script to examine the first AMT datasets sent by Andy Rees on the  © Fabio Benedetti, ETH Zürich, IBP, UP Group.

### Aims to:
#  - Read and examine th content of the various long files shared by Andey Rees on the 29/06/2021

### Latest update: 30/06/2021

library("raster")
library("marmap")
library("tidyverse")
library("reshape2")
library("RColorBrewer")
library("viridis")

world <- map_data("world") 

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Read the data and figure out structure!
setwd("/Users/fabiobenedetti/Desktop/work/PostDocs/ETHZ/AtlantECO/AtlantECO-BASE/data/Traditional/AMT/AMT_Long_Datasets") ; 
dir() 
### List of datasets: 
# - OPC size spectra
# - pigments
# - nutrients
# - CTD profiles
# - FACS?

### A) CTD profiles
# First, examien colnames
d <- read.table("AMT01-29_CTD.dat", sep = "\t")
dim(d) ; str(d) ; head(d)
### We have: Date       Time  Lat     Lon    Depth  Temp    Sal   Oxygen    Fluor
###         yyyy/mm/dd  GMT  [degN]  [degE]   [m]  [degC]  [PSU]   [uM]    [mg/m3] 

d <- read.table("AMT01-29_CTD.dat", sep = "", skip = 2, dec = ".")
dim(d) ; str(d)
# And manually add colnames
colnames(d) <- c("Date","Time_GMT","decimalLatitude","decimalLongitude","Depth","Temp_degC","Salinity","Oxygen_µM","Fluorescence_mg/m3")
summary(d)
min(d$Temp_degC) ; d[d$Temp_degC < 0,"Temp_degC"] ### Temp profiles not clean...
min(d$Salinity) ; d[d$Salinity < 10,"Salinity"] ### Sal profiles not clean...
min(d$Oxygen_µM) #; d[d$Temp_degC < 0,"Oxygen_µM"] ### Temp profiles not clean...
min(d$Fluorescence_mg/m3) ; d[d$Salinity < 10,"Fluorescence_mg/m3"] ### Sal profiles not clean...

d[d$Temp_degC < -2,]
summary(d[d$Temp_degC > -2,]) # still some faulty Sal and Fluo values in there
d[d$Salinity < 1,]
d[d$Oxygen_µM < 1 & d$Oxygen_µM > -999,]

ggplot() + geom_polygon(aes(x = long, y = lat, group = group),
       data = world[world$long <= 29 & world$long >= -80 & world$lat >= -70 & world$lat <= 70,],
       fill = "grey85", colour = "grey50", size = 0.3) +
   geom_point(aes(x = decimalLongitude, y = decimalLatitude), data = d, colour = "#b3cde3", alpha = 0.65) +
   coord_quickmap() + scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
       labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
   scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
       labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
   theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
       panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom")


### -------------------------------------------------------

### B) Nutrients
d <- read.table("AMT01-29_nutrients.dat", sep = "\t")
dim(d) ; str(d) ; head(d)
### We have:   Date     Time   Lat     Lon    Depth   Nitri  Nitra  Phos   Sil
###         yyyy/mm/dd   GMT  [degN]  [degE]   [m]     [uM]  [uM]   [uM]   [uM]
d <- read.table("AMT01-29_nutrients.dat", sep = "", skip = 2, dec = ".")
dim(d) ; str(d)
# And manually add colnames
colnames(d) <- c("Date","Time_GMT","decimalLatitude","decimalLongitude","Depth","Nitrites_µM","Nitrates_µM","Phosphates_µM","Silicates_µM")
summary(d)


### -------------------------------------------------------

### C) Pigments
d <- read.table("AMT01-23_pigments.dat", h = T)
dim(d) ; str(d) ; head(d)
### We have:   Date     Time   Lat     Lon    Depth   [TChla] [dvChla] [TChlb] [TChlc]  [Caro] [Allo]   [But]  [Diad] [Diato]  [Fuco]   [Hex]  [Peri]   [Zea]   [Lut]   [Viol]
###         yyyy/mm/dd   GMT  [degN]  [degE]   [m]    [mg/m3]

d <- read.table("AMT01-23_pigments.dat", skip = 2, dec = ".")
dim(d) ; str(d)
# And manually add colnames
colnames(d) <- c("Date","decimalLatitude","decimalLongitude","Depth",
                "TChla","dvChla","TChlb","TChlc","Caro","Allo","But","Diad","Diato",
                "Fuco","Hex","Peri","Zea","Lut","Viol")
summary(d)
### 15 pigments variables
unique(d$Date)

### -------------------------------------------------------

### D) OPC size spectra
d <- read.table("AMT01-22_OPC_size_spectra.dat", sep = "\t")
dim(d) ; str(d) ; head(d)

### We have: Cruise Year Month Day Lat Lon Slope Intercept R2 RMS_residuals Production(ugC/m3/d) Biomass(ugC/m3) Depth(m) Temp(degC)type 
d <- read.table("AMT01-22_OPC_size_spectra.dat", skip = 1, dec = ".")
dim(d) ; str(d)
head(d)
# And manually add colnames
colnames(d) <- c("Cruise","Year","Month","Day","decimalLatitude","decimalLongitude","Slope","Intercept",
                "R2","RMS_residuals","Production_µgC/m3/d","Biomass_µgC/m3","Depth","Temp_degC","Type","","","")
#
summary(d)

### -------------------------------------------------------

### E) FACS (not .dat file)
d <- read.csv("AMT13-29_FACS.csv", h = T, sep = ",", dec = ".")
dim(d)
summary(d)
str(d)
colnames(d)
unique(d$Type) # ?
unique(d$CTD) # ?
unique(d$Bot.Depth.m.) # ?

colnames(d) <- c("Cruise","Station","Type","Date","decimalLongitude","decimalLatitude",
                "Bathymetry","Depth","CTD","Longitude","Latitude","Synechococcus",
                "Prochlorococcus","Picoeukaryotes","Nanoeukaryotes","Coccolithophores",
                "Cryptophyta","HNABact","LNABact","TotBact")

summary(d)
unique(d$Date)
unique(d$Depth)
unique(d$TotBact_cellsml)

### Why 2 coordinates fields?
ggplot(data = d, aes(x = decimalLongitude, y = Longitude)) + geom_point() + theme_minimal()
# Return records where decimalLongitude != Longitude
d[which(d$decimalLongitude != d$Longitude),]


ggplot(data = d, aes(x = decimalLatitude, y = Latitude)) + geom_point() + theme_minimal()
# Latitudes are OK

### Convert last 9 variables to numeric




### ----------------------------------------------------------------------------------------------------------------------------



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
