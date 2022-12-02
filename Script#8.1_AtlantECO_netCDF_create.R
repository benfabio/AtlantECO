
##### ATLANTECO SCRIPT 8.1 ----------------------------------------------------------------------------------------------------------------------------
##### 18/02/2022: R Script to create a standard netCDF file from an AtlantECO dataset (TARA Oceans Zooscan data) © Fabio Benedetti, ETH Zürich, IBP, UP Group.

#  - Read the excel sheet or the .txt file formatetd to the AtlantECO WP2 format
#  - Use Script 8.0 to put data in a .nc and adapt it to your data
#  - Propose 2 alternative strategies for multi-variable datasets (e.g., distribution data for many taxonomic groups or genes)

### Latest update: 21/02/2022

# install.packages("xlsx")
# library("marmap")
library("tidyverse")
library("reshape2")
library("lubridate")
library("xlsx")
library("ncdf4")
library("raster")

# world <- map_data("world")  # coastline for maps

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Get the demo data from Brandao, Benedetti et al. (2021): zooplankton abundances from vertical net tows samples analyzed with the Zooscan.
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Plankton_Imaging") # dir()
data <- get(load("TARA_Oceans_plankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports_26_07_21.RData"))
# dim(data)
# str(data)
# colnames(data)
### Define the variables that will be used as dimensions of the .nc
#unique(data$eventDate) ; str(data$eventDate)
# - decimalLatitude -> resample on WOA grid (see below)
# - decimalLongitude -> resample on WOA grid 
# - eventDate -> define dimension of dates based on this? Or use 3 different dimensions separately: D/M/Y...
# - MinDepth -> resample on WOA vertical grid 
# - MaxDepth -> resample on WOA vertical grid 
### !!! Note MinDepth and MaxDepth are separated because this specific dataset because the samples come from net tows; for niskin bottle samples, a single 'Depth' value should be found
# - MeshSize 
# - ScientificName (either create a dimension, r create separate separate nc files for each name)
### --> define an array based on those dimensions and the intersection will be the abundance values in #/m3
### 18/02/22: Actually NO: creates overdimensionality and RAM usage ossues in R. 

### Create a vector of 'variables' from 'data' that'll put in the final netCDF
# data$Note ; data$ScientificName # Combine those to create your variable vector

data$variable <- paste(data$ScientificName," (",data$Note,")", sep = "")
variables <- unique(data$variable) # 123 variables

### Get the standard 3D grid (x,y,z) from the WOA (time dimension is dataset-dependent for now)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/netCDF files")
# dir()
# In case you want to remove an older file
if( "TARA_Oceans_mesoozooplankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports.nc" %in% dir() ) {
    file.remove("TARA_Oceans_mesoozooplankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports.nc")    
} # eo if loop

ncin <- nc_open("woa13_all_o_monthly.nc")
# print(ncin) # to check what's inside
# Has lat, lon, depth, time. Extract those
# Longitude dimension
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
# unique(lon)
# Latitude dimension
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
# unique(lat)
# Depth dimensiosn (for MinDepth and MaxDepth)
depth <- ncvar_get(ncin,"depth")
ndepth <- dim(depth)
# cloe netcdf
nc_close(ncin)
gc()

# Dates --> derive from the data
dates <- unique(data$eventDate)[order(unique(data$eventDate))] # Format is: YYYY-MM-DD
ndates <- length(dates)

### !!! NOTE: adding further dimensions like taxonomic groups or size fraction generates RAM allocation issues --> Need to keep dimensionality low (n = 4; x,y,z,t) and then find another way to stack all measurements (by adding variables instead of dimensions for instance)

### We propose 2 alternative strategies: 
### A) Create one .nc file and and fill it with one variable (ncvar_put()), close the .nc file, re-open it in 'write mode' and then add further varianles in it (ncvar_add() in a lapply for instance) --> this will generate one big (potentially huge) .nc file.

### B) Create one .nc file and and fill it with a variable (ncvar_put()), but do so for each variable of interest separately (have more numerous but less heavy .nc files).


### 2°) Use the data above to define the dimensions of the netCDF, create the netCDF file and fill it with all your variable(s) of interest.
### Ok, let's try the 1st strategy: create an empty .nc file 
ncpath <- getwd() # current working directory - feel free to modify this as you will
ncname <- "TARA_Oceans_mesoozooplankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports"  
ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")
dname <- "concentration"  # 'val' 
# Then define the contents of the file:
londim <- ncdim_def("lon", "degrees_east", as.double(lon) ) 
latdim <- ncdim_def("lat", "degrees_north", as.double(lat) ) 
timedim <- ncdim_def("time", "date (YYYY-MM-DD)", as.double(dates) )
depthdim <- ncdim_def("depth", "meters", as.double(depth) )

### Before filling the .nc file with many variables, create an initial one. Let's just take variables[1].
### Like, in Script #8.0, first create an empty array and fill it based on the indices
abund_array <- array(NA, dim = c(nlon, nlat, ndepth, ndates) )

subset <- data[data$variable == variables[1],] # dim(subset)
i2 <- sapply(subset$decimalLongitude, function(x) which.min(abs(lon - x)) ) 
j2 <- sapply(subset$decimalLatitude, function(x) which.min(abs(lat - x)) ) 
k2 <- sapply(subset$MaxDepth, function(x) which.min(abs(depth - x)) ) 
l2 <- sapply(subset$eventDate, function(x)  which(dates == x) ) 

# To check: head(val_array[cbind(j2,k2,l2,m2)])
abund_array[cbind(i2,j2,k2,l2)] <- as.matrix(subset$MeasurementValue) 
# summary(abund_array) # Nice, seems to have worked.

# Define the variable(s)
fillvalue <- NA
varname <- variables[1] # Name of the variable whose values are stored in 'ddf$value'
var_def <- ncvar_def(varname,"#/m3", list(londim,latdim,timedim,depthdim), missval = -999, varname, prec = "float")
# var_def

### Create the .nc file in your directory
ncout <- nc_create(ncfname, list(var_def), force_v4 = T) # add variables in the 'list()'
# OK

# Put variable #1
ncvar_put(ncout,var_def,abund_array)
# Check content
print(ncout)
names(ncout$var) # vector of variables names inside nc
 
### Put additional attributes into dimension and data variables
# ?ncatt_put
ncatt_put(ncout,"lon","axis","decimalLongitude", verbose = T) 
ncatt_put(ncout,"lat","axis","decimalLatitude")
ncatt_put(ncout,"time","axis","Months")
ncatt_put(ncout,"depth","axis","Depth")

# add global attributes
ncatt_put(ncout, varid = 0, attname = "Dataset", attval = "AtlantECO data - WP2 - Imaging data") # if varid==0, then a global attribute is written instead of a variable's attribute
ncatt_put(ncout, varid = 0,"Institution", "ETH Zürich, D-USYS, IBP, UP group")
ncatt_put(ncout, varid = 0,"Description", "Mesozooplankton concentration derived vertical net tow samples from TARA Oceans analysed with the Zooscan imaging system (Gorsky et al., 2010)")
ncatt_put(ncout, varid = 0,"DOI", "doi:10.1038/s41598-021-94615-5")
ncatt_put(ncout, varid = 0, "Funding statement","This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 862923. This output reflects only the author’s view, and the European Union cannot be held responsible for any use that may be made of the information contained therein.")

history <- paste("Fabio Benedetti (fabio.benedetti@usys.ethz.ch); last update: ", date(), sep = ", ")
ncatt_put(ncout, varid = 0, "Last update by:",history)

# Feel free to add as many as you want

### Need to close it before being able to write new var into it (...)
nc_close(ncout) # dir()

### Clear some RAM
rm(abund_array,var_def,varname,subset,i2,j2,k2,l2) ; gc()

### Open it again but in writing mode
ncout <- nc_open("TARA_Oceans_mesoozooplankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports.nc", write = TRUE )
# print(ncout)

### Now, this is where you start adding each variables in this netCDF - try it through a mclapply :-)
# require("parallel")
# v <- variables[13]
lapply(X = variables[2:length(variables)], function(v) {
    
            ### Message
            message(paste("Preparing ",v," to be added into the .nc file", sep = ""))
            
            # Define empty multidem array
            abund_array <- array(NA, dim = c(nlon, nlat, ndepth, ndates) )

            subset <- data[data$variable == variables[1],] # dim(subset)
            i2 <- sapply(subset$decimalLongitude, function(x) which.min(abs(lon - x)) ) 
            j2 <- sapply(subset$decimalLatitude, function(x) which.min(abs(lat - x)) ) 
            k2 <- sapply(subset$MaxDepth, function(x) which.min(abs(depth - x)) ) 
            l2 <- sapply(subset$eventDate, function(x)  which(dates == x) ) 

            # Assign values to empty array
            abund_array[cbind(i2,j2,k2,l2)] <- as.matrix(subset$MeasurementValue)         
            
            ### Define var to be put in .nc
            # ?ncvar_def
            var_def <- ncvar_def(name = v, units = "#/m3", dim = list(londim,latdim,timedim,depthdim), missval = -999, longname = v, prec = "float")

            # Put variable v in netCDF file
            message(paste("Adding ",v," to the .nc file", sep = ""))
            # ?ncvar_add
            ncout <- ncvar_add(nc = ncout, v = var_def)
            ncvar_put(ncout, var_def, abund_array)
    
            # Clear some RAM
            rm(abund_array,var_def,subset,i2,j2,k2,l2) ; gc()
            
        }#, mc.cores = 20
        
) # eo lapply

nc_close(ncout)

### Check
nc.new <- nc_open("TARA_Oceans_mesoozooplankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports.nc")
names(nc.new$var)
print(nc.new)
nc_close(nc.new)


### 21/02/22: Need to think about how to add a land mask into the netCDF. Probably need to add this as a variable in the netcdf.
### Could use either bathymetry (using 'marmap') or distance to coast (which you compyted from bathymetry), or both.

### ----------------------------------------------------------------------------------------------------------------------------


### 21/02/22
### 3°) Test second strategy: create a netcdf file for each variable (at least tghe files will be lighter)
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/netCDF files")
dir()
ncin <- nc_open("woa13_all_o_monthly.nc")
# Has lat, lon, depth, time. Extract those
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
# Latitude dimension
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
# Depth dimensiosn (for MinDepth and MaxDepth)
depth <- ncvar_get(ncin,"depth")
ndepth <- dim(depth)
# close netcdf
nc_close(ncin)
gc()

# Dates --> derive from the data
dates <- unique(data$eventDate)[order(unique(data$eventDate))] # Format is: YYYY-MM-DD
ndates <- length(dates)

### Same as above but in a mclapply lapply
require("parallel")

mclapply(X = variables, function(v) {
    
            ### Message
            message(paste("Preparing ",v," to be added into the .nc file", sep = ""))
            
            ncpath <- getwd() # current working directory - feel free to modify this as you will
            ncname <- paste("TARA_Oceans_mesoozooplankton_zooscan_abundance_Brandao_et_al._2021.Sc.Reports_",v)
            ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")
            dname <- "concentration"  # 'val' 
            # Then define the contents of the file:
            londim <- ncdim_def("lon", "degrees_east", as.double(lon) ) 
            latdim <- ncdim_def("lat", "degrees_north", as.double(lat) ) 
            timedim <- ncdim_def("time", "date (YYYY-MM-DD)", as.double(dates) )
            depthdim <- ncdim_def("depth", "meters", as.double(depth) )

            ### Before filling the .nc file with many variables, create an initial one. Let's just take variables[1].
            ### Like in Script #8.0, first create an empty array and fill it based on the indices
            abund_array <- array(NA, dim = c(nlon, nlat, ndepth, ndates) )

            subset <- data[data$variable == v,] # dim(subset)
            i2 <- sapply(subset$decimalLongitude, function(x) which.min(abs(lon - x)) ) 
            j2 <- sapply(subset$decimalLatitude, function(x) which.min(abs(lat - x)) ) 
            k2 <- sapply(subset$MaxDepth, function(x) which.min(abs(depth - x)) ) 
            l2 <- sapply(subset$eventDate, function(x)  which(dates == x) ) 

            # To check: head(val_array[cbind(j2,k2,l2,m2)])
            abund_array[cbind(i2,j2,k2,l2)] <- as.matrix(subset$MeasurementValue) 
            # summary(abund_array) # Nice, seems to have worked.

            # Define the variable(s)
            fillvalue <- -999
            varname <- variables[1] # Name of the variable whose values are stored in 'ddf$value'
            var_def <- ncvar_def(varname,"#/m3", list(londim,latdim,timedim,depthdim), missval = fillvalue, varname, prec = "float")
            # var_def

            ### Create the .nc file in your directory
            ncout <- nc_create(ncfname, list(var_def), force_v4 = T) # add variables in the 'list()'
            # OK

            # Put variable #1
            ncvar_put(ncout,var_def,abund_array)

            ### Put additional attributes into dimension and data variables
            # ?ncatt_put
            ncatt_put(ncout,"lon","axis","decimalLongitude", verbose = T) 
            ncatt_put(ncout,"lat","axis","decimalLatitude")
            ncatt_put(ncout,"time","axis","Months")
            ncatt_put(ncout,"depth","axis","Depth")

            # Add attributes to the netCDF (title, references, funding statement, contact etc.)
            ncatt_put(ncout, varid = 0, attname = "Dataset", attval = "AtlantECO data - WP2 - Imaging data") # if varid==0, then a global attribute is written instead of a variable's attribute
            ncatt_put(ncout, varid = 0,"Institution", "ETH Zürich, D-USYS, IBP, UP group")
            ncatt_put(ncout, varid = 0,"Description", "Mesozooplankton concentration derived vertical net tow samples from TARA Oceans analysed with the Zooscan imaging system (Gorsky et al., 2010)")
            ncatt_put(ncout, varid = 0,"DOI", "doi:10.1038/s41598-021-94615-5")
            ncatt_put(ncout, varid = 0, "Funding statement","This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 862923. This output reflects only the author’s view, and the European Union cannot be held responsible for any use that may be made of the information contained therein.")

            history <- paste("Fabio Benedetti (fabio.benedetti@usys.ethz.ch); last update: ", date(), sep = ", ")
            ncatt_put(ncout, varid = 0, "Last update by:",history)

            # Feel free to add as many as you want

            ### Need to close it before being able to write new var into it (...)
            nc_close(ncout) # dir()

            ### Clear some RAM
            rm(abund_array,var_def,varname,subset,i2,j2,k2,l2,history) ; gc()
            
        }, mc.cores = 20
        
) # eo lapply



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------