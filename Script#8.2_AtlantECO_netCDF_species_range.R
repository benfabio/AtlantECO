
##### ATLANTECO SCRIPT 8.2 ----------------------------------------------------------------------------------------------------------------------------
##### 13/09/2022: R Script to create a standard netCDF file that will include all the modelled zooplankton (and phytoplankton?) surface monthly species ranges (Min/Max/Mean/Median/Sdtev HSI across SDMs) for AtlantECO-MAPS, based on Script#8.1 © Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load .nc file from the WOA and the monthly modelled mean HSI of a zooplankton species to create a first .nc file per species
# - Iteratively add the other 4 variables (median/min/max/stdev) to the species .nc files
# - Do the same for phytoplankton
# - BONUS: use the group-level ensemble projections of monthly SR (baseline and future) and store that in netCDFs files

### Latest update: 20/09/2022

library("tidyverse")
library("reshape2")
library("lubridate")
library("xlsx")
library("ncdf4")
library("raster")
library("parallel")

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Load one of the WOA's .nc file to get the dimensions needed (3D: lon, lat, months) and upload the monthly modelled ranges of the first zooplankton species (Abyla trigona) to create the first .nc

### Directory with with WOA's nc file
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors"); dir()
ncin <- nc_open("woa13_all_o_monthly.nc")
# print(ncin) # to check what's inside
# Has lat, lon, depth, time.
# Longitude dimension
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
# unique(lon)
# Latitude dimension
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
# unique(lat)
# close netcdf
nc_close(ncin)
gc()
# manually create months dimensions 
months <- c(1:12)
nmonths <- 12

### Directory with the modelled species ranges
setwd("/net/kryo/work/fabioben/GBD/Zooplankton_species_HSI_tables_for_Ian_05_03_21/"); dir()
# t <- get(load("table_HSI_allruns_Zonosagitta_pulchra_GLM_may.Rdata"))
# head(t)
# Ok, start from these outputs. For each species, you want a data.frame containing: cell_id, x, y, species, Month, Min/Max/Mean/Median/Stdev HSI 

### First, get vector of species names
species <- dir()[grep("table_HSI_allruns_",dir())]
# sp <- str_replace_all(species[1], pattern = "table_HSI_allruns_", replacement = "") 
species <- str_replace_all(species, pattern = "table_HSI_allruns_", replacement = "") 
species <- str_replace_all(species, pattern = "_GLM", replacement = "") 
species <- str_replace_all(species, pattern = "_GAM", replacement = "") 
species <- str_replace_all(species, pattern = "_ANN", replacement = "") 
species <- str_replace_all(species, pattern = "_jan.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_feb.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_mar.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_apr.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_may.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_jun.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_jul.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_aug.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_sep.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_oct.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_nov.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_dec.Rdata", replacement = "") 
species <- unique(species) # length(species)

### Add this as an additional dimension of the .nc ? 
#nspecies <- length(species)

# For testing
# sp <- "Gaetanus_tenuispinus"
mclapply(species, function(sp) {
    
        # Useless message
        message(paste(sp, sep = ""))
        files <- dir()[grep(sp,dir())] # files
        # Load and rvind all sp projections
        res <- lapply(files, function(f) { d <- get(load(f)) ; return(d) } ) 
        # Rbind
        t <- dplyr::bind_rows(res)
        # dim(t); str(t)
        rm(res); gc()
        # Rescale to 0-1 (HSI)
        t[,c(1:10)] <- (t[,c(1:10)])/1000
        # Compute average across 10 SDM RUNS
        t$mean <- rowMeans(as.matrix(t[,c(1:10)]))
        # Compute Min/Max/Mean/Median/Stdev HSI per cell, month across SDM
        ddf <- t %>% group_by(id,month) %>% summarize(x = as.character(unique(x)), y = as.character(unique(y)),
                Min = min(mean), Max = max(mean), Mean = mean(mean), Median = median(mean), Stdev = sd(mean) )
        # Check
        # dim(ddf); summary(ddf)
        ddf$x <- as.numeric(ddf$x)
        ddf$y <- as.numeric(ddf$y)
        
        # Save
        save(x = ddf, file = paste("table_ens_mon_HSI_AtlantECO-MAPS1_",sp,"_13_09_22.RData", sep = ""))
    
    }, mc.cores = 25 
    
) # eo mclapply

### Check that file list is complete
# filestocomplete <- paste("table_ens_mon_HSI_AtlantECO-MAPS1_",species,"_13_09_22.RData", sep = "") # filestocomplete is the list of theoretical files 
# setdiff(filestocomplete, dir()[grep("_13_09_22.RData",dir())]) # Should return 0
# OK. Ready to create .nc file et load these fields in that .nc file

### Create empty .nc file and fill it progressively with all the species-level ensemble projections
# List files of interest in dir()
files <- dir()[grep("_13_09_22.RData",dir())]
# length(files) # should be 504

sp <- "Gaetanus_tenuispinus"

for(sp in species) {
    
    # Creating .nc file for: 
    message(sp)
    data <- as.data.frame(get(load(paste("table_ens_mon_HSI_AtlantECO-MAPS1_",sp,"_13_09_22.RData", sep = ""))))
    
    data$month2 <- NA
    data[data$month == "jan","month2"] <- 1
    data[data$month == "feb","month2"] <- 2
    data[data$month == "mar","month2"] <- 3
    data[data$month == "apr","month2"] <- 4
    data[data$month == "may","month2"] <- 5
    data[data$month == "jun","month2"] <- 6
    data[data$month == "jul","month2"] <- 7
    data[data$month == "aug","month2"] <- 8
    data[data$month == "sep","month2"] <- 9
    data[data$month == "oct","month2"] <- 10
    data[data$month == "nov","month2"] <- 11
    data[data$month == "dec","month2"] <- 12
    
    ncpath <- getwd() # 
    ncname <- paste("AtlantECO-BASE-v1_microbiome_monthly_surface_",sp,"_habitat_suitability_index_SDM_Benedettietal.2021_20220913", sep = "")  
    ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")
    #dname <- "HSI" 
    # Then define the contents of the file:
    londim <- ncdim_def("lon", "degrees_east", as.double(lon) ) 
    latdim <- ncdim_def("lat", "degrees_north", as.double(lat) ) 
    timedim <- ncdim_def("month", "month", as.double(months) )
    
    ### Before filling the .nc file with many variables, create an initial one. Let's just take variables[1].
    ### Like, in Script #8.0, first create an empty array and fill it based on the indices
    array <- array(NA, dim = c(nlon, nlat, nmonths) )
    
    i2 <- sapply(data$x, function(x) which.min(abs(lon - x)) ) 
    j2 <- sapply(data$y, function(x) which.min(abs(lat - x)) ) 
    l2 <- sapply(data$month2, function(x)  which(months == x) ) 
    
    array[cbind(i2,j2,l2)] <- as.matrix(data$Mean) 
    # summary(array) # Nice, seems to have worked.
    
    # Define the variable(s)
    fillvalue <- NA
    varname <- "Mean HSI" # Name of the variable whose values are stored in 'array'
    var_def <- ncvar_def(varname,"Probabilities (0 to 1)", list(londim,latdim,timedim), missval = -999, varname, prec = "float")
    
    ### Create the .nc file in your directory
    ncout <- nc_create(ncfname, list(var_def), force_v4 = T) # add variables in the 'list()'

    # Put variable #1 (evareg HSI)
    ncvar_put(ncout, var_def, array)
    # Check content
    #print(ncout)
    #names(ncout$var) # vector of variables names inside nc
 
    ### Put additional attributes into dimension and data variables
    # ?ncatt_put
    ncatt_put(ncout,"lon","axis","decimalLongitude") 
    ncatt_put(ncout,"lat","axis","decimalLatitude")
    ncatt_put(ncout,"month","axis","Months")

    # add global attributes
    ncatt_put(ncout, varid = 0, attname = "AtlantECO-MAPS-v1", attval = paste("Monthly ensemble habitat suitability suitability indices (HSI) for ",sp, sep = "")) # if varid==0, then a global attribute is written instead of a variable's attribute
    ncatt_put(ncout, varid = 0,"Institution", "ETH Zürich, D-USYS, IBP, UP group")
    ncatt_put(ncout, varid = 0,"Description", "Ensemble projections of monthly surface habitat suitability indices (HSI; aka presence probabilities) based on three standard species distribution models (GLM, GAM and ANN) based on global presence data with a group-level target background for pseudoabsence data (full methodology described in Benedetti et al., 2021)")
    ncatt_put(ncout, varid = 0,"DOI", "doi:10.1038/s41467-021-25385-x")
    ncatt_put(ncout, varid = 0, "Funding statement","This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 862923. This output reflects only the author’s view, and the European Union cannot be held responsible for any use that may be made of the information contained therein.")

    history <- paste("Fabio Benedetti (fabio.benedetti@usys.ethz.ch); last update: ", date(), sep = ", ")
    ncatt_put(ncout, varid = 0, "Last update by:", history)

    ### Need to close it before being able to write new var into it (...)
    nc_close(ncout) 

    ### Clear some RAM
    rm(array,var_def,varname,data,i2,j2,l2) ; gc()    
    
} # eo for loop - sp in species


### And now, add more variables (Median/Min/Max/Stdev) in the 
variables <- c("Median HSI","Min HSI","Max HSI","Stdev HSI")
fillvalue <- NA

# For testing the below:
# sp <- species[1]

for(sp in species) {
    
    ### Re-load the data.frame with the ensemble projections of HSI
    message(sp)
    data <- as.data.frame(get(load(paste("table_ens_mon_HSI_AtlantECO-MAPS1_",sp,"_13_09_22.RData", sep = ""))))
    
    data$month2 <- NA
    data[data$month == "jan","month2"] <- 1
    data[data$month == "feb","month2"] <- 2
    data[data$month == "mar","month2"] <- 3
    data[data$month == "apr","month2"] <- 4
    data[data$month == "may","month2"] <- 5
    data[data$month == "jun","month2"] <- 6
    data[data$month == "jul","month2"] <- 7
    data[data$month == "aug","month2"] <- 8
    data[data$month == "sep","month2"] <- 9
    data[data$month == "oct","month2"] <- 10
    data[data$month == "nov","month2"] <- 11
    data[data$month == "dec","month2"] <- 12
    
    ### Re-open the .nc you created before
    ncout <- nc_open(paste("AtlantECO-BASE-v1_microbiome_monthly_surface_",sp,"_habitat_suitability_index_SDM_Benedettietal.2021_20220913.nc", sep = ""), write = TRUE)
    # For tetsing var below
    # v <- variables[1]
    lapply(X = variables, function(v) {
    
                ### Message
                message(paste("Preparing ",v," to be added into the .nc file", sep = ""))
            
                # Define empty multidem array
                array <- array(NA, dim = c(nlon,nlat,nmonths) )

                i2 <- sapply(data$x, function(x) which.min(abs(lon - x)) ) 
                j2 <- sapply(data$y, function(x) which.min(abs(lat - x)) ) 
                l2 <- sapply(data$month2, function(x)  which(months == x) ) 

                # Assign values to empty array based on v
                if(v == "Median HSI") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(data$Median) 
                    
                } else if(v == "Min HSI") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(data$Min) 
                    
                } else if(v == "Max HSI") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(data$Max) 
                    
                } else if(v == "Stdev HSI") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(data$Stdev) 
                    
                } # eo if else loop            
            
                ### Define var to be put in .nc
                # ?ncvar_def
                var_def <- ncvar_def(name = v, units = "Probabilities (0 to 1)", dim = list(londim,latdim,timedim), missval = -999, longname = v, prec = "float")

                # Put variable v in netCDF file
                message(paste("Adding ",v," to the .nc file", sep = ""))
                # ?ncvar_add
                ncout <- ncvar_add(nc = ncout, v = var_def)
                ncvar_put(ncout, var_def, array)
    
                # Clear some RAM
                rm(array,var_def,i2,j2,l2) ; gc()
            
            }
        
    ) # eo lapply
    
    # Check
    # print(ncout)
    
    nc_close(ncout)
    rm(data) ; gc()
    
    message("                       ")
    
} # eo for loop - sp in species


### ----------------------------------------------------------------------------------------------------------------------------

### 14/09/2022: Same as above, but for phytoplankton

# Directory with with WOA's nc file
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors"); dir()
ncin <- nc_open("woa13_all_o_monthly.nc")
# print(ncin) # to check what's inside
# Has lat, lon, depth, time.
# Longitude dimension
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
# unique(lon)
# Latitude dimension
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
# unique(lat)
# close netcdf
nc_close(ncin)
gc()
# manually create months dimensions 
months <- c(1:12)
nmonths <- 12

### Get phytoplankton species HSI files
setwd("/net/kryo/work/fabioben/GBD/Phytoplankton_species_HSI_tables/") ; dir()
### First, get vector of species names
species <- dir()[grep("table_HSI_allruns_",dir())]
# sp <- str_replace_all(species[1], pattern = "table_HSI_allruns_", replacement = "") 
species <- str_replace_all(species, pattern = "table_HSI_allruns_", replacement = "") 
species <- str_replace_all(species, pattern = "_GLM", replacement = "") 
species <- str_replace_all(species, pattern = "_GAM", replacement = "") 
species <- str_replace_all(species, pattern = "_ANN", replacement = "") 
species <- str_replace_all(species, pattern = "_jan.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_feb.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_mar.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_apr.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_may.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_jun.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_jul.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_aug.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_sep.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_oct.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_nov.Rdata", replacement = "") 
species <- str_replace_all(species, pattern = "_dec.Rdata", replacement = "") 
species <- unique(species) # length(species) # 341

### Compute Mean/Median/Min/Max/Stdev for each taxa
#sp <- species[1]; sp
mclapply(species, function(sp) {
    
        # Useless message
        message(paste(sp, sep = ""))
        files <- dir()[grep(sp,dir())] # files
        # Load and rvind all sp projections
        res <- lapply(files, function(f) { d <- get(load(f)) ; return(d) } ) 
        # Rbind
        t <- dplyr::bind_rows(res)
        # dim(t); str(t)
        rm(res); gc()
        # Rescale to 0-1 (HSI)
        t[,c(1:10)] <- (t[,c(1:10)])/1000
        # Compute average across 10 SDM RUNS
        t$mean <- rowMeans(as.matrix(t[,c(1:10)]))
        # Compute Min/Max/Mean/Median/Stdev HSI per cell, month across SDM
        ddf <- t %>% group_by(id,month) %>% summarize(x = as.character(unique(x)), y = as.character(unique(y)),
                Min = min(mean), Max = max(mean), Mean = mean(mean), Median = median(mean), Stdev = sd(mean) )
        # Check
        # dim(ddf); summary(ddf)
        ddf$x <- as.numeric(ddf$x)
        ddf$y <- as.numeric(ddf$y)
        
        # Save
        save(x = ddf, file = paste("table_ens_mon_HSI_AtlantECO-MAPS1_",sp,"_14_09_22.RData", sep = ""))
    
    }, mc.cores = 30 
    
) # eo mclapply

### Check that file list is complete
filestocomplete <- paste("table_ens_mon_HSI_AtlantECO-MAPS1_",species,"_14_09_22.RData", sep = "") # filestocomplete is the list of theoretical files 
setdiff(filestocomplete, dir()[grep("_14_09_22.RData",dir())]) # Should return 0
# OK. Ready to create .nc file et load these fields in that .nc file

files <- dir()[grep("_14_09_22.RData",dir())]
# length(files) # should be 341

#sp <- "Gaetanus_tenuispinus"

for(sp in species) {
    
    # Creating .nc file for: 
    message(sp)
    data <- as.data.frame(get(load(paste("table_ens_mon_HSI_AtlantECO-MAPS1_",sp,"_14_09_22.RData", sep = ""))))
    
    data$month2 <- NA
    data[data$month == "jan","month2"] <- 1
    data[data$month == "feb","month2"] <- 2
    data[data$month == "mar","month2"] <- 3
    data[data$month == "apr","month2"] <- 4
    data[data$month == "may","month2"] <- 5
    data[data$month == "jun","month2"] <- 6
    data[data$month == "jul","month2"] <- 7
    data[data$month == "aug","month2"] <- 8
    data[data$month == "sep","month2"] <- 9
    data[data$month == "oct","month2"] <- 10
    data[data$month == "nov","month2"] <- 11
    data[data$month == "dec","month2"] <- 12
    
    ncpath <- getwd() # 
    ncname <- paste("AtlantECO-BASE-v1_microbiome_monthly_surface_",sp,"_habitat_suitability_index_SDM_Benedettietal.2021_20220914", sep = "")  
    ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")
    #dname <- "HSI" 
    # Then define the contents of the file:
    londim <- ncdim_def("lon", "degrees_east", as.double(lon) ) 
    latdim <- ncdim_def("lat", "degrees_north", as.double(lat) ) 
    timedim <- ncdim_def("month", "month", as.double(months) )
    
    ### Before filling the .nc file with many variables, create an initial one. Let's just take variables[1].
    ### Like, in Script #8.0, first create an empty array and fill it based on the indices
    array <- array(NA, dim = c(nlon, nlat, nmonths) )
    
    i2 <- sapply(data$x, function(x) which.min(abs(lon - x)) ) 
    j2 <- sapply(data$y, function(x) which.min(abs(lat - x)) ) 
    l2 <- sapply(data$month2, function(x)  which(months == x) ) 
    
    array[cbind(i2,j2,l2)] <- as.matrix(data$Mean) 
    # summary(array) # Nice, seems to have worked.
    
    # Define the variable(s)
    fillvalue <- NA
    varname <- "Mean HSI" # Name of the variable whose values are stored in 'array'
    var_def <- ncvar_def(varname,"Probabilities (0 to 1)", list(londim,latdim,timedim), missval = -999, varname, prec = "float")
    
    ### Create the .nc file in your directory
    ncout <- nc_create(ncfname, list(var_def), force_v4 = T) # add variables in the 'list()'

    # Put variable #1 (evareg HSI)
    ncvar_put(ncout, var_def, array)
    # Check content
    #print(ncout)
    #names(ncout$var) # vector of variables names inside nc
 
    ### Put additional attributes into dimension and data variables
    # ?ncatt_put
    ncatt_put(ncout,"lon","axis","decimalLongitude") 
    ncatt_put(ncout,"lat","axis","decimalLatitude")
    ncatt_put(ncout,"month","axis","Months")

    # add global attributes
    ncatt_put(ncout, varid = 0, attname = "AtlantECO-MAPS-v1", attval = paste("Monthly ensemble habitat suitability suitability indices (HSI) for ",sp, sep = "")) # if varid==0, then a global attribute is written instead of a variable's attribute
    ncatt_put(ncout, varid = 0,"Institution", "ETH Zürich, D-USYS, IBP, UP group")
    ncatt_put(ncout, varid = 0,"Description", "Ensemble projections of phytoplankton monthly surface habitat suitability indices (HSI; aka presence probabilities) based on three standard species distribution models (GLM, GAM and ANN) based on global presence data with a group-level target background for pseudoabsence data (full methodology described in Benedetti et al., 2021)")
    ncatt_put(ncout, varid = 0,"DOI", "doi:10.1038/s41467-021-25385-x")
    ncatt_put(ncout, varid = 0, "Funding statement","This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 862923. This output reflects only the author’s view, and the European Union cannot be held responsible for any use that may be made of the information contained therein.")

    history <- paste("Fabio Benedetti (fabio.benedetti@usys.ethz.ch); last update: ", date(), sep = ", ")
    ncatt_put(ncout, varid = 0, "Last update by:", history)

    ### Need to close it before being able to write new var into it (...)
    nc_close(ncout) 

    ### Clear some RAM
    rm(array,var_def,varname,data,i2,j2,l2) ; gc()    
    
} # eo for loop - sp in species


### And now, add more variables (Median/Min/Max/Stdev) in the 
variables <- c("Median HSI","Min HSI","Max HSI","Stdev HSI")
fillvalue <- NA

# For testing the below:
# sp <- species[1]

for(sp in species) {
    
    ### Re-load the data.frame with the ensemble projections of HSI
    message(sp)
    data <- as.data.frame(get(load(paste("table_ens_mon_HSI_AtlantECO-MAPS1_",sp,"_14_09_22.RData", sep = ""))))
    
    data$month2 <- NA
    data[data$month == "jan","month2"] <- 1
    data[data$month == "feb","month2"] <- 2
    data[data$month == "mar","month2"] <- 3
    data[data$month == "apr","month2"] <- 4
    data[data$month == "may","month2"] <- 5
    data[data$month == "jun","month2"] <- 6
    data[data$month == "jul","month2"] <- 7
    data[data$month == "aug","month2"] <- 8
    data[data$month == "sep","month2"] <- 9
    data[data$month == "oct","month2"] <- 10
    data[data$month == "nov","month2"] <- 11
    data[data$month == "dec","month2"] <- 12
    
    ### Re-open the .nc you created before
    ncout <- nc_open(paste("AtlantECO-BASE-v1_microbiome_monthly_surface_",sp,"_habitat_suitability_index_SDM_Benedettietal.2021_20220914.nc", sep = ""), write = TRUE)
    # For tetsing var below
    # v <- variables[1]
    lapply(X = variables, function(v) {
    
                ### Message
                message(paste("Preparing ",v," to be added into the .nc file", sep = ""))
            
                # Define empty multidem array
                array <- array(NA, dim = c(nlon,nlat,nmonths) )

                i2 <- sapply(data$x, function(x) which.min(abs(lon - x)) ) 
                j2 <- sapply(data$y, function(x) which.min(abs(lat - x)) ) 
                l2 <- sapply(data$month2, function(x)  which(months == x) ) 

                # Assign values to empty array based on v
                if(v == "Median HSI") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(data$Median) 
                    
                } else if(v == "Min HSI") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(data$Min) 
                    
                } else if(v == "Max HSI") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(data$Max) 
                    
                } else if(v == "Stdev HSI") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(data$Stdev) 
                    
                } # eo if else loop            
            
                ### Define var to be put in .nc
                # ?ncvar_def
                var_def <- ncvar_def(name = v, units = "Probabilities (0 to 1)", dim = list(londim,latdim,timedim), missval = -999, longname = v, prec = "float")

                # Put variable v in netCDF file
                message(paste("Adding ",v," to the .nc file", sep = ""))
                # ?ncvar_add
                ncout <- ncvar_add(nc = ncout, v = var_def)
                ncvar_put(ncout, var_def, array)
    
                # Clear some RAM
                rm(array,var_def,i2,j2,l2) ; gc()
            
            }
        
    ) # eo lapply
    
    # Check
    # print(ncout)
    
    nc_close(ncout)
    rm(data) ; gc()
    
    message("                       ")
    
} # eo for loop - sp in species


### ----------------------------------------------------------------------------------------------------------------------------

### 19/09/22: Store the group-level ensemble projections of monthly SR (baseline and future; Min/Max/Mean/Median/Stdev) in netCDFs files
### There are 14 groups (3 phyto-, 11 zooplankton), and 2 times period, so it should generate 28 netCDF files

### Directory with with WOA's nc file
setwd("/net/kryo/work/fabioben/OVERSEE/data/env_predictors"); dir()
ncin <- nc_open("woa13_all_o_monthly.nc")
# print(ncin) # to check what's inside
# Has lat, lon, depth, time.
# Longitude dimension
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
# unique(lon)
# Latitude dimension
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
# unique(lat)
# close netcdf
nc_close(ncin); gc()
# manually create months dimensions 
months <- c(1:12)
nmonths <- 12

### Directory to SDM ensemble projections
setwd("/net/kryo/work/fabioben/OVERSEE/data/biology/data_for_group_studies"); dir()
base <- get(load("table_mon_rich_baseline_groups_22_10_20.Rdata"))
# dim(base)
# head(base)
#summary(base)
fut <- get(load("table_mon_rich_2100-2000_groups_22_10_20.Rdata"))
# dim(fut)
# head(fut)
# summary(fut) # No NAs in 'rich' - is not ok
# unique(base$group) ; unique(fut$group) 

### Remember, in rich of 'base' but not 'fut', all 0 should be NaN
base[base$rich == 0,"rich"] <- NA
fut[fut$rich == 0,"rich"] <- NA
# summary(base); summary(fut)

### For each cell, month and group, derive Min/Max/Mean/Median/Stdev SR (sum of HSI)
ens.base <- data.frame(base %>% group_by(cell_id,month,group) %>% summarize(x = unique(x), y = unique(y),
                Min = min(rich), Max = max(rich), Mean = mean(rich), Median = median(rich), Stdev = sd(rich) ) 
)
# Check
# dim(ens.base)
# summary(ens.base)

### For future time period, need to rotate the longitudes again...
fut$x2 <- fut$x
fut[which(fut$x > 179.5),"x2"] <- fut[which(fut$x > 179.5),"x"] - 360
#unique(fut$x); unique(fut$x2)

ens.fut <- data.frame(fut %>% group_by(cell_id,month,group) %>% summarize(x = unique(x2), y = unique(y),
                Min = min(rich), Max = max(rich), Mean = mean(rich), Median = median(rich), Stdev = sd(rich) ) 
)
# Check
#dim(ens.fut)
#summary(ens.fut)

### OK, ready
groups <- unique(ens.base$group); groups

### Do it for the baseline (2012-2031) time period first
# g <- "Calanoida"
for(g in groups) {
    
    # Creating .nc file for: 
    message(g)
    sub <- ens.base[ens.base$group == g,] # head(sub)
    
    sub$month2 <- NA
    sub[sub$month == "jan","month2"] <- 1
    sub[sub$month == "feb","month2"] <- 2
    sub[sub$month == "mar","month2"] <- 3
    sub[sub$month == "apr","month2"] <- 4
    sub[sub$month == "may","month2"] <- 5
    sub[sub$month == "jun","month2"] <- 6
    sub[sub$month == "jul","month2"] <- 7
    sub[sub$month == "aug","month2"] <- 8
    sub[sub$month == "sep","month2"] <- 9
    sub[sub$month == "oct","month2"] <- 10
    sub[sub$month == "nov","month2"] <- 11
    sub[sub$month == "dec","month2"] <- 12
    
    ncpath <- getwd() # 
    ncname <- paste("AtlantECO-MAPS-v1_microbiome_monthly_surface_",g,"_species_richness_2012-2031_Benedettietal.2021_20220919", sep = "")  
    ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")
 
    # Then define the contents of the file:
    londim <- ncdim_def("lon", "degrees_east", as.double(lon) ) 
    latdim <- ncdim_def("lat", "degrees_north", as.double(lat) ) 
    timedim <- ncdim_def("month", "month", as.double(months) )
    
    ### Before filling the .nc file with many variables, create an initial one. Let's just take variables[1].
    ### Like, in Script #8.0, first create an empty array and fill it based on the indices
    array <- array(NA, dim = c(nlon,nlat,nmonths) )
    
    i2 <- sapply(sub$x, function(x) which.min(abs(lon - x)) ) 
    j2 <- sapply(sub$y, function(x) which.min(abs(lat - x)) ) 
    l2 <- sapply(sub$month2, function(x)  which(months == x) ) 
    
    array[cbind(i2,j2,l2)] <- as.matrix(sub$Mean) 
    # summary(array) # Nice, seems to have worked.
    
    # Define the variable(s)
    fillvalue <- NA
    varname <- "Mean species richness" # Name of the variable whose values are stored in 'array'
    var_def <- ncvar_def(varname,"(sum of species-level habitat suitabilitiy indices; 2012-2031 period)", list(londim,latdim,timedim), missval = -999, varname, prec = "float")
    
    ### Create the .nc file in your directory
    ncout <- nc_create(ncfname, list(var_def), force_v4 = T) # add variables in the 'list()'

    # Put variable #1 (evareg HSI)
    ncvar_put(ncout, var_def, array)
    # Check content
    #print(ncout)
    #names(ncout$var) # vector of variables names inside nc
 
    ### Put additional attributes into dimension and data variables
    # ?ncatt_put
    ncatt_put(ncout,"lon","axis","decimalLongitude") 
    ncatt_put(ncout,"lat","axis","decimalLatitude")
    ncatt_put(ncout,"month","axis","Months")

    # add global attributes
    ncatt_put(ncout, varid = 0, attname = "AtlantECO-MAPS-v1", attval = paste("Monthly species richness of ",g," for the 2012-2031 period", sep = "")) # if varid==0, then a global attribute is written instead of a variable's attribute
    ncatt_put(ncout, varid = 0,"Institution", "ETH Zürich, D-USYS, IBP, UP group")
    ncatt_put(ncout, varid = 0,"Description", "Ensemble projections of monthly surface species richness (i.e., computed through the sum of species-level habitat suitabilities) based on three standard species distribution models (GLM, GAM and ANN) trained on global presence data with a group-level target background for pseudoabsence data (full methodology described in Benedetti et al., 2021)")
    ncatt_put(ncout, varid = 0,"DOI", "doi:10.1038/s41467-021-25385-x")
    ncatt_put(ncout, varid = 0, "Funding statement","This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 862923. This output reflects only the author’s view, and the European Union cannot be held responsible for any use that may be made of the information contained therein.")

    history <- paste("Fabio Benedetti (fabio.benedetti@usys.ethz.ch); last update: ", date(), sep = ", ")
    ncatt_put(ncout, varid = 0, "Last update by:", history)

    ### Need to close it before being able to write new var into it (...)
    nc_close(ncout) 

    ### Clear some RAM
    rm(array,var_def,varname,data,i2,j2,l2) ; gc()    
    message("                            ")
     
} # eo for loop - sp in species


### Add the other 4 variables (Min/Max/Median/Stdev)
variables <- c("Median species richness","Min species richness","Max species richness","Stdev species richness")
fillvalue <- NA

for(g in groups) {
    
    ### Re-load the data.frame with the ensemble projections of HSI
    message(g)
    sub <- ens.base[ens.base$group == g,] 
    
    sub$month2 <- NA
    sub[sub$month == "jan","month2"] <- 1
    sub[sub$month == "feb","month2"] <- 2
    sub[sub$month == "mar","month2"] <- 3
    sub[sub$month == "apr","month2"] <- 4
    sub[sub$month == "may","month2"] <- 5
    sub[sub$month == "jun","month2"] <- 6
    sub[sub$month == "jul","month2"] <- 7
    sub[sub$month == "aug","month2"] <- 8
    sub[sub$month == "sep","month2"] <- 9
    sub[sub$month == "oct","month2"] <- 10
    sub[sub$month == "nov","month2"] <- 11
    sub[sub$month == "dec","month2"] <- 12
    
    ### Re-open the .nc you created before
    ncout <- nc_open(paste("AtlantECO-MAPS-v1_microbiome_monthly_surface_",g,"_species_richness_2012-2031_Benedettietal.2021_20220919.nc", sep = ""), write = T)
    # For tetsing var below
    # v <- variables[1]
    lapply(X = variables, function(v) {
    
                ### Message
                message(paste("Preparing ",v," to be added into the .nc file", sep = ""))
            
                # Define empty multidem array
                array <- array(NA, dim = c(nlon,nlat,nmonths) )

                i2 <- sapply(sub$x, function(x) which.min(abs(lon - x)) ) 
                j2 <- sapply(sub$y, function(x) which.min(abs(lat - x)) ) 
                l2 <- sapply(sub$month2, function(x)  which(months == x) ) 

                # Assign values to empty array based on v
                if(v == "Median species richness") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(sub$Median) 
                    
                } else if(v == "Min species richness") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(sub$Min) 
                    
                } else if(v == "Max species richness") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(sub$Max) 
                    
                } else if(v == "Stdev species richness") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(sub$Stdev) 
                    
                } # eo if else loop            
            
                ### Define var to be put in .nc
                # ?ncvar_def
                var_def <- ncvar_def(name = v, units = "", dim = list(londim,latdim,timedim), missval = -999, longname = v, prec = "float")

                # Put variable v in netCDF file
                message(paste("Adding ",v," to the .nc file", sep = ""))
                # ?ncvar_add
                ncout <- ncvar_add(nc = ncout, v = var_def)
                ncvar_put(ncout, var_def, array)
    
                # Clear some RAM
                rm(array,var_def,i2,j2,l2) ; gc()
            
            }
        
    ) # eo lapply
    
    # Check
    # print(ncout)
    
    nc_close(ncout)
    rm(data) ; gc()
    
    message("                       ")
    
} # eo for loop - g in groups


### --------------------------------------------------------------------------------

### 20/09/22: Same as above nuy for the future time period now (2081-2100)
# g <- "Calanoida" # For testing functions below

for(g in groups) {
    
    # Creating .nc file for: 
    message(g)
    sub <- ens.fut[ens.fut$group == g,]
    
    sub$month2 <- NA
    sub[sub$month == "jan","month2"] <- 1
    sub[sub$month == "feb","month2"] <- 2
    sub[sub$month == "mar","month2"] <- 3
    sub[sub$month == "apr","month2"] <- 4
    sub[sub$month == "may","month2"] <- 5
    sub[sub$month == "jun","month2"] <- 6
    sub[sub$month == "jul","month2"] <- 7
    sub[sub$month == "aug","month2"] <- 8
    sub[sub$month == "sep","month2"] <- 9
    sub[sub$month == "oct","month2"] <- 10
    sub[sub$month == "nov","month2"] <- 11
    sub[sub$month == "dec","month2"] <- 12
    
    # summary(sub)
    
    ncpath <- getwd() # 
    ncname <- paste("AtlantECO-MAPS-v1_microbiome_monthly_surface_",g,"_species_richness_2081-2100_Benedettietal.2021_20220919", sep = "")  
    ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")
    #dname <- "HSI" 
    # Then define the contents of the file:
    londim <- ncdim_def("lon", "degrees_east", as.double(lon) ) 
    latdim <- ncdim_def("lat", "degrees_north", as.double(lat) ) 
    timedim <- ncdim_def("month", "month", as.double(months) )
    
    ### Before filling the .nc file with many variables, create an initial one. Let's just take variables[1].
    ### Like, in Script #8.0, first create an empty array and fill it based on the indices
    array <- array(NA, dim = c(nlon, nlat, nmonths) )
    
    i2 <- sapply(sub$x, function(x) which.min(abs(lon - x)) ) 
    j2 <- sapply(sub$y, function(x) which.min(abs(lat - x)) ) 
    l2 <- sapply(sub$month2, function(x)  which(months == x) ) 
    
    array[cbind(i2,j2,l2)] <- as.matrix(sub$Mean) 
    # summary(array) # Nice, seems to have worked.
    
    # Define the variable(s)
    fillvalue <- NA
    varname <- "Mean species richness" # Name of the variable whose values are stored in 'array'
    var_def <- ncvar_def(varname,"sum of species-level habitat suitabilitiy indices; 2081-2100 period", list(londim,latdim,timedim), missval = -999, varname, prec = "float")
    
    ### Create the .nc file in your directory
    ncout <- nc_create(ncfname, list(var_def), force_v4 = T) # add variables in the 'list()'

    # Put variable #1 (evareg HSI)
    ncvar_put(ncout, var_def, array)
    # Check content
    #print(ncout)
    #names(ncout$var) # vector of variables names inside nc
 
    ### Put additional attributes into dimension and data variables
    # ?ncatt_put
    ncatt_put(ncout,"lon","axis","decimalLongitude") 
    ncatt_put(ncout,"lat","axis","decimalLatitude")
    ncatt_put(ncout,"month","axis","Months")

    # add global attributes
    ncatt_put(ncout, varid = 0, attname = "AtlantECO-MAPS-v1", attval = paste("Monthly ensemble species richness of ",g," for the 2012-2031 period", sep = "")) # if varid==0, then a global attribute is written instead of a variable's attribute
    ncatt_put(ncout, varid = 0,"Institution", "ETH Zürich, D-USYS, IBP, UP group")
    ncatt_put(ncout, varid = 0,"Description", "Ensemble projections of future monthly surface species richness (i.e., computed through the sum of species-level habitat suitabilities) based on three standard species distribution models (GLM, GAM and ANN) trained on global presence data with a group-level target background for pseudoabsence data (full methodology described in Benedetti et al., 2021)")
    ncatt_put(ncout, varid = 0,"DOI", "doi:10.1038/s41467-021-25385-x")
    ncatt_put(ncout, varid = 0, "Funding statement","This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 862923. This output reflects only the author’s view, and the European Union cannot be held responsible for any use that may be made of the information contained therein.")

    history <- paste("Fabio Benedetti (fabio.benedetti@usys.ethz.ch); last update: ", date(), sep = ", ")
    ncatt_put(ncout, varid = 0, "Last update by:", history)

    ### Need to close it before being able to write new var into it (...)
    nc_close(ncout) 

    ### Clear some RAM
    rm(array,var_def,varname,data,i2,j2,l2) ; gc()  
    message("                                   ")  
    
} # eo for loop - g in groups


### Add the other 4 variables (Min/Max/Median/Stdev)
variables <- c("Median species richness","Min species richness","Max species richness","Stdev species richness")
fillvalue <- NA

for(g in groups) {
    
    ### Re-load the data.frame with the ensemble projections of HSI
    message(g)
    sub <- ens.fut[ens.fut$group == g,] 
    
    sub$month2 <- NA
    sub[sub$month == "jan","month2"] <- 1
    sub[sub$month == "feb","month2"] <- 2
    sub[sub$month == "mar","month2"] <- 3
    sub[sub$month == "apr","month2"] <- 4
    sub[sub$month == "may","month2"] <- 5
    sub[sub$month == "jun","month2"] <- 6
    sub[sub$month == "jul","month2"] <- 7
    sub[sub$month == "aug","month2"] <- 8
    sub[sub$month == "sep","month2"] <- 9
    sub[sub$month == "oct","month2"] <- 10
    sub[sub$month == "nov","month2"] <- 11
    sub[sub$month == "dec","month2"] <- 12
    
    ### Re-open the .nc you created before
    ncout <- nc_open(paste("AtlantECO-MAPS-v1_microbiome_monthly_surface_",g,"_species_richness_2081-2100_Benedettietal.2021_20220919.nc", sep = ""), write = T)
    # For tetsing var below
    lapply(X = variables, function(v) {
    
                ### Message
                message(paste("Preparing ",v," to be added into the .nc file", sep = ""))
            
                # Define empty multidem array
                array <- array(NA, dim = c(nlon,nlat,nmonths) )

                i2 <- sapply(sub$x, function(x) which.min(abs(lon - x)) ) 
                j2 <- sapply(sub$y, function(x) which.min(abs(lat - x)) ) 
                l2 <- sapply(sub$month2, function(x)  which(months == x) ) 

                # Assign values to empty array based on v
                if(v == "Median species richness") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(sub$Median) 
                    
                } else if(v == "Min species richness") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(sub$Min) 
                    
                } else if(v == "Max species richness") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(sub$Max) 
                    
                } else if(v == "Stdev species richness") {
                    
                    array[cbind(i2,j2,l2)] <- as.matrix(sub$Stdev) 
                    
                } # eo if else loop            
            
                ### Define var to be put in .nc
                # ?ncvar_def
                var_def <- ncvar_def(name = v, units = "", dim = list(londim,latdim,timedim), missval = -999, longname = v, prec = "float")

                # Put variable v in netCDF file
                message(paste("Adding ",v," to the .nc file", sep = ""))
                # ?ncvar_add
                ncout <- ncvar_add(nc = ncout, v = var_def)
                ncvar_put(ncout, var_def, array)
    
                # Clear some RAM
                rm(array,var_def,i2,j2,l2) ; gc()
            
            }
        
    ) # eo lapply
    
    # Check
    # print(ncout)
    
    nc_close(ncout)
    rm(data) ; gc()
    
    message("                       ")
    
} # eo for loop - g in groups



### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
