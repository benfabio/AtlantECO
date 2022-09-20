
##### ATLANTECO SCRIPT 10.0 ----------------------------------------------------------------------------------------------------------------------------
##### 17/03/2022: R Script to  (might be useful for modelling experiments in WP2 for MAPS1 deliverables).  
##### © Nielja Knecht (NK) & Fabio Benedetti (FB), ETH Zürich, IBP, UP Group.

### R script below aims to:
# - Combine all the monthly predictors fields gathered by Nielja Knecht (ETH Zürich) into a single matrix
# - Save the combined file of monthly climatologies to 3 complementary formats: a) .txt (data.frame), b) .grd (raster) and c) .nc (netCDF)
# - Use it as a basis for

### Latest update: 30/03/2022

library("tidyverse")
library("rgeos")
library("rgdal")
library("raster")
library("ncdf4")
library("maps")
library("marmap")
library("reshape2")
library("RColorBrewer")
library("viridis")
library("xlsx")
library("geosphere")
library("parallel")
library("here")

setwd( here("all_env_vars_all_months_NK_17_03_22/") ) ; dir()

# Load coastline for mapping
world <- map_data("world")
# Get the WOA 1x1 cell grid from one of their .nc files as a raster object in R (might be useful if some .nc files do not already follow WOA's grid)
grid <- raster::raster("woa18_decav_t_allmonths_surfaceSST.nc")
# class(ras)
# ras
crs(grid) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
# extent(grid)
# plot(grid)

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Load all climatologies and rbind/cbind them into one data.frame

# Vector of predictors names:
preds <- c("Dist2coast","SST","Temp_mld","Temp_mld.grad","SSS","Sal_mld","MLD","Wind.speed","EddyKineticEnergy",
        "PAR","Zeu","O2","O2_200m","O2_mld","TAlk","DIC","pCO2","Revelle","Omega_Calcite","Omega_Aragonite",
        "PIC","bbp443","Kd490","CHLA","Nitrates","Nitrates_mld","Nitrates_mld.grad",
        "Phosphates","Phosphates_mld","Phosphates_mld.grad","Silicates","Silicates_mld","Silicates_mld.grad")
# length(preds)
### Add bathymetry layer later (and maybe a land vs. ocean flag based on SST)

### In a lapply, load the monthly values from each cooresponding netcdf and rbind. Re-sample on common WOA grid if necessary along the way
# Quick for loop to display the spatial extent of each nectdf
# f <- "woa18_all_o_allmonths_surface.nc"
# f <- "Zeu_lee_SeaWIFS_allmonths.nc"

### Quick for loop to check thenumber of vars per .nc file
# for(f in dir()) {
#
#     var_nc <- nc_open(f)
#
#     if( length( names(var_nc$var) ) == 1 ) {
#         message(paste(f, ' has 1 variable inside', sep = ""))
#         message(paste('', sep = ""))
#     } else {
#         message(paste("!!! ", f, ' has > 1 variable inside !!!', sep = ""))
#         message(paste('', sep = ""))
#     }
#
#     nc_close(var_nc)
#     gc()
#
# } # eo for loop
### OK, pco2_related_vars.nc is the only .nc with > 1 var

# p <- "bbp443"
# p <- "SST"
 
res.clims <- lapply(preds, function(p) {

        # Useless message
        message(paste("Loading .nc corresponding to ", p, sep = ""))
        message(paste("", sep = ""))
        
        # Load the netcdf as rater object depending on 'p'
        # 1st if else loop based on var name
        if(p == "TAlk") {
            nc <- "pco2_related_vars.nc"
            ras <- raster::brick(nc, varname = "talk")
        } else if(p == "DIC") {      
            nc <- "pco2_related_vars.nc"   
            ras <- raster::brick(nc, varname = "dic")         
        } else if(p == "pCO2") {          
            nc <- "pco2_related_vars.nc"
            ras <- raster::brick(nc, varname = "spco2")     
        } else if(p == "Revelle") {     
            nc <- "pco2_related_vars.nc"
            ras <- raster::brick(nc, varname = "revelle_factor")    
        } else if(p == "Omega_Calcite") {
            nc <- "pco2_related_vars.nc"
            ras <- raster::brick(nc, varname = "omega_ca")  
        } else if(p == "Omega_Aragonite") {
            nc <- "pco2_related_vars.nc"
            ras <- raster::brick(nc, varname = "omega_ar")
        } else if(p == "Dist2coast") {  
            nc <- "dist2coast_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "SST") { 
            nc <- "woa18_decav_t_allmonths_surfaceSST.nc"
            ras <- raster::stack(nc)  
        } else if(p == "Temp_mld") { 
            nc <- "woa18_decav_t_allmonths_mld_average_SODA3.4.2.nc"
            ras <- raster::stack(nc)  
        } else if(p == "Temp_mld.grad") {   
            nc <- "woa18_decav_t_allmonths_mld_gradient.nc"  
            ras <- raster::stack(nc) 
        } else if(p == "SSS") {  
            nc <- "woa18_decav_s_allmonths_surfaceSSS.nc"
            ras <- raster::stack(nc)
        } else if(p == "Sal_mld") {  
            nc <- "woa18_decav_s_allmonths_mld_average_SODA3.4.2.nc"
            ras <- raster::stack(nc)   
        } else if(p == "MLD") {  
            nc <- "MLD_SODA.nc"
            ras <- raster::stack(nc)
        } else if(p == "Wind.speed") {
            nc <- "wind_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "EddyKineticEnergy") {
            nc <- "EKE_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "PAR") {
            nc <- "par_SeaWIFS_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "Zeu") {
            nc <- "Zeu_lee_SeaWIFS_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "O2") {
            nc <- "woa18_all_o_allmonths_surface.nc"
            ras <- raster::stack(nc)
        } else if(p == "O2_200m") {
            nc <- "woa18_all_o_allmonths_depth_200.nc"
            ras <- raster::stack(nc)
        } else if(p == "O2_mld") {
            nc <- "woa18_all_o_allmonths_mld_average_SODA3.4.2.nc"
            ras <- raster::stack(nc)
        } else if(p == "PIC") {
            nc <- "pic_SeaWIFS_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "bbp443") {
            nc <- "bbp_443_gsm_SeaWIFS_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "Kd490") {
            nc <- "Kd_490_SeaWIFS_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "CHLA") {
            nc <- "chlor_a_SeaWIFS_allmonths.nc"
            ras <- raster::stack(nc)
        } else if(p == "Nitrates") {
            nc <- "woa18_all_n_allmonths_surface.nc"
            ras <- raster::stack(nc)
        } else if(p == "Nitrates_mld") {
            nc <- "woa18_all_n_allmonths_mld_average_SODA3.4.2.nc"
            ras <- raster::stack(nc)
        } else if(p == "Nitrates_mld.grad") {
            nc <- "woa18_all_n_allmonths_mld_gradient.nc"
            ras <- raster::stack(nc)
        } else if(p == "Phosphates") {
            nc <- "woa18_all_p_allmonths_surface.nc"
            ras <- raster::stack(nc)
        } else if(p == "Phosphates_mld") {
            nc <- "woa18_all_p_allmonths_mld_average_SODA3.4.2.nc"
            ras <- raster::stack(nc)
        } else if(p == "Phosphates_mld.grad") {
            nc <- "woa18_all_p_allmonths_mld_gradient.nc"
            ras <- raster::stack(nc)
        } else if(p == "Silicates") {
            nc <- "woa18_all_i_allmonths_surface.nc"
            ras <- raster::stack(nc)
        } else if(p == "Silicates_mld") {
            nc <- "woa18_all_i_allmonths_mld_average_SODA3.4.2.nc"
            ras <- raster::stack(nc)
        } else if(p == "Silicates_mld.grad") {
            nc <- "woa18_all_i_allmonths_mld_gradient.nc"
            ras <- raster::stack(nc)
        } # eo 1st if else loop - depends on 'p'
               
        # 2nd if else loop depdening on extent (some .nc have faulty coordinates when you load them through raster functions)
        if( extent(ras)@xmax == 180 ) {
        
                ddf <- as.data.frame(ras, xy = T)
               
                if( length(ddf) == 14 ) {
                    
                    colnames(ddf) <- c("x","y","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
                    m.ddf <- melt(ddf, id.vars = c("x","y"))
                    colnames(m.ddf) <- c("x","y","Month","value")
                    m.ddf$variable <- p
                    m.ddf$Month <- as.character(m.ddf$Month)
                    m.ddf[m.ddf$Month == "Jan","Month"] <- "01"
                    m.ddf[m.ddf$Month == "Feb","Month"] <- "02"
                    m.ddf[m.ddf$Month == "Mar","Month"] <- "03"
                    m.ddf[m.ddf$Month == "Apr","Month"] <- "04"
                    m.ddf[m.ddf$Month == "May","Month"] <- "05"
                    m.ddf[m.ddf$Month == "Jun","Month"] <- "06"
                    m.ddf[m.ddf$Month == "Jul","Month"] <- "07"
                    m.ddf[m.ddf$Month == "Aug","Month"] <- "08"
                    m.ddf[m.ddf$Month == "Sep","Month"] <- "09"
                    m.ddf[m.ddf$Month == "Oct","Month"] <- "10"
                    m.ddf[m.ddf$Month == "Nov","Month"] <- "11"
                    m.ddf[m.ddf$Month == "Dec","Month"] <- "12"
                    
                    m.ddf$Month <- as.numeric(m.ddf$Month)
                    
                    # m.ddf$Month <- factor(m.ddf$Month)
                    
                } else {
                    
                    message(paste("WRONG DIMENSIONS - NOT X/Y/12 MONTHS", sep = ""))
                    
                } # eo 3rd if else loop
                
                rm(ras,ddf) ; gc()
        
        } else {
        
                # If wrong spatial extent: load the values using ncdf4 functions instead of raster functions
                rm(ras); gc()
                var_nc <- nc_open(nc)  # var_nc
                
                if( length( names(var_nc$var) ) == 1 ) {
                  var_nam <- names(var_nc$var)
                  var_val <- ncvar_get(var_nc, var_nam)  # dim(var_val) ; summary(var_val)
                }
                # # Melt matrix
#                 m.ddf <- melt(var_val)
#                 # head(m.ddf) ; summary(m.ddf) # WRONG VALUES IN DIMENSIONS
#
#                 m.ddf <- data.frame(c(var_val))
#                 colnames(m.ddf)[1] <- p
#                 # Add lon and lat columns
#                 # str(var_nc$dim$lon)
#                 # unique(var_nc$dim$lon$vals)
#                 # unique(var_nc$dim$lat$vals)
#                 # var_nc$dim$time$vals
#
#                 m.ddf["x"] = rep(var_nc$dim$lon$vals)
#                 m.ddf["y"] = rep(var_nc$dim$lat$vals, each = 360)
#                 # Add a month column to env_df
#                 m.ddf["Month"] = rep(var_nc$dim$time$vals, each = 360*180)
#                 # Sort
#                 m.ddf <- m.ddf[with(m.ddf, order(get("x"),get("y"),Month)),]
#                 # head(m.ddf)
#                 # Order should be: x,y,month,value,varname
#                 colnames(m.ddf)[1] <- "value"
#                 m.ddf$variable <- p
#                 # Reorder columns by their index
#                 m.ddf <- m.ddf[,c("x","y","Month","value","variable")]
#                 m.ddf$Month <- as.numeric(m.ddf$Month)
                
                # Check dimensions so that they are all in the same order (month, lat, lon)
                if( sum((dim(var_val) == c(360,180,12))) == 3 ) {
                  var_val = var_val
                  env_sub_df = data.frame(c(var_val))
                  colnames(env_sub_df)[1] <- p
                  # Add lon and lat columns
                  env_sub_df["x"] = rep(-179.5:179.5)
                  env_sub_df["y"] = rep(-89.5:89.5, each = 360)
                  # Add a month column to env_df
                  env_sub_df["Month"] = rep(1:12, each = 360*180)
                  # Sort it
                  m.ddf <- env_sub_df[with(env_sub_df, order(get("x"), get("y"), Month)),]
                  rm(env_sub_df) ; gc()
                  
                  # Return m.ddf in correct order: x    y Month    value variable
                  m.ddf <- melt(m.ddf, id.vars = c("x","y","Month"))

                } else if( sum((dim(var_val) == c(12,360,180))) == 3 ) {
                  var_val = aperm(var_val, c(2, 3, 1))
                  env_sub_df = data.frame(c(var_val))
                  colnames(env_sub_df)[1] <- p
                  # Add lon and lat columns
                  env_sub_df["x"] = c(var_nc$dim$lon$vals) #rep(-179.5:179.5)
                  env_sub_df["y"] = rep(var_nc$dim$lat$vals, each = length(var_nc$dim$lon$vals)) #rep(89.5:-89.5, each = 360)
                   #Add a month column to env_df
                  env_sub_df["Month"] = rep(1:12, each = 360*180)
                  # Sort it
                  m.ddf <- env_sub_df[with(env_sub_df, order(get("x"), get("y"), Month)),]
                  rm(env_sub_df) ; gc()
                  
                  # Return m.ddf in correct order: x    y Month    value variable
                  m.ddf <- melt(m.ddf, id.vars = c("x","y","Month"))
                  
                } # eo if else loop to extract proper coordinates (re-sample on WOA grid after)
                    
                
                ### Re-sample grid points (e.g. attrbute grid cells to obs) using cellFromXY
                # ?cellFromXY 
                coords2 <- m.ddf[,c("x","y")]
                cells <- cellFromXY(grid, as.matrix(coords2))
                # cells <- unique(cells) # ; cells
                # Extract X and Y coordinates from 'ras' 
                m.ddf$x <- raster::xyFromCell(object = grid, cell = cells)[,"x"]
                m.ddf$y <- raster::xyFromCell(object = grid, cell = cells)[,"y"]
                # head(m.ddf)
                # summary(m.ddf) # looks good
                    
                ### Close nc 
                nc_close(var_nc)                  
                
            } # eo 2nd if else loop    
            
            ### Return m.ddf
            return(m.ddf)
        
    }
    
) # eo lapply

### And into darkness rbind them
clims <- bind_rows(res.clims)
dim(clims)
str(clims)
unique(clims$variable)
# Make room
rm(res.clims) ; gc()

### Add cell id and check these are unique across all variables 
# unique(clims$x) # problem
# unique(clims$y) # looks good too
clims$id <- factor(paste(clims$x, clims$y, sep = "_"))
length(unique(clims$id)) # should be 64800 

# Make sure they all follow the same order (id+Month)
# res.ord <- lapply(unique(clims$variable), function(v) {
#             # Subset
#             sub <- clims[clims$variable == v,]
#             # Reorder
#             sub <- sub[with(sub,order(id,Month)),]
#             # Return
#             return(sub)
#         }
# ) # eo lapply
# ord.clims <- bind_rows(res.ord)
# dim(ord.clims)
# head(ord.clims) # length(unique(ord.clims$id))
# rm(clims); gc()
### Convert back the Months to numbers
# levels(ord.clims$Month)
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Jan"] <- "01"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Feb"] <- "02"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Mar"] <- "03"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Apr"] <- "04"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "May"] <- "05"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Jun"] <- "06"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Jul"] <- "07"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Aug"] <- "08"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Sep"] <- "09"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Oct"] <- "10"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Nov"] <- "11"
# levels(ord.clims$Month)[levels(ord.clims$Month) == "Dec"] <- "12"


### Put the variables as columsn using dcast
# head(clims)
d.clims <- dcast(data = clims, formula = id + x + y + Month ~ variable, value.var = "value")#, #fun.aggregate = mean, fill = NA)
dim(d.clims) # should have 64800*12 = 777600 rows
head(d.clims) # unique(d.clims$Month)

# Re-order Month levels
# d.clims$Month <- factor(d.clims$Month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"))

# Make maps to check visually
# v <- "CHLA" # for testing
for( v in colnames(d.clims)[5:length(d.clims)] ) {
    
    message(paste("Mapping ", v, sep = ""))
    
    maps <- ggplot() + geom_tile(aes(x = x, y = y, fill = get(v)), data = d.clims) + coord_quickmap() +
        geom_polygon(aes(x = long, y = lat, group = group),
        data = world[world$long <= 180,], fill = "grey85", colour = "grey50", size = 0.3) +
        scale_fill_viridis(name = v) + 
        scale_x_continuous(name = "", breaks = c(-180,-120,-60,0,60,120,180),
        labels = c("180°","120°W","60°W","GM","60°E","120°E","180°"), expand = c(0,0)) +
        scale_y_continuous(name = "", breaks = c(-90,-60,-30,0,30,60,90),
        labels = c("-90°N","-60°N","-30°N","Eq","30°N","60°N","90°N"), expand = c(0,0)) +
        theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
        panel.grid.major = element_line(colour = "white",linetype = "dashed"), legend.position = "bottom") +
        facet_wrap(.~factor(Month))
    #
    ggsave(plot = maps, filename = paste("panel_maps_mon_",v,"_22_03_22.jpg", sep = ""), dpi = 300, width = 15, height = 15)
    
} # eo for loop 

### Looks good! Save "d.clims" as .csv file and make a raster stack out of it too
head(d.clims)
str(d.clims)
write.csv(d.clims, file = "table_monthly_climatologies_N.Knecht_22_03_22.csv", sep = ";")

d <- read.csv("table_monthly_climatologies_N.Knecht_22_03_22.csv")
head(d)
str(d)
summary(d)


### 22/03/22: Re-load table and make a nice raster out of it
p <- "SST"
for(p in colnames(d)[c(6:length(d))] ) {
    
            message(paste("Converting monthly climatologies of ",p," into rasters stacks", sep = ""))
    
            # Subset months and convert all 12 into rasters to stack
            sub.p <- d[,c("x","y","Month",p)]
            
            r1 <- sub.p[sub.p$Month == 1,c("x","y",p)]
            r2 <- sub.p[sub.p$Month == 2,c("x","y",p)]
            r3 <- sub.p[sub.p$Month == 3,c("x","y",p)]
            r4 <- sub.p[sub.p$Month == 4,c("x","y",p)]
            r5 <- sub.p[sub.p$Month == 5,c("x","y",p)]
            r6 <- sub.p[sub.p$Month == 6,c("x","y",p)]
            r7 <- sub.p[sub.p$Month == 7,c("x","y",p)]
            r8 <- sub.p[sub.p$Month == 8,c("x","y",p)]
            r9 <- sub.p[sub.p$Month == 9,c("x","y",p)]
            r10 <- sub.p[sub.p$Month == 10,c("x","y",p)]
            r11 <- sub.p[sub.p$Month == 11,c("x","y",p)]
            r12 <- sub.p[sub.p$Month == 12,c("x","y",p)]
           
            # Convert to raster objects
            coordinates(r1) <- ~ x + y ; gridded(r1) <- TRUE; r1 <- raster(r1)
            coordinates(r2) <- ~ x + y ; gridded(r2) <- TRUE; r2 <- raster(r2)
            coordinates(r3) <- ~ x + y ; gridded(r3) <- TRUE; r3 <- raster(r3)
            coordinates(r4) <- ~ x + y ; gridded(r4) <- TRUE; r4 <- raster(r4)
            coordinates(r5) <- ~ x + y ; gridded(r5) <- TRUE; r5 <- raster(r5)
            coordinates(r6) <- ~ x + y ; gridded(r6) <- TRUE; r6 <- raster(r6)
            coordinates(r7) <- ~ x + y ; gridded(r7) <- TRUE; r7 <- raster(r7)
            coordinates(r8) <- ~ x + y ; gridded(r8) <- TRUE; r8 <- raster(r8)
            coordinates(r9) <- ~ x + y ; gridded(r9) <- TRUE; r9 <- raster(r9)
            coordinates(r10) <- ~ x + y ; gridded(r10) <- TRUE; r10 <- raster(r10)
            coordinates(r11) <- ~ x + y ; gridded(r11) <- TRUE; r11 <- raster(r11)
            coordinates(r12) <- ~ x + y ; gridded(r12) <- TRUE; r12 <- raster(r12)
            
            # Add CRS
            crs(r1) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r2) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r3) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r4) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r5) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r6) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r7) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r8) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r9) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r10) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r11) <- "+proj=longlat +datum=WGS84 +no_defs"
            crs(r12) <- "+proj=longlat +datum=WGS84 +no_defs"
    
            # Stack all 12 monthly rasters
            raster.stack.mon <- raster()
            raster.stack.mon <- brick(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12)
            # raster.stack.mon 
            # plot(raster.stack.mon)
            names(raster.stack.mon) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
            
            # Save
            message(paste("Saving monthly climatologies of ",p," as a raster file", sep = ""))
            writeRaster(raster.stack.mon, paste("raster_mon_clim_",p,"_22_03_22.grd", sep = ""), format = "raster")
            message(paste("", sep = ""))
            
            # Return raster layer
            rm(sub.p, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)
            gc()
             
} # eo FUN
    
### Nice. Now try to make a .nc file out of all of them...
# Test
# setwd( here("all_env_vars_all_months_NK_17_03_22/raster_files/") ) ; dir()
# test <- raster::stack("raster_mon_clim_Wind.speed_22_03_22.grd")
# test
# plot(log10(test))
# rm(test) ; gc()


### ----------------------------------------------------------------------------------------------------------------------------

### 30/03/22: Put all raster layers into a common netCDF (derived from Script#8.1), starting from the .csv file

library("tidyverse")
library("reshape2")
library("lubridate")
library("xlsx")
library("ncdf4")
library("raster")
library("here")

setwd( here("all_env_vars_all_months_NK_17_03_22/") ) ; dir()

d <- read.csv("table_monthly_climatologies_N.Knecht_22_03_22.csv")
head(d)
str(d)
d <- d[,c(2:length(d))]
variables <- colnames(d)[c(5:length(d))] # 33 vars
variables

### Define dimensions of the netcdf:
setwd( here("all_env_vars_all_months_NK_17_03_22/netCDF_files") ) ; dir()
ncin <- nc_open("woa18_decav_t_allmonths_surfaceSST.nc")
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
# time dimension 
time <- ncvar_get(ncin,"time")
nt <- dim(time)
# close netcdf
nc_close(ncin)
gc()


### Use 'd' above, create the netCDF file and fill it with all your variable(s) of interest.
### Ok, let's try the 1st strategy: create an empty .nc file 
ncpath <- getwd() # current working directory - feel free to modify this as you will
ncname <- "all_monthly_env_climatologies_N.Knecht_30_03_22"  
ncfname <- paste(ncpath,"/", ncname, ".nc", sep = "")
# dname <- "concentration"  # 'val' 
# Then define the contents of the file:
londim <- ncdim_def("lon","degrees_east", as.double(lon) ) 
latdim <- ncdim_def("lat","degrees_north", as.double(lat) ) 
timedim <- ncdim_def("time","months", as.double(time) )

### Before filling the .nc file with all the variables, create an initial one. Let's just take variables[1].
array <- array(NA, dim = c(nlon,nlat,nt) )

subset <- d[,c("x","y","Month",variables[1])] # dim(subset)
i2 <- sapply(subset$x, function(x) which.min(abs(lon - x)) ) 
j2 <- sapply(subset$y, function(x) which.min(abs(lat - x)) ) 
k2 <- sapply(subset$Month, function(x) which(time == x) ) 

# To check: head(val_array[cbind(j2,k2,l2,m2)])
array[cbind(i2,j2,k2)] <- as.matrix(subset[,variables[1]]) 
# summary(array) # Nice, seems to have worked.

# Define the variable(s)
fillvalue <- NA
varname <- variables[1] # Name of the variable whose values are stored in 'ddf$value'
var_def <- ncvar_def(varname,"m-1", list(londim,latdim,timedim), missval = fillvalue, varname, prec = "float")
# var_def

### Create the .nc file in your directory
ncout <- nc_create(ncfname, list(var_def), force_v4 = T) # add variables in the 'list()'
# OK

# Put variable #1
ncvar_put(ncout,var_def,array)
# Check content
print(ncout)
names(ncout$var) # vector of variables names inside nc

### 30/03/22: Add ncatt_put for varable 'v' based on 'varid' argument?
# ?ncatt_put
# attname: Name of the attribute to write. 
# attval: Attribute to write.
### Attributes to add: 
# - full varname 
ncatt_put(ncout, varid = varname, attname = "Full name", attval = "Particulate backscattering coefficient at 443nm")
# - unit
ncatt_put(ncout, varid = varname, attname = "Unit", attval = "m-1")
# - depth layer(s) covered
ncatt_put(ncout, varid = varname, attname = "Depth coverage", attval = "Surface")
# - time coverage (years)
ncatt_put(ncout, varid = varname, attname = "Time coverage", attval = "1997-2010")
# - source (url link)
ncatt_put(ncout, varid = varname, attname = "Source", attval = "SeaWiFS Mission page 2019; https://oceandata.sci.gsfc.nasa.gov/directaccess/SeaWiFS/Mapped/Monthly_Climatology/9km/bbp_443_gsm/")
# - reference/DOI
ncatt_put(ncout, varid = varname, attname = "Reference", attval = "NASA Goddard Space Flight Center, Ocean Ecology Laboratory, Ocean Biology Processing Group; (2018): Sea-viewing Wide Field-of-view Sensor (SeaWiFS) Garver-Siegel-Maritorena (GSM) Model Data, NASA OB.DAAC. doi: 10.5067/ORBVIEW-2/SEAWIFS/L3M/GSM/2018. Accessed on 03/17/2022")

 
### Put additional attributes to the whole netCDF
ncatt_put(ncout,"lon","axis","decimalLongitude", verbose = T) 
ncatt_put(ncout,"lat","axis","decimalLatitude")
ncatt_put(ncout,"time","axis","Month")

# add global attributes
ncatt_put(ncout, varid = 0, attname = "Data", attval = "AtlantECO-WP2-Environmental climatologies") # if varid==0, then a global attribute is written instead of a variable's attribute
ncatt_put(ncout, varid = 0,"Institution", "ETH Zürich, D-USYS, IBP, UP group")
ncatt_put(ncout, varid = 0,"Description", "Monthly climatologies of 33 environmental predictors implemented by Nielja Knecht during her Master thesis")
ncatt_put(ncout, varid = 0, "Funding statement","This project has received funding from the European Union’s Horizon 2020 research and innovation programme under grant agreement no. 862923. This output reflects only the author’s view, and the European Union cannot be held responsible for any use that may be made of the information contained therein.")

history <- paste("Fabio Benedetti (fabio.benedetti@usys.ethz.ch); last update: ", date(), sep = ", ")
ncatt_put(ncout, varid = 0, "Last update by:",history)

### Check
print(ncout) # Nice!

# Need to close it before being able to write new var into it (...)
nc_close(ncout)
# Clear some RAM
rm(array,var_def,varname,subset,i2,j2,k2) ; gc()


### Open it again but in writing mode
ncout <- nc_open("all_monthly_env_climatologies_N.Knecht_30_03_22.nc", write = TRUE)
# print(ncout)

### Now, this is where you start adding each variables in this netCDF - try it through a mclapply :-)
# require("parallel")
# v <- variables[13]
lapply(X = variables[2:4], function(v) {
    
            ### Message
            message(paste("Preparing ",v," to be added into the .nc file (defining attributes, data etc.)", sep = ""))
            
            # Define the attributes based on 'v' and the info provided by Nielja Knecht in the excel sheet:
            # full varname, unit, depth layer(s) covered, time covarage, source, reference  
            if(v == "CHLA") {
                
                full.name <- "Surface chlorophyll a concentration"
                unit <- "mg/m3"
                depth <- "Surface"
                coverage <- "1997-2010"
                source <- "SeaWiFS Mission page 2019; https://oceandata.sci.gsfc.nasa.gov/directaccess/SeaWiFS/Mapped/Monthly_Climatology/9km/chlor_a/"
                ref <- "NASA Goddard Space Flight Center, Ocean Ecology Laboratory, Ocean Biology Processing Group; (2018): Sea-viewing Wide Field-of-view Sensor (SeaWiFS) Chlorophyll Data, NASA OB.DAAC. doi: 10.5067/ORBVIEW-2/SEAWIFS/L3M/CHL/2018. Accessed on 03/17/2022"
                
            } else if(v == "DIC") {      
                
                full.name <- "Surface dissolved inorganic carbon concentration"
                unit <- "µmol/kg"
                depth <- "Surface"
                coverage <- "1982-2020"
                source <- "OceanSODA - ETHZ; https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0220059"
                ref <- "Gregor, L., & Gruber, N. (2021). OceanSODA-ETHZ: a global gridded data set of the surface ocean carbonate system for seasonal to decadal studies of ocean acidification. Earth System Science Data, 13(2), 777–808."
                
            } else if(v == "pCO2") {          
                
                full.name <- "Partial pressure of carbon dioxyde (CO2)"
                unit <- "µatm"
                depth <- "Surface"
                coverage <- "1982-2020"
                source <- "OceanSODA - ETHZ; https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0220059"
                ref <- "Gregor, L., & Gruber, N. (2021). OceanSODA-ETHZ: a global gridded data set of the surface ocean carbonate system for seasonal to decadal studies of ocean acidification. Earth System Science Data, 13(2), 777–808."
                
            } else if(v == "Revelle") {     
                
                full.name <- "Revelle (buffer) factor (ratio of instataneous change in CO2 to the change in total DIC)"
                unit <- "unitless"
                depth <- "Surface"
                coverage <- "1982-2020"
                source <- "OceanSODA - ETHZ; https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0220059"
                ref <- "Gregor, L., & Gruber, N. (2021). OceanSODA-ETHZ: a global gridded data set of the surface ocean carbonate system for seasonal to decadal studies of ocean acidification. Earth System Science Data, 13(2), 777–808."
                
            } else if(v == "Omega_Calcite") {
                
                full.name <- "Calcite saturation state (>1.0 when supersaturated)"
                unit <- "unitless"
                depth <- "Surface"
                coverage <- "1982-2020"
                source <- "OceanSODA - ETHZ; https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0220059"
                ref <- "Gregor, L., & Gruber, N. (2021). OceanSODA-ETHZ: a global gridded data set of the surface ocean carbonate system for seasonal to decadal studies of ocean acidification. Earth System Science Data, 13(2), 777–808."
                
            } else if(v == "Omega_Aragonite") {
                
                full.name <- "Aragonite saturation state (>1.0 when supersaturated)"
                unit <- "unitless"
                depth <- "Surface"
                coverage <- "1982-2020"
                source <- "OceanSODA - ETHZ; https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0220059"
                ref <- "Gregor, L., & Gruber, N. (2021). OceanSODA-ETHZ: a global gridded data set of the surface ocean carbonate system for seasonal to decadal studies of ocean acidification. Earth System Science Data, 13(2), 777–808."
                
            } else if(v == "Dist2coast") {  
                
                full.name <- "Haversine distance to closest coast (i.e., isobath 0m) derived from bathymetry data"
                unit <- "km"
                depth <- "Surface"
                coverage <- "-"
                source <- "ETOPO1 1 Arc-Minute Global Relief Model; https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ngdc.mgg.dem:316"
                ref <- "NOAA National Geophysical Data Center. 2009: ETOPO1 1 Arc-Minute Global Relief Model. NOAA National Centers for Environmental Information."
                
            } else if(v == "SST") { 
                
                full.name <- "Sea surface temperature"
                unit <- "°C"
                depth <- "Surface"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18.pl?parameter=t"
                ref <- "Locarnini, R. A., A. V. Mishonov, O. K. Baranova, T. P. Boyer, M. M. Zweng, H. E. Garcia, J. R. Reagan, D. Seidov, K. Weathers, C. R. Paver, and I. Smolyar, 2018. World Ocean Atlas 2018, Volume 1: Temperature. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 81, 52pp."
                
            } else if(v == "Temp_mld") { 
                
                full.name <- "Mean temperature over the MLD"
                unit <- "°C"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18.pl?parameter=t"
                ref <- "Locarnini, R. A., A. V. Mishonov, O. K. Baranova, T. P. Boyer, M. M. Zweng, H. E. Garcia, J. R. Reagan, D. Seidov, K. Weathers, C. R. Paver, and I. Smolyar, 2018. World Ocean Atlas 2018, Volume 1: Temperature. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 81, 52pp."
                
            } else if(v == "Temp_mld.grad") {   
                
                full.name <- "Difference between surface temperature and temperature at MLD"
                unit <- "°C"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18.pl?parameter=t"
                ref <- "Locarnini, R. A., A. V. Mishonov, O. K. Baranova, T. P. Boyer, M. M. Zweng, H. E. Garcia, J. R. Reagan, D. Seidov, K. Weathers, C. R. Paver, and I. Smolyar, 2018. World Ocean Atlas 2018, Volume 1: Temperature. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 81, 52pp."
            
            } else if(v == "SSS") {  
                
                full.name <- "Sea surface salinity"
                unit <- "unitless"
                depth <- "Surface"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18.pl?parameter=s"
                ref <- "Zweng, M. M., J. R. Reagan, D. Seidov, T. P. Boyer, R. A. Locarnini, H. E. Garcia, A. V. Mishonov, O. K. Baranova, K. Weathers, C. R. Paver, and I. Smolyar, 2018. World Ocean Atlas 2018, Volume 2: Salinity. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 82, 50pp."
                
            } else if(v == "Sal_mld") {  
                
                full.name <- "Mean salinity over the MLD"
                unit <- "unitless"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18.pl?parameter=s"
                ref <- "Zweng, M. M., J. R. Reagan, D. Seidov, T. P. Boyer, R. A. Locarnini, H. E. Garcia, A. V. Mishonov, O. K. Baranova, K. Weathers, C. R. Paver, and I. Smolyar, 2018. World Ocean Atlas 2018, Volume 2: Salinity. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 82, 50pp."
                  
            } else if(v == "MLD") {  
                
                full.name <- "Mixed layer depth"
                unit <- "m"
                depth <- "Surface to 2000m depth"
                coverage <- "1980-2018"
                source <- "SODA 3.4.2 https://www2.atmos.umd.edu/~ocean/index_files/soda3.4.2_mn_download_b.htm"
                ref <- "Carton, J. A., Chepurin, G. A., & Chen, L. (2018). SODA3: A new ocean climate reanalysis. Journal of Climate, 31(17), 6967–6983. https://doi.org/10.1175/jcli-d-18-0149.1"
                
            } else if(v == "Wind.speed") {
                
                full.name <- "Surface wind speed"
                unit <- "m/s"
                depth <- "Surface"
                coverage <- "1987-2011"
                source <- "https://podaac.jpl.nasa.gov/"
                ref <- "Atlas, R., Hoffman, R. N., Ardizzone, J., Leidner, S. M., Jusem, J. C., Smith, D. K., & Gombos, D. (2011). A Cross-calibrated, Multiplatform Ocean Surface Wind Velocity Product for Meteorological and Oceanographic Applications. Bulletin of the American Meteorological Society, 92(2), 157-174. doi:10.1175/2010bams2946.1"
                
            } else if(v == "EddyKineticEnergy") {
                
                full.name <- "Eddy Kinetic Energy"
                unit <- "m2/s2"
                depth <- "Surface"
                coverage <- "1993-2012"
                source <- "https://cds.climate.copernicus.eu/cdsapp#/dataset/satellite-sea-level-global?tab=overview"
                ref <- "Computed from northward and eastward geostrophic velocities (assuming sea level for geoid) following: Qiu, B., & Chen, S. (2004). Seasonal Modulations in the Eddy Field of the South Pacific Ocean. Journal of Physical Oceanography, 34(7), 1515-1527. doi:10.1175/1520-0485(2004)034<1515:Smitef>2.0.Co;2"
                
            } else if(v == "PAR") {
                
                full.name <- "Photosynthetically active radiation"
                unit <- "µmol/m2.s"
                depth <- "Surface"
                coverage <- "1997-2010"
                source <- "SeaWiFS Mission page 2019;https://oceandata.sci.gsfc.nasa.gov/directaccess/SeaWiFS/Mapped/Monthly_Climatology/9km/par/"
                ref <- "NASA Goddard Space Flight Center, Ocean Ecology Laboratory, Ocean Biology Processing Group; (2018): Sea-viewing Wide Field-of-view Sensor (SeaWiFS) Photosynthetically Available Radiation Data, NASA OB.DAAC. doi: 10.5067/ORBVIEW-2/SEAWIFS/L3M/PAR/2018. Accessed on 2019/02/29."
                
            } else if(v == "Zeu") {
                
                full.name <- "Depth of the euphotic zone"
                unit <- "m"
                depth <- "Surface (satellite product)"
                coverage <- "1997-2010"
                source <- "SeaWiFS Mission page 2019; https://oceandata.sci.gsfc.nasa.gov/directaccess/SeaWiFS/Mapped/Monthly_Climatology/9km/Zeu_lee/"
                ref <- "NASA Goddard Space Flight Center, Ocean Ecology Laboratory, Ocean Biology Processing Group; (2018): Sea-viewing Wide Field-of-view Sensor (SeaWiFS) Euphotic Depth Data, NASA OB.DAAC. doi: 10.5067/ORBVIEW-2/SEAWIFS/L3M/ZLEE/2018. Accessed on 03/17/2022."
                
            } else if(v == "O2") {
                
                full.name <- "Dissolved oxygen concentration"
                unit <- "µmol/kg"
                depth <- "Surface"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=o"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 3: Dissolved Oxygen, Apparent Oxygen Utilization, and Oxygen Saturation. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 83, 38pp."
                
            } else if(v == "O2_200m") {
                
                full.name <- "Dissolved oxygen concentration at 200m depth"
                unit <- "µmol/kg"
                depth <- "200m"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=o"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 3: Dissolved Oxygen, Apparent Oxygen Utilization, and Oxygen Saturation. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 83, 38pp."
                
            } else if(v == "O2_mld") {
                
                full.name <- "Mean dissolved oxygen concentration over the MLD"
                unit <- "µmol/kg"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=o"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 3: Dissolved Oxygen, Apparent Oxygen Utilization, and Oxygen Saturation. A. Mishonov Technical Ed.; NOAA Atlas NESDIS 83, 38pp."
                
            } else if(v == "PIC") {
                
                full.name <- "Surface particulate inorganic carbon concentration"
                unit <- "mol/m3"
                depth <- "Surface"
                coverage <- "1997-2010"
                source <- "SeaWiFS Mission page 2019; https://oceandata.sci.gsfc.nasa.gov/directaccess/SeaWiFS/Mapped/Monthly_Climatology/9km/pic/"
                ref <- "NASA Goddard Space Flight Center, Ocean Ecology Laboratory, Ocean Biology Processing Group; (2018): Sea-viewing Wide Field-of-view Sensor (SeaWiFS) Particulate Inorganic Carbon Data, NASA OB.DAAC. doi: 10.5067/ORBVIEW-2/SEAWIFS/L3M/PIC/2018. Accessed on 03/17/2022."
                
            } else if(v == "Kd490") {
                
                full.name <- "Diffuse attenuation coefficient for downwelling irradiance at 490 nm"
                unit <- "m-1"
                depth <- "Surface"
                coverage <- "1997-2010"
                source <- "SeaWiFS Mission page 2019; https://oceandata.sci.gsfc.nasa.gov/directaccess/SeaWiFS/Mapped/Monthly_Climatology/9km/Kd_490/"
                ref <- "NASA Goddard Space Flight Center, Ocean Ecology Laboratory, Ocean Biology Processing Group; (2018): Sea-viewing Wide Field-of-view Sensor (SeaWiFS) Downwelling Diffuse Attenuation Coefficient Data, NASA OB.DAAC. doi: 10.5067/ORBVIEW-2/SEAWIFS/L3M/KD/2018. Accessed on 03/17/2022."
                
            } else if(v == "TAlk") {
                
                full.name <- "Total alkalinity"
                unit <- "µmol/kg"
                depth <- "Surface"
                coverage <- "1982-2020"
                source <- "OceanSODA - ETHZ; https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0220059"
                ref <- "Gregor, L., & Gruber, N. (2021). OceanSODA-ETHZ: a global gridded data set of the surface ocean carbonate system for seasonal to decadal studies of ocean acidification. Earth System Science Data, 13(2), 777–808."
                
            } else if(v == "Nitrates") {
                
                full.name <- "Surface nitrates concentration"
                unit <- "µmol/kg"
                depth <- "Surface"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=n"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } else if(v == "Nitrates_mld") {
                
                full.name <- "Mean nitrates concentration over the MLD"
                unit <- "µmol/kg"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=n"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } else if(v == "Nitrates_mld.grad") {
                
                full.name <- "Difference between surface nitrates concentration and concentration at MLD"
                unit <- "µmol/kg"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=n"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } else if(v == "Phosphates") {
                
                full.name <- "Surface phosphates concentration"
                unit <- "µmol/kg"
                depth <- "Surface"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=p"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } else if(v == "Phosphates_mld") {
                
                full.name <- "Mean phosphates concentration over the MLD"
                unit <- "µmol/kg"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=p"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } else if(v == "Phosphates_mld.grad") {
                
                full.name <- "Difference between surface phosphates concentration and concentration at MLD"
                unit <- "µmol/kg"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=p"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } else if(v == "Silicates") {
                
                full.name <- "Surface silicates concentration"
                unit <- "µmol/kg"
                depth <- "Surface"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=i"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } else if(v == "Silicates_mld") {
                
                full.name <- "Mean silicates concentration over the MLD"
                unit <- "µmol/kg"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=i"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } else if(v == "Silicates_mld.grad") {
                
                full.name <- "Difference between surface silicates concentration and concentration at MLD"
                unit <- "µmol/kg"
                depth <- "Surface to climatological MLD"
                coverage <- "1955-2018"
                source <- "World Ocean Atlas (WOA) 2018; https://www.ncei.noaa.gov/access/world-ocean-atlas-2018/bin/woa18oxnu.pl?parameter=i"
                ref <- "Garcia, H. E., K. Weathers, C. R. Paver, I. Smolyar, T. P. Boyer, R. A. Locarnini, M. M. Zweng, A. V. Mishonov, O. K. Baranova, D. Seidov, and J. R. Reagan, 2018. World Ocean Atlas 2018, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate and nitrate+nitrite, silicate). A. Mishonov Technical Ed.; NOAA Atlas NESDIS 84, 35pp."
                
            } # eo 1st if else loop - depends on 'v'
            
            # Define empty multidem array
            array <- array(NA, dim = c(nlon,nlat,nt) )

            subset <- d[,c("x","y","Month",v)] # dim(subset)
            i2 <- sapply(subset$x, function(x) which.min(abs(lon - x)) ) 
            j2 <- sapply(subset$y, function(x) which.min(abs(lat - x)) ) 
            k2 <- sapply(subset$Month, function(x) which(time == x) ) 
            
            # Assign values to empty array
            array[cbind(i2,j2,k2)] <- as.matrix(subset[,c(v)])      
            
            # Define var to be put in .nc
            var_def <- ncvar_def(v, unit, list(londim,latdim,timedim), missval = NA, v, prec = "float") 

            # Put variable v in netCDF file
            message(paste("Adding ",v," to the .nc file", sep = ""))
            # ?ncvar_add
            ncout <- ncvar_add(nc = ncout, v = var_def, verbose = T)
            ncvar_put(ncout, var_def, array)
            
            # Add attributes with ncatt_put
            message(paste("Adding attributes of ",v, sep = ""))
            ncatt_put(ncout, varid = v, attname = "Full name", attval = full.name)
            ncatt_put(ncout, varid = v, attname = "Unit", attval = unit)
            ncatt_put(ncout, varid = v, attname = "Depth coverage", attval = depth)
            ncatt_put(ncout, varid = v, attname = "Time coverage", attval = coverage)
            ncatt_put(ncout, varid = v, attname = "Source", attval = source)
            ncatt_put(ncout, varid = v, attname = "Reference", attval = ref)
    
            # Clear some RAM
            rm(array,var_def,subset,i2,j2,k2) ; gc()
            message(paste("", sep = ""))
            
        }
        
) # eo lapply

# Close to be able to check again
nc_close(ncout)

### Check one last time
nc.new <- nc_open("all_monthly_env_climatologies_N.Knecht_30_03_22.nc")
print(nc.new)
names(nc.new$var)
# Close if OK
nc_close(nc.new)


### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------