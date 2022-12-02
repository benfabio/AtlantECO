
##### ATLANTECO SCRIPT 7.4 ----------------------------------------------------------------------------------------------------------------------------
##### 12/07/2022: R Script to convert abundance measurements to Carbon content measurements based on the Min/Max/Mean/Stdev individual C masses compiled for the various zooplankton groups/species based on previous compilations ©Fabio Benedetti, ETH Zürich, IBP, UP Group.

# - Load the dataset (excel sheet or .csv file) containing the individual C content estimates; load the abundance datasets 
# - use function or for loop to 

### Latest update: 12/07/2022

library("tidyverse")
library("reshape2")
library("readxl")
library("xlsx")
library("parallel")

### ----------------------------------------------------------------------------------------------------------------------------

### Groups whose point abundance measurements can be converted to biomass (in µgC.m-3 or mgC.m-3):
# - Copepoda (Hexanauplia) - waiting for permission from T. O'Brien (NOAA) to use data compiled in COPEPEDIA IndMass table
# - Euphausiacea (krill) - to do
# - Jellyfish (Cnidaria+Ctenophora) - to do
# - Chaetognatha - to do
# - Appendicularia (larvaceans) - to do 
# - Amphipoda - C conversion factors to establish
# - Ostracoda - C conversion factors to establish

groups <- c("Hexanauplia","Euphausiacea","Jellyfish","Chaetognatha","Appendicularia","Amphipoda","Ostracoda") # as they're encoded in the abundance datasets directory 

### ----------------------------------------------------------------------------------------------------------------------------

### 1°) Load the group-specific/species-specific C content estimates
setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/3_biomass")
# For all zooplankton groups BUT copepods
ind.c.zoo <- as.data.frame(read_excel("table_tally_ScientificNames_abund_zooplankton_except_Hexanauplia_20_06_22.xlsx"))
# For all copepods
ind.c.cops <- read.table("table_ind_c_masses_complete_Hexanauplia_08_07_22.txt", h = T, sep = "\t", dec = ".")

# Check if imported correctly
dim(ind.c.zoo); dim(ind.c.cops)
str(ind.c.zoo); str(ind.c.cops)
head(ind.c.zoo); head(ind.c.cops)
summary(ind.c.zoo)
summary(ind.c.cops)

# Mean.Carbon.Mass did not laod as numerics for some reason
ind.c.zoo$perc <- as.numeric(ind.c.zoo$perc)
ind.c.zoo$MinCarbonMass <- as.numeric(ind.c.zoo$MinCarbonMass)
ind.c.zoo$MaxCarbonMass <- as.numeric(ind.c.zoo$MaxCarbonMass)
ind.c.zoo$MeanCarbonMass <- as.numeric(ind.c.zoo$MeanCarbonMass)
ind.c.zoo$StdvCarbonMass <- as.numeric(ind.c.zoo$StdvCarbonMass)
summary(ind.c.zoo)
# Check NA again
ind.c.zoo[is.na(ind.c.zoo$MeanCarbonMass),] # eggs, polyp and 'Jellyfish'
ind.c.cops[is.na(ind.c.cops$Min),] # n = 1, hence no min/max/stdev


### 2°) Load the group's abundance data compilation and perform conversions based on the values reported in 
# Code a simple for loop (for g in 'groups') to convert abundance measurements to C measurements

# For testing for loop below: 
g <- "Chaetognatha"
# g <- "Jellyfish"
# g <- "Hexanauplia"

for(g in groups) {
    
    # Useless message
    message(paste("Converting abundances to biomasses for: ",g, sep = ""))
    
    if(g == "Hexanauplia") {
        ind.c <- ind.c.cops
    } else {
        # subset group of interest in 'ind.c.zoo'
        ind.c <- ind.c.zoo[which(ind.c.zoo$Group == g),]
    } # eo if els eloop based on 'g'
    
    # get abundance data for group of interest 
    setwd("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/2_abundance")
    file <- dir()[grep(paste("AtlantECO-BASEv1_dataset_",g,sep=""),dir())] # file
    abund <- get(load(file[grep("RData",file)]))
    # dim(abund) ; str(abund)
    
    # Missing a couple of columns in abund: MinBiomass/MaxBiomass/MeanBiomass/BiomassUnit/Reference
    abund <- subset(abund, select = -Biomass_mgCm3) 
    abund <- add_column(abund, MeanBiomass = NA, .before = "BiomassConvFactor") 
    abund <- add_column(abund, MaxBiomass = NA, .before = "MeanBiomass") 
    abund <- add_column(abund, MinBiomass = NA, .before = "MaxBiomass") 
    abund <- add_column(abund, BiomassUnit = NA, .after = "BiomassConvFactor") 
    abund <- add_column(abund, BiomassConvReference = NA, .after = "BiomassConvFactor") 
    # colnames(abund)
    
    # Now, just fill MeanBiomass/MaxBiomass/MinBiomass/BiomassUnit/BiomassConvRefence in 'abund' based on 'ind.c'
    # Watchout, for some groups like copeods, krill and jellyfish, carbon mass estimates are life-form specific, not the case for Appendicularia and Chaetognatha. If g %in% c("Chaetognatha","Appendicularia", ...), then just use ScientificNames to find matching C mass estimates, otherwise use ScientificNames x LifeForm combination
    
    if( g %in% c("Chaetognatha","Appendicularia","Amphipoda","Ostracoda") ) {
        
        # Check if names are common 
        if( length(setdiff(unique(abund$ScientificName), unique(ind.c$ScientificName))) == 0 ) { 
        
            # Use parallel computing to subset abund per ScientificNames 
            names <- unique(abund$ScientificName)
            # n <- names[13]
            res <- mclapply(names, function(n) {
                
                        message(paste("Attributing Min/Max/Mean biomass to: ",n, sep = ""))
                        
                        # Subset abund based on 'n'
                        sub <- abund[abund$ScientificName == n,]
                        sub.c <- ind.c[ind.c$ScientificName == n,]
                        
                        # Use 'sub.c' to attribute MeanBiomass/MaxBiomass/MinBiomass/BiomassUnit/BiomassConvRefence to 'sub'
                        sub$MeanBiomass <- (sub$MeasurementValue)*(sub.c$MeanCarbonMass)
                        sub$MinBiomass <- (sub$MeasurementValue)*(sub.c$MinCarbonMass)
                        sub$MaxBiomass <- (sub$MeasurementValue)*(sub.c$MaxCarbonMass)
                        sub$BiomassConvFactor <- unique(sub.c$MeanCarbonMass)
                        sub$BiomassConvReference <- unique(sub.c$Reference)
                        sub$BiomassUnit <- unique(sub.c$Unit)
                        
                        # Return
                        return(sub)
                
                    }, mc.cores = 20 # n = 20 or 25 on kryo
            
            ) # eo mclapply 
            # Rbind
            biom <- dplyr::bind_rows(res)
            rm(res); gc()
            
                
        } else { 
            
            message("Breaking loop because non matching elements between abund dataset & c mass dataset")
            break
            
        } # eo if else loop        
        
    } else {
        
        # Use ScientificName * LifeForm combination instead of ScientificNames alone
        ind.c$combination <- factor(paste(ind.c$ScientificName, ind.c$LifeForm, sep = "; "))
        abund$combination <- factor(paste(abund$ScientificName, abund$LifeForm, sep = "; "))
        
        # Check if combination are common to both datasets
        if( length(setdiff(unique(abund$combination), unique(ind.c$combination))) == 0 ) { 
        
            # Use parallel computing to subset abund per ScientificNames 
            names <- unique(abund$combination)
            # n <- names[3]
            res <- mclapply(names, function(n) {
                
                        message(paste("Attributing Min/Max/Mean biomass to: ",n, sep = ""))
                        
                        # Subset abund based on 'n'
                        sub <- abund[abund$combination == n,]
                        sub.c <- ind.c[ind.c$combination == n,]
                        
                        # Use 'sub.c' to attribute MeanBiomass/MaxBiomass/MinBiomass/BiomassUnit/BiomassConvRefence to 'sub'
                        sub$MeanBiomass <- (sub$MeasurementValue)*(sub.c$MeanCarbonMass)
                        sub$MinBiomass <- (sub$MeasurementValue)*(sub.c$MinCarbonMass)
                        sub$MaxBiomass <- (sub$MeasurementValue)*(sub.c$MaxCarbonMass)
                        sub$BiomassConvFactor <- unique(sub.c$MeanCarbonMass)
                        sub$BiomassConvReference <- unique(sub.c$Reference)
                        sub$BiomassUnit <- unique(sub.c$Unit)
                        
                        # Return
                        return(sub)
                
                    }, mc.cores = 20 # n = 20 or 25 on kryo
            
            ) # eo mclapply 
            # Rbind
            biom <- dplyr::bind_rows(res)
            rm(res); gc()
            
            # dim(biom) ; str(biom)
            
            # Drop the 'combination column'
            biom <- subset(biom, select = - combination)  
                
        } else { 
            
            message("Breaking loop because non matching elements between abund dataset & c mass dataset")
            break
            
        } # eo if else loop     
        
    } # eo if else loop - g
    
    setwd(paste("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/3_biomass/",g, sep = ""))
    message(paste("Saving converted dataset for: ",g, sep = ""))
    save(biom, file = paste("AtlantECO-BASEv1_dataset_",g,"_abundances+biomass_XX_07_22.RData", sep = ""))
    write.csv(biom, file = paste("AtlantECO-BASEv1_dataset_",g,"_abundances+biomass_XX_07_22.csv", sep = ""), sep = ";")
    
} # for g in 'groups'

setwd(paste("/net/kryo/work/fabioben/AtlantECO/AtlantECO_BASE/Traditional/AtlantECO-BASE1-datasets/3_biomass/", sep = ""))
dir()

### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------------------------------------