##########################################################################
### CODE FOR DOWNLOADING AND WRANGLING Foliar DATA FROM NEON DATA PORTAL
### Created Feb 28th, 2018
### Updated June 15, 2018
### S. Weintraub
### Modified for foiliar and root sampling by W. Wieder June 2019
### Modified for litter chemistry by W. Wieder April 2020
##########################################################################

# Add working directory for additional user as needed
if (file.exists('/Users/wwieder/')){
  dir <- ("/Users/wwieder/Desktop/Working_files/INCyTE/NEONdata_files")
  dir2 <- (dir)
  setwd(dir)
}


### Use nneo package to get a list of NEON data products and their IDs
# install.packages("nneo") # install if needed, cran version
# devtools::install_github("ropenscilabs/nneo") # install if needed, development version
library(nneo)
# make a table of all NEON Data Products (DPs)
products <- nneo_products()

# subset for litter DPs only
litterProds <- subset(products, grepl("litter", products$productName, ignore.case = TRUE), 
                    select=c("productCode", "productName")) 
litterProds # Pick out the DPs you want, then down below


### Download and 'stack' site-month files using the neonUtilities package
# devtools::install_github("NEONScience/NEON-utilities/neonUtilities", dependencies=TRUE, force=TRUE) # install package if needed 
library(neonUtilities) # load package
# download all data for a given product, then unzip, stack by table, and keep 'stacked' outputs

# Litter chemical properties, DP1.10031.001
{
  zipsByProduct(dpID="DP1.10031.001", site="all", package="basic", check.size=F)
  stackByTable(paste0(getwd(), "/filesToStack10031"), folder=T, saveUnzippedFiles = F)
}



library(geoNEON) # load geoNEON
library(tidyverse)

# read in foliar files
ltr_cn <- read.csv(paste
                   (dir, "filesToStack10031/stackedFiles/ltr_litterCarbonNitrogen.csv", 
                     sep = "/"), header = T)
names(ltr_cn)
ltr_lig <- read.csv(paste
                    (dir, "filesToStack10031/stackedFiles/ltr_litterLignin.csv", 
                      sep = "/"), header = T)
names(ltr_lig)


# This is slow, and maybe not necessary
#--------------------------------------------
# use def.extr.geo.os function in geoNEON to get plot-level metadata
## ltr <- def.extr.geo.os(ltr_cn, 'namedLocation')
### names(ltr)
# export csv files
## write.csv(ltr, paste(dir, "spatial_ltr.csv", sep = "/"), row.names = F)
#--------------------------------------------

################
#Start here if data already are local
################

### Make 'master' files with phys-chem combined plus possible predictor variables
library(tidyverse)
### Load Climate - will need to change file paths
##MAP <- read.csv(paste(dir2, "climate_precip.csv", sep = "/"), header = T, stringsAsFactors = F)
##MAT <- read.csv(paste(dir2, "climate_temp.csv", sep = "/"), header = T, stringsAsFactors = F)

# Join foliar CN and chemistry, "by" includes all common columns
ltr_chem <- left_join(x = ltr_cn, y = ltr_lig, 
                             by = c("domainID", "siteID","namedLocation","plotID",  
                                    "plotType",'setDate',"collectDate"))

# Not sure how to best align CN & lignin data?
# also wondering how to consider leaves vs. needles
# weight results by litter fluxes collected throughout year?

names(ltr_chem)

# quick look at data
hist(ltr_chem$CNratio)
hist(ltr_chem$nitrogenPercent)
hist(ltr_chem$ligninPercent)
print(ltr_chem$cnSampleCode)

# group by siteID
boxplot(ltr_chem$CNratio ~ ltr_chem$siteID, 
        main = 'NEON litter C:N', las=2) 

boxplot(ltr_chem$ligninPercent ~ ltr_chem$siteID, 
        main = 'NEON litter lignin (%)', las=2) 


