##########################################################################
### CODE FOR DOWNLOADING AND WRANGLING Foliar DATA FROM NEON DATA PORTAL
### Created Feb 28th, 2018
### Updated June 15, 2018
### S. Weintraub
### Modified for foiliar and root sampling by W. Wieder June 2019
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

# subset for foliar DPs only
foliarProds <- subset(products, grepl("foliar", products$productName, ignore.case = TRUE), 
                    select=c("productCode", "productName")) 
foliarProds # Pick out the DPs you want, then down below
rootProds <- subset(products, grepl("root", products$productName, ignore.case = TRUE), 
                      select=c("productCode", "productName")) 
rootProds # Pick out the DPs you want, then down below

### Download and 'stack' site-month files using the neonUtilities package
# devtools::install_github("NEONScience/NEON-utilities/neonUtilities", dependencies=TRUE, force=TRUE) # install package if needed 
library(neonUtilities) # load package
# download all data for a given product, then unzip, stack by table, and keep 'stacked' outputs

# Plant foliar physical and chemical properties, DP1.10026.001
{
  zipsByProduct(dpID="DP1.10026.001", site="all", package="basic", check.size=F)
  stackByTable(paste0(getwd(), "/filesToStack10026"), folder=T, saveUnzippedFiles = F)
}
# Root chemical properties, DP1.10102.001
{
  zipsByProduct(dpID="DP1.10102.001", site="all", package="basic", check.size=F)
  stackByTable(paste0(getwd(), "/filesToStack10102"), folder=T, saveUnzippedFiles = F)
}
# !!! Biomass only, no chemistry !!!!
# Root sampling (Megapit), DP1.10066  
#{
#  zipsByProduct(dpID="DP1.10066.001", site="all", package="basic", check.size=F)
#  stackByTable(paste0(getwd(), "/filesToStack10066"), folder=T, saveUnzippedFiles = F)
#}

# !!! No chemistry data here either !!!!
# Root sampling tower plots, DP1.10067
#{
#  zipsByProduct(dpID="DP1.10067.001", site="all", package="basic", check.size=F)
#  stackByTable(paste0(getwd(), "/filesToStack10067"), folder=T, saveUnzippedFiles = F)
#}


### Get physiographic data about the plots (slope, aspect, etc) using geoNEON package
# library(devtools) # load devTools if needed
# devtools::install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE) # install geoNeon
library(geoNEON) # load geoNEON
library(tidyverse)

# read in foliar files
cfc_cn <- read.csv(paste
                   (dir, "filesToStack10026/stackedFiles/cfc_carbonNitrogen.csv", 
                     sep = "/"), header = T)
names(cfc_cn)
cfc_ele <- read.csv(paste
                    (dir, "filesToStack10026/stackedFiles/cfc_elements.csv", 
                      sep = "/"), header = T)
names(cfc_ele)
cfc_var <- read.csv(paste
                    (dir, "filesToStack10026/stackedFiles/cfc_fieldData.csv", 
                      sep = "/"), header = T)
names(cfc_var)
cfc_var$scientificName
cfc_var$individualID
print(unique(cfc_var$scientificName))

# root chemistry data
bbc_chem <- read.csv(paste
                    (dir, "filesToStack10102/stackedFiles/bbc_rootChemistry.csv", 
                      sep = "/"), header = T)
names(bbc_chem)

# root data, but no chemistry! 
##bbc_chem2 <- read.csv(paste
#                     (dir, "filesToStack10067/stackedFiles/bbc_chemistryPooling.csv", 
#                       sep = "/"), header = T)
#bbc_mass <- read.csv(paste
#                      (dir, "filesToStack10067/stackedFiles/bbc_rootmass.csv", 
#                        sep = "/"), header = T)
#names(bbc_chem2)

# root sampling, tower plots, only biomass, no chemistry
#mpr_chem <- read.csv(paste
#                     (dir, "filesToStack10066/stackedFiles/mpr_perrootsample.csv", 
#                       sep = "/"), header = T)
#names(mpr_chem)

# use def.extr.geo.os function in geoNEON to get plot-level metadata
cfc <- def.extr.geo.os(cfc_cn, 'namedLocation')
bbc <- def.extr.geo.os(bbc_chem, 'namedLocation')
names(bbc)
# export csv files
write.csv(cfc, paste(dir, "spatial_cfc.csv", sep = "/"), row.names = F)
write.csv(bbc, paste(dir, "spatial_bbc.csv", sep = "/"), row.names = F)

################
#Start here if data already are local
################

### Make 'master' files with phys-chem combined plus possible predictor variables
library(tidyverse)
### Load Climate - will need to change file paths
##MAP <- read.csv(paste(dir2, "climate_precip.csv", sep = "/"), header = T, stringsAsFactors = F)
##MAT <- read.csv(paste(dir2, "climate_temp.csv", sep = "/"), header = T, stringsAsFactors = F)

# Join foliar CN and chemistry, "by" includes all common columns
foliar_chem1 <- left_join(x = cfc_cn, y = cfc_ele, 
                             by = c("domainID", "siteID","namedLocation","plotID",  
                                    "plotType"))
names(cfc_var)
foliar_chem  <- left_join(x = foliar_chem1, y = cfc_var,
                         by = c("domainID", "siteID","namedLocation","plotID",  
                                "plotType"))

# foliar N as % 'Percent nitrogen in a sample on a dry weight basis' 
# foliar P as % 'Concentration of phophorus in plant foliage sample on a dry mass basis'
hist(foliar_chem$nitrogenPercent)
hist(foliar_chem$foliarPhosphorusConc) 
foliar_chem$NP <- foliar_chem$nitrogenPercent / foliar_chem$foliarPhosphorusConc
hist(foliar_chem$NP[foliar_chem$NP<50]) 
boxplot(foliar_chem$NP[foliar_chem$NP<50] ~ 
          foliar_chem$siteID[foliar_chem$NP<50], 
        main = 'NEON foliar N:P', las=2) 
abline(h=16, lty=2)
boxplot(foliar_chem$NP[foliar_chem$NP<50] ~ 
          foliar_chem$scientificName[foliar_chem$NP<50], 
        main = 'NEON foliar N:P', xlab=NA) 
abline(h=16, lty=2)

names(foliar_chem)
print(unique(foliar_chem$siteID))

names(bbc_chem)
boxplot(bbc_chem$CNratio[bbc_chem$CNratio>10  & bbc_chem$CNratio<300] ~ 
          bbc_chem$siteID[bbc_chem$CNratio>10 & bbc_chem$CNratio<300],
        main = 'NEON root C:N', las=2, ) 
abline(h=42, lty=2)
print(unique(bbc_chem$siteID))

# size class data included in bbc_chem$poolSampleID
temp <- str_sub(as.character(bbc_chem$poolSampleID),25,29)
sizeMin <- sapply(strsplit(temp,"-"), `[`, 1)  #select first element
sizeMax <- sapply(strsplit(temp,"-"), `[`, 2)  #select second element
sizeMax <- sapply(strsplit(sizeMax,".", fixed = T), `[`, 1) #select text before dot
sizeMin[sizeMin=='05'] <- '0.5'
sizeMax[sizeMax=='05'] <- '0.5'
bbc_chem$sizeMin <- as.numeric(sizeMin)
bbc_chem$sizeMax <- as.numeric(sizeMax)
boxplot(bbc_chem$CNratio[bbc_chem$CNratio>10  & bbc_chem$CNratio<300] ~ 
          bbc_chem$sizeMax[bbc_chem$CNratio>10 & bbc_chem$CNratio<300],
        main = 'NEON root C:N', las=2, xlab='max size') 
abline(h=42, lty=2)

# Write the file
write.csv(foliar_chem, paste(dir2, "NEONfoliarChem_all.csv", sep = "/"), row.names = F)
write.csv(bbc_chem, paste(dir2, "NEONrootChem_all.csv", sep = "/"), row.names = F)


