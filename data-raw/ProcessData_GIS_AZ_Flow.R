# Prepare data for example for AZ, GIS, NHD+ flow line, HI and LO gradient
#
# Erik.Leppo@tetratech.com
# 20180612
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)
library(rgdal)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZ_flow_HI.RDA" # 15.9 MB
load(file.path(wd, "data-raw", "GIS", myFile))#fc.HI.proj
data_GIS_Flow_HI <- fc.HI.proj

myFile <- "AZ_flow_LO.RDA" # 28.4 MB
load(file.path(wd, "data-raw", "GIS", myFile))#fc.LO.proj
data_GIS_Flow_LO <- fc.LO.proj


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
devtools::use_data(data_GIS_Flow_HI, overwrite = TRUE)
devtools::use_data(data_GIS_Flow_LO, overwrite = TRUE)

# HI, 87.7 MB compresses to 12.5 MB
# LO, 162.9 MB compresses to 22.9 MB
