# Prepare data for example for AZ, GIS, NHD+ flow line, HI and LO gradient
#
# Erik.Leppo@tetratech.com
# 20180612
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 20250923, EWL
# fix some non-ASCII characters
# rgdal so have to run in old version of R (e.g., 4.3.3) with rdgal installed
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)
library(rgdal)
library(sp)

# 1. Get data and process #####
# 1.1. Import Data
myFile <- "AZ_flow_HI.RDA" # 15.9 MB
load(file.path(wd, "data-raw", "GIS", myFile))#fc.HI.proj
data_GIS_Flow_HI <- fc.HI.proj

myFile <- "AZ_flow_LO.RDA" # 28.4 MB
load(file.path(wd, "data-raw", "GIS", myFile))#fc.LO.proj
data_GIS_Flow_LO <- fc.LO.proj


# Reproject (transform) ####
# aea from "outline"
# summary(data_GIS_outline)

my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m
           +no_defs +ellps=GRS80 +towgs84=0,0,0"

data_GIS_Flow_HI.proj <- sp::spTransform(data_GIS_Flow_HI, CRS(my.aea))
data_GIS_Flow_LO.proj <- sp::spTransform(data_GIS_Flow_LO, CRS(my.aea))

# QC
proj4string(data_GIS_Flow_HI)
proj4string(data_GIS_Flow_HI.proj)
proj4string(data_GIS_Flow_LO)
proj4string(data_GIS_Flow_LO.proj)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_GIS_Flow_HI <- data_GIS_Flow_HI.proj
data_GIS_Flow_LO <- data_GIS_Flow_LO.proj

usethis::use_data(data_GIS_Flow_HI, overwrite = TRUE)
usethis::use_data(data_GIS_Flow_LO, overwrite = TRUE)

# HI, 87.7 MB compresses to 12.5 MB
# LO, 162.9 MB compresses to 22.9 MB
