# Prepare data for example for AZ, GIS, state outline
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
myFile <- "AZ_State"
shp <- readOGR(dsn=file.path(wd, "data-raw", "GIS", "state"), layer=myFile)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_GIS_AZ_Outline <- shp
devtools::use_data(data_GIS_AZ_Outline, overwrite = TRUE)


