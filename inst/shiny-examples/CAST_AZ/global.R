# Global Shiny Stuff

# Pakcages
library(shiny)
library(CASTfxn)
library(shinyjs)
# #library(leaflet)
# library(dplyr)
# #library(shinyjs)
# library(cluster)
# library(dplyr)
# library(ggplot2)
# library(maps)
# library(maptools)
# library(raster)
# library(RColorBrewer)
# library(readxl)
# library(reshape)
# library(rgdal)
# library(rgeos)
# library(RgoogleMaps)
# library(rpart)

# Source
#source(file.path(".", "external", "runCASTfxn.R"))
source(file.path(".", "external", "getTimeSeq.R"))
source(file.path(".", "external", "getWoE.R"))

#
#setwd("C:/Users/Erik.Leppo/OneDrive - Tetra Tech, Inc/MyDocs_OneDrive/GitHub/CASTfxn/inst/shiny-examples/CAST_v2")

# # data directory
# dn_data <- file.path(getwd(), "data")
# 
# 
# # Site
# LU.Stations <- read.delim(file.path(dn_data, "data.Stations.LookUp.tab"), stringsAsFactors = FALSE)


dir_data <- file.path(".", "data")
# ## Stations - PickList
data.Stations <- read.delim(file.path(dir_data, "data.Stations.LookUp.tab", sep="")
                            , stringsAsFactors = FALSE)
LU.Stations <- data.Stations[,"StationID"]


zip_name <- "NULL"


# # Sites ####
# # df.sites.map
# fn.sites <- "df.sites.map.rda"
# load(file.path(dn_data, fn.sites))
# 
# # SMC watersheds #####
# # poly.smc.proj
# fn.SMC <- "poly.smc.proj.rda"
# load(file.path(dn_data, fn.SMC))
# 
# # Flowlines ####
# # lines.flowline.proj
# fn.Flowline.SMC <- "lines.flowline.proj.rda"
# load(file.path(dn_data, fn.Flowline.SMC))
# 
# # SiteIDs ####
# mySites <- df.sites.map[,"StationID_Master"]
# 
# # COMIDs ####
# myComID <- as.character(sort(unique(lines.flowline.proj@data[, "COMID"])))


# Map height fix
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height