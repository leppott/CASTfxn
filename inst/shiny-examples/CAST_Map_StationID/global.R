# Global Shiny Stuff

# Libraries
library(shiny)
library(leaflet)
library(dplyr)
#library(rgdal)
#library(shinyjs)

# data directory
myDir <- file.path(getwd(), "data")

# Sites ####
# df.sites.map
fn.sites <- "df.sites.map.rda"
load(file.path(myDir, fn.sites))

# SMC watersheds #####
# poly.smc.proj
fn.SMC <- "poly.smc.proj.rda"
load(file.path(myDir, fn.SMC))

# Flowlines ####
# lines.flowline.proj
fn.Flowline.SMC <- "lines.flowline.proj.rda"
load(file.path(myDir, fn.Flowline.SMC))

# SiteIDs ####
mySites <- as.character(sort(unique(df.sites.map[, "StationID_Master"])))

# COMIDs ####
myComID <- as.character(sort(unique(lines.flowline.proj@data[, "COMID"])))


# Map height fix
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height