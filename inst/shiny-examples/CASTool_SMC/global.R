# Global Shiny Stuff
# CASTool_SMC

# Packages ####
library(shiny)   # for Shiny app
library(CASTfxn) # 
library(shinyjs)
library(leaflet) # used for map
library(readxl)  # import Excel data
library(stringr)
library(dplyr)
library(ggrepel)
library(tidyr)
# Old not needed
# library(shinyFiles)
# library(cluster)
# library(ggplot2)
# library(maps)
# library(maptools)
# library(raster)
# library(RColorBrewer)
# library(reshape)
# library(rgdal)
# library(rgeos)
# library(RgoogleMaps)
# library(rpart)

# Source ####
# source(file.path(".", "external", "runCASTfxn.R"))
# source(file.path(".", "external", "getTimeSeq.R"))
# source(file.path(".", "external", "getWoE.R"))

# Global Variables ####

`%>%` <- dplyr::`%>%`

# data directory
dir_data <- file.path(".", "Data")
dir_results <- file.path(".", "Results") # Assumed as subdir of working directory.
dir_wd <- file.path(".")
## Stations - PickList
data.Stations <- read.delim(file.path(dir_data, "SMCSitesFinal.tab")
                            , stringsAsFactors = FALSE)
LU.Stations <- data.Stations[,"StationID_Master"]

zip_name <- "NULL"

# Targeted Locations
## Add to selection boxes to easy to pick out.
# targ_SiteID <- c("SMC04134", "905S15201", "907S05514", "SMC12246", "907SDSDR8"
#                  , "907SDSDR9", "SMC04134", "905SDYSA7", "906S02246", "SMC01606")
targ_SiteID <- NA


# Clean up ####
# Remove www\Results (and sub dirs) at start up (and recreate)
dir_www_Results <- file.path(".", "www", "Results")
ifelse(dir.exists(dir_www_Results)==TRUE, unlink(dir_www_Results, recursive = TRUE), NA)
ifelse(dir.exists(dir_www_Results)==FALSE, dir.create(dir_www_Results), NA)

# Map/GIS data ####
# # Sites ###
# # df.sites.map
fn.sites <- "df.sites.map.rda"
load(file.path(dir_data, fn.sites))

# SMC watersheds ####
# poly.smc.proj
# fn.SMC <- "poly.smc.proj.rda"
# load(file.path(dir_data, fn.SMC))
# 2020-12-08, use simple version
fn.SMC <- "poly.smc.proj.simple05.rda"
load(file.path(dir_data, fn.SMC))
poly.smc.proj <- poly.smc.proj.simple05

# Flowlines ###
# lines.flowline.proj
fn.Flowline.SMC <- "lines.flowline.proj.rda"
load(file.path(dir_data, fn.Flowline.SMC))

# SiteIDs ###
#mySites <- df.sites.map[,"StationID_Master"]
mySites <- as.character(sort(unique(df.sites.map[, "StationID_Master"])))

# COMIDs ###
myComID <- as.character(sort(unique(lines.flowline.proj@data[, "COMID"])))

# Map height fix
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height


# Functions ####

# get HTML
# getHTML <- function(fn_html){
#   #fn_disclaimer_html <- file.path(".", "data", "Disclaimer_Key.html")
#   fe_html <- file.exists(fn_html)
#   if(fe_html==TRUE){
#     return(includeHTML(fn_html))
#   } else {
#     return(NULL)
#   }
# }##getHTML~END

# Not all NA
not_all_na <- function(x) {!all(is.na(x))}

# Copy over Results for Display
# TargetSiteID_Results <- "403S01784" # "801RB8197" # "403S01784"
CopyResults <- function(TargetSiteID_Results){
  #
  dn_Results <- file.path(".", "Results", TargetSiteID_Results)
  dn_www <- file.path(".", "www", "Results", TargetSiteID_Results)
  #
  # Create SiteID folder
  ifelse(dir.exists(dn_www)==FALSE, dir.create(file.path(dn_www)), NA)
  # Names; Subfolders and Files
  dn_sub <- c("CandidateCauses"
              , "CoOccurrence"
              , "SiteInfo"
              , "StressorResponse"
              , "TimeSequence"
              , "VerifiedPredictions"
              , "WoE")
  fn_pdf <- c(paste0(TargetSiteID_Results, ".boxes.ALL.pdf")
              , paste0(TargetSiteID_Results, ".CoOccurrence.ALL.pdf")
              , NA #paste0(TargetSiteID_Results, "_map_leaflet.html")
              , paste0(TargetSiteID_Results, ".SR.BMI.ALL.pdf")
              , paste0("\\BMI\\", TargetSiteID_Results, ".TS.ALL.pdf")
              , paste0(TargetSiteID_Results, ".SR.SSTV.ALL.pdf")
              , NA)
  dn_sub_www <- file.path(dn_www, dn_sub)
  dn_sub_Results <- file.path(dn_Results, dn_sub)
  # Create Subfolders
  sapply(dn_sub_www ,function(x) ifelse(dir.exists(x)==FALSE, dir.create(x), NA))
  dn_sub2 <- file.path(dn_sub_www[5], "BMI")
  ifelse(dir.exists(dn_sub2)==FALSE, dir.create(dn_sub2), NA)
  # Copy Files
  for(i in 1:6){
    fn_i <- fn_pdf[i]
    fn_i_from <- file.path(dn_sub_Results[i], fn_i)
    fn_i_to <- file.path(dn_sub_www[i],fn_i)
    if(file.exists(fn_i_from)){
      file.copy(fn_i_from, fn_i_to, overwrite = TRUE)
    }##IF~file.exists~END
  }##FOR~i~END
  #
}##CopyResults~END
