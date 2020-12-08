# Shiny, Global
# RPPTool_SMC

# Packages ####
library(shiny)
library(shinyFiles)
library(shinyjs)
library(CASTfxn)
library(readxl)
library(stringr)
library(dplyr)
library(ggrepel)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(maptools)
library(viridis)
library(rgdal)
library(leaflet)
# not sure
library(shinyBS)
library(DT)

# old
# library(cluster)
# library(maps)
# library(raster)
# library(RColorBrewer)
# library(reshape)
# library(rgeos)
# library(RgoogleMaps)
# library(rpart)

# Source ####
source(file.path(".", "external", "R", "addMunicipality.R"))
source(file.path(".", "external", "R", "drawAllScoresPlot.R"))
source(file.path(".", "external", "R", "drawBarPlot.R"))
source(file.path(".", "external", "R", "getBCGScores.R"))
source(file.path(".", "external", "R", "getBCGtiers.R"))
source(file.path(".", "external", "R", "getConnectivity.R"))
source(file.path(".", "external", "R", "getConnectivityScores.R"))
source(file.path(".", "external", "R", "getOpportunityScores.R"))
source(file.path(".", "external", "R", "getReachMap.R"))
source(file.path(".", "external", "R", "getScaledStressors.R"))
source(file.path(".", "external", "R", "getStressorScores.R"))
source(file.path(".", "external", "R", "getThreatScores.R"))
source(file.path(".", "external", "R", "updateAllScoresTable.R"))

# Global Variables ####
url_map <- a("Shiny Reach Selection Map", href="https://leppott.shinyapps.io/CAST_Map_COMID")

# define pipe
`%>%` <- dplyr::`%>%`

# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 300MB.
options(shiny.maxRequestSize = 300*1024^2)

# Targeted Locations
## Add to selection boxes to easy to pick out.
# targ_SiteID <- c( "907S05514", "SMC04134", "905S15201", "SMC12246", "907SDSDR8"
#                 , "907SDSDR9", "905SDYSA7", "906S02246", "SMC01606")
# targ_COMID <- c("20331434", "20331398", "20324447", "20330890", "20329782"
#                 , "20331170", "20331408")
targ_SiteID <- NA
targ_COMID <- NA

# data directory
dir_data <- file.path(".", "Data")
#dir_results <- file.path(".", "Results") # Assumed as subdir of working directory.
dir_wd <- file.path(".")
## Stations - PickList
data.Stations <- read.delim(file.path(dir_data, "SMCSitesFinal.tab")
                            , stringsAsFactors = FALSE)
LU.Stations <- data.Stations[, "StationID_Master"]


zip_name <- "NULL"

# www Results ####

# Remove www\Results (and sub dirs) at start up (and recreate)
dir_www_Results <- file.path(".", "www", "Results")
ifelse(dir.exists(dir_www_Results)==TRUE, unlink(dir_www_Results, recursive = TRUE), NA)
ifelse(dir.exists(dir_www_Results)==FALSE, dir.create(dir_www_Results), NA)


# Map, Stations and Reach ####
# data directory
myDir <- file.path(".", "data")

# Sites ###
# df.sites.map
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
mySites <- as.character(sort(unique(df.sites.map[, "StationID_Master"])))
len_sel_SiteID <- length(targ_SiteID) + length(mySites)

# COMIDs ###
#myComID <- as.character(sort(unique(lines.flowline.proj@data[, "COMID"])))
df_COMID_data <- as.data.frame(read_excel(file.path(".", "Data", "SMC_TestCOMIDs.xlsx"), sheet = "SMC_TestReaches"))
myComID <- as.character(sort(unique(df_COMID_data[, "COMID"])))
len_sel_COMID <- length(targ_COMID) + length(myComID)

# Map height fix
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height

# Functions ####
getHTML <- function(fn_html){
  #fn_disclaimer_html <- file.path(".", "data", "Disclaimer_Key.html")
  fe_html <- file.exists(fn_html)
  if(fe_html==TRUE){
    return(includeHTML(fn_html))
  } else {
    return(NULL)
  }
}##getHTML~END

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

