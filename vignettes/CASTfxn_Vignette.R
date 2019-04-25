## ----Setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo=TRUE)
library(knitr)
library(CASTfxn)

## ----LoadPkg, eval=FALSE-------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("leppott/CASTfxn")

## ----ex_getSiteInfo------------------------------------------------------
TargetSiteID <- "SRCKN001.61"
dir_results <- file.path(getwd(), "Results")

CurrentDir <- getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")

# data, example included with package
data.Stations.Info <- data_Sites
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.cluster       <- data_Cluster_Hi
data.mod           <- data_ReachMod

# Map data
map_flowline  <- data_GIS_Flow_HI
map_flowline2 <- data_GIS_Flow_LO
map_outline   <- data_GIS_AZ_Outline
# Project site data to USGS Albers Equal Area
usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
              +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
              +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# projection for outline
my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
           +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
map_proj <- my.aea

# Run getSiteInfo
list.SiteSummary <- getSiteInfo(TargetSiteID
                                , dir_results
                                , data.Stations.Info
                                , data.SampSummary
                                , data.303d.ComID
                                , data.bmi.metrics
                                , data.algae.metrics
                                , data.cluster
                                , data.mod
                                , map_proj
                                , map_outline
                                , map_flowline)

## ----Output_getSiteInfo_str----------------------------------------------
str(list.SiteSummary)

## ----Output_getSiteInfo_JPG, echo=TRUE-----------------------------------
TargetSiteID <- "SRCKN001.61"
myJPG <- paste0(TargetSiteID,".map.jpg")
fn.img <- file.path(getwd(), "Results", TargetSiteID, myJPG)
include_graphics(fn.img)

## ----ex_getChemDataSubsets-----------------------------------------------
TargetSiteID <- "SRCKN001.61"

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")
dir_results <- file.path(getwd(), "Results")

# Run getSiteInfo
# data, example included with package
data.Stations.Info <- data_Sites
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.mod           <- data_ReachMod
#
#' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID, "ElevCategory"])
if(elev_cat=="HI"){
   data.cluster <- data_Cluster_Hi
} else if(elev_cat=="LO") {
   data.cluster <- data_Cluster_Lo
}
#
# Map data
# AZ
map_flowline  <- data_GIS_Flow_HI
map_flowline2 <- data_GIS_Flow_LO
map_outline   <- data_GIS_AZ_Outline
# Project site data to USGS Albers Equal Area
usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
              +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
              +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# projection for outline
my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
           +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
map_proj <- my.aea

#
list.SiteSummary <- getSiteInfo(TargetSiteID
                                , dir_results
                                , data.Stations.Info
                                , data.SampSummary
                                , data.303d.ComID
                                , data.bmi.metrics
                                , data.algae.metrics
                                , data.cluster
                                , data.mod
                                , map_proj
                                , map_outline
                                , map_flowline)

# Data getChemDataSubset
site.COMID <- list.SiteSummary$COMID
site.Clusters <- list.SiteSummary$ClustIDs
data.chem.raw      <- data_Chem
data.chem.info     <- data_ChemInfo

# Run getChemDataSubsets
list.data <- getChemDataSubsets(TargetSiteID
                                , comid=site.COMID
                                , cluster=site.Clusters
                                , data.cluster=data.cluster
                                , data.Stations.Info=data.Stations.Info
                                , data.chem.raw=data.chem.raw
                                , data.chem.info=data.chem.info)

## ----Output_getChemDataSubsets_str---------------------------------------
str(list.data)

## ----ex_getStressorList, warning=FALSE-----------------------------------
TargetSiteID <- "SRCKN001.61"

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")
dir_results <- file.path(getwd(), "Results")

# Data getSiteInfo
# data, example included with package
data.Stations.Info <- data_Sites        # need for getSiteInfo and getChemDataSubsets
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.mod           <- data_ReachMod

# Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID, "ElevCategory"])
if(elev_cat=="HI"){
   data.cluster <- data_Cluster_Hi
} else if(elev_cat=="LO") {
   data.cluster <- data_Cluster_Lo
}

# Map data
# AZ
map_flowline  <- data_GIS_Flow_HI
map_flowline2 <- data_GIS_Flow_LO
map_outline   <- data_GIS_AZ_Outline
# Project site data to USGS Albers Equal Area
usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
              +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
              +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# projection for outline
my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
           +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
map_proj <- my.aea

# Run getSiteInfo
list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
                                , data.SampSummary, data.303d.ComID
                                , data.bmi.metrics, data.algae.metrics
                                , data.cluster, data.mod
                                , map_proj, map_outline, map_flowline)

# Data getChemDataSubsets
# data import, example 
# data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
# data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
# data, example included with package
site.COMID <- list.SiteSummary$COMID
site.Clusters <- list.SiteSummary$ClustIDs
data.chem.raw <- data_Chem
data.chem.info <- data_ChemInfo

# Run getChemDataSubsets
list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
                                , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
                                , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)

# datasets getStressorList
chem.info <- list.data$chem.info
cluster.chem <- list.data$cluster.chem
cluster.samps <- list.data$cluster.samps
ref.sites <- list.data$ref.sites
site.chem <- list.data$site.chem

# set cutoff for possible stressor identification
probsLow <- 0.10
probsHigh <- 0.90 

# Run getStressorList
list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
                                 , cluster.samps, ref.sites, site.chem
                                 , probsHigh, probsLow)

## ----Output_getStressorList_str------------------------------------------
str(list.stressors)

## ----Output_getStressorList_txt------------------------------------------
TargetSiteID <- "SRCKN001.61"
# filename
fn.pctrank <- file.path(getwd(), "Results", TargetSiteID
                        , paste0(TargetSiteID, ".chem.pctrank.txt"))
# read
df.pctrank <- read.delim(fn.pctrank)
# Structure
str(df.pctrank)
# create table
# first 6 rows and columns
kable(df.pctrank[1:6, 1:6], caption="partial table")
# box plot
myJPG <- paste0(TargetSiteID,".boxes.Nutrients.jpg")
fn.img <- file.path(getwd(), "Results", TargetSiteID, myJPG)
include_graphics(fn.img)

