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
clustertype <- "5"
useLU <- FALSE

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")

# data, example included with package
data.Stations.Info <- data_Sites
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.cluster       <- data_Cluster_Hi
data.mod           <- data_ReachMod
 
list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)

## ----Output_getSiteInfo_str----------------------------------------------
str(list.SiteSummary)

## ----Output_getSiteInfo_JPG, echo=TRUE-----------------------------------
TargetSiteID <- "SRCKN001.61"
myJPG <- paste0(TargetSiteID,".map.jpg")
fn.img <- file.path(getwd(), "Results", TargetSiteID, myJPG)
include_graphics(fn.img)

## ----ex_getChemDataSubsets-----------------------------------------------
TargetSiteID <- "SRCKN001.61"
clustertype <- "5"
useLU <- FALSE

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")

# Run getSiteInfo
# data, example included with package
data.Stations.Info <- data_Sites
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.cluster       <- data_Cluster_Hi
data.mod           <- data_ReachMod
#
list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)

site.COMID <- list.SiteSummary$COMID
site.Clusters <- list.SiteSummary$ClustIDs

# data, example included with package
data.chem.raw <- data_Chem
data.chem.info <- data_ChemInfo

# Run getChemDataSubsets
list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)

## ----Output_getChemDataSubsets_str---------------------------------------
str(list.data)

## ----ex_getStressorList, warning=FALSE-----------------------------------
TargetSiteID <- "SRCKN001.61"
clustertype <- "5"
useLU <- FALSE

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")

# datasets getSiteInfo
# data, example included with package
data.Stations.Info <- data_Sites
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.cluster       <- data_Cluster_Hi
data.mod           <- data_ReachMod
#
# Run getSiteInfo
list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)

# datasets getChemDataSubsets
site.COMID <- list.SiteSummary$COMID
site.Clusters <- list.SiteSummary$ClustIDs

# data import, example 
# data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
# data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))

# data, example included with package
data.chem.raw <- data_Chem
data.chem.info <- data_ChemInfo

# Run getChemDataSubsets
list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)

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
                        , paste0(TargetSiteID, "chem.pctrank.txt"))
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

## ----ex_getClusterInfo---------------------------------------------------
TargetSiteID <- "SRCKN001.61"
clustertype <- "5"
useLU <- FALSE

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")

# datasets getSiteInfo
# data, example included with package
data.Stations.Info <- data_Sites
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.cluster       <- data_Cluster_Hi
data.mod           <- data_ReachMod

# Run getSiteInfo
list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
 
# datasets getChemDataSubsets
site.COMID <- list.SiteSummary$COMID
site.Clusters <- list.SiteSummary$ClustIDs
# data, example included with package
data.chem.raw <- data_Chem
data.chem.info <- data_ChemInfo

#
# Run getChemDataSubsets
list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)

# datasets getClusterInfo
ref.reaches <- list.data$ref.reaches
refSiteCOMIDs <- list.data$ref.reaches

# Run getClusterInfo
getClusterInfo(site.COMID, clustertype, site.Clusters, ref.reaches, useLU)

## ----Output_getClusterInfo_txt-------------------------------------------
TargetSiteID <- "SRCKN001.61"
myJPG <- paste0(TargetSiteID,".cluster.ElevWs.jpg")
fn.img <- file.path(getwd(), "Results", TargetSiteID, myJPG)
include_graphics(fn.img)

## ----Ex_getBMImatches----------------------------------------------------
TargetSiteID <- "SRCKN001.61"
clustertype <- "5"
useLU <- FALSE

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")

# datasets getSiteInfo
# data, example included with package
data.Stations.Info <- data_Sites
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.cluster       <- data_Cluster_Hi
data.mod           <- data_ReachMod
#
# Run getSiteInfo
list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)

# datasets getChemDataSubsets
site.COMID <- list.SiteSummary$COMID
site.Clusters <- list.SiteSummary$ClustIDs

# data, example included with package
data.chem.raw <- data_Chem
data.chem.info <- data_ChemInfo

# Run getChemDataSubsets
list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)

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
                                 
# datasets getBMIMatches
## remove "none"
stressors <- list.stressors$stressors[list.stressors$stressors != "none"]


# Run getBMIMatches
list.MatchBMIData <- getBMIMatches(stressors, list.data)

## ----Output_BMIMatches_str-----------------------------------------------
str(list.MatchBMIData)

## ----ex_getBMIStressorResponses, eval=FALSE------------------------------
#  predint <- 0.75
#  varLegLoc <- "topright"
#  BMIresp <- c("CSCI", "MMI_Score", "TotalTaxSPL_Sc", "DipTaxSPL_Sc"
#               , "IntolTaxSPL_Sc", "HBISPL_Sc", "PlecoPct_Sc", "ScrapPctSPL_Sc"
#               , "TrichTax_Sc", "EphemTax_Sc", "EphemPct_Sc", "Dom01PctSPL_Sc")
#  
#  TargetSiteID <- "SRCKN001.61"
#  clustertype <- "5"
#  useLU <- FALSE
#  
#  CurrentDir<-getwd()
#  myDir.Data <- paste(CurrentDir,"data/",sep="/")
#  
#  # datasets getSiteInfo
#  # data, example included with package
#  data.Stations.Info <- data_Sites
#  data.SampSummary   <- data_SampSummary
#  data.303d.ComID    <- data_303d
#  data.bmi.metrics   <- data_BMIMetrics
#  data.algae.metrics <- data_AlgMetrics
#  data.cluster       <- data_Cluster_Hi
#  data.mod           <- data_ReachMod
#  #
#  # Run getSiteInfo
#  list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#  
#  # datasets getChemDataSubsets
#  site.COMID <- list.SiteSummary$COMID
#  site.Clusters <- list.SiteSummary$ClustIDs
#  
#  # data, example included with package
#  data.chem.raw <- data_Chem
#  data.chem.info <- data_ChemInfo
#  
#  # Run getChemDataSubsets
#  list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#  
#  # datasets getStressorList
#  chem.info <- list.data$chem.info
#  cluster.chem <- list.data$cluster.chem
#  cluster.samps <- list.data$cluster.samps
#  ref.sites <- list.data$ref.sites
#  site.chem <- list.data$site.chem
#  
#  # set cutoff for possible stressor identification
#  probsLow <- 0.10
#  probsHigh <- 0.90
#  
#  # Run getStressorList
#  list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#                                   , cluster.samps, ref.sites, site.chem
#                                   , probsHigh, probsLow)
#  
#  # datasets getBMIMatches
#  ## remove "none"
#  stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#  
#  # Run getBMIMatches
#  list.MatchBMIData <- getBMIMatches(stressors, list.data)
#  
#  # Run getBMIStressorResponses
#  getBMIStressorResponses(stressors, list.MatchBMIData)

## ----Output_BMIStressorResponses, eval=FALSE-----------------------------
#  str(list.MatchBMIData)

## ----ex_getAlgMatches, eval=FALSE----------------------------------------
#  TargetSiteID <- "SRCKN001.61"
#  clustertype <- "5"
#  useLU <- FALSE
#  
#  CurrentDir<-getwd()
#  myDir.Data <- paste(CurrentDir,"data/",sep="/")
#  
#  # datasets getSiteInfo
#  # data, example included with package
#  data.Stations.Info <- data_Sites
#  data.SampSummary   <- data_SampSummary
#  data.303d.ComID    <- data_303d
#  data.bmi.metrics   <- data_BMIMetrics
#  data.algae.metrics <- data_AlgMetrics
#  data.cluster       <- data_Cluster_Hi
#  data.mod           <- data_ReachMod
#  #
#  # Run getSiteInfo
#  list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#  
#  # datasets getChemDataSubsets
#  site.COMID <- list.SiteSummary$COMID
#  site.Clusters <- list.SiteSummary$ClustIDs
#  
#  # data, example included with package
#  data.chem.raw <- data_Chem
#  data.chem.info <- data_ChemInfo
#  
#  # Run getChemDataSubsets
#  list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#  
#  # datasets getStressorList
#  chem.info <- list.data$chem.info
#  cluster.chem <- list.data$cluster.chem
#  cluster.samps <- list.data$cluster.samps
#  ref.sites <- list.data$ref.sites
#  site.chem <- list.data$site.chem
#  
#  # set cutoff for possible stressor identification
#  probsLow <- 0.10
#  probsHigh <- 0.90
#  
#  # Run getStressorList
#  list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#                                   , cluster.samps, ref.sites, site.chem
#                                   , probsHigh, probsLow)
#  
#  # datasets getAlgMatches
#  ## remove "none"
#  stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#  
#  # Run getAlgMatches
#  list.MatchAlgData <- getAlgMatches(stressors, list.data)

## ----Output_getAlgMatches_str, eval=FALSE--------------------------------
#  str(list.MatchAlgData)

## ----ex_getAlgStressorResponses, eval=FALSE------------------------------
#  predint <- 0.75
#  varLegLoc <- "topright"
#  
#  TargetSiteID <- "SRCKN001.61"
#  clustertype <- "5"
#  useLU <- FALSE
#  
#  CurrentDir<-getwd()
#  myDir.Data <- paste(CurrentDir,"data/",sep="/")
#  
#  # datasets getSiteInfo
#  # data, example included with package
#  data.Stations.Info <- data_Sites
#  data.SampSummary   <- data_SampSummary
#  data.303d.ComID    <- data_303d
#  data.bmi.metrics   <- data_BMIMetrics
#  data.algae.metrics <- data_AlgMetrics
#  data.cluster       <- data_Cluster_Hi
#  data.mod           <- data_ReachMod
#  #
#  # Run getSiteInfo
#  list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#  
#  # datasets getChemDataSubsets
#  site.COMID <- list.SiteSummary$COMID
#  site.Clusters <- list.SiteSummary$ClustIDs
#  
#  # data, example included with package
#  data.chem.raw <- data_Chem
#  data.chem.info <- data_ChemInfo
#  
#  # Run getChemDataSubsets
#  list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#  
#  # datasets getStressorList
#  chem.info <- list.data$chem.info
#  cluster.chem <- list.data$cluster.chem
#  cluster.samps <- list.data$cluster.samps
#  ref.sites <- list.data$ref.sites
#  site.chem <- list.data$site.chem
#  
#  # set cutoff for possible stressor identification
#  probsLow <- 0.10
#  probsHigh <- 0.90
#  
#  # Run getStressorList
#  list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#                                   , cluster.samps, ref.sites, site.chem
#                                   , probsHigh, probsLow)
#  
#  # datasets getAlgMatches
#  ## remove "none"
#  stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#  
#  # Run getAlgMatches
#  list.MatchAlgData <- getAlgMatches(stressors, list.data)
#  
#  # data getAltStressorResponses
#  data.algae.metrics <- data_AlgMetrics
#  AlgResp <- colnames(data.algae.metrics[4:ncol(data.algae.metrics)])
#  predint <- 0.75
#  varLegLoc <- "topright"
#  
#  # Run getAlgStressorResponses
#  getAlgStressorResponses(stressors, list.MatchAlgData)

## ----Output_getAlgStressorResonses_str, eval=FALSE-----------------------
#  str(list.MatchAlgData)

## ----ex_getStressorSpecificRegressions, eval=FALSE-----------------------
#  predint <- 0.75
#  varLegLoc <- "topright"
#  
#  TargetSiteID <- "SRCKN001.61"
#  clustertype <- "5"
#  useLU <- FALSE
#  
#  CurrentDir <- getwd()
#  myDir.Data <- paste(CurrentDir,"data/",sep="/")
#  
#  # datasets getSiteInfo
#  # data, example included with package
#  data.Stations.Info <- data_Sites
#  data.SampSummary   <- data_SampSummary
#  data.303d.ComID    <- data_303d
#  data.bmi.metrics   <- data_BMIMetrics
#  data.algae.metrics <- data_AlgMetrics
#  data.cluster       <- data_Cluster_Hi
#  data.mod           <- data_ReachMod
#  #
#  # Run getSiteInfo
#  list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#  
#  # datasets getChemDataSubsets
#  site.COMID <- list.SiteSummary$COMID
#  site.Clusters <- list.SiteSummary$ClustIDs
#  
#  # data import, example
#  # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
#  # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
#  
#  # data, example included with package
#  data.chem.raw <- data_Chem
#  data.chem.info <- data_ChemInfo
#  
#  # Run getChemDataSubsets
#  list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#  
#  # datasets getStressorList
#  chem.info <- list.data$chem.info
#  cluster.chem <- list.data$cluster.chem
#  cluster.samps <- list.data$cluster.samps
#  ref.sites <- list.data$ref.sites
#  site.chem <- list.data$site.chem
#  
#  # set cutoff for possible stressor identification
#  probsLow <- 0.10
#  probsHigh <- 0.90
#  
#  # Run getStressorList
#  list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#                                   , cluster.samps, ref.sites, site.chem
#                                   , probsHigh, probsLow)
#  
#  # datasets getBMIMatches
#  ## remove "none"
#  stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#  
#  # Run getBMIMatches
#  list.MatchBMIData <- getBMIMatches(stressors, list.data)
#  
#  # datasets getStressorSpecificRegressions
#  # data import, example
#  # data.bmi.taxa.raw <- read.delim(paste(myDir.Data,"data.bmi.taxa.raw.tab",sep=""))
#  # data.SSTV.totabund <- read.delim(paste(myDir.Data,"data.totabund.bySample.tab",sep=""))
#  #
#  # data, example included with package
#  data.bmi.taxa.raw <- data_BMIcounts
#  data.SSTV.totabund <- data_BMIRelAbund
#  
#  # Run getStressorSpecificRegressions
#  getStressorSpecificRegressions(list.MatchBMIData)

## ----Output_getStressorSpecificRegressions_str, eval=FALSE---------------
#  str(list.MatchAlgData)

## ----Output_getSSDplot, warnings=FALSE-----------------------------------
# Example 3 
# https://www.epa.gov/caddis-vol4/caddis-volume-4-data-analysis-download-software
# ssd_generator_v1.xlsm
myDF <- data_SSD_generator
myRT   <- "ResponseType"
myTaxa <- "Taxa"
myExp  <- "Exposure"
# Run function
p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)
p3

## ----Output, eval=FALSE--------------------------------------------------
#  # Install CASTfxn ####
#  # library(devtools)
#  # install_github("ALincolnTt/CASTfxn")
#  
#  # Set up ####
#  library(CASTfxn)
#  library(readxl)
#  
#  #Dir.Base <- "C:/Users/ann.lincoln/OneDrive - Tetra Tech, Inc/_ActiveProjects/AZ/RCodeData/ChemData"
#  #Dir.Base <- "P:/Current/OtherGov/City of San Diego/FY2018/CAST_2018/QC_20180705/data"
#  Dir.Base <- "C:/Users/Erik.Leppo/OneDrive - Tetra Tech, Inc/MyDocs_OneDrive/GitHub/CASTfxn/vignettes/Results"
#  #setwd("C:\\Users\\Erik.Leppo\\OneDrive - Tetra Tech, Inc\\MyDocs_OneDrive\\GitHub\\CASTfxn_Ann\\CASTfxn")
#  
#  #Dir.Base <- "C:\\Users\\Erik.Leppo\\OneDrive - Tetra Tech, Inc\\MyDocs_OneDrive\\SanDiego\\Results"
#  setwd(Dir.Base)
#  
#  # Site Selection ####
#  TargetSiteID <- "LCNUT011.29"  # "VRELL009.02"
#  TargetElevation <- "HI"
#  
#  # Read Files ####
#  #Read all data files
#  data.SampSummary <- data_SampSummary
#  data.mod         <- data_ReachMod
#  data.chem.info   <- data_ChemInfo
#  data.303d.ComID  <- data_303d
#  
#  # Subset ####
#  # These need subsetting
#  if (TargetElevation == "HI") {
#      data.cluster   <- data_Cluster_Hi
#      fn.coOccur.hi  <- file.path(Dir.Base,"AZCoOccurData_HI.tab")
#      data.coOccur   <- read.table(fn.coOccur.hi, header = TRUE, sep = "\t")
#      ibi.thresholds <- c(45,52)
#  } else {
#      data.cluster   <- data_Cluster_Lo
#      fn.coOccur.lo  <- file.path(Dir.Base, "AZCoOccurData_LO.tab")
#      data.coOccur   <- read.table(fn.coOccur.lo, header = TRUE, sep = "\t")
#      ibi.thresholds <- c(39,50)
#  }
#  
#  # CAST, Stations ####
#  data.Stations.Info <- data_Sites
#  data.Stations.Info <- data.Stations.Info[data.Stations.Info$ElevCategory == TargetElevation,]
#  
#  # CAST, Chem ####
#  ## Use data rather than example file
#  data.chem.raw <- data_Chem
#  #
#  fn.cheminfo2   <- file.path(Dir.Base, "AZStressorInfoFinal.xlsx")
#  data.chem.info2 <- read_excel(fn.cheminfo2, sheet = 1, skip = 0)
#  # #
#  # fn.chem <- file.path(Dir.Base, "AZStressorDataFinal.tab")
#  # data.chem.raw <- read.delim(fn.chem, na.strings = "NA")
#  #
#  analytes      <- data.chem.info$StdParamName[data.chem.info$UseInStressorID == 1]
#  data.chem.raw <- data.chem.raw[data.chem.raw$StdParamName %in% analytes,]
#  data.chem.raw <- data.chem.raw[data.chem.raw$ElevCategory == TargetElevation,]
#  
#  # CAST, BMI, metrics ####
#  data.bmi.metrics <- data_BMIMetrics
#  data.bmi.metrics <- data.bmi.metrics[data.bmi.metrics$ElevCategory == TargetElevation,]
#  data.bmi.metrics <- data.bmi.metrics[data.bmi.metrics$StationID_Master != "VROAK042.78",]
#  data.bmi.metrics <- data.bmi.metrics[data.bmi.metrics$StationID_Master != "SRHAG007.47",]
#  data.bmi.metrics <- data.bmi.metrics[,c("StationID_Master","BMISampID",
#                                          "BMI.Metrics.SampID","CollDate",
#                                          "ElevCategory","NarRat","IBI",
#                                          "TotalTaxSPL_Sc","DipTaxSPL_Sc",
#                                          "IntolTaxSPL_Sc","HBISPL_Sc","PlecoPct_Sc",
#                                          "ScrapPctSPL_Sc","ScrapTaxSPL_Sc",
#                                          "TrichTax_Sc","EphemTax_Sc","EphemPct_Sc",
#                                          "Dom01PctSPL_Sc")]
#  data.bmi.metrics <- data.bmi.metrics[, unlist(lapply(data.bmi.metrics,
#                                        function(x) !all(is.na(x))))]
#  
#  # CAST, Alg, metrics ####
#  fn.alg.metrics     <- file.path(Dir.Base, "AZAlgaeMetrics.tab")
#  data.algae.metrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t")
#  data.algae.metrics <- data.algae.metrics[!is.na(data.algae.metrics),]
#  
#  # CAST, misc ####
#  fn.bmi.raw        <- file.path(Dir.Base, "AZBenthicCountsFinal.tab")
#  data.bmi.taxa.raw <- read.table(fn.bmi.raw, header = TRUE, sep = "\t")
#  
#  fn.MT.bmi   <- file.path(Dir.Base, "AZBenthicMasterTaxa.tab")
#  data.MT.bmi <- read.table(fn.MT.bmi, header = TRUE, sep = "\t",
#                            stringsAsFactors = FALSE)
#  
#  # read LkpDir (.tab file)
#  # fn.lkpdir <- "C:/Users/ann.lincoln/OneDrive - Tetra Tech, Inc/_ActiveProjects/AZ/RCodeData/ChemData/AZLkpDir.tab"
#  # data.lkp.dir <- read.table(fn.lkpdir, header = TRUE, sep = "\t",
#  #                            col.names = c("StdParamName","DipTaxSPL_Sc",
#  #                                          "Dom01PctSPL_Sc","EphemPct_Sc",
#  #                                          "EphemTax_Sc","HBISPL_Sc","IBI",
#  #                                          "IntolTaxSPL_Sc","PlecoPct_Sc",
#  #                                          "ScrapPctSPL_Sc","ScrapTaxSPL_Sc",
#  #                                          "TotalTaxSPL_Sc","TrichTax_Sc"),
#  #                            colClasses = c("character","numeric","numeric",
#  #                                           "numeric","numeric","numeric","numeric",
#  #                                           "numeric","numeric","numeric",
#  #                                           "numeric","numeric","numeric"))
#  # data.lkp.dir <- data_LkpDir
#  
#  list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU=FALSE)
#  # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo, Samps = mySamps,
#                                              # BMImetrics = myBMImetrics,
#                                              # AlgMetrics = myAlgaeMetrics,
#                                              # ReachInfo = myReachInfo,
#                                              # COMID = myCOMID,
#                                              # ClustIDs = myClustIDs)
#  site.COMID <- list.SiteSummary$COMID
#  site.Clusters <- list.SiteSummary$ClustIDs
#  SiteInfo <- list.SiteSummary$SiteInfo
#  
#  list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters,
#                                  clustertype, useLU=FALSE)
#  # mySubsets <- list(ref.sites = refSiteIDs,
#                      # ref.reaches = refSiteCOMIDs,
#                      # cluster.samps = cluster.chem.samps,
#                      # chem.info = chems.groups.sort,
#                      # all.chems = all.chems3,
#                      # cluster.chem = cluster.chem.tab5,
#                      # site.chem = site.chem4)
#  ref.sites <- list.data$ref.sites
#  ref.reaches <- list.data$ref.reaches
#  cluster.samps <- list.data$cluster.samps
#  cluster.chem <- list.data$cluster.chem
#  site.chem <- list.data$site.chem
#  chem.info <- list.data$chem.info
#  
#  # Get Cluster Info
#  getClusterInfo(site.COMID, clustertype, site.Clusters, ref.reaches, useLU=FALSE)
#  
#  # CAST, Stressors ####
#  # Get Stressor List
#  list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info,
#                                    cluster.chem, cluster.samps, ref.sites,
#                                    site.chem, probsHigh=0.75, probsLow=0.25,
#                                    useLU=FALSE)
#  # myStressors <- list(stressors = stressorlist, site.stressor.pctrank = site.pctrank)
#  
#  stressors <- list.stressors$stressors
#  
#  if ((length(stressors) == 1) && stressors[1] == "none") {
#      # No stressors returned
#      print(paste("No stressors identified for site", TargetSiteID, sep = " "))
#      flush.console()
#  } else {
#    #
#      stressors <- c(stressors[2:length(stressors)])
#      # Get BMI matches
#      list.MatchBMIData <- getBMIMatches(stressors, list.data)
#      # myBMIMatchData <- list(all.b.str = all.mbmi.stress
#      #                        , cl.b.str = cl.mbmi.stress
#      #                        , site.b.str = site.mbmi.stress
#      #                        , all.b.rsp = all.mbmi.resp
#      #                        , cl.b.rsp = cl.mbmi.resp
#      #                        , site.b.rsp = site.mbmi.resp)
#  
#      # Get BMI Stressor Responses
#      getBMIStressorResponses(stressors, list.MatchBMIData)
#  
#      if (TargetSiteID %in% unique(data.coOccur$StationID_Master)) {
#          getCoOccur(data.coOccur, ID.plot=TargetSiteID, col.ID="StationID_Master"
#                  , col.Group="Group", col.Bio="IBI", col.Stressors=c(stressors)
#                  , Bio.Nar.Brk=c(0, ibi.thresholds, 100)
#                  , Bio.Nar.Lab=c("Impaired","Marginal","Good")
#                  , Bio.Deg.Brk=c(0, min(ibi.thresholds), 100)
#                  , Bio.Deg.Lab=c("Impaired", "Good"))
#      }
#  
#      # Get Stressor-specific regressions
#      predint <- 0.75
#      varLegLoc <- "topright"
#      myDir.Data <- getwd()
#      # read in relative abundance file
#      fn.RelAbund <- "AZ_Benthics_RelAbund.xlsx"
#      BMIRelAbund <- read_excel(fn.RelAbund, sheet=1)
#      #data.SSTV.totabund <- data_BMIRelAbund
#      data.SSTV.totabund <- BMIRelAbund
#  
#      getStressorSpecificRegressions(list.MatchBMIData, predint, varLegLoc)
#      # }
#  
#      # Erik, 20180712, commented out since no data
#      # list.MatchAlgData <- getAlgMatches(stressors, list.data)
#      # # myAlgMatchData <- list(all.a.str = all.malg.stress
#      # #                        , cl.a.str = cl.malg.stress
#      # #                        , site.a.str = site.malg.stress
#      # #                        , all.a.rsp = all.malg.resp
#      # #                        , cl.a.rsp = cl.malg.resp
#      # #                        , site.a.rsp = site.malg.resp)
#      #
#      # # Get Algal Stressor Responses
#      # getAlgStressorResponses(stressors, list.MatchAlgData)
#  
#      # getSSDs
#      #getSSDplot(Data, ResponseType, Taxa, Exposure)
#      myDF <- data_SSD_generator
#      myRT   <- "ResponseType"
#      myTaxa <- "Taxa"
#      myExp  <- "Exposure"
#      # Run function
#      p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)
#  
#  }

