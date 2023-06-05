#  Copyright 2023 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
# CASTfxn (Specific for SMC)
# Erik.Leppo@tetratech.com, 20180710
# Ann.RoseberryLincoln@tetratech.com, 20230605
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.3.0
#
# library(devtools)
# install_github("leppott/CASTfxn")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Shiny code for use in Shiny App
# 2020-10-30, Erik
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#rm(list=ls())

boo_Shiny <- FALSE

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, Start ####
# external/CASTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# 02, Set up ####
# Progress, 02
if (boo_Shiny == TRUE) {
  prog_det <- "Set up"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
boo.debug <- TRUE
debug.person <- "Ann"
if (boo_Shiny == TRUE) {
  gitpath <- file.path(".", "external", "R")  # used in RPPTool but not CASTool until getReport
  dir_rmd <- file.path(".", "external", "rmd")
  wd <- file.path(".")
  dir_data <- file.path(wd, "Data")
  dir_results <- file.path(wd, "Results")
  printClusterInfo <- TRUE
} else {# Not using shiny app
  #
  # in global in shiny
  not_all_na <- function(x) {!all(is.na(x))}
  #
  # Set up required functions ### DO NOT CHANGE! #
  # library(CASTfxn)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  #
  if (boo.debug == TRUE & debug.person == "Ann") {
    wd <- getwd() # "C:/Users/ann.lincoln/Documents" ARL 2023-05-22
    gitpath <- file.path(wd, "GitHub", "CASTfxn", "R") # ARL 2023-05-22
    dir_rmd <- file.path(wd, "GitHub", "CASTfxn", "inst", "rmd") # ARL 2023-05-22
    localdir <- "C:/Users/ann.lincoln/Documents/SEP_CAST"
    dir_data <- file.path(localdir, "Data")
    dir_results <- file.path(localdir, "Results")
    printClusterInfo <- FALSE
    boo_plot_user <- TRUE
    # NOTE: to run all sites, comment out line 639
    #if (boo.debug == TRUE & debug.person == "Ann") {
    source(file.path(gitpath, "getCoOccurDataset.R"))
    source(file.path(gitpath, "getTimeSeq.R"))
    source(file.path(gitpath, "getDataSets.R"))
    source(file.path(gitpath, "getComparators.R"))
    source(file.path(gitpath, "getSiteInfo.R"))
    source(file.path(gitpath, "getSiteMap.R"))
    source(file.path(gitpath, "getClusterInfo.R"))
    source(file.path(gitpath, "getStressorList.R"))
    source(file.path(gitpath, "getCoOccur.R"))
    source(file.path(gitpath, "getBioStressorResponses.R"))
    source(file.path(gitpath, "getVerifiedPredictions.R"))
    source(file.path(gitpath, "getOutliers.R"))
    source(file.path(gitpath, "getWoE.R"))
    source(file.path(gitpath, "getQualSites.R"))
    source(file.path(gitpath, "getSummaryAllSites.R"))
    source(file.path(gitpath, "getReport.R"))
    #}
  } else if (boo.debug == TRUE & debug.person == "Erik") {
    library(CASTfxn)
    #gitpath <- file.path(system.file(package = "CASTfxn"), "R")
    dir_rmd <- file.path(system.file(package = "CASTfxn"), "inst", "rmd")
    wd <- "C://Users//Erik.Leppo//OneDrive - Tetra Tech, Inc//MyDocs_OneDrive//GitHub//CASTfxn//inst//shiny-examples//CAST_SMC"
    dir_data <- file.path(wd, "Data")
    dir_results <- file.path(wd, "Results")
    printClusterInfo <- TRUE
    site <- "SMC04134"
    TargetSiteID <- site
    b <- 1
  } else if (boo.debug == TRUE) {
    # This should be an error condition, because Ann & Erik are only people
  } else {#boo.debug == FALSE
    # Install CASTfxn package
    library(CASTfxn)
    # Set local directory info
    wd <- file.path(".")
    dir_data <- file.path(wd, "Data")
    dir_results <- file.path(wd, "Results")
    boo_plot_user <- TRUE
  }
  #
  #
}## IF ~ boo_Shiny ~ END

msg <- paste0("debug = ", boo.debug
              , ifelse(boo.debug == FALSE, ""
                       , paste0(", person = ", debug.person)))
message(msg)

startprep.time <- Sys.time()

#~~~~~~~~~~~~~~~~~~~~~~~
# 03, Select region ####
# Progress, 03
region <- "SanDiego" # options: SanDiego, AZ, WA, OR

# Read CASTool_Metadata.xlsx
fn.CASTmeta <- file.path(dir_data, "CASTool_Metadata.xlsx")
data_CASTmeta <- readxl::read_excel(fn.CASTmeta, na = "", trim_ws = TRUE)

# Required user-designated options
removeOutliers <- as.logical(data_CASTmeta["removeOutliers", region])
useBC     <- as.logical(data_CASTmeta["useBC", region])
samplim   <- as.integer(data_CASTmeta["samplim", region])
probsHigh <- as.numeric(data_CASTmeta["probsHigh", region])
probsLow  <- as.numeric(data_CASTmeta["probsLow", region])
DOlim     <- as.numeric(data_CASTmeta["DOlim", region])
pHlimLow  <- as.numeric(data_CASTmeta["pHlimLow", region])
pHlimHigh <- as.numeric(data_CASTmeta["pHlimHigh", region])
lagdays   <- as.integer(data_CASTmeta["lagdays", region])
biocommlist   <- unlist(stringr::str_split(data_CASTmeta["biocommlist", region], ", "))
siteQual2Plot <- as.character(data_CASTmeta["siteQual2Plot", region])
# report_format <- as.character(data_CASTmeta["report_format", region])

# removeOutliers <- TRUE
# useBC     <- TRUE # Use Bray-Curtis biological dissimilarity distance matrix
# samplim   <- 10   # samples <= which candidate causes can't be identified
# probsHigh <- 0.75
# probsLow  <- 0.25
# DOlim     <- 7
# pHlimLow  <- 6.5
# pHlimHigh <- 9
# lagdays   <- 10
# biocommlist   <- c("bmi","algae")
# siteQual2Plot <- "not degraded" # options:"reference","better than","not degraded"
# report_format <- "html"    # word, pdf are the other options

# Specify Base Filenames # These are the files used to run the analyses
#
# Specify Base Filenames # These are the files used to run the analyses
fn.targets          <- file.path(dir_data, data_CASTmeta["fn.targets", region])
fn.Sites.Info       <- file.path(dir_data, data_CASTmeta["fn.Sites.Info", region])
# fn.SampSummary      <- file.path(dir_data, data_CASTmeta["fn.targets", region])
fn.cheminfo         <- file.path(dir_data, data_CASTmeta["fn.cheminfo", region])
fn.chemdata         <- file.path(dir_data, data_CASTmeta["fn.chemdata", region])
fn.modelinfo        <- file.path(dir_data, data_CASTmeta["fn.modelinfo", region])
fn.modeldata        <- file.path(dir_data, data_CASTmeta["fn.modeldata", region])
fn.bmi.metrics      <- file.path(dir_data, data_CASTmeta["fn.bmi.metrics", region])
fn.bmi.cscicore     <- file.path(dir_data, data_CASTmeta["fn.bmi.cscicore", region])
fn.bmi.metrics.info <- file.path(dir_data, data_CASTmeta["fn.bmi.metrics.info", region])
fn.bmi.raw          <- file.path(dir_data, data_CASTmeta["fn.bmi.raw", region])
fn.MT.bmi           <- file.path(dir_data, data_CASTmeta["fn.MT.bmi", region])
fn.alg.metrics      <- file.path(dir_data, data_CASTmeta["fn.alg.metrics", region])
fn.alg.metrics.info <- file.path(dir_data, data_CASTmeta["fn.alg.metrics.info", region])
fn.alg.raw          <- file.path(dir_data, data_CASTmeta["fn.alg.raw", region])
fn.MT.alg           <- file.path(dir_data, data_CASTmeta["fn.MT.alg", region])
fn.fish.metrics      <- file.path(dir_data, data_CASTmeta["fn.fish.metrics", region])
fn.fish.metrics.info <- file.path(dir_data, data_CASTmeta["fn.fish.metrics.info", region])
fn.fish.raw          <- file.path(dir_data, data_CASTmeta["fn.fish.raw", region])
fn.MT.fish           <- file.path(dir_data, data_CASTmeta["fn.MT.fish", region])
fn.bcdist           <- file.path(dir_data, data_CASTmeta["fn.bcdist", region])
fn.cluster          <- file.path(dir_data, data_CASTmeta["fn.cluster", region])
fn.clusterinfo      <- file.path(dir_data, data_CASTmeta["fn.clusterinfo", region])
fn.bkgdata          <- file.path(dir_data, data_CASTmeta["fn.bkgdata", region])
fn.bkginfo          <- file.path(dir_data, data_CASTmeta["fn.bkginfo", region])

# fn.targets          <- file.path(dir_data,"SMCTestSites.xlsx")
# fn.Sites.Info       <- file.path(dir_data,"SMCSitesFinal.tab")
# fn.SampSummary      <- file.path(dir_data,"SMCSiteSummary.tab")
# fn.cheminfo         <- file.path(dir_data,"SMCMeasStressInfoFinal.tab")
# fn.chemdata         <- file.path(dir_data,"SMCMeasStressDataFinal.tab")
# fn.modelinfo        <- file.path(dir_data,"SMCModelStressInfoFinal.tab")
# fn.modeldata        <- file.path(dir_data,"SMCModelStressDataFinal.tab")
# fn.bmi.metrics      <- file.path(dir_data,"SMCBenthicMetricsFinal.tab")
# fn.bmi.cscicore     <- file.path(dir_data,"SMCBenthicCSCIcore.tab")
# fn.bmi.metrics.info <- file.path(dir_data,"SMCBenthicMetricsInfo.tab")
# fn.bmi.raw          <- file.path(dir_data, "SMCBenthicCountsFinal.tab")
# fn.MT.bmi           <- file.path(dir_data, "SMCBenthicMasterTaxa.tab")
# fn.alg.metrics      <- file.path(dir_data, "SMCAlgaeMetricsFinal.tab")
# fn.alg.metrics.info <- file.path(dir_data, "SMCAlgaeMetricsInfo.tab")
# fn.alg.raw          <- file.path(dir_data, "SMCAlgaeCountsFinal.tab")
# fn.MT.alg           <- file.path(dir_data, "SMCAlgaeMasterTaxa.tab")
# fn.bcdist           <- file.path(dir_data, "SMCBCDist.tab")
# fn.cluster          <- file.path(dir_data, "SMCClusterData.tab")
# fn.clusterinfo      <- file.path(dir_data, "SMCClusterInfo.tab")
# fn.bkgdata          <- file.path(dir_data, "SMCSiteBkgdData.tab")
# fn.bkginfo          <- file.path(dir_data, "SMCSiteBkgdInfo.tab")

# Load GIS files
message("Loading GIS files.")
if (boo_Shiny == TRUE) {
  # 2020-09-09, use RDA saved version
  # NOT sure how to handle this
  outline  <- poly.smc.proj
  flowline <- lines.flowline.proj
} else {
  dsn_outline         <- file.path(dir_data, data_CASTmeta["dsn_outline", region])
  lyr_outline         <- data_CASTmeta["dsn_outline", region]
  dsn_flowline        <- file.path(dir_data, data_CASTmeta["lyr_outline", region])
  lyr_flowline        <- data_CASTmeta["lyr_outline", region]

  # Get boundary file for desired region
  sp_outline <- sf::read_sf(dsn = file.path(dsn_outline)
                            , layer = lyr_outline) %>%
    sf::st_transform(crs = 4326) # EPSG identifier for WGS84

  # Get all flowlines -- note that this gives a warning
  # (In CPL_transform...GDAL Message 1: sub-geometry [n]
  # has coordinate dimension 2, but container has 3) ARL 2023-05-24
  sp_flowline <- sf::read_sf(dsn = file.path(dsn_flowline)
                             , layer = lyr_flowline) %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_zm(drop = TRUE, what = "ZM")
}## IF ~ boo_Shiny ~ END

# Specify user-defined variables
# Stressors
meas.stress <- unlist(stringr::str_split(data_CASTmeta["meas.stress", region], ", "))
chem.stress <- unlist(stringr::str_split(data_CASTmeta["chem.stress", region], ", "))
hab.stress <- data_CASTmeta["hab.stress", region]
mod.stress <- data_CASTmeta["mod.stress", region]

# meas.stress <- c("ChemSampleID", "PhabSampID", "FldChemSampID")
# chem.stress <- c("ChemSampleID", "FldChemSampID")
# hab.stress <- "PhabSampID"
# mod.stress <- "FlowSampID"

# Bio responses
for (bio in seq_along(biocommlist)) {
  bio <- tolower(bio)
  if (bio == "bmi") {
    bmi_thresholds <- as.numeric(unlist(stringr::str_split(data_CASTmeta["bmi_thresholds", region], ", ")))
    bmi_narrative <- unlist(stringr::str_split(data_CASTmeta["bmi_narrative", region], ", "))
    bmi_deg_thres <- as.numeric(unlist(stringr::str_split(data_CASTmeta["bmi_deg_thres", region], ", ")))
    bmi_deg_text <- unlist(stringr::str_split(data_CASTmeta["bmi_deg_text", region], ", "))
    bmiIndexGp <- unlist(stringr::str_split(data_CASTmeta["bmiIndexGp", region], ", "))
    bmiResp <- data_CASTmeta["bmiResp", region]
    bmiRespDate <- data_CASTmeta["bmiRespDate", region]
  }
  if (bio == "alg") {
    alg_thresholds <- as.numeric(unlist(stringr::str_split(data_CASTmeta["alg_thresholds", region], ", ")))
    alg_narrative  <- unlist(stringr::str_split(data_CASTmeta["alg_narrative", region], ", "))
    alg_deg_thres  <- as.numeric(unlist(stringr::str_split(data_CASTmeta["alg_deg_thres", region], ", ")))
    alg_deg_text   <- unlist(stringr::str_split(data_CASTmeta["alg_deg_text", region], ", "))
    algIndexGp     <- unlist(stringr::str_split(data_CASTmeta["algIndexGp", region], ", "))
    algResp        <- data_CASTmeta["algResp", region]
    algRespDate    <- data_CASTmeta["algRespDate", region]
  }
  if (bio == "fish") {
    fish_thresholds <- as.numeric(unlist(stringr::str_split(data_CASTmeta["fish_thresholds", region], ", ")))
    fish_narrative  <- unlist(stringr::str_split(data_CASTmeta["fish_narrative", region], ", "))
    fish_deg_thres  <- as.numeric(unlist(stringr::str_split(data_CASTmeta["fish_deg_thres", region], ", ")))
    fish_deg_text   <- unlist(stringr::str_split(data_CASTmeta["fish_deg_text", region], ", "))
    fishIndexGp     <- unlist(stringr::str_split(data_CASTmeta["fishIndexGp", region], ", "))
    fishResp        <- data_CASTmeta["fishResp", region]
    fishRespDate    <- data_CASTmeta["fishRespDate", region]
  }
}

# BMI responses
# bmi_thresholds <- c(-2, 0.62, 0.799, 0.919, 2)
# bmi_narrative <- c("very likely altered", "likely altered"
#                    , "possibly altered", "likely intact")
# bmi_deg_thres <- c(-2, 0.799, 2)
# bmi_deg_text <- c("Yes", "No")
# bmiIndexGp <- c("CSCI", "OoverE", "MMI")
# bmiResp <- "BMISampID"
# bmiRespDate <- "BMISampDate"

# Algal responses
# alg_thresholds <- c(-2, 0.82, 2)
# alg_narrative  <- c("Degraded", "Not Degraded")
# alg_deg_thres  <- c(-2, 0.82, 2)
# alg_deg_text   <- c("Yes", "No")
# algIndexGp     <- c("MMIhybrid", "MMIdiatom", "MMIsba")
# algResp        <- "AlgSampID"
# algRespDate    <- "AlgSampDate"

# USGS aea for SoCal is below
socal.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
                +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
                +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# aea used for AZ is below
# az.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
#             +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
my.aea <- socal.aea

#~~~~~~~~~~~~~~~~~~~~~~~
# 04, Site data files ####
# Progress, 04
if (boo_Shiny == TRUE) {
  prog_det <- "Load Site Data Files"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

## Get site location info and other metadata (e.g., waterbody name)
if (!is.na(fn.Sites.Info)) {
  data_Sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
} else {
  msg <- "fn.Sites.Info is NA"
  message(msg)
}

# Get cluster data
if (!is.na(fn.cluster)) {
  data_cluster <- read.delim(fn.cluster, header = TRUE, sep = "\t"
                             , stringsAsFactors = FALSE)
} else {
  msg <- "fn.cluster is NA"
  message(msg)
}

# Get cluster data metadata
if (!is.na(fn.clusterinfo)) {
  data_clusterInfo <- read.delim(fn.clusterinfo, header = TRUE, sep = "\t"
                                 , stringsAsFactors = FALSE)
} else {
  msg <- "fn.clusterinfo is NA"
  message(msg)
}

# Get background data (StreamCat)
if (!is.na(fn.bkgdata)) {
  df_bkgdata <- read.table(fn.bkgdata, header = TRUE, sep = "\t"
                           , na.strings = c("","NA"))
} else {
  msg <- "fn.bkgdata is NA"
  message(msg)
}

# Get background metadata
if (!is.na(fn.bkginfo)) {
  df_bkginfo <- read.table(fn.bkginfo, header = TRUE, sep = "\t"
                           , na.strings = c("", "NA")
                           , stringsAsFactors = FALSE)
} else {
  msg <- "fn.bkginfo is NA"
  message(msg)
}

# Get Bray-Curtis dissimilarity matrix
if (useBC == TRUE & !is.na(fn.bcdist)) {
  # Get BC dissimilarity distance matrix to subset cluster sites to comparators
  data_BCdist <- read.delim(fn.bcdist, header = TRUE, sep = "\t"
                            , stringsAsFactors = FALSE)
} else if (useBC == FALSE) {
  msg <- "Use biological filter is FALSE"
  message(msg)

} else {
  msg <- "fn.bcdist is NA"
  message(msg)

}## IF ~ useBC ~ END

# remove filename variables
rm(fn.Sites.Info, fn.cluster, fn.clusterinfo, fn.bkginfo, fn.bkginfo, fn.bcdist)

#~~~~~~~~~~~~~~~~~~~~~~~
# 05, CAST, Measured data and metadata ####
# Progress, 05
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Chem"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

## Get metadata for all measured stressors
if (!is.na(fn.cheminfo)) {
  data_chemInfo   <- read.delim(fn.cheminfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  data_chemInfo   <- data_chemInfo %>%
    mutate(Analyte = StdParamName) %>%
    filter(UseInStressorID == 1)
  colMeasInvScore <- as.vector(data_chemInfo$StdParamName[data_chemInfo$DirIncStress == "Dec"])
  SSTVparms       <- unique(data_chemInfo$StdParamName[data_chemInfo$SSTV == 1])
} else {
  msg <- "fn.cheminfo is NA"
  message(msg)
}
rm(fn.cheminfo)

## Get measured stressor values
if (!is.na(fn.chemdata)) {
  data_chemAll <- read.delim(fn.chemdata, header = TRUE, sep = "\t",
                             na.strings = "NA", stringsAsFactors = FALSE)
  ## Subset for only Use == TRUE
  # analytes     <- data_stressInfo$StdParamName[data_stressInfo$UseInStressorID == 1]
  analytes     <- as.character(data_stressInfo$StdParamName)
  data_chemRaw <- data_chemAll[data_chemAll$StdParamName %in% analytes,]

  ## Average duplicate data
  data_chemRaw <- data_chemRaw %>%
    mutate(SampleDate = lubridate::mdy(SampDate)) %>%
    select(StationID_Master, ChemSampleID, SampDate, StdParamName
           , ResultValue, SampleDate) %>%
    group_by(StationID_Master, ChemSampleID, SampDate, StdParamName
             , SampleDate) %>%
    summarize(MeanResultValue = mean(ResultValue), .groups = "drop_last") %>%
    rename(ResultValue = MeanResultValue)
  data_chemRaw <- unique(data_chemRaw) # should be redundant

  ## Get measured parameter names and separately, algal parameter names
  measParams <- as.vector(unique(data_chemRaw$StdParamName))
  algParams  <- as.vector(unique(data_chemRaw$StdParamName[grepl("^AFDM|^Chlor_a|^Pheophytin"
                                                                 ,data_chemRaw$StdParamName)]))

  ## getOutliers returns a dataframe with ChemSampleID, StdParamName, ResultValue,
  ## IQRmethod, SDmethod, Outlier
  data_outliers <- getOutliers(df_data = data_chemRaw
                               , df_meta = data_chemInfo)
  ## Merge outlier flags with raw data by sample ID (should be all.y not all.x) -- CHECK!
  data_chemRaw <- merge(data_chemRaw, data_outliers
                        , by.x = c("ChemSampleID", "StdParamName", "ResultValue")
                        , by.y = c("ChemSampleID", "StdParamName", "ResultValue")
                        , all.x = TRUE)
  data_chemRaw <- data_chemRaw[,c("StationID_Master", "ChemSampleID", "SampDate"
                                  , "StdParamName", "ResultValue", "SampleDate"
                                  , "IQRmethod", "SDmethod", "Outlier")]
  # Clean up
  rm(data_chemAll)
} else {
  msg <- "fn.chemdata is NA"
  message(msg)
  data_chemRaw <- NULL
}
rm(fn.chemdata)


#~~~~~~~~~~~~~~~~~~~~~~~
# 06, CAST, Modeled data and metadata ####
# Progress, 06
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Model"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# Get metadata for modeled stressor data
if (!is.na(fn.modelinfo)) {
  data_modelInfo   <- read.delim(fn.modelinfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  data_modelInfo   <- data_modelInfo %>%
    mutate(Analyte = StdParamName) %>%
    filter(UseInStressorID == 1)
  colModelInvScore <- as.vector(data_modelInfo$StdParamName[data_modelInfo$DirIncStress == "Dec"])
} else {
  msg <- "fn.modelinfo is NA"
  message(msg)
}
rm(fn.modelinfo)

# Get modeled stressor data
if (!is.na(fn.modeldata)) {
  data_modelAll <- read.delim(fn.modeldata, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  ## Subset for only Use == TRUE
  # useParams     <- data_modelInfo$StdParamName[data_modelInfo$UseInStressorID == 1]
  useParams     <- as.character(data_stressInfo$StdParamName)
  data_modelRaw <- data_modelAll[data_modelAll$StdParamName %in% useParams,]

  ## Obtain SampleYear -- but SampDate is all NA, so this is meaningless
  data_modelRaw <- data_modelRaw %>%
    mutate(SampYear = NA, SampleDate = NA) %>%
    select(StationID_Master, ChemSampleID, SampDate, StdParamName
           , ResultValue, SampleDate)

  ## getOutliers returns a dataframe with ChemSampleID, StdParamName, ResultValue,
  ## IQRmethod, SDmethod, Outlier
  data_modoutliers <- getOutliers(df_data = data_modelRaw
                                  , df_meta = data_modelInfo)

  ## Merge outlier flags with raw data by sample ID (should be all.y not all.x)
  data_modelRaw <- merge(data_modelRaw, data_modoutliers
                         , by.x = c("ChemSampleID", "StdParamName", "ResultValue")
                         , by.y = c("ChemSampleID", "StdParamName", "ResultValue")
                         , all.x = TRUE)
  data_modelRaw <- data_modelRaw[,c("StationID_Master", "ChemSampleID", "SampDate"
                                    , "StdParamName", "ResultValue", "SampleDate"
                                    , "IQRmethod", "SDmethod", "Outlier")]
  # Clean up
  rm(data_modelAll)
} else {
  msg <- "fn.modeldata is NA"
  message(msg)
  data_modelRaw <- NULL
}
rm(fn.modeldata)


#~~~~~~~~~~~~~~~~~~~~~~~
# 07, CAST, Combine measured and modeled data ####
# Progress, 07
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Combine stressor data and metadata"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# Combine metadata for all stressor into one datafile
if (exists(data_chemInfo) & exists(data_modelInfo)) {
  chemMetaNames  <- colnames(data_chemInfo)
  modelMetaNames <- colnames(data_modelInfo)
  extraNames     <- chemMetaNames[!(chemMetaNames %in% modelMetaNames)]
  for (e in 1:length(extraNames)) {
    newCol <- extraNames[e]
    data_modelInfo[[newCol]] <- NA
  }
  data_modelInfo  <- data_modelInfo[,chemMetaNames]
  data_stressInfo <- rbind(data_chemInfo, data_modelInfo)
  rm(data_chemInfo, data_modelInfo)
} else if (exists(data_chemInfo)) {
  data_stressInfo <- data_chemInfo
  rm(data_chemInfo)
} else if (exists(data_modelInfo)) {
  data_stressInfo <- data_modelInfo
  rm(data_modelInfo)
} else {
  msg <- "Neither measured nor modeled metadata are available"
  message(msg)
}

# Select only necessary columns -- ARL 2023-05-25
data_stressInfo <- dplyr::distinct(data_stressInfo, StdParamName, GroupNum
                                   , GroupName, LogTransf, SSD, SSTV, SensMin
                                   , SensMax, TolMin, TolMax, UseInStressorID
                                   , DirIncStress, SSTVname, Label)

# Combine measured and modeled parameters with inverse scoring
col_StressInvScore <- as.vector(data_stressInfo$StdParamName[data_stressInfo$DirIncStress == "Dec"])


# Bio responses
for (bio in seq_along(biocommlist)) {
  bio <- tolower(bio)
  #~~~~~~~~~~~~~~~~~~~~~~~
  # 08-10, CAST, Bio response data ####
  # Progress, 08-10
  if (boo_Shiny == TRUE) {
    prog_det <- paste0("Data, ", bio, ", Response data")
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END

  if (bio == "bmi") {
    # Get raw BMI data
    if (!is.na(fn.bmi.raw)) {
      data_BMIcounts <- read.table(fn.bmi.raw, header = TRUE, sep = "\t"
                                   , stringsAsFactors = FALSE)
    } else {
      msg <- "fn.bmi.raw is NA"
      message(msg)
    }
    rm(fn.bmi.raw)
    # Get BMI master taxa data
    if (!is.na(fn.MT.bmi)) {
      data_BMIMasterTaxa <- read.table(fn.MT.bmi, header = TRUE, sep = "\t"
                                       , stringsAsFactors = FALSE)
    } else {
      msg <- "fn.MT.bmi is NA"
      message(msg)
    }
    rm(fn.MT.bmi)
    # Get BMI metric data
    if (!is.na(fn.bmi.metrics)) {
      data_bmiMetrics <- read.delim(fn.bmi.metrics, header = TRUE, sep = "\t",
                                    na.strings = "NA", stringsAsFactors = FALSE)
      # data_bmiMetrics <- data_bmiMetrics[,c("StationID_Master", "BMISampID"
      #                                       , "BMISampDate", "Quality", "CSCI"
      #                                       , "MMI", "OoverE", "Taxonomic_Richness"
      #                                       , "Intolerant_Percent", "Shredder_Taxa"
      #                                       , "Clinger_PercentTaxa"
      #                                       , "Coleoptera_PercentTaxa"
      #                                       , "EPT_PercentTaxa")]
      # data_bmiMetrics <- data_bmiMetrics[, unlist(lapply(data_bmiMetrics,
      #                                                    function(x) !all(is.na(x))))]
      # colnames(data_bmiMetrics) <- c("StationID_Master","BMISampID"
      #                                , "CollDate", "Quality", "CSCI", "MMI"
      #                                , "OoverE", "Taxonomic_Richness"
      #                                , "Intolerant_Percent", "Shredder_Taxa"
      #                                , "Clinger_PercentTaxa", "Coleoptera_PercentTaxa"
      #                                , "EPT_PercentTaxa")
      data_bmiMetrics <- data_bmiMetrics %>%
        select_if(not_all_na) %>%
        mutate(BMISampDate = lubridate::mdy(CollDate)) %>%
        select(-CollDate)
      data_bmiMetrics <- unique(data_bmiMetrics)
    } else {
      msg <- "fn.bmi.metrics is NA"
      message(msg)
    }
    rm(fn.bmi.metrics)

    if (!is.na(fn.bmi.metrics.info)) {
      message("Read fn.bmi.metrics.info")
      data_bmiMetricsInfo <- read.delim(fn.bmi.metrics.info, header = TRUE, sep = "\t"
                                        , na.strings = "NA", stringsAsFactors = FALSE)
      # Is the following line necessary? -- CHECK!
      data_bmiMetricsInfo <- data_bmiMetricsInfo[, c("MetricName",	"MetricLabel", "IndexYN")]
      bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
      bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN == "Yes"])
    } else {
      msg <- "fn.bmi.metrics.info is NA"
      message(msg)
    }
    rm(fn.bmi.metrics.info)

    if (boo_Shiny == TRUE) {
      prog_det <- "Data, BMI, Response data"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_bmiCoOccur <- getCoOccurDataset(dataDir = dir_data
                                         , df_sites = data_Sites
                                         , df_model = data_modelRaw
                                         , df_meas = data_chemRaw
                                         , biocomm = "BMI"
                                         , df_resp = data_bmiMetrics
                                         , index = bmiIndex
                                         , lagdays = lagdays
                                         , removeOutliers = removeOutliers)
    # returns df_coOccur as data_bmiCoOccur
    # Are the following steps necessary? -- CHECK!
    message("BMI, Params Keep")
    bmiParamsKEEP <- setdiff(colnames(data_bmiCoOccur), bmiModelParamsDEL)
    message("BMI, Select Params Keep")
    data_bmiCoOccur <- dplyr::select(data_bmiCoOccur, all_of(bmiParamsKEEP))
    data_bmiCoOccur <- dplyr::select_if(data_bmiCoOccur, not_all_na)

  }
  if (bio == "alg") {
    if (boo_Shiny == TRUE) {
      prog_det <- "Data, Alg, Metrics, Metadata"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get raw algal data
    if (!is.na(fn.alg.raw)) {
      data_AlgCounts <- read.table(fn.alg.raw, header = TRUE, sep = "\t")
    } else {
      msg <- "fn.alg.raw is NA"
      message(msg)
    }
    rm(fn.alg.raw)

    # Get algal master taxa data
    if (!is.na(fn.MT.alg)) {
      data_AlgMasterTaxa <- read.table(fn.MT.alg, header = TRUE, sep = "\t",
                                       stringsAsFactors = FALSE)
    } else {
      msg <- "fn.MT.alg is NA"
      message(msg)
    }
    rm(fn.MT.alg)

    # Get algal metrics data
    if (!is.na(fn.alg.metrics)) {
      data_AlgMetrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
      data_AlgMetrics <- data_AlgMetrics %>%
        mutate(AlgSampDate = lubridate::mdy(AlgSampDate)) %>%
        mutate(AlgSampFlag = NA)
      # data_AlgMetrics <- dplyr::select(data_AlgMetrics, -all_of(algMetricsDiscard))
    } else {
      msg <- "fn.alg.metrics is NA"
      message(msg)
    }
    rm(fn.alg.metrics)

    # Get algal metrics metadata
    if (!is.na(fn.alg.metrics.info)) {
      data_AlgMetricsInfo <- read.delim(fn.alg.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      algMetrics <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN == 1])
      # algMetricsDiscard <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN == 0])
      algIndex <- as.character(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$IndexYN == "Yes"])

    } else {
      msg <- "fn.alg.metrics.info is NA"
      message(msg)
    }
    rm(fn.alg.metrics.info)

    if (boo_Shiny == TRUE) {
      prog_det <- "Data, Alg, Co-Occurrence"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_algCoOccur <- getCoOccurDataset(dataDir = dir_data
                                         , df_sites = data_Sites
                                         , df_model = data_modelRaw
                                         , df_meas = data_chemRaw
                                         , biocomm = "Alg"
                                         , df_resp = data_AlgMetrics
                                         , index = algIndex
                                         , lagdays = lagdays
                                         , removeOutliers = removeOutliers)
    # returns df_coOccur as data_algCoOccur
    algParamsKEEP   <- setdiff(colnames(data_algCoOccur), algParamsDEL)
    data_algCoOccur <- dplyr::select(data_algCoOccur, all_of(algParamsKEEP))
    data_algCoOccur <- dplyr::select_if(data_algCoOccur, not_all_na)

  }
  if (bio == "fish") {
    if (boo_Shiny == TRUE) {
      prog_det <- "Data, Fish, Metrics, Metadata"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get raw fish data
    if (!is.na(fn.fish.raw)) {
      data_FishCounts <- read.table(fn.fish.raw, header = TRUE, sep = "\t")
    } else {
      msg <- "fn.fish.raw is NA"
      message(msg)
    }
    rm(fn.fish.raw)

    # Get fish master taxa data
    if (!is.na(fn.MT.fish)) {
      data_FishMasterTaxa <- read.table(fn.MT.fish, header = TRUE, sep = "\t",
                                       stringsAsFactors = FALSE)
    } else {
      msg <- "fn.MT.fish is NA"
      message(msg)
    }
    rm(fn.MT.fish)

    # Get fish metrics data
    if (!is.na(fn.fish.metrics)) {
      data_FishMetrics <- read.table(fn.fish.metrics, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
      data_FishMetrics <- data_FishMetrics %>%
        mutate(FishSampleDate = lubridate::mdy(FishSampDate)) %>%
        mutate(FishSampFlag = NA)
    } else {
      msg <- "fn.fish.metrics is NA"
      message(msg)
    }
    rm(fn.fish.metrics)

    # Get fish metrics metadata
    if (!is.na(fn.fish.metrics.info)) {
      data_FishMetricsInfo <- read.delim(fn.fish.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      fishMetrics <- as.vector(data_FishMetricsInfo$MetricName[data_FishMetricsInfo$UseYN == 1])
      # fishMetricsDiscard <- as.vector(data_FishMetricsInfo$MetricName[data_FishMetricsInfo$UseYN == 0])
      fishIndex <- as.character(data_FishMetricsInfo$MetricName[data_FishMetricsInfo$IndexYN == "Yes"])

    } else {
      msg <- "fn.fish.metrics.info is NA"
      message(msg)
    }
    rm(fn.fish.metrics.info)

    if (boo_Shiny == TRUE) {
      prog_det <- "Data, Fish, Co-Occurrence"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_fishCoOccur <- getCoOccurDataset(dataDir = dir_data
                                         , df_sites = data_Sites
                                         , df_model = data_modelRaw
                                         , df_meas = data_chemRaw
                                         , biocomm = "Fish"
                                         , df_resp = data_FishMetrics
                                         , index = fishIndex
                                         , lagdays = lagdays
                                         , removeOutliers = removeOutliers)
    # returns df_coOccur as data_fishCoOccur
    # fishParamsKEEP   <- setdiff(colnames(data_fishCoOccur), fishParamsDEL)
    # data_algCoOccur <- dplyr::select(data_fishCoOccur, all_of(fishParamsKEEP))
    data_algCoOccur <- dplyr::select_if(data_algCoOccur, not_all_na)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~
# 11, CAST, Create sample summary file ####
# Progress, 11
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Create sample summary"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# Prepare stressor data
data_modeltrim <- as.data.frame(data_modelRaw) %>%
  dplyr::distinct(StationID_Master, ChemSampleID, StdParamName, SampleDate)

data_meastrim <- as.data.frame(data_chemRaw) %>%
  dplyr::distinct(StationID_Master, ChemSampleID, StdParamName, SampleDate)
# rbind measured and modeled data
data_stresstrim <- rbind(data_meastrim, data_modeltrim)

# Identify field, lab, phab, model types
data_stresstrim <- merge(data_stresstrim, data_stressInfo, by = "StdParamName")
data_stresstrim <- data_stresstrim %>%
  dplyr::select(StationID_Master, ChemSampleID, GroupName, StdParamName
                , SampleDate, Label) %>%
  dplyr::mutate(Type = case_when(GroupName == "Habitat" ~ "Habitat"
                                 , GroupName == "Modeled flow metrics" ~ "Modeled flow"
                                 , grepl("Field-measured", Label) == TRUE ~ "Field chemistry"
                                 , TRUE ~ "Lab chemistry")) %>%
  dplyr::distinct(StationID_Master, SampleDate, ChemSampleID, Type) %>%
  tidyr::pivot_wider(id_cols = c(StationID_Master, SampleDate), names_from = Type
                     , values_from = ChemSampleID, values_fill = "")

# Identify response samples
if (exists("data_bmiMetrics")) {
  data_bmiMetrics <- data_bmiMetrics %>%
    dplyr::distinct(StationID_Master, BMISampID, BMISampDate)
  data_stresstrim <- merge(data_stresstrim, data_bmiMetrics, by = "StationID_Master")
} else {
  msg <- "No BMI data available"
  message(msg)
}
if (exists("data_algMetrics")) {
  data_algMetrics <- data_algMetrics %>%
    dplyr::distinct(StationID_Master, AlgSampID, AlgSampDate)
  data_stresstrim <- merge(data_stresstrim, data_algMetrics, by = "StationID_Master")
} else {
  msg <- "No Algal data available"
  message(msg)
}
if (exists("data_fishMetrics")) {
  data_fishMetrics <- data_fishMetrics %>%
    dplyr::distinct(StationID_Master, FishSampID, FishSampDate)
  data_stresstrim <- merge(data_stresstrim, data_fishMetrics, by = "StationID_Master")
} else {
  msg <- "No Algal data available"
  message(msg)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      OLD CODE -- NOT NECESSARY?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prepare df_allStress file (write for RPP use) -- NOT required for OR/WA
# data_modeltrim <- as.data.frame(data_modelRaw) %>%
#   dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
#                 , ResultValue, IQRmethod, SDmethod, Outlier) #%>%
# # Sample date is already NA
# # dplyr::mutate(SampleDate = NA)
# data_meastrim <- as.data.frame(data_chemRaw) %>%
#   dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
#                 , ResultValue, IQRmethod, SDmethod, Outlier)
# # rbind measured and modeled data
# data_Stress <- rbind(data_meastrim, data_modeltrim)
# data_Stress <- data_Stress %>%
#   dplyr::mutate(IQRmethod = ifelse(!is.na(IQRmethod), IQRmethod, "NE")
#                 , SDmethod = ifelse(!is.na(SDmethod), SDmethod, "NE")) %>%
#   dplyr::mutate(Outlier = case_when((IQRmethod=="Good") & (SDmethod=="Good") ~ "Good"
#                                     , (IQRmethod == "NE") | (SDmethod == "NE") ~ "NE"
#                                     , TRUE ~ "Outlier"))

# Write stressor data and metadata for use in RPPTool
# fn.stress4RPP <- file.path(dir_data, "SMC_AllStressData.tab")
# fn.stressmeta4RPP <- file.path(dir_data, "SMC_AllStressInfo.tab")
# write.table(data_Stress, fn.stress4RPP, append = FALSE, col.names = TRUE
#             , row.names = FALSE, sep = "\t")
# write.table(data_stressInfo, fn.stressmeta4RPP, append = FALSE, col.names = TRUE
#             , row.names = FALSE, sep = "\t")

# # Get sample summary data (No ReachMod file or 303d file available 20200827)
# data_SampSummary <- read.delim(fn.SampSummary, header = TRUE, sep = "\t")
# # data_mods        <- data_ReachMod   # Check this
# # data_303d        <- data_303d       # Check this
# rm(fn.SampSummary)


# # 06, CAST, BMI, taxonomic data
# # Progress, 06
# if (boo_Shiny == TRUE) {
#   prog_det <- "Data, BMI, Taxonomic"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(mySleepTime)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# #
# data_BMIcounts <- read.table(fn.bmi.raw, header = TRUE, sep = "\t")
#                              #, stringsAsFactors = FALSE)
# data_BMIMasterTaxa <- read.table(fn.MT.bmi, header = TRUE, sep = "\t"
#                                  , stringsAsFactors = FALSE)
# rm(fn.bmi.raw, fn.MT.bmi)
#
# # 07, CAST, BMI, metrics
# # Progress, 07
# if (boo_Shiny == TRUE) {
#   prog_det <- "Data, BMI, Metrics"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(mySleepTime)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# #
# data_bmiMetrics <- read.delim(fn.bmi.metrics, header = TRUE, sep = "\t",
#                               na.strings = "NA", stringsAsFactors = FALSE)
# data_bmiMetrics <- data_bmiMetrics[,c("StationID_Master", "BMISampID"
#                                       , "BMISampDate", "Quality", "CSCI"
#                                       , "MMI", "OoverE", "Taxonomic_Richness"
#                                       , "Intolerant_Percent", "Shredder_Taxa"
#                                       , "Clinger_PercentTaxa"
#                                       , "Coleoptera_PercentTaxa"
#                                       , "EPT_PercentTaxa")]
# data_bmiMetrics <- data_bmiMetrics[, unlist(lapply(data_bmiMetrics,
#                                                    function(x) !all(is.na(x))))]
# colnames(data_bmiMetrics) <- c("StationID_Master","BMISampID"
#                                , "CollDate", "Quality", "CSCI", "MMI"
#                                , "OoverE", "Taxonomic_Richness"
#                                , "Intolerant_Percent", "Shredder_Taxa"
#                                , "Clinger_PercentTaxa", "Coleoptera_PercentTaxa"
#                                , "EPT_PercentTaxa")
# data_bmiMetrics <- data_bmiMetrics %>%
#   mutate(BMISampDate = lubridate::mdy(CollDate)) %>%
#   select(-CollDate)
# data_bmiMetrics <- unique(data_bmiMetrics)
# rm(fn.bmi.metrics)
#
# data_cscicore <- read.delim(fn.bmi.cscicore, header = TRUE, sep = "\t"
#                             , na.strings = "NA", stringsAsFactors = FALSE)
# data_cscicore <- data_cscicore[,c("stationid", "county", "smcshed", "latitude"
#                                   , "longitude", "stationcode", "sampleid"
#                                   , "samplemonth", "sampleday", "sampleyear"
#                                   , "collectionmethodcode", "fieldreplicate"
#                                   , "count", "pcnt_ambiguous_individuals")]
# data_cscicore <- data_cscicore %>%
#   mutate(date_text = paste(samplemonth, sampleday, sampleyear, sep = "/")
#          , BMISampID = paste(stationid, date_text, collectionmethodcode
#                              , fieldreplicate, sep = "_")
#          , BMISampFlag = ifelse((count < 250) & (pcnt_ambiguous_individuals > 50)
#                                 , "Insufficient individuals and large percent ambiguity"
#                                 , ifelse(count < 250, "Insufficient individuals"
#                                          , ifelse(pcnt_ambiguous_individuals > 50
#                                                   , "Large percent ambiguity"
#                                                   , NA)))) %>%
#   rename(StationID_Master = stationid, BMISampCount = count
#          , PctAmbigInd = pcnt_ambiguous_individuals) %>%
#   select(StationID_Master, BMISampID, BMISampCount, PctAmbigInd, BMISampFlag)
# data_cscicore <- unique(data_cscicore)
#
# data_bmiMetrics <- merge(data_bmiMetrics, data_cscicore
#                          , by.x = c("StationID_Master", "BMISampID")
#                          , by.y = c("StationID_Master", "BMISampID")
#                          , all.x = TRUE)
#
# data_tmpbmicount <- unique(data_BMIcounts[,c("BMISampID","SampleTotAbund")])
# data_bmiMetrics <- data_bmiMetrics %>%
#   mutate(BMISampCount = ifelse(is.na(BMISampCount)
#                                , data_tmpbmicount$SampleTotAbund
#                                , BMISampCount)) %>%
#   mutate(BMISampFlag = ifelse(is.na(BMISampFlag) & (BMISampCount < 250)
#                               , "Insufficient number of individuals", BMISampFlag))
# rm(data_tmpbmicount)
#
# data_bmiMetrics <- data_bmiMetrics %>%
#   mutate(BMISampFlag = ifelse(is.na(PctAmbigInd) & is.na(BMISampFlag)
#                               , ifelse(BMISampCount >= 250
#                                        , paste0("Unknown percent ambiguous individuals")
#                                        , paste0("Unknown number of and percent "
#                                                 , "ambiguous individuals"))
#                               , ifelse(is.na(PctAmbigInd)
#                                        , paste0("Insufficient number of and unknown "
#                                                 ,"percent ambiguous individuals")
#                                        , BMISampFlag)))
#
# # 08, CAST, BMI, metrics metadata
# # Progress, 08
# if (boo_Shiny == TRUE) {
#   prog_det <- "Data, BMI, Metrics, Metadata"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(mySleepTime)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# #
# message("Read fn.bmi.metrics.info")
# data_bmiMetricsInfo <- read.delim(fn.bmi.metrics.info, header = TRUE, sep = "\t"
#                                   , na.strings = "NA", stringsAsFactors = FALSE)
# message("Select data")
# data_bmiMetricsInfo <- data_bmiMetricsInfo[,c("MetricName",	"MetricLabel", "IndexYN")]
# bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
# bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN == "Yes"])
#
# # 09, CAST, BMI, Co-Occurence
# # Progress, 09
# if (boo_Shiny == TRUE) {
#   prog_det <- "Data, BMI, Co-Occurrence"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(mySleepTime)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# #
# # Generate co-occurrence data set (same day samples; modeled data match any day)
# data_bmiCoOccur <- getCoOccurDataset(dataDir = dir_data
#                                      , df_sites = data_Sites
#                                      , df_model = data_modelRaw
#                                      , df_meas = data_chemRaw
#                                      , biocomm = "BMI"
#                                      , df_resp = data_bmiMetrics
#                                      , index = bmiIndex
#                                      , lagdays = lagdays
#                                      , removeOutliers = removeOutliers)
# # returns df_coOccur as data_bmiCoOccur
# message("BMI, Params Keep")
# bmiParamsKEEP <- setdiff(colnames(data_bmiCoOccur), bmiModelParamsDEL)
# message("BMI, Select Params Keep")
# data_bmiCoOccur <- dplyr::select(data_bmiCoOccur, all_of(bmiParamsKEEP))
# data_bmiCoOccur <- dplyr::select_if(data_bmiCoOccur, not_all_na)

# # 10, CAST, Alg, metrics metadata
# # Progress, 10
# if (boo_Shiny == TRUE) {
#   prog_det <- "Data, Alg, Metrics, Metadata"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(mySleepTime)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# #
# data_AlgMetricsInfo <- read.delim(fn.alg.metrics.info, header = TRUE, sep = "\t",
#                                   na.strings = "NA", stringsAsFactors = FALSE)
# algMetrics <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN == 1])
# algMetricsDiscard <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN == 0])
# algIndex <- as.character(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$IndexYN == "Yes"])
#
# # 11, CAST, Alg, metrics
# # Progress, 11
# if (boo_Shiny == TRUE) {
#   prog_det <- "Data, Alg, Metrics"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(mySleepTime)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# #
# data_AlgMetrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t",
#                               stringsAsFactors = FALSE)
# data_AlgMetrics <- data_AlgMetrics %>%
#   mutate(AlgSampDate = lubridate::mdy(AlgSampDate)) %>%
#   mutate(AlgSampFlag = NA)
# data_AlgMetrics <- dplyr::select(data_AlgMetrics, -all_of(algMetricsDiscard))
# rm(fn.alg.metrics)
#
# # 12, CAST, Alg taxonomic data
# # Progress, 12
# if (boo_Shiny == TRUE) {
#   prog_det <- "Data, Alg, Taxonomic"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(mySleepTime)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# #
# message("Read fn.alg.raw")
# data_AlgCounts <- read.table(fn.alg.raw, header = TRUE, sep = "\t")
# message("Read fn.MT.alg")
# data_AlgMasterTaxa <- read.table(fn.MT.alg, header = TRUE, sep = "\t",
#                                  stringsAsFactors = FALSE)
# rm(fn.alg.raw, fn.MT.alg)
#
# # 13, CAST, Alg, Co-Occurence
# # Progress, 13
# if (boo_Shiny == TRUE) {
#   prog_det <- "Data, Alg, Co-Occurrence"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(mySleepTime)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# #
# # Generate co-occurrence data set (same day samples; modeled data match any day)
# data_algCoOccur <- getCoOccurDataset(dataDir = dir_data
#                                      , df_sites = data_Sites
#                                      , df_model = data_modelRaw
#                                      , df_meas = data_chemRaw
#                                      , biocomm = "Alg"
#                                      , df_resp = data_AlgMetrics
#                                      , index = algIndex
#                                      , lagdays = lagdays
#                                      , removeOutliers = removeOutliers)
# # returns df_coOccur as data_algCoOccur
# algParamsKEEP   <- setdiff(colnames(data_algCoOccur), algParamsDEL)
# data_algCoOccur <- dplyr::select(data_algCoOccur, all_of(algParamsKEEP))
# data_algCoOccur <- dplyr::select_if(data_algCoOccur, not_all_na)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# RUN CASTool
# 12, Target site Selection ####
# Progress, 12
if (boo_Shiny == TRUE) {
  prog_det <- "Site Selection"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
df_targets <- read_excel(fn.targets, col_names = TRUE, trim_ws = TRUE, skip = 0)

endprep.time <- Sys.time()
elapsedprep.time <- endprep.time - startprep.time
msg <- paste("Prep completed in", elapsedprep.time)
message(msg)

ifelse(!dir.exists(file.path(dir_results)) == TRUE
       , dir.create(file.path(dir_results))
       , FALSE)

fn_runstats <- paste0("RunStats_", format.Date(Sys.Date(),"%Y%m%d"), ".tab")
df_runstats <- as.data.frame(cbind("TargetSiteID", "Biocomm", "NumStressors"
                                   , "NumLoE", "ElapsedTime"))
write.table(df_runstats, file.path(dir_results,fn_runstats), append = FALSE
            , col.names = FALSE, row.names = FALSE, sep = "\t")

### Evaluate each target site
## Use this for debugging
if (boo_Shiny == TRUE) {
  df_targets <- data.frame("TargetSiteID" = input$Station, "Chosen by" = NA, "Comment" = NA)
  names(df_targets)[2] <- "Chosen by"
} else if (boo.debug == TRUE & debug.person == "Ann") {
  # df_targets <- dplyr::filter(df_targets, TargetSiteID  == "SMC04134")
  df_targets <- dplyr::filter(df_targets, TargetSiteID %in% c("SMC04134", "402BA0031"))
  msg <- paste0("Number of target sites = ", nrow(df_targets))
  message(msg)
}

# 15, Main Code ####
# Progress, 15
if (boo_Shiny == TRUE) {
  prog_det <- "Main Code Start"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
# FOR ~ site ~ START ####
for (site in 1:nrow(df_targets)) {
  startsite.time <- Sys.time()
  TargetSiteID <- df_targets$TargetSiteID[site]

  if (is.na(TargetSiteID)) {
    next()
  }
  msg <- paste0("Evaluating site: ",TargetSiteID)
  message(msg)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Biocomm-independent functions

  # Create high-level results folder structure
  dir_sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(dir_results, dir_sub2)) == TRUE
         , dir.create(file.path(dir_results, dir_sub2))
         , FALSE)

  # Establish data gaps file
  gaps <- cbind.data.frame("fxnname", "condition", "result", "comment")
  fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
  fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
  write.table(gaps, fn.gaps, append = FALSE, col.names = FALSE
              , row.names = FALSE, sep = "\t")


  # 16, getComparators ####
  # Progress, 16
  if (boo_Shiny == TRUE) {
    prog_det <- "getComparators"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Identify comparator sites
  # This is predicated on the fact that BC distance is calculated based on
  # expected benthic macroinvertebrate taxa. If there are ever different
  # BC matrices for different biocomms, then this must move into the biocomm
  # loop or it needs to be run more than once for each biocomm here, since
  # it's used in getSiteInfo immediately afterward.
  list.CompSites <- getComparators(TargetSiteID
                                   , df_sites = data_Sites
                                   , df_bioCoOccur = data_bmiCoOccur
                                   , bioIndex = bmiIndex
                                   , useBC = useBC
                                   , df_bcdist = data_BCdist
                                   , bc_cutoff = 0.05
                                   , dir_results = dir_results
                                   , dir_sub = "SiteInfo")
  # Returns: myCompSites <- list(comp.sites = comp.sites
  #                             , gap.compsites = gap.statement
  comp_sites <- list.CompSites$comp.sites
  msg <- "getComparators is complete."
  message(msg)

  # 17, getSiteInfo ####
  # Progress, 17
  if (boo_Shiny == TRUE) {
    prog_det <- "getSiteInfo"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  # Get site information for general use (map, sample summary, etc)

  # Map plots only ref sites, and that's probably for the best
  list.SiteSummary <- getSiteInfo(TargetSiteID = TargetSiteID
                                  , data_Sites = data_Sites
                                  , data_bkgdata = df_bkgdata
                                  , data_bkginfo = df_bkginfo
                                  , data_SampSummary = data_SampSummary
                                  , data_303d = NULL
                                  , data_bmiMetrics = data_bmiMetrics
                                  , bmiIndexGp = bmiIndexGp
                                  , data_algMetrics = data_AlgMetrics
                                  , algIndexGp = algIndexGp
                                  , comp_sites = comp_sites
                                  , data_cluster = data_cluster
                                  , data_mods = NULL
                                  , dir_photo = file.path(dir_data,"Photos")
                                  , dir_results = dir_results
                                  , dir_sub = "SiteInfo")
  # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo,
  #                                Samps = mySamps,
  #                                BMImetrics = myBMImetrics,
  #                                AlgMetrics = myAlgaeMetrics,
  #                                COMID = myCOMID,
  #                                ClustID = myClustID,
  #                                impair = myImpairments,
  #                                mods = myReachMods
  #                                refCOMIDs = myRefCOMIDs)
  getSiteMap(sp_outline = sp_outline
             , sp_flowline = sp_flowline
             , allSites = data_Sites
             , TargetSite = TargetSiteID
             , dir_results = dir_results
             , dir_sub = "SiteInfo"
             , dir_map_rmd = dir_rmd)
  # Prints static and leaflet maps (.png and .html)
  msg <- "getSiteInfo is complete."
  message(msg)

  # 18, Get Cluster Info ####
  # Progress, 18
  if (boo_Shiny == TRUE) {
    prog_det <- "getClusterInfo"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Get Cluster Info
  if (printClusterInfo == TRUE) {
    getClusterInfo(TargetSiteID
                   , siteCOMID = list.SiteSummary$COMID
                   , siteCluster = list.SiteSummary$ClustID
                   , refSiteCOMIDs = list.SiteSummary$refCOMIDs
                   , data_cluster = data_cluster
                   , data_clusterInfo = data_clusterInfo
                   , dir_results = dir_results
                   , dir_sub = "ClusterInfo"
                   , boo_plot = boo_plot_user)
    msg <- "getClusterInfo is complete."
    message(msg)
  }## IF ~ printClusterInfo ~ END

  # 19, Munge str/resp ####
  # Progress, 19
  if (boo_Shiny == TRUE) {
    prog_det <- "Munge, Str/Resp"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare flags for types of stressor and response data to use
  avail.data <- data_SampSummary[data_SampSummary$StationID_Master == TargetSiteID,]
  avail.data <- avail.data[,c(6:ncol(avail.data))]
  avail.data <- avail.data %>% select_if(not_all_na)
  samptypes <- names(avail.data)

  if (any(samptypes %in% meas.stress)) { # Either chem or phab samps exist
    useMeasStress = TRUE
    if (!any(samptypes %in% chem.stress)) {         # No chem samps
      gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0
                                          , "No chemistry stressors available.")
      colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")

      gap.phab.stress <- cbind.data.frame("general", "HabStress", 1
                                          , "Habitat stressors available.")
      colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")

    } else if (!any(samptypes %in% hab.stress)) {   # No habitat samps
      gap.phab.stress <- cbind.data.frame("general", "HabStress", 0
                                          , "No habitat stressors available.")
      colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")

      gap.chem.stress <- cbind.data.frame("general", "ChemStress", 1
                                          , "Chemistry stressors available.")
      colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
    } else {
      gap.phab.stress <- cbind.data.frame("general", "HabStress", 1
                                          , "Habitat stressors available.")
      colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")

      gap.chem.stress <- cbind.data.frame("general", "ChemStress", 1
                                          , "Chemistry stressors available.")
      colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
    }
    df_allStress <- data_chemRaw
  } else {# No measured stressors at all
    useMeasStress = FALSE
    gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0, "No chemistry stressors available.")
    colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")

    gap.phab.stress <- cbind.data.frame("general", "HabStress", 0, "No habitat stressors available.")
    colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
  } ### End If statement for measured stressors


  if (any(samptypes %in% mod.stress)) {
    useModStress = TRUE
    gap.mod.stress <- cbind.data.frame("general", "useModStress", 1, "Modeled stressors available.")
    colnames(gap.mod.stress) <- c("fxnname", "condition", "result", "comment")
    if (exists("df_allStress") == TRUE) {
      df_allStress <- rbind(df_allStress, data_modelRaw)
    } else {
      df_allStress <- data_modelRaw
    }
  } else {
    useModStress = FALSE
    gap.mod.stress <- cbind.data.frame("general", "useModStress", 0, "No modeled stressors available.")
    colnames(gap.mod.stress) <- c("fxnname", "condition", "result", "comment")
  } ### End If statement for modeled stressors

  if (any(samptypes == bmiResp)) {
    useBMI = TRUE
    gap.bmi.rsp <- cbind.data.frame("general", "useBMI", 1, "BMI responses available.")
    colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
  } else{
    useBMI = FALSE
    gap.bmi.rsp <- cbind.data.frame("general", "useBMI", 0, "No BMI responses available.")
    colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
  } ### End If statement for benthic macroinvertebrate responses

  if (any(samptypes == algResp)) {
    useAlg = TRUE
    gap.alg.rsp <- cbind.data.frame("general", "useALG", 1, "Algae responses available.")
    colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
  } else {
    useAlg = FALSE
    gap.alg.rsp <- cbind.data.frame("general", "useALG", 0, "No algae responses available.")
    colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
  } ### End If statement for measured stressorsalgal responses

  gaps <- rbind.data.frame(gap.chem.stress, gap.phab.stress, gap.mod.stress
                           , gap.bmi.rsp, gap.alg.rsp)
  fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
  fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
  write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
              , row.names = FALSE, sep = "\t")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if ((useMeasStress == FALSE) & (useModStress == FALSE)) {
  #     # No stressor data available
  #     gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0, "No chemistry stressors available.")
  #     colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
  # }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # FOR ~ b ~ START ####
  if (boo.debug == TRUE & debug.person == "Erik") {
    # 1 = bmi, 2 = alg
    biocommlist <- "alg"
  }


  for (b in 1:length(biocommlist)) {

    noStressors <- FALSE
    noResponses <- FALSE

    NE_true <- FALSE

    if ((useMeasStress == FALSE) & (useModStress == FALSE)) {
      # No stressor data available
      gap.stress <- cbind.data.frame("general", "Stressors", 0
                                     , "No stressor data available.")
      colnames(gap.stress) <- c("fxnname", "condition", "result"
                                , "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gap.stress, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
      noStressors <- TRUE
    }

    if ((useAlg == FALSE) & (useBMI == FALSE)) {
      # No stressor data available
      gap.resp <- cbind.data.frame("general", "Responses", 0
                                   , "No response data available.")
      colnames(gap.resp) <- c("fxnname", "condition", "result"
                              , "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gap.resp, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
      noResponses <- TRUE
    }
    if ((noStressors == TRUE) | (noResponses == TRUE)) {
      msg <- ifelse((noStressors == TRUE) & (noResponses == TRUE)
                    , paste0("No stressor or response data are available for "
                             , TargetSiteID)
                    , ifelse(noStressors == TRUE
                             , paste0("No stressor data are available for "
                                      , TargetSiteID)
                             , paste0("No response data are available for "
                                      , TargetSiteID)))
      message(msg)
      next
    }

    numLoE = 0

    LoEs <- c("TS", "CO", "SR", "VP", "SSD")
    df_LoE <- as.data.frame(LoEs)
    colnames(df_LoE) <- "LoE"
    df_LoE <- df_LoE %>%
      mutate(LoE = as.character(LoE)
             , Completed = as.integer(0)
             , ResultsDir = as.character(NA))

    # Define biocomm data
    bioComm <- biocommlist[b]
    if ((bioComm == "bmi") && (useBMI == TRUE)) {
      data_bioCoOccur <- data_bmiCoOccur
      bioIndex <- bmiIndex
      bioIndexGp <- bmiIndexGp
      bioMetricNames <- bmiMetrics
      bioMetricData <- data_bmiMetrics
      bioMetricInfo <- data_bmiMetricsInfo
      bioTaxaData <- data_BMIcounts
      bioMasterTaxa <- data_BMIMasterTaxa
      colBio <- bmiIndex
      colBioSample <- bmiResp
      colBioSampDate <- bmiRespDate
      BioNarBrk <- bmi_thresholds
      BioNarLab <- bmi_narrative
      BioDegBrk <- bmi_deg_thres
      BioDegLab <- bmi_deg_text
      modelParams <- bmiModelParamsKeep
      bioParmsDEL <- bmiModelParamsDEL
    } else if ((bioComm == "algae") && (useAlg == TRUE)) {
      data_bioCoOccur <- data_algCoOccur
      bioIndex <- algIndex
      bioIndexGp <- algIndexGp
      bioMetricNames <- algMetrics
      bioMetricData <- data_AlgMetrics
      bioMetricInfo <- data_AlgMetricsInfo
      bioTaxaData <- data_AlgCounts
      bioMasterTaxa <- data_AlgMasterTaxa
      colBio <- algIndex
      colBioSample <- algResp
      colBioSampDate <- algRespDate
      BioNarBrk <- alg_thresholds
      BioNarLab <- alg_narrative
      BioDegBrk <- alg_deg_thres
      BioDegLab <- alg_deg_text
      modelParams <- algModelParamsKeep
      bioParmsDEL <- algParamsDEL
    } else {
      msg <- paste0(bioComm, " is not a valid biological community.")
      message(msg)
      next()
    }

    # If no paired stressor-response samples for target site, no eval possible
    if (!(TargetSiteID %in% data_bioCoOccur$StationID_Master)) { # Not in data_bioCoOccur
      noStressors = TRUE
    } else {
      dfTarget <- dplyr::filter(data_bioCoOccur, StationID_Master == TargetSiteID)
      if (all(is.na(dfTarget[,11:ncol(dfTarget)]))) { # In data_bioCoOccur but all values NA
        noStressors = TRUE
      } else {
        noStressors = FALSE
      }
    }

    # If no paired stressors, write to data gaps file
    if (noStressors == TRUE) {
      msg <- paste0("No paired stressor-response samples for", TargetSiteID
                    , " for the ", bioComm, " community.")
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      gapcomment <- paste0("No paired stressor-", bioComm, " samples are available "
                           , "for ", TargetSiteID, " within ", lagdays, " days, "
                           , "with the stressor sample being obtained prior "
                           , "to the response sample.")
      gaps <- cbind.data.frame("getCoOccurDataset", paste0("Paired stressor-"

                                                           , bioComm, " data"), 0, gapcomment)

      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

      # Write run-time stats to file
      endsite.time <- Sys.time()
      elapsedsite.time <- endsite.time - startsite.time

      df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                     , "Biocomm" = bioComm
                                     , "NumStressors" = NA
                                     , "NumLoE" = numLoE
                                     , "ElapsedTime" = elapsedsite.time))
      write.table(df_temp, file.path(dir_results,fn_runstats)
                  , append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

      rm(dfTarget)
      next()
    } ### End no stressors statement

    # 20, getQualSites ####
    # Progress, 20
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; getQualSites")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Run analyses
    # Identify "quality" sites using different definitions
    list.BioQualSites <- getQualSites(TargetSiteID
                                      , df_sites = data_Sites
                                      , biocomm = bioComm
                                      , df_qual = data_bioCoOccur
                                      , colBio = colBio
                                      , colBioSample = "RespSampID"
                                      , colStressSample = "StressSampID"
                                      , comp_sites = comp_sites
                                      , useBC = useBC
                                      , BioNarBrk = BioNarBrk
                                      , BioNarLab = BioNarLab
                                      , BioDegBrk = BioDegBrk
                                      , BioDegLab = c("Yes", "No")
                                      , dir_results = dir_results)
    # Returns: myQualSites <- list(dfQuality = df_qual
    #                              , allRefBioSites = all.ref
    #                              , allRefBioRespSamps = all.ref.samps.bio
    #                              , allRefBioStressSamps = all.ref.samps.stress
    #                              , allRefBioReaches = all.ref.reaches
    #                              , allGoodBioSites = all.good
    #                              , allGoodBioRespSamps = all.samp.good.bio
    #                              , allGoodBioStressSamps = all.samp.good.stress
    #                              , allGoodBioReaches = all.good.reaches
    #                              , allBTBioSites = all.better
    #                              , allBTBioRespSamps = all.samp.better.bio
    #                              , allBTBioStressSamps = all.samp.better.stress
    #                              , allBTBioReaches = all.better.reaches)

    allBioRefSites <- switch(siteQual2Plot
                             , "reference" = list.BioQualSites$allRefBioSites
                             , "not degraded" = list.BioQualSites$allGoodBioSites
                             , "better than" = list.BioQualSites$allBTBioSites)
    allBioRefRespSamps <- switch(siteQual2Plot
                                 , "reference" = list.BioQualSites$allRefBioRespSamps
                                 , "not degraded" = list.BioQualSites$allGoodBioRespSamps
                                 , "better than" = list.BioQualSites$allBTBioRespSamps)
    allBioRefStressSamps <- switch(siteQual2Plot
                                   , "reference" = list.BioQualSites$allRefBioStressSamps
                                   , "not degraded" = list.BioQualSites$allGoodBioStressSamps
                                   , "better than" = list.BioQualSites$allBTBioStressSamps)
    allBioRefReaches <- switch(siteQual2Plot
                               , "reference" = list.BioQualSites$allRefBioReaches
                               , "not degraded" = list.BioQualSites$allGoodBioReaches
                               , "better than" = list.BioQualSites$allBTBioReaches)
    msg <- paste0("getQualSites is complete for ", bioComm, ".")
    message(msg)

    # 21, getDataSets ####
    # Progress, 21
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; getDataSets")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get data sets for stressors paired with response data, if available
    listPairedStressResp <- getDataSets(TargetSiteID
                                        , compSites = comp_sites
                                        , df_coOccur = data_bioCoOccur
                                        , measParams = measParams
                                        , modelParams = modelParams
                                        , biocomm = bioComm
                                        , bioIndex = bioIndex
                                        , colBioSample = colBioSample
                                        , colBioSampDate = colBioSampDate
                                        , df_biometrics = bioMetricData
                                        , df_stressinfo = data_stressInfo)
    # Returns: mySubsets <- list(siteStressInfo = df_stressinfo
    #                   , allBioStress = allBioStressData
    #                   , compBioStress = compBioStressData
    #                   , siteBioStress = siteBioStressData
    #                   , allBioResp = allBioRespData
    #                   , compBioResp = compBioRespData
    #                   , siteBioResp = siteBioRespData)
    msg <- "Stressor and response data prepared, for all possible stressors."
    message(msg)

    compPairedSR <- listPairedStressResp$compBioStress %>%
      select(-StressSampDate, -RespSampDate, -RespSampID)
    sitePairedSR <- listPairedStressResp$siteBioStress %>%
      select(-StressSampDate, -RespSampDate, -RespSampID)
    sitePairedStressors <- as.vector(colnames(sitePairedSR[,3:ncol(sitePairedSR)]))
    # message("paired")

    # Prepare data sets of all stressors ever detected at the target site
    if (removeOutliers == TRUE) {
      siteStressAll <- data_Stress %>%
        dplyr::filter(StationID_Master == TargetSiteID) %>%
        dplyr::filter(!is.na(ResultValue)) %>%
        dplyr::filter(Outlier != "Outlier")
      siteStressAll <- tidyr::pivot_wider(siteStressAll
                                          , names_from = StdParamName
                                          , values_from = ResultValue) %>%
        dplyr::rename(StressSampID = ChemSampleID
                      , StressSampDate = SampleDate)
      if (ncol(siteStressAll) > 7) {
        siteStressAllCore <- siteStressAll[1:6]
        siteStressAllParms <- siteStressAll[,7:ncol(siteStressAll)] %>%
          dplyr::select_if(not_all_na)
        siteStressAll <- cbind(siteStressAllCore, siteStressAllParms)
        rm(siteStressAllCore, siteStressAllParms)
      }
      siteDetectsAll <- as.vector(colnames(siteStressAll[,4:ncol(siteStressAll)]))
      compStressAll <- data_Stress %>%
        dplyr::filter(StationID_Master %in% comp_sites) %>%
        dplyr::filter(!is.na(ResultValue)) %>%
        dplyr::filter(Outlier != "Outlier") %>%
        dplyr::filter(StdParamName %in% siteDetectsAll)
      compStressAll <- tidyr::pivot_wider(compStressAll
                                          , names_from = StdParamName
                                          , values_from = ResultValue) %>%
        dplyr::rename(StressSampID = ChemSampleID
                      , StressSampDate = SampleDate)
      siteRespAll <- bioMetricData %>%
        dplyr::filter(StationID_Master == TargetSiteID) %>%
        dplyr::rename(RespSampID = eval(colBioSample)
                      , RespSampDate = eval(colBioSampDate))
    } else {# removeOutliers==FALSE
      siteStressAll <- data_Stress %>%
        dplyr::filter(StationID_Master == TargetSiteID) %>%
        dplyr::filter(!is.na(ResultValue)) #%>%
      # dplyr::filter(Outlier != "Outlier")
      siteStressAll <- tidyr::pivot_wider(siteStressAll
                                          , names_from = StdParamName
                                          , values_from = ResultValue) %>%
        dplyr::rename(StressSampID = ChemSampleID
                      , StressSampDate = SampleDate)
      siteStressAll <- dplyr::select_if(siteStressAll
                                        , not_all_na(siteStressAll[7:ncol(siteStressAll)]))
      siteDetectsAll <- as.vector(colnames(siteStressAll[,4:ncol(siteStressAll)]))
      compStressAll <- data_Stress %>%
        dplyr::filter(StationID_Master %in% comp_sites) %>%
        dplyr::filter(!is.na(ResultValue)) %>%
        dplyr::filter(StdParamName %in% siteDetectsAll)
      compStressAll <- tidyr::pivot_wider(compStressAll
                                          , names_from = StdParamName
                                          , values_from = ResultValue) %>%
        dplyr::rename(StressSampID = ChemSampleID
                      , StressSampDate = SampleDate)
      siteRespAll <- bioMetricData %>%
        dplyr::filter(StationID_Master == TargetSiteID) %>%
        dplyr::rename(RespSampID = eval(colBioSample)
                      , RespSampDate = eval(colBioSampDate))
    }## IF ~ removeOutliers ~ END
    # message("remove Outliers")

    # Log removed outliers as data gaps
    # data_stressInfo <- listPairedStressResp$siteStressInfo
    data_StressLabeled <- merge(data_Stress
                                , listPairedStressResp$siteStressInfo[,c("StdParamName","Label")]
                                , by = "StdParamName"
                                , all.x =  TRUE)
    siteOutliers <- data_StressLabeled %>%
      dplyr::filter(StationID_Master == TargetSiteID) %>%
      dplyr::filter(!is.na(ResultValue)) %>%
      dplyr::filter(Outlier == "Outlier")
    compOutliers <- data_StressLabeled %>%
      dplyr::filter(StationID_Master %in% comp_sites) %>%
      dplyr::filter(!is.na(ResultValue)) %>%
      dplyr::filter(Outlier == "Outlier")
    allOutliers <- data_StressLabeled %>%
      dplyr::filter(!is.na(ResultValue)) %>%
      dplyr::filter(Outlier == "Outlier")
    message("Log removed outliers as data gaps.")

    if (nrow(siteOutliers) > 0) {
      for (r in 1:nrow(siteOutliers)) {
        stressor <- siteOutliers$StdParamName[r]
        strLabel <- siteOutliers$Label[r]
        result <- siteOutliers$ResultValue[r]
        siteID <- as.character(siteOutliers$StationID_Master[r])
        gapcomment <- paste0(siteID
                             , " value removed as an outlier."
                             , " Transformation applied prior to"
                             , " identification as necessary.")
        gaps <- cbind.data.frame("Site outliers", strLabel, result
                                 , gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
      }
    }## IF ~ siteOutliers ~ END
    message("site outliers")

    message(paste0("comp outliers, n = ", nrow(compOutliers)))
    if (nrow(compOutliers) > 0) {
      for (r in 1:nrow(compOutliers)) {
        stressor <- compOutliers$StdParamName[r]
        strLabel <- compOutliers$Label[r]
        result <- compOutliers$ResultValue[r]
        siteID <- as.character(compOutliers$StationID_Master[r])
        if (siteID != TargetSiteID) {
          gapcomment <- paste0(siteID
                               , " value removed as an outlier."
                               , " Transformation applied prior to"
                               , " identification as necessary.")
          gaps <- cbind.data.frame("Comparator outliers", strLabel, result
                                   , gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
        }
      }
    }## IF ~ compOutliers ~ END
    message("comp outliers")

    if (nrow(allOutliers) > 0) {
      for (r in 1:nrow(allOutliers)) {
        stressor <- allOutliers$StdParamName[r]
        strLabel <- allOutliers$Label[r]
        result <- allOutliers$ResultValue[r]
        siteID <- as.character(allOutliers$StationID_Master)[r]
        if (!(siteID %in% comp_sites)) {
          gapcomment <- paste0("Value removed as an outlier for site "
                               , siteID
                               , " Transformation applied prior to"
                               , " identification as necessary.")
          gaps <- cbind.data.frame("All data outliers", strLabel, result
                                   , gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
        }
      }
    }## IF ~ allOutliers ~ END
    message("all outliers")

    # 22, getStressorList ####
    # Progress, 22
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; getStressorList")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get Stressor List using all stressors ever detected at the target site
    list.stressors <- getStressorList(TargetSiteID
                                      , siteCluster = list.SiteSummary$ClustID # integer
                                      , chemInfo = data_stressInfo # dataframe
                                      , clusterChem = compStressAll # dataframe
                                      , siteQual2Plot = siteQual2Plot # character
                                      , refSamps = allBioRefStressSamps # vector
                                      , refSites = allBioRefSites # vector
                                      , siteChem = siteStressAll # dataframe
                                      , samplim = samplim # integer (# samples below which can't id)
                                      , probsHigh = probsHigh # numeric
                                      , probsLow = probsLow # numeric
                                      , DOlim = DOlim # numeric
                                      , pHlimLow = pHlimLow # numeric
                                      , pHlimHigh = pHlimHigh # numeric
                                      , biocomm = bioComm # character
                                      , bioParmsDEL = bioParmsDEL # vector
                                      , dir_results = dir_results # vector
                                      , dir_sub = "CandidateCauses"
                                      , boo_plot = boo_plot_user)
    # Returns: myStressors <- list(stressors = stressorlist
    #                     , site.stressor.pctrank = site.pctrank
    #                     , stressors_LogTransf
    #                     , stressors_Excepted)
    stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
    stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
    msg <- "getStressorList is complete."
    message(msg)

    stressorsNOpairing <- setdiff(stressors, sitePairedStressors)
    stressorsWPairedResponses <- intersect(stressors, sitePairedStressors)

    ### MODIFY siteStressAll to keep all core cols and only stressor cols

    # If no stressors are identified, no analyses can be performed. Error msg.
    if (length(stressors) == 0) {
      msg <- paste("No stressors identified for", TargetSiteID)
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      gapcomment <- paste0("No potential stressors fall outside the specified "
                           , "quantile range (", probsLow, " to ", probsHigh,").")
      gaps <- cbind.data.frame("getStressorList", "Number of stressors", 0
                               , gapcomment)
      colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

      # Write run-time stats to file
      endsite.time <- Sys.time()
      elapsedsite.time <- endsite.time - startsite.time

      df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                     , "Biocomm" = bioComm
                                     , "NumStressors" = length(stressors)
                                     , "NumLoE" = numLoE
                                     , "ElapsedTime" = elapsedsite.time))
      write.table(df_temp, file.path(dir_results,fn_runstats)
                  , append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
      next()
    } ### End no stressors statement

    if (length(stressorsNOpairing) > 0) {
      for (s in 1:length(stressorsNOpairing)) {
        # Candidate causes identified as possible stressors but without
        # paired response data to allow evaluation
        # Grab labels instead of stdparamname
        stressname <- stressorsNOpairing[s]
        strLabel <- unique(as.character(data_stressInfo$Label[data_stressInfo$Analyte == stressname]))
        gapcomment <- paste0("Stressor detected but paired response "
                             ,"data are not available.")
        gaps <- cbind.data.frame("getStressorList", strLabel, 0
                                 , gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
      }
    } ### End unpaired stressors statement

    if (length(stressorsWPairedResponses) == 0) {
      NE_true <- TRUE
      # Candidate causes identified as stressors had no response sample
      # obtained within lagdays following the stressor sample collection
      gapcomment <- paste0("No identified possible stressors had a response "
                           , "sample obtained within ", lagdays, " days of "
                           , "stressor sample collection.")
      gaps <- cbind.data.frame("getStressorList", "Paired stresssor/responses"
                               , 0, gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
    } else {
      NE_true <- FALSE
      stressorsUsed <- as.data.frame(stressorsWPairedResponses)
      colnames(stressorsUsed)[1] <- "Stressor"
      stressorsUsed <- merge(stressorsUsed
                             , data_stressInfo[,c("StdParamName","Label")]
                             , by.x = "Stressor"
                             , by.y = "StdParamName"
                             , all.x = TRUE)
      stressorsUsed <- unique(stressorsUsed)
      fn.stressorsUsed <- file.path(dir_results,TargetSiteID
                                    , toupper(bioComm)
                                    , "CandidateCauses/"
                                    , paste0(TargetSiteID, "_"
                                             ,toupper(bioComm)
                                             , "_CandCauses_StressorsEvaluated.tab"))
      write.table(stressorsUsed, fn.stressorsUsed, append = FALSE
                  , col.names = TRUE, row.names = FALSE, sep = "\t")
    } # End paired stressors statement

    # Either all are paired or some are
    stressors_logtransf <- data_stressInfo$LogTransf[data_stressInfo$StdParamName
                                                     %in% stressorsWPairedResponses]

    ## Evaluate LoEs ----
    # Continue evaluation if data are available
    if (NE_true) { # No paired stressor response data available. Move to next biocomm or site.
      # Write run-time stats to file
      endsite.time <- Sys.time()
      elapsedsite.time <- endsite.time - startsite.time

      df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                     , "Biocomm" = bioComm
                                     , "NumStressors" = length(stressors)
                                     , "NumLoE" = numLoE
                                     , "ElapsedTime" = elapsedsite.time))
      write.table(df_temp, file.path(dir_results,fn_runstats)
                  , append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")


    } else {

      # 23, getTimeSeq ####
      # Progress, 23
      if (boo_Shiny == TRUE) {
        prog_det <- "getTimeSeq"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      # Create time sequence graphics
      # Uses all site stressor and response data, but not paired
      getTimeSeq(TargetSiteID
                 , biocomm = bioComm
                 , BioResp = bioMetricNames
                 , df_stress = siteStressAll
                 , df_resp = siteRespAll
                 , stressors = stressorsWPairedResponses
                 , df_stressinfo = data_stressInfo
                 , df_respinfo = bioMetricInfo
                 , dir_results = dir_results
                 , dir_sub = "TimeSequence"
                 , boo_plot = boo_plot_user)
      msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
      message(msg)

      dirTS <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                         , "TimeSequence")
      if (dir.exists(dirTS) == TRUE) {
        if (length(list.files(dirTS)) > 0) {
          numLoE = numLoE + 1
          df_LoE$Completed[df_LoE$LoE == "TS"] <- 1
          df_LoE$ResultsDir[df_LoE$LoE == "TS"] <- dirTS
        }## IF ~ length ~ END
      }## IF ~ dir.exists(dirTS) ~ END

      # 24, getCoOccurr ####
      # Progress, 24
      if (boo_Shiny == TRUE) {
        prog_det <- "getCoOccurr"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      # Get Response-based co-occurrence
      if (TargetSiteID %in% unique(data_bioCoOccur$StationID_Master)) {
        msg <- "Starting Co-occurrence"
        message(msg)
        getCoOccur(df_data = data_bioCoOccur
                   , TargetSiteID = TargetSiteID
                   , col_ID = "StationID_Master"
                   , colStressSamp = "StressSampID"
                   , colRespSamp = "RespSampID"
                   , colGroup = "clust"
                   , colBio = colBio
                   , colStressors = stressorsWPairedResponses
                   , df_stressinfo = data_stressInfo
                   , BioNarBrk = BioNarBrk
                   , BioNarLab = BioNarLab
                   , BioDegBrk = BioDegBrk
                   , BioDegLab = c("Yes", "No")
                   , biocomm = bioComm
                   , dir_plots = dir_results
                   , dir_sub = "CoOccurrence"
                   , col_StressInvScore = col_StressInvScore
                   , boo_plot = boo_plot_user)
      } else {
        # gapcomment <- "Stressor detected but paired response not available"
        # gaps <- cbind.data.frame("getStressorList", stressorsNOpairing[s], 0
        #                          , gapcomment)
        # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        # fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        # write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
        #             , row.names = FALSE, sep = "\t")
      } ### End getCoOccur
      msg <- paste0("getCoOccur for ", bioComm, " is complete.")
      message(msg)

      dirCO <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                         , "CoOccurrence")
      if (dir.exists(dirCO) == TRUE) {
        if ((length(list.files(dirCO)) > 0) == TRUE) {
          numLoE = numLoE + 1
          df_LoE$Completed[df_LoE$LoE == "CO"] <- 1
          df_LoE$ResultsDir[df_LoE$LoE == "CO"] <- dirCO
        }
      }

      # Refine all.b.str, cl.b.str, and site.b.str for just identified stressors
      core.cols <- c("StationID_Master", "StressSampDate", "RespSampDate"
                     , "StressSampID", "RespSampID")

      all.b.str <- listPairedStressResp$allBioStress %>%
        select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
        select(StressSampID, RespSampID, StationID_Master
               , eval(stressorsWPairedResponses))
      cl.b.str <- listPairedStressResp$compBioStress %>%
        select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
        select(StressSampID, RespSampID, StationID_Master
               , eval(stressorsWPairedResponses))
      site.b.str <- listPairedStressResp$siteBioStress %>%
        select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
        select(StressSampID, RespSampID, StationID_Master
               , eval(stressorsWPairedResponses))

      all.b.rsp <- listPairedStressResp$allBioResp %>%
        select(RespSampID, StressSampID, StationID_Master, RespSampDate
               , Quality, eval(bioMetricNames))
      cl.b.rsp <- listPairedStressResp$compBioResp %>%
        select(RespSampID, StressSampID, StationID_Master, RespSampDate
               , Quality, eval(bioMetricNames))
      site.b.rsp <- listPairedStressResp$siteBioResp %>%
        select(RespSampID, StressSampID, StationID_Master, RespSampDate
               , Quality, eval(bioMetricNames))

      siteStressInfo <- listPairedStressResp$siteStressInfo

      list_MatchBioData <- list("all.b.str"    = all.b.str
                                , "cl.b.str"   = cl.b.str
                                , "site.b.str" = site.b.str
                                , "all.b.rsp"  = all.b.rsp
                                , "cl.b.rsp"   = cl.b.rsp
                                , "site.b.rsp" = site.b.rsp)

      # 25, getBioStressorResponses ####
      # Progress, 25
      if (boo_Shiny == TRUE) {
        prog_det <- "getBioStressorResponses"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      # Get Stressor Responses
      getBioStressorResponses(TargetSiteID
                              , stressors = stressorsWPairedResponses
                              , stressorInfo = siteStressInfo
                              , BioResp = bioMetricNames
                              , BioInfo = bioMetricInfo
                              , list.MatchBioData = list_MatchBioData
                              , ref.sites = allBioRefStressSamps
                              , siteQual2Plot = siteQual2Plot
                              , biocomm = bioComm
                              , dir_results = dir_results
                              , dir_sub = "StressorResponse"
                              , boo_plot = boo_plot_user)
      msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
      message(msg)

      dirSR <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                         , "StressorResponse")
      if (dir.exists(dirSR) == TRUE) {
        if (length(list.files(dirSR)) > 0) {
          numLoE = numLoE + 1
          df_LoE$Completed[df_LoE$LoE == "SR"] <- 1
          df_LoE$ResultsDir[df_LoE$LoE == "SR"] <- dirSR
        } else {
          numLoE = numLoE + 1
          df_LoE$Completed[df_LoE$LoE == "SR"] <- 0
          df_LoE$ResultsDir[df_LoE$LoE == "SR"] <- NA
          unlink(dirSR, recursive = TRUE)
        }
      }

      # 26, getVerifiedPredictions ####
      # Progress, 26
      if (boo_Shiny == TRUE) {
        prog_det <- "getVerifiedPredictions"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      # Get Stressor-specific regressions
      if (any(SSTVparms %in% stressorsWPairedResponses)) {
        getVerifiedPredictions(TargetSiteID
                               , SSTVanalytes = as.character(SSTVparms)
                               , colBioSample = colBioSample
                               , stressors = stressorsWPairedResponses
                               , stressorInfo = siteStressInfo
                               , dataBioTaxa = bioTaxaData
                               , dataMasterTaxa = bioMasterTaxa
                               , matchedData = list_MatchBioData
                               , BioIndex_Val = bioIndex
                               , BioIndex_Nar = "Quality"
                               , BioIndex_Nar_Deg = "Degraded"
                               , dir_results = dir_results
                               , dir_sub = "VerifiedPredictions"
                               , biocomm = bioComm
                               , boo_plot = boo_plot_user)
      } else {
        msg <- "No possible stressors have stressor-specific tolerance values."
        message(msg)
        gapcomment <- paste0("Stressors having stressor-specific tolerance "
                             , "values are not identified at this site.")
        gaps <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0
                                 , gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
      } ### End getVP evaluation

      msg <- paste0("getVerifiedPredictions for ", bioComm, " is complete.")
      message(msg)

      dirVP <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                         , "VerifiedPredictions")
      if (dir.exists(dirVP) == TRUE) {
        if (length(list.files(dirVP)) > 0) {
          numLoE = numLoE + 1
          df_LoE$Completed[df_LoE$LoE == "VP"] <- 1
          df_LoE$ResultsDir[df_LoE$LoE == "VP"] <- dirVP
        }
      }

      # # Not enabled yet
      # # getSSDs
      # # getSSDplot(Data, ResponseType, Taxa, Exposure)
      # # myDF <- data_SSD_generator
      # # myRT   <- "ResponseType"
      # # myTaxa <- "Taxa"
      # # myExp  <- "Exposure"
      # # Run function
      # # p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)

      # 27, getWOE ####
      # Progress, 27
      if (boo_Shiny == TRUE) {
        prog_det <- "getWOE"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      getWoE(TargetSiteID
             , biocomm = bioComm
             , index = bioIndex
             , dir_results = dir_results
             , dfLoE = df_LoE
             , dfQual = list.BioQualSites$dfQuality
             , dfStr = list_MatchBioData$site.b.str
             , dfRank = list.stressors$site.stressor.pctrank
             , dfStressInfo = siteStressInfo
             , df_coOccur = data_bioCoOccur
             , BioResp = bioMetricNames)
      msg <- paste0("getWoE for ", bioComm, " is complete.")
      message(msg)

    }## IF ~ NE_true ~ END

    # Write run-time stats to file
    endsite.time <- Sys.time()
    elapsedsite.time <- endsite.time - startsite.time

    df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                   , "Biocomm" = bioComm
                                   , "NumStressors" = length(stressors)
                                   , "NumLoE" = numLoE
                                   , "ElapsedTime" = elapsedsite.time))

    # if (site == 1) {
    #   df_runstats <- df_temp
    # } else {
    #   df_runstats <- rbind(df_runstats, df_temp)
    # } ### End gather run stats
    # Shiny mod (always 1)
    #df_runstats <- df_temp  # Shiny
    write.table(df_temp, file.path(dir_results,fn_runstats)
                , append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")

  } ### End biocomm loop
  # FOR ~ b ~ END ####

  # 28, getReport ####
  # Progress, 28
  if (boo_Shiny == TRUE) {
    prog_det <- "getReport"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Shiny add ons
  if (boo_Shiny == TRUE) {
    report_type     <- "preliminary"
  } else {
    report_type     <- "summary"
  }
  # dir_data_abs    <- normalizePath(dir_data)
  # dir_results_abs <- normalizePath(dir_results)
  # dir_rmd         <- normalizePath(dir_rmd)
  strFile_RMD     <- file.path(dir_rmd, paste0("Report_Results_", report_type, ".rmd"))
  message(paste0("file = ", strFile_RMD))
  message(paste0("exists = ", file.exists(strFile_RMD)))
  #
  # Get final report (Executive Summary style)
  # Report (rmd file) is not working, but since it will change, I'm simply commenting
  # out the code so it doesn't run  --- ARL 2023-05-26
  # getReport(TargetSiteID
  #           , probsHigh = probsHigh
  #           , probsLow = probsLow
  #           , useBMI = useBMI
  #           , useAlg = FALSE
  #           , useAlg = useAlg
  #           , useBC = TRUE
  #           , removeOutliers = removeOutliers
  #           , DOlim = DOlim
  #           , pHlimHigh = pHlimHigh
  #           , pHlimLow = pHlimLow
  #           , lagdays = lagdays
  #           , bmiIndex = bmiIndex
  #           , algIndex = algIndex
  #           , dir_data = dir_data
  #           , dir_results = dir_results
  #           , report_type = report_type
  #           , report_format = "html"
  #           , dir_rmd = dir_rmd)

  dfGaps <- read.table(file.path(dir_results, TargetSiteID
                                 , paste0(TargetSiteID,"_datagaps.tab"))
                       , header = TRUE, sep = "\t")
  dfGaps <- unique(dfGaps)
  write.table(dfGaps, file.path(dir_results, TargetSiteID
                                , paste0(TargetSiteID,"_datagaps.tab"))
              , append = FALSE, col.names = TRUE, row.names = FALSE
              , sep = "\t")

} ### End TargetSite loop # not used in Shiny
# FOR ~ site ~ END ####

rm(site)

# 29, getSummaryAllSites ####
# Progress, 29
if (boo_Shiny == TRUE) {
  prog_det <- "getSummaryAllSites"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
# getSummaryAllSites
getSummaryAllSites(biocommlist = c("bmi", "algae")
                   , bmiIndex = "CSCI"
                   , algIndex = "MMIhybrid"
                   , dir_data = dir_data
                   , dir_results = dir_results
                   , dir_sub = "WoE"
                   , df_sites = NULL)

msg <- "getSummaryAllSites is complete."
message(msg)

# rm(list=ls())

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, END ####
# external/RPPTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
