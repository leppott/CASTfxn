# Copyright 2024 TetraTech. All rights reserved.
# Use, copying, modification, or distribution of this file or any of its contents
# is expressly prohibited without prior written permission of TetraTech.
#
#
# CASTfxn (Specific for SMC)
# Erik.Leppo@tetratech.com, 20180710
# Ann.RoseberryLincoln@tetratech.com, 20230605
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.3.1
#
# library(devtools)
# install_github("leppott/CASTfxn")
# requires packages: dplyr, ggplot2, lubridate, purrr, readxl, sf, shiny,
#                    stringr, tibble, tidyr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Shiny code for use in Shiny App
# 2020-10-30, Erik
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#rm(list=ls())

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, Start ####
# external/CASTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

boo_Shiny <- FALSE

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
    # wd <- getwd() # "C:/Users/ann.lincoln/Documents" ARL 2023-05-22
    region <- "WA"
    # regionAbbr <- "WA"
    wd <- "C:/Users/ann.lincoln/Documents" # ARL 2023-06-29
    gitpath <- file.path(wd, "GitHub", "CASTfxn", "R") # ARL 2023-05-22
    dir_rmd <- file.path(wd, "GitHub", "CASTfxn", "inst", "rmd") # ARL 2023-05-22
    localdir <- file.path(wd, "CASTool_DATA")
    dir_data <- file.path(localdir, region, "Data")
    dir_results <- file.path(localdir, region, "Results")
    printClusterInfo <- FALSE
    boo_plot_user <- TRUE
    # NOTE: to run all sites, comment out line 639
    #if (boo.debug == TRUE & debug.person == "Ann") {
    source(file.path(gitpath, "getCoOccurDataset.R"))
    source(file.path(gitpath, "getTimeSeq.R"))
    source(file.path(gitpath, "getDataSets.R"))
    source(file.path(gitpath, "getComparators.R"))
    source(file.path(gitpath, "getAvailableDataTypes.R"))
    source(file.path(gitpath, "writeOutliers.R"))
    source(file.path(gitpath, "getSiteInfo.R"))
    source(file.path(gitpath, "getSiteMap.R"))
    source(file.path(gitpath, "getClusterInfo.R"))
    source(file.path(gitpath, "getStressorList.R"))
    source(file.path(gitpath, "getCoOccur.R"))
    source(file.path(gitpath, "getSufficiency.R"))
    source(file.path(gitpath, "getBioStressorResponses.R"))
    source(file.path(gitpath, "getVerifiedPredictions.R"))
    source(file.path(gitpath, "getOutliers.R"))
    source(file.path(gitpath, "getWoE.R"))
    source(file.path(gitpath, "getQualSites.R"))
    source(file.path(gitpath, "getSummaryAllSites.R"))
    source(file.path(gitpath, "getReport.R"))
    source(file.path(gitpath, "readCASToolData.R"))
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
}## IF ~ boo_Shiny ~ END

msg <- paste0("debug = ", boo.debug
              , ifelse(boo.debug == FALSE, ""
                       , paste0(", person = ", debug.person)))
message(msg)

startprep.time <- Sys.time()

#~~~~~~~~~~~~~~~~~~~~~~~
# 03, Select region variables ####
# Progress, 03
# region <- "WA" # options: SMC, AZ, WA, OR
#
# Read CASTool_Metadata.xlsx
fn.CASTmeta   <- file.path(localdir, "CASTool_Metadata.xlsx")
data_CASTmeta <- readxl::read_excel(fn.CASTmeta, na = "", trim_ws = TRUE)
data_CASTmeta <- data_CASTmeta %>%
  dplyr::select(Variable, eval(region)) %>%
  tidyr::pivot_wider(names_from = Variable, values_from = eval(region))

# Required user-designated options
# subregion        <- as.character(dplyr::select(data_CASTmeta, subregion))
if (region %in% state.abb) {
  regionName     <- state.name[which(state.abb == region)]
} else {
  regionName     <- region
}
removeOutliers   <- as.logical(dplyr::select(data_CASTmeta, removeOutliers))
useBC            <- as.logical(dplyr::select(data_CASTmeta, useBC))
samplim          <- as.integer(dplyr::select(data_CASTmeta, samplim))
probsHigh        <- as.numeric(dplyr::select(data_CASTmeta, probsHigh))
probsLow         <- as.numeric(dplyr::select(data_CASTmeta, probsLow))
DOlim            <- as.numeric(dplyr::select(data_CASTmeta, DOlim))
pHlimLow         <- as.numeric(dplyr::select(data_CASTmeta, pHlimLow))
pHlimHigh        <- as.numeric(dplyr::select(data_CASTmeta, pHlimHigh))
lagdays          <- as.integer(unlist(stringr::str_split(dplyr::select(data_CASTmeta, lagdays), ", ")))
biocommlist      <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, biocommlist), ", "))
siteQual2Plot    <- as.character(dplyr::select(data_CASTmeta, siteQual2Plot))
printClusterInfo <- as.logical(dplyr::select(data_CASTmeta, printClusterInfo))
# report_format <- as.character(data_CASTmeta["report_format", region])

# Specify Base Filenames # These are the files used to run the analyses
fn.targets           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.targets))
fn.Sites.Info        <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.Sites.Info))
fn.measinfo          <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.measinfo))
fn.measdata          <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.measdata))
fn.modelinfo         <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.modelinfo))
fn.modeldata         <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.modeldata))
fn.bmi.metrics       <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.metrics))
fn.bmi.qualifiers    <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.qualifiers))
fn.bmi.metrics.info  <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.metrics.info))
fn.bmi.raw           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.raw))
fn.MT.bmi            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.MT.bmi))
fn.alg.metrics       <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.metrics))
fn.alg.metrics.info  <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.metrics.info))
fn.alg.raw           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.raw))
fn.MT.alg            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.MT.alg))
fn.fish.metrics      <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.metrics))
fn.fish.metrics.info <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.metrics.info))
fn.fish.raw          <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.raw))
fn.MT.fish           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.MT.fish))
fn.bcdist            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bcdist))
fn.cluster           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.cluster))
fn.clusterinfo       <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.clusterinfo))
fn.bkgdata           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bkgdata))
fn.bkginfo           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bkginfo))

# Load GIS files
message("Loading GIS files.")
if (boo_Shiny == TRUE) {
  # 2020-09-09, use RDA saved version
  # NOT sure how to handle this
  outline  <- poly.smc.proj
  flowline <- lines.flowline.proj
} else {
  load(file.path(localdir, region, "Clusters", "Boundary"
                 , paste0(region, "_BoundaryShapefile.rda")))  # STATE.shp
  if (!is.na(state.name[match(region, state.abb)])) {
    load(file.path(localdir, region, "Clusters", "NHDPlus"     # NHD.STATE (flowlines in the state)
                   , paste0(regionName, "_NHDclusters.rda")))
  } else {
    load(file.path(localdir, region, "Clusters", "NHDPlus"
                   , paste0(region, "_NHDclusters.rda")))
  }

}## IF ~ boo_Shiny ~ END
# rm(dsn_outline, lyr_outline, dsn_flowline, lyr_flowline)

# Specify user-defined variables
# Stressors
datum <- as.character(dplyr::select(data_CASTmeta, datum))
siteColName <- as.character(dplyr::select(data_CASTmeta, siteColName))
refColName <- as.character(dplyr::select(data_CASTmeta, refColName))
outcaseColName <- as.character(dplyr::select(data_CASTmeta, outcaseColName))
outcaseLabel <- as.character(dplyr::select(data_CASTmeta, outcaseLabel))
incaseColName <- as.character(dplyr::select(data_CASTmeta, incaseColName))
incaseLabel <- as.character(dplyr::select(data_CASTmeta, incaseLabel))
# meas.stress <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, meas.stress), ", "))
# chem.stress <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, chem.stress), ", "))
# hab.stress <- as.character(dplyr::select(data_CASTmeta, hab.stress))
# mod.stress <- as.character(dplyr::select(data_CASTmeta, mod.stress))

# Bio responses
for (b in seq_along(biocommlist)) {
  bio <- tolower(biocommlist[b])
  if (bio == "bmi") {
    bmi_thresholds  <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta
                                                                          , bmi_thresholds), ", ")))
    bmi_narrative   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_narrative), ", "))
    bmi_deg_thres   <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_deg_thres)
                                                            , ", ")))
    bmi_deg_text    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_deg_text), ", "))
    bmiIndexGp      <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmiIndexGp), ", "))
    calcBMIRelAbund <- as.logical(dplyr::select(data_CASTmeta, calcBMIRelAbund))
    # bmiCounts      <- as.character(dplyr::select(data_CASTmeta, bmiCounts))
    # bmiTaxon       <- as.character(dplyr::select(data_CASTmeta, bmiTaxon))
    # bmiResp        <- as.character(dplyr::select(data_CASTmeta, bmiResp))
    # bmiRespDate    <- as.character(dplyr::select(data_CASTmeta, bmiRespDate))
    bmiModParams    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmiModParams), ", "))
  }
  if (bio == "algae") {
    alg_thresholds  <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_thresholds)
                                                           , ", ")))
    alg_narrative   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_narrative)
                                                , ", "))
    alg_deg_thres   <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_deg_thres)
                                                           , ", ")))
    alg_deg_text    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_deg_text), ", "))
    algIndexGp      <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, algIndexGp), ", "))
    calcAlgRelAbund <- as.logical(dplyr::select(data_CASTmeta, calcAlgRelAbund))
    # algCounts      <- as.character(dplyr::select(data_CASTmeta, algCounts))
    # algTaxon       <- as.character(dplyr::select(data_CASTmeta, algTaxon))
    # algResp        <- as.character(dplyr::select(data_CASTmeta, algResp))
    # algRespDate    <- as.character(dplyr::select(data_CASTmeta, algRespDate))
    algModParams    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, algModParams), ", "))
  }
  if (bio == "fish") {
    fish_thresholds  <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_thresholds)
                                                            , ", ")))
    fish_narrative   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_narrative), ", "))
    fish_deg_thres   <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_deg_thres)
                                                            , ", ")))
    fish_deg_text    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_deg_text), ", "))
    fishIndexGp      <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fishIndexGp), ", "))
    calcFishRelAbund <- as.logical(dplyr::select(data_CASTmeta, calcFishRelAbund))
    # fishCounts      <- as.character(dplyr::select(data_CASTmeta, fishCounts))
    # fishTaxon       <- as.character(dplyr::select(data_CASTmeta, fishTaxon))
    # fishResp        <- as.character(dplyr::select(data_CASTmeta, fishResp))
    # fishRespDate    <- as.character(dplyr::select(data_CASTmeta, fishRespDate))
    fishModParams    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fishModParams), ", "))
  }
}
rm(b, bio, data_CASTmeta)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What is the purpose of this? Is it ever used? NO, never
# USGS aea for SoCal is below
# socal.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#                 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# aea used for AZ is below
# az.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
#             +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# my.aea <- socal.aea
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
if (basename(fn.Sites.Info) != "NA") {
  data_Sites <- readCASToolData(fn = fn.Sites.Info
                                , NAs = c("", "na", "NA", "N/A"))
  # data_Sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t"
  #                          , stringsAsFactors = FALSE)
  # Rename or add OutcaseCol to sites file
  if (!is.na(outcaseColName)) {               # outside the case is defined
    if (outcaseColName %in% colnames(data_Sites)) {
      data_Sites <- dplyr::rename(data_Sites, OutcaseCol = all_of(outcaseColName))
    } else {
      msg <- paste0("Adding ", outcaseColName
                    , " column to site file with values equal to "
                    , outcaseLabel, ".")
      message(msg)
      data_Sites <- dplyr::mutate(data_Sites, OutcaseCol = outcaseLabel)
    }
  } else {                                 # outside the case is not defined
    msg <- paste0("Adding ", outcaseColName
                  , " column to site file with values equal to "
                  , outcaseLabel, ".")
    message(msg)
    data_Sites <- dplyr::mutate(data_Sites, OutcaseCol = outcaseLabel)
  }

  # Rename or add RefSiteFlag to sites file
  if (!is.na(refColName)) {                # reference column name is defined
    data_Sites <- dplyr::rename(data_Sites, RefSiteFlag = all_of(refColName))
  } else {
    msg <- paste("Adding ", refColName, " column to site file with values equal to 0 (FALSE)."
                 , "No sites will be depicted as reference.", sep = "\n")
    message(msg)
    data_Sites <- dplyr::mutate(data_Sites, RefSiteFlag = 0)
  }

  # Rename IncaseCol in sites file or send error message
  if (!is.na(incaseColName)) {
    data_Sites <- dplyr::rename(data_Sites, IncaseCol = all_of(incaseColName))
  } else {
    if (useBC == TRUE) {
      message("Either incaseColName must be specified or useBC must be TRUE, and required files provided")
      stop()
    }
  }

  # Rename StationID in sites file or send error message
  if (!is.na(siteColName)) {
    data_Sites <- dplyr::rename(data_Sites, StationID = all_of(siteColName))
  } else {
    message("Station identifier column name is not specified in the metadata.")
    stop()
  }

  # Create a vector of refSites
  refSites <- data_Sites$StationID[data_Sites$RefSiteFlag == 1]

} else {
  msg <- "fn.Sites.Info is NA"
  message(msg)
}
rm(fn.Sites.Info, refColName)

# Get cluster data
if (basename(fn.cluster) != "NA") {
  data_cluster <- readCASToolData(fn = fn.cluster, NAs = c("", "na", "NA", "N/A"))
} else {
  msg <- "fn.cluster is NA"
  message(msg)
}
rm(fn.cluster)

# Get cluster data metadata
if (basename(fn.clusterinfo) != "NA") {
  data_clusterInfo <- readCASToolData(fn = fn.clusterinfo, NAs = c("", "na", "NA", "N/A"))
} else {
  msg <- "fn.clusterinfo is NA"
  message(msg)
}
rm(fn.clusterinfo)

# Get background data (StreamCat)
if (basename(fn.bkgdata) != "NA") {
  data_bkgdata <- readCASToolData(fn = fn.bkgdata, NAs = c("", "na", "NA", "N/A"))
} else {
  msg <- "fn.bkgdata is NA"
  message(msg)
}
rm(fn.bkgdata)

# Get background metadata
if (basename(fn.bkginfo) != "NA") {
  data_bkginfo <- readCASToolData(fn = fn.bkginfo, NAs = c("", "na", "NA", "N/A"))
} else {
  msg <- "fn.bkginfo is NA"
  message(msg)
}
rm(fn.bkginfo)

# Get Bray-Curtis dissimilarity matrix
if (useBC == TRUE & basename(fn.bcdist) != "NA") {
  # Get BC dissimilarity distance matrix to subset cluster sites to comparators
  data_BCdist <- readCASToolData(fn = fn.bcdist, NAs = c("", "na", "NA", "N/A"))
} else if (useBC == FALSE) {
  msg <- "Use biological filter is FALSE"
  message(msg)
} else {
  msg <- "fn.bcdist is NA"
  message(msg)
}## IF ~ useBC ~ END
rm(fn.bcdist)

#~~~~~~~~~~~~~~~~~~~~~~~
# 05, Measured data and metadata ####
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
if (basename(fn.measinfo) != "NA") {
  data_chemInfo <- readCASToolData(fn = fn.measinfo, NAs = c("", "na", "NA", "N/A"))
  data_chemInfo   <- data_chemInfo %>%
    dplyr::filter(UseInStressorID == 1)
} else {
  msg <- "fn.measinfo is NA"
  message(msg)
}
rm(fn.measinfo)

## Get measured stressor values
if (basename(fn.measdata) != "NA") {
  data_chemAll <- readCASToolData(fn = fn.measdata, NAs = c("", "na", "NA", "N/A"))
  analytes     <- as.character(data_chemInfo$StdParamName)
  data_chemRaw <- data_chemAll[data_chemAll$StdParamName %in% analytes,]

  ## Average duplicate data
  data_chemRaw <- data_chemRaw %>%
    dplyr::mutate(StressSampleDate = lubridate::mdy(StressSampleDate)) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName
           , ResultValue) %>%
    dplyr::group_by(StationID, StressSampleID, StressSampleDate, StdParamName) %>%
    dplyr::summarize(MeanResultValue = mean(ResultValue), .groups = "drop_last") %>%
    dplyr::rename(ResultValue = MeanResultValue) %>%
    dplyr::filter(!is.na(ResultValue))
  data_chemRaw <- unique(data_chemRaw) # should be unique, long-form sample/analyte

  ## Get measured parameter names and separately, algal parameter names
  measParams <- as.vector(unique(data_chemRaw$StdParamName))
  algParams  <- as.vector(unique(data_chemRaw$StdParamName[grepl("^AFDM|^Chlor_a|^Pheophytin"
                                                                 ,data_chemRaw$StdParamName)]))
  measStressData <- TRUE

} else {
  msg <- "fn.measdata is NA"
  message(msg)
  data_chemRaw <- NULL
  measStressData <- FALSE
}
rm(fn.measdata, analytes)

#~~~~~~~~~~~~~~~~~~~~~~~
# 06, Modeled data and metadata ####
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
if (basename(fn.modelinfo) != "NA") {
  data_modelInfo <- readCASToolData(fn = fn.modelinfo, NAs = c("", "na", "NA", "N/A"))
  # data_modelInfo   <- read.delim(fn.modelinfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  data_modelInfo   <- data_modelInfo %>%
    dplyr::mutate(Analyte = StdParamName) %>%
    dplyr::filter(UseInStressorID == 1)
} else {
  msg <- "fn.modelinfo is NA"
  message(msg)
}
rm(fn.modelinfo)

# Get modeled stressor data
if (basename(fn.modeldata) != "NA") {
  data_modelAll <- readCASToolData(fn = fn.modeldata, NAs = c("", "na", "NA", "N/A"))
  useParams     <- as.character(data_modelInfo$StdParamName)
  data_modelRaw <- data_modelAll[data_modelAll$StdParamName %in% useParams, ]

  ## Obtain SampleYear -- but SampDate is all NA, so this is meaningless
  data_modelRaw <- data_modelRaw %>%
    dplyr::mutate(SampYear = NA, SampleDate = NA) %>%
    dplyr::select(StationID, ChemSampleID, SampDate, StdParamName
                  , ResultValue, SampleDate)

  modelStressData <- TRUE

} else {
  msg <- "fn.modeldata is NA"
  message(msg)
  data_modelRaw <- NULL
  modelStressData <- FALSE
}
rm(fn.modeldata)

#~~~~~~~~~~~~~~~~~~~~~~~
# 07, Combine stressor data ####
# Progress, 07
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Combine stressor data and metadata"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# Combine metadata for all stressors into one datafile
if (exists("data_chemInfo") & exists("data_modelInfo")) {
  chemMetaNames  <- colnames(data_chemInfo)
  modelMetaNames <- colnames(data_modelInfo)
  extraNames     <- chemMetaNames[!(chemMetaNames %in% modelMetaNames)]
  for (e in 1:length(extraNames)) {
    newCol <- extraNames[e]
    data_modelInfo[[newCol]] <- NA
  }
  data_modelInfo  <- data_modelInfo[, chemMetaNames]
  data_stressInfo <- rbind(data_chemInfo, data_modelInfo)
  rm(data_chemInfo, data_modelInfo)
  rm(chemMetaNames, modelMetaNames, extraNames, newCol, e)
} else if (exists("data_chemInfo")) {
  data_stressInfo <- data_chemInfo
  rm(data_chemInfo)
} else if (exists("data_modelInfo")) {
  data_stressInfo <- data_modelInfo
  rm(data_modelInfo)
} else {
  msg <- "Neither measured nor modeled metadata are available"
  message(msg)
}

# Modify stressor info metadata; id direction of stress; id SSTVs
# Select only necessary columns -- ARL 2023-05-25
data_stressInfo <- dplyr::distinct(data_stressInfo, StdParamName, GroupNum
                                   , GroupName, LogTransf, SSD, SSTV, SensMin
                                   , SensMax, TolMin, TolMax, UseInStressorID
                                   , DirIncStress, SSTVname, Label)

# Combine measured and modeled parameters with inverse scoring
col_StressInvScore <- as.vector(data_stressInfo$StdParamName[data_stressInfo$DirIncStress == "Dec"])
SSTVparms <- unique(data_stressInfo$StdParamName[data_stressInfo$SSTV == 1])

# Combine raw data for all stressors into one datafile
if (exists("data_chemRaw") & exists("data_modelRaw")) {
  data_Stress <- rbind(data_chemRaw, data_modelRaw)
} else if (exists("data_chemRaw")) {
  data_Stress <- data_chemRaw
} else if (exists("data_modelRaw")) {
  data_Stress <- data_modelRaw
} else {
  msg <- "Neither measured nor modeled metadata are available"
  message(msg)
}

## getOutliers returns a dataframe with ChemSampleID, StdParamName, ResultValue,
## IQRmethod, SDmethod, Outlier
## Nonsensical values are flagged as possible data entry errors
data_StressOutliers <- getOutliers(df_data = data_Stress
                                   , df_meta = data_stressInfo
                                   , dir_plots = file.path(dir_results, "Histograms"))
## Merge outlier flags with raw data by sample ID (should be all.y not all.x) -- CHECK!
data_Stress <- merge(data_Stress, data_StressOutliers
                      , by.x = c("StressSampleID", "StdParamName", "ResultValue")
                      , by.y = c("StressSampleID", "StdParamName", "ResultValue")
                      , all.x = TRUE)
data_Stress <- data_Stress %>%
  dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName, LogTransf
                , ResultValue, TransfResult, IQRmethod, SDmethod, Outlier) %>%
  dplyr::mutate(Outlier = ifelse(is.na(Outlier), "Possible data entry error"
                                 , Outlier))
# Clean up
rm(data_chemAll, data_StressOutliers)

# Remove measured outliers here!
if (removeOutliers) {
  data_stressoutliers <- data_Stress %>%
    dplyr::filter(!(Outlier %in% c("Good", "NE")))
  data_Stress <- data_Stress %>%
    dplyr::filter(Outlier %in% c("Good", "NE"))
} else {
  # don't do anything differently
}

# Bio responses
boo.bmi <- FALSE
boo.alg <- FALSE
boo.fish <- FALSE
list.bioParamsDEL <- list() # initialize an empty list
data_respTrim <- data.frame()

for (b in seq_along(biocommlist)) {
  bio <- tolower(biocommlist[b])
  #~~~~~~~~~~~~~~~~~~~~~~~
  # 08-10, Bio response data ####
  # Progress, 08-10
  if (boo_Shiny == TRUE) {
    prog_det <- paste0("Data, ", bio, ", Response data")
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END

  if ((bio == "bmi") & !boo.bmi) {
    # Read bmi data files
    message("Reading BMI data files")
    boo.bmi <- TRUE

    # Get raw BMI data
    if (basename(fn.bmi.raw) != "NA") {
      data_BMIcounts <- readCASToolData(fn = fn.bmi.raw
                                        , NAs = c("", "na", "NA", "N/A"))

      data_BMISampTotAbund <- data_BMIcounts %>%
        dplyr::mutate(RespSampleDate = lubridate::mdy(RespSampleDate)) %>%
        dplyr::group_by(RespSampleID, RespSampleDate) %>%
        dplyr::summarize(SampleTotAbund = sum(NumIndividuals, na.rm = TRUE)
                         , .groups = "drop_last")
      data_BMIcounts <- merge(data_BMIcounts, data_BMISampTotAbund
                              , by = c("RespSampleID", "RespSampleDate")
                              , all.x = TRUE)

      if (calcBMIRelAbund == TRUE) {     # Only write this column if needed
        data_BMIcounts <- data_BMIcounts %>%
          dplyr::mutate(RelAbund = round(NumIndividuals / SampleTotAbund, 5))
      }

    } else {
      msg <- "fn.bmi.raw is NA"
      message(msg)
    }

    # Get csci core data -- this should be a file that contains response sample
    # qualifiers, but neither OR nor WA have anything similar
    if (basename(fn.bmi.qualifiers) != "NA") {
      data_cscicore <- readCASToolData(fn = fn.bmi.qualifiers
                                       , NAs = c("", "na", "NA", "N/A"))
      data_cscicore <- data_cscicore[, c("stationid", "samplemonth", "sampleday"
                                         , "sampleyear", "collectionmethodcode"
                                         , "fieldreplicate", "count"
                                         , "pcnt_ambiguous_individuals")]
      data_cscicore <- data_cscicore %>%
        dplyr::mutate(date_text = paste(samplemonth, sampleday, sampleyear, sep = "/")
                      , RespSampleID = paste(stationid, date_text, collectionmethodcode
                                          , fieldreplicate, sep = "_")
                      , RespSampleDate = lubridate::mdy(date_text)) %>%
        dplyr::rename(StationID = stationid, PctAmbigInd = pcnt_ambiguous_individuals
                      , NumIndividuals = count) %>%
        dplyr::select(StationID, RespSampleID, RespSampleDate, NumIndividuals
                      , PctAmbigInd)
      data_cscicore <- unique(data_cscicore)
    } else {
      msg <- "fn.bmi.qualifiers is NA"
      message(msg)
    }

    # Get BMI master taxa data
    if (!is.na(basename(fn.MT.bmi))) {
      data_BMIMasterTaxa <- readCASToolData(fn = fn.MT.bmi
                                            , NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.MT.bmi is NA"
      message(msg)
    }

    # Get BMI metric data
    if (basename(fn.bmi.metrics) != "NA") {
      data_bmiMetrics <- readCASToolData(fn = fn.bmi.metrics
                                         , NAs = c("", "na", "NA", "N/A"))

      data_bmiMetrics <- data_bmiMetrics %>%
        dplyr::select_if(not_all_na) %>%
        dplyr::mutate(RespSampleDate = lubridate::mdy(RespSampleDate))

      data_bmiMetrics <- unique(data_bmiMetrics)

      data_bmiMetrics <- merge(data_bmiMetrics, data_BMISampTotAbund
                               , by = c("RespSampleID", "RespSampleDate")
                               , all.x = TRUE)
      data_BMITrim <- data_bmiMetrics %>%
        dplyr::select(StationID , RespSampleID, RespSampleDate) %>%
        dplyr::mutate(biocomm = "BMISampleID")
      data_respTrim <- rbind(data_respTrim, unique(data_BMITrim))
      rm(data_BMITrim)

      if (exists("data_cscicore")) {
        data_bmiMetrics <- merge(data_bmiMetrics, data_cscicore
                                 , by = c("StationID", "RespSampleID"
                                          , "RespSampleDate", "NumIndividuals")
                                 , all.x = TRUE)
        data_bmiMetrics <- data_bmiMetrics %>%
          dplyr::mutate(BMISampFlag = case_when(count < 250 & PctAmbigInd > 50 ~
                                                  "Insufficient individuals and large percent ambiguity"
                                                , count < 250 ~ "Insufficient individuals"
                                                , PctAmbigInd > 50 ~ "Large percent ambiguity"
                                                , TRUE ~ NA)) %>%
          dplyr::select(!count)
        rm(data_cscicore)
      } else {
        data_bmiMetrics <- data_bmiMetrics %>%
          dplyr::mutate(PctAmbigInd = NA, BMISampFlag = NA)
      }
      rm(data_BMISampTotAbund)
    } else {
      msg <- "fn.bmi.metrics is NA"
      message(msg)
    }

    # Get BMI metric info and add Quality
    if (basename(fn.bmi.metrics.info) != "NA") {
      data_bmiMetricsInfo <- readCASToolData(fn = fn.bmi.metrics.info
                                             , NAs = c("", "na", "NA", "N/A"))
      bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
      bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN == "Yes"])
      data_bmiMetrics$Quality <- cut(data_bmiMetrics[, bmiIndex]
                                     , breaks = bmi_deg_thres
                                     , labels = bmi_deg_text)
    } else {
      msg <- "fn.bmi.metrics.info is NA"
      message(msg)
    }

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    # SMC version writes a co-occur data file to dataDir (Data directory) -- 20230711 Removed dataDir ARL
    data_bmiCoOccur <- getCoOccurDataset(df_sites = data_Sites
                                         , df_stress = data_Stress
                                         , biocomm = "BMI"
                                         , df_resp = data_bmiMetrics
                                         , index = bmiIndex
                                         , lagdays = lagdays)
    # returns df_coOccur as data_bmiCoOccur

    if (!is.na(bmiModParams)) {
      # Identify modeled parameters to keep or delete (per client)
      bmiModelParamsDEL  <- setdiff(modelParams, bmiModParams)
      # modelParams: all modeled Params;
      # bmiModParams: input data from client re which modeled parameters to use when evaluating bmi responses
      data_bmiCoOccur <- data_bmiCoOccur %>%
        dplyr::select(!all_of(bmiModelParamsDEL)) %>%
        dplyr::select_if(not_all_na)
      list.bioParamsDEL <- append(list.bioParamsDEL, list(BMI = bmiModelParamsDEL))

      rm(bmiModParams)
    } else {
      message("No modeled data, if any, should be excluded from BMI evaluations.")
    } ## END delete ModParams not useful for bmi eval

  } else { # NO BMI data
    message("No BMI data available")
  }

  if ((bio == "algae") & !boo.alg) {
    # Read alg data files
    message("Reading Algae data files")
    boo.alg <- TRUE

    # Get raw algal data
    if (basename(fn.alg.raw) != "NA") {
      data_algCounts <- readCASToolData(fn = fn.alg.raw
                                        , NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.alg.raw is NA"
      message(msg)
    }

    # Get algal master taxa data
    if (basename(fn.MT.alg) != "NA") {
      data_algMasterTaxa <- readCASToolData(fn = fn.MT.alg
                                            , NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.MT.alg is NA"
      message(msg)
    }

    # Get algal metrics data
    if (basename(fn.alg.metrics) != "NA") {
      data_algMetrics <- readCASToolData(fn = fn.alg.metrics
                                         , NAs = c("", "na", "NA", "N/A"))
      data_algMetrics <- data_algMetrics %>%
        dplyr::mutate(AlgSampDate = lubridate::mdy(AlgSampDate)) %>%
        dplyr::mutate(AlgSampFlag = NA)

      data_algTrim <- data_algMetrics %>%
        dplyr::select(StationID , RespSampleID, RespSampleDate) %>%
        dplyr::mutate(biocomm = "AlgSampleID")
      data_respTrim <- rbind(data_respTrim, unique(data_algTrim))
      rm(data_algTrim)

    } else {
      msg <- "fn.alg.metrics is NA"
      message(msg)
    }

    # Get algal metrics metadata
    if (basename(fn.alg.metrics.info) != "NA") {
      data_algMetricsInfo <- readCASToolData(fn = fn.alg.metrics.info
                                             , NAs = c("", "na", "NA", "N/A"))
      algMetrics <- as.vector(data_algMetricsInfo$MetricName[data_algMetricsInfo$UseYN == 1])
      algIndex <- as.character(data_algMetricsInfo$MetricName[data_algMetricsInfo$IndexYN == "Yes"])
      data_algMetrics$Quality <- cut(data_algMetrics[, algIndex]
                                     , breaks = alg_deg_thres
                                     , labels = alg_deg_text)

    } else {
      msg <- "fn.alg.metrics.info is NA"
      message(msg)
    }

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    # SMC version writes a co-occur data file to dataDir (Data directory) -- 20230711 Removed dataDir ARL
    data_algCoOccur <- getCoOccurDataset(df_sites = data_Sites
                                         , df_stress = data_Stress
                                         , biocomm = "Alg"
                                         , df_resp = data_algMetrics
                                         , index = algIndex
                                         , lagdays = lagdays)
    # returns df_coOccur as data_algCoOccur

    if (!is.na(algModParams)) {
      # Identify modeled parameters to keep or delete (per client)
      algModelParamsDEL  <- setdiff(modelParams, algModParams)
      # modelParams: all modeled Params;
      # bmiModParams: input data from client re which modeled parameters to use when evaluating bmi responses
      data_algCoOccur <- data_algCoOccur %>%
        dplyr::select(!all_of(algModelParamsDEL)) %>%
        dplyr::select_if(not_all_na)
      list.bioParamsDEL <- append(list.bioParamsDEL, list(ALG = algModelParamsDEL))

      rm(algModParams)
    }  else {
      message("No modeled data, if any, should be excluded from algal evaluations.")
    } ## END delete ModParams not useful for algal eval

  } else { # NO algae data
    message("No algae data available")
  }

  # Read fish data
  if ((bio == "fish") & !boo.fish) {
    # Read alg data files
    message("Reading Algae data files")
    boo.fish <- TRUE

    # Get raw fish data
    if (basename(fn.fish.raw) != "NA") {
      data_fishCounts <- readCASToolData(fn = fn.fish.raw
                                         , NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.fish.raw is NA"
      message(msg)
    }

    # Get fish master taxa data
    if (basename(fn.MT.fish) != "NA") {
      data_fishMasterTaxa <- readCASToolData(fn = fn.MT.fish
                                             , NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.MT.fish is NA"
      message(msg)
    }

    # Get fish metrics data
    if (basename(fn.fish.metrics) != "NA") {
      data_fishMetrics <- readCASToolData(fn = fn.fish.metrics
                                          , NAs = c("", "na", "NA", "N/A"))
      data_fishMetrics <- data_fishMetrics %>%
        dplyr::mutate(FishSampleDate = lubridate::mdy(FishSampDate)) %>%
        dplyr::mutate(FishSampFlag = NA)

      data_fishTrim <- data_fishMetrics %>%
        dplyr::select(StationID , RespSampleID, RespSampleDate) %>%
        dplyr::mutate(biocomm = "FishSampleID")
      data_respTrim <- rbind(data_respTrim, unique(data_fishMetrics))
      rm(data_fishTrim)

    } else {
      msg <- "fn.fish.metrics is NA"
      message(msg)
    }

    # Get fish metrics metadata
    if (basename(fn.fish.metrics.info) != "NA") {
      data_fishMetricsInfo <- readCASToolData(fn = fn.fish.metrics.info
                                              , NAs = c("", "na", "NA", "N/A"))
      fishMetrics <- as.vector(data_fishMetricsInfo$MetricName[data_fishMetricsInfo$UseYN == 1])
      fishIndex <- as.character(data_fishMetricsInfo$MetricName[data_fishMetricsInfo$IndexYN == "Yes"])
      data_fishMetrics$Quality <- cut(data_fishMetrics[, fishIndex]
                                     , breaks = fish_deg_thres
                                     , labels = fish_deg_text)

    } else {
      msg <- "fn.fish.metrics.info is NA"
      message(msg)
    }

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    # SMC version writes a co-occur data file to dataDir (Data directory) -- 20230711 Removed dataDir ARL
    data_fishCoOccur <- getCoOccurDataset(df_sites = data_Sites
                                          , df_stress = data_Stress
                                          , biocomm = "Fish"
                                          , df_resp = data_fishMetrics
                                          , index = fishIndex
                                          , lagdays = lagdays)
    # returns df_coOccur as data_fishCoOccur

    if (!is.na(fishModParams)) {
      # Identify modeled parameters to keep or delete (per client)
      fishModelParamsDEL  <- setdiff(modelParams, fishModParams)
      # modelParams: all modeled Params;
      # bmiModParams: input data from client re which modeled parameters to use when evaluating bmi responses
      data_fishCoOccur <- data_fishCoOccur %>%
        dplyr::select(!all_of(fishModelParamsDEL)) %>%
        dplyr::select_if(not_all_na)
      list.bioParamsDEL <- append(list.bioParamsDEL, list(FISH = fishModelParamsDEL))

      rm(algModParams)
    }  else {
      message("No modeled data, if any, should be excluded from fish evaluations.")
    } ## END delete ModParams not useful for algal eval

  } else { # NO fish data
    message("No fish data available")
  }
}

if (boo.bmi == FALSE) {
  message("No BMI data available")
  bmiResp <- NULL
  bmiIndexGp <- NULL
  data_bmiMetrics <- NULL
  data_bmiMetricsInfo <- NULL
  data_bmiCoOccur <- NULL
}
if (boo.alg == FALSE) {
  message("No algae data available")
  algResp <- NULL
  algIndexGp <- NULL
  data_algMetrics <- NULL
  data_algMetricsInfo <- NULL
  data_algCoOccur <- NULL
}
if (boo.fish == FALSE) {
  message("No fish data available")
  fishResp <- NULL
  fishIndexGp <- NULL
  data_fishMetrics <- NULL
  data_fishMetricsInfo <- NULL
  data_fishCoOccur <- NULL
}
# Clean up
rm(b, bio, boo.bmi, boo.alg, boo.fish)
rm(fn.bmi.raw, fn.bmi.metrics, fn.bmi.metrics.info, fn.MT.bmi, fn.bmi.qualifiers)
rm(fn.alg.raw, fn.alg.metrics, fn.alg.metrics.info, fn.MT.alg)
rm(fn.fish.raw, fn.fish.metrics, fn.fish.metrics.info, fn.MT.fish)

#~~~~~~~~~~~~~~~~~~~~~~~
# 11, Sample summary ####
# Progress, 11
# NOTE: This must use all of the data, including outliers
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Sample Summary"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# Prepare stressor data
# Identify field, lab, and phab stressor types
data_meas <- dplyr::filter(data_Stress, !is.na(StressSampleDate))
data_model <- dplyr::filter(data_Stress, is.na(StressSampleDate))
if (nrow(data_meas) > 0) {
  data_sampSummary <- unique(data_meas[, c("StationID", "StressSampleID"
                                              , "StdParamName", "StressSampleDate")])
  data_sampSummary <- merge(data_sampSummary, data_stressInfo, by = "StdParamName")
  data_sampSummary <- data_sampSummary %>%
    dplyr::select(StationID, StressSampleID, GroupName, StdParamName
                  , StressSampleDate, Label) %>%
    dplyr::mutate(Type = case_when(GroupName == "Habitat" ~ "HabitatSampleID"
                                   , grepl("Field-measured", Label) == TRUE ~ "FieldSampleID"
                                   , TRUE ~ "ChemistrySampleID"))
  data_sampSummary <- unique(data_sampSummary[, c("StationID", "StressSampleID"
                                                  , "StressSampleDate", "Type")])
  data_sampSummary <- data_sampSummary %>%
    tidyr::pivot_wider(id_cols = c(StationID, StressSampleDate), names_from = Type
                       , values_from = StressSampleID, values_fill = NA)
  chemsamptypes <- colnames(dplyr::select(data_sampSummary, dplyr::ends_with("SampleID")))
}

# # Identify response samples
data_respTrim <- data_respTrim %>%
  tidyr::pivot_wider(id_cols = c(StationID, RespSampleDate), names_from = biocomm
                     , values_from = RespSampleID, values_fill = NA)
data_respTrim <- unique(data_respTrim)
respsamptypes <- colnames(dplyr::select(data_respTrim, dplyr::ends_with("SampleID")))

# Combine with response data types
data_sampSummary <- merge(data_sampSummary, data_respTrim
                          , by.x = c("StationID", "StressSampleDate")
                          , by.y = c("StationID", "RespSampleDate")
                          , all = TRUE)
rm(data_respTrim)

# Add modeled data, if they exist
if (nrow(data_model) > 0) {
  data_modelTrim <- as.data.frame(data_model) %>%
    dplyr::distinct(StationID, StressSampleID) %>%
    dplyr::rename(ModeledSampleID = StressSampleID)

  data_sampSummary <- merge(data_sampSummary, data_modelTrim
                            , by.x = "StationID"
                            , by.y = "StationID"
                            , all = TRUE)
  data_sampSummary <- unique(data_sampSummary)

  data_sampSummary <- data_sampSummary %>%
    dplyr::rename(SampleDate = StressSampleDate) %>%
    dplyr::select(StationID, SampleDate, all_of(chemsamptypes), ModeledSampleID
                  , all_of(respsamptypes))
} else {
  data_sampSummary <- data_sampSummary %>%
    dplyr::rename(SampleDate = StressSampleDate) %>%
    dplyr::select(StationID, SampleDate, all_of(chemsamptypes), all_of(respsamptypes))
}

# Add COMID, IncaseCol, OutcaseCol (add labels when writing table)
if (is.na(incaseColName)) {
  data_sampSummary <- merge(data_Sites[, c("StationID", "COMID", "OutcaseCol")]
                            , data_sampSummary, by = "StationID"
                            , all = TRUE)
  data_sampSummary <- unique(data_sampSummary)
} else {
  data_sampSummary <- merge(data_Sites[, c("StationID", "COMID"
                                           , "OutcaseCol", "IncaseCol")]
                            , data_sampSummary, by = "StationID"
                            , all = TRUE)
  data_sampSummary <- unique(data_sampSummary)
}

# FOR TESTING ONLY
write.table(data_sampSummary, file.path(dir_data, "TESTSummarySiteSamples.tab")
            , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")

rm(fn.CASTmeta)
rm(chemsamptypes, respsamptypes)

#~~~~~~~~~~~~~~~~~~~~~~~
# RUN CASTool
# 12, Target site selection ####
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
df_targets <- readCASToolData(fn = fn.targets, NAs = c("", "NA"))
rm(fn.targets)

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

rm(startprep.time, endprep.time, elapsedprep.time, df_runstats)

### Evaluate each target site
## Use this for debugging
if (boo_Shiny == TRUE) {
  df_targets <- data.frame("TargetSiteID" = input$Station
                           , "Chosen by" = NA, "Comment" = NA)
  names(df_targets)[2] <- "Chosen by"
} else if (boo.debug == TRUE & debug.person == "Ann") {
  df_targets <- dplyr::filter(df_targets, TargetSiteID == "BIO06600_BURP15")
  # df_targets <- dplyr::filter(df_targets, TargetSiteID %in% c("SMC04134", "402BA0031"))
  msg <- paste0("Number of target sites = ", nrow(df_targets))
  message(msg)
}
rm(msg)

#~~~~~~~~~~~~~~~~~~~~~~~
# 13, Main Code ####
# Progress, 13
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
for (site in seq_along(df_targets)) {
  startsite.time <- Sys.time()
  TargetSiteID <- df_targets$TargetSiteID[site]
  if (boo.debug == TRUE & debug.person == "Ann") {
    if (region == "WA") {
      TargetSiteID <- "ERR06600_000451"
      TargetSiteID <- "PSS05515_007726"
      TargetSiteID <- "WAM06600_003688"
      TargetSiteID <- "WAM06600_000586"   # Temp (tests getVerifiedPredictions.R)
      TargetSiteID <- "BIO06600_BURP15"
      TargetSiteID <- "RSM06600_007971"
    } else if (region == "OR") {

    } else {

    }
  }

  if (is.na(TargetSiteID)) {
    next()
  }
  msg <- paste0("Evaluating site: ", TargetSiteID)
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
  fn.gaps <- file.path(dir_results, TargetSiteID
                       , paste0(TargetSiteID, "_datagaps.tab"))
  write.table(gaps, fn.gaps, append = FALSE, col.names = FALSE
              , row.names = FALSE, sep = "\t")

  # 14, getComparators ####
  # Progress, 14
  if (boo_Shiny == TRUE) {
    prog_det <- "getComparators"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Identify comparator sites & write # comparators to data gaps file
  # This is predicated on the fact that BC distance is calculated based on
  # expected benthic macroinvertebrate taxa. If there are ever different
  # BC matrices for different biocomms, then this must move into the biocomm
  # loop or it needs to be run more than once for each biocomm here, since
  # it's used in getSiteInfo immediately afterward.
  list.CompSites <- getComparators(TargetSiteID = TargetSiteID
                                   , df_sites = data_Sites
                                   , df_bioCoOccur = data_bmiCoOccur
                                   , bioIndex = bmiIndex
                                   , useBC = useBC
                                   , outcaseColName = outcaseColName
                                   , incaseColName = incaseColName
                                   , df_bcdist = data_BCdist
                                   , bc_cutoff = 0.05
                                   , dir_results = dir_results
                                   , dir_sub = "SiteInfo")
  # If useBC == TRUE
  # Returns: myCompSites <- list(comp.sites = comp.sites ("inside the case")
  #                              , all.sites = cluster.sites) ("outside the case")
  # If useBC == FALSE
  # Returns: myCompSites <- list(comp.sites = cluster.sites ("inside the case")
  #                              , all.sites = elig.sites or other sites specified as "outside the case")
  # (all sites in ecoregion, specified elevation, or other region = "outside the case")
  comp_sites <- list.CompSites$comp.sites # inside the case
  all_sites <- list.CompSites$all.sites   # outside the case
  outcaseID <- list.CompSites$outcaseID   # outside the case identifier (value)
  incaseID <- list.CompSites$incaseID     # inside the case identifier (value)
  rm(list.CompSites)
  msg <- "getComparators is complete."
  message(msg)

  # 15, getSiteInfo, getSiteMap, writeOutliers ####
  # Progress, 15
  if (boo_Shiny == TRUE) {
    prog_det <- "getSiteInfo, getSiteMap, writeOutliers"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  # Get site information for general use (map, sample summary, etc)

  # Create site info folder with background bar graphs, boxplots for bio indices,
  # and folder for photos
  list.SiteSummary <- getSiteInfo(TargetSiteID = TargetSiteID
                                  , data_Sites = data_Sites
                                  , data_bkgdata = NULL
                                  , data_bkginfo = NULL
                                  , data_SampSummary = data_sampSummary
                                  , data_bmiMetrics = data_bmiMetrics
                                  , bmiIndexGp = bmiIndexGp
                                  , data_algMetrics = data_algMetrics
                                  , algIndexGp = algIndexGp
                                  , data_fishMetrics = data_fishMetrics
                                  , fishIndexGp = fishIndexGp
                                  , comp_sites = comp_sites
                                  , all_sites = all_sites
                                  , outcaseLabel = outcaseLabel
                                  , incaseLabel = incaseLabel
                                  , useBC = useBC
                                  # , data_cluster = NULL
                                  # , data_mods = NULL
                                  # , data_303d = NULL
                                  , dir_photo = file.path(dir_data,"Photos")
                                  , dir_results = dir_results
                                  , dir_sub = "SiteInfo")
  # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo,
  #                                Samps = mySamps,
  #                                BMImetrics = myBMImetrics,
  #                                AlgMetrics = myAlgaeMetrics,
  #                                COMID = myCOMID,
  #                                refCOMIDs = myRefCOMIDs)
  # clustID <- list.SiteSummary$ClustID

  # Create site map
  getSiteMap(sp_outline = STATE.shp
             , sp_flowline = NHD.clust
             , region = region
             , df_sites = data_Sites
             , allSites = all_sites
             , compSites = comp_sites
             , TargetSiteID = TargetSiteID
             , useBC = useBC
             , dir_results = dir_results
             , dir_sub = "SiteInfo"
             , dir_map_rmd = dir_rmd)
  # Prints static and leaflet maps (.png and .html)

  # Prepare data sets of all stressors ever detected at the target site
  # This requires data_Stress, which either includes or excludes outliers
  # used in getStressorList and getTimeSeq
  # TODO: Pivot wider from TransfResult or ResultValue? Probably should use
  # TransfResult, so site data aren't plotting on a different scale.
  siteStressAll <- data_Stress %>%
    dplyr::select(!c(IQRmethod, SDmethod, Outlier)) %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    tidyr::pivot_wider(names_from = StdParamName
                       , values_from = ResultValue) %>%
    dplyr::select_if(not_all_na)
  siteStressAllTransf <- data_Stress %>%
    dplyr::select(!c(IQRmethod, SDmethod, Outlier)) %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::filter(!is.na(TransfResult)) %>%
    tidyr::pivot_wider(names_from = StdParamName
                       , values_from = TransfResult) %>%
    dplyr::select_if(not_all_na)
  siteDetectsAll <- as.vector(colnames(siteStressAll))
  siteDetectsAll <- siteDetectsAll[!(siteDetectsAll %in%
                                       c("StationID", "StressSampleID"
                                         , "StressSampleDate"))]

  # Write target site outliers, comparator site outliers (inside the case),
  # and all outliers (outside the case)
  writeOutliers(TargetSiteID = TargetSiteID
                , df_outliers = data_stressoutliers
                , df_stressInfo = data_stressInfo
                , siteDetects = siteDetectsAll
                , compSites = comp_sites
                , allSites = all_sites
                , dir_results = dir_results)
  # Writes outliers to data gaps file

  msg <- "getSiteInfo, getSiteMap, and writeOutliers are complete."
  message(msg)

  # 16, getClusterInfo ####
  # Progress, 16
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
  if (printClusterInfo) {
    if (is.na(subregion)) {
      #OutcaseCol = Cluster; subregion = ecoregion or other division; useBC = TRUE
      clusterID = outcaseID
      subregion <- NULL
    } else {
      #OutcaseCol = ecoregion or other division; IncaseCol = Cluster; useBC = FALSE
      clusterID = incaseID
    }
    #
    getClusterInfo(TargetSiteID = TargetSiteID
                   , siteCOMID = list.SiteSummary$COMID
                   , siteCluster = clusterID
                   , refSiteCOMIDs = list.SiteSummary$refCOMIDs
                   , subregion = subregion
                   , data_cluster = data_cluster
                   , data_clusterInfo = data_clusterInfo
                   , dir_results = dir_results
                   , dir_sub = "ClusterInfo")
    msg <- "getClusterInfo is complete."
    message(msg)
  }## IF ~ printClusterInfo ~ END
  rm(list.SiteSummary)

  # 17, getAvailableDataTypes ####
  # Progress, 17
  if (boo_Shiny == TRUE) {
    prog_det <- "getAvailableDataTypes"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Prepare flags for types of stressor and response data to use
  list.AvailData <- getAvailableDataTypes(TargetSiteID = TargetSiteID
                                          , df_SampSummary = data_sampSummary
                                          , measStressSamps = measStressData
                                          , modStressSamps = modelStressData
                                          , biocommlist = biocommlist
                                          , dir_results = dir_results)
  # Returns: myAvailData <- list(useBMI = useBMI
  #                              , useAlg = useAlg
  #                              , useFish = useFish
  #                              , noStressors = noStressors
  #                              , noResponses = noResponses)
  noStressors <- list.AvailData$noStressors
  noResponses <- list.AvailData$noResponses
  useBMI      <- list.AvailData$useBMI
  useAlg      <- list.AvailData$useAlg
  useFish     <- list.AvailData$useFish

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
  rm(list.AvailData, noStressors, noResponses)

  # 18, getStressorList ####
  # Progress, 18
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
  # regardless of match status
  list.stressors <- getStressorList(TargetSiteID = TargetSiteID
                                    , outcaseLabel = outcaseLabel   # used for subtitle
                                    , outcaseID = outcaseID         # used for title
                                    , outcaseSites = all_sites      # vector
                                    , incaseSites = comp_sites      # vector
                                    , refSites = refSites           # vector
                                    , siteChem = siteDetectsAll     # dataframe
                                    , df_Stress = data_Stress       # dataframe
                                    , chemInfo = data_stressInfo    # dataframe
                                    , samplim = samplim             # integer (#samps < which can't id)
                                    , probsHigh = probsHigh         # numeric
                                    , probsLow = probsLow           # numeric
                                    , DOlim = DOlim                 # numeric
                                    , pHlimLow = pHlimLow           # numeric
                                    , pHlimHigh = pHlimHigh         # numeric
                                    , biocommlist = biocommlist     # character
                                    , listbioParamsDEL = list.bioParamsDEL # list of vectors
                                    , dir_results = dir_results     # vector
                                    , dir_sub = "CandidateCauses")
  # Returns: myStressors <- list(stressors = stressorlist
  #                     , site.stressor.pctrank = site.pctrank
  #                     , stressors_LogTransf)
  stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
  stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
  msg <- "getStressorList is complete."
  message(msg)

  # Evaluate possible error conditions
  # If no stressors are identified, no analyses can be performed. Error msg.
  if (length(stressors) == 0) {
    msg <- paste("No candidate causes identified for", TargetSiteID)
    message(msg)

    # No identified stressors may be a data gap, but may not be, either
    gapcomment <- paste0("No potential stressors fall outside the specified "
                         , "quantile range (", probsLow, " to ", probsHigh, ").")
    gaps <- cbind.data.frame("getStressorList", "Number of stressors", 0
                             , gapcomment)
    colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
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
    write.table(df_temp, file.path(dir_results, fn_runstats)
                , append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
    next()
  } ### End no stressors statement GO TO NEXT SITE


  # FOR ~ b ~ START ####
  if (boo.debug == TRUE & debug.person == "Erik") {
    # 1 = bmi, 2 = alg, 3 = fish
    biocommlist <- "alg"
  }

  for (b in seq_along(biocommlist)) {

    NE_true <- FALSE
    numLoE = 0

    LoEs <- c("TS", "CO", "Suff", "Grad", "VP", "SSD")
    df_LoE <- as.data.frame(LoEs)
    colnames(df_LoE) <- "LoE"
    df_LoE <- df_LoE %>%
      dplyr::mutate(LoE = as.character(LoE)
                    , Completed = as.integer(0)
                    , ResultsDir = as.character(NA))

    # Define biocomm data
    bioComm <- tolower(biocommlist[b])
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
      # colBioSample <- bmiResp
      # colBioSampDate <- bmiRespDate
      if (!is.na(bmi_thresholds)) {
        BioNarBrk <- bmi_thresholds
      } else {
        BioNarBrk <- NULL
      }
      if (!is.na(bmi_narrative)) {
        BioNarLab <- bmi_narrative
      } else {
        BioNarLab <- NULL
      }
      BioDegBrk <- bmi_deg_thres
      BioDegLab <- bmi_deg_text
      if (exists("bmiModelParamsDEL")) {
        bioParmsDEL <- bmiModelParamsDEL
      } else {
        bioParmsDEL <- NULL
      }
    } else if ((bioComm == "algae") && (useAlg == TRUE)) {
      data_bioCoOccur <- data_algCoOccur
      bioIndex <- algIndex
      bioIndexGp <- algIndexGp
      bioMetricNames <- algMetrics
      bioMetricData <- data_algMetrics
      bioMetricInfo <- data_algMetricsInfo
      bioTaxaData <- data_algCounts
      bioMasterTaxa <- data_algMasterTaxa
      colBio <- algIndex
      # colBioSample <- algResp
      # colBioSampDate <- algRespDate
      BioNarBrk <- alg_thresholds
      BioNarLab <- alg_narrative
      BioDegBrk <- alg_deg_thres
      BioDegLab <- alg_deg_text
      if (exists("algModelParamsDEL")) {
        bioParmsDEL <- algModelParamsDEL
      } else {
        bioParmsDEL <- NULL
      }
    } else if ((bioComm == "fish") && (useFish == TRUE)) {
      data_bioCoOccur <- data_fishCoOccur
      bioIndex <- fishIndex
      bioIndexGp <- fishIndexGp
      bioMetricNames <- fishMetrics
      bioMetricData <- data_fishMetrics
      bioMetricInfo <- data_fishMetricsInfo
      bioTaxaData <- data_fishCounts
      bioMasterTaxa <- data_fishMasterTaxa
      colBio <- fishIndex
      # colBioSample <- fishResp
      # colBioSampDate <- fishRespDate
      BioNarBrk <- fish_thresholds
      BioNarLab <- fish_narrative
      BioDegBrk <- fish_deg_thres
      BioDegLab <- fish_deg_text
      if (exists("fishModelParamsDEL")) {
        bioParmsDEL <- fishModelParamsDEL
      } else {
        bioParmsDEL <- NULL
      }
    } else {
      msg <- paste0(bioComm, " is not a valid biological community.")
      message(msg)
      next()
    }

    # If no paired stressor-response samples for target site, no eval possible
    # First 10 colnames of data_bioCoOccur are:
    # "StationID", "IncaseCol", "OutcaseCol", "StressSampleDate", "RespSampleDate",
    # "StressSampleID", "BioComm", "RespSampleID", bioIndex, "Quality"
    # , "RespSampFlag"    # THIS COLUMN IS NOT INCLUDED
    # Remaining columns are stressors/responses
    if (!(TargetSiteID %in% data_bioCoOccur$StationID)) { # Not in data_bioCoOccur
      noStressors = TRUE
    } else {
      dfTarget <- dplyr::filter(data_bioCoOccur, StationID == TargetSiteID)
      if (all(is.na(dfTarget[, stressors]))) { # In data_bioCoOccur but all values NA
        # if (all(is.na(dfTarget[, 11:ncol(dfTarget)]))) { # In data_bioCoOccur but all values NA
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
      gapcomment <- paste0("No stressor samples are available for ", TargetSiteID
                           , " within ", lagdays[1], " days before, and ", lagdays[2]
                           , " after the ", biocomm, " sample(s) was(were) obtained.")
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
      write.table(df_temp, file.path(dir_results, fn_runstats)
                  , append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

      rm(dfTarget)
      next()
    } ### End no stressors statement

    # 19, getQualSites ####
    # Progress, 19
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
    # Identify "quality" samples using different definitions
    # These are identified only from paired stressor-response samples
    # Data gaps statements from getSiteInfo aren't necessarily paired

    # IMPORTANT ----
    # The only required part of this is to identify "better than" minimum quality
    # target sample as measured by the index values; Quality is added to the response
    # metric file during the data prep process
    # dfQuality includes the following columns, which are used by the function getWoE:
    # [1] "StationID"      "IncaseCol"      "OutcaseCol"     "StressSampleID"
    # [5] "RespSampleID"   "BIBI100"        "BioDeg"         "BioNarrative"
    # [9] "InsideCaseYN"   "OutsideCaseYN"  "BetterThan"
    # The rows in this dataframe are all sites both inside the case and all sites
    # outside the case.
    list.BioQualSites <- getQualSites(TargetSiteID = TargetSiteID
                                      , df_sites = data_Sites
                                      , biocomm = bioComm
                                      , df_qual = data_bioCoOccur
                                      , colBio = colBio
                                      , compSites = comp_sites # inside the case
                                      , allSites = all_sites # outside the case
                                      , useBC = useBC
                                      , outcaseColName = "OutcaseCol"
                                      , outcaseID = outcaseID
                                      , dir_results = dir_results
                                      , dir_sub = "SiteInfo")
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

    allQual2PlotSamps <- switch(tolower(siteQual2Plot)
                                   , "reference" = list.BioQualSites$allRefBioStressSamps
                                   , "not degraded" = list.BioQualSites$allGoodBioStressSamps
                                   , "better than" = list.BioQualSites$allBTBioStressSamps)
    msg <- paste0("getQualSites is complete for ", bioComm, ".")
    message(msg)

    # Prepare data set of all response index values ever determined for the
    # target site for use in getTimeSeq
    siteRespAll <- bioMetricData %>%
      dplyr::filter(StationID == TargetSiteID) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, all_of(bioIndex))

    # 20, getDataSets ####
    # Progress, 20
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
    # The difference between this and getCoOccurDataset is that this function
    # subsets that dataset to just the matched data that are required:
    # stressors detected and responses measured at the target site and related
    # lists from inside the case and outside the case sites
    listPairedStressResp <- getDataSets(TargetSiteID = TargetSiteID
                                        , compSites = comp_sites
                                        , allSites = all_sites
                                        , df_coOccur = data_bioCoOccur
                                        , siteStressors = stressors
                                        , bioParmsDEL = bioParmsDEL
                                        , df_biometrics = bioMetricData
                                        , df_stressinfo = data_stressInfo)
    # Returns: mySubsets <- list(siteStressInfo = df_stressinfo
    #                            , allBioStress = allBioStressData
    #                            , compBioStress = compBioStressData
    #                            , siteBioStress = siteBioStressData
    #                            , allBioResp = allBioRespData
    #                            , compBioResp = compBioRespData
    #                            , siteBioResp = siteBioRespData)
    msg <- "Stressor and response data prepared, for all possible stressors."
    message(msg)

    # 22, getTimeSeq ####
    # Progress, 22
    if (boo_Shiny == TRUE) {
      prog_det <- "getTimeSeq"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Create time sequence graphics for ONLY target site
    # Uses all site stressor and response data, but not necessarily paired
    getTimeSeq(TargetSiteID
               , biocomm = bioComm
               , BioResp = bioMetricNames
               , df_stress = siteStressAll
               , df_resp = siteRespAll
               , stressors = stressors
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

    # rm(siteStressAll, siteRespAll)

    # 23, getCoOccur ####
    # Progress, 23
    if (boo_Shiny == TRUE) {
      prog_det <- "getCoOccurr"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get Co-occurrence from comparator sites with better than biology
    if (TargetSiteID %in% unique(data_bioCoOccur$StationID)) {
      msg <- "Starting Co-occurrence"
      message(msg)
      getCoOccur(TargetSiteID = TargetSiteID
                 , df_data = data_bioCoOccur[data_bioCoOccur$StationID %in% comp_sites, ]
                 , incaseLabel = incaseLabel
                 , colBio = bioIndex
                 , useBetter = FALSE
                 , colStressors = stressors
                 , df_stressinfo = data_stressInfo
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

    # 24, getSufficiency ####
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
    # Get stressors sufficient to cause biological impairment using all comparator samples
    if (TargetSiteID %in% unique(data_bioCoOccur$StationID)) {
      msg <- "Starting Sufficiency"
      message(msg)
      getSufficiency(TargetSiteID = TargetSiteID
                     , df_data = data_bioCoOccur
                     , compSites = comp_sites
                     , stressors = stressors
                     , df_stressinfo = data_stressInfo
                     , biocomm = bioComm
                     , colBio = bioIndex
                     , dir_plots = dir_results
                     , dir_sub = "Sufficiency"
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
    msg <- paste0("getSufficiency for ", bioComm, " is complete.")
    message(msg)

    dirSRlog <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                       , "Sufficiency")
    if (dir.exists(dirSRlog) == TRUE) {
      if ((length(list.files(dirSRlog)) > 0) == TRUE) {
        numLoE = numLoE + 1
        df_LoE$Completed[df_LoE$LoE == "SRLog"] <- 1
        df_LoE$ResultsDir[df_LoE$LoE == "SRLog"] <- dirSRlog
      }
    }

    # Refine all.b.str, cl.b.str, and site.b.str for just identified stressors
    core.cols <- c("StationID", "StressSampleDate", "RespSampleDate"
                   , "StressSampleID", "RespSampleID")

    all.b.str <- listPairedStressResp$allBioStress %>%
      dplyr::select(all_of(core.cols), all_of(stressors)) %>%
      dplyr::select(StressSampleID, RespSampleID, StationID, all_of(stressors))
    cl.b.str <- listPairedStressResp$compBioStress %>%
      dplyr::select(all_of(core.cols), all_of(stressors)) %>%
      dplyr::select(StressSampleID, RespSampleID, StationID, all_of(stressors))
    site.b.str <- listPairedStressResp$siteBioStress %>%
      dplyr::select(all_of(core.cols), all_of(stressors)) %>%
      dplyr::select(StressSampleID, RespSampleID, StationID, all_of(stressors))

    all.b.rsp <- listPairedStressResp$allBioResp %>%
      dplyr::select(RespSampleID, StressSampleID, StationID, RespSampleDate
             , Quality, all_of(bioMetricNames))
    cl.b.rsp <- listPairedStressResp$compBioResp %>%
      dplyr::select(RespSampleID, StressSampleID, StationID, RespSampleDate
             , Quality, all_of(bioMetricNames))
    site.b.rsp <- listPairedStressResp$siteBioResp %>%
      dplyr::select(RespSampleID, StressSampleID, StationID, RespSampleDate
             , Quality, all_of(bioMetricNames))

    siteStressInfo <- listPairedStressResp$siteStressInfo

    list_MatchBioData <- list("all.b.str"    = all.b.str
                              , "cl.b.str"   = cl.b.str
                              , "site.b.str" = site.b.str
                              , "all.b.rsp"  = all.b.rsp
                              , "cl.b.rsp"   = cl.b.rsp
                              , "site.b.rsp" = site.b.rsp)

    rm(listPairedStressResp, all.b.str, cl.b.str, site.b.str, all.b.rsp
       , cl.b.rsp, site.b.rsp)

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
    # Get Stressor Responses inside (comparators) and outside (all) the case
    getBioStressorResponses(TargetSiteID = TargetSiteID
                            , stressors = stressors
                            , df_stressinfo = siteStressInfo
                            , BioResp = bioMetricNames
                            , df_respinfo = bioMetricInfo
                            , list.MatchBioData = list_MatchBioData
                            , qual2plotSamps = allQual2PlotSamps
                            , siteQual2Plot = siteQual2Plot
                            , biocomm = bioComm
                            , p.val_cutoff = 0.05
                            , r2_cutoff = 0.1
                            , dir_plots = dir_results
                            , dir_sub = "StressorResponse"
                            , boo_plot = boo_plot_user)
    msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
    message(msg)

    dirSRLin <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                       , "StressorResponse")
    if (dir.exists(dirSRLin) == TRUE) {
      if (length(list.files(dirSRLin)) > 0) {
        numLoE = numLoE + 1
        df_LoE$Completed[df_LoE$LoE == "SRLin"] <- 1
        df_LoE$ResultsDir[df_LoE$LoE == "SRLin"] <- dirSRLin
      } else {
        numLoE = numLoE + 1
        df_LoE$Completed[df_LoE$LoE == "SRLin"] <- 0
        df_LoE$ResultsDir[df_LoE$LoE == "SRLin"] <- NA
        unlink(dirSRLin, recursive = TRUE)
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
    # Get Stressor-specific regressions using comparator sites
    if (any(SSTVparms %in% stressors)) {
      getVerifiedPredictions(TargetSiteID = TargetSiteID
                             , stressors = stressors
                             , df_stressinfo = siteStressInfo
                             , SSTVanalytes = as.character(SSTVparms)
                             , list.MatchBioData = list_MatchBioData
                             , biocomm = bioComm
                             # , colBioSample = colBioSample
                             , df_BioTaxaRelAbund = bioTaxaData
                             , df_MasterTaxa = bioMasterTaxa
                             , colBio = bioIndex
                             , BioIndex_Nar = "Quality"
                             , BioIndex_Nar_Deg = "Degraded"
                             , dir_plots = dir_results
                             , dir_sub = "VerifiedPredictions"
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

    getWoE(TargetSiteID = TargetSiteID
           , outcaseLabel = outcaseLabel
           , biocomm = bioComm
           , index = bioIndex
           , BioResp = bioMetricNames
           , dfQual = list.BioQualSites$dfQuality
           , dfStr = list_MatchBioData$site.b.str
           , dfRank = list.stressors$site.stressor.pctrank
           , dfStressInfo = siteStressInfo
           , df_coOccur = data_bioCoOccur
           , dfLoE = df_LoE
           , dir_results = dir_results
           , dir_WoE = "WoE")
    msg <- paste0("getWoE for ", bioComm, " is complete.")
    message(msg)

    # Write run-time stats to file
    endsite.time <- Sys.time()
    elapsedsite.time <- endsite.time - startsite.time

    df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                   , "Biocomm" = bioComm
                                   , "NumStressors" = length(stressors)
                                   , "NumLoE" = numLoE
                                   , "ElapsedTime" = elapsedsite.time))

    write.table(df_temp, file.path(dir_results, fn_runstats)
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
  #           , useAlg = useAlg
  #           , useBC = useBC
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
  #           , dir_rmd = dir_rmd
  #           , siteQual2Plot = siteQual2Plot)

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
                   , fishIndex = NULL
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
