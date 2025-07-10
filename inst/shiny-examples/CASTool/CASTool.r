# Copyright 2025 TetraTech. All rights reserved.
# Use, copying, modification, or distribution of this file or any of its contents
# is expressly prohibited without prior written permission of TetraTech.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.4.3
#
# CASTfxn
# Erik.Leppo@tetratech.com, 20180710
# Ann.RoseberryLincoln@tetratech.com, 20230605
#
# library(devtools)
# install_github("leppott/CASTfxn")
# requires packages: dplyr, ggplot2, lubridate, nhdplusTools, purrr, readxl, sf,
#                    shiny, stringr, tibble, tidyr, tmap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Shiny code for use in Shiny App
# 2020-10-30, Erik
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#rm(list=ls())

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, Start ####
# external/CASTool.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

boo_Shiny <- FALSE
# library(tidyverse) #LCN added
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
  gitpath <- file.path(".", "external", "R")  # used in getReport
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
  # library(readxl)
  # library(dplyr)
  # library(tidyr)
  # library(stringr)
  #
  if (boo.debug == TRUE & debug.person == "Ann") {
    region <- "WA" # Must match column header in file "CASTool_Metadata.xlsx"

    #LCN file paths
    # wd <- dirname(dirname(getwd()))
    # gitpath <- file.path(wd, "CASTfxn_6.3.6.4" , "CASTfxn", "R")
    # dir_rmd <- file.path(wd, "CASTfxn_6.3.6.4",  "CASTfxn", "inst", "rmd")
    # localdir <- file.path(wd, "CASTfxn_6.3.6.4", "CASTool_Data")

    wd <- "C:/Users/ann.lincoln/Documents" # ARL 2025-01-13
    gitpath <- file.path(wd, "GitHub", "CASTfxn", "R") # ARL 2023-05-22
    dir_rmd <- file.path(wd, "GitHub", "CASTfxn", "inst", "rmd") # ARL 2023-05-22

    localdir <- file.path(wd, "CASTool_DATA")
    dir_data <- file.path(localdir, "UploadedData_Test")
    dir_results <- file.path(localdir, region, "Results")
    boo_plot_user <- TRUE

    # define pipe
    `%>%` <- dplyr::`%>%`

    ## source functions ----
    ## All data
    source(file.path(gitpath, "readCASToolData.R"))
    source(file.path(gitpath, "getOutliers.R"))
    source(file.path(gitpath, "getCoOccurDataset.R"))
    source(file.path(gitpath, "getAllSamplesTable.R"))
    ## Target site & inside/outside case
    source(file.path(gitpath, "getComparators.R"))
    source(file.path(gitpath, "getSiteInfo.R"))
    source(file.path(gitpath, "getWSStressorFigs.R"))
    source(file.path(gitpath, "getSiteMap.R"))
    source(file.path(gitpath, "writeOutliers.R"))
    # source(file.path(gitpath, "getClusterInfo.R")) # no longer used
    source(file.path(gitpath, "getAvailableDataTypes.R"))
    # source(file.path(gitpath, "getStressorList.R")) # no longer used
    source(file.path(gitpath, "getQualSites.R"))
    # source(file.path(gitpath, "getDataSets.R")) # no longer used
    ### Evaluate lines of evidence
    source(file.path(gitpath, "getCoOccur.R"))
    source(file.path(gitpath, "getTimeSeq.R"))
    source(file.path(gitpath, "getSufficiency.R"))
    source(file.path(gitpath, "getBioStressorResponses.R"))
    source(file.path(gitpath, "getVerifiedPredictions.R"))
    source(file.path(gitpath, "getVPSSI.R"))
    ### Summarize findings
    source(file.path(gitpath, "getWoE.R"))
    source(file.path(gitpath, "getReport.R"))
    ## Summarize findings for all test sites
    # source(file.path(gitpath, "getSummaryAllSites.R"))
    #}
  } else if (boo.debug == TRUE & debug.person == "Erik") {
    library(CASTfxn)
    # gitpath <- file.path(system.file(package = "CASTfxn"), "R")
    dir_rmd <- file.path(system.file(package = "CASTfxn"), "inst", "rmd")
    wd <- "C://Users//Erik.Leppo//OneDrive - Tetra Tech, Inc//MyDocs_OneDrive//GitHub//CASTfxn//inst//shiny-examples//CAST_SMC"
    dir_data <- file.path(wd, "Data")
    dir_results <- file.path(wd, "Results")
    printClusterInfo <- FALSE # Deprecated
    site <- "SMC04134"
    TargetSiteID <- site
    b <- 1
  } else if (boo.debug == TRUE & debug.person == "Laura") {
    # This should be an error condition, because Ann & Erik are only people
    #LCN file paths
    region <- "WA_LCN"
    wd <- dirname(dirname(getwd()))
    gitpath <- file.path(wd, "CASTfxn_6.3.6.4" , "CASTfxn", "R")
    dir_rmd <- file.path(wd, "CASTfxn_6.3.6.4",  "CASTfxn", "inst", "rmd")
    localdir <- file.path(wd, "CASTfxn_6.3.6.4", "CASTool_Data")
    dir_data <- file.path(localdir, region, "Data")
    dir_results <- file.path(localdir, region, "Results")

    source(file.path(gitpath, "readCASToolData.R"))
    source(file.path(gitpath, "getOutliers.R"))
    source(file.path(gitpath, "getCoOccurDataset.R"))
    source(file.path(gitpath, "getAllSamplesTable.R"))
    ## Target site & inside/outside case
    source(file.path(gitpath, "getComparators.R"))
    source(file.path(gitpath, "getSiteInfo.R"))
    source(file.path(gitpath, "getSiteMap.R"))
    source(file.path(gitpath, "writeOutliers.R"))
    # source(file.path(gitpath, "getClusterInfo.R")) # no longer used
    source(file.path(gitpath, "getAvailableDataTypes.R"))
    # source(file.path(gitpath, "getStressorList.R")) # no longer used
    source(file.path(gitpath, "getQualSites.R"))
    # source(file.path(gitpath, "getDataSets.R")) # no longer used
    ### Evaluate lines of evidence
    source(file.path(gitpath, "getCoOccur.R"))
    source(file.path(gitpath, "getTimeSeq.R"))
    source(file.path(gitpath, "getSufficiency.R"))
    source(file.path(gitpath, "getBioStressorResponses.R"))
    source(file.path(gitpath, "getVerifiedPredictions.R"))
    source(file.path(gitpath, "getVPSSI.R"))
    ### Summarize findings
    source(file.path(gitpath, "getWoE.R"))
    source(file.path(gitpath, "getReport.R"))
    ## Summarize findings for all test sites
    source(file.path(gitpath, "getSummaryAllSites.R"))
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

## Color assignments ####
# Based on ito_seven from ggpubfigs
data_plotvars <- data.frame("Type" = c("target", "insideND", "insideD", "outsideND", "outsideD"),
                            "Fill" = c("#CC79A7", "#56B4E9", "#E69F00", "#0072B2", "#D55E00"),
                            "Shape" = c(24, 21, 25, 21, 25),
                            "Size" = c(1.75, 0.8, 1, 0.8, 1),
                            "Alpha" = c(1, 0.5, 0.7, 0.5, 0.7))
refOutline_col <- "#26F7FD" # LCN changed from "#009E73"
# Note: this change may not be colorblind-friendly
# check site map using https://www.color-blindness.com/coblis-color-blindness-simulator/

## Other plot variables ####
plot_dpi <- 600
plot_H <- 6
plot_W <- 8
plot_units <- "in"

#~~~~~~~~~~~~~~~~~~~~~~~
# 03, Select region variables ####
# Progress, 03
# region <- "WA" # options: SMC, AZ, WA, OR

# Read CASTool_Metadata.xlsx
fn.CASTmeta   <- file.path(localdir, "CASTool_Metadata.xlsx")
data_CASTmeta <- readxl::read_excel(fn.CASTmeta, na = "", trim_ws = TRUE)
data_CASTmeta <- data_CASTmeta %>%
  dplyr::select(Variable, all_of(region)) %>%
  tidyr::pivot_wider(names_from = Variable, values_from = all_of(region))

fn.SC.WSvars  <- file.path(localdir, "SelectedStreamCatStressors.csv")
fn.outline <- file.path(localdir, "NHDPlus", "gadm41_USA_shp", "gadm41_USA_1.shp")

# Required user-designated options
# LCN this code only handles if region is a state name or abbreviation
if (region %in% state.abb) {
  regionName        <- state.name[which(state.abb == region)]
} else if (region %in% state.name) {
  regionName        <- region
  region            <- state.abb[which(state.name == regionName)]
} else if (region == "WA_LCN") {
  regionName <- "WA"
} else {
  # region is not a standard, accepted region (e.g., SMC)
  # this will affect watershed-scale stressors and maps
  if (region == "SMC") {
    outline  <- poly.smc.proj
    flowline <- lines.flowline.proj
  }
}
removeOutliers      <- as.logical(dplyr::select(data_CASTmeta, removeOutliers))
useBC               <- as.logical(dplyr::select(data_CASTmeta, useBC))
bc_cutoff           <- as.numeric(dplyr::select(data_CASTmeta, bc_cutoff))
samplim             <- as.integer(dplyr::select(data_CASTmeta, samplim))
DOlim               <- as.numeric(dplyr::select(data_CASTmeta, DOlim))
pHlimLow            <- as.numeric(dplyr::select(data_CASTmeta, pHlimLow))
pHlimHigh           <- as.numeric(dplyr::select(data_CASTmeta, pHlimHigh))
lagdays             <- as.integer(unlist(stringr::str_split(dplyr::select(data_CASTmeta,
                                                                          lagdays), ", ")))
biocommlist         <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, biocommlist), ", "))
useAllCompReaches   <- as.logical(dplyr::select(data_CASTmeta, useAllCompReaches))
# report_format <- as.character(data_CASTmeta["report_format", region])

# Specify Base Filenames # These are the files used to run the analyses
fn.targets           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.targets))
fn.Sites.Info        <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.Sites.Info))
fn.meas.info         <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.meas.info))
fn.meas.data         <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.meas.data))
fn.model.info        <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.model.info))
fn.model.data        <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.model.data))
fn.bmi.metrics       <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.metrics))
fn.bmi.qualifiers    <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.qualifiers))
fn.bmi.metrics.info  <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.metrics.info))
fn.bmi.raw           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.raw))
fn.bmi.MT            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.MT))
fn.alg.metrics       <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.metrics))
fn.alg.metrics.info  <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.metrics.info))
fn.alg.raw           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.raw))
fn.alg.MT            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.MT))
fn.fish.metrics      <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.metrics))
fn.fish.metrics.info <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.metrics.info))
fn.fish.raw          <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.raw))
fn.fish.MT           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.MT))
fn.bcdist            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bcdist))
fn.cluster           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.cluster))
fn.WSstressor.Data   <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.WSstressor.Data))
fn.WSstressor.Info   <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.WSstressor.Info))

# Specify user-defined variables
# Stressors
datum          <- as.character(dplyr::select(data_CASTmeta, datum))
outcaseColName <- as.character(dplyr::select(data_CASTmeta, outcaseColName))
outcaseLabel   <- as.character(dplyr::select(data_CASTmeta, outcaseLabel))
incaseColName  <- as.character(dplyr::select(data_CASTmeta, incaseColName))
incaseLabel    <- as.character(dplyr::select(data_CASTmeta, incaseLabel))

# Bio responses
for (b in seq_along(biocommlist)) {
  bio <- tolower(biocommlist[b])
  if (bio == "bmi") {
    bmi_deg_thres   <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_deg_thres),
                                                            ", ")))
    bmi_deg_text    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_deg_text), ", "))
    bmiIndexGp      <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmiIndexGp), ", "))
    bmiModParams    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmiModParams), ", "))
    bmiSuffInds     <- as.numeric(dplyr::select(data_CASTmeta, bmiSuffInds))
    bmiPctAmbInds   <- as.numeric(dplyr::select(data_CASTmeta, bmiPctAmbInds))
    # calcBMIRelAbund <- as.logical(dplyr::select(data_CASTmeta, calcBMIRelAbund))
  }
  if (bio == "algae") {
    alg_deg_thres   <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_deg_thres),
                                                            ", ")))
    alg_deg_text    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_deg_text), ", "))
    algIndexGp      <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, algIndexGp), ", "))
    # calcAlgRelAbund <- as.logical(dplyr::select(data_CASTmeta, calcAlgRelAbund))
    algModParams    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, algModParams), ", "))
  }
  if (bio == "fish") {
    fish_deg_thres   <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_deg_thres),
                                                             ", ")))
    fish_deg_text    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_deg_text), ", "))
    fishIndexGp      <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fishIndexGp), ", "))
    # calcFishRelAbund <- as.logical(dplyr::select(data_CASTmeta, calcFishRelAbund))
    fishModParams    <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fishModParams), ", "))
  }
}
rm(b, bio, data_CASTmeta)

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

## Get GIS files ####
message("Loading GIS files.")
if (boo_Shiny == TRUE) {
  # 2020-09-09, use RDA saved version
  # NOT sure how to handle this # ARL 2025-04-13
  outline  <- poly.smc.proj
  flowline <- lines.flowline.proj
} else {
  # fn.outline moved to the top with other hard-coded file locations
  STATE.shp <- sf::read_sf(fn.outline) %>%
    dplyr::filter(NAME_1 == regionName) %>%
    sf::st_transform(crs = 5070) %>%
    sf::st_buffer(300)
  NHD.STATE <- nhdplusTools::get_nhdplus(AOI = STATE.shp) %>%
    dplyr::filter(ftype %in% c("StreamRiver", "ArtificialPath", "Connector",
                               "CanalDitch", "Drainageway")) %>%
    dplyr::select(comid, geometry) %>%
    dplyr::rename(COMID = comid)
}## IF ~ boo_Shiny ~ END
rm(fn.outline)

## Get cluster data ####
if (basename(fn.cluster) != "NA") {
  data_cluster <- readCASToolData(fn = fn.cluster, NAs = c("", "na", "NA", "N/A"))
  ## Merge clusterID into spatial reach file for map imaging
  NHD.STATE <- dplyr::left_join(NHD.STATE, data_cluster, by = "COMID")
  ## Remove reaches without clusterIDs
  NHD.STATE <- NHD.STATE[!is.na(NHD.STATE$ClusterID), ]
  ## Select only required columns
  NHD.STATE <- dplyr::select(NHD.STATE, COMID, ClusterID, geometry)
} else {
  msg <- "fn.cluster is NA"
  message(msg)
}
# rm(fn.cluster) fn.cluster is used in the final report

## Get site location ####
if (basename(fn.Sites.Info) != "NA") {
  data_Sites <- readCASToolData(fn = fn.Sites.Info, NAs = c("", "na", "NA", "N/A"))
  if ("ClusterID" %in% colnames(data_Sites)) {
    data_Sites <- dplyr::select(!ClusterID)
  }
  data_Sites <- merge(data_Sites, data_cluster, by = "COMID", all.x = TRUE)

  # Rename or add OutcaseCol to sites file
  if (!is.na(outcaseColName)) {             # outside the case is defined
    if (outcaseColName %in% colnames(data_Sites)) {
      data_Sites <- dplyr::rename(data_Sites, OutcaseCol = all_of(outcaseColName))
    } else {
      msg <- paste0("Replacing ", outcaseColName, " with OutcaseCol with values ",
                    "equal to ", outcaseLabel, ".")
      message(msg)
      data_Sites <- dplyr::mutate(data_Sites, OutcaseCol = outcaseLabel)
    }
  } else {                                 # outside the case is not defined
    msg <- paste0("Adding 'OutcaseCol' column to site file with values equal to ",
                  outcaseLabel, ".")
    message(msg)
    data_Sites <- dplyr::mutate(data_Sites, OutcaseCol = outcaseLabel)
    outcaseColName <- "OutcaseCol"
  }

  # Rename IncaseCol in sites file or send error message
  if (!is.na(incaseColName)) {
    data_Sites <- dplyr::rename(data_Sites, IncaseCol = all_of(incaseColName))
  } else {
    if (useBC == TRUE) {
      msg <- paste0("Either incaseColName must be specified or useBC must be TRUE, ",
                    "and required files provided")
      message(msg)
      stop()
    }
  }

  # Create a vector of refSites
  refSites <- data_Sites$StationID[data_Sites$RefSiteFlag == 1]

} else {
  msg <- "fn.Sites.Info is NA"
  message(msg)
}
rm(fn.Sites.Info)

## Get StreamCat data & metadata ----
if (exists("fn.WSstressor.Data")) {
  data_stressorWS <- readCASToolData(fn = fn.WSstressor.Data,
                                     NAs = c("", "na", "NA", "N/A"))
} else {
  msg <- "fn.WSstressor.Data is NA"
  message(msg)
}
rm(fn.WSstressor.Data)

if (exists("fn.WSstressor.Info")) {
  data_stressorinfoWS <- readCASToolData(fn = fn.WSstressor.Info,
                                         NAs = c("", "na", "NA", "N/A"))
} else {
  msg <- "fn.WSstressor.Info is NA"
  message(msg)
}
rm(fn.WSstressor.Info)

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
if (basename(fn.meas.info) != "NA") {
  data_chemInfo <- readCASToolData(fn = fn.meas.info,
                                   NAs = c("", "na", "NA", "N/A"))
  data_chemInfo   <- data_chemInfo %>%
    dplyr::filter(UseInStressorID == 1)
} else {
  msg <- "fn.meas.info is NA"
  message(msg)
}
rm(fn.meas.info)

## Get measured stressor values
if (basename(fn.meas.data) != "NA") {
  data_chemAll <- readCASToolData(fn = fn.meas.data,
                                  NAs = c("", "na", "NA", "N/A"))

  ## Average duplicate data
  data_chemAll <- data_chemAll %>%
    dplyr::mutate(StressSampleDate = lubridate::parse_date_time(StressSampleDate,
                                                                orders = c("ymd", "mdy", "dmy")) %>%
                    lubridate::date()) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName,
                  ResultValue) %>%
    dplyr::group_by(StationID, StressSampleID, StressSampleDate, StdParamName) %>%
    dplyr::summarize(MeanResultValue = mean(ResultValue), .groups = "drop_last") %>%
    dplyr::rename(ResultValue = MeanResultValue) %>%
    dplyr::filter(!is.na(ResultValue))
  data_chemAll <- unique(data_chemAll) # should be unique, long-form sample/analyte

  # Duplicate all pH values, one to use for alkaline environments (higher is better)
  # and one to use for acidic environments (lower is better)
  data_pHAcid <- data_chemAll %>%
    dplyr::filter(StdParamName == "pH") %>%
    dplyr::mutate(StdParamName = "pH_acidicEnv")
  data_chemAll <- data_chemAll %>%
    dplyr::mutate(StdParamName = ifelse(StdParamName == "pH",
                                        "pH_alkEnv",
                                        StdParamName))
  data_chemRaw <- rbind(data_chemAll, data_pHAcid)
  rm(data_pHAcid)

  analytes     <- as.character(data_chemInfo$StdParamName)
  data_chemRaw <- data_chemRaw[data_chemRaw$StdParamName %in% analytes, ]

  ## Get measured parameter names and separately, algal parameter names
  measParams <- as.vector(unique(data_chemRaw$StdParamName))
  algParams  <- as.vector(unique(data_chemRaw$StdParamName[grepl("^AFDM|^Chlor_a|^Pheophytin",
                                                                 data_chemRaw$StdParamName)]))
  measStressData <- TRUE

} else {
  msg <- "fn.meas.data is NA"
  message(msg)
  data_chemRaw <- NULL
  measStressData <- FALSE
}
rm(fn.meas.data, analytes, data_chemAll)

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
if (basename(fn.model.info) != "NA") {
  data_modelInfo <- readCASToolData(fn = fn.model.info,
                                    NAs = c("", "na", "NA", "N/A"))
  data_modelInfo   <- data_modelInfo %>%
    dplyr::filter(UseInStressorID == 1)
} else {
  msg <- "fn.model.info is NA"
  message(msg)
}
rm(fn.model.info)

# Get modeled stressor data
if (basename(fn.model.data) != "NA") {
  data_modelAll <- readCASToolData(fn = fn.model.data,
                                   NAs = c("", "na", "NA", "N/A"))
  useParams     <- as.character(data_modelInfo$StdParamName)
  data_modelRaw <- data_modelAll[data_modelAll$StdParamName %in% useParams, ]

  ## Obtain SampleYear -- but SampleDate is all NA, so this is meaningless
  data_modelRaw <- data_modelRaw %>%
    dplyr::mutate(SampYear = NA, SampleDate = NA) %>%
    dplyr::select(StationID, ChemSampleID, SampDate, StdParamName,
                  ResultValue, SampleDate)

  modelStressData <- TRUE
  rm(useParams, data_modelAll)
} else {
  msg <- "fn.model.data is NA"
  message(msg)
  data_modelRaw <- NULL
  modelStressData <- FALSE
}
rm(fn.model.data)

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

# Select only necessary columns -- ARL 2023-05-25
data_stressInfo <- dplyr::distinct(data_stressInfo, StdParamName, Label,
                                   LogTransf, UseInStressorID, DirIncStress,
                                   SSTVname.bmi, SensMax.bmi, SensMin.bmi,
                                   SSTVname.alg, SensMax.alg, SensMin.alg,
                                   SSTVname.fish, SensMax.fish, SensMin.alg,
                                   SSIndex, SourceGroup)

# Combine raw data for all stressors into one datafile
if (exists("data_chemRaw") & exists("data_modelRaw")) {
  data_Stress <- rbind(data_chemRaw, data_modelRaw)
  rm(data_chemRaw, data_modelRaw)
} else if (exists("data_chemRaw")) {
  data_Stress <- data_chemRaw
  rm(data_chemRaw)
} else if (exists("data_modelRaw")) {
  data_Stress <- data_modelRaw
  rm(data_modelRaw)
} else {
  msg <- "Neither measured nor modeled metadata are available"
  message(msg)
}

## getOutliers returns a dataframe with ChemSampleID, StdParamName, ResultValue,
## IQRmethod, SDmethod, Outlier
## Nonsensical values are flagged as possible data entry errors
data_StressOutliers <- getOutliers(df_data = data_Stress,
                                   df_meta = data_stressInfo,
                                   dir_plots = file.path(dir_results, "Histograms"))
## Merge outlier flags with raw data by sample ID
data_Stress <- merge(data_Stress, data_StressOutliers,
                     by.x = c("StressSampleID", "StdParamName", "ResultValue"),
                     by.y = c("StressSampleID", "StdParamName", "ResultValue"),
                     all.x = TRUE)
data_Stress <- data_Stress %>%
  dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName, LogTransf,
                ResultValue, TransfResult, IQRmethod, SDmethod, Outlier) %>%
  dplyr::mutate(Outlier = ifelse(is.na(Outlier), "Possible data entry error",
                                 Outlier))
# Clean up
rm(data_StressOutliers)

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
      data_bmiCounts <- readCASToolData(fn = fn.bmi.raw,
                                        NAs = c("", "na", "NA", "N/A"))

      data_bmiCounts <- data_bmiCounts %>%
        dplyr::mutate(RespSampleDate = lubridate::parse_date_time(RespSampleDate,
                                                                  orders = c("ymd", "mdy", "dmy")) %>%
                        lubridate::date(),
                      RespSampleID = stringr::str_replace_all(RespSampleID, "[:punct:]", "_"),
                      StationID = stringr::str_replace_all(StationID, "[:punct:]", "_"))
      # LCN needed to add for provided dataset so RespSampIDs matched

      # TODO: comment this section out if using BioMonTools for data prep
      # Require data_bmiCounts to have both NumIndividuals, and PctIndividuals
      # as well as total sample count
      data_BMISampTotAbund <- data_bmiCounts %>%
        dplyr::group_by(RespSampleID, RespSampleDate) %>%
        dplyr::summarize(SampleTotAbund = sum(NumIndividuals, na.rm = TRUE),
                         SampleTotTaxa = dplyr::n(),
                         .groups = "drop_last")

      data_bmiCounts <- merge(data_bmiCounts, data_BMISampTotAbund,
                              by = c("RespSampleID", "RespSampleDate"),
                              all.x = TRUE)

      # if (calcBMIRelAbund == TRUE) {     # Only write this column if needed
      #   data_bmiCounts <- data_bmiCounts %>%
      #     dplyr::mutate(PctInd = round(NumIndividuals / SampleTotAbund, 5))
      # } # end section to comment out if using BioMonTools for data prep

      data_bmiCounts <- data_bmiCounts %>%
        dplyr::rename(PctInd = RelAbund) %>%
        dplyr::mutate(PctTaxa = round(1/SampleTotTaxa, 5),
                      BMISampFlag = ifelse(SampleTotAbund < bmiSuffInds,
                                           "Insufficient individuals", NA))

    } else {
      msg <- "fn.bmi.raw is NA"
      message(msg)
    }

    # Get BMI master taxa data
    if (!is.na(basename(fn.bmi.MT))) {
      data_bmiMasterTaxa <- readCASToolData(fn = fn.bmi.MT,
                                            NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.bmi.MT is NA"
      message(msg)
    }

    # Get BMI metric data
    if (basename(fn.bmi.metrics) != "NA") {
      data_bmiMetrics <- readCASToolData(fn = fn.bmi.metrics,
                                         NAs = c("", "na", "NA", "N/A"))

      data_bmiMetrics <- data_bmiMetrics %>%
        dplyr::select_if(not_all_na) %>%
        dplyr::mutate(RespSampleDate = lubridate::parse_date_time(RespSampleDate,
                                                                  orders = c("ymd", "mdy", "dmy")) %>%
                        lubridate::date())

      data_bmiMetrics <- unique(data_bmiMetrics)

      data_BMITrim <- data_bmiMetrics %>%
        dplyr::select(StationID , RespSampleID, RespSampleDate) %>%
        dplyr::mutate(biocomm = "BMISampleID")
      data_respTrim <- rbind(data_respTrim, unique(data_BMITrim))
      rm(data_BMITrim, data_BMISampTotAbund)
    } else {
      msg <- "fn.bmi.metrics is NA"
      message(msg)
    }

    # Get BMI metric info and add Quality (based on user-supplied data in CASTool_Metadata.xlsx)
    if (basename(fn.bmi.metrics.info) != "NA") {
      data_bmiMetricsInfo <- readCASToolData(fn = fn.bmi.metrics.info,
                                             NAs = c("", "na", "NA", "N/A"))
      data_bmiMetricsInfo <- data_bmiMetricsInfo %>%
        dplyr::filter(UseYN == "Y") %>%
        dplyr::select(MetricName, MetricLabel, IndexYN, TrendWIncStress,
                      CutoffValue, InclusiveIndicator)
      bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
      bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN == "Yes"])
      data_bmiMetrics$Quality <- cut(data_bmiMetrics[, bmiIndex],
                                     breaks = bmi_deg_thres,
                                     labels = bmi_deg_text)
    } else {
      msg <- "fn.bmi.metrics.info is NA"
      message(msg)
    }

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_bmiCoOccur <- getCoOccurDataset(df_sites = data_Sites,
                                         df_stress = data_Stress,
                                         biocomm = "BMI",
                                         df_resp = data_bmiMetrics,
                                         index = bmiIndex,
                                         lagdays = lagdays)

    if (!is.na(bmiModParams)) {
      # Identify modeled parameters to keep or delete (per client)
      bmiModelParamsDEL  <- setdiff(modelParams, bmiModParams)
      # modelParams: all modeled Params;
      # bmiModParams: input data from client re which modeled parameters to use
      # when evaluating bmi responses
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
      data_algCounts <- readCASToolData(fn = fn.alg.raw,
                                        NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.alg.raw is NA"
      message(msg)
    }

    # Get algal master taxa data
    if (basename(fn.alg.MT) != "NA") {
      data_algMasterTaxa <- readCASToolData(fn = fn.alg.MT,
                                            NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.alg.MT is NA"
      message(msg)
    }

    # Get algal metrics data
    if (basename(fn.alg.metrics) != "NA") {
      data_algMetrics <- readCASToolData(fn = fn.alg.metrics,
                                         NAs = c("", "na", "NA", "N/A"))
      data_algMetrics <- data_algMetrics %>%
        dplyr::mutate(AlgSampDate = lubridate::parse_date_time(AlgSampDate,
                                                               orders = c("ymd", "mdy", "dmy")) %>%
                        lubridate::date()) %>%
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

    # Get algal metrics metadata & Quality (based on user-supplied data in CASTool_Metadata.xlsx)
    if (basename(fn.alg.metrics.info) != "NA") {
      data_algMetricsInfo <- readCASToolData(fn = fn.alg.metrics.info,
                                             NAs = c("", "na", "NA", "N/A"))
      data_algMetricsInfo <- data_algMetricsInfo %>%
        dplyr::filter(UseYN == "Y") %>%
        dplyr::select(MetricName, MetricLabel, IndexYN, TrendWIncStress,
                      CutoffValue, InclusiveIndicator)
      algMetrics <- as.vector(data_algMetricsInfo$MetricName[data_algMetricsInfo$UseYN == 1])
      algIndex <- as.character(data_algMetricsInfo$MetricName[data_algMetricsInfo$IndexYN == "Yes"])
      data_algMetrics$Quality <- cut(data_algMetrics[, algIndex],
                                     breaks = alg_deg_thres,
                                     labels = alg_deg_text)

    } else {
      msg <- "fn.alg.metrics.info is NA"
      message(msg)
    }

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_algCoOccur <- getCoOccurDataset(df_sites = data_Sites,
                                         df_stress = data_Stress,
                                         biocomm = "Alg",
                                         df_resp = data_algMetrics,
                                         index = algIndex,
                                         lagdays = lagdays)
    # returns df_coOccur as data_algCoOccur

    if (!is.na(algModParams)) {
      # Identify modeled parameters to keep or delete (per client)
      algModelParamsDEL  <- setdiff(modelParams, algModParams)
      # modelParams: all modeled Params;
      # bmiModParams: input data from client re which modeled parameters to use
      # when evaluating bmi responses
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
      data_fishCounts <- readCASToolData(fn = fn.fish.raw,
                                         NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.fish.raw is NA"
      message(msg)
    }

    # Get fish master taxa data
    if (basename(fn.fish.MT) != "NA") {
      data_fishMasterTaxa <- readCASToolData(fn = fn.fish.MT,
                                             NAs = c("", "na", "NA", "N/A"))
    } else {
      msg <- "fn.fish.MT is NA"
      message(msg)
    }

    # Get fish metrics data & Quality (based on user-supplied data in CASTool_Metadata.xlsx)
    if (basename(fn.fish.metrics) != "NA") {
      data_fishMetrics <- readCASToolData(fn = fn.fish.metrics,
                                          NAs = c("", "na", "NA", "N/A"))
      data_fishMetrics <- data_fishMetrics %>%
        dplyr::mutate(FishSampleDate = lubridate::parse_date_time(FishSampDate,
                                                                  orders = c("ymd", "mdy", "dmy")) %>%
                        lubridate::date()) %>%
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
      data_fishMetricsInfo <- readCASToolData(fn = fn.fish.metrics.info,
                                              NAs = c("", "na", "NA", "N/A"))
      data_fishMetricsInfo <- data_fishMetricsInfo %>%
        dplyr::filter(UseYN == "Y") %>%
        dplyr::select(MetricName, MetricLabel, IndexYN, TrendWIncStress,
                      CutoffValue, InclusiveIndicator)
      fishMetrics <- as.vector(data_fishMetricsInfo$MetricName[data_fishMetricsInfo$UseYN == 1])
      fishIndex <- as.character(data_fishMetricsInfo$MetricName[data_fishMetricsInfo$IndexYN == "Yes"])
      data_fishMetrics$Quality <- cut(data_fishMetrics[, fishIndex],
                                      breaks = fish_deg_thres,
                                      labels = fish_deg_text)

    } else {
      msg <- "fn.fish.metrics.info is NA"
      message(msg)
    }

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_fishCoOccur <- getCoOccurDataset(df_sites = data_Sites,
                                          df_stress = data_Stress,
                                          biocomm = "Fish",
                                          df_resp = data_fishMetrics,
                                          index = fishIndex,
                                          lagdays = lagdays)
    # returns df_coOccur as data_fishCoOccur

    if (!is.na(fishModParams)) {
      # Identify modeled parameters to keep or delete (per client)
      fishModelParamsDEL  <- setdiff(modelParams, fishModParams)
      # modelParams: all modeled Params;
      # fishModParams: input data from client re which modeled parameters to use
      # when evaluating fish responses
      data_fishCoOccur <- data_fishCoOccur %>%
        dplyr::select(!all_of(fishModelParamsDEL)) %>%
        dplyr::select_if(not_all_na)
      list.bioParamsDEL <- append(list.bioParamsDEL, list(FISH = fishModelParamsDEL))

      rm(fishModParams)
    } else {
      message("No modeled data, if any, should be excluded from fish evaluations.")
    } ## END delete ModParams not useful for algal eval
  } else { # NO fish data
    message("No fish data available")
  }
}

if (boo.bmi == FALSE) {
  message("No BMI data available")
  bmiResp <- NULL
  bmiIndex <- NULL
  bmiIndexGp <- NULL
  data_bmiMetrics <- NULL
  data_bmiMetricsInfo <- NULL
  data_bmiCoOccur <- NULL
}
if (boo.alg == FALSE) {
  message("No algae data available")
  algResp <- NULL
  algIndex <- NULL
  algIndexGp <- NULL
  data_algMetrics <- NULL
  data_algMetricsInfo <- NULL
  data_algCoOccur <- NULL
}
if (boo.fish == FALSE) {
  message("No fish data available")
  fishResp <- NULL
  fishIndex <- NULL
  fishIndexGp <- NULL
  data_fishMetrics <- NULL
  data_fishMetricsInfo <- NULL
  data_fishCoOccur <- NULL
}
# Clean up
rm(b, bio, boo.bmi, boo.alg, boo.fish)
rm(fn.bmi.raw, fn.bmi.metrics, fn.bmi.metrics.info, fn.bmi.MT, fn.bmi.qualifiers)
rm(fn.alg.raw, fn.alg.metrics, fn.alg.metrics.info, fn.alg.MT)
rm(fn.fish.raw, fn.fish.metrics, fn.fish.metrics.info, fn.fish.MT)

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

data_sampSummary <- getAllSamplesTable(df.stress = data_Stress,
                                       df.stressInfo = data_stressInfo,
                                       df.resp = data_respTrim,
                                       df.sites = data_Sites)
# Returns: data_sampSummary (df.sampSummary)
# Colnames include: StationID, COMID, OutcaseCol, IncaseCol, SampleDate,
# ChemistrySampleID, FieldSampleID, HabitatSampleID, ModeledSampleID,
# BMISampleID, AlgSampleID, FishSampleID (assuming all sample types are available)
rm(fn.CASTmeta, data_respTrim)
# Data prep completed
#~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~
# RUN CASTool ####
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

### Evaluate each target site
## Use this for debugging
if (boo_Shiny == TRUE) {
  df_targets <- data.frame("TargetSiteID" = input$Station,
                           "Chosen by" = NA, "Comment" = NA)
  names(df_targets)[2] <- "Chosen by"
} else if (boo.debug == TRUE & debug.person == "Ann") {
  # df_targets <- dplyr::filter(df_targets, TargetSiteID == "BIO06600_BURP15")
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
for (site in seq_along(1:nrow(df_targets))) {
  TargetSiteID <- df_targets$TargetSiteID[site]
  # if (boo.debug == TRUE & debug.person == "Ann") {
  # For debugging purposes only # these need to be - in LCN version of files
  # if (region == "WA") {
  # TargetSiteID <- "BIO06600_BURP15"   # No degraded samples (6 of them)
  # TargetSiteID <- "ERR06600_005995"   # Assigned new COMID; removed all detected stressors
  # TargetSiteID <- "PSS05515_007726"   # All samples degraded; low DO
  # TargetSiteID <- "RSM06600_007971"   # No degraded samples
  # TargetSiteID <- "WAM06600_000586"   # All samples degraded; Temp (tests getVerifiedPredictions.R)
  # TargetSiteID <- "WAM06600_003688"   # One of two samples degraded
  # TargetSiteID <- "WAM06600-003688"   # One of two samples degraded
  # LCN version of files uses - instead of _
  # } else if (region == "OR") {
  #
  # } else {
  # Different state here
  #   }
  # }

  if (is.na(TargetSiteID)) {
    next
  } else if (is.na(data_Sites$IncaseCol[data_Sites$StationID == TargetSiteID])) {
    msg <- "No inside-the-case identifier available"
    message(msg)
    next
  }
  msg <- paste0("Evaluating site: ", TargetSiteID)
  message(msg)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Biocomm-independent functions ####

  # Create high-level results folder structure
  dir_sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(dir_results, dir_sub2)) == TRUE,
         dir.create(file.path(dir_results, dir_sub2)), FALSE)

  # Define datagaps data frame ####
  gaps <- data.frame(fxnname = character(), condition = character(),
                     result = character(), comment = character())
  fn.gaps <- file.path(dir_results, TargetSiteID,
                       paste0(TargetSiteID, "_datagaps.tab"))
  write.table(gaps, fn.gaps, append = FALSE, col.names = TRUE,
              row.names = FALSE, sep = "\t")

  # 14, getComparators ####
  ## Progress, 14
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
  list.CompSites <- getComparators(TargetSiteID = TargetSiteID,
                                   df_sites = data_Sites,
                                   df_cluster = data_cluster,
                                   df_bioCoOccur = data_bmiCoOccur,
                                   bioIndex = bmiIndex,
                                   useBC = useBC,
                                   df_bcdist = data_BCdist,
                                   bc_cutoff = 0.05,
                                   outcaseColName = outcaseColName,
                                   outcaseLabel = outcaseLabel,
                                   incaseColName = incaseColName,
                                   incaseLabel = incaseLabel,
                                   useAllCompReaches = useAllCompReaches,
                                   dir_results = dir_results,
                                   dir_sub = "SiteInfo")
  # Returns: list.CompSites$TargetCOMID (Reach on which target site is located)
  #          list.CompSites$comp.sites (vector of unique inside-the-case sites
  #          regardless of useBC)
  #          list.CompSites$comp.reaches (vector of unique inside-the-case reaches
  #          having sites on them if useAllReaches == FALSE, else all
  #          inside-the-case reaches)
  #          list.CompSites$all.sites (vector of unique outside-the-case sites,
  #          regardless of useBC)
  #          list.CompSites$all.reaches (vector of unique outside-the-case reaches
  #          having sites on them)
  #          list.CompSites$incaseID (inside-the-case identifier, NULL if useBC
  #          is TRUE)
  #          list.CompSites$outcaseID (outside-the-case identifier)
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

  # Create site info folder with watershed-scale stressor boxplots,
  # boxplots for bio indices, and folder for photos
  getSiteInfo(TargetSiteID = TargetSiteID,
              TargetCOMID = list.CompSites$TargetCOMID,
              df_Sites = data_Sites,
              df_SampSummary = data_sampSummary,
              biocommlist = biocommlist,
              df_BMIMetrics = data_bmiMetrics,
              BMIIndexGp = bmiIndexGp,
              df_ALGMetrics = data_algMetrics,
              ALGIndexGp = algIndexGp,
              df_FishMetrics = data_fishMetrics,
              FishIndexGp = fishIndexGp,
              comp.sites = list.CompSites$comp.sites,
              all.sites = list.CompSites$all.sites,
              OutcaseLabel = outcaseLabel,
              IncaseLabel = incaseLabel,
              useBC = useBC,
              plotvars = data_plotvars,
              refSiteCol = refOutline_col,
              plotdpi = plot_dpi,
              plotH = plot_H,
              plotW = plot_W,
              plotunits = plot_units,
              dir_photo = file.path(dir_data, "Photos"),
              dir_results = dir_results,
              dir_sub = "SiteInfo",
              boo_plot = TRUE)

  getWSStressorFigs(df_WSData = data_stressorWS,
                    df_WSInfo = data_stressorinfoWS,
                    comp.reaches = list.CompSites$comp.reaches,
                    TargetCOMID = list.CompSites$TargetCOMID,
                    dir_sub = "SiteInfo",
                    df_SampSummary = data_sampSummary,
                    biocommlist = biocommlist,
                    boo_plot = TRUE)

  # Create site map
  getSiteMap(sp_outline = STATE.shp,
             sp_flowline = NHD.STATE,
             region = regionName,
             datum = datum,
             df_sites = data_Sites,
             allSites = list.CompSites$all.sites,
             compSites = list.CompSites$comp.sites,
             TargetSiteID = TargetSiteID,
             useBC = useBC,
             plotvars = data_plotvars,
             refOutline = refOutline_col,
             dir_results = dir_results,
             dir_sub = "SiteInfo",
             dir_map_rmd = dir_rmd)
  # Prints static map (.png)

  msg <- "getSiteInfo, getSiteMap, and writeOutliers are complete."
  message(msg)

  # 16, getAvailableDataTypes ####
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
  list.AvailData <- getAvailableDataTypes(TargetSiteID = TargetSiteID,
                                          df_stress = data_Stress,
                                          df_SampSummary = data_sampSummary,
                                          measStressSamps = measStressData,
                                          modStressSamps = modelStressData,
                                          biocommlist = biocommlist,
                                          dir_results = dir_results)
  # Returns: myAvailData <- list(useBMI = useBMI,
  #                              useAlg = useAlg,
  #                              useFish = useFish,
  #                              noStressors = noStressors,
  #                              noResponses = noResponses)
  noStressors    <- list.AvailData$noStressors
  noResponses    <- list.AvailData$noResponses
  useBMI         <- list.AvailData$useBMI
  useAlg         <- list.AvailData$useAlg
  useFish        <- list.AvailData$useFish
  siteDetectsAll <- list.AvailData$siteDetectsAll
  rm(list.AvailData)

  if ((noStressors == TRUE) | (noResponses == TRUE)) {

    msg <- ifelse((noStressors == TRUE) & (noResponses == TRUE),
                  paste0("No stressor or response data are available for ",
                         TargetSiteID),
                  ifelse(noStressors == TRUE,
                         paste0("No stressor data are available for ",
                                TargetSiteID),
                         paste0("No response data are available for ",
                                TargetSiteID)))
    message(msg)

    gaps <- cbind.data.frame("getAvailData", "Number detects/responses", 0, msg)
    colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                row.names = FALSE, sep = "\t")

    next

  } ### End no stressors statement GO TO NEXT SITE
  rm(noStressors, noResponses)

  # Write target site outliers, comparator site outliers (inside the case),
  # and all outliers (outside the case)
  writeOutliers(TargetSiteID = TargetSiteID,
                df_outliers = data_stressoutliers,
                df_stressInfo = data_stressInfo,
                siteDetects = siteDetectsAll,
                compSites = list.CompSites$comp.sites,
                allSites = list.CompSites$all.sites,
                dir_results = dir_results)
  # Writes outliers to data gaps file

  # FOR ~ b ~ START ####
  if (boo.debug == TRUE & debug.person == "Erik") {
    # 1 = bmi, 2 = alg, 3 = fish
    biocommlist <- "alg"
  }

  for (b in seq_along(biocommlist)) {

    # Define biocomm data
    bioComm <- tolower(biocommlist[b])
    if ((bioComm == "bmi") && (useBMI == TRUE)) {
      data_bioCoOccur <- data_bmiCoOccur
      bioIndex <- bmiIndex
      bioIndexGp <- bmiIndexGp
      bioMetricNames <- bmiMetrics
      bioMetricData <- data_bmiMetrics
      bioMetricInfo <- data_bmiMetricsInfo
      bioTaxaData <- data_bmiCounts
      bioMasterTaxa <- data_bmiMasterTaxa
      colBio <- bmiIndex
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
      next
    }

    ### Define LoE dataframe ----
    df_LoE <- data.frame(StationID = character(),
                         StressSampleID = character(),
                         StressSampleDate = as.Date(character()),
                         RespSampleID = character(),
                         RespSampleDate = as.Date(character()),
                         bioComm = character(),
                         bioIndexName = character(),
                         bioIndex = numeric(),
                         Quality = character(),
                         Stressor = character(),
                         StressorValue = numeric(),
                         LoE = character(),
                         Score = character(),
                         stringsAsFactors = FALSE)

    # Site-specific paired SR
    # If no paired stressor-response samples for target site, no eval possible
    # First 10 colnames of data_bioCoOccur are:
    # "StationID", "IncaseCol", "OutcaseCol", "StressSampleDate", "RespSampleDate",
    # "StressSampleID", "RespSampleID, "BioComm", bioIndex, "Quality"
    # Remaining columns (11:ncol) are stressors/responses
    if (!(TargetSiteID %in% data_bioCoOccur$StationID)) { # Not in data_bioCoOccur
      noPairedSamps = TRUE
    } else {
      dfTarget <- dplyr::filter(data_bioCoOccur, StationID == TargetSiteID)
      if (all(is.na(dfTarget[, siteDetectsAll]))) { # In data_bioCoOccur but all values NA
        noPairedSamps = TRUE
      } else {
        noPairedSamps = FALSE
      }
    }

    # If no paired stressors, write to data gaps file and output to runstats file
    # Proceed to next target site
    if (noPairedSamps == TRUE) {
      msg <- paste0("No paired stressor-response samples for", TargetSiteID,
                    " for the ", bioComm, " community within specified lag days.")
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      gapcomment <- paste0("No stressor samples are available for ", TargetSiteID,
                           " within ", lagdays[1], " days before, and ", lagdays[2],
                           " after the ", biocomm, " sample(s) was(were) obtained.")
      gaps <- cbind.data.frame("getCoOccurDataset",
                               paste0("Paired stressor-", bioComm, " data"),
                               0, gapcomment)

      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")

      next
    } ### End no stressors statement
    rm(dfTarget)

    ## 17, getQualSites ####
    # Progress, 18
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; getQualSites")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Run analyses
    # Identify "quality" samples using different definitions
    # These are identified only from paired stressor-response samples
    # Data gaps statements from getSiteInfo aren't necessarily paired

    # IMPORTANT
    # This step adds "RefSiteFlag", BetterThan", "IncaseYN", and "OutcaseYN" to
    # the dataframe, data_bioCoOccur, allowing subsets to be created as needed.
    df_PairedStressResp <- getQualSites(TargetSiteID = TargetSiteID,
                                        biocomm = bioComm,
                                        df_qual = data_bioCoOccur,
                                        colBio = bioIndex,
                                        refSites = refSites,
                                        compSites = list.CompSites$comp.sites, # inside the case
                                        allSites = list.CompSites$all.sites, # outside the case
                                        stressors = siteDetectsAll,
                                        dir_results = dir_results,
                                        dir_sub = "SiteInfo")
    # Returns: df_PairedStressResp, a dataframe with the following columns:
    # [1] "StationID"          "IncaseCol"          "OutcaseCol"         "StressSampleDate"
    # [5] "RespSampleDate"     "StressSampleID"     "RespSampleID"       "BioComm"
    # [9] "RefSiteFlag"        "IncaseYN"           "OutcaseYN"          "BetterThan"
    # [13] colBio              "Quality"            all_of(stressors)
    # The rows in this dataframe may be subset as desired to either inside-the-case or
    # outside-the-case with the boolean columns "IncaseYN" and "OutcaseYN"

    # Remove any stressors with fewer than samplim comparator samples
    df_PairedStressResp.stats <- df_PairedStressResp %>%
      dplyr::filter(StationID %in% list.CompSites$comp.sites) %>%
      dplyr::select(StationID, IncaseCol, OutcaseCol, StressSampleDate,
                    RespSampleDate, StressSampleID, RespSampleID, BioComm,
                    all_of(colBio), Quality, all_of(siteDetectsAll)) %>%
      tidyr::pivot_longer(cols = !(StationID:Quality), names_to = "Stressor",
                          values_to = "StressorValue", values_drop_na = TRUE) %>%
      dplyr::group_by(Stressor) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop_last")

    insuffSamples <- df_PairedStressResp.stats$Stressor[which(df_PairedStressResp.stats$n < samplim)]

    df_PairedStressResp <- df_PairedStressResp %>%
      dplyr::select(!all_of(insuffSamples))

    # Write these to data gaps file
    if (length(insuffSamples) == 0) {
      for (i in seq_along(insuffSamples)) {
        str <- insuffSamples[i]
        msg <- paste0("Insufficient numbers of samples are available for ", str, ".")
        message(msg)

        # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        gapcomment <- "This stressor will not be evaluated."
        gaps <- cbind.data.frame("samplim comparison", str, paste0("<", samplim),
                                 gapcomment)

        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
      } #End loop over stressors
    } #End if

    df_PairedSRTransf <- df_PairedStressResp %>%
      dplyr::select(StationID, IncaseCol, OutcaseCol, StressSampleDate,
                    RespSampleDate, StressSampleID, RespSampleID, BioComm,
                    RefSiteFlag, IncaseYN, OutcaseYN, BetterThan, all_of(colBio),
                    Quality)

    df_StressTrim <- data_Stress %>%
      dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName,
                    TransfResult) %>%
      dplyr::filter(StdParamName %in% siteDetectsAll) %>%
      tidyr::pivot_wider(names_from = StdParamName, values_from = "TransfResult")

    df_PairedSRTransf <- merge(df_PairedSRTransf, df_StressTrim,
                               by = c("StationID", "StressSampleID", "StressSampleDate"),
                               all.x = TRUE)


    rm(df_StressTrim)
    msg <- paste0("getQualSites is complete for ", bioComm, ".")
    message(msg)

    ## 18, getCoOccur ####
    # Progress, 21
    if (boo_Shiny == TRUE) {
      prog_det <- "getCoOccur"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get Co-occurrence from comparator samples with not degraded samples
    # Answers the question: "Are the observed stressor levels consistent
    # with impairment where and when it occurs"?
    if (TargetSiteID %in% unique(data_bioCoOccur$StationID)) {
      msg <- "Starting Co-occurrence"
      message(msg)

      list.StressorMetaData <- getCoOccur(TargetSiteID = TargetSiteID,
                                          df_data = df_PairedSRTransf,
                                          detects = siteDetectsAll,
                                          df_stressinfo = data_stressInfo,
                                          compsites = list.CompSites$comp.sites,
                                          biocomm = bioComm,
                                          colBio = bioIndex,
                                          pHlimLow = pHlimLow,
                                          pHlimHigh = pHlimHigh,
                                          DOlim = DOlim,
                                          plotvars = data_plotvars,
                                          plotdpi = plot_dpi,
                                          plotH = plot_H,
                                          plotW = plot_W,
                                          plotunits = plot_units,
                                          dir_plots = dir_results,
                                          dir_sub = "_WoE",
                                          boo_plot = boo_plot_user)
      # TODO: add labels or legend for dotted lines (pHlimLow, pHlimHigh, DOlim)
      # TODO: Capture NA scores and replace with NE -- added line 307 to getCoOccur

      df_stressorMetadata <- list.StressorMetaData$df_stressorMetadata
      notEvaluated <- c(insuffSamples, list.StressorMetaData$notEvaluated)
      df_COscores <- list.StressorMetaData$df_COscores
    }

    if (nrow(df_stressorMetadata) == 0) {
      msg <- paste0("No candidate causes to evaluate further for ",
                    TargetSiteID, " for the ", bioComm, " community.")
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      gaps <- cbind.data.frame("getCoOccur", msg, 0, gapcomment)
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")
      next
    }

    if (nrow(df_COscores) != 0) {
      df_LoE <- df_COscores
    }
    rm(df_COscores, list.StressorMetaData)

    msg <- paste0("getCoOccur for ", bioComm, " is complete.")
    message(msg)

    ## 19, getTimeSeq ####
    # Progress, 20
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
    df_TS_scores <- getTimeSeq(TargetSiteID,
                               biocomm = bioComm,
                               bioindex = bioIndexGp,
                               df_stress = data_Stress,
                               df_resp = bioMetricData[bioMetricData$StationID == TargetSiteID, ],
                               df_respinfo = bioMetricInfo,
                               df_stressinfo = df_stressorMetadata,
                               plotdpi = plot_dpi,
                               plotH = plot_H,
                               plotW = plot_W,
                               plotunits = plot_units,
                               dir_results = dir_results,
                               dir_sub = "_WoE",
                               boo_plot = boo_plot_user)
    # TODO: why are there missing values or values outside the scale range in getTimeSeq?
    # Getting many warnings (45 for site WAM06600_000586)

    if (nrow(df_TS_scores) != 0) {
      df_LoE <- rbind(df_LoE, df_TS_scores)
    }
    rm(df_TS_scores)

    msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
    message(msg)

    ## 20, getSufficiency ####
    # Progress, 24
    if (boo_Shiny == TRUE) {
      prog_det <- "getSufficiency"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get stressors sufficient to cause biological impairment using all comparator samples
    msg <- "Starting Sufficiency"
    message(msg)

    df_SuffScores <- getSufficiency(TargetSiteID = TargetSiteID,
                                    df_data = df_PairedSRTransf,
                                    compSites = list.CompSites$comp.sites,
                                    df_stressinfo = df_stressorMetadata,
                                    biocomm = bioComm,
                                    colBio = bioIndex,
                                    plotvars = data_plotvars,
                                    plotdpi = plot_dpi,
                                    plotH = plot_H,
                                    plotW = plot_W,
                                    plotunits = plot_units,
                                    dir_plots = dir_results,
                                    dir_sub = "_WoE",
                                    boo_plot = boo_plot_user)
    # TODO: add labels or legend for dotted lines (pHlimLow, pHlimHigh, DOlim)
    # TODO: Capture NA scores and replace with NE -- added line 240 & removed

    if (nrow(df_SuffScores) != 0) {
      df_LoE <- rbind(df_LoE, df_SuffScores)
    }
    rm(df_SuffScores)

    msg <- paste0("getSufficiency for ", bioComm, " is complete.")
    message(msg)

    ## 21, getBioStressorResponses ####
    # Progress, 25
    if (boo_Shiny == TRUE) {
      prog_det <- "getBioStressorResponses"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get Stressor Responses inside (comparators) and outside (all) the case
    df_gradscores <- getBioStressorResponses(TargetSiteID = TargetSiteID,
                                             df_stressinfo = df_stressorMetadata,
                                             df_respinfo = bioMetricInfo,
                                             df_respdata = bioMetricData,
                                             df_datapaired = df_PairedSRTransf,
                                             biocomm = bioComm,
                                             bioindex = bioIndex,
                                             min_cases = samplim,
                                             p.val_cutoff = 0.05,
                                             r2_cutoff = 0.25,
                                             plotvars = data_plotvars,
                                             refOutline = refOutline_col,
                                             plotdpi = plot_dpi,
                                             plotH = plot_H,
                                             plotW = plot_W,
                                             plotunits = plot_units,
                                             dir_plots = dir_results,
                                             dir_sub = "_WoE",
                                             boo_pred_warn = TRUE,
                                             boo_plot = boo_plot_user)
    # TODO: Capture NA scores and replace with NE -- fixed in multiple places, esp. lines 795 & 809

    if (nrow(df_gradscores != 0)) {
      df_LoE <- rbind(df_LoE, df_gradscores)
    }
    rm(df_gradscores)

    msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
    message(msg)

    ## 22, getVerifiedPredictions ####
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
    sstv.name <- paste0("SSTVname.", bioComm)
    stressors.sstv <- as.vector(unlist(df_stressorMetadata$Stressor[!is.na(df_stressorMetadata[[sstv.name]])]))
    stressors.ssi <- as.vector(unlist(df_stressorMetadata$Stressor[!is.na(df_stressorMetadata$SSIndex)]))

    if (length(stressors.ssi) == 0 & length(stressors.sstv) == 0) {

      msg <- "No site stressors have stressor-specific tolerance values or stressor-specific indices."
      message(msg)
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      gaps <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0, msg)
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")

    } else {

      if (length(stressors.sstv) > 0) { # one or more stressors.sstv

        df_VPscores <- getVerifiedPredictions(TargetSiteID = TargetSiteID,
                                              stressors.sstv = stressors.sstv,
                                              df_stressinfo = df_stressorMetadata,
                                              df_paired = df_PairedSRTransf,
                                              biocomm = bioComm,
                                              df_bioTaxaData = bioTaxaData,
                                              df_MasterTaxa = bioMasterTaxa,
                                              colBio = bioIndex,
                                              plotvars = data_plotvars,
                                              plotdpi = plot_dpi,
                                              plotH = plot_H,
                                              plotW = plot_W,
                                              plotunits = plot_units,
                                              dir_plots = dir_results,
                                              dir_sub = "_WoE",
                                              boo_plot = boo_plot_user)

        if (nrow(df_VPscores != 0)) {
          df_LoE <- rbind(df_LoE, df_VPscores)
        }
        rm(df_VPscores)

      } else { # no sstvs

        msg <- "No site stressors have stressor specific tolerance values"
        message(msg)

        # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        gaps <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0, msg)
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")

      }

      if (length(stressors.ssi) > 0) { # one or more stressors.ssi

        df_VPSSIscores <- getVPSSI(TargetSiteID = TargetSiteID,
                                   stressors.ssi = stressors.ssi,
                                   df_stressinfo = df_stressorMetadata,
                                   df_paired = df_PairedSRTransf,
                                   biocomm = bioComm,
                                   df_bioMetricData = bioMetricData,
                                   df_bioMetricInfo = bioMetricInfo,
                                   colBio = bioIndex,
                                   plotvars = data_plotvars,
                                   plotdpi = plot_dpi,
                                   plotH = plot_H,
                                   plotW = plot_W,
                                   plotunits = plot_units,
                                   dir_plots = dir_results,
                                   dir_sub = "_WoE",
                                   boo_plot = boo_plot_user)

        if (nrow(df_VPSSIscores != 0)) {
          df_LoE <- rbind(df_LoE, df_VPSSIscores)
        }
        rm(df_VPSSIscores)

      } else { # no ssis

        msg <- "No site stressors have stressor specific indices"
        message(msg)

        # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        gaps <- cbind.data.frame("getVPSSIscores", TargetSiteID, 0, msg)
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")

      }

    } ### End getVP evaluation

    msg <- paste0("getVerifiedPredictions for ", bioComm, " is complete.")
    message(msg)

    ## 23, getWOE ####
    # Progress, 27
    if (boo_Shiny == TRUE) { # needs updating
      prog_det <- "getWOE"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    getWoE(TargetSiteID = TargetSiteID,
           biocomm = bioComm,
           dfLoE = df_LoE,
           dfStress = df_stressorMetadata,
           dir_results = dir_results,
           dir_WoE = "_WoE")
    msg <- paste0("getWoE for ", bioComm, " is complete.")
    message(msg)

  } ### End biocomm loop
  ## FOR ~ b ~ END ####

  ## 24, getReport ####
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
    report_type <- "preliminary"
  } else {
    report_type <- "summary"
  }

  getReport(TargetSiteID,
            biocomms,
            primeIndex = bmiIndex,
            removeOutliers = removeOutliers,
            samplim,
            useBC = useBC,
            lagdays = lagdays,
            DOlim = DOlim,
            pHlimLow = pHlimLow,
            pHlimHigh = pHlimHigh,
            bmiIndex = bmiIndex,
            algIndex = algIndex,
            fishIndex = fishIndex,
            useBMI = useBMI,
            useAlg = useAlg,
            useFish = useFish,
            dir_data = dir_data,
            dir_results = dir_results,
            report_type = "full",
            report_format = "html",
            dir_rmd = dir_rmd)

  dfGaps <- read.table(file.path(dir_results, TargetSiteID,
                                 paste0(TargetSiteID,"_datagaps.tab")),
                       header = TRUE, sep = "\t")
  dfGaps <- unique(dfGaps)
  write.table(dfGaps, file.path(dir_results, TargetSiteID,
                                paste0(TargetSiteID,"_datagaps.tab")),
              append = FALSE, col.names = TRUE, row.names = FALSE,
              sep = "\t")

} ### End TargetSite loop # not used in Shiny
# FOR ~ site ~ END ####

rm(site)

# 25, getSummaryAllSites ####
# Progress, 29
if (boo_Shiny == TRUE) {
  prog_det <- "getSummaryAllSites"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# getSummaryAllSites(biocommlist = biocommlist,
#                    bmiIndex = bmiIndex,
#                    algIndex = NULL,
#                    fishIndex = NULL,
#                    dir_data = dir_data,
#                    dir_results = dir_results,
#                    dir_sub = "WoE",
#                    df_sites = NULL)

# msg <- "getSummaryAllSites is complete."
# message(msg)

# rm(list=ls())

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, END ####
# external/RPPTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
