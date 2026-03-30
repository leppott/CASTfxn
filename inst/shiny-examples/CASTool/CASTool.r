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
# 2025-09-24, Erik, start mods for updated Shiny App
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tic <- Sys.time()
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, Start ####
# external/CASTool.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Define global variables
boo_Shiny <- TRUE # Whether to run the code in Shiny mode (set to FALSE if running script outside of the app)
boo.debug <- FALSE # Whether to run the code in debug mode
dn_checked_sk <- "_CheckedInputs" # Name of checked inputs folder
boo.plot.user <- TRUE # Whether to generate line of evidence plots

if(boo_Shiny == FALSE){
  # User edits these lines
  in.dir <- "C:/Users/lnaslund/Documents/CASTool_Data/DataNoHelper/Data" # File path of data directory
  out.dir <- "C:/Users/lnaslund/Documents/CASTool_Data/DataNoHelper/Results" # File path of results directory
  region <- "DEPied" # Name of region

  # Helper packages
  pakInd <- setdiff("pak", .packages(all.available = TRUE))
  if(rlang::is_empty(pakInd)==FALSE){
    install.packages("pak")
  }

  # baseDataInd <- setdiff("CASToolBaseDataPckg", .packages(all.available = TRUE))
  # wsDataInd <- setdiff("CASToolWSStressorPckg", .packages(all.available = TRUE))
  # clustDataInd <- setdiff("CASToolClusterPckg", .packages(all.available = TRUE))
  #
  # if(rlang::is_empty(baseDataInd)==FALSE){
  #   message("Installing CASToolBaseDataPckg")
  #   pak::pak("laura-naslund/CASToolBaseDataPckg")
  # }
  #
  # if(rlang::is_empty(wsDataInd)==FALSE){
  #     message("Installing CASToolWSStressorPckg")
  #     pak::pak("laura-naslund/CASToolWSStressorPckg")
  # }
  # if(rlang::is_empty(clustDataInd)==FALSE){
  #   message("Installing CASToolClusterPckg")
  #   pak::pak("laura-naslund/CASToolClusterPckg")
  # }
  #
  # library(CASToolWSStressorPckg)
  # library(CASToolBaseDataPckg)
  # library(CASToolClusterPckg)

  helperInd <- setdiff("CASToolHelperPckg", .packages(all.available = TRUE))

  if(rlang::is_empty(helperInd)==FALSE){
    message("Installing CASToolHelperPckg")
    pak::pak("laura-naslund/CASToolHelperPckg")
  }

  library(CASToolHelperPckg)

  # Source all functions
  devtools::load_all()

  useBC <- FALSE
}

# define pipe
`%>%` <- dplyr::`%>%`
`:=` <- data.table::`:=`

## Color assignments ####
# Based on ito_seven from ggpubfigs
data_plotvars <- data.frame("Type" = c("target", "insideND", "insideD", "outsideND", "outsideD"),
                            "Fill" = c("#CC79A7", "#56B4E9", "#E69F00", "#0072B2", "#D55E00"),
                            "Shape" = c(24, 21, 25, 21, 25),
                            "Size" = c(1.75, 0.8, 1, 0.8, 1),
                            "Alpha" = c(1, 0.5, 0.7, 0.5, 0.7))
refOutline_col <- "#000000"

## Other plot variables ####
plot_dpi <- 600
plot_H <- 6
plot_W <- 8
plot_units <- "in"

# 01, Set up ####
# Progress, 02
## Shiny ----
if (boo_Shiny == TRUE) {
  ### 00, Initialize----
  # need biocomm first
  dir_rmd     <- file.path(system.file(package = "CASTfxn"), "rmd")
  wd          <- getwd()
  dir_data    <- dn_data
  dir_results <- dn_results
  data_CASTmeta_prog <- readRDS(file.path(dir_data, dn_checked_sk, "CASTmetadata.rds"))
  data_CASTmeta_prog <- data_CASTmeta_prog %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Value)
  biocommlist_prog <- data_CASTmeta_prog %>% dplyr::pull(biocommlist) %>% stringr::str_split(", |,") %>% unlist()
  n_biocomm_prog <- length(biocommlist_prog)

  #
  prog_cnt <- 0
  # Number of increments
  #prog_n <- 23 * n_biocomm_prog
  prog_n <- 16 + (7 * n_biocomm_prog)
  prog_sleep <- 0.25

  prog_det <- "Set up output structure"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
} # IF ~ boo_Shiny ~ END

#~~~~~~~~~~~~~~~~~~~~~~~
# 02, Check inputs ####
# Progress, 02

if (boo_Shiny == TRUE) {
  prog_det <- "Check input data files"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
} else {

  list.Tables <- checkInputs(dir.uploaded = in.dir,
                             dir.out = out.dir)
  TableOne    <- list.Tables$TableOne

  write.csv(TableOne, file.path(out.dir, region, "TableOne.csv"), row.names = FALSE)
  TableTwo    <- list.Tables$TableTwo
  write.csv(TableTwo, file.path(out.dir, region, "TableTwo.csv"), row.names = FALSE)

}## IF ~ boo_Shiny ~ END

#~~~~~~~~~~~~~~~~~~~~~~~
# 03, Select region variables ####
# Progress, 03
if (boo_Shiny == TRUE) {
  prog_det <- "Pull values from metadata"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

out.dir <- file.path(out.dir, region)

## Load CASTool_Metadata ####
data_CASTmeta <- readRDS(file.path(out.dir, dn_checked_sk, "CASTmetadata.rds"))
data_CASTmeta <- data_CASTmeta %>%
  tidyr::pivot_wider(names_from = Variable, values_from = Value)

## Read loaded.rds ####
data_loaded <- readRDS(file.path(out.dir, dn_checked_sk, "loaded.rds"))
loaded      <- as.character(data_loaded$Object)

### Helper import boolean
helperImport <- data_CASTmeta %>% dplyr::pull(helperImport) %>% as.logical()
helperImport <- ifelse(is.na(helperImport), FALSE, helperImport)

### WS boolean
boo.WS <- data_CASTmeta %>% dplyr::pull(exploreWSStressor) %>% as.logical()
boo.WS <- ifelse(is.na(boo.WS), FALSE, boo.WS)

## Target sample boolean
targetSampleLabels <- data_CASTmeta %>% dplyr::pull(targetSampleLabels) %>% as.logical()
targetSampleLabels <- ifelse(is.na(targetSampleLabels), FALSE, targetSampleLabels)

# Set up booleans for different data types available
boo.meas  <- FALSE
boo.model <- FALSE
# boo.WS.loaded    <- FALSE
for (l in seq_along(loaded)) {
  msg <- paste0("booleans, ", l, "/", length(loaded))
  message(msg)
  object <- loaded[l]
  if (grepl("chem", object) == TRUE)  {
    boo.meas  <- TRUE
    if (grepl("Info", object) == TRUE) {
      meta.meas <- object
    } else {
      data.meas <- object
    }
  }
  if (grepl("model", object) == TRUE) {
    boo.model <- TRUE
    if (grepl("Info", object) == TRUE) {
      meta.mod <- object
    } else {
      data.mod <- object
    }
  }
}
rm(l, object, data_loaded)

## Get variables ####
### Response data ####
biocommlist <- data_CASTmeta %>% dplyr::pull(biocommlist) %>% stringr::str_split(", |,") %>% unlist()
# Bio responses
for (b in seq_along(biocommlist)) {
  bio <- tolower(biocommlist[b])
  calcRelAbund       <- as.logical(dplyr::select(data_CASTmeta, calcRelAbund))
  if (bio == "bmi") {
    bmiIndexGp       <- data_CASTmeta %>% dplyr::pull(bmiIndexGp) %>% stringr::str_split(", |,") %>% unlist()
  }
  if (bio == "alg") {
    algIndexGp       <- data_CASTmeta %>% dplyr::pull(algIndexGp) %>% stringr::str_split(", |,") %>% unlist()
  }
  if (bio == "fish") {
    fishIndexGp       <- data_CASTmeta %>% dplyr::pull(fishIndexGp) %>% stringr::str_split(", |,") %>% unlist()
  }
}## FOR ~ b

### Stressor data ####
removeOutliers  <- as.logical(dplyr::select(data_CASTmeta, removeOutliers))
samplim         <- as.integer(dplyr::select(data_CASTmeta, samplim))
r2_cutoff       <- as.numeric(dplyr::select(data_CASTmeta, r2_cutoff))
p.val_cutoff    <- as.numeric(dplyr::select(data_CASTmeta, p.val_cutoff))

if (boo.meas) {
  DOlim         <- as.numeric(dplyr::select(data_CASTmeta, DOlim))
  pHlimLow      <- as.numeric(dplyr::select(data_CASTmeta, pHlimLow))
  pHlimHigh     <- as.numeric(dplyr::select(data_CASTmeta, pHlimHigh))
  # 20251216, QC
  # remove white space since free text so can split on just ","
  # replace select with pull
  lagdays       <- as.integer(
    unlist(
      stringr::str_split(
        gsub(" ",
             "",
             dplyr::pull(data_CASTmeta, lagdays)
             ),
        ",")
      )
  )
}## IF ~ boo.meas

useAllCompReaches  <- as.logical(dplyr::select(data_CASTmeta, useAllCompReaches))

### Site variables ####
if (boo_Shiny) {
  # should be single entries so should be ok
  datum          <- as.character(dplyr::pull(data_CASTmeta, datum))
  outcaseColName <- as.character(dplyr::pull(data_CASTmeta, outcaseColName))
  outcaseLabel   <- as.character(dplyr::pull(data_CASTmeta, outcaseLabel))
  incaseColName  <- as.character(dplyr::pull(data_CASTmeta, incaseColName))
  incaseLabel    <- as.character(dplyr::pull(data_CASTmeta, incaseLabel))
} else {
  datum          <- as.character(dplyr::select(data_CASTmeta, datum))
  outcaseColName <- as.character(dplyr::select(data_CASTmeta, outcaseColName))
  outcaseLabel   <- as.character(dplyr::select(data_CASTmeta, outcaseLabel))
  incaseColName  <- as.character(dplyr::select(data_CASTmeta, incaseColName))
  incaseLabel    <- as.character(dplyr::select(data_CASTmeta, incaseLabel))
}## IF ~ boo_Shiny

rm(b, bio)

#~~~~~~~~~~~~~~~~~~~~~~~
# 04, Site data files ####
# Progress, 04
if (boo_Shiny == TRUE) {
  prog_det <- "Load site data"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

list.SiteData <- prepSiteData(out.dir = file.path(out.dir, dn_checked_sk),
                              outcaseLabel = outcaseLabel,
                              incaseColName = incaseColName,
                              useBC = useBC,
                              outcaseColName = outcaseColName)

data_Sites    <- list.SiteData$site
data_cluster  <- list.SiteData$cluster
refSites      <- list.SiteData$refSites
rm(list.SiteData)

#~~~~~~~~~~~~~~~~~~~~~~~
# 05, Measured data and metadata ####
# Progress, 05
if (boo_Shiny == TRUE) {
  prog_det <- "Load measured stressor data"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

if (boo.meas) {
  list.measStress   <- prepMeasStressorData(in.dir = file.path(out.dir, dn_checked_sk),
                                            out.dir = out.dir,
                                            fn.data = paste0(data.meas, ".rds"),
                                            fn.meta = paste0(meta.meas, ".rds"),
                                            removeOutliers = removeOutliers,
                                            sub.dir = "_Histograms")
  data_chemInfo     <- list.measStress$data_chemInfo
  data_chemRaw      <- list.measStress$data_chemRaw
  data_measoutliers <- list.measStress$data_measoutliers

  rm(list.measStress)
}

#~~~~~~~~~~~~~~~~~~~~~~~
# 06, Modeled data and metadata ####
# Progress, 06
if (boo_Shiny == TRUE) {
  prog_det <- "Load modeled stressor data"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

if (boo.model) {
  list.modStress     <- prepModStressorData(in.dir = file.path(out.dir, dn_checked_sk),
                                            out.dir = out.dir,
                                            fn.data = paste0(data.meas, ".rds"),
                                            fn.meta = paste0(meta.meas, ".rds"),
                                            removeOutliers = removeOutliers,
                                            sub.dir = "_Histograms")
  data_modelInfo     <- list.modStress$data_modelInfo
  data_modelRaw      <- list.modStress$data_modelAll %>%
    dplyr::mutate(StressSampleDate = NA) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, TransfValue)
  data_modeloutliers <- list.modStress$data_modeloutliers
  rm(list.modStress)
}

#~~~~~~~~~~~~~~~~~~~~~~~
# 07, Combine stressor data ####
# Progress, 07
if (boo_Shiny == TRUE) {
  prog_det <- "Combine stressor data and metadata"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
# remove outliers
# Combine metadata for all stressors into one datafile
if (boo.meas && boo.model) {
  # Combine metadata
  chemMetaNames   <- colnames(data_chemInfo)
  modelMetaNames  <- colnames(data_modelInfo)
  extraNames      <- chemMetaNames[!(chemMetaNames %in% modelMetaNames)]
  for (e in seq_len(length(extraNames))) {
    newCol        <- extraNames[e]
    data_modelInfo[[newCol]] <- NA
  }
  data_modelInfo  <- data_modelInfo[, chemMetaNames]
  data_stressInfo <- rbind(data_chemInfo, data_modelInfo)
  rm(data_chemInfo, data_modelInfo)
  rm(chemMetaNames, modelMetaNames, extraNames, newCol, e)

  data_stressInfo <- dplyr::distinct(data_stressInfo,
                                     StdParamName,
                                     Label,
                                     LogTransf,
                                     UseInStressorID,
                                     DirIncStress,
                                     SSTVname.bmi,
                                     SensMax.bmi,
                                     SensMin.bmi,
                                     SSTVname.alg,
                                     SensMax.alg,
                                     SensMin.alg,
                                     SSTVname.fish,
                                     SensMax.fish,
                                     SensMin.fish,
                                     SSIndex,
                                     SourceGroup)
  # Combine data
  data_Stress     <- rbind(data_chemRaw, data_modelRaw)
  rm(data_chemRaw, data_modelRaw)
  # Combine outliers
  if (removeOutliers) {
    data_stressoutliers <- rbind(data_measoutliers, data_modeloutliers)
    rm(data_measoutliers, data_modeloutliers)
  }
} else if (boo.meas) {
  data_stressInfo <- data_chemInfo
  data_Stress     <- data_chemRaw
  rm(data_chemInfo, data_chemRaw)
  if (!is.na(removeOutliers) & removeOutliers) {
    data_stressoutliers <- data_measoutliers
    rm(data_measoutliers)
  } else {
    data_stressoutliers <- NULL
  }## IF ~ removeOutliers
} else {
  data_stressInfo <- data_modelInfo
  data_Stress     <- data_modelRaw
  rm(data_modelInfo, data_modelRaw)
  if (!is.na(removeOutliers) & removeOutliers) {
    data_stressoutliers <- data_modeloutliers
    rm(data_modeloutliers)
  } else {
    data_stressoutliers <- NULL
  }## IF ~ removeOutliers
}

# If using, get WS stressor data
if (boo.WS == TRUE & helperImport == FALSE) {
  data_stressorWS     <- readRDS(file.path(out.dir, dn_checked_sk, "data_stressorWS.rds"))
  data_stressorinfoWS <- readRDS(file.path(out.dir, dn_checked_sk,
                                           "data_stressorinfoWS.rds"))

  if(exists("data_stressorWS") & exists("data_stressorinfoWS")){
    message("Watershed stressor data provided by user loaded.")
  } else{
    message("Watershed stressor data provided by user not found.")
  }


} else if(boo.WS == TRUE & helperImport == TRUE){

  # ## check that region available
  # utils::data("available_regions", package = "CASToolBaseDataPckg")
  # regionAvailable <- region %in% available_regions$Region

  conusStates <- setdiff(state.name, c("Alaska", "Hawaii"))

  regionAvailable <- region %in% conusStates

  if(regionAvailable == TRUE){
    message("Downloading watershed stressor data from helper package.")

    # data_stressorWS <- retrieve_stressor_data(region)
    # data_stressorinfoWS <- retrieve_stressor_info(region)

    data_stressorWS <- CASToolHelperPckg::getWSStressorData(region)
    data_stressorinfoWS <- CASToolHelperPckg::getWSStressorInfo()

    if("comid" %in% names(data_stressorWS)){
      data_stressorWS <- data_stressorWS %>% dplyr::rename("COMID" = "comid")
    }

  } else{
    message("Watershed stressor data not available for the specified region. ")
  }
}

# Bio responses
boo.bmi <- FALSE
boo.alg <- FALSE
boo.fish <- FALSE
data_respTrim <- data.frame()

responsesOutput <- data.frame(MetricName = character(),
                              MetricLabel = character(),
                              BioComm = character(),
                              IndexYN = character())

for (b in seq_along(biocommlist)) {
  bio <- tolower(biocommlist[b])
  #~~~~~~~~~~~~~~~~~~~~~~~
  # 08, Bio response data ####
  # Progress, 08-10
  if (boo_Shiny == TRUE) {
    prog_det <- paste0("Load ", bio, ", response data")
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END

  if (bio == "bmi") {
    # Read bmi data files
    message("Reading BMI data files")
    boo.bmi <- TRUE
    useBC <- ifelse(is.na(useBC), FALSE, useBC)# QC
    calcRelAbund <- ifelse(is.na(calcRelAbund), FALSE, calcRelAbund)# QC
    list.bmiData <- prepRespData(out.dir  = file.path(out.dir, dn_checked_sk),
                                 bio      = "bmi",
                                 loaded   = loaded,
                                 useBC    = useBC,
                                 bioIndex = bmiIndexGp,
                                 calcRelAbund = calcRelAbund)
    data_bmiMetrics     <- list.bmiData$data_bioMetrics
    data_bmiMetricsInfo <- list.bmiData$data_bioMetricsInfo
    data_bmiCounts      <- list.bmiData$data_bioCounts
    data_bmiMasterTaxa  <- list.bmiData$data_bioMasterTaxa

    responsesOutput <- responsesOutput |>
      dplyr::bind_rows(data_bmiMetricsInfo |>
                         dplyr::select(MetricName, MetricLabel, IndexYN) |>
                         dplyr::mutate(BioComm = bio))

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_bmiCoOccur <- getCoOccurDataset(df_sites  = data_Sites,
                                         df_stress = data_Stress,
                                         biocomm   = "BMI",
                                         df_resp   = data_bmiMetrics,
                                         index     = bmiIndexGp,
                                         lagdays   = lagdays)
    data_respTrim <- rbind(data_respTrim,
                           data_bmiCoOccur[, c("StationID",
                                               "RespSampleID",
                                               "RespSampleDate",
                                               "BioComm")] %>%
                             dplyr::mutate(biocomm = "BMISampleID"))
  } # end BMI
  if (bio == "alg") {
    # Read alg data files
    message("Reading alg data files")
    boo.alg <- TRUE
    useBC <- ifelse(is.na(useBC), FALSE, useBC)# QC
    calcRelAbund <- ifelse(is.na(calcRelAbund), FALSE, calcRelAbund)# QC
    list.algData <- prepRespData(out.dir  = file.path(out.dir, dn_checked_sk),
                                 bio      = "alg",
                                 loaded   = loaded,
                                 useBC    = useBC,
                                 bioIndex = algIndexGp,
                                 calcRelAbund = calcRelAbund)
    data_algMetrics     <- list.algData$data_bioMetrics
    data_algMetricsInfo <- list.algData$data_bioMetricsInfo
    data_algCounts      <- list.algData$data_bioCounts
    data_algMasterTaxa  <- list.algData$data_bioMasterTaxa

    responsesOutput <- responsesOutput |>
      dplyr::bind_rows(data_algMetricsInfo |>
                         dplyr::select(MetricName, MetricLabel, IndexYN) |>
                         dplyr::mutate(BioComm = bio))

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_algCoOccur <- getCoOccurDataset(df_sites  = data_Sites,
                                         df_stress = data_Stress,
                                         biocomm   = "alg",
                                         df_resp   = data_algMetrics, # LCN 9/22/25 changed from data_bmiMetrics
                                         index     = algIndexGp,
                                         lagdays   = lagdays)
    data_respTrim <- rbind(data_respTrim,
                           data_algCoOccur[, c("StationID", "RespSampleID",
                                               "RespSampleDate", "BioComm")]%>%
                             dplyr::mutate(biocomm = "AlgSampleID"))
  } # end ALG
  if (bio == "fish") {
    # Read fish data files
    message("Reading fish data files")
    boo.fish <- TRUE
    useBC <- ifelse(is.na(useBC), FALSE, useBC)# QC
    calcRelAbund <- ifelse(is.na(calcRelAbund), FALSE, calcRelAbund)# QC
    list.fishData <- prepRespData(out.dir  = file.path(out.dir, dn_checked_sk),
                                  bio      = "fish",
                                  loaded   = loaded,
                                  useBC    = useBC,
                                  bioIndex = fishIndexGp,
                                  calcRelAbund = calcRelAbund)
    data_fishMetrics     <- list.fishData$data_bioMetrics
    data_fishMetricsInfo <- list.fishData$data_bioMetricsInfo
    data_fishCounts      <- list.fishData$data_bioCounts
    data_fishMasterTaxa  <- list.fishData$data_bioMasterTaxa

    responsesOutput <- responsesOutput |>
      dplyr::bind_rows(data_fishMetricsInfo |>
                         dplyr::select(MetricName, MetricLabel, IndexYN) |>
                         dplyr::mutate(BioComm = bio))

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_fishCoOccur <- getCoOccurDataset(df_sites = data_Sites,
                                         df_stress = data_Stress,
                                         biocomm   = "FISH", # TODO make sure this is ok capitalized
                                         df_resp   = data_fishMetrics, # LCN 9/22/25 changed from data_bmiMetrics
                                         index     = fishIndexGp,
                                         lagdays   = lagdays)

    data_respTrim <- rbind(data_respTrim,
                           data_fishCoOccur[, c("StationID", "RespSampleID",
                                                "RespSampleDate", "BioComm")] %>%
                             dplyr::mutate(biocomm = "FishSampleID"))
  } # end FISH

}## FOR ~ b

# QC, 20251215
# fails, if not all col names present
col2check_data_respTrim <- c("StationID",
                             "RespSampleID",
                             "RespSampleDate",
                             "biocomm")
if(sum(col2check_data_respTrim %in% names(data_respTrim)) ==
   length(col2check_data_respTrim)) {
  data_respTrim <- data_respTrim %>%
  dplyr::distinct(StationID, RespSampleID, RespSampleDate, biocomm)
}## IF ~ col2check_data_respTrim

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

#~~~~~~~~~~~~~~~~~~~~~~~
# 09, Sample summary ####
# Progress, 11
# NOTE: This must use all of the data, including outliers
if (boo_Shiny == TRUE) {
  prog_det <- "Generate summary of sample types"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
data_sampSummary <- getAllSamplesTable(df.stress     = data_Stress,
                                       df.stressInfo = data_stressInfo,
                                       df.resp       = data_respTrim,
                                       df.sites      = data_Sites,
                                       incaseColName = incaseColName)
# Returns: data_sampSummary (df.sampSummary)
# Colnames include: StationID, COMID, OutcaseCol, IncaseCol, SampleDate,
# ChemistrySampleID, FieldSampleID, HabitatSampleID, ModeledSampleID,
# BMISampleID, AlgSampleID, FishSampleID (assuming all sample types are available)
rm(data_respTrim)
# Data prep completed
#~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~
# RUN CASTool ####
# 10, Target site selection ####
# Progress, 12
if (boo_Shiny == TRUE) {
  prog_det <- "Select target site"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
df_targets <- readRDS(file.path(out.dir, dn_checked_sk, "df_targets.rds"))

### Evaluate each target site
## Use this for debugging
if (boo_Shiny == TRUE) {
  df_targets <- data.frame("TargetSiteID" = input$si_checked_sites_targ,
                           "Chosen by" = NA,
                           "Comment" = NA)
  names(df_targets)[2] <- "Chosen by"
  # ok since Shiny only works on 1 sites
}

#~~~~~~~~~~~~~~~~~~~~~~~
# 11, Main Code ####
# Progress, 13
if (boo_Shiny == TRUE) {
  prog_det <- "Check target site data availability"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

status_df <- data.frame(TargetSiteID = character(), status = character(), reason = character())

# FOR ~ site ~ START ####
for (site in seq_len(nrow(df_targets))) {
  TargetSiteID <- df_targets$TargetSiteID[site]
  dir_results <- out.dir

  if (is.na(TargetSiteID)) {
    temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID), status = "Failed", reason = "TargetSiteID is NA")
    status_df <- status_df %>% dplyr::bind_rows(temp_status)
    next
  } else if(data_Sites %>% dplyr::filter(StationID == TargetSiteID) %>% nrow() == 0){ # LCN added 20250917
    temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID), status = "Failed", reason = "TargetSiteID not found in Sites file")
    status_df <- status_df %>% dplyr::bind_rows(temp_status)

    msg <- "TargetSiteID not found in the Sites file."
    message(msg)
    next
  } else if (is.na(data_Sites$IncaseCol[data_Sites$StationID == TargetSiteID])) {
    temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID), status = "Failed", reason = "TargetSiteID not assigned an inside-the-case identifier")
    status_df <- status_df %>% dplyr::bind_rows(temp_status)

    msg <- "No inside-the-case identifier available"
    message(msg)
    next
  }
  msg <- paste0("Evaluating site: ", TargetSiteID)
  message(msg)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Biocomm-independent functions ####

  # Create high-level results folder structure
  #dir_results <- out.dir
  dir_sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(dir_results, dir_sub2)) == TRUE,
         dir.create(file.path(dir_results, dir_sub2)), FALSE)

  # Define datagaps data frame ####
  gaps    <- data.frame(fxnname = character(), condition = character(),
                        result = character(), comment = character())

  # Initialize stressor elimination data frame
  df_stressorElim <- data.frame(
    Stressor = character(),
    Biocomm = character(),
    Reason = character()
  )

  # 12, getComparators ####
  ## Progress, 14
  if (boo_Shiny == TRUE) {
    prog_det <- "Get comparator site data"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Identify comparator sites & write # comparators to data gaps file
  # This is predicated on the fact that BC distance is calculated based on
  # expected benthic macroinvertebrate taxa. If there are ever different
  # BC matrices for different biocomms, then this must move into the biocomm
  # loop or it needs to be run more than once for each biocomm here, since
  # it's used in getSiteInfo immediately afterward.

  compSitesList <- list()

  for(b in seq_along(biocommlist)){
    bio <- tolower(biocommlist[b])

    if(bio == "bmi"){
      # QC
      if (!exists("data_BCdist")) {
        data_BCdist <- NULL
      }## IF ~ exists(data_BCdist)
      list.CompSites.bmi <- getComparators(TargetSiteID = TargetSiteID,
                                       df_sites = data_Sites,
                                       df_cluster = data_cluster,
                                       df_bioCoOccur = data_bmiCoOccur,
                                       bioIndex = bmiIndexGp,
                                       bio = bio,
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
      compSitesList[[bio]] <- list.CompSites.bmi
    }
    if(bio == "alg"){
      list.CompSites.alg <- getComparators(TargetSiteID = TargetSiteID,
                                           df_sites = data_Sites,
                                           df_cluster = data_cluster,
                                           df_bioCoOccur = data_algCoOccur,
                                           bioIndex = algIndexGp,
                                           bio = bio,
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
      compSitesList[[bio]] <- list.CompSites.alg
    }
    if(bio == "fish"){
      list.CompSites.fish <- getComparators(TargetSiteID = TargetSiteID,
                                           df_sites = data_Sites,
                                           df_cluster = data_cluster,
                                           df_bioCoOccur = data_fishCoOccur,
                                           bioIndex = fishIndexGp,
                                           bio = bio,
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
      compSitesList[[bio]] <- list.CompSites.fish
    }
  }

  # just chooses the first biocomm as list.CompSites, which shouldn't matter unless potentially useBC = TRUE
  list.CompSites <- compSitesList[[1]]

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

  # 13, getSiteInfo, getSiteMap, writeOutliers ####
  # Progress, 15
  if (boo_Shiny == TRUE) {
    prog_det <- "Generate index boxplots and create site map"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  # Get site information for general use (map, sample summary, etc)

  # Create site info folder with watershed-scale stressor boxplots,
  # boxplots for bio indices, and folder for photos
  list.SiteInfo <- getSiteInfo(TargetSiteID   = TargetSiteID,
              TargetCOMID    = list.CompSites$TargetCOMID,
              df_Sites       = data_Sites,
              df_SampSummary = data_sampSummary,
              biocommlist    = biocommlist,
              df_BMIMetrics  = data_bmiMetrics,
              BMIIndexGp     = bmiIndexGp,
              df_ALGMetrics  = data_algMetrics,
              ALGIndexGp     = algIndexGp,
              df_FishMetrics = data_fishMetrics,
              FishIndexGp    = fishIndexGp,
              comp.sites     = list.CompSites$comp.sites,
              all.sites      = list.CompSites$all.sites,
              IncaseLabel    = incaseLabel,
              OutcaseLabel   = outcaseLabel,
              useBC          = useBC,
              plotvars       = data_plotvars,
              refSiteCol     = refOutline_col,
              plotdpi        = plot_dpi,
              plotH          = plot_H,
              plotW          = plot_W,
              plotunits      = plot_units,
              dir_photo      = file.path(in.dir, region, "Photos"),
              dir_results    = out.dir,
              dir_sub        = "SiteInfo",
              boo_plot       = TRUE)

  if (boo.WS) {
    list.WSStressorFigs <- getWSStressorFigs(TargetSiteID      = TargetSiteID,
                      df_WSData         = data_stressorWS,
                      df_WSInfo         = data_stressorinfoWS,
                      df_Sites          = data_Sites,
                      comp.reaches      = list.CompSites$comp.reaches,
                      TargetCOMID       = list.CompSites$TargetCOMID,
                      useAllCompReaches = useAllCompReaches,
                      useBC             = useBC,
                      dir_sub           = "SiteInfo",
                      df_SampSummary    = data_sampSummary,
                      biocommlist       = biocommlist,
                      boo_plot          = TRUE,
                      plotdpi           = plot_dpi,
                      plotH             = plot_H,
                      plotW             = plot_W,
                      plotunits         = plot_units,
                      dir_results       = out.dir)
  }


  # Create site map
  boundary <- readRDS(file.path(out.dir, dn_checked_sk, "boundary.rds"))
  reaches <- readRDS(file.path(out.dir, dn_checked_sk, "reaches.rds")) %>%
    dplyr::mutate(COMID = as.character(COMID))
  flowline <- reaches %>%
    dplyr::left_join(data_cluster %>%
                       dplyr::mutate(COMID = as.character(COMID),
                              ClusterID = as.factor(ClusterID)),
                     by = "COMID")


  getSiteMap(sp_outline   = boundary,
             sp_flowline  = flowline,
             region       = region,
             datum        = datum,
             df_sites     = data_Sites,
             allSites     = list.CompSites$all.sites,
             compSites    = list.CompSites$comp.sites,
             TargetSiteID = TargetSiteID,
             useBC        = useBC,
             plotvars     = data_plotvars,
             refOutline   = refOutline_col,
             dir_results  = dir_results,
             dir_sub      = "SiteInfo",
             dir_map_rmd  = file.path(system.file(package = "CASTfxn"), "rmd"))
  # Prints static map (.png)

  msg <- "getSiteInfo, getWSstressorFigs, and getSiteMap are complete."
  message(msg)

  # 14, getAvailableDataTypes ####
  # Progress, 17
  if (boo_Shiny == TRUE) {
    prog_det <- "Identify outliers"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Prepare flags for types of stressor and response data to use
  list.AvailData <- getAvailableDataTypes(TargetSiteID   = TargetSiteID,
                                          df_stress      = data_Stress,
                                          df_SampSummary = data_sampSummary,
                                          biocommlist    = biocommlist,
                                          dir_results    = dir_results)

  noStressors    <- list.AvailData$noStressors
  noResponses    <- list.AvailData$noResponses
  useBMI         <- list.AvailData$useBMI
  useAlg         <- list.AvailData$useAlg
  useFish        <- list.AvailData$useFish
  siteDetectsAll <- list.AvailData$siteDetectsAll

  stressUse <- data_stressInfo |>
    dplyr::filter(UseInStressorID == 1) |>
    dplyr::pull(StdParamName)

  initialStress <- data_Stress |>
    dplyr::filter(is.na(TransfResult)==FALSE) |>
    dplyr::filter(StdParamName %in% stressUse) |>
    dplyr::distinct(StdParamName) |>
    dplyr::pull(StdParamName)

  df_initialStress <- data.frame(Stressor = initialStress) |>
    dplyr::left_join(data_stressInfo |> dplyr::select(StdParamName, Label), by = c("Stressor" = "StdParamName"))

  #rm(list.AvailData)

  if ((noStressors == TRUE) | (noResponses == TRUE)) {

   if(noStressors == TRUE){
      cond <- "Number of stressor samples"
      msg <- paste0("No stressor data are available for ",
                    TargetSiteID)
    }
    if(noResponses == TRUE){
      cond <- "Number of response samples"
      msg <- paste0("No response data are available for ",
                    TargetSiteID)
    }

    message(msg)

    gap.statement <- data.frame(
      fxnname = "getAvailableDataTypes",
      condition = cond,
      result = "0",
      comment = msg
    )

    gaps <- gaps |> dplyr::bind_rows(gap.statement)

    temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID), status = "Failed", reason = msg)
    status_df <- status_df %>% dplyr::bind_rows(temp_status)

    next

  } ### End no stressors statement GO TO NEXT SITE
  rm(noStressors, noResponses)

  # Write target site outliers, comparator site outliers (inside the case),
  # and all outliers (outside the case)
  list.Outliers <- writeOutliers(TargetSiteID  = TargetSiteID,
                df_outliers   = data_stressoutliers,
                df_stressInfo = data_stressInfo,
                df_Sites      = data_Sites,
                useBC         = FALSE,
                siteDetectsAll= siteDetectsAll,
                compSites     = list.CompSites$comp.sites,
                allSites      = list.CompSites$all.sites,
                dir_results   = dir_results)
  # Writes outliers to data gaps file

  msg <- "getAvailableDataTypes and writeOutliers are complete."
  message(msg)

  # FOR ~ b ~ START ####
  # if (boo.debug == TRUE & debug.person == "Erik") {
  #   # 1 = bmi, 2 = alg, 3 = fish
  #   biocommlist <- "alg"
  # }

  for (b in seq_along(biocommlist)) {

    # Define biocomm data
    bioComm <- tolower(biocommlist[b])
    if ((bioComm == "bmi") && (useBMI == TRUE)) {
      data_bioCoOccur <- data_bmiCoOccur
      bioIndexGp      <- bmiIndexGp
      bioMetricData   <- data_bmiMetrics
      bioMetricInfo   <- data_bmiMetricsInfo
      bioTaxaData     <- data_bmiCounts
      bioMasterTaxa   <- data_bmiMasterTaxa

      list.CompSites <- list.CompSites.bmi

      # colBio <- bmiIndex
    } else if ((bioComm == "alg") && (useAlg == TRUE)) {
      data_bioCoOccur <- data_algCoOccur
      bioIndexGp      <- algIndexGp
      bioMetricData   <- data_algMetrics
      bioMetricInfo   <- data_algMetricsInfo
      bioTaxaData     <- data_algCounts
      bioMasterTaxa   <- data_algMasterTaxa

      list.CompSites <- list.CompSites.alg

      # colBio <- algIndex
    } else if ((bioComm == "fish") && (useFish == TRUE)) {
      data_bioCoOccur <- data_fishCoOccur
      bioIndexGp      <- fishIndexGp
      bioMetricData   <- data_fishMetrics
      bioMetricInfo   <- data_fishMetricsInfo
      bioTaxaData     <- data_fishCounts
      bioMasterTaxa   <- data_fishMasterTaxa

      list.CompSites <- list.CompSites.fish

      # colBio <- fishIndex
    } else {
      msg <- paste0(bioComm, " is not a valid biological community.")
      message(msg)
      next
    }

    ### Define LoE dataframe ----
    df_LoE <- data.frame(StationID        = character(),
                         StressSampleID   = character(),
                         StressSampleDate = as.Date(character()),
                         RespSampleID     = character(),
                         RespSampleDate   = as.Date(character()),
                         bioComm          = character(),
                         bioIndexName     = character(),
                         bioIndex         = numeric(),
                         Quality          = character(),
                         Stressor         = character(),
                         StressorValue    = numeric(),
                         LoE              = character(),
                         Score            = character(),
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

      gapcomment <- paste0("No stressor samples are available for ", TargetSiteID,
                           " within ", lagdays[1], " days before, and ", lagdays[2],
                           " after the ", bioComm, " sample(s) was(were) obtained.")

      gap.statement <- data.frame(
        fxnname = "getCoOccurDataset",
        condition = paste0("Paired stressor-", bioComm, " data"),
        result = "0",
        comment = gapcomment
      )

      gaps <- gaps |> dplyr::bind_rows(gap.statement)

      msg <- paste0(msg, "\nProceeding to next response community or site, ",
                    "as appropriate.")
      message(msg)

      next
    } ### End no stressors statement

    possibleStressors <- intersect(initialStress, names(dfTarget))
    targMeasStress <- dfTarget |>
      dplyr::select(dplyr::all_of(possibleStressors)) |>
      tidyr::pivot_longer(cols = everything()) |>
      dplyr::filter(is.na(value)==FALSE) |>
      dplyr::distinct(name) |>
      dplyr::pull(name)

    targNotMeasStress <- setdiff(initialStress, targMeasStress)

    if(length(targNotMeasStress) > 0){
      tempElim <- data.frame(Stressor = targNotMeasStress,
                             Biocomm = bioComm,
                             Reason = "Not measured at target site")

      df_stressorElim <- df_stressorElim |>
        dplyr::bind_rows(tempElim)
    }


    ## 15, getQualSites ####
    # Progress, 18
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; summarize index values")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Run analyses
    # Identify "quality" samples using different definitions
    # These are identified only from paired stressor-response samples
    # Data gaps statements from getSiteInfo aren't necessarily paired

    # IMPORTANT
    # This step adds "RefSiteFlag", BetterThan", "IncaseYN", and "OutcaseYN" to
    # the dataframe, data_bioCoOccur, allowing subsets to be created as needed.
     list.QualSites <- getQualSites(TargetSiteID = TargetSiteID,
                                        biocomm      = bioComm,
                                        df_qual      = data_bioCoOccur,
                                        colBio       = bioIndexGp,
                                        refSites     = refSites,
                                        compSites    = list.CompSites$comp.sites, # inside the case
                                        allSites     = list.CompSites$all.sites, # outside the case
                                        #stressors    = siteDetectsAll,
                                        stressors = targMeasStress,
                                        dir_results  = dir_results,
                                        dir_sub      = "SiteInfo")

    df_PairedStressResp <- list.QualSites$df_qual
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
                    all_of(bioIndexGp), Quality,
                    #all_of(siteDetectsAll)
                    all_of(targMeasStress)
                    ) %>%
      tidyr::pivot_longer(cols = !(StationID:Quality), names_to = "Stressor",
                          values_to = "StressorValue", values_drop_na = TRUE) %>%
      dplyr::group_by(Stressor) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop_last")

    insuffSamples <- df_PairedStressResp.stats$Stressor[which(df_PairedStressResp.stats$n < samplim)]

    df_PairedStressResp <- df_PairedStressResp %>%
      dplyr::select(!all_of(insuffSamples))

    rm(df_PairedStressResp.stats)

    # Write these to data gaps file
    if (length(insuffSamples) != 0) { # changed 3/10/26 LCN was previously == 0, which is incorrect
      for (i in seq_along(insuffSamples)) {
        str <- paste(bioComm, insuffSamples[i], sep = ": ")
        msg <- paste0("Insufficient number of paired ", bioComm, " samples are available for ", str, ". This stressor will not be evaluated.")
        message(msg)

        gap.statement <- data.frame(
          fxnname = "getQualSites",
          condition = str,
          result = paste0("<", samplim),
          comment = msg)

        gaps <- gaps |>
          dplyr::bind_rows(gap.statement)

        tempElim <- data.frame(Stressor = insuffSamples[i],
                               Biocomm = bioComm,
                               Reason = "Insufficient paired samples")

        df_stressorElim <- df_stressorElim |>
          dplyr::bind_rows(tempElim)
      } #End loop over stressors
    } #End if

    priorElim <- df_stressorElim |>
      dplyr::filter(Biocomm == bioComm) |>
      dplyr::distinct(Stressor) |>
      dplyr::pull(Stressor)

    stressorMeasSuff <- setdiff(initialStress, priorElim)

    if(length(stressorMeasSuff) == 0){
      temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID),
                                status = "Failed",
                                reason = "No stressors remaining after removing stressors with no paired stressor-response samples at the target site and stressors with insufficient number of samples at comparator sites. ")
      status_df <- status_df %>% dplyr::bind_rows(temp_status)

      next
    }

    msg <- paste0("getQualSites is complete for ", bioComm, ".")
    message(msg)

    ## 16, getCoOccur ####
    # Progress, 21
    if (boo_Shiny == TRUE) {
      prog_det <-  paste0(bioComm, "; run co-occurrence line of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get Co-occurrence from comparator samples with not degraded samples
    # Answers the question: "Are the observed stressor levels consistent
    # with impairment where and when it occurs"?
    if (TargetSiteID %in% unique(data_bioCoOccur$StationID)) {
      msg <- "Starting Co-occurrence"
      message(msg)

      list.StressorMetaData <- getCoOccur(TargetSiteID  = TargetSiteID,
                                          df_data       = df_PairedStressResp,
                                          #detects       = siteDetectsAll, # needed to change this because possible a stressor was measured but not matched to response
                                          detects = stressorMeasSuff,
                                          df_stressinfo = data_stressInfo,
                                          compsites     = list.CompSites$comp.sites,
                                          biocomm       = bioComm,
                                          colBio        = bioIndexGp,
                                          pHlimLow      = pHlimLow,
                                          pHlimHigh     = pHlimHigh,
                                          DOlim         = DOlim,
                                          plotvars      = data_plotvars,
                                          plotdpi       = plot_dpi,
                                          plotH         = plot_H,
                                          plotW         = plot_W,
                                          plotunits     = plot_units,
                                          dir_plots     = dir_results,
                                          dir_sub       = "_WoE",
                                          boo_plot      = boo.plot.user,
                                          incaseLabel   = incaseLabel,
                                          targetSampleLabels = targetSampleLabels)

      df_stressorMetadata <- list.StressorMetaData$df_stressorMetadata
      #notEvaluated <- c(insuffSamples, list.StressorMetaData$notEvaluated)
      df_COscores  <- list.StressorMetaData$df_COscores

      if(length(list.StressorMetaData$notEvaluated)>0){
        tempElim <- data.frame(Stressor = list.StressorMetaData$notEvaluated,
                               Biocomm = bioComm,
                               Reason = "Co-occurrence")

        df_stressorElim <- df_stressorElim |>
          dplyr::bind_rows(tempElim)
      }

    }

    if (nrow(df_stressorMetadata) == 0) {
      msg <- paste0("No candidate causes to evaluate further for ",
                    TargetSiteID, " for the ", bioComm, " community.")
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      gapcomment <- paste0(bioComm, ": All candidate causes were eliminated by the co-occurrence line of evidence for ",
                           TargetSiteID)

      gap.statement <- data.frame(
        fxnname = "getCoOccur",
        condition = msg,
        result = as.character(0),
        comment = gapcomment)

      gaps <- gaps |>
        dplyr::bind_rows(gap.statement)

      temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID),
                                status = "Failed",
                                reason = "No stressors remaining after removing stressors with no paired stressor-response samples at the target site,  stressors with insufficient number of samples at comparator sites, and co-occurrence screening. ")
      status_df <- status_df %>% dplyr::bind_rows(temp_status)

      next
    }

    if (nrow(df_COscores) != 0) {
      df_LoE <- df_COscores
    }
    rm(df_COscores, list.StressorMetaData)

    msg <- paste0("getCoOccur for ", bioComm, " is complete.")
    message(msg)

    ## 17, getTimeSeq ####
    # Progress, 20
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; run time sequence line of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Create time sequence graphics for ONLY target site
    # Uses all site stressor and response data, but not necessarily paired
    df_TS_scores <- getTimeSeq(TargetSiteID,
                               biocomm       = bioComm,
                               bioindex      = bioIndexGp,
                               df_stress     = data_Stress,
                               df_resp       = bioMetricData[bioMetricData$StationID == TargetSiteID, ],
                               df_respinfo   = bioMetricInfo,
                               df_stressinfo = df_stressorMetadata,
                               df_paired     = df_PairedStressResp,
                               plotdpi       = plot_dpi,
                               plotH         = plot_H,
                               plotW         = plot_W,
                               plotunits     = plot_units,
                               dir_results   = dir_results,
                               dir_sub       = "_WoE",
                               boo_plot      = boo.plot.user)

    if (nrow(df_TS_scores) != 0) {
      df_LoE <- rbind(df_LoE, df_TS_scores)
    }
    rm(df_TS_scores)

    msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
    message(msg)

    ## 18, getSufficiency ####
    # Progress, 24
    if (boo_Shiny == TRUE) {
      prog_det <-  paste0(bioComm, "; run sufficiency line of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get stressors sufficient to cause biological impairment using all comparator samples
    msg <- "Starting Sufficiency"
    message(msg)

    df_SuffScores <- getSufficiency(TargetSiteID  = TargetSiteID,
                                    df_data       = df_PairedStressResp,
                                    compSites     = list.CompSites$comp.sites,
                                    df_stressinfo = df_stressorMetadata,
                                    biocomm       = bioComm,
                                    colBio        = bioIndexGp,
                                    plotvars      = data_plotvars,
                                    plotdpi       = plot_dpi,
                                    plotH         = plot_H,
                                    plotW         = plot_W,
                                    plotunits     = plot_units,
                                    dir_plots     = dir_results,
                                    dir_sub       = "_WoE",
                                    boo_plot      = boo.plot.user,
                                    targetSampleLabels = targetSampleLabels)

    if (nrow(df_SuffScores) != 0) {
      df_LoE <- rbind(df_LoE, df_SuffScores)
    }
    rm(df_SuffScores)

    msg <- paste0("getSufficiency for ", bioComm, " is complete.")
    message(msg)

    ## 19, getBioStressorResponses ####
    # Progress, 25
    if (boo_Shiny == TRUE) {
      prog_det <-  paste0(bioComm, "; run biological gradient line of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get Stressor Responses inside (comparators) and outside (all) the case
    list.BioStressorResponses <- getBioStressorResponses(TargetSiteID  = TargetSiteID,
                                             df_stressinfo = df_stressorMetadata,
                                             df_respinfo   = bioMetricInfo,
                                             df_respdata   = bioMetricData,
                                             df_datapaired = df_PairedStressResp,
                                             biocomm       = bioComm,
                                             bioindex      = bioIndexGp,
                                             min_cases     = samplim,
                                             p.val_cutoff  = p.val_cutoff,
                                             r2_cutoff     = r2_cutoff,
                                             plotvars      = data_plotvars,
                                             refOutline    = refOutline_col,
                                             plotdpi       = plot_dpi,
                                             plotH         = plot_H,
                                             plotW         = plot_W,
                                             plotunits     = plot_units,
                                             dir_plots     = dir_results,
                                             dir_sub       = "_WoE",
                                             boo_pred_warn = TRUE,
                                             boo_plot      = boo.plot.user,
                                             targetSampleLabels = targetSampleLabels)

    df_gradscores <- list.BioStressorResponses$df.scores

    if (nrow(df_gradscores != 0)) {
      df_LoE <- rbind(df_LoE, df_gradscores)
    }
    rm(df_gradscores)

    msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
    message(msg)

    ## 20, getVerifiedPredictions ####
    # Progress, 26
    if (boo_Shiny == TRUE) {
      prog_det <-  paste0(bioComm, "; verified predictions lines of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get Stressor-specific regressions using comparator sites
    sstv.name      <- paste0("SSTVname.", bioComm)
    stressors.sstv <- as.vector(unlist(df_stressorMetadata$Stressor[!is.na(df_stressorMetadata[[sstv.name]])]))
    stressors.ssi  <- as.vector(unlist(df_stressorMetadata$Stressor[!is.na(df_stressorMetadata$SSIndex)]))

    # if (length(stressors.ssi) == 0 & length(stressors.sstv) == 0) {
    #
    #   msg <- paste0(bioComm, ": No site stressors have stressor-specific tolerance values or indices.")
    #   message(msg)
    #
    #   gap.statement <- data.frame(
    #     fxnname = "getVerifiedPredictions",
    #     condition = "Stressor-specific tolerance values and indices",
    #     result = "0",
    #     comment = msg
    #   )
    #
    #   gaps <- gaps |>
    #     dplyr::bind_rows(gap.statement)
    #
    # } else {
    if (length(stressors.ssi) != 0 | length(stressors.sstv) != 0) {

      if (length(stressors.sstv) > 0) { # one or more stressors.sstv

        list.VerifiedPredictions <- getVerifiedPredictions(TargetSiteID   = TargetSiteID,
                                              stressors.sstv = stressors.sstv,
                                              df_stressinfo  = df_stressorMetadata,
                                              df_paired      = df_PairedStressResp,
                                              biocomm        = bioComm,
                                              df_bioTaxaData = bioTaxaData,
                                              df_MasterTaxa  = bioMasterTaxa,
                                              colBio         = bioIndexGp,
                                              plotvars       = data_plotvars,
                                              plotdpi        = plot_dpi,
                                              plotH          = plot_H,
                                              plotW          = plot_W,
                                              plotunits      = plot_units,
                                              dir_plots      = dir_results,
                                              dir_sub        = "_WoE",
                                              boo_plot       = boo.plot.user,
                                              targetSampleLabels = targetSampleLabels)

        df_VPscores <- list.VerifiedPredictions$df.scores

        if (nrow(df_VPscores)!= 0) { # LCN changed 20250917
          df_LoE <- rbind(df_LoE, df_VPscores)
        }
        rm(df_VPscores)

      } else { # no sstvs

        msg <- "No site stressors have stressor-specific tolerance values"
        message(msg)

        gap.statement <- data.frame(
          fxnname = "getVerifiedPredictions",
          condition = TargetSiteID,
          result = "0",
          comment = msg
        )

        gaps <- gaps |>
          dplyr::bind_rows(gap.statement)

      }

      if (length(stressors.ssi) > 0) { # one or more stressors.ssi

        info.stress.ssi <- df_stressorMetadata %>%
          dplyr::filter(Stressor %in% stressors.ssi) %>%
          dplyr::select(Stressor, SSIndex, Label, LogTransf)

        info.ssi <- bioMetricInfo %>%
          dplyr::filter(MetricName %in% unique(info.stress.ssi$SSIndex))

        if(nrow(info.ssi)==0){
          msg <- paste0(paste(unique(info.stress.ssi$SSIndex), collapse = ", "), " are listed as stressor-specific index in stressor metadata but are not found as a metric to be included in ", bioComm, " response metadata.")
          message(msg)

          gap.statement <- data.frame(
            fxnname = "getVPSSIscores",
            condition = "Missing stressor-specific index information",
            result = unique(info.stress.ssi$SSIndex),
            comment = msg
          )

          gaps <- gaps |>
            dplyr::bind_rows(gap.statement)

        } else{
          list.VPSSIscores <- getVPSSI(TargetSiteID     = TargetSiteID,
                                       stressors.ssi    = stressors.ssi,
                                       df_stressinfo    = df_stressorMetadata,
                                       df_paired        = df_PairedStressResp,
                                       biocomm          = bioComm,
                                       df_bioMetricData = bioMetricData,
                                       df_bioMetricInfo = bioMetricInfo,
                                       colBio           = bioIndexGp,
                                       plotvars         = data_plotvars,
                                       plotdpi          = plot_dpi,
                                       plotH            = plot_H,
                                       plotW            = plot_W,
                                       plotunits        = plot_units,
                                       dir_plots        = dir_results,
                                       dir_sub          = "_WoE",
                                       boo_plot         = boo.plot.user,
                                       targetSampleLabels = targetSampleLabels)

          df_VPSSIscores <- list.VPSSIscores$df.scores

          if (nrow(df_VPSSIscores) != 0) { # LCN changed 20250917
            df_LoE <- rbind(df_LoE, df_VPSSIscores)
          }
          rm(df_VPSSIscores)
        }



      } else { # no ssis

        msg <- "No site stressors have stressor-specific indices"
        message(msg)

        gap.statement <- data.frame(
          fxnname = "getVPSSIscores",
          condition = TargetSiteID,
          result = "0",
          comment = msg
        )

        gaps <- gaps |>
          dplyr::bind_rows(gap.statement)

      }

    } ### End getVP evaluation

    msg <- paste0("getVerifiedPredictions for ", bioComm, " is complete.")
    message(msg)

    ## 21, getWOE ####
    # Progress, 27
    if (boo_Shiny == TRUE) { # needs updating
      prog_det <-  paste0(bioComm, "; get weight of evidence table")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # LCN addition 3/10/26 to get rid of observations with only BioGrad scores
    df_LoE <- df_LoE |> dplyr::filter(is.na(StressorValue)==FALSE)

    getWoE(TargetSiteID = TargetSiteID,
           biocomm      = bioComm,
           dfLoE        = df_LoE,
           dfStress     = df_stressorMetadata,
           dir_results  = dir_results,
           dir_WoE      = "_WoE",
           plotdpi = plot_dpi,
           plotH = plot_H,
           plotW = plot_W,
           plotunits = plot_units,
           boo_plot = boo.plot.user)
    msg <- paste0("getWoE for ", bioComm, " is complete.")
    message(msg)

  } ### End biocomm loop
  ## FOR ~ b ~ END ####

  ## 22, getReport ####
  # Progress, 28
  if (boo_Shiny == TRUE) {
    prog_det <- "Get report"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #

  df_stressorElim <- df_stressorElim |>
    dplyr::left_join(data_stressInfo |> dplyr::select(StdParamName, Label), by = c("Stressor" = "StdParamName"))

  write.csv(df_stressorElim,
            file.path(dir_results,
                      TargetSiteID,
                      paste0(TargetSiteID,
                             "_StressorsEliminated.csv")),
            row.names = FALSE)

  write.csv(df_initialStress,
            file.path(dir_results,
                      TargetSiteID,
                      paste0(TargetSiteID,
                             "_InitialStressors.csv")),
            row.names = FALSE)

  if(exists("info.ssi")){
    responsesOutput <- responsesOutput |>
      dplyr::mutate(SSI = dplyr::if_else(MetricName %in% info.ssi$MetricName, "Y", "N"))
  } else{
    responsesOutput <- responsesOutput |>
      dplyr::mutate(SSI = "N")
  }


  write.csv(responsesOutput,
            file.path(dir_results,
                      TargetSiteID,
                      paste0(TargetSiteID,
                             "_Responses.csv")),
            row.names = FALSE)

  # Shiny add ons
  if (boo_Shiny == TRUE) {
    report_type <- "full" # summary preliminary full
    # browser()
    # getwd()
    # list.files()

    # copy RMD so works in Shiny
    ## render switches working directory to location of RMD
    rmd2copy <- list.files(file.path(system.file(package = "CASTfxn"), "rmd"),
                           pattern = "\\.rmd$",
                           full.names = TRUE)
    file.copy(rmd2copy, ".", overwrite = TRUE)

    # need graphic as well
    svg2copy <- list.files(file.path(system.file(package = "CASTfxn"), "rmd"),
                           pattern = "\\.svg$",
                           full.names = TRUE)
    file.copy(svg2copy, ".", overwrite = TRUE)

    # browser()
    # not found, added to function call

    # report
    getReport(TargetSiteID = TargetSiteID,
              biocommlist    = biocommlist,
              regionName     = region,
              primeIndex     = bmiIndexGp,
              removeOutliers = removeOutliers,
              samplim        = samplim,
              r2_cutoff      = r2_cutoff,
              p.val_cutoff   = p.val_cutoff,
              useBC          = useBC,
              lagdays        = lagdays,
              DOlim          = DOlim,
              pHlimLow       = pHlimLow,
              pHlimHigh      = pHlimHigh,
              bmiIndex       = bmiIndexGp,
              algIndex       = algIndexGp,
              fishIndex      = fishIndexGp,
              useBMI         = useBMI,
              useAlg         = useAlg,
              useFish        = useFish,
              dir_data       = normalizePath(dir_data),
              dir_results    = normalizePath(dir_results),
              report_type    = report_type, # full, preliminary, summary
              report_format  = "html",
              dir_rmd = ".", # added for Shiny after copy RMD
              boo.WS = boo.WS,
              data_sampSummary = data_sampSummary,
              data_bmiMetrics = data_bmiMetrics,
              data_algMetrics = data_algMetrics,
              data_fishMetrics = data_fishMetrics,
              data_stressInfo = data_stressInfo,
              #siteDetectsAll = siteDetectsAll,
              siteDetectsAll = targMeasStress
              )

  } else {
    report_type <- "full"

    getReport(TargetSiteID = TargetSiteID,
              biocommlist    = biocommlist,
              regionName     = region,
              primeIndex     = bmiIndexGp,
              removeOutliers = removeOutliers,
              samplim        = samplim,
              r2_cutoff      = r2_cutoff,
              p.val_cutoff   = p.val_cutoff,
              useBC          = useBC,
              lagdays        = lagdays,
              DOlim          = DOlim,
              pHlimLow       = pHlimLow,
              pHlimHigh      = pHlimHigh,
              bmiIndex       = bmiIndexGp,
              algIndex       = algIndexGp,
              fishIndex      = fishIndexGp,
              useBMI         = useBMI,
              useAlg         = useAlg,
              useFish        = useFish,
              dir_data       = normalizePath(dir_data),
              dir_results    = normalizePath(dir_results),
              report_type    = "full",
              report_format  = "html",
              boo.WS = boo.WS,
              data_sampSummary = data_sampSummary,
              data_bmiMetrics = data_bmiMetrics,
              data_algMetrics = data_algMetrics,
              data_fishMetrics = data_fishMetrics,
              data_stressInfo = data_stressInfo,
              #siteDetectsAll = siteDetectsAll,
              siteDetectsAll = targMeasStress
              )

  }## IF ~ boo_Shiny

  gaps <- gaps |>
    dplyr::bind_rows(tryCatch(list.CompSites.alg$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.CompSites.fish$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.CompSites.bmi$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.SiteInfo$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.WSStressorFigs$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.AvailData$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.Outliers$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.QualSites$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.StressorMetadata$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.BioStressorResponses$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.VerifiedPredictions$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.VPSSIscores$df_gap, error = function(e) NULL))

  write.csv(gaps, file.path(dir_results,
                                TargetSiteID,
                                paste0(TargetSiteID, "_datagaps.csv")),
              row.names = FALSE)

  # dfGaps <- read.table(file.path(dir_results,
  #                                TargetSiteID,
  #                                paste0(TargetSiteID, "_datagaps.tab")),
  #                      header = TRUE,
  #                      sep = "\t")

  # dfGaps <- unique(dfGaps)
  # write.table(dfGaps, file.path(dir_results,
  #                               TargetSiteID,
  #                               paste0(TargetSiteID, "_datagaps.tab")),
  #             append = FALSE,
  #             col.names = TRUE,
  #             row.names = FALSE,
  #             sep = "\t")




  # # siteDetectsAll needed for Shiny app
  # fn_detects_all <- file.path(dir_results,
  #                             TargetSiteID,
  #                             paste0(TargetSiteID,
  #                                    "_DetectsAll.csv"))
  # df_detects_all <- data.frame(Detects_All = siteDetectsAll)
  # write.csv(df_detects_all,
  #             fn_detects_all,
  #             append = FALSE,
  #             col.names = TRUE,
  #             row.names = FALSE)

# status
  temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID), status = "Passed", reason = "")
  status_df <- status_df %>% dplyr::bind_rows(temp_status)

  } # End TargetSite loop

# FOR ~ site ~ END ####

fn_status <- file.path(dir_results,
                       paste0("TargetSiteID_Status_",
                              format(Sys.Date(),"%Y%m%d"),
                              "_",
                              format(Sys.time(),"%H%M%S"),
                              ".csv"))
write.csv(status_df,
          fn_status,
          row.names = FALSE)

rm(site)

toc <- Sys.time()
msg <- paste0("report time (min): ",
              round(difftime(toc, tic, units = "min"), 2))
message(msg)

# 23, Clean Up ----
# Clean up operations
if (boo_Shiny == TRUE) {
  prog_det <- "Clean Up"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# siteDetectsAll needed for Shiny app
# fn_detects_all <- file.path(dir_results,
#                             TargetSiteID,
#                             paste0(TargetSiteID,
#                                    "_DetectsAll.tab"))
# df_detects_all <- data.frame(Detects_All = siteDetectsAll)
# write.table(df_detects_all,
#             fn_detects_all,
#             append = FALSE,
#             col.names = TRUE,
#             row.names = FALSE,
#             sep = "\t")

# 23 getSummaryAllSites
# Progress, 29
# if (boo_Shiny == TRUE) {
#   prog_det <- "getSummaryAllSites"
#   prog_cnt <- prog_cnt + 1
#   prog_msg <- paste0("Step ", prog_cnt)
#   prog_inc <- 1 / prog_n
#   incProgress(prog_inc, message = prog_msg, detail = prog_det)
#   Sys.sleep(prog_sleep)
#   message(paste(prog_msg, prog_det, sep = "; "))
# }## IF ~ boo_Shiny ~ END
# Nothing here, 20251215, still a section?

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, END ####
# external/RPPTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
