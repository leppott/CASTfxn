#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#  R version 4.4.3
#
#' @title prepSiteData
#'
#' @description Prepare measured stressor data for CASTool
#'
#' @details Reviews each uploaded file against both user-input data (either via the shiny app or contained in the CASToolMetadata.xlsx file) to evaluate 1) if the file exists, 2) if it contains the required columns (which requires that they be named correctly), 3) whether datatypes meet requirements.
#'
#' If any required files do not exist, return the list of missing files and shut down.
#' If all required files exist, proceed to internal checks of the columns and data.
#' Lastly, perform minimal joins to determine missing values.
#'
#' dir_data and dir_results should be absolute and not relative paths.
#' The function `normalizePath` can be used to convert from relative to absolute path.
#'
#' @param out.dir directory where all rds files are stored
#' @param bio x
#' @param loaded x
#' @param useBC x
#' @param bioIndex x
#'
#' @return A list containing data_Sites and data_cluster to be used in the CASTool.
#'
#' @export
prepRespData <- function(out.dir,
                         bio,
                         loaded,
                         useBC,
                         bioIndex) {

  boo.debug <- FALSE
  if (boo.debug) {
    bio      <- "bmi"
    loaded   <- loaded
    useBC    <- useBC
    bioIndex <- bioIndexGp
  }
  
  # define pipe
  `%>%` <- dplyr::`%>%`

  if (bio == "bmi") {
    data_bioMetrics       <- readRDS(file.path(out.dir, "data_bmiMetrics.rds"))
    data_bioMetricsInfo   <- readRDS(file.path(out.dir, "data_bmiMetricsInfo.rds"))
    if (("data_bmiMasterTaxa" %in% loaded) && ("data_bmiCounts" %in% loaded)) {
      data_bioMasterTaxa  <- readRDS(file.path(out.dir, "data_bmiMasterTaxa.rds"))
      data_bioCounts      <- readRDS(file.path(out.dir, "data_bmiCounts.rds"))
    }
    if (useBC == TRUE) {
      data_BCdist         <- readRDS(file.path(out.dir, "data_BCdist.rds"))
    } else {
      data_BCdist         <- NULL
    }
  }
  if (bio == "alg") {
    data_bioMetrics       <- readRDS(file.path(out.dir, "data_algMetrics.rds"))
    data_bioMetricsInfo   <- readRDS(file.path(out.dir, "data_algMetricsInfo.rds"))
    if (("data_algMasterTaxa" %in% loaded) && ("data_algCounts" %in% loaded)) {
      data_bioMasterTaxa  <- readRDS(file.path(out.dir, "data_algMasterTaxa.rds"))
      data_bioCounts      <- readRDS(file.path(out.dir, "data_algCounts.rds"))
    }
  }
  if (bio == "fish") {
    data_bioMetrics       <- readRDS(file.path(out.dir, "data_fishMetrics.rds"))
    data_bioMetricsInfo   <- readRDS(file.path(out.dir, "data_fishMetricsInfo.rds"))
    if (("data_fishMasterTaxa" %in% loaded) && ("data_fishCounts" %in% loaded)) {
      data_bioMasterTaxa  <- readRDS(file.path(out.dir, "data_fishMasterTaxa.rds"))
      data_bioCounts      <- readRDS(file.path(out.dir, "data_fishCounts.rds"))
    }
  }

  # Get BIO metric data ----
  data_bioMetrics <- data_bioMetrics %>%
    dplyr::select_if(not_all_na) %>%
    dplyr::mutate(RespSampleDate = lubridate::parse_date_time(RespSampleDate,
                                      orders = c("ymd", "mdy", "dmy")) %>%
                    lubridate::date(),
                  RespSampleID = stringr::str_replace_all(RespSampleID, "[:punct:]", "_"),
                  StationID = stringr::str_replace_all(StationID, "[:punct:]", "_"))
  data_bioMetrics <- unique(data_bioMetrics)

  # Get BIO metric info ----
  # Add Quality (based on user-supplied data in list.bio.vars & data_bioMetricsInfo)
  data_bioMetricsInfo <- data_bioMetricsInfo %>%
    dplyr::filter(UseYN == "Y") %>%
    dplyr::select(MetricName, MetricLabel, IndexYN, TrendWIncStress,
                  CutoffValue, InclusiveIndicator)
  bioIndex.cutval <- data_bioMetricsInfo$CutoffValue[data_bioMetricsInfo$MetricName == bioIndex]
  bioIndex.dir <- data_bioMetricsInfo$TrendWIncStress[data_bioMetricsInfo$MetricName == bioIndex]
  bioIndex.Inc <- data_bioMetricsInfo$InclusiveIndicator[data_bioMetricsInfo$MetricName == bioIndex]

  ## Identify quality for primary index ----
  if (bioIndex.Inc == "≤" | bioIndex.Inc == "<=") {
    if (bioIndex.dir == "Dec") { # Lower values of the index represent more stress
      data_bioMetrics$Quality <- cut(data_bioMetrics[, bioIndex],
                                     breaks = c(-Inf, bioIndex.cutval, Inf),
                                     right = TRUE,
                                     labels = c("Degraded", "Not degraded"))
    } else { # Higher values of the index represent more stress
      data_bioMetrics$Quality <- cut(data_bioMetrics[, bioIndex],
                                     breaks = c(-Inf, bioIndex.cutval, Inf),
                                     right = TRUE,
                                     labels = c("Not degraded", "Degraded"))
    }
  } else if (bioIndex.Inc == "≥" | bioIndex.Inc == ">=") {
    if (bioIndex.dir == "Dec") { # Lower values of the index represent more stress
      data_bioMetrics$Quality <- cut(data_bioMetrics[, bioIndex],
                                     breaks = c(-Inf, bioIndex.cutval, Inf),
                                     right = FALSE,
                                     labels = c("Degraded", "Not degraded"))
    } else { # Higher values of the index represent more stress
      data_bioMetrics$Quality <- cut(data_bioMetrics[, bioIndex],
                                     breaks = c(-Inf, bioIndex.cutval, Inf),
                                     right = FALSE,
                                     labels = c("Not degraded", "Degraded"))
    }
  } else if (bioIndex.Inc == "<") {
    if (bioIndex.dir == "Dec") { # Lower values of the index represent more stress
      data_bioMetrics$Quality <- cut(data_bioMetrics[, bioIndex],
                                     breaks = c(-Inf, bioIndex.cutval, Inf),
                                     right = FALSE,
                                     labels = c("Degraded", "Not degraded"))
    } else { # Higher values of the index represent more stress
      data_bioMetrics$Quality <- cut(data_bioMetrics[, bioIndex],
                                     breaks = c(-Inf, bioIndex.cutval, Inf),
                                     right = FALSE,
                                     labels = c("Not degraded", "Degraded"))
    }
  } else { # (ssi.inclind == ">")
    if (bioIndex.dir == "Dec") { # Lower values of the index represent more stress
      data_bioMetrics$Quality <- cut(data_bioMetrics[, bioIndex],
                                     breaks = c(-Inf, bioIndex.cutval, Inf),
                                     right = TRUE,
                                     labels = c("Degraded", "Not degraded"))
    } else { # Higher values of the index represent more stress
      data_bioMetrics$Quality <- cut(data_bioMetrics[, bioIndex],
                                     breaks = c(-Inf, bioIndex.cutval, Inf),
                                     right = TRUE,
                                     labels = c("Not degraded", "Degraded"))
    }

  } # End cut statements

  # If loaded, get bioCounts ----
  if (exists("data_bioCounts")) {
    ## Get BIO count data ----
    data_bioCounts <- data_bioCounts %>%
      dplyr::mutate(RespSampleDate = lubridate::parse_date_time(RespSampleDate,
                                          orders = c("ymd", "mdy", "dmy")) %>%
                      lubridate::date(),
                    RespSampleID = stringr::str_replace_all(RespSampleID, "[:punct:]", "_"),
                    StationID = stringr::str_replace_all(StationID, "[:punct:]", "_"))

    if (calcRelAbund == TRUE) {     # Only write this column if needed
      data_bioCounts <- data_bioCounts %>%
        dplyr::group_by(RespSampleID, RespSampleDate) %>%
        dplyr::mutate(SampleTotAbund = sum(NumInd, na.rm = TRUE),
                         NumTaxa = dplyr::n(),
                         PctInd = round(NumInd / SampleTotAbund, 5),
                         PctTaxa = round(1 / NumTaxa, 5)) %>%
        dplyr::ungroup()
    } else {
      data_bioCounts <- data_bioCounts %>%
        dplyr::rename(PctInd = RelAbund) %>%
        dplyr::group_by(RespSampleID, RespSampleDate) %>%
        dplyr::mutate(NumTaxa = dplyr::n(),
                         PctTaxa = round(1 / SampleTotTaxa, 5)) %>%
        dplyr::ungroup()
    } # end if calcRelAbund == TRUE
    data_bioCounts <- dplyr::select(data_bioCounts, StationID, RespSampleID,
                                    RespSampleDate, TaxonID, NumInd, NumTaxa,
                                    PctInd, PctTaxa)
  }

  # Get Master Taxa file - Done above

  myRespData <- list(data_bioMetrics = data_bioMetrics,
                     data_bioMetricsInfo = data_bioMetricsInfo,
                     data_bioCounts = data_bioCounts,
                     data_bioMasterTaxa = data_bioMasterTaxa)
  return(myRespData)

}
