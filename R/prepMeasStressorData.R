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
#' @param in.dir x
#' @param out.dir directory where all rds files are stored
#' @param fn.data x
#' @param fn.meta x
#' @param removeOutliers x
#' @param sub.dir x
#'
#' @return A list containing data_Sites and data_cluster to be used in the CASTool.
#'
#' @examples
#' # None at this time 
#' @export
prepMeasStressorData <- function(in.dir,
                                 out.dir,
                                 fn.data,
                                 fn.meta,
                                 removeOutliers,
                                 sub.dir = "_Histograms") {
  # Global Bindings
  UseInStressorID <- StdParamName <- StationID <- StressSampleID <- 
    StressSampleDate <- TransfResult <- IQRmethod <- SDmethod <- Outlier <- NULL
                                                 
  # Debug
  boo.debug = TRUE
  if (boo.debug) {
    sub.dir = "_Histograms"
  }
  
  # define pipe
  `%>%` <- dplyr::`%>%`

  # Create output folder
  out.folders <- c(out.dir, sub.dir)

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      dir.path <- file.path(out.folders[i])
    } else {
      dir.path <- file.path(dir.path, out.folders[i])
    }
    if (dir.exists(dir.path) == FALSE) {
      dir.create(dir.path)
    }
  }

  out.dir <- dir.path


  # Load RDS files
  data_chemInfo   <- readRDS(file.path(in.dir, fn.meta))
  data_chemAll    <- readRDS(file.path(in.dir, fn.data))

  ## Get metadata for all measured stressors
  data_chemInfo   <- data_chemInfo %>%
    dplyr::filter(UseInStressorID == 1)

  ## Get measured stressor values to use in the stressor id
  params2use      <- as.character(data_chemInfo$StdParamName)
  data_chemAll    <- dplyr::filter(data_chemAll, StdParamName %in% params2use)

  ## getOutliers ----
  ## returns a dataframe with ChemSampleID, StdParamName, TransfResult,
  ## IQRmethod, SDmethod, Outlier
  data_measOutliers <- getOutliers(df_data   = data_chemAll,
                                   df_meta   = data_chemInfo,
                                   dir_plots = out.dir)
  ## Merge outlier flags with raw data by sample ID
  data_chemAll <- merge(data_chemAll, data_measOutliers,
                        by.x = c("StationID", "StressSampleID", "StressSampleDate",
                                 "StdParamName", "ResultValue"),
                        by.y = c("StationID", "StressSampleID", "StressSampleDate",
                                 "StdParamName", "ResultValue"),
                        all.x = TRUE)
  data_chemAll <- data_chemAll %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName,
                  TransfResult, IQRmethod, SDmethod, Outlier)
  # Clean up
  rm(data_measOutliers, params2use)

  ## Average duplicate data -- remove outliers first, if desired
  if (removeOutliers) {
    data_measoutliers <- data_chemAll %>%
      dplyr::filter(Outlier == "Outlier")
    data_chemAll <- data_chemAll %>%
      dplyr::filter(Outlier %in% c("Good", "NE"))
  }

  data_chemAll <- data_chemAll %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName,
                  TransfResult) %>%
    dplyr::mutate(StressSampleDate = lubridate::parse_date_time(StressSampleDate,
                                              orders = c("ymd", "mdy", "dmy")) %>%
                    lubridate::date(),
                  StressSampleID = stringr::str_replace_all(StressSampleID, "[:punct:]", "_"),
                  StationID = stringr::str_replace_all(StationID, "[:punct:]", "_")) %>%
    dplyr::group_by(StationID, StressSampleID, StressSampleDate, StdParamName) %>%
    dplyr::mutate(TransfResult = mean(TransfResult, na.rm = TRUE)) %>%
    dplyr::filter(!is.na(TransfResult))
  data_chemAll_dups <- data_chemAll[duplicated(data_chemAll),]
  data_chemAll <- unique(data_chemAll) # should be unique, long-form sample/analyte

  # Duplicate all pH values, one to use for alkaline environments (higher is better)
  # and one to use for acidic environments (lower is better)
  data_pHAcid <- data_chemAll %>%
    dplyr::filter(StdParamName == "pH") %>%
    dplyr::mutate(StdParamName = "pH_acidicEnv")
  data_chemAll <- data_chemAll %>%
    dplyr::mutate(StdParamName = ifelse(StdParamName == "pH", "pH_alkEnv",
                                        StdParamName))
  data_chemRaw <- rbind(data_chemAll, data_pHAcid)
  rm(data_pHAcid)

  # Adjust dates for samples collected on multiple dates with the same ID
  analytes <- unique(as.character(data_chemRaw$StdParamName))
  data_chemRaw <- as.data.frame(data_chemRaw) %>%
    dplyr::filter(!is.na(TransfResult)) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate,
                  StdParamName, TransfResult) %>%
    tidyr::pivot_wider(names_from = StdParamName,
                       values_from = TransfResult)
  data_chemRaw <- data_chemRaw %>%
    dplyr::group_by(StationID, StressSampleID) %>%
    dplyr::mutate(StressSampleDate = min(StressSampleDate)) %>%
    dplyr::ungroup()
  data_chemRaw <- data_chemRaw %>%
    tidyr::pivot_longer(cols = all_of(analytes),
                        names_to = "StdParamName",
                        values_to = "TransfResult")
  data_chemRaw <- dplyr::filter(data_chemRaw, !is.na(TransfResult))

  myChemData <- list(data_chemInfo = data_chemInfo,
                     data_chemRaw = data_chemRaw,
                     data_measoutliers = data_measoutliers)

  return(myChemData)

}
