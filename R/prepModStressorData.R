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
#' @export
prepModStressorData <- function(in.dir,
                                out.dir,
                                fn.data,
                                fn.meta,
                                removeOutliers,
                                sub.dir = "_Histograms") {
  # Global Bindings
  UseInStressorID <- StdParamName <- data_measOutliers <- StationID <- 
    StressSampleID <- StressSampleDate <- TransfResult <- IQRmethod <- 
    SDmethod <- Outlier <- data_model_all <- NULL
  
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
  data_modelInfo <- readRDS(file.path(in.dir, fn.meta))
  data_modelAll  <- readRDS(file.path(in.dir, fn.data))

  ## Get metadata for all measured stressors
  data_modelInfo   <- data_modelInfo %>%
    dplyr::filter(UseInStressorID == 1)

  ## Get measured stressor values to use in the stressor id
  params2use    <- as.character(data_modelInfo$StdParamName)
  data_modelAll <- dplyr::filter(data_modelAll, StdParamName %in% params2use)

  ## getOutliers returns a dataframe with modelSampleID, StdParamName, TransfResult,
  ## IQRmethod, SDmethod, Outlier
  data_modelOutliers <- getOutliers(df_data   = data_modelAll,
                                    df_meta   = data_modelInfo,
                                    dir_plots = out.dir)
  ## Merge outlier flags with raw data by sample ID
  data_modelAll <- merge(data_modelAll, data_measOutliers,
                        by.x = c("StationID", "StressSampleID", "StressSampleDate",
                                 "StdParamName", "ResultValue"),
                        by.y = c("StationID", "StressSampleID", "StressSampleDate",
                                 "StdParamName", "ResultValue"),
                        all.x = TRUE)
  data_modelAll <- data_modelAll %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName,
                  TransfResult, IQRmethod, SDmethod, Outlier)
  # Clean up
  rm(data_modelOutliers, params2use)

  ## Average duplicate data -- remove outliers first, if desired
  if (removeOutliers) {
    data_modeloutliers <- data_modelAll %>%
      dplyr::filter(Outlier == "Outlier")
    data_modelAll <- data_modelAll %>%
      dplyr::filter(Outlier %in% c("Good", "NE"))
  } else {
    data_modeloutliers <- NULL
  }

  data_modelAll <- data_modelAll %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName,
                  TransfResult) %>%
    dplyr::mutate(StressSampleID = stringr::str_replace_all(StressSampleID, "[:punct:]", "_"),
                  StationID = stringr::str_replace_all(StationID, "[:punct:]", "_")) %>%
    dplyr::group_by(StationID, StressSampleID, StressSampleDate, StdParamName) %>%
    dplyr::mutate(TransfResult = mean(TransfResult, na.rm = TRUE)) %>%
    dplyr::filter(!is.na(TransfResult))
  data_model_all_dups <- data_model_all[duplicated(data_modelAll),]
  data_modelAll <- unique(data_modelAll) # should be unique, long-form sample/analyte

  mymodelData <- list(data_modelInfo    = data_modelInfo,
                     data_modelRaw      = data_modelAll,
                     data_modeloutliers = data_modeloutliers)

  return(mymodelData)

}
