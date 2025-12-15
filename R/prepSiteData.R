#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#  R version 4.4.3
#
#' @title prepSiteData
#'
#' @description Prepare site data for CASTool, adding ClusterID, if needed
#'
#' @details Retrieves saved site and cluster objects from their respective rds files.
#'
#' @param out.dir directory where all rds files are stored
#' @param outcaseLabel x
#' @param incaseColName x
#' @param useBC x
#' @param outcaseColName x
#'
#' @return A list containing data_Sites and data_cluster to be used in the CASTool.
#'
#' @keywords internal
#' @examples
#' # None at this time
#' @importFrom rlang .data
#' @export
#'
prepSiteData <- function(out.dir,
                         outcaseLabel = NULL,
                         incaseColName = NULL,
                         useBC = NULL,
                         outcaseColName = NULL) {

  # define pipe
  `%>%` <- dplyr::`%>%`

  data_Sites <- readRDS(file.path(out.dir, "data_Sites.rds"))
  data_cluster <- readRDS(file.path(out.dir, "data_cluster.rds"))
  data_Sites <- data_Sites %>%
    dplyr::mutate(StationID = stringr::str_replace_all(.data$StationID, "[:punct:]", "_"))

  if (!("ClusterID" %in% colnames(data_Sites))) {
    data_Sites <- merge(data_Sites, data_cluster, by = "COMID", all.x = TRUE)
  }

  # Rename or add OutcaseCol to sites file
  if (!is.na(outcaseColName)) {             # outside the case is defined
    if (outcaseColName %in% colnames(data_Sites)) {
      data_Sites <- dplyr::rename(data_Sites, OutcaseCol = dplyr::all_of(outcaseColName))
    } else {
      msg <- paste0("Replacing ", outcaseColName, " with 'OutcaseCol' having ",
                    "values equal to '", outcaseLabel, "'.")
      message(msg)
      data_Sites <- dplyr::mutate(data_Sites, OutcaseCol = outcaseColName)
    }
  } else {                                 # outside the case is not defined
    msg <- paste0("Adding 'OutcaseCol' column to site file with values equal to '",
                  outcaseLabel, "'.")
    message(msg)
    data_Sites <- dplyr::mutate(data_Sites, OutcaseCol = outcaseLabel)
    outcaseColName <- "OutcaseCol"
  }

  # Rename IncaseCol in sites file or send error message
  if (!is.na(incaseColName)) {
    data_Sites <- dplyr::rename(data_Sites, IncaseCol = dplyr::all_of(incaseColName))
  } else {
    if (useBC == TRUE) {
      msg <- paste0("Bray-Curtis dissimilarity distance matrix must be available.")
      message(msg)
    } else {
      msg <- paste0("Either incaseColName must be specified or useBC must be TRUE, ",
                    "and required files provided")
      message(msg)
      stop()
    }
  }

  # Create a vector of refSites
  refSites <- data_Sites$StationID[data_Sites$RefSiteFlag == 1]

  mySiteData <- list(sites = data_Sites,
                     cluster = data_cluster,
                     refSites = refSites)
  return(mySiteData)

}
