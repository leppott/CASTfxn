#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#' @title Get Available Data
#'
#' @description Identifies which data are available for the target site.
#'
#' @details Using the entire possible set of data types, this function determines
#' which sample types are available for the target site based on the summary
#' sample data. Sample ID column names are required to end with "SampleID".
#'
#' Uses the library dplyr
#'
#' @param TargetSiteID Site ID for the site being evaluated
#' @param df_SampSummary dataframe containing sample IDs for samples collected
#'                       at the target site, organized by sample date (rows) and
#'                       type (columns)
#' @param measStressSamps boolean indicating if measured stressor data are expected..
#' @param modStressSamps boolean indicating if modeled stressor data are expected.
#' @param biocommlist vector of biocommunity data expected.
#' @param dir_results Directory containing all results. Default is file.path(getwd(),"Results")
#'
#' @return A list containing five boolean values 1) useBMI, 2) useAlg, 3) useFish,
#'         4) noStressors, and 5) noResponses.
#'
#' @keywords internal
#'
#' @export
getAvailableDataTypes <- function(TargetSiteID
                                  , df_SampSummary
                                  , measStressSamps
                                  , modStressSamps = NULL
                                  , biocommlist = biocommlist
                                  , dir_results = file.path(getwd(), "Results")
                                  ) {##FUNCTION.START

  boo.DEBUG <- FALSE

  if(boo.DEBUG) {
    TargetSiteID = TargetSiteID
    df_SampSummary = data_sampSummary
    measStressSamps = measStressData
    modStressSamps = modelStressData
    biocommlist = biocommlist
    dir_results = dir_results
  }

  # define pipe and not_all_na function
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}

  # Check for directory, if not existing, create
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID)) == TRUE
         , dir.create(file.path(dir_results, TargetSiteID))
         , FALSE)

  fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
  fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
  gapsCols <- c("fxnname", "condition", "result", "comment")
  gaps <- as.data.frame(matrix(ncol = 4, nrow = 0, dimnames = list(NULL, gapsCols)))

  avail.data <- data_sampSummary %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::select(ends_with("SampleID")) %>%
    dplyr::select_if(not_all_na)
  samptypes <- names(avail.data)

  if (measStressSamps & modStressSamps) {
    stressSamps <- c("ChemistrySampleID", "FieldSampleID", "HabitatSampleID"
                     , "ModeledSampleID")
  } else if (measStressSamps) {
    stressSamps <- c("ChemistrySampleID", "FieldSampleID", "HabitatSampleID")
  } else {
    stressSamps <- "ModeledSampleID"
  }

  availStressSamps <- intersect(samptypes, stressSamps)
  missingStressSamps <- setdiff(stressSamps, samptypes)

  if (length(availStressSamps) == 0) {
    noStressors <-TRUE
  } else {
    noStressors <- FALSE
    for (m in seq_along(missingStressSamps))
      missing <- missingStressSamps[m]
      gap.stress <- cbind.data.frame("general", missing, 0
                                          , "No data available.")
      colnames(gap.stress) <- c("fxnname", "condition", "result", "comment")
      gaps <- rbind(gaps, gap.stress)
      rm(gap.stress)
  }

  for (b in seq_along(biocommlist)) {
    bio <- tolower(biocommlist[b])
    if (bio == "bmi") {
      if ("BMISampleID" %in% samptypes) {
        useBMI <- TRUE
      } else {   # BMI data are included, but this site does not have BMI data
        useBMI <- FALSE
        gap.bmi.rsp <- rbind(cbind("general", "BMISampleID", 0, "No data available."))
        colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
        gaps <- rbind(gaps, gap.bmi.rsp)
        rm(gap.bmi.rsp)
      }
    }
    if (bio == "alg") {
      if ("AlgSampleID" %in% samptypes) {
        useAlg <- TRUE
      } else {   # Algal data are included, but this site does not have algal data
        useAlg <- FALSE
        gap.alg.rsp <- cbind.data.frame("general", "AlgSampleID", 0, "No data available.")
        colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
        gaps <- rbind(gaps, gap.alg.rsp)
        rm(gap.alg.rsp)
      }
    }
    if (bio == "fish") {
      if ("FishSampleID" %in% samptypes) {
        useFish <- TRUE
      } else {   # Fish data are included, but this site does not have fish data
        useFish <- FALSE
        gap.fish.rsp <- cbind.data.frame("general", "FishSampleID", 0, "No data available.")
        colnames(gap.fish.rsp) <- c("fxnname", "condition", "result", "comment")
        gaps <- rbind(gaps, gap.fish.rsp)
        rm(gap.fish.rsp)
      }
    }
  }

  if (nrow(gaps) > 0) {
    if (file.exists(fn.gaps)) {
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
    } else {
      write.table(gaps, fn.gaps, append = FALSE, col.names = TRUE
                  , row.names = FALSE, sep = "\t")
    }
  }

  if (!exists("useBMI")) {
    useBMI <- FALSE
  }
  if (!exists("useAlg")) {
    useAlg <- FALSE
  }
  if (!exists("useFish")) {
    useFish <- FALSE
  }
  if (!useBMI & !useAlg & !useFish) {
    noResponses <- TRUE
  } else {
    noResponses <- FALSE
  }

  myAvailData <- list(useBMI = useBMI
                      , useAlg = useAlg
                      , useFish = useFish
                      , noStressors = noStressors
                      , noResponses = noResponses)

  return(myAvailData)

}
