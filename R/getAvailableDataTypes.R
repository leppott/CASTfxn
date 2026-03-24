#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
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
# @param measStressSamps boolean indicating if measured stressor data are expected..
# @param modStressSamps boolean indicating if modeled stressor data are expected.
#' @param df_stress
#' @param biocommlist vector of biocommunity data expected.
#' @param dir_results Directory containing all results.
#'
#' @return A list containing five boolean values 1) useBMI, 2) useAlg, 3) useFish,
#'         4) noStressors, 5) noResponses, and 6) siteDetectsAll.
#'
#' @keywords internal
#' @examples
#' # None at this time
#' @export
getAvailableDataTypes <- function(TargetSiteID,
                                  df_SampSummary,
                                  df_stress,
                                  biocommlist,
                                  dir_results) {##FUNCTION.START

  # Global Bindings
  data_sampSummary <- data_Stress <- StationID <- StdParamName <-
    TransfResult <- NULL

  boo.DEBUG <- FALSE

  if(boo.DEBUG) {
    TargetSiteID <- TargetSiteID
    df_SampSummary <- dplyr::mutate(data_sampSummary, ModeledSampleID = NA)
    df_stress <- data_Stress
    biocommlist <- biocommlist
    dir_results <- dir_results
  }

  # define pipe and not_all_na function
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}

  # Check for directory, if not existing, create
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID)) == TRUE,
         dir.create(file.path(dir_results, TargetSiteID)),
         FALSE)

  # Initialize gaps df
  df_gap <- data.frame(fxnname = character(), condition = character(), result = character(), comment = character())

  # ID stressor samples ----
  avail.data <- df_SampSummary %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::select(dplyr::ends_with("SampleID")) %>%
    dplyr::select_if(not_all_na)
  samptypes <- names(avail.data)

  core.resp.colnames <- c("StationID", "COMID", "OutcaseCol", "IncaseCol",
                          "SampleDate", "BMISampleID", "AlgSampleID", "FishSampleID")

  availStressSamps <- setdiff(samptypes, core.resp.colnames)
  missingStressSamps <- setdiff(colnames(df_SampSummary), availStressSamps)
  missingStressSamps <- setdiff(missingStressSamps, core.resp.colnames)

  if (length(missingStressSamps) > 0) {
    # Write missing categories stressors as data gaps (if any)
    for (m in seq_along(missingStressSamps)) {
      missing <- missingStressSamps[m]

      gap.statement <- data.frame(
        fxnname = "getAvailableDataTypes",
        condition = "Missing category of target site stressor data",
        result = missing,
        comment = paste0("No ", missing, " samples available for ", TargetSiteID)
      )

      df_gap <- df_gap |> dplyr::bind_rows(gap.statement)
    }
  }

  if (length(availStressSamps) == 0) {
    noStressors <- TRUE
    siteDetectsAll <- NULL
  } else {
    noStressors <- FALSE
    # Prepare data sets of all stressors ever detected at the target site

    siteDetectsAll <- df_stress |>
      dplyr::filter(StationID == TargetSiteID) |>
      dplyr::filter(is.na(TransfResult) == FALSE) |>
      dplyr::distinct(StdParamName) |>
      dplyr::pull(StdParamName)

    if(length(siteDetectsAll) == 0){
      # LCN removed 3/23/26 duplicative of gap message generated in skeleton code

      # msg <- paste("No stressor observations identified for", TargetSiteID)
      # message(msg)
      #
      # gap.statement <- data.frame(
      #   fxnname = "getAvailableDataTypes",
      #   condition = "Number of stressor observations",
      #   result = "0",
      #   comment = msg
      # )
      #
      # df_gap <- df_gap |> dplyr::bind_rows(gap.statement)

      noStressors <- TRUE
    }

    # # This syntax doesn't work because the length(siteDetectsAll) == 0 condition will never happen because the colnames will include ID columns from pivoted data frame
    # siteStressAll <- df_stress %>%
    #   dplyr::filter(StationID == TargetSiteID) %>%
    #   tidyr::pivot_wider(names_from = StdParamName,
    #                      values_from = TransfResult) %>%
    #   dplyr::select_if(not_all_na)
    #
    # siteDetectsAll <- as.vector(colnames(siteStressAll))
    #
    # if (length(siteDetectsAll) != 0) {
    #   siteDetectsAll <- siteDetectsAll[!(siteDetectsAll %in%
    #                                        c("StationID", "StressSampleID",
    #                                          "StressSampleDate"))]
    # } else {
    #   msg <- paste("No detected stressors identified for", TargetSiteID)
    #   message(msg)
    #
    #   gaps.stress <- cbind.data.frame("getAvailData", "Number of detects", 0, msg)
    #   colnames(gaps.stress) <- c("fxnname", "condition", "result", "comment")
    #   gaps <- rbind(gaps, gaps.stress)

    # }
  }

  # ID response samples ----
  for (b in seq_along(biocommlist)) {
    bio <- tolower(biocommlist[b])
    if (bio == "bmi") {
      if ("BMISampleID" %in% samptypes) {
        useBMI <- TRUE
      } else {   # BMI data are included, but this site does not have BMI data
        useBMI <- FALSE

        gap.statement <- data.frame(
          fxnname = "getAvailableDataTypes",
          condition = "BMISampleID",
          result = "0",
          comment = paste0("No benthic macroinvertebrate samples available at ", TargetSiteID)
        )

        df_gap <- df_gap |> dplyr::bind_rows(gap.statement)

      }
    }
    if (bio == "alg") {
      if ("AlgSampleID" %in% samptypes) {
        useAlg <- TRUE
      } else {   # Algal data are included, but this site does not have algal data
        useAlg <- FALSE

        gap.statement <- data.frame(
          fxnname = "getAvailableDataTypes",
          condition = "AlgSampleID",
          result = "0",
          comment = paste0("No algae samples available at ", TargetSiteID)
        )

        df_gap <- df_gap |> dplyr::bind_rows(gap.statement)

      }
    }
    if (bio == "fish") {
      if ("FishSampleID" %in% samptypes) {
        useFish <- TRUE
      } else {   # Fish data are included, but this site does not have fish data
        useFish <- FALSE

        gap.statement <- data.frame(
          fxnname = "getAvailableDataTypes",
          condition = "FishSampleID",
          result = "0",
          comment = paste0("No fish samples available at ", TargetSiteID)
        )

        df_gap <- df_gap |> dplyr::bind_rows(gap.statement)

      }
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

  myAvailData <- list(useBMI = useBMI,
                      useAlg = useAlg,
                      useFish = useFish,
                      noStressors = noStressors,
                      noResponses = noResponses,
                      siteDetectsAll = siteDetectsAll,
                      df_gap = df_gap)

  return(myAvailData)

}
