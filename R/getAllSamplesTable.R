#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Sample Summary
#'
#' @description Get summary of all stressor and response samples obtained sorted
#'              by site and date.
#'
#' @details Summarizes all sites, reaches, outside-the-case IDs, inside-the-case IDs,
#' sample dates, and sample identifiers collected at the site on those dates,
#' organized by type
#'
#' Requires packages dplyr, tidyr
#'
#' Required objects:
#'
#' * data_Sites containing minimally these columns:
#'   StationID, COMID, OutcaseCol, IncaseCol
#'
#' * data_Stress containing minimally these columns:
#'   StationID, StressSampleID, StressSampleDate, StdParamName
#'
#' * data_respTrim containing minimally these columns:
#'   StationID, RespSampleID, RespSampleDate, biocomm
#'
#' * out.dir: the directory to which to write the final all sites summary;
#'   the same as the data input directory, ideally, although may be elsewhere
#'
#' @param df.stress dataframe containing stressor data for all sites, including
#'                  measured or observed quantitative water chemistry and quality,
#'                  physical habitat data, and modeled data.
#' @param df.stressInfo dataframe containing stressor metadata, including stressor
#'                      names, labels, whether the stressor should be evaluated, if
#'                      it should be log-transformed, and whether or not there are
#'                      associated stressor-specific tolerance values or indices.
#' @param df.resp dataframe containing all response samples obtained from a site,
#'                including multiple biological communities, if sampled.
#' @param df.sites dataframe containing StationID, COMID, IncaseCol, and OutcaseCol
#'
#' @return A dataframe of site sample information sorted by StationID and SampleDate
#'         (both ascending), along with COMID, OutcaseCol, IncaseCol, and identifiers
#'         for samples collected on the date specified, with columns for chemistry
#'         data, physical habitat data, modeled data, benthic macroinvertebrate
#'         response data, algal response data, and fish response data. If any
#'         specific category of data aren't available (e.g., fish) those columns
#'         are omitted.
#'
#' @examples
#' # None at this time 
#' @export
getAllSamplesTable <- function(df.stress,
                               df.stressInfo,
                               df.resp,
                               df.sites) {

  boo.debug = FALSE

  if (boo.debug) {
    df.stress <- data_Stress
    df.stressInfo <- data_stressInfo
    df.resp <- data_respTrim
    df.sites <- data_Sites
  }

  # define pipe
  `%>%` <- dplyr::`%>%`

  # Prepare stressor data
  # Identify dated stressor types
  df.meas <- dplyr::filter(df.stress, !is.na(StressSampleDate))
  # Identify undated stressor types
  df.model <- dplyr::filter(df.stress, is.na(StressSampleDate))

  # Obtain dated stressor sample IDs ----
  if (nrow(df.meas) > 0) {
    df.sampSummary <- unique(df.meas[, c("StationID", "StressSampleID",
                                         "StdParamName", "StressSampleDate")])
    df.sampSummary <- merge(df.sampSummary, df.stressInfo, by = "StdParamName")
    df.sampSummary <- df.sampSummary %>%
      dplyr::select(StationID, StressSampleID, SourceGroup, StdParamName,
                    StressSampleDate, Label) %>%
      dplyr::mutate(Type = paste0(gsub(" ", "", stringr::str_to_title(SourceGroup)),
                                  "SampleID"))
    df.sampSummary <- unique(df.sampSummary[, c("StationID", "StressSampleID",
                                                "StressSampleDate", "Type")])
    df.sampSummary <- df.sampSummary %>%
      tidyr::pivot_wider(id_cols = c(StationID, StressSampleDate), names_from = Type,
                         values_from = StressSampleID, values_fill = NA)
    chemsamptypes <- colnames(dplyr::select(df.sampSummary, dplyr::ends_with("SampleID")))
  }

  # Identify response samples ----
  df.resp <- df.resp %>%
    tidyr::pivot_wider(id_cols = c(StationID, RespSampleDate), names_from = biocomm,
                       values_from = RespSampleID, values_fill = NA)
  df.resp <- unique(df.resp)
  respsamptypes <- colnames(dplyr::select(df.resp, dplyr::ends_with("SampleID")))

  # Combine with response data types
  df.sampSummary <- merge(df.sampSummary, df.resp,
                          by.x = c("StationID", "StressSampleDate"),
                          by.y = c("StationID", "RespSampleDate"),
                          all = TRUE)
  rm(df.resp)

  # Add undated stressor data ----
  if (nrow(df.model) > 0) {
    df.modelTrim <- as.data.frame(df.model) %>%
      dplyr::distinct(StationID, StressSampleID) %>%
      dplyr::rename(ModeledSampleID = StressSampleID)

    df.sampSummary <- merge(df.sampSummary, df.modelTrim, by = "StationID",
                            all = TRUE)
    df.sampSummary <- unique(df.sampSummary)

    df.sampSummary <- df.sampSummary %>%
      dplyr::rename(SampleDate = StressSampleDate) %>%
      dplyr::select(StationID, SampleDate, all_of(chemsamptypes),
                    ModeledSampleID, all_of(respsamptypes))
  } else {
    df.sampSummary <- df.sampSummary %>%
      dplyr::rename(SampleDate = StressSampleDate) %>%
      dplyr::select(StationID, SampleDate, all_of(chemsamptypes),
                    all_of(respsamptypes))
  }

  if (is.na(incaseColName)) {
    df.sampSummary <- merge(df.sites[, c("StationID", "COMID", "OutcaseCol")],
                            df.sampSummary, by = "StationID", all.y = TRUE)
    df.sampSummary <- unique(df.sampSummary)
  } else {
    df.sampSummary <- merge(df.sites[, c("StationID", "COMID", "OutcaseCol",
                                         "IncaseCol")], df.sampSummary,
                            by = "StationID", all.y = TRUE)
    df.sampSummary <- unique(df.sampSummary)
  }

  return(df.sampSummary)

}
