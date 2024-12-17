#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Get Matching Stressor-Response Data
#'
#' @description Prepare matched stressor/response data as a single dataframe
#'
#' @details Generates a dataframe and writes a table with stressor and response
#' data for all stressor and response samples obtained within a specified time
#' of each other. User specified lag time between stressor sample (prior) and
#' response sample (after) may be employed, with a default of same day samples.
#'
#' Improvements: Add BioDegLab and BioNarLab?
#'
#' Uses the libraries dplyr and tidyr.
#'
#' @param df_sites Dataframe containing site data for all sites.
#' @param df_model Dataframe containing modeled stressor data for all sites that
#' have them. Default = NULL.
#' @param df_meas Dataframe containing measured stressor data for all sites that
#' have them.
#' @param biocomm Biological community; algae or BMI.
#' @param df_resp Dataframe containing biological response metrics.
#' @param index Name of the response index column.
#' @param lagdays c(before, after). The number of days allowed between the
#' stressor sample date and the response sample date, before and after.
#' Default = c(0, 0) (same day).
#'
#' @return A dataframe containing matched stressor response data based on
#' a stressor sample obtained between lagdays before and lagdays after the
#' response sample was obtained.
#'
#' @keywords internal
#'
#' @export
getCoOccurDataset <- function(df_sites
                              , df_stress
                              , biocomm
                              , df_resp
                              , index
                              , lagdays = c(0, 0)
                              ) {##FUNCTION.START

  # Debug
  boo_DEBUG <- FALSE

  if  (boo_DEBUG == TRUE) {
    df_sites = data_Sites
    df_stress = data_Stress
    biocomm = "BMI"
    df_resp = data_bmiMetrics
    index = bmiIndex
    lagdays = lagdays
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}
  biocomm <- tolower(biocomm)

  df_model <- dplyr::filter(df_stress, is.na(StressSampleDate))
  df_meas <- dplyr::filter(df_stress, !is.na(StressSampleDate))

  # Read data files (stressor and response)
  if (biocomm == "bmi") {
    df_resp <- df_resp[, c("StationID", "RespSampleDate", "RespSampleID"
                          , index, "Quality", "BMISampFlag")] %>%
      dplyr::rename(RespSampFlag = BMISampFlag)
  } else if (biocomm == "alg") {
    df_resp <- df_resp[,c("StationID", "RespSampleDate", "RespSampleID"
                          , index, "Quality", "AlgSampFlag")] %>%
      dplyr::rename(RespSampFlag = AlgSampFlag)
  } else if (biocomm == "fish") {
    df_resp <- df_resp[,c("StationID", "RespSampleDate", "RespSampleID"
                          , index, "Quality", "FishSampFlag")] %>%
      dplyr::rename(RespSampFlag = FishSampFlag)
  } else {
    print("Biological community type not used.")
    flush.console()
  }

  # Clean up modeled data and convert to wide format ----
  # Changed tidyr::spread to newer tidyr::pivot_wider ARL 2023-05-25
  # TODO: Select TransfResult or ResultValue?
  if (nrow(df_model) > 0) {
    df_model <- df_model %>%
      dplyr::select(StationID, StdParamName, ResultValue) %>%
      tidyr::pivot_wider(names_from = StdParamName, values_from = ResultValue)
    modColnames <- names(df_model)
    modColnames <- modColnames[!(modColnames %in% "StationID")]

    # Merge modeled stressor data and response data (date not required)
    df_modresp <- merge(df_resp, df_model, by.x = "StationID"
                        , by.y = "StationID", all = TRUE)
    df_modresp <- df_modresp %>%
      dplyr::select(StationID
                    , RespSampleDate
                    , RespSampleID
                    , Quality
                    , all_of(index)
                    , RespSampleFlag
                    , all_of(modColnames))

    rm(df_model, df_resp)
    respColnames <- c("RespSampID", index, "Quality", "RespSampFlag")
  }

  # Clean up measured data and convert to wide format ----
  # Changed tidyr::spread to newer tidyr::pivot_wider ARL 2023-05-25
  # TODO: Select TransfResult or ResultValue?
  if (nrow(df_meas) > 0) {
    df_meas <- as.data.frame(df_meas) %>%
      dplyr::filter(!is.na(ResultValue)) %>%
      dplyr::select(StationID, StressSampleID, StressSampleDate
                    , StdParamName, ResultValue) %>%
      dplyr::group_by(StationID, StressSampleID, StressSampleDate
                      , StdParamName) %>%
      dplyr::summarise(meanResult = mean(ResultValue, na.rm = TRUE)
                       , .groups = "drop_last") %>%
      dplyr::rename(ResultValue = meanResult) %>%
      tidyr::pivot_wider(names_from = StdParamName, values_from = ResultValue)
    measColnames <- names(df_meas)
    measColnames <- measColnames[!(measColnames %in% c("StationID"
                                                       , "StressSampleID"
                                                       , "StressSampleDate"))]
  }

  # Merge site/bmi data with measured data by station & date
  if (exists("df_modresp") & exists("df_meas")) {
    df_coOccur2 <- fuzzyjoin::fuzzy_left_join(df_modresp, df_meas
                                              , by = c("StationID" = "StationID"
                                                       , "RespSampleDate" = "StressSampleDate")
                                              , match_fun = list(`==`, function(x, y)
                                                (x - y >= 0 & x - y <= lagdays[1]) |
                                                  abs(x - y) <= lagdays[2])) %>%
      dplyr::filter(!is.na(StationID.y)) %>%
      dplyr::rename(StationID = StationID.x)

    # Select the minimum diffDays match only (avoids more than 1 match)
    df_coOccur3 <- unique(df_coOccur2) %>%
      dplyr::select(StationID, StressSampleDate, RespSampleDate
                    , StressSampleID) %>%
      dplyr::mutate(diff = as.numeric(RespSampleDate - StressSampleDate)) %>%
      dplyr::mutate(mindiff = min(abs(diff))) %>%
      dplyr::filter(mindiff == abs(diff)) %>%
      dplyr::distinct(StationID, StressSampleDate, RespSampleDate, StressSampleID)

    df_coOccur <- unique(merge(df_coOccur3, df_modresp
                               , by = c("StationID", "RespSampleDate")
                               # , by.y = c("StationID", "RespSampleDate")
                               , all.x = TRUE))
    df_coOccur <- unique(merge(df_coOccur, df_meas
                               , by = c("StationID", "StressSampleDate", "StressSampleID")
                               # , by.y = c("StationID", "StressSampleDate", "ChemSampleID")
                               , all.x = TRUE))
    rm(df_coOccur2, df_coOccur3)
  } else if (exists("df_meas")) {
    df_coOccur2 <- fuzzyjoin::fuzzy_left_join(df_resp, df_meas
                                              , by = c("StationID" = "StationID"
                                                       , "RespSampleDate" = "StressSampleDate")
                                              , match_fun = list(`==`, function(x, y)
                                                (x - y >= 0 & x - y <= lagdays[1]) |
                                                  abs(x - y) <= lagdays[2])) %>%
      dplyr::filter(!is.na(StationID.y)) %>%
      dplyr::rename(StationID = StationID.x)
    # Select the minimum diffDays match only (avoids more than 1 match)
    df_coOccur <- df_coOccur2 %>%
      dplyr::mutate(diff = as.numeric(RespSampleDate - StressSampleDate)) %>%
      dplyr::mutate(mindiff = min(abs(diff))) %>%
      dplyr::filter(mindiff == abs(diff)) %>%
      dplyr::select(!c(`StationID.y`, diff, mindiff))
    rm(df_coOccur2)
  } else {
    df_coOccur <- df_modresp
  }

  df_coOccur$BioComm <- biocomm
  if(exists("modColnames") & exists("measColnames")) {
    df_coOccur <- df_coOccur %>%
      dplyr::select(StationID, StressSampleDate, RespSampleDate, StressSampleID
                    , RespSampleID, BioComm, all_of(index), Quality, all_of(modColnames)
                    , all_of(measColnames)) %>%
      dplyr::select_if(not_all_na)
  } else if (exists("measColnames")) {
    df_coOccur <- df_coOccur %>%
      dplyr::select(StationID, StressSampleDate, RespSampleDate, StressSampleID
                    , RespSampleID, BioComm, all_of(index), Quality
                    , all_of(measColnames)) %>%
      dplyr::select_if(not_all_na)
  } else {
    df_coOccur <- df_coOccur %>%
      dplyr::select(StationID, StressSampleDate, RespSampleDate, StressSampleID
                    , RespSampleID, BioComm, all_of(index), Quality, all_of(modColnames)) %>%
      dplyr::select_if(not_all_na)
  }

  # Add field RespSampFlag, then rearrange field order
  # For San Diego data, RespSampFlag does appear in the dataframe
  # if (!("RespSampFlag" %in% colnames(df_coOccur))) {
  #   df_coOccur$RespSampFlag <- NA
  #   df_coOccur <- df_coOccur[, c(1:8, ncol(df_coOccur), 9:(ncol(df_coOccur) - 1))]
  # }

  df_sites <- dplyr::select(df_sites, StationID, ends_with("caseCol"))
  df_coOccur <- merge(df_sites, df_coOccur)

  return(df_coOccur)

}







