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
                              , df_model = NULL
                              , df_meas
                              , biocomm
                              , df_resp
                              , index
                              , lagdays = c(0, 0)
                              ) {##FUNCTION.START

  # Debug
  boo_DEBUG <- FALSE

  if  (boo_DEBUG == TRUE) {
    df_sites = data_Sites
    df_model = data_modelRaw
    df_meas = data_chemRaw
    biocomm = "BMI"
    df_resp = data_bmiMetrics
    index = bmiIndex
    lagdays = lagdays
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}
  biocomm <- tolower(biocomm)

  # Read data files (stressor and response)
  if (biocomm == "bmi") {
    df_resp <- df_resp[, c("StationID_Master", "BMISampDate", "BMISampID"
                          , "Quality", index, "BMISampFlag")] %>%
      dplyr::rename(RespSampDate = BMISampDate) %>%
      dplyr::rename(RespSampID = BMISampID) %>%
      dplyr::rename(RespSampFlag = BMISampFlag)
  } else if (biocomm == "alg") {
    df_resp <- df_resp[,c("StationID_Master", "AlgSampDate", "AlgSampID"
                          , "Quality", index, "AlgSampFlag")] %>%
      dplyr::rename(RespSampDate = AlgSampDate) %>%
      dplyr::rename(RespSampID = AlgSampID) %>%
      dplyr::rename(RespSampFlag = AlgSampFlag)
  } else if (biocomm == "fish") {
    df_resp <- df_resp[,c("StationID_Master", "FishSampDate", "FishSampID"
                          , "Quality", index, "FishSampFlag")] %>%
      dplyr::rename(RespSampDate = FishSampDate) %>%
      dplyr::rename(RespSampID = FishSampID) %>%
      dplyr::rename(RespSampFlag = FishSampFlag)
  } else {
    print("Biological community type not used.")
    flush.console()
  }

  # Clean up modeled data -- Changed tidyr::spread to newer tidyr::pivot_wider ARL 2023-05-25
  if (!is.null(df_model)) {
    df_model <- df_model %>%
      dplyr::select(StationID_Master, StdParamName, ResultValue) %>%
      tidyr::pivot_wider(names_from = StdParamName, values_from = ResultValue)
    modColnames <- names(df_model)
    modColnames <- modColnames[!(modColnames %in% "StationID_Master")]

    # Merge modeled stressor data and response data
    df_modresp <- merge(df_resp, df_model, by.x = "StationID_Master"
                        , by.y = "StationID_Master", all = TRUE)
    df_modresp <- df_modresp %>%
      dplyr::select(StationID_Master
                    , RespSampDate
                    , RespSampID
                    , Quality
                    , all_of(index)
                    , RespSampFlag
                    , all_of(modColnames))

    rm(df_model, df_resp)
    respColnames <- c("RespSampID", "Quality", index, "RespSampFlag")
  }

  # Clean up measured data and convert to wide format
  # Changed tidyr::spread to newer tidyr::pivot_wider ARL 2023-05-25
  df_meas <- as.data.frame(df_meas) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::select(StationID_Master, ChemSampleID, SampleDate
                  , StdParamName, ResultValue) %>%
    dplyr::group_by(StationID_Master, ChemSampleID, SampleDate
                    , StdParamName) %>%
    dplyr::summarise(meanResult = mean(ResultValue), .groups = "drop_last") %>%
    dplyr::rename(ResultValue = meanResult) %>%
    tidyr::pivot_wider(names_from = StdParamName, values_from = ResultValue) %>%
    dplyr::rename(StressSampDate = SampleDate)
  measColnames <- names(df_meas)
  measColnames <- measColnames[!(measColnames %in% c("StationID_Master"
                                                     , "ChemSampleID"
                                                     , "StressSampDate"))]

  # Merge site/bmi data with measured data by station & date
  if (exists("df_modresp")) {
    df_coOccur2 <- fuzzyjoin::fuzzy_left_join(df_modresp, df_meas
                                              , by = c("StationID_Master" = "StationID_Master"
                                                       , "RespSampDate" = "StressSampDate")
                                              , match_fun = list(`==`, function(x, y)
                                                (x - y >= 0 & x - y < lagdays[1]) | abs(x - y) <= lagdays[2])) %>%
      dplyr::filter(!is.na(StationID_Master.y)) %>%
      dplyr::rename(StationID_Master = StationID_Master.x) %>%
      dplyr::rename(StressSampID = ChemSampleID)

    # Select the minimum diffDays match only (avoids more than 1 match)
    df_coOccur3 <- unique(df_coOccur2) %>%
      dplyr::select(StationID_Master, StressSampDate, RespSampDate
                    , StressSampID) %>%
      dplyr::mutate(diff = as.numeric(RespSampDate - StressSampDate)) %>%
      dplyr::mutate(mindiff = min(abs(diff))) %>%
      dplyr::filter(mindiff == abs(diff)) %>%
      dplyr::distinct(StationID_Master, StressSampDate, RespSampDate, StressSampID)

    df_coOccur <- unique(merge(df_coOccur3, df_modresp
                               , by.x = c("StationID_Master", "RespSampDate")
                               , by.y = c("StationID_Master", "RespSampDate")
                               , all.x = TRUE))
    df_coOccur <- unique(merge(df_coOccur, df_meas
                               , by.x = c("StationID_Master", "StressSampDate", "StressSampID")
                               , by.y = c("StationID_Master", "StressSampDate", "ChemSampleID")
                               , all.x = TRUE))

  } else {
    df_coOccur2 <- fuzzyjoin::fuzzy_left_join(df_resp, df_meas
                                              , by = c("StationID_Master" = "StationID_Master"
                                                       , "RespSampDate" = "StressSampDate")
                                              , match_fun = list(`==`, function(x, y)
                                                (x - y >= 0 & x - y < lagdays[1]) | abs(x - y) <= lagdays[2])) %>%
      dplyr::filter(!is.na(StationID_Master.y)) %>%
      dplyr::rename(StationID_Master = StationID_Master.x) %>%
      dplyr::rename(StressSampID = ChemSampleID)

    # Select the minimum diffDays match only (avoids more than 1 match)
    df_coOccur3 <- unique(df_coOccur2) %>%
      dplyr::select(StationID_Master, StressSampDate, RespSampDate
                    , StressSampID) %>%
      dplyr::mutate(diff = as.numeric(RespSampDate - StressSampDate)) %>%
      dplyr::mutate(mindiff = min(abs(diff))) %>%
      dplyr::filter(mindiff == abs(diff)) %>%
      dplyr::distinct(StationID_Master, StressSampDate, RespSampDate, StressSampID)

    df_coOccur <- unique(merge(df_coOccur, df_meas
                               , by.x = c("StationID_Master", "StressSampDate", "StressSampID")
                               , by.y = c("StationID_Master", "StressSampDate", "ChemSampleID")
                               , all.x = TRUE))
  }

  df_coOccur$BioComm <- biocomm
  df_coOccur <- df_coOccur %>%
    # dplyr::mutate(BioComm = all_of(biocomm)) %>%
    dplyr::select(StationID_Master, StressSampDate, RespSampDate
                  , StressSampID, BioComm, all_of(respColnames)
                  , all_of(modColnames), all_of(measColnames)) %>%
    dplyr::select_if(not_all_na)

  rm(df_coOccur2, df_coOccur3)

  # Add field RespSampFlag, then rearrange field order
  # For San Diego data, RespSampFlag does appear in the dataframe
  if (!("RespSampFlag" %in% colnames(df_coOccur))) {
    df_coOccur$RespSampFlag <- NA
    df_coOccur <- df_coOccur[, c(1:8, ncol(df_coOccur), 9:(ncol(df_coOccur) - 1))]
  }

  if (!exists("df_sites$IncaseCol")){
    df_sites <- df_sites[, c("StationID_Master", "OutcaseCol")]
    df_coOccur <- merge(df_sites, df_coOccur)
  } else {
    df_sites <- df_sites[, c("StationID_Master", "OutcaseCol", "IncaseCol")]
    df_coOccur <- merge(df_sites, df_coOccur)
  }

  return(df_coOccur)

}







