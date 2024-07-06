#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#  R version 4.3.1
#
#
#' @title Identifies Stressor Outliers
#'
#' @description Flags stressor outliers using two methods: 3 times IQR and
#'              6 times sd. If both suggest a given value as an outlier, the
#'              value is identified as an outlier.
#'
#' @details Generates faceted time sequence graphics (stressor/response, one
#' atop the other). All stressor/response data are graphed.
#' Improvements: Add scoring.
#'
#' Uses the library dplyr.
#'
#' @param df_data dataframe containing all stressor data values
#' @param df_meta dataframe containing stressor metadata
#'
#' @return Dataframe containing sample ID, stressor name, stressor value,
#'         IQR method flag, SD method flag, Outlier flag
#'
#' @keywords internal
#'
#' @export
getOutliers <- function(df_data, df_meta) {##FUNCTION.START

  # Debug
  boo_DEBUG <- FALSE
  boo_meas <- TRUE
  if (boo_DEBUG == TRUE) {
    if (boo_meas == TRUE) {
      df_data = data_chemRaw
      df_meta = data_chemInfo
    } else {
      df_data = data_modelRaw
      df_meta = data_modelInfo
    }
  }

  # Define pipe
  `%>%` <- dplyr::`%>%`
  # not_all_na <- function(x) {!all(is.na(x))}

  # Ensure uniqueness of df_meta
  df_meta <- df_meta %>%
    dplyr::select(StdParamName, LogTransf) %>%
    dplyr::group_by(StdParamName) %>%
    dplyr::summarise(LogTransf = max(LogTransf), .groups = "drop_last")

  # Merge data with metadata (note that some metadata may not be in the raw data)
  df_data <- merge(df_data, df_meta[,c("StdParamName","LogTransf")]
                   , by.x = "StdParamName", by.y = "StdParamName"
                   , all.x = TRUE)
  df_data <- df_data %>%
    dplyr::mutate(ResultValue = ifelse(ResultValue <= 0 & LogTransf == 1
                                       , NA, ResultValue)) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::mutate(TransfResult = ifelse(LogTransf == 1, log1p(ResultValue)
                                        , ResultValue))

  params <- unique(as.character(df_data$StdParamName))

  for (p in 1:length(params)) { # Iterate over parameters

    paramName <- params[p]

    df_sub <- df_data %>%  # Subset for just one parameter at a time
      dplyr::filter(StdParamName == paramName)

    # 3*IQR method for identifying outliers
    iqr <- stats::quantile(df_sub$TransfResult, probs = c(0.25, 0.75)
                           , na.rm = TRUE)
    iqr1.5 <- 1.5 * stats::IQR(df_sub$TransfResult, na.rm = TRUE)
    outlowlim <- iqr[1] - iqr1.5
    outhilim <- iqr[2] + iqr1.5

    df_sub <- df_sub %>%
      dplyr::mutate(IQRmethod = ifelse((TransfResult < outlowlim)
                                       , "Outlier low"
                                       , ifelse((TransfResult > outhilim)
                                                , "Outlier high", "Good")))

    # 6*sd method for identifying outliers
    paramMean <- mean(df_sub$TransfResult[is.finite(df_sub$TransfResult)]
                      , na.rm = TRUE)
    paramSD <- stats::sd(df_sub$TransfResult[is.finite(df_sub$TransfResult)]
                  , na.rm = TRUE)
    df_sub <- df_sub %>%
      dplyr::mutate(SDmethod = ifelse((abs(TransfResult - paramMean) >
                                         (6 * paramSD)), "Outlier", "Good"))

    # Combine the findings
    # If both IQR & SD methods agree on "Good", then mark as "Good"
    # If either IQR method OR SD method aren't evaluated, then mark as "NE"
    # If either IQR method or SD method suggest an outlier, then mark as "Outlier"
    df_sub <- df_sub %>%
      dplyr::mutate(IQRmethod = ifelse(!is.na(IQRmethod), IQRmethod, "NE")
                    , SDmethod = ifelse(!is.na(SDmethod), SDmethod, "NE")) %>%
      dplyr::mutate(Outlier = case_when((IQRmethod == "Good") & (SDmethod == "Good") ~ "Good"
                                        , (IQRmethod == "NE") | (SDmethod == "NE") ~ "NE"
                                        , grepl("Outlier", IQRmethod) & (SDmethod == "Outlier") ~ "Outlier"
                                        , TRUE ~ "Good"))

    if (p == 1) {
      df_temp <- df_sub
    } else {
      df_temp <- rbind(df_temp, df_sub)
    }

  } # End parameter iteration

  df_temp <- dplyr::select(df_temp, ChemSampleID, StdParamName, ResultValue
                           , IQRmethod, SDmethod, Outlier)

  # Return what?
  myOutliers <- df_temp

}
