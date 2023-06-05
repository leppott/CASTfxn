#  Copyright 2020 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Time Sequence Graphics
#'
#' @description Graph time-specific stressor-response values.
#'
#' @details Generates faceted time sequence graphics (stressor/response, one
#' atop the other). All stressor/response data are graphed.
#' Improvements: Add scoring.
#'
#' Uses the libraries dplyr, tidyr, ggplot2, and ggrepel.
#'
#' @param dataDir Directory containing all data
#' @param dir_results Directory containing all results. Default = "file.path(getwd(),"Results")"
#' @param stressors stressors
#' @param biocomm Biological community; algae or BMI. Default = "BMI".
#' @param BioResp Biological response variable names. For example, BMI metrics
#' or Algae metrics.
#' @param df_stress Stressor values.
#' @param df_resp Response values for the specified biological community and metrics.
#' @param colname.SampID Name of the column for the response sample identifier.
#'
#' @return One or more jpgs in SiteID/TemporalSequence/Biocomm subfolder of the
#'        "Results" folder of working directory. No scores are currently generated.
#'
#' @keywords internal
#'
#' @export
getOutliers <- function(df_data, df_meta) {

  # Debug
  boo_DEBUG <- FALSE
  if (boo_DEBUG == TRUE) {
    df_data = data_chemRaw
    df_meta = data_chemInfo
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

  # LogTransform data that need to be transformed, removing negative values (invalid)
  df_data2Transf <- df_data %>%
    dplyr::filter(LogTransf == 1) %>%
    dplyr::mutate(ResultValue = ifelse(ResultValue <= 0, NA, ResultValue)) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::mutate(TransfResult = log10(ResultValue))

  df_dataNoTransf <- df_data %>%
    dplyr::filter(LogTransf == 0) %>%
    dplyr::mutate(TransfResult = ResultValue)

  df_data <- rbind(df_data2Transf, df_dataNoTransf)

  params <- unique(as.character(df_data$StdParamName))

  for (p in 1:length(params)) { # Iterate over parameters

    paramName <- params[p]

    df_sub <- df_data %>%  # Subset for just one parameter at a time
      dplyr::filter(StdParamName == paramName)

    # 3*IQR method for identifying outliers
    iqr <- quantile(df_sub$TransfResult, probs=c(0.25,0.75), na.rm = TRUE)
    iqr1.5 <- 1.5*IQR(df_sub$TransfResult, na.rm = TRUE)
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
    paramSD <- sd(df_sub$TransfResult[is.finite(df_sub$TransfResult)]
                  , na.rm = TRUE)
    df_sub <- df_sub %>%
      dplyr::mutate(SDmethod = ifelse((abs(TransfResult - paramMean) >
                                         (6*paramSD)), "Outlier", "Good"))

    # Combine the findings
    # If both IQR & SD methods agree on "Good", then mark as "Good"
    # If either IQR method OR SD method aren't evaluated, then mark as "NE"
    # If either IQR method or SD method suggest an outlier, then mark as "Outlier"
    df_sub <- df_sub %>%
      dplyr::mutate(IQRmethod = ifelse(!is.na(IQRmethod), IQRmethod, "NE")
                    , SDmethod = ifelse(!is.na(SDmethod), SDmethod, "NE")) %>%
      dplyr::mutate(Outlier = case_when((IQRmethod=="Good") & (SDmethod=="Good") ~ "Good"
                                        , (IQRmethod == "NE") | (SDmethod == "NE") ~ "NE"
                                        , TRUE ~ "Outlier"))

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
