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
getOutliers <- function(df_data, df_meta, dir_plots) {##FUNCTION.START

  # Debug
  boo_DEBUG <- FALSE
  if (boo_DEBUG == TRUE) {
    df_data = data_Stress
    df_meta = data_stressInfo
    dir_plots = file.path(dir_results, "Histograms")
  }

  # Define pipe
  `%>%` <- dplyr::`%>%`
  # not_all_na <- function(x) {!all(is.na(x))}

  ifelse(!dir.exists(dir_results) == TRUE
         , dir.create(dir_results)
         , FALSE)
  ifelse(!dir.exists(dir_plots) == TRUE
         , dir.create(dir_plots)
         , FALSE)

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
    dplyr::mutate(ResultValue = ifelse(ResultValue < 0 & LogTransf == 1
                                       , NA, ResultValue)) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::mutate(TransfResult = ifelse(LogTransf == 1
                                        , suppressWarnings(log1p(ResultValue))
                                        , ResultValue))

  params <- unique(as.character(df_data$StdParamName))

  for (a in seq_along(params)) { # Iterate over parameters

    paramName <- params[a]
    msg <- paste0("Evaluating ", paramName)
    message(msg)

    df_sub <- df_data %>%  # Subset for just one parameter at a time
      dplyr::filter(StdParamName == paramName) %>%
      dplyr::select(StationID, StressSampleID, StressSampleDate
                    , StdParamName, LogTransf, ResultValue, TransfResult)

    # Test transformed variables
    # df_sub1 <- df_data %>%  # Subset for just one parameter at a time
    #   dplyr::filter(StdParamName == paramName) %>%
    #   dplyr::select(StationID, StressSampleID, StressSampleDate
    #                 , StdParamName, LogTransf, ResultValue)
    #
    # # Need to change definition of LogTransf to are the data for the StdParamName already log transformed?
    # if (min(df_sub1$ResultValue) < 0 | !(paramName %in% c("pH", "DO_mg_L"))) {
    #   fnorm_orig <- fitdistrplus::fitdist(df_sub1$ResultValue, "norm")
    #   flnorm_orig <- fitdistrplus::fitdist(df_sub1$ResultValue, "lnorm")
    #   df_sub1_gofstat <- fitdistrplus::gofstat(list(fnorm_orig, flnorm_orig))
    #   df_sub <- df_sub %>%
    #     dplyr::mutate(BestFitDistr = ifelse(df_sub1_gofstat$aic[1] < df_sub1_gofstat$aic[2]
    #                                         , "normal", "lognormal")
    #                   , UseVal = ifelse(BestFitDistr == "normal", ResultValue, TransfResult))
    #   fitdistrplus::cdfcomp(list(fnorm_orig, flnorm_orig), xlogscale = TRUE
    #                         , ylogscale = TRUE, legendtext = c("normal", "lognormal"))
    # }
    #
    # df_sub2 <- df_data %>%
    #   dplyr::filter(StdParamName == paramName) %>%
    #   dplyr::select(StationID, StressSampleID, StressSampleDate
    #                 , StdParamName, LogTransf, TransfResult) %>%
    #   dplyr::mutate(BestFitDistr = ifelse(LogTransf == 1, "lognormal", "normal")
    #                 , UseVal = ifelse(BestFitDistr == "normal", ResultValue, TransfResult))
    # fnorm_transf <- fitdistrplus::fitdist(df_sub2$TransfResult, "norm")
    # flnorm_transf <- fitdistrplus::fitdist(df_sub2$TransfResult, "lnorm")
    # df_sub2_gofstat <- fitdistrplus::gofstat(list(fnorm_transf, flnorm_transf))
    # fitdistrplus::cdfcomp(list(fnorm_transf, flnorm_transf), xlogscale = TRUE
    #                               , ylogscale = TRUE, legendtext = c("normal", "lognormal"))


    ## Transform/scale variables ----
    ## Plot histograms before/after ----
    # cols <- setdiff(colnames(WS.STATE.final), c("COMID", "US_L3CODE"))
    # df.temp <- df_sub[, c("COMID", "US_L3CODE")]
    # df.temp_scaled <- df_sub[, c("COMID", "US_L3CODE")]
    # df.lambda <- data.frame(Variable = character(), Lambda = double())
    # for (c in seq_along(cols)) {
    #   col <- cols[c]
    #   print(col)

    fn <- paste0(paramName, ".png")
    fn2 <- paste0(paramName, "_transf.png")

    # plot histogram of data
    p1 <- ggplot2::ggplot(df_sub1, ggplot2::aes(x = ResultValue)) +
      ggplot2::geom_histogram(bins = 500) +
      ggplot2::ggtitle(paste0("Histogram of ", paramName, " observations")) +
      ggplot2::xlab(paramName) +
      ggplot2::theme_bw()
    ggplot2::ggsave(file.path(dir_plots, fn), p1, width = 6, height = 4
                    , units = "in")

    if (unique(df_sub2$LogTransf) == 1) {
      p2 <- ggplot2::ggplot(df_sub2, ggplot2::aes(x = TransfResult)) +
        ggplot2::geom_histogram(bins = 500) +
        ggplot2::ggtitle(paste0("Histogram of Log1p transformed ", paramName, " observations")) +
        ggplot2::xlab(paramName) +
        ggplot2::theme_bw()
      ggplot2::ggsave(file.path(dir_plots, fn2), p2, width = 6, height = 4
                      , units = "in")
    }

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
      dplyr::mutate(Outlier = dplyr::case_when((IQRmethod == "Good") & (SDmethod == "Good") ~ "Good"
                                               , (IQRmethod == "NE") | (SDmethod == "NE") ~ "NE"
                                               , grepl("Outlier", IQRmethod) & (SDmethod == "Outlier") ~ "Outlier"
                                               , TRUE ~ "Good"))

    if (a == 1) {
      df_temp <- df_sub
    } else {
      df_temp <- rbind(df_temp, df_sub)
    }
  } # End parameter iteration

  df_temp <- dplyr::select(df_temp, StressSampleID, StdParamName, ResultValue
                           , TransfResult, LogTransf, IQRmethod, SDmethod, Outlier)

  # Return what?
  myOutliers <- df_temp

}
