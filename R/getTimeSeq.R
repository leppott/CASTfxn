#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Time Sequence Line of Evidence (Graphics only)
#'
#' @description Graph time-specific stressor-response values.
#'
#' @details Generates faceted time sequence graphics (stressor/response, one
#' atop the other). All stressor/response data are graphed.
#' Improvements: Add scoring.
#'
#' Uses the packages dplyr, ggplot2, lubridate, tidyr
#'
#' @param TargetSiteID site identifier for the site being evaluated (the Target Site)
#' @param biocomm biological response community (bmi, algae, or fish)
#' @param bioindex the column name containing the biological index value
#' @param df_stress dataframe containing all target site stressors, regardless of matching status.
#' @param df_resp dataframe containing the target site biological response samples'
#' index and metric values, regardless of matching status
#' @param df_respinfo dataframe containing response metadata, specifically "Label"
#' @param df_stressinfo dataframe containing stressor metadata, specifically "Label"
#' @param plotdpi Standardized dpi value for graphics. Default = plot_dpi.
#' @param plotH Standardized height value for graphics. Default = plot_H.
#' @param plotW Standardized width value for graphics. Default = plot_W.
#' @param plotunits Standardized units for graphics. Default = plot_units.
#' @param dir_results Directory containing all results. Default = file.path(getwd(),"Results").
#' NOTE: the code adds a middle directory, BioComm, between the Results and the subdirectory.
#' @param dir_sub Subdirectory for outputs from this function. Default = "TimeSequence"
#' @param boo_plot Flag declaring whether the plots should be saved or not. Default = TRUE.
#'
#' @return One or more pngs in SiteID/Biocomm/TemporalSequence subfolder of the
#'        "Results" folder. No scores are currently generated.
#'
#' @keywords internal
#'
#' @export
getTimeSeq <- function(TargetSiteID,
                       biocomm,
                       bioindex,
                       df_stress,
                       df_resp,
                       df_respinfo,
                       df_stressinfo,
                       df_paired,
                       plotdpi = plot_dpi,
                       plotH = plot_H,
                       plotW = plot_W,
                       plotunits = plot_units,
                       dir_results = file.path(getwd(),"Results"),
                       dir_sub = "_WoE",
                       boo_plot = TRUE) {##FUNCTION.START

  # Debug
  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    TargetSiteID
    biocomm = bioComm
    bioindex = bioIndex
    df_stress = data_Stress
    df_resp = bioMetricData[bioMetricData$StationID == TargetSiteID, ]
    df_respinfo = data_bmiMetricsInfo
    df_stressinfo = df_stressorMetadata
    plotdpi = plot_dpi
    plotH = plot_H
    plotW = plot_W
    plotunits = plot_units
    dir_results = dir_results
    dir_sub = "_WoE"
  }

  # Define pipe
  `%>%` <- dplyr::`%>%`

  not_all_na <- function(x) {!all(is.na(x))}
  biocomm <- toupper(biocomm)

  # Write results directory ----
  out.folders <- c(dir_results, TargetSiteID, biocomm, dir_sub)

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      path <- file.path(out.folders[i])
    } else {
      path <- file.path(path, out.folders[i])
    }
    if (dir.exists(path) == FALSE) {
      dir.create(path)
    }
  }

  path <- file.path(dir_results, TargetSiteID, biocomm, dir_sub)

  # Get vector of target site stressors
  stressors <- as.vector(unlist(df_stressinfo$Stressor))
  metrics <- as.vector(unlist(df_respinfo$MetricName))

  # df_stress <- df_stress %>%
  #   dplyr::filter(StationID == TargetSiteID) %>%
  #   tidyr::pivot_wider(names_from = "StdParamName", values_from = "TransfResult") %>%
  #   dplyr::select(StationID, StressSampleID, StressSampleDate, all_of(stressors))

  if (any(is.na(df_stress$StressSampleDate))) {
    msg <- "NA values in Sample Date indicative of modeled stressor data."
    message(msg)
    df_NAs <- as.data.frame(dplyr::filter(df_stress, is.na(StressSampleDate)))

    # TODO: Fix this!
    for (i in 1:nrow(df_NAs)) {
      stressNA <- df_NAs$variable[i]
      gapcomment <- "No date is available for modeled stressors."
      df.temp <- cbind.data.frame("getTimeSeq", stressNA, 0, gapcomment)
      if (i == 1) {
        gaps <- df.temp
      } else {
        gaps <- rbind(gaps, df.temp)
      }
    }
    fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                row.names = FALSE, sep = "\t")
  } # End modeled data write to data gaps

  data_paired <- df_paired %>% 
    dplyr::filter(StationID == TargetSiteID) %>% 
    dplyr::select(StationID, StressSampleDate, RespSampleDate, StressSampleID, RespSampleID, Quality, all_of(stressors)) %>% 
    dplyr::left_join(df_resp %>% dplyr::select(StationID, RespSampleID, RespSampleDate, all_of(metrics)), by = c("StationID", "RespSampleID", "RespSampleDate"))
  # LCN 20250916 not doing anything with modeled stressors. I'm fine if they are included, they just won't be informative plots.
  
  # df_stress <- df_stress %>%
  #   dplyr::filter(!is.na(StressSampleDate)) %>%   # Eliminate modeled stressors which are not time-bound
  #   dplyr::select(StationID, StressSampleID, StressSampleDate, all_of(stressors)) %>%
  #   dplyr::select_if(not_all_na)
  # 
  # stressors <- intersect(stressors, colnames(df_stress))
  # 
  # df_resp <- df_resp %>%
  #   dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, all_of(metrics)) %>%
  #   dplyr::select_if(not_all_na)
  # 
  # metricData <- intersect(metrics, colnames(df_resp))

  count = 1
  totplots <- length(stressors) * length(metrics)

  # Loop over stressors
  msgNum <- 0
  for (s in seq_along(stressors)) {
    stressname <- stressors[s]
    stressLabel <- df_stressinfo$Label[df_stressinfo$Stressor == stressname]
    stressLogYN <- df_stressinfo$LogTransf[df_stressinfo$Stressor == stressname]
    if (stressLogYN == 1) {
      stressLabel <- paste0("Log1p ", stressLabel)
    }

    # Write graphics directory ----
    out.dir <- dirname(dir_results)
    out.folders <- c(out.dir, basename(dir_results), TargetSiteID, biocomm, stressname)

    for (i in 1:length(out.folders)) {
      if (i == 1) {
        dir_path_stress <- file.path(out.folders[i])
      } else {
        dir_path_stress <- file.path(dir_path_stress, out.folders[i])
      }
      if (dir.exists(dir_path_stress) == FALSE) {
        dir.create(dir_path_stress)
      }
    }

    # Loop over responses
    for (r in seq_along(metrics)) {
      metricname <- metrics[r]
      metricLabel <- df_respinfo$MetricLabel[df_respinfo$MetricName == metricname]
      msgNum <- msgNum + 1

      # Create filename for graphic
      fn = paste0(TargetSiteID, "_", make.names(stressname), "_",
                  biocomm, "_", make.names(metricname), "_TS.png")
      fpath = file.path(dir_path_stress, fn)

      # subset dataframe for dates and stressor/response values
      df.plotresp <- data_paired %>%
        dplyr::select(StationID, RespSampleDate, all_of(metricname)) %>%
        dplyr::rename(SampleDate = RespSampleDate, Value = {{metricname}}) %>%
        dplyr::mutate(type = "Response", Value = signif(Value, digits = 3)) %>%
        dplyr::filter(!is.na(Value))

      df.plotstress <- data_paired %>%
        dplyr::select(StationID, StressSampleDate, all_of(stressname)) %>%
        dplyr::rename(SampleDate = StressSampleDate, Value = {{stressname}}) %>%
        dplyr::mutate(type = "Stressor", Value = signif(Value, digits = 3)) %>%
        dplyr::filter(!is.na(Value))

      df.plot <- rbind(df.plotstress, df.plotresp) %>%
        dplyr::mutate(type = factor(type, levels = c("Stressor", "Response"),
                                    labels = c(stressLabel, metricLabel)))

      # Get min/max date range
      minDate <- as.Date(min(df.plot$SampleDate) - 30)
      minYear <- lubridate::year(minDate)
      minDate <- lubridate::ymd(paste0(minYear, "/01/01"))
      maxDate <- as.Date(max(df.plot$SampleDate) + 30)
      maxYear <- lubridate::year(maxDate)
      maxDate <- lubridate::ymd(paste0(maxYear, "/12/31"))

      df.plot <- df.plot %>%
        tidyr::complete(tidyr::nesting(type),
                        SampleDate = seq(minDate, maxDate, by = "day"))
      # LCN added to set desired date range from the first ggplot call

      msg <- paste0("Plotting bar graphs (", msgNum, "/", totplots, ") ",
                    stressname, " and ", metricname)
      message(msg)

      p_ts <- ggplot2::ggplot(df.plot, ggplot2::aes(x = SampleDate,
                                                    y = as.numeric(Value)))
      p_ts <- p_ts + ggplot2::geom_col(col = "black", fill = "black",
                                       linewidth = 0.2, alpha = 0.5)
      p_ts <- p_ts + ggplot2::geom_label(ggplot2::aes(x = SampleDate,
                                                      y = as.numeric(Value)/2,
                                                      label = Value))
      p_ts <- p_ts + ggplot2::facet_wrap(~type, ncol = 1, scales = "free_y")
      p_ts <- p_ts + ggplot2::theme_bw()
      p_ts <- p_ts + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                        hjust = 1,
                                                                        size = 6),
                                    panel.grid.minor = ggplot2::element_blank())
      p_ts <- p_ts + ggplot2::scale_x_date(limits = c(minDate, maxDate),
                                           date_labels = "%Y-%m-%d",
                                           date_breaks = "3 months")
      p_ts <- p_ts + ggplot2::labs(title = paste(TargetSiteID,
                                                 "Stressor/Response Time Series"),
                                   x = "Sample Date", y = "Value")

      if(boo_plot){
        ggplot2::ggsave(filename = fpath, p_ts, dpi = plotdpi, width = plotW,
                        height = plotH, units = plotunits)
      }## IF ~ boo_plot ~ END

    } # END loop over responses & graphics

  } # END loop over stressors

  # df_stress <- df_stress %>%
  #   tidyr::pivot_longer(cols = all_of(stressors), names_to = "Stressor",
  #                       values_to = "StressorValue", values_drop_na = TRUE)
  # df_resp <- dplyr::select(df_resp, StationID, RespSampleID, RespSampleDate,
  #                          all_of(bioIndexGp), Quality)
  # 
  # df_TS_scores <- merge(df_stress, df_resp,
  #                       by.x = c("StationID", "StressSampleDate"),
  #                       by.y = c("StationID", "RespSampleDate"),
  #                       all.x = TRUE)
  # df_TS_scores <- df_TS_scores %>%
  #   dplyr::mutate(RespSampleDate = StressSampleDate, # Why set these equal if they are not?
  #                 LoE = "TS",
  #                 Score = "NE",
  #                 bioIndexName := {{bioindex}},
  #                 bioComm := {{biocomm}}) %>%
  #   dplyr::rename(bioIndex := {{bioindex}})
  # 
  # df_TS_scores <- merge(df_TS_scores,
  #                       df_stressinfo[, c("Stressor", "LogTransf", "Label")],
  #                       by = "Stressor", all.x = TRUE)
  # 
  # df_TS_scores <- df_TS_scores %>%
  #   dplyr::mutate(Label = ifelse(LogTransf == 1,
  #                                paste0("Log1p ", Label),
  #                                Label),
  #                 StressorValue = ifelse(LogTransf == 1,
  #                                        log1p(StressorValue),
  #                                        StressorValue)) %>%
  #   dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
  #                 RespSampleDate, bioComm, bioIndexName, bioIndex, Quality,
  #                 Label, StressorValue, LoE, Score) %>%
  #   dplyr::rename(Stressor = Label)
  # 
  
  df_TS_scores <- data_paired %>% 
    tidyr::pivot_longer(cols = all_of(stressors), names_to = "Stressor", values_to = "StressorValue") %>% 
    tidyr::pivot_longer(cols = all_of(metrics), names_to = "bioIndexName", values_to = "bioIndex") %>% 
    dplyr::filter(bioIndexName == bioindex, !is.na(StressorValue)) %>% 
    dplyr::mutate(bioComm = biocomm, LoE = "TS", Score = "NE") %>% 
    dplyr::left_join(df_stressinfo %>% dplyr::select(Stressor, LogTransf, Label), by = "Stressor") %>% 
    dplyr::mutate(Label = ifelse(LogTransf == 1,
                          paste0("Log1p ", Label),
                          Label)) %>% 
    dplyr::select(-Stressor, -LogTransf) %>% 
    dplyr::rename(Stressor = Label)

  return(df_TS_scores)

}
