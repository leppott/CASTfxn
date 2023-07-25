#  Copyright 2023 TetraTech. All rights reserved.
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
#' Uses the packages dplyr, tidyr, ggplot2
#'
#' @param TargetSiteID site identifier for the site being evaluated (the Target Site)
#' @param biocomm biological response community (bmi, algae, or fish)
#' @param BioResp vector of biological response metric names (including the index)
#' @param stressors vector of stressors identified as candidate causes for the target site
#' @param df_stress dataframe containing all target site stressors, regardless of matching status.
#' @param df_stressinfo dataframe containing stressor metadata, specifically "Label"
#' @param df_resp dataframe containing the target site biological response samples'
#' index and metric values, regardless of matching status
#' @param df_respinfo dataframe containing response metadata, specifically "Label"
#' @param dir_results Directory containing all results. Default = file.path(getwd(),"Results").
#' NOTE: the code adds a middle directory, BioComm, between the Results and the subdirectory.
#' @param dir_sub Subdirectory for outputs from this function. Default = "TimeSequence"
#' @param boo_plot Flag declaring whether the plots should be saved or not. Default = TRUE.
#'
#' @return One or more jpgs in SiteID/Biocomm/TemporalSequence subfolder of the
#'        "Results" folder. No scores are currently generated.
#'
#' @keywords internal
#'
#' @export
getTimeSeq <- function(TargetSiteID
                       , biocomm
                       , BioResp
                       , stressors
                       , df_stress
                       , df_stressinfo
                       , df_resp
                       , df_respinfo
                       , dir_results = file.path(getwd(),"Results")
                       , dir_sub = "TimeSequence"
                       , boo_plot = TRUE
                       ) {##FUNCTION.START

  # Debug
  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    TargetSiteID
    biocomm = bioComm
    BioResp = bioMetricNames
    stressors = stressors
    df_stress = siteStressAll
    df_stressinfo = data_stressInfo
    df_resp = siteRespAll
    df_respinfo = data_bmiMetricsInfo
    dir_results = dir_results
    dir_sub = "TimeSequence"
  }


  # Define pipe
  `%>%` <- dplyr::`%>%`

  not_all_na <- function(x) {!all(is.na(x))}
  biocomm <- toupper(biocomm)

  # Check for presence of TemporalSequence directory. If not present, create
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID)) == TRUE
         , dir.create(file.path(dir_results, TargetSiteID))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID, biocomm)) == TRUE
         , dir.create(file.path(dir_results, TargetSiteID, biocomm))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID, biocomm, dir_sub)) == TRUE
         , dir.create(file.path(dir_results, TargetSiteID, biocomm, dir_sub))
         , FALSE)

  path <- file.path(dir_results, TargetSiteID, biocomm, dir_sub)

  skipflag = FALSE

  # Prep measured stressor data
  df_stress <- df_stress %>%
    dplyr::select(-StationID_Master) %>%
    dplyr::select_if(not_all_na) %>%
    tidyr::pivot_longer(!c(StressSampID, StressSampDate)
                        , names_to = "StdParamName"
                        , values_to = "ResultValue") %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::group_by(StressSampDate, StdParamName) %>%
    dplyr::summarize(meanResultValue = signif(mean(ResultValue, na.rm = TRUE)
                                              , digits = 3), .groups = "drop_last") %>%
    dplyr::rename(SampDate = StressSampDate, variable = StdParamName) %>%
    dplyr::filter(variable %in% stressors)

  if (any(is.na(df_stress$SampDate))) {
    msg <- "NA values in Sample Date indicative of modeled stressor data."
    message(msg)
    # print(msg)
    # flush.console()
    df_NAs <- as.data.frame(dplyr::filter(df_stress, is.na(SampDate))) %>%
      dplyr::select(variable)
    df_stress <- dplyr::filter(df_stress, !is.na(SampDate)) # Removes modeled stressors, which have not date

    for (i in 1:nrow(df_NAs)) {
      stressNA <- df_NAs$variable[i]
      gapcomment <- "No date is available for modeled stressors."
      df.temp <- cbind.data.frame("getTimeSeq", stressNA, 0
                                  , gapcomment)
      if (i == 1) {
        gaps <- df.temp
      } else {
        gaps <- rbind(gaps, df.temp)
      }
    }
    fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")

  }
  df_stressinfo <- unique(df_stressinfo[, c("StdParamName", "Label")])
  df_stress <- merge(df_stress, df_stressinfo, by.x = "variable"
                     , by.y = "StdParamName")
  df_stress$type <- "Stressor"

  # Prep response data
  df_resp <- df_resp %>%
    dplyr::select_if(not_all_na) %>%
    dplyr::select(!c(StationID_Master, Quality, ends_with("SampFlag"))) %>%
    tidyr::pivot_longer(!c(RespSampID, RespSampDate)
                        , names_to = "Biometric"
                        , values_to = "ResultValue") %>%
    dplyr::filter(!is.na(ResultValue), Biometric %in% BioResp)
  df_resp$ResultValue <- as.numeric(df_resp$ResultValue)
  df_resp <- df_resp %>%
    dplyr::group_by(RespSampDate, Biometric) %>%
    dplyr::summarize(meanResultValue = signif(mean(ResultValue, na.rm = TRUE)
                                              , digits=3), .groups = "drop_last") %>%
    dplyr::rename(SampDate = RespSampDate, variable = Biometric)
  df_respinfo <- unique(df_respinfo[, c("MetricName", "MetricLabel")])
  df_resp <- merge(df_resp, df_respinfo, by.x = "variable", by.y = "MetricName")
  df_resp <- dplyr::rename(df_resp, Label = MetricLabel)
  df_resp$type <- "Response"

  skipflag <- ifelse(nrow(df_resp) == 0, TRUE, FALSE)

  if (skipflag == FALSE) {

    # Ensure all data in one dataframe
    df.data <- rbind(as.data.frame(df_stress)
                     , as.data.frame(df_resp))

    minDate <- as.Date(min(df.data$SampDate) - 30)
    maxDate <- as.Date(max(df.data$SampDate) + 30)
    diffDate <- paste(round((maxDate - minDate)/10, 2), "days")
    # print(diffDate)
    # flush.console()

    # Loop over each stressor
    ppi = 300
    plot_H <- 6
    plot_W <- 9
    stresses <- unique(df_stress[, c("variable", "Label", "type")])
    count = 1

    for (s in 1:nrow(stresses)) {

      stressName = stresses[s, "variable"]
      stressLabel = as.character(stresses[s, "Label"])
      # print(paste0("s=",s," stressor is "))

      # Plot time series for stressor & bio response
      responses <- unique(df_resp[, c("variable", "Label", "type")])
      totplots <- nrow(stresses) * nrow(responses)
      for (r in 1:nrow(responses)) {

        respName = responses[r, "variable"]
        respLabel = as.character(responses[r, "Label"])

        fn = paste0(TargetSiteID, "_", biocomm, "_TS_", stressName, "_"
                    , respName, ".png")
        fpath = file.path(path, fn)

        df.plot <- df.data %>%
          dplyr::filter(variable %in% c(stressName,respName))
        df.plot$Label <- factor(df.plot$Label
                                , levels = c(df.plot$Label[1]
                                             , df.plot$Label[2]))
        maxStress <- max(df.plot$meanResultValue[df.plot$variable == stressName])
        maxResp <- max(df.plot$meanResultValue[df.plot$variable == respName])

        msg <- paste0("Plotting bar graphs (", count, "/", totplots, ") "
                      , stressName, " and ", respName)
        message(msg)

        p_ts <- ggplot2::ggplot(df.plot, ggplot2::aes(x = SampDate
                                                      , y = as.numeric(meanResultValue)))
        p_ts <- p_ts + ggplot2::geom_col(fill = "black", width = 0.8)
        # p_ts <- p_ts + ggplot2::geom_col(fill = "black", width = colwid
        #              , position = ggplot2::position_dodge(preserve = "single")
        #              , na.rm = TRUE)
        # p_ts <- p_ts + ggrepel::geom_text_repel(ggplot2::aes(label=meanResultValue)
        #                         , hjust= 2, vjust = 0, size=2.5, na.rm = TRUE)

        # p_ts <- p_ts + ggplot2::geom_text(ggplot2::aes(label=df.plot$meanResultValue)
        p_ts <- p_ts + ggplot2::geom_text(ggplot2::aes(label = meanResultValue)
                                          , hjust = 2, vjust = 0.5
                                          , size = 3, na.rm = TRUE)
        p_ts <- p_ts + ggplot2::facet_wrap(~Label, ncol = 1, scales = "free_y")
        p_ts <- p_ts + ggplot2::theme_bw()
        p_ts <- p_ts + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                                          , hjust = 1
                                                                          , size = 8)
                                      , panel.grid.minor = ggplot2::element_blank())
        p_ts <- p_ts + ggplot2::scale_x_date(limits = c(minDate, maxDate)
                                             , date_labels = "%m/%d/%Y"
                                             , date_breaks = diffDate)
        p_ts <- p_ts + ggplot2::labs(title = paste(TargetSiteID
                                                   ,"Stressor/Response Time Series")
                                     , x = "Sample Date", y = "Value")
        if(boo_plot){
          ggplot2::ggsave(filename = fpath, p_ts, dpi = ppi, width = plot_W
                          , height = plot_H, units = "in")
        }## IF ~ boo_plot ~ END
        count = count + 1
      } # End loop over responses

    } # End loop over stressors
  } else {
    msg <- paste("No", biocomm, "response data available for", TargetSiteID)
    message(msg)
  }

}
