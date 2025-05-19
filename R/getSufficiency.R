#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.2
#
#' @title Stressor Sufficiency Line of Evidence
#'
#' @description Generates logistic regressions to answer the question:
#' are stressor levels sufficient to explain the observed impairment?
#' Also writes a tab-delimited text file containing scores for this line
#' of evidence.
#'
#' @details \strong{Derive Evidence for Stressor Sufficiency from Field
#' Observational Studies.}
#'
#' Stressor-response from field observational studies: Is the level of the
#' stressor sufficient to explain the level of biological effect observed at
#' the site?
#'
#' Using all comparator sites, fit logistical regression curve of the probability
#' of poor condition (i.e., poor biological index score) as a function of
#' stressor level.  Compare stressor levels from test site to levels
#' corresponding to median (50%) and low (20%) probabilities of observing poor
#' condition.
#'
#' 1. Supports the case for the candidate cause.  Stressor levels at the test
#' site are above the lower confidence limit  (LCL) corresponding to 50%
#' probability of observing poor condition
#'
#' 0. Indeterminate.  Stressor levels at the test site are between the LCL
#' corresponding to 50% probability of observing poor condition and the UCL
#' corresponding to 20% probability of observing poor condition.
#'
#' -1. Weakens the case for the candidate cause.  Stressor levels at the test
#' site are below the upper confidence limit (UCL) corresponding to 20%
#' probability of observing poor condition.
#'
#' Cut function is used to assign narrative categories and degraded status
#' based on the provided biological score, ensuring criteria are applied the
#' same across all sites.
#'
#' The BioDegLab has to remain as the default values of Yes and No. Other
#' values will break the code.
#'
#' Only a single biological measurement is used. But multiple stressors can be
#' used.
#'
#' Uses the libraries dplyr, tidyr, and ggplot2.
#'
#'
#' @param TargetSiteID ID of station to be evaluated. May have one or many samples.
#' @param df_data dataframe containing matched stressor-response data for the
#'                biological response community desired
#' @param compSites vector containing comparator site IDs
#' @param df_stressinfo dataframe containing stressor metadata (LogTransf, Label)
#' @param biocomm Biological community; BMI, algae, or fish  Default = "BMI".
#' @param colBio df_data column name for the field with biological index value.
#' @param dir_plots Directory to save plots. Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function. Default = "Sufficiency"
#' @param boo_plot Boolean value indicating whether or not to print the plot.
#'                 Defaults to TRUE.
#'
#' @return Writes individual plots as pngs, and a tab-delimited text file with
#'         scores for the sufficiency line of evidence to a directory:
#'         "Results/TargetSiteID/BioComm/Sufficiency".
#'
#' @examples
#' \dontrun{
#'}
#' @export
getSufficiency <- function(TargetSiteID,
                           df_data,
                           compSites,
                           df_stressinfo,
                           biocomm,
                           colBio,
                           plotvars,
                           plot_dpi,
                           plot_H,
                           plot_W,
                           plot_units,
                           dir_plots = file.path(getwd(), "Results"),
                           dir_sub = "Sufficiency",
                           boo_plot = TRUE) {##FUNCTION.START

  boo_DEBUG <- FALSE

  if (boo_DEBUG==TRUE) {
    TargetSiteID = TargetSiteID
    df_data = df_PairedSRTransf
    compSites = list.CompSites$comp.sites
    df_stressinfo = list.stressors$stressors
    biocomm = bioComm
    colBio = bioIndex
    plotvars = data_plotvars
    plot_dpi = plot_dpi
    plot_H = plot_H
    plot_W = plot_W
    plot_units = plot_units
    dir_plots = dir_results
    dir_sub = "Sufficiency"
    boo_plot = boo_plot_user
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)
  stressors <- as.vector(df_stressinfo$Stressor)

  # define scoring limits
  negStart <- 0
  negEnd   <- 0.2  # same as zeroStart
  zeroEnd  <- 0.5  # same as posStart
  posEnd   <- 1
  midNeg   <- ((negEnd - negStart) / 2) + negStart
  midZero  <- ((zeroEnd - negEnd) / 2) + negEnd
  midPos   <- ((posEnd - zeroEnd) / 2) + zeroEnd
  # arrow labels
  aLabNeg  <- "-1"
  aLabZero <- "0"
  aLabPos  <- "1"

  # Write results directory ----
  out.dir <- dirname(dir_plots)
  out.folders <- c(out.dir, basename(dir_plots), TargetSiteID, biocomm, dir_sub)

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      dir_path <- file.path(out.folders[i])
    } else {
      dir_path <- file.path(dir_path, out.folders[i])
    }
    if (dir.exists(dir_path) == FALSE) {
      dir.create(dir_path)
    }
  }

  # Get dataset
  df_data <- df_data %>%
    dplyr::filter(StationID %in% compSites) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                  RespSampleDate, Quality, all_of(colBio), all_of(stressors))

  df_target <- dplyr::filter(df_data, StationID == TargetSiteID)

  # Transform stressor data as required
  strInfo <- df_stressinfo %>%
    dplyr::filter(Stressor %in% stressors) %>%
    dplyr::select(Stressor, LogTransf, Label)

  # Create Score Output File # add Bio.Nar just before Quality
  df.scores <- df_data %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                  RespSampleDate, all_of(colBio), Quality) %>%
    dplyr::mutate(StressorCode  = as.character(NA),
                  StressorValue = as.numeric(NA),
                  n             = as.character(NA),
                  SRpred_Deg    = as.character(NA),
                  Sc_SRlog      = as.character(NA),
                  BioComm       = as.character(NA),
                  Label         = as.character(NA))
  # remove all rows
  df.scores <- df.scores[0, ]

  # Calculate quantiles on Comparator Sites
  # Loop, j ####
  for (j in seq_along(stressors)) { ##FOR.j.START
    #
    str <- stressors[j]
    j.len <- length(stressors)
    jlog <- as.numeric(strInfo$LogTransf[strInfo$Stressor == str])
    jlabel <- as.character(strInfo$Label[strInfo$Stressor == str])

    if (jlog == 1) {
      jlabel <- paste("Log1p ", jlabel)
    }

    message(paste0("Processing item (", j, "/", j.len, "); ", str, "\n"))
    utils::flush.console()

    df.score.j <- df_data %>%
      dplyr::select(StationID, StressSampleID, StressSampleDate,
                    RespSampleID, RespSampleDate, Quality, all_of(colBio),
                    all_of(str)) %>%
      dplyr::filter(StationID == TargetSiteID) %>%
      tidyr::pivot_longer(cols = all_of(str), names_to = "StressorCode",
                          values_to = "StressorValue")

    df.plot <- df_data %>%
      dplyr::select(all_of(colBio), Quality, all_of(str))

    df.plot <- df.plot[!is.na(df.plot[, str]), ]

    if (nrow(df.plot) > 0) { # This uses the dataframe with transformed (if necessary) values
      df.plot <- df.plot %>%
        dplyr::rename(y = eval(colBio), x = all_of(str)) %>%
        dplyr::mutate(y.name = ifelse(Quality == "Degraded", 1, 0)) %>%
        dplyr::select(y, Quality, y.name, x)
      fit <- stats::glm(y.name ~ x, data = df.plot, family = stats::binomial)
      useVal <- "normal"
      j_values <- data.frame(x = df_target[, str])
      df.plot <- df.plot[stats::complete.cases(df.plot), ]

      #  Stressor Response Curve
      n_cc_df_plot <- nrow(df.plot[stats::complete.cases(df.plot[, c("x", "y")])
                                   , c("x", "y")])
      # create data for curve (type "response" gives probabilities)
      newdat <- data.frame(x = seq(min(df.plot$x, na.rm = TRUE)
                                   , max(df.plot$x, na.rm = TRUE), len = 100))
      newdat$y.name <- stats::predict(fit, newdata = newdat, type = "response")

      # Scoring
      j_SR_predict <- stats::predict(fit, newdata = j_values, type = "response")
      j_SR_score <- cut(j_SR_predict,
                        breaks = c(0, 0.2, 0.5, 1),
                        labels = c(-1, 0, 1))
      # plot ####
      # File Names
      fn_png_p1 <- paste0(TargetSiteID, "_", biocomm, "_SRInLog_", str, ".png")
      ppi       <- 300

      # Create (ggplot)
      bio_col <- c("gray25", "steelblue2")
      bio_shp <- c(25, 21) # down triangle and circle
      bio_size <- c(3, 3)

      ## Plot, Variables, Target Site Line
      targ_line_col <- "red"
      targ_line_lty <- 2
      targ_line_lwd <- 1
      targ_vals <- as.numeric(unlist(j_values))

      legendtitle <- "Samples"
      ylabel <- "Relative probability of degraded condition"
      maintitleSR <- paste0(TargetSiteID, ": Stressor-response (logistic regression) line of evidence")
      subtitleSR <-"Are stressor levels sufficient to explain the observed impairment?"
      subtitleSR <- stringr::str_wrap(subtitleSR, 100)

      captionSR <- paste(paste0("All comparator samples (n=", n_cc_df_plot, ").")
                         , paste0("Score = ", paste(j_SR_score, collapse = ", "), ".")
                         , sep = "\n")

      # Annotation values
      # Score = -1 runs from 0 to 0.20 on the y axis
      # Score = 0 runs from 0.20 to 0.50 on the y axis
      # Score = 1 runs from 0.50 to 1 on the y axis
      xmin <- min(df.plot$x, na.rm = TRUE)
      xmax <- max(df.plot$x, na.rm = TRUE)
      xseg <- xmax + (0.02 * xmax)

      # Get base info for scores table
      df.score.j <- df.score.j %>%
        dplyr::mutate(BioComm = biocomm,
                      Label = jlabel,
                      n = nrow(df.plot),
                      SRpred_Deg = j_SR_predict,
                      Sc_SRlog = j_SR_score) %>%
        dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                      RespSampleDate, all_of(colBio), Quality, StressorCode,
                      StressorValue, n, SRpred_Deg, Sc_SRlog, BioComm, Label)

      # plot1, ggplot ####
      p1 <- ggplot2::ggplot(df.plot, ggplot2::aes(x = x, y = y.name)) +
        ggplot2::geom_point(ggplot2::aes(color = "black", shape = Quality
                                         , fill = Quality)
                            , alpha = 0.5, size = 2, na.rm = TRUE) +
        ggplot2::scale_fill_manual(name = legendtitle
                                   , breaks = c("Degraded", "Not degraded")
                                   , values = bio_col, drop = FALSE) +
        ggplot2::scale_color_manual(name = legendtitle
                                    , breaks = c("Degraded", "Not degraded")
                                    , values = bio_col, drop = FALSE) +
        ggplot2::scale_shape_manual(name = legendtitle
                                    , breaks = c("Degraded", "Not degraded")
                                    , values = bio_shp, drop = FALSE) +
        ggplot2::geom_vline(xintercept = targ_vals, color = targ_line_col
                            , lty = targ_line_lty, lwd = targ_line_lwd
                            , na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = c(0.2, 0.5), color = "black"
                            , lty = 2, na.rm = TRUE) +
        ggplot2::annotate("segment", y = negStart, yend = negEnd, x = xseg,
                          color = "orange", linewidth = 0.7, alpha = 0.6,
                          arrow = grid::arrow(ends = "both", type = "open",
                                              length = grid::unit(0.2, "cm"))) +
        ggplot2::annotate("segment", y = negEnd, yend = zeroEnd, x = xseg,
                          color = "orange", linewidth = 0.7, alpha = 0.6,
                          arrow = grid::arrow(ends = "both", type = "open",
                                              length = grid::unit(0.2, "cm"))) +
        ggplot2::annotate("segment", y = zeroEnd, yend = posEnd, x = xseg,
                          color = "orange", linewidth = 0.7, alpha = 0.6,
                          arrow = grid::arrow(ends = "both", type = "open",
                                              length = grid::unit(0.2, "cm"))) +
        ggplot2::annotate("text", x = xmax, y = c(midNeg, midZero, midPos),
                          label = c(aLabNeg, aLabZero, aLabPos), color = "orange") +
        ggplot2::labs(y = ylabel, x = jlabel) +
        ggplot2::geom_line(ggplot2::aes(y = y.name, x = x), data = newdat
                           , color = "black", lwd = 1, na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)
                       , plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(title = maintitleSR, subtitle = subtitleSR
                      , caption = captionSR)

      if ((boo_plot) == TRUE) {
        ggplot2::ggsave(filename = file.path(dir_path, fn_png_p1), plot = p1,
                        dpi = plot_dpi, width = plot_W, height = plot_H,
                        units = plot_units)
      }

    } ##IF.PLOT.END

    # Write to scores table
    if (exists("df.scores")) {
      df.scores <- rbind(df.scores, df.score.j)
    }

  } ##FOR.j.END

  # Save scores file
  df.scores <- dplyr::mutate(df.scores,
                             Sc_SRlog = ifelse(is.na(Sc_SRlog), "NE", Sc_SRlog))
  fn.scores <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm
                                          , "_SRLog_Scores.tab"))
  utils::write.table(df.scores, file = fn.scores, append = FALSE
                     , col.names = TRUE, row.names = FALSE, sep = "\t")

  df_SuffScores <- df.scores %>%
    dplyr::rename(Stressor = Label, bioComm = BioComm, bioIndex = colBio,
                  Score = Sc_SRlog) %>%
    dplyr::mutate(LoE = "Suff") %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                  RespSampleDate, bioComm, bioIndex, Quality, Stressor,
                  StressorValue, LoE, Score)

  return(df_SuffScores)

}##FUNCTION.END

