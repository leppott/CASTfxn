#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Verified Predictions
#'
#' @description Get verified predictions.
#'
#' @details
#'
#' Required packages: dplyr, ggplot2, grid, stringr, tidyr
#'
#' @param TargetSiteID Site ID
#' @param stressors.ssi vector of stressors identified as candidate causes with SSIs
#' @param df_stressinfo dataframe of stressor metadata
#' @param df_paired list_MatchBioData
#' @param biocomm default = "bmi"
#' @param df_bioMetricData dataframe of raw metric data
#' @param df_bioMetricInfo dataframe of metric metadata
#' @param colBio column containing the overall biological index score
#' @param plotvars dataframe containing the default plotting variables (color,
#'                  fill, alpha, shape for target, inside the case not degraded,
#'                  inside the case degraded, outside the case not degraded, and
#'                  outside the case degraded)
#' @param plotdpi standardized plot dpi
#' @param plotH standardized plot height
#' @param plotW standardized plot width
#' @param plotunits units for plot height and width
#' @param dir_plots default = file.path(getwd(), "Results")
#' @param dir_sub default = "VerifiedPredictions"
#' @param boo_plot = TRUE
#'
#' @return Results text file and png files to "Results" "VerifiedPredictions" folder
#' in working directory of box plots
#'
#' @examples
#' \dontrun{
#' }
#'
#' #~~~~~~~~~~~~~~~~
#' @export
#'
getVPSSI <- function(TargetSiteID,
                     stressors.ssi,
                     df_stressinfo,
                     df_paired,
                     biocomm,
                     df_bioMetricData,
                     df_bioMetricInfo,
                     colBio,
                     plotvars = data_plotvars,
                     plotdpi = plot_dpi,
                     plotH = plot_H,
                     plotW = plot_W,
                     plotunits = plot_units,
                     dir_plots = file.path(getwd(), "Results"),
                     dir_sub = "_WoE",
                     boo_plot = TRUE) {##FUNCTION.START

  # Debugging
  boo.DEBUG <- FALSE
  #
  if (boo.DEBUG == TRUE) {##IF.boo.DEBUG.START
    TargetSiteID = TargetSiteID
    stressors.ssi = stressors.ssi
    df_stressinfo = df_stressorMetadata
    df_paired = df_PairedStressResp
    biocomm = bioComm
    df_bioMetricData = bioMetricData
    df_bioMetricInfo = bioMetricInfo
    colBio = bioIndex
    plotvars = data_plotvars
    plotdpi = plot_dpi
    plotH = plot_H
    plotW = plot_W
    plotunits = plot_units
    dir_plots = dir_results
    dir_sub = "_WoE"
    boo_plot = boo_plot_user
  }##IF.boo.DEBUG.END

  # define pipe; standardize biocomm
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)

  # Write results directory ----
  out.dir <- dirname(dir_plots)
  out.folders <- c(out.dir, basename(dir_plots), TargetSiteID, biocomm, dir_sub)

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      dir.path <- file.path(out.folders[i])
    } else {
      dir.path <- file.path(dir.path, out.folders[i])
    }
    if (dir.exists(dir.path) == FALSE) {
      dir.create(dir.path)
    }
  }

  # Identify data gaps (missing SSIs)
  # SSI data gaps ----
  if (length(stressors.ssi) == 0) {
    # This should never occur!
    gapcomment <- paste0("No stressor-specific indices are available.")
    gaps <- cbind.data.frame("getVPSSI", "No SSI available", 0
                             , gapcomment)
    colnames(gaps) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_plots, TargetSiteID,fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")

  } else {

    # Prepare generic plot variables ----
    plotvars  <- plotvars %>%
      dplyr::filter(Type %in% c("target", "insideND", "insideD"))
    bio_col     <- unlist(plotvars$Fill)
    bio_fill    <- unlist(plotvars$Fill)
    bio_shape   <- unlist(plotvars$Shape)
    bio_size    <- unlist(plotvars$Size)
    bio_alpha   <- unlist(plotvars$Alpha)

    ## Plot, Variables, Target Site Line
    targ_line_col <- bio_col[1]
    targ_line_lty <- 2
    targ_line_lwd <- 0.5

    ## Plot, arrow labels
    aLabPos <- "1"
    aLabZero <- "0"
    aLabNeg <- "-1"

    ## Merge stressor info with metric info to obtain ssi data
    info.stress <- df_stressinfo %>%
      dplyr::filter(Stressor %in% stressors.ssi) %>%
      dplyr::select(Stressor, SSIndex, Label, LogTransf)

    info.ssi <- df_bioMetricInfo %>%
      dplyr::filter(MetricName %in% unique(info.stress$SSIndex))

    # Loop over SSIndices ----
    for (i in nrow(info.ssi)) { # QC for nrow == 0?

      ssi.name <- info.ssi$MetricName[i]
      ssi.label <- stringr::str_to_title(info.ssi$MetricLabel[i])
      ssi.dir <- info.ssi$TrendWIncStress[i]
      ssi.cutoff <- info.ssi$CutoffValue[i]
      ssi.inclind <- info.ssi$InclusiveIndicator[i]
      ssi.stressors <- info.stress$Stressor[info.stress$SSIndex == ssi.name]
      df.targetdata <- df_paired %>%
        dplyr::filter(StationID == TargetSiteID) %>%
        dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                      RespSampleDate, all_of(ssi.stressors)) %>%
        tidyr::pivot_longer(cols = all_of(ssi.stressors), names_to = "Stressor",
                            values_to = "TransfValue", values_drop_na = TRUE)

      ## QC data availability ----
      if (ssi.name %in% colnames(df_bioMetricData) == FALSE) {

        notInData <- TRUE
        gapcomment <- paste0(ssi.name, " not in ", biocomm, " metric data.")
        gaps <- cbind.data.frame("getVPSSI", "No SSI data available", 0,
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_plots, TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
        next

      } #END check for ssi in metric data

      # Create data set for use
      df.comp <- df_paired %>%
        dplyr::filter(IncaseYN == 1)
      df.data <- merge(df.comp, df_bioMetricData,
                       by = c("StationID", "RespSampleID", "RespSampleDate",
                              colBio, "Quality"))
      str_comp <- "inside-the-case samples"
      rm(df.comp)

      # Prep data for plots ----
      df_plot <- dplyr::mutate(df.data, BioComm = {{biocomm}},
                               SSIndex = {{ssi.name}}) %>%
        dplyr::rename(SSIValue = {{ssi.name}}) %>%
        dplyr::select(StationID, IncaseCol, StressSampleID, StressSampleDate,
                      RespSampleID, RespSampleDate, BioComm, all_of(colBio),
                      Quality, SSIndex, SSIValue, all_of(ssi.stressors)) %>%
        dplyr::filter(!is.na(SSIndex))

      # If cutoff value ----
      if (!is.na(ssi.cutoff)) {

        message(paste0(ssi.name, " has a cutoff value."))

        # Prep data for logistic regression and scoring
        if (ssi.inclind == "≤" | ssi.inclind == "<=") {
          if (ssi.dir == "Dec") { # Lower values of the index represent more stress
            df_plot$SSIqual <- cut(df_plot$SSIValue, breaks = c(-Inf, ssi.cutoff, Inf),
                                   right = TRUE,
                                   labels = c("Degraded", "Not degraded"))
          } else { # Higher values of the index represent more stress
            df_plot$SSIqual <- cut(df_plot$SSIValue, breaks = c(-Inf, ssi.cutoff, Inf),
                                   right = TRUE,
                                   labels = c("Not degraded", "Degraded"))
          }
        } else if (ssi.inclind == "≥" | ssi.inclind == ">=") {
          if (ssi.dir == "Dec") { # Lower values of the index represent more stress
            df_plot$SSIqual <- cut(df_plot$SSIValue, breaks = c(-Inf, ssi.cutoff, Inf),
                                   right = FALSE,
                                   labels = c("Degraded", "Not degraded"))
          } else { # Higher values of the index represent more stress
            df_plot$SSIqual <- cut(df_plot$SSIValue, breaks = c(-Inf, ssi.cutoff, Inf),
                                   right = FALSE,
                                   labels = c("Not degraded", "Degraded"))
          }
        } else if (ssi.inclind == "<") {
          if (ssi.dir == "Dec") { # Lower values of the index represent more stress
            df_plot$SSIqual <- cut(df_plot$SSIValue, breaks = c(-Inf, ssi.cutoff, Inf),
                                   right = FALSE,
                                   labels = c("Degraded", "Not degraded"))
          } else { # Higher values of the index represent more stress
            df_plot$SSIqual <- cut(df_plot$SSIValue, breaks = c(-Inf, ssi.cutoff, Inf),
                                   right = FALSE,
                                   labels = c("Not degraded", "Degraded"))
          }
        } else { # (ssi.inclind == ">")
          if (ssi.dir == "Dec") { # Lower values of the index represent more stress
            df_plot$SSIqual <- cut(df_plot$SSIValue, breaks = c(-Inf, ssi.cutoff, Inf),
                                   right = TRUE,
                                   labels = c("Degraded", "Not degraded"))
          } else { # Higher values of the index represent more stress
            df_plot$SSIqual <- cut(df_plot$SSIValue, breaks = c(-Inf, ssi.cutoff, Inf),
                                   right = TRUE,
                                   labels = c("Not degraded", "Degraded"))
          }

        } # End cut statements

        # Create scoring dataframe (empty)
        df.scores.log <- df_plot %>%
          dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                        RespSampleDate, SSIndex, SSIValue, SSIqual) %>%
          dplyr::mutate(Stressor       = as.character(NA),
                        StressorValue  = as.numeric(NA),
                        n              = as.character(NA),
                        VPpred_Deg     = as.numeric(NA),
                        Sc_VPlog       = as.character(NA),
                        BioComm        = as.character(NA),
                        Label          = as.character(NA)) %>%
          dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                        RespSampleDate, BioComm, Stressor, Label, StressorValue,
                        SSIndex, SSIValue, SSIqual, n, VPpred_Deg, Sc_VPlog)
        cols.scores.log <- colnames(df.scores.log)

        # remove all rows
        df.scores.log <- df.scores.log[0, ]

        for (j in seq_along(ssi.stressors)) {

          str <- ssi.stressors[j]
          slab <- info.stress$Label[info.stress$Stressor == str]
          slogyn <- info.stress$LogTransf[info.stress$Stressor == str]

          if (slogyn == 1) {
            slab <- paste0("Log1p ", slab)
          }

          # Write graphics directory ----
          out.dir <- dirname(dir_plots)
          out.folders <- c(out.dir, basename(dir_plots), TargetSiteID, biocomm, str)

          for (d in 1:length(out.folders)) {
            if (d == 1) {
              dir_path_stress <- file.path(out.folders[d])
            } else {
              dir_path_stress <- file.path(dir_path_stress, out.folders[d])
            }
            if (dir.exists(dir_path_stress) == FALSE) {
              dir.create(dir_path_stress)
            }
          }

          df_plot.log <- df_plot %>%
            dplyr::rename(y = SSIndex, x = {{str}}) %>%
            dplyr::mutate(y.name = ifelse(SSIqual == "Degraded", 1, 0)) %>%
            dplyr::select(y, SSIqual, y.name, x)
          fit <- stats::glm(y.name ~ x, data = df_plot.log, family = stats::binomial)
          j_values <- data.frame(x = df_plot[[str]][df_plot$StationID == TargetSiteID])
          df_plot.log <- df_plot.log[stats::complete.cases(df_plot.log[, c("x", "y.name")]), ]

          n_cc_df_plot <- nrow(df_plot.log) - nrow(df_plot[df_plot$StationID == TargetSiteID, ])

          # create data for curve (type "response" gives probabilities)
          newdat <- data.frame(x = seq(min(df_plot.log$x, na.rm = TRUE),
                                       max(df_plot.log$x, na.rm = TRUE), len = 100))
          newdat$y.name <- stats::predict(fit, newdata = newdat, type = "response")

          # Scoring
          j_VPlog_predict <- stats::predict(fit, newdata = j_values, type = "response")
          j_VPlog_score <- cut(j_VPlog_predict,
                               breaks = c(0, 0.2, 0.5, 1),
                               labels = c(-1, 0, 1))

          j_values_scores <- cbind(j_values, j_VPlog_predict, j_VPlog_score) %>%
            dplyr::rename(StressorValue = x,
                          VPpred_Deg = j_VPlog_predict,
                          Sc_VPlog = j_VPlog_score)

          df_plot.log_target <- df_plot %>%
            dplyr::filter(StationID == TargetSiteID) %>%
            dplyr::select(StationID, StressSampleID, StressSampleDate,
                          RespSampleID, RespSampleDate, SSIndex,
                          SSIValue, SSIqual, all_of(str))

          df_plot.log_target <- merge(df_plot.log_target, j_values_scores,
                                      by.x = str, by.y = "StressorValue")

          df_plot.log_target <- df_plot.log_target %>%
            tidyr::pivot_longer(cols = all_of(str), names_to = "Stressor",
                                values_to = "StressorValue") %>%
            dplyr::mutate(BioComm = biocomm,
                          n = n_cc_df_plot,
                          Label = slab) %>%
            dplyr::select(all_of(cols.scores.log))

          if (j == 1) {
            df.scores.log <- df_plot.log_target
          } else {
            df.scores.log <- rbind(df.scores.log, df_plot.log_target)
          }

          # define filename for logistic regression plot
          fn_png_p2 <- paste0(TargetSiteID, str, "_", biocomm, "_", ssi.name,
                              "_VIreg.png")

          # Identify plot vars for logistic plots
          negStart <- 0
          negEnd   <- 0.2  # same as zeroStart
          zeroEnd  <- 0.5  # same as posStart
          posEnd   <- 1
          midNeg   <- ((negEnd - negStart) / 2) + negStart
          midZero  <- ((zeroEnd - negEnd) / 2) + negEnd
          midPos   <- ((posEnd - zeroEnd) / 2) + zeroEnd

          # plot logistic regressions ####
          legendtitle <- "Samples"
          ylabel <- paste0("Relative probability of degraded condition (", ssi.name, ")")
          maintitleSR <- paste0(TargetSiteID,
                                ": Verified prediction (logistic regression) line of evidence")
          subtitleSR <-"Are stressor-specific index levels sufficient to explain the observed impairment?"
          subtitleSR <- stringr::str_wrap(subtitleSR, 100)

          captionSR <- paste(paste0("All inside-the-case samples (n = ", n_cc_df_plot, ")."),
                             paste0("Score = ", paste(j_VPlog_score, collapse = ", "), "."),
                             sep = "\n")

          # Annotation values
          # Score = -1 runs from 0 to 0.20 on the y axis
          # Score = 0 runs from 0.20 to 0.50 on the y axis
          # Score = 1 runs from 0.50 to 1 on the y axis
          xmin <- min(df_plot.log$x, na.rm = TRUE)
          xmax <- max(df_plot.log$x, na.rm = TRUE)
          xseg <- xmax + (0.02 * xmax)

          target.vals <- unique(df_plot.log_target$StressorValue)

          msg <- paste0("printing logistic regression for ", str, " against ", ssi.name)
          message(msg)

          p2 <- ggplot2::ggplot(df_plot.log, ggplot2::aes(x = x, y = y.name)) +
            ggplot2::geom_point(ggplot2::aes(color = "black", shape = SSIqual,
                                             fill = SSIqual, size = SSIqual),
                                alpha = 0.5, size = 2, na.rm = TRUE) +
            ggplot2::geom_line(ggplot2::aes(y = y.name, x = x), data = newdat
                               , color = "black", lwd = 1, na.rm = TRUE) +
            ggplot2::scale_fill_manual(name = legendtitle,
                                       breaks = c("Not degraded", "Degraded"),
                                       values = bio_col[2:3], drop = FALSE) +
            ggplot2::scale_color_manual(name = legendtitle,
                                        breaks = c("Not degraded", "Degraded"),
                                        values = bio_col[2:3], drop = FALSE) +
            ggplot2::scale_shape_manual(name = legendtitle,
                                        breaks = c("Not degraded", "Degraded"),
                                        values = bio_shape[2:3], drop = FALSE) +
            ggplot2::scale_size_manual(name = legendtitle,
                                       breaks = c("Not degraded", "Degraded"),
                                       values = bio_size[2:3], drop = FALSE) +
            ggplot2::geom_vline(xintercept = target.vals, color = targ_line_col,
                                lty = targ_line_lty, lwd = targ_line_lwd,
                                na.rm = TRUE) +
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
            ggplot2::labs(y = ylabel, x = slab) +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
            ggplot2::labs(title = maintitleSR, subtitle = subtitleSR,
                          caption = captionSR)

          ggplot2::ggsave(filename = file.path(dir_path_stress, fn_png_p2),
                          plot = p2, dpi = plotdpi, width = plotW,
                          height = plotH, units = plotunits)

        }

      } # Completed logistic regression plot

      ## Prep boxplots ----
      ### Add quantiles ----
      df_plot1_stats <- df_plot %>%
        dplyr::filter(StationID != TargetSiteID) %>%
        dplyr::filter(Quality == "Not degraded") %>%
        dplyr::group_by(SSIndex) %>%
        dplyr::summarise(n = dplyr::n(),
                         Min = min(SSIValue, na.rm = TRUE),
                         q25 = quantile(SSIValue, probs = 0.25, na.rm = TRUE),
                         q50 = quantile(SSIValue, probs = 0.50, na.rm = TRUE),
                         q75 = quantile(SSIValue, probs = 0.75, na.rm = TRUE),
                         Max = max(SSIValue, na.rm = TRUE),
                         .groups = "drop_last")
      # Identify min/max index values for scoring purposes
      minVal <- df_plot1_stats$Min
      maxVal <- df_plot1_stats$Max

      # Merge quantiles back into df_plot
      df_plot1 <- merge(df_plot, df_plot1_stats, by = "SSIndex")
      df_plot1_target <- dplyr::filter(df_plot1, StationID == TargetSiteID)
      df_plot1_nottarget <- dplyr::filter(df_plot1, StationID != TargetSiteID) %>%
        dplyr::filter(Quality == "Not degraded")

      df_plot1 <- rbind(df_plot1_target, df_plot1_nottarget)

      ### Score SSI boxplots ----
      if (ssi.dir == "Dec") {##IF~TrendWIncStress == Dec~START
        # Inverse Scoring
        df.scores.i <- df_plot1_target %>%
          dplyr::mutate(Sc_VPSSI_box = dplyr::case_when(SSIValue > q50 ~ -1,
                                                        SSIValue < q25 ~ 1,
                                                        TRUE ~ 0)) %>%
          dplyr::select(StationID, IncaseCol, StressSampleID, StressSampleDate,
                        RespSampleID, RespSampleDate, BioComm, all_of(colBio),
                        Quality, SSIndex, SSIValue, Sc_VPSSI_box, n, Min, q25,
                        q50, q75, Max)

        # Identify the position of score labels relative to the arrow
        # Use first sample only, because otherwise return a list of all samples
        box_qHI <- df_plot1$q50[1]
        box_qLO <- df_plot1$q25[1]
        segNeg <- ((maxVal - box_qHI) / 2) + box_qHI
        segZero <- ((box_qHI - box_qLO) / 2) + box_qLO
        segPos <- ((box_qLO - minVal) / 2) + minVal

      } else { # ssi.dir = "Inc"
        # Regular Scoring
        df.scores.i <- df_plot1_target %>%
          dplyr::mutate(Sc_VPSSI_box = dplyr::case_when(SSIValue < q50 ~ -1,
                                                        SSIValue > q75 ~ 1,
                                                        TRUE ~ 0)) %>%
          dplyr::select(StationID, IncaseCol, StressSampleID, StressSampleDate,
                        RespSampleID, RespSampleDate, BioComm, all_of(colBio),
                        Quality, SSIndex, SSIValue, Sc_VPSSI_box, n, Min, q25,
                        q50, q75, Max)

        # Identify the position of score labels relative to the arrow
        # Use first sample only, because otherwise return a list of all samples
        box_qHI <- df_plot1$q75[1]
        box_qLO <- df_plot1$q50[1]
        segPos <- ((maxVal - box_qHI) / 2) + box_qHI
        segZero <- ((box_qHI - box_qLO) / 2) + box_qLO
        segNeg <- ((box_qLO - minVal) / 2) + minVal

      }##IF~j_in_InvSc~END

      scores <- unlist(as.vector(df.scores.i$Sc_VPSSI_box))
      lab.Score <- paste0("Score = ", paste0(scores, collapse = ", "))

      df.scores.i <- merge(df.scores.i, df.targetdata,
                           by = c("StationID", "StressSampleID", "StressSampleDate",
                                  "RespSampleID", "RespSampleDate"))
      df.scores.i <- df.scores.i %>%
        dplyr::select(StationID, IncaseCol, StressSampleID, StressSampleDate,
                      RespSampleID, RespSampleDate, BioComm, all_of(colBio),
                      Quality, Stressor, TransfValue, SSIndex, SSIValue,
                      Sc_VPSSI_box, n, Min, q25, q50, q75, Max)

      # Either create or append scores
      if (i == 1) {
        df.scores.box <- df.scores.i
      } else {
        df.scores.box <- rbind(df.scores.box, df.scores.i)
      }

      ### Create boxplot ----
      str_title <- paste0(TargetSiteID, ": Verified prediction line of evidence ",
                          "for ", ssi.label)
      str_title <- stringr::str_wrap(str_title, 100)
      if (ssi.dir == "Dec") { # metric has lower values with increased stress
        str_subtitle <- paste0("Do the data support the prediction that the ",
                               "stressor-specific index value will be ",
                               "lower than that observed at ", str_comp, "?")
      } else { # metric has higher values with increased stress
        str_subtitle <- paste0("Do the data support the prediction that the ",
                               "stressor-specific index value will be ",
                               "higher than that observed at ", str_comp, "?")
      }
      str_subtitle <- stringr::str_wrap(str_subtitle, 100)
      legendtitle <- "Samples*"
      str_xlab  <- ssi.name
      lab.N <- paste0("n = ", unique(df.scores.i$n))

      str_caption <- paste0("Inside-the-case samples with paired stressor/response ",
                            "samples (", lab.N, ").\n", lab.Score, ".\n*Sample ",
                            "quality rated based on overall biological index.")

      msg <- paste0("printing boxplot for ", ssi.name)
      message(msg)

      targetvals <- as.numeric(unlist(df.scores.i[, "SSIValue"]))
      i.Group <- as.numeric(unique(df.scores.i$IncaseCol))
      str_ylab <- paste0("Inside-the-case samples selected from ", incaseLabel,
                         " = ", i.Group)
      xseg <- i.Group + 0.5

      p1 <- ggplot2::ggplot(df_plot1, ggplot2::aes(y = SSIValue,
                                                   x = IncaseCol,
                                                   group = IncaseCol)) +
        ggplot2::geom_boxplot(data = df_plot1_nottarget, outliers = TRUE,
                              outlier.size = 0.5, na.rm = TRUE, staplewidth = 0.5) +
        ggplot2::coord_flip() +
        ggplot2::geom_hline(yintercept = targetvals, color = targ_line_col,
                            lty = targ_line_lty, lwd = targ_line_lwd, na.rm = TRUE) +
        # ggplot2::geom_hline(yintercept = c(box_qLO, box_qHI), color = "black",
        #                     lty = 2, na.rm = TRUE) +
        ggplot2::geom_jitter(ggplot2::aes(color = "black", shape = Quality,
                                          fill = Quality), alpha = 0.5,
                             na.rm = TRUE, width = 0.25, height = 0.01) +
        ggplot2::annotate("segment", y = minVal, yend = box_qLO, x = c(xseg, xseg),
                          color = "orange", linewidth = 0.7, alpha = 0.6,
                          arrow = grid::arrow(ends = "both", type = "open",
                                              length = grid::unit(0.2, "cm"))) +
        ggplot2::annotate("segment", y = box_qLO, yend = box_qHI, x = c(xseg, xseg),
                          color = "orange", linewidth = 0.7, alpha = 0.6,
                          arrow = grid::arrow(ends = "both", type = "open",
                                              length = grid::unit(0.2, "cm"))) +
        ggplot2::annotate("segment", y = box_qHI, yend = maxVal, x = c(xseg, xseg),
                          color = "orange", linewidth = 0.7, alpha = 0.6,
                          arrow = grid::arrow(ends = "both", type = "open",
                                              length = grid::unit(0.2, "cm"))) +
        ggplot2::annotate("text", x = xseg + 0.02, y = c(segNeg, segZero, segPos),
                          label = c(aLabNeg, aLabZero, aLabPos), color = "orange") +
        ggplot2::scale_color_manual(name = legendtitle,
                                    breaks = c("Not degraded", "Degraded"),
                                    values = bio_col[2:3], drop = TRUE) +
        ggplot2::scale_fill_manual(name = legendtitle,
                                   breaks = c("Not degraded", "Degraded"),
                                   values = bio_fill[2:3], drop = TRUE) +
        ggplot2::scale_shape_manual(name = legendtitle,
                                    breaks = c("Not degraded", "Degraded"),
                                    values = bio_shape[2:3], drop = TRUE) +
        ggplot2::labs(title = str_title, subtitle = str_subtitle,
                      caption = str_caption, y = ssi.label, x = str_ylab) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
                       plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10),
                       plot.caption = ggplot2::element_text(size = 8)) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank())

      for (j in seq_along(ssi.stressors)) {

        str <- ssi.stressors[j]

        # Write graphics directory ----
        out.dir <- dirname(dir_plots)
        out.folders <- c(out.dir, basename(dir_plots), TargetSiteID, biocomm, str)

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

        fn_png_p1 <- paste0(TargetSiteID, "_", str, "_", biocomm, "_", ssi.name,
                            "_VIbox.png")

        ggplot2::ggsave(filename = file.path(dir_path_stress, fn_png_p1),
                        plot = p1, dpi = plotdpi, width = plotW, height = plotH,
                        units = plotunits)
      } #End loop over stressors

    } ##FOR.SSI END

    if (exists("df.scores.log")) {
      fn.scores.log <- paste(TargetSiteID, biocomm, "VPSSILog", "Scores.tab", sep = "_")
      write.table(df.scores.log, file.path(dir.path, fn.scores.log), append = FALSE,
                  col.names = TRUE, row.names = FALSE, sep = "\t")
    }

    if (exists("df.scores.box")) {
      fn.scores.box <- paste(TargetSiteID, biocomm, "VPSSIBox", "Scores.tab", sep = "_")
      write.table(df.scores.box, file.path(dir.path, fn.scores.box), append = FALSE,
                  col.names = TRUE, row.names = FALSE, sep = "\t")
    }

  } ## END SSIs > 0

  # Scores ----
  df.scores <- merge(df.scores.box, df_stressinfo, by = c("Stressor", "SSIndex"))

  df.scores <- df.scores  %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                  RespSampleDate, BioComm, SSIndex, SSIValue, Label,
                  TransfValue, Sc_VPSSI_box) %>%
    dplyr::rename(Stressor = Label, StressorValue = TransfValue,
                  bioComm = BioComm, bioIndexName = SSIndex,
                  bioIndex = SSIValue, Score = Sc_VPSSI_box) %>%
    dplyr::mutate(LoE = "VP_SSIbox", Quality = NA) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                  RespSampleDate, bioComm, bioIndexName, bioIndex, Quality,
                  Stressor, StressorValue, LoE, Score)

  if (exists("df.scores.log")) {
    df.scores.log <- df.scores.log %>%
      dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                    RespSampleDate, BioComm, SSIndex, SSIValue, SSIqual, Label,
                    StressorValue, Sc_VPlog) %>%
      dplyr::rename(Stressor = Label, Score = Sc_VPlog, bioComm = BioComm,
                    bioIndex = SSIValue, bioIndexName = SSIndex,
                    Quality = SSIqual) %>%
      dplyr::mutate(LoE = "VP_SSIlog") %>%
      dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                    RespSampleDate, bioComm, bioIndexName, bioIndex, Quality,
                    Stressor, StressorValue, LoE, Score)
    df.scores <- rbind(df.scores, df.scores.log)
  }

  return(df.scores)

} ##FUNCTION.END
