#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Biological Stressor-Response Gradient Lines of Evidence
#'
#' @description Use linear regression to evaluate stressor-response gradients
#'              inside the case or outside the case.
#'
#' @details Biological (BMI, Algae, or Fish) stressor linear regressions.
#'
#' Requires packages dplyr, ggplot2, stringr, tidyr
#'
#' @param TargetSiteID Site ID
#' @param df_stressinfo stressor metadata for stressors considered candidate
#'                      causes of impairment at the target site
#' @param df_respinfo Bio metric metadata
#' @param df_respdata Biological metric data for all response samples
#' @param df_datapaired dataframe of matched biological response and stressor data.
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param bioindex Name of the biological index column
#' @param min_cases Minimum number of paired samples; samplim from CASTool_Metadata
#'                  Default = samplim.
#' @param p.val_cutoff p-value cutoff above which the slope is not considered significant
#'                     Default = 0.05
#' @param r2_cutoff r2 value below which the relationship has too much variance
#'                  Default = 0.1
#' @param plotvars colors, shapes, fills, and transparencies for each type (target,
#'                 not degraded, degraded, inside-the-case, and outside-the-case)
#' @param refOutline color of the reference sites outline
#' @param plotdpi standardized dpi for all plots
#' @param plotH standardized height for all plots
#' @param plotW standardized width for all plots
#' @param dir_plots Directory to save plots. Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function. Default = "StressorResponse"
#' @param boo_pred_warn Should warnings for prediction be suppressed. Default = TRUE.
#' @param boo_plot Boolean value to save plots. Default = TRUE.
#'
#' @return Writes one or more graphics depicting stressor-response relationships,
#'         a correlation tile plot, and two tab-delimited text files;
#'         stressor correlations and scores. Returns a dataframe containing
#'         target site scores for inside-the-case and outside-the-case.
#'
#' @examples
#' \dontrun{}
#' @export
getBioStressorResponses <- function(TargetSiteID,
                                    df_stressinfo,
                                    df_respinfo,
                                    df_respdata,
                                    df_datapaired,
                                    biocomm,
                                    bioindex,
                                    min_cases = samplim,
                                    p.val_cutoff = 0.05,
                                    r2_cutoff = 0.1,
                                    plotvars = data_plotvars,
                                    refOutline = refOutline_col,
                                    plotdpi,
                                    plotH,
                                    plotW,
                                    plotunits,
                                    dir_plots,
                                    dir_sub = "_WoE",
                                    boo_pred_warn = TRUE,
                                    boo_plot = TRUE) {##FUNCTION.START

  boo.DEBUG <- FALSE

  if (boo.DEBUG == TRUE) {
    TargetSiteID = TargetSiteID
    df_stressinfo = df_stressorMetadata
    df_respinfo = bioMetricInfo
    df_respdata = bioMetricData
    df_datapaired = df_PairedSRTransf
    biocomm = bioComm
    bioindex = bioIndex
    min_cases = samplim
    p.val_cutoff = 0.05
    r2_cutoff = 0.2
    plotvars = data_plotvars
    refOutline = refOutline_col
    plotdpi = plot_dpi
    plotH = plot_H
    plotW = plot_W
    plotunits = plot_units
    dir_plots = dir_results
    dir_sub = "_WoE"
    boo_pred_warn = TRUE
    boo_plot = TRUE
  }

  # Correlation file output header row
  cn_cor_pref <- c("StationID", "biocomm", "stressName", "stressLabel",
                   "respName","respLabel", "n", "statistic", "p.value",
                   "estimate", "r2")

  # Definitions ####
  biocomm <- toupper(biocomm)
  not_all_na <- function(x) {!all(is.na(x))}
  `%>%` <- dplyr::`%>%`

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

  # Plot vars, inside-the-case
  plotvarsIn  <- plotvars %>%
    dplyr::filter(Type %in% c("target", "insideND", "insideD"))
  bio_fill_in    <- rev(unlist(plotvarsIn$Fill)) # Degraded, Not degraded, Target
  bio_shape_in   <- rev(unlist(plotvarsIn$Shape)) # down triangle, circle, triangle
  bio_size_in    <- rev(unlist(plotvarsIn$Size)) # Degraded, Not degraded, Target
  bio_alpha_in   <- rev(unlist(plotvarsIn$Alpha)) # Degraded, Not degraded, Target

  # Plot vars, outside-the-case
  plotvarsOut  <- plotvars %>%
    dplyr::filter(Type %in% c("target", "outsideND", "outsideD"))
  bio_fill_out    <- rev(unlist(plotvarsOut$Fill)) # Degraded, Not degraded, Target
  bio_shape_out   <- rev(unlist(plotvarsOut$Shape)) # down triangle, circle, triangle
  bio_size_out    <- rev(unlist(plotvarsOut$Size)) # Degraded, Not degraded, Target
  bio_alpha_out   <- rev(unlist(plotvarsOut$Alpha)) # Degraded, Not degraded, Target

  # Merge other metrics into paired dataset with transformed stressor values
  df_datapaired <- merge(df_datapaired, df_respdata,
                       by = c("StationID", "RespSampleID", "RespSampleDate",
                              bioindex, "Quality"),
                       all.x = TRUE)
  df_datapaired <- df_datapaired %>% dplyr::select_if(not_all_na)

  df_SiteData <- df_datapaired[df_datapaired$StationID == TargetSiteID, ]
  df_CompData <- df_datapaired[df_datapaired$IncaseYN == 1, ]
  df_AllData <- df_datapaired[df_datapaired$OutcaseYN == 1 & df_datapaired$IncaseYN == 0, ]
  df_AllData <- unique(rbind(df_SiteData, df_AllData))

  df_CompNotDeg <- df_CompData[df_CompData$Quality == "Not degraded", ]
  df_AllNotDeg <- df_AllData[df_AllData$Quality == "Not degraded", ]

  # Get stressors and responses for looping over
  stressors <- as.vector(unlist(df_stressinfo$Stressor))
  BioResp <- as.vector(unlist(df_respinfo$MetricName))

  # compare stressors list with stressors in paired dataset
  stressors <- intersect(stressors, colnames(df_SiteData))
  BioResp <- intersect(BioResp, colnames(df_SiteData))

  ngraph <- 0

  #QC
  if (boo.DEBUG == TRUE) { ##IF.boo.DEBUG.START
    # p
    p <- 1
    #q
    q <- 1
  } ##IF.boo.DEBUG.END

  # move from plotting section
  #p
  p.len <- length(stressors)
  #q
  q.len <- length(BioResp)

  # boo.pryr <- FALSE

  # FOR.p ####
  for (p in 1:length(stressors)) {

    stressName <- stressors[p]
    varFlag <- 1
    varFlag.b <- 1

    log.yn <- as.logical(df_stressinfo$LogTransf[df_stressinfo$Stressor == stressName])

    if (log.yn == TRUE) {
      stressLabel <- as.character(df_stressinfo$Label[df_stressinfo$Stressor == stressName])
      stressLabel <- paste0("Log1p ", stressLabel)
    } else {
      stressLabel <- as.character(df_stressinfo$Label[df_stressinfo$Stressor == stressName])
    }

    # Write graphics directory ----
    out.dir <- dirname(dir_plots)
    out.folders <- c(out.dir, basename(dir_plots), TargetSiteID, biocomm, stressName)

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
    varFileOut <- file.path(dir_path_stress, paste0(TargetSiteID, "_", biocomm,
                                                    "_BioGrad_"))

    # DEBUG
    if (boo.DEBUG == TRUE) { ##IF.boo.DEBUG.START
      message(paste0("p; ", p, "; ", stressors[p]))
      flush.console()
    } ##IF.boo.DEBUG.END

    # FOR.q ####
    for (q in 1:length(BioResp)) {

      varFlag <- 1
      varFlag.b <- 1
      boo_corr <- TRUE
      boo_all <- TRUE
      respName <- BioResp[q]
      respLabel <- as.character(df_respinfo$MetricLabel[df_respinfo$MetricName == respName])
      pq <- q.len * (p - 1) + q
      pq.len <- p.len * q.len

      # Determine expected direction of slope
      dirIncStress <- unique(df_stressinfo$DirIncStress[df_stressinfo$Stressor == stressName])
      dirIncStress <- tolower(dirIncStress)
      dirRespBad <- unique(df_respinfo$TrendWIncStress[df_respinfo$MetricName == respName])
      dirRespBad <- tolower(dirRespBad)
      if (dirIncStress == "inc" & dirRespBad == "dec") {
        # increasing stressor decreases biological integrity when bad values are
        # lower than good values; slope should be negative
        exp.dir <- -1
      } else if (dirIncStress == "inc" & dirRespBad == "inc") {
        # increasing stressor decreases biological integrity when bad values are
        # higher than good values; slope should be positive
        exp.dir <- 1
      } else if (dirIncStress == "dec" & dirRespBad == "dec") {
        # decreasing stressor decreases biological integrity when bad values are
        # lower than good values; slope should be positive
        exp.dir <- 1
      } else { # (dirIncStress == "dec" & dirRespBad == "inc")
        # decreasing stressor decreases biological integrity when bad values are
        # higher than good values; slope should be negative
        exp.dir <- -1
      }

      # QC
      if (boo.DEBUG == TRUE) { ##IF.boo.DEBUG.START
        message(paste0("Item (", pq, "/", pq.len, ")"))
        message(paste0("q; ", q, "; ", respName))
        flush.console()
      } ##IF.boo.DEBUG.END

      # Create df_plot_all & QC
      df_plot_all <- df_AllData %>%
        dplyr::select(StationID, StressSampleID, RespSampleID, Quality,
                      RefSiteFlag, all_of(stressName), all_of(respName))
      df_plot_all <- df_plot_all[stats::complete.cases(df_plot_all), ]

      # QC
      if (nrow(df_plot_all) < min_cases) {
        txt.score <- paste0("< ", min_cases, " cases")
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName,
                             " (", p, "/", p.len, "), ", respName, " (", q,
                             "/", q.len, "); score = ", txt.score)
        message(msg.status)
        gapcomment <- txt.score
        gaps <- cbind.data.frame("getBioStressorResponse",
                                 "Number of complete cases (outside case)",
                                 nrow(df_plot_all),
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
        next
      }
      if (sum(is.na(df_plot_all[[stressName]])) == nrow(df_plot_all)) {
        txt.score <- "stressors all NA or NAN"
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName,
                             " (", p, "/", p.len, "), ", respName, " (", q,
                             "/", q.len, "); score = ", txt.score)
        message(msg.status)
        gapcomment <- txt.score
        gaps <- cbind.data.frame("getBioStressorResponse",
                                 "No stressor data (outside case)",
                                 nrow(df_plot_all),
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
        next
      }

      df_plot_all_ref <- df_plot_all %>%
        dplyr::filter(RefSiteFlag == 1) %>%
        dplyr::select(StationID, StressSampleID, RespSampleID, Quality,
                      RefSiteFlag, all_of(stressName), all_of(respName))
      df_plot_all_ref <- df_plot_all_ref[stats::complete.cases(df_plot_all_ref), ]

      #get all cluster data to plot
      df_plot_cl <- df_CompData %>%
        dplyr::select(StationID, StressSampleID, RespSampleID, Quality,
                      RefSiteFlag, all_of(stressName), all_of(respName))
      df_plot_cl <- df_plot_cl[stats::complete.cases(df_plot_cl), ]

      #get all cluster reference data
      df_plot_cl_ref <- df_plot_cl %>%
        dplyr::filter(RefSiteFlag == 1) %>%
        dplyr::select(StationID, StressSampleID, RespSampleID, Quality,
                      RefSiteFlag, all_of(stressName), all_of(respName))
      df_plot_cl_ref <- df_plot_cl_ref[stats::complete.cases(df_plot_cl_ref), ]

      #get target site data to plot
      df_plot_site <- df_SiteData %>%
        dplyr::select(StationID, StressSampleID, RespSampleID, Quality,
                      RefSiteFlag, all_of(stressName), all_of(respName))
      df_plot_site <- df_plot_site[stats::complete.cases(df_plot_site), ]

      # Check for missing data and write to data gaps file
      if ((nrow(df_plot_all) > 0) == FALSE) { # SHOULD NEVER HAPPEN
        gapcomment <- "No stressor data available for any sites in the outside-the-case dataset."
        gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
      }
      if ((nrow(df_plot_all_ref) > 0) == FALSE) {
        gapcomment <- paste0("No stressor data available for reference",
                             " sites in the outside-the-case dataset")
        gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
      }
      if ((nrow(df_plot_cl) > 0) == FALSE) { # SHOULD NEVER HAPPEN
        gapcomment <- "No stressor data available for any inside-the-case sites."
        gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
      }
      if ((nrow(df_plot_cl_ref) > 0) == FALSE) {
        gapcomment <- paste0("No stressor data available for reference",
                             " inside-the-case sites.")
        gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
      }
      if ((nrow(df_plot_site) > 0) == FALSE) { # SHOULD NEVER HAPPEN
        gapcomment <- "No stressor data available for the target site."
        gaps <- cbind.data.frame("getBioStressorResponse", stressName,
                                 0, gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
      }

      # QC for NA/NAN/Inf
      # 20190606, log1p of 0 or negative gives errors for linear model (lm) below.
      if (nrow(df_plot_all) > 0) { # SHOULD NEVER HAPPEN
        df_plot_all[!is.finite(df_plot_all[, stressName]), stressName]         <- NA
      }
      if (nrow(df_plot_all_ref) > 0) {
        df_plot_all_ref[!is.finite(df_plot_all_ref[, stressName]), stressName] <- NA
      }
      if (nrow(df_plot_cl) > 0) { # SHOULD NEVER HAPPEN
        df_plot_cl[!is.finite(df_plot_cl[, stressName]), stressName]           <- NA
      }
      if (nrow(df_plot_cl_ref) > 0) {
        df_plot_cl_ref[!is.finite(df_plot_cl_ref[, stressName]), stressName]   <- NA
      }
      if (nrow(df_plot_site) > 0) { # SHOULD NEVER HAPPEN
        df_plot_site[!is.finite(df_plot_site[, stressName]), stressName]       <- NA
      }

      # Cluster
      # LM and Corr, Cluster ####
      # ~~~ Check QC of Corr Table at end of code ~~~~
      if (nrow(df_plot_cl[complete.cases(df_plot_cl), ]) > 2) { ##IF~nrow(df_plot_cl)~START

        if (stats::sd(df_plot_cl[, stressName], na.rm = TRUE) == 0) { # Vertical line
          boo_corr <- FALSE

          gapcomment <- paste0("Stressor data in the comparator set have ",
                               "a standard deviation of zero: ",
                               "all values are equal.")
          gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                   gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
          fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                      row.names = FALSE, sep = "\t")

        } else if (stats::sd(df_plot_cl[, respName], na.rm = TRUE) == 0) { # Horizontal line
          boo_corr <- FALSE

          gapcomment <- paste0("Response data in the comparator set have ",
                               "a standard deviation of zero: ",
                               "all values are equal.")
          gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                   gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
          fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                      row.names = FALSE, sep = "\t")
        } else {  # SD <> 0 along vertical and horizontal

          # 20190228, QC for no data
          model_cl <- stats::lm(df_plot_cl[, respName] ~ df_plot_cl[, stressName],
                                na.action = na.exclude) #cluster only
          if (boo_pred_warn == TRUE) {
            suppressWarnings(model_cl_pred <- stats::predict(model_cl,
                                                             interval = "prediction",
                                                             level = 0.75))
          } else {
            model_cl_pred <- stats::predict(model_cl,
                                            interval = "prediction",
                                            level = 0.75)
          }
          model_cl_val  <- cbind(df_plot_cl, model_cl_pred) #predictions for all cluster values
          #
          slope_cl <- signif(summary(model_cl)$coefficients[[2]], 3)
          intercept_cl <- signif(summary(model_cl)$coefficients[[1]], 3)
          pval_intercept_cl <- signif(summary(model_cl)$coefficients[[7]], 3)
          pval_slope_cl <- signif(summary(model_cl)$coefficients[[8]], 3)
          # r2
          r_cl <- stats::cor(df_plot_cl[, respName], df_plot_cl[, stressName],
                             method = "pearson", use = "pairwise.complete.obs")
          r2_cl <- formatC(r_cl^2, format = "f", digits = 3)
          n_str_cl <- length(df_plot_cl[, stressName])
          # Correlation
          c1S_cl <- (stats::cor.test(df_plot_cl[, respName], df_plot_cl[, stressName],
                                     method = "pearson", use = "pairwise.complete.obs"))
          df.corr_cl <- data.frame(cbind(TargetSiteID, biocomm, stressName,
                                         stressLabel, respName, respLabel,
                                         c1S_cl$parameter + 1,
                                         signif(c1S_cl$statistic, 2),
                                         signif(c1S_cl$p.value, 2),
                                         signif(c1S_cl$estimate, 2),
                                         r2_cl))
          names(df.corr_cl) <- cn_cor_pref
          pval.corr_cl <- signif(c1S_cl$p.value, 2)
          #
          slope.dir_cl <- sign(slope_cl) # 1 = positive, -1 = negative
          #
        } # End std dev If eval

      } else { # <=2 rows of data
        boo_corr <- FALSE
        n_str_cl <- nrow(df_plot_cl)

        gapcomment <- paste0("Only two paired stressor-response samples ",
                             "are available for the comparator set.")
        gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")

      }##IF~nrow(df_plot_cl)~END

      # ALL
      # LM and Corr, All ####
      # ~~~ Check QC of Corr Table at end of code ~~~~
      if(nrow(df_plot_all[complete.cases(df_plot_all), ]) > 2) { ##IF~nrow(df_plot_cl)~START

        if(stats::sd(df_plot_all[, stressName], na.rm = TRUE) == 0) { # Vertical line
          boo_all <- FALSE
          gapcomment <- paste0("Stressor data across all sites in the ",
                               "outside-the-case set have a standard deviation ",
                               "of zero: all values are equal.")
          gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                   gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
          fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                      row.names = FALSE, sep = "\t")
        } else if(stats::sd(df_plot_all[, respName], na.rm = TRUE) == 0) {
          boo_all <- FALSE
          gapcomment <- paste0("Response data across all sites in the ",
                               "outside-the-case set have a standard deviation ",
                               "of zero: all values are equal.")
          gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                   gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
          fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                      row.names = FALSE, sep = "\t")
        } else {  # SD <> 0
          # 20190228, QC for no data
          boo_all <- TRUE
          model_all <- stats::lm(df_plot_all[, respName] ~ df_plot_all[, stressName],
                                 na.action = na.exclude) # outside the case
          if(boo_pred_warn == TRUE){
            suppressWarnings(model_all_pred <- stats::predict(model_all,
                                                              interval = "prediction",
                                                              level = 0.75))
          } else {
            model_all_pred <- stats::predict(model_all, interval = "prediction",
                                             level = 0.75)
          }
          model_all_val  <- cbind(df_plot_all, model_all_pred) #predictions for outside the case
          #
          slope_all <- signif(summary(model_all)$coefficients[[2]], 3)
          intercept_all <- signif(summary(model_all)$coefficients[[1]], 3)
          pval_intercept_all <- signif(summary(model_all)$coefficients[[7]], 3)
          pval_slope_all <- signif(summary(model_all)$coefficients[[8]], 3)
          # r2
          r_all <- stats::cor(df_plot_all[, respName], df_plot_all[, stressName],
                              method = "pearson", use = "pairwise.complete.obs")
          r2_all <- formatC(r_all^2, format = "f", digits = 3)
          n_str_all <- length(df_plot_all[, stressName])
          # Corelation
          c1S_all <- (stats::cor.test(df_plot_all[, respName], df_plot_all[, stressName],
                                      method = "pearson", use = "pairwise.complete.obs"))
          df.corr_all <- data.frame(cbind(TargetSiteID, biocomm, stressName,
                                          stressLabel, respName, respLabel,
                                          c1S_all$parameter + 1,
                                          signif(c1S_all$statistic, 2),
                                          signif(c1S_all$p.value, 2),
                                          signif(c1S_all$estimate, 2),
                                          r2_all))
          names(df.corr_all) <- cn_cor_pref
          pval.corr_all <- signif(c1S_all$p.value, 2)
          #
          # 20180621, scoring
          slope.dir_all <- sign(slope_all) #1 = positive, -1 = negative
          #
        } # End std dev If eval statement

      } else { # <=2 samples
        boo_all <- FALSE
        n_str_all <- nrow(df_plot_all)

        gapcomment <- paste0("Only two or fewer paired stressor-response samples ",
                             "are available for all sites in the outside-the-case dataset.")
        gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0,
                                 gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")

      }##IF~nrow(df_plot_all)~END

      # Corr table output ####
      # # Create results data frame
      # ~~~ Check QC of Corr Table at end of code ~~~~
      if (boo_corr == TRUE) { ##IF~boo_corr~START
        if (varFlag == 1) {  #First time through loop
          df.CorrTable <- df.corr_cl
        } else {
          df.CorrTable <- rbind(df.CorrTable, df.corr_cl) # if not first iteration then append
        } # IF, END
        boo.Append    <- TRUE
        boo.col.names <- FALSE
        if (pq==1) { ##IF~pq~START
          boo.Append    <- !boo.Append
          boo.col.names <- !boo.col.names
        } ##IF~pq~END

        fn_corr <- paste0(TargetSiteID, "_", biocomm, "_BG_InsideCorrs.tab")
        utils::write.table(df.CorrTable,
                           file.path(dir_path, fn_corr),
                           sep = "\t", quote = FALSE, row.names = FALSE,
                           col.names = boo.col.names, append = boo.Append)
        pval.corr = signif(c1S_cl$p.value, 2)
      }##IF~boo_corr~END
      #

      # Scoring, Inside the Case ####
      if (nrow(df_plot_site) > 0) { ##IF~nrow(df_plot_site)~END
        for (f in 1:nrow(df_plot_site)) { ##FOR~f~START
          # Score, inside the case
          # Generate scores based on slope, significance value, and r2
          if (boo_corr == TRUE) { # Cluster data should be scored
            if ((nrow(df_plot_cl) >= min_cases) &&
                (abs(pval.corr_cl) <= p.val_cutoff) && (r2_cl >= r2_cutoff)) { ##IF~length~START
              # print to console p (stressName) and q (respName)
              if (slope.dir_cl == exp.dir) {
                txt.score_cl <- "1"
                sr.score_cl = 1
              } else if (slope.dir_cl != exp.dir) {
                txt.score_cl <- "-1"
                sr.score_cl = -1
              } else {
                txt.score_cl <- "inconclusive"
                sr.score_cl = 0
              }
            } else {
              txt.score_cl <- "0"
              sr.score_cl = 0
            }##IF~length~START
          } else { # <=2 sites in cluster; cannot be scored
            txt.score_cl <- "NE"
            sr.score_cl = NA
          }

          # Score, all
          if (boo_all == TRUE) {
            if ((length(df_plot_all) >= min_cases) &&
                (abs(pval.corr_all) <= p.val_cutoff) && (r2_all >= r2_cutoff)) { ##IF~length~START
              # print to console p (stressName) and q (respName)
              if (slope.dir_all == exp.dir) {
                txt.score_all <-  "1"
                sr.score_all = 1
              } else if (slope.dir_all != exp.dir) {
                txt.score_all <- "-1"
                sr.score_all = -1
              } else {
                txt.score_all <- "inconclusive"
                sr.score_all = 0
              }
            } else {
              txt.score_all <- "0"
              sr.score_all = 0
            }##IF~length~START

          } else { # boo_all == FALSE
            txt.score_all <- "NE"
            sr.score_all = NA
          }

          #
        } ##FOR~f~END
        #if (boo.pryr==TRUE) {##IF.boo.pryr.START
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName,
                             " (", p, "/", p.len, "), ", respName, " (",
                             q, "/", q.len, "); score (outside; inside) = ",
                             txt.score_all, "; ", txt.score_cl)
        message(msg.status)
        #}##IF.boo.pryr.START

        df.temp2 <- as.data.frame(cbind("StationID" = TargetSiteID,
                                        "biocomm" = biocomm,
                                        "stressName" = stressName,
                                        "stressLabel" = stressLabel,
                                        "respName" = respName,
                                        "respLabel" = respLabel,
                                        "n_site" = length(df_plot_site),
                                        "n_comp" = n_str_cl,
                                        "SRLin_Score_inside" = sr.score_cl,
                                        "n_out" = n_str_all,
                                        "SRLin_Score_outside" = sr.score_all))

        if (varFlag.b == 1) { # First time through this loop
          df.sc.sr <- df.temp2
        } else {
          df.sc.sr <- rbind(df.sc.sr, df.temp2)
        }

        df.sc.sr$SRLin_Score_outside <- ifelse(!is.na(df.sc.sr$SRLin_Score_outside),
                                               as.character(df.sc.sr$SRLin_Score_outside),
                                               "NE")
        df.sc.sr$SRLin_Score_inside <- ifelse(!is.na(df.sc.sr$SRLin_Score_inside),
                                              as.character(df.sc.sr$SRLin_Score_inside),
                                              "NE")

        # Pivot longer site data before merging with df.sc.sr
        df_SiteDataStrLong <- df_SiteData %>%
          dplyr::select(StationID, StressSampleID, StressSampleDate,
                        RespSampleID, RespSampleDate, all_of(stressName)) %>%
          tidyr::pivot_longer(cols = c(all_of(stressName)),
                              names_to = "stressName",
                              values_to = "stressVal")
        df_SiteDataRespLong <- df_SiteData %>%
          dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                        RespSampleDate, all_of(respName), Quality) %>%
          tidyr::pivot_longer(cols = c(all_of(respName)), names_to = "respName",
                              values_to = "respVal")
        df_SiteDataLong <- merge(df_SiteDataStrLong, df_SiteDataRespLong,
                                 by = c("StationID", "StressSampleID",
                                        "StressSampleDate", "RespSampleID",
                                        "RespSampleDate"))
        rm(df_SiteDataStrLong, df_SiteDataRespLong)

        df.sc.sr <- merge(df_SiteDataLong, df.sc.sr,
                          by = c("StationID", "stressName", "respName"),
                          all.x = TRUE)
        df.sc.sr <- df.sc.sr %>%
          dplyr::mutate(SRLin_Score_inside = ifelse(is.na(stressVal),
                                                    "NE", SRLin_Score_inside),
                        SRLin_Score_outside = ifelse(is.na(stressVal),
                                                    "NE", SRLin_Score_outside)) %>%
          dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                        RespSampleDate, biocomm, stressName, stressLabel, stressVal,
                        respName, respLabel, respVal, Quality, n_site, n_comp,
                        SRLin_Score_inside, SRLin_Score_outside)

        #if(boo.pryr==TRUE){
        fn_scores <- paste0(TargetSiteID, "_", biocomm, "_BG_Scores.tab")
        fp_scores <- file.path(dir_path, fn_scores)

        boo.Append    <- TRUE
        boo.col.names <- FALSE
        if (file.exists(fp_scores) == FALSE) {
          # can't rely on pq==1 as that may not have data
          boo.Append    <- !boo.Append
          boo.col.names <- !boo.col.names
        }

        # Add biocomm, 20190425
        utils::write.table(df.sc.sr, fp_scores,
                           sep = "\t", quote = FALSE, row.names = FALSE,
                           col.names = boo.col.names, append = boo.Append)
        #}
        # Moved from inside FOR.f
      } else {
        txt.score_all <- "NE"
        txt.score_cl <- "NE"
        sr.score_all <- NA
        sr.score_cl <- NA
        # txt.score <- "No Data"
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName,
                             " (", p, "/", p.len, "), ", respName, " (",
                             q, "/", q.len, "); score (all; cluster) = ",
                             txt.score_all, "; ", txt.score_cl)
        message(msg.status)
      } ##IF~nrow(df_plot_site)~END
      #

      # Rename columns to generic "Stressor" and "Response" for easier plotting
      df_plot_all <- dplyr::rename(df_plot_all, Stressor = {{stressName}},
                                   Response = {{respName}}) %>%
        # dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, Target, Quality))
        dplyr::select(StationID, Stressor, Response, Quality, RefSiteFlag)
      xmin_all <- unique(min(df_plot_all$Stressor))
      xmax_all <- unique(max(df_plot_all$Stressor))
      df_plot_all_ref <- dplyr::rename(df_plot_all_ref, Stressor = {{stressName}},
                                       Response = {{respName}}) %>%
        dplyr::select(StationID, Stressor, Response, Quality, RefSiteFlag)
      model_all_val <- dplyr::rename(model_all_val, Stressor = {{stressName}},
                                     Response = {{respName}}) %>%
        dplyr::select(StationID, Stressor, Response, Quality, lwr, upr)
      df_plot_cl <- dplyr::rename(df_plot_cl, Stressor = {{stressName}},
                                  Response = {{respName}}) %>%
        dplyr::select(StationID, Stressor, Response, Quality, RefSiteFlag)
      xmin_cl <- unique(min(df_plot_cl$Stressor))
      xmax_cl <- unique(max(df_plot_cl$Stressor))
      df_plot_cl_ref <- dplyr::rename(df_plot_cl_ref, Stressor = {{stressName}},
                                      Response = {{respName}}) %>%
        dplyr::select(StationID, Stressor, Response, Quality, RefSiteFlag)
      model_cl_val <- dplyr::rename(model_cl_val, Stressor = {{stressName}},
                                    Response = {{respName}}) %>%
        dplyr::select(StationID, Stressor, Response, Quality, lwr, upr)
      df_plot_site <- dplyr::rename(df_plot_site, Stressor = {{stressName}},
                                    Response = {{respName}}) %>%
        dplyr::select(StationID, Stressor, Response, Quality, RefSiteFlag)


      ## Plot, inputs ####
      boo_plot_ref    <- ifelse(nrow(df_plot_all_ref[!is.na(df_plot_all_ref$Stressor), ]) > 0,
                                TRUE, FALSE)
      boo_plot_cl     <- ifelse(nrow(df_plot_cl[!is.na(df_plot_cl$Stressor), ]) > 0,
                                TRUE, FALSE)
      boo_plot_cl_ref <- ifelse(nrow(df_plot_cl_ref[!is.na(df_plot_cl_ref$Stressor), ]) > 0,
                                TRUE, FALSE)
      boo_plot_targ   <- ifelse(nrow(df_plot_site[!is.na(df_plot_site$Stressor), ]) > 0,
                                TRUE, FALSE)

      ## Plot, Variables, Strings
      str_title <- paste0(TargetSiteID, ": Stressor-Response (linear regression) line of evidence")
      str_subtitle1.in <- "Is there evidence of a biological gradient from inside the case?\n"
      str_subtitle1.out <- "Is there evidence of a biological gradient from outside the case?\n"
      str_subtitle2 <- "Linear regression with 75th percentile prediction interval"
      str_subtitle.in <- paste0(str_subtitle1.in, str_subtitle2)
      str_subtitle.out <- paste0(str_subtitle1.out, str_subtitle2)
      str_xlab  <- stressLabel
      str_ylab  <- respLabel
      # if then for equation
      if (sum(!is.na(df_plot_cl$Stressor)) > 2 || sum(!is.na(df_plot_cl$Response)) > 2) {
        ##IF.equation.START
        str_caption_cl <- paste(paste0("Regression (inside-the-case samples): ",
                                       "y = ", slope_cl, " x + ", intercept_cl),
                                paste0("r2 = ", r2_cl),
                                paste0("p-value = ", pval.corr_cl),
                                paste0("n = ", n_str_cl),
                                paste0("score = ", txt.score_cl),
                                sep = " ~ ")
      } else {
        str_caption_cl <- paste0("Regression (inside-the-case samples): ",
                                 "Fewer than 3 samples.")
      }##IF.equation.END
      #
      if (sum(!is.na(df_plot_all$Stressor)) > 2 || sum(!is.na(df_plot_all$Response)) > 2) {
        ##IF.equation.START
        str_caption_all <- paste(paste0("Regression (outside-the-case samples): ",
                                        "y = ", slope_all, " x + ", intercept_all),
                                 paste0("r2 = ", r2_all),
                                 paste0("p-value = ", pval.corr_all),
                                 paste0("n = ", n_str_all),
                                 paste0("score = ", txt.score_all),
                                 sep = " ~ ")
      } else {
        str_caption_all <- "Regression (outside-the-case): Fewer than 3 samples."
      } ##IF.equation.END
      #
      qualtext <- "not degraded*"
      str_caption_qual <- "*Samples rated not degraded."
      str_caption_all <- paste0(str_caption_all, "\n", str_caption_qual)
      leg_all_ref <- paste0("outside-the-case", qualtext)
      str_caption_cl <- paste0(str_caption_cl, "\n", str_caption_qual)
      leg_cl_ref <- paste0("inside-the-case ", qualtext)

      ## Plot, outside ####
      boo.Plot <- ifelse(nrow(df_plot_site) == 0, FALSE, TRUE)
      # skip plot if no data for target site
      if (boo.Plot == TRUE) { ##IF.boo.Plot.START
        # ggplot, main
        if (exists("model_all_val") & exists("df_plot_all")) {
          # Linear model (all data)
          p_SR_all <- ggplot2::ggplot() +
            ggplot2::geom_smooth(data = model_all_val,
                                 ggplot2::aes(x = Stressor, y = Response),
                                 method = lm,
                                 color = "black",
                                 fill = "black",
                                 alpha = 0.2,
                                 formula = y ~ x, # Added to avoid message
                                 show.legend = FALSE,
                                 na.rm = TRUE)
          p_SR_all <- p_SR_all +
            ggplot2::geom_line(data = model_all_val,
                               ggplot2::aes(x = Stressor, y = lwr),
                               color = "black",
                               linetype = "dashed",
                               show.legend = FALSE,
                               na.rm = TRUE)
          p_SR_all <- p_SR_all +
            ggplot2::geom_line(data = model_all_val,
                               ggplot2::aes(x = Stressor, y = upr),
                               color = "black",
                               linetype = "dashed",
                               show.legend = FALSE,
                               na.rm = TRUE)
          p_SR_all <- p_SR_all +
            ggplot2::geom_point(data = df_plot_all,
                                ggplot2::aes(x = Stressor, y = Response,
                                             color = Quality, fill = Quality,
                                             shape = Quality), alpha = 0.5,
                                na.rm = TRUE) +
            ggplot2::scale_fill_manual(name = "Quality",
                                       breaks = c("Degraded", "Not degraded"),
                                       values = bio_fill_out, drop = FALSE) +
            ggplot2::scale_color_manual(name = "Quality",
                                        breaks = c("Degraded", "Not degraded"),
                                        values = bio_fill_out, drop = FALSE) +
            ggplot2::scale_shape_manual(name = "Quality",
                                        breaks = c("Degraded", "Not degraded"),
                                        values = bio_shape_out, drop = FALSE)
        } #END regression and points

        # ggplot, point subsets
        # if (boo_plot_ref == TRUE) { ##IF~boo_plot_ref~START
        #   p_SR_all <- p_SR_all +
        #     ggplot2::geom_point(data = df_plot_all_ref,
        #                         ggplot2::aes(x = Stressor, y = Response,
        #                                      fill = Quality, shape = Quality),
        #                         color = refOutline, alpha = 0.5, na.rm = TRUE)
        # } else {
        #   p_SR_all <- p_SR_all +
        #     ggplot2::geom_blank(ggplot2::aes(color = ref_Outline))
        # } ##IF~boo_plot_ref~END
        #
        if (boo_plot_targ == TRUE) { ##IF~boo_plot_targ~START
          p_SR_all <- p_SR_all +
            ggplot2::geom_point(data=df_plot_site,
                                ggplot2::aes(x = Stressor, y = Response),
                                color = "black", shape = bio_shape_out[3],
                                fill = bio_fill_out[3], size = bio_size_out[3]*1.5,
                                na.rm = TRUE)
        } else {
          p_SR_all <- p_SR_all +
            ggplot2::geom_blank(ggplot2::aes(color = bio_fill_out[3],
                                             shape = bio_shape_out[3],
                                             fill = bio_fill_out[3]))
        } ##IF~boo_plot_targ~END

        if (grepl("^pH_a", stressName)) {
          if (pHlimLow >= xmin_all) {
            p_SR_all <- p_SR_all +
              ggplot2::geom_vline(ggplot2::aes(xintercept = pHlimLow,
                                               linetype = "pH lower limit"),
                                  color = "black", lty = 3)
          }
          if (pHlimHigh <= xmax_all) {
            p_SR_all <- p_SR_all +
              ggplot2::geom_vline(ggplot2::aes(xintercept = pHlimHigh,
                                               linetype = "pH upper limit"),
                                  color = "black", lty = 3)
          }
        }
        if (grepl("^DO", stressName) & (DOlim >= xmin_all)) {
          p_SR_all <- p_SR_all +
            ggplot2::geom_vline(xintercept = DOlim, color = "black", lty = 3)
        }

        # other
        p_SR_all <- p_SR_all +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                         plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
                         plot.caption = ggplot2::element_text(size = 6),
                         legend.title = ggplot2::element_text(size = 8),
                         legend.text = ggplot2::element_text(size = 6),
                         axis.title = ggplot2::element_text(size = 8)) +
          ggplot2::labs(title = str_title, subtitle = str_subtitle.out,
                        caption = str_caption_all, x = str_xlab, y = str_ylab)

        # Write biological gradient for all sites
        fn_png_out <- paste0(varFileOut, make.names(stressName), "_",
                             make.names(respName), "_Outside.png")
        if (boo_plot) {
          ggplot2::ggsave(fn_png_out, p_SR_all, width = plotW, height = plotH,
                          units = plotunits, dpi = plotdpi)
        } ## IF ~ boo_plot ~ END

        # if (respName == bioindex) {
        #   plotname <- paste0(stressName, "_", biocomm, "_GO")
        #   suppressWarnings(assign(plotname, p_SR_all))
        # }

        ## Plot, inside ####
        if (boo_plot_cl == TRUE) { ##IF~boo_plot_cl~START
          # Regression, cluster
          if (exists("model_cl_val") & exists("df_plot_cl")) {
            # Linear model (cluster)
            p_SR_cl <- ggplot2::ggplot() +
              ggplot2::geom_smooth(data = model_cl_val,
                                   ggplot2::aes(x = Stressor, y = Response),
                                   method = lm,
                                   color = "black",
                                   fill = "black",
                                   alpha = 0.2,
                                   formula = y ~ x, # Added to avoid message
                                   show.legend = FALSE,
                                   na.rm = TRUE)
            p_SR_cl <- p_SR_cl +
              ggplot2::geom_line(data = model_cl_val,
                                 ggplot2::aes(x = Stressor, y = lwr),
                                 color = "black",
                                 linetype = "dashed",
                                 show.legend = FALSE,
                                 na.rm = TRUE)
            p_SR_cl <- p_SR_cl +
              ggplot2::geom_line(data = model_cl_val,
                                 ggplot2::aes(x = Stressor, y = upr),
                                 color = "black",
                                 linetype = "dashed",
                                 show.legend = FALSE,
                                 na.rm = TRUE)
            p_SR_cl <- p_SR_cl +
              ggplot2::geom_point(data = df_plot_cl,
                                  ggplot2::aes(x = Stressor, y = Response,
                                               color = Quality, fill = Quality,
                                               shape = Quality), alpha = 0.5,
                                  na.rm = TRUE) +
              ggplot2::scale_fill_manual(name = "Quality",
                                         breaks = c("Degraded", "Not degraded"),
                                         values = bio_fill_in, drop = FALSE) +
              ggplot2::scale_color_manual(name = "Quality",
                                          breaks = c("Degraded", "Not degraded"),
                                          values = bio_fill_in, drop = FALSE) +
              ggplot2::scale_shape_manual(name = "Quality",
                                          breaks = c("Degraded", "Not degraded"),
                                          values = bio_shape_in, drop = FALSE)
          } #END regression and points
          #
          # if (boo_plot_cl_ref == TRUE) { ##IF~boo_plot_cl_ref~START
          #   p_SR_cl <- p_SR_cl +
          #     ggplot2::geom_point(data = df_plot_cl_ref,
          #                         ggplot2::aes(x = Stressor, y = Response,
          #                                      fill = Quality, shape = Quality),
          #                         color = refOutline, alpha = 0.5, na.rm = TRUE)
          # } else {
          #   p_SR_cl <- p_SR_cl +
          #     ggplot2::geom_blank(ggplot2::aes(color = ref_Outline))
          # } ##IF~boo_plot_cl_ref~END
          #
          if (boo_plot_targ == TRUE) { ##IF~boo_plot_targ~START
            p_SR_cl <- p_SR_cl +
              ggplot2::geom_point(data=df_plot_site,
                                  ggplot2::aes(x = Stressor, y = Response),
                                  color = "black", shape = bio_shape_in[3],
                                  fill = bio_fill_in[3], size = bio_size_in[3]*1.5,
                                  na.rm = TRUE)
          } else {
            p_SR_cl <- p_SR_cl +
              ggplot2::geom_blank(ggplot2::aes(color = "black",
                                               shape = bio_shape_in[3],
                                               fill = bio_fill_in[3]))
          } ##IF~boo_plot_targ~END

          if (grepl("^pH_a", stressName)) {
            if (pHlimLow >= xmin_cl) {
              p_SR_cl <- p_SR_cl +
                ggplot2::geom_vline(ggplot2::aes(xintercept = pHlimLow,
                                                 linetype = "pH lower limit"),
                                    color = "black", lty = 3)
            }
            if (pHlimHigh <= xmax_cl) {
              p_SR_cl <- p_SR_cl +
                ggplot2::geom_vline(ggplot2::aes(xintercept = pHlimHigh,
                                                 linetype = "pH upper limit"),
                                    color = "black", lty = 3)
            }
          }
          if (grepl("^DO", stressName) & (DOlim >= xmin_cl)) {
            p_SR_cl <- p_SR_cl +
              ggplot2::geom_vline(xintercept = DOlim, color = "black", lty = 3)
          }

          # other
          p_SR_cl <- p_SR_cl +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
                           plot.caption = ggplot2::element_text(size = 6),
                           legend.title = ggplot2::element_text(size = 8),
                           legend.text = ggplot2::element_text(size = 6),
                           axis.title = ggplot2::element_text(size = 8)) +
            ggplot2::labs(title = str_title, subtitle = str_subtitle.in,
                          caption = str_caption_cl, x = str_xlab, y = str_ylab)
          #
          # Write biological gradient for all sites
          fn_png_in <- paste0(varFileOut, make.names(stressName), "_",
                              make.names(respName), "_Inside.png")
          if (boo_plot) {
            ggplot2::ggsave(fn_png_in, p_SR_cl, width = plotW, height = plotH,
                            units = plotunits, dpi = plotdpi)
            ngraph = ngraph + 1
          } ## IF ~ boo_plot ~ END
          #
          # if (respName == bioindex) {
          #   plotname <- paste0(stressName, "_", biocomm, "_GI")
          #   suppressWarnings(assign(plotname, p_SR_cl))
          # }

        } ##IF.boo.Plot.END

      } ##IF.boo.plot.TRUE.END

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      varFlag <- 0
      varFlag.b <- 0 # Set varFlag.b to zero

    } ##FOR.q.END

  } ##FOR.p.END

  ## END LR plots ####
  #
  # CorrPlot ####
  ## read
  if (boo_corr==TRUE) {
    fn_corr <- paste0(TargetSiteID, "_", biocomm, "_BG_InsideCorrs.tab")
    fp_corr <- file.path(dir_path, fn_corr)

    if (file.exists(fp_corr)==TRUE) {
      df_corr <- utils::read.delim(fp_corr)

      # Columns: c("StationID", "biocomm", "stressName", "respName",
      # "n", "statistic", "p.value", "estimate", "r2")
      cn_cor_x    <- colnames(df_corr)
      cn_cor_match <- sum(cn_cor_x %in% cn_cor_pref)
      if (cn_cor_match != length(cn_cor_pref)) { ##IF~length~START
        df_corr <- utils::read.delim(fp_corr, header = FALSE, col.names = cn_cor_pref)
        utils::write.table(df_corr, fp_corr, sep = "\t", quote = FALSE, row.names = FALSE)
      } ##IF~length~END

      df_corr <- unique(df_corr) %>% dplyr::rename(Estimate = estimate)

      # Plot, Variables, Strings
      str_title <- paste0(TargetSiteID, ": Stressor-Response Correlations")
      str_ylab  <- "Stressors"
      str_xlab  <- "Responses"
      wrap_length <- 28
      # Create plot
      p_cp <- ggplot2::ggplot(df_corr, ggplot2::aes(x = stringr::str_wrap(respLabel, wrap_length),
                                       y = stringr::str_wrap(stressLabel, wrap_length),
                                       fill = Estimate)) +
        ggplot2::geom_tile(color = "black", lwd = 1, linetype = 1) +
        ggplot2::geom_text(ggplot2::aes(label = Estimate), color = "black", size = 2.25) +
        ggplot2::scale_fill_gradient2(low = "blue", high = "red", midpoint = 0,
                                      limits = c(-1, 1), guide = "colorbar") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
                       legend.title = ggplot2::element_text(size = 8),
                       legend.text = ggplot2::element_text(size = 6),
                       axis.title = ggplot2::element_text(size = 10),
                       axis.text.x = ggplot2::element_text(size = 5, angle = 45,
                                                           hjust = 1),
                       axis.text.y = ggplot2::element_text(size = 5, angle = 30)) +
        ggplot2::labs(title = str_title, x = str_xlab, y = str_ylab)

      # Save correlation plot
      fn_png_cp <- file.path(dir_path,
                             paste0(TargetSiteID, "_", biocomm, "_InsideCorrPlot.png"))
      ggplot2::ggsave(fn_png_cp, p_cp, width = plotH, height = plotW,
                      units = plotunits, dpi = plotdpi)

      msg.corr <- "Printing correlation plot."
      message(msg.corr)
    } ## END Read corr file
  } ## END create corrplot

  # Scores ----
  df.scores <- read.delim(fp_scores, header = TRUE, na.strings = c("", "NA"),
                          strip.white = TRUE, stringsAsFactors = FALSE)

  df.scores <- df.scores %>%
    dplyr::filter(respName == bioindex) %>%
    dplyr::rename(bioComm = biocomm, bioIndex = respVal, Stressor = stressLabel,
                  StressorValue = stressVal) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("SR"), names_to = "LoE",
                        values_to = "Score")  %>%
    dplyr::mutate(LoE = ifelse(LoE == "SRLin_Score_inside", "Gradient (inside)",
                               "Gradient (outside)"),
                  bioIndexName := {{bioindex}}) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                  RespSampleDate, bioComm, bioIndexName, bioIndex, Quality,
                  Stressor, StressorValue, LoE, Score)

  return(df.scores)

} ##FUNCTION.END
