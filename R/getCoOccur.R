#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Co-Occurrence and Sufficiency Lines of Evidence
#'
#' @description Generates box plots and and logistic regressions to answer the
#'              questions: 1) are the observed stressor levels consistent with
#'              impairment where and when it occurs? and 2) are stressor levels
#'              sufficient to explain the observed impairment? Also writes a
#'              tab-delimited text file containing scores for these two lines of
#'              evidence.
#'
#' @details \strong{Derive evidence for spatial/temporal co-occurrence.}
#'
#' Stressor-response from field observational studies: Are higher levels of the
#' stressor observed where and when the biological effect occurs?
#'
#' Box plots are used to show the distribution of the stressor levels at comparator
#' sites with better biological condition.  If a site has multiple biological
#' condition scores the lowest score is used to determine "better" sites.
#'
#' Samples are scored:
#'
#' 1. Supports the case for candidate cause.  Stressor levels at the test sites
#' are above the 75th percentile of comparator sites having higher biological
#' quality.
#'
#' 0. Indeterminate.  Stressor levels at the test site are below the 50th
#' percentile of comparator sites having higher biological quality.
#'
#' -1. Weakens the case for the candidate cause.  Stressor levels at the test
#' sites are between the 50th and 75th percentile of comparator sites having
#' higher biological quality.
#'
#' Multiple stressors can be used. Stressors for which all target samples are
#' scored -1 are not included in further lines of evidence evaluation.
#'
#' Uses the libraries dplyr, wrapr, ggplot2, and gridExtra.
#'
#' @param TargetSiteID ID of station/sample(s) to plot
#' @param df_data data frame with paired stressor/response data, where stressor
#'                data are transformed.
#' @param detects all stressors detected in any samples from the target site
#' @param df_stressinfo dataframe containing stressor metadata (e.g., Label,
#'                      DirIncStress, and other columns from the stressor
#'                      information metadata file).
#' @param compsites vector of comparator sites. Defaults to list.CompSites$comp.sites
#' @param biocomm Biological community; fish, algae, or BMI.  Default = "BMI".
#' @param colBio df_data column with biological index numeric value.
#' @param pHlimLow The lower limit of pH considered to be supportive of a
#'                 biological community. Defaults to pH of 6.5.
#' @param pHlimHigh The upper limit of pH considered to be supportive of a
#'                  biological community. Defaults to pH of 9.
#' @param DOlim The lower limit of DO in mg/L considered to be supportive of a
#'              biological community. Defaults to 7 mg/L
#' @param plotvars Colors, fills, shapes, transparencies for each type (target,
#'                 not degraded, degraded, inside-the-case, outside-the-case).
#'                 Default = data_plotvars.
#' @param plotdpi DPI for plots for standardization. Default = plot_dpi.
#' @param plotH Plot height for standardization. Default = plot_H.
#' @param plotW Plot width for standardization. Default = plot_W.
#' @param plotunits Plot units for standardization. Default = plot_units.
#' @param dir_plots Directory to save plots.  Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "CoOccurrence"
#' @param boo_plot Boolean value to save plots.  Default = TRUE.
#'
#' @return Writes individual plots as pngs, and a tab-delimited text file with
#'         scores for each line of evidence (co-occurrence & sufficiency) to a
#'         "Results/TargetSiteID/BioComm/CoOccurrence" directory. Returns a list
#'         containing 3 elements: a dataframe of stressor metadata corresponding
#'         with stressors continuing forward in the analysis, a vector of
#'         stressors not continuing forward in the analysis, and a dataframe
#'         of all the scores continuing forward.
#'
#' @examples
#' \dontrun{
#'}
#' @export
getCoOccur <- function(TargetSiteID,
                       df_data,
                       detects,
                       df_stressinfo,
                       compsites = list.CompSites$comp.sites,
                       biocomm,
                       colBio,
                       pHlimLow = 5,
                       pHlimHigh = 9,
                       DOlim = 6,
                       plotvars = data_plotvars,
                       plotdpi = plot_dpi,
                       plotH = plot_H,
                       plotW = plot_W,
                       plotunits = plot_units,
                       dir_plots = dir_results,
                       dir_sub = "CoOccurrence",
                       boo_plot = TRUE) {##FUNCTION.START

  boo_DEBUG <- FALSE

  if (boo_DEBUG==TRUE) {
    TargetSiteID = TargetSiteID
    df_data = df_PairedSRTransf
    detects = siteDetectsAll
    df_stressinfo = data_stressInfo
    compsites = list.CompSites$comp.sites
    biocomm = bioComm
    colBio = bioIndex
    pHlimLow = pHlimLow
    pHlimHigh = pHlimHigh
    DOlim = DOlim
    plotvars = data_plotvars
    plotdpi = plot_dpi
    plotH = plot_H
    plotW = plot_W
    plotunits = plot_units
    dir_plots = dir_results
    dir_sub = "CoOccurrence"
    boo_plot = TRUE
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)
  not_all_na <- function(x) {!all(is.na(x))}

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

  ## Plot, Variables
  # Define generic plot variables ----
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
  targ_line_lwd <- 1

  # arrow labels
  aLabPos <- "1"
  aLabZero <- "0"
  aLabNeg <- "-1"

  ## Limit line types (dotted)
  lim_lty <- 3

  # Start evaluation ----
  # Prep metadata, 20250330 --
  df_stressinfo <- dplyr::rename(df_stressinfo, Stressor = StdParamName) %>%
    dplyr::filter(Stressor %in% detects)
  colStressors <- as.vector(unlist(df_stressinfo$Stressor))

  # Prep data, 20250330 --
  df_data <- dplyr::select(df_data, StationID, StressSampleID, StressSampleDate,
                           IncaseCol, OutcaseCol, RespSampleID, RespSampleDate,
                           RefSiteFlag, BetterThan, all_of(colBio), Quality,
                           all_of(detects)) %>%
    dplyr::filter(StationID %in% c(TargetSiteID, compsites))

  ## Create Score Output File ####
  df.scores <- cbind(df_data[0, c("StationID", "IncaseCol", "StressSampleID",
                                  "StressSampleDate", "RespSampleID",
                                  "RespSampleDate", colBio, "Quality")],
                     data.frame(Stressor = character(), StressorValue = double(),
                                n = integer(), q25 = double(), q50 = double(),
                                q75 = double(), Sc_Box = integer(),
                                biocomm = character(), Label = character(),
                                stringsAsFactors = FALSE))

  # Filter for only not degraded samples plus target samples
  df.target <- dplyr::filter(df_data, StationID == TargetSiteID)
  df.comp <- dplyr::filter(df_data, Quality == "Not degraded")
  df.comp <- dplyr::filter(df.comp, StationID != TargetSiteID)
  df.comp <- rbind(df.target, df.comp)

  df.comp <- dplyr::select_if(df.comp, not_all_na)
  detects <- intersect(detects, colnames(df.comp))

  df.stats <- df.comp %>%
    dplyr::select(all_of(detects)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Stressor",
                        values_to = "StressorValue") %>%
    dplyr::mutate(NotNA = ifelse(!is.na(StressorValue), 1, 0)) %>%
    dplyr::group_by(Stressor) %>%
    dplyr::summarize(n = sum(NotNA, na.rm = TRUE),
                     q25 = quantile(StressorValue, probs = 0.25, na.rm = TRUE),
                     q50 = quantile(StressorValue, probs = 0.50, na.rm = TRUE),
                     q75 = quantile(StressorValue, probs = 0.75, na.rm = TRUE),
                     minVal = min(StressorValue, na.rm = TRUE),
                     maxVal = max(StressorValue, na.rm = TRUE))

  df.i <- dplyr::filter(df.comp, StationID == TargetSiteID)
  i.Group <- df.i[1, "IncaseCol"]

  # Loop over detects quantiles ####
  for (j in seq_along(detects)) {##FOR.j.START
    #
    stressname <- detects[j]
    j.len <- length(detects)
    #
    message(paste0("Processing stressor (", j, "/", j.len, ") ",
                   stressname, ".\n"))
    utils::flush.console()

    # Comp Score for box plot
    colInvScore <- df_stressinfo %>%
      dplyr::filter(Stressor == stressname) %>%
      dplyr::select(DirIncStress)
    colInvScore <- as.character(colInvScore)

    stresslabel <- df_stressinfo %>%
      dplyr::filter(Stressor == stressname) %>%
      dplyr::mutate(stresslabel = ifelse(LogTransf == 1,
                                         paste0("Log1p ", Label),
                                         Label)) %>%
      dplyr::select(stresslabel)
    stresslabel <- as.character(stresslabel)

    # Select only necessary columns
    df.j <- df.i %>%
      dplyr::select(StationID, IncaseCol, StressSampleID, StressSampleDate,
                    RespSampleID, RespSampleDate, all_of(colBio), Quality,
                    all_of(stressname)) %>%
      dplyr::rename(StressorValue = {{stressname}}) %>%
      dplyr::mutate(Stressor := {{stressname}}) %>%
      dplyr::mutate(n = df.stats$n[df.stats$Stressor == stressname],
                    q25 = df.stats$q25[df.stats$Stressor == stressname],
                    q50 = df.stats$q50[df.stats$Stressor == stressname],
                    q75 = df.stats$q75[df.stats$Stressor == stressname]) %>%
      dplyr::filter(!is.na(StressorValue))

    ## Score samples ####
    ## Use different criteria for some parameters (Specifically pH and DO)
    ## Score pH in both directions
    if (stressname == "pH_alkEnv") { # pH is a decreaser in alkalkine environments
      # Parameter is pH; need two scores, one for acid environments & one for alkaline
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(StressorValue < pHlimLow ~ 1,
                                                StressorValue < q25 ~ 1,
                                                StressorValue > q50 ~ -1,
                                                TRUE ~ 0))
    } else if (stressname == "pH_acidicEnv") {
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(StressorValue > pHlimHigh ~ 1,
                                                StressorValue > q75 ~ 1,
                                                StressorValue < q50 ~ -1,
                                                TRUE ~ 0))
    } else if (grepl("^DO", stressname, perl = TRUE, ignore.case = FALSE) == TRUE) {
      #Parameter is DO; If values are < DOlim, then 1 by definition
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(StressorValue < DOlim ~ 1,
                                                StressorValue > q50 ~ -1,
                                                StressorValue < q25 ~ 1,
                                                TRUE ~0))
    } else if (colInvScore == "Dec") {
      # Inverse Scoring
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(StressorValue > q50 ~ -1,
                                                StressorValue < q25 ~ 1,
                                                TRUE ~0))
    } else {
      # Regular Scoring
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(StressorValue > q75 ~ 1,
                                                StressorValue < q50 ~ -1,
                                                TRUE ~ 0))
    }

    # Append scores to table
    cols <- colnames(df.scores)
    df.j <- df.j %>%
      dplyr::mutate(biocomm = "BMI",
                    Label = stresslabel) %>%
      dplyr::select(all_of(cols))

    df.scores <- rbind(df.scores, df.j)

    ## Box Plot of Not Degraded Comparator sites
    scores <- unlist(as.vector(df.j$Sc_Box))
    lab.Score <- paste0("Score = ", paste0(scores, collapse = ", "))
    lab.N <- paste0("n = ", unique(df.j$n))

    # File Names
    fn_png_p1 <- paste0(TargetSiteID, "_", biocomm, "_CoOccur_",
                        make.names(stressname), ".png")

    # Create (ggplot)
    lab_comp <- paste0("Comparator samples selected from ", incaseLabel,
                       " = ", i.Group)

    # scoring lines
    maxVal <- df.stats$maxVal[df.stats$Stressor == stressname]
    minVal <- df.stats$minVal[df.stats$Stressor == stressname]
    if (colInvScore == "Dec") {##IF~j_in_InvSc~START
      # Inverse Scoring
      box_qHI <- df.stats$q50[df.stats$Stressor == stressname]
      box_qLO <- df.stats$q25[df.stats$Stressor == stressname]
      segNeg <- ((maxVal - box_qHI) / 2) + box_qHI
      segZero <- ((box_qHI - box_qLO) / 2) + box_qLO
      segPos <- ((box_qLO - minVal) / 2) + minVal
    } else {
      # Regular Scoring
      box_qHI <- df.stats$q75[df.stats$Stressor == stressname]
      box_qLO <- df.stats$q50[df.stats$Stressor == stressname]
      segPos <- ((maxVal - box_qHI) / 2) + box_qHI
      segZero <- ((box_qHI - box_qLO) / 2) + box_qLO
      segNeg <- ((box_qLO - minVal) / 2) + minVal
    }##IF~j_in_InvSc~END

    legendtitle <- "Samples"
    maintitleCO <- paste0(TargetSiteID, ": Co-occurrence line of evidence")
    subtitleCO <-"Are the observed stressor levels consistent with impairment where and when it occurs?"
    subtitleCO <- stringr::str_wrap(subtitleCO, 100)

    # plot1, ggplot ####
    df.plot <- df.comp
    lab.sub <- paste0("Comparator samples with paired stressor/response samples",
                      " (", lab.N, ").\n", lab.Score, ".")
    lab.sub <- stringr::str_to_sentence(paste0("Not degraded ", lab.sub))

    targetvals <- as.numeric(unlist(df.j[, "StressorValue"]))
    xseg <- i.Group + 0.5

    p1 <- ggplot2::ggplot(df.plot, ggplot2::aes(y = .data[[stressname]],
                                                x = IncaseCol,
                                                group = IncaseCol)) +
      ggplot2::geom_boxplot(outliers = TRUE, outlier.size = 0.5, na.rm = TRUE,
                            staplewidth = 0.5) +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = targetvals, color = targ_line_col,
                          lty = targ_line_lty, lwd = targ_line_lwd, na.rm = TRUE) +
      ggplot2::geom_hline(yintercept = c(box_qLO, box_qHI), color = "black",
                          lty = 2, na.rm = TRUE) +
      ggplot2::geom_jitter(ggplot2::aes(color = Quality, shape = Quality,
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
                                  breaks = c("Degraded", "Not degraded"),
                                  values = bio_col, drop = TRUE) +
      ggplot2::scale_fill_manual(name = legendtitle,
                                 breaks = c("Degraded", "Not degraded"),
                                 values = bio_col, drop = TRUE) +
      ggplot2::scale_shape_manual(name = legendtitle,
                                  breaks = c("Degraded", "Not degraded"),
                                  values = bio_shp, drop = TRUE) +
      ggplot2::labs(title = maintitleCO, subtitle = subtitleCO,
                    caption = lab.sub, y = stresslabel, x = lab_comp) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())

    if (grepl("^pH_a", stressname, perl = TRUE, ignore.case = FALSE)) {
      p1 + ggplot2::geom_hline(yintercept = pHlimHigh, color = "burlywood4",
                               lty = lim_lty) +
        ggplot2::geom_hline(yintercept = pHlimLow, color = "burlywood4",
                            lty = lim_lty)
    }
    if (grepl("^DO", stressname, perl = TRUE, ignore.case = FALSE) == TRUE) {
      p1 + ggplot2::geom_hline(yintercept = DOlim, color = "burlywood4",
                               lty = lim_lty)
    }

    ggplot2::ggsave(filename = file.path(dir_path, fn_png_p1), plot = p1,
                    dpi = plotdpi, width = plotW, height = plotH, units = plotunits)

  }##FOR.j.END

  # Identify stressors ####
  fn.scores <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm,
                                          "_CO_Scores.tab"))
  utils::write.table(df.scores, file = fn.scores, col.names = TRUE,
                     row.names = FALSE, sep = "\t", append = FALSE)

  stressors <- unique(df.scores$Stressor[df.scores$Sc_Box != -1])
  notstressors <- unique(df.scores$Stressor[df.scores$Sc_Box == -1])
  notstressors <- setdiff(notstressors, stressors)
  # prevents a stressor identified by 1 sample from appearing in the "notstressors" vector

  # if notstressors has more than one row, write data to data gaps
  if (length(notstressors) > 0) {
    for (s in seq_along(notstressors)) {
      notstress <- notstressors[s]
      msg <- paste0(notstress, " identified as not a candidate cause for ",
                    TargetSiteID," for the ", bioComm, " community.")
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      gaps <- cbind.data.frame("getCoOccur", paste0(notstress, " score for ",
                                                    bioComm, " refutes"),
                               -1, msg)

      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")
    }
  } ### End no stressors statement

  # Prep df.scores for export to include in df_LoEs
  df.scores <- dplyr::filter(df.scores, Stressor %in% stressors) %>%
    dplyr::select(!Stressor) %>%
    dplyr::rename(bioComm = biocomm, bioIndex = all_of(colBio), Score = Sc_Box,
                  Stressor = Label) %>%
    dplyr::mutate(LoE = "CO", bioIndexName := {{colBio}}) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                  RespSampleDate, bioComm, bioIndexName, bioIndex, Quality,
                  Stressor, StressorValue, LoE, Score)

  df.stressorMetadata <- dplyr::filter(df_stressinfo, Stressor %in% stressors)
  return(list(df_stressorMetadata = df.stressorMetadata,
              notEvaluated = notstressors,
              df_COscores = df.scores))

}##FUNCTION.END
