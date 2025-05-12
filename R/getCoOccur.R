#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.2
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
#' @param incaseLabel A label describing the inside-the-case samples.
#' @param biocomm Biological community; fish, algae, or BMI.  Default = "BMI".
#' @param colBio df_data column with biological index numeric value.
#' @param useBetter Boolean flag for whether or not to use samples scoring better
#'                  than the maximum degraded target sample or the minimum not
#'                  degraded sample, if none of the target samples are degraded.
#'                  Defaults to FALSE.
#' @param df_stressinfo dataframe containing stressor metadata (e.g., Label,
#'                      DirIncStress, and other columns from the stressor
#'                      information metadata file).
#' @param pHlimLow The lower limit of pH considered to be supportive of a
#'                 biological community. Defaults to pH of 6.5.
#' @param pHlimHigh The upper limit of pH considered to be supportive of a
#'                  biological community. Defaults to pH of 9.
#' @param DOlim The lower limit of DO in mg/L considered to be supportive of a
#'              biological community. Defaults to 7 mg/L
#' @param dir_plots Directory to save plots.  Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "CoOccurrence"
#' @param boo_plot Boolean value to save plots.  Default = TRUE.
#'
#' @return Writes individual plots as pngs, and a tab-delimited text file with
#'         scores for each line of evidence (co-occurrence & sufficiency) to a
#'         "Results/TargetSiteID/BioComm/CoOccurrence" directory.
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
                       onlyNotDeg = TRUE,
                       useBetter = FALSE,
                       pHlimLow = 5,
                       pHlimHigh = 9,
                       DOlim = 6,
                       plotvars,
                       plot_dpi,
                       plot_H,
                       plot_W,
                       plot_units,
                       dir_plots = file.path(getwd(), "Results"),
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
    onlyNotDeg = onlyNotDeg
    useBetter = useBetter
    pHlimLow = pHlimLow
    pHlimHigh = pHlimHigh
    DOlim = DOlim
    plotvars = data_plotvars
    plot_dpi = plot_dpi
    plot_H = plot_H
    plot_W = plot_W
    plot_units = plot_units
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

  # Create Score Output File ####
  df.scores <- cbind(df_data[0, c("StationID", "IncaseCol", "StressSampleID",
                                  "RespSampleID", colBio, "Quality")],
                     data.frame(Param_Name = character(), Param_Value = double(),
                                n = integer(), q25 = double(), q50 = double(),
                                q75 = double(), Sc_Box = integer(),
                                biocomm = character(), Label = character(),
                                stringsAsFactors = FALSE))

  if (useBetter == TRUE) {
    # Subset df_data for comparator sites having better biology
    df.comp <- df_data %>%
      dplyr::filter(BetterThan == 1)
  } else {
    # Subset df_data for comparator sites
    df.comp <- df_data
  }

  # Filter for only not degraded samples plus target samples
  if (onlyNotDeg == TRUE) {
    df.target <- dplyr::filter(df.comp, StationID == TargetSiteID)
    df.comp <- dplyr::filter(df.comp, Quality == "Not degraded")
    df.comp <- dplyr::filter(df.comp, StationID != TargetSiteID)
    df.comp <- rbind(df.target, df.comp)
  }

  df.comp <- dplyr::select_if(df.comp, not_all_na)
  detects <- intersect(detects, colnames(df.comp))

  df.stats <- df.comp %>%
    dplyr::select(all_of(detects)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Param_Name",
                        values_to = "Param_Value") %>%
    dplyr::mutate(NotNA = ifelse(!is.na(Param_Value), 1, 0)) %>%
    dplyr::group_by(Param_Name) %>%
    dplyr::summarize(n = sum(NotNA, na.rm = TRUE),
                     q25 = quantile(Param_Value, probs = 0.25, na.rm = TRUE),
                     q50 = quantile(Param_Value, probs = 0.50, na.rm = TRUE),
                     q75 = quantile(Param_Value, probs = 0.75, na.rm = TRUE),
                     minVal = min(Param_Value, na.rm = TRUE),
                     maxVal = max(Param_Value, na.rm = TRUE))

  df.i <- dplyr::filter(df.comp, StationID == TargetSiteID)
  i.Group <- df.i[1, "IncaseCol"]

  # Loop, j, calc quantiles ####
  for (j in seq_along(detects)) {##FOR.j.START
    #
    stressname <- detects[j]
    j.len <- length(detects)
    #
    message(paste0("Processing stressor (", j, "/", j.len, ") ", stressname, ".\n"))
    utils::flush.console()

    # Select only necessary columns
    df.j <- df.i %>%
      dplyr::select(StationID, IncaseCol, StressSampleID, RespSampleID,
                    all_of(colBio), Quality, all_of(stressname)) %>%
      dplyr::rename(Param_Value = {{stressname}}) %>%
      dplyr::mutate(Param_Name = stressname) %>%
      dplyr::mutate(n = df.stats$n[df.stats$Param_Name == stressname],
                    q25 = df.stats$q25[df.stats$Param_Name == stressname],
                    q50 = df.stats$q50[df.stats$Param_Name == stressname],
                    q75 = df.stats$q75[df.stats$Param_Name == stressname]) %>%
      dplyr::filter(!is.na(Param_Value))

    # Comp Score for box plot
    colInvScore <- df_stressinfo %>%
      dplyr::filter(Stressor == stressname) %>%
      dplyr::select(DirIncStress)
    colInvScore <- as.character(colInvScore)

    # Score samples ####
    ## Use different criteria for some parameters (Specifically pH and DO)
    ## Score pH in both directions
    if (stressname == "pH_alkEnv") { # pH is a decreaser in alkalkine environments
      # Parameter is pH; need two scores, one for acid environments & one for alkaline
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(Param_Value < pHlimLow ~ 1,
                                                Param_Value < q25 ~ 1,
                                                Param_Value > q50 ~ -1,
                                                TRUE ~ 0))
    } else if (stressname == "pH_acidicEnv") {
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(Param_Value > pHlimHigh ~ 1,
                                                Param_Value > q75 ~ 1,
                                                Param_Value < q50 ~ -1,
                                                TRUE ~ 0))
    } else if (grepl("^DO", stressname, perl = TRUE, ignore.case = FALSE) == TRUE) {
      #Parameter is DO; If values are < DOlim, then 1 by definition
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(Param_Value < DOlim ~ 1,
                                                Param_Value > q50 ~ -1,
                                                Param_Value < q25 ~ 1,
                                                TRUE ~0))
    } else if (colInvScore == "Dec") {
      # Inverse Scoring
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(Param_Value > q50 ~ -1,
                                                Param_Value < q25 ~ 1,
                                                TRUE ~0))
    } else {
      # Regular Scoring
      df.j <- df.j %>%
        dplyr::mutate(Sc_Box = dplyr::case_when(Param_Value > q75 ~ 1,
                                                Param_Value < q50 ~ -1,
                                                TRUE ~ 0))
    }

    # Append scores to table
    cols <- colnames(df.scores)
    df.j <- df.j %>%
      dplyr::mutate(biocomm = "BMI",
                    Label = df_stressinfo$Label[df_stressinfo$Stressor == stressname]) %>%
      dplyr::select(all_of(cols))

    df.scores <- rbind(df.scores, df.j)

    ## Box Plot of Comparator Sites (with better bio)
    scores <- unlist(as.vector(df.j$Sc_Box))
    # scores <- unlist(as.vector(df.i.n[, "Sc_Box"]))
    lab.Score <- paste0("Score = ", paste0(scores, collapse = ", "))
    # lab.N     <- paste0("n = ", unique(df.i[, paste0("n_", stressname)][1]))
    lab.N <- paste0("n = ", unique(df.j$n))

    # plots ####
    # File Names
    fn_png_p1 <- paste0(TargetSiteID, "_", biocomm, "_CoOccur_",
                        make.names(stressname), ".png")
    ppi       <- 300

    # Create (ggplot)
    bio_col <- c("gray25", "steelblue2")
    bio_shp <- c(25, 21) # down triangle and circle
    bio_size <- c(5, 2)
    lab_comp <- paste0("Comparator samples selected from ", incaseLabel,
                       " = ", i.Group)

    # scoring lines
    maxVal <- df.stats$maxVal[df.stats$Param_Name == stressname]
    minVal <- df.stats$minVal[df.stats$Param_Name == stressname]
    if (colInvScore == "Dec") {##IF~j_in_InvSc~START
      # Inverse Scoring
      box_qHI <- df.stats$q50[df.stats$Param_Name == stressname]
      box_qLO <- df.stats$q25[df.stats$Param_Name == stressname]
      segNeg <- ((maxVal - box_qHI) / 2) + box_qHI
      segZero <- ((box_qHI - box_qLO) / 2) + box_qLO
      segPos <- ((box_qLO - minVal) / 2) + minVal
    } else {
      # Regular Scoring
      box_qHI <- df.stats$q75[df.stats$Param_Name == stressname]
      box_qLO <- df.stats$q50[df.stats$Param_Name == stressname]
      segPos <- ((maxVal - box_qHI) / 2) + box_qHI
      segZero <- ((box_qHI - box_qLO) / 2) + box_qLO
      segNeg <- ((box_qLO - minVal) / 2) + minVal
    }##IF~j_in_InvSc~END

    # arrow labels
    aLabPos <- "1"
    aLabZero <- "0"
    aLabNeg <- "-1"

    ## Plot, Variables, Target Site Line
    targ_line_col <- "red"
    targ_line_lty <- 2
    targ_line_lwd <- 1

    ## Limit line types (dotted)
    lim_lty <- 3

    # Get wordy label for the y-axis
    jlog <- df_stressinfo$LogTransf[df_stressinfo$Stressor == stressname]
    if (jlog == 1) {
      jlabel <- paste0("Log1p ",
                       df_stressinfo$Label[df_stressinfo$Stressor == stressname])
    } else {
      jlabel <- df_stressinfo$Label[df_stressinfo$Stressor == stressname]
    }
    legendtitle <- "Samples"
    maintitleCO <- paste0(TargetSiteID, ": Co-occurrence line of evidence")
    subtitleCO <-"Are the observed stressor levels consistent with impairment where and when it occurs?"
    subtitleCO <- stringr::str_wrap(subtitleCO, 100)

    # plot1, ggplot ####
    if (useBetter) {
      df.plot <- df.compBT
      lab.sub <- paste0("Comparator samples with paired stressor/response samples and ",
                        "higher response scores (", lab.N, ").\n", lab.Score, ".")
    } else {
      df.plot <- df.comp
      lab.sub <- paste0("Comparator samples with paired stressor/response samples",
                        " (", lab.N, ").\n", lab.Score, ".")
    }

    if (onlyNotDeg) {
      lab.sub <- stringr::str_to_sentence(paste0("Not degraded ", lab.sub))
    }

    targetvals <- as.numeric(unlist(df.j[, "Param_Value"]))
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
                                  breaks = c("Degraded", "Not degraded"),
                                  values = bio_col, drop = TRUE) +
      ggplot2::scale_fill_manual(name = legendtitle,
                                 breaks = c("Degraded", "Not degraded"),
                                 values = bio_col, drop = TRUE) +
      ggplot2::scale_shape_manual(name = legendtitle,
                                  breaks = c("Degraded", "Not degraded"),
                                  values = bio_shp, drop = TRUE) +
      ggplot2::labs(title = maintitleCO, subtitle = subtitleCO,
                    caption = lab.sub, y = jlabel, x = lab_comp) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())

    # if (stressname == "pH_alkEnv") {
    #   p1 + ggplot2::geom_hline(yintercept = pHlimLow, color = "black",
    #                            lty = lim_lty)
    # }
    # if (stressname == "pH_acidicEnv") {
    #   p1 + ggplot2::geom_hline(yintercept = pHlimHigh, color = "black",
    #                            lty = lim_lty)
    # }
    # if (grepl("^DO", stressname, perl = TRUE, ignore.case = FALSE) == TRUE) {
    #   p1 + ggplot2::geom_hline(yintercept = DOlim, color = "black", lty = lim_lty)
    # }

      ggplot2::ggsave(filename = file.path(dir_path, fn_png_p1), plot = p1,
                      dpi = ppi, width = 8, height = 6, units = "in")

  }##FOR.j.END

  # Identify stressors ####
  fn.scores <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm,
                                          "_CO_Scores.tab"))
  utils::write.table(df.scores, file = fn.scores, col.names = TRUE,
                     row.names = FALSE, sep = "\t", append = FALSE)

  stressors <- unique(df.scores$Param_Name[df.scores$Sc_Box != -1])
  notstressors <- unique(df.scores$Param_Name[df.scores$Sc_Box == -1])
  notstressors <- setdiff(notstressors, stressors)
  # prevents a stressor identified by 1 sample fromm appearing in the "notstressors" vector

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

  df.stressorMetadata <- dplyr::filter(df_stressinfo, Stressor %in% stressors)
  return(list(df_stressorMetadata = df.stressorMetadata,
              notEvaluated = notstressors))

}##FUNCTION.END
