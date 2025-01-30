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
#' \strong{Derive Evidence for Stressor-Response Relationships from Field
#' Observational Studies.}
#'
#' Stressor-response from field observational studies: Is the level of the
#' stressor sufficient to explain the level of biological effect observed at
#' the site?
#'
#' Using all comparator sites, fit logistical regression curve of the probability
#' of poor condition (i.e., poor California index score) as a function of
#' stressor level.  Compare stressor levels from test site to levels
#' corresponding to median (50%) and low (20%) probabilities of observing poor
#' condition.
#'
#' 1. Supports the case for the candidate cause. Stressor levels at the test
#' site are above the lower confidence limit (LCL) corresponding to 50%
#' probability of observing poor condition
#'
#' 0. Indeterminate. Stressor levels at the test site are between the LCL
#' corresponding to 50% probability of observing poor condition and the UCL
#' corresponding to 20% probability of observing poor condition.
#'
#' -1. Weakens the case for the candidate cause. Stressor levels at the test
#' site are below the upper confidence limit (UCL) corresponding to 20%
#' probability of observing poor condition.
#'
#' Cut function is used to assign narrative categories and degraded status based
#' on provided biological score.
#' Ensures criteria are applied the same across all sites.
#'
#' The BioDegLab has to remain as the default values of Yes and No.
#' Other values will break the code.
#'
#' Only a single biological measurement is used. But multiple stressors can be
#' used.
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
                       incaseLabel,
                       biocomm,
                       colBio,
                       useBetter = FALSE,
                       df_stressinfo,
                       pHlimLow = 6.5,
                       pHlimHigh = 9,
                       DOlim = 7,
                       dir_plots = file.path(getwd(), "Results"),
                       dir_sub = "CoOccurrence",
                       boo_plot = TRUE) {##FUNCTION.START

  boo_DEBUG <- FALSE

  if (boo_DEBUG==TRUE) {
    TargetSiteID = TargetSiteID
    df_data = df_PairedSRTransf
    incaseLabel = incaseLabel
    colBio = bioIndex
    useBetter = FALSE
    df_stressinfo = list.stressors$stressors
    biocomm = bioComm
    dir_plots = dir_results
    dir_sub = "CoOccurrence"
    pHlimLow = 6.5
    pHlimHigh = 9
    DOlim = 7
    boo_plot = TRUE
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)

  # QC, 20190418
  colStressors <- as.vector(unlist(df_stressinfo$Stressor))

  # QC, 20190418
  colStressors.NotPresent <- colStressors[!(colStressors %in% names(df_data))]
  if (length(colStressors.NotPresent) !=0 ) {##IF~bad stressors~START
    msg.warning <- paste0("Stressors listed below are not present in the ",
                          "provided data frame (df_data) and were not analyzed: \n",
                          paste(colStressors.NotPresent, collapse="\n"), "\n\n")
    message(msg.warning)
    utils::flush.console()
    colStressors <- colStressors[colStressors %in% names(df_data)]
  }##IF~bad stressors~END

  # Identify columns to keep for the analysis
  col.KEEP      <- c("StationID", "IncaseCol", "StressSampleID", "RespSampleID",
                     colBio, "Quality", colStressors)
  #
  # default sample ID
  if(is.null(TargetSiteID)){##IF.isnull.ID.START
    TargetSiteID <- as.character(sort(unique(df_data[, "StationID"])))[1]
  }##IF.isnull.ID.END

  # QC (site in data) ####
  boo_QC_site <- TargetSiteID %in% df_data[, "StationID"]
  if (boo_QC_site == FALSE) {##IF~boo_QC_site~START
    name_df <- deparse(substitute(df_data))
    name_col <- deparse(substitute("StationID"))
    name_df_col <- paste0(name_df, name_col)
    msg_NoSite <- paste0("Target site (", TargetSiteID,
                         ") was *not* found in the function inputs ",
                         "(df_data, column StationID).")
    stop(msg_NoSite)
  }##IF~boo_QC_site~END
  #
  # Create dirs ####
  #wd <- getwd()
  #dir.sub <- "Results"
  dir_sub2 <- TargetSiteID
  dir_sub3 <- biocomm
  dir_sub4 <- dir_sub
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2)) == TRUE,
         dir.create(file.path(dir_plots, dir_sub2)),
         FALSE)
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2, dir_sub3)) == TRUE,
         dir.create(file.path(dir_plots, dir_sub2, dir_sub3)),
         FALSE)
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4)) == TRUE,
         dir.create(file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4)),
         FALSE)

  dir_path <- file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4)

  # Create Score Output File ####
  df.scores <- cbind(df_data[0, c("StationID", "IncaseCol", "StressSampleID",
                                  "RespSampleID", colBio, "Quality")],
                     data.frame(Param_Name = character(), Param_Value = double(),
                                n = integer(), q25 = double(), q50 = double(),
                                q75 = double(), Sc_Box = character(),
                                biocomm = character(), Label = character(),
                                stringsAsFactors = FALSE))

  # Save scores file (append to later)
  fn.scores <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm,
                                          "_CO_Scores.tab"))
  utils::write.table(df.scores, file=fn.scores, append = FALSE,
                     col.names = TRUE, row.names=FALSE, sep="\t")

  if (useBetter == TRUE) {
    # Subset df_data for comparator sites having better biology
    df.compBT <- df_data %>%
      dplyr::filter(IncaseYN == 1 & BetterThan == 1)
  } else {
    # Subset df_data for comparator sites
    df.comp <- df_data %>%
      dplyr::filter(IncaseYN == 1)
  }
  df.i <- df.comp[df.comp$StationID == TargetSiteID, ]
  i.Group <- df.i[1, "IncaseCol"]

  if (boo_DEBUG==TRUE) {##IF.boo_DEBUG.START
    j <- colStressors[1]
  }##IF.boo_DEBUG.END
  # outside loop just in case forget to turn off debug flag

  # Loop, j, calc quantiles ####
  for (j in seq_along(colStressors)) {##FOR.j.START
    #
    stressname <- colStressors[j]
    j.len <- length(colStressors)
    #
    message(paste0("Processing stressor (", j, "/", j.len, ") ", stressname, ".\n"))
    utils::flush.console()

    #
    if (useBetter == TRUE) {
      df.i[, paste0("n_", stressname)] <- sum(!is.na(df.compBT[, stressname]))
      df.i[, paste0("q25_", stressname)] <- stats::quantile(df.compBT[, stressname],
                                                   probs=0.25, na.rm=TRUE)
      df.i[, paste0("q50_", stressname)] <- stats::quantile(df.compBT[, stressname],
                                                   probs=0.50, na.rm=TRUE)
      df.i[, paste0("q75_", stressname)] <- stats::quantile(df.compBT[, stressname],
                                                   probs=0.75, na.rm=TRUE)
      minVal <- min(df.compBT[, stressname], na.rm = TRUE)
      maxVal <- max(df.compBT[, stressname], na.rm = TRUE)
    } else {
      df.i[, paste0("n_", stressname)] <- sum(!is.na(df.comp[, stressname]))
      df.i[, paste0("q25_", stressname)] <- stats::quantile(df.comp[, stressname],
                                                   probs=0.25, na.rm=TRUE)
      df.i[, paste0("q50_", stressname)] <- stats::quantile(df.comp[, stressname],
                                                   probs=0.50, na.rm=TRUE)
      df.i[, paste0("q75_", stressname)] <- stats::quantile(df.comp[, stressname],
                                                   probs=0.75, na.rm=TRUE)
      minVal <- min(df.comp[, stressname], na.rm = TRUE)
      maxVal <- max(df.comp[, stressname], na.rm = TRUE)
    } ## Quantiles calculated

    # Comp Score for box plot
    colInvScore <- df_stressinfo %>%
      dplyr::filter(Stressor == stressname) %>%
      dplyr::select(DirIncStress)
    colInvScore <- as.character(colInvScore)

    # Score samples ####
    if (colInvScore == "Dec") {##IF~j_in_InvSc~START
      ## Use different criteria for some parameters (Specifically pH and DO)
      if (grepl("^pH", stressname, perl = TRUE, ignore.case = FALSE) == TRUE) {  # Parameter is pH
        vals <- df_data %>%
          dplyr::filter(StationID == TargetSiteID) %>%
          dplyr::select(all_of(stressname))
        vals <- as.vector(vals[!is.na(vals)])
        # if pH val < pHlimLow then 1
        # if pH val > pHlimHigh then 1
        # if pH val between pHlimLow & pHlimHigh, then what?
        if (any(vals < pHlimLow)) {
          print("pH low")
          flush.console()
          # Inverse Scoring
          df.i[, paste0("Sc_Box_", stressname)] <-
            ifelse(df.i[, stressname] > df.i[, paste0("q50_", stressname)],
                   -1,
                   ifelse(df.i[, stressname] < df.i[, paste0("q25_", stressname)],
                          1, 0))
        } else if(any(vals > pHlimHigh)) {
          print("pH high")
          flush.console()
          # Regular Scoring
          df.i[, paste0("Sc_Box_", stressname)] <-
            ifelse(df.i[, stressname] > df.i[, paste0("q75_", stressname)],
                   -1,
                   ifelse(df.i[, stressname] < df.i[, paste0("q50_", stressname)],
                          -1, 0))
        }
      } else if (grepl("^DO", stressname, perl = TRUE, ignore.case = FALSE) == TRUE) {  #Parameter is DO
        vals <- df_data %>%
          dplyr::filter(StationID == TargetSiteID) %>%
          dplyr::select(all_of(stressname))
        vals <- as.vector(vals[!is.na(vals)])
        df.i[, paste0("Sc_Box_", stressname)] <-
          ifelse(df.i[, stressname] > df.i[, paste0("q50_", stressname)],
                 -1,
                 ifelse(df.i[, stressname] < DOlim, 1, 0))
      } else {
        # Inverse Scoring
        df.i[, paste0("Sc_Box_", stressname)] <-
          ifelse(df.i[, stressname] > df.i[,paste0("q50_", stressname)],
                 -1,
                 ifelse(df.i[, stressname] < df.i[, paste0("q25_", stressname)],
                        1, 0))
      }
    } else {
      # Regular Scoring
      df.i[, paste0("Sc_Box_", stressname)] <-
        ifelse(df.i[, stressname] > df.i[,paste0("q75_", stressname)],
               1,
               ifelse(df.i[, stressname] < df.i[, paste0("q50_", stressname)],
                      -1, 0))
    }##IF~j_in_InvSc~END

    # Plots
    # Need to filter df.i to get rid of NA for "j" (stressor)
    # order values by j then get multiple comp scores
    df.i.n <- df.i[!is.na(df.i[, stressname]), ]
    # df.i.n <- df.i.n[order(df.i.n[, j]), ]

    if (nrow(df.i.n) != 0) {##IF.nrow.START
      # Save to Score/Results file
      df.i.n[, "Param_Name"]  <- stressname
      df.i.n[, "Param_Value"] <- df.i.n[, stressname]
      df.i.n[, "n"]           <- df.i.n[, paste0("n_", stressname)]
      df.i.n[, "q25"]         <- df.i.n[, paste0("q25_", stressname)]
      df.i.n[, "q50"]         <- df.i.n[, paste0("q50_", stressname)]
      df.i.n[, "q75"]         <- df.i.n[, paste0("q75_", stressname)]
      df.i.n[, "Sc_Box"]      <- df.i.n[, paste0("Sc_Box_", stressname)]
      # df.i.n append to output (only keep matching columns)
      df.scores.i.n <- merge(df.scores, df.i.n[, (names(df.i.n) %in% names(df.scores))],
                             all.y=TRUE)
      # 2019-05-20, sort by score
      df.scores.i.n <- df.scores.i.n[order(df.scores.i.n[, "Param_Value"]), ]

      ## Box Plot of Comparator Sites (with better bio)
      scores <- unlist(as.vector(df.i.n[, "Sc_Box"]))
      lab.Score <- paste0("Score = ", paste0(scores, collapse = ", "))
      # num <- df.i
      lab.N     <- paste0("n = ", unique(df.i[, paste0("n_", stressname)][1]))

      # plots ####
      # File Names
      fn_png_p1 <- paste0(TargetSiteID, "_", biocomm, "_CoOccur_",
                          make.names(stressname), ".png")
      ppi       <- 300

      # Create (ggplot)
      bio_col <- c("gray25", "steelblue2")
      bio_shp <- c(25, 21) # down triangle and circle
      bio_size <- c(3, 2)
      lab_comp <- paste0("Comparator samples selected from ", incaseLabel,
                         " = ", i.Group)

      # scoring lines
      if (colInvScore == "Dec") {##IF~j_in_InvSc~START
        # TODO: TEST THIS!
        # Inverse Scoring
        box_qHI <- df.scores.i.n$q50[1]
        box_qLO <- df.scores.i.n$q25[1]
        segNeg <- ((maxVal - box_qHI) / 2) + box_qHI
        segZero <- ((box_qHI - box_qLO) / 2) + box_qLO
        segPos <- ((box_qLO - minVal) / 2) + minVal
      } else {
        # Regular Scoring
        box_qHI <- df.scores.i.n$q75[1]
        box_qLO <- df.scores.i.n$q50[1]
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

      # Get wordy label for the y-axis
      jlog <- df_stressinfo$LogTransf[df_stressinfo$Stressor == stressname]
      if (jlog == 1) {
        jlabel <- paste0("Log1p ",
                         df_stressinfo$Label[df_stressinfo$Stressor == stressname])
      } else {
        jlabel <- df_stressinfo$Label[df_stressinfo$Stressor == stressname]
      }
      legendtitle <- "Samples"
      maintitleCO <- "Co-occurrence line of evidence"
      subtitleCO <-"Are the observed stressor levels consistent with impairment where and when it occurs?"
      subtitleCO <- stringr::str_wrap(subtitleCO, 100)

      # plot1, ggplot ####
      if (useBetter) {
        df.plot <- df.compBT
        lab.sub <- paste0("Comparator samples with higher ", colBio,
                          " scores (", lab.N, ").\n", lab.Score, ".")
      } else {
        df.plot <- df.comp
        lab.sub <- paste0("Comparator samples with paired ", stressname, " and ",
                          colBio, " (", lab.N, ").\n", lab.Score, ".")
      }

      targetvals <- as.numeric(unlist(df.i[, stressname]))
      xseg <- i.Group + 0.5

      p1<- ggplot2::ggplot(df.plot, ggplot2::aes(y = .data[[stressname]],  # ARL 2023-05-25
                                                 x = IncaseCol,
                                                 group = IncaseCol)) +
        ggplot2::geom_boxplot(outliers = TRUE, outlier.size = 0.5, na.rm = TRUE,
                              staplewidth = 0.5) +
        ggplot2::coord_flip() +
        ggplot2::geom_jitter(ggplot2::aes(color = "black", shape = Quality,
                                          fill = Quality), alpha = 0.5,
                             na.rm = TRUE, width = 0.25, height = 0.01) +
        ggplot2::geom_hline(yintercept = targetvals, color = targ_line_col,
                            lty = targ_line_lty, lwd = targ_line_lwd, na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = c(box_qLO, box_qHI), color = "black",
                            lty = 2, na.rm = TRUE) +
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

      if(boo_plot){
        ggplot2::ggsave(filename=file.path(dir_path, fn_png_p1),
                        plot=p1, dpi=ppi, width=8, height=6, units="in")
      }## IF ~ boo_plot ~ END
      #}##IF~non-empty~END


      # add biocomm, 20190425
      df.scores.i.n[, "biocomm"] <- biocomm
      df.scores.i.n[, "Label"] <- unique(jlabel)

      # Save tabular scores
      utils::write.table(df.scores.i.n, file=fn.scores, col.names = FALSE,
                         row.names=FALSE, sep="\t", append=TRUE)
      # Remove
      rm(df.scores.i.n)

    } else {
      # no data
      message(paste0("   All values NA for stressor (", stressname, ").\n"))
      utils::flush.console()
      # add data to scores table
      column_names <- c("Param_Name", "Param_Value", "n", "q25", "q50",
                        "q75", "Sc_Box")
      df.i.NA <- df.i[1, 1:5]
      df.i.NA[, column_names] <- NA
      df.i.NA[, "Param_Name"] <- stressname
      # add biocomm, 20190425
      df.i.NA[, "biocomm"] <- biocomm
      df.i.NA[, "Label"] <- unique(jlabel)
      utils::write.table(df.i.NA, file=fn.scores, col.names = FALSE,
                         row.names=FALSE, sep="\t", append=TRUE)

    }##IF.nrow.END
    #

  }##FOR.j.END
  #

}##FUNCTION.END
