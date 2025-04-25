#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Verified Predictions
#'
#' @description Get verified predictions.
#'
#' @details
#'
#' Required packages: dplyr, ggplot2, stringr, tidyr
#'
#' @param TargetSiteID Site ID
#' @param stressors vector of stressors identified as candidate causes
#' @param df_stressinfo dataframe of stressor metadata
#' @param SSTVanalytes vector containing Stressors for stressors with stressor-specific tolerance values
#' @param df_paired list_MatchBioData
#' @param biocomm default = "bmi"
#' @param colBioSample column name for the response sample ID
#' @param df_bioTaxaData dataframe of raw response data (counts or relative abundance)
#' @param df_MasterTaxa dataframe of master taxa with SSTV values determined for individual taxa
#' @param colBio default = "IBI"
#' @param BioIndex_Nar default = "Quality"
#' @param BioIndex_Nar_Deg default = "Degraded"
#' @param dir_plots default = file.path(getwd(), "Results")
#' @param dir_sub default = "VerifiedPredictions"
#' @param boo_plot = TRUE
#'
#' @return Results text file and png files to "Results" "VerifiedPredictions" folder
#' in working directory of box plots
#'
#' @export
#'
getVerifiedPredictions <- function(TargetSiteID,
                                   stressors.sstv,
                                   df_stressinfo,
                                   df_paired,
                                   biocomm,
                                   df_bioTaxaData,
                                   df_MasterTaxa,
                                   siteQual2Plot,
                                   colBio,
                                   plot_vars,
                                   plot_dpi,
                                   plot_H,
                                   plot_W,
                                   plot_units,
                                   dir_plots = file.path(getwd(), "Results"),
                                   dir_sub = "VerifiedPredictions_SSTVs",
                                   boo_plot = TRUE) {##FUNCTION.START

  # Debugging
  boo.DEBUG <- TRUE
  #
  if (boo.DEBUG == TRUE) {##IF.boo.DEBUG.START
    TargetSiteID = TargetSiteID
    stressors.sstv = stressors.sstv
    df_stressinfo = df_stressorMetadata # list.stressors$stressors
    df_paired = df_PairedSRTransf
    biocomm = bioComm
    df_bioTaxaData = bioTaxaData
    df_MasterTaxa = bioMasterTaxa
    siteQual2Plot = siteQual2Plot
    colBio = bioIndex
    plot_vars = data_plotvars
    plot_dpi = plot_dpi
    plot_H = plot_H
    plot_W = plot_W
    plot_units = plot_units
    dir_plots = dir_results
    dir_sub = "VerifiedPredictions_SSTVs"
    boo_plot = boo_plot_user
    tv <- 1
  }##IF.boo.DEBUG.END

  # define pipe
  `%>%` <- dplyr::`%>%`
  col.Bio.Deg   <- "Quality"
  # QC, biocomm
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

  # Create vector of stressors (to identify data gaps)
  stressors <- as.vector(unlist(df_stressinfo$Stressor))

  # SSTV data gaps ----
  if (length(stressors.sstv) == 0) {

      gapcomment <- paste0("No stressor-specific tolerance values.")
      gaps <- cbind.data.frame("getVerifiedPredictions", "No SSTV data", 0
                               , gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_plots, TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

  }

  if (length(stressors.sstv) > 0) {
    ## Subset stressInfo ----
    df_SSTV <- df_stressinfo %>%
      dplyr::filter(Stressor %in% stressors.sstv) %>%
      dplyr::select(Stressor, LogTransf, SSTVname, SensMin, SensMax, TolMin,
                    TolMax, Label)
    df_SSTV <- unique(df_SSTV)

    SSTVnames <- as.vector(unique(df_SSTV$SSTVname))
    mtcols <- colnames(df_MasterTaxa)

    # Match sstv to master taxa file ----
    # Check whether master taxa file contains SSTVname (tol vals for that stressor),
    # if so, add to keepMTcol vector; if not write to data gaps file
    for (n in seq_along(SSTVnames)) {  # If more than one SSTV, then must iterate
      name <- SSTVnames[n]
      SSTVlabel <- as.character(df_SSTV$Label[df_SSTV$SSTVname == name])

      if (name %in% mtcols) {  # Check if TV data in Master Taxa file
        if (exists("keepMTcol")) {
          keepMTcol <- c(keepMTcol, name)
        } else {
          keepMTcol <- name
        }
      } else {
        # no taxa in MT taxa are assigned tol values for this stressor
        gapcomment <- paste0("No ", biocomm, " taxa have tolerance "
                             , "values available for this SSTV.")
        gaps <- cbind.data.frame("getVerifiedPredictions", SSTVlabel, 0, gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(dir_plots, TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        if (exists("deleteSSTVname")) {
          deleteSSTVnames <- c(deleteSSTVnames, name)
        } else {
          deleteSSTVnames <- name
        }
      }
      rm(SSTVlabel)
    }

    # Create taxa file for SSTVs ----
    if (exists("keepMTcol") == TRUE) { # Some stressors have SSTV vals in master taxa file

      # Merge biotaxa results with master taxa file ----
      df_SSTVtaxa <- df_MasterTaxa %>%
        dplyr::select(TaxonID, all_of(keepMTcol))

      df_SSTVtaxa <- df_SSTVtaxa %>% filter(if_any(-1, ~ !is.na(.))) # LCN changed from df_SSTVtaxa[rowSums(!is.na(df_SSTVtaxa[, -1])) >= 1, ] which fails when there is only one SSTV

      df_bioTaxaData <- merge(df_bioTaxaData, df_SSTVtaxa, by = "TaxonID")

      boo.continue = TRUE

    } else {

      boo.continue = FALSE

    }

    if (boo.continue == TRUE) { # Have taxa

      # 20190513, remove scores file if exists
      fn_scores <-  file.path(dir.path, paste0(TargetSiteID, "_", biocomm
                                               , "_VP_SSTV_Scores.tab"))
      if (file.exists(fn_scores)) { file.remove(fn_scores) }

      # Obtain relevant data from df_paired
      ## Prep stressor data ----
      qual <- switch(tolower(siteQual2Plot),
                     "reference" = "RefSiteFlag",
                     "not degraded" = "Quality",
                     "better than" = "BetterThan")

      # Filter for outside case sites (but use inside the case sites)
      # Trim unnecessary columns
      df_stress.sstv <- df_paired %>%
        dplyr::filter(OutcaseYN == 1) %>%
        dplyr::mutate(Quality = forcats::fct_expand(Quality, "Target")) %>%
        dplyr::select(StationID, IncaseYN, StressSampleID, StressSampleDate,
                      RespSampleID, RespSampleDate, BioComm, all_of(colBio),
                      RefSiteFlag, Quality, BetterThan, all_of(stressors.sstv))

      for (tv in seq_along(keepMTcol)) { # Obtain one or more sstv columns

        sstv <- keepMTcol[tv]
        sstv.sensmin <- df_stressinfo$SensMin[df_stressinfo$SSTVname == sstv]
        sstv.sensmin <- sstv.sensmin[!is.na(sstv.sensmin)]
        sstv.sensmax <- df_stressinfo$SensMax[df_stressinfo$SSTVname == sstv]
        sstv.sensmax <- sstv.sensmax[!is.na(sstv.sensmax)]
        sstv.label   <- df_stressinfo$Label[df_stressinfo$SSTVname == sstv]
        sstv.label   <- sstv.label[!is.na(sstv.label)]

        # Modify taxon count data to identify sensmin and sensmin through sensmax
        if (grepl("^\\d*$", sstv.sensmin) && grepl("^\\d*$", sstv.sensmax)) { # tv is numeric
          # convert to numeric
          sstv.sensmin <- as.numeric(sstv.sensmin)
          sstv.sensmax <- as.numeric(sstv.sensmax)

          sstv.sensall <- seq(from = sstv.sensmin, to = sstv.sensmax, by = 1)
          sstv.sensall.gp <- paste0(sstv.sensall, collapse = ", ")

          # Generate Labels to be used as groups
          sstv.sensminLabel <- paste(sstv, "SensMin", sep = "_")
          sstv.sensallLabel <- paste(sstv, "SensAll", sep = "_")

          df_temp <- df_bioTaxaData %>%
            dplyr::mutate({{sstv.sensminLabel}} := ifelse(.data[[sstv]] == sstv.sensmin,
                                                          "Most sensitive",
                                                          NA),
                          {{sstv.sensallLabel}} := ifelse(.data[[sstv]] %in% sstv.sensall,
                                                          "All sensitive",
                                                          NA))
        } else { # tv is character
          sstv.sensminLabel <- paste(sstv, "SensMin", sep = "_")
          sstv.sensallLabel <- paste(sstv, "SensAll", sep = "_")
          sstv.sensall.gp   <- paste0(sstv.sensmin, ", ", sstv.sensmax)

          df_temp <- df_bioTaxaData %>%
            dplyr::mutate({{sstv.sensminLabel}} := ifelse(.data[[sstv]] == sstv.sensmin,
                                                          sstv.sensmin, NA),
                          {{sstv.sensallLabel}} := dplyr::case_when(.data[[sstv]] %in%
                                                                      c(sstv.sensmin, sstv.sensmax) ~
                                                                      sstv.sensall.gp,
                                                                    TRUE ~ NA))
        } ## End assignments

        if (tv == 1) { # merge temp df with df_resp
          df_resp <- df_temp
        } else {
          df_resp <- merge(df_resp, df_temp,
                           by = c("TaxonID", "StationID", "RespSampleID",
                                  "RespSampleDate", "BMISampFlag", "NumIndividuals",
                                  "PctInd", "SampleTotAbund", "SampleTotTaxa",
                                  "PctTaxa", keepMTcol))
        }

        # Remove sstv variables, labels
        suppressWarnings(rm(sstv, sstv.sensmin, sstv.sensmax, sstv.label,
                            sstv.sensall, sstv.sensall.gp, sstv.sensallLabel,
                            sstv.sensmaxLabel, sstv.sensminLabel))

      } ## END for tv

      # Summarize data
      df_resp.summary <- df_resp %>%
        tidyr::pivot_longer(cols = dplyr::contains("Sens"),
                            names_to = "Group", values_to = "Label",
                            values_ptypes = character(),
                            values_drop_na = TRUE) %>%
        dplyr::group_by(StationID, RespSampleID, RespSampleDate, Group, Label) %>%
        dplyr::summarise(NumInds = sum(NumIndividuals, na.rm = TRUE),
                         PctInds = sum(PctInd, na.rm = TRUE),
                         NumTaxa = dplyr::n(),
                         PctTaxa = sum(PctTaxa, na.rm = TRUE),
                         .groups = "drop_last") %>%
        dplyr::mutate(Group = sub("(_SensMin)$", "", Group),
                      Group = sub("(_SensAll)$", "", Group))

      df_GpLbl <- unique(df_resp.summary[, c("Group", "Label")])

      df_tv <- merge(df_stress.sstv, df_resp.summary,
                     by = c("StationID", "RespSampleID", "RespSampleDate"), all = TRUE)

      # Loop - Score SSTVs ####
      for (s in seq_along(stressors.sstv)) {

        stressor <- stressors.sstv[s]
        message(paste("Scoring", stressor))
        stressorLabel <- df_stressinfo$Label[df_stressinfo$Stressor == stressor]
        tolval <- df_SSTV$SSTVname[df_SSTV$Stressor == stressor]
        tolval.min <- paste0(tolval, "_SensMin")
        tolval.all <- paste0(tolval, "_SensAll")

        df_tv.incase <- dplyr::filter(df_tv, Group == {{tolval}} & IncaseYN == 1) %>%
          dplyr::select(StationID, RespSampleID, RespSampleDate, IncaseYN,
                        StressSampleID, StressSampleDate, BioComm, all_of(colBio),
                        RefSiteFlag, Quality, BetterThan, all_of(stressor), Group,
                        Label, NumInds, PctInds, NumTaxa, PctTaxa) %>%
          dplyr::mutate(PctInds = signif(PctInds * 100, digits = 3),
                        PctTaxa = signif(PctTaxa * 100, digits = 3)) %>%
          tidyr::pivot_longer(cols = NumInds:PctTaxa, names_to = "variable",
                              values_to = "value")

        if (nrow(dplyr::filter(df_tv.incase, StationID == TargetSiteID)) == 0) {
          # Create dataframe containing response values to include
          df_tv.target <- df_tv[df_tv$StationID == TargetSiteID, ]
          # Select only the columns prior to Group
          df_tv.target <- df_tv.target %>%
            dplyr::select(StationID, RespSampleID, RespSampleDate, IncaseYN,
                          StressSampleID, StressSampleDate, BioComm, all_of(colBio),
                          RefSiteFlag, Quality, BetterThan, all_of(stressor)) %>%
            dplyr::mutate(Group := {{tolval}})
          df_tv.target <- unique(df_tv.target) # Reduce to individual samples
          # Subset the group/label dataframe to the current tolval group
          df_GpLbl.tolval <- dplyr::filter(df_GpLbl, Group == {{tolval}})
          # Merge Label into the dataframe & add NumInds, PctInds, NumTaxa, PctTaxa
          df_tv.target <- merge(df_tv.target, df_GpLbl.tolval, by = "Group")
          df_tv.target <- df_tv.target %>%
            dplyr::mutate(NumInds = 0, PctInds = 0, NumTaxa = 0, PctTaxa = 0) %>%
            dplyr::select(StationID, RespSampleID, RespSampleDate, IncaseYN,
                          StressSampleID, StressSampleDate, BioComm, all_of(colBio),
                          RefSiteFlag, Quality, BetterThan, all_of(stressor), Group,
                          Label, NumInds, PctInds, NumTaxa, PctTaxa) %>%
            tidyr::pivot_longer(cols = NumInds:PctTaxa, names_to = "variable",
                                values_to = "value")
          # Add target samples back into df_tv.incase
          df_tv.incase <- rbind(df_tv.incase, df_tv.target)
        }

        ## Scoring ####
        # Get percentiles by most sensitive, all sensitive for each of
        # NumTaxa, %Taxa, NumInds, %Inds over all comparator sites
        df_quantiles.incase <- df_tv.incase %>%
          dplyr::select(Label, variable, value) %>%
          dplyr::group_by(Label, variable) %>%
          dplyr::summarise(min = suppressWarnings(min(value, na.rm = TRUE)),
                           q25 = quantile(value, probs = 0.25, na.rm = TRUE),
                           q50 = quantile(value, probs = 0.50, na.rm = TRUE),
                           q75 = quantile(value, probs = 0.75, na.rm = TRUE),
                           max = suppressWarnings(max(value, na.rm = TRUE)),
                           .groups = "drop_last")

        df_tv.incase <- merge(df_tv.incase, df_quantiles.incase,
                              by = c("Label", "variable"))

        # Calculate num samples better than, & better than, not degraded
        # Yields 1-row x 3-col df (# samps BT, # samps not deg, # samps BT & not deg)
        df_tv.incase.summary <- df_tv.incase %>%
          dplyr::distinct(StationID, RespSampleID, RespSampleDate, Quality, BetterThan) %>%
          dplyr::mutate(QualityNum = ifelse(Quality == "Not degraded", 1, 0),
                        BTNotDeg = ifelse((BetterThan == 1 & Quality == 1), 1, 0)) %>%
          dplyr::filter(StationID != TargetSiteID) %>% # Do NOT include target site samples
          dplyr::summarise(numSampsBT = sum(BetterThan, na.rm = TRUE),
                           numSampsNotDeg = sum(QualityNum, na.rm = TRUE),
                           numSampsBTNotDeg = sum(BTNotDeg, na.rm = TRUE),
                           .groups = "drop_last")

        # Assign scores to target site
        df_tbl_scores <- dplyr::filter(df_tv.incase, StationID == TargetSiteID) %>%
          dplyr::select(StationID, RespSampleID, RespSampleDate, IncaseYN,
                        StressSampleID, StressSampleDate, BioComm, all_of(colBio),
                        RefSiteFlag, Quality, BetterThan, all_of(stressor), Group,
                        Label, variable, value, q25, q50, q75) %>%
          dplyr::rename(StressorValue = {{stressor}},
                        Response = variable,
                        ResponseValue = value) %>%
          dplyr::mutate(Score = dplyr::case_when(ResponseValue < q25 ~ 1,
                                                 dplyr::between(ResponseValue, q25, q50) ~ 0,
                                                 ResponseValue > q50 ~ -1),
                        StressorLabel = stressorLabel,
                        Stressor := {{stressor}},
                        nBetterBio = as.integer(df_tv.incase.summary$numSampsBT),
                        nBetterBioNotDeg = as.integer(df_tv.incase.summary$numSampsBTNotDeg)) %>%
          dplyr::select(StationID, RespSampleID, RespSampleDate, StressSampleID,
                        StressSampleDate, BioComm, all_of(colBio), RefSiteFlag,
                        Quality, BetterThan, StressorLabel, Stressor, StressorValue,
                        Group, Label, Response, ResponseValue, q25, q50, Score,
                        nBetterBio, nBetterBioNotDeg)

        #
        boo_append <- TRUE
        boo_colnames <- FALSE

        if (file.exists(fn_scores) == FALSE) {##IF~file.exists(fn_scores)~START
          # invert for 1st instance
          boo_append <- !boo_append
          boo_colnames <- !boo_colnames
        }##IF~file.exists(fn_scores)~END

        utils::write.table(df_tbl_scores, file = fn_scores,
                           col.names = boo_colnames, row.names = FALSE,
                           sep="\t", append = boo_append)

        # Prepare plots ----
        # Boxplots: x = Label [SensMin, SensMax], y = value,
        # Group = variable [NumInds, PctInds, NumTaxa, PctTaxa], df_tv.incase
        # Jitterplots: all incase degraded [grey25, down triangle],
        # not degraded [steelblue, round], target [red triangle]
        df_tv.incase <- df_tv.incase %>%
          dplyr::mutate(Quality = as.character(Quality),
                        Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                        Quality = factor(Quality,
                                         levels = c("Not degraded", "Degraded", "Target"),
                                         labels = c("Not degraded", "Degraded", "Target")))

        df_tv.notTarget <- dplyr::filter(df_tv.incase, StationID != TargetSiteID)
        df_tv.target <- dplyr::filter(df_tv.incase, StationID == TargetSiteID)

        df_sstv.scores <- merge(df_quantiles.incase, df_tbl_scores)

        str_scores <- df_sstv.scores %>%
          dplyr::filter(Group == {{tolval}}) %>%
          dplyr::select(Label, variable, max, q25, q50, RespSampleDate, Score) %>%
          dplyr::arrange(Label, variable, max, RespSampleDate) %>%
          dplyr::group_by(Label, variable, max, q25, q50) %>%
          dplyr::summarise(Scores = toString(Score),
                           .groups = "drop_last") %>%
          dplyr::mutate(min = -10,
                        segNeg = ((q25 - min) / 2) + min,
                        aLabNeg = -1,
                        segZero = ((q50 - q25) / 2) + q25,
                        aLabZero = 0,
                        segPos = ((max - q50) / 2) + q50,
                        aLabPos = 1,
                        Scores = paste0("Scores = ", Scores))

        str_scores_max <- str_scores %>%
          dplyr::group_by(variable) %>%
          dplyr::summarise(OverallMax = max(max, na.rm = TRUE),
                           .groups = "drop_last")

        str_scores <- merge(str_scores, str_scores_max)

        ## Plot, Variables
        ## Prepare colors, sizes, etc  ----
        plot_vars  <- plot_vars %>%
          dplyr::filter(Type %in% c("target", "insideND", "insideD"))
        bio_fill    <- rev(unlist(plot_vars$Fill)) # Degraded, Not degraded, Target
        bio_shape   <- rev(unlist(plot_vars$Shape)) # down triangle, circle, and triangle
        bio_size    <- rev(unlist(plot_vars$Size)) # Degraded, Not degraded, Target
        bio_alpha   <- rev(unlist(plot_vars$Alpha)) # Degraded, Not degraded, Target

        # Prepare labels
        str_title <- paste0(TargetSiteID, ": Verified prediction "
                            ,"line of evidence for ", stressorLabel)
        str_title <- stringr::str_wrap(str_title, 100)
        str_subtitle <- paste0("Do the data support the prediction that",
                               " the abundance and richness of sensitive",
                               " taxa will be lower than that observed at",
                               " comparator sites?")
        str_subtitle <- stringr::str_wrap(str_subtitle, 100)
        legendtitle <- "Samples"
        str_xlab  <- ""

        # Arrow labels
        # aLabPos <- "1"
        # aLabZero <- "0"
        # aLabNeg <- "-1"

        ##PLOT VARIABLES ~ END

        fn_png_p1 <- paste0(TargetSiteID, "_", biocomm, "_VP_SSTV_", stressor, ".png")

        p_tv <- ggplot2::ggplot(NULL, ggplot2::aes(x = Label, y = value,
                                                   group = Label)) +
          ggplot2::geom_boxplot(data = df_tv.incase, outliers = TRUE,
                                outlier.size = 0.5, na.rm = TRUE,
                                staplewidth = 0.5, linewidth = 0.25) +
          ggplot2::geom_jitter(data = df_tv.notTarget,
                               ggplot2::aes(color = Quality, shape = Quality,
                                            fill = Quality, alpha = Quality),
                               na.rm = TRUE, width = 0.15, height = 0.01,
                               size = 0.4) +
          ggplot2::geom_jitter(data = df_tv.target,
                               ggplot2::aes(color = Quality, shape = Quality,
                                            fill = Quality, alpha = Quality),
                               na.rm = TRUE, width = 0.2, height = 0.01,
                               size = 1.5) +
          ggplot2::geom_boxplot(data = df_tv.incase,
                                ggplot2::aes(group = Label), outliers = TRUE,
                                outlier.size = 0.5, na.rm = TRUE,
                                staplewidth = 0.5, linewidth = 0.25, fill = NA) +
          ggplot2::coord_flip() +
          ggplot2::facet_wrap(. ~ variable, scales = "free") +
          ggplot2::scale_color_manual(name = legendtitle,
                                      breaks = c("Degraded", "Not degraded", "Target"),
                                      values = bio_fill, drop = TRUE) +
          ggplot2::scale_fill_manual(name = legendtitle,
                                     breaks = c("Degraded", "Not degraded", "Target"),
                                     values = bio_fill, drop = TRUE) +
          ggplot2::scale_shape_manual(name = legendtitle,
                                      breaks = c("Degraded", "Not degraded", "Target"),
                                      values = bio_shape, drop = TRUE) +
          ggplot2::scale_alpha_manual(name = legendtitle,
                                      breaks = c("Degraded", "Not degraded", "Target"),
                                      values = bio_alpha, drop = TRUE) +
          ggplot2::labs(title = str_title, subtitle = str_subtitle) +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 8),
                         plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 6)) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_text(size = 6),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(size = 6),
                         axis.ticks.y = ggplot2::element_blank())

        p_tv <- p_tv +
          ggplot2::geom_text(data = str_scores, size = 2, hjust = 1.5, vjust = 5,
                             ggplot2::aes(x = Label, y = OverallMax, label = Scores)) #+
          # ggplot2::geom_segment(data = str_scores, color = "orange",
          #                       ggplot2::aes(x = Label, xend = Label,
          #                                    y = min, yend = q25),
          #                       arrow = grid::arrow(ends = "both", type = "open",
          #                                            length = grid::unit(0.08, "cm"))) +
          # ggplot2::geom_segment(data = str_scores, color = "orange",
          #                       ggplot2::aes(x = Label, xend = Label,
          #                                    y = q25, yend = q50),
          #                       arrow = grid::arrow(ends = "both", type = "open",
          #                                           length = grid::unit(0.08, "cm"))) +
          # ggplot2::geom_segment(data = str_scores, color = "orange",
          #                       ggplot2::aes(x = Label, xend = Label,
          #                                    y = q50, yend = OverallMax),
          #                       arrow = grid::arrow(ends = "both", type = "open",
          #                                           length = grid::unit(0.08, "cm"))) #+
          # ggplot2::geom_text(data = str_scores, size = 2, hjust = 1.5, vjust = -5,
          #                    ggplot2::aes(x = Label, y = segNeg, label = aLabNeg,
          #                                 color = "orange")) +
          # ggplot2::geom_text(data = str_scores, size = 2, hjust = 1.5, vjust = -5,
          #                    ggplot2::aes(x = Label, y = segZero, label = aLabZero,
          #                                 color = "orange")) +
          # ggplot2::geom_text(data = str_scores, size = 2, hjust = 1.5, vjust = -5,
          #                    ggplot2::aes(x = Label, y = segPos, label = aLabPos,
          #                                 color = "orange"))

        if (boo_plot) {
          ggplot2::ggsave(filename = file.path(dir.path, fn_png_p1), plot = p_tv,
                          dpi = plot_dpi, width = plot_W, height = plot_H,
                          units = plot_units)
        }## IF ~ boo_plot ~ END

      }## FOR SSTV ~ END

    }## IF ~ boo_continue ~ END

  }## IF ~ SSTV ~ END

}##FUNCTION.END

