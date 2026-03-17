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
#' Required packages: dplyr, forcats, ggplot2, grid, stringr, tidyr
#'
#' @param TargetSiteID Site ID
#' @param stressors.sstv vector of stressors with stressor-specific tolerance values
#' @param df_stressinfo dataframe of stressor metadata
#' @param df_paired dataframe of paired stressor-response samples
#' @param biocomm default = "bmi"
#' @param df_bioTaxaData dataframe of raw response data (counts or relative abundance)
#' @param df_MasterTaxa dataframe of master taxa with SSTV values determined for individual taxa
#' @param colBio default = "IBI"
#' @param plotvars Standardized sizes, fills, shapes, and transparencies for plots.
#' Defaults to data_plotvars
#' @param plotdpi Standardized plot dpi. Defaults to plot_dpi. 600
#' @param plotH Standardized plot height. Defaults to plot_H. 6
#' @param plotW Standardized plot width. Defaults to plot_W. 8
#' @param plotunits Standardized plot units. Defaults to plot_units. "in"
#' @param dir_plots default = file.path(getwd(), "Results")
#' @param dir_sub default = "VerifiedPredictions"
#' @param boo_plot = TRUE
#'
#' @return Results text file and png files to "Results" "VerifiedPredictions" folder
#' in working directory of box plots
#' @examples
#' # None at this time
#' @export
#'
getVerifiedPredictions <- function(TargetSiteID,
                                   stressors.sstv,
                                   df_stressinfo,
                                   df_paired,
                                   biocomm,
                                   df_bioTaxaData,
                                   df_MasterTaxa,
                                   colBio,
                                   plotvars,
                                   plotdpi = 600,
                                   plotH = 6,
                                   plotW = 8,
                                   plotunits = "in",
                                   dir_plots = file.path(getwd(), "Results"),
                                   dir_sub = "_WoE",
                                   boo_plot = TRUE,
                                   targetSampleLabels = targetSampleLabels) {##FUNCTION.START

  `:=` <- data.table::`:=`

  # Global Bindings
  df_stressorMetadata <- df_PairedStressResp <- bioComm <- bioTaxaData <-
    bioMasterTaxa <- bioIndexGp <- dir_results <- boo.plot.user <- SSTVname <-
    SensMin <- SensMax <- Stressor <- LogTransf <- Label <- TaxonID <-
    IncaseYN <- Quality <- StationID <- IncaseCol <- StressSampleID <-
    StressSampleDate <- RespSampleID <- RespSampleDate <- RefSiteFlag <-
    BetterThan <- sstv.sensall <- sstv.sensmaxLabel <- Group <- NumInd <-
    PctInd <- PctTaxa <- NumInds <- PctInds <- NumTaxa <- variable <- value <-
    Min <- q25 <- q50 <- q75 <- Max <- StressorLabel <- StressorValue <-
    Response <- ResponseValue <- Score <- Scores <- Type <- OverallMax <-
    bioIndex <- bioIndexName <- LoE <- BioComm <- NULL

  # # Debugging
  boo.DEBUG <- FALSE
  # #
  if (boo.DEBUG == TRUE) {##IF.boo.DEBUG.START
    TargetSiteID = TargetSiteID
    stressors.sstv = stressors.sstv
    df_stressinfo = df_stressorMetadata
    df_paired = df_PairedStressResp
    biocomm = bioComm
    df_bioTaxaData = bioTaxaData
    df_MasterTaxa = bioMasterTaxa
    colBio = bioIndexGp
    plotvars = plotvars
    plotdpi = 600
    plotH = 6
    plotW = 8
    plotunits = "in"
    dir_plots = dir_results
    dir_sub = "_WoE"
    boo_plot = boo.plot.user
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

  # Initialize gap df
  df_gap <- data.frame(fxnname = character(), condition = character(), result = character(), comment = character())

  # Create vector of stressors (to identify data gaps)
  stressors <- as.vector(unlist(df_stressinfo$Stressor))

  # SSTV data gaps ----
  # if (length(stressors.sstv) == 0) { # duplicative of gap comment written in skeleton code
  #     gapcomment <- paste0("No stressor-specific tolerance values.")
  #     gaps <- cbind.data.frame("getVerifiedPredictions", "No SSTV data",
  #                              0, gapcomment)
  #     colnames(gaps) <- c("fxnname", "condition", "result", "comment")
  #     fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
  #     fn.gaps <- file.path(dir_plots, TargetSiteID,fn.gaps)
  #     utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
  #                 row.names = FALSE, sep = "\t")
  # }

  # Outer loop over stressors with SSTVs
  if (length(stressors.sstv) > 0) {
    ## Subset stressInfo ----
    sstv.name <- paste0("SSTVname.", tolower(biocomm))
    sstv.sens.max <- paste0("SensMax.", tolower(biocomm))
    sstv.sens.min <- paste0("SensMin.", tolower(biocomm))
    df_SSTV <- df_stressinfo %>%
      dplyr::rename(SSTVname := {{sstv.name}},
                    SensMin := {{sstv.sens.min}},
                    SensMax := {{sstv.sens.max}}) %>%
      dplyr::filter(Stressor %in% stressors.sstv) %>%
      dplyr::select(Stressor, LogTransf, SSTVname, SensMin, SensMax, Label)
    df_SSTV <- unique(df_SSTV)

    SSTVnames <- as.vector(unique(df_SSTV$SSTVname))
    mtcols <- colnames(df_MasterTaxa)

    if (exists("keepMTcol")) { suppressWarnings(rm(keepMTcol)) }

    # Match sstv to master taxa file ----
    # If debugging, run this loop
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
        gap.statement <- data.frame(
          fxnname = "getVerifiedPredictions",
          condition = SSTVlabel,
          result = "0",
          comment = paste0("No ", biocomm, " taxa have tolerance ",
                           "values available for this SSTV.")
        )

        df_gap <- df_gap |>
          dplyr::bind_rows(gap.statement)

        # gapcomment <- paste0("No ", biocomm, " taxa have tolerance ",
        #                      "values available for this SSTV.")
        # gaps <- cbind.data.frame("getVerifiedPredictions", SSTVlabel, 0, gapcomment)
        # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        # fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        # fn.gaps <- file.path(dir_plots, TargetSiteID, fn.gaps)
        # utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
        #             row.names = FALSE, sep = "\t")
        if (exists("deleteSSTVname")) {
          deleteSSTVnames <- c(deleteSSTVnames, name)
        } else {
          deleteSSTVnames <- name
        }
      }
      rm(SSTVlabel)
    }

    # Create taxa file for SSTVs ----
    # if debugging, run this
    if (exists("keepMTcol") == TRUE) { # Some stressors have SSTV vals in master taxa file
      # Merge biotaxa results with master taxa file ----
      df_SSTVtaxa <- df_MasterTaxa %>%
        dplyr::select(TaxonID, dplyr::all_of(keepMTcol))

      df_SSTVtaxa <- df_SSTVtaxa %>%
        dplyr::filter(dplyr::if_any(-1, ~ !is.na(.)))

      df_bioTaxaData <- merge(df_bioTaxaData, df_SSTVtaxa)

      boo.continue = TRUE
    } else {
      boo.continue = FALSE
    }

    # Taxa exist; isolate groups, labels
    if (boo.continue == TRUE) { # Have taxa

      # 20190513, remove scores file if exists
      fn_scores <-  file.path(dir.path, paste0(TargetSiteID, "_", biocomm,
                                               "_VP_SSTV_Scores.csv"))
      if (file.exists(fn_scores)) { file.remove(fn_scores) }

      # Filter for inside case sites & trim unnecessary columns
      df_stress.sstv <- df_paired %>%
        dplyr::filter(IncaseYN == 1) %>%
        dplyr::mutate(Quality = forcats::fct_expand(Quality, "Target")) %>%
        dplyr::select(StationID, IncaseCol, StressSampleID, StressSampleDate,
                      RespSampleID, RespSampleDate, BioComm,
                      dplyr::all_of(colBio),
                      RefSiteFlag, Quality, BetterThan,
                      dplyr::all_of(stressors.sstv))
      nTargetSamples <- nrow(df_stress.sstv[df_stress.sstv$StationID == TargetSiteID,])

      for (tv in seq_along(keepMTcol)) { # Obtain one or more sstv columns

        sstv <- keepMTcol[tv]
        # subset stressInfo for specific SSTV
        df_sstvInfo <- dplyr::filter(df_SSTV, SSTVname == sstv)
        sstv.label   <- df_sstvInfo$Label
        sstv.label   <- sstv.label[!is.na(sstv.label)]
        # assume SensMin is a vector, either character or numeric
        sstv.sensmin <- unlist(stringr::str_split(dplyr::select(df_sstvInfo, SensMin), ","))
        sstv.sensmin <- sstv.sensmin[!is.na(sstv.sensmin)]
        if (suppressWarnings(!is.na(as.numeric(sstv.sensmin[1])))) {
          sstv.sensmin <- as.numeric(sstv.sensmin)
        }
        # assume SensMax is a vector, either character or numeric
        sstv.sensmax <- unlist(stringr::str_split(dplyr::select(df_sstvInfo, SensMax),
                                                  ","))
        sstv.sensmax <- sstv.sensmax[!is.na(sstv.sensmax)]
        if (suppressWarnings(!is.na(as.numeric(sstv.sensmax[1])))) {
          sstv.sensmax <- as.numeric(sstv.sensmax)
        }

        # Modify taxon count data to identify sensmin and sensmin through sensmax
        if (is.numeric(sstv.sensmin) && is.numeric(sstv.sensmax)) { # tv is numeric

          if (length(sstv.sensmin) > 1) {
            # Handle case where sensmin defines endpoints of a range
            sstv.sensmin.gp <- df_bioTaxaData %>%
              dplyr::select(dplyr::all_of(sstv)) %>%
              dplyr::filter(dplyr::between(get(sstv), sstv.sensmin[1],
                                           sstv.sensmin[2]))
              # dplyr::filter(dplyr::between(dplyr::.data[[sstv]], sstv.sensmin[1],
              #                              sstv.sensmin[2]))
            sstv.sensmin.gp <- unique(sstv.sensmin.gp)
            sstv.sensmin.gp <- sort(as.numeric(unlist(sstv.sensmin.gp)))
          } else { # only one value for sensmin
            sstv.sensmin.gp <- sstv.sensmin
          }

          if (length(sstv.sensmax) > 1) {
            # Handle case where sensmax defines endpoints of a range
            sstv.sensmax.gp <- df_bioTaxaData %>%
              dplyr::select(dplyr::all_of(sstv)) %>%
              dplyr::filter(dplyr::between(get(sstv), sstv.sensmax[1],
                                           sstv.sensmax[2]))
            # dplyr::filter(dplyr::between(dplyr::.data[[sstv]], sstv.sensmax[1],
            #                              sstv.sensmax[2]))
            sstv.sensmax.gp <- unique(sstv.sensmax.gp)
            sstv.sensmax.gp <- sort(as.numeric(unlist(sstv.sensmax.gp)))
          } else { # only one value for sensmin
            sstv.sensmax.gp <- sstv.sensmax
          }

          sstv.sensall.gp <- union(sstv.sensmin.gp, sstv.sensmax.gp)

          # Generate Labels to be used as groups
          sstv.sensminLabel <- paste(sstv, "SensMin", sep = "_")
          sstv.sensallLabel <- paste(sstv, "SensAll", sep = "_")

          df_temp <- df_bioTaxaData %>%
            dplyr::mutate({{sstv.sensminLabel}} := ifelse(get(sstv) %in% sstv.sensmin.gp,
                                                          "Most sensitive",
                                                          NA),
                          {{sstv.sensallLabel}} := ifelse(get(sstv) %in% sstv.sensall.gp,
                                                          "All sensitive",
                                                          NA))
          # dplyr::mutate({{sstv.sensminLabel}} := ifelse(dplyr::.data[[sstv]] %in% sstv.sensmin.gp,
          #                                               "Most sensitive",
          #                                               NA),
          #               {{sstv.sensallLabel}} := ifelse(dplyr::.data[[sstv]] %in% sstv.sensall.gp,
          #                                               "All sensitive",
          #                                               NA))
        } else { # tv is character or vector
          sstv.sensminLabel <- paste(sstv, "SensMin", sep = "_")
          sstv.sensmin.gp   <- paste0(sstv.sensmin, collapse = ", ")
          sstv.sensmax.gp   <- paste0(sstv.sensmax, collapse = ", ")
          sstv.sensallLabel <- paste(sstv, "SensAll", sep = "_")
          sstv.sensall.gp   <- paste0(sstv.sensmin.gp, ", ", sstv.sensmax.gp)

          df_temp <- df_bioTaxaData %>%
            dplyr::mutate({{sstv.sensminLabel}} := ifelse(get(sstv) == sstv.sensmin,
                                                          sstv.sensmin.gp, NA),
                          {{sstv.sensallLabel}} := dplyr::case_when(get(sstv) %in%
                                                                      c(sstv.sensmin, sstv.sensmax) ~
                                                                      sstv.sensall.gp,
                                                                    TRUE ~ NA))
          # dplyr::mutate({{sstv.sensminLabel}} := ifelse(dplyr::.data[[sstv]] == sstv.sensmin,
          #                                               sstv.sensmin.gp, NA),
          #               {{sstv.sensallLabel}} := dplyr::case_when(dplyr::.data[[sstv]] %in%
          #                                                           c(sstv.sensmin, sstv.sensmax) ~
          #                                                           sstv.sensall.gp,
          #                                                         TRUE ~ NA))
        } ## End assignments

        if (tv == 1) { # merge temp df with df_resp
          df_resp <- df_temp
        } else {
          df_resp <- merge(df_resp, df_temp,
                           by = c("StationID", "RespSampleID", "RespSampleDate",
                                  "TaxonID", "NumInd", "PctInd", "PctTaxa",
                                  "NumTaxa", keepMTcol))
        }

        # Remove sstv variables, labels
        suppressWarnings(rm(sstv, sstv.sensmin, sstv.sensmax, sstv.label,
                            sstv.sensall, sstv.sensall.gp, sstv.sensallLabel,
                            sstv.sensmaxLabel, sstv.sensminLabel))

      } ## END for tv
      rm(df_temp)

      # Summarize data
      df_resp.summary <- df_resp %>%
        tidyr::pivot_longer(cols = dplyr::contains("Sens"),
                            names_to = "Group", values_to = "Label",
                            values_ptypes = character(),
                            values_drop_na = TRUE) %>%
        dplyr::group_by(StationID, RespSampleID, RespSampleDate, Group, Label) %>%
        dplyr::summarise(NumInds = sum(NumInd, na.rm = TRUE),
                         PctInds = sum(PctInd, na.rm = TRUE),
                         NumTaxa = dplyr::n(),
                         PctTaxa = sum(PctTaxa, na.rm = TRUE),
                         .groups = "drop_last") %>%
        dplyr::mutate(Group = sub("(_SensMin)$", "", Group),
                      Group = sub("(_SensAll)$", "", Group))

      df_GpLbl <- unique(df_resp.summary[, c("Group", "Label")])

      # MASTER dataframe ####
      df_tv <- merge(df_stress.sstv, df_resp.summary,
                     by = c("StationID", "RespSampleID", "RespSampleDate"),
                     all.x = TRUE)

      # Loop - Score SSTVs ####
      for (s in seq_along(stressors.sstv)) {

        stressor <- stressors.sstv[s]
        message(paste("Scoring", stressor))
        stressorLabel <- df_stressinfo$Label[df_stressinfo$Stressor == stressor]
        stressLog <- df_stressinfo$LogTransf[df_stressinfo$Stressor == stressor]
        stressorLabel <- ifelse(stressLog == 1,
                                paste0("Log1p ", stressorLabel),
                                stressorLabel)
        tolval <- df_SSTV$SSTVname[df_SSTV$Stressor == stressor]
        tolval.min <- paste0(tolval, "_SensMin")
        tolval.all <- paste0(tolval, "_SensAll")

        # Write graphics directory ----
        out.dir <- dirname(dir_plots)
        out.folders <- c(out.dir, basename(dir_plots), TargetSiteID, biocomm, stressor)

        for (f in 1:length(out.folders)) {
          if (f == 1) {
            dir_path_stress <- file.path(out.folders[f])
          } else {
            dir_path_stress <- file.path(dir_path_stress, out.folders[f])
          }
          if (dir.exists(dir_path_stress) == FALSE) {
            dir.create(dir_path_stress)
          }
        }

        ## Prep target sample data frame ####
        df_tv.target <- dplyr::filter(df_tv, Group == {{tolval}} &
                                        StationID == TargetSiteID)
        cols2use <- colnames(df_tv.target)

        # Determine if not all rows expected actually exist (no taxa in a category)
        if (nrow(df_tv.target) == 0) { # no target samples have any sensitive taxa
          df_stress.sstv.target <- df_stress.sstv %>%
            dplyr::filter(StationID == TargetSiteID) %>%
            dplyr::mutate(NumInds = 0, PctInds = 0,
                          NumTaxa = 0, PctTaxa = 0,
                          Group = tolval)
          df_tv.target <- merge(df_stress.sstv.target, df_GpLbl)
          df_tv.target <- df_tv.target %>%
            dplyr::select(dplyr::all_of(cols2use))
        } # This creates missing data for all stressor samples (with paired responses)

        # ASSUMPTION: Most sensitive will disappear prior to all sensitive
        if (nrow(df_tv.target) %% nTargetSamples != 0) {
          # Identify missing sample
          df_tv.target.qc1 <- df_tv.target %>%
            dplyr::select(StationID, RespSampleID, RespSampleDate, IncaseCol,
                          StressSampleID, StressSampleDate, BioComm,
                          dplyr::all_of(colBio), RefSiteFlag, Quality, BetterThan,
                          dplyr::all_of(stressor), Group, Label, NumInds, PctInds,
                          NumTaxa, PctTaxa) %>%
            dplyr::group_by(StationID, RespSampleID, RespSampleDate, IncaseCol,
                            StressSampleID, StressSampleDate, BioComm, Group) %>%
            dplyr::summarize(n = dplyr::n(), .groups = "drop_last") %>%
            dplyr::filter(n != 2)

          # Identify missing label
          df_tv.target.qc2 <- merge(df_tv.target.qc1, df_tv.target)
          allLabels <- unique(df_tv$Label[df_tv$Group == tolval])
          currentLabel <- unique(df_tv.target.qc2$Label)
          newLabel <- setdiff(allLabels, currentLabel)
          newLabel <- newLabel[!is.na(newLabel)]

          df_tv.target.qc2 <- df_tv.target.qc2 %>%
            dplyr::select(!n) %>%
            dplyr::mutate(Label = newLabel,
                          NumInds = 0,
                          PctInds = 0,
                          NumTaxa = 0,
                          PctTaxa = 0) %>%
            dplyr::select(dplyr::all_of(cols2use))

          df_tv.target <- rbind(df_tv.target, df_tv.target.qc2)

          rm(df_tv.target.qc1, df_tv.target.qc2)
        } # End if

        df_tv.target <- df_tv.target %>%
          dplyr::filter(!is.na(get(stressor))) %>% # Exclude samples ND for stressor
          #dplyr::filter(!is.na(dplyr::.data[[stressor]])) %>% # Exclude samples ND for stressor
          dplyr::select(dplyr::all_of(cols2use)) %>%
          dplyr::mutate(PctInds = signif(PctInds * 100, digits = 3),
                        PctTaxa = signif(PctTaxa * 100, digits = 3)) %>%
          tidyr::pivot_longer(cols = NumInds:PctTaxa, names_to = "variable",
                              values_to = "value")

        # Prep comparator (EXCLUDING target) sample data frame ####
        df_tv.incase <- dplyr::filter(df_tv, Group == {{tolval}} &
                                        Quality == "Not degraded") %>%
          dplyr::filter(StationID != TargetSiteID) %>% # Exclude Target Samples
          dplyr::filter(!is.na(get(stressor))) %>% # Exclude samples ND for stressor
          # dplyr::filter(!is.na(dplyr::.data[[stressor]])) %>% # Exclude samples ND for stressor
          dplyr::select(dplyr::all_of(cols2use)) %>%
          dplyr::mutate(PctInds = signif(PctInds * 100, digits = 3),
                        PctTaxa = signif(PctTaxa * 100, digits = 3)) %>%
          tidyr::pivot_longer(cols = NumInds:PctTaxa, names_to = "variable",
                              values_to = "value")

        # Scoring ####
        # Get percentiles for scoring
        # EXCLUDING Target samples
        df_quantiles.incase <- df_tv.incase %>%
          dplyr::select(Label, variable, value) %>%
          dplyr::group_by(Label, variable) %>%
          dplyr::summarise(Min = suppressWarnings(min(value, na.rm = TRUE)),
                           q25 = stats::quantile(value, probs = 0.25, na.rm = TRUE),
                           q50 = stats::quantile(value, probs = 0.50, na.rm = TRUE),
                           q75 = stats::quantile(value, probs = 0.75, na.rm = TRUE),
                           Max = suppressWarnings(max(value, na.rm = TRUE)),
                           .groups = "drop_last")

        df_tbl_scores <- merge(df_tv.target, df_quantiles.incase,
                               by = c("Label", "variable"))

        # Assign scores to target site
        df_tbl_scores <- df_tbl_scores %>%
          dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                        RespSampleDate, IncaseCol, BioComm, dplyr::all_of(colBio),
                        Quality, dplyr::all_of(stressor), Group, Label,
                        variable, value,
                        Min, q25, q50, q75, Max) %>%
          dplyr::rename(StressorValue = {{stressor}},
                        Response = variable,
                        ResponseValue = value) %>%
          dplyr::mutate(Score = dplyr::case_when(ResponseValue < q25 ~ 1,
                                                 dplyr::between(ResponseValue, q25, q50) ~ 0,
                                                 ResponseValue > q50 ~ -1),
                        StressorLabel = stressorLabel,
                        Stressor := {{stressor}}) %>%
          dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                        RespSampleDate, BioComm, dplyr::all_of(colBio), Quality,
                        StressorLabel, Stressor, StressorValue, Group, Label,
                        Response, ResponseValue, Min, q25, q50, q75, Max, Score)

        # boo_append <- TRUE
        # boo_colnames <- FALSE

        if (s == 1) {
          df.scores <- df_tbl_scores
        } else {
          df.scores <- rbind(df.scores, df_tbl_scores)
        }

        # Rbind target and comparator dataframes
        df_tv.incase <- rbind(df_tv.target, df_tv.incase)
        rm(df_tv.target)

        # Prepare plots ####
        # Boxplots: x = Label [SensMin, SensMax], y = value,
        # Group = variable [NumInds, PctInds, NumTaxa, PctTaxa], df_tv.incase
        df_tv.incase <- df_tv.incase %>%
          dplyr::mutate(Quality = as.character(Quality),
                        Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                        Quality = factor(Quality,
                                         levels = c("Not degraded", "Degraded", "Target"),
                                         labels = c("Not degraded", "Degraded", "Target")))

        df_tv.notTarget <- dplyr::filter(df_tv.incase, StationID != TargetSiteID)
        df_tv.target <- dplyr::filter(df_tv.incase, StationID == TargetSiteID)

        str_scores <- df_tbl_scores %>%
          dplyr::filter(Group == {{tolval}}) %>%
          dplyr::rename(variable = Response) %>%
          dplyr::select(Label, variable, Min, Max, q25, q50, RespSampleDate, Score) %>%
          dplyr::arrange(Label, variable, Min, Max, RespSampleDate) %>%
          dplyr::group_by(Label, variable, Min, Max, q25, q50) %>%
          dplyr::summarise(Scores = toString(Score),
                           .groups = "drop_last") %>%
          dplyr::mutate(#min = -10,
                        segNeg = ((q25 - Min) / 2) + Min,
                        aLabNeg = -1,
                        segZero = ((q50 - q25) / 2) + q25,
                        aLabZero = 0,
                        segPos = ((Max - q50) / 2) + q50,
                        aLabPos = 1,
                        Scores = paste0("Scores = ", Scores))

        str_scores_max <- str_scores %>%
          dplyr::group_by(variable) %>%
          dplyr::summarise(OverallMax = max(Max, na.rm = TRUE),
                           .groups = "drop_last")

        str_scores <- merge(str_scores, str_scores_max)
        rm(str_scores_max)

        ## Plot, Variables
        ## Prepare colors, sizes, etc  ----
        plotvars  <- plotvars %>%
          dplyr::filter(Type %in% c("target", "insideND", "insideD"))
        bio_fill    <- rev(unlist(plotvars$Fill)) # Degraded, Not degraded, Target
        bio_shape   <- rev(unlist(plotvars$Shape)) # down triangle, circle, and triangle
        bio_size    <- rev(unlist(plotvars$Size)) # Degraded, Not degraded, Target
        bio_alpha   <- rev(unlist(plotvars$Alpha)) # Degraded, Not degraded, Target

        # Prepare labels
        str_title <- paste0(TargetSiteID, ": Stressor-specific tolerance values line of evidence for ", stressorLabel)
        str_title <- stringr::str_wrap(str_title, 100)
        str_subtitle <- "Does the target sample have a lower number of individuals, percent of individuals, number of taxa, and percent of all sensitive and the most sensitive taxa compared to unimpaired, comparator samples?"
        str_subtitle <- stringr::str_wrap(str_subtitle, 100)
        legendtitle <- "Samples"
        str_xlab  <- ""

        ##PLOT VARIABLES ~ END

        fn_png_p1 <- paste0(TargetSiteID, "_", make.names(stressor), "_",
                            biocomm, "_", colBio, "_VPSSTV.png")

        p_tv <- ggplot2::ggplot(data = df_tv.incase,
                                ggplot2::aes(x = Label, y = value, group = Label)) +
          ggplot2::geom_boxplot(data = df_tv.incase,
                                outliers = FALSE,
                                #outliers = TRUE,
                                #outlier.size = 0.5,
                                na.rm = TRUE,
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
                                ggplot2::aes(group = Label),
                                outliers = FALSE,
                                # outliers = TRUE,
                                # outlier.size = 0.5,
                                na.rm = TRUE,
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
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12),
                         plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10)) +
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_text(size = 8),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(size = 8),
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




        # if(targetSampleLabels == TRUE){
        #   p_tv +
        #     ggplot2::geom_jitter(data = df_tv.target,
        #                          ggplot2::aes(color = Quality, shape = Quality,
        #                                       fill = Quality, alpha = Quality),
        #                          na.rm = TRUE, width = 0.2, height = 0.01,
        #                          size = 1.5) +
        #     ggrepel::geom_label_repel(data = df_tv.target, ggplot2::aes(label = RespSampleID, color = Quality), size = 3)
        # }
        #

        if (boo_plot) {
          ggplot2::ggsave(filename = file.path(dir_path_stress, fn_png_p1),
                          plot = p_tv, dpi = plotdpi, width = plotW,
                          height = plotH, units = plotunits)
        }## IF ~ boo_plot ~ END

      }## FOR SSTV ~ END

      df.scores <- df.scores %>%
        dplyr::group_by(StationID, RespSampleID, RespSampleDate, StressSampleID,
                        StressSampleDate, BioComm, !!rlang::sym(colBio), Quality,
                        StressorLabel, StressorValue, Group) %>%
        # dplyr::group_by(StationID, RespSampleID, RespSampleDate, StressSampleID,
        #                 StressSampleDate, BioComm, dplyr::.data[[colBio]], Quality,
        #                 StressorLabel, StressorValue, Group) %>%
        dplyr::summarise(Score = mean(Score, na.rm = TRUE), .groups = "drop_last") %>%
        dplyr::rename(bioIndex := {{colBio}}, bioComm = BioComm,
                      Stressor = StressorLabel) %>%
        dplyr::mutate(LoE = "VP_SSTV", bioIndexName = colBio) %>%
        dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                      RespSampleDate, bioComm, bioIndexName, bioIndex, Quality,
                      Stressor, StressorValue, LoE, Score)

      write.csv(df.scores, file = fn_scores, row.names = FALSE)

    }## IF ~ boo_continue ~ END

  }## IF ~ SSTV ~ END

  return(list(df.scores = df.scores,
              df_gap = df_gap))

}##FUNCTION.END

