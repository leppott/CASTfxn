#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
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
#' Box plots are used to show the distribution of the stressor levels at compartor
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
#' @param df_data data frame with data.
#' @param TargetSiteID ID of station/sample to plot; can be single or multiple.
#' Default is first entry in df_data[, "StationID"]
#' @param colBio df_data column with biological numeric value.
#' @param colStressors df_data column(s) with stressor variable(s); can be
#' single or multiple.
#' @param df_stressinfo dataframe containing stressor metadata (UseYN, LogTransf, Label)
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param dir_plots Directory to save plots.  Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "CoOccurrence"
#' @param col.Stressor.InvSc Stressors as columns of df_data that have inverse scoring for box plots.
#' Default = pH and DO; c("DO_f_.", "DO_f_mg_L", "DO_f_unk", "DOSat_f_."
#' , "DOSat_f_unk", "DO_uf_mg_L", "pH", "pH_SU")
#' @param boo_plot Boolean value to save plots.  Default = TRUE.
#'
#' @return Writes individual plots as pngs, and a tab-delimited text file with
#'         scores for each line of evidence (co-occurrence & sufficiency) to a
#'         "Results/TargetSiteID/BioComm/CoOccurrence" directory.
#'
#' @examples
#' \dontrun{
#' # Example #1, CA data (multiple sites)
#' #
#' #Load Data
#' df_data <- data_CoOccur_CA
#' #
#' colGroup     <- "Group"
#' colBio       <- "CSCI"
#' colStressors <- c("DO_uf_mg_L", "TN_uf_mg_L", "TP_mg_L")
#' col_ID        <- "StationID"
#' #
#' BioNarBrk <- c(-2, 0.62, 0.799, 0.919, 2)
#' BioNarLab <- c("very likely altered", "likely altered"
#'                 , "possibly altered ", "likely intact")
#' BioDegBrk <- c(-2, 0.799, 2)
#' BioDegLab <- c("Yes", "No")
#' biocomm <- "bmi"
#' dir_plots <- file.path(getwd(), "Results")
#' dir_sub <- "CoOccurrence"
#' #
#' TargetSiteID <- c("SMC08335", "901SJSJC9", "911TCAM01", "403STC004")
#' #
#' # Specify stressors by name
#' col_StressInvScore <- c("DO_uf_mg_L", "pH")
#'
#' #
#' getCoOccur(df_data, TargetSiteID, col_ID, colGroup, colBio, colStressors
#'         , BioNarBrk, BioNarLab, BioDegBrk, BioDegLab
#'         , biocomm, dir_plots, dir_sub, col_StressInvScore
#'         )
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example #2, AZ data (single site)
#' #
#' TargetSiteID <- c("SRCKN001.61")
#' #
#' # comparator Data based on elevation category
#' boo_Lo <- TargetSiteID %in% data_CoOccur_AZ_Lo$StationID
#' if(boo_Lo==TRUE){
#'    df_data <- data_CoOccur_AZ_Lo
#' } else {
#'    df_data <- data_CoOccur_AZ_Hi
#' }
#' #
#' colGroup     <- "Group"
#' colBio       <- "IBI"
#' colStressors <- c("Calcium_uf_mg_L", "Copper_uf_ug_L", "DO_f_mg_L", "SpecCond_umhos_cm")
#' col_ID        <- "StationID"
#' #
#' BioNarBrk <- c(0, 45, 52, 100)
#' BioNarLab <- c("Most Disturbed", "Intermediate", "Least Disturbed")
#' BioDegBrk <- c(0, 45, 100)
#' BioDegLab <- c("Yes", "No")
#' biocomm <- "bmi"
#' dir_plots <- file.path(getwd(), "Results")
#' dir_sub <- "CoOccurrence"
#'
#' # Specify stressors by name
#' #col_StressInvScore <- c("DO_f_.", "DO_f_mg_L", "DO_f_unk", "DOSat_f_.", "DOSat_f_unk", "pH_SU")
#' # Get stressors from chem.info
#' col_StressInvScore <- data_ChemInfo[data_ChemInfo[, "DirIncStress"] == "Dec", "StdParamName"]
#'
#' #
#' getCoOccur(df_data, TargetSiteID, col_ID, colGroup, colBio, colStressors
#'         , BioNarBrk, BioNarLab, BioDegBrk, BioDegLab
#'         , biocomm, dir_plots, dir_sub, col_StressInvScore
#'         )
#'}
#' @export
getCoOccur <- function(TargetSiteID
                       , df_data
                       # , col_ID
                       # , colStressSamp
                       # , colRespSamp
                       # , colGroup = "IncaseCol"
                       , incaseLabel = incaseLabel
                       , colBio = bioIndex
                       , useBetter = TRUE
                       , colStressors
                       , df_stressinfo
                       # , BioNarBrk = c(-2, 0.62, 0.799, 0.919, 2)
                       # , BioNarLab = c("very likely altered", "likely altered"
                       #                 , "possibly altered ", "likely intact")
                       # , BioDegBrk = c(-2, 0.799, 2)
                       # , BioDegLab = c("Yes", "No")
                       , biocomm = "BMI"
                       , dir_plots = file.path(getwd(), "Results")
                       , dir_sub = "CoOccurrence"
                       , col_StressInvScore
                       , boo_plot = TRUE
) {##FUNCTION.START

  boo_DEBUG <- FALSE

  if (boo_DEBUG==TRUE) {
    TargetSiteID = TargetSiteID
    df_data = data_bioCoOccur
    # col_ID = "StationID"
    # colStressSamp = "StressSampID"
    # colRespSamp = "RespSampID"
    # colGroup = "IncaseCol"
    incaseLabel = incaseLabel
    colBio = bioIndex
    colStressors = stressors
    df_stressinfo = data_stressInfo
    # BioNarBrk = BioNarBrk
    # BioNarLab = BioNarLab
    # BioDegBrk = BioDegBrk
    # BioDegLab = BioDegLab
    biocomm = bioComm
    dir_plots = dir_results
    dir_sub = "CoOccurrence"
    col_StressInvScore = col_StressInvScore
    pHlimLow = 6.5
    pHlimHigh = 9
    DOlim = 7
    boo_plot = TRUE
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)

  # QC, 20190418
  colStressors <- unique(colStressors)

  # QC, 20190418
  colStressors.NotPresent <- colStressors[!(colStressors %in% names(df_data))]
  if(length(colStressors.NotPresent)!=0){##IF~bad stressors~START
    msg.warning <- paste0("Stressors listed below are not present in the "
                          , "provided data frame (df_data) and were not analyzed: \n"
                          , paste(colStressors.NotPresent, collapse="\n"), "\n\n")
    message(msg.warning)
    utils::flush.console()
    colStressors <- colStressors[colStressors %in% names(df_data)]
  }##IF~bad stressors~END

  #
  myDateTime    <- format(Sys.time(),"%Y%m%d_%H%M%S")

  ### TODO: Lookup where degraded/not degraded is first assigned ----
  # Answer: When the biocoOccur file is generated (getCoOccurDataset)

  col.KEEP      <- c("StationID", "IncaseCol", "StressSampleID", "RespSampleID"
                     , colBio, "Quality", colStressors)
  #
  # default sample ID
  if(is.null(TargetSiteID)){##IF.isnull.ID.START
    TargetSiteID <- as.character(sort(unique(df_data[, "StationID"])))[1]
  }##IF.isnull.ID.END


  # Create Score Output File
  df.scores <- cbind(df_data[0, c("StationID", "IncaseCol", "StressSampleID"
                                  , "RespSampleID", colBio, "Quality")],
                     data.frame(Param_Name = character(), Param_Value = double()
                                , n = integer(), q25 = double(), q50 = double()
                                , q75 = double(), Sc_Box = character()
                                , biocomm = character(), Label = character()
                                , stringsAsFactors = FALSE))

  # QC (site in data) ####
  boo_QC_site <- TargetSiteID %in% df_data[, "StationID"]
  if(boo_QC_site==FALSE){##IF~boo_QC_site~START
    name_df <- deparse(substitute(df_data))
    name_col <- deparse(substitute("StationID"))
    name_df_col <- paste0(name_df, name_col)
    msg_NoSite <- paste0("Target site (", TargetSiteID
                         , ") was *not* found in the function inputs "
                         , "(df_data, column StationID).")
    stop(msg_NoSite)
  }##IF~boo_QC_site~END
  #
  #wd <- getwd()
  #dir.sub <- "Results"
  dir_sub2 <- TargetSiteID
  dir_sub3 <- biocomm
  dir_sub4 <- dir_sub
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2))==TRUE
         , dir.create(file.path(dir_plots, dir_sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2, dir_sub3))==TRUE
         , dir.create(file.path(dir_plots, dir_sub2, dir_sub3))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4))==TRUE
         , dir.create(file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4))
         , FALSE)

  dir_path <- file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4)
  plot_png <- vector(2, mode="list")
  # #
  # Save scores file (append to later)
  # fn.scores <- file.path(wd, dir.sub, dir_sub2, paste0(TargetSiteID,".CoOccurrence.Scores.", myDateTime,".txt"))
  fn.scores <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm
                                          , "_CO_Scores.tab"))
  utils::write.table(df.scores, file=fn.scores, append = FALSE
                     , col.names = TRUE, row.names=FALSE, sep="\t")
  #
  df.i <- df_data[df_data[, "StationID"] == TargetSiteID, col.KEEP]
  i.Group <- df.i[, "IncaseCol"][1]
  i.Bio <- min(df_data[df_data[, "StationID"] == TargetSiteID, colBio], na.rm=TRUE)

  # Filter for selected variables
  mapping <- c(COL.GROUP = "IncaseCol", COL.BIO = colBio)
  # Comparator Site Data
  wrapr::let(alias = mapping
             , expr = {
               df.comp <- df_data[, col.KEEP] %>% dplyr::filter(COL.GROUP == i.Group)
             })
  # Better Bio Comparator Site Data
  wrapr::let(alias = mapping
             , expr = {
               df.comp.bio.better <- df.comp %>% dplyr::filter(COL.BIO > i.Bio)
             })

  #
  if (boo_DEBUG==TRUE) {##IF.boo_DEBUG.START
    j <- colStressors[1]
    #par(mfrow=c(3,2))
  }##IF.boo_DEBUG.END
  # outside loop just in case forget to turn off debug flag

  # Calculate quantiles on Comparator Sites
  # Loop, j ####
  for (j in colStressors){##FOR.j.START
    #
    j.num <- match(j, colStressors)
    j.len <- length(colStressors)
    #
    message(paste0("Processing stressor (", j.num, "/", j.len, ") ", j, ".\n"))
    utils::flush.console()
    #
    if (useBetter) {
    df.i[ ,paste0("n_", j)] <- sum(!is.na(df.comp.bio.better[, j]))
    df.i[, paste0("q25_", j)] <- stats::quantile(df.comp.bio.better[, j]
                                                 , probs=0.25, na.rm=TRUE)
    df.i[, paste0("q50_", j)] <- stats::quantile(df.comp.bio.better[, j]
                                                 , probs=0.50, na.rm=TRUE)
    df.i[, paste0("q75_", j)] <- stats::quantile(df.comp.bio.better[, j]
                                                 , probs=0.75, na.rm=TRUE)
    } else {
      df.i[ ,paste0("n_", j)] <- sum(!is.na(df.comp[, j]))
      df.i[, paste0("q25_", j)] <- stats::quantile(df.comp[, j]
                                                   , probs=0.25, na.rm=TRUE)
      df.i[, paste0("q50_", j)] <- stats::quantile(df.comp[, j]
                                                   , probs=0.50, na.rm=TRUE)
      df.i[, paste0("q75_", j)] <- stats::quantile(df.comp[, j]
                                                   , probs=0.75, na.rm=TRUE)
    }
    # Comp Score for box plot
    if (j %in% col_StressInvScore) {##IF~j_in_InvSc~START
      ## Use different criteria for some parameters (Specifically pH and DO)
      if (grepl("^pH", j, perl = TRUE, ignore.case = FALSE) == TRUE) {  # Parameter is pH
        vals <- df_data %>%
          dplyr::filter(StationID == TargetSiteID) %>%
          dplyr::select(eval(j))
        vals <- as.vector(vals[!is.na(vals)])
        if(any(vals < pHlimLow)) {
          print("pH low")
          flush.console()
          # Inverse Scoring
          df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q50_", j)]
                                                 , -1
                                                 , ifelse(df.i[, j] < df.i[, paste0("q25_",j)]
                                                          , 1, 0))
        } else if(any(vals > pHlimHigh)) {
          print("pH high")
          flush.console()
          # Regular Scoring
          df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q75_", j)]
                                                 , 1
                                                 , ifelse(df.i[, j] < df.i[, paste0("q50_",j)]
                                                          , -1, 0))
          # col_StressInvScore <- setdiff(col_StressInvScore, j)
        }
      } else if (grepl("^DO", j, perl = TRUE, ignore.case = FALSE) == TRUE) {  #Parameter is DO
        vals <- df_data %>%
          dplyr::filter(StationID == TargetSiteID) %>%
          dplyr::select(eval(j))
        vals <- as.vector(vals[!is.na(vals)])

        if (any(vals < DOlim)) {
          print("DO low")
          flush.console()
          # Inverse Scoring
          df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q50_", j)]
                                                 , -1
                                                 , ifelse(df.i[, j] < df.i[, paste0("q25_", j)]
                                                          , 1, 0))
        } else {
          df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q50_", j)]
                                                 , -1, 0)
        }
      } else {
        # Inverse Scoring
        df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q50_", j)], -1
                                               , ifelse(df.i[, j] < df.i[, paste0("q25_", j)]
                                                        , 1, 0))
      }
    } else {
      # Regular Scoring
      df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q75_", j)], 1
                                             , ifelse(df.i[, j] < df.i[, paste0("q50_",j)], -1, 0))
    }##IF~j_in_InvSc~END

    df.i[is.na(df.i[, j]), paste0("Sc_Box_", j)] <- NA
    df.i[is.na(df.i[, paste0("Sc_Box_", j)]), paste0("Sc_Box_", j)] <- "NE"

    # Plots
    # Need to filter df.i to get rid of NA for "j" (stressor)
    # order values by j then get multiple comp scores
    df.i.n <- df.i[!is.na(df.i[, j]), ]
    df.i.n <- df.i.n[order(df.i.n[, j]), ]

    if (nrow(df.i.n) != 0) {##IF.nrow.START
      # Save to Score/Results file
      df.i.n[, "Param_Name"]  <- j
      df.i.n[, "Param_Value"] <- df.i.n[, j]
      df.i.n[, "n"]           <- df.i.n[, paste0("n_", j)]
      df.i.n[, "q25"]         <- df.i.n[, paste0("q25_", j)]
      df.i.n[, "q50"]         <- df.i.n[, paste0("q50_", j)]
      df.i.n[, "q75"]         <- df.i.n[, paste0("q75_", j)]
      df.i.n[, "Sc_Box"]      <- df.i.n[, paste0("Sc_Box_", j)]
      # df.i.n append to output (only keep matching columns)
      df.scores.i.n <- merge(df.scores, df.i.n[, (names(df.i.n) %in% names(df.scores))]
                             , all.y=TRUE)
      # 2019-05-20, sort by score
      df.scores.i.n <- df.scores.i.n[order(df.scores.i.n[, "Param_Value"]), ]

      ## Box Plot of Comparator Sites (with better bio)
      lab.Score <- paste0("Score = ", paste0(df.i.n[, paste0("Sc_Box_", j)]
                                             , collapse=", "))
      lab.N     <- paste0("n = ", df.i[,paste0("n_", j)][1])

      # plots ####
      # File Names
      fn_png_p1 <- paste0(TargetSiteID, "_", biocomm, "_CoOccur_", make.names(j), ".png")
      ppi       <- 300

      # Create (ggplot)
      bio_col <- c("midnightblue", "cyan2")
      bio_shp <- c(21, 25) # circle and down triangle
      bio_size <- c(3, 2)
      lab_comp <- paste0("Comparator samples selected from ", incaseLabel
                         , " = ", i.Group)

      # scoring lines
      if(j %in% col_StressInvScore){##IF~j_in_InvSc~START
        # Inverse Scoring
        box_qHI <- df.scores.i.n$q50[1]
        box_qLO <- df.scores.i.n$q25[1]
      } else {
        # Regular Scoring
        box_qHI <- df.scores.i.n$q75[1]
        box_qLO <- df.scores.i.n$q50[1]
      }##IF~j_in_InvSc~END

      ## Plot, Variables, Target Site Line
      targ_line_col <- "red"
      targ_line_lty <- 2
      targ_line_lwd <- 1

      # Get wordy label for the y-axis
      jlabel <- df_stressinfo$Label[df_stressinfo$StdParamName == j]
      jlog <- df_stressinfo$LogTransf[df_stressinfo$StdParamName == j]
      legendtitle <- "Samples"
      maintitleCO <- "Co-occurrence line of evidence"
      subtitleCO <-"Are the observed stressor levels consistent with impairment where and when it occurs?"
      subtitleCO <- stringr::str_wrap(subtitleCO, 100)

      # if non-empty
      # if(sum(is.na(df.comp.bio.better[, j]))!=nrow(df.comp.bio.better)){##IF~non-empty~START
      # plot1, ggplot ####
      if (useBetter) {
        df.plot <- df.comp.bio.better
        # lab.sub <- paste0("Comparator samples with higher ", colBio, " scores and paired "
        #                   , j, " data (", lab.N, ").\n ", lab.Score,".")
        lab.sub <- paste0("Comparator samples with higher ", colBio
                          , " scores (", lab.N, ").\n", lab.Score, ".")
      } else {
        df.plot <- df.comp
        lab.sub <- paste0("Comparator samples with paired ", j, " and ", colBio
                          , " (", lab.N, ").\n", lab.Score, ".")
      }

      p1<- ggplot2::ggplot(df.plot, ggplot2::aes(y = .data[[j]]  # ARL 2023-05-25
                                          , x = IncaseCol, group = IncaseCol)) +
        ggplot2::geom_boxplot(na.rm = TRUE) +
        ggplot2::coord_flip() +
        ggplot2::geom_point(ggplot2::aes(color = "black", shape = Quality
                                         , fill = Quality), alpha = 0.5
                            , na.rm = TRUE, position = "jitter") +
        # ggplot2::geom_jitter(size=2, alpha=0.5, na.rm=TRUE
        #                      , ggplot2::aes_string(color=col.SiteTypeQuality
        #                                            , shape=col.SiteTypeQuality
        #                                            , fill=col.SiteTypeQuality)) +
        ggplot2::geom_hline(yintercept = df.i[,j], color = targ_line_col
                            , lty = targ_line_lty, lwd = targ_line_lwd, na.rm = TRUE) +
        ggplot2::scale_color_manual(name = legendtitle
                                    , breaks = c("Degraded", "Not degraded")
                                    , values = bio_col, drop = TRUE) +
        ggplot2::scale_fill_manual(name = legendtitle
                                   , breaks = c("Degraded", "Not degraded")
                                   , values = bio_col, drop = TRUE) +
        ggplot2::scale_shape_manual(name = legendtitle
                                    , breaks = c("Degraded", "Not degraded")
                                    , values = bio_shp, drop = TRUE) +
        ggplot2::labs(title = maintitleCO, subtitle = subtitleCO, caption = lab.sub
                      , y = jlabel, x = lab_comp) +
        ggplot2::geom_hline(yintercept = c(box_qLO, box_qHI), color = "black"
                            , lty = 2, na.rm = TRUE) +
        # ggplot2::guides(colour = ggplot2::guide_legend("Samples")
        #                 , size = ggplot2::guide_legend("Samples")
        #                 , shape = ggplot2::guide_legend("Samples")) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)
                       , plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank()
                       # ggplot2::theme(axis.text.y=ggplot2::element_text(color="white")
                       , axis.ticks.y = ggplot2::element_blank())
      # Capture plot (png)
      # Capture most recent plot to a list
      # print(p1)
      # plots_pdf[[ij.num]] <- grDevices::recordPlot()
      # p1
      # plot_png[[1]] <- grDevices::recordPlot()
      if(boo_plot){
        ggplot2::ggsave(filename=file.path(dir_path, fn_png_p1)
                        , plot=p1
                        , dpi=ppi, width=8, height=6, units="in")
      }## IF ~ boo_plot ~ END
      #}##IF~non-empty~END


      # add biocomm, 20190425
      df.scores.i.n[, "biocomm"] <- biocomm
      df.scores.i.n[, "Label"] <- unique(jlabel)

      # Save tabular scores
      utils::write.table(df.scores.i.n, file=fn.scores
                         , col.names = FALSE, row.names=FALSE, sep="\t", append=TRUE)
      # Remove
      rm(df.scores.i.n)

    } else {
      # no data
      message(paste0("   All values NA for stressor (", j, ").\n"))
      utils::flush.console()
      # add data to scores table
      column_names <- c("Param_Name", "Param_Value", "n", "q25", "q50"
                        , "q75", "Sc_Box")
      df.i.NA <- df.i[1,1:5]
      df.i.NA[, column_names] <- NA
      df.i.NA[, "Param_Name"] <- j
      # add biocomm, 20190425
      df.i.NA[, "biocomm"] <- biocomm
      df.i.NA[, "Label"] <- unique(jlabel)
      utils::write.table(df.i.NA, file=fn.scores, col.names = FALSE
                         , row.names=FALSE, sep="\t", append=TRUE)

    }##IF.nrow.END
    #

  }##FOR.j.END
  #
  #    par(par.orig)
  #

}##FUNCTION.END
