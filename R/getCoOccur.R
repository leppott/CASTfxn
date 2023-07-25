#  Copyright 2023 TetraTech. All rights reserved.
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
#' @details \strong{Derive evidence fo spatial/temporal co-occurrence.}
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
#' Default is first entry in df_data[, col_ID]
#' @param col_ID df_data column with unique Station/Sample identifier.
#' @param colStressSamp df_data column with stressor sample identifier
#' @param colRespSamp df_data column with response sample identifier
#' @param colGroup df_data column containing "outside the case" sites, from
#'                 which comparator samples are selected
#' @param colBio df_data column with biological numeric value.
#' @param colStressors df_data column(s) with stressor variable(s); can be
#' single or multiple.
#' @param df_stressinfo dataframe containing stressor metadata (UseYN, LogTransf, Label)
#' @param BioNarBrk Biological assessment narrative, cut function breaks.
#' Should be in order from bad (low) to good (high).
#' Default = c(-2, 0.62, 0.799, 0.919, 2)
#' @param BioNarLab Biological assessment narrative, cut function labels.
#' Should be in order from bad (low) to good (high).
#' Default = c("very likely altered", "likely altered", "possibly altered ",
#' "likely intact")
#' @param BioDegBrk Biological assessment degraded status, cut function breaks.
#' Should be in order from bad (low) to good (high).
#' Default = c(-2, 0.799, 2)
#' @param BioDegLab Biological assessment degraded status, cut function labels.
#' Should be in order from bad (low) to good (high).
#' Defaults are referenced in the code so if change the code will break.
#' Default = c("Yes", "No").
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
#' col_ID        <- "StationID_Master"
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
#' boo_Lo <- TargetSiteID %in% data_CoOccur_AZ_Lo$StationID_Master
#' if(boo_Lo==TRUE){
#'    df_data <- data_CoOccur_AZ_Lo
#' } else {
#'    df_data <- data_CoOccur_AZ_Hi
#' }
#' #
#' colGroup     <- "Group"
#' colBio       <- "IBI"
#' colStressors <- c("Calcium_uf_mg_L", "Copper_uf_ug_L", "DO_f_mg_L", "SpecCond_umhos_cm")
#' col_ID        <- "StationID_Master"
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
                       , col_ID
                       , colStressSamp
                       , colRespSamp
                       , colGroup
                       , colBio
                       , colStressors
                       , df_stressinfo
                       , BioNarBrk = c(-2, 0.62, 0.799, 0.919, 2)
                       , BioNarLab = c("very likely altered", "likely altered"
                                       , "possibly altered ", "likely intact")
                       , BioDegBrk = c(-2, 0.799, 2)
                       , BioDegLab = c("Yes", "No")
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
    col_ID = "StationID_Master"
    colStressSamp = "StressSampID"
    colRespSamp = "RespSampID"
    colGroup = outcaseColName
    colBio = colBio
    colStressors = stressors
    df_stressinfo = data_stressInfo
    BioNarBrk = BioNarBrk
    BioNarLab = BioNarLab
    BioDegBrk = BioDegBrk
    BioDegLab = c("Yes", "No")
    biocomm = bioComm
    dir_plots = dir_results
    dir_sub = "CoOccurrence"
    col_StressInvScore = col_StressInvScore
    pHlimLow = 6.5
    pHlimHigh = 9
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
  col.Bio.Nar   <- "Bio.Nar"
  col.Bio.Deg   <- "Bio.Deg"
  #
  col.KEEP      <- c(col_ID, colGroup, colStressSamp, colRespSamp, colBio
                     , col.Bio.Nar, col.Bio.Deg, colStressors)
  #
  # Assign Bio Narrative and Status
  df_data[, col.Bio.Nar] <- cut(df_data[,colBio]
                                , breaks=BioNarBrk
                                , labels=BioNarLab)
  df_data[, col.Bio.Deg] <- cut(df_data[,colBio]
                                , breaks=BioDegBrk
                                , labels=BioDegLab)

  # Change Levels (factors) as 1=No and 2=Yes
  ## Used to later convert to 0=No (not degraded) and 1=Yes (degraded)
  df_data$Bio.Deg <- factor(df_data$Bio.Deg, c("No", "Yes"))

  # Add missing variable
  col.SiteTypeQuality <- col.Bio.Deg
  #
  # default sample ID
  if(is.null(TargetSiteID)){##IF.isnull.ID.START
    TargetSiteID <- as.character(sort(unique(df_data[,col_ID])))[1]
  }##IF.isnull.ID.END


  # Create Score Output File
  df.scores <- df_data[, col.KEEP]
  # # Add necessary Fields
  # Add columns
  df.scores[, "Param_Name"]  <- as.character(NA)
  df.scores[, "Param_Value"] <- as.numeric(NA)
  df.scores[, "n"]           <- as.character(NA)
  df.scores[, "q25"]         <- as.character(NA)
  df.scores[, "q50"]         <- as.character(NA)
  df.scores[, "q75"]         <- as.character(NA)
  df.scores[, "Sc_Box"]      <- as.character(NA)
  # df.scores[, "SR_pred_Deg"] <- as.character(NA)
  # df.scores[, "Sc_SR"]       <- as.character(NA)
  df.scores[, "biocomm"]     <- as.character(NA)
  df.scores[, "Label"]       <- as.character(NA)

  # Remove columns
  col.remove <- names(df.scores) %in% colStressors
  df.scores <- df.scores[, !col.remove]
  #
  # remove all rows
  df.scores <- df.scores[0, ]

  #
  if(boo_DEBUG==TRUE){##IF.boo_DEBUG.START
    i <- TargetSiteID[1]
  }##IF.boo_DEBUG.END
  # outside loop just in case forget to turn off debug flag

  # Analysis for each "test" sample
  # Loop, i ####
  for (i in TargetSiteID){##FOR.i.START
    #
    i_TargetSiteID <- i
    #
    # QC (site in data) ####
    boo_QC_site <- i_TargetSiteID %in% df_data[, col_ID]
    if(boo_QC_site==FALSE){##IF~boo_QC_site~START
      name_df <- deparse(substitute(df_data))
      name_col <- deparse(substitute(col_ID))
      name_df_col <- paste0(name_df, name_col)
      msg_NoSite <- paste0("Target site (", i_TargetSiteID
                           , ") was *not* found in the function inputs "
                           , "(df_data, TargetSiteID, and col_ID).")
      stop(msg_NoSite)
    }##IF~boo_QC_site~END
    #
    #wd <- getwd()
    #dir.sub <- "Results"
    dir_sub2 <- i_TargetSiteID
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
    # PDF, old ####
    #fn.pdf    <- paste0(TargetSiteID, ".CoOccurrence.ALL.", myDateTime,".pdf")
    # fn.pdf    <- paste0(TargetSiteID, ".CoOccurrence.ALL.pdf")
    # if(boo_DEBUG==FALSE){##IF.boo_DEBUG.START
    #   grDevices::pdf(file=file.path(wd, dir.sub, dir_sub2, fn.pdf), width=6, height=8)
    # }##IF.boo_DEBUG.END
    plots_pdf <- vector(1, mode="list")
    plot_png <- vector(2, mode="list")
    # #
    # Save scores file (append to later)
    # fn.scores <- file.path(wd, dir.sub, dir_sub2, paste0(TargetSiteID,".CoOccurrence.Scores.", myDateTime,".txt"))
    fn.scores <- file.path(dir_path, paste0(i_TargetSiteID, "_", biocomm
                                            , "_CO_Scores.tab"))
    utils::write.table(df.scores, file=fn.scores, append = FALSE
                       , col.names = TRUE, row.names=FALSE, sep="\t")
    #
    i.num <- match(i, TargetSiteID)
    i.len <- length(TargetSiteID)
    #
    df.i <- df_data[df_data[,col_ID]==i, col.KEEP]
    i.Group <- df.i[,colGroup][1]
    i.Bio <- min(df_data[df_data[, col_ID]==i, colBio], na.rm=TRUE)

    # Filter for selected variables

    mapping <- c(COL.GROUP=colGroup, COL.BIO=colBio)
    # Comparator Site Data
    wrapr::let(alias=mapping
               , expr={
                 df.comp <- df_data[, col.KEEP] %>% dplyr::filter(COL.GROUP==i.Group)
               })
    # Better Bio Comparator Site Data
    wrapr::let(alias=mapping
               , expr={
                 df.comp.bio.better <- df.comp %>% dplyr::filter(COL.BIO>i.Bio)
               })

    #
    if(boo_DEBUG==TRUE){##IF.boo_DEBUG.START
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
      ij.num <- ((i.num-1)*j.len) + j.num
      ij.len <- i.len * j.len
      #
      message(paste0("Processing item (",ij.num,"/",ij.len,"); ID ("
                     , i.num, "/", i.len, ") ", i
                     , "; Stressors (", j.num, "/", j.len, ") ", j, ".\n"))
      utils::flush.console()
      #
      df.i[ ,paste0("n_", j)] <- sum(!is.na(df.comp.bio.better[, j]))
      #df.i[, paste0("q20_", j)] <- stats::quantile(df.comp.bio.better[, j], probs=0.20, na.rm=TRUE)
      df.i[, paste0("q25_", j)] <- stats::quantile(df.comp.bio.better[, j]
                                                   , probs=0.25, na.rm=TRUE)
      df.i[, paste0("q50_", j)] <- stats::quantile(df.comp.bio.better[, j]
                                                   , probs=0.50, na.rm=TRUE)
      df.i[, paste0("q75_", j)] <- stats::quantile(df.comp.bio.better[, j]
                                                   , probs=0.75, na.rm=TRUE)
      # Comp Score for box plot
      ## Use different criteria for some parameters
      if(grepl("^pH_", j, perl = TRUE, ignore.case = FALSE)==TRUE) {
        vals <- df_data %>%
          dplyr::filter(StationID_Master==TargetSiteID) %>%
          dplyr::select(eval(j))
        vals <- as.vector(vals[!is.na(vals)])
        if(any(vals<pHlimLow)) {
          print("pH low")
          flush.console()
          # Inverse Scoring
          df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q50_", j)]
                                                 , -1
                                                 , ifelse(df.i[, j] < df.i[, paste0("q25_",j)]
                                                          , 1, 0))
        } else if(any(vals>pHlimHigh)) {
          print("pH high")
          flush.console()
          # Regular Scoring
          df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q75_", j)]
                                                 , 1
                                                 , ifelse(df.i[, j] < df.i[, paste0("q50_",j)]
                                                          , -1, 0))
          col_StressInvScore <- setdiff(col_StressInvScore, j)
        }
      } else if(j %in% col_StressInvScore){##IF~j_in_InvSc~START
        # Inverse Scoring
        df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q50_", j)], -1
                                               , ifelse(df.i[, j] < df.i[, paste0("q25_",j)], 1, 0))
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
        lab.N     <- paste0("n = ",df.i[,paste0("n_",j)][1])

        # plots ####
        # File Names
        #fn.pdf    <- paste0(TargetSiteID, ".CoOccurrence.ALL.", myDateTime,".pdf")
        # fn_title <- stringr::str_to_title(make.names(j))
        # fn_title <- gsub("\\s","",fn_title)
        # fn_title <- gsub("\\.","",fn_title)

        fn_png_p1 <- paste0(i_TargetSiteID, "_", biocomm, "_CoOccur_", make.names(j), ".png")
        # fn_png_p2 <- paste0(i_TargetSiteID, "_", biocomm, "_SRInLog_", make.names(j), ".png")
        # fn_png_p3 <- paste0(i_TargetSiteID, "_", biocomm, "_SRInLog_Log1p_", make.names(j), ".png")
        ppi       <- 300

        # Create (ggplot)
        # lab.sub <- paste0("Comparator samples with higher ", colBio, " scores and paired "
        #                   , j, " data (", lab.N, ").\n ", lab.Score,".")
        lab.sub <- paste0("Comparator samples with higher ", colBio
                          , " scores (", lab.N, ").\n", lab.Score, ".")
        bio_col <- c("dark gray", "blue")
        bio_shp <- c(21, 25) # circle and down triangle
        bio_size <- c(3, 2)
        lab_comp <- paste0("Comparator samples selected from cluster = ", i.Group)

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
        jlabel <- df_stressinfo$Label[df_stressinfo$StdParamName==j]
        jlog <- df_stressinfo$LogTransf[df_stressinfo$StdParamName==j]
        legendtitle <- "Degraded samples"
        maintitleCO <- paste0(i, ": Co-occurrence line of evidence")
        subtitleCO <-"Are the observed stressor levels consistent with impairment where and when it occurs?"
        subtitleCO <- stringr::str_wrap(subtitleCO, 100)

        # maintitleSR <- paste0(i, ": Stressor-response (logistic regression) line of evidence")
        # subtitleSR <-"Are stressor levels sufficient to explain the observed impairment?"
        # subtitleSR <- stringr::str_wrap(subtitleSR, 100)

        # if non-empty
        #if(sum(is.na(df.comp.bio.better[,j]))!=nrow(df.comp.bio.better)){##IF~non-empty~START
        # plot1, ggplot ####
        p1<- ggplot2::ggplot(df.comp.bio.better
                             # , ggplot2::aes_string(y=as.name(j)
                             , ggplot2::aes(y=.data[[j]]  # ARL 2023-05-25
                                            , x=colGroup, group=colGroup)) +
          ggplot2::geom_boxplot(na.rm = TRUE) +
          ggplot2::coord_flip() +
          ggplot2::geom_point(ggplot2::aes(color=Bio.Deg, shape=Bio.Deg
                                           , fill=Bio.Deg)
                              , alpha=0.5, na.rm = TRUE, position = "jitter") +
          # ggplot2::geom_jitter(size=2, alpha=0.5, na.rm=TRUE
          #                      , ggplot2::aes_string(color=col.SiteTypeQuality
          #                                            , shape=col.SiteTypeQuality
          #                                            , fill=col.SiteTypeQuality)) +
          ggplot2::geom_hline(yintercept = df.i[,j], color=targ_line_col
                              , lty=targ_line_lty, lwd=targ_line_lwd, na.rm = TRUE) +
          ggplot2::scale_color_manual(name=legendtitle
                                      , breaks=c("Yes", "No"), values=bio_col
                                      , drop=FALSE) +
          ggplot2::scale_fill_manual(name=legendtitle
                                     , breaks=c("Yes", "No"), values=bio_col
                                     , drop=FALSE) +
          ggplot2::scale_shape_manual(name=legendtitle
                                      , breaks=c("Yes", "No"), values=bio_shp
                                      , drop=FALSE) +
          ggplot2::labs(title=maintitleCO, subtitle=subtitleCO, caption=lab.sub) +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)
                         , plot.subtitle = ggplot2::element_text(hjust=0.5)) +
          ggplot2::theme(axis.text.y=ggplot2::element_blank()
          # ggplot2::theme(axis.text.y=ggplot2::element_text(color="white")
                         , axis.ticks.y=ggplot2::element_blank()) +
          ggplot2::labs(y=jlabel, x=lab_comp) +
          ggplot2::geom_hline(yintercept = c(box_qLO, box_qHI), color="black"
                              , lty=2, na.rm = TRUE)
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

        # ## Logistic Regression (all comparator sites)
        #
        # # #~~~~~~~~~~~~~~~~~~~
        # # (plot with all sites in cluster (comparators) not just by condition group)
        # col.glm <- c(colBio, col.Bio.Deg, j)
        # #df.comp.glm <- df.comp[complete.cases(df.comp[,col.glm]), col.glm]
        #
        # df.comp.glm <- df.comp[stats::complete.cases(df.comp[, col.glm]), col.glm]
        #
        # # create data frame with known column names
        # df.plot <- df.comp.glm
        # names(df.plot) <- c("y","Bio.Deg","x")
        # # Confirm Levels (factors) as 1=No and 2=Yes
        # df.plot$Bio.Deg <- factor(df.plot$Bio.Deg, c("No", "Yes"))
        # # fix so so 0=No and 1=Yes
        # df.plot$y.name <- as.numeric(df.plot$Bio.Deg) - 1
        #
        # n_cc_df_plot <- sum(stats::complete.cases(df.plot[,c("x","y")]))
        #
        # # 20190416, comment out p2 and p3, moving to getBSR
        # #  Stressor Response Curve
        # if(sum(stats::complete.cases(df.plot))>0){##IF.complete.cases.START
        #   #
        #   fit <- stats::glm(y.name ~ x, data=df.plot, family=stats::binomial)
        #   # create data for curve
        #   newdat <- data.frame(x = seq(min(df.plot$x, na.rm = TRUE)
        #                              , max(df.plot$x, na.rm = TRUE)
        #                              , len = 100))
        #   newdat$y.name <- stats::predict(fit, newdata = newdat
        #                                   , type = "response") #se.fit=TRUE
        #   # type=response is for probabilities.
        #
        #   # Scoring
        #   # j_values <- data.frame(x=df.i[,j])
        #   j_values <- data.frame(x = df.scores.i.n[, "Param_Value"])
        #   # sort values, 2019-05-20
        #   j_SR_predict <- stats::predict(fit, newdata = j_values, type = "response")
        #   j_SR_score <- cut(j_SR_predict
        #                     , breaks=c(0, 0.2, 0.5, 1)
        #                     , labels=c(-1, 0, 1))
        #
        #   # # Add scores df so can save
        #   df.scores.i.n[, "SR_pred_Deg"] <- j_SR_predict
        #   df.scores.i.n[, "Sc_SR"] <- j_SR_score
        #
        #   #
        #   # lab.sub <- paste0("All comparator samples with ", colBio, " and paired "
        #   #                   , j, " data (n=", n_cc_df_plot, ").\n Score = "
        #   #                   , paste(j_SR_score, collapse=", "),".")
        #   lab.sub <- paste0("All comparator samples (n=", n_cc_df_plot
        #                     , ").\n Score = ", paste(j_SR_score, collapse=", ")
        #                     , ".")
        #
        #   # plot2, ggplot ####
        #   p2 <- ggplot2::ggplot(df.plot, ggplot2::aes(x=x, y=y.name)) +
        #     ggplot2::geom_point(ggplot2::aes(color=Bio.Deg, shape=Bio.Deg
        #                                      , fill=Bio.Deg)
        #                         , alpha=0.5, size=2, na.rm = TRUE) +
        #     ggplot2::scale_fill_manual(name=legendtitle
        #                                , breaks=c("Yes", "No")
        #                                , values=bio_col, drop=FALSE) +
        #     ggplot2::scale_color_manual(name=legendtitle
        #                                 , breaks=c("Yes", "No")
        #                                 , values=bio_col, drop=FALSE) +
        #     ggplot2::scale_shape_manual(name=legendtitle
        #                                 , breaks=c("Yes", "No")
        #                                 , values=bio_shp, drop=FALSE) +
        #     ggplot2::geom_vline(xintercept = df.i[,j], color=targ_line_col
        #                         , lty=targ_line_lty, lwd=targ_line_lwd, na.rm = TRUE) +
        #     ggplot2::geom_hline(yintercept = c(0.2, 0.5), color="black"
        #                         , lty=2, na.rm = TRUE) +
        #     ggplot2::labs(title=i, y="Relative probability of degraded condition"
        #                   , x=jlabel) +
        #     ggplot2::geom_line(ggplot2::aes(y=y.name, x=x), data=newdat
        #                        , color="blue", lwd=1, na.rm = TRUE) +
        #     ggplot2::theme_bw() +
        #     ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)
        #                    , plot.subtitle = ggplot2::element_text(hjust=0.5)) +
        #     ggplot2::labs(title=maintitleSR, subtitle=subtitleSR, caption=lab.sub)
        #   # p2
        #   # plot_png[[2]] <- grDevices::recordPlot()
        #   if(boo_plot){
        #     ggplot2::ggsave(filename=file.path(dir_path, fn_png_p2)
        #                     , plot=p2
        #                     , dpi=ppi, width=8, height=6, units="in")
        #   }## IF ~ boo_plot ~ END

          # Save Plots
          #
          # PDF, p1 and p2
          #grDevices::pdf(file=file.path(wd, dir.sub, dir_sub2, fn.pdf), width=6, height=8)
          # p3 <- gridExtra::grid.arrange(p1, p2, ncol=1, nrow=2 )
          #p3
          # Capture most recent plot to a list
          # plots_pdf[[ij.num]] <- grDevices::recordPlot()
          # grDevices::dev.off()
          #
          # ggplot mods
          ## Size modifier - 4:3 isn't big enough for all of text on ggplots
          #size_mod <- 1.5
          #
          # # png, p1
          # grDevices::jpeg(filename = file.path(wd, dir.sub, dir_sub2, fn_png_p1)
          #                 , width = size_mod*4*ppi, height = size_mod*3*ppi, quality=100
          #                 , pointsize = 8
          #                 , res = ppi)
          #    grDevices::replayPlot(1)
          # grDevices::dev.off()
          #
          # png, p2
          # grDevices::jpeg(filename = file.path(wd, dir.sub, dir_sub2, fn_png_p2)
          #                 , width = size_mod*4*ppi, height = size_mod*3*ppi, quality=100
          #                 , pointsize=8
          #                 , res = ppi)
          #    grDevices::replayPlot(2)
          # grDevices::dev.off()
          #

        # } else {
        #   #
        #   # Save Plots
        #   #
        #   # PDF, p1 only
        #   # if(exists("p1")==TRUE & exists("p2")==FALSE){
        #   #   p3 <- gridExtra::grid.arrange(p1, ncol=1, nrow=2)
        #   # }
        #   # if(exists("p1")==FALSE & exists("p2")==TRUE){
        #   #   p3 <- gridExtra::grid.arrange(p2, ncol=1, nrow=2)
        #   # }
        #   # # print(p3)
        #   # # Capture most recent plot to a list
        #   # plots_pdf[[ij.num]] <- grDevices::recordPlot()
        #   # grDevices::dev.off()
        #   #
        #   # ggplot mods
        #   ## Size modifier - 4:3 isn't big enough for all of text on ggplots
        #   #size_mod <- 1.5
        #   #
        #   # png, p1
        #   # grDevices::jpeg(filename = file.path(wd, dir.sub, dir_sub2, fn_png_p1)
        #   #                 , width = size_mod*4*ppi, height = size_mod*3*ppi, quality=100
        #   #                 , pointsize=8
        #   #                 , res = ppi)
        #   #    grDevices::replayPlot(1)
        #   # grDevices::dev.off()
        #
        # }##IF.complete.cases.END

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
    # PDF, new ####
    # Create PDF from list of recorded plots
    # if(boo_DEBUG==FALSE){##IF.boo_DEBUG.START
    #   fn.pdf    <- paste0(i_TargetSiteID, "_", biocomm, "_CoOccurrence_ALL.pdf")
    #   grDevices::pdf(file=file.path(dir_path, fn.pdf)
    #                  , width=6, height=8) #p3
    #   for (p in plots_pdf){##FOR.gp.START
    #     if(is.null(p)==TRUE) {next}
    #     grDevices::replayPlot(p)
    #   }##FOR.gp.END
    #   rm(plots_pdf)
    #     #
    #   grDevices::dev.off()
    # }##IF.boo_DEBUG.END
    # PDF (ALL) (close for i)
    # grDevices::dev.off()

    #
  }##FOR.i.END

}##FUNCTION.END
