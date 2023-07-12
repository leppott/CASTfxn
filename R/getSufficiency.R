#  Copyright 2023 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
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
#' Uses the libraries dplyr, wrapr, and ggplot2.
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
getSufficiency <- function(df_data
                       , TargetSiteID = NULL
                       , col_ID
                       , colStressSamp
                       , colRespSamp
                       , colGroup
                       , colBio
                       , colStressors
                       , df_stressinfo
                       , BioDegBrk = c(-2, 0.799, 2)
                       , BioDegLab = c("Yes", "No")
                       , biocomm = "bmi"
                       , dir_plots = file.path(getwd(), "Results")
                       , dir_sub = "Sufficiency"
                       , boo_plot = TRUE
                       ) {##FUNCTION.START

  boo_DEBUG <- FALSE

  if (boo_DEBUG==TRUE) {
    df_data = data_bioCoOccur[data_bioCoOccur$StationID_Master %in% comp_sites, ]
    TargetSiteID = TargetSiteID
    col_ID = "StationID_Master"
    colStressSamp = "StressSampID"
    colRespSamp = "RespSampID"
    colGroup = "clust"
    colBio = colBio
    colStressors = stressorsWPairedResponses
    df_stressinfo = data_stressInfo
    BioDegBrk = BioDegBrk
    BioDegLab = c("Yes", "No")
    biocomm = bioComm
    dir_plots = dir_results
    dir_sub = "Sufficiency"
    boo_plot = TRUE
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)

  # QC, 20190418
  colStressors <- unique(colStressors)

  # QC, 20190418
  colStressors.NotPresent <- colStressors[!(colStressors %in% names(df_data))]
  if (length(colStressors.NotPresent) != 0) { ##IF~bad stressors~START
    msg.warning <- paste0("Stressors listed below are not present in the "
                          , "provided data frame (df_data) and were not analyzed: \n"
                          , paste(colStressors.NotPresent, collapse="\n"), "\n\n")
    message(msg.warning)
    utils::flush.console()
    colStressors <- colStressors[colStressors %in% names(df_data)]
  } ##IF~bad stressors~END

  #
  col.Bio.Deg   <- "Bio.Deg"
  col.KEEP      <- c(col_ID, colGroup, colStressSamp, colRespSamp, colBio
                     , col.Bio.Deg, colStressors)
  #
  # Assign Bio Status
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
  if (is.null(TargetSiteID)) { ##IF.isnull.ID.START
    TargetSiteID <- as.character(sort(unique(df_data[,col_ID])))[1]
  } ##IF.isnull.ID.END


  # Create Score Output File
  df.scores <- df_data[, col.KEEP]
  df.scores[, "Param_Name"]  <- as.character(NA)
  df.scores[, "Param_Value"] <- as.numeric(NA)
  df.scores[, "n"]           <- as.character(NA)
  df.scores[, "q50"]         <- as.numeric(NA)
  df.scores[, "SR_pred_Deg"] <- as.character(NA)
  df.scores[, "Sc_SR"]       <- as.character(NA)
  df.scores[, "biocomm"]     <- as.character(NA)
  df.scores[, "Label"]       <- as.character(NA)

  # Remove columns
  col.remove <- names(df.scores) %in% colStressors
  df.scores <- df.scores[, !col.remove]
  #
  # remove all rows
  df.scores <- df.scores[0, ]

  #
  # if (boo_DEBUG == TRUE) { ##IF.boo_DEBUG.START
  #   i <- TargetSiteID[1]
  # } ##IF.boo_DEBUG.END
  # outside loop just in case forget to turn off debug flag

  # Analysis for each "test" sample
  # QC (site in data) ####
  boo_QC_site <- TargetSiteID %in% df_data[, col_ID]
  if (boo_QC_site == FALSE) { ##IF~boo_QC_site~START
    name_df <- deparse(substitute(df_data))
    name_col <- deparse(substitute(col_ID))
    name_df_col <- paste0(name_df, name_col)
    msg_NoSite <- paste0("Target site (", TargetSiteID
                         , ") was *not* found in the function inputs "
                         , "(df_data, TargetSiteID, and col_ID).")
    stop(msg_NoSite)
  } ##IF~boo_QC_site~END
  #

  # Create subdirectory
  dir_sub2 <- TargetSiteID
  dir_sub3 <- biocomm
  dir_sub4 <- dir_sub
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2)) == TRUE
         , dir.create(file.path(dir_plots, dir_sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2, dir_sub3)) == TRUE
         , dir.create(file.path(dir_plots, dir_sub2, dir_sub3))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4)) == TRUE
         , dir.create(file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4))
         , FALSE)

  dir_path <- file.path(dir_plots, dir_sub2, dir_sub3, dir_sub4)

  # Save scores file (append to later)
  fn.scores <- file.path(dir_path, paste0(i_TargetSiteID, "_", biocomm
                                          , "_SRLog_Scores.tab"))

  utils::write.table(df.scores, file = fn.scores, append = FALSE
                     , col.names = TRUE, row.names = FALSE, sep = "\t")
  #

  # Start evaluation
  i.num <- match(i, TargetSiteID)
  i.len <- length(TargetSiteID)
  #
  df.i <- df_data[df_data[, col_ID] == TargetSiteID, col.KEEP]
  i.Group <- df.i[, colGroup][1]
  i.Bio <- min(df_data[df_data[, col_ID] == TargetSiteID, colBio], na.rm = TRUE)

  # Filter for selected variables
  mapping <- c(COL.GROUP = colGroup, COL.BIO = colBio)
  # Comparator Site Data
  wrapr::let(alias = mapping
             , expr = {
               df.comp <- df_data[, col.KEEP] %>%
                 dplyr::filter(COL.GROUP == i.Group)
             })
  #
  if (boo_DEBUG == TRUE) { ##IF.boo_DEBUG.START
    j <- colStressors[1]
  } ##IF.boo_DEBUG.END
  # outside loop just in case forget to turn off debug flag

  # Calculate quantiles on Comparator Sites
  # Loop, j ####
  for (j in colStressors) { ##FOR.j.START
    #
    j.num <- match(j, colStressors)
    j.len <- length(colStressors)
    #
    ij.num <- ((i.num - 1) * j.len) + j.num
    ij.len <- i.len * j.len
    #
    message(paste0("Processing item (", ij.num, "/", ij.len, "); ID ("
                   , i.num, "/", i.len, ") ", i, "; Stressors (", j.num
                   , "/", j.len, ") ", j, ".\n"))
    utils::flush.console()
    #
    df.i[, paste0("n_", j)] <- sum(!is.na(df.comp[, j]))

    # Plots
    # Need to filter df.i to get rid of NA for "j" (stressor)
    # order values by j then get multiple comp scores
    df.i.n <- df.i[!is.na(df.i[, j]), ]
    df.i.n <- df.i.n[order(df.i.n[, j]), ]

    if (nrow(df.i.n) != 0) { ##IF.nrow.START
      # Save to Score/Results file
      df.i.n[, "Param_Name"]  <- j
      df.i.n[, "Param_Value"] <- df.i.n[, j]
      df.i.n[, "n"]           <- df.i.n[, paste0("n_", j)]
      df.scores.i.n <- merge(df.scores, df.i.n[, (names(df.i.n) %in% names(df.scores))]
                             , all.y=TRUE)
      # 2019-05-20, sort by score
      df.scores.i.n <- df.scores.i.n[order(df.scores.i.n[, "Param_Value"]), ]

      ## Box Plot of Comparator Sites (with better bio)
      lab.N     <- paste0("n = ",df.i[,paste0("n_", j)][1])

      # plots ####
      # File Names
      fn_png_p1 <- paste0(i_TargetSiteID, "_", biocomm, "_SRInLog_"
                          , make.names(j), ".png")
      ppi       <- 300

      # Create (ggplot)
      bio_col <- c("dark gray", "blue")
      bio_shp <- c(21, 25) # circle and down triangle
      bio_size <- c(3, 2)
      lab_comp <- paste0("Comparator samples selected from cluster = ", i.Group)

      ## Plot, Variables, Target Site Line
      targ_line_col <- "red"
      targ_line_lty <- 2
      targ_line_lwd <- 1

      # Get wordy label for the y-axis
      jlog <- df_stressinfo$LogTransf[df_stressinfo$StdParamName == j]
      jlabel <- df_stressinfo$Label[df_stressinfo$StdParamName == j]
      legendtitle <- "Degraded samples"

      maintitleSR <- paste0(i, ": Stressor-response (logistic regression) line of evidence")
      subtitleSR <-"Are stressor levels sufficient to explain the observed impairment?"
      subtitleSR <- stringr::str_wrap(subtitleSR, 100)

      ## Logistic Regression (all comparator sites)
      # #~~~~~~~~~~~~~~~~~~~
      # (plot with all sites in cluster (comparators) not just by condition group)
      col.glm <- c(colBio, col.Bio.Deg, j)
      df.comp.glm <- df.comp[stats::complete.cases(df.comp[, col.glm]), col.glm]

      # create data frame with known column names
      df.plot <- df.comp.glm
      # Confirm Levels (factors) as 1=No and 2=Yes
      df.plot$Bio.Deg <- factor(df.plot$Bio.Deg, c("No", "Yes"))

      # fix so 0=No and 1=Yes
      df.plot$y.name <- as.numeric(df.plot$Bio.Deg) - 1

      # If LogTransf == TRUE, then test both untransformed & transformed models
      if (jlog == 1) {
        df.plot$log1p_j <- log1p(df.comp.glm[[j]])
        names(df.plot) <- c("y", "Bio.Deg", "x1", "y.name", "x2")

        if (sum(stats::complete.cases(df.plot)) > 0) {
          # Test orig model (fit1)
          df.plot1 <- dplyr::select(df.plot, y, Bio.Deg, x1, y.name)
          fit1 <- stats::glm(y.name ~ x1, data = df.plot1, family = stats::binomial)
          # Test log1p model (fit2)
          df.plot2 <- dplyr::select(df.plot, y, Bio.Deg, x2, y.name)
          fit2 <- stats::glm(y.name ~ x2, data = df.plot2, family = stats::binomial)
          # Compare two models
          if (fit2$deviance <= fit1$deviance) {
            df.plot <- df.plot2 %>%
              dplyr::select(y, Bio.Deg, y.name, x2) %>%
              dplyr::rename(x = x2)
            jlabel <- paste0("Log1p ", jlabel)
            j_values <- data.frame(x = log1p(df.scores.i.n[, "Param_Value"]))
          } else {
            df.plot <- df.plot1 %>%
              dplyr::select(y, Bio.Deg, y.name, x1) %>%
              dplyr::rename(x = x1)
            j_values <- data.frame(x = df.scores.i.n[, "Param_Value"])
          }
          rm(fit1, fit2, df.plot1, df.plot2)
        }
        fit <- stats::glm(y.name ~ x, data = df.plot, family = stats::binomial)
      } else if (sum(stats::complete.cases(df.plot)) > 0) {
        names(df.plot) <- c("y", "Bio.Deg", "x", "y.name")
        fit <- stats::glm(y.name ~ x, data = df.plot, family = stats::binomial)
        j_values <- data.frame(x = df.scores.i.n[, "Param_Value"])
      } else { # no complete cases
        # NEEDS SOMETHING HERE!
      }
      #  Stressor Response Curve
      n_cc_df_plot <- sum(stats::complete.cases(df.plot[, c("x", "y")]))
      # create data for curve
      newdat <- data.frame(x = seq(min(df.plot$x, na.rm = TRUE)
                                   , max(df.plot$x, na.rm = TRUE), len = 100))
      newdat$y.name <- stats::predict(fit, newdata = newdat, type = "response")

      # Scoring
      j_SR_predict <- stats::predict(fit, newdata = j_values, type = "response")
      j_SR_score <- cut(j_SR_predict
                        , breaks = c(0, 0.2, 0.5, 1)
                        , labels = c(-1, 0, 1))

      # Add scores df so can save
      df.scores.i.n[, "SR_pred_Deg"] <- j_SR_predict
      df.scores.i.n[, "Sc_SR"] <- j_SR_score

      lab.sub <- paste0("All comparator samples (n=", n_cc_df_plot
                        , ").\n Score = ", paste(j_SR_score, collapse = ", ")
                        , ".")

      # plot1, ggplot ####
      p1 <- ggplot2::ggplot(df.plot, ggplot2::aes(x = x, y = y.name)) +
        ggplot2::geom_point(ggplot2::aes(color = Bio.Deg, shape = Bio.Deg
                                         , fill = Bio.Deg)
                            , alpha = 0.5, size = 2, na.rm = TRUE) +
        ggplot2::scale_fill_manual(name = legendtitle
                                   , breaks = c("Yes", "No")
                                   , values = bio_col, drop = FALSE) +
        ggplot2::scale_color_manual(name = legendtitle
                                    , breaks = c("Yes", "No")
                                    , values = bio_col, drop = FALSE) +
        ggplot2::scale_shape_manual(name = legendtitle
                                    , breaks = c("Yes", "No")
                                    , values = bio_shp, drop = FALSE) +
        ggplot2::geom_vline(xintercept = log1p(df.i[,j]), color = targ_line_col
                            , lty = targ_line_lty, lwd = targ_line_lwd
                            , na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = c(0.2, 0.5), color = "black"
                            , lty = 2, na.rm = TRUE) +
        ggplot2::labs(title = i, y = "Relative probability of degraded condition"
                      , x = jlabel) +
        ggplot2::geom_line(ggplot2::aes(y = y.name, x = x), data = newdat
                           , color = "blue", lwd = 1, na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)
                       , plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(title = maintitleSR, subtitle = subtitleSR, caption = lab.sub)

      ggplot2::ggsave(filename = file.path(dir_path, fn_png_p1)
                      , plot = p1
                      , dpi = ppi, width = 8, height = 6, units = "in")

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
                        , "q75", "Sc_Box", "SR_pred_Deg", "Sc_SR")
      df.i.NA <- df.i[1, 1:5]
      df.i.NA[, column_names] <- NA
      df.i.NA[, "Param_Name"] <- j
      # add biocomm, 20190425
      df.i.NA[, "biocomm"] <- biocomm
      df.i.NA[, "Label"] <- unique(jlabel)
      utils::write.table(df.i.NA, file = fn.scores, col.names = FALSE
                         , row.names = FALSE, sep = "\t", append = TRUE)

    }##IF.nrow.END

  }##FOR.j.END

}##FUNCTION.END

