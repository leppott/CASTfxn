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
#' Uses the libraries dplyr, tidyr, and ggplot2.
#'
#' @param TargetSiteID ID of station to be evaluated. May have one or many samples.
#' @param df_data dataframe containing matched stressor-response data for the
#'                biological response community desired
#' @param compSites vector containing comparator site IDs
#' @param stressors vector of stressors identified as candidate causes
#' @param df_stressinfo dataframe containing stressor metadata (LogTransf, Label)
#' @param biocomm Biological community; BMI, algae, or fish  Default = "BMI".
#' @param colBio df_data column name for the field with biological index value.
#' @param BioDegBrk Biological assessment degraded status, cut function breaks.
#' Should be in order from bad (low) to good (high).
#' Default = c(-2, 0.799, 2)
#' @param BioDegLab Biological assessment degraded status, cut function labels.
#' Should be in order from bad (low) to good (high).
#' Defaults are referenced in the code so if change the code will break.
#' Default = c("Yes", "No").
#' @param dir_plots Directory to save plots. Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function. Default = "Sufficiency"
#'
#' @return Writes individual plots as pngs, and a tab-delimited text file with
#'         scores for the sufficiency line of evidence to a directory:
#'         "Results/TargetSiteID/BioComm/Sufficiency".
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
#' stressors <- c("DO_uf_mg_L", "TN_uf_mg_L", "TP_mg_L")
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
#' getCoOccur(df_data, TargetSiteID, col_ID, colGroup, colBio, stressors
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
#' stressors <- c("Calcium_uf_mg_L", "Copper_uf_ug_L", "DO_f_mg_L", "SpecCond_umhos_cm")
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
#' getCoOccur(df_data, TargetSiteID, col_ID, colGroup, colBio, stressors
#'         , BioNarBrk, BioNarLab, BioDegBrk, BioDegLab
#'         , biocomm, dir_plots, dir_sub, col_StressInvScore
#'         )
#'}
#' @export
getSufficiency <- function(TargetSiteID
                           , df_data
                           , compSites
                           , stressors
                           , df_stressinfo
                           , biocomm
                           , colBio
                           , BioDegBrk = c(-2, 0.799, 2)
                           , BioDegLab = c("Yes", "No")
                           , dir_plots = file.path(getwd(), "Results")
                           , dir_sub = "Sufficiency"
                           , boo_plot = TRUE
                           ) {##FUNCTION.START

  boo_DEBUG <- FALSE

  if (boo_DEBUG==TRUE) {

    df_data = data_bioCoOccur
    TargetSiteID = TargetSiteID
    compSites = comp_sites
    stressors = stressors
    df_stressinfo = data_stressInfo
    biocomm = bioComm
    colBio = bioIndex
    BioDegBrk = BioDegBrk
    BioDegLab = c("Yes", "No")
    dir_plots = dir_results
    dir_sub = "Sufficiency"
    boo_plot = boo_plot_user

  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)

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

  # Get dataset
  df_data <- df_data %>%
    dplyr::filter(StationID_Master %in% compSites) %>%
    dplyr::select(StationID_Master, StressSampID, StressSampDate, RespSampID
                  , RespSampDate, all_of(colBio), all_of(stressors))

  # Assign Bio Status
  df_data[, "Bio.Deg"] <- cut(df_data[, colBio]
                              , breaks = BioDegBrk
                              , labels = BioDegLab)
  # df_data[, "Bio.Nar"] <- cut(df_qual[, colBio]
  #                             , breaks = BioNarBrk
  #                             , labels = BioNarLab)

  # Change Levels (factors) as 1=No and 2=Yes
  ## Used to later convert to 0=No (not degraded) and 1=Yes (degraded)
  df_data$Bio.Deg <- factor(df_data$Bio.Deg, c("No", "Yes"))
  df_data <- dplyr::select(df_data, StationID_Master, StressSampID, StressSampDate
                           , RespSampID, RespSampDate, all_of(colBio), Bio.Deg
                           , all_of(stressors))

  df_target <- dplyr::filter(df_data, StationID_Master == TargetSiteID)

  # Transform stressor data as required
  strInfo <- as.data.frame(cbind("StdParamName" = stressors
                                 , "LogTransfYN" = stressors_logtransf))
  strInfo <- merge(strInfo, df_stressinfo[, c("StdParamName", "Label")]
                   , by = "StdParamName")

  # Create Score Output File # add Bio.Nar just before Bio.Deg
  df.scores <- df_data %>%
    dplyr::select(StationID_Master, StressSampID, RespSampID, all_of(colBio)
                  , Bio.Deg) %>%
    dplyr::mutate(ParamName   = as.character(NA)
                  , ParamValue  = as.numeric(NA)
                  , Log1pValue  = as.numeric(NA)
                  , n           = as.character(NA)
                  , SRpred_Deg  = as.character(NA)
                  , Sc_SRlog    = as.character(NA)
                  , BioComm       = as.character(NA)
                  , Label       = as.character(NA))
  # remove all rows
  df.scores <- df.scores[0, ]

  # Calculate quantiles on Comparator Sites
  # Loop, j ####
  for (j in seq_along(stressors)) { ##FOR.j.START
    #
    str = stressors[j]
    j.len <- length(stressors)
    jlog <- as.numeric(strInfo$LogTransfYN[strInfo$StdParamName == str])
    jlabel <- as.character(strInfo$Label[strInfo$StdParamName == str])
    #
    message(paste0("Processing item (", j, "/", j.len, "); ", str, "\n"))
    utils::flush.console()

    df.score.j <- df_data %>%
      dplyr::select(StationID_Master, StressSampID, StressSampDate
                    , RespSampID, RespSampDate, all_of(colBio), Bio.Deg
                    , all_of(str)) %>%
      dplyr::filter(StationID_Master == TargetSiteID) %>%
      tidyr::pivot_longer(cols = all_of(str), names_to = "ParamName"
                          , values_to = "ParamValue")

    df.plot <- df_data %>%
      dplyr::select(all_of(colBio), Bio.Deg, all_of(str))

    df.plot <- df.plot[!is.na(df.plot[, str]),]

    if (nrow(df.plot) > 0) {
      # If LogTransf == TRUE, then test both untransformed & transformed models
      if (jlog == 1) {
        df.plot <- df.plot %>%
          dplyr::rename(y = eval(colBio), x1 = all_of(str)) %>%
          dplyr::mutate(y.name = as.numeric(Bio.Deg) - 1
                        , x2 = log1p(x1))
        # df.plot$log1p <- log1p(df.comp.glm[[j]])
        # names(df.plot) <- c("y", "Bio.Deg", "x1", "y.name", "x2")

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
            useVal <- "log1p"
            j_values <- data.frame(x = log1p(df_target[, str]))
            x_intercept <- as.numeric(x = j_values)
          } else {
            df.plot <- df.plot1 %>%
              dplyr::select(y, Bio.Deg, y.name, x1) %>%
              dplyr::rename(x = x1)
            useVal <- "normal"
            j_values <- data.frame(x = df_target[, str])
            x_intercept <- as.numeric(x = j_values)
          }
          rm(fit1, fit2, df.plot1, df.plot2)
        }
        fit <- stats::glm(y.name ~ x, data = df.plot, family = stats::binomial)
      } else if (sum(stats::complete.cases(df.plot)) > 0) {
        df.plot <- df.plot %>%
          dplyr::rename(y = eval(colBio), x = all_of(str)) %>%
          dplyr::mutate(y.name = as.numeric(Bio.Deg) - 1) %>%
          dplyr::select(y, Bio.Deg, y.name, x)
        fit <- stats::glm(y.name ~ x, data = df.plot, family = stats::binomial)
        useVal <- "normal"
        j_values <- data.frame(x = df_target[, str])
        x_intercept <- as.numeric(x = j_values)
      } else { # no complete cases
        # NEEDS SOMETHING HERE!
      }

      #  Stressor Response Curve
      n_cc_df_plot <- nrow(df.plot[stats::complete.cases(df.plot[, c("x", "y")])
                                   , c("x", "y")])
      # create data for curve (type "response" gives probabilities)
      newdat <- data.frame(x = seq(min(df.plot$x, na.rm = TRUE)
                                   , max(df.plot$x, na.rm = TRUE), len = 100))
      newdat$y.name <- stats::predict(fit, newdata = newdat, type = "response")

      # Scoring
      j_SR_predict <- stats::predict(fit, newdata = j_values, type = "response")
      j_SR_score <- cut(j_SR_predict
                        , breaks = c(0, 0.2, 0.5, 1)
                        , labels = c(-1, 0, 1))
      # plot ####
      # File Names
      fn_png_p1 <- paste0(TargetSiteID, "_", biocomm, "_SRInLog_", str, ".png")
      ppi       <- 300

      # Create (ggplot)
      bio_col <- c("dark gray", "blue")
      bio_shp <- c(21, 25) # circle and down triangle
      bio_size <- c(3, 2)
      # lab_comp <- paste0("Comparator samples selected from outside the case ("
      #                    , outcaseLabel, " ", outcaseID, ")")

      ## Plot, Variables, Target Site Line
      targ_line_col <- "red"
      targ_line_lty <- 2
      targ_line_lwd <- 1

      legendtitle <- "Degraded samples"
      ylabel <- "Relative probability of degraded condition"
      maintitleSR <- paste0(TargetSiteID, ": Stressor-response (logistic regression) line of evidence")
      subtitleSR <-"Are stressor levels sufficient to explain the observed impairment?"
      subtitleSR <- stringr::str_wrap(subtitleSR, 100)

      captionSR <- paste0("All comparator samples (n=", n_cc_df_plot
                          , ").\n Score = ", paste(j_SR_score, collapse = ", ")
                          , ".")

      # Get base info for scores table
      df.score.j <- df.score.j %>%
        dplyr::mutate(BioComm = biocomm
                      , Label = jlabel
                      , Log1pValue = ifelse(useVal == "log1p"
                                            , log1p(ParamValue)
                                            , NA)
                      , n = nrow(df.plot)
                      , SRpred_Deg = j_SR_predict
                      , Sc_SRlog = j_SR_score) %>%
        dplyr::select(StationID_Master, StressSampID, RespSampID, all_of(colBio)
                      , Bio.Deg, ParamName, ParamValue, Log1pValue, n, SRpred_Deg
                      , Sc_SRlog, BioComm, Label)

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
        ggplot2::geom_vline(xintercept = x_intercept, color = targ_line_col
                            , lty = targ_line_lty, lwd = targ_line_lwd
                            , na.rm = TRUE) +
        ggplot2::geom_hline(yintercept = c(0.2, 0.5), color = "black"
                            , lty = 2, na.rm = TRUE) +
        ggplot2::labs(y = ylabel, x = jlabel) +
        ggplot2::geom_line(ggplot2::aes(y = y.name, x = x), data = newdat
                           , color = "blue", lwd = 1, na.rm = TRUE) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)
                       , plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(title = maintitleSR, subtitle = subtitleSR
                      , caption = captionSR)

      if ((boo_plot) == TRUE) {
        ggplot2::ggsave(filename = file.path(dir_path, fn_png_p1), plot = p1
                        , dpi = ppi, width = 8, height = 6, units = "in")
      }

    } ##IF.PLOT.END

    # Write to scores table
    if (exists("df.scores")) {
      df.scores <- rbind(df.scores, df.score.j)
    }

  } ##FOR.j.END

  # Save scores file (append to later)
  fn.scores <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm
                                          , "_SRLog_Scores.tab"))
  utils::write.table(df.scores, file = fn.scores, append = FALSE
                     , col.names = TRUE, row.names = FALSE, sep = "\t")

}##FUNCTION.END

