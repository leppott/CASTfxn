#  Copyright 2023 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Weight-of-Evidence summary
#'
#' @description Summarize weight of evidence using scores from other functions.
#'
#' @details Stressor-based weight of evidence for the stressor as a cause of impairment
#'          for the specified biological community. Combines information from
#'          the co-occurrence, stressor-response using data from or outside the
#'          case, verified predictions, and stressor-response from laboratory data.
#'
#' Uses the packages dplyr and tidyr.
#'
#' @param TargetSiteID Site ID
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param index Index name (IBI, CSCI, ASCI, etc.) Default = "IBI".
#' @param dir_results Directory to save plots.  Default = working directory and Results.
#' @param dfLoE data frame, LoE
#' @param dfQual data frame, Qual
#' @param dfStr data frame, stressors
#' @param dfRank Percent rank of each target sample stressor in the distribution
#'               of that stressor among all comparator samples
#' @param dfStressInfo data frame, StressInfo
#' @param df_coOccur CoOccur dataframe corresponding with stressors and specified biocomm
#' @param BioResp BioResp
# @param CO_sub Subdirectory containing co-occurrence results. Default = "CoOccurrence".
# @param SR_sub Subdirectory containing stressor-response results. Default = "StressorResponse".
# @param VP_sub Subdirectory containing verified prediction results.  Default = "VerifiedPredictions".
# @param SSD_sub Subdirectory containing SSD results. Default = "SSD".
#'
#' @return Four tab-delimited tables containing weight of evidence information:
#'         summary of the site results by stressor; more detailed review of
#'         each site sample by stressor and each line of evidence; weight of evidence
#'         for metrics evaluated for stressor-response lines of evidence;
#'         lookup table describing the short line of evidence code.
#'
#' Lines of evidence evaluated (if possible):
#' TS: Temporal sequence (not evaluated currently)
#' CO: Spatial/temporal co-occurrence (consistency) -- boxplots
#' CO: Spatial/temporal co-occurrence (sufficiendy) -- logistic regression
#' SR: Stressor-response relationships from inside the case -- linear regression
#' SR: Stressor-response relationships from outside the case -- linear regression
#' VP: Verified predictions -- boxplots
#' SSD: Stressor-response relationships from laboratory studies (not evaluated currently)
#'
#' @keywords internal
#'
#' @export
getWoE <- function(TargetSiteID
                   , outcaseLabel
                   , biocomm
                   , index
                   , BioResp
                   , dfQual
                   , dfStr
                   , dfRank
                   , dfStressInfo
                   , df_coOccur
                   , dfLoE
                   , dir_results = file.path(getwd(), "Results")
                   , dir_WoE = "WoE"
                   ) {##FUNCTION.START

  # QC data
  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    TargetSiteID = TargetSiteID
    outcaseLabel = outcaseLabel
    biocomm = bioComm
    index = bioIndex
    BioResp = bioMetricNames
    dfQual = list.BioQualSites$dfQuality
    dfStr = list_MatchBioData$site.b.str
    dfRank = list.stressors$site.stressor.pctrank
    dfStressInfo = siteStressInfo
    df_coOccur = data_bioCoOccur
    dfLoE = df_LoE
    dir_results = dir_results
    dir_WoE = "WoE"
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)
  LoEundef <- "SSD_ToxicityCurve"

  subdir = TargetSiteID
  ifelse(!dir.exists(file.path(dir_results, subdir, biocomm, dir_WoE)) == TRUE
         , dir.create(file.path(dir_results, subdir, biocomm, dir_WoE))
         , FALSE)
  dirWoE <- file.path(dir_results, subdir, biocomm, dir_WoE)

  LoEcols <- c("StationID_Master", "StressSampID", "RespSampID", "Response"
               , "ResponseValue", "Stressor", "StressorValue", "n", "nType"
               , "Score", "LoEtrim", "LoE", "Analysis", "InOut", "biocomm")

  # Filenames for all files containing scores
  fnTSScores <- paste0(TargetSiteID, "_", toupper(biocomm)
                       , "_TS_Scores.tab")
  fnCOScores <- paste0(TargetSiteID, "_", toupper(biocomm)
                       , "_CO_Scores.tab")
  fnSRLogScores <- paste0(TargetSiteID, "_", toupper(biocomm)
                       , "_SRLog_Scores.tab")
  fnSRScores <- paste0(TargetSiteID, "_", toupper(biocomm)
                       , "_SRLin_Scores.tab")
  fnVPScores <- paste0(TargetSiteID, "_", toupper(biocomm)
                       , "_VP_Scores.tab")
  fnSSDScores <- paste0(TargetSiteID, "_", toupper(biocomm)
                        , "_SSD_Scores.tab")

  # Base data = dfQual
  # TargetSiteID, StressSampID, RespSampID, Index Score, BioNarrative, and
  # Biodegraded (Y/N)

  # In a separate file in Site Info folder:
  # Num comparators, Num comparators not degraded, Num comparators degraded
  # Num comp. better than, better than not degraded, better than degraded

  # Remove existing dfEvidenceLong, if it exists (usually when debugging)
  if (exists("dfEvidenceLong")) { suppressWarnings(rm(dfEvidenceLong)) }
  if (exists("dfTemp")) { rm(dfTemp) }

  # prep dfStr for use in creating NE datasets for lines not evaluated
  # Convert tidyr::gather to tidyr::pivot longer ARL 2023-05-29
  numSamps <- length(unique(as.character(dfStr$StressSampID)))
  dfStr <- dfStr %>%
    tidyr::pivot_longer(cols = !c(StationID_Master, StressSampID, RespSampID)
                        , names_to = "Stressor", values_to = "StressorValue")

  dfSampDates <- df_coOccur %>%
    dplyr::filter(StationID_Master == TargetSiteID) %>%
    dplyr::select(StationID_Master, RespSampID, RespSampDate, StressSampID
                  , StressSampDate)

  # Get stressor names vector to combine with LoE names vector to ensure
  # scores will be calculated appropriately.
  stressornames <- data.frame("index" = 1
                              , "Stressor" = unique(as.character(dfStr$Stressor)))
  LoEnames <- data.frame("index" = 1
                         , "LoEtrim" = c("TS_barplot", "CO_boxplot"
                                         , "SS_InCase_LogRegr", "SR_InCase_LinRegr"
                                         , "SR_OutCase_LinRegr", "VP_boxplot_senstaxa"
                                         , "VP_boxplot_toltaxa", "SSD_ToxicityCurve"))
  LoEnames <- dplyr::filter(LoEnames, !(LoEtrim %in% LoEundef))
  df_allStrLoE <- merge(stressornames, LoEnames, by = "index", all = TRUE) %>%
    dplyr::select(!index) %>%
    dplyr::mutate(StationID_Master = TargetSiteID) %>%
    dplyr::mutate(LoE = case_when(LoEtrim == "TS_barplot" ~
                                    "Time sequence"
                                  , LoEtrim == "CO_boxplot" ~
                                    "Co-occurrence"
                                  , LoEtrim == "SS_InCase_LogRegr" ~
                                    "Stressor-sufficiency in the case"
                                  , LoEtrim == "SR_InCase_LinRegr" ~
                                    "Stressor-response gradient inside the case"
                                  , LoEtrim == "SR_OutCase_LinRegr" ~
                                    "Stressor-response gradient outside the case"
                                  , LoEtrim == "VP_boxplot_senstaxa" ~
                                    "Verified prediction using stressor-specific tolerance values"
                                  , LoEtrim == "VP_boxplot_toltaxa" ~
                                    "Verified prediction using stressor-specific tolerance values"
                                  , LoEtrim == "SSD_ToxicityCurve" ~
                                    "Taxon sensitivity distribution")
                  , nType = case_when(LoEtrim == "TS_barplot" ~
                                        "Target site samples only"
                                      , LoEtrim == "CO_boxplot" ~
                                        "Comparator samples with better biology"
                                      , LoEtrim == "SS_InCase_LogRegr" ~
                                        "All comparator samples"
                                      , LoEtrim == "SR_InCase_LinRegr" ~
                                        "All comparator samples"
                                      , LoEtrim == "SR_OutCase_LinRegr" ~
                                        "All samples outside the case"
                                      , LoEtrim == "VP_boxplot_senstaxa" ~
                                        "Comparator samples with better biology"
                                      , LoEtrim == "VP_boxplot_toltaxa" ~
                                        "Comparator samples with better biology"
                                      , LoEtrim == "SSD_ToxicityCurve" ~
                                        "Laboratory toxicity studies")
                  , Analysis = case_when(LoEtrim == "TS_barplot" ~
                                           "Temporal patterns"
                                         , LoEtrim == "CO_boxplot" ~
                                           "Box plot"
                                         , LoEtrim == "SS_InCase_LogRegr" ~
                                           "Logistic regression"
                                         , LoEtrim == "SR_InCase_LinRegr" ~
                                           "Linear regression"
                                         , LoEtrim == "SR_OutCase_LinRegr" ~
                                           "Linear regression"
                                         , LoEtrim == "VP_boxplot_senstaxa" ~
                                           "Box plot"
                                         , LoEtrim == "VP_boxplot_toltaxa" ~
                                           "Box plot"
                                         , LoEtrim == "SSD_ToxicityCurve" ~
                                           "Probability of extirpation")
                  , InOut = case_when(LoEtrim == "TS_barplot" ~
                                        "Inside the case"
                                      , LoEtrim == "CO_boxplot" ~
                                        "Inside the case"
                                      , LoEtrim == "SS_InCase_LogRegr" ~
                                        "Inside the case"
                                      , LoEtrim == "SR_InCase_LinRegr" ~
                                        "Inside the case"
                                      , LoEtrim == "SR_OutCase_LinRegr" ~
                                        "Outside the case"
                                      , LoEtrim == "VP_boxplot_senstaxa" ~
                                        "Inside the case"
                                      , LoEtrim == "VP_boxplot_toltaxa" ~
                                        "Inside the case"
                                      , LoEtrim == "SSD_ToxicityCurve" ~
                                        "Outside the case"))

  # Iterate over dfLoE to obtain all the evidence for each available line
  # For each LoE ####
  for (l in 1:nrow(dfLoE)) {

    chrLoE <- dfLoE$LoE[l]
    booUse <- dfLoE$Completed[l]
    dirLoE <- dfLoE$ResultsDir[l]

    scored <- ifelse(booUse == 1, " which was evaluated."
                     , " which was not evaluated.")
    msg <- paste0("Processing ", chrLoE, scored)
    message(msg)
    # print(msg)
    # flush.console()

    # Create dummy dataframe
    dfTemp <- dfStr
    dfTemp <- merge(dfStr, dfQual[, c("RespSampID", index)]
                    , by.x = "RespSampID", by.y = "RespSampID")
    dfTemp <- dplyr::mutate(dfTemp, Response = index)
    dfTemp <- dplyr::rename(dfTemp, ResponseValue = all_of(index))
    dfTemp <- dfTemp[!is.na(dfTemp$StressorValue), ]

    if (booUse == 0) { # LoE was not evaluated. Need to enter NE scores for given LoE

      # No TS evaluated ####
      if (chrLoE == "TS") {
        dfTemp <- dfTemp %>%
          dplyr::mutate(n = numSamps
                        , nType = "Target site samples only"
                        , Score = NA
                        # , Score = "NE"
                        , LoEtrim = "TS_barplot"
                        , LoE = "Time sequence"
                        , Analysis = "Temporal patterns"
                        , InOut = "Inside the case"
                        , biocomm = biocomm) %>%
          dplyr::select(StationID_Master, StressSampID, RespSampID
                        , Response, ResponseValue, Stressor
                        , StressorValue, n, nType, Score, LoEtrim
                        , LoE, Analysis, InOut, biocomm)
        message("Line 255")
      }
      # No CO evaluated ####
      if (chrLoE == "CO") {
        dfTemp <- dfTemp %>%
          dplyr::mutate(n = NA
                        , nType = "Comparator samples with better biology"
                        , Score = NA
                        # , Score = "NE"
                        , LoEtrim = "CO_boxplot"
                        , LoE = "Co-occurrence"
                        , Analysis = "Boxplot"
                        , InOut = "Inside the case"
                        , biocomm = biocomm) %>%
          dplyr::select(StationID_Master, StressSampID, RespSampID
                        , Response, ResponseValue, Stressor
                        , StressorValue, n, nType, Score, LoEtrim
                        , LoE, Analysis, InOut, biocomm)
        message("Line 273")
      }
      # No SRLog evaluated ####
      if (chrLoE == "SRLog") {
        dfTemp <- dfTemp %>%
          dplyr::mutate(n = NA
                        , nType = "All comparator samples"
                        , Score = NA
                        # , Score = "NE"
                        , LoEtrim = "SR_InCase_LogRegr"
                        , LoE = "Stressor-sufficiency in the case"
                        , Analysis = "Logistic regression"
                        , InOut = "Inside the case"
                        , biocomm = biocomm) %>%
          dplyr::select(StationID_Master, StressSampID, RespSampID
                        , Response, ResponseValue, Stressor
                        , StressorValue, n, nType, Score, LoEtrim
                        , LoE, Analysis, InOut, biocomm)
        message("Line 291")
      }
      # No SRLin evaluated ####
      if (chrLoE == "SRLin") {
        dfTemp1 <- dfTemp %>%
          dplyr::mutate(n = NA
                        , nType = "All comparator samples"
                        , Score = NA
                        # , Score = "NE"
                        , LoEtrim = "SR_InCase_LinRegr"
                        , LoE = "Stressor-response gradient in the case"
                        , Analysis = "Linear regression"
                        , InOut = "Inside the case"
                        , biocomm = biocomm) %>%
          dplyr::select(StationID_Master, StressSampID, RespSampID
                        , Response, ResponseValue, Stressor
                        , StressorValue, n, nType, Score, LoEtrim
                        , LoE, Analysis, InOut, biocomm)
        dfTemp2 <- dfTemp %>%
          dplyr::mutate(n = numSamps
                        , nType = "All samples from outside the case"
                        , Score = NA
                        # , Score = "NE"
                        , LoEtrim = "SR_OutCase_LinRegr"
                        , LoE = "Stressor-response gradient outside the case"
                        , Analysis = "Linear regression"
                        , InOut = "Outside the case"
                        , biocomm = biocomm) %>%
          dplyr::select(StationID_Master, StressSampID, RespSampID
                        , Response, ResponseValue, Stressor
                        , StressorValue, n, nType, Score, LoEtrim
                        , LoE, Analysis, InOut, biocomm)

        dfTemp <- rbind(dfTemp1, dfTemp2)
        message("Line 325")
      }
      # No VP evaluated ####
      if (chrLoE == "VP") {
        dfTemp1 <- dfTemp %>%
          dplyr::mutate(ResponseValue = NA
                        , n = NA
                        , nType = "Comparator samples with better biology"
                        , Score = NA
                        # , Score = "NE"
                        , LoEtrim = "VP_boxplot_senstaxa"
                        , LoE = paste0("Verified prediction using "
                                       , "stressor-specific tolerance values")
                        , Analysis = "Box plot"
                        , InOut = "Inside the case"
                        , biocomm = biocomm) %>%
          dplyr::select(StationID_Master, StressSampID, RespSampID
                        , Response, ResponseValue, Stressor
                        , StressorValue, n, nType, Score, LoEtrim
                        , LoE, Analysis, InOut, biocomm)
        dfTemp2 <- dfTemp %>%
          dplyr::mutate(ResponseValue = NA
                        , n = NA
                        , nType = "Comparator samples with better biology"
                        , Score = NA
                        # , Score = "NE"
                        , LoEtrim = "VP_boxplot_toltaxa"
                        , LoE = paste0("Verified prediction using "
                                       , "stressor-specific tolerance values")
                        , Analysis = "Box plot"
                        , InOut = "Inside the case"
                        , biocomm = biocomm) %>%
          dplyr::select(StationID_Master, StressSampID, RespSampID
                        , Response, ResponseValue, Stressor
                        , StressorValue, n, nType, Score, LoEtrim
                        , LoE, Analysis, InOut, biocomm)

        dfTemp <- rbind(dfTemp1, dfTemp2)
        message("Line 363")
      }
      # No SSD evaluated ####
      if (chrLoE == "SSD") {
        dfTemp <- dfTemp %>%
          dplyr::mutate(Response = "Number expected taxa not observed"
                        , ResponseValue = NA
                        , n = NA
                        , nType = "Laboratory toxicity studies"
                        , Score = NA
                        # , Score = "NE"
                        , LoEtrim = "SSD_ToxicityCurve"
                        , LoE = "Taxon sensitivity distribution"
                        , Analysis = "Probability of extirpation"
                        , InOut = "Outside the case"
                        , biocomm = biocomm) %>%
          dplyr::select(StationID_Master, StressSampID, RespSampID
                        , Response, ResponseValue, Stressor
                        , StressorValue, n, nType, Score, LoEtrim
                        , LoE, Analysis, InOut, biocomm)
        message("Line 383")
      }

      gapcomment <- "Line of evidence not evaluated."
      gaps <- cbind.data.frame("getWoE", chrLoE, 0
                               , gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
    } else { # booUse==1

      # Get Time Sequence data ####
      if (chrLoE == "TS") {
        if (file.exists(file.path(dirLoE, fnTSScores))) {
          # Pull data into temp data structure
          # Currently not scored, so file does not exist--ever
          next

        } else {
          # No scores available
          # dfTemp <- dfStr
          # dfTemp <- merge(dfStr, dfQual[,c("RespSampID",index)]
          #                 , by.x = "RespSampID", by.y = "RespSampID")
          # dfTemp <- dfTemp[!is.na(dfTemp$StressorValue),]
          # dfTemp <- dplyr::rename(dfTemp, ResponseValue = all_of(index))

          dfTemp <- dfTemp %>%
            dplyr::mutate(n = numSamps
                          , nType = "Target site samples only"
                          # , Score = NA
                          , Score = "NE"
                          , LoEtrim = "TS_barplot"
                          , LoE = "Time sequence"
                          , Analysis = "Temporal patterns"
                          , InOut = "Inside the case"
                          , biocomm = biocomm) %>%
            dplyr::select(StationID_Master, StressSampID, RespSampID
                          , Response, ResponseValue, Stressor
                          , StressorValue, n, nType, Score, LoEtrim
                          , LoE, Analysis, InOut, biocomm)

          gapcomment <- "Time sequence line of evidence is not scored."
          gaps <- cbind.data.frame("getWoE", chrLoE, 0
                                   , gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          message("Line 434")
        }
      } # End TS LoE

      # Get CoOccurrence data ####
      if (chrLoE == "CO") {
        if (file.exists(file.path(dirLoE, fnCOScores))) {
          dfCO <- read.table(file.path(dirLoE, fnCOScores)
                             , header = TRUE, sep = "\t"
                             , stringsAsFactors = FALSE)
          colnames(dfCO) <- c("StationID_Master", outcaseLabel, "StressSampID"
                              , "RespSampID", index, "BioNarrative"
                              , "BioDegYN", "Stressor", "StressorValue"
                              , "n", "q25", "q50", "q75", "Sc_Boxplot"
                              , "biocomm", "Label")
          dfCO <- dplyr::select(dfCO, -Label)
          dfCO <- dfCO[!is.na(dfCO$StressorValue),]
          dfCO <- unique(dfCO)

          # Pull out co-occurrence scores from co-occurrence file
          dfCO <- dfCO %>%
            dplyr::mutate(Response = index
                          , nType = "Comparator samples with better biology"
                          , LoEtrim = "CO_boxplot"
                          , LoE = "Co-occurrence"
                          , Analysis = "Box plot"
                          , InOut = "Inside the case") %>%
            dplyr::rename(ResponseValue = all_of(index)
                          , Score = Sc_Boxplot)

          dfCO <- dfCO[!is.na(dfCO$Score), ]
          dfCO <- dfCO[, LoEcols]

          dfTemp <- dfCO
          rm(dfCO)
          message("Line 470")

        } else {
          # No scores available
          dfTemp <- dfTemp %>%
            dplyr::mutate(n = NA
                          , nType = "Comparator samples with better biology"
                          # , Score = NA
                          , Score = "NE"
                          , LoEtrim = "CO_boxplot"
                          , LoE = "Co-occurrence"
                          , Analysis = "Boxplot"
                          , InOut = "Inside the case"
                          , biocomm = biocomm) %>%
            dplyr::select(StationID_Master, StressSampID, RespSampID
                          , Response, ResponseValue, Stressor
                          , StressorValue, n, nType, Score, LoEtrim
                          , LoE, Analysis, InOut, biocomm)

          message("Line 489")
        }
      } # End CO LoE (plus SR logistic regressions)

      # Get Sufficiency data ####
      if (chrLoE == "SRLog") {
        if (file.exists(file.path(dirLoE, fnSRLogScores))) {
          dfSRLog <- read.table(file.path(dirLoE, fnSRLogScores)
                             , header = TRUE, sep = "\t"
                             , stringsAsFactors = FALSE)
          colnames(dfSRLog) <- c("StationID_Master", "StressSampID", "RespSampID"
                                 , index, "BioDegYN", "Stressor", "StressorValue"
                                 , "Log1pValue", "n", "SR_pred_Deg", "SC_SRLog"
                                 , "biocomm", "Label")
          dfSRLog <- dplyr::select(dfSRLog, -Label)
          dfSRLog <- dfSRLog[!is.na(dfSRLog$StressorValue), ]
          dfSRLog <- unique(dfSRLog)

          # Pull out the SR logistic regression scores from co-occurrence file
          dfSRLog <- dfSRLog %>%
            dplyr::mutate(Response = index
                          , nType = "All comparator samples"
                          , LoEtrim = "SR_InCase_LogRegr"
                          , LoE = "Stressor-sufficiency in the case"
                          , Analysis = "Logistic regression"
                          , InOut = "Inside the case") %>%
            dplyr::rename(ResponseValue = all_of(index)
                          , Score = SC_SRLog)

          dfSRLog <- dfSRLog[!is.na(dfSRLog$Score), ]
          dfSRLog <- dfSRLog[, LoEcols]

          dfTemp <- dfSRLog
          rm(dfSRLog)
          message("Line 524")

        } else {
          # No scores available
          dfTemp <- dfTemp %>%
            dplyr::mutate(n = NA
                          , nType = "All comparator samples"
                          # , Score = NA
                          , Score = "NE"
                          , LoEtrim = "SR_InCase_LogRegr"
                          , LoE = "Stressor-sufficiency in the case"
                          , Analysis = "Logistic regression"
                          , InOut = "Inside the case"
                          , biocomm = biocomm) %>%
            dplyr::select(StationID_Master, StressSampID, RespSampID
                          , Response, ResponseValue, Stressor
                          , StressorValue, n, nType, Score, LoEtrim
                          , LoE, Analysis, InOut, biocomm)
          message("Line 542")
        }
      } # End CO LoE (plus SR logistic regressions)

      # Get Stressor-Response data ####
      if (chrLoE == "SRLin") {
        if (file.exists(file.path(dirLoE,fnSRScores))) {
          dfSR <- read.table(file.path(dirLoE, fnSRScores)
                             , header = TRUE, sep = "\t"
                             , stringsAsFactors = FALSE
                             , comment.char = "")

          colnames(dfSR) <- c("StationID_Master", "Stressor", "Response"
                              , "StressSampID", "RespSampID", "Quality"
                              , "StressorValue", "ResponseValue", "biocomm"
                              , "stressLabel", "respLabel", "n_site", "n_comp"
                              , "SRlin_ScoreComp", "n_out", "SRlin_ScoreOut")
          dfSR <- dfSR[!is.na(dfSR$StressorValue),]
          dfSR <- unique(dfSR)
          dfSR <- dplyr::select(dfSR, -stressLabel, -respLabel)

          # SR linear regression inside the case
          dfSRlin_inside <- dfSR %>%
            dplyr::mutate(nType = "All comparator samples"
                           , LoEtrim = "SR_InCase_LinRegr"
                           , LoE = "Stressor-response gradient inside the case"
                           , Analysis = "Linear regression"
                           , InOut = "Inside the case") %>%
            dplyr::rename(n = n_comp, Score = SRlin_ScoreComp)
          dfSRlin_inside <- dfSRlin_inside[!is.na(dfSRlin_inside$Score),]
          dfSRlin_inside <- dfSRlin_inside[,LoEcols]

          # SR linear regression outside the case
          dfSRlin_outside <- dfSR %>%
            dplyr::mutate(nType = "All samples outside the case"
                           , LoEtrim = "SR_OutCase_LinRegr"
                           , LoE = "Stressor-response gradient outside the case"
                           , Analysis = "Linear regression"
                           , InOut = "Outside the case") %>%
            dplyr::rename(n = n_out, Score = SRlin_ScoreOut)
          dfSRlin_outside <- dfSRlin_outside[!is.na(dfSRlin_outside$Score),]
          dfSRlin_outside <- dfSRlin_outside[,LoEcols]

          dfTemp <- rbind(dfSRlin_inside, dfSRlin_outside)
          rm(dfSR, dfSRlin_inside, dfSRlin_outside)
          message("Line 588")

          # Metrics
          dfMetrics <- dfTemp %>%
            dplyr::filter(!(Response %in% index))
          metricsDir <- dfLoE$ResultsDir[l]

          # Index
          dfTemp <- dfTemp %>%
            dplyr::filter(Response %in% index)
          message("Line 598")

        } else {
          # No scores available
          dfTemp1 <- dfTemp %>%
            dplyr::mutate(n = NA
                          , nType = "All comparator samples"
                          # , Score = NA
                          , Score = "NE"
                          , LoEtrim = "SR_InCase_LinRegr"
                          , LoE = "Stressor-response gradient inside the case"
                          , Analysis = "Linear regression"
                          , InOut = "Inside the case"
                          , biocomm = biocomm) %>%
            dplyr::select(StationID_Master, StressSampID, RespSampID
                          , Response, ResponseValue, Stressor
                          , StressorValue, n, nType, Score, LoEtrim
                          , LoE, Analysis, InOut, biocomm)
          dfTemp2 <- dfTemp %>%
            dplyr::mutate(n = numSamps
                          , nType = "All samples outside the case"
                          # , Score = NA
                          , Score = "NE"
                          , LoEtrim = "SR_OutCase_LinRegr"
                          , LoE = "Stressor-response gradient outside the case"
                          , Analysis = "Linear regression"
                          , InOut = "Outside the case"
                          , biocomm = biocomm) %>%
            dplyr::select(StationID_Master, StressSampID, RespSampID
                          , Response, ResponseValue, Stressor
                          , StressorValue, n, nType, Score, LoEtrim
                          , LoE, Analysis, InOut, biocomm)

          dfTemp <- rbind(dfTemp1, dfTemp2)
          message("Line 632")
        }
      } # End SR LoE (linear regressions)

      # Get Verified Prediction data ####
      if (chrLoE == "VP") {
        if (file.exists(file.path(dirLoE,fnVPScores))) {
          dfVP <- read.table(file.path(dirLoE, fnVPScores)
                             , header = TRUE, sep = "\t"
                             , stringsAsFactors = FALSE)
          colnames(dfVP) <- c("StationID_Master", "RespSampID"
                              , "IndexValue", "Quality", "StressSampID"
                              , "Label", "Stressor", "StressorValue"
                              , "Response", "ResponseValue", "qLoValue_Cutoff"
                              , "qHiValue_Cutoff", "Score", "biocomm"
                              , "n", "n_BetterNotDegraded")
          dfVP <- dfVP[!is.na(dfVP$StressorValue), ]
          dfVP <- unique(dfVP)
          dfVP <- dplyr::select(dfVP, -Label)

          # Pull out co-occurrence scores from co-occurrence file
          dfVP <- dfVP %>%
            dplyr::mutate(nType = "Comparator samples with better biology"
                          , LoEtrim = ifelse(Response=="Sensitive Taxa"
                                             , "VP_boxplot_senstaxa"
                                             , "VP_boxplot_toltaxa")
                          , LoE = paste0("Verified prediction using "
                                         , "stressor-specific tolerance values")
                          , Analysis = "Box plot"
                          , InOut = "Inside the case")

          dfVP <- dfVP[!is.na(dfVP$Score), ]
          dfVP <- dfVP[, LoEcols]

          dfTemp <- dfVP
          rm(dfVP)
          message("Line 668")

        } else {
          # No scores available
          # Create dummy dataframe with NE for all scores
          dfTemp1 <- dfTemp %>%
            dplyr::mutate(ResponseValue = NA
                          , n = NA
                          , nType = "Comparator samples with better biology"
                          # , Score = NA
                          , Score = "NE"
                          , LoEtrim = "VP_boxplot_senstaxa"
                          , LoE = paste0("Verified prediction using "
                                         , "stressor-specific tolerance values")
                          , Analysis = "Box plot"
                          , InOut = "Inside the case"
                          , biocomm = biocomm) %>%
            dplyr::select(StationID_Master, StressSampID, RespSampID
                          , Response, ResponseValue, Stressor
                          , StressorValue, n, nType, Score, LoEtrim
                          , LoE, Analysis, InOut, biocomm)
          dfTemp2 <- dfTemp %>%
            dplyr::mutate(ResponseValue = NA
                          , n = NA
                          , nType = "Comparator samples with better biology"
                          # , Score = NA
                          , Score = "NE"
                          , LoEtrim = "VP_boxplot_toltaxa"
                          , LoE = paste0("Verified prediction using "
                                         , "stressor-specific tolerance values")
                          , Analysis = "Box plot"
                          , InOut = "Inside the case"
                          , biocomm = biocomm) %>%
            dplyr::select(StationID_Master, StressSampID, RespSampID
                          , Response, ResponseValue, Stressor
                          , StressorValue, n, nType, Score, LoEtrim
                          , LoE, Analysis, InOut, biocomm)

          dfTemp <- rbind(dfTemp1, dfTemp2)
          message("Line 707")
        }
      } # End VP LoE

      # Get Species Sensitivity Distribution data ####
      if (chrLoE == "SSD") {
        if (file.exists(file.path(dirLoE,fnSSDScores))) {
          # Not yet implemented
        } else {
          # No scores available
        }
      } # End SSD LoE
    } # End booUse == 1

    # Combine the different lines of evidence dataframes into one
    if (!exists("dfEvidenceLong")) {
      if (exists("dfTemp")) {
        dfEvidenceLong <- dfTemp
        rm(dfTemp)
      }
    } else {
      if (exists("dfTemp")) {
        dfEvidenceLong <- rbind(dfEvidenceLong, dfTemp)
        rm(dfTemp)
      }
    } # End creating dfEvidenceLong

  } # End iteration over all LoE

  # Grab colnames of all possible LoEs here
  # Add in all LoE possible for all stressors.
  # Typically, VP will not be evaluate for all stressors and will need to be added.
  if (any(is.na(dfEvidenceLong$ResponseValue))) {
    colOrder <- colnames(dfEvidenceLong)
    dfSampResp <- dfEvidenceLong %>%
      dplyr::distinct(RespSampID, Response, ResponseValue) %>%
      dplyr::filter(!is.na(ResponseValue))
    dfEvidenceLong <- merge(dfEvidenceLong, dfSampResp
                             , by = c("RespSampID", "Response")
                             , all = TRUE)
    dfEvidenceLong <- dfEvidenceLong %>%
      dplyr::mutate(ResponseValue = ifelse(!is.na(ResponseValue.x)
                                            , ResponseValue.x
                                            , ResponseValue.y)) %>%
      dplyr::select(all_of(colOrder))
  }

  dfEvidenceLong <- dplyr::filter(dfEvidenceLong, !(LoEtrim %in% LoEundef))
  LoEcolnames <- unique(dfEvidenceLong$LoEtrim)

  coreData <- dfEvidenceLong %>%
    dplyr::distinct(StationID_Master, StressSampID, RespSampID, Response
                    , Stressor, StressorValue, biocomm)

  dfEvidCurrent <- dfEvidenceLong %>%
    dplyr::distinct(Stressor, LoEtrim, StationID_Master, LoE, nType, Analysis, InOut)

  dfEvidNE <- setdiff(df_allStrLoE, dfEvidCurrent)
  if (nrow(dfEvidNE) > 0) {
    dfEvidNE <- merge(coreData, dfEvidNE, by = c("StationID_Master", "Stressor"))
    cur_cols <- colnames(dfEvidenceLong)
    new_cols <- colnames(dfEvidNE)
    add_cols <- setdiff(cur_cols, new_cols)
    if (length(add_cols) > 0) {
      for (nm in add_cols) {
        dfEvidNE[[nm]] <- NA
      }
    }
    dfEvidNE <- dplyr::select(dfEvidNE, all_of(cur_cols))

    dfEvidenceLong <- rbind(dfEvidenceLong, dfEvidNE) %>%
      dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>%
      dplyr::arrange(Stressor, LoEtrim)
  } else {
    dfEvidenceLong <- dfEvidenceLong %>%
      dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>%
      dplyr::arrange(Stressor, LoEtrim)
  }

  # Merge stressor group name and percent rank into siteStressInfo
  # Convert gather to pivot longer (ARL 2023-05-29)
  dfRank <- dplyr::select(dfRank, -StressSampDate) %>%
    tidyr::pivot_longer(cols = !c(StationID_Master, StressSampID)
                        , names_to = "Stressor"
                        , values_to = "StressorPctRank"
                        , values_drop_na = TRUE)

  dfStressInfo <- dfStressInfo %>%
    select(StdParamName, GroupName, Label) %>%
    rename(Stressor = StdParamName, StressorType = GroupName)

  dfStrGpRank <- unique(merge(dfRank
                              , dfStressInfo[, c("Stressor", "StressorType")]
                              , by.x = "Stressor"
                              , by.y = "Stressor"))

  dfStrGpRankQual <- unique(merge(dfStrGpRank, dfQual
                                  , by.x = c("StationID_Master", "StressSampID")
                                  , by.y = c("StationID_Master", "StressSampID")))

  # Need to separately grab modeled flow data and merge with resp samps (all for station) & rbind
  # dfStrGpRankQual only merged measured data
  dfStrGpRankModl <- dfStrGpRank[grepl("_modeledflow", dfStrGpRank$StressSampID),]
  dfQualModl <- dfQual %>%
    dplyr::select(StationID_Master, OutcaseCol, RespSampID, all_of(index)
                          , BioDeg, BioNarrative, ComparatorYN, BetterThan
                          , OutsideCaseYN)
  dfStrGpRankQualModl <- unique(merge(dfStrGpRankModl, dfQualModl))
  dfStrGpRankQualModl <- dplyr::select(dfStrGpRankQualModl, StationID_Master
                                       , -StressSampID, Stressor, StressorPctRank
                                       , StressorType, OutcaseCol, RespSampID
                                       , all_of(index), BioDeg, BioNarrative
                                       , ComparatorYN, BetterThan, OutsideCaseYN)
  dfStrGpRankQualModl <- merge(dfStrGpRankQualModl, dfSampDates
                               , by.x = c("StationID_Master", "RespSampID")
                               , by.y = c("StationID_Master", "RespSampID"))
  dfStrGpRankQualModl <- dplyr::select(dfStrGpRankQualModl, StationID_Master
                                       , StressSampID, Stressor, StressorPctRank
                                       , StressorType, OutcaseCol, RespSampID
                                       , all_of(index), BioDeg, BioNarrative
                                       , ComparatorYN, BetterThan, OutsideCaseYN)
  dfStrGpRankQual <- rbind(dfStrGpRankQual, dfStrGpRankQualModl)

  if (boo_DEBUG==FALSE) { rm(dfRank, dfStrGpRank) }

  # Merge in Quality info and Stressor Types
  dfEvidenceLong <- merge(dfStrGpRankQual, dfEvidenceLong
                          , by.x = c("StationID_Master", "StressSampID"
                                     , "Stressor", "RespSampID")
                          , by.y = c("StationID_Master", "StressSampID"
                                     , "Stressor", "RespSampID"))

  # Add additional information to the Long form (e.g., Label)
  dfEvidenceLong <- merge(dfEvidenceLong, dfStressInfo[, c("Stressor","Label")]
                          , all.x = TRUE)
  colnames(dfEvidenceLong)[which(names(dfEvidenceLong) == "OutcaseCol")] <- outcaseColName
  dfEvidenceLong <- dfEvidenceLong %>%
    dplyr::rename(BioComm = biocomm, Inside_Outside = InOut) %>%
    dplyr::select(StationID_Master, all_of(outcaseColName), BioComm, RespSampID
                  , all_of(index), BioDeg, BioNarrative, Response, ResponseValue
                  , StressSampID, StressorType, Label, Stressor, StressorValue
                  , StressorPctRank, Score, n, nType, LoEtrim, LoE, Analysis
                  , Inside_Outside)
  dfEvidenceLong <- dfEvidenceLong %>%
    dplyr::mutate(Score = ifelse(is.na(Score), "NE", Score)
                  , Finding = case_when(Score == "NE" ~ "Not evaluated"
                                        , Score == "1" ~ "Supports"
                                        , Score == "-1" ~ "Refutes"
                                        , Score == 0 ~ "Indeterminate"
                                        , TRUE ~ "Unknown")) %>%
    dplyr::arrange(StressorType, Label, StressSampID, LoE)

  dfEvidDetail <- dfEvidenceLong %>%
    dplyr::mutate(Score = ifelse(is.na(Score), "NE", Score)
                  , n = ifelse(is.na(n), 0, n))

  # Write the detailed data file
  fnEvidLong <- paste0(TargetSiteID,"_", biocomm, "_WoE_DetailedLoEs.tab")
  write.table(dfEvidDetail, file.path(dirWoE, fnEvidLong), append = FALSE
              , col.names = TRUE, row.names = FALSE, sep = "\t")
  rm(dfEvidDetail)

  if (df_LoE$Completed[df_LoE$LoE == "SRLin"] == 1) {
    # Merge in Quality info and Stressor Types for Metrics
    dfMetricsLong <- merge(dfStrGpRankQual, dfMetrics
                           , by.x = c("StationID_Master", "StressSampID"
                                      , "Stressor", "RespSampID")
                           , by.xy = c("StationID_Master", "StressSampID"
                                       , "Stressor", "RespSampID"))

    # Add additional information to the Long form for Metrics
    dfMetricsLong <- merge(dfMetricsLong, dfStressInfo[,c("Stressor","Label")]
                           , all.x = TRUE)
    colnames(dfMetricsLong)[which(names(dfMetricsLong) == "OutcaseCol")] <- outcaseColName
    dfMetricsLong <- dfMetricsLong %>%
      dplyr::rename(BioComm = biocomm, Inside_Outside = InOut) %>%
      dplyr::select(StationID_Master, all_of(outcaseColName), BioComm, RespSampID
                    , all_of(index), BioDeg, BioNarrative, Response, ResponseValue
                    , StressSampID, StressorType, Label, Stressor, StressorValue
                    , StressorPctRank, Score, n, nType, LoEtrim, LoE, Analysis
                    , Inside_Outside) %>%
      dplyr::mutate(Finding = case_when(Score == "NE" ~ "Not evaluated"
                                        , Score == "1" ~ "Supports"
                                        , Score == "-1" ~ "Refutes"
                                        , Score == 0 ~ "Indeterminate"
                                        , TRUE ~ "Unknown")) %>%
      dplyr::arrange(StressorType, Label, StressSampID, LoE) %>%
      dplyr::mutate(Score = ifelse(is.na(Score), "NE", Score))

    # Write the detailed data file
    fnEvidLongMetrics <- paste0(TargetSiteID,"_", biocomm, "_WoE_DetailedMetricsLoEs.tab")
    write.table(dfMetricsLong, file.path(dirWoE,fnEvidLongMetrics), append = FALSE
                , col.names = TRUE, row.names = FALSE, sep = "\t")
  }

  # Pivot scores to wide format
  # Convert tidyr::spread to tidyr::pivot_wider ARL 2023-05-29
  dfEvidenceWide <- dfEvidenceLong %>%
    dplyr::distinct(StressSampID, Label, Stressor, StressorValue, Score, LoEtrim)

  dfEvidenceWide <- dfEvidenceWide %>%
    tidyr::pivot_wider(names_from = "LoEtrim", values_from = Score)
    # dplyr::mutate(Score = as.numeric(Score)) %>%
    # dplyr::group_by(StressSampID, Label, Stressor, StressorValue, LoEtrim) %>%
    # dplyr::summarize(TotScore = sum(Score), .groups = "drop_last") #%>%
    # dplyr::rename(Score = TotScore) %>%
    # tidyr::spread(key = "LoEtrim", value = sum(Score, na.rm=TRUE), fill=NA)
  dfEvidenceWide <- as.data.frame(dfEvidenceWide)
  endcol <- ncol(dfEvidenceWide)
  dfEvidenceWide[, 4:endcol][is.na(dfEvidenceWide[, 4:endcol])] <- "NE"

  # Provide text interpretation of score
  dfEvidenceCounts <- dfEvidenceLong %>%
    dplyr::select(RespSampID, StressSampID, Label, Stressor, StressorValue
                  , Inside_Outside, Finding, Score) %>%
    dplyr::group_by(RespSampID, StressSampID, Label, Stressor, StressorValue
                    , Inside_Outside, Finding, Score) %>%
    dplyr::summarise(NumLoE = n(), .groups="drop_last") %>%
    dplyr::mutate(Heading = case_when(Inside_Outside == "Inside the case" &
                                        Finding == "Supports" ~ "NumInSupport"
                                      , Inside_Outside == "Inside the case" &
                                        Finding == "Refutes" ~ "NumInRefute"
                                      , Inside_Outside == "Inside the case" &
                                        Finding == "Indeterminate" ~ "NumInIndet"
                                      , Inside_Outside == "Inside the case" &
                                        Finding == "Not evaluated" ~ "NumInNotEval"
                                      , Inside_Outside == "Outside the case" &
                                        Finding == "Supports" ~ "NumOutSupport"
                                      , Inside_Outside == "Outside the case" &
                                        Finding == "Refutes" ~ "NumOutRefute"
                                      , Inside_Outside == "Outside the case" &
                                        Finding == "Indeterminate" ~ "NumOutIndet"
                                      , Inside_Outside == "Outside the case" &
                                        Finding == "Not evaluated" ~ "NumOutNotEval"))

  # Need to convert counts to wide format to get Num not eval, and totals
  # Convert tidyr::spread to tidyr::pivot_wider ARL 2023-05-29
  dfEvidenceCounts <- as.data.frame(dfEvidenceCounts)
  dfEvidCountsWide <- dfEvidenceCounts %>%
    dplyr::select(-Inside_Outside, -Finding, -Score) %>%
    dplyr::group_by(StressSampID, Label, Stressor, StressorValue) %>%
    tidyr::pivot_wider(names_from = "Heading", values_from = "NumLoE", values_fill = 0)

  # Account for missing columns
  colNamesLoEcounts <- c("NumInSupport", "NumInRefute", "NumInIndet", "NumInNotEval"
                         , "NumOutSupport", "NumOutRefute", "NumOutIndet", "NumOutNotEval")
  colNamesInEvidCountsWide <- colnames(dfEvidCountsWide)
  colNamesInEvidCounts <- colNamesInEvidCountsWide[grepl("^Num.*$",colNamesInEvidCountsWide)]
  colNamesNeeded <- setdiff(colNamesLoEcounts,colNamesInEvidCounts)
  colNamesKeep <- setdiff(colNamesInEvidCountsWide,colNamesLoEcounts)

  if (length(colNamesNeeded) > 0) {
    for (nm in colNamesNeeded) {
      dfEvidCountsWide[[nm]] <- 0
    }
  }

  dfEvidCountsWide <- dfEvidCountsWide[, c(colNamesKeep, colNamesLoEcounts)]

  # Get totals
  dfEvidCountsWide <- dfEvidCountsWide %>%
    dplyr::mutate(TotSupport = NumInSupport + NumOutSupport
                  , TotRefute = NumInRefute + NumOutRefute
                  , TotIndet = NumInIndet + NumOutIndet
                  , TotNotEval = NumInNotEval + NumOutNotEval
                  , WoE = case_when(TotIndet > (TotSupport + TotRefute) ~ "Indeterminate"
                                    , TotSupport > TotRefute ~ "Supports"
                                    , TotRefute > TotSupport ~ "Refutes"
                                    , TRUE ~ "Indeterminate"))

  # Merge individual LoE with summary LoE
  dfEvidCountsWide <- merge(dfEvidenceWide, dfEvidCountsWide
                            , by.x = c("StressSampID", "Label", "Stressor"
                                       , "StressorValue")
                            , by.y = c("StressSampID", "Label", "Stressor"
                                       , "StressorValue"))

  # Merge basic data back in to summary
  dfEvidBasic <- unique(dfEvidenceLong[, c("StationID_Master", "Cluster"
                                           , "BioComm", "RespSampID", index
                                           , "BioDeg", "BioNarrative", "StressSampID"
                                           , "StressorType", "Label", "Stressor"
                                           , "StressorValue", "StressorPctRank")])

  dfEvidCountsWide <- merge(dfEvidBasic, dfEvidCountsWide
                            , by.x = c("StressSampID", "Label", "Stressor"
                                       , "StressorValue", "RespSampID")
                            , by.y = c("StressSampID", "Label", "Stressor"
                                       , "StressorValue", "RespSampID"))
  # Get Sample Dates
  dfEvidCountsWide <- merge(dfEvidCountsWide, dfSampDates)

  # Order the columns sensibly
  dfEvidCountsWide <- dfEvidCountsWide[, c("StationID_Master", "Cluster", "BioComm"
                                           , "RespSampID", "RespSampDate", index
                                           , "BioDeg", "BioNarrative", "StressSampID"
                                           , "StressSampDate", "StressorType", "Label"
                                           , "Stressor", "StressorValue", "StressorPctRank"
                                           , LoEcolnames, colNamesLoEcounts, "TotSupport"
                                           , "TotRefute", "TotIndet", "TotNotEval", "WoE")] %>%
    dplyr::arrange(BioDeg, RespSampDate, StressSampDate, StressorType
                   , Label, StressorValue)

  fnEvidDetails <- paste0(TargetSiteID, "_", biocomm, "_WoE_ScoresTable.tab")
  write.table(dfEvidCountsWide, file = file.path(dirWoE, fnEvidDetails)
              , append = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)

  # Get the unique "core" columns for the exec summary file
  startcol <- which(colnames(dfEvidCountsWide) == LoEcolnames[1])
  endcol <- ncol(dfEvidCountsWide)
  dfEvidCountsWide[,startcol:endcol][dfEvidCountsWide[, startcol:endcol] == "NE"] <- NA
  indexcol <- which(colnames(dfEvidCountsWide) == index)

  dfData4ES <- unique(dfEvidCountsWide[, c("StationID_Master", "BioComm", "BioDeg"
                                          , index
                                          , as.character("RespSampID")
                                          , as.character("StressSampID")
                                          , "StressorType", "Stressor"
                                          , LoEcolnames, "WoE")]) %>%
    dplyr::mutate(WoEnumeric = ifelse(WoE == "Supports", 1
                                      , ifelse(WoE == "Refutes", -1
                                               , 0))) %>%
    dplyr::select(-WoE) %>% dplyr::rename(WoE = WoEnumeric
                                          , IndexScore = all_of(index))
  dfData4ES <- dfData4ES %>%
    group_by(StationID_Master, BioComm, BioDeg, StressorType) %>%
    dplyr::summarize(NumRespSamples = n_distinct(RespSampID)
                     , minIndex = min(IndexScore, na.rm = TRUE)
                     , meanIndex = mean(IndexScore, na.rm = TRUE)
                     , maxIndex = max(IndexScore, na.rm = TRUE)
                     , NumStressSamples = n_distinct(StressSampID)
                     , NumStressors = n_distinct(Stressor)
                     , WtTot_WoE = round(sum(WoE, na.rm = TRUE)/n(), 3)
                     , WtTotTS_barplot = ifelse(all(is.na(TS_barplot)), NA
                                                , round(sum(as.integer(TS_barplot)
                                                            , na.rm = TRUE)/n(), 3))
                     , WtTotCO_boxplot = ifelse(all(is.na(CO_boxplot)), NA
                                                , round(sum(as.integer(CO_boxplot)
                                                            , na.rm = TRUE)/n(), 3))
                     , WtTotSR_InCase_LogRegr = ifelse(all(is.na(SR_InCase_LogRegr)), NA
                                                       , round(sum(as.integer(SR_InCase_LogRegr)
                                                                   , na.rm = TRUE)/n(), 3))
                     , WtTotSR_InCase_LinRegr = ifelse(all(is.na(SR_InCase_LinRegr)), NA
                                                       , round(sum(as.integer(SR_InCase_LinRegr)
                                                                   , na.rm = TRUE)/n(), 3))
                     , WtTotSR_OutCase_LinRegr = ifelse(all(is.na(SR_OutCase_LinRegr)), NA
                                                        , round(sum(as.integer(SR_OutCase_LinRegr)
                                                                    , na.rm = TRUE)/n(), 3))
                     , WtTotVP_boxplot = ifelse(all(is.na(VP_boxplot_senstaxa)) &
                                                  all(is.na(VP_boxplot_toltaxa)), NA
                                                , round(sum(as.integer(VP_boxplot_senstaxa)
                                                            + as.integer(VP_boxplot_toltaxa)
                                                            , na.rm = TRUE)/n(), 3))
                     , .groups = "drop_last")

  dfData4ES <- as.data.frame(dfData4ES)
  startcol <- which(colnames(dfData4ES) == "WtTot_WoE")
  endcol <- ncol(dfData4ES)
  dfData4ES[, startcol:endcol][is.na(dfData4ES[, startcol:endcol])] <- "NE"

  fnES <- paste0(TargetSiteID, "_", biocomm, "_WoE_ExecSummary.tab")
  write.table(dfData4ES, file = file.path(dirWoE, fnES), append = FALSE
              , col.names = TRUE, row.names = FALSE, sep = "\t")
  #
}

