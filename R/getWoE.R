#' @title Weight-of-Evidence summary
#' 
#' @description Summarize weight of evidence using scores from other functions.
#' 
#' @details Stressor-based weight of evidence for the stressor as a cause of impairment
#'          for the specified biological community. Combines information from
#'          the co-occurrence, stressor-response using data from or outside the 
#'          case, verified predictions, and stressor-response from laboratory data.
#' 
#' Uses the libraries dplyr and tidyr.
#' 
#' @param TargetSiteID Site ID
#' @param dfRank Percent rank of each stressor in the distribution of comparator sites.
#' @param df_coOccur CoOccur dataframe corresponding with stressors and specified biocomm
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param index Index name (IBI, CSCI, ASCI, etc.) Default = "IBI".
#' @param dir_results Directory to save plots.  Default = working directory and Results.
#' @param CO_sub Subdirectory containing co-occurrence results. Default = "CoOccurrence".
#' @param SR_sub Subdirectory containing stressor-response results. Default = "StressorResponse".
#' @param VP_sub Subdirectory containing verified prediction results.  Default = "VerifiedPredictions".
#' @param SSD_sub Subdirectory containing SSD results. Default = "SSD".
#' 
#' @return Four tab-delimited tables containing weight of evidence information:
#'         summary of the site results by stressor; more detailed review of 
#'         each site sample by stressor and each line of evidence; weight of evidence
#'         for metrics evaluated for stressor-response lines of evidence;
#'         lookup table describing the short line of evidence code.
#' 
#' Lines of evidence evaluated (if possible):
#' CO: Spatial/temporal co-occurrence 
#' SR: Stressor-response relationships from the field
#' VP: Verified predictions
#' SR: Stressor-response relationships from other field studies
#' SSD: Stressor-response relationships from laboratory studies
#' 
#' @keywords internal
#' 
#' @export
getWoE <- function(TargetSiteID
                   , biocomm = "bmi"
                   , index = "CSCI"
                   , dir_results = dir_results
                   , dfLoE = df_LoE
                   , dfQual = list.BioQualSites$dfQuality
                   , dfStr = list_MatchBioData$site.b.str
                   , dfRank = list.stressors$site.stressor.pctrank
                   , dfStressInfo = siteStressInfo
                   , df_coOccur = data_bioCoOccur
                   , BioResp = bioMetricNames) {
    
    # QC data
    boo_DEBUG <- FALSE
    
    if (boo_DEBUG == TRUE) {
        TargetSiteID = TargetSiteID
        biocomm = bioComm
        index = bioIndex
        dir_results = dir_results
        dfLoE = df_LoE
        dfQual = list.BioQualSites$dfQuality
        dfStr = list_MatchBioData$site.b.str
        dfRank = list.stressors$site.stressor.pctrank
        dfStressInfo = siteStressInfo
        df_coOccur = data_bioCoOccur
        BioResp = bioMetricNames
    }

    # define pipe
    `%>%` <- dplyr::`%>%`
    biocomm <- toupper(biocomm)

    subdir = TargetSiteID
    ifelse(!dir.exists(file.path(dir_results, subdir, biocomm, "WoE"))==TRUE
           , dir.create(file.path(dir_results, subdir, biocomm, "WoE"))
           , FALSE)
    dirWoE <- file.path(dir_results, subdir, biocomm, "WoE")
   
    LoEcols <- c("StationID_Master", "StressSampID", "RespSampID", "Response"
                 , "ResponseValue", "Stressor", "StressorValue", "n", "nType"
                 , "Score", "LoEtrim", "LoE", "Analysis", "InOut", "biocomm")

    # Filenames for all files containing scores
    fnTSScores <- paste0(TargetSiteID, "_", toupper(biocomm)
                         , "_TS_Scores.tab")
    fnCOScores <- paste0(TargetSiteID, "_", toupper(biocomm)
                         , "_CO_Scores.tab")
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
    if(exists("dfEvidenceLong")){rm(dfEvidenceLong)}
    if(exists("dfTemp")){rm(dfTemp)}
    
    # prep dfStr for use in creating NE datasets for lines not evaluated
    numSamps <- length(unique(as.character(dfStr$StressSampID)))
    dfStr <- dfStr %>%
        tidyr::gather(key = "Stressor", value = "StressorValue"
                      , -StationID_Master, -StressSampID, -RespSampID)
    dfSampDates <- df_coOccur %>%
        dplyr::filter(StationID_Master == TargetSiteID) %>%
        dplyr::select(StationID_Master, RespSampID, RespSampDate
                      , StressSampID, StressSampDate)
    
    # Iterate over dfLoE to obtain all the evidence for each available line
    for (l in 1:nrow(dfLoE)) {

        chrLoE <- dfLoE$LoE[l]
        booUse <- dfLoE$Completed[l]
        dirLoE <- dfLoE$ResultsDir[l]
        
        scored <- ifelse(booUse == 1, " which was evaluated.", " which was not evaluated.")
        msg <- paste0("Processing ", chrLoE, scored)
        message(msg)
        # print(msg)
        # flush.console()
        
        # Create dummy dataframe
        dfTemp <- dfStr
        dfTemp <- merge(dfStr, dfQual[,c("RespSampID",index)]
                        , by.x = "RespSampID", by.y = "RespSampID")
        dfTemp <- dplyr::rename(dfTemp, ResponseValue = index)
        dfTemp <- dfTemp[!is.na(dfTemp$StressorValue),]

        if(booUse==0){ # LoE was not evaluated. Need to enter NE scores for given LoE
            
            # Create dummy dataframe
            # dfTemp <- dfStr
            # dfTemp <- merge(dfStr, dfQual[,c("RespSampID",index)]
            #                 , by.x = "RespSampID", by.y = "RespSampID")
            # dfTemp <- dplyr::rename(dfTemp, ResponseValue = index)
            # dfTemp <- dfTemp[!is.na(dfTemp$StressorValue),]

            if (chrLoE == "TS") {
                dfTemp <- dfTemp %>%
                    dplyr::mutate(Response = index
                                  , n = numSamps
                                  , nType = "Target site samples only"
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
            }
            if (chrLoE == "CO") {   # Come back to this one
                dfTemp1 <- dfTemp %>%
                    dplyr::mutate(Response = index
                                  , n = NA
                                  , nType = "Comparator samples with better biology"
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
                dfTemp2 <- dfTemp %>%
                    dplyr::mutate(Response = index
                                  , n = NA
                                  , nType = "All comparator samples"
                                  , Score = "NE"
                                  , LoEtrim = "SR_InCase_LogRegr"
                                  , LoE = "Stressor-response in the case"
                                  , Analysis = "Logistic regression"
                                  , InOut = "Inside the case"
                                  , biocomm = biocomm) %>%
                    dplyr::select(StationID_Master, StressSampID, RespSampID
                                  , Response, ResponseValue, Stressor
                                  , StressorValue, n, nType, Score, LoEtrim
                                  , LoE, Analysis, InOut, biocomm)

                dfTemp <- rbind(dfTemp1, dfTemp2)
            }
            if (chrLoE == "SR") {   # Come back to this one
                dfTemp1 <- dfTemp %>%
                    dplyr::mutate(Response = index
                                  , n = NA
                                  , nType = "All comparator samples"
                                  , Score = "NE"
                                  , LoEtrim = "SR_InCase_LinRegr"
                                  , LoE = "Stressor-response in the case"
                                  , Analysis = "Linear regression"
                                  , InOut = "Inside the case"
                                  , biocomm = biocomm) %>%
                    dplyr::select(StationID_Master, StressSampID, RespSampID
                                  , Response, ResponseValue, Stressor
                                  , StressorValue, n, nType, Score, LoEtrim
                                  , LoE, Analysis, InOut, biocomm)
                dfTemp2 <- dfTemp %>%
                    dplyr::mutate(Response = index
                                  , n = numSamps
                                  , nType = "All cluster samples"
                                  , Score = "NE"
                                  , LoEtrim = "SR_OutCase_LinRegr"
                                  , LoE = "Stressor-response in the case"
                                  , Analysis = "Linear regression"
                                  , InOut = "Outside the case"
                                  , biocomm = biocomm) %>%
                    dplyr::select(StationID_Master, StressSampID, RespSampID
                                  , Response, ResponseValue, Stressor
                                  , StressorValue, n, nType, Score, LoEtrim
                                  , LoE, Analysis, InOut, biocomm)

                dfTemp <- rbind(dfTemp1, dfTemp2)
            }
            if (chrLoE == "VP") {
                dfTemp1 <- dfTemp %>%
                    dplyr::mutate(Response = "Sensitive Taxa"
                                  , ResponseValue = NA
                                  , n = NA
                                  , nType = "Comparator samples with better biology"
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
                    dplyr::mutate(Response = "Tolerant Taxa"
                                  , ResponseValue = NA
                                  , n = NA
                                  , nType = "Comparator samples with better biology"
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
            }
            if (chrLoE == "SSD") {
                dfTemp <- dfTemp %>%
                    dplyr::mutate(Response = "Number expected taxa not observed"
                                  , ResponseValue = NA
                                  , n = NA
                                  , nType = "Laboratory toxicity studies"
                                  , Score = "NE"
                                  , LoEtrim = "SSD_ToxicityCurve"
                                  , LoE = "Taxon sensitivity distribution"
                                  , Analysis = "Probability of extirpation"
                                  , InOut = "Outside the case"
                                  , biocomm = biocomm) %>%
                    dplyr::select(StationID_Master, StressSampID, RespSampID
                                  , Response, ResponseValue, Stressor
                                  , StressorValue, n, nType, Score, LoEtrim
                                  , LoE, Analysis, InOut, biocomm)
            }

            gapcomment <- "Line of evidence not evaluated."
            gaps <- cbind.data.frame("getWoE", chrLoE, 0
                                     , gapcomment)
            colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(dir_results, TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
        } else { # booUse==1; If an LoE wasn't evaluated, skip to the next LoE
        
            # Get Time Sequence data
            if (chrLoE == "TS") {
                if (file.exists(file.path(dirLoE,fnTSScores))) {
                    # Pull data into temp data structure
                    # Currently not scored, so file does not exist--ever
                    next
    
                } else {
                    # No scores available
                    dfTemp <- dfStr
                    dfTemp <- merge(dfStr, dfQual[,c("RespSampID",index)]
                                    , by.x = "RespSampID", by.y = "RespSampID")
                    dfTemp <- dfTemp[!is.na(dfTemp$StressorValue),]
                    dfTemp <- dplyr::rename(dfTemp, ResponseValue = index)
                    
                    dfTemp <- dfTemp %>%
                        dplyr::mutate(Response = index
                                      , n = numSamps
                                      , nType = "Target site samples only"
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
                    fn.gaps <- file.path(dir_results, TargetSiteID,fn.gaps)
                    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                                , row.names = FALSE, sep = "\t")
                }
            } # End TS LoE
    
            # Get CoOccurrence data
            if (chrLoE == "CO") {
                if (file.exists(file.path(dirLoE,fnCOScores))) {
                    dfCO <- read.table(file.path(dirLoE,fnCOScores)
                                        , header = TRUE, sep = "\t"
                                        , stringsAsFactors = FALSE)
                    colnames(dfCO) <- c("StationID_Master", "Cluster", "StressSampID"
                                        , "RespSampID", index, "BioNarrative"
                                        , "BioDegYN", "Stressor", "StressorValue"
                                        , "n", "q25", "q50", "q75", "Sc_Boxplot"
                                        , "SR_pred_Deg", "SC_SRLog", "biocomm"
                                        , "Label")
                    dfCO <- dplyr::select(dfCO, -Label)
                    dfCO <- dfCO[!is.na(dfCO$StressorValue),]
                    dfCO <- unique(dfCO)
                    
                    # Pull out co-occurrence scores from co-occurrence file
                    dfCO1 <- dfCO %>%
                        dplyr::mutate(Response = index
                                      , nType = "Comparator samples with better biology"
                                      , LoEtrim = "CO_boxplot"
                                      , LoE = "Co-occurrence"
                                      , Analysis = "Box plot"
                                      , InOut = "Inside the case") %>%
                        dplyr::rename(ResponseValue = eval(index)
                                      , Score = Sc_Boxplot)
                    
                    dfCO1 <- dfCO1[!is.na(dfCO1$Score),]
                    dfCO1 <- dfCO1[,LoEcols]
                    
                    # Pull out the SR logistic regression scores from co-occurrence file
                    dfSRlog <- dfCO %>%
                        dplyr::mutate(Response = index
                                      , nType = "All comparator samples"
                                      , LoEtrim = "SR_InCase_LogRegr"
                                      , LoE = "Stressor-response in the case"
                                      , Analysis = "Logistic regression"
                                      , InOut = "Inside the case") %>%
                        dplyr::rename(ResponseValue = eval(index)
                                      , Score = SC_SRLog)
                    
                    dfSRlog <- dfSRlog[!is.na(dfSRlog$Score),]
                    dfSRlog <- dfSRlog[,LoEcols]
                    
                    dfTemp <- rbind(dfCO1, dfSRlog)
                    rm(dfCO, dfCO1, dfSRlog)
                    
                } else {
                    # No scores available
                    dfTemp1 <- dfTemp %>%
                        dplyr::mutate(Response = index
                                      , n = NA
                                      , nType = "Comparator samples with better biology"
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
                    dfTemp2 <- dfTemp %>%
                        dplyr::mutate(Response = index
                                      , n = NA
                                      , nType = "All comparator samples"
                                      , Score = "NE"
                                      , LoEtrim = "SR_InCase_LogRegr"
                                      , LoE = "Stressor-response in the case"
                                      , Analysis = "Logistic regression"
                                      , InOut = "Inside the case"
                                      , biocomm = biocomm) %>%
                        dplyr::select(StationID_Master, StressSampID, RespSampID
                                      , Response, ResponseValue, Stressor
                                      , StressorValue, n, nType, Score, LoEtrim
                                      , LoE, Analysis, InOut, biocomm)
                    
                    dfTemp <- rbind(dfTemp1, dfTemp2)
                }
            } # End CO LoE (plus SR logistic regressions)
            
            # Get Stressor-Response data
            if (chrLoE == "SR") {
                if (file.exists(file.path(dirLoE,fnSRScores))) {
                    dfSR <- read.table(file.path(dirLoE, fnSRScores)
                                       , header = TRUE, sep = "\t"
                                       , stringsAsFactors = FALSE
                                       , comment.char = "")
                    
                    colnames(dfSR) <- c("StationID_Master", "Stressor", "Response"
                                        , "StressSampID", "RespSampID", "Quality"
                                        , "StressorValue", "ResponseValue", "biocomm"
                                        , "stressLabel", "respLabel"
                                        , "n_site", "n_all", "SRlin_ScoreAll"
                                        , "n_clust", "SRlin_ScoreCluster")
                    dfSR <- dfSR[!is.na(dfSR$StressorValue),]
                    dfSR <- unique(dfSR)
                    dfSR <- dplyr::select(dfSR, -stressLabel, -respLabel)
                    
                    # SR linear regression inside the case
                    dfSRlin_inside <- dfSR %>%
                        dplyr:: mutate(nType = "All comparator samples"
                                       , LoEtrim = "SR_InCase_LinRegr"
                                       , LoE = "Stressor-response in the case"
                                       , Analysis = "Linear regression"
                                       , InOut = "Inside the case") %>%
                        dplyr::rename(n = n_clust, Score = SRlin_ScoreCluster)
                    dfSRlin_inside <- dfSRlin_inside[!is.na(dfSRlin_inside$Score),]
                    dfSRlin_inside <- dfSRlin_inside[,LoEcols]                
                    
                    # SR linear regression outside the case
                    dfSRlin_outside <- dfSR %>%
                        dplyr:: mutate(nType = "All cluster samples"
                                       , LoEtrim = "SR_OutCase_LinRegr"
                                       , LoE = "Stressor-response in the case"
                                       , Analysis = "Linear regression"
                                       , InOut = "Outside the case") %>%
                        dplyr::rename(n = n_all, Score = SRlin_ScoreAll)
                    dfSRlin_outside <- dfSRlin_outside[!is.na(dfSRlin_outside$Score),]
                    dfSRlin_outside <- dfSRlin_outside[,LoEcols]
                    
                    dfTemp <- rbind(dfSRlin_inside, dfSRlin_outside)
                    rm(dfSR, dfSRlin_inside, dfSRlin_outside)
    
                    # Metrics
                    dfMetrics <- dfTemp %>%
                        dplyr::filter(!Response %in% index)
                    
                    # Index
                    dfTemp <- dfTemp %>%
                        dplyr::filter(Response %in% index)
                    
                } else {
                    # No scores available
                    dfTemp1 <- dfTemp %>%
                        dplyr::mutate(Response = index
                                      , n = NA
                                      , nType = "All comparator samples"
                                      , Score = "NE"
                                      , LoEtrim = "SR_InCase_LinRegr"
                                      , LoE = "Stressor-response in the case"
                                      , Analysis = "Linear regression"
                                      , InOut = "Inside the case"
                                      , biocomm = biocomm) %>%
                        dplyr::select(StationID_Master, StressSampID, RespSampID
                                      , Response, ResponseValue, Stressor
                                      , StressorValue, n, nType, Score, LoEtrim
                                      , LoE, Analysis, InOut, biocomm)
                    dfTemp2 <- dfTemp %>%
                        dplyr::mutate(Response = index
                                      , n = numSamps
                                      , nType = "All cluster samples"
                                      , Score = "NE"
                                      , LoEtrim = "SR_OutCase_LinRegr"
                                      , LoE = "Stressor-response in the case"
                                      , Analysis = "Linear regression"
                                      , InOut = "Outside the case"
                                      , biocomm = biocomm) %>%
                        dplyr::select(StationID_Master, StressSampID, RespSampID
                                      , Response, ResponseValue, Stressor
                                      , StressorValue, n, nType, Score, LoEtrim
                                      , LoE, Analysis, InOut, biocomm)
                    
                    dfTemp <- rbind(dfTemp1, dfTemp2)
                }
            } # End SR LoE (linear regressions)
            
            # Get Verified Prediction data
            if (chrLoE == "VP") {
                if (file.exists(file.path(dirLoE,fnVPScores))) {
                    dfVP <- read.table(file.path(dirLoE,fnVPScores)
                                       , header = TRUE, sep = "\t"
                                       , stringsAsFactors = FALSE)
                    colnames(dfVP) <- c("StationID_Master", "RespSampID"
                                        , "IndexValue", "Quality", "StressSampID"
                                        , "Label", "Stressor", "StressorValue"
                                        , "Response", "ResponseValue", "qLoValue_Cutoff"
                                        , "qHiValue_Cutoff", "Score", "biocomm"
                                        , "n", "n_BetterNotDegraded")
                    dfVP <- dfVP[!is.na(dfVP$StressorValue),]
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
                    
                    dfVP <- dfVP[!is.na(dfVP$Score),]
                    dfVP <- dfVP[,LoEcols]
                    
                    dfTemp <- dfVP
                    rm(dfVP)
    
                } else {
                    # No scores available
                    # Create dummy dataframe with NE for all scores
                    dfTemp1 <- dfTemp %>%
                        dplyr::mutate(Response = "Sensitive Taxa"
                                      , ResponseValue = NA
                                      , n = NA
                                      , nType = "Comparator samples with better biology"
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
                        dplyr::mutate(Response = "Tolerant Taxa"
                                      , ResponseValue = NA
                                      , n = NA
                                      , nType = "Comparator samples with better biology"
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
                }
            } # End VP LoE
            
            # Get Species Sensitivity Distribution data
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
    LoEcolnames <- unique(dfEvidenceLong$LoEtrim)
    
    # Merge stressor group name and percent rank into siteStressInfo
    dfRank <- dplyr::select(dfRank,-IQRmethod,-SDmethod,-Outlier,-StressSampDate)
    dfRank <- tidyr::gather(dfRank, key = "Stressor", value = "StressorPctRank"
                      , -StationID_Master, -StressSampID)
    dfRank <- dplyr::filter(dfRank, !is.na(StressorPctRank))
    
    dfStressInfo <- dfStressInfo %>%
        select(StdParamName, GroupName, Label) %>%
        rename(Stressor = StdParamName, StressorType = GroupName)
    
    dfStrGpRank <- unique(merge(dfRank, dfStressInfo[,c("Stressor", "StressorType")]
                                , by.x = "Stressor"
                                , by.y = "Stressor"))
    
    dfStrGpRankQual <- unique(merge(dfStrGpRank, dfQual
                                    , by.x = c("StationID_Master", "StressSampID")
                                    , by.y = c("StationID_Master", "StressSampID")))
    
    # Need to separately grab modeled flow data and merge with resp samps (all for station) & rbind
    dfStrGpRankModl <- dfStrGpRank[grepl("_modeledflow",dfStrGpRank$StressSampID),]
    dfQualModl <- dfQual[,c("StationID_Master","clust","RespSampID",index
                            ,"BioDeg","BioNarrative","ComparatorYN","BetterThan")]
    dfStrGpRankQualModl <- unique(merge(dfStrGpRankModl, dfQualModl))
    dfStrGpRankQualModl <- dplyr::select(dfStrGpRankQualModl, StationID_Master
                                         , -StressSampID, Stressor, StressorPctRank
                                         , StressorType, clust, RespSampID
                                         , eval(index), BioDeg, BioNarrative
                                         , ComparatorYN, BetterThan)
    dfStrGpRankQualModl <- merge(dfStrGpRankQualModl, dfSampDates
                                 , by.x = c("StationID_Master", "RespSampID")
                                 , by.y = c("StationID_Master", "RespSampID"))
    dfStrGpRankQualModl <- dplyr::select(dfStrGpRankQualModl, StationID_Master
                                         , StressSampID, Stressor, StressorPctRank
                                         , StressorType, clust, RespSampID
                                         , eval(index), BioDeg, BioNarrative
                                         , ComparatorYN, BetterThan)
    dfStrGpRankQual <- rbind(dfStrGpRankQual, dfStrGpRankQualModl)
    
    if (boo_DEBUG==FALSE) { rm(dfRank, dfStrGpRank) }

    # Merge in Quality info and Stressor Types
    dfEvidenceLong <- merge(dfStrGpRankQual, dfEvidenceLong
                            , by.x = c("StationID_Master", "StressSampID"
                                       , "Stressor", "RespSampID")
                            , by.y = c("StationID_Master", "StressSampID"
                                       , "Stressor", "RespSampID"))
    
    # Add additional information to the Long form 
    dfEvidenceLong <- merge(dfEvidenceLong, dfStressInfo[,c("Stressor","Label")]
                            , all.x = TRUE)
    dfEvidenceLong <- dfEvidenceLong %>%
        dplyr::rename(Cluster = clust, BioComm = biocomm, Inside_Outside = InOut) %>%
        dplyr::select(StationID_Master, Cluster, BioComm, RespSampID, eval(index)
                      , BioDeg, BioNarrative, Response, ResponseValue, StressSampID
                      , StressorType, Label, Stressor, StressorValue, StressorPctRank
                      , Score, n, nType, LoEtrim, LoE, Analysis, Inside_Outside) %>%
        dplyr::mutate(Finding = ifelse(Score=="NE","Not evaluated"
                                       , ifelse(as.numeric(Score)>0, "Supports"
                                       , ifelse(as.numeric(Score)<0, "Refutes"
                                       , ifelse(as.numeric(Score)==0, "Indeterminate"
                                       , NA))))) %>%
        dplyr::arrange(StressorType, Label, StressSampID, LoE)

    # Write the detailed data file
    fnEvidLong <- paste0(TargetSiteID,"_", biocomm, "_WoE_DetailedLoEs.tab")
    write.table(dfEvidenceLong, file.path(dirWoE,fnEvidLong), append = FALSE
                , col.names = TRUE, row.names = FALSE, sep = "\t")
    
    if (file.exists(file.path(dirLoE,fnSRScores))) {
        # Merge in Quality info and Stressor Types for Metrics
        dfMetricsLong <- merge(dfStrGpRankQual, dfMetrics
                                , by.x = c("StationID_Master", "StressSampID"
                                           , "Stressor", "RespSampID")
                                , by.xy = c("StationID_Master", "StressSampID"
                                            , "Stressor", "RespSampID"))
        
        # Add additional information to the Long form for Metrics
        dfMetricsLong <- merge(dfMetricsLong, dfStressInfo[,c("Stressor","Label")]
                                , all.x = TRUE)
        dfMetricsLong <- dfMetricsLong %>%
            dplyr::rename(Cluster = clust, BioComm = biocomm, Inside_Outside = InOut) %>%
            dplyr::select(StationID_Master, Cluster, BioComm, RespSampID, eval(index)
                          , BioDeg, BioNarrative, Response, ResponseValue, StressSampID
                          , StressorType, Label, Stressor, StressorValue, StressorPctRank
                          , Score, n, nType, LoEtrim, LoE, Analysis, Inside_Outside) %>%
            dplyr::mutate(Finding = ifelse(Score=="NE","Not evaluated"
                                           , ifelse(as.numeric(Score)>0, "Supports"
                                           , ifelse(as.numeric(Score)<0, "Refutes"
                                           , ifelse(as.numeric(Score)==0, "Indeterminate"
                                           , NA))))) %>%
            dplyr::arrange(StressorType, Label, StressSampID, LoE)
    
        # Write the detailed data file
        fnEvidLongMetrics <- paste0(TargetSiteID,"_", biocomm, "_WoE_DetailedMetricsLoEs.tab")
        write.table(dfMetricsLong, file.path(dirWoE,fnEvidLongMetrics), append = FALSE
                    , col.names = TRUE, row.names = FALSE, sep = "\t")
    }
    
    dfEvidenceWide <- unique(dfEvidenceLong[,c("StressSampID","Label","Stressor"
                                               ,"StressorValue","Score","LoEtrim")])
    # Pivot scores to wide format
    dfEvidenceWide <- dfEvidenceWide %>%
        dplyr::mutate(Score = as.numeric(Score)) %>%
        dplyr::group_by(StressSampID, Label, Stressor, StressorValue, LoEtrim) %>%
        dplyr::summarize(TotScore = sum(Score), .groups = "drop") %>%
        dplyr::rename(Score = TotScore) %>%
        tidyr::spread(key = "LoEtrim", value = sum(Score, na.rm=TRUE), fill=NA)
    dfEvidenceWide <- as.data.frame(dfEvidenceWide)
    endcol <- ncol(dfEvidenceWide)
    dfEvidenceWide[,4:endcol][is.na(dfEvidenceWide[,4:endcol])] <- "NE"
    
    # Provide text interpretation of score
    dfEvidenceCounts <- dfEvidenceLong %>%
        dplyr::select(RespSampID, StressSampID, Label, Stressor, StressorValue
                      , Inside_Outside, Finding, Score) %>%
        dplyr::group_by(RespSampID, StressSampID, Label, Stressor, StressorValue
                        , Inside_Outside, Finding, Score) %>%
        dplyr::summarise(NumLoE = n())
    
    # dfEvidenceCounts2 <- dfEvidenceCounts
    dfEvidenceCounts$Heading <- NA
    dfEvidenceCounts$Heading <- ifelse(dfEvidenceCounts$Inside_Outside == "Inside the case"
                                        & dfEvidenceCounts$Finding == "Supports"
                                        , "NumInSupport", dfEvidenceCounts$Heading)
    dfEvidenceCounts$Heading <- ifelse(dfEvidenceCounts$Inside_Outside == "Inside the case"
                                        & dfEvidenceCounts$Finding == "Refutes"
                                        , "NumInRefute", dfEvidenceCounts$Heading)
    dfEvidenceCounts$Heading <- ifelse(dfEvidenceCounts$Inside_Outside == "Inside the case"
                                        & dfEvidenceCounts$Finding == "Indeterminate"
                                        , "NumInIndet", dfEvidenceCounts$Heading)
    dfEvidenceCounts$Heading <- ifelse(dfEvidenceCounts$Inside_Outside == "Inside the case"
                                       & dfEvidenceCounts$Finding == "Not evaluated"
                                       , "NumInNotEval", dfEvidenceCounts$Heading)
    dfEvidenceCounts$Heading <- ifelse(dfEvidenceCounts$Inside_Outside == "Outside the case"
                                        & dfEvidenceCounts$Finding == "Supports"
                                        , "NumOutSupport", dfEvidenceCounts$Heading)
    dfEvidenceCounts$Heading <- ifelse(dfEvidenceCounts$Inside_Outside == "Outside the case"
                                        & dfEvidenceCounts$Finding == "Refutes"
                                        , "NumOutRefute", dfEvidenceCounts$Heading)
    dfEvidenceCounts$Heading <- ifelse(dfEvidenceCounts$Inside_Outside == "Outside the case"
                                        & dfEvidenceCounts$Finding == "Indeterminate"
                                        , "NumOutIndet", dfEvidenceCounts$Heading)
    dfEvidenceCounts$Heading <- ifelse(dfEvidenceCounts$Inside_Outside == "Outside the case"
                                       & dfEvidenceCounts$Finding == "Not evaluated"
                                       , "NumOutNotEval", dfEvidenceCounts$Heading)
    
    # Need to convert counts to wide format to get Num not eval, and totals
    dfEvidenceCounts <- as.data.frame(dfEvidenceCounts)
    dfEvidCountsWide <- dfEvidenceCounts %>%
        dplyr::select(-Inside_Outside, -Finding, -Score) %>%
        dplyr::group_by(StressSampID, Label, Stressor, StressorValue) %>%
        tidyr::spread(key = "Heading", value = "NumLoE", fill = 0)

    # Account for missing columns
    colNamesLoEcounts <- c("NumInSupport", "NumInRefute", "NumInIndet", "NumInNotEval"
                           , "NumOutSupport", "NumOutRefute", "NumOutIndet", "NumOutNotEval")
    colNamesInEvidCountsWide <- colnames(dfEvidCountsWide)
    colNamesInEvidCounts <- colNamesInEvidCountsWide[grepl("^Num.*$",colNamesInEvidCountsWide)]
    colNamesNeeded <- setdiff(colNamesLoEcounts,colNamesInEvidCounts)
    colNamesKeep <- setdiff(colNamesInEvidCountsWide,colNamesLoEcounts)

    if (length(colNamesNeeded)>0) {
        for (nm in colNamesNeeded) {
            dfEvidCountsWide[[nm]] <- 0
        }
    }
    
    dfEvidCountsWide <- dfEvidCountsWide[,c(colNamesKeep, colNamesLoEcounts)]
    
    # Merge individual LoE with summary LoE
    dfEvidCountsWide <- merge(dfEvidenceWide, dfEvidCountsWide
                              , by.x = c("StressSampID", "Label", "Stressor"
                                         , "StressorValue")
                              , by.y = c("StressSampID", "Label", "Stressor"
                                         , "StressorValue"))
    
    dfEvidCountsWide <- dfEvidCountsWide %>%
        dplyr::mutate(TotSupport = NumInSupport + NumOutSupport
                      , TotRefute = NumInRefute + NumOutRefute
                      , TotIndet = NumInIndet + NumOutIndet
                      , TotNotEval = NumInNotEval + NumOutNotEval
                      , WoE = ifelse(TotIndet > (TotSupport + TotRefute)
                                     , "Indeterminate"
                                     , ifelse(TotSupport > TotRefute, "Supports"
                                              , ifelse(TotRefute > TotSupport, "Refutes"
                                                       , "Indeterminate"))))
    
    # Merge basic data back in to summary
    dfEvidBasic <- dfEvidenceLong %>%
        dplyr::select(StationID_Master, Cluster, BioComm, RespSampID, eval(index)
                      , BioDeg, BioNarrative, StressSampID, StressorType, Label
                      , Stressor, StressorValue, StressorPctRank)
    dfEvidBasic <- unique(as.data.frame(dfEvidBasic))
    
    dfEvidCountsWide <- merge(dfEvidBasic, dfEvidCountsWide
                               , by.x = c("StressSampID", "Label", "Stressor"
                                          , "StressorValue", "RespSampID")
                               , by.y = c("StressSampID", "Label", "Stressor"
                                          , "StressorValue", "RespSampID"))
    # Get Sample Dates
    dfEvidCountsWide <- merge(dfEvidCountsWide, dfSampDates)
    
    # Order the columns sensibly
    dfEvidCountsWide <- dfEvidCountsWide %>%
        dplyr::select(StationID_Master, Cluster, BioComm, RespSampID, RespSampDate
                      , eval(index), BioDeg, BioNarrative, StressSampID, StressSampDate
                      , StressorType, Label, Stressor, StressorValue, StressorPctRank
                      , eval(LoEcolnames), eval(colNamesLoEcounts), TotSupport
                      , TotRefute, TotIndet, TotNotEval, WoE) %>%
        dplyr::arrange(BioDeg, RespSampDate, StressSampDate, StressorType
                       , Label, StressorValue)

    fnEvidDetails <- paste0(TargetSiteID,"_",biocomm,"_WoE_ScoresTable.tab")
    write.table(dfEvidCountsWide, file = file.path(dirWoE, fnEvidDetails)
                , append = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)

    ## Create sample summary (# samps, min, avg, max values separately for deg/not)
    # dfStressorSummary <- dfEvidCountsWide %>%
    #     dplyr::select(StationID_Master, index, BioDeg, StressorType, Label
    #                   , Stressor, StressorValue, eval(LoEcolnames), WoE) %>%
    #     dplyr::group_by(StationID_Master, index, BioDeg, StressorType
    #                     , Label, Stressor) %>%
    #     dplyr::summarize(nSamps = n()
    #                      , MinStressorValue = min(StressorValue, na.rm=TRUE)
    #                      , AvgStressorValue = mean(StressorValue, na.rm = TRUE)
    #                      , MaxStressorValue = max(StressorValue, na.rm = TRUE))
    
    # Get the unique "core" columns for the exec summary file
    startcol <- which(colnames(dfEvidCountsWide)==LoEcolnames[1])
    endcol <- ncol(dfEvidCountsWide)
    dfEvidCountsWide[,startcol:endcol][dfEvidCountsWide[,startcol:endcol]=="NE"] <- NA
    indexcol <- which(colnames(dfEvidCountsWide)==index)
    
    dfData4ES <- unique(dfEvidCountsWide[,c("StationID_Master", "BioComm", "BioDeg"
                                            , index
                                            , as.character("RespSampID")
                                            , as.character("StressSampID")
                                            , "StressorType", "Stressor"
                                            , LoEcolnames, "WoE")]) %>%
        dplyr::mutate(WoEnumeric = ifelse(WoE=="Supports", 1
                                         , ifelse(WoE=="Refutes", -1
                                         , 0))) %>%
        dplyr::select(-WoE) %>% dplyr::rename(WoE=WoEnumeric
                                              , IndexScore = index)
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
                                                    , na.rm=TRUE)/n(), 3))
                         , WtTotCO_boxplot = ifelse(all(is.na(CO_boxplot)), NA
                                                    , round(sum(as.integer(CO_boxplot)
                                                    , na.rm=TRUE)/n(), 3))
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
                         , .groups = "drop") %>%
        dplyr::select(-.groups)

    dfData4ES <- as.data.frame(dfData4ES)
    startcol <- which(colnames(dfData4ES)=="WtTot_WoE")
    endcol <- ncol(dfData4ES)
    dfData4ES[,startcol:endcol][is.na(dfData4ES[,startcol:endcol])] <- "NE"

    fnES <- paste0(TargetSiteID,"_",biocomm,"_WoE_ExecSummary.tab")
    write.table(dfData4ES, file = file.path(dirWoE, fnES), append = FALSE
                , col.names = TRUE, row.names = FALSE, sep = "\t")
    

    #     
}

