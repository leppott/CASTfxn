#  Copyright 2020 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents 
#  is expressly prohibited without prior written permission of TetraTech.
#
#  You can contact the author at:
#  - RPPTool R package source repository : https://github.com/ALincolnTt/RPPTool


# Ann.RoseberryLincoln@tetratech.com
# Erik.Leppo@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.0.2
# 
# library(devtools)
# install_github("ALincolnTt/RPPTool")
#
# Add Shiny code for use in Shiny App
# 2020-09-10, Erik
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

updateAllScoresTable <- function(dfAllScores, listWeights, fn_allscores
                                 , BioDegBrk=c(-2, 0.799, 2)
                                 , BioDegLab=c("Degraded","Not degraded")) { # FUNCTION.START
    
    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`
    
    if (boo_DEBUG==TRUE) {
        dfAllScores <- dfAllScores
        listWeights <- listWeights
        fn_allscores <- fn_allscores
        BioDegBrk=c(-2, 0.799, 2)
        BioDegLab=c("Degraded", "Not degraded")
    }
    
    myTime <- lubridate::now()
    myDate <- stringr::str_replace_all(stringr::str_extract(myTime,"\\d{4}-\\d{2}-\\d{2}")
                                       , "-", "")
    myTime <- stringr::str_replace_all(stringr::str_extract(myTime,"\\d{2}:\\d{2}:\\d{2}")
                                       , ":", "")
    fn_allscoresFULL <- paste0(fn_allscores,myDate,"_",myTime,".tab")
    fn_allscoresSUMMARY <- paste0(fn_allscores,"Summary_",myDate,"_",myTime,".tab")
    
    indBioCond_rest <- c("wtd_pot_BioCondInd_rest"
                         , "wtd_pot_BioCxnInd"
                         , "wtd_pot_StressorInd"
                         , "wtd_pot_StressorCxnInd")
    indBioCond_prot <- c("wtd_pot_BioCondInd_prot"
                         , "wtd_pot_BioCxnInd"
                         , "wtd_pot_StressorInd"
                         , "wtd_pot_StressorCxnInd")
    indThreat <- c("wtd_thr_FireHazardInd"
                   , "wtd_thr_PlannedDevelopInd")
    indOpp <- c("wtd_opp_RecrInd"
                , "wtd_opp_MSCPInd"
                , "wtd_opp_NASVIInd"
                , "wtd_opp_UserDefInd")
    subsRPP_rest <- c("wtd_sub_PotentialSubIdx_rest"
                      , "wtd_sub_ThreatSubIdx"
                      , "wtd_sub_OppSubIdx")
    subsRPP_prot <- c("wtd_sub_PotentialSubIdx_prot"
                      , "wtd_sub_ThreatSubIdx"
                      , "wtd_sub_OppSubIdx")
    wts_potentialinds <- c("wt_pot_BioCondInd"
                           , "wt_pot_BioCxnInd"
                           , "wt_pot_StressorInd", "wt_pot_StressorCxnInd")
    wts_threatinds <- c("wt_thr_FireHazardInd"
                        , "wt_thr_PlannedDevelopInd")
    wts_oppinds <- c("wt_opp_RecrInd"
                     , "wt_opp_MSCPInd"
                     , "wt_opp_NASVIInd"
                     , "wt_opp_UserDefInd")
    wts_subs <- c("wt_PotentialSubIdx"
                  , "wt_ThreatSubIdx"
                  , "wt_OppSubIdx")
    
    # Add missing columns (after running at start in Shiny)
    col_req <- c("pot_BioCondInd_rest"
                 , "pot_BioCxnInd"
                 , "pot_StressorInd"
                 , "pot_StressorCxnInd"
                 , "thr_FireHazardInd"
                 , "thr_PlannedDevelopInd"
                 , "opp_RecrInd"
                 , "opp_MSCPInd"
                 , "opp_NASVIInd"
                 , "opp_UserDefInd"
                 , "pot_BioCondInd_prot"
                 ,  "wt_opp_UserDefInd")
    col_req_add <- col_req[!(col_req %in% names(dfAllScores))]
    if (length(col_req_add)>0){
        dfAllScores[, col_req_add] <- 1
    }
    
    # Rename columns so that update works (names are consistent)
    dfAllScores <- dfAllScores %>%
        dplyr::mutate(wt_pot_BioCondInd = ifelse(is.na(pot_BioCondInd_rest)
                                                , NA
                                                , listWeights$wtPot_Bio)
                      , wt_pot_BioCxnInd = ifelse(is.na(pot_BioCxnInd)
                                                    , NA
                                                    , listWeights$wtPot_CxnBio)
                      , wt_pot_StressorInd = ifelse(is.na(pot_StressorInd)
                                                    , NA
                                                    , listWeights$wtPot_Stress)
                      , wt_pot_StressorCxnInd = ifelse(is.na(pot_StressorCxnInd)
                                                        , NA
                                                        , listWeights$wtPot_CxnStress)
                      , wt_thr_FireHazardInd = ifelse(is.na(thr_FireHazardInd)
                                                    , NA
                                                    , listWeights$wtThreat_Fire)
                      , wt_thr_PlannedDevelopInd = ifelse(is.na(thr_PlannedDevelopInd)
                                                        , NA
                                                        , listWeights$wtThreat_LUdev)
                      , wt_opp_RecrInd = ifelse(is.na(opp_RecrInd)
                                                , NA
                                                , listWeights$wtOpp_Recr)
                      , wt_opp_MSCPInd = ifelse(is.na(opp_MSCPInd)
                                                , NA
                                                , listWeights$wtOpp_MSCPs)
                      , wt_opp_NASVIInd = ifelse(is.na(opp_NASVIInd)
                                                , NA
                                                , listWeights$wtOpp_NASVI)
                      , wt_opp_UserDefInd = ifelse(is.na(opp_UserDefInd)
                                                    , NA
                                                    , listWeights$wtOpp_UserDefined)
                      , wtd_pot_BioCondInd_rest = ifelse(is.na(pot_BioCondInd_rest)
                                                        , NA
                                                        , signif((wt_pot_BioCondInd*pot_BioCondInd_rest),3))
                      , wtd_pot_BioCondInd_prot = ifelse(is.na(pot_BioCondInd_prot)
                                                        , NA
                                                        , signif((wt_pot_BioCondInd*pot_BioCondInd_prot),3))
                      , wtd_pot_BioCxnInd = ifelse(is.na(pot_BioCxnInd)
                                                   , NA
                                                   , signif((as.numeric(wt_pot_BioCondInd)*as.numeric(pot_BioCxnInd)),3))
                      , wtd_pot_StressorInd = ifelse(is.na(pot_StressorInd)
                                                     , NA
                                            , signif((wt_pot_BioCondInd*pot_StressorInd),3))
                      , wtd_pot_StressorCxnInd = ifelse(is.na(pot_StressorCxnInd)
                                                        , NA
                                                        , signif((wt_pot_BioCondInd*pot_StressorCxnInd),3))
                      , wtd_thr_FireHazardInd = ifelse(is.na(thr_FireHazardInd)
                                                       , NA
                                                       , signif((wt_thr_FireHazardInd*thr_FireHazardInd),3))
                      , wtd_thr_PlannedDevelopInd = ifelse(is.na(thr_PlannedDevelopInd)
                                                           , NA
                                                           , signif((wt_thr_PlannedDevelopInd*thr_PlannedDevelopInd),3))
                      , wtd_thr_PlannedDevelopInd = ifelse(wtd_thr_PlannedDevelopInd < 0
                                                           , 0
                                                           , wtd_thr_PlannedDevelopInd)
                      , wtd_opp_RecrInd = ifelse(is.na(opp_RecrInd)
                                                 , NA
                                                 , signif((wt_opp_RecrInd*opp_RecrInd),3))
                      , wtd_opp_MSCPInd = ifelse(is.na(opp_MSCPInd)
                                                 , NA
                                                 , signif((wt_opp_MSCPInd*opp_MSCPInd),3))
                      , wtd_opp_NASVIInd = ifelse(is.na(opp_NASVIInd)
                                                  , NA
                                                  , signif((wt_opp_NASVIInd*opp_NASVIInd),3))
                      , wtd_opp_UserDefInd = ifelse(is.na(wt_opp_UserDefInd)
                                                    , NA
                                                    , signif((wt_opp_UserDefInd*opp_UserDefInd),3)))
    
    # Get denomiator (sum of weights for each sub index)
    # Erik, 20200929
    rowsums_wts_potentialinds <- rowSums(dfAllScores[, wts_potentialinds], na.rm = TRUE)
    rowsums_wts_threatinds    <- rowSums(dfAllScores[, wts_threatinds], na.rm = TRUE)
    rowsums_wts_oppinds       <- rowSums(dfAllScores[, wts_oppinds], na.rm = TRUE)
    # Get length of values for each row
    len_wts_potentialinds <- rowSums(!is.na(dfAllScores[, wts_potentialinds]), na.rm = TRUE)
    len_wts_threatinds    <- rowSums(!is.na(dfAllScores[, wts_threatinds]), na.rm = TRUE)
    len_wts_oppinds       <- rowSums(!is.na(dfAllScores[, wts_oppinds]), na.rm = TRUE)
    # Fix NA (so don't get divide by zero errors later)
    len_wts_potentialinds <- ifelse(len_wts_potentialinds == 0, NA, len_wts_potentialinds)
    len_wts_threatinds    <- ifelse(len_wts_threatinds == 0, NA, len_wts_threatinds)
    len_wts_oppinds       <- ifelse(len_wts_oppinds == 0, NA, len_wts_oppinds)
    
    
    # Normalize wtd by sum of wts
    # Erik, 20200929
    dfAllScores <- dfAllScores %>%
        dplyr::mutate(wtd_pot_BioCondInd_rest = ifelse(is.na(wtd_pot_BioCondInd_rest)
                                                         , NA
                                                         , signif((wtd_pot_BioCondInd_rest/rowsums_wts_potentialinds),3))
                      , wtd_pot_BioCondInd_prot = ifelse(is.na(wtd_pot_BioCondInd_prot)
                                                         , NA
                                                         , signif((wtd_pot_BioCondInd_prot/rowsums_wts_potentialinds),3))
                      , wtd_pot_BioCxnInd = ifelse(is.na(wtd_pot_BioCxnInd)
                                                   , NA
                                                   , signif((wtd_pot_BioCxnInd/rowsums_wts_potentialinds),3))
                      , wtd_pot_StressorInd = ifelse(is.na(wtd_pot_StressorInd)
                                                     , NA
                                                     , signif((wtd_pot_StressorInd/rowsums_wts_potentialinds),3))
                      , wtd_pot_StressorCxnInd = ifelse(is.na(wtd_pot_StressorCxnInd)
                                                        , NA
                                                        , signif((wtd_pot_StressorCxnInd/rowsums_wts_potentialinds),3))
                      , wtd_thr_FireHazardInd = ifelse(is.na(wtd_thr_FireHazardInd)
                                                       , NA
                                                       , signif((wtd_thr_FireHazardInd/rowsums_wts_threatinds),3))
                      , wtd_thr_PlannedDevelopInd = ifelse(is.na(wtd_thr_PlannedDevelopInd)
                                                           , NA
                                                           , signif((wtd_thr_PlannedDevelopInd/rowsums_wts_threatinds),3))
                      , wtd_opp_RecrInd = ifelse(is.na(wtd_opp_RecrInd)
                                                 , NA
                                                 , signif((wtd_opp_RecrInd/rowsums_wts_oppinds),3))
                      , wtd_opp_MSCPInd = ifelse(is.na(wtd_opp_MSCPInd)
                                                 , NA
                                                 , signif((wtd_opp_MSCPInd/rowsums_wts_oppinds),3))
                      , wtd_opp_NASVIInd = ifelse(is.na(wtd_opp_NASVIInd)
                                                  , NA
                                                  , signif((wtd_opp_NASVIInd/rowsums_wts_oppinds),3))
                      , wtd_opp_UserDefInd = ifelse(is.na(wtd_opp_UserDefInd)
                                                    , NA
                                                    , signif((wtd_opp_UserDefInd/rowsums_wts_oppinds),3))
        )## mutate ~ END
    
    
    
    # Calculate subindices, ensuring that NAs propagate properly
    dfAllScores$sub_PotentialSubIdx_rest <- ifelse(rowSums(is.na(dfAllScores[,indBioCond_rest]))==length(indBioCond_rest)
                                            , NA
                                            , signif(rowSums(dfAllScores[,indBioCond_rest], na.rm = TRUE)
                                                     /len_wts_potentialinds
                                                     , 3))
    
    dfAllScores$sub_PotentialSubIdx_prot <- ifelse(rowSums(is.na(dfAllScores[,indBioCond_prot]))==length(indBioCond_prot)
                                            , NA
                                            , signif(rowSums(dfAllScores[,indBioCond_prot], na.rm = TRUE)
                                                     /len_wts_potentialinds
                                                , 3))
    dfAllScores$sub_ThreatSubIdx <- ifelse(rowSums(is.na(dfAllScores[,indThreat]))==length(indThreat)
                                    , NA
                                    , 1-signif(rowSums(dfAllScores[,indThreat], na.rm = TRUE)
                                               /len_wts_threatinds
                                               , 3))
    dfAllScores$sub_OppSubIdx <- ifelse(rowSums(is.na(dfAllScores[,indOpp]))==length(indOpp)
                                , NA
                                , signif(rowSums(dfAllScores[, indOpp], na.rm = TRUE)
                                         /len_wts_oppinds
                                         , 3))
    
    dfAllScores <- dfAllScores %>%
        dplyr::mutate(wt_PotentialSubIdx = ifelse(is.na(sub_PotentialSubIdx_rest)
                                                  , NA
                                                  , listWeights$wt_subPot)
                      , wt_ThreatSubIdx = ifelse(is.na(sub_ThreatSubIdx)
                                                 , NA
                                                 , listWeights$wt_subThreat)
                      , wt_OppSubIdx = ifelse(is.na(sub_OppSubIdx)
                                              , NA
                                              , listWeights$wt_subOpp)
                      , wtd_sub_PotentialSubIdx_rest = ifelse(is.na(sub_PotentialSubIdx_rest)
                                                              , NA
                                                              , signif((wt_PotentialSubIdx*sub_PotentialSubIdx_rest),3))
                      , wtd_sub_PotentialSubIdx_prot = ifelse(is.na(sub_PotentialSubIdx_prot)
                                                              , NA
                                                              , signif((wt_PotentialSubIdx*sub_PotentialSubIdx_prot),3))
                      , wtd_sub_ThreatSubIdx = ifelse(is.na(sub_ThreatSubIdx)
                                                      , NA
                                                      , signif((wt_ThreatSubIdx*sub_ThreatSubIdx),3))
                      , wtd_sub_OppSubIdx = ifelse(is.na(sub_OppSubIdx)
                                                   , NA
                                                   , signif((wt_OppSubIdx*sub_OppSubIdx),3)))

    # Need to correct this to use weights in the denominator
    # Create "effective weight" where if score is NA, then weight is NA
    dfAllScores$idx_RPPIndex_Rest <- ifelse(rowSums(is.na(dfAllScores[,subsRPP_rest]))==length(subsRPP_rest)
                                            , NA
                                            , signif(rowSums(dfAllScores[,subsRPP_rest], na.rm = TRUE)
                                                     /rowSums(dfAllScores[,wts_subs], na.rm = TRUE)
                                                     ,3))
    dfAllScores$idx_RPPIndex_Prot <- ifelse(rowSums(is.na(dfAllScores[,subsRPP_prot]))==length(subsRPP_prot)
                                            , NA
                                            , signif(rowSums(dfAllScores[,subsRPP_prot], na.rm = TRUE)
                                                     /rowSums(dfAllScores[,wts_subs], na.rm = TRUE)
                                                     ,3))

    dfAllScores <- dplyr::select(dfAllScores, COMID
                                  , CSCIpred_qt05
                                  , CSCIpred_qt35
                                  , CSCIpred_qt50
                                  , CSCIpred_qt65
                                  , CSCIpred_qt95
                                  , BCGpred_qt05
                                  , BCGpred_qt50
                                  , BCGpred_qt95
                                  , CSCIobs
                                  , BCGobs
                                  , idx_RPPIndex_Rest
                                  , idx_RPPIndex_Prot
                                  , wt_PotentialSubIdx
                                  , sub_PotentialSubIdx_rest
                                  , sub_PotentialSubIdx_prot
                                  , wtd_sub_PotentialSubIdx_rest
                                  , wtd_sub_PotentialSubIdx_prot
                                  , wt_ThreatSubIdx
                                  , sub_ThreatSubIdx
                                  , wtd_sub_ThreatSubIdx
                                  , wt_OppSubIdx
                                  , sub_OppSubIdx
                                  , wtd_sub_OppSubIdx
                                  , wt_pot_BioCondInd
                                  , pot_BioCondInd_rest
                                  , pot_BioCondInd_prot
                                  , wtd_pot_BioCondInd_rest
                                  , wtd_pot_BioCondInd_prot
                                  , wt_pot_BioCxnInd
                                  , pot_BioCxnInd
                                  , wtd_pot_BioCxnInd
                                  , wt_pot_StressorInd
                                  , pot_StressorInd
                                  , wtd_pot_StressorInd
                                  , wt_pot_StressorCxnInd
                                  , pot_StressorCxnInd
                                  , wtd_pot_StressorCxnInd
                                  , wt_thr_FireHazardInd
                                  , thr_FireHazardInd
                                  , wtd_thr_FireHazardInd
                                  , thr_FireHazardIndComment
                                  , wt_thr_PlannedDevelopInd
                                  , thr_PlannedDevelopInd
                                  , wtd_thr_PlannedDevelopInd
                                  , thr_PlannedDevelopIndComment
                                  , wt_opp_RecrInd
                                  , opp_RecrInd
                                  , wtd_opp_RecrInd
                                  , opp_RecrIndComment
                                  , wt_opp_MSCPInd
                                  , opp_MSCPInd
                                  , wtd_opp_MSCPInd
                                  , opp_MSCPIndComment
                                  , wt_opp_NASVIInd
                                  , opp_NASVIInd
                                  , wtd_opp_NASVIInd
                                  , wt_opp_UserDefInd
                                  , opp_UserDefInd
                                  , wtd_opp_UserDefInd
                                  , HWBonus
                                  , BCGbonusRestore
                                  , BCGbonusProtect)
    
    
    write.table(dfAllScores, fn_allscoresFULL, append = FALSE
                , col.names = TRUE, row.names = FALSE, sep = "\t")
    
    rm(fn_allscores, fn_allscoresFULL, indBioCond_rest, indBioCond_prot
       , indThreat, indOpp, subsRPP_rest, subsRPP_prot, wts_potentialinds
       , wts_threatinds, wts_oppinds, wts_subs, listWeights)       
    
    # Generate "pretty" output file with only relevant scores
    # Columns needed, in this order:
    # COMID
    # StationID -- NOT INCLUDED
    # StreamName -- NOT INCLUDED
    # CSCI
    # Observed or Predicted
    # Index Type
    # RPPIndex
    # Rank (by type)
    # Potential Subindex
    # Biological Condition Indicator
    # Biological Connectivity Indicator
    # Stressor Indicator
    # Stressor Connectivity Indicator
    # Threat Subindex
    # Fire Hazard Indicator
    # Planned Development Indicator
    # Opportunity Subindex
    # Recreation Indicator
    # MSCP Indicator
    # NASVI Indicator
    # User-applied Indicator
    
    # Vectors of colnames to use
    restColNames <- c("idx_RPPIndex_Rest"
                      , "wtd_sub_PotentialSubIdx_rest"
                      , "wtd_pot_BioCondInd_rest")
    protColNames <- c("idx_RPPIndex_Prot"
                      , "wtd_sub_PotentialSubIdx_prot"
                      , "wtd_pot_BioCondInd_prot")
    otherColNames <- c("wtd_pot_BioCxnInd"
                       , "wtd_pot_StressorInd"
                       , "wtd_pot_StressorCxnInd"
                       , "wtd_sub_ThreatSubIdx"
                       , "wtd_thr_FireHazardInd"
                       , "wtd_thr_PlannedDevelopInd"
                       , "wtd_sub_OppSubIdx"
                       , "wtd_opp_RecrInd"
                       , "wtd_opp_MSCPInd"
                       , "wtd_opp_NASVIInd"
                       , "wtd_opp_UserDefInd")
    
    # Add summary columns and select the core columns
    dfAllScoresSummary <- dfAllScores %>%
        dplyr::mutate(CSCI = ifelse(!is.na(CSCIobs)
                                    , CSCIobs
                                    , ifelse(!is.na(CSCIpred_qt50)
                                             , CSCIpred_qt50
                                             , NA))
                      , BCGTier = ifelse(!is.na(BCGobs)
                                         , BCGobs
                                         , ifelse(!is.na(BCGpred_qt50)
                                                , BCGpred_qt50
                                                , NA))
                      , BioType = ifelse(!is.na(CSCIobs)
                                          , "Observed"
                                          , ifelse(!is.na(CSCIpred_qt50)
                                                   , "Predicted"
                                                   , NA))
                      , Quality = ifelse(!is.na(CSCI)
                                         , cut(CSCI, breaks=BioDegBrk
                                               , labels=BioDegLab)
                                         , NA)
                      , IndexType = ifelse(Quality==1
                                           , "Restore"
                                           , ifelse(Quality==2
                                                    , "Protect"
                                                    , NA))) %>%
        dplyr::select(COMID, CSCI, BCGTier, BioType, IndexType)
    
    # Filter for just "protect" streams
    dfAllScoresSummaryProtect <- dfAllScoresSummary %>%
        dplyr::filter(IndexType=="Protect")
    dfAllScoresSummaryProtect <- merge(dfAllScoresSummaryProtect
                                        , dfAllScores[, c("COMID", protColNames, otherColNames)]
                                        , by.x="COMID"
                                        , by.y="COMID")
    
    # Anonymize columns and obtain rank (via row number, ties rank according to occurrence)
    dfAllScoresSummaryProtect <- dfAllScoresSummaryProtect %>%
        dplyr::rename(RPPIndex = idx_RPPIndex_Prot
                      , PotSubindex = wtd_sub_PotentialSubIdx_prot
                      , BioCondnInd = wtd_pot_BioCondInd_prot
                      , BioCxnInd = wtd_pot_BioCxnInd
                      , StressorInd = wtd_pot_StressorInd
                      , StressorCxnInd = wtd_pot_StressorCxnInd
                      , ThreatSubindex = wtd_sub_ThreatSubIdx
                      , FireHazardInd = wtd_thr_FireHazardInd
                      , PlannedDevInd = wtd_thr_PlannedDevelopInd
                      , OppSubindex = wtd_sub_OppSubIdx
                      , RecreationInd = wtd_opp_RecrInd
                      , MSCPInd = wtd_opp_MSCPInd
                      , NASVIInd = wtd_opp_NASVIInd
                      , UserAppliedInd = wtd_opp_UserDefInd) %>%
        dplyr::mutate(RankByIndexType=dplyr::row_number(desc(RPPIndex)))

    # Filter for just "restore" streams
    dfAllScoresSummaryRestore <- dfAllScoresSummary %>%
        dplyr::filter(IndexType=="Restore")
    dfAllScoresSummaryRestore <- merge(dfAllScoresSummaryRestore
                        , dfAllScores[, c("COMID", restColNames, otherColNames)]
                        , by.x="COMID", by.y="COMID")

    # Anonymize columns and obtain rank (via row number, ties rank according to occurrence)
    dfAllScoresSummaryRestore <- dfAllScoresSummaryRestore %>%
        dplyr::rename(RPPIndex = idx_RPPIndex_Rest
                      , PotSubindex = wtd_sub_PotentialSubIdx_rest
                      , BioCondnInd = wtd_pot_BioCondInd_rest
                      , BioCxnInd = wtd_pot_BioCxnInd
                      , StressorInd = wtd_pot_StressorInd
                      , StressorCxnInd = wtd_pot_StressorCxnInd
                      , ThreatSubindex = wtd_sub_ThreatSubIdx
                      , FireHazardInd = wtd_thr_FireHazardInd
                      , PlannedDevInd = wtd_thr_PlannedDevelopInd
                      , OppSubindex = wtd_sub_OppSubIdx
                      , RecreationInd = wtd_opp_RecrInd
                      , MSCPInd = wtd_opp_MSCPInd
                      , NASVIInd = wtd_opp_NASVIInd
                      , UserAppliedInd = wtd_opp_UserDefInd) %>%
        dplyr::mutate(RankByIndexType=dplyr::row_number(desc(RPPIndex)))
    
    dfAllScoresSummary <- rbind(dfAllScoresSummaryRestore 
                                , dfAllScoresSummaryProtect)
    dfAllScoresSummary <- dplyr::select(dfAllScoresSummary
                                        , COMID
                                        , CSCI
                                        , BCGTier
                                        , BioType
                                        , IndexType
                                        , RPPIndex
                                        , RankByIndexType
                                        , RPPIndex
                                        , PotSubindex
                                        , BioCondnInd
                                        , BioCxnInd
                                        , StressorInd
                                        , StressorCxnInd
                                        , ThreatSubindex
                                        , FireHazardInd
                                        , PlannedDevInd
                                        , OppSubindex
                                        , RecreationInd
                                        , MSCPInd
                                        , NASVIInd
                                        , UserAppliedInd)
    
    write.table(dfAllScoresSummary, fn_allscoresSUMMARY, append = FALSE
                , col.names = TRUE, row.names = FALSE, sep = "\t")
    
    rm(dfAllScoresSummaryRestore, dfAllScoresSummaryProtect, protColNames
       , restColNames, otherColNames, BioDegBrk, BioDegLab)
    
    myScores <- list(dfAllScores=dfAllScores, dfAllScoresSummary=dfAllScoresSummary)

    return(myScores)
    
}