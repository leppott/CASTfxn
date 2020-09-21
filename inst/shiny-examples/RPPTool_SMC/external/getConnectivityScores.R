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

# 
# NOTE: This functionscores BCG potential for improvement based on higher BCG tiers 
# upstream and (optionally) downstream. It also evaluates presence of similar 
# stressors upstream (if desired). If stressor data are used for the reach 
# itself (stressor indicator), then stressors are used in the connectivity 
# indicator by default with the same weighted, normalized values.

# BCG Connectivity = +1 for each tier increased in a connected reach, multiplied
# by the fraction of total stream length contributed, summed over all "better"
# reaches. Bonus is given (if user chooses) for headwaters or for reaches
# for which observed BCG > predicted BCG (50th percentile)
# Upstream is included by default; Downstream is included per user choice

# Stressor Connectivity = normalized weighted average of target reach stressors
# observed at upstream connected reaches, summed over stressors and multiplied
# by the fraction of stream length.

getConnectivityScores <- function(TargetCOMID, useStressor=FALSE
                                  , useDownStream=TRUE, dfCxnData
                                  , dfBCGData, listStressData
                                  , results_dir) {
    
    boo_DEBUG <- FALSE
    
    if (boo_DEBUG==TRUE) {
        TargetCOMID=reach
        useStressor=TRUE
        useDownStream=TRUE
        dfCxnData=dfCxns
        dfBCGData = dfBCGscores
        listStressData=listStressScores
        results_dir=results_dir
    }
    
    # create subdirectories corresponding to 2nd grouping level
    ifelse(!dir.exists(file.path(results_dir,TargetCOMID))==TRUE
           , dir.create(file.path(results_dir,TargetCOMID))
           , FALSE)
    comid_dir <-file.path(results_dir, TargetCOMID)
    
    
    # Identify stressor score for stressors upstream
    if (useStressor==TRUE) {
        # Idenfity stressors on Target Reach & get these stressors for all reaches
        dfUseStressors <- listStressData$dfWtNormStressRecent %>%
            dplyr::filter(COMID == TargetCOMID) %>%
            dplyr::select(COMID, Stressor, Weight, WtAdjVal)

        # Get sum of weights for all stressors id'd at the target reach
        sumWeights <- dfUseStressors %>%
            dplyr::summarise(sumWeights = sum(Weight, na.rm = TRUE)
                             , .groups="drop_last")
        sumWeights <- as.numeric(sumWeights)
        
        # Combine dfCxns with stressor data, then subset to target reach stressors
        dfCxnStressors <- listStressData$dfWtNormStressRecent %>%
            dplyr::filter(COMID %in% dfCxnData$COMID) %>%
            dplyr::filter(Stressor %in% dfUseStressors$Stressor)
        dfCxnStressors <- dfCxnStressors %>%
            dplyr::select(COMID,WtAdjVal) %>%
            dplyr::group_by(COMID) %>%
            dplyr::summarise(SumWtAdjVal = sum(WtAdjVal, na.rm = TRUE)
                             , .groups="drop_last") %>%
            dplyr::mutate(sumWeights = eval(sumWeights))
        
        dfCxnStressors <- merge(dfCxnData, dfCxnStressors, all.x = TRUE)
        dfCxnStressors <- dfCxnStressors %>%
            dplyr::filter(UpDown=="Up") %>%
            dplyr::mutate(ReachWtdStressor = SumWtAdjVal*FractionLength/sumWeights
                          , TargetCOMID = TargetCOMID) %>%
            dplyr::select(TargetCOMID, COMID, FTYPE, FromNode, ToNode, LENGTHKM
                          , StartFlag, AggLengthKM, UpDown, TotalLength
                          , FractionLength, SumWtAdjVal, sumWeights
                          , ReachWtdStressor)
        
        # y = (pmin + ((x-xmin)*(pmax-pmin))/(xmax-xmin))
        # Get total StressCxnScore for TargetCOMID
        StressCxnScore <- dfCxnStressors %>%
            dplyr::select(ReachWtdStressor) %>%
            dplyr::summarise(TotalScore = 1-sum(ReachWtdStressor, na.rm = TRUE)
                             , .groups="drop_last")
        
        StressCxnScore <- as.numeric(StressCxnScore)
        
    } else { 
        StressCxnScore <- NA
        dfCxnStressors <- dfCxnData %>%
            dplyr::select(COMID, FTYPE, FromNode, ToNode, LENGTHKM
                          , StartFlag, AggLengthKM, UpDown, TotalLength
                          , FractionLength) %>%
            dplyr::mutate(TargetCOMID = TargetCOMID, SumWtAdjVal = NA
                          , sumWeights = 0, ReachWtdStressor = NA) %>%
            dplyr::select(TargetCOMID, COMID, FTYPE, FromNode, ToNode, LENGTHKM
                          , StartFlag, AggLengthKM, UpDown, TotalLength
                          , FractionLength, SumWtAdjVal, sumWeights
                          , ReachWtdStressor)
    } # No stressor data
    
    # Identify BCG score for upstream and downstream (if useDS == TRUE)
    dfCxnBCG <- merge(dfCxnData, dfBCGData[,c("COMID", "BMISampleDate", "CSCIobs"
                                              , "BCGobs", "BCGpred_qt50")],
                       by.x = "COMID", by.y = "COMID", all.x = TRUE)

    TargetBCGobs <- as.numeric(dfCxnBCG$BCGobs[dfCxnBCG$COMID==TargetCOMID])
    TargetBCGpred <- as.numeric(dfCxnBCG$BCGpred_qt50[dfCxnBCG$COMID==TargetCOMID])
    deltaBCG <- max(dfBCGData$BCGobs, na.rm=TRUE) - min(dfBCGData$BCGobs
                                                          , na.rm=TRUE)
    dfCxnBCG <- dfCxnBCG %>% dplyr::filter(UpDown!="Origin")
    if (!is.na(TargetBCGobs)) { # Target reach has observed BCG
        dfCxnBCG <- dfCxnBCG %>%
            dplyr::mutate(BCGobsPLUS = ifelse(!is.na(BCGobs) # Connected reach BCG is NA
                                              , ifelse(BCGobs<TargetBCGobs # Connected reach is better than target
                                                       , TargetBCGobs - BCGobs
                                                       , 0)
                                              , ifelse(is.na(BCGpred_qt50) # Predicted reach BCG is NA
                                                       , NA
                                                       , ifelse(BCGpred_qt50>TargetBCGobs
                                                                , BCGpred_qt50 - TargetBCGobs
                                                                , 0))))
    } else { # Target reach only has predicted BCG
        dfCxnBCG$BCGobsPLUS = NA
    }
    
    if (!is.na(TargetBCGpred)) { # TargetBCG is predicted
        dfCxnBCG <- dfCxnBCG %>%
            dplyr::mutate(BCGpredPLUS = ifelse(!is.na(BCGobs) # Observed reach BCG is NA
                                               , ifelse(BCGobs<TargetBCGpred
                                                        , TargetBCGpred - BCGobs 
                                                        , 0)
                                               , ifelse(is.na(BCGpred_qt50) # Predicted reach BCG is NA
                                                        , NA
                                                        , ifelse(BCGpred_qt50>TargetBCGpred
                                                                 , BCGpred_qt50 - TargetBCGpred
                                                                 , 0))))
    } else {
        dfCxnBCG$BCGpredPLUS = NA
    }
    
    dfCxnBCG <- dfCxnBCG %>%
        dplyr::mutate(ReachBCGScore = ifelse(!is.na(BCGobsPLUS), BCGobsPLUS
                                             , ifelse(!is.na(BCGpredPLUS)
                                                      , BCGpredPLUS, NA))
                      , ReachWtBCGScore = ifelse(!is.na(ReachBCGScore)
                                                 , ReachBCGScore*FractionLength
                                                 , NA)
                      , TargetCOMID = TargetCOMID
                      , TargetBCGobs = TargetBCGobs
                      , TargetBCGpred = TargetBCGpred) %>%
        dplyr::select(TargetCOMID, COMID, FTYPE, FromNode, ToNode, LENGTHKM
                      , StartFlag, AggLengthKM, UpDown, TotalLength, FractionLength
                      , BMISampleDate, BCGobs, TargetBCGobs, BCGobsPLUS
                      , BCGpred_qt50, TargetBCGpred, BCGpredPLUS, ReachBCGScore
                      , ReachWtBCGScore)
    
    # Get total BCGCxnScore for TargetCOMID
    if (useDownStream==FALSE) {
        BCGCxnScore <- dfCxnBCG %>%
            dplyr::select(UpDown, ReachWtBCGScore) %>%
            dplyr::filter(UpDown=="Up") %>%
            dplyr::filter(UpDown!="Origin") %>%
            dplyr::summarise(TotalScore=sum(ReachWtBCGScore,na.rm=TRUE)/deltaBCG
                             , .groups="drop_last")
        BCGCxnScore <- as.numeric(BCGCxnScore)
    } else {
        BCGCxnScore <- dfCxnBCG %>%
            dplyr::select(UpDown, ReachWtBCGScore) %>%
            dplyr::filter(UpDown!="Origin") %>%
            dplyr::summarise(TotalScore=sum(ReachWtBCGScore,na.rm=TRUE)/deltaBCG
                             , .groups="drop_last")
        BCGCxnScore <- as.numeric(BCGCxnScore)
    }

    dfScores <- as.data.frame(cbind(TargetCOMID, BCGCxnScore, StressCxnScore))
    colnames(dfScores) <- c("COMID", "pot_BioCxnInd", "pot_StressorCxnInd")
    write.table(dfCxnBCG
                ,file.path(comid_dir,paste0(TargetCOMID,"_CxnBioScores.tab"))
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
    write.table(dfCxnStressors
                ,file.path(comid_dir,paste0(TargetCOMID,"_CxnStressorScores.tab"))
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
    
    myCxnScores = list(dfConnectivityScores = dfScores, dfCxnBCG = dfCxnBCG
                       , dfCxnStressors = dfCxnStressors)
    
    return(myCxnScores)

}