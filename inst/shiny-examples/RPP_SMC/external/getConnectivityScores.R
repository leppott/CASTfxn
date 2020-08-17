# getConnectivityScores (Specific for SMC)
# Ann.RoseberryLincoln@tetratech.com
# R v3.5.1
# 
# NOTE: This scores BCG potential for improvement based on higher BCG tiers 
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
    
    dfWtNormStressRecent=listStressData$dfWtNormStressRecent
    dfWeights=listStressData$dfWeights
    
    # Identify stressor score for stressors upstream
    if (useStressor==TRUE) {
        # Idenfity stressors on Target Reach & get these stressors for all reaches
        dfUseStressors <- listStressScores$dfWtNormStressRecent %>%
            dplyr::filter(COMID == TargetCOMID) %>%
            dplyr::select(COMID, Stressor, Weight, WtAdjVal)

        # Get sum of weights for all stressors id'd at the target reach
        sumWeights <- dfUseStressors %>%
            dplyr::summarise(sumWeights = sum(Weight, na.rm = TRUE))
        sumWeights <- as.numeric(sumWeights)
        
        # Combine dfCxns with stressor data, then subset to target reach stressors
        dfCxnStressors <- listStressScores$dfWtNormStressRecent %>%
            dplyr::filter(COMID %in% dfCxnData$COMID) %>%
            dplyr::filter(Stressor %in% dfUseStressors$Stressor)
        dfCxnStressors <- dfCxnStressors %>%
            dplyr::select(COMID,WtAdjVal) %>%
            dplyr::group_by(COMID) %>%
            dplyr::summarise(SumWtAdjVal = sum(WtAdjVal, na.rm = TRUE)) %>%
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
            dplyr::summarise(TotalScore = 1-sum(ReachWtdStressor, na.rm = TRUE))
        
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
    dfCxnBCG <- merge(dfCxnData, dfBCGData[,c("COMID", "BMISampleDate", "CSCI"
                                              , "BCGLevel", "BCGqt50")],
                       by.x = "COMID", by.y = "COMID", all.x = TRUE)

    TargetBCGobs <- as.numeric(dfCxnBCG$BCGLevel[dfCxnBCG$COMID==TargetCOMID])
    TargetBCGpred <- as.numeric(dfCxnBCG$BCGqt50[dfCxnBCG$COMID==TargetCOMID])
    deltaBCG <- max(dfBCGData$BCGLevel, na.rm=TRUE) - min(dfBCGData$BCGLevel
                                                          , na.rm=TRUE)
    dfCxnBCG <- dfCxnBCG %>% dplyr::filter(UpDown!="Origin")
    if (!is.na(TargetBCGobs)) { # Target reach has observed BCG
        dfCxnBCG <- dfCxnBCG %>%
            dplyr::mutate(BCGobsPLUS = ifelse(!is.na(BCGLevel) # Obs reach BCG
                                              , ifelse(BCGLevel>TargetBCGobs
                                                       , BCGLevel - TargetBCGobs
                                                       , 0)
                                              , ifelse(!is.na(BCGqt50) # Predicted reach BCG
                                                       , ifelse(BCGqt50>TargetBCGobs
                                                                , BCGqt50 - TargetBCGobs
                                                                , 0)
                                                       , NA)))
    } else { # Target reach only has predicted BCG
        dfCxnBCG$BCGobsPLUS = NA
    }
    
    if (!is.na(TargetBCGpred)) { # TargetBCG is predicted
        dfCxnBCG <- dfCxnBCG %>%
            dplyr::mutate(BCGpredPLUS = ifelse(!is.na(BCGLevel) # Observed reach BCG
                                               , ifelse(BCGLevel>TargetBCGpred
                                                        , BCGLevel - TargetBCGpred
                                                        , 0)
                                               , ifelse(!is.na(BCGqt50) # Predicted reach BCG
                                                        , ifelse(BCGqt50>TargetBCGpred
                                                                 , BCGqt50 - TargetBCGpred
                                                                 , 0)
                                                        , NA)))
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
                      , BMISampleDate, BCGLevel, TargetBCGobs, BCGobsPLUS
                      , BCGqt50, TargetBCGpred, BCGpredPLUS, ReachBCGScore
                      , ReachWtBCGScore)
    
    # Get total BCGCxnScore for TargetCOMID
    if (useDownStream==FALSE) {
        BCGCxnScore <- dfCxnBCG %>%
            dplyr::select(UpDown, ReachWtBCGScore) %>%
            dplyr::filter(UpDown=="Up") %>%
            dplyr::filter(UpDown!="Origin") %>%
            dplyr::summarise(TotalScore=sum(ReachWtBCGScore,na.rm=TRUE)/deltaBCG)
        BCGCxnScore <- as.numeric(BCGCxnScore)
    } else {
        BCGCxnScore <- dfCxnBCG %>%
            dplyr::select(UpDown, ReachWtBCGScore) %>%
            dplyr::filter(UpDown!="Origin") %>%
            dplyr::summarise(TotalScore=sum(ReachWtBCGScore,na.rm=TRUE)/deltaBCG)
        BCGCxnScore <- as.numeric(BCGCxnScore)
    }

    dfScores <- as.data.frame(cbind(TargetCOMID, BCGCxnScore, StressCxnScore))
    colnames(dfScores) <- c("COMID", "BCGCxnScore", "StressCxnScore")
    write.table(dfCxnBCG
                ,file.path(comid_dir,paste0(TargetCOMID,"_CxnBCGScores.tab"))
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
    write.table(dfCxnStressors
                ,file.path(comid_dir,paste0(TargetCOMID,"_CxnStressorScores.tab"))
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
    
    myCxnScores = list(dfConnectivityScores = dfScores, dfCxnBCG = dfCxnBCG
                       , dfCxnStressors = dfCxnStressors)
    
    return(myCxnScores)

}