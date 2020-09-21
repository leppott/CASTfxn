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
# NOTE: This takes ONLY the most recent CSCI score/BCG tier for the target reach.
# If there is more than one site on the reach, only the most recent data are used.

# For reaches with sites
# Question 1: Does observed fall within predicted distribution?
#    If so, is it < q35, between q35 and q65, or > q65?
#    If <qt35, then rscore = +2 (can go up 2 levels); pscore = 0
#    If >qt65, then rscore = 0; pscore = 2
#    Else, rscore = 1; pscore = 1

# Question 2: Does improving biology mean delta BCG tier?
#    For restoration (rscore)
#    If qt95 is in the same BCG tier as observed, add 0
#    If qt95 is in a higher BCG tier than observed, add delta BCG tiers
#    For Protection (pscore)
#    If qt05 is in the same BCG tier as observed, add 0
#    If qt05 is in a lower BCG tier than observed, add delta BCG tiers

# For reaches without sites
# Question 3: We've assigned BCG level as qt50 of the predicted distribution
#    For purposes of scoring, should we assume that the reach is at qt50 
#    (most likely) or at qt05 or qt95 (least likely)?
#    If qt50->qt95 increases BCG tier, then rscore = delta BCG tiers
#    If qt50->qt05 decreases BCG tier, then pscore = delta BCG tiers

getBCGScores <- function(dfBCGcutoffs, dfreachBCGobs, dfreachBCGpred, dfHWflag
                         , HWBonus, BCGBonus, minYear, maxYear) { # FUNCTION.START
    
    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`
    
    if (boo_DEBUG==TRUE) {
        dfBCGcutoffs <- listBCGdata$BCGcutoff
        dfreachBCGobs <- listBCGdata$obsReachBCG
        dfreachBCGpred <- listBCGdata$predReachBCG
        dfHWflag <- dfNetwork[, c("COMID", "StartFlag")]
        HWBonus <- 1
        BCGBonus <- 1
        minYear <- 2008
        maxYear <- 2020
    }
    
    BCGlevels <- as.vector(dfBCGcutoffs[,"BCGLevel"])
    maxBCG <- max(BCGlevels)

    # Get most recent BCG tier
    dfreachBCGobs <- dfreachBCGobs %>%
        dplyr::group_by(COMID) %>%
        dplyr::filter(BMISampleDate==max(BMISampleDate)) %>%
        dplyr::mutate(Year=lubridate::year(BMISampleDate)) %>%
        dplyr::filter(Year>=minYear, Year<=maxYear) %>%
        dplyr::select(COMID, BMISampleDate, CSCI, BCGLevel)
    
    dfCriticalQTs <- dfreachBCGpred[,c("COMID","qt05","qt35","qt50","qt65"
                                       ,"qt95","BCGqt05","BCGqt50","BCGqt95")]
    dfCriticalQTs <- merge(dfCriticalQTs, dfreachBCGobs, by.x = "COMID"
                           , by.y = "COMID", all.x = TRUE)
    dfCriticalQTs <- merge(dfCriticalQTs, dfHWflag, by.x = "COMID"
                           , by.y = "COMID")
    
    # Scoring algorithm
    dfCriticalQTs <- dfCriticalQTs %>%
        dplyr::mutate(HWBonus = ifelse(StartFlag==1,HWBonus,0)
                      , rscore_BCGbonus = ifelse(is.na(BCGLevel), 0
                                                 , ifelse(BCGqt05>BCGLevel, 0
                                                          , ifelse(4>=BCGqt05, 1
                                                                   , 0)))
                      , pscore_BCGbonus = ifelse(is.na(BCGLevel), 0
                                                 , ifelse(BCGqt95<BCGLevel, 0
                                                          , ifelse(2<=BCGqt95, 1
                                                                   , 0)))) %>%
        dplyr::mutate(rscore_BCGbonus = ifelse(BCGBonus==0, 0, rscore_BCGbonus)
                      , pscore_BCGbonus = ifelse(BCGBonus==0, 0, pscore_BCGbonus)
                      , rscore_pred = BCGqt50 - BCGqt95
                      , pscore_pred = BCGqt05 - BCGqt50
                      , rscore_obsBCG = ifelse(BCGLevel==BCGqt95, 0
                                               , BCGLevel - BCGqt95)
                      , pscore_obsBCG = ifelse(BCGLevel==BCGqt05, 0
                                               , BCGqt05 - BCGLevel)
                      , rscore_obsCSCI = ifelse(CSCI<qt35, 2
                                                , ifelse(CSCI>qt65,0,1))
                      , pscore_obsCSCI = ifelse(CSCI<qt35, 0
                                                , ifelse(CSCI>qt65,0,2))
                      , rscore_obsBOTH = ifelse(is.na(BCGLevel), NA
                                                , ifelse(rscore_obsBCG+rscore_obsCSCI<0
                                                         , 0, rscore_obsBCG + rscore_obsCSCI))
                      , pscore_obsBOTH = ifelse(is.na(BCGLevel), NA
                                                , ifelse(pscore_obsBCG+pscore_obsCSCI<0
                                                         , 0, pscore_obsBCG + pscore_obsCSCI))
                      , rscore_final = ifelse(is.na(rscore_obsBOTH)
                            , (rscore_pred+HWBonus+rscore_BCGbonus)/(maxBCG+max(HWBonus,na.rm=TRUE)+max(rscore_BCGbonus,na.rm=TRUE))
                            , (rscore_obsBOTH+HWBonus+rscore_BCGbonus)/(maxBCG+max(HWBonus,na.rm=TRUE)+max(rscore_BCGbonus,na.rm=TRUE)))
                      , pscore_final = ifelse(is.na(pscore_obsBOTH)
                            , (pscore_pred+HWBonus+pscore_BCGbonus)/(maxBCG+max(HWBonus,na.rm=TRUE)+max(pscore_BCGbonus,na.rm=TRUE))
                            , (pscore_obsBOTH + HWBonus + pscore_BCGbonus)/(maxBCG+max(HWBonus,na.rm=TRUE)+max(pscore_BCGbonus,na.rm=TRUE))))
    
    dfCriticalQTs <- dfCriticalQTs %>%
        dplyr::select(COMID, qt05, qt35, qt50, qt65, qt95, BCGqt05, BCGqt50, BCGqt95
                      , BMISampleDate, CSCI, BCGLevel, rscore_obsCSCI, pscore_obsCSCI
                      , rscore_obsBCG, pscore_obsBCG, rscore_obsBOTH, pscore_obsBOTH
                      , HWBonus, rscore_BCGbonus, pscore_BCGbonus, rscore_final
                      , pscore_final) %>%
        dplyr::rename(CSCIpred_qt05 = qt05
                      , CSCIpred_qt35 = qt35
                      , CSCIpred_qt50 = qt50
                      , CSCIpred_qt65 = qt65
                      , CSCIpred_qt95 = qt95
                      , BCGpred_qt05 = BCGqt05
                      , BCGpred_qt50 = BCGqt50
                      , BCGpred_qt95 = BCGqt95
                      , CSCIobs = CSCI
                      , BCGobs = BCGLevel
                      , pot_BioCondInd_rest = rscore_final
                      , pot_BioCondInd_prot = pscore_final
                      , BCGbonusRestore = rscore_BCGbonus
                      , BCGbonusProtect = pscore_BCGbonus
                      , pot_BioCondInd_rest = rscore_final
                      , pot_BioCondInd_prot = pscore_final)
    
    return(dfBCGscores = dfCriticalQTs)
    
}