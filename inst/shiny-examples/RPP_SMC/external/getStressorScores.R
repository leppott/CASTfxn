# getStressorScores (Specific for SMC)
# Ann.RoseberryLincoln@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 
# NOTE: This takes ONLY analytes (including modeled data) that are supported
# by the weight of evidence as likely causes of benthic macroinvertebrate
# impairment at any stream in the dataset, but ONLY FOR SITES FOR WHICH CASTOOL
# RESULTS ARE AVAILABLE! In other words, any stressor, if it is a supported
# cause of impairment in any sample from any site in the data set is included
# as output from this function. If a stressor is never run through the CASTool
# or is never supported as a cause of impairment, it won't show up. Data are 
# reduced to one stressor value (most recent) per COMID for measured data.
# Modeled data include one stressor per site, even if more than one site is on 
# the target COMID.

getStressorScores <- function(dfSites, dfAllStressVals, fnWeights) { # FUNCTION.START
    
    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`

    if (boo_DEBUG==TRUE) {
        dfSites <- dfSites
        dfAllStressVals <- listScaledStr01All$df_allSMCStressVals
        fnWeights <- fn_stresswtsIN
    }
    
    dfSites <- dfSites[, c("StationID_Master", "COMID")]
    df_allStressVals2 <- merge(dfAllStressVals, dfSites
                              , by.x = "StationID_Master"
                              , by.y = "StationID_Master"
                              , all.x = TRUE)
    
    dfStressValYear <- df_allStressVals2 %>%
        dplyr::mutate(Year=lubridate::year(StressSampleDate))
    dfStressValNOYear <- dfStressValYear[is.na(dfStressValYear$Year),]

    dfStressValYearFiltered <- dfStressValYear %>%
        dplyr::filter(Year>=minYear, Year<=maxYear) %>%
        dplyr::mutate(StationID_Master=as.character(StationID_Master)) %>%
        dplyr::group_by(COMID, Stressor) %>%
        dplyr::filter(StressSampleDate==max(StressSampleDate))
    
    # rbind values with dates and modeled data without dates, using the 
    # reduced modeled data (one value per COMID)
    dfStressValRecent <- rbind(as.data.frame(dfStressValYearFiltered)
                               , as.data.frame(dfStressValNOYear)) %>%
        dplyr::select(StationID_Master, COMID, StressSampID, StressSampleDate
                       , Year, Stressor, AdjStressorValue, NumSamps)

    dfWeights <- read.delim(fnWeights, header=TRUE, stringsAsFactors=FALSE, sep="\t")
    dfStrValRecentWts<- merge(dfStressValRecent, dfWeights, by.x = "Stressor"
                              , by.y = "Stressor", all.y = TRUE)
    
    # Final data table for use on maps, etc.
    dfStrValRecentWts <- dfStrValRecentWts %>%
        dplyr::mutate(WtAdjVal = (AdjStressorValue * Weight)) %>%
        dplyr::select(COMID, StationID_Master, StressSampID, StressSampleDate
                      , Year, StressorGroup, Stressor, Label, AdjStressorValue
                      , NumSamps, Weight, WtAdjVal) %>%
        dplyr::arrange(COMID, StationID_Master, StressSampleDate, StressorGroup
                       , Stressor)

    # Final score calculations
    dfStrScores <- dfStrValRecentWts %>%
        dplyr::filter(Weight!=0) %>%
        dplyr::select(COMID, Weight, WtAdjVal) %>%
        dplyr::filter(!is.na(WtAdjVal)) %>%
        dplyr::group_by(COMID) %>%
        dplyr::summarize(sumWeights = sum(Weight)
                         , StressorScore = 1-sum(WtAdjVal)/(sumWeights)
                         , .groups = "drop_last")
    
    myStressorScores <- list(dfStrScores = dfStrScores, dfWeights = dfWeights
                             , dfWtNormStressRecent = dfStrValRecentWts)
    
    return(myStressorScores)
    
}