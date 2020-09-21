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

getStressorScores <- function(dfSites, dfAllStressVals, fnWeights, maxYear, minYear) { # FUNCTION.START
    
    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`

    if (boo_DEBUG==TRUE) {
        dfSites <- dfSites
        dfAllStressVals <- listScaledStr01All$df_allSMCStressVals
        fnWeights <- fn_stresswtsIN
        maxYear <- lubridate::year(Sys.Date()) # Obtained from user (NOTE: this is inclusive)
        minYear <- maxYear - 12 # Inclusive (defaults to 2008 to present on 4/17/2020)
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
        dplyr::summarize(sumStressWts = sum(Weight, na.rm = TRUE)
                         , pot_StressorInd = 1-sum(WtAdjVal)/(sumStressWts)
                         , .groups="drop_last")
    
    myStressorScores <- list(dfStrScores = dfStrScores, dfWeights = dfWeights
                             , dfWtNormStressRecent = dfStrValRecentWts)
    
    return(myStressorScores)
    
}