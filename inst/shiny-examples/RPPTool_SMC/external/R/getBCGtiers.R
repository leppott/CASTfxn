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

getBCGtiers <- function(fn_Index2BCG, fn_predIndexByReach, fn_obsIndexBySite
                        , dfSites) {
    
    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`
    
    if (boo_DEBUG==TRUE) {
        fn_Index2BCG
        fn_predIndexByReach
        fn_obsIndexBySite
        dfSites = dfSites
    }
    
    # Get data mapping CSCI score to BCG level based on max probability
    dfCSCI2BCG <- readxl::read_excel(fn_Index2BCG, skip=1, na="NA", trim_ws=TRUE)
    dfCSCI2BCG <- dfCSCI2BCG %>%
        dplyr::select(CSCI, BCGLevel) %>%
        dplyr::mutate(CSCI = round(CSCI,2))
    
    breakpoints <- dfCSCI2BCG %>%
        dplyr::group_by(BCGLevel) %>%
        dplyr::summarize(minCSCI = min(CSCI, na.rm = TRUE)
                         , maxCSCI = max(CSCI, na.rm = TRUE)
                         , .groups="drop_last") %>%
        dplyr::mutate(cutoff=0)
    
    for (i in 1:max(breakpoints$BCGLevel)) {
        if (i==1) {
            breakpoints$cutoff[i] = 2 * breakpoints$maxCSCI[i]
            minval = breakpoints$minCSCI[i]
        } else if (i==6) {
            breakpoints$cutoff[i] = 0
        } else {
            maxval = breakpoints$maxCSCI[i]
            breakpoints$cutoff[i] = (maxval + minval)/2
        }
    }
    
    # Get predicted CSCI scores ####
    # for all COMIDs for which predictions are available
    dfpredCSCI <- readxl::read_excel(fn_predIndexByReach, skip=0, na="-999999", trim_ws=TRUE)
    quantiles <- stringr::str_extract_all(colnames(dfpredCSCI), "^qt\\d{2}$"
                                          , simplify=TRUE)
    quantiles <- quantiles[!(quantiles=="")]
    dfpredCSCI <- dfpredCSCI %>%
        dplyr::select(COMID, eval(quantiles)) %>% #[,c("COMID", quantiles)]
        dplyr::mutate_at(quantiles, ~round(.,2))
    
    # Get BCG categories #### 
    # for each quantile
    for (q in 1:length(quantiles)) {
        quant <- as.character(stringr::str_extract(quantiles[q], "^qt\\d{2}$"))
        newcolname <- paste0("BCG",quant)
        dfpredCSCI <- merge(dfpredCSCI, dfCSCI2BCG, by.x = quant, by.y = "CSCI",
                            all.x = TRUE)
        colnames(dfpredCSCI)[ncol(dfpredCSCI)] <- newcolname
    }
    dfpredCSCI_BCG <- dfpredCSCI
    if (!boo_DEBUG) {rm(dfpredCSCI, quant, newcolname, q)}
    
    # Get most recent observed CSCI per site; if >1 sample on same date, average
    dfObsCSCI <- read.delim(fn_obsIndexBySite, header = TRUE, sep = "\t")
    
    dfObsCSCI <- dfObsCSCI %>%
        dplyr::select(StationID_Master, BMISampDate, CSCI) %>%
        dplyr::mutate(BMISampleDate = lubridate::mdy(BMISampDate)) %>%
        dplyr::select(-BMISampDate) %>%
        dplyr::group_by(StationID_Master, BMISampleDate) %>%
        dplyr::summarise(avgCSCI = round(mean(CSCI),2), .groups="drop_last") %>%
        dplyr::rename(CSCI = avgCSCI)
    dfObsCSCI <- as.data.frame(dfObsCSCI)
    
    # Get BCG tier corresponding with CSCI score for MAPPING ONLY
    dfObsCSCI_BCG <- merge(dfObsCSCI, dfCSCI2BCG, by.x = "CSCI"
                           , by.y = "CSCI", all.x = TRUE)
    
    # Create data frame with geographic coordinates for map (ultimately)
    sitecols <- c("StationID_Master","COMID","FinalLatitude","FinalLongitude")
    dfObsCSCI_BCG_xy <- merge(dfObsCSCI_BCG, dfSites[,sitecols], all.x = TRUE)
    dfObsCSCI_BCG_xy <- dfObsCSCI_BCG_xy[!is.na(dfObsCSCI_BCG_xy$FinalLatitude),]
    
    # 11 reaches (as of 4/16/2020) have multiple sites sampled on the same day
    # Select most recent on any reach and average same-day on same reach
    dfObsCSCI_BCG_COMID <- dfObsCSCI_BCG_xy %>%
        dplyr::select(COMID, BMISampleDate, CSCI) %>%
        dplyr::group_by(COMID) %>%
        dplyr::filter(BMISampleDate==max(BMISampleDate))
    dfObsCSCI_BCG_COMID <- dfObsCSCI_BCG_COMID %>%
        dplyr::group_by(COMID, BMISampleDate) %>%
        dplyr::summarise(avgCSCI = round(mean(CSCI),2), .groups="drop_last") %>%
        dplyr::rename(CSCI = avgCSCI)
    dfObsCSCI_BCG_COMID <- as.data.frame(dfObsCSCI_BCG_COMID)
    dfObsCSCI_BCG_COMID <- merge(dfObsCSCI_BCG_COMID, dfCSCI2BCG)
    dfObsCSCI_BCG_COMID <- dfObsCSCI_BCG_COMID[,c("COMID", "BMISampleDate"
                                                  , "CSCI", "BCGLevel")]
    
    if (!boo_DEBUG) {rm(dfObsCSCI, dfObsCSCI_BCG, dfCSCI2BCG)}
    
    myBCGtiers <- list(BCGcutoffs = breakpoints
                       , predReachBCG=dfpredCSCI_BCG
                       , obsSiteBCGxy=dfObsCSCI_BCG_xy
                       , obsReachBCG=dfObsCSCI_BCG_COMID)

}