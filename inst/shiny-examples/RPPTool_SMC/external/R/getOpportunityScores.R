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

getOpportunityScores <- function(fn_currentLU, fn_MSCP, fn_NASVI) {
    
    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`
    
    if (boo_DEBUG==TRUE) {
        message("DEBUG==TRUE")
        fn_currentLU=fn_currentLU
        fn_MSCP=fn_MSCP
        fn_NASVI=fn_NASVI
    }
    
    # Get Recreational Opportunities Indicator
    dfCLU <- readxl::read_excel(fn_currentLU, sheet = "SMCCatchment_CurrentLU_Percents"
                                , na = c("", "NA"), trim_ws = TRUE, skip = 0)
    dfCLU_meta <- readxl::read_excel(fn_currentLU, sheet = "Metadata"
                                     , na = c("", "NA"), trim_ws = TRUE, skip = 0)
    
    oppCLUcols <- dfCLU_meta %>% 
        dplyr::filter(SubIndex=="Opportunity") %>%
        dplyr::select(ColName)
    oppCLUcols <- as.vector(oppCLUcols$ColName)
    noppCLUcols <- length(oppCLUcols)
    
    dfCLU_opp <- dfCLU[,c("FEATUREID", oppCLUcols)]
    dfCLU_opp <- as.data.frame(dfCLU_opp)
    dfCLU_opp$TotNA <- rowSums(is.na(dfCLU_opp[,oppCLUcols]))
    dfCLU_opp$TotP_opp <- rowSums(dfCLU_opp[,oppCLUcols], na.rm = TRUE)
    dfCLU_opp <- dfCLU_opp %>%
        dplyr::mutate(opp_RecrInd = ifelse(TotNA == noppCLUcols, NA
                                          , signif(TotP_opp/100,3))) %>%
        dplyr::rename(COMID = FEATUREID) %>%
        dplyr::select(COMID, opp_RecrInd)
    
    # Get MSCP Indicator
    dfMSCP <- readxl::read_excel(fn_MSCP, sheet = "MSCP_CompleteData"
                                 , na = c("","NA"), trim_ws = TRUE, skip = 1
                                 , col_names = c("FEATUREID"
                                                 , "AreaSqKM"
                                                 , "P_AgUpFCA"
                                                 , "P_AgUpNoFCA"
                                                 , "P_BslnPres"
                                                 , "P_DevLand"
                                                 , "P_NoCode"
                                                 , "P_RipFCA"
                                                 , "P_RipNoFCA"
                                                 , "P_RMS1"
                                                 , "P_RMS2"
                                                 , "P_RMS3"
                                                 , "P_RMS4"
                                                 , "P_TribLand"
                                                 , "P_GCLndfl"
                                                 , "P_OthrLand"
                                                 , "P_OutsPAMA"
                                                 , "P_PAMA"
                                                 , "P_Preserve"
                                                 , "P_SpecDist"
                                                 , "P_TakeAuth"
                                                 , "P_TribFee"
                                                 , "P_TribTrst"
                                                 , "P_USFS"
                                                 , "P_ConsvSbj"
                                                 , "P_HrdlnPrs"
                                                 , "P_MajAmend"
                                                 , "P_MinAmdSC"
                                                 , "P_MinAmend"
                                                 , "P_SanFeDsg"
                                                 , "P_SanFeOSp"
                                                 , "P_UcLndMLJ")
                                 , col_types = c("numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"
                                                 , "numeric"))
    dfMSCP <- dfMSCP %>%
        dplyr::group_by(FEATUREID, AreaSqKM) %>%
        dplyr::summarise_all(~sum(., na.rm = TRUE), .groups="drop_last")
    
    dfMSCP_meta <- readxl::read_excel(fn_MSCP, sheet = "Metadata"
                                      , na = c("","NA"), trim_ws = TRUE, skip = 0)
    
    MSCPcols <- dfMSCP_meta %>%
        dplyr::filter(IncludeYN=="Yes") %>%
        dplyr::select(ColName)
    MSCPcols <- as.vector(MSCPcols$ColName)
    nMSCPcols <- length(MSCPcols)
    
    dfMSCP_opp <- dfMSCP[,c("FEATUREID", MSCPcols)]
    dfMSCP_opp <- as.data.frame(dfMSCP_opp)
    dfMSCP_opp$TotNA <- rowSums(is.na(dfMSCP_opp[,MSCPcols]))
    dfMSCP_opp$TotP_opp <- rowSums(dfMSCP_opp[,MSCPcols], na.rm = TRUE)
    dfMSCP_opp <- dfMSCP_opp %>%
        dplyr::mutate(opp_MSCPInd = ifelse(TotNA == nMSCPcols, NA
                                             , signif(TotP_opp/100,3))) %>%
        dplyr::mutate(opp_MSCPInd = ifelse(is.na(opp_MSCPInd), 0, opp_MSCPInd)
                      , opp_MSCPInd = ifelse(opp_MSCPInd>1,1,opp_MSCPInd)) %>%
        dplyr::rename(COMID = FEATUREID) %>%
        dplyr::select(COMID, opp_MSCPInd)
    
    # Get NASVI Indicator
    dfNASVI <- readxl::read_excel(fn_NASVI, sheet = "SMCCatchment_NASVI"
                                 , na = c("","NA"), trim_ws = TRUE, skip = 0)
    dfNASVI <- dfNASVI %>%
        dplyr::select(FEATUREID, WghtIndex) %>%
        dplyr::rename(COMID = FEATUREID, NASVIopp = WghtIndex) %>%
        dplyr::mutate(NASVIopp = NASVIopp) %>%
        dplyr::mutate(xmin = min(NASVIopp, na.rm = TRUE)
                      , xmax = max(NASVIopp, na.rm = TRUE)
                      , deltaxminmax = xmax-xmin
                      , deltax_xmin = NASVIopp - xmin
                      , opp_NASVIInd = signif((NASVIopp-xmin)/(xmax-xmin),3)) %>%
        dplyr::select(COMID,opp_NASVIInd)

    # PUll together opportunity indicator scores
    dfOppScores <- merge(dfCLU_opp, dfMSCP_opp, by.x = "COMID", by.y = "COMID"
                         , all = TRUE)
    
    dfOppScores <- dfOppScores %>% 
        dplyr::mutate(opp_MSCPIndComment = ifelse(!is.na(opp_MSCPInd), NA
                                           , "Not in MSCP region."))
    
    dfOppScores <- merge(dfOppScores, dfNASVI, by.x = "COMID", by.y = "COMID"
                         , all = TRUE)
    
    dfOppScores <- dfOppScores %>%
        dplyr::mutate(opp_RecrIndComment = ifelse(!is.na(opp_RecrInd), NA
                                            , "Not in SANDAG region.")
                      , opp_MSCPIndComment = ifelse(is.na(opp_MSCPIndComment) & is.na(opp_MSCPInd)
                                           , "Not in SANDAG region.", opp_MSCPIndComment)
                      , opp_UserDefInd = 1) %>%
        dplyr::select(COMID, opp_RecrInd, opp_RecrIndComment, opp_MSCPInd, opp_MSCPIndComment
                      , opp_NASVIInd, opp_UserDefInd)
    
    return(dfOppScores)

}