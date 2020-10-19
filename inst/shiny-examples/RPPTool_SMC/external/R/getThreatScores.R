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

getThreatScores <- function(fn_fireHazard, fn_plannedLU, fn_currentLU
                            , useModerateFireHazard) {

    boo_DEBUG <- FALSE
    `%>%` <- dplyr::`%>%`
    
    if (boo_DEBUG==TRUE) {
        message("DEBUG = TRUE")
        fn_plannedLU <- file.path(data_dir, "SMCCatchment_PlannedLU_Percents.xlsx")
        fn_currentLU <- file.path(data_dir, "SMCCatchment_CurrentLU_Percents.xlsx")
        dfLUthreat <- listLUdata$LUthreat
        fn_fireHazard <- file.path(data_dir, "SMCCatchment_FireHazard_Percents.tab")
        useModerateFireHazard <- 0
    }
    
    # Get Fire Hazard Indicator Score ####
    dfFH <- read.delim(fn_fireHazard, na.strings = c("", "NA"), strip.white = TRUE
                       , col.names = c("COMID", "AreaSqKM", "P_High", "P_Mod"
                                       , "P_VeryHigh", "P_TotalFire")
                       , colClasses = c("numeric", "numeric", "numeric", "numeric"
                                        , "numeric", "numeric"), skip = 1)
    
    if (useModerateFireHazard == 1) {
        dfFH <- dfFH[,c("COMID", "P_TotalFire")] %>%
            dplyr::mutate(P_TotalFire = ifelse(is.na(P_TotalFire), 0, P_TotalFire/100)) %>%
            dplyr::rename(thr_FireHazardInd = P_TotalFire) 
        
    } else {
        useCols <- c("P_VeryHigh","P_High")
        dfFH <- dfFH %>% 
            dplyr::mutate(P_TotalFire = rowSums(dfFH[,useCols], na.rm = TRUE)/100) %>%
            dplyr::mutate(P_TotalFire = signif(P_TotalFire,3)) %>%
            dplyr::select(COMID, P_TotalFire) %>%
            dplyr::mutate(P_TotalFire = ifelse(is.na(P_TotalFire),0,P_TotalFire)) %>%
            dplyr::rename(thr_FireHazardInd = P_TotalFire)
    }
    
    noLU <- "Decrease in developed land in catchment."
    noFH <- "Not in fire hazard data."
    
    # Get Planned Land Use, developed ####
    dfPLU <- readxl::read_excel(fn_plannedLU, sheet = "SMCCatchment_PlannedLU_Percents"
                                , na = c("", "NA"), trim_ws = TRUE, skip = 1
                                , col_names = c("FEATUREID"
                                                , "AreaSqKM"
                                                , "P_Agric"
                                                , "P_Airstrip"
                                                , "P_ArtrlCom"
                                                , "P_AutoDshp"
                                                , "P_BayLag"
                                                , "P_BchActve"
                                                , "P_BchPass"
                                                , "P_Casino"
                                                , "P_Cemetery"
                                                , "P_CommArpt"
                                                , "P_CommShop"
                                                , "P_CommsUtl"
                                                , "P_ConvCtr"
                                                , "P_Dorm"
                                                , "P_ElemSch"
                                                , "P_ExtrcInd"
                                                , "P_FirePlce"
                                                , "P_FldCrops"
                                                , "P_Freeway"
                                                , "P_GAviArpt"
                                                , "P_GolfClbH"
                                                , "P_GolfCrse"
                                                , "P_GovtOfce"
                                                , "P_GrpQtrFc"
                                                , "P_HlthCare"
                                                , "P_HospGen"
                                                , "P_HotelHi"
                                                , "P_HvyIndus"
                                                , "P_IndianRs"
                                                , "P_IndusPrk"
                                                , "P_IntsvAgr"
                                                , "P_JailPris"
                                                , "P_JrColleg"
                                                , "P_JrMidSch"
                                                , "P_Junkyard"
                                                , "P_LakeRes"
                                                , "P_Library"
                                                , "P_LiteInds"
                                                , "P_LndscpOS"
                                                , "P_Marina"
                                                , "P_MarinTrml"
                                                , "P_MblHmPrk"
                                                , "P_MFmRes"
                                                , "P_MFmRsNoU"
                                                , "P_MilArpt"
                                                , "P_MilBrcks"
                                                , "P_MilTrain"
                                                , "P_MilUse"
                                                , "P_Mission"
                                                , 'P_MixUse'
                                                , "P_Monastry"
                                                , "P_NbhdShop"
                                                , "P_OfficeHi"
                                                , "P_OfficeLo"
                                                , "P_OlyTrnCt"
                                                , "P_OpnSpcPk"
                                                , "P_OrchVine"
                                                , "P_OthrSch"
                                                , "P_OthrTrans"
                                                , "P_OthrUniv"
                                                , "P_ParkRide"
                                                , "P_PkgLtStr"
                                                , "P_PkgLtSur"
                                                , "P_PostOfce"
                                                , "P_PrkActve"
                                                , "P_PubSemiP"
                                                , "P_PubServ"
                                                , "P_PubStor"
                                                , "P_Racetrck"
                                                , "P_RailStn"
                                                , "P_RdROW"
                                                , "P_RecHigh"
                                                , "P_RecLow"
                                                , "P_RegShop"
                                                , "P_ReligFac"
                                                , "P_Resort"
                                                , "P_ResRec"
                                                , "P_RrROW"
                                                , "P_RtlTrde"
                                                , "P_SchDstOf"
                                                , "P_ServStn"
                                                , "P_SFmDtchd"
                                                , "P_SFmMltUn"
                                                , "P_SFmRes"
                                                , "P_SFmRsNoU"
                                                , "P_SnMcUCSD"
                                                , "P_SpecComm"
                                                , "P_SpRuRes"
                                                , "P_SrHiSch"
                                                , "P_SRmOccUn"
                                                , "P_StdArena"
                                                , "P_TourAttn"
                                                , "P_UCSDHosp"
                                                , "P_UndevNat"
                                                , "P_Water"
                                                , "P_WhslTrde"
                                                , "P_WpnsFac"
                                                , "P_Wrhsng")
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
                                                , "numeric"
                                                , "numeric"
                                                , "numeric"
                                                , "numeric"
                                                , "numeric"
                                                , 'numeric'
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
                                                , "numeric"
                                                , 'numeric'
                                                , "numeric"))
    
    dfPLU <- dfPLU %>%
        dplyr::group_by(FEATUREID, AreaSqKM) %>%
        dplyr::summarise_all(~sum(., na.rm = TRUE), .groups="drop_last")
    
    dfPLU_meta <- readxl::read_excel(fn_plannedLU, sheet = "Metadata"
                                     , na = c("", "NA"), trim_ws = TRUE, skip = 0)
    dfCLU <- readxl::read_excel(fn_currentLU, sheet = "SMCCatchment_CurrentLU_Percents"
                                , na = c("", "NA"), trim_ws = TRUE, skip = 0)

    dfCLU_meta <- readxl::read_excel(fn_currentLU, sheet = "Metadata"
                                     , na = c("", "NA"), trim_ws = TRUE, skip = 0)
    
    devPLUcols <- dfPLU_meta %>% 
        dplyr::filter(SubIndex=="Threat") %>%
        dplyr::select(ColName)
    devPLUcols <- as.vector(devPLUcols$ColName)
    ndevPLUcols <- length(devPLUcols)
    
    dfPLU_dev <- dfPLU[,c("FEATUREID", devPLUcols)]
    dfPLU_dev <- as.data.frame(dfPLU_dev)
    dfPLU_dev$TotNA <- rowSums(is.na(dfPLU_dev[,devPLUcols]))
    dfPLU_dev$TotP_dev <- rowSums(dfPLU_dev[,devPLUcols], na.rm = TRUE)
    dfPLU_dev <- dfPLU_dev %>%
        dplyr::mutate(P_PDEV_ALL = ifelse(TotNA == ndevPLUcols, NA, TotP_dev/100)) %>%
        dplyr::rename(COMID = FEATUREID) %>%
        dplyr::select(COMID, P_PDEV_ALL)
    
    # Get Current Land Use, developed ####
    devCLUcols <- dfCLU_meta %>% 
        dplyr::filter(SubIndex=="Threat") %>%
        dplyr::select(ColName)
    devCLUcols <- as.vector(devCLUcols$ColName)
    ndevCLUcols <- length(devCLUcols)
    
    dfCLU_dev <- dfCLU[,c("FEATUREID", devCLUcols)]
    dfCLU_dev <- as.data.frame(dfCLU_dev)
    dfCLU_dev$TotNA <- rowSums(is.na(dfCLU_dev[,devCLUcols]))
    dfCLU_dev$TotP_dev <- rowSums(dfCLU_dev[,devCLUcols], na.rm = TRUE)
    dfCLU_dev <- dfCLU_dev %>%
        dplyr::mutate(P_CDEV_ALL = ifelse(TotNA == ndevCLUcols, NA, TotP_dev/100)) %>%
        dplyr::rename(COMID = FEATUREID) %>%
        dplyr::select(COMID, P_CDEV_ALL)
    
    # Get change in developed land use (with comments)
    dfDevLU <- merge(dfPLU_dev, dfCLU_dev, by.x="COMID", by.y="COMID", all=TRUE)
    dfDevLU <- dfDevLU %>% 
        dplyr::mutate(P_PDEV_ALL = ifelse(is.na(P_PDEV_ALL),0,round(P_PDEV_ALL,3))
                      , P_CDEV_ALL = ifelse(is.na(P_CDEV_ALL),0,round(P_CDEV_ALL,3))
                      , thr_PlannedDevelopInd = signif((P_PDEV_ALL - P_CDEV_ALL),3)) %>%
        dplyr::mutate(thr_PlannedDevelopIndComment = ifelse(thr_PlannedDevelopInd<0,noLU,NA)
                      , thr_PlannedDevelopInd = ifelse(thr_PlannedDevelopInd<0,0
                                                       , thr_PlannedDevelopInd))
        # dplyr::select(COMID, DeltaDev, DeltaDevComment)
    
    # Get Threats Subindex
    dfThreats <- merge(dfFH, dfDevLU, by.x = "COMID", by.y = "COMID", all = TRUE)
    dfThreats <- dfThreats %>%
        dplyr::mutate(thr_PlannedDevelopIndComment = ifelse(is.na(thr_PlannedDevelopInd)
                                               , "Not in land use data region."
                                               , thr_PlannedDevelopIndComment)
                      , thr_FireHazardIndComment = ifelse(is.na(thr_FireHazardInd), noFH, NA))

    return(dfThreats)
    
}
    