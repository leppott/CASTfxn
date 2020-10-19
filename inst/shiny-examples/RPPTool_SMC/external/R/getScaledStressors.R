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
# or is never supported as a cause of impairment, it won't show up.

getScaledStressors <- function(fn_allstress
                               , fn_allstressinfo
                               , qtile = 0
                               , dir_CASTresults) { # FUNCTION.START
    #
    boo_DEBUG <- FALSE
    start.time <- Sys.time()
    #
    if(boo_DEBUG==TRUE){
        fn_allstress=file.path(dir_CASTdata,"SMC_AllStressData.tab")
        fn_allstressinfo=file.path(dir_CASTdata,"SMC_AllStressInfo.tab")
        qtile <- 0
        dir_CASTresults=dir_CASTresults
    } ##IF~boo_DEBUG~END
    
    # Define pipe
    `%>%` <- dplyr::`%>%`
    not_all_na <- function(x) {!all(is.na(x))}
    
    # Adjust significant digits
    sigfig <- function(vec, n=3) {
        return (formatC(signif(vec, digits = n), digits = n
                        , format = "fg"))
    }
    
    # Get recent CAST results files ####
    # (WoE details)
    fnlistFULL <- list.files(dir_CASTresults, pattern = "OverallWoEDetails"
                             , full.names = TRUE)
    if (length(fnlistFULL)>0) {
        fnlist <- list.files(dir_CASTresults, pattern = "OverallWoEDetails")
        fnmodtimes <- cbind(fnlist, file.info(fnlistFULL))
        rownames(fnmodtimes) <- NULL
        colnames(fnmodtimes)[1] <- "CASTfile"
        fnmodtimes <- fnmodtimes %>%
            dplyr::select(CASTfile, mtime) %>%
            dplyr::group_by(CASTfile) %>%
            dplyr::summarise(MostRecent = max(mtime), .groups="drop_last")
        fnWoE=as.character(fnmodtimes$CASTfile)
        dfWoE <- read.delim(file.path(dir_CASTresults,fnWoE)
                            , header = TRUE, stringsAsFactors = FALSE
                            , sep = "\t")

        # Get stressors supported as causes of BMI impairment
        dfWoE <- dfWoE %>%
            dplyr::filter(WoE=="Supports", BioComm=="BMI") %>%
            dplyr::select(StationID_Master, FinalLatitude, FinalLongitude
                          , BioDeg, Stressor, StressorType)
        
        if (!boo_DEBUG) {rm(fnlist, fnlistFULL, fnmodtimes, fnWoE)}
        
        if (nrow(dfWoE)>0) { # Supported causal stressors identified
            stressorsFound <- TRUE
            suppCauses <- as.character(unique(dfWoE$Stressor))
            # Get stressor data from CASTool ####
            dfStressVal <- read.delim(fn_allstress, header = TRUE
                                      , stringsAsFactors = FALSE
                                      , sep = "\t")
            dfStressVal <- dfStressVal %>%
                dplyr::mutate(BioComm="BMI") %>%
                dplyr::rename(StressSampID=ChemSampleID
                              , Stressor=StdParamName
                              , StressSampDate=SampleDate
                              , StressorValue=ResultValue) %>%
                dplyr::filter(Outlier != "Outlier") %>%
                dplyr::select(StationID_Master, StressSampID, StressSampDate
                              , Stressor, StressorValue)
            
            # Scale values to upper/lower qtiles for each target site/stressor combo
            dfStressVal <- unique(dfStressVal)
            dfStressVal <- dfStressVal %>%
                dplyr::group_by(Stressor) %>%
                dplyr::mutate(x = StressorValue
                              , xmin = min(StressorValue, na.rm = TRUE)
                              , xmax = max(StressorValue, na.rm = TRUE)
                              , deltaxminmax = xmax-xmin
                              , deltax_xmin = x - xmin
                              , pmin = ifelse(qtile!=0
                                              , as.numeric(sigfig(quantile(StressorValue
                                                                           , probs=qtile
                                                                           , na.rm=TRUE, names=F)))
                                              , 0)
                              , pmax = ifelse(qtile!=0
                                              , as.numeric(sigfig(quantile(StressorValue
                                                                           , probs=1-qtile
                                                                           , na.rm=TRUE, names=F)))
                                              , 1)
                              , y = (pmin + ((x-xmin)*(pmax-pmin))/(xmax-xmin))
                              , NumSamps = n())
            dfStressVal <- dfStressVal %>%
                dplyr::mutate(StressorValue = as.numeric(sigfig(StressorValue, n=3))
                              , AdjStressorValue = as.numeric(sigfig(y, n=3))
                              , StressSampleDate = lubridate::ymd(StressSampDate)) %>%
                dplyr::select(StationID_Master, Stressor, StressSampID, StressSampleDate
                              , StressorValue, AdjStressorValue, NumSamps)
            dfStressVal <- as.data.frame(dfStressVal)
            
            dfStressInfo <- read.delim(fn_allstressinfo, header = TRUE
                                       , stringsAsFactors = FALSE, sep = "\t")
            dfStressInfo <- dfStressInfo %>%
                dplyr::select(GroupName, Analyte, Label) %>%
                dplyr::rename(Stressor=Analyte, StressorGroup=GroupName) %>%
                dplyr::filter(Stressor %in% suppCauses) %>%
                dplyr::mutate(Weight=1) %>%
                dplyr::arrange(StressorGroup, Stressor)
            dfStressInfo <- unique(dfStressInfo)
        } else {
            dfStressInfo <- NA
            dfStressVal <- NA
            stressorsFound <- FALSE
        }
    
    } else {
        dfStressInfo <- NA
        dfStressVal <- NA
        stressorsFound <- FALSE
        message("CASTool Weight of Evidence results are not available.")
    }
    
    end.time <- Sys.time()
    elapsed.time <- end.time - start.time
    message(paste0("Elapsed time = ", format.difftime(elapsed.time)))

    myScaledStressors <- list(stressorsFound=stressorsFound
                              , df_allSMCStressInfo=dfStressInfo
                              , df_allSMCStressVals=dfStressVal
                              , dfWoE=dfWoE)
    
    return(myScaledStressors)
    
}
