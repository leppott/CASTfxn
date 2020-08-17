#' @title Get summary of all sites in results directory
#' 
#' @description Pull relevant information from the final Weight of Evidence
#' file and combine it with the Sites data file to generate a mappable
#' result file.
#' 
#' @details The final, overall summary file lies at the root of the results
#' directory and contains site ID, latitude, longitude, sample name, biological
#' index score, stressor name, stressor value, and overal weight of evidence.
#' 
#' Uses the libraries dplyr, tidyr, ggplot2, and ggthemes.
#' 
#' @param dir_data Directory containing original data. 
#' Default = "file.path(getwd(),"Data")"
#' @param dir_results Directory containing all results. 
#' Default = "file.path(getwd(),"Results")"
#' @param dir_sub Subdirectory for WoE outputs used by this function. 
#' Default = "WoE"
#' @param TargetSiteID Site ID
#' @param TargetCOMID NHDPlus version 2 COMID for the reach on which the site lies.
#' 
#' @return Tab-delimited text summary data file containing site id, latitude,
#' longitude, cluster, response (bio index), response value (index score), 
#' degraded flag (Y/N), stressor sampleID, stressor name, stressor value,
#' stressor percent rank among comparators, number of paired samples, and 
#' final weight of evidence.
#' 
#' @keywords internal
#' 
#' @export
getSummaryAllSites <- function(biocommlist = c("bmi", "algae")
                               , bmiIndex = "CSCI"
                               , algIndex = "MMIhybrid"
                               , dir_data = file.path(getwd(),"Data")
                               , dir_results = file.path(getwd(), "Results")
                               , dir_sub = "WoE"
                               , df_sites = NULL) {
    
    # define pipe
    `%>%` <- dplyr::`%>%`
    boo_DEBUG <- FALSE
    
    if(boo_DEBUG==TRUE) {
        wd = "C:/Users/ann.lincoln/Documents/SEP_CAST"
        biocommlist = toupper(c("bmi", "algae"))
        bmiIndex = "CSCI"
        algIndex = "MMIhybrid"
        dir_data = file.path(wd,"Data")
        dir_results = file.path(wd, "Results")
        dir_sub = "WoE"
        df_sites = NULL
    }
    
#<<<<<<< 201909_ARL
    ### Start ####
    if (!dir.exists(file.path(dir_results))==TRUE) { # Check for results dir
        print("Results directory not found.")
        flush.console()
    } else { # Results dir exists
        site_dirs <- list.files(dir_results)
        rmfile <- site_dirs[grep("^RunStats_\\d{8}\\.tab$", site_dirs)]
        site_dirs <- site_dirs[!site_dirs %in% rmfile]
        
        rmfile <- site_dirs[grep("\\.7z$", site_dirs)]
        site_dirs <- site_dirs[!site_dirs %in% rmfile] # List of sites in results
#=======
# Pull Request, 20200817   
#    if (!dir.exists(file.path(dir_results))==TRUE) {
#        message("Results directory not found.")
#        #flush.console()
#    } else {
#        # site_dirs <- list.files(dir_results)
#        # rmfile <- site_dirs[grep("^RunStats_\\d{8}\\.tab$", site_dirs)]
#        # site_dirs <- site_dirs[!site_dirs %in% rmfile]
#        # # Remove .zip or .7z files
#        # rmfile <- site_dirs[grep("(\\.7z$)|(\\.zip$)", site_dirs)]
#        # site_dirs <- site_dirs[!site_dirs %in% rmfile]
#        
#        site_dirs <- list.dirs(dir_results, full.names = FALSE, recursive = FALSE)
#>>>>>>> master
        
        for (site in (1:length(site_dirs))) { # Loop over each site
            # Get Target Site ID
            TargetSiteID <- site_dirs[site]
            print(paste0("Evaluating ", TargetSiteID))
            flush.console()
            for (b in (1:length(biocommlist))) { # For each biological community
                biocomm = biocommlist[b]
                if (biocomm=="BMI") { 
                    bioIndex = bmiIndex 
                } else { 
                    bioIndex = algIndex
                }
                # Get WoE path & file lists (under TargetSiteID)
                woe_path <- file.path(dir_results, TargetSiteID, biocomm, "WoE")
                woe_detailfiles <- list.files(woe_path, pattern = "WoE_ScoresTable")
                woe_stressfiles <- list.files(woe_path, pattern = "WoE_ExecSummary")
                
                # If there are no files matching criteria, move on
                # If there are one or more (for each biocomm), read them
                if (length(woe_detailfiles)==0) {
                    message(paste0("No WoE detailed scores available for "
                                 , TargetSiteID, " for ", biocomm, "."))
#<<<<<<< 201909_ARL
                    flush.console()
                    next()
#=======
                    #flush.console()
#>>>>>>> master
                } else {
                    for (dfile in (1:length(woe_detailfiles))) {
                        # Read file
                        fndet <- woe_detailfiles[dfile]
                        fndet <- file.path(woe_path,fndet)
                        df_details <- read.delim(fndet, header = TRUE, sep = "\t"
                                                 , na.strings = c("", NA))
                        colnames(df_details)[6] <- "IndexScore"
                        if (dfile==1) {
                            df_detBiocomm <- df_details
                        } else {
                            df_detBiocomm <- rbind(df_detBiocomm, df_details)
                        }
                    }
                }
                
                if (length(woe_stressfiles)==0) {
                    message(paste0("No WoE executive summary found for "
                                 , TargetSiteID, " for ", biocomm, "."))
                    #flush.console()
                } else {
                    for (dfile in (1:length(woe_stressfiles))) {
                        # Read file
                        fnstr <- woe_stressfiles[dfile]
                        fnstr <- file.path(woe_path,fnstr)
                        df_stress <- read.delim(fnstr, header = TRUE, sep = "\t"
                                                , na.strings = c("", NA))
                        if (dfile==1) {
                            df_strBiocomm <- df_stress
                        } else {
                            df_strBiocomm <- rbind(sd_strBiocomm, df_stress)
                        }
                    }
                }
                
                if (b==1) {
                    df_detSite <- df_detBiocomm
                    df_strSite <- df_strBiocomm
                } else {
                    df_detSite <- rbind(df_detSite, df_detBiocomm)
                    df_strSite <- rbind(df_strSite, df_strBiocomm)
                }
                
            } # Process individual biocomm for an individual site
            
            # Combine results for each site into one dataframe
            if (site==1) {
                df_detAllSites <- df_detSite
                df_strAllSites <- df_strSite
            } else {
                df_detAllSites <- rbind(df_detAllSites, df_detSite)
                df_strAllSites <- rbind(df_strAllSites, df_strSite)
            }
            
            # get candidate causes path and chem value file lists (BMI only)
            # candcause_path <- file.path(dir_results, TargetSiteID, "BMI"
            #                             , "CandidateCauses")
            # cc_valuefiles <- list.files(candcause_path, pattern = "ChemValues")

            # Transpose, add biocomm, TargetSiteID to chemvalues
            # if (length(cc_valuefiles)==0) {
            #     print(paste0("No stressor values file found for "
            #                  , TargetSiteID, " for ", biocomm, "."))
            #     flush.console()
            #     next()
            # } else {
            #     for (valfile in (1:length(cc_valuefiles))) {
            #         # Read file
            #         fnStrVals <- cc_valuefiles[valfile]
            #         fnStrVals <- file.path(candcause_path,fnStrVals)
            #         df_StrVals <- read.delim(fnStrVals, header = TRUE
            #                                  , sep = "\t"
            #                                  , na.strings = c("", NA))
            #         dfStrValsLong <- df_StrVals %>%
            #             dplyr::select(-IQRmethod, -SDmethod, -Outlier) %>%
            #             tidyr::gather(key = "Stressor", value = "StressorValue"
            #                           , na.rm = TRUE
            #                           , -StationID_Master, -StressSampID
            #                           , -StressSampDate) %>%
            #             dplyr::mutate(TargetSite = TargetSiteID
            #                           , BioComm = "BMI") %>%
            #             dplyr::select(TargetSite, BioComm, StationID_Master
            #                           , StressSampID, StressSampDate, Stressor
            #                           , StressorValue)
            #         
            #     }
            #     
            #     if (site==1) {
            #         df_StrValsAll <- dfStrValsLong
            #     } else {
            #         df_StrValsAll <- rbind(df_StrValsAll, dfStrValsLong)
            #     }
            #     
            # } # Complete compilation of stressor values
            # 
            # cc_valuefiles <- NULL

        } # Finish iterate through sites in results folder
        
        # Merge with site data to get latitude and longitude
        if (is.null(df_sites)==TRUE) {
            fn.sites <- fn.Sites.Info <- file.path(dir_data,"SMCSitesFinal.tab")
            df_sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t")
        }
        
        df_SitesLatLong <- df_sites[,c("StationID_Master", "FinalLatitude"
                                       , "FinalLongitude", "clust")]
        
        df_WoEDetails <- merge(df_detAllSites, df_SitesLatLong
                               , by.x = c("StationID_Master", "Cluster")
                               , by.y = c("StationID_Master", "clust"))
        
        df_WoEDetails <- df_WoEDetails %>%
            dplyr::select(StationID_Master
                   , FinalLatitude
                   , FinalLongitude
                   , Cluster
                   , BioComm
                   , BioDeg
                   , IndexScore
                   , RespSampID
                   , StressorType
                   , StressSampID
                   , Stressor
                   , StressorValue
                   , StressorPctRank
                   , WoE
                   , TS_barplot
                   , CO_boxplot
                   , SR_InCase_LogRegr
                   , SR_InCase_LinRegr
                   , SR_OutCase_LinRegr
                   , VP_boxplot_senstaxa
                   , VP_boxplot_toltaxa
                   , SSD_ToxicityCurve)
        
        
        df_WoESummary <- merge(df_SitesLatLong, df_strAllSites
                               , by.x = "StationID_Master"
                               , by.y = "StationID_Master")
        
        df_WoESummary <- df_WoESummary %>%
            dplyr::select(StationID_Master
                   , FinalLatitude
                   , FinalLongitude
                   , clust
                   , BioComm
                   , BioDeg
                   , NumRespSamples
                   , minIndex
                   , meanIndex
                   , maxIndex
                   , StressorType
                   , NumStressSamples
                   , NumStressors
                   , WtTot_WoE
                   , WtTotTS_barplot
                   , WtTotCO_boxplot
                   , WtTotSR_InCase_LogRegr
                   , WtTotSR_InCase_LinRegr
                   , WtTotSR_OutCase_LinRegr
                   , WtTotVP_boxplot) %>%
            dplyr::rename(Cluster = clust
                          , Overall_WoE = WtTot_WoE) %>%
            dplyr::arrange(StationID_Master, desc(BioComm)
                           , desc(BioDeg), StressorType)

    } # Finish iterate through site directories loop
    
    myDate <- lubridate::ymd(lubridate::today())
    myDate <- stringr::str_replace_all(myDate, "-", "")
    fnES <- file.path(dir_results, paste0("OverallWoESummary_"
                                                ,myDate,".tab"))
    write.table(df_WoESummary, fnES, append = FALSE, col.names = TRUE
                , row.names = FALSE, sep = "\t")
    fnDetails <- file.path(dir_results, paste0("OverallWoEDetails_"
                                          ,myDate,".tab"))
    write.table(df_WoEDetails, fnDetails, append = FALSE, col.names = TRUE
                , row.names = FALSE, sep = "\t")
    
    message("Completed compiling WoE summary.")
    #flush.console()
    
    fnStressorVals <- file.path(dir_results, paste0("OverallStressorValues_"
                                                    , myDate, ".tab"))
    write.table(df_StrValsAll, fnStressorVals, append = FALSE, col.names = TRUE
                , row.names = FALSE, sep = "\t")
    
    print("Completed compiling stressor values.")
    flush.console()
    
    rm(list = ls())
    
}
    
