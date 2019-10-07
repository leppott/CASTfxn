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
#' 
#' @keywords internal
#' 
#' @export
getSummaryAllSites <- function(dir_data = file.path(getwd(),"Data")
                               , dir_results = file.path(getwd(), "Results")
                               , dir_sub = "WoE"
                               , df_sites = NULL) {
    
    # define pipe
    `%>%` <- dplyr::`%>%`
    
    dir_data = file.path(getwd(),"Data")
    dir_results = file.path(getwd(), "Results")
    dir_sub = "WoE"
    df_sites = NULL
    
    if (!dir.exists(file.path(dir_results))==TRUE) {
        print("Results directory not found.")
        flush.console()
    } else {
        site_dirs <- list.files(dir_results)
        
        for (site in (1:length(site_dirs))) {
            
            # Get Target Site ID & skip any site folders that are qualified
            TargetSiteID <- site_dirs[site]
            if (grepl("_", TargetSiteID)==TRUE) {
                next     # Site folder is qualified, likely w/an error
            }
            
            # Get WoE path & file lists (under TargetSiteID)
            woe_path <- file.path(dir_results, TargetSiteID, "WoE")
            woe_detailfiles <- list.files(woe_path, pattern = "WoEdetail")
            woe_stressfiles <- list.files(woe_path, pattern = "WoEstressor")
            
            # If there are no files matching criteria, move on
            # If there are one or more (for each biocomm), read them
            if (length(woe_detailfiles)==0) {
                print(paste0("No WoE detail files found for ", TargetSiteID))
                flush.console()
            } else {
                for (dfile in (1:length(woe_detailfiles))) {
                    # Read file
                    fndet <- woe_detailfiles[dfile]
                    biocomm <- stringr::str_extract(fndet , pattern = "(bmi|alg)")
                    fndet <- file.path(woe_path,fndet)
                    df_details <- read.delim(fndet, header = TRUE, sep = "\t"
                                             , na.strings = c("", NA))
                    df_details <- dplyr::mutate(df_details, BioComm = eval(biocomm))
                    if (dfile==1) {
                        df_details_All <- df_details
                    } else {
                        df_details_All <- rbind(df_details_All, df_details)
                    }
                }
            }
            
            if (length(woe_stressfiles)==0) {
                print(paste0("No WoE stressor files found for ", TargetSiteID))
                flush.console()
            } else {
                for (dfile in (1:length(woe_stressfiles))) {
                    # Read file
                    fnstr <- woe_stressfiles[dfile]
                    biocomm <- stringr::str_extract(fnstr , pattern = "(bmi|alg)")
                    fnstr <- file.path(woe_path,fnstr)
                    df_stress <- read.delim(fnstr, header = TRUE, sep = "\t"
                                             , na.strings = c("", NA))
                    df_stress <- dplyr::mutate(df_stress, BioComm = eval(biocomm))
                    if (dfile==1) {
                        df_stress_All <- df_stress
                    } else {
                        df_stress_All <- rbind(df_stress_All, df_stress)
                    }
                }
            }
            
            # Merge stressor and detailed WoE data
            df_WoE <- merge(df_stress_All, df_details_All
                            , by.x = c("StationID_Master", "Bio.Deg", "WoE"
                                       , "GroupName", "Stressor", "BioComm")
                            , by.y = c("StationID_Master", "Bio.Deg", "WoE"
                                       , "GroupName", "Stressor", "BioComm"))

            if (site==1) {
                df_WoE_All <- df_WoE
            } else {
                df_WoE_All <- rbind(df_WoE_All, df_WoE)
            }
            
        }
        # Merge with site data to get latitude and longitude
        if (is.null(df_sites)==TRUE) {
            fn.sites <- fn.Sites.Info <- file.path(dir_data,"SMCSitesFinal.tab")
            df_sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t")
        }
        
        df_SitesLatLong <- df_sites[,c("StationID_Master", "FinalLatitude"
                                       , "FinalLongitude", "clust")]
        
        df_WoE_All <- merge(df_SitesLatLong, df_WoE_All
                            , by.x = "StationID_Master"
                            , by.y = "StationID_Master")
        
        df_WoE_All <- df_WoE_All %>%
            select(StationID_Master
                   , FinalLatitude
                   , FinalLongitude
                   , clust
                   , BioComm
                   , Response
                   , ResponseValue
                   , Bio.Deg
                   , GroupName
                   , StressSampID
                   , Stressor
                   , StressorValue
                   , PctRank
                   , nSamples
                   , WoE)
        myDate <- lubridate::ymd(lubridate::today())
        myDate <- stringr::str_replace_all(myDate, "-", "")
        fn.overall <- file.path(dir_results, paste0("OverallWoESummary_"
                                                    ,myDate,".tab"))
        write.table(df_WoE_All, fn.overall, append = FALSE, col.names = TRUE
                    , row.names = FALSE, sep = "\t")
    }
    
    print("Completed compiling WoE summary.")
    flush.console()
    
    
}
    
