#  Copyright 2023 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Get summary of all sites in results directory
#'
#' @description Pull relevant information from the final Weight of Evidence
#' file and combine it with the Sites data file to generate a usable
#' result file.
#'
#' @details The final, overall summary file lies at the root of the results
#' directory and contains site ID, latitude, longitude, sample name, biological
#' index score, stressor name, stressor value, and overall weight of evidence.
#'
#' Uses the libraries dplyr, tidyr, ggplot2, and ggthemes.
#'
#' @param biocomlist biological community; bmi or algae.  Default = c("bmi", "algae")
#' @param bmiIndex BMI index name; Default = "CSCI"
#' @param algIndex Algae index name; Default = "MMIhybrid"
#' @param dir_data Directory containing original data.
#' Default = "file.path(getwd(),"Data")"
#' @param dir_results Directory containing all results.
#' Default = "file.path(getwd(),"Results")"
#' @param dir_sub Subdirectory for weight of evidence (WoE) outputs used by this function.
#' Default = "WoE"
#' @param df_sites Data frame of sites.  Default = NULL
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
getSummaryAllSites <- function(biocommlist
                               , bmiIndex
                               , algIndex
                               , fishIndex
                               , dir_data = file.path(getwd(),"Data")
                               , dir_results = file.path(getwd(), "Results")
                               , dir_sub = "WoE"
                               , df_sites
                               ) {##FUNCTION.START

  # define pipe
  `%>%` <- dplyr::`%>%`

  # DEBUG ####
  boo_DEBUG <- FALSE

  if(boo_DEBUG==TRUE) {
    message("DEBUG = TRUE")
    wd = localdir #"C:/Users/ann.lincoln/Documents/SEP_CAST"
    biocommlist = toupper(c("bmi", "algae"))
    bmiIndex = "CSCI"
    algIndex = "MMIhybrid"
    fishIndex = NULL
    dir_data = dir_data
    dir_results = dir_results
    # dir_data = file.path(wd,"Data")
    # dir_results = file.path(wd, "Results")
    dir_sub = "WoE"
    df_sites = NULL
  }## IF ~ boo_DEBUG ~ END

  # Expected columns ####
  allLoEsWoE <- c("WoE", "TS_barplot", "CO_boxplot", "SR_InCase_LogRegr"
                  , "SR_InCase_LinRegr", "SR_OutCase_LinRegr", "VP_boxplot_senstaxa"
                  , "VP_boxplot_toltaxa", "SSD_ToxicityCurve")
  coreCols <- c("StationID_Master", "FinalLatitude", "FinalLongitude", "Cluster"
                , "BioComm", "BioDeg", "IndexScore", "RespSampID", "StressorType"
                , "StressSampID", "Stressor", "StressorValue", "StressorPctRank")

  # Start ####
  # message("getSummaryAllSites, 01, start")
  if (!dir.exists(file.path(dir_results))==TRUE) { # Check for results dir
    message("Results directory not found.")
  } else { # Results dir exists
    site_dirs <- list.dirs(normalizePath(dir_results), full.names = FALSE
                           , recursive = FALSE)

    # message("getSummaryAllSites, 02, for ~ site")
    for (site in (1:length(site_dirs))) { # Loop over each site
      # Get Target Site ID
      TargetSiteID <- site_dirs[site]
      message(paste0("Evaluating ", TargetSiteID))

      # message("getSummaryAllSites, 02, for ~ b")
      for (b in (1:length(biocommlist))) { # For each biological community
        biocomm = toupper(biocommlist[b])
        if ((biocomm == "BMI") & exists("bmiIndex")) {
          bioIndex <- bmiIndex
        } else if ((biocomm == "ALGAE") & exists("algIndex")) {
          bioIndex <- algIndex
        } else if ((biocomm == "FISH") & (exists(fishIndex) | !is.null(fishIndex))) {
          bioIndex <- fishIndex
        } else {
          bioIndex <- NULL
        } ## FOR ~ biocomm ~ END
        # message(paste0("b = ", b))
        # message(paste0("bioIndex = ", bioIndex))

        # Get WoE path & file lists (under TargetSiteID)
        woe_path <- normalizePath(file.path(dir_results, TargetSiteID
                                            , toupper(biocomm), "WoE"))
        woe_detailfiles <- list.files(woe_path, pattern = "WoE_ScoresTable")
        woe_stressfiles <- list.files(woe_path, pattern = "WoE_ExecSummary")

        # If there are no files matching criteria, move on
        # If there are one or more (for each biocomm), read them
        ## Get WoE score files ####
        if (length(woe_detailfiles) == 0) {
          message(paste0("No WoE detailed scores available for "
                         , TargetSiteID, " for ", biocomm, "."))
          next()
        } else if (is.null(bioIndex)) {
          message("Biological community not recognized")
          next()
        } else {
          for (dfile in (1:length(woe_detailfiles))) {
            # Read file
            fndet <- woe_detailfiles[dfile]
            fndet <- file.path(woe_path, fndet)
            df_details <- read.delim(fndet, header = TRUE, sep = "\t"
                                     , na.strings = c("", NA)
                                     , stringsAsFactors = FALSE)
            colnames(df_details)[6] <- "IndexScore"
            if (dfile == 1) {
              #if(!exists("df_detBiocomm")){
              df_detBiocomm <- df_details
            } else {
              df_detBiocomm <- rbind(df_detBiocomm, df_details)
            }
          }## FOR ~ dfile ~ END
        }## IF ~ length(woe_detailfiles)==0 ~ END

        # Get WoE detail files ####
        if (length(woe_stressfiles) == 0) {
          message(paste0("No WoE executive summary found for "
                         , TargetSiteID, " for ", biocomm, "."))
        } else {
          for (dfile in (1:length(woe_stressfiles))) {
            # Read file
            fnstr <- woe_stressfiles[dfile]
            fnstr <- file.path(woe_path, fnstr)
            df_stress <- read.delim(fnstr, header = TRUE, sep = "\t"
                                    , na.strings = c("", NA)
                                    , stringsAsFactors = FALSE)
            if (dfile == 1) {
              #if(!exists("df_strBiocomm")){
              df_strBiocomm <- df_stress
            } else {
              df_strBiocomm <- rbind(df_strBiocomm, df_stress)
            }
          }## FOR ~ dfile ~ END
        }## IF ~ length(woe_stressfiles)==0 ~ END


        if(!exists("df_detSite")){
          df_detSite <- df_detBiocomm
          msg <- "df_detSite = df_detBiocomm"
        } else {
          df_detSite <- rbind(df_detSite, df_detBiocomm)
          msg <- "df_detSite = rbind(df_detSite, df_detBiocomm)"
        }## IF ~ exists("df_detSite) ~ END
        # message(msg)
        #
        if(!exists("df_strSite")){
          df_strSite <- df_strBiocomm
        } else {
          df_strSite <- rbind(df_strSite, df_strBiocomm)
        }## IF ~ exists("df_strSite) ~ END

      }## FOR ~ b ~ END # Process individual biocomm for an individual site

    }## FOR ~ site ~ END # Finish iterate through sites in results folder


    # Get site lat/long/cluster ####
    # Merge with site data to get latitude and longitude
    # message("getSummaryAllSites, xx, merge lat-long data")
    if (is.null(df_sites) == TRUE) {
      fn.Sites.Info <- file.path(dir_data, "SMCSitesFinal.tab")
      fn.sites <- fn.Sites.Info
      # fn.sites <- fn.Sites.Info <- file.path(dir_data,"SMCSitesFinal.tab")
      df_sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t")
    }## IF ~ is.null(df_sites)==TRUE) ~ END

    # WoE Summary ####
    # message("getSummaryAllSites, 03, WoE Summary")
    df_SitesLatLong <- df_sites[, c("StationID_Master", "FinalLatitude"
                                   , "FinalLongitude")]


    # message("names(df_detSite)")
    # message(paste(names(df_detSite), collapse = ", "))
    # message("names(df_SitesLatLong)")
    # message(paste(names(df_SitesLatLong), collapse = ", "))
    df_WoEDetails <- merge(df_detSite, df_SitesLatLong
                           , by = "StationID_Master")

    LoECols <- intersect(allLoEsWoE, colnames(df_WoEDetails))
    coreSite <- intersect(coreCols, colnames(df_WoEDetails))

    # Create WoE details for all sites ####
    df_WoEDetails <- unique(df_WoEDetails[, c(coreSite, LoECols)])

    # Create WoE summary for all sites ####
    df_WoESummary <- merge(df_SitesLatLong, df_strSite
                           , by.x = "StationID_Master"
                           , by.y = "StationID_Master")

    df_WoESummary <- df_WoESummary %>%
      dplyr::select(StationID_Master
                    , FinalLatitude
                    , FinalLongitude
                    # , all_of(OutsideColName)
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
      dplyr::rename(Overall_WoE = WtTot_WoE) %>%
      dplyr::arrange(StationID_Master, desc(BioComm)
                     , desc(BioDeg), StressorType)
    df_WoESummary <- unique(df_WoESummary)

  }## FOR ~ !dir.exists(file.path(dir_results))==TRUE ~ END
  # Finish iterate through site directories loop

  # Clean Up ####
  # message("getSummaryAllSites, xx, clean up")
  myDate <- lubridate::ymd(lubridate::today())
  myDate <- stringr::str_replace_all(myDate, "-", "")
  fnES <- file.path(dir_results, paste0("OverallWoESummary_"
                                        , myDate, ".tab"))
  write.table(df_WoESummary, fnES, append = FALSE, col.names = TRUE
              , row.names = FALSE, sep = "\t")
  fnDetails <- file.path(dir_results, paste0("OverallWoEDetails_"
                                             , myDate, ".tab"))
  write.table(df_WoEDetails, fnDetails, append = FALSE, col.names = TRUE
              , row.names = FALSE, sep = "\t")

  # message("getSummaryAllSites, Completed compiling WoE summary.")

}## FUNCTION ~ END

