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

boo_Shiny <- FALSE
boo_DEBUG <- FALSE

    if(boo_Shiny == TRUE){
      wd <- file.path(".")
    } else {
      # rm(list=ls())
      boo_Shiny <- FALSE
      
      library(readxl)
      library(dplyr)
      library(tidyr)
      library(ggplot2)

      gitpath <- "C:/Users/ann.lincoln/Documents/GitHub/RPPTool" 
      
      # Source required functions #
      wd <- getwd()
      source(file.path(gitpath,"drawAllScoresPlot.R"))
      source(file.path(gitpath,"drawBarPlot.R"))
      source(file.path(gitpath,"getBCGScores.R"))
      source(file.path(gitpath,"getBCGtiers.R"))
      source(file.path(gitpath,"getConnectivity.R"))
      source(file.path(gitpath,"getConnectivityScores.R"))
      source(file.path(gitpath,"getOpportunityScores.R"))
      source(file.path(gitpath,"getReachMap.R"))
      source(file.path(gitpath,"getScaledStressors.R"))
      source(file.path(gitpath,"getStressorScores.R"))
      source(file.path(gitpath,"getThreatScores.R"))
      source(file.path(gitpath,"updateAllScoresTable.R"))
      rm(gitpath)
    }## IF ~ boo_Shiny ~ END
    
    # define pipe
    `%>%` <- dplyr::`%>%`
    # define function for user interaction
    useStrWtsFile <- function() {
        msg <- paste("Select stressor weight file to use: "
                     , paste0("1: Use existing file: ", fn_stresswtsOUT)
                     , "2: Use another file. "
                     , "3: Write new file for editing."
                     , ""
                     , "If #3, edit the file "
                     , file.path(data_dir, fn_stresswtsOUT)
                     , "and run this program again."
                     , sep = "\n")
        answer <- readline(prompt=msg)
        answer <- as.integer(answer)
        if (answer == 1 | is.null(answer)) {
            fn_stresswtsIN <- file.path(data_dir,fn_stresswtsOUT)
        } else if (answer == 2) {
            response <- readline(prompt="Enter full filename and path: ")
            fn_stresswtsIN <- response
            message(paste0("Using ", fn_stresswtsIN))
        } else {
            fn_stresswtsIN <- "STOP"
            write.table(listScaledStr01All$df_allSMCStressInfo
                        , file.path(data_dir,fn_stresswtsOUT)
                        , append=FALSE
                        , col.names=TRUE, row.names=FALSE, sep="\t")
        }
        return(fn_stresswtsIN)
    }
    # not_all_na <- function(x) {!all(is.na(x))}
    
    if (boo_DEBUG==TRUE) {
      TargetCOMIDs=c(17569571, 20325195, 20329746, 20331170, 20331434, 20333052
                     , 22549067, 17562522, 17563304)
      # TargetCOMIDs = 20331170
    }
    myDate <- lubridate::ymd(lubridate::today())
    myDate <- stringr::str_replace_all(myDate, "-", "")
    start.time <- Sys.time()
    
    # 03, Set RPPTool directories ####
    # Progress, 02
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Input", "; Directory Names")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    if(boo_Shiny == TRUE){
      sep_rpp_dir <- file.path(".")
    } else {
      sep_rpp_dir <- "C:/Users/ann.lincoln/Documents/SEP_RPP"
    }## IF ~ boo_Shiny ~ END
    data_dir <- file.path(sep_rpp_dir,"Data")
    results_dir <- file.path(sep_rpp_dir,"Results")
    
    # 04, Set output file names ####
    # Progress, 03
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Output", "; File Names")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    # These are not expected to be user-modified
    fn_numsampsyear   <- file.path(results_dir,"NumStressSamplesByYear.png")
    fn_numbmiyear     <- file.path(results_dir,"NumBioSampsByYear.png")
    fn_stresswtsOUT   <- "StressorWeights.tab"
    fn_BCGscores      <- file.path(results_dir,paste0("RPPTool_BCGScores_",myDate
                                                      ,".tab"))
    fn_StressorScores <- file.path(results_dir
                                   ,paste0("RPPTool_StressorScores_",myDate,".tab"))
    fn_StressScoreDetails <- file.path(results_dir
                                       ,paste0("RPPTool_StressorScoreDetails_"
                                               ,myDate,".tab"))
    fn_cxnsALL <- file.path(results_dir
                            ,paste0("RPPTool_AllConnectedReaches_",myDate,".tab"))
    fn_cxnscoredetail <- file.path(results_dir
                                   , paste0("RPPTool_AllConnectivityScoreDetails_"
                                            ,myDate,".tab"))
    fn_allscores   <- file.path(results_dir,paste0("RPPTool_AllScores"))
    
    # 05, User-defined variables ####
    # Progress, 04
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Input", "; Variable Names")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    # Is it possible to have a browse button, so the user doesn't have to type it?
    # Note that these are all default values
    if(boo_Shiny == TRUE){
      useCASTresults <- input$useCASTresults
      maxYear <- input$year_max
      minYear <- input$year_min
    } else {
      useCASTresults <- TRUE
      maxYear <- lubridate::year(Sys.Date()) # Obtained from user (NOTE: this is inclusive)
      minYear <- maxYear - 12 # Inclusive (defaults to 2008 to present on 4/17/2020)
    }## IF ~ boo_Shiny ~ END 
    if (useCASTresults==TRUE) {
      if(boo_Shiny == TRUE){
        dir_CASTdata    <- file.path(".", "")
        dir_CASTresults <- file.path(".")
      } else {
        dir_CASTdata <- "C:/Users/ann.lincoln/Documents/SEP_CAST/Data" # Obtained from user
        dir_CASTresults <- "C:/Users/ann.lincoln/Documents/SEP_CAST/Results" # Obtained from user
      }## IF ~ boo_Shiny ~ END
      fn_allstress     <- "SMC_AllStressData.tab" # Change only if file name differs
      fn_allstressmeta <- "SMC_AllStressInfo.tab" # Change only if file name differs
      usePrevStressWts <- FALSE
      if (usePrevStressWts) { # User must supply stress weighting file to use
        # fn_stresswtsIN <- ""
      } else {
        fn_stresswtsIN <- file.path(data_dir,fn_stresswtsOUT) # Change only if a copy of the weights file is prepared
      }## IF ~ usePrevStressWts ~ END
    }## IF useCASTresults ~ END
    
    if(boo_Shiny == TRUE){
      # Connectivity variables
      cxndist_km        <- input$cxndist_km #5
      useHWbonus        <- input$useHWbonus #0 # FALSE (default)
      useBCGbonus       <- input$useBCGbonus #0 # FALSE (default)
      useDownstream     <- input$useDownstream #0 # FALSE (default)
      useModerateFireHazard <- input$useModFireHazard # FALSE (default)
      # Indicator weights
      wtPot_BCG         <- input$wt_Pot_BCG
      wtPot_CxnBCG      <- input$wt_Pot_CxnBCG
      wtPot_Stress      <- input$wt_Pot_Stress
      wtPot_CxnStress   <- input$wt_Pot_CxnStress
      wtThreat_Fire     <- input$wt_Threat_Fire
      wtThreat_LU       <- input$wt_Threat_LU # Probably need to separate out categories
      wtOpp_ParksNow    <- input$wt_Opp_ParksNow
      wtOpp_MSCPs       <- input$wt_Opp_MSCPs
      wtOpp_NASVI       <- input$wt_Opp_NASVIBCG
      wtOpp_UserDefined <- input$wt_Opp_UserDefined
      wtPot_subidx      <- 1 #input$wt_SubIndex_Pot
      wtThreat_subidx   <- 1 #input$wt_SubIndex_Threat
      wtOpp_subidx      <- 1 #input$wt_SubIndex_Opp
    } else {
      # Connectivity variables
      cxndist_km      <- 5
      useHWbonus      <- 0 # FALSE (default)
      useBCGbonus     <- 0 # FALSE (default)
      useDownstream   <- 0 # FALSE (default)
      useModerateFireHazard <- 0 # FALSE (default)
      # Indicator weights
      wtPot_BCG       <- 1
      wtPot_CxnBCG    <- 1
      wtPot_Stress    <- 1
      wtPot_CxnStress <- 1
      wtThreat_Fire   <- 1
      wtThreat_LU     <- 1 # Probably need to separate out categories
      wtOpp_ParksNow  <- 1
      wtOpp_MSCPs     <- 1
      wtOpp_NASVI     <- 1
      wtOpp_UserDefined <- 1 # Allowed values = c(0,1,2)
      wtPot_subidx    <- 1
      wtThreat_subidx <- 1
      wtOpp_subidx    <- 1
    }## IF ~ boo_Shiny ~ END
    
    
    
    listWeights <- list(wtPot_Bio=wtPot_BCG, wtPot_CxnBio=wtPot_CxnBCG
                        , wtPot_Stress=wtPot_Stress, wtPot_CxnStress=wtPot_CxnStress
                        , wtThreat_Fire=wtThreat_Fire, wtThreat_LUdev=wtThreat_LU
                        , wtOpp_Recr=wtOpp_ParksNow, wtOpp_MSCPs=wtOpp_MSCPs
                        , wtOpp_NASVI=wtOpp_NASVI, wtOpp_UserDefined=wtOpp_UserDefined
                        , wt_subPot=wtPot_subidx, wt_subThreat=wtThreat_subidx
                        , wt_subOpp=wtOpp_subidx)
    
    rm(wtPot_BCG, wtPot_CxnBCG, wtPot_Stress, wtPot_CxnStress, wtThreat_Fire
       , wtThreat_LU, wtOpp_ParksNow, wtOpp_MSCPs, wtOpp_NASVI, wtOpp_UserDefined
       , wtOpp_subidx, wtPot_subidx, wtThreat_subidx)
    
    # 06, Set input file names ####
    # Progress, 05
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Input", "; File Names")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    # Required files (after getting user defined stuff)
    fn_TargetCOMIDs     <- file.path(data_dir, "SMC_TestCOMIDs.xlsx")
    # Potential/Restoration Subindex Data
    fn_sites            <- file.path(data_dir, "SMCSitesFinal.tab")
    fn_network          <- file.path(data_dir,"NHDPlusNetwork.xlsx")
    fn_obsIndexBySite   <- file.path(data_dir,"SMCBenthicMetricsFinal.tab")
    fn_Index2BCG        <- file.path(data_dir,"BCG_ProportionalOdds_20200113.xlsx")
    fn_predIndexByReach <- file.path(data_dir,"SCAPE_data.xlsx")
    # Threats Subindex Data
    fn_plannedLU        <- file.path(data_dir, "SMCCatchment_PlannedLU_Percents.xlsx")
    fn_fireHazard       <- file.path(data_dir, "SMCCatchment_FireHazard_Percents.tab")
    fn_currentLU        <- file.path(data_dir, "SMCCatchment_CurrentLU_Percents.xlsx")
    # Opportunities Subindex Data
    fn_MSCP             <- file.path(data_dir, "SMCCatchment_MSCP_CompleteData.xlsx")
    fn_NASVI            <- file.path(data_dir, "SMCCatchment_NASVI_Results.xlsx")
    # Mapping 
    dsn_outline         <- file.path(data_dir,"SMCBoundary")
    lyr_outline         <- "SMCBoundary"
    dsn_flowline        <- file.path(data_dir,"SMCReaches")
    if(boo_Shiny == TRUE){
      lyr_flowline        <- "SMCReaches"
    } else {
      lyr_flowline        <- "SMCReachesNHDv2"
    }## IF ~ boo_Shiny ~ END
    
    # Create results dir, if it doesn't exist
    ifelse(!dir.exists(file.path(results_dir))==TRUE
           , dir.create(file.path(results_dir))
           , FALSE)
    
    # Sites data (general site info, including name, lat, long)
    dfSites <- read.delim(fn_sites, header=TRUE, stringsAsFactors=FALSE, sep="\t")
    sitecols <- c("StationID_Master", "COMID", "FinalLatitude", "FinalLongitude")
    rm(fn_sites)
    
    # NHD+ v2 network data (especially to/from nodes, COMIDs, and lengths)
    dfNetwork <- readxl::read_excel(fn_network, sheet=1, na="NA", trim_ws=TRUE)
    dfNetworkNoData <- dfNetwork[is.na(dfNetwork$FromNode),]
    rm(fn_network)
    
    # 07, Get stressor data from CASTool ####
    # Progress, 06
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Data", "; Stressors")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    if (useCASTresults==TRUE) {
      
      if (exists("dir_CASTdata") && exists("dir_CASTresults")) {
        
        listScaledStr01All <- getScaledStressors(fn_allstress=file.path(dir_CASTdata
                                                                        , fn_allstress)
                                                 , fn_allstressinfo=file.path(dir_CASTdata
                                                                              , fn_allstressmeta)
                                                 , dir_CASTresults=dir_CASTresults)
        
        if (listScaledStr01All$stressorsFound==TRUE) { # Found candidate causes
          
          dfStressInfo <- as.data.frame(listScaledStr01All$df_allSMCStressInfo)
          
          # Check for existing stressor weight file #
          # if (usePrevStressWts==TRUE) {
          dataFiles <- list.files(data_dir)
            if (fn_stresswtsOUT %in% dataFiles) {
              # Display existing file to user; ask if it should be used
                
                # USER INPUT REQUIRED HERE #
              fn_stresswtsIN <- useStrWtsFile()
              if (fn_stresswtsIN=="STOP") {
                  stop()
              }
              
            } else {
              msg <- "No existing stressor weight file in data directory."
              message(msg)
              write.table(listScaledStr01All$df_allSMCStressInfo
                          , fn_stresswtsOUT
                          , append=FALSE
                          , col.names=TRUE, row.names=FALSE, sep="\t")
            } # end No weight file
            
          # } else { # Do not use existing file
          #   write.table(listScaledStr01All$df_allSMCStressInfo
          #               , fn_stresswtsOUT
          #               , append=FALSE
          #               , col.names=TRUE, row.names=FALSE, sep="\t")        
          # } # End check use existing file
          
          # Plot number of samples by year, and ask user to select min and max year
          dfStressPlot <- listScaledStr01All$df_allSMCStressVals[,c("StationID_Master"
                                                                    , "StressSampID"
                                                                    , "StressSampleDate")]
          dfStressPlot <- unique(dfStressPlot)
          
          Stressor_barplot <- drawBarPlot(df.data=dfStressPlot
                                          , fn.plotpath=fn_numsampsyear
                                          , plotType = "bar"
                                          , groupCol="StressSampleDate"
                                          , valCol="StressSampID"
                                          , plot_W=4, plot_H=4, ppi=300
                                          , str_title="Number of stressor samples collected each year"
                                          , str_subtitle="SMC dataset"
                                          , str_ylab="Number of samples"
                                          , str_xlab="Year", str_caption=NULL
                                          , title_size=10, subtitle_size=8
                                          , axistextx_size=8, axistexty_size=8
                                          , caption_size=8)
          
        } else { # No candidate causes found
          msg <- paste0("No candidate causes found. "
                        , "No stressor or stressor connectivity scores "
                        , "will be calculated.")
          message(msg)
        }
        
      } else { # Cannot find CAST directories 
        msg <- "Unable to locate either the CASTool data or results."
        message(msg)
      }
      
    }## useCASTresults ~ END
    
    # Shiny to display p_barplot; ask user to ID min/max years (inclusive)
    # Shiny to display table of stressors (labels, weights) and ask user
    # to alter weights (allowed values = 0, 1, 2)
    
    # NOT WORKING ~~~~~~~~~~~~~~~~~
    # cat(paste0("Edit stressor weights in ", fn_stresswtsOUT
    #            , " and press Enter to continue when ready."))
    # invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # 08, Get stressor scores ####
    # Progress, 07
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Scores", "; Stressors")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    if ((useCASTresults==TRUE) && (listScaledStr01All$stressorsFound==TRUE)) { # User wants to use CAST results
      
      listStressScores <- getStressorScores(dfSites = dfSites
                                            , dfAllStressVals = listScaledStr01All$df_allSMCStressVals
                                            , fnWeights = fn_stresswtsIN
                                            , maxYear = maxYear
                                            , minYear = minYear)
      
      write.table(listStressScores$dfStrScores, fn_StressorScores, append = FALSE
                  , col.names = TRUE, row.names = FALSE, sep = "\t")
      
      write.table(listStressScores$dfWtNormStressRecent, fn_StressScoreDetails
                  , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
      
      reachesWStressorScores <- listStressScores$dfStrScores[,"COMID"]
      
      dfAllScores = listStressScores$dfStrScores
      if (boo_DEBUG==FALSE) {
        rm(fn_StressorScores, fn_StressScoreDetails, fn_allstress, fn_allstressmeta
           , fn_numsampsyear, dfStressPlot, Stressor_barplot, dir_CASTdata
           , dir_CASTresults, fn_stresswtsIN, fn_stresswtsOUT, usePrevStressWts)
      }##IF ~ boo_DEBUG ~ END
    } else {
      listStressScores <- NULL  
    }## IF ~ useCASTResults & listScaledStr01All$stressorsFound  ~ END
    
    
    
    # Get predicted BCG data for reaches, observed BCG data for sites
    # 09, Get BCG scores ####
    # Progress, 08
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("BCG", "; Scores")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    listBCGdata <- getBCGtiers(fn_Index2BCG, fn_predIndexByReach, fn_obsIndexBySite
                               , dfSites)
    Bio_barplot <- drawBarPlot(df.data=listBCGdata$obsSiteBCGxy
                               , fn.plotpath=fn_numbmiyear
                               , plotType = "bar"
                               , groupCol="BMISampleDate"
                               , valCol="BMISampleDate"
                               , plot_W=4, plot_H=4, ppi=300
                               , str_title="Number of biological samples collected each year"
                               , str_subtitle="SMC dataset"
                               , str_ylab="Number of samples"
                               , str_xlab="Year", str_caption=NULL
                               , title_size=10, subtitle_size=8
                               , axistextx_size=8, axistexty_size=8
                               , caption_size=8)
    if (useHWbonus==TRUE) { hwbonus = 1 } else { hwbonus = 0 }
    if (useBCGbonus==TRUE) { bcgbonus = 1 } else { bcgbonus = 0 }
    dfBCGscores <- getBCGScores(dfBCGcutoffs = listBCGdata$BCGcutoff
                                , dfreachBCGobs = listBCGdata$obsReachBCG
                                , dfreachBCGpred = listBCGdata$predReachBCG
                                , dfHWflag = dfNetwork[,c("COMID","StartFlag")]
                                , HWBonus = hwbonus, BCGBonus = bcgbonus
                                , minYear = minYear, maxYear = maxYear)
    
    write.table(dfBCGscores, fn_BCGscores, append = FALSE, col.names = TRUE
                , row.names = FALSE, sep = "\t")
    
    if (exists("dfAllScores")) {
      dfAllScores <- merge(dfBCGscores, dfAllScores, by.x="COMID", by.y="COMID"
                           , all.x=TRUE)
    } else {
      dfAllScores <- dfBCGscores
      dfAllScores$sumStressWts <- NA
      dfAllScores$pot_StressorInd <- NA
      # dfAllScores$WtdStressScore <- NA
    }
    
    if(boo_DEBUG==FALSE) {
      rm(fn_BCGscores, fn_Index2BCG, fn_numbmiyear, fn_obsIndexBySite, Bio_barplot
         , fn_predIndexByReach, useBCGbonus, useHWbonus, bcgbonus, hwbonus
         , maxYear, minYear, dfSites)
    }
    
    # 10, Get Threat Indicators, Subindex scores ####
    # Progress, 09
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Subindex Scores", "; Threats")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    dfThreatScores <- getThreatScores(fn_fireHazard=fn_fireHazard
                                      , fn_plannedLU=fn_plannedLU
                                      , fn_currentLU=fn_currentLU
                                      , useModerateFireHazard=useModerateFireHazard)
    dfAllScores <- merge(dfAllScores, dfThreatScores, by.x="COMID", by.y="COMID"
                         , all.x=TRUE)
    
    if(boo_DEBUG == FALSE){
      rm(fn_fireHazard, fn_plannedLU, useModerateFireHazard, dfThreatScores)
    }
    
    # 11, Get Opportunity Indicators, Subindex scores ####
    # Progress, 10
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Subindex Scores", "; Opportunities")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    dfOpportunityScores <- getOpportunityScores(fn_currentLU=fn_currentLU
                                                , fn_MSCP=fn_MSCP
                                                , fn_NASVI=fn_NASVI)
    dfAllScores <- merge(dfAllScores, dfOpportunityScores, by.x="COMID", by.y="COMID"
                         , all.x=TRUE)
    
    if(boo_DEBUG!=TRUE) {
      rm(fn_currentLU, fn_MSCP, fn_NASVI, dfOpportunityScores)
    }
    
    # Initialize columns needed in AllScores
    dfAllScores$pot_BioCxnInd = NA
    dfAllScores$pot_StressorCxnInd = NA
    
    # 12, ITERATE OVER TARGET REACHES ####
    # Progress, 11
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Calc", "; Iterate over target reaches")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    
    if(boo_Shiny == TRUE){
      dfTargetCOMIDs <- data.frame("TargetCOMID" = TargetCOMID)
    } else {
      dfTargetCOMIDs <- readxl::read_excel(fn_TargetCOMIDs, trim_ws = TRUE
                                           , skip = 0)
      colnames(dfTargetCOMIDs)[1] <- "TargetCOMID"
    }## IF ~ boo_Shiny ~ END
    
    if(boo_DEBUG==TRUE) {
      dfTargetCOMIDs <- dfTargetCOMIDs[dfTargetCOMIDs$TargetCOMID %in% TargetCOMIDs,]
      # dfTargetCOMIDs <- dfTargetCOMIDs[dfTargetCOMIDs$TargetCOMID==20331434,]
    }
    
    for (r in 1:nrow(dfTargetCOMIDs)) {
        
      reach <- dfTargetCOMIDs$TargetCOMID[r]
      userDefOpp <- dfTargetCOMIDs$UserDefinedOpp[r]

      # 13, Get connected reaches ####
      # Progress, 12
      if(boo_Shiny == TRUE){
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- paste0("Calc", "; Connected reaches")
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      
      if (!(reach %in% dfNetwork$COMID)) {
          message(paste0(reach," is not in the NHD Network for the study region."))
      } else if (dfNetwork$FTYPE[dfNetwork$COMID==reach]=="Coastline") {
          message(paste0(reach," is coastline and will not be evaluated."))
      } else {
          message(paste0("Evaluating connectivity for reach ", reach))
          
          dfCxns <- getConnectivity(TargetCOMID = reach
                                    , cxndist_km = cxndist_km
                                    , dfNetwork = dfNetwork
                                    , results_dir = results_dir)
          message(paste0("Connections identified."))

          # 14, Get connectivity scores ####
          # Progress, 13
          if(boo_Shiny == TRUE){
            prog_cnt <- prog_cnt + 1
            prog_msg <- paste0("Step ", prog_cnt)
            prog_det <- paste0("Calc", "; Connectivity Scores")
            incProgress(prog_inc, message = prog_msg, detail = prog_det)
            Sys.sleep(mySleepTime)
            message(paste(prog_msg, prog_det, sep = "; "))
          }## IF ~ boo_Shiny ~ END
    
          if (useCASTresults==TRUE) { # User wants to use CAST results
            if (listScaledStr01All$stressorsFound==TRUE) { # Candidate causes found
              if (reach %in% reachesWStressorScores$COMID) { # Target reach has candidate causes
                useStressorTF=TRUE
              } else { # Target reach has no candidate causes
                useStressorTF=FALSE
              }
            } else { # No candidate causes found in CAST results
              useStressorTF=FALSE
            }
          } else { # User doesn't want to use CAST results 
            useStressorTF=FALSE
          }## IF ~ useCASTresults ~ END
          
        
          if (nrow(dfCxns)==1) { # Isolated reach in NHDPlus
              message(paste0(reach, " has no connected reaches."))
          } else {
              listCxnScores <- getConnectivityScores(TargetCOMID = reach
                                                     , useStressor = useStressorTF
                                                     , useDownStream = TRUE
                                                     , dfCxnData = dfCxns
                                                     , dfBCGData = dfBCGscores
                                                     , listStressData = listStressScores
                                                     , results_dir = results_dir)
              message(paste0("Connection scores calculated."))
              
              dfConnScores <- listCxnScores$dfConnectivityScores
              dfCxnsBCG <- listCxnScores$dfCxnBCG %>%
                  dplyr::select(-c(FTYPE, FromNode, ToNode, StartFlag, AggLengthKM
                                   , UpDown, TotalLength, FractionLength))
              dfCxnsStressors <- listCxnScores$dfCxnStressors
              dfConnScoresDetail <- merge(dfCxnsBCG
                                          , dfCxnsStressors
                                          , by.x=c("TargetCOMID", "COMID")
                                          , by.y=c("TargetCOMID", "COMID")
                                          , all=TRUE)
              rm(dfCxnsBCG, dfCxnsStressors)
              
              dfCxns$TargetCOMID <- reach
              write.table(dfCxns, file.path(results_dir,reach,paste0(reach,"_Cxns.tab"))
                          , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
              
              if (!exists("dfCxnsALL")) {
                  dfCxnsALL <- dfCxns
                  dfCxnsALLdetail <- dfConnScoresDetail
              } else {
                  dfCxnsALL <- rbind(dfCxnsALL, dfCxns)
                  dfCxnsALLdetail <- rbind(dfCxnsALLdetail, dfConnScoresDetail)
                  
              }
          }
          
          if(exists("dfConnScores")){
              dfAllScores <- dfAllScores %>%
                  dplyr::mutate(pot_BioCxnInd = ifelse(COMID==dfConnScores$COMID
                                                       , dfConnScores$pot_BioCxnInd
                                                       , pot_BioCxnInd)
                                , pot_StressorCxnInd = ifelse(COMID==dfConnScores$COMID
                                                              , dfConnScores$pot_StressorCxnInd
                                                              , pot_StressorCxnInd))
          } else {
              dfAllScores$pot_BioCxnInd[dfAllScores$COMID==reach] <- NA
              dfAllScores$pot_StressorCxnInd[dfAllScores$COMID==reach] <- NA
          }
          
          # Add User-defined opportunity (BPJ)
          dfAllScores$opp_UserDefInd[dfAllScores$COMID==reach] <- ifelse(is.null(userDefOpp), NA, userDefOpp)
          # dfAllScores$opp_UserDefInd <- ifelse(dfAllScores$COMID==reach
          #                   , ifelse(is.null(userDefOpp), NA, userDefOpp)
          #                   , dfTargetCOMIDs$UserDefinedOpp[dfTargetCOMIDs$COMID==reach])
      } # reach in NHD network; evaluate
      
    }## FOR ~ r ~ END # Finish looping over target reaches
    
    # 15, Clean up connections ####
    # 15, Clean up connections data table, write connections and connections details
    # Progress, 15
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Calc", "; Clean Up")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    if (nrow(dfCxnsALL)>1) {
        dfCxnsALL <- dplyr::filter(dfCxnsALL, UpDown!="Origin")
        write.table(dfCxnsALL, fn_cxnsALL, append = FALSE, col.names = TRUE
                    , row.names = FALSE, sep = "\t")
        write.table(dfCxnsALLdetail, fn_cxnscoredetail, append = FALSE, col.names = TRUE
                    , row.names = FALSE, sep = "\t")
    }
    
    if(!boo_DEBUG){
      rm(dfConnScores, dfConnScoresDetail, dfCxns, dfCxnsALLdetail, dfBCGscores
         , fn_cxnsALL, fn_cxnscoredetail, fn_TargetCOMIDs, r, reach, useDownstream
         , useStressorTF, userDefOpp)
      #, useStressorTF, userDefOpp, useCASTresults, reachesWStressorScores)
    }
    
    # 16, Make updateable All Scores table ####
    # Progress, 16
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Calc", "; update all scores table")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    
    listAllScores <- updateAllScoresTable(dfAllScores, listWeights, fn_allscores
                                          , BioDegBrk=c(-2, 0.799, 2)
                                          , BioDegLab=c("Degraded", "Not degraded"))
    
    # ITERATE OVER TARGET REACHES NOW WITH SCORES #
    # 17, Create Target Reach-specific score graphics and maps ####
    # Progress, 17
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Target Reach", "; graphics and maps")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    
    # Simplify lists to only needed dataframes
    dfAllSites = listBCGdata$obsSiteBCGxy
    dfAllScoresSummary = listAllScores$dfAllScoresSummary
    if(useCASTresults==TRUE){
      dfWoE = listScaledStr01All$dfWoE
    }else{
      dfWoE=NULL
      dfStressInfo=NULL
    }
    
    # Remove objects no longer needed
    if(!boo_DEBUG){
      # rm(listAllScores, listBCGdata, listCxnScores, listScaledStr01All
      rm(listAllScores, listBCGdata, listCxnScores
         , listStressScores, listWeights, dfAllScores, dfNetwork
         , dfNetworkNoData, myDate, sitecols)
    }
    
   
    for (r in 1:nrow(dfTargetCOMIDs)) {
      
      TargetReach <- dfTargetCOMIDs$TargetCOMID[r]

      if (!(TargetReach %in% dfAllScoresSummary$COMID)) {
          message(paste0("No scores are available for ", TargetReach))
      } else {
        # Draw score graphics
        message(paste0("Generating score graphics for ", TargetReach))
        drawAllScoresPlot(TargetReach = TargetReach
                            , allScores = dfAllScoresSummary
                            , dfSiteInfo = dfAllSites
                            , dfStressors = dfWoE
                            , dfStressorInfo = dfStressInfo
                            , results_dir = results_dir)

        # Draw maps
        message(paste0("Generating maps for ", TargetReach))
        leafMap <- getReachMap(dsn_outline = dsn_outline
                               , lyr_outline = lyr_outline
                               , dsn_flowline = dsn_flowline
                               , lyr_flowline = lyr_flowline
                               , allSites = dfAllSites
                               , allCxns = dfCxnsALL
                               , allScores = dfAllScoresSummary
                               , cxndist_km = cxndist_km
                               , TargetCOMID=TargetReach
                               , results_dir=results_dir
                               , plotLMAP=FALSE)
        
        # Write connected reaches score summary table
        cxnScoresSummary <- dfAllScoresSummary[dfAllScoresSummary$COMID %in% dfCxnsALL$COMID,]
        targetScoresSummary <- dfAllScoresSummary[dfAllScoresSummary$COMID==TargetReach,]
        cxnScoresSummary <- rbind(targetScoresSummary, cxnScoresSummary)
        fn_cxnScoresSummary <- file.path(results_dir,TargetReach
                                         ,paste0(TargetReach,"_CxnScoresSummary.tab"))
        write.table(cxnScoresSummary, fn_cxnScoresSummary, append=FALSE
                    , col.names=TRUE, row.names=FALSE, sep="\t")
        rm(cxnScoresSummary, targetScoresSummary, fn_cxnScoresSummary)
        
      }
      
    }## FOR ~ r ~ END
    
    # 18, Calc, Run time ####
    # Progress, 18
    if(boo_Shiny == TRUE){
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- paste0("Finish", "; Calc Time")
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    end.time <- Sys.time()
    elapsed.time <- end.time - start.time
    message(paste0("Complete; elapsed time = ", format.difftime(elapsed.time)))

