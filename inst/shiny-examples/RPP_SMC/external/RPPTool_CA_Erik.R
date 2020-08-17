# RPPTool (Specific for SMC)
# Erik.Leppo@tetratech.com
# Ann.RoseberryLincoln@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 
# library(devtools)
# install_github("leppott/CASTfxn")

#rm(list=ls())

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal)
library(viridis)
library(maptools)
library(leaflet)
# removed
# library(broom)

#gitpath <- "C:/Users/Erik.Leppo/OneDrive - Tetra Tech, Inc/MyDocs_OneDrive/GitHub/RPPTool"
dn_ext <- file.path(".", "external")

# Source required functions ####
wd <- getwd()
source(file.path(dn_ext,"getScaledStressors.R"))
source(file.path(dn_ext,"getStressorScores.R"))
source(file.path(dn_ext,"getBCGtiers.R"))
source(file.path(dn_ext,"getBCGScores.R"))
source(file.path(dn_ext,"drawBarPlot.R"))
source(file.path(dn_ext,"getConnectivity.R"))
source(file.path(dn_ext,"getConnectivityScores.R"))
source(file.path(dn_ext,"getReachMap.R"))

# define pipe
#`%>%` <- dplyr::`%>%`  # Global
# not_all_na <- function(x) {!all(is.na(x))}
boo_DEBUG = FALSE
if (boo_DEBUG==TRUE) {
    TargetCOMIDs=c(20325195, 20329746, 20331170, 20331434, 20333052, 22549067)
}
# myDate <- lubridate::ymd(lubridate::today())
# myDate <- stringr::str_replace_all(myDate, "-", "")
myDate <- format(Sys.Date(), "%Y%m%d")
start.time <- Sys.time()

# Dir, RPPTool ####
#sep_rpp_dir <- "C:/Users/ann.lincoln/Documents/SEP_RPP"
sep_rpp_dir <- file.path(".")
data_dir <- file.path(sep_rpp_dir,"Data")
results_dir <- file.path(sep_rpp_dir,"Results")

# OUTPUT, FileNames ####
# These are not expected to be user-modified
fn_numsampsyear <- file.path(results_dir,"NumSamplesByYear.png")
fn_stresswtsOUT <- file.path(data_dir,"StressorWeights.tab")
fn_BCGscores <- file.path(results_dir,paste0("RPPTool_BCGScores_",myDate,".tab"))
fn_BCGhistograms <- file.path(results_dir,paste0("RPPToolBCGScores_",myDate,".png"))
fn_StressorScores <- file.path(results_dir
                               ,paste0("RPPTool_StressorScores_",myDate,".tab"))
fn_StressScoreDetails <- file.path(results_dir
                                   ,paste0("RPPTool_StressorScoreDetails_"
                                           ,myDate,".tab"))
fn_cxnsALL <- file.path(results_dir,paste0("RPPTool_AllConnectedReaches_"
                                           ,myDate,".tab"))
fn_cxnscoredetail <- file.path(results_dir
                               , paste0("RPPTool_AllConnectivityScoreDetails_"
                                                   , myDate, ".tab"))
fn_allscores <- file.path(results_dir,paste0("RPPTool_AllScores_",myDate,".tab"))



# INPUT, User ####
# Is it possible to have a browse button, so the user doesn't have to type it?
useCASTresults <- TRUE
if (useCASTresults==TRUE) {
    # Input Files
    ## CAST, Input
    dir_CASTdata <- "C:/Users/ann.lincoln/Documents/SEP_CAST/Data" # Obtained from user
    fn_allstress <- "SMC_AllStressData.tab" # Change only if file name differs
    fn_allstressmeta <- "SMC_AllStressInfo.tab" # Change only if file name differs
    ## CAST, Results
    dir_CASTresults <- "C:/Users/ann.lincoln/Documents/SEP_CAST/Results" # Obtained from user
    fn_stresswtsIN <- fn_stresswtsOUT # Change only if a copy of the weights file is prepared
    ## User Input
    maxYear <- lubridate::year(Sys.Date()) # Obtained from user (NOTE: this is inclusive)
    minYear <- maxYear - 12 # Inclusive (defaults to 2008 to present on 4/17/2020)
    usePrevStressWts <- FALSE
}











# Connectivity variables
cxndist_km <- 5
useHWbonus <- 0 # FALSE (default)
useBCGbonus <- 0 # FALSE (default)
useDownstream <- 0 # FALSE (default)

# Indicator weights
wtPot_BCG <- 1
wtPot_CxnBCG <- 1
wtPot_Stress <- 1
wtPot_CxnStress <- 1
wtThreat_Fire <- 1
wtThreat_LU <- 1 # Probably need to separate out categories
wtOpp_ParksNow <- 1
wtOpp_MSCPs <- 1
wtOpp_NASVI <- 1
wtOpp_UserDefined <- 1

# INPUT, FileNames ####
# Required files (after getting user defined stuff)
fn_TargetCOMIDs <- file.path(data_dir, "SMC_TestCOMIDs.xlsx")
fn_sites <- file.path(data_dir, "SMCSitesFinal.tab")
fn_network <- file.path(data_dir,"NHDPlusNetwork.xlsx")
fn_obsIndexBySite <- file.path(data_dir,"SMCBenthicMetricsFinal.tab")
fn_Index2BCG <- file.path(data_dir,"BCG_ProportionalOdds_20200113.xlsx")
fn_predIndexByReach <- file.path(data_dir,"SCAPE_data.xlsx")
dsn_outline <- "Data/SMCBoundary"
lyr_outline <- "SMCBoundary_aea"
proj_wgs84 <- "+proj=longlat +datum=WGS84 +no_def"
dsn_flowline <- "Data/SMCReaches"
lyr_flowline <- "SMCReaches_aes"

# Prep outline/flowline shapefiles for maps
# sp_outline <- rgdal::readOGR(dsn = "Data/SMCBoundary", layer = "SMCBoundary_aea")
# sp_outline_wgs <- spTransform(sp_outline, CRS("+proj=longlat +datum=WGS84 +no_def"))
# outlines <- list(outline_aea = sp_outline, outline_wgs = sp_outline_wgs)

# Prepare SMC flowlines
# sp_flowline <- rgdal::readOGR(dsn = "Data/SMCReaches", layer = "SMCReaches_aea")
# sp_flowline_wgs <- spTransform(sp_flowline, CRS("+proj=longlat +datum=WGS84 +no_def"))
# flowlines <- list(flowline_aea = sp_flowline, flowline_wgs = sp_flowline_wgs)

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

# Calc, stressor data from CASTool ####
if (useCASTresults==TRUE) {
    
    if (exists("dir_CASTdata") && exists("dir_CASTresults")) {
        
        listScaledStr01All <- getScaledStressors(fn_allstress=file.path(dir_CASTdata
                                                                        , fn_allstress)
                                                 , fn_allstressinfo=file.path(dir_CASTdata
                                                                              , fn_allstressmeta)
                                                 , dir_CASTresults=dir_CASTresults)
        
        if (listScaledStr01All$stressorsFound==TRUE) { # Found candidate causes
            
            dfStressInfo <- as.data.frame(listScaledStr01All$df_allSMCStressInfo)
            
            # Check for existing stressor weight file ####
            if (usePrevStressWts==TRUE) {
                if (file.exists("fn_stresswtsOUT")) {
                    # Display existing file to user; ask if it should be used
                    fn_stresswtsIN = fn_stresswtsOUT
                } else {
                    msg <- "No existing stressor weight in data directory."
                    message(msg)
                    #print(msg)
                    # flush.console()
                    write.table(listScaledStr01All$df_allSMCStressInfo, fn_stresswtsOUT
                                , append=FALSE
                                , col.names=TRUE, row.names=FALSE, sep="\t")
                } # end No weight file
                
            } else { # Do not use existing file
                
                write.table(listScaledStr01All$df_allSMCStressInfo, fn_stresswtsOUT
                            , append=FALSE
                            , col.names=TRUE, row.names=FALSE, sep="\t")        
            } # End check use existing file
            
            # Plot number of samples by year, and ask user to select min and max year
            dfStressPlot <- listScaledStr01All$df_allSMCStressVals[,c("StationID_Master"
                                                                      , "StressSampID"
                                                                      , "StressSampleDate")]
            dfStressPlot <- unique(dfStressPlot)
            
            p_barplot <- drawBarPlot(df.data=dfStressPlot, fn.plotpath=fn_numsampsyear
                                     , plotType = "bar"
                                     , groupCol="StressSampleDate"
                                     , valCol="StressSampID"
                                     , plot_W=4, plot_H=4, ppi=300
                                     , str_title="Number of samples collected each year"
                                     , str_subtitle="SMC dataset"
                                     , str_ylab="Number of samples"
                                     , str_xlab="Year", str_caption=NULL
                                     , title_size=10, subtitle_size=8
                                     , axistextx_size=8, axistexty_size=8
                                     , caption_size=8)
            
        } else { # No candidate causes found
            msg <- paste0("No candidate causes found. "
                       , "No stressor or stressor connecitivity scores "
                       , "will be calculated.")
            message(msg)
            #print(msg)
            # flush.console()
        }
        
    } else { # Cannot find CAST directories 
        msg <- "Unable to locate either the CASTool data or results."
        message(msg)
        #print(msg)
        # flush.console()
    }
    
}

# *USER INPUT REQUIRED* ####
# Shiny to display p_barplot; ask user to ID min/max years (inclusive)
# Shiny to display table of stressors (labels, weights) and ask user
# to alter weights (allowed values = 0, 1, 2)

# NOT WORKING ~~~~~~~~~~~~~~~~~
# cat(paste0("Edit stressor weights in ", fn_stresswtsOUT
#            , " and press Enter to continue when ready."))
# invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calc, stressor scores ####
if ((useCASTresults==TRUE) && (listScaledStr01All$stressorsFound==TRUE)) { # User wants to use CAST results
    
    listStressScores <- getStressorScores(dfSites = dfSites
                                          , dfAllStressVals = listScaledStr01All$df_allSMCStressVals
                                          , fnWeights = fn_stresswtsIN)
    
    write.table(listStressScores$dfStrScores, fn_StressorScores, append = FALSE
                , col.names = TRUE, row.names = FALSE, sep = "\t")
    
    write.table(listStressScores$dfWtNormStressRecent, fn_StressScoreDetails
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
    
    reachesWStressorScores <- listStressScores$dfStrScores[,"COMID"]
    
    dfAllScores = listStressScores$dfStrScores
    
}
if (boo_DEBUG==FALSE) {
    rm(fn_StressorScores, fn_StressScoreDetails, fn_allstress, fn_allstressmeta
       , fn_numsampsyear, dfStressPlot, p_barplot)
}

# Get predicted BCG data for reaches, observed BCG data for sites
# Calc, BCG scores ####
listBCGdata <- getBCGtiers(fn_Index2BCG, fn_predIndexByReach, fn_obsIndexBySite
                           , dfSites)
if (useHWbonus==TRUE) { hwbonus = 1 } else { hwbonus = 0 }
if (useBCGbonus==TRUE) { bcgbonus = 1 } else { bcgbonus = 0 }
dfBCGscores <- getBCGScores(dfBCGcutoffs = listBCGdata$BCGcutoff
                            , dfreachBCGobs = listBCGdata$obsReachBCG
                            , dfreachBCGpred = listBCGdata$predReachBCG
                            , dfHWflag = dfNetwork[,c("COMID","StartFlag")]
                            , HWBonus = hwbonus, BCGBonus = bcgbonus
                            , minYear = minYear, maxYear = maxYear)

# Plot results
dfBCGscorePlot <- dfBCGscores[,c("COMID","BioScoreRestore","BioScoreProtect")] %>%
    dplyr::filter(!is.na(BioScoreRestore)) %>%
    tidyr::gather(key="Type", value="Score", -COMID)
numReaches = length(unique(as.vector(dfBCGscorePlot$COMID)))
BCGplot <- drawBarPlot(df.data = dfBCGscorePlot
                       , fn.plotpath = fn_BCGhistograms
                       , plotType = "histogram"
                       , groupCol="Type", valCol="Score"
                       , plot_W=4, plot_H=4, ppi=300
                       , str_title = "Histogram of biology scores for protection or restoration"
                       , str_subtitle = "SMC Region"
                       , str_xlab = "Normalized Biology Score"
                       , str_ylab = paste0("Number of reaches (total scored: "
                                           , numReaches, ")")
                       , str_caption = NULL, title_size=10, subtitle_size=8
                       , axistextx_size=8, axistexty_size=8, caption_size=8)

write.table(dfBCGscores, fn_BCGscores, append = FALSE, col.names = TRUE
            , row.names = FALSE, sep = "\t")

if (exists("dfAllScores")) {
    dfAllScores <- merge(dfBCGscores, dfAllScores, by.x="COMID", by.y="COMID"
                         , all.x=TRUE)
} else {
    dfAllScores <- dfBCGscores
    dfAllScores$StressorScore <- NA
    dfAllScores$WtdStressScore <- NA
}

if(boo_DEBUG==FALSE) {
    rm(BCGplot, dfBCGscorePlot, fn_BCGhistograms, fn_BCGscores, fn_obsIndexBySite
       , fn_predIndexByReach, numReaches, useBCGbonus, useHWbonus, bcgbonus, hwbonus)
}


# Calc, ThreatSubindex ####

# Calc, OpportunitySubindex ####


# Initialize columns needed in AllScores
dfAllScores$BioCxnScore = NA
dfAllScores$StressorCxnScore = NA

# ITERATE OVER TARGET REACHES ####
dfTargetCOMIDs <- readxl::read_excel(fn_TargetCOMIDs, trim_ws = TRUE
                                     , skip = 0)
if(boo_DEBUG==TRUE) {
    dfTargetCOMIDs <- dfTargetCOMIDs[dfTargetCOMIDs$TargetCOMID %in% TargetCOMIDs,]
}

for (r in 1:nrow(dfTargetCOMIDs)) {
    reach <- dfTargetCOMIDs$TargetCOMID[r]
    
    if (reach %in% dfNetworkNoData$COMID) {
        print(paste0("Reach ", reach, " has no data in the NHDPlus network. "
                     , "Connectivity cannot be determined."))
        flush.console()
        
        # Add reach to dfCxns and to dfCxns and dfConnScoresDetail
        
        next
    } else {
        print(paste0("Evaluating reach ", reach))
        flush.console()
    }

    # Calc, connected reaches ####
    dfCxns <- getConnectivity(TargetCOMID = reach
                              , cxndist_km = cxndist_km
                              , dfNetwork = dfNetwork
                              , results_dir = results_dir)
    print(paste0("Connections identified."))
    flush.console()
    
    # Calc, connectivity scores ####
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
    }
    
    listCxnScores <- getConnectivityScores(TargetCOMID = reach
                                         , useStressor = useStressorTF
                                         , useDownStream = TRUE
                                         , dfCxnData = dfCxns
                                         , dfBCGData = dfBCGscores
                                         , listStressData = listStressScores
                                         , results_dir = results_dir)
    print(paste0("Connection scores calculated."))
    flush.console()
    
    dfConnScores <- listCxnScores$dfConnectivityScores
    dfConnScoresDetail <- merge(listCxnScores$dfCxnsBCG
                                , listCxnScores$dfCxnsStressors
                                , all=TRUE)
    dfCxns$TargetCOMID <- reach
    write.table(dfCxns, file.path(results_dir,reach,paste0(reach,"_Cxns.tab"))
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")

    if (r==1) {
        dfCxnsALL <- dfCxns
        dfCxnsALLdetail <- dfConnScoresDetail
    } else {
        dfCxnsALL <- rbind(dfCxnsALL, dfCxns)
        dfCxnsALLdetail <- rbind(dfCxnsALLdetail, dfConnScoresDetail)
    }

    dfAllScores <- dfAllScores %>%
        dplyr::mutate(BioCxnScore = ifelse(COMID==dfConnScores$COMID
                                           , dfConnScores$BCGCxnScore
                                           , BioCxnScore)
                      , StressorCxnScore = ifelse(COMID==dfConnScores$COMID
                                              , dfConnScores$StressCxnScore
                                              , StressorCxnScore))
    
} # Finish looping over target reaches

write.table(dfCxnsALL, fn_cxnsALL, append = FALSE, col.names = TRUE
            , row.names = FALSE, sep = "\t")
write.table(dfCxnsALLdetail, fn_cxnscoredetail, append = FALSE, col.names = TRUE
            , row.names = FALSE, sep = "\t")

dfAllScores <- dfAllScores %>% # Currently not working!
    dplyr::mutate(WtdBioScoreRestore = ifelse(is.na(BioScoreRestore), NA
                                            , BioScoreRestore * eval(wtPot_BCG))
                  , WtdBioScoreProtect = ifelse(is.na(BioScoreProtect), NA
                                              , BioScoreProtect * eval(wtPot_BCG))
                  , WtdStressorScore = ifelse(is.na(StressorScore), NA
                                              , StressorScore * eval(wtPot_Stress))
                  , WtdBioCxnScore = ifelse(is.na(BioCxnScore), NA
                                              , BioCxnScore * eval(wtPot_CxnBCG))
                  , WtdStressorCxnScore = ifelse(is.na(StressorCxnScore), NA
                                            , StressorCxnScore * eval(wtPot_CxnBCG)))

dfAllScores$Potential_Restore = rowMeans(dfAllScores[,c("WtdBioScoreRestore"
                                                   , "WtdStressorScore"
                                                   , "WtdBioCxnScore"
                                                   , "WtdStressorCxnScore")]
                                         , na.rm = TRUE)
dfAllScores$Potential_Protect = rowMeans(dfAllScores[,c("WtdBioScoreProtect"
                                                        , "WtdStressorScore"
                                                        , "WtdBioCxnScore"
                                                        , "WtdStressorCxnScore")]
                                         , na.rm = TRUE)

# dfAllScores <- dfAllScores %>%
#     dplyr::select(COMID, WtdRestBCGscore, WtdProtBCGscore, WtdStressScore
#                   , WtdConnScore, RestPotSubIdx, ProtPotSubIdx)

nonetwork <- as.vector(dfNetworkNoData$COMID)
targets <- as.vector(dfTargetCOMIDs$TargetCOMID)
dfAllScores <- dfAllScores %>%
    dplyr::mutate(Comment = ifelse((COMID %in% nonetwork)
                                   , "Not in NHDPlus network", 
                                   ifelse((COMID %in% targets)
                                          , "Evaluated connectivity"
                                          , "Included base data only")))

write.table(dfAllScores, fn_allscores, append = FALSE, col.names = TRUE
            , row.names = FALSE, sep = "\t")



end.time <- Sys.time()
elapsed.time <- end.time - start.time
print(paste0("Elapsed time = ", format.difftime(elapsed.time)))
flush.console()
stop()

# Question: Is there a way to highlight connected reaches when a user clicks on 
# a target reach for which connected reaches have been identified?

# Layers to show on final map:
# base layers (roads, terrain)
# SMC outline
# SMC reaches
# Final RPP score
# Potential subindex score (Restoration/Protection separately?)
# Threats subindex score
# Opportunities subindex score

# Link to table?

# map ####
map <- getReachMap(proj = proj_wgs84
                   , dsn_boundary = dsn_outline
                   , lyr_boundary = lyr_outline
                   , dsn_reaches = dsn_flowline
                   , lyr_reaches = lyr_flowline
                   , allSites = dfObsCSCI_BCG_xy
                   , allCxns = dfCxnsALL
                   , TargetCOMID = TargetCOMID)
