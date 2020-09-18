#  Copyright 2020 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents 
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Biological Stressor Responses
#' 
#' @description Get Biological (Algae or BMI) stressor responses.
#' 
#' @details Biological (Algae or BMI) stressor regressions.
#' 
#' @param TargetSiteID Site ID
#' @param stressors stressors
#' @param stressorInfo stressor info
#' @param BioResp Biological response variables.  For example, BMI metrics or Algae metrics.
#' @param BioInfo Bio info
#' @param list.MatchBioData list of matched biological (BMI or algae) and stressor data.
#' @param ref.sites Reference sites.
#' @param siteQual2Plot site quality to plot
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param dir_results Directory to save plots.  Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "StressorResponse"
#' @param boo_pred_warn Should warnings for prediction be suppressed.  Default = TRUE.
# @param LogTransf Value for if stressor variables should be log10 transformed; 1=TRUE, 0=FALSE.
#' 
#' @return A jpg in SiteID subfolder of the "Results" folder of working directory.  
#' And two tab-delimited text files; stressor correlations and scores.
#' 
# @importFrom pryr "%<a-%"
#' 
#' @examples
#' \dontrun{
#' # Example 1, BMI
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
#' biocomm <- "bmi"
#' 
#' # datasets getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites       # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.mod           <- data_ReachMod
#' 
#' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
#'                     , "ElevCategory"])
#' if(elev_cat=="HI"){
#'    data.cluster <- data_Cluster_Hi
#' } else if(elev_cat=="LO") {
#'    data.cluster <- data_Cluster_Lo
#' }
#' 
#' # Map data
#' # San Diego
#' #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
#' #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
#' # AZ
#' map_flowline  <- data_GIS_Flow_HI
#' map_flowline2 <- data_GIS_Flow_LO
#' if(elev_cat=="HI"){
#'    map_flowline <- data_GIS_Flow_HI
#' } else if(elev_cat=="LO") {
#'    map_flowline <- data_GIS_Flow_LO
#' }
#' map_outline   <- data_GIS_AZ_Outline
#' # Project site data to USGS Albers Equal Area
#' usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#'               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' # projection for outline
#' my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
#'            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' map_proj <- my.aea
#' #
#' dir_sub <- "SiteInfo"
#' 
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
#'                                 , data.SampSummary, data.303d.ComID
#'                                 , data.bmi.metrics, data.algae.metrics
#'                                 , data.cluster, data.mod
#'                                 , map_proj, map_outline, map_flowline
#'                                 , dir_sub=dir_sub)
#' 
#' # Data getChemDataSubsets
#' # data import, example 
#' # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
#' # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
#'                                 , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
#'                                 , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
#' 
#' # Data getStressorList
#' chem.info <- list.data$chem.info
#' cluster.chem <- list.data$cluster.chem
#' cluster.samps <- list.data$cluster.samps
#' ref.sites <- list.data$ref.sites
#' site.chem <- list.data$site.chem
#' dir_sub <- "CandidateCauses"
#' 
#' # set cutoff for possible stressor identification
#' probsLow <- 0.10
#' probsHigh <- 0.90 
#' 
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, site.chem
#'                                  , probsHigh, probsLow, biocomm, dir_results
#'                                  , dir_sub)
#'                                  
#' # Data getBioMatches, BMI
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
#' LogTransf <- stressors_logtransf
#' data.bio.metrics <- data_BMIMetrics
#' 
#' # Run getBioMatches
#' list.MatchBioData <- getBioMatches(stressors, list.data, list.SiteSummary, data.SampSummary
#'                                    , data.chem.raw, data.bio.metrics, biocomm)
#'   
#' # Data getBioStressorResponses, BMI 
#' BioResp <- c("IBI", "TotalTaxSPL_Sc", "DipTaxSPL_Sc"
#'              , "IntolTaxSPL_Sc", "HBISPL_Sc", "PlecoPct_Sc", "ScrapPctSPL_Sc"
#'              , "TrichTax_Sc", "EphemTax_Sc", "EphemPct_Sc", "Dom01PctSPL_Sc") 
#' dir_sub <- "StressorResponse"
#'              
#' # Run getBioStressorResponses, BMI               
#' getBioStressorResponses(TargetSiteID, stressors, BioResp, list.MatchBioData
#'                         , LogTransf, ref.sites, biocomm, dir_results, dir_sub)
#' 
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, Algae
#' TargetSiteID <- "LCBEN002.57"
#' dir_results <- file.path(getwd(), "Results")
#' biocomm <- "algae"
#' 
#' # datasets getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites       # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.mod           <- data_ReachMod
#' 
#' #' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
#'                     , "ElevCategory"])
#' if(elev_cat=="HI"){
#'    data.cluster <- data_Cluster_Hi
#' } else if(elev_cat=="LO") {
#'    data.cluster <- data_Cluster_Lo
#' }
#' 
#' # Map data
#' # San Diego
#' #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
#' #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
#' # AZ
#' map_flowline  <- data_GIS_Flow_HI
#' map_flowline2 <- data_GIS_Flow_LO
#' if(elev_cat=="HI"){
#'    map_flowline <- data_GIS_Flow_HI
#' } else if(elev_cat=="LO") {
#'    map_flowline <- data_GIS_Flow_LO
#' }
#' map_outline   <- data_GIS_AZ_Outline
#' # Project site data to USGS Albers Equal Area
#' usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#'               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' # projection for outline
#' my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
#'            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' map_proj <- my.aea
#' #
#' dir_sub <- "SiteInfo"
#' 
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
#'                                 , data.SampSummary, data.303d.ComID
#'                                 , data.bmi.metrics, data.algae.metrics
#'                                 , data.cluster, data.mod
#'                                 , map_proj, map_outline, map_flowline
#'                                 , dir_sub=dir_sub)
#' 
#' # Data getChemDataSubsets
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' dir_sub <- "CandidateCauses"
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
#'                                 , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
#'                                 , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
#' 
#' # Data getStressorList
#' chem.info <- list.data$chem.info
#' cluster.chem <- list.data$cluster.chem
#' cluster.samps <- list.data$cluster.samps
#' ref.sites <- list.data$ref.sites
#' site.chem <- list.data$site.chem
#' 
#' # set cutoff for possible stressor identification
#' probsLow <- 0.10
#' probsHigh <- 0.90 
#' biocomm <- "algae"
#' 
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, site.chem
#'                                  , probsHigh, probsLow, biocomm, dir_results)
#'                                  
#' # Data getBioMatches, Algae
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
#' LogTransf <- stressors_logtransf
#' data.bio.metrics <- data_AlgMetrics
#'
#' # Run getBioMatches, Algae
#' list.MatchBioData <- getBioMatches(stressors, list.data, list.SiteSummary, data.SampSummary
#'                                   , data.chem.raw, data.bio.metrics, biocomm)
#' 
#' # Data getBioStressorResponses, Algae
#' BioResp <- colnames(data.bio.metrics[6:52])
#' dir_sub <- "StressorResponse"
#' 
#' # Run getBioStressorResponses, Algae
#' getBioStressorResponses(TargetSiteID, stressors, BioResp, list.MatchBioData
#'                        , LogTransf, ref.sites, biocomm, dir_results, dir_sub)
#' }
#
#' @export
getBioStressorResponses <- function(TargetSiteID
                                    , stressors
                                    , stressorInfo=siteStressInfo
                                    , BioResp
                                    , BioInfo
                                    , list.MatchBioData
                                    , ref.sites
                                    , siteQual2Plot
                                    , biocomm="bmi"
                                    , dir_results=file.path(getwd(), "Results")
                                    , dir_sub="StressorResponse"
                                    , boo_pred_warn = TRUE
                                    ) {##FUNCTION.START
  # DEBUG
  boo.DEBUG <- FALSE
  ## Trigger DEBUG actions below for when debugging.
  
  if (boo.DEBUG == TRUE) {
      TargetSiteID
      stressors <-stressorsWPairedResponses
      stressorInfo <- siteStressInfo
      BioResp = bioMetricNames
      BioInfo = bioMetricInfo
      list.MatchBioData = list_MatchBioData
      ref.sites=allBioRefStressSamps
      siteQual2Plot=siteQual2Plot
      biocomm=bioComm
      dir_results=dir_results
      dir_sub="StressorResponse"
      boo_pred_warn = TRUE
  }

  # # list output
  # all.b.str  <- list.MatchBioData[["all.b.str"]]
  # all.b.rsp  <- list.MatchBioData[["all.b.rsp"]]
  # cl.b.str   <- list.MatchBioData[["cl.b.str"]]
  # cl.b.rsp   <- list.MatchBioData[["cl.b.rsp"]]
  # site.b.str <- list.MatchBioData[["site.b.str"]]
  # site.b.rsp <- list.MatchBioData[["site.b.rsp"]]
  
  
  # Correlation file output header row
  cn_cor_pref <- c("StationID_Master", "biocomm", "stressName", "stressLabel"
                   ,"respName","respLabel", "n", "statistic", "p.value"
                   , "estimate", "r2")
  
  
  # Community ####
  biocomm <- toupper(biocomm)
  not_all_na <- function(x) {!all(is.na(x))}
  # Define pipe
  `%>%` <- dplyr::`%>%`
  
  # Check for no data
  # if(biocomm=="BMI"){##IF.biocomm.START
    #
    col_Bio_Metrics_SampID <- "RespSampID"
    min_cases <- 20
    all.x.str  <- "all.b.str"
    cl.x.str   <- "cl.b.str"
    site.x.str <- "site.b.str"
    all.x.rsp  <- "all.b.rsp"
    cl.x.rsp   <- "cl.b.rsp"
    site.x.rsp <- "site.b.rsp"
    #
    qc_row_site_rsp <-  nrow(list.MatchBioData$site.b.rsp)
    #
    # missing stressors
    stressors_missing <- stressors[!(stressors %in% names(list.MatchBioData$all.b.str))]
    stressors <- stressors[stressors %in% names(list.MatchBioData$all.b.str)]
    #
  
  # QC, site rsp ####
  #qc_row_site_rsp <-  nrow(list.MatchBioData$site.a.rsp)
  if(qc_row_site_rsp==0){##IF~qc_row_site_rsp~START
    msg_Stop_site_rsp <- paste0("list.MatchBioData does not contain "
                                , "any site response data for the specified "
                                , "biological community (", biocomm, ").")
    stop(msg_Stop_site_rsp)
  }##IF~qc_row_site_rsp~START
  
  # QC, bad stressors
  if(length(stressors_missing)>0){##IF~length(stressors_missing)~START
    msg_warn_stressors_missing <- paste0("The following stressors are missing "
                                         , "from the 'list.MatchBioData' input:\n"
                                         , paste(stressors_missing, collapse=", "))
    warning(msg_warn_stressors_missing)
  }##IF~length(stressors_missing)~END
  
  
  #{
  # QC, NUMERIC ####
  all.x.str_numeric <- unlist(lapply(list.MatchBioData[[all.x.str]], is.numeric)) 
  all.x.rsp_numeric <- unlist(lapply(list.MatchBioData[[all.x.rsp]], is.numeric)) 
  #
  str_numeric <- all.x.str_numeric[stressors]
  rsp_numeric <- all.x.rsp_numeric[BioResp]
  #
  str_num_false <- names(str_numeric[str_numeric==FALSE])
  rsp_num_false <- names(rsp_numeric[rsp_numeric==FALSE])
  #
  msg_stop_base    <- "One or more input variables are non-numeric; "
  #
  if(length(str_num_false)==0){##IF~len_str~START
  } else {
    msg_stop_num_str <- paste0(msg_stop_base, "stressors: "
                               , paste(str_num_false, collapse=", "))
    stop(msg_stop_num_str)
  }##IF~len_str~END
  #
  if (length(rsp_num_false)==0) {##IF~len_rsp~START
  } else {
    msg_stop_num_rsp <- paste0(msg_stop_base, "stressors: "
                               , paste(rsp_num_false, collapse=", "))
    stop(msg_stop_num_rsp)
  }##IF~len_rsp~END
  #  
  #}##QC, NUMERIC ~ END
  
  col_StationID  <- "StationID_Master"
  col_ChemSampID <- "StressSampID"

  # check for and create (if necessary) "Results" subdirectory of working directory
  # wd <- getwd()
  # dir.sub <- "Results"
  wd <- dirname(dir_results)
  dir.sub <- basename(dir_results)
  dir.sub2 <- TargetSiteID
  dir.sub3 <- biocomm
  dir.sub4 <- dir_sub
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3))
         , FALSE)
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4))
         , FALSE)
  
  dir_path <- file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4)
  # Comment out, 20190423, when remove varLegLoc as input
  # helper
  # RegPlotSet <- getRegPlotSet(varLegLoc)
  # varInset  <- RegPlotSet[1]
  # varSpacer <- RegPlotSet[2]
  # varLegOpp <- RegPlotSet[3]
  
  plot_H <- 4
  plot_W <- 9
  
  #BioResp <- colnames(list.MatchBioData[["all.b.rsp"]])[16:ncol(list.MatchBioData[["all.b.rsp"]])]
  
  #QC
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    # p
    #stressors <- stressors[12]
    p <- 1
    #q
    #BioResp <- BioResp[c(7:11)]
    q <- 1
  }##IF.boo.DEBUG.END

  
  # move from plotting section
  #p
  p.len <- length(stressors)
  #q
  q.len <- length(BioResp)
  
 # boo.pryr <- FALSE
  
  # Capture each plot in a list for the PDF
  #plots.pq <- vector(length(BioResp), mode="list")
  plots.pq <- vector(q.len*p.len, mode="list")
  ppi<-300
  varFileOut <- file.path(dir_path,paste0(TargetSiteID, "_", biocomm, "_SRLin_"
                                          , "_"))
  # varFileOut = paste0("Results/",TargetSiteID, "/", dir.sub3, "/", TargetSiteID
  #                     , ".SR.", bio_prefix, ".")
  
  LogTransf <- stressorInfo[stressorInfo$StdParamName %in% stressors
                            , c("StdParamName","LogTransf")]
  LogTransf <- unique(LogTransf)
  
  # Prep site data for merging with scores
  ##site.b.str %>%
  df_str <- list.MatchBioData[["site.b.str"]] %>% 
      tidyr::gather(key = "Stressor", value = "StressorValue"
                    , c(-StationID_Master, -StressSampID, -RespSampID))
  #site.b.rsp %>%
  df_rsp <- list.MatchBioData[["site.b.rsp"]] %>% 
      dplyr::select(-RespSampDate) %>%
      tidyr::gather(key = "Response", value = "ResponseValue"
                    , c(-StationID_Master, -StressSampID, -RespSampID, -Quality))
  df_SiteData <- merge(df_str, df_rsp
                      , by.x = c("StationID_Master", "StressSampID", "RespSampID")
                      , by.y = c("StationID_Master", "StressSampID", "RespSampID")
                      , all = TRUE)
  df_SiteData <- df_SiteData %>%
      dplyr::select(StationID_Master, StressSampID, RespSampID, Quality
                    , Stressor, StressorValue, Response, ResponseValue)
  rm(df_str, df_rsp)

  # FOR.p ####
  for (p in 1:length(stressors)) {

    stressName <- stressors[p]
    stressLabel <- as.character(stressorInfo$Label[stressorInfo$StdParamName==stressName])
    varFlag <- 1
    varFlag.b <- 1

    # if (stressName %in% c("DO_uf_mg_L", "pH_SU", "Temp_degC", "Flow_calc_cfs",
    #                       "Flow_cfs")) {##IF.stressName.START
    #     log.yn <- FALSE
    #   } else {
    #     log.yn <- TRUE
    # }##IF.stressName.END
    # 
    log.yn <- as.logical(LogTransf$LogTransf[LogTransf$StdParamName==stressName])
    
    # Determine expected direction of slope
    dirIncStress <- unique(stressorInfo$DirIncStress[stressorInfo$StdParamName==stressName])
    if (dirIncStress == "Inc") {
        exp.dir <- -1
    } else {
        exp.dir <- 1
    }
    
    # DEBUG
    if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
      message(paste0("p; ",p, "; ", stressors[p]))
      flush.console()
    }##IF.boo.DEBUG.END

    # FOR.q ####
    for (q in 1:length(BioResp)) { 

      varFlag <- 1
      varFlag.b <- 1
      boo_corr <- TRUE
      boo_all <- TRUE
      respName <- BioResp[q]
      respLabel <- as.character(BioInfo$MetricLabel[BioInfo$MetricName==respName])
      pq <- q.len*(p-1)+q
      pq.len <- p.len * q.len
      
     # boo.pryr <- TRUE
      
      # QC
      if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
        message(paste0("Item (", pq, "/", pq.len, ")"))
        message(paste0("q; ", q, "; ", respName))
        flush.console()
      }##IF.boo.DEBUG.END
      
      # Data Munging ####
      #{##NoIssues.Munging.START
      # Columns to keep
      col_keep <- c(col_StationID, col_ChemSampID, col_Bio_Metrics_SampID)
      col_keep_str <- c(col_keep, stressName)
      col_keep_rsp <- c(col_keep, respName)
      
      # get all data to plot
      all.xvar<- list.MatchBioData[[all.x.str]][, col_keep_str]
      all.yvar<- list.MatchBioData[[all.x.rsp]][, col_keep_rsp]
      #df.plot1 <- merge(all.xvar[,2:3], all.yvar[,2:3], by.x = col_Bio_Metrics_SampID, by.y = col_Bio_Metrics_SampID)
      all.merge <- merge(all.xvar, all.yvar)
      df_plot_all <- all.merge[stats::complete.cases(all.merge), ]
      colnames(df_plot_all)[colnames(df_plot_all)==stressName] <- "Stressor"
      colnames(df_plot_all)[colnames(df_plot_all)==respName]   <- "Response"
      # QC
      if (nrow(df_plot_all) < min_cases) { 
        txt.score <- "< 20 cases"
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName
                             , " (", p, "/", p.len, "), ", respName, " (", q
                             , "/", q.len, "); score = ", txt.score)
        message(msg.status)
        next 
      }
      if(sum(is.na(df_plot_all$Stress))==nrow(df_plot_all)) {
        txt.score <- "stressors all NA or NAN"
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName
                             , " (", p, "/", p.len, "), ", respName, " (", q
                             , "/", q.len, "); score = ", txt.score)
        message(msg.status)
        next 
      }
      
      #get all ref data to plot
      all.ref.xvar <- subset(all.xvar, all.xvar$StressSampID %in% ref.sites)
      all.ref.yvar <- subset(all.yvar, all.yvar$StressSampID %in% ref.sites)
      #df.plot2 <- merge(all.ref.xvar[,2:3], all.ref.yvar[,2:3], by.x = col_Bio_Metrics_SampID, by.y = col_Bio_Metrics_SampID)
      all.ref.merge <- merge(all.ref.xvar, all.ref.yvar)
      df_plot_all_ref <- all.ref.merge[stats::complete.cases(all.ref.merge), ]
      colnames(df_plot_all_ref)[colnames(df_plot_all_ref)==stressName] <- "Stressor"
      colnames(df_plot_all_ref)[colnames(df_plot_all_ref)==respName]   <- "Response"
  
      #get all cluster data to plot
      cl.xvar<- list.MatchBioData[[cl.x.str]][, col_keep_str]
      cl.yvar<- list.MatchBioData[[cl.x.rsp]][, col_keep_rsp]
      #df.plot3 <- merge(cl.xvar[,2:3], cl.yvar[,2:3], by.x = col_Bio_Metrics_SampID, by.y = col_Bio_Metrics_SampID)
      cl.merge <- merge(cl.xvar, cl.yvar)
      df_plot_cl <- cl.merge[stats::complete.cases(cl.merge), ]
      colnames(df_plot_cl)[colnames(df_plot_cl)==stressName] <- "Stressor"
      colnames(df_plot_cl)[colnames(df_plot_cl)==respName]   <- "Response"
      
      #get all cluster ref data to plot
      cl.ref.xvar <- subset(cl.xvar, cl.xvar$StressSampID %in% ref.sites)
      cl.ref.yvar <- subset(cl.yvar, cl.yvar$StressSampID %in% ref.sites)
      #df.plot4 <- merge(cl.ref.xvar[,2:3], cl.ref.yvar[,2:3], by.x = col_Bio_Metrics_SampID, by.y = col_Bio_Metrics_SampID)
      cl.ref.merge <- merge(cl.ref.xvar, cl.ref.yvar)
      df_plot_cl_ref <- cl.ref.merge[stats::complete.cases(cl.ref.merge), ]
      colnames(df_plot_cl_ref)[colnames(df_plot_cl_ref)==stressName] <- "Stressor"
      colnames(df_plot_cl_ref)[colnames(df_plot_cl_ref)==respName]   <- "Response"
      
      #get target site data to plot
      site.xvar<- list.MatchBioData[[site.x.str]][, col_keep_str]
      site.yvar<- list.MatchBioData[[site.x.rsp]][, col_keep_rsp]
      #df.plot5 <- merge(site.xvar, site.yvar, by.x = col_Bio_Metrics_SampID, by.y = col_Bio_Metrics_SampID)
      site.merge <- merge(site.xvar, site.yvar)
      df_plot_site <- site.merge[stats::complete.cases(site.merge), ]
      colnames(df_plot_site)[colnames(df_plot_site)==stressName] <- "Stressor"
      colnames(df_plot_site)[colnames(df_plot_site)==respName]   <- "Response"
      
      # Log Transform
      if (log.yn == TRUE) {##IF.log.yn.START
          if (nrow(df_plot_all) > 0) { # SHOULD NEVER HAPPEN
              df_plot_all[, "Stressor"]     <- log10(df_plot_all[, "Stressor"])

          } else {
              gapcomment <- "No stressor data available for any sites in the cluster."
              gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")          }
          if (nrow(df_plot_all_ref) > 0) {
              df_plot_all_ref[, "Stressor"] <- log10(df_plot_all_ref[, "Stressor"])

          } else {
              gapcomment <- "No stressor data available for reference sites in the cluster."
              gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")          }
          if (nrow(df_plot_cl) > 0) { # SHOULD NEVER HAPPEN
              df_plot_cl[, "Stressor"]      <- log10(df_plot_cl[, "Stressor"])

          } else {
              
              gapcomment <- "No stressor data available for any comparator sites."
              gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")          }
          if (nrow(df_plot_cl_ref) > 0) {
              df_plot_cl_ref[, "Stressor"]  <- log10(df_plot_cl_ref[, "Stressor"])

          } else {
              gapcomment <- "No stressor data available for reference comparator sites."
              gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")          }
          if (nrow(df_plot_site) > 0) { # SHOULD NEVER HAPPEN
              df_plot_site[, "Stressor"]    <- log10(df_plot_site[, "Stressor"])

          } else {
              gapcomment <- "No stressor data available for the target site."
              gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
          }

      }##IF.log.yn.END
      
      # QC for NA/NAN/Inf
      # 20190606, log10 of 0 or negative gives errors for linear model (lm) below.
      if (nrow(df_plot_all) > 0) { # SHOULD NEVER HAPPEN
          df_plot_all[!is.finite(df_plot_all[, "Stressor"]), "Stressor"]         <- NA
      }
      if (nrow(df_plot_all_ref) > 0) {
          df_plot_all_ref[!is.finite(df_plot_all_ref[, "Stressor"]), "Stressor"] <- NA
      }
      if (nrow(df_plot_cl) > 0) { # SHOULD NEVER HAPPEN
          df_plot_cl[!is.finite(df_plot_cl[, "Stressor"]), "Stressor"]           <- NA
      }
      if (nrow(df_plot_cl_ref) > 0) {
          df_plot_cl_ref[!is.finite(df_plot_cl_ref[, "Stressor"]), "Stressor"]   <- NA
      }
      if (nrow(df_plot_site) > 0) { # SHOULD NEVER HAPPEN
          df_plot_site[!is.finite(df_plot_site[, "Stressor"]), "Stressor"]   <- NA
      }
      
      #}##NoIssues.Munging.END
      
      # Cluster
      # LM and Corr, Cluster ####
      # ~~~ Check QC of Corr Table at end of code ~~~~
      if(nrow(df_plot_cl[complete.cases(df_plot_cl),])>2){##IF~nrow(df_plot_cl)~START
          
          if(stats::sd(df_plot_cl$Stressor, na.rm=TRUE)==0){ # Vertical line
              boo_corr <- FALSE 
              
              gapcomment <- paste0("Stressor data in the comparator set have "
                                   , "a standard deviation of zero: "
                                   , "all values are equal.")
              gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
              
          } else if(stats::sd(df_plot_cl$Response, na.rm=TRUE)==0) { # Horizontal line
              boo_corr <- FALSE 
              
              gapcomment <- paste0("Response data in the comparator set have "
                                   , "a standard deviation of zero: "
                                   , "all values are equal.")
              gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
          } else {  # SD <> 0 along vertical and horizontal
              
              # 20190228, QC for no data
              model_cl <- stats::lm(df_plot_cl$Response ~ df_plot_cl$Stressor
                                    , na.action=na.exclude) #cluster only
              if(boo_pred_warn==TRUE){
                  suppressWarnings(model_cl_pred <- stats::predict(model_cl
                                    , interval = "prediction", level = 0.75))
              } else {
                  model_cl_pred <- stats::predict(model_cl
                                                  , interval = "prediction"
                                                  , level = 0.75)
              }
              model_cl_val  <- cbind(df_plot_cl, model_cl_pred) #predictions for all cluster values
              # 
              slope_cl <- signif(summary(model_cl)$coefficients[[2]], 3)
              intercept_cl <- signif(summary(model_cl)$coefficients[[1]], 3)
              pval_intercept_cl <- signif(summary(model_cl)$coefficients[[7]], 3)
              pval_slope_cl <- signif(summary(model_cl)$coefficients[[8]], 3)
              # r2
              r_cl <- stats::cor(df_plot_cl$Response, df_plot_cl$Stressor
                                 , method="pearson",use="pairwise.complete.obs")
              r2_cl <- formatC(r_cl^2, format="f", digits=3)
              n_str_cl <- length(df_plot_cl$Stressor)
              # Correlation
              c1S_cl <- (stats::cor.test(df_plot_cl$Response, df_plot_cl$Stressor
                                         , method="pearson", use="pairwise.complete.obs"))
              df.corr_cl <- data.frame(cbind(TargetSiteID, biocomm, stressName
                                             , stressLabel, respName, respLabel
                                             , c1S_cl$parameter+1
                                             , signif(c1S_cl$statistic, 2)
                                             , signif(c1S_cl$p.value, 2)
                                             , signif(c1S_cl$estimate, 2)
                                             , r2_cl))
              #names(df.corr_cl) <- c("StationID_Master", "biocomm", "stressName", "respName", "n", "statistic", "p.value", "estimate", "r2")
              names(df.corr_cl) <- cn_cor_pref
              pval.corr_cl <- signif(c1S_cl$p.value, 2)
              #
              # 20180621, scoring
              slope.dir_cl <- sign(slope_cl) #1 = positive, -1 = negative
              #              
          } # End std dev If eval
          
        } else { # <=2 rows of data
            boo_corr <- FALSE
            n_str_cl <- nrow(df_plot_cl)
       
            gapcomment <- paste0("Only two paired stressor-response samples "
                            , "are available for the comparator set.")
            gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0, gapcomment)
            colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                   , row.names = FALSE, sep = "\t")      
       
        }##IF~nrow(df_plot_cl)~END
      
        # ALL
        # LM and Corr, All ####
        # ~~~ Check QC of Corr Table at end of code ~~~~
      if(nrow(df_plot_all[complete.cases(df_plot_all),])>2){##IF~nrow(df_plot_cl)~START
          
            if(stats::sd(df_plot_all$Stressor, na.rm=TRUE)==0){ # Vertical line
                boo_all <- FALSE 
                gapcomment <- paste0("Stressor data across all sites in the "
                                   , "cluster have a standard deviation of "
                                   , "zero: all values are equal.")
                gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                       , gapcomment)
                colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
            } else if(stats::sd(df_plot_all$Response, na.rm=TRUE)==0) {
                boo_all <- FALSE 
                gapcomment <- paste0("Response data across all sites in the "
                                     , "cluster have a standard deviation of "
                                     , "zero: all values are equal.")
                gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                         , gapcomment)
                colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                            , row.names = FALSE, sep = "\t")
            } else {  # SD <> 0              
              # 20190228, QC for no data
                boo_all <- TRUE 
              model_all <- stats::lm(df_plot_all$Response ~ df_plot_all$Stressor
                                     , na.action=na.exclude) #cluster only
              if(boo_pred_warn==TRUE){
                  suppressWarnings(model_all_pred <- stats::predict(model_all
                                                                    , interval = "prediction", level = 0.75))
              } else {
                  model_all_pred <- stats::predict(model_all, interval = "prediction"
                                                   , level = 0.75)
              }
              model_all_val  <- cbind(df_plot_all, model_all_pred) #predictions for all cluster values
              # 
              slope_all <- signif(summary(model_all)$coefficients[[2]], 3)
              intercept_all <- signif(summary(model_all)$coefficients[[1]], 3)
              pval_intercept_all <- signif(summary(model_all)$coefficients[[7]], 3)
              pval_slope_all <- signif(summary(model_all)$coefficients[[8]], 3)
              # r2
              r_all <- stats::cor(df_plot_all$Response, df_plot_all$Stressor
                                  , method="pearson",use="pairwise.complete.obs")
              r2_all <- formatC(r_all^2, format="f", digits=3)
              n_str_all <- length(df_plot_all$Stressor)
              # Corelation
              c1S_all <- (stats::cor.test(df_plot_all$Response, df_plot_all$Stressor
                                          , method="pearson", use="pairwise.complete.obs"))
              df.corr_all <- data.frame(cbind(TargetSiteID, biocomm, stressName
                                              , stressLabel, respName, respLabel
                                              , c1S_all$parameter+1
                                              , signif(c1S_all$statistic, 2)
                                              , signif(c1S_all$p.value, 2)
                                              , signif(c1S_all$estimate, 2)
                                              , r2_all))
              #names(df.corr_all) <- c("StationID_Master", "biocomm", "stressName"
              #, "respName", "n", "statistic", "p.value", "estimate", "r2")
              names(df.corr_all) <- cn_cor_pref
              pval.corr_all <- signif(c1S_all$p.value, 2)
              #
              # 20180621, scoring
              slope.dir_all <- sign(slope_all) #1 = positive, -1 = negative
              # exp.dir <- data.lkp.dir[stressName,respName]
              # exp.dir <- -1
              #
          } # End std dev If eval statement

      } else { # <=2 samples
          boo_all <- FALSE
          n_str_all <- nrow(df_plot_all)

          gapcomment <- paste0("Only two or fewer paired stressor-response samples "
                               , "are available for all sites in the cluster.")
          gaps <- cbind.data.frame("getBioStressorResponse", stressName, 0
                                   , gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          
      }##IF~nrow(df_plot_all)~END              


      # Corr table output ####
      # # Create results data frame
      # ~~~ Check QC of Corr Table at end of code ~~~~
      if(boo_corr==TRUE){##IF~boo_corr~START
        if (varFlag==1) {  #First time through loop
          df.CorrTable <- df.corr_cl
        } else {
          df.CorrTable <- rbind(df.CorrTable, df.corr_cl)  #  if not first iteration then append
        } # IF, END
        boo.Append    <- TRUE
        boo.col.names <- FALSE
        if (pq==1){##IF~pq~START
          boo.Append    <- !boo.Append
          boo.col.names <- !boo.col.names
        }##IF~pq~END
        #if(boo.pryr==TRUE){
        # Add biocomm (20190425)
        #df.CorrTable[, "biocomm"] <- biocomm
        fn_corr <- paste0(TargetSiteID, "_", biocomm, "_SRLin_Corrs.tab")
        utils::write.table(df.CorrTable
                           , file.path(dir_path, fn_corr)
                           , sep="\t", quote=FALSE, row.names=FALSE
                           , col.names=boo.col.names, append=boo.Append)  
        #}
        pval.corr = signif(c1S_cl$p.value, 2)
      }##IF~boo_corr~END
      #

      # Scoring, Cluster ####
      if(nrow(df_plot_site)>0){##IF~nrow(df_plot_site)~END
        for (f in 1:nrow(df_plot_site)) {##FOR~f~START
          # Score, cluster
          # Generate scores based on slope, significance value, and r2
            if (boo_corr==TRUE) { # Cluster data should be scored
                if ((nrow(df_plot_cl)>=5) && (abs(pval.corr_cl)<=0.1) && (r2_cl>=0.1)) {##IF~length~START
                    # print to console p (stressName) and q (respName)
                    if (slope.dir_cl == exp.dir) {
                        #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 1")) 
                        txt.score_cl <-  "1"
                        sr.score_cl = 1
                    } else if (slope.dir_cl != exp.dir) {
                        # print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = -1"))
                        txt.score_cl <- "-1"
                        sr.score_cl = -1
                    } else {
                        #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = inconclusive"))
                        txt.score_cl <- "inconclusive"  
                        sr.score_cl = 0
                    }
                } else {
                    #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 0"))
                    txt.score_cl <- "0"  
                    sr.score_cl = 0
                }##IF~length~START
            } else { # <=2 sites in cluster; cannot be scored
                #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 0"))
                txt.score_cl <- "NE"  
                sr.score_cl = NA
            }

          # Score, all
            if (boo_all == TRUE) {
                if ((length(df_plot_all)>=5) && (abs(pval.corr_all)<=0.1) && (r2_all>=0.1)) {##IF~length~START
                    # print to console p (stressName) and q (respName)
                    if (slope.dir_all == exp.dir) {
                        #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 1")) 
                        txt.score_all <-  "1"
                        sr.score_all = 1
                    } else if (slope.dir_all != exp.dir) {
                        # print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = -1"))
                        txt.score_all <- "-1"
                        sr.score_all = -1
                    } else {
                        #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = inconclusive"))
                        txt.score_all <- "inconclusive"  
                        sr.score_all = 0
                    }
                } else {
                    #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 0"))
                    txt.score_all <- "0"  
                    sr.score_all = 0
                }##IF~length~START
            
            } else { # boo_all == FALSE
                txt.score_all <- "NE"  
                sr.score_all = NA
            }

          #
        }##FOR~f~END
        #if (boo.pryr==TRUE) {##IF.boo.pryr.START
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName
                             , " (", p, "/", p.len, "), ", respName, " ("
                             , q, "/", q.len, "); score (all, cluster) = "
                             , txt.score_all, ", ", txt.score_cl)
        message(msg.status)
        #}##IF.boo.pryr.START
        df.temp2 <- as.data.frame(cbind("StationID_Master"=TargetSiteID
                                        , "biocomm"=biocomm
                                        , "stressName"=stressName
                                        , "stressLabel"=stressLabel
                                        , "respName"=respName
                                        , "respLabel"=respLabel
                                        , "n_site"=length(df_plot_site)
                                        , "n_all"=n_str_all
                                        , "SR_Score_all"=sr.score_all
                                        , "n_cluster"=n_str_cl
                                        , "SR_Score_cluster"=sr.score_cl)
                                  )
        if (varFlag.b==1) { # First time through this loop
          df.sc.sr <- df.temp2
        } else {
          df.sc.sr <- rbind(df.sc.sr, df.temp2)
        }
        
        df.sc.sr <- merge(df_SiteData, df.sc.sr
                          , by.x = c("StationID_Master", "Stressor", "Response")
                          , by.y = c("StationID_Master", "stressName", "respName")
                          , all.y = TRUE)
        df.sc.sr$SR_Score_all <- ifelse(is.na(df.sc.sr$SR_Score_all)
                                        , "NE"
                                        , as.character(df.sc.sr$SR_Score_all))
        df.sc.sr$SR_Score_cluster <- ifelse(is.na(df.sc.sr$SR_Score_cluster)
                                        , "NE"
                                        , as.character(df.sc.sr$SR_Score_cluster))
                                                                        
        #if(boo.pryr==TRUE){
        fn_scores <- paste0(TargetSiteID, "_", biocomm, "_SRLin_Scores.tab")
        fp_scores <- file.path(dir_path, fn_scores)
        
        boo.Append    <- TRUE
        boo.col.names <- FALSE
        if (file.exists(fp_scores)==FALSE){
          # can't rely on pq==1 as that may not have data
          boo.Append    <- !boo.Append
          boo.col.names <- !boo.col.names
        }
        
        # Add biocomm, 20190425
        #df.sc.sr[, "biocomm"] <- biocomm
        utils::write.table(df.sc.sr
                           , fp_scores
                           , sep="\t", quote=FALSE, row.names=FALSE
                           , col.names=boo.col.names, append=boo.Append) 
        #}
        # Moved from inside FOR.f
      } else {
        sr.score_all <- "NE"
        sr.score_cl <- "NE"
        txt.score <- "No Data"
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName
                             , " (", p, "/", p.len, "), ", respName, " ("
                             , q, "/", q.len, "); score (all, cluster) = "
                             , txt.score)
        message(msg.status)
      }##IF~nrow(df_plot_site)~END
      #
      
      ## Plot, inputs ####
      ## Plot, portions
      boo_plot_ref    <- ifelse(nrow(df_plot_all_ref[!is.na(df_plot_all_ref$Stressor),])>0, TRUE, FALSE)
      boo_plot_cl     <- ifelse(nrow(df_plot_cl[!is.na(df_plot_cl$Stressor),])>0, TRUE, FALSE)
      boo_plot_cl_ref <- ifelse(nrow(df_plot_cl_ref[!is.na(df_plot_cl_ref$Stressor),])>0, TRUE, FALSE)
      boo_plot_targ   <- ifelse(nrow(df_plot_site[!is.na(df_plot_site$Stressor),])>0, TRUE, FALSE)
      
      ## Plot, Variables, Strings
      str_title <- paste0(TargetSiteID, ": Stressor-Response (linear regression) line of evidence")
      str_subtitle1 <- "Is there evidence of a biological gradient from inside or outside the case?"
      str_subtitle2 <- "Linear regression with 75th percentile prediction interval"
      str_subtitle <- stringr::str_wrap(paste0(str_subtitle1,str_subtitle2),75)
      str_xlab  <- paste0(ifelse(log.yn==TRUE, "Log10 ", ""), stressLabel)
      str_ylab  <- respLabel
      # if then for equation
      if (sum(!is.na(df_plot_cl$Stressor)) > 2 || sum(!is.na(df_plot_cl$Response)) > 2) {##IF.equation.START
        str_caption_cl <- paste(paste0("Regression (comparators, inside the case): "
                                       , "y = ", slope_cl, " x + ", intercept_cl)
                             , paste0("r2 = ", r2_cl)
                             , paste0("p-value = ", pval.corr_cl)
                             , paste0("n = ", n_str_cl)
                             , paste0("score = ", sr.score_cl)
                             , sep=" ~ ")
      } else {
        str_caption_cl <- "Regression (comparators, inside the case):  Less than 3 data points in comparator set."
      }##IF.equation.END
      #
      if (sum(!is.na(df_plot_all$Stressor)) > 2 || sum(!is.na(df_plot_all$Response)) > 2) {##IF.equation.START
        str_caption_all <- paste(paste0("Regression (all, outside the case): "
                                        , "y = ", slope_all, " x + ", intercept_all)
                                , paste0("r2 = ", r2_all)
                                , paste0("p-value = ", pval.corr_all)
                                , paste0("n = ", n_str_all)
                                , paste0("score = ", sr.score_all)
                                , sep=" ~ ")
      } else {
        str_caption_all <- "Regression (all):  Less than 3 data points."
      }##IF.equation.END
      #
      if (siteQual2Plot=="not degraded") {
          qualtext <- "not degraded*"
          str_caption_qual <- "*Samples rated not degraded."
          str_caption <- paste0(str_caption_all, "\n", str_caption_cl, "\n", str_caption_qual)
          leg_all_ref <- qualtext
          leg_cl_ref <- paste0("comparator ",qualtext)
      } else if (siteQual2Plot=="better than") {
          qualtext <- "better quality*"
          str_caption_qual <-"*Samples with biological quality better than the minimum target site quality."
          str_caption <- paste0(str_caption_all, "\n", str_caption_cl, "\n", str_caption_qual)
          leg_all_ref <- qualtext
          leg_cl_ref <- paste0("comparator ",qualtext)
      } else { 
          qualtext <- "all ref"
          str_caption <- paste0(str_caption_all, "\n", str_caption_cl, "\n", str_caption_qual)
          leg_all_ref <- "all ref"
          leg_cl_ref <- "comparator ref"
      }

      ## Plot, Variables, Colors
      col_sites_all     <- "dark gray"
      col_sites_all_ref <- "blue"
      col_sites_cl      <- "cyan3"
      col_sites_cl_ref  <- col_sites_all_ref
      col_sites_targ    <- "red"
      col_line_cl       <- col_sites_cl
      col_line_all      <- "black"
      
      ## Plot, Variables, Fill
      fill_sites_all     <- col_sites_all
      fill_sites_all_ref <- fill_sites_all
      fill_sites_cl      <- col_sites_cl
      fill_sites_cl_ref  <- fill_sites_cl 
      fill_sites_targ    <- col_sites_targ
      
      ## Plot, Variables, Points
      pch_sites_all     <- 19 # solid circle
      pch_sites_all_ref <- 21 # circle outline
      pch_sites_cl      <- 19
      pch_sites_cl_ref  <- pch_sites_all_ref
      pch_sites_targ    <- 17 # triangle
      
      ## Plot, Variables, Sizes
      cex_mod <- 2
      cex_sites_all     <- 1 #cex_mod*0.3
      cex_sites_all_ref <- cex_sites_all
      cex_sites_cl      <- cex_mod*0.95
      cex_sites_cl_ref  <- cex_sites_cl
      cex_sites_targ    <- cex_mod*1.2
      
      ## Plot, Variables, Alpha
      alpha_lm_all <- 0.5
      alpha_lm_cl  <- 0.25
      
      ## Plot, Variables, Legend
      leg_name   <- "Samples"
      leg_labels <- c("all", leg_all_ref, "comparator", leg_cl_ref, "target")
      leg_shape  <- c(pch_sites_all, pch_sites_all_ref, pch_sites_cl, pch_sites_cl_ref, pch_sites_targ)
      leg_col    <- c(col_sites_all, col_sites_all_ref, col_sites_cl, col_sites_cl_ref, col_sites_targ)
      leg_fill   <- c(fill_sites_all, fill_sites_all_ref, fill_sites_cl, fill_sites_cl_ref, fill_sites_targ)
     # }# Plot, Variables ~ END
      
      # Plot, ggplot ####
      # Plot, Plot
      boo.Plot <- ifelse(nrow(df_plot_site)==0, FALSE, TRUE)
      # skip plot if no data for target site
      if(boo.Plot==TRUE){##IF.boo.Plot.START
        # ggplot, main
        p_SR <- ggplot2::ggplot(df_plot_all
                                , ggplot2::aes_(x=~Stressor,y=~Response
                                                , color="all", shape="all", fill="all")
                    , size=cex_sites_all) +
                 ggplot2::geom_point(na.rm = TRUE)
        #  
        # ggplot, point subsets
        # Add points if exist, otherwise plot dummy values
        if(boo_plot_ref==TRUE){##IF~boo_plot_ref~START
          p_SR <- p_SR + 
            ggplot2::geom_point(data=df_plot_all_ref
                , ggplot2::aes_(x=~Stressor, y=~Response, color="all ref"
                                , shape="all ref", fill="all ref")
                , size=cex_sites_all_ref
                , na.rm = TRUE)
        } else {
          p_SR <- p_SR + 
            ggplot2::geom_blank(ggplot2::aes(color="all ref"
                                             , shape="all ref", fill="all ref"))
        }##IF~boo_plot_ref~END
        #
        if(boo_plot_cl==TRUE){##IF~boo_plot_cl~START
          p_SR <- p_SR + 
            ggplot2::geom_point(data=df_plot_cl
                , ggplot2::aes_(x=~Stressor, y=~Response, color="cluster"
                                , shape="cluster", fill="cluster")
                , size=cex_sites_cl
                , na.rm = TRUE)
        } else {
          p_SR <- p_SR + 
            ggplot2::geom_blank(ggplot2::aes(color="cluster", shape="cluster", fill="cluster"))
        }##IF~boo_plot_cl~END  
        #
        if(boo_plot_cl_ref==TRUE){##IF~boo_plot_cl_ref~START
          p_SR <- p_SR + 
            ggplot2::geom_point(data=df_plot_cl_ref
                , ggplot2::aes_(x=~Stressor, y=~Response, color="cluster ref"
                                , shape="cluster ref", fill="cluster ref")
                , size=cex_sites_cl_ref
                , na.rm = TRUE)
        } else {
          p_SR <- p_SR + 
            ggplot2::geom_blank(ggplot2::aes(color="cluster ref", shape="cluster ref", fill="cluster ref"))
        }##IF~boo_plot_cl_ref~END
        #
        if(boo_plot_targ==TRUE){##IF~boo_plot_targ~START
          p_SR <- p_SR + 
            ggplot2::geom_point(data=df_plot_site
                , ggplot2::aes_(x=~Stressor,y=~Response, color="target"
                                , shape="target", fill="target")
                , size=cex_sites_targ
                , na.rm = TRUE)
        } else {
          p_SR <- p_SR + 
            ggplot2::geom_blank(ggplot2::aes(color="target", shape="target", fill="target"))
        }
        ##IF~boo_plot_targ~END
        # 
        # Add rest of plot  
        p_SR <- p_SR + 
          ggplot2::scale_shape_manual(name=leg_name, labels=leg_labels, values=leg_shape)  + 
          ggplot2::scale_color_manual(name=leg_name, labels=leg_labels, values=leg_col) +
          ggplot2::scale_fill_manual(name=leg_name, labels=leg_labels, values=leg_fill)
        # Regression, all
        if (exists("model_all_val")) {
            # Linear model (all data)
            p_SR <- p_SR + 
              ggplot2::stat_smooth(data=df_plot_all
                                  , method=lm
                                  , color=col_line_all
                                  , fill=fill_sites_all
                                  , alpha=alpha_lm_all
                                  , formula = y ~ x # Added to avaoid message
                                  , show.legend=FALSE
                                  , na.rm = TRUE)
            p_SR <- p_SR + 
              ggplot2::geom_line(data=model_all_val
                                , ggplot2::aes_(y=~lwr)
                                , color=col_line_all
                                , linetype="dashed"
                                , show.legend=FALSE
                                , size = 1.25
                                , na.rm = TRUE)
            p_SR <- p_SR + 
              ggplot2::geom_line(data=model_all_val
                                , ggplot2::aes_(y=~upr)
                                , color=col_line_all
                                , linetype="dashed"
                                , show.legend=FALSE
                                , size = 1.25
                                , na.rm = TRUE)
        }
        # Regression, cluster
        if (exists("model_cl_val")) {
            # Linear model (cluster)
            p_SR <- p_SR + 
              ggplot2::stat_smooth(data=df_plot_cl
                                  , method=lm
                                  , color=col_line_cl
                                  , fill=fill_sites_cl
                                  , alpha=alpha_lm_cl
                                  , formula = y ~ x # Added to avaoid message
                                  , show.legend=FALSE
                                  , na.rm = TRUE)
            p_SR <- p_SR + 
              ggplot2::geom_line(data=model_cl_val
                                , ggplot2::aes_(y=~lwr)
                                , color=col_line_cl
                                , linetype="dashed"
                                , show.legend=FALSE
                                , na.rm = TRUE)
            p_SR <- p_SR + 
              ggplot2::geom_line(data=model_cl_val
                                , ggplot2::aes_(y=~upr)
                                , color=col_line_cl
                                , linetype="dashed"
                                , show.legend=FALSE
                                , na.rm = TRUE)
        }

        # other
        p_SR <- p_SR + 
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5, size=12)
                            , plot.subtitle=ggplot2::element_text(hjust=0.5, size=10)
                            , plot.caption=ggplot2::element_text(size=8)
                            , legend.title=ggplot2::element_text(size=10)
                            , legend.text=ggplot2::element_text(size=8)
                            , axis.title=ggplot2::element_text(size=10)) + 
          ggplot2::labs(title=str_title, subtitle = str_subtitle
                            , caption=str_caption, x=str_xlab, y=str_ylab)
        #
        print(p_SR)
        plots.pq[[pq]] <- grDevices::recordPlot()
        #
        fn_png <- paste0(varFileOut, make.names(stressName), "_"
                         , make.names(respName), ".png")
        ggplot2::ggsave(fn_png, p_SR, width=plot_W, height=plot_H, units="in")
        #
      }##IF.boo.Plot.END

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      ## PDF, capture plot in list
      ### Need to run plot.pryr as is only created above
      #boo.pryr <- TRUE
      #  plot.pryr
      #boo.pryr <- FALSE
      #pq <- q.len*(p-1)+q
      # plots.pq[[pq]] <- grDevices::recordPlot()
      # 
      # ## JPG, Create
      # # grDevices::jpeg(filename = paste(varFileOut, stressName, "_", respName,
      # #                                  ".jpg", sep = ""), width = 4 * ppi,
      # #                 height = 3 * ppi, quality=100, pointsize=8, res = ppi)
      # #   plot.pryr
      # # grDevices::dev.off()
      # 
      # fn_jpg <- paste0(varFileOut, stressName, "_", respName, ".jpg")
      # ggplot2::ggsave(fn_jpg, p_SR)
      #
      varFlag <- 0
      varFlag.b <- 0 # Set varFlag.b to zero
    }##FOR.q.END
    #grDevices::graphics.off()
  }##FOR.p.END
  
  # END ####
  ## PDF ####
  # Create PDF from list
  fn_pdf <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm,"_SRLin_ALL.pdf"))
  grDevices::pdf(file=fn_pdf, width=plot_W, height=plot_H)
    for (pq in plots.pq){##FOR.gp.START
      #grDevices::replayPlot(g.plot)
      if(is.null(pq)==TRUE) {next}
      grDevices::replayPlot(pq)
    }##FOR.gp.END
  grDevices::dev.off()
 # rm(plots.pq)
  #
  # utils::write.table(df.CorrTable
  #                    , file.path(wd,dir.sub,dir.sub2,"StressRespCorrs.BMI.txt")
  #                    , sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
  # utils::write.table(df.sc.sr
  #                    , file.path(wd,dir.sub,dir.sub2,"StressRespScores.BMI.txt")
  #                   , sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE) 
  #
  # CorrPlot ####
  ## read
  if (boo_corr==TRUE) {
      fn_corr <- paste0(TargetSiteID,"_", biocomm, "_SRLin_Corrs.tab")
      fp_corr <- file.path(dir_path, fn_corr)
      
      if (exists(fp_corr)==TRUE) {
          df_corr <- utils::read.delim(fp_corr)
          
          # QC, 20190313
          # QC, Corr table ####
          ## Special case where the function doesn't save the header row
          ### Unable to track down cause so implement QC check here.
          #cn_cor_pref <- c("StationID_Master", "biocomm", "stressName", "respName", "n", "statistic", "p.value", "estimate", "r2")
          cn_cor_x    <- colnames(df_corr)
          cn_cor_match <- sum(cn_cor_x %in% cn_cor_pref)
          if(cn_cor_match!=length(cn_cor_pref)){##IF~length~START
              df_corr <- utils::read.delim(fp_corr, header = FALSE, col.names = cn_cor_pref)
              utils::write.table(df_corr, fp_corr, sep="\t", quote=FALSE, row.names=FALSE )
          }##IF~length~END
          
          ## transpose 
          # 20190305; shouldn't need mean or unique but just in case, should be complete dups
          df_corr <- unique(df_corr)
          df_corr_r <- reshape2::dcast(df_corr, stressName ~ respName
                                       , fun.aggregate=mean
                                       , value.var="estimate"
                                       , na.rm=TRUE)
          df_corrplot <- t(df_corr_r[,-1])
          colnames(df_corrplot) <- df_corr_r[,1]
          ## jpg
          fn_png_cp <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm
                                                  , "_SRLin_CorrPlot.png"))
          grDevices::png(filename = fn_png_cp
                         , width = 4 * ppi
                         , height = 3 * ppi)
          corrplot::corrplot(df_corrplot, method="circle")
          grDevices::dev.off()
          
          msg.corr <- "Printing correlation plot."
          message(msg.corr)
      }
  }
  #
}##FUNCTION.END
