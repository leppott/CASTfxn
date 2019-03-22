#' @title Biological Stressor Responses
#' 
#' @description Get Biological (Algae or BMI) stressor responses.
#' 
#' @details Biological (Algae or BMI) stressor regressions.
#' 
#' @param TargetSiteID Site ID
#' @param stressors stressors
#' @param BioResp Biological response variables.  For example, BMI metrics or Algae metrics.
#' @param list.MatchBioData list of matched biological (BMI or algae) and stressor data.
#' @param LogTransf Value for if stressor variables should be log10 transformed; 1=TRUE, 0=FALSE.
#' @param ref.sites Reference sites.
#' @param predint Prediction interval. Default = 0.75
#' @param varLegLoc Plot legend location.  For regressions this will be opposite. Default="topright"
#' @param biocomm Biological community; algae or BMI.  Default = "BMI"
#' 
#' @return A jpg in SiteID subfoler of the "Results" folder of working directory.  
#' And two tab-delimited text files; stressor correlations and scores.
#' 
# @importFrom pryr "%<a-%"
#' 
#' @examples
#' \dontrun{
#' # Example 1, BMI
#' predint <- 0.75
#' varLegLoc <- "topright"
#'              
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
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
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID, "ElevCategory"])
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
#' map_outline   <- data_GIS_AZ_Outline
#' # Project site data to USGS Albers Equal Area
#' usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#'               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' # projection for outline
#' my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
#'            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' map_proj <- my.aea
#' 
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
#'                                 , data.SampSummary, data.303d.ComID
#'                                 , data.bmi.metrics, data.algae.metrics
#'                                 , data.cluster, data.mod
#'                                 , map_proj, map_outline, map_flowline)
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
#' 
#' # set cutoff for possible stressor identification
#' probsLow <- 0.10
#' probsHigh <- 0.90 
#' 
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, site.chem
#'                                  , probsHigh, probsLow)
#'                                  
#' # Data getBioMatches
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
#' LogTransf <- stressors_logtransf
#' 
#' # Run getBioMatches
#' biocomm <- "BMI"
#' list.MatchBioData <- getBioMatches(stressors, list.data, biocomm)     
#'   
#' # Data getBMIStressorResponses   
#' biocomm <- "BMI"    
#' BioResp <- c("IBI", "TotalTaxSPL_Sc", "DipTaxSPL_Sc"
#'              , "IntolTaxSPL_Sc", "HBISPL_Sc", "PlecoPct_Sc", "ScrapPctSPL_Sc"
#'              , "TrichTax_Sc", "EphemTax_Sc", "EphemPct_Sc", "Dom01PctSPL_Sc") 
#'              
#' # Run getBMIStressorResponses                 
#' getBioStressorResponses(TargetSiteID, stressors, BioResp, list.MatchBioData
#'                         , LogTransf, ref.sites, predint, varLegLoc, biocomm)
#' 
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, Algae
#' predint <- 0.75
#' varLegLoc <- "topright"
#' 
#' TargetSiteID <- "LCBEN002.57"
#' dir_results <- file.path(getwd(), "Results")
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
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID, "ElevCategory"])
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
#' map_outline   <- data_GIS_AZ_Outline
#' # Project site data to USGS Albers Equal Area
#' usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#'               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' # projection for outline
#' my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
#'            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' map_proj <- my.aea
#' 
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
#'                                 , data.SampSummary, data.303d.ComID
#'                                 , data.bmi.metrics, data.algae.metrics
#'                                 , data.cluster, data.mod
#'                                 , map_proj, map_outline, map_flowline)
#' 
#' # Data getChemDataSubsets
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
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
#' 
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, site.chem
#'                                  , probsHigh, probsLow)
#'                                  
#' # Data getBioMatches
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
#' LogTransf <- stressors_logtransf
#'
#' # Run getBioMatches
#' biocomm <- "algae"
#' list.MatchBioData <- getBioMatches(stressors, list.data, biocomm)
#' 
#' # Data getBioStressorResponses
#' data.algae.metrics <- data_AlgMetrics
#' BioResp <- colnames(data.algae.metrics[6:48])
#' biocomm <- "algae"
#' 
#' # Run getAlgStressorResponses
#' getBioStressorResponses(TargetSiteID, stressors, BioResp, list.MatchBioData
#'                        , LogTransf, ref.sites, predint, varLegLoc, biocomm)
#' }
#
#' @export
getBioStressorResponses <- function(TargetSiteID, stressors, BioResp, list.MatchBioData
                                    , LogTransf, ref.sites, predint=0.75, varLegLoc="topright"
                                    , biocomm="bmi") {##FUNCTION.START
  # DEBUG
  boo.DEBUG <- FALSE
  ## Trigger DEBUG actions below for when debugging.
  
  # Community ####
  biocomm <- tolower(biocomm)
  # Check for no data
  if(biocomm=="bmi"){##IF.biocomm.START
    #
    bio_prefix <- "BMI"
    col_Bio_Metrics_SampID <- "BMI.Metrics.SampID"
    min_cases <- 20
    all.x.str  <- "all.b.str"
    cl.x.str   <- "cl.b.str"
    site.x.str <- "site.b.str"
    all.x.rsp  <- "all.b.rsp"
    cl.x.rsp   <- "cl.b.rsp"
    site.x.rsp <- "site.b.rsp"
    #
  } else if(biocomm=="algae"){
    #
    bio_prefix <- "Alg"
    col_Bio_Metrics_SampID <- "Algae.Metrics.SampID"
    min_cases <- 20
    all.x.str  <- "all.a.str"
    cl.x.str   <- "cl.a.str"
    site.x.str <- "site.a.str"
    all.x.rsp  <- "all.a.rsp"
    cl.x.rsp   <- "cl.a.rsp"
    site.x.rsp <- "site.a.rsp"
    #
  } else {
    # Non Valid biological community
    Msg_Stop <- print(paste0("Non-valid biological community specified (", biocomm,"). Only values of 'bmi' and 'algae' are valid."))
    stop(Msg_Stop)
  }##IF.biocomm.END
  
  
  # QC, site rsp ####
  qc_row_site_rsp <-  nrow(list.MatchBioData$site.a.rsp)
  if(qc_row_site_rsp==0){##IF~qc_row_site_rsp~START
    msg_Stop_site_rsp <- "No site response data in list.MatchBioData."
    stop(msg_Stop_site_rsp)
  }##IF~qc_row_site_rsp~START
  
  
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
    msg_stop_num_str <- paste0(msg_stop_base, "stressors: ", paste(str_num_false, collapse=", "))
    stop(msg_stop_num_str)
  }##IF~len_str~END
  #
  if (length(rsp_num_false)==0) {##IF~len_rsp~START
  } else {
    msg_stop_num_rsp <- paste0(msg_stop_base, "stressors: ", paste(rsp_num_false, collapse=", "))
    stop(msg_stop_num_rsp)
  }##IF~len_rsp~END
  #  
  #}##QC, NUMERIC ~ END
  
  
  col_StationID  <- "StationID_Master"
  col_ChemSampID <- "ChemSampleID"

  # check for and create (if necessary) "Results" subdirectory of working directory
  wd <- getwd()
  dir.sub <- "Results"
  dir.sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  #
  # helper
  RegPlotSet <- getRegPlotSet(varLegLoc)
  varInset  <- RegPlotSet[1]
  varSpacer <- RegPlotSet[2]
  varLegOpp <- RegPlotSet[3]
  
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
  varFileOut = paste0("Results/",TargetSiteID, "/", TargetSiteID, ".SR.", bio_prefix, ".")
  
  LogTransf <- as.logical(LogTransf)
  
  # FOR.p ####
  for (p in 1:length(stressors)) {
    stressName <- stressors[p]
    varFlag <- 1
    varFlag.b <- 1

    # if (stressName %in% c("DO_uf_mg_L", "pH_SU", "Temp_degC", "Flow_calc_cfs",
    #                       "Flow_cfs")) {##IF.stressName.START
    #     log.yn <- FALSE
    #   } else {
    #     log.yn <- TRUE
    # }##IF.stressName.END
    # 
    log.yn <- LogTransf[p]
    
    # DEBUG
    if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
      print(paste0("p; ",p))
      flush.console()
    }##IF.boo.DEBUG.END

    # FOR.q ####
    for (q in 1:length(BioResp)) { 
      varFlag <- 1
      varFlag.b <- 1
      boo_corr <- TRUE
      respName <- BioResp[q]
      pq <- q.len*(p-1)+q
      pq.len <- p.len * q.len
      
     # boo.pryr <- TRUE
      
      # QC
      if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
        print(paste0("Item (", pq, "/", pq.len, ")"))
        print(paste0("q; ", respName))
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
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = ", txt.score)
        print(msg.status)
        next 
      }
      if(sum(is.na(df_plot_all$Stress))==nrow(df_plot_all)) {
        txt.score <- "stressors all NA or NAN"
        msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = ", txt.score)
        print(msg.status)
        next 
      }
      
      #get all ref data to plot
      all.ref.xvar <- subset(all.xvar, all.xvar$StationID_Master %in% ref.sites)
      all.ref.yvar <- subset(all.yvar, all.yvar$StationID_Master %in% ref.sites)
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
      cl.ref.xvar <- subset(cl.xvar, cl.xvar$StationID_Master %in% ref.sites)
      cl.ref.yvar <- subset(cl.yvar, cl.yvar$StationID_Master %in% ref.sites)
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
        df_plot_all[, "Stressor"]     <- log10(df_plot_all[, "Stressor"])
        df_plot_all_ref[, "Stressor"] <- log10(df_plot_all_ref[, "Stressor"])
        df_plot_cl[, "Stressor"]      <- log10(df_plot_cl[, "Stressor"])
        df_plot_cl_ref[, "Stressor"]  <- log10(df_plot_cl_ref[, "Stressor"])
        df_plot_site[, "Stressor"]    <- log10(df_plot_site[, "Stressor"])
      }##IF.log.yn.END
      
      #}##NoIssues.Munging.END
      
      # LM and Corr ####
      if(nrow(df_plot_cl)>0){##IF~nrow(df_plot_cl)~START
        # 20190228, QC for no data
        model_cl <- lm(df_plot_cl$Response ~ df_plot_cl$Stressor) #cluster only
        model_cl_pred <- predict(model_cl, interval = "prediction", level = 0.75)
        model_cl_val  <- cbind(df_plot_cl, model_cl_pred) #predictions for all cluster values
        # 
        slope <- signif(summary(model_cl)$coefficients[[2]], 3)
        intercept <- signif(summary(model_cl)$coefficients[[1]], 3)
        pval_intercept <- signif(summary(model_cl)$coefficients[[7]], 3)
        pval_slope <- signif(summary(model_cl)$coefficients[[8]], 3)
        # r2
        r <- stats::cor(df_plot_cl$Response, df_plot_cl$Stressor, method="pearson",use="pairwise.complete.obs")
        r2 <- formatC(r^2, format="f", digits=3)
        n_str <- length(df_plot_cl$Stressor)
        # Corelation
        c1S <- (stats::cor.test(df_plot_cl$Response, df_plot_cl$Stressor, method="pearson", use="pairwise.complete.obs"))
        df.corr <- data.frame(cbind(stressName, respName, signif(c1S$statistic,2)
                                    , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
        names(df.corr) <- c("stressName", "respName", "statistic", "p.value", "estimate", "r2")
        pval.corr <- signif(c1S$p.value, 2)
        #
        # 20180621, scoring
        slope.dir <- sign(slope) #1 = positive, -1 = negative
        # exp.dir <- data.lkp.dir[stressName,respName]
        exp.dir <- -1
        #
      } else {
       boo_corr <- FALSE 
      }##IF~nrow(df_plot_cl)~END


      #{# Plot, Variables ~ START
      #
      
      ## Plot, portions
      boo_plot_ref    <- ifelse(nrow(df_plot_all_ref)>0, TRUE, FALSE)
      boo_plot_cl     <- ifelse(nrow(df_plot_cl)>0, TRUE, FALSE)
      boo_plot_cl_ref <- ifelse(nrow(df_plot_cl_ref)>0, TRUE, FALSE)
      boo_plot_targ   <- ifelse(nrow(df_plot_site)>0, TRUE, FALSE)
      
      ## Plot, Variables, Strings
      str_title <- paste(TargetSiteID, stressName, respName, sep=" ~ ")
      str_subtitle <- "Linear regression with 75th percentile prediction interval"
      str_xlab  <- paste0(ifelse(log.yn==TRUE, "Log10 ", ""), stressName)
      str_ylab  <- respName
      # if then for equation
      if (sum(!is.na(df_plot_cl$Stressor)) > 2 || sum(!is.na(df_plot_cl$Response)) > 2) {##IF.equation.START
        str_caption <- paste(paste0("Cluster regression: ", "y = ", slope, " x + ", intercept)
                             , paste0("r2 = ", r2)
                             , paste0("p-value = ", pval.corr)
                             , paste0("n = ", n_str)
                             , sep=" ~ ")
      } else {
        str_caption <- "Cluster regression:  Less than 2 data points in cluster. "
      }##IF.equation.END

      ## Plot, Variables, Colors
      col_sites_all     <- "dark gray"
      col_sites_all_ref <- "blue"
      col_sites_cl      <- "cyan3"
      col_sites_cl_ref  <- col_sites_all_ref
      col_sites_targ    <- "red"
      col_line          <- "black"
      
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
      
      ## Plot, Variables, Legend
      leg_name   <- "Sites"
      leg_labels <- c("all", "all ref", "cluster", "cluster ref", "target")
      leg_shape  <- c(pch_sites_all, pch_sites_all_ref, pch_sites_cl, pch_sites_cl_ref, pch_sites_targ)
      leg_col    <- c(col_sites_all, col_sites_all_ref, col_sites_cl, col_sites_cl_ref, col_sites_targ)
      leg_fill   <- c(fill_sites_all, fill_sites_all_ref, fill_sites_cl, fill_sites_cl_ref, fill_sites_targ)
     # }# Plot, Variables ~ END
      
      # ggplot ####
      # Plot, Plot
      
      boo.Plot <- ifelse(nrow(df_plot_site)==0, FALSE, TRUE)
      # skip plot if no data for target site
      if(boo.Plot==TRUE){##IF.boo.Plot.START
        # ggplot, main
        p_SR <- ggplot2::ggplot(df_plot_all, ggplot2::aes_(x=~Stressor,y=~Response, color="all", shape="all", fill="all"), size=cex_sites_all) +
                 ggplot2::geom_point()
        #  
        # ggplot, point subsets
        # Add points if exist, otherwise plot dummy values
        if(boo_plot_ref==TRUE){##IF~boo_plot_ref~START
          p_SR <- p_SR + ggplot2::geom_point(data=df_plot_all_ref, ggplot2::aes_(x=~Stressor, y=~Response, color="all ref", shape="all ref", fill="all ref"), size=cex_sites_all_ref)
        } else {
          p_SR <- p_SR + ggplot2::geom_blank(ggplot2::aes(color="all ref", shape="all ref", fill="all ref"))
        }##IF~boo_plot_ref~END
        #
        if(boo_plot_cl==TRUE){##IF~boo_plot_cl~START
          p_SR <- p_SR + ggplot2::geom_point(data=df_plot_cl, ggplot2::aes_(x=~Stressor, y=~Response, color="cluster", shape="cluster", fill="cluster"), size=cex_sites_cl)
        } else {
          p_SR <- p_SR + ggplot2::geom_blank(ggplot2::aes(color="cluster", shape="cluster", fill="cluster"))
        }##IF~boo_plot_cl~END  
        #
        if(boo_plot_cl_ref==TRUE){##IF~boo_plot_cl_ref~START
          p_SR <- p_SR + ggplot2::geom_point(data=df_plot_cl_ref, ggplot2::aes_(x=~Stressor, y=~Response, color="cluster ref", shape="cluster ref", fill="cluster ref"), size=cex_sites_cl_ref)
        } else {
          p_SR <- p_SR + ggplot2::geom_blank(ggplot2::aes(color="cluster ref", shape="cluster ref", fill="cluster ref"))
        }##IF~boo_plot_cl_ref~END
        #
        if(boo_plot_targ==TRUE){##IF~boo_plot_targ~START
          p_SR <- p_SR + ggplot2::geom_point(data=df_plot_site, ggplot2::aes_(x=~Stressor,y=~Response, color="target", shape="target", fill="target"), size=cex_sites_targ)
        } else {
          p_SR <- p_SR + ggplot2::geom_blank(ggplot2::aes(color="target", shape="target", fill="target"))
        }
        ##IF~boo_plot_targ~END
        # 
        # Add rest of plot  
        p_SR <- p_SR + ggplot2::scale_shape_manual(name=leg_name, labels=leg_labels, values=leg_shape)  + 
                       ggplot2::scale_color_manual(name=leg_name, labels=leg_labels, values=leg_col) +
                       ggplot2::scale_fill_manual(nam=leg_name, labels=leg_labels, values=leg_fill) +
                       ggplot2::stat_smooth(method=lm, color=col_line, show.legend=FALSE) + 
                       ggplot2::geom_line(data=model_cl_val, ggplot2::aes_(y=~lwr), color=col_line, linetype="dashed", show.legend=FALSE) + 
                       ggplot2::geom_line(data=model_cl_val, ggplot2::aes_(y=~upr), color=col_line, linetype="dashed", show.legend=FALSE) + 
                       ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5), plot.subtitle=ggplot2::element_text(hjust=0.5)) + 
                       ggplot2::labs(title=str_title, subtitle = str_subtitle, caption=str_caption, x=str_xlab, y=str_ylab)
        #
        print(p_SR)
        plots.pq[[pq]] <- grDevices::recordPlot()
        #
        fn_jpg <- paste0(varFileOut, stressName, "_", respName, ".jpg")
        ggplot2::ggsave(fn_jpg, p_SR, width=plot_W, height=plot_H, units="in")
        #
      }##IF.boo.Plot.END


        # # Create results data frame
        if(boo_corr==TRUE){##IF~boo_corr~START
          if (varFlag==1) {  #First time through loop
            df.CorrTable <- df.corr
          } else {
            df.CorrTable <- rbind(df.CorrTable, df.corr)  #  if not first iteration then append
          } # IF, END
          boo.Append    <- TRUE
          boo.col.names <- FALSE
          if (pq==1){##IF~pq~START
            boo.Append    <- !boo.Append
            boo.col.names <- !boo.col.names
          }##IF~pq~END
          #if(boo.pryr==TRUE){
          fn_corr <- paste0(TargetSiteID,".SR.",bio_prefix,".Corrs.txt")
          utils::write.table(df.CorrTable
                             , file.path(wd,dir.sub,dir.sub2,fn_corr)
                             , sep="\t", quote=FALSE, row.names=FALSE
                             , col.names=boo.col.names, append=boo.Append)  
          #}
          pval.corr = signif(c1S$p.value, 2)
        }##IF~boo_corr~END
        
        
        # #Print equation, r2, and p-value
        # if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
        #   eqn <- paste("Cluster regression: ", "y =", slope, "x +", intercept
        #                  , "; ", "r2 =", r2, "; ", "p-value =", pval.corr
        #                  ,"; ","n =",length(varX))
        #   symbshape <- c(1, 16, 2, 17, 19)
        #   symbcol <- c("darkgrey", "blue", "cyan4", "blue", "red")
        #   symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", 
        #                 TargetSiteID)
        #   graphics::mtext(eqn, side=1, line=4, bty="n", col=c("black"), cex=0.6)
        #   graphics::legend(varLegOpp, symbname, pch=symbshape, col=symbcol
        #                    , cex=0.6, lwd="1", bg="white")
        # }##IF.length.END
        #
        
         # Scoring ####

        if(nrow(df_plot_site)>0){##IF~nrow(df_plot_site)~END
          for (f in 1:nrow(df_plot_site)) {##FOR~f~START
            # Generate scores based on slope, significance value, and r2
            if ((length(df_plot_cl)>=5) && (abs(pval.corr)<=0.1) && (r2>=0.1)) {##IF~length~START
              # print to console p (stressName) and q (respName)
                if (slope.dir == exp.dir) {
                  #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 1")) 
                  txt.score <-  "1"
                  sr.score = 1
                } else if (slope.dir != exp.dir) {
                   # print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = -1"))
                    txt.score <- "-1"
                   sr.score = -1
                } else {
                    #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = inconclusive"))
                  txt.score <- "inconclusive"  
                  sr.score = 1
                }
            } else {
                #print(paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 0"))
              txt.score <- "0"  
              sr.score = 0
            }##IF~length~START
           #
          }##FOR~f~END
          #if (boo.pryr==TRUE) {##IF.boo.pryr.START
          msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = ", txt.score)
          print(msg.status)
          #}##IF.boo.pryr.START
          df.temp2 <- as.data.frame(cbind("StationID_Master"=TargetSiteID, # "Group" = cluster,
                                          "Param_Name"=stressName,"BMI_Metric"=respName,
                                          "n"=length(df_plot_site),#"Param_Value"=varXprime[f],
                                          #"BMI_MetricValue"=varYprime[f],
                                          "SR_Score"=sr.score))
          if (varFlag.b==1) { # First time through this loop
            df.sc.sr <- df.temp2
          } else {
            df.sc.sr <- rbind(df.sc.sr, df.temp2)
          }
          
          #if(boo.pryr==TRUE){
          fn_scores <- paste0(TargetSiteID,".SR.",bio_prefix,".Scores.txt")
          fp_scores <- file.path(wd, dir.sub, dir.sub2, fn_scores)
          
          boo.Append    <- TRUE
          boo.col.names <- FALSE
          if (file.exists(fp_scores)==FALSE){
            # can't rely on pq==1 as that may not have data
            boo.Append    <- !boo.Append
            boo.col.names <- !boo.col.names
          }
          
          utils::write.table(df.sc.sr
                             , fp_scores
                             , sep="\t", quote=FALSE, row.names=FALSE
                             , col.names=boo.col.names, append=boo.Append) 
          #}
          # Moved from inside FOR.f
        } else {
          txt.score <- "No Data"
          msg.status <- paste0("Item (", pq, "/", pq.len, "), ", stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = ", txt.score)
          print(msg.status)
        }##IF~nrow(df_plot_site)~END
        #

        #
        
     # }##plot.pryr.END
        #~~~~~~~~~~~~~~~~~~~
        # OLD plot stuff [END]
        #~~~~~~~~~~~~~~~~~~~
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
  fn_pdf <- file.path(getwd(), "Results", TargetSiteID, paste0(TargetSiteID,".SR.",bio_prefix,".ALL.pdf"))
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
  fn_corr <- paste0(TargetSiteID,".SR.",bio_prefix,".Corrs.txt")
  fp_corr <- file.path(wd, dir.sub, dir.sub2, fn_corr)
  df_corr <- read.delim(fp_corr)
  
  # QC, 20190313
  ## Special case where the function doesn't save the header row
  ### Unable to track down cause so implement QC check here.
  cn_cor_pref <- c("stressName", "respName", "statistic", "p.value", "estimate", "r2")
  cn_cor_x    <- colnames(df_corr)
  cn_cor_match <- sum(cn_cor_x %in% cn_cor_pref)
  if(cn_cor_match!=length(cn_cor_pref)){##IF~length~START
    df_corr <- read.delim(fp_corr, header = FALSE, col.names = cn_cor_pref)
    write.table(df_corr, fp_corr, sep="\t", quote=FALSE, row.names=FALSE )
  }##IF~length~END
  
  ## transpose 
  # 20190305; shouldn't need mean or unique but just in case, should be complete dups
  df_corr <- unique(df_corr)
  df_corr_r <- reshape2::dcast(df_corr, stressName ~ respName, fun.aggregate=mean, value.var="estimate"
                               , na.rm=TRUE)
  df_corrplot <- t(df_corr_r[,-1])
  colnames(df_corrplot) <- df_corr_r[,1]
  ## jpg
  fn_jpg_cp <- file.path(wd, dir.sub, dir.sub2, paste0(TargetSiteID, ".SR.",bio_prefix,".CorrPlot.jpg"))
  grDevices::jpeg(filename = fn_jpg_cp
                  , width = 4 * ppi
                  , height = 3 * ppi
                  , quality=100
                  )
    corrplot::corrplot(df_corrplot, method="circle")
  grDevices::dev.off()
  #
}##FUNCTION.END
