#' @title Biological and Chemistry data matches.
#' 
#' @description Get Biological (Algae or BMI) samples and chemistry sample matches.
#' 
#' @details Matched biological (algae/BMI) and chem samples.
#' 
#' Required objects:
#' 
#' * data.SampSumamry; StationID_Master, CollDate, ChemSampleID, PhabSampID, BMI.Metrics.SampID, Algae.Metrics.SampID
#' 
#' * data.chem.raw; StationID_Master, ChemSampleID
#' 
#' @param stressors stressors
#' @param list.data data list
#' @param list.SiteSummary Site summary data; output of getSiteInfo function.
#' @param data.SampSummary x
#' @param data.chem.raw x
#' @param data.bio.metrics Biological metric data (algae or BMI).
#' @param biocomm Biological community; algae or BMI.  Default = "BMI"
#' 
#' @return A summary list; all.b.str, cl.b.str, site.b.str, all.b.rsp, cl.b.rsp
#' , and site.b.rsp.
#' 
#' @examples
#' # Example 1, BMI
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
#' 
#' # Data getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites        # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.cluster       <- data_Cluster_Hi   # need for getSiteInfo and getChemDataSubsets
#' data.mod           <- data_ReachMod
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
#' 
#' # Run getBioMatches
#' biocomm <- "BMI"
#' data.bio.metrics <- data_BMIMetrics
#' list.MatchBMIData <- getBioMatches(stressors, list.data,  list.SiteSummary, data.SampSummary, data.chem.raw, data.bio.metrics, biocomm)
#' 
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example 2, Algae
#' TargetSiteID <- "LCBEN002.57"
#' dir_results <- file.path(getwd(), "Results")
#' 
#' # Data getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites        # need for getSiteInfo and getChemDataSubsets
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
#'
#' # Run getBioMatches
#' biocomm <- "algae"
#' data.bio.metrics <- data_AlgMetrics
#' list.MatchAlgData <- getBioMatches(stressors, list.data, list.SiteSummary, data.SampSummary, data.chem.raw, data.bio.metrics, biocomm)
#' 
# 
#' @export
getBioMatches <- function(stressors, list.data, list.SiteSummary, data.SampSummary, data.chem.raw, data.bio.metrics, biocomm="BMI") {##FUNCTION.START
  # QC
  biocomm <- tolower(biocomm)
  #
  all.chems <- list.data[["all.chems"]]
  cl.chems  <- list.data[["cluster.chem"]]
  site.chem <- list.data[["site.chem"]]
  
  # Check for no data
  if(biocomm=="bmi"){##IF.biocomm.START
    #
    if (nrow(list.SiteSummary$BMImetrics)==0) {##IF.nrow.bmi.START
      # No BMI Responses Found
      print(paste0("No BMI response data available for ", TargetSiteID,
                   ". Regression data illustrate cluster relationships only."))
      utils::flush.console()
    }##IF.nrow.bmi.END
    #
  } else if(biocomm=="algae"){
    #
    if (nrow(list.SiteSummary$AlgMetrics)==0) {##IF.nrow.alg.START
      # No Algae Responses Found
      print(paste("No algae response data available for ", TargetSiteID,
                  ". Regression data illustrate cluster relationships only.",
                  sep = ""))
      utils::flush.console()
    }##IF.nrow.alg.END
    #
  } else {
    # Non Valid biological community
    Msg_Stop <- print(paste0("Non-valid biological community specified (", biocomm,"). Only values of 'bmi' and 'algae' are valid."))
    stop(Msg_Stop)
  }##IF.biocomm.END

  # CHEM
  # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
  # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
  # These aren't in all.chems, because they don't have data corresponding to the site data
  useChemSamps <- all.chems$ChemSampleID
  mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
  
  # Samps ####
  #
  ## Samps, Algae
  # malg.Samps <- stats::na.omit(data.SampSummary[,c("ChemSampleID",
  #                                                  "Algae.Metrics.SampID")])
  # malg.use.samps <- subset(malg.Samps, malg.Samps$ChemSampleID %in% mUseSamps)
  # # Clean up
  # malg.use.samps$Algae.Metrics.SampID <- 
  #   stringr::str_remove(malg.use.samps$Algae.Metrics.SampID, "_EMAP")
  # malg.use.samps$Algae.Metrics.SampID <- 
  #   stringr::str_remove(malg.use.samps$Algae.Metrics.SampID, "_Multihabitat")
  #
  ## Samps, BMI
  # mbmi.Samps <- stats::na.omit(data.SampSummary[,c("ChemSampleID","BMI.Metrics.SampID")])
  # mbmi.use.samps <- subset(mbmi.Samps, mbmi.Samps$ChemSampleID %in% mUseSamps)
  # 
  # Stressors ####
  #
  ## Stressors, ALL
  all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
  all.str.samps[is.na(all.str.samps)] <- NA
  all.stress <- merge(unique(data.chem.raw[,c("StationID_Master", "ChemSampleID")])
                      , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
  all.stress <- all.stress[,colSums(is.na(all.stress)) < nrow(all.stress)]
  #
  ## Stressors, Algae
  # alg stresor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
  # all.malg.stress <- subset(all.stress, ChemSampleID %in% malg.use.samps$ChemSampleID)
  # all.malg.stress <- merge(malg.use.samps, all.malg.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
  # cl.malg.stress <- subset(all.malg.stress, ChemSampleID %in% cl.chems$ChemSampleID)
  # site.malg.stress <- subset(all.malg.stress, ChemSampleID %in% site.chem$ChemSampleID)
  #
  ## Stressors, BMI
  # bmi stressor data to use: all.mbmi.stress, cl.mbmi.stress, and site.stress
  # all.mbmi.stress <- subset(all.stress, ChemSampleID %in% mbmi.use.samps$ChemSampleID)
  # all.mbmi.stress <- merge(mbmi.use.samps, all.mbmi.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
  # cl.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% cl.chems$ChemSampleID)
  # site.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% site.chem$ChemSampleID)
  
  # Response ####
  #
  ## Response, Algae
  # alg response data to use: all.malg.resp, cl.malg.resp, and site.malg.resp
  # all.malg.resp <- subset(data.algae.metrics, Algae.Metrics.SampID %in% malg.use.samps$Algae.Metrics.SampID)
  # cl.chems1 <- merge(cl.chems, all.malg.resp[,c("StationID_Master", "Algae.Metrics.SampID")]
  #                    , by.x = "StationID_Master", by.y = "StationID_Master")
  # cl.malg.resp <- subset(all.malg.resp, Algae.Metrics.SampID %in% cl.chems1$Algae.Metrics.SampID)
  # site.chem1 <- as.data.frame(stringr::str_remove(site.chem$ChemSampleID, "_\\d{4}\\-\\d{2}\\-\\d{2}"))
  # colnames(site.chem1)[1] <- "StationID_Master"
  # site.chem1 <- merge(site.chem1, all.malg.resp[,c("StationID_Master", "Algae.Metrics.SampID")]
  #                     , by.x = "StationID_Master", by.y = "StationID_Master")
  # site.chem1 <- unique(site.chem1)
  # site.malg.resp <- subset(all.malg.resp, Algae.Metrics.SampID %in% site.chem1$Algae.Metrics.SampID)
  #
  # 20190301, use same procedure for alg as bmi
  # all.malg.resp.0 <- subset(data.algae.metrics, Algae.Metrics.SampID %in% malg.use.samps$Algae.Metrics.SampID)
  # all.malg.resp   <- merge(malg.use.samps, all.malg.resp.0, by.x = "Algae.Metrics.SampID", by.y = "Algae.Metrics.SampID")
  # cl.malg.resp    <- subset(all.malg.resp, ChemSampleID %in% cl.chems$ChemSampleID)
  # site.malg.resp  <- subset(all.malg.resp, ChemSampleID %in% site.chem$ChemSampleID)
  
  #
  ## Response, BMI
  # bmi response data to use: all.mbmi.resp, cl.mbmi.resp, and site.mbmi.resp
  # all.mbmi.resp.0 <- subset(data.bmi.metrics, BMISampID %in% mbmi.use.samps$BMI.Metrics.SampID)
  # all.mbmi.resp   <- merge(mbmi.use.samps, all.mbmi.resp.0, by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
  # cl.mbmi.resp    <- subset(all.mbmi.resp, ChemSampleID %in% cl.chems$ChemSampleID)
  # site.mbmi.resp  <- subset(all.mbmi.resp, ChemSampleID %in% site.chem$ChemSampleID)
  
  ## Stressor and Response, Bio
  if(biocomm=="bmi"){##IF~biocom-str/resp~START
    # Samps
    mbio.Samps <- stats::na.omit(data.SampSummary[,c("ChemSampleID","BMI.Metrics.SampID")])
    mbio.use.samps <- subset(mbio.Samps, mbio.Samps$ChemSampleID %in% mUseSamps)
    # Stressor
    all.mbio.stress  <- subset(all.stress, ChemSampleID %in% mbio.use.samps$ChemSampleID)
    all.mbio.stress  <- merge(mbio.use.samps, all.mbio.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
    cl.mbio.stress   <- subset(all.mbio.stress, ChemSampleID %in% cl.chems$ChemSampleID)
    site.mbio.stress <- subset(all.mbio.stress, ChemSampleID %in% site.chem$ChemSampleID)
    # Response
    all.mbio.resp.0 <- subset(data.bio.metrics, BMISampID %in% mbio.use.samps$BMI.Metrics.SampID)
    all.mbio.resp   <- merge(mbio.use.samps, all.mbio.resp.0, by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
    cl.mbio.resp    <- subset(all.mbio.resp, ChemSampleID %in% cl.chems$ChemSampleID)
    site.mbio.resp  <- subset(all.mbio.resp, ChemSampleID %in% site.chem$ChemSampleID)
  } else if(biocomm=="algae"){
    # Samps
    mbio.Samps <- stats::na.omit(data.SampSummary[,c("ChemSampleID",
                                                     "Algae.Metrics.SampID")])
    mbio.use.samps <- subset(mbio.Samps, mbio.Samps$ChemSampleID %in% mUseSamps)
    # Clean up
    mbio.use.samps$Algae.Metrics.SampID <- 
      stringr::str_remove(mbio.use.samps$Algae.Metrics.SampID, "_EMAP")
    mbio.use.samps$Algae.Metrics.SampID <- 
      stringr::str_remove(mbio.use.samps$Algae.Metrics.SampID, "_Multihabitat")
    # Stressor
    all.mbio.stress  <- subset(all.stress, ChemSampleID %in% mbio.use.samps$ChemSampleID)
    all.mbio.stress  <- merge(mbio.use.samps, all.mbio.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
    cl.mbio.stress   <- subset(all.mbio.stress, ChemSampleID %in% cl.chems$ChemSampleID)
    site.mbio.stress <- subset(all.mbio.stress, ChemSampleID %in% site.chem$ChemSampleID)
    # Response
    all.mbio.resp.0 <- subset(data.bio.metrics, Algae.Metrics.SampID %in% mbio.use.samps$Algae.Metrics.SampID)
    all.mbio.resp   <- merge(mbio.use.samps, all.mbio.resp.0, by.x = "Algae.Metrics.SampID", by.y = "Algae.Metrics.SampID")
    cl.mbio.resp    <- subset(all.mbio.resp, ChemSampleID %in% cl.chems$ChemSampleID)
    site.mbio.resp  <- subset(all.mbio.resp, ChemSampleID %in% site.chem$ChemSampleID)
  }##IF~biocom-str/resp~END
  

  # Output ####
  # if(biocomm=="bmi"){##IF.biocomm.START
  #   #
  #   myMatchData <- list(all.b.str    = all.mbmi.stress
  #                       , cl.b.str   = cl.mbmi.stress
  #                       , site.b.str = site.mbmi.stress
  #                       , all.b.rsp  = all.mbmi.resp
  #                       , cl.b.rsp   = cl.mbmi.resp
  #                       , site.b.rsp = site.mbmi.resp)
  #   #
  # } else if(biocomm=="algae"){
  #   #
  #   myMatchData <- list(all.a.str    = all.malg.stress
  #                       , cl.a.str   = cl.malg.stress
  #                       , site.a.str = site.malg.stress
  #                       , all.a.rsp  = all.malg.resp
  #                       , cl.a.rsp   = cl.malg.resp
  #                       , site.a.rsp = site.malg.resp)
  #   #
  # }##IF.biocomm.END
  
  myMatchData <- list(all.a.str    = all.mbio.stress
                      , cl.a.str   = cl.mbio.stress
                      , site.a.str = site.mbio.stress
                      , all.a.rsp  = all.mbio.resp
                      , cl.a.rsp   = cl.mbio.resp
                      , site.a.rsp = site.mbio.resp)
  #
  return(myMatchData)
  #
}##FUNCTION.END

# Potential Updates
# Merged getAlgMatches and getBMIMatches.
# Could convert some objects to "bio" instead of "alg" and "bmi".
# then only have to do the operation once.
