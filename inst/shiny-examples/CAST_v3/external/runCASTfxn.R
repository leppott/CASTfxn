# Helper functions for running CASTfxn functions within Shiny.
# Erik.Leppo@tetratech.com
# 20190605


foo_testCallHandler <- function() {
  message("Processing item, 1/94; Al2O3Cat")
  Sys.sleep(1)
  message("Processing item, 2/94; Al2O3Ws")
  Sys.sleep(1)
  message("Processing item, 3/94; BFICat")
  Sys.sleep(1)
  warning("and a warning")
}
# Only works with messages (not cat or print)
# https://github.com/daattali/advanced-shiny/tree/master/show-warnings-messages


Run_Cluster <- function() {
  shiny::withProgress({
    TargetSiteID <- "SRCKN001.61"
    #TargetSiteID <- input$Station
    dir_results <- file.path(".", "Results")
    #
    # Data getSiteInfo
    # data, example included with package
    data.Stations.Info <- CASTfxn::data_Sites
    data.SampSummary   <- CASTfxn::data_SampSummary
    data.303d.ComID    <- CASTfxn::data_303d
    data.bmi.metrics   <- CASTfxn::data_BMIMetrics
    data.algae.metrics <- CASTfxn::data_AlgMetrics
    data.mod           <- CASTfxn::data_ReachMod
    #
    # Increment the progress bar, and update the detail text.
    msgDetail_A <- "Load input data"
    msgDetail_B <- "Base Data"
    incProgress(0.1, detail = paste0(msgDetail_A, ", ", msgDetail_B))
    #
    #' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
    elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
                                           , "ElevCategory"])
    if(elev_cat=="HI"){
      data.cluster <- data_Cluster_Hi
    } else if(elev_cat=="LO") {
      data.cluster <- data_Cluster_Lo
    }
    #
    # Increment the progress bar, and update the detail text.
    incProgress(0.3, detail = "cluster")
    #
    # Map data
    # San Diego
    #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
    #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
    # AZ
    map_flowline  <- CASTfxn::data_GIS_Flow_HI
    map_flowline2 <- CASTfxn::data_GIS_Flow_LO
    if(elev_cat=="HI"){
      map_flowline <- CASTfxn::data_GIS_Flow_HI
    } else if(elev_cat=="LO") {
      map_flowline <- CASTfxn::data_GIS_Flow_LO
    }
    map_outline   <- CASTfxn::data_GIS_AZ_Outline
    # Project site data to USGS Albers Equal Area
    usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
    +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
    +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    # projection for outline
    my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
    +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    map_proj <- my.aea
    # 
    # Increment the progress bar, and update the detail text.
    msgDetail_A <- "Load input data"
    msgDetail_B <- "SiteInfo"
    incProgress(0.1, detail = paste0(msgDetail_A, ", ", msgDetail_B))
    #
    dir_sub <- "SiteInfo"
    #
    # Run getSiteInfo
    list.SiteSummary <- CASTfxn::getSiteInfo(TargetSiteID
                                             , dir_results
                                             , data.Stations.Info
                                             , data.SampSummary
                                             , data.303d.ComID
                                             , data.bmi.metrics
                                             , data.algae.metrics
                                             , data.cluster
                                             , data.mod
                                             , map_proj
                                             , map_outline
                                             , map_flowline
                                             , dir_sub=dir_sub)
    #
    # Increment the progress bar, and update the detail text.
    msgDetail_A <- "Run"
    msgDetail_B <- "SiteInfo"
    incProgress(0.1, detail = paste0(msgDetail_A, ", ", msgDetail_B))
    #
    # Data getChemDataSubsets
    # data, example included with package
    data.chem.raw <- data_Chem
    data.chem.info <- data_ChemInfo
    site.COMID <- list.SiteSummary$COMID
    site.Clusters <- list.SiteSummary$ClustIDs
    #
    # Increment the progress bar, and update the detail text.
    msgDetail_A <- "Load input data"
    msgDetail_B <- "ChemDataSubsets"
    incProgress(0.1, detail = paste0(msgDetail_A, ", ", msgDetail_B))
    #
    # Run getChemDataSubsets
    list.data <- CASTfxn::getChemDataSubsets(TargetSiteID
                                             , comid=site.COMID
                                             , cluster=site.Clusters
                                             , data.cluster=data.cluster
                                             , data.Stations.Info=data.Stations.Info
                                             , data.chem.raw=data.chem.raw
                                             , data.chem.info=data.chem.info)
    #
    # Increment the progress bar, and update the detail text.
    msgDetail_A <- "Run"
    msgDetail_B <- "ChemDataSubsets"
    incProgress(0.1, detail = paste0(msgDetail_A, ", ", msgDetail_B))
    #
    # Data getClusterInfo
    ref.reaches <- list.data$ref.reaches
    refSiteCOMIDs <- list.data$ref.reaches
    dir_sub <- "ClusterInfo"
    #
    # Increment the progress bar, and update the detail text.
    msgDetail_A <- "Load input data"
    msgDetail_B <- "ChemDataSubsets"
    incProgress(0.1, detail = paste0(msgDetail_A, ", ", msgDetail_B))
    
    # Run getClusterInfo
    getClusterInfo(site.COMID, site.Clusters, ref.reaches, dir_results, dir_sub)
    #
    # Increment the progress bar, and update the detail text.
    msgDetail_A <- "Run"
    msgDetail_B <- "ChemDataSubsets"
    incProgress(0.3, detail = paste0(msgDetail_A, ", ", msgDetail_B))
    #
  }##expr~END
  , message = "Creating Cluster Info"
  )##withProgress~END
}##Run_Cluster~END



