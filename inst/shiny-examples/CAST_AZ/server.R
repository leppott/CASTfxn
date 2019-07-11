#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Packages
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Output ####
  
  output$StationID <- renderText({
    paste0("Selected Station = ", input$Station)
  })##StationID~END
  
  output$fn_Map <- renderText({
    file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, ".map.leaflet.html"))
  })##fn_Map~END
  
  output$fe_Map <- renderText({
    paste0("Map file exists = ", file.exists(file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, ".map.leaflet.html"))))
  })##fe_Map~END
  
  output$Map_html <- renderUI(function() {
    fn_map_html <- file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, ".map.leaflet.html"))
    #
    fe_map_html <- file.exists(fn_map_html)
    #
    if(fe_map_html==TRUE){
      includeHTML(fn_map_html)
      #HTML(readLines(fn_map_html))
    } else {
      return(NULL)
    }
  })##Map_html~END
  
  # Test if zip file exists
  output$boo_zip <- function() {
    fn_zip_boo <- paste0(input$Station, "_", input$BioComm, ".zip")
    return(file.exists(file.path(".", "Results", fn_zip_boo)) == TRUE)
  }##boo_zip~END
  
  observeEvent({
    c(input$Station, input$BioComm, input$b_RunAll)
  } , {
   fn_zip_toggle <- paste0(input$Station, "_", input$BioComm, ".zip")
   toggleState(id="b_downloadData", condition = file.exists(file.path(".", "Results", fn_zip_toggle)) == TRUE)
  })##~toggleState~END
  
  
  # BUTTONS ####
  # b_download ####
  # Downloadable csv of selected dataset
  output$b_downloadData <- downloadHandler(
    # use index and date time as file name
    #myDateTime <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    filename = function() {
      paste0(input$Station, "_", input$BioComm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(fname) {##content~START
      # tmpdir <- tempdir()
      #setwd(tempdir())
      file.copy(file.path(".", "Results", paste0(input$Station, "_", input$BioComm, ".zip")), fname)
      #
    }##content~END
    #, contentType = "application/zip"
  )##downloadData~END
  

  # Run CASTfxn ####
  
  # foo_testCallHandler <- function() {
  #   message("Processing item, 1/94; Al2O3Cat")
  #   Sys.sleep(1)
  #   message("Processing item, 2/94; Al2O3Ws")
  #   Sys.sleep(1)
  #   message("Processing item, 3/94; BFICat")
  #   Sys.sleep(1)
  #   warning("and a warning")
  # }
  # Only works with messages (not cat or print)
  # https://github.com/daattali/advanced-shiny/tree/master/show-warnings-messages
  
  
  Run_Map <- function(){
    shiny::withProgress({
      #
      # Number of increments
      n_inc <- 4
      #
      #TargetSiteID <- "SRCKN001.61"
      TargetSiteID <- input$Station
      dir_results <- file.path(".", "Results")
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      incProgress(1/n_inc, detail = "data frames")
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
      incProgress(1/n_inc, detail = "cluster")
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
      incProgress(1/n_inc, detail = "Load input data")
      #
      dir_sub <- "SiteInfo"
      #
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
      # Increment the progress bar, and update the detail text.
      incProgress(1/n_inc, detail = "Function")
      #
    }##expr~END
    , message = "Creating BioStressoResponses"
    )##withProgress~END
  }##Run_Map~END
  
  
  Run_Cluster <- function() {
    shiny::withProgress({
      #
      # Number of increments
      n_inc <- 8
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Base Data"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      #TargetSiteID <- "SRCKN001.61"
      TargetSiteID <- input$Station
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
      msgDetail_A <- "SiteInfo"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
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
      dir_sub <- "SiteInfo"
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "SiteInfo"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
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
      msgDetail_A <- "ChemDataSubsets"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getChemDataSubsets
      # data, example included with package
      data.chem.raw  <- data_Chem
      data.chem.info <- data_ChemInfo
      site.COMID     <- list.SiteSummary$COMID
      site.Clusters  <- list.SiteSummary$ClustIDs
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ChemDataSubsets"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
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
      msgDetail_A <- "Cluster"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getClusterInfo
      ref.reaches   <- list.data$ref.reaches
      refSiteCOMIDs <- list.data$ref.reaches
      dir_sub <- "ClusterInfo"
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Cluster"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getClusterInfo
      getClusterInfo(site.COMID, site.Clusters, ref.reaches, dir_results, dir_sub)
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Cluster function"
      msgDetail_B <- "COMPLETE"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
    }##expr~END
    , message = "Creating Cluster Info"
    )##withProgress~END
  }##Run_Cluster~END
  
  Run_Candidate <- function(){
    shiny::withProgress({
      #
      # Number of increments
      n_inc <- 8
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Base Data"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Example 1, BMI
      #TargetSiteID <- "SRCKN001.61"
      TargetSiteID <- input$Station
      dir_results  <- file.path(".", "Results")
      #biocomm      <- "bmi"
      biocomm      <- input$BioComm
      #
      # datasets getSiteInfo
      # data, example included with package
      data.Stations.Info <- data_Sites       # need for getSiteInfo and getChemDataSubsets
      data.SampSummary   <- data_SampSummary
      data.303d.ComID    <- data_303d
      data.bmi.metrics   <- data_BMIMetrics
      data.algae.metrics <- data_AlgMetrics
      data.mod           <- data_ReachMod
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "SiteInfo"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
      elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
                                             , "ElevCategory"])
      if(elev_cat=="HI"){
        data.cluster <- data_Cluster_Hi
      } else if(elev_cat=="LO") {
        data.cluster <- data_Cluster_Lo
      }
      
      # Map data
      # San Diego
      #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
      #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
      # AZ
      map_flowline  <- data_GIS_Flow_HI
      map_flowline2 <- data_GIS_Flow_LO
      if(elev_cat=="HI"){
        map_flowline <- data_GIS_Flow_HI
      } else if(elev_cat=="LO") {
        map_flowline <- data_GIS_Flow_LO
      }
      map_outline   <- data_GIS_AZ_Outline
      # Project site data to USGS Albers Equal Area
      usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
      +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
      +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      # projection for outline
      my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
      +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      map_proj <- my.aea
      #
      dir_sub <- "SiteInfo" 
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "SiteInfo"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getSiteInfo
      list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
                                      , data.SampSummary, data.303d.ComID
                                      , data.bmi.metrics, data.algae.metrics
                                      , data.cluster, data.mod
                                      , map_proj, map_outline, map_flowline
                                      , dir_sub=dir_sub)
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ChemDataSubsets"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getChemDataSubsets
      # data import, example 
      # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
      # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
      site.COMID     <- list.SiteSummary$COMID
      site.Clusters  <- list.SiteSummary$ClustIDs
      # data, example included with package
      data.chem.raw  <- data_Chem
      data.chem.info <- data_ChemInfo
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ChemDataSubsets"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getChemDataSubsets
      list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
                                      , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
                                      , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
      #
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Stressor List"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getStressorList
      chem.info     <- list.data$chem.info
      cluster.chem  <- list.data$cluster.chem
      cluster.samps <- list.data$cluster.samps
      ref.sites     <- list.data$ref.sites
      site.chem     <- list.data$site.chem
      dir_sub       <- "CandidateCauses"
      #
      # set cutoff for possible stressor identification
      probsLow  <- 0.10
      probsHigh <- 0.90 
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Stressor List"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getStressorList
      list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
                                        , cluster.samps, ref.sites, site.chem
                                        , probsHigh, probsLow, biocomm, dir_results
                                        , dir_sub)
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Candidate Causes"
      msgDetail_B <- "COMPLETE"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      }##expr~END
      , message = "Creating Candidate Causes"
    )##withProgress~END
  }##Run_Candidate~END
  
  Run_CoOccur <- function(){
    shiny::withProgress({
      #
      # Number of increments
      n_inc <- 3
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Load input data"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Example #2, AZ data (single site)
      #
      #TargetSiteID <- c("SRCKN001.61")
      TargetSiteID <- input$Station
      #
      #
      # Cluster Data based on elevation category
      boo_Lo <- TargetSiteID %in% CASTfxn::data_CoOccur_AZ_Lo$StationID_Master
      if(boo_Lo==TRUE){
        df.data <- CASTfxn::data_CoOccur_AZ_Lo
      } else {
        df.data <- CASTfxn::data_CoOccur_AZ_Hi
      }
      #
      col.Group     <- "Group"
      col.Bio       <- "IBI"
      col.Stressors <- c("Calcium_uf_mg_L", "Copper_uf_ug_L", "DO_f_mg_L", "SpecCond_umhos_cm")
      col.ID        <- "StationID_Master"
      #
      Bio.Nar.Brk <- c(0, 45, 52, 100)
      Bio.Nar.Lab <- c("Most Disturbed", "Intermediate", "Least Disturbed")
      Bio.Deg.Brk <- c(0, 45, 100)
      Bio.Deg.Lab <- c("Yes", "No")
      biocomm <- "bmi"
      #biocomm <- input$BioComm
      dir.plots <- file.path(".", "Results")
      dir_sub <- "CoOccurrence"
      col.Stressors.InvSc <- c("DO_f_.", "DO_f_mg_L", "DO_f_unk", "DOSat_f_.", "DOSat_f_unk", "pH_SU")
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "CoOccurrence"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      getCoOccur(df.data, TargetSiteID, col.ID, col.Group, col.Bio, col.Stressors
                 , Bio.Nar.Brk, Bio.Nar.Lab, Bio.Deg.Brk, Bio.Deg.Lab
                 , biocomm, dir.plots, dir_sub, col.Stressors.InvSc
      )
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "CoOccurrence"
      msgDetail_B <- "COMPLETE"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
    }##expr~END
    , message = "Creating Co-Occurrence plots"
    )##withProgress~END
  }##Run_CoOccur~END
  
  Run_BSR <- function(){
    shiny::withProgress({
      #
      # Number of increments
      n_inc <- 13
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Base Data"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Example 1, BMI
      #TargetSiteID <- "SRCKN001.61"
      TargetSiteID <- input$Station
      dir_results  <- file.path(".", "Results")
      #biocomm      <- "bmi"
      biocomm      <- input$BioComm
      #
      # datasets getSiteInfo
      # data, example included with package
      data.Stations.Info <- data_Sites       # need for getSiteInfo and getChemDataSubsets
      data.SampSummary   <- data_SampSummary
      data.303d.ComID    <- data_303d
      data.bmi.metrics   <- data_BMIMetrics
      data.algae.metrics <- data_AlgMetrics
      data.mod           <- data_ReachMod
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "SiteInfo"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
      elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
                                             , "ElevCategory"])
      if(elev_cat=="HI"){
        data.cluster <- data_Cluster_Hi
      } else if(elev_cat=="LO") {
        data.cluster <- data_Cluster_Lo
      }
      
      # Map data
      # San Diego
      #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
      #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
      # AZ
      map_flowline  <- data_GIS_Flow_HI
      map_flowline2 <- data_GIS_Flow_LO
      if(elev_cat=="HI"){
        map_flowline <- data_GIS_Flow_HI
      } else if(elev_cat=="LO") {
        map_flowline <- data_GIS_Flow_LO
      }
      map_outline   <- data_GIS_AZ_Outline
      # Project site data to USGS Albers Equal Area
      usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
      +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
      +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      # projection for outline
      my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
      +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      map_proj <- my.aea
      #
      dir_sub <- "SiteInfo" 
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "SiteInfo"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getSiteInfo
      list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
                                      , data.SampSummary, data.303d.ComID
                                      , data.bmi.metrics, data.algae.metrics
                                      , data.cluster, data.mod
                                      , map_proj, map_outline, map_flowline
                                      , dir_sub=dir_sub)
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ChemDataSubsets"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getChemDataSubsets
      # data import, example 
      # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
      # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
      site.COMID     <- list.SiteSummary$COMID
      site.Clusters  <- list.SiteSummary$ClustIDs
      # data, example included with package
      data.chem.raw  <- data_Chem
      data.chem.info <- data_ChemInfo
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ChemDataSubsets"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getChemDataSubsets
      list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
                                      , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
                                      , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
      #
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Stressor List"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getStressorList
      chem.info     <- list.data$chem.info
      cluster.chem  <- list.data$cluster.chem
      cluster.samps <- list.data$cluster.samps
      ref.sites     <- list.data$ref.sites
      site.chem     <- list.data$site.chem
      dir_sub       <- "CandidateCauses"
      #
      # set cutoff for possible stressor identification
      probsLow  <- 0.10
      probsHigh <- 0.90 
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Stressor List"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getStressorList
      list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
                                        , cluster.samps, ref.sites, site.chem
                                        , probsHigh, probsLow, biocomm, dir_results
                                        , dir_sub)
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Matches"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getBioMatches, BMI
      ## remove "none"
      stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
      stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
      LogTransf <- stressors_logtransf
      #
      if(biocomm=="bmi"){
        data.bio.metrics <- data_BMIMetrics
      } else if(biocomm=="algae"){
        data.bio.metrics <- data_AlgMetrics
      }
      
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Matches"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getBioMatches
      list.MatchBioData <- getBioMatches(stressors, list.data, list.SiteSummary, data.SampSummary
                                         , data.chem.raw, data.bio.metrics, biocomm)
      
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Stressor Responses"
      msgDetail_B <- "Load input data"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getBioStressorResponses, BMI 
      if(biocomm=="bmi"){
        BioResp <- c("IBI", "TotalTaxSPL_Sc", "DipTaxSPL_Sc"
                     , "IntolTaxSPL_Sc", "HBISPL_Sc", "PlecoPct_Sc", "ScrapPctSPL_Sc"
                     , "TrichTax_Sc", "EphemTax_Sc", "EphemPct_Sc", "Dom01PctSPL_Sc")
      } else if(biocomm=="algae"){
        BioResp <- colnames(data.bio.metrics[6:52])
      }
       
      dir_sub <- "StressorResponse"
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Stressor Responses"
      msgDetail_B <- "Run"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Run getBioStressorResponses, BMI               
      getBioStressorResponses(TargetSiteID, stressors, BioResp, list.MatchBioData
                              , LogTransf, ref.sites, biocomm, dir_results, dir_sub)
      
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Stressor Responses"
      msgDetail_B <- "COMPLETE"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
    }##expr~END
    , message = "Creating BioStressor Responses"
    )##withProgress~END
  }##Run_BSR~END
  
  Run_VP <- function(){
    withProgress({
      #TargetSiteID <- "SRCKN001.61"
      TargetSiteID <- input$Station
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
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
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
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
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
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
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
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
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
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
      # Data getClusterInfo
      ref.reaches <- list.data$ref.reaches
      refSiteCOMIDs <- list.data$ref.reaches
      dir_sub <- "ClusterInfo"
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Load input data"
      msgDetail_B <- "ChemDataSubsets"
      incProgress(1/n_inc, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      
      # Run getClusterInfo
      getClusterInfo(site.COMID, site.Clusters, ref.reaches, dir_results, dir_sub)
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Run"
      msgDetail_B <- "ChemDataSubsets"
      incProgress(0.3, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      #
    }##expr~END
    , message = "Creating Verified Predictions"
    )##withProgress~END
  }##Run_VP~END
  
  Run_ALL <- function(){
    shiny::withProgress({
      #
      # Number of increments
      n_prog <- 23 # confirmed 20190703
      mySleepTime <- 0.5
      #
      # Remove Zip ####
      fn_zip_results <- list.files(file.path(".", "Results"), ".zip", full.names = TRUE)
      if(length(fn_zip_results)>0){
        file.remove(fn_zip_results)
      }##IF~length(fn_zip_results)~END
      #
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getSiteInfo ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Base Data"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Example 1, BMI
      #TargetSiteID <- "SRCKN001.61"
      TargetSiteID <- input$Station
      dir_results  <- file.path(".", "Results")
      #biocomm      <- "bmi"
      biocomm      <- input$BioComm
      #
      # datasets getSiteInfo
      # data, example included with package
      data.Stations.Info <- data_Sites       # need for getSiteInfo and getChemDataSubsets
      data.SampSummary   <- data_SampSummary
      data.303d.ComID    <- data_303d
      data.bmi.metrics   <- data_BMIMetrics
      data.algae.metrics <- data_AlgMetrics
      data.mod           <- data_ReachMod
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "SiteInfo"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
      elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
                                             , "ElevCategory"])
      if(elev_cat=="HI"){
        data.cluster <- data_Cluster_Hi
      } else if(elev_cat=="LO") {
        data.cluster <- data_Cluster_Lo
      }

      # Map data
      # San Diego
      #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
      #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
      # AZ
      map_flowline  <- data_GIS_Flow_HI
      map_flowline2 <- data_GIS_Flow_LO
      if(elev_cat=="HI"){
        map_flowline <- data_GIS_Flow_HI
      } else if(elev_cat=="LO") {
        map_flowline <- data_GIS_Flow_LO
      }
      map_outline   <- data_GIS_AZ_Outline
      # Project site data to USGS Albers Equal Area
      usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      # projection for outline
      my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      map_proj <- my.aea
      #
      dir_sub <- "SiteInfo"
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "SiteInfo"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Run getSiteInfo
      list.SiteSummary <- getSiteInfo(TargetSiteID
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

      # getChemDataSubsets
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ChemDataSubsets"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Data getChemDataSubsets
      # data import, example
      # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
      # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
      site.COMID     <- list.SiteSummary$COMID
      site.Clusters  <- list.SiteSummary$ClustIDs
      # data, example included with package
      data.chem.raw  <- data_Chem
      data.chem.info <- data_ChemInfo
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ChemDataSubsets"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Run getChemDataSubsets
      list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
                                      , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
                                      , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getStressorList ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Stressor List"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Data getStressorList
      chem.info     <- list.data$chem.info
      cluster.chem  <- list.data$cluster.chem
      cluster.samps <- list.data$cluster.samps
      ref.sites     <- list.data$ref.sites
      site.chem     <- list.data$site.chem
      dir_sub       <- "CandidateCauses"
      #
      # set cutoff for possible stressor identification
      probsLow  <- 0.10
      probsHigh <- 0.90
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Stressor List"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Run getStressorList
      list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
                                        , cluster.samps, ref.sites, site.chem
                                        , probsHigh, probsLow, biocomm, dir_results
                                        , dir_sub)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getBioMatches ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Matches"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Data getBioMatches, BMI
      ## remove "none"
      stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
      stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
      LogTransf <- stressors_logtransf
      #
      if(biocomm=="bmi"){
        data.bio.metrics <- data_BMIMetrics
      } else if(biocomm=="algae"){
        data.bio.metrics <- data_AlgMetrics
      }

      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Matches"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Run getBioMatches
      list.MatchBioData <- getBioMatches(stressors, list.data, list.SiteSummary, data.SampSummary
                                         , data.chem.raw, data.bio.metrics, biocomm)


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getBioStressorResponses ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Stressor Responses"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Data getBioStressorResponses, BMI
      if(biocomm=="bmi"){
        BioResp <- c("IBI", "TotalTaxSPL_Sc", "DipTaxSPL_Sc"
                     , "IntolTaxSPL_Sc", "HBISPL_Sc", "PlecoPct_Sc", "ScrapPctSPL_Sc"
                     , "TrichTax_Sc", "EphemTax_Sc", "EphemPct_Sc", "Dom01PctSPL_Sc")
      } else if(biocomm=="algae"){
        BioResp <- colnames(data.bio.metrics[6:52])
      }

      dir_sub <- "StressorResponse"
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Bio Stressor Responses"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Run getBioStressorResponses, BMI
      getBioStressorResponses(TargetSiteID, stressors, BioResp, list.MatchBioData
                              , LogTransf, ref.sites, biocomm, dir_results, dir_sub)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Functions above are needed for the functions below.
      # (getSiteInfo, getChemDataSet, getStressorList, getBioMatches)
      # The functions below are all end points; 
      # i.e., not used as inputs for other functions.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getClusterInfo ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Cluster"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Data getClusterInfo
      site.COMID <- list.SiteSummary$COMID
      site.Clusters <- list.SiteSummary$ClustIDs
      #
      ref.reaches   <- list.data$ref.reaches
      refSiteCOMIDs <- list.data$ref.reaches
      dir_sub <- "ClusterInfo"
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Cluster"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      # #
      # Run getClusterInfo
      getClusterInfo(TargetSiteID, site.COMID, site.Clusters, ref.reaches
                        , data.cluster, dir_results, dir_sub) 
      
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getCoOccur ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "CoOccurrence"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)

      # Cluster Data based on elevation category
      boo_Lo <- TargetSiteID %in% CASTfxn::data_CoOccur_AZ_Lo$StationID_Master
      if(boo_Lo==TRUE){
        df.data <- CASTfxn::data_CoOccur_AZ_Lo
      } else {
        df.data <- CASTfxn::data_CoOccur_AZ_Hi
      }
      #
      col.Group     <- "Group"
      col.Bio       <- "IBI"
      col.Stressors <- c("Calcium_uf_mg_L", "Copper_uf_ug_L", "DO_f_mg_L", "SpecCond_umhos_cm")
      col.ID        <- "StationID_Master"
      #
      Bio.Nar.Brk <- c(0, 45, 52, 100)
      Bio.Nar.Lab <- c("Most Disturbed", "Intermediate", "Least Disturbed")
      Bio.Deg.Brk <- c(0, 45, 100)
      Bio.Deg.Lab <- c("Yes", "No")
      biocomm <- "bmi"
      #biocomm <- input$BioComm
      dir.plots <- file.path(".", "Results")
      dir_sub <- "CoOccurrence"
      col.Stressors.InvSc <- c("DO_f_.", "DO_f_mg_L", "DO_f_unk", "DOSat_f_.", "DOSat_f_unk", "pH_SU")
      #
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "CoOccurrence"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Run, getCoOccur
      getCoOccur(df.data, TargetSiteID, col.ID, col.Group, col.Bio, col.Stressors
                 , Bio.Nar.Brk, Bio.Nar.Lab, Bio.Deg.Brk, Bio.Deg.Lab
                 , biocomm, dir.plots, dir_sub, col.Stressors.InvSc
      )

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getVerifiedPredictions ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Verified Predictions"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      # 
      # data, example included with package
      data.bio.taxa.raw  <- data_BMIcounts
      data.SSTV.totabund <- data_BMIRelAbund
      BioIndex_Val       <- "IBI"
      BioIndex_Nar       <- "NarRat"
      BioIndex_Nar_Deg   <- "Violates"
      dir_sub            <- "VerifiedPredictions"
      biocomm <- "bmi"

      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Verified Predictions"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Run, getVerifiedPredictions
      getVerifiedPredictions(TargetSiteID
                             , data.SampSummary
                             , data.bio.taxa.raw
                             , data.chem.info
                             , data.SSTV.totabund
                             , data.MT.bio
                             , list.MatchBioData
                             , ref.sites
                             , BioIndex_Val
                             , BioIndex_Nar
                             , BioIndex_Nar_Deg
                             , dir_results
                             , dir_sub)


      # 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getWoE ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Weight of Evidence"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      # 

      # 
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Weight of Evidence"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      getWoE(TargetSiteID
             , df.rank = list.stressors$site.stressor.pctrank
             , df.coOccur = data.bmi.coOccur
             , biocomm = "bmi"
             , index = "IBI"
             , dir_results = file.path(".", "Results")
             , CO_sub = "CoOccurrence"
             , SR_sub = "StressorResponse"
             , VP_sub = "VerifiedPredictons"
             , SSD_sub = "SSD")
      
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # getReport ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Report"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      # 
      dir_results <- file.path(".", "Results")
      report_type <- "summary"
      report_format <- "html"
      
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Report"
      msgDetail_B <- "Run"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #
      # Run, getReport
      getReport(TargetSiteID, dir_results, report_type, report_format)
      # 
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create Zip File ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Zip"
      msgDetail_B <- "create File"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      
      # Create zip file
      fn_zip_contents <- list.files(file.path(".", "Results", TargetSiteID), full.names = TRUE)
      fn_zip <- paste0(input$Station, "_", input$BioComm, ".zip")
      zip(file.path(".", "Results", fn_zip), fn_zip_contents)
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Complete ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ALL"
      msgDetail_B <- "COMPLETE"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime * 10)
      #
      
      #
    }, message = "Run ALL")##witProgress~END
  }##Run_ALL~END
  
  # 00RunAll ####
  
  observeEvent(input$b_RunAll, {
    withCallingHandlers({
      shinyjs::html(id="text_console_ALL", html="")
      # Run function that shows console output
      Run_ALL()
      }
      , message = function(m) {
        shinyjs::html(id = "text_console_ALL", html = m$message, add = FALSE)
      }
      , warning = function(m) {
        shinyjs::html(id = "text_console_ALL", html = paste0(" ... ", m$message), add = TRUE)
      })##withCallingHandlers~END
  })##observeEvent~input$b_RunAll~ENDs
  
  
  # 01Map ####
  
  observeEvent(input$Create01Map, {
    # Console messages to Shiny
    #https://deanattali.com/blog/advanced-shiny-tips/
    #
    # No messages to capture
    #
    withCallingHandlers({
      shinyjs::html(id = "text_console_Map", html = "")
      # Run function to capture console output
      #foo_testCallHandler()
      Run_Map()
    }
    , message = function(m) {
      shinyjs::html(id = "text_console_Map", html = m$message, add = FALSE)
    }
    , warning = function(m) {
      shinyjs::html(id = "text_console_Map", html = paste0(" ... ", m$message), add = TRUE)
    })##withCallingHandlers~END
    
  })##observeEvent~Create01Map
  
  # 02Cluster ####
  
  observeEvent(input$Create02ClusterInfo, {
    withCallingHandlers({
      shinyjs::html(id = "txt_console_Cluster", html = "")
      # Run function to capture console output
      #foo_testCallHandler()
      Run_Cluster()
    }
    , message = function(m) {
      shinyjs::html(id = "txt_console_Cluster", html = m$message, add = FALSE)
    }
    , warning = function(m) {
      shinyjs::html(id = "txt_console_Cluster", html = paste0(" ... ", m$message), add = TRUE)
    })##withCallingHandlers~END
  })##observeEvent~Create02Cluster
  
  # 03Candidate ####
  # No messages to capture
  observeEvent(input$Create03CandidateCauses, {
    withCallingHandlers({
      shinyjs::html(id = "txt_console_Candidate", html = "")
      # Run function to capture console output
      #foo_testCallHandler()
      Run_Candidate()
    }
    , message = function(m) {
      shinyjs::html(id = "txt_console_Candidate", html = m$message, add = FALSE)
    }
    , warning = function(m) {
      shinyjs::html(id = "txt_console_Candidate", html = paste0(" ... ", m$message), add = TRUE)
    })##withCallingHandlers~END
  })##observeEvent~Create03CandidateCauses
  
  # 04Co-Occur ####
  observeEvent(input$Create04CoOccur, {
    withCallingHandlers({
      shinyjs::html(id = "txt_console_CoOccur", html = "")
      # Run function to capture console output
      #foo_testCallHandler()
      Run_CoOccur()
    }
    , message = function(m) {
      shinyjs::html(id = "txt_console_CoOccur", html = m$message, add = FALSE)
    }
    , warning = function(m) {
      shinyjs::html(id = "txt_console_CoOccur", html = paste0(" ... ", m$message), add = TRUE)
    })##withCallingHandlers~END
    #
    # display results
    #fn_img <- list.files(file.path(".", "Results", input$Station, "CoOccurrence"), ".jpg")
    # create HTML from RMD
    
    #
  })##observeEvent~Create04CoOccur
  
  # 05SR ####
  observeEvent(input$Create05BioStressorResponses, {
    withCallingHandlers({
      shinyjs::html(id = "txt_console_SR", html = "")
      # Run function to capture console output
      #foo_testCallHandler()
      Run_BSR()
    }
    , message = function(m) {
      shinyjs::html(id = "txt_console_SR", html = m$message, add = FALSE)
    }
    , warning = function(m) {
      shinyjs::html(id = "txt_console_SR", html = paste0(" ... ", m$message), add = TRUE)
    })##withCallingHandlers~END
  })##observeEvent~Create05SR
  
  # 06VP ####
  
  observeEvent(input$Create06VerifiedPredictions, {
    withCallingHandlers({
      shinyjs::html(id = "txt_console_VP", html = "")
      # Run function to capture console output
      #foo_testCallHandler()
      Run_VP()
    }
    , message = function(m) {
      shinyjs::html(id = "txt_console_VP", html = m$message, add = FALSE)
    }
    , warning = function(m) {
      shinyjs::html(id = "txt_console_VP", html = paste0(" ... ", m$message), add = TRUE)
    })##withCallingHandlers~END
  })##observeEvent~Create06VP
  
  # 07Results ####
  
  # output$downloadData <- downloadHandler(
  #   filename <- function() {
  #     paste0(input$Station, ".zip")
  #   }##filename~END
  #   , content <- function(file) {
  #     # zip file name
  #     fn_zip <- paste0(input$Station, ".zip")
  #     # Generate Zip file
  #     utils::zip(file.path(., "Results", fn_zip), file.path(getwd(), "Results", input$Station))
  #     # Copy to user "file"
  #     fn_copy_from <- file.path(., "Results", fn_zip)
  #     file.copy(fn_copy_from, file)
  #   }##content~END
  #   , contentType = "application/zip"
  # )##downloadData~END
  # 
  #  
  # output$downloadData_Test <- downloadHandler(
  #   filename <- function() {
  #     paste("tst", "zip", sep=".")
  #   },
  # 
  #   content <- function(file) {
  #     file.copy("test.zip", file)
  #   },
  #   contentType = "application/zip"
  # )##downloadData_Test~END
  # #outputOptions(output, "downloadData_Test", suspendWhenHidden=FALSE)
  # # https://groups.google.com/forum/#!topic/shiny-discuss/TWikVyknHYA
  # 
  # 
  # 
  # observeEvent(input$CreateZip, {
  #   fn_zip <- paste0(input$Station, ".zip")
  #   # Generate Zip file
  #   #utils::zip(file.path(getwd(), "Results", fn_zip), file.path(getwd(), "Results", input$Station))
  #   file.copy(file.path(., "Results", "test.zip"), file.path(., "Results", "test2.zip"))
  #   #
  #   # communicate that it is done to the user?!  file.exists?
  #   #
  # })##observeEvent~CreateZip~END
  
  
  
})##server~END
