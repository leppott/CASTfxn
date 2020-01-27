#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Packages
#library(shiny)
options(shiny.maxRequestSize=100*1024^2) # increase max file upload to 100 MB

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Output ####
  
  output$StationID <- renderText({
    paste0("Selected Station = ", input$Station)
  })##StationID~END
  
  output$fn_Map <- renderText({
    file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, "_map_leaflet.html"))
  })##fn_Map~END
  
  output$fe_Map <- renderText({
    paste0("Map file exists = ", file.exists(file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, "_map_leaflet.html"))))
  })##fe_Map~END
  


  
  output$Map_html <- renderUI({
    getHTML(file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, "_map_leaflet.html")))
    # #
    # fn_map_html <- file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, "_map_leaflet.html"))
    # #
    # fe_map_html <- file.exists(fn_map_html)
    # #
    # if(fe_map_html==TRUE){
    #   return(includeHTML(fn_map_html))
    #   #HTML(readLines(fn_map_html))
    # } else {
    #   return(NULL)
    # }
  })##Map_html~END
  
  output$Disclaimer_html <- renderUI({
    getHTML(file.path(".", "www", "Disclaimer_Key.html"))
    #
    # fn_disclaimer_html <- file.path(".", "data", "Disclaimer_Key.html")
    # #
    # fe_disclaimer_html <- file.exists(fn_disclaimer_html)
    #
    #if(fe_disclaimer_html==TRUE){
    #  includeHTML(file.path(".", "data", "Disclaimer_Key.html"))
      #HTML(readLines(fn_map_html))
    #} else {
    #  NULL
    #}
    
    
  })##Disclaimer_html~END
  
  # Test if zip file exists
  output$boo_zip <- function() {
    fn_zip_boo <- paste0(input$Station, "_", input$BioComm, ".zip")
    return(file.exists(file.path(".", "Results", fn_zip_boo)) == TRUE)
  }##boo_zip~END
  
  observeEvent({
    c(input$Station, input$BioComm, input$b_RunAll)
  } , {
   fn_zip_toggle <- paste0(input$Station, ".zip")
   toggleState(id="b_downloadData", condition = file.exists(file.path(".", "Results", fn_zip_toggle)) == TRUE)
  })##~toggleState~END
  
  observeEvent({
    input$Station
  }, {
    TargetSiteID <- input$Station
    CopyResults(TargetSiteID)
  })##~CopyResults
  
  
  # BUTTONS ####
  # b_download ####
  # Downloadable csv of selected dataset
  output$b_downloadData <- downloadHandler(
    # use index and date time as file name
    #myDateTime <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    filename = function() {
      paste0(input$Station, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(fname) {##content~START
      # tmpdir <- tempdir()
      #setwd(tempdir())
      file.copy(file.path(".", "Results", paste0(input$Station, ".zip")), fname)
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
      # fn_zip_results <- list.files(file.path(".", "Results"), ".zip", full.names = TRUE)
      # if(length(fn_zip_results)>0){
      #   file.remove(fn_zip_results)
      # }##IF~length(fn_zip_results)~END
      # Remove only the current station's zip file
      TargetSiteID <- input$Station
      fn_zip <- file.path(".", "Results", paste0(TargetSiteID, ".zip"))
      if (file.exists(fn_zip)==TRUE){
        file.remove(fn_zip)
      }##IF~file.exists~END
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Load Ann's files and such
      gitpath <- file.path(".", "external") # might need something different
      source(file.path(gitpath, "getCoOccurDataset.R"))
      source(file.path(gitpath, "getTimeSeq.R"))
      source(file.path(gitpath, "getWoE.R"))
      source(file.path(gitpath, "getDataSets.R"))
      source(file.path(gitpath, "getComparators.R"))
      source(file.path(gitpath, "getDataGaps.R"))
      # source(file.path(gitpath, "getSiteBackground.R"))
      source(file.path(gitpath, "getQualSites.R"))
      source(file.path(gitpath, "getSiteInfo.R"))
      source(file.path(gitpath, "getClusterInfo.R"))
      source(file.path(gitpath, "getStressorList.R"))
      source(file.path(gitpath, "getCoOccur.R"))
      source(file.path(gitpath, "getBioStressorResponses.R"))
      source(file.path(gitpath, "getVerifiedPredictions.R"))
      
      # Timer, Start
      startprep.time <- Sys.time()
      
      # Start Parameters 
      robsHigh=0.75
      probsLow=0.25
      lagdays=10
      biocommlist <- c("bmi","algae")
      # Specify the "good" sites to call out on graphics
      # siteQual2Plot = "reference"
      # siteQual2Plot = "better than"
      siteQual2Plot = "not degraded"
      report_format="html"    # word, pdf are the other options
      
      # Data filenames
      dir_data <- file.path(".", "Data") # update in case the wd was changed
      # Specify Base Filenames # These are the files used to run the analyses
      fn.targets <- file.path(dir_data,"SMCTestSites.xlsx")
      fn.Sites.Info <- file.path(dir_data,"SMCSitesFinal.tab")
      fn.SampSummary <- file.path(dir_data,"SMCSiteSummary.tab")
      fn.cheminfo <- file.path(dir_data,"SMCMeasStressInfoFinal.tab")
      fn.chemdata <- file.path(dir_data,"SMCMeasStressDataFinal.tab")
      fn.modelinfo <- file.path(dir_data,"SMCModelStressInfoFinal.tab")
      fn.modeldata <- file.path(dir_data,"SMCModelStressDataFinal.tab")
      fn.bmi.metrics <- file.path(dir_data,"SMCBenthicMetricsFinal.tab")
      fn.bmi.raw <- file.path(dir_data, "SMCBenthicCountsFinal.tab")
      fn.MT.bmi <- file.path(dir_data, "SMCBenthicMasterTaxa.tab")
      fn.alg.metrics <- file.path(dir_data, "SMCAlgaeMetrics.tab")
      fn.alg.raw <- file.path(dir_data, "SMCAlgaeCountsFinal.tab")
      fn.MT.alg <- file.path(dir_data, "SMCAlgaeMasterTaxa.tab")
      fn.bcdist <- file.path(dir_data, "SMCBCDist.tab")
      fn.cluster <- file.path(dir_data, "SMCClusterData.tab")
      fn.clusterinfo <- file.path(dir_data,"SMCClusterInfo.tab")
      fn.bkgdata <- file.path(dir_data, "SMCSiteBkgdData.tab")
      fn.bkginfo <- file.path(dir_data, "SMCSiteBkgdInfo.tab")
      # Load GIS files
      message("Loading GIS files.")
      outline <- rgdal::readOGR(dsn = "Data/SMCBoundary", layer = "SMCBoundary_aea")
      flowline <- rgdal::readOGR(dsn = "Data/SMCReaches", layer = "SMCReaches_aea")
      
      # Specify user-defined variables
      # Stressors
      meas.stress <- c("ChemSampleID", "PhabSampID", "FldChemSampID")
      chem.stress <- c("ChemSampleID", "FldChemSampID")
      hab.stress <- "PhabSampID"
      mod.stress <- c("FlowBMISampID", "FlowAlgSampID")
      
      # BMI responses
      bmi_thresholds <- c(-2, 0.62, 0.799, 0.919, 2)
      bmi_narrative <- c("very likely altered", "likely altered"
                         , "possibly altered", "likely intact")
      bmi_deg_thres <- c(-2, 0.799, 2)
      bmi_deg_text <- c("Yes", "No")
      bmiIndex <- "CSCI"
      bmiIndexGp <- c("CSCI", "OoverE", "MMI")
      bmiResp <- "BMISampID"
      bmiRespDate <- "BMISampDate"
      bmiMetrics <- c(bmiIndex, "MMI", "OoverE", "Taxonomic_Richness"
                      , "Intolerant_Percent", "Shredder_Taxa", "Clinger_PercentTaxa"
                      , "Coleoptera_PercentTaxa", "EPT_PercentTaxa")
      
      # Algal responses
      alg_thresholds <- c(-2, 0.82, 2)
      alg_narrative <- c("Degraded", "Not Degraded")
      alg_deg_thres <- c(-2, 0.82, 2)
      alg_deg_text <- c("Yes", "No")
      algIndex <- "MMIhybrid"
      algIndexGp <- c("MMIhybrid", "MMIdiatom", "MMIsba")
      algResp <- "AlgSampID"
      algRespDate <- "AlgSampDate"
      algMetrics <- c("MMIdiatom"
                      , "propsppOxyReqDO_10_rawdiatom"
                      , "cntsppBCG3_rawdiatom"
                      , "propCyclotella_rawdiatom"
                      , "propSurirella_rawdiatom"
                      , "propsppIndicatorClass_TP_low_rawdiatom"
                      , "propsppOrgNNHHONF_rawdiatom"
                      , "MMIsba"
                      , "cntsppBCG3_rawsba"
                      , "propsppIndicatorClass_NonRef_rawsba"
                      , "propsppGreen_rawsba"
                      , "cntsppIndicatorClass_Cu_high_rawsba"
                      , "cntsppIndicatorClass_TP_high_rawsba"
                      , "cntsppIndicatorClass_DOC_high_rawsba"
                      , "propsppmosttol_rawsba"
                      , algIndex
                      , "cntsppBCG3_rawhybrid"
                      , "propCyclotella_rawhybrid"
                      , "propsppOxyReqDO_10_rawhybrid"
                      , "propSurirella_rawhybrid"
                      , "propsppIndicatorClass_DOC_high_rawhybrid"
                      , "propsppIndicatorClass_Cu_high_rawhybrid"
                      , "propsppOrgNNHHONF_rawhybrid"
                      , "propsppIndicatorClass_TN_low_rawhybrid")
      alg_thresholds <- c(-2, 0.82, 2)
      alg_deg_thres <- c(-2, 0.82, 2)
      algResp <- "AlgSampID"
      
      # USGS aea for SoCal is below
      socal.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
                +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
                +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      # aea used for AZ is below
      # az.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
      #             +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      my.aea = socal.aea
      
      #~~~~~~~~~~~~~~~~~~~~~~~
      # Read datafiles
      ## Get site location info and other metadata (e.g., waterbody name)
      data_Sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t")
      rm(fn.Sites.Info)
      
      # Get sample summary data
      data_SampSummary <- read.delim(fn.SampSummary, header = TRUE, sep = "\t")
      data_mods        <- data_ReachMod   # Check this
      data_303d  <- data_303d       # Check this
      rm(fn.SampSummary)
      
      # CAST, Chem & other measured data ####
      ## Get metadata for all measured stressors
      data_chemInfo <- read.delim(fn.cheminfo, header = TRUE, sep = "\t")
      data_chemInfo <- mutate(data_chemInfo, Analyte = StdParamName)
      colMeasInvScore = as.vector(data_chemInfo$StdParamName[data_chemInfo$DirIncStress=="Dec"])
      SSTVparms <- data_chemInfo$StdParamName[data_chemInfo$SSTV==1]
      rm(fn.cheminfo)
      
      # Get metadata for modeled stressor data
      data_modelInfo <- read.delim(fn.modelinfo, header = TRUE, sep = "\t")
      data_modelInfo <- mutate(data_modelInfo, Analyte = StdParamName)
      colModelInvScore = as.vector(data_modelInfo$StdParamName[data_modelInfo$DirIncStress=="Dec"])
      rm(fn.modelinfo)
      
      # Combine metadata for all stressor into one datafile
      chemMetaNames <- colnames(data_chemInfo)
      modelMetaNames <- colnames(data_modelInfo)
      extraNames <- chemMetaNames[!(chemMetaNames %in% modelMetaNames)]
      for (e in 1:length(extraNames)) {
        newCol <- extraNames[e]
        data_modelInfo[[newCol]] <- NA
      }
      data_modelInfo <- data_modelInfo[,chemMetaNames]
      data_stressInfo <- rbind(data_chemInfo, data_modelInfo)
      
      ## Get measured stressor values
      data_chemAll <- read.delim(fn.chemdata, header = TRUE, sep = "\t",
                                 na.strings = "NA")
      analytes      <- data_stressInfo$StdParamName[data_stressInfo$UseInStressorID == 1]
      data_chemRaw <- data_chemAll[data_chemAll$StdParamName %in% analytes,]
      data_chemRaw <- data_chemRaw %>%
        mutate(SampleDate = lubridate::mdy(SampDate)) %>%
        select(StationID_Master, ChemSampleID, SampDate, StdParamName
               , ResultValue, SampleDate) %>%
        group_by(StationID_Master, ChemSampleID, SampDate, StdParamName
                 , SampleDate) %>%
        summarize(MeanResultValue = mean(ResultValue)) %>%
        rename(ResultValue = MeanResultValue)
      data_chemRaw <- unique(data_chemRaw)
      rm(fn.chemdata, data_chemAll)
      measParams <- as.vector(unique(data_chemRaw$StdParamName))
      
      # Get modeled stressor data
      data_modelAll <- read.delim(fn.modeldata, header = TRUE, sep = "\t")
      useParams      <- data_modelInfo$StdParamName[data_modelInfo$UseInStressorID == 1]
      data_modelRaw <- data_modelAll[data_modelAll$StdParamName %in% useParams,]
      data_modelRaw <- data_modelRaw %>%
        mutate(SampYear = lubridate::year(lubridate::mdy(SampDate))) %>%
        select(StationID_Master, ChemSampleID, SampDate, StdParamName
               , ResultValue)
      rm(fn.modeldata, data_modelAll)
      modelParams <- as.vector(unique(data_modelRaw$StdParamName))
      
      # Prepare df_allStress file
      data_modeltrim <- as.data.frame(data_modelRaw) %>%
        dplyr::select(StationID_Master, ChemSampleID, StdParamName, ResultValue) %>%
        dplyr::mutate(SampleDate = NA)
      data_meastrim <- as.data.frame(data_chemRaw) %>%
        dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate, ResultValue)
      data_Stress <- rbind(data_meastrim, data_modeltrim)
      
      # Combine measured and modeled parameters with inverse scoring
      col_StressInvScore <- c(colMeasInvScore, colModelInvScore)
      
      # CAST, BMI, metrics ####
      data_bmiMetrics <- read.delim(fn.bmi.metrics, header = TRUE, sep = "\t",
                                    na.strings = "NA", stringsAsFactors = FALSE)
      data_bmiMetrics <- data_bmiMetrics[,c("StationID_Master", "BMISampID"
                                            , "BMISampDate", "Quality", "CSCI"
                                            , "MMI", "OoverE", "Taxonomic_Richness"
                                            , "Intolerant_Percent", "Shredder_Taxa"
                                            , "Clinger_PercentTaxa"
                                            , "Coleoptera_PercentTaxa"
                                            , "EPT_PercentTaxa")]
      data_bmiMetrics <- data_bmiMetrics[, unlist(lapply(data_bmiMetrics,
                                                         function(x) !all(is.na(x))))]
      colnames(data_bmiMetrics) <- c("StationID_Master","BMISampID"
                                     , "CollDate", "Quality", "CSCI", "MMI"
                                     , "OoverE", "Taxonomic_Richness"
                                     , "Intolerant_Percent", "Shredder_Taxa"
                                     , "Clinger_PercentTaxa", "Coleoptera_PercentTaxa"
                                     , "EPT_PercentTaxa")
      data_bmiMetrics <- data_bmiMetrics %>%
        mutate(BMISampDate = lubridate::mdy(CollDate)) %>%
        select(-CollDate)
      rm(fn.bmi.metrics)
      
      # CAST, BMI taxonomic data ####
      data_bmiTaxaRaw <- read.table(fn.bmi.raw, header = TRUE, sep = "\t")
      
      data_MTbmi <- read.table(fn.MT.bmi, header = TRUE, sep = "\t",
                               stringsAsFactors = FALSE)
      # data_bmiTaxaRaw <- mutate(data_bmiTaxaRaw, BMI.Metrics.SampID = BMISampID)
      rm(fn.bmi.raw, fn.MT.bmi)
      
      # Generate co-occurrence data set (same day samples; modeled data match any day)
      data_bmiCoOccur <- getCoOccurDataset(dataDir = dir_data
                                           , df_sites = data_Sites
                                           , df_model = data_modelRaw
                                           , df_meas = data_chemRaw
                                           , biocomm = "BMI"
                                           , df_resp = data_bmiMetrics
                                           , index = bmiIndex
                                           , lagdays = lagdays)
      # returns df_coOccur as data_bmiCoOccur
      # write.table(data_bmiCoOccur, file.path(getwd(),"Results","bmiCoOccur.tab")
      #             ,append=FALSE,col.names = TRUE, row.names = FALSE, sep = "\t")
      
      # # CAST, Alg, metrics ####
      data_algMetrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
      rm(fn.alg.metrics)
      # 
      # # CAST, BMI taxonomic data ####
      data_algTaxaRaw <- read.table(fn.alg.raw, header = TRUE, sep = "\t")
      
      data_MTalg <- read.table(fn.MT.alg, header = TRUE, sep = "\t",
                               stringsAsFactors = FALSE)
      rm(fn.alg.raw, fn.MT.alg)
      # 
      # # Generate co-occurrence data set (same day samples; modeled data match any day)
      data_algCoOccur <- getCoOccurDataset(dataDir = dir_data
                                           , df_sites = data_Sites
                                           , df_model = data_modelRaw
                                           , df_meas = data_chemRaw
                                           , biocomm = "Alg"
                                           , df_resp = data_algMetrics
                                           , index = algIndex
                                           , lagdays = lagdays)
      # returns df_coOccur as data_algCoOccur
      # write.table(data_algCoOccur, file.path(getwd(),"Results","algCoOccur.tab")
      #             ,append=FALSE,col.names = TRUE, row.names = FALSE, sep = "\t")
      
      # Get cluster data
      data_cluster <- read.delim(fn.cluster, header = TRUE, sep = "\t")
      rm(fn.cluster)
      
      # Get cluster data metadata
      data_clusterInfo <- read.delim(fn.clusterinfo, header = TRUE, sep = "\t")
      rm(fn.clusterinfo)
      
      # Get background data (StreamCat)
      df_bkgdata <- read.table(fn.bkgdata, header = TRUE, sep = "\t"
                               , na.strings = c("","NA"))
      
      # Get background metadata
      df_bkginfo <- read.table(fn.bkginfo, header = TRUE, sep = "\t"
                               , na.strings = c("", "NA")
                               , stringsAsFactors = FALSE)
      
      # Get BC dissimilarity distance matrix to subset cluster sites to comparators
      data_BCdist <- read.delim(fn.bcdist, header = TRUE, sep = "\t")
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # RUN CASTool
      # Site Selection ####
      df_targets <- read_excel(fn.targets, col_names = TRUE, trim_ws = TRUE, skip = 0)
      
      endprep.time <- Sys.time()
      elapsedprep.time <- endprep.time - startprep.time
      message(paste("Prep completed in", elapsedprep.time))
      # flush.console()
      
      for (site in 1:nrow(df_targets)) {
        startsite.time <- Sys.time()
        #TargetSiteID = "801RB8197"  
        #TargetSiteID = "403S01784"  # all stressor data
        #TargetSiteID <- df_targets$TargetSiteID[site]
        #TargetSiteID <- input$Station
        
        message(paste0("Evaluating site: ",TargetSiteID))
        # flush.console()
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Biocomm-independent functions
        
        # Create high-level results folder structure
        dir_sub1 <- "Results"
        dir_sub2 <- TargetSiteID
        ifelse(!dir.exists(dir_sub1)==TRUE
               , dir.create(dir_sub1)
               , FALSE)
        ifelse(!dir.exists(file.path(dir_sub1, dir_sub2))==TRUE
               , dir.create(file.path(dir_sub1, dir_sub2))
               , FALSE)
        dir_results = file.path(wd, dir_sub1)
        
        # Establish data gaps file
        gaps <- cbind.data.frame("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = FALSE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        
        # Identify comparator sites
        list.CompSites <- getComparators(TargetSiteID
                                         , df_sites = data_Sites
                                         , useBC = TRUE
                                         , df_bcdist = data_BCdist
                                         , bc_cutoff = 0.05
                                         , dir_results = dir_results
                                         , dir_sub = "SiteInfo")
        # Returns: myCompSites <- list(comp.sites = comp.sites
        #                             , gap.compsites = gap.statement
        comp_sites <- list.CompSites$comp.sites
        message("getComparators is complete.")
        # flush.console()
        
        # Get site information for general use (map, sample summary, etc)
        # Map plots only ref sites, and that's probably for the best
        list.SiteSummary <- getSiteInfo(TargetSiteID = TargetSiteID
                                        , data_Sites = data_Sites
                                        , data_bkgdata = df_bkgdata
                                        , data_bkginfo = df_bkginfo
                                        , data_SampSummary = data_SampSummary
                                        , data_303d = data_303d
                                        , data_bmiMetrics = data_bmiMetrics
                                        , bmiIndexGp = bmiIndexGp
                                        , data_algMetrics = data_algMetrics
                                        , algIndexGp = algIndexGp
                                        , comp_sites = comp_sites
                                        , data_cluster = data_cluster
                                        , data_mods = data_mods
                                        , map_proj = my.aea
                                        , map_outline = outline
                                        , map_flowline = flowline
                                        , map_flowline2 = NULL
                                        , dir_photo = file.path(getwd(),"Data","Photos")
                                        , dir_results = dir_results
                                        , dir_sub = "SiteInfo")
        # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo, 
        #                                Samps = mySamps, 
        #                                BMImetrics = myBMImetrics, 
        #                                AlgMetrics = myAlgaeMetrics, 
        #                                COMID = myCOMID, 
        #                                ClustID = myClustID,
        #                                impair = myImpairments,
        #                                mods = myReachMods
        #                                refCOMIDs = myRefCOMIDs)
        message("getSiteInfo is complete.")
        # flush.console()
        
        # Get Cluster Info
        getClusterInfo(TargetSiteID
                       , siteCOMID=list.SiteSummary$COMID
                       , siteCluster=list.SiteSummary$ClustID
                       # , siteQual2Plot="Reference"
                       , refSiteCOMIDs=list.SiteSummary$refCOMIDs
                       , data_cluster = data_cluster
                       , data_clusterInfo = data_clusterInfo
                       , dir_results=file.path(wd,"Results")
                       , dir_sub="ClusterInfo")
        message("getClusterInfo is complete.")
        # flush.console()
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Prepare flags for types of stressor and response data to use
        avail.data <- data_SampSummary[data_SampSummary$StationID_Master == TargetSiteID,]
        avail.data <- avail.data[,c(7:ncol(avail.data))]
        not_all_na <- function(x) {!all(is.na(x))}
        avail.data <- avail.data %>% select_if(not_all_na)
        samptypes <- names(avail.data)
        
        if (any(samptypes %in% meas.stress)) { # Either chem or phab samps exist
          useMeasStress = TRUE
          if (!any(samptypes %in% chem.stress)) {         # No chem samps
            gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0
                                                , "No chemistry stressors available.")
            colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
            
            gap.phab.stress <- cbind.data.frame("general", "HabStress", 1
                                                , "Habitat stressors available.")
            colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
            
          } else if (!any(samptypes %in% hab.stress)) {   # No habitat samps
            gap.phab.stress <- cbind.data.frame("general", "HabStress", 0
                                                , "No habitat stressors available.")
            colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
            
            gap.chem.stress <- cbind.data.frame("general", "ChemStress", 1
                                                , "Chemistry stressors available.")
            colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
          } else {
            gap.phab.stress <- cbind.data.frame("general", "HabStress", 1
                                                , "Habitat stressors available.")
            colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
            
            gap.chem.stress <- cbind.data.frame("general", "ChemStress", 1
                                                , "Chemistry stressors available.")
            colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
          }
          df_allStress <- data_chemRaw
          df_allStressInfo <- data_chemInfo
        } else {    # No measured stressors at all
          useMeasStress = FALSE
          gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0, "No chemistry stressors available.")
          colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
          
          gap.phab.stress <- cbind.data.frame("general", "HabStress", 0, "No habitat stressors available.")
          colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
        } ### End If statement for measured stressors
        
        if (any(samptypes %in% mod.stress)) {
          useModStress = TRUE
          gap.mod.stress <- cbind.data.frame("general", "useModStress", 1, "Modeled stressors available.")
          colnames(gap.mod.stress) <- c("fxnname", "condition", "result", "comment")
          if (exists("df_allStress")==TRUE) {
            df_allStress <- rbind(df_allStress, df_modelRaw)
          }
        } else { 
          useModStress = FALSE 
          gap.mod.stress <- cbind.data.frame("general", "useModStress", 0, "No modeled stressors available.")
          colnames(gap.mod.stress) <- c("fxnname", "condition", "result", "comment")
        } ### End If statement for modeled stressors
        
        if (any(samptypes == bmiResp)) {
          useBMI = TRUE
          gap.bmi.rsp <- cbind.data.frame("general", "useBMI", 1, "BMI responses available.")
          colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
        } else{
          useBMI = FALSE
          gap.bmi.rsp <- cbind.data.frame("general", "useBMI", 0, "No BMI responses available.")
          colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
        } ### End If statement for benthic macroinvertebrate responses
        
        if (any(samptypes == alg.resp)) {
          useAlg = TRUE
          gap.alg.rsp <- cbind.data.frame("general", "useALG", 1, "Algae responses available.")
          colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
        } else {
          useAlg = FALSE
          gap.alg.rsp <- cbind.data.frame("general", "useBMI", 0, "No algae responses available.")
          colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
        } ### End If statement for measured stressorsalgal responses
        
        gaps <- rbind.data.frame(gap.chem.stress, gap.phab.stress, gap.mod.stress
                                 , gap.bmi.rsp, gap.alg.rsp)
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
        
        for (b in 1:length(biocommlist)) {
          
          # Define biocomm data
          bioComm <- biocommlist[b]
          if ((bioComm=="bmi") && (useBMI==TRUE)) {
            
            data_bioCoOccur <- data_bmiCoOccur
            bioIndex <- bmiIndex
            bioIndexGp <- bmiIndexGp
            bioMetricNames <- bmiMetrics
            bioMetricData <- data_bmiMetrics
            bioTaxaData <- data_BMIcounts
            bioMasterTaxa <- data_BMIMasterTaxa
            colBio <- bmiIndex
            colBioSample <- bmiResp
            colBioSampDate <- bmiRespDate
            BioNarBrk <- bmi_thresholds
            BioNarLab <- bmi_narrative
            BioDegBrk <- bmi_deg_thres
            BioDegLab <- bmi_deg_text
            
          } else if ((bioComm=="algae") && (useAlg==TRUE)) {
            
            data_bioCoOccur <- data_bmiCoOccur
            bioIndex <- algIndex
            bioIndexGp <- algIndexGp
            bioMetricNames <- algMetrics
            bioMetricData <- data_algMetrics
            bioTaxaData <- data_Algcounts
            bioMasterTaxa <- data_AlgMasterTaxa
            colBio <- algIndex
            colBioSample <- algResp
            colBioSampDate <- algRespDate
            BioNarBrk <- alg_thresholds
            BioNarLab <- alg_narrative
            BioDegBrk <- alg_deg_thres
            BioDegLab <- alg_deg_text
            
          } else {
            message(paste0(bioComm, " is not a valid biological community at this time."))
          }
          
          # Run analyses
          # Identify "quality" sites using different definitions
          list.BioQualSites <- getQualSites(TargetSiteID
                                            , df_sites = data_Sites
                                            , biocomm = "bmi"
                                            , df_qual = data_bioCoOccur
                                            , colBio = colBio
                                            , colBioSample = "RespSampID"
                                            , colStressSample = "StressSampID"
                                            , BioDegBrk = BioDegBrk
                                            , BioDegLab = c("Yes", "No"))
          # Returns: myQualSites <- list(allRefBioSites = all.ref
          #                              , allRefBioRespSamps = all.ref.samps.bio
          #                              , allRefBioStressSamps = all.ref.samps.stress
          #                              , allRefBioReaches = all.ref.reaches
          #                              , allGoodBioSites = all.good
          #                              , allGoodBioRespSamps = all.samp.good.bio
          #                              , allGoodBioStressSamps = all.samp.good.stress
          #                              , allGoodBioReaches = all.good.reaches
          #                              , allBTBioSites = all.better
          #                              , allBTBioRespSamps = all.samp.better.bio
          #                              , allBTBioStressSamps = all.samp.better.stress
          #                              , allBTBioReaches = all.better.reaches)
          
          allBioRefSites <- switch(siteQual2Plot
                                   , "reference"=list.BioQualSites$allRefBioSites
                                   , "not degraded"=list.BioQualSites$allGoodBioSites
                                   , "better than"=list.BioQualSites$allBTBioSites)
          allBioRefRespSamps <- switch(siteQual2Plot
                                       , "reference"=list.BioQualSites$allRefBioRespSamps
                                       , "not degraded"=list.BioQualSites$allGoodBioRespSamps
                                       , "better than"=list.BioQualSites$allBTBioRespSamps)
          allBioRefStressSamps <- switch(siteQual2Plot
                                         , "reference"=list.BioQualSites$allRefBioStressSamps
                                         , "not degraded"=list.BioQualSites$allGoodBioStressSamps
                                         , "better than"=list.BioQualSites$allBTBioStressSamps)
          allBioRefReaches <- switch(siteQual2Plot
                                     , "reference"=list.BioQualSites$allRefBioReaches
                                     , "not degraded"=list.BioQualSites$allGoodBioReaches
                                     , "better than"=list.BioQualSites$allBTBioReaches)
          message(paste0("getQualSites is complete for ", bioComm, "."))
          # flush.console()        
          
          # Get data sets for stressors paired with response data, if available
          listPairedStressResp <- getDataSets(TargetSiteID
                                              , compSites = comp_sites
                                              , df_coOccur = data_bioCoOccur
                                              , measParams = measParams
                                              , modelParams = modelParams
                                              , biocomm = "bmi"
                                              , bioIndex = bioIndex
                                              , colBioSample = colBioSample
                                              , colBioSampDate = colBioSampDate
                                              , df_biometrics = bioMetricData
                                              , df_stressinfo = data_stressInfo)
          # Returns: mySubsets <- list(siteStressInfo = df_stressinfo
          #                   , allBioStress = allBioStressData
          #                   , compBioStress = compBioStressData
          #                   , siteBioStress = siteBioStressData
          #                   , allBioResp = allBioRespData
          #                   , compBioResp = compBioRespData
          #                   , siteBioResp = siteBioRespData)
          message("Stressor and response data prepared, for all possible stressors.")
          # flush.console()
          
          compPairedSR <- listPairedStressResp$compBioStress %>%
            select(-StressSampDate, -RespSampDate, -RespSampID)
          sitePairedSR <- listPairedStressResp$siteBioStress %>%
            select(-StressSampDate, -RespSampDate, -RespSampID)
          sitePairedStressors <- as.vector(colnames(sitePairedSR[,3:ncol(sitePairedSR)]))
          
          # Prepare data sets of all stressors ever detected at the target site
          siteStressAll <- data_Stress %>%
            dplyr::filter(StationID_Master==TargetSiteID) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            tidyr::spread(key=StdParamName, value=ResultValue) %>%
            dplyr::select_if(not_all_na) %>%
            dplyr::rename(StressSampID = ChemSampleID
                          , StressSampDate = SampleDate)
          siteDetectsAll <- as.vector(colnames(siteStressAll[,4:ncol(siteStressAll)]))
          compStressAll <- data_Stress %>%
            dplyr::filter(StationID_Master %in% comp_sites) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(StdParamName %in% siteDetectsAll) %>%
            tidyr::spread(key=StdParamName, value=ResultValue) %>%
            dplyr::rename(StressSampID = ChemSampleID
                          , StressSampDate = SampleDate)
          siteRespAll <- bioMetricData %>%
            dplyr::filter(StationID_Master == TargetSiteID) %>%
            dplyr::rename(RespSampID = eval(colBioSample)
                          , RespSampDate = BMISampDate)
          
          # Get Stressor List using all stressors ever detected at the target site
          list.stressors <- getStressorList(TargetSiteID
                                            , siteCluster=list.SiteSummary$ClustID
                                            , chemInfo=data_stressInfo
                                            , clusterChem=compStressAll
                                            , siteQual2Plot=siteQual2Plot
                                            , refSamps=allBioRefStressSamps
                                            , siteChem=siteStressAll
                                            , probsHigh=probsHigh
                                            , probsLow=probsLow
                                            , biocomm="bmi"
                                            , dir_results=dir_results
                                            , dir_sub="CandidateCauses")
          # Returns: myStressors <- list(stressors = stressorlist
          #                     , site.stressor.pctrank = site.pctrank
          #                     , stressors_LogTransf)
          stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
          stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
          message("getStressorList is complete.")
          # flush.console()
          
          stressorsNOpairing <- setdiff(stressors, sitePairedStressors)
          stressorsWPairedResponses <- intersect(stressors, sitePairedStressors)
          
          # If no stressors are identified, no analyses can be performed. Error msg.
          if (length(stressors) == 0) {
            message(paste("No stressors identified for", TargetSiteID))
            # flush.console()
            
            # No identified stressors may be a data gap, but may not be, either
            gapcomment <- paste0("No potential stressors fall outside the specified "
                                 , "quantile range (", probsLow, " to ", probsHigh,").")
            gap.candidates <- cbind.data.frame("getStressorList", "Number of stressors", 0
                                               , gapcomment)
            colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gap.statement, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            
            next()
          } ### End no stressors statement
          if (length(stressorsNOpairing)>0) {
            for (s in 1:length(stressorsNOpairing)) {
              # Candidate causes identified as possible stressors but without
              # paired response data to allow evaluation
              gapcomment <- "Stressor detected but paired response not available"
              gaps <- cbind.data.frame("getStressorList", stressorsNOpairing[s], 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
            }
          } ### End unpaired stressors statement
          
          # Either all are paired or some are
          stressors_logtransf <- data_stressInfo$LogTransf[data_stressInfo$StdParamName 
                                                           %in% stressorsWPairedResponses]
          
          # Create time sequence graphics
          # Uses all site stressor and response data, but not paired
          getTimeSeq(TargetSiteID
                     , stressors
                     , biocomm = "BMI"
                     , BioResp = bioMetricNames
                     , df_stress = siteStressAll ### Need to be not just paired data
                     , df_resp = siteRespAll ### Need to be not just paired data
                     , colSampID = colBioSample
                     , dir_results = dir_results
                     , dir_sub = "TimeSequence")
          message(paste0("getTimeSeq for ", bioComm, " is complete."))
          # flush.console()
          
          # Get Response-based co-occurrence
          if (TargetSiteID %in% unique(data_bioCoOccur$StationID_Master)) {
            message("Starting Co-occurrence")
            # flush.console()
            getCoOccur(df_data = data_bioCoOccur
                       , TargetSiteID = TargetSiteID
                       , col_ID = "StationID_Master"
                       , colGroup = "clust"
                       , colBio = colBio
                       , colStressors = c(stressorsWPairedResponses)
                       , BioNarBrk = BioNarBrk
                       , BioNarLab = BioNarLab
                       , BioDegBrk = BioDegBrk
                       , BioDegLab = c("Yes", "No")
                       , biocomm = bioComm
                       , dir_plots = dir_results
                       , dir_sub = "CoOccurrence"
                       , col_StressInvScore = col_StressInvScore)
          } else {
            # data gap
          } ### End getCoOccur
          message("getCoOccur for ", bioComm, " is complete.")
          # flush.console()
          
          # Refine all.b.str, cl.b.str, and site.b.str for just identified stressors
          core.cols <- c("StationID_Master", "StressSampDate", "RespSampDate"
                         , "StressSampID", "RespSampID")
          
          all.b.str <- listPairedStressResp$allBioStress %>%
            select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
            select(StressSampID, RespSampID, StationID_Master
                   , eval(stressorsWPairedResponses))
          cl.b.str <- listPairedStressResp$compBioStress %>%
            select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
            select(StressSampID, RespSampID, StationID_Master
                   , eval(stressorsWPairedResponses))
          site.b.str <- listPairedStressResp$siteBioStress %>%
            select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
            select(StressSampID, RespSampID, StationID_Master
                   , eval(stressorsWPairedResponses))
          
          all.b.rsp <- listPairedStressResp$allBioResp %>%
            select(RespSampID, StressSampID, StationID_Master, RespSampDate
                   , Quality, eval(BMImetrics))
          cl.b.rsp <- listPairedStressResp$compBioResp %>%
            select(RespSampID, StressSampID, StationID_Master, RespSampDate
                   , Quality, eval(BMImetrics))
          site.b.rsp <- listPairedStressResp$siteBioResp %>%
            select(RespSampID, StressSampID, StationID_Master, RespSampDate
                   , Quality, eval(BMImetrics))
          
          siteStressInfo <- listPairedStressResp$siteStressInfo
          
          list_MatchBioData <- list("all.b.str" = all.b.str
                                    , "cl.b.str" = cl.b.str
                                    , "site.b.str" = site.b.str
                                    , "all.b.rsp" = all.b.rsp
                                    , "cl.b.rsp" = cl.b.rsp
                                    , "site.b.rsp" = site.b.rsp)
          
          # Get Stressor Responses
          getBioStressorResponses(TargetSiteID
                                  , stressors = stressorsWPairedResponses
                                  , stressorInfo = siteStressInfo
                                  , BioResp = bioMetricNames
                                  , list.MatchBioData = list_MatchBioData
                                  , LogTransf = stressors_logtransf
                                  , ref.sites = allBioRefStressSamps
                                  , siteQual2Plot = siteQual2Plot
                                  , biocomm = bioComm
                                  , dir_results = dir_results
                                  , dir_sub = "StressorResponse")
          message(paste0("getBioStressorResponses for ", bioComm, " is complete."))
          # flush.console()
          
          # Get Stressor-specific regressions
          df_SSTVtotabund <- data_bmiTaxaRaw[,c("BMISampID", "FinalID"
                                                ,"RelAbundInds")]
          if (SSTVparms %in% stressors) {
            targ_bio_bad <- list_MatchBioData$site.b.rsp[list_MatchBioData$site.b.rsp[
              , "Quality"]=="Degraded", bioIndex]
            targ_bio_good <- list_MatchBioData$site.b.rsp[list_Mat]
            if (length(targ_bio_bad)==0) {
              message("There are no 'worse' bio sites for comparison for this site.")
              # flush.console()
            } else {
              getVerifiedPredictions(TargetSiteID
                                     , stressors = stressorsWPairedResponses
                                     , stressorInfo <- siteStressInfo
                                     , data.bio.taxa.raw = bioTaxaData
                                     # , data_stressInfo = siteStressInfo
                                     , data.SSTV.totabund = df_SSTVtotabund
                                     , data.MT.bio = bioMasterTaxa
                                     , matchedData = list_MatchBioData
                                     # , ref.sites = allRefSites
                                     # , siteQual2Plot = siteQual2Plot
                                     , BioIndex_Val = bmiIndex
                                     , BioIndex_Nar = "Quality"
                                     , BioIndex_Nar_Deg = "Degraded"
                                     , dir_results=dir_results
                                     , dir_sub="VerifiedPredictions"
                                     , biocomm="bmi")
            }
          } else {
            message("No possible stressors have stressor-specific tolerance values.")
            # flush.console()
          } ### End getVP evaluation
          
          message(paste0("getVerifiedPredictions for ", bioComm, " is complete."))
          # flush.console()
          
          # # Not enabled 
          # # getSSDs
          # # getSSDplot(Data, ResponseType, Taxa, Exposure)
          # # myDF <- data_SSD_generator
          # # myRT   <- "ResponseType"
          # # myTaxa <- "Taxa"
          # # myExp  <- "Exposure"
          # # Run function
          # # p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)
          
          getWoE(TargetSiteID
                 , df_rank = list.stressors$site.stressor.pctrank
                 , df_chemInfo = listStressBMIAllData$siteStressInfo
                 , df_coOccur = data_bmiCoOccur
                 , biocomm = "bmi"
                 , index = bmiIndex
                 , dir_results = file.path(wd, "Results")
                 , CO_sub = "CoOccurrence"
                 , SR_sub = "StressorResponse"
                 , VP_sub = "VerifiedPredictons"
                 , SSD_sub = "SSD")
          message(paste0("getWoE for ", bioComm, " is complete."))
          # flush.console()
          
          # Get final report (Executive Summary style)
          getReport(TargetSiteID, dir_results=file.path(wd, "Results")
                    , report_type="summary", report_format=report_format
                    , dir_rmd=file.path(system.file(package = "CASTfxn"), "rmd"))
          message(paste0("getReport for ", bioComm, " is complete."))
          # flush.console()
          
          # rm(list.SiteSummary, list.data, list.stressors, list.ChemBMIData
          #    , chem.info, stressors, stressors_logtransf, data.SSTV.totabund)
          # 
          endsite.time <- Sys.time()
          elapsedsite.time <- endsite.time - startsite.time
          
          if (site == 1) {
            df_temp <- as.data.frame(c("TargetSiteID" = TargetSiteID
                                       , "NumStressors" = length(stressors)
                                       , "Biocomm" = bioComm
                                       , "ElapsedTime" = elapsedsite.time))
            df_runstats <- df_temp
          } else {
            df_temp <- as.data.frame(c("TargetSiteID" = TargetSiteID
                                       , "NumStressors" = length(stressors)
                                       , "Biocomm" = bioComm
                                       , "ElapsedTime" = elapsedsite.time))
            df_runstats <- rbind(df_runstats, df_temp)
          } ### End gather run stats
          
        } ### End biocomm loop
        
        
        
        
        # 
        # 
        # }
      } ### End TargetSite loop
      
      #~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Determine and print elapsed time
      end.time <- Sys.time()
      elapsed.time <- end.time - start.time
      message(paste(site, "sites completed in", elapsed.time))
      # flush.console()
      rm(site)
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Test 20191024, 
      
     
      
      msgDetail_A <- "Base Data"
      msgDetail_B <- "Load input data"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      
      #
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getSiteInfo ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Base Data"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Example 1, BMI
      # #TargetSiteID <- "SRCKN001.61"
      # TargetSiteID <- input$Station
      # dir_results  <- file.path(".", "Results")
      # #biocomm      <- "bmi"
      # biocomm      <- input$BioComm
      # #
      # # datasets getSiteInfo
      # # data, example included with package
      # data.Stations.Info <- data_Sites       # need for getSiteInfo and getChemDataSubsets
      # data.SampSummary   <- data_SampSummary
      # data.303d.ComID    <- data_303d
      # data.bmi.metrics   <- data_BMIMetrics
      # data.algae.metrics <- data_AlgMetrics
      # data.mod           <- data_ReachMod
      # #
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "SiteInfo"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
      # elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
      #                                        , "ElevCategory"])
      # if(elev_cat=="HI"){
      #   data.cluster <- data_Cluster_Hi
      # } else if(elev_cat=="LO") {
      #   data.cluster <- data_Cluster_Lo
      # }
      # 
      # # Map data
      # # San Diego
      # #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
      # #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
      # # AZ
      # map_flowline  <- data_GIS_Flow_HI
      # map_flowline2 <- data_GIS_Flow_LO
      # if(elev_cat=="HI"){
      #   map_flowline <- data_GIS_Flow_HI
      # } else if(elev_cat=="LO") {
      #   map_flowline <- data_GIS_Flow_LO
      # }
      # map_outline   <- data_GIS_AZ_Outline
      # # Project site data to USGS Albers Equal Area
      # usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      # # projection for outline
      # my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      # map_proj <- my.aea
      # #
      # dir_sub <- "SiteInfo"
      # #
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "SiteInfo"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Run getSiteInfo
      # list.SiteSummary <- getSiteInfo(TargetSiteID
      #                                 , dir_results
      #                                 , data.Stations.Info
      #                                 , data.SampSummary
      #                                 , data.303d.ComID
      #                                 , data.bmi.metrics
      #                                 , data.algae.metrics
      #                                 , data.cluster
      #                                 , data.mod
      #                                 , map_proj
      #                                 , map_outline
      #                                 , map_flowline
      #                                 , dir_sub=dir_sub)
      # 
      # # getChemDataSubsets
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "ChemDataSubsets"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Data getChemDataSubsets
      # # data import, example
      # # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
      # # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
      # site.COMID     <- list.SiteSummary$COMID
      # site.Clusters  <- list.SiteSummary$ClustIDs
      # # data, example included with package
      # data.chem.raw  <- data_Chem
      # data.chem.info <- data_ChemInfo
      # #
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "ChemDataSubsets"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Run getChemDataSubsets
      # list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
      #                                 , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
      #                                 , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getStressorList ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Stressor List"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Data getStressorList
      # chem.info     <- list.data$chem.info
      # cluster.chem  <- list.data$cluster.chem
      # cluster.samps <- list.data$cluster.samps
      # ref.sites     <- list.data$ref.sites
      # site.chem     <- list.data$site.chem
      # dir_sub       <- "CandidateCauses"
      # #
      # # set cutoff for possible stressor identification
      # probsLow  <- 0.10
      # probsHigh <- 0.90
      # #
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Stressor List"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Run getStressorList
      # list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
      #                                   , cluster.samps, ref.sites, site.chem
      #                                   , probsHigh, probsLow, biocomm, dir_results
      #                                   , dir_sub)
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getBioMatches ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Bio Matches"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Data getBioMatches, BMI
      # ## remove "none"
      # stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
      # stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
      # LogTransf <- stressors_logtransf
      # #
      # if(biocomm=="bmi"){
      #   data.bio.metrics <- data_BMIMetrics
      # } else if(biocomm=="algae"){
      #   data.bio.metrics <- data_AlgMetrics
      # }
      # 
      # #
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Bio Matches"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Run getBioMatches
      # list.MatchBioData <- getBioMatches(stressors, list.data, list.SiteSummary, data.SampSummary
      #                                    , data.chem.raw, data.bio.metrics, biocomm)
      # 
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getBioStressorResponses ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Bio Stressor Responses"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Data getBioStressorResponses, BMI
      # if(biocomm=="bmi"){
      #   BioResp <- c("IBI", "TotalTaxSPL_Sc", "DipTaxSPL_Sc"
      #                , "IntolTaxSPL_Sc", "HBISPL_Sc", "PlecoPct_Sc", "ScrapPctSPL_Sc"
      #                , "TrichTax_Sc", "EphemTax_Sc", "EphemPct_Sc", "Dom01PctSPL_Sc")
      # } else if(biocomm=="algae"){
      #   BioResp <- colnames(data.bio.metrics[6:52])
      # }
      # 
      # dir_sub <- "StressorResponse"
      # #
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Bio Stressor Responses"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Run getBioStressorResponses, BMI
      # getBioStressorResponses(TargetSiteID, stressors, BioResp, list.MatchBioData
      #                         , LogTransf, ref.sites, biocomm, dir_results, dir_sub)
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # Functions above are needed for the functions below.
      # # (getSiteInfo, getChemDataSet, getStressorList, getBioMatches)
      # # The functions below are all end points; 
      # # i.e., not used as inputs for other functions.
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getClusterInfo ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Cluster"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Data getClusterInfo
      # site.COMID <- list.SiteSummary$COMID
      # site.Clusters <- list.SiteSummary$ClustIDs
      # #
      # ref.reaches   <- list.data$ref.reaches
      # refSiteCOMIDs <- list.data$ref.reaches
      # dir_sub <- "ClusterInfo"
      # #
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Cluster"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # # #
      # # Run getClusterInfo
      # getClusterInfo(TargetSiteID, site.COMID, site.Clusters, ref.reaches
      #                   , data.cluster, dir_results, dir_sub) 
      # 
      # 
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getCoOccur ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "CoOccurrence"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # 
      # # Cluster Data based on elevation category
      # boo_Lo <- TargetSiteID %in% CASTfxn::data_CoOccur_AZ_Lo$StationID_Master
      # if(boo_Lo==TRUE){
      #   df.data <- CASTfxn::data_CoOccur_AZ_Lo
      # } else {
      #   df.data <- CASTfxn::data_CoOccur_AZ_Hi
      # }
      # #
      # col.Group     <- "Group"
      # col.Bio       <- "IBI"
      # col.Stressors <- c("Calcium_uf_mg_L", "Copper_uf_ug_L", "DO_f_mg_L", "SpecCond_umhos_cm")
      # col.ID        <- "StationID_Master"
      # #
      # Bio.Nar.Brk <- c(0, 45, 52, 100)
      # Bio.Nar.Lab <- c("Most Disturbed", "Intermediate", "Least Disturbed")
      # Bio.Deg.Brk <- c(0, 45, 100)
      # Bio.Deg.Lab <- c("Yes", "No")
      # biocomm <- "bmi"
      # #biocomm <- input$BioComm
      # dir.plots <- file.path(".", "Results")
      # dir_sub <- "CoOccurrence"
      # col.Stressors.InvSc <- c("DO_f_.", "DO_f_mg_L", "DO_f_unk", "DOSat_f_.", "DOSat_f_unk", "pH_SU")
      # #
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "CoOccurrence"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Run, getCoOccur
      # getCoOccur(df.data, TargetSiteID, col.ID, col.Group, col.Bio, col.Stressors
      #            , Bio.Nar.Brk, Bio.Nar.Lab, Bio.Deg.Brk, Bio.Deg.Lab
      #            , biocomm, dir.plots, dir_sub, col.Stressors.InvSc
      # )
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getVerifiedPredictions ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Verified Predictions"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # # 
      # # data, example included with package
      # data.bio.taxa.raw  <- data_BMIcounts
      # data.SSTV.totabund <- data_BMIRelAbund
      # BioIndex_Val       <- "IBI"
      # BioIndex_Nar       <- "NarRat"
      # BioIndex_Nar_Deg   <- "Violates"
      # dir_sub            <- "VerifiedPredictions"
      # biocomm <- "bmi"
      # 
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Verified Predictions"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Run, getVerifiedPredictions
      # getVerifiedPredictions(TargetSiteID
      #                        , data.SampSummary
      #                        , data.bio.taxa.raw
      #                        , data.chem.info
      #                        , data.SSTV.totabund
      #                        , data.MT.bio
      #                        , list.MatchBioData
      #                        , ref.sites
      #                        , BioIndex_Val
      #                        , BioIndex_Nar
      #                        , BioIndex_Nar_Deg
      #                        , dir_results
      #                        , dir_sub)
      # 
      # 
      # # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getWoE ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Weight of Evidence"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # # 
      # 
      # # 
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Weight of Evidence"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # getWoE(TargetSiteID
      #        , df.rank = list.stressors$site.stressor.pctrank
      #        , df.coOccur = data.bmi.coOccur
      #        , biocomm = "bmi"
      #        , index = "IBI"
      #        , dir_results = file.path(".", "Results")
      #        , CO_sub = "CoOccurrence"
      #        , SR_sub = "StressorResponse"
      #        , VP_sub = "VerifiedPredictons"
      #        , SSD_sub = "SSD")
      # 
      # 
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # getReport ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Report"
      # msgDetail_B <- "Load input data"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # # 
      # dir_results <- file.path(".", "Results")
      # report_type <- "summary"
      # report_format <- "html"
      # 
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Report"
      # msgDetail_B <- "Run"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # #
      # # Run, getReport
      # getReport(TargetSiteID, dir_results, report_type, report_format)
      # # 
      # 
      # #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # # Create Zip File ####
      # # Increment the progress bar, and update the detail text.
      # msgDetail_A <- "Zip"
      # msgDetail_B <- "Create file"
      # incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      # Sys.sleep(mySleepTime)
      # 
      # # Create zip file
      # fn_zip_contents <- list.files(file.path(".", "Results", TargetSiteID), full.names = TRUE)
      # fn_zip <- paste0(input$Station, "_", input$BioComm, ".zip")
      # zip(file.path(".", "Results", fn_zip), fn_zip_contents)
      # 
      
      # CopyResults ####
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Results"
      msgDetail_B <- "Prepare for display"
      incProgress(1/n_prog, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime * 10)
      # Copy from Results to www/Results
      CopyResults(TargetSiteID)

      
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
  
  output$pdf_Candidate <- renderUI({
    TargetSiteID <- input$Station
    txt_dir  <- "CandidateCauses"
    txt_file <- "boxes"
    # working directory changes to 'www' for this operation.
    src_pdf <- file.path(".", "Results", TargetSiteID, txt_dir
                         , paste0(TargetSiteID, ".", txt_file, ".ALL.pdf"))
    tags$iframe(style="height:600px; width:100%", src=src_pdf)
  })
  
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
  
  
  output$pdf_CoOccur <- renderUI({
    TargetSiteID <- input$Station
    txt_dir  <- "CoOccurrence"
    txt_file <- "CoOccurrence"
    # working directory changes to 'www' for this operation.
    src_pdf <- file.path(".", "Results", TargetSiteID, txt_dir
                         , paste0(TargetSiteID, ".", txt_file, ".ALL.pdf"))
    tags$iframe(style="height:600px; width:100%", src=src_pdf)
  })
  

  # output$pdf_CoOccur <- renderUI({
  #   TargetSiteID <- input$Station
  #   txt_dir  <- "CoOccurrence"
  #   txt_file <- "CoOccurrence"
  #   # working directory changes to 'www' for this operation.
  #   src_pdf <- file.path(".", "Results", TargetSiteID, txt_dir
  #                        , paste0(TargetSiteID, ".", txt_file, ".ALL.pdf"))
  #   # src_pdf <- paste("http://localhost/Results"
  #   #                  , paste0(TargetSiteID, ".", txt_file, ".ALL.pdf"), sep="/")
  #   tags$iframe(style="height:600px; width:100%", src=src_pdf)
  # })
  # 
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
  
  
  output$pdf_SR <- renderUI({
    TargetSiteID <- input$Station
    txt_dir  <- "StressorResponse"
    txt_file <- "SR.BMI"
    # working directory changes to 'www' for this operation.
    src_pdf <- file.path(".", "Results", TargetSiteID, txt_dir
                         , paste0(TargetSiteID, ".", txt_file, ".ALL.pdf"))
    tags$iframe(style="height:600px; width:100%", src=src_pdf)
  })
  
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
  
  output$pdf_VP <- renderUI({
    TargetSiteID <- input$Station
    txt_dir  <- "VerifiedPredictions"
    txt_file <- "SR.SSTV"
    # working directory changes to 'www' for this operation.
    src_pdf <- file.path(".", "Results", TargetSiteID, txt_dir
                         , paste0(TargetSiteID, ".", txt_file, ".ALL.pdf"))
    tags$iframe(style="height:600px; width:100%", src=src_pdf)
  })
  
  # Time Sequence ####
  
  output$pdf_TS_BMI <- renderUI({
    TargetSiteID <- input$Station
    txt_dir  <- "TimeSequence"
    txt_file <- "TS"
    # working directory changes to 'www' for this operation.
    src_pdf <- file.path(".", "Results", TargetSiteID, txt_dir, "BMI"
                         , paste0(TargetSiteID, ".", txt_file, ".ALL.pdf"))
    tags$iframe(style="height:600px; width:100%", src=src_pdf)
  })
  
  output$pdf_TS_Alg <- renderUI({
    TargetSiteID <- input$Station
    txt_dir  <- "TimeSequence"
    txt_file <- "TS"
    # working directory changes to 'www' for this operation.
    src_pdf <- file.path(".", "Results", TargetSiteID, txt_dir, "Algae"
                         , paste0(TargetSiteID, ".", txt_file, ".ALL.pdf"))
    tags$iframe(style="height:600px; width:100%", src=src_pdf)
  })
  
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
