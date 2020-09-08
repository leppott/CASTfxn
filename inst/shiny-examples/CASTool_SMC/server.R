# Server.R, CAST - SMC
#

# Packages
#library(shiny)
options(shiny.maxRequestSize=100*1024^2) # increase max file upload to 100 MB

# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {

  # Stop Shiny App when close browser
  session$onSessionEnded(stopApp)
  
  # Map, StationID ####
  
  # palette
  pal.tidal <- colorBin(palette=c("red", "blue"), domain=lines.flowline.proj$LENGTHKM)
  pal.smc   <- colorFactor(palette = "Set3", domain=poly.smc.proj$CUNAME)
  
  # Map
  
  # map_x <- leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17)
  # 
  # 
  # output$map_test <- leaflet::renderLeaflet({
  #   map_x
  #  # leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17)
  # })

  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
  
  
  # Station, test ####
  output$map_station2 <- leaflet::renderLeaflet({
    #

    # map_x


    # leaflet() %>%
    #   # Groups, Base
    #   addTiles(group="OSM (default)") %>%  #default tile too cluttered
    #   addProviderTiles("CartoDB.Positron", group="Positron") %>%
    #   addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") #%>%
    #   # # Groups, Overlay
      # addPolygons(data=poly.smc.proj
      #             , color="green"
      #             , fill=FALSE
      #             , group="Watersheds")
      # ) %>%
      # addPolylines(data=lines.flowline.proj
      #              , color= "blue"
      #              , highlightOptions=highlightOptions(bringToFront=TRUE, color="purple" )
      #              , popup=~paste0(GNIS_NAME, as.character("<br> COMID = "), COMID)
      #              , group="Streams"
      # ) %>%
      # addCircles(data=df.sites.map
      #            , lng=~FinalLongitude
      #            , lat=~FinalLatitude
      #            , popup=~paste0(StationID_Master, as.character("<br>"), WaterbodyName)
      #            , color="orange"
      #            , group="Sites"
      #            , highlightOptions = highlightOptions(bringToFront = TRUE, color="red")
      #            , radius=20
      # ) %>%
      # addCircles(data=df.sites.map[df.sites.map[, "StationID_Master"]=="SMC04134", ]
      #            , lng=~FinalLongitude
      #            , lat=~FinalLatitude
      #            , popup=~paste0(StationID_Master, as.character("<br>"), WaterbodyName)
      #            , color="black"
      #            , group="Sites_selected"
      #            , layerId = "layer_site_selected"
      #            , radius=30
      # ) %>%
      # # Bounding (to SMC region)
      # fitBounds(lng1 = poly.smc.proj@bbox[1]
      #           , lat1 = poly.smc.proj@bbox[4]
      #           , lng2 = poly.smc.proj@bbox[3]
      #           , lat2 = poly.smc.proj@bbox[2]
      # ) %>%
      # # Layers
      # addLayersControl(
      #   baseGroups = c("OSM (default)", "Positron", "Toner Lite")
      #   , overlayGroups=c("Watersheds", "Streams", "Sites")
      # ) %>%
      # # Legend
      # addLegend("bottomleft", colors=c("green", "blue", "purple", "orange", "red", "black")
      #           , labels=c("Watersheds", "Streams", "Streams (mouse-over)", "Sites", "Sites (mouse-over)", "Sites (selected)")
      #           , values=NA
      # ) %>%
      # addMiniMap(toggleDisplay = TRUE)
  })#output$map.station2 ~ END

  # Station ####
  output$map_station <- renderLeaflet({
    #
    leaflet() %>%
      # Groups, Base
      addTiles(group="OSM (default)") %>%  #default tile too cluttered
      addProviderTiles("CartoDB.Positron", group="Positron") %>%
      addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>%
      # Groups, Overlay
      addPolygons(data=poly.smc.proj
                  , color="green"
                  , fill=FALSE
                  , group="Watersheds"
      ) %>%
      addPolylines(data=lines.flowline.proj
                   , color= "blue"
                   , highlightOptions=highlightOptions(bringToFront=TRUE, color="purple" )
                   , popup=~paste0(GNIS_NAME, as.character("<br> COMID = "), COMID)
                   , group="Streams"
      ) %>%
      addCircles(data=df.sites.map
                 , lng=~FinalLongitude
                 , lat=~FinalLatitude
                 , popup=~paste0(StationID_Master, as.character("<br>"), WaterbodyName)
                 , color="orange"
                 , group="Sites"
                 , highlightOptions = highlightOptions(bringToFront = TRUE, color="red")
                 , radius=20
      ) %>%
      addCircles(data=df.sites.map[df.sites.map[, "StationID_Master"]=="SMC04134", ]
                 , lng=~FinalLongitude
                 , lat=~FinalLatitude
                 , popup=~paste0(StationID_Master, as.character("<br>"), WaterbodyName)
                 , color="black"
                 , group="Sites_selected"
                 , layerId = "layer_site_selected"
                 , radius=30
      ) %>%
      # Bounding (to SMC region)
      fitBounds(lng1 = poly.smc.proj@bbox[1]
                , lat1 = poly.smc.proj@bbox[4]
                , lng2 = poly.smc.proj@bbox[3]
                , lat2 = poly.smc.proj@bbox[2]
      ) %>%
      # Layers
      addLayersControl(
        baseGroups = c("OSM (default)", "Positron", "Toner Lite")
        , overlayGroups=c("Watersheds", "Streams", "Sites")
      ) %>%
      # Legend
      addLegend("bottomleft", colors=c("green", "blue", "purple", "orange", "red", "black")
                , labels=c("Watersheds", "Streams", "Streams (mouse-over)", "Sites", "Sites (mouse-over)", "Sites (selected)")
                , values=NA
      ) %>%
      addMiniMap(toggleDisplay = TRUE)
  })#output$map.smc.END

  # # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   #lines.flowline.proj[lines.flowline.proj$COMID == input$comid.select, ]
  #   #if(input$comid.select!="Erik"){
  #     lines.flowline.proj[lines.flowline.proj$COMID == input$comid.select, ]
  #     #lines.flowline.proj[lines.flowline.proj$COMID == "20331944", ]
  #   #} else {
  #   #  lines.flowline.proj
  #   #}
  # })
  # # # Reactive values for dimensions of subsetted data
  # fD.centroid <- reactive({
  #   c(filteredData$CENTROID_X, filteredData$CENTROID_Y)
  #   #c(-117.1, 32.8)
  # })
  #

  # myX <- -117.1
  #myX <- filteredData$CENTROID_X

  # x <- fD.bbox[1]

  # fD.bbox <- lines.flowline.proj@bbox
  # #fD.bbox <- filteredData@bbox
  #
  # fD.cent.lat <- mean(fD.bbox[2], fD.bbox[4])
  # fD.cent.lng <- mean(fD.bbox[1], fD.bbox[3])

  #
  #
  # # Modify Polylines
  #observe({
  observeEvent(input$siteid.select,{
    #
    df_filtered <- df.sites.map[df.sites.map$StationID_Master == input$siteid.select, ]

    #
    # get centroid (use mean just in case have duplicates)
    view.cent <- c(mean(df_filtered$FinalLongitude), mean(df_filtered$FinalLatitude))
    #
    # modify map
    leafletProxy("map_station") %>%
      #clearShapes() %>%  # removes all layers
      removeShape("layer_site_selected") %>%
      #addPolylines(data=filteredData()
      addCircles(data=df_filtered
                 , lng=~FinalLongitude
                 , lat=~FinalLatitude
                 , popup=~paste0(StationID_Master, as.character("<br>"), WaterbodyName)
                 , color = "black"
                 , group = "Sites_selected"
                 , layerId = "layer_site_selected"
                 , radius=30) %>%
      # addPolylines(data=df_filtered
      #              , color="orange"
      #              , popup=~COMID
      #              #, highlightOptions=highlightOptions(bringToFront=TRUE
      #              #                                    , color="red" )
      #              , group="Streams_Select"
      #              , layerId = "layer_Stream_Select") %>%
      #setView(fD.centroid[1], fD.centroid[2], zoom=10)
      #setView(view.cent[1], view.cent[2], zoom=10)
      #  #fitBounds(df_filtered@bbox[1], df_filtered@bbox[2], df_filtered@bbox[3], df_filtered@bbox[4])
      setView(view.cent[1], view.cent[2], zoom = 16) # 1= whole earth


    #setView(filteredData$CENTROID_X, filteredData$CENTROID_Y, zoom=10)

    #setView(filteredData@bbox[1], filteredData@bbox[4], zoom=10)
    #setView(getCenter(filteredData())[1], getCenter(filteredData())[2], zoom=10)
    # centroid.lat <- mean(lines.flowline.proj@bbox[2], lines.flowline.proj@bbox[4])
    # centroid.lng <- mean(lines.flowline.proj@bbox[1], lines.flowline.proj@bbox[3])
    # centroid.lat <- 32.75
    # centroid.lng <- 117.1
    # setView(centroid.lng, centroid.lat, zoom=10)
    #setView(lng=fD.bounds[1], lat=fD.bounds[2], zoom=10)

    #     setView(lng=fD.cent.lng, lat=fD.cent.lat, zoom=10)
    #setView(-120, 34, zoom=10)

  }) ## observeEvent(input$siteid.select ~ END

  # Output ####

  #url_map <- a("Shiny Site Selection Map", href="https://leppott.shinyapps.io/CAST_Map_SiteID")
  output$URL_Shiny_Map <- renderUI({tagList("URL link", url_map)})

  output$StationID <- renderText({
    paste0("Selected Station = ", input$Station)
  })##StationID~END

  output$fn_Map <- renderText({
    file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, "_map_leaflet.html"))
  })##fn_Map~END

  output$fe_Map <- renderText({
    paste0("Map file exists = ", file.exists(file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, "_map_leaflet.html"))))
  })##fe_Map~END

  # Help ####
  output$help_html <- renderUI({
    fn_html <- file.path(".", "www", "ShinyHelp.html")
    fe_html <- file.exists(fn_html)
    if(fe_html==TRUE){
      return(includeHTML(fn_html))
    } else {
      return(NULL)
    }##IF~fe_html~END
  })##help_html~END

#  getHTML <- function(fn_html){
#    #fn_disclaimer_html <- file.path(".", "data", "Disclaimer_Key.html")
#    fe_html <- file.exists(fn_html)
#    if(fe_html==TRUE){
#      return(includeHTML(fn_html))
#    } else {
#      return(NULL)
#    }
#  }##getHTML~END
  
  output$LegKey_html <- renderUI({
    fn_html <- file.path(".", "www", "Legend_Key.html")
    fe_html <- file.exists(fn_html)
   if(fe_html==TRUE){
     return(includeHTML(fn_html))
   } else {
     return(NULL)
   }##IF ~ fe_html ~ END
  })##LegKey_html~END

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



  # Test if zip file exists
  output$boo_zip <- function() {
    fn_zip_boo <- paste0(input$Station, ".zip")
    return(file.exists(file.path(".", "Results", fn_zip_boo)) == TRUE)
  }##boo_zip~END

  # observeEvent({
  #   c(input$Station, input$b_RunAll)
  # } , {
  #  fn_zip_toggle <- paste0(input$Station, ".zip")
  #  toggleState(id="b_downloadData", condition = file.exists(file.path(".", "Results", fn_zip_toggle)) == TRUE)
  # })##~toggleState~END

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

  # b_dir_user_*
  # Shiny directory buttons
  # volumes <- c(wd = ".", "R Installation" = R.home(), shinyFiles::getVolumes()())
  # shinyFiles::shinyDirChoose(input, 'dir_user_input', roots=volumes, session = session)
  # shinyFiles::shinyDirChoose(input, 'dir_user_output', roots=volumes, session = session)
  #
  # observe({
  #   cat("\ninput$directory value:\n\n")
  #   print(input$directory)
  # })



  ## print to browser


  # output$directorypath <- renderPrint({
  #   parseDirPath(volumes, input$directory)
  # })


  #
 # shinyFiles::shinyDirChoose(input, "dir_user_input", roots = volumes, session = session, restrictions = system.file(package = "base"))
  #
  # output$dir_user_input_path <- renderPrint({
  #   shinyFiles::parseDirPath(volumes, input$dir_user_input)
  # })


  # shinyDirChoose(input, "dir_user_input")
  # dir_user_input <- reactive(input$dir_user_input)


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

  Run_ALL2 <- function(){
    # Updates from Ann's "skeleton" code, 2020-08-25
    #
    shiny::withProgress({
      #
      start.time <- Sys.time() # Added 2020-08-17 to match with line 2744 (after getSummaryAllSites)
      # Number of increments
      prog_n <- 26 + 7 + 1
      prog_inc <- 1/prog_n
      prog_cnt <- 0
      mySleepTime <- 0.5
      #
      # Remove Zip ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Remove Zip"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
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
      # source(file.path(gitpath, "getCoOccurDataset.R"))
      # source(file.path(gitpath, "getTimeSeq.R"))
      # source(file.path(gitpath, "getDataSets.R"))
      # source(file.path(gitpath, "getComparators.R"))
      # source(file.path(gitpath, "getSiteInfo.R"))
      # source(file.path(gitpath, "getClusterInfo.R"))
      # source(file.path(gitpath, "getStressorList.R"))
      # source(file.path(gitpath, "getCoOccur.R"))
      # source(file.path(gitpath, "getBioStressorResponses.R"))
      # source(file.path(gitpath, "getVerifiedPredictions.R"))
      # source(file.path(gitpath, "getOutliers.R"))
      # source(file.path(gitpath, "getWoE.R"))
      # source(file.path(gitpath, "getQualSites.R"))
      # source(file.path(gitpath, "getSummaryAllSites.R"))
      # source(file.path(gitpath, "getReport.R"))

      # source(file.path(gitpath, "getDataGaps.R"))
      # source(file.path(gitpath, "getSiteBackground.R"))

      # put in global
      #not_all_na <- function(x) {!all(is.na(x))}

      # Timer, Start
      startprep.time <- Sys.time()

      # Required user-designated options
      wd <- file.path(".")
      dir_data <- file.path(wd, "Data")
      dir_results <- file.path(wd, "Results")
      #
      removeOutliers <- TRUE
      useBC <- TRUE # Use Bray-Curtis biological dissimilarity distance matrix
      probsHigh=0.75
      probsLow=0.25
      DOlim=7
      pHlimLow=6.5
      pHlimHigh=9
      lagdays=10
      biocommlist <- c("bmi","algae")
      siteQual2Plot = "not degraded" # options:"reference","better than","not degraded"
      report_format="html"    # word, pdf are the other options

      # Specify Base Filenames # These are the files used to run the analyses
      # Data Files ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Load Data Files"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      fn.targets          <- file.path(dir_data,"SMCTestSites.xlsx")
      fn.Sites.Info       <- file.path(dir_data, "SMCSitesFinal.tab")
      fn.SampSummary      <- file.path(dir_data, "SMCSiteSummary.tab")
      fn.cheminfo         <- file.path(dir_data, "SMCMeasStressInfoFinal.tab")
      fn.chemdata         <- file.path(dir_data, "SMCMeasStressDataFinal.tab")
      fn.modelinfo        <- file.path(dir_data, "SMCModelStressInfoFinal.tab")
      fn.modeldata        <- file.path(dir_data, "SMCModelStressDataFinal.tab")
      fn.bmi.metrics      <- file.path(dir_data, "SMCBenthicMetricsFinal.tab")
      fn.bmi.cscicore     <- file.path(dir_data, "SMCBenthicCSCIcore.tab")
      fn.bmi.metrics.info <- file.path(dir_data, "SMCBenthicMetricsInfo.tab")
      fn.bmi.raw          <- file.path(dir_data, "SMCBenthicCountsFinal.tab")
      fn.MT.bmi           <- file.path(dir_data, "SMCBenthicMasterTaxa.tab")
      fn.alg.metrics      <- file.path(dir_data, "SMCAlgaeMetricsFinal.tab")
      fn.alg.metrics.info <- file.path(dir_data, "SMCAlgaeMetricsInfo.tab")
      fn.alg.raw          <- file.path(dir_data, "SMCAlgaeCountsFinal.tab")
      fn.MT.alg           <- file.path(dir_data, "SMCAlgaeMasterTaxa.tab")
      fn.bcdist           <- file.path(dir_data, "SMCBCDist.tab")
      fn.cluster          <- file.path(dir_data, "SMCClusterData.tab")
      fn.clusterinfo      <- file.path(dir_data, "SMCClusterInfo.tab")
      fn.bkgdata          <- file.path(dir_data, "SMCSiteBkgdData.tab")
      fn.bkginfo          <- file.path(dir_data, "SMCSiteBkgdInfo.tab")
      #
      # GIS
      # outline <- rgdal::readOGR(dsn = "Data/SMCBoundary", layer = "SMCBoundary_aea")
      # flowline <- rgdal::readOGR(dsn = "Data/SMCReaches", layer = "SMCReaches_aea")

      # Load GIS files
      message("Loading GIS files.")
      outline <- rgdal::readOGR(dsn = file.path(dir_data, "SMCBoundary"), layer = "SMCBoundary_aea", pointDropZ = TRUE)
      flowline <- suppressWarnings(rgdal::readOGR(dsn = file.path(dir_data, "SMCReaches"), layer = "SMCReaches_aea", pointDropZ = TRUE))
      # warning z-dimension discarded.  "pointDropZ = TRUE" does not remove the warning

      # Specify user-defined variables
      # Stressors
      meas.stress <- c("ChemSampleID", "PhabSampID", "FldChemSampID")
      chem.stress <- c("ChemSampleID", "FldChemSampID")
      hab.stress <- "PhabSampID"
      mod.stress <- "FlowSampID"

      # BMI responses
      bmi_thresholds <- c(-2, 0.62, 0.799, 0.919, 2)
      bmi_narrative <- c("very likely altered", "likely altered"
                         , "possibly altered", "likely intact")
      bmi_deg_thres <- c(-2, 0.799, 2)
      bmi_deg_text <- c("Yes", "No")
      bmiIndexGp <- c("CSCI", "OoverE", "MMI")
      bmiResp <- "BMISampID"
      bmiRespDate <- "BMISampDate"
      # bmiMetrics <- c(bmiIndex, "MMI", "OoverE", "Taxonomic_Richness"
      #                 , "Intolerant_Percent", "Shredder_Taxa", "Clinger_PercentTaxa"
      #                 , "Coleoptera_PercentTaxa", "EPT_PercentTaxa")


      # Algal responses
      alg_thresholds <- c(-2, 0.82, 2)
      alg_narrative <- c("Degraded", "Not Degraded")
      alg_deg_thres <- c(-2, 0.82, 2)
      alg_deg_text <- c("Yes", "No")
      algIndexGp <- c("MMIhybrid", "MMIdiatom", "MMIsba")
      algResp <- "AlgSampID"
      algRespDate <- "AlgSampDate"
      # algMetrics <- c("MMIdiatom"
      #                 , "propsppOxyReqDO_10_rawdiatom"
      #                 , "cntsppBCG3_rawdiatom"
      #                 , "propCyclotella_rawdiatom"
      #                 , "propSurirella_rawdiatom"
      #                 , "propsppIndicatorClass_TP_low_rawdiatom"
      #                 , "propsppOrgNNHHONF_rawdiatom"
      #                 , "MMIsba"
      #                 , "cntsppBCG3_rawsba"
      #                 , "propsppIndicatorClass_NonRef_rawsba"
      #                 , "propsppGreen_rawsba"
      #                 , "cntsppIndicatorClass_Cu_high_rawsba"
      #                 , "cntsppIndicatorClass_TP_high_rawsba"
      #                 , "cntsppIndicatorClass_DOC_high_rawsba"
      #                 , "propsppmosttol_rawsba"
      #                 , algIndex
      #                 , "cntsppBCG3_rawhybrid"
      #                 , "propCyclotella_rawhybrid"
      #                 , "propsppOxyReqDO_10_rawhybrid"
      #                 , "propSurirella_rawhybrid"
      #                 , "propsppIndicatorClass_DOC_high_rawhybrid"
      #                 , "propsppIndicatorClass_Cu_high_rawhybrid"
      #                 , "propsppOrgNNHHONF_rawhybrid"
      #                 , "propsppIndicatorClass_TN_low_rawhybrid")

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
      data_Sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      rm(fn.Sites.Info)

      # Get sample summary data
      data_SampSummary <- read.delim(fn.SampSummary, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      data_mods        <- data_ReachMod   # Check this
      data_303d        <- data_303d       # Check this
      rm(fn.SampSummary)

      # CAST, Chem & other measured data ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, Chem"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      ## Get metadata for all measured stressors
      data_chemInfo   <- read.delim(fn.cheminfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      data_chemInfo   <- mutate(data_chemInfo, Analyte = StdParamName)
      colMeasInvScore <- as.vector(data_chemInfo$StdParamName[data_chemInfo$DirIncStress=="Dec"])
      SSTVparms <- unique(data_chemInfo$StdParamName[data_chemInfo$SSTV==1])
      rm(fn.cheminfo)

      # Get metadata for modeled stressor data
      data_modelInfo   <- read.delim(fn.modelinfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      data_modelInfo   <- mutate(data_modelInfo, Analyte = StdParamName)
      colModelInvScore <- as.vector(data_modelInfo$StdParamName[data_modelInfo$DirIncStress=="Dec"])
      rm(fn.modelinfo)

      # Combine metadata for all stressor into one datafile
      chemMetaNames  <- colnames(data_chemInfo)
      modelMetaNames <- colnames(data_modelInfo)
      extraNames     <- chemMetaNames[!(chemMetaNames %in% modelMetaNames)]
      for (e in 1:length(extraNames)) {
        newCol <- extraNames[e]
        data_modelInfo[[newCol]] <- NA
      }## FOR ~ e ~ END
      data_modelInfo <- data_modelInfo[,chemMetaNames]
      data_stressInfo <- rbind(data_chemInfo, data_modelInfo)

      ## Get measured stressor values
      data_chemAll <- read.delim(fn.chemdata, header = TRUE, sep = "\t",
                                 na.strings = "NA", stringsAsFactors = FALSE)
      analytes     <- data_stressInfo$StdParamName[data_stressInfo$UseInStressorID == 1]
      data_chemRaw <- data_chemAll[data_chemAll$StdParamName %in% analytes,]
      data_chemRaw <- data_chemRaw %>%
        mutate(SampleDate = lubridate::mdy(SampDate)) %>%
        select(StationID_Master, ChemSampleID, SampDate, StdParamName
               , ResultValue, SampleDate) %>%
        group_by(StationID_Master, ChemSampleID, SampDate, StdParamName
                 , SampleDate) %>%
        summarize(MeanResultValue = mean(ResultValue), .groups = "drop_last") %>%
        rename(ResultValue = MeanResultValue)
      data_chemRaw <- unique(data_chemRaw)
      data_outliers <- getOutliers(df_data = data_chemRaw
                                   , df_meta = data_chemInfo)
      data_chemRaw <- merge(data_chemRaw, data_outliers
                            , by.x = c("ChemSampleID", "StdParamName", "ResultValue")
                            , by.y = c("ChemSampleID", "StdParamName", "ResultValue")
                            , all.x = TRUE)
      data_chemRaw <- data_chemRaw[,c("StationID_Master", "ChemSampleID", "SampDate"
                                      , "StdParamName", "ResultValue", "SampleDate"
                                      , "IQRmethod", "SDmethod", "Outlier")]
      rm(fn.chemdata, data_chemAll)
      measParams <- as.vector(unique(data_chemRaw$StdParamName))
      algParams <- as.vector(unique(data_chemRaw$StdParamName[grepl("^AFDM|^Chlor_a|^Pheophytin"
                                                                    ,data_chemRaw$StdParamName)]))

      # Get modeled stressor data (skeleton 186)
      data_modelAll <- read.delim(fn.modeldata, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      useParams     <- data_modelInfo$StdParamName[data_modelInfo$UseInStressorID == 1]
      data_modelRaw <- data_modelAll[data_modelAll$StdParamName %in% useParams,]
      data_modelRaw <- data_modelRaw %>%
        mutate(SampYear = lubridate::year(lubridate::mdy(SampDate))
               , SampleDate =  lubridate::mdy(SampDate)) %>%
        select(StationID_Master, ChemSampleID, SampDate, StdParamName
               , ResultValue, SampleDate)
      data_modoutliers <- getOutliers(df_data = data_modelRaw
                                      , df_meta = data_modelInfo)
      data_modelRaw <- merge(data_modelRaw, data_modoutliers
                             , by.x = c("ChemSampleID", "StdParamName", "ResultValue")
                             , by.y = c("ChemSampleID", "StdParamName", "ResultValue")
                             , all.x = TRUE)
      data_modelRaw <- data_modelRaw[,c("StationID_Master", "ChemSampleID", "SampDate"
                                        , "StdParamName", "ResultValue", "SampleDate"
                                        , "IQRmethod", "SDmethod", "Outlier")]
      rm(fn.modeldata, data_modelAll)
      rm(data_chemInfo, data_modelInfo)

      # Identify modeled parameters to keep or delete (per SCCWRP) (skeleton 207)
      modelParams <- as.vector(unique(data_modelRaw$StdParamName))
      bmiModelParamsKeep <- c("HighDur_Wet", "HighNum_Dry", "MaxMonthQ_Wet"
                              , "NoDisturb_Average", "Q99_Average", "QmaxIDR_All"
                              , "RBI_Dry")
      bmiModelParamsDEL  <- setdiff(modelParams, bmiModelParamsKeep)
      algModelParamsKeep <- c("HighDur_Dry", "HighNum_Dry", "MaxMonthQ_Dry"
                              , "NoDisturb_Dry", "Qmax_Dry", "QmaxIDR_All")
      algModelParamsDEL <- setdiff(modelParams, algModelParamsKeep)
      algParamsDEL      <- c(algModelParamsDEL, algParams)

      # Prepare df_allStress file (skeleton 218)
      data_modeltrim <- as.data.frame(data_modelRaw) %>%
        dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
                      , ResultValue, IQRmethod, SDmethod, Outlier) %>%
        dplyr::mutate(SampleDate = NA)
      data_meastrim <- as.data.frame(data_chemRaw) %>%
        dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
                      , ResultValue, IQRmethod, SDmethod, Outlier)
      data_Stress <- rbind(data_meastrim, data_modeltrim)
      # Skelton, added 2020-08-25
      fn.stress4RPP <- file.path(dir_data,"SMC_AllStressData.tab")
      fn.stressmeta4RPP <- file.path(dir_data,"SMC_AllStressInfo.tab")
      write.table(data_Stress, fn.stress4RPP, append = FALSE, col.names = TRUE
                  , row.names = FALSE, sep = "\t")
      write.table(data_stressInfo, fn.stressmeta4RPP, append = FALSE, col.names = TRUE
                  , row.names = FALSE, sep = "\t")

      # Combine measured and modeled parameters with inverse scoring
      col_StressInvScore <- c(colMeasInvScore, colModelInvScore)

      # CAST, BMI, taxonomic data ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, BMI, Taxonomic"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      data_BMIcounts <- read.table(fn.bmi.raw, header = TRUE, sep = "\t")

      data_MTbmi     <- read.table(fn.MT.bmi, header = TRUE, sep = "\t",
                               stringsAsFactors = FALSE)
      # data_bmiTaxaRaw <- mutate(data_bmiTaxaRaw, BMI.Metrics.SampID = BMISampID)
      rm(fn.bmi.raw, fn.MT.bmi)

      # CAST, BMI, metrics ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, BMI, Metrics"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
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
      data_bmiMetrics <- unique(data_bmiMetrics)
      rm(fn.bmi.metrics)

      data_cscicore <- read.delim(fn.bmi.cscicore, header = TRUE, sep = "\t"
                                  , na.strings = "NA", stringsAsFactors = FALSE)
      data_cscicore <- data_cscicore[,c("stationid", "county", "smcshed", "latitude"
                                        , "longitude", "stationcode", "sampleid"
                                        , "samplemonth", "sampleday", "sampleyear"
                                        , "collectionmethodcode", "fieldreplicate"
                                        , "count", "pcnt_ambiguous_individuals")]
      data_cscicore <- data_cscicore %>%
        mutate(date_text = paste(samplemonth,sampleday,sampleyear,sep="/")
               , BMISampID = paste(stationid, date_text, collectionmethodcode
                                   , fieldreplicate, sep = "_")
               , BMISampFlag = ifelse((count<250) & (pcnt_ambiguous_individuals>50)
                                      , "Insufficient individuals and large percent ambiguity"
                                      , ifelse(count<250, "Insufficient individuals"
                                               , ifelse(pcnt_ambiguous_individuals>50
                                                        , "Large percent ambiguity"
                                                        , NA)))) %>%
        rename(StationID_Master = stationid, BMISampCount = count
               , PctAmbigInd = pcnt_ambiguous_individuals) %>%
        select(StationID_Master, BMISampID, BMISampCount, PctAmbigInd, BMISampFlag)
      data_cscicore <- unique(data_cscicore)

      data_bmiMetrics <- merge(data_bmiMetrics, data_cscicore
                               , by.x = c("StationID_Master", "BMISampID")
                               , by.y = c("StationID_Master", "BMISampID")
                               , all.x = TRUE)

      data_tmpbmicount <- unique(data_BMIcounts[,c("BMISampID","SampleTotAbund")])
      data_bmiMetrics <- data_bmiMetrics %>%
        mutate(BMISampCount = ifelse(is.na(BMISampCount)
                                     , data_tmpbmicount$SampleTotAbund
                                     , BMISampCount)) %>%
        mutate(BMISampFlag = ifelse(is.na(BMISampFlag) & (BMISampCount < 250)
                                    , "Insufficient number of individuals", BMISampFlag))
      rm(data_tmpbmicount)

      data_bmiMetrics <- data_bmiMetrics %>%
        mutate(BMISampFlag = ifelse(is.na(PctAmbigInd) & is.na(BMISampFlag)
                                    , ifelse(BMISampCount >= 250
                                             , paste0("Unknown percent ambiguous individuals")
                                             , paste0("Unknown number of and percent "
                                                      , "ambiguous individuals"))
                                    , ifelse(is.na(PctAmbigInd)
                                             , paste0("Insufficient number of and unknown "
                                                      ,"percent ambiguous individuals")
                                             , BMISampFlag)))

      # CAST, BMI, metrics metadata ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, BMI, Metrics, Metadata"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # skeleton 316
      data_bmiMetricsInfo <- read.delim(fn.bmi.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      data_bmiMetricsInfo <- data_bmiMetricsInfo[,c("MetricName",	"MetricLabel", "IndexYN")]
      bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
      bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN=="Yes"])

      # Generate co-occurrence data set (same day samples; modeled data match any day)
      data_bmiCoOccur <- getCoOccurDataset(dataDir = dir_data
                                           , df_sites = data_Sites
                                           , df_model = data_modelRaw
                                           , df_meas = data_chemRaw
                                           , biocomm = "BMI"
                                           , df_resp = data_bmiMetrics
                                           , index = bmiIndex
                                           , lagdays = lagdays
                                           , removeOutliers = removeOutliers)
      # returns df_coOccur as data_bmiCoOccur
      bmiParamsKEEP   <- setdiff(colnames(data_bmiCoOccur), bmiModelParamsDEL)
      data_bmiCoOccur <- dplyr::select(data_bmiCoOccur, all_of(bmiParamsKEEP))
      # 2020-04-10, add "all_of" to excise tidyverse message.
      # write.table(data_bmiCoOccur, file.path(getwd(),"Results","bmiCoOccur.tab")
      #             ,append=FALSE,col.names = TRUE, row.names = FALSE, sep = "\t")


      # Skeleton 2020-08-25, swap order of Alg metrics and Alg metric metadata


      # CAST, Alg, metrics metadata ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, Alg, Metrics, Metadata"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      data_AlgMetricsInfo <- read.delim(fn.alg.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      algMetrics <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN==1])
      algMetricsDiscard <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN==0])
      algIndex   <- as.character(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$IndexYN=="Yes"])
      # Added algMetricsDiscard from skeleton 2020-08-25, line 343


      # CAST, Alg, metrics ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, Alg, Metrics"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # skeleton 339
      data_AlgMetrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
      data_AlgMetrics <- data_AlgMetrics %>%
        dplyr::mutate(AlgSampDate = lubridate::mdy(AlgSampDate)) %>%
        dplyr::mutate(AlgSampFlag = NA)
      data_AlgMetrics <- dplyr::select(data_AlgMetrics, -algMetricsDiscard)
      rm(fn.alg.metrics)
      # 2020-08-25, added -algMetricsDiscard line


      # CAST, Alg taxonomic data ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, Alg, Taxonomic"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # skeleton 356
      data_AlgCounts <- read.table(fn.alg.raw, header = TRUE, sep = "\t")
      data_AlgMasterTaxa <- read.table(fn.MT.alg, header = TRUE, sep = "\t",
                                       stringsAsFactors = FALSE)
      rm(fn.alg.raw, fn.MT.alg)
      #

      # # Generate co-occurrence data set (same day samples; modeled data match any day)
      data_algCoOccur <- getCoOccurDataset(dataDir = dir_data
                                           , df_sites = data_Sites
                                           , df_model = data_modelRaw
                                           , df_meas = data_chemRaw
                                           , biocomm = "Alg"
                                           , df_resp = data_AlgMetrics
                                           , index = algIndex
                                           , lagdays = lagdays
                                           , removeOutliers = removeOutliers)
      # returns df_coOccur as data_algCoOccur
      algParamsKEEP <- setdiff(colnames(data_algCoOccur), algParamsDEL)
      data_algCoOccur <- dplyr::select(data_algCoOccur, all_of(algParamsKEEP))
      # write.table(data_algCoOccur, file.path(getwd(),"Results","algCoOccur.tab")
      #             ,append=FALSE,col.names = TRUE, row.names = FALSE, sep = "\t")

      # Get cluster data
      data_cluster <- read.delim(fn.cluster, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      rm(fn.cluster)

      # Get cluster data metadata
      data_clusterInfo <- read.delim(fn.clusterinfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      rm(fn.clusterinfo)

      # Get background data (StreamCat)
      df_bkgdata <- read.table(fn.bkgdata, header = TRUE, sep = "\t"
                               , na.strings = c("","NA"))

      # Get background metadata
      df_bkginfo <- read.table(fn.bkginfo, header = TRUE, sep = "\t"
                               , na.strings = c("", "NA")
                               , stringsAsFactors = FALSE)

      if (useBC == TRUE) {
        # Get BC dissimilarity distance matrix to subset cluster sites to comparators
        data_BCdist <- read.delim(fn.bcdist, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # RUN CASTool
      # Site Selection ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Site Selection"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      #df_targets <- read_excel(fn.targets, col_names = TRUE, trim_ws = TRUE, skip = 0)
      # running single site so don't need df_targets and a loop.
      # skeleton 402
      endprep.time <- Sys.time()
      elapsedprep.time <- round(endprep.time - startprep.time, 2)
      msg <- paste("Prep completed in", elapsedprep.time)
      # print(msg)
      # flush.console()
      message(msg)


      ifelse(!dir.exists(file.path(dir_results))==TRUE
             , dir.create(file.path(dir_results))
             , FALSE)

      fn_runstats <- paste0("RunStats_", format.Date(Sys.Date(),"%Y%m%d"), ".tab")
      df_runstats <- as.data.frame(cbind("TargetSiteID", "Biocomm", "NumStressors"
                                         , "NumLoE", "ElapsedTime"))
      write.table(df_runstats, file.path(dir_results,fn_runstats), append = FALSE
                  , col.names = FALSE, row.names = FALSE, sep = "\t")

      # Main Code ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Main Code Start"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # TargetSiteID = "403S02363"
      # for (site in 1:length(TargetSiteID)) {
      #  for (site in 1:nrow(df_targets)) {
      startsite.time <- Sys.time()
      #   TargetSiteID <- df_targets$TargetSiteID[site] # already defined by Shiny interface
      # if (is.na(TargetSiteID)) { # Shiny, don't need this part
      #   next()
      # }
      msg <- paste0("Evaluating site: ",TargetSiteID)
      message(msg)
      # print(msg)
      # flush.console()

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Biocomm-independent functions
      # Skeleton 440

      # Create high-level results folder structure
      dir_sub2 <- TargetSiteID
      ifelse(!dir.exists(file.path(dir_results, dir_sub2))==TRUE
             , dir.create(file.path(dir_results, dir_sub2))
             , FALSE)

      # Establish data gaps file
      gaps <- cbind.data.frame("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = FALSE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

      # getComparators ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "getComparators"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # Identify comparator sites
      # This is predicated on the fact that BC distance is calculated based on
      # expected benthic macroinvertebrate taxa. If there are ever different
      # BC matrices for different biocomms, then this must move into the biocomm
      # loop or it needs to be run more than once for each biocomm here, since
      # it's used in getSiteInfo immediately afterward.
      list.CompSites <- getComparators(TargetSiteID
                                       , df_sites = data_Sites
                                       , df_bioCoOccur = data_bmiCoOccur
                                       , bioIndex = bmiIndex
                                       , useBC = useBC
                                       , df_bcdist = data_BCdist
                                       , bc_cutoff = 0.05
                                       , dir_results = dir_results
                                       , dir_sub = "SiteInfo")
      # Returns: myCompSites <- list(comp.sites = comp.sites
      #                             , gap.compsites = gap.statement
      comp_sites <- list.CompSites$comp.sites
      msg <- "getComparators is complete."
      message(msg)
      # print(msg)
      # flush.console()

      # getSiteInfo ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "getSiteInfo"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
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
                                      , data_algMetrics = data_AlgMetrics
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
      msg <- "getSiteInfo is complete."
      message(msg)
      # print(msg)
      # flush.console()

      # getClusterInfo ####
      # skeleton 510
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "getClusterInfo"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # Get Cluster Info
      getClusterInfo(TargetSiteID
                     , siteCOMID=list.SiteSummary$COMID
                     , siteCluster=list.SiteSummary$ClustID
                     , refSiteCOMIDs=list.SiteSummary$refCOMIDs
                     , data_cluster = data_cluster
                     , data_clusterInfo = data_clusterInfo
                     , dir_results=dir_results
                     , dir_sub="ClusterInfo")
      msg <- "getClusterInfo is complete."
      message(msg)
      # print(msg)
      # flush.console()

      # Munge str/resp ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Munge, Str/Resp"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Prepare flags for types of stressor and response data to use
      avail.data <- data_SampSummary[data_SampSummary$StationID_Master == TargetSiteID,]
      avail.data <- avail.data[,c(6:ncol(avail.data))]
      avail.data <- avail.data %>% select_if(not_all_na)
      samptypes  <- names(avail.data)

      #wd <- file.path(".") #2020-02-03, remove 2020-08-26

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
        #df_allStressInfo <- data_chemInfo  # Skeleton 561, remove 2020-08-26
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
          df_allStress <- rbind(df_allStress, data_modelRaw)
        } else {
          df_allStress <- data_modelRaw
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

      if (any(samptypes == algResp)) {
        useAlg = TRUE
        gap.alg.rsp <- cbind.data.frame("general", "useALG", 1, "Algae responses available.")
        colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
      } else {
        useAlg = FALSE
        gap.alg.rsp <- cbind.data.frame("general", "useALG", 0, "No algae responses available.")
        colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
      } ### End If statement for measured stressorsalgal responses

      gaps <- rbind.data.frame(gap.chem.stress, gap.phab.stress, gap.mod.stress
                               , gap.bmi.rsp, gap.alg.rsp)
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # if ((useMeasStress==FALSE) & (useModStress==FALSE)) {
      #     # No stressor data available
      #     gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0, "No chemistry stressors available.")
      #     colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
      # }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Skeleton 622
      for (b in 1:length(biocommlist)) {

        noStressors <- FALSE
        noResponses <- FALSE

        NE_true <- FALSE  # 2020-08-26, added from skeleton

        if ((useMeasStress==FALSE) & (useModStress==FALSE)) {
          # No stressor data available
          gap.stress <- cbind.data.frame("general", "Stressors", 0
                                         , "No stressor data available.")
          colnames(gap.stress) <- c("fxnname", "condition", "result"
                                    , "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
          write.table(gap.stress, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          noStressors <- TRUE
        }
        if ((useAlg==FALSE) & (useBMI==FALSE)) {
          # No stressor data available
          gap.resp <- cbind.data.frame("general", "Responses", 0
                                       , "No response data available.")
          colnames(gap.resp) <- c("fxnname", "condition", "result"
                                  , "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
          write.table(gap.resp, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          noResponses <- TRUE
        }
        if ((noStressors==TRUE) | (noResponses==TRUE)) {
          msg <- ifelse((noStressors==TRUE) & (noResponses==TRUE)
                        , paste0("No stressor or response data are available for "
                                 , TargetSiteID)
                        , ifelse(noStressors==TRUE
                                 , paste0("No stressor data are available for "
                                          , TargetSiteID)
                                 , paste0("No response data are available for "
                                          , TargetSiteID)))
          message(msg)
          # print(msg)
          # flush.console()
          next
        }

        numLoE = 0

        LoEs <- c("TS", "CO", "SR", "VP", "SSD")
        df_LoE <- as.data.frame(LoEs)
        colnames(df_LoE) <- "LoE"
        df_LoE <- df_LoE %>%
          mutate(LoE = as.character(LoE)
                 , Completed = as.integer(0)
                 , ResultsDir = as.character(NA))

        # Define biocomm data
        # skeleton 679
        bioComm <- biocommlist[b]
        if ((bioComm=="bmi") && (useBMI==TRUE)) {

          data_bioCoOccur <- data_bmiCoOccur
          bioIndex <- bmiIndex
          bioIndexGp <- bmiIndexGp
          bioMetricNames <- bmiMetrics
          bioMetricData <- data_bmiMetrics
          bioMetricInfo <- data_bmiMetricsInfo
          bioTaxaData <- data_BMIcounts
          bioMasterTaxa <- data_BMIMasterTaxa
          colBio <- bmiIndex
          colBioSample <- bmiResp
          colBioSampDate <- bmiRespDate
          BioNarBrk <- bmi_thresholds
          BioNarLab <- bmi_narrative
          BioDegBrk <- bmi_deg_thres
          BioDegLab <- bmi_deg_text
          modelParams <- bmiModelParamsKeep
          bioParmsDEL <- bmiModelParamsDEL

        } else if ((bioComm=="algae") && (useAlg==TRUE)) {

          data_bioCoOccur <- data_algCoOccur
          bioIndex <- algIndex
          bioIndexGp <- algIndexGp
          bioMetricNames <- algMetrics
          bioMetricData <- data_AlgMetrics
          bioMetricInfo <- data_AlgMetricsInfo
          bioTaxaData <- data_AlgCounts
          bioMasterTaxa <- data_AlgMasterTaxa
          colBio <- algIndex
          colBioSample <- algResp
          colBioSampDate <- algRespDate
          BioNarBrk <- alg_thresholds
          BioNarLab <- alg_narrative
          BioDegBrk <- alg_deg_thres
          BioDegLab <- alg_deg_text
          modelParams <- algModelParamsKeep
          bioParmsDEL <- algParamsDEL

        } else {
          msg <- paste0(bioComm, " is not a valid biological community.")
          message(msg)
          # print(msg)
          # flush.console()
          next()
        }

        #skeleton 726, replace section
        # If no paired stressor-response samples for target site, no eval possible
        #<<<<<<< 201909_ARL
        if (!(TargetSiteID %in% data_bioCoOccur$StationID_Master)) { # Not in data_bioCoOccur
          noStressors = TRUE
        } else {
          dfTarget <- dplyr::filter(data_bioCoOccur, StationID_Master==TargetSiteID)
          if (all(is.na(dfTarget[,11:ncol(dfTarget)]))) { # In data_bioCoOccur but all values NA
            noStressors = TRUE
          } else {
            noStressors = FALSE
          }
        }
        if (noStressors==TRUE) {
          #===
          #        if (!(TargetSiteID %in% data_bioCoOccur$StationID_Master)) {
          #>>>>>>> master
          msg <- paste0("No paired stressor-response samples for", TargetSiteID
                       , " for the ", bioComm, " community.")
          message(msg)

          # No identified stressors may be a data gap, but may not be, either
          gapcomment <- paste0("No paired stressor-", bioComm, " samples are available "
                               , "for ", TargetSiteID, " within ", lagdays, " days, "
                               , "with the stressor sample being obtained prior "
                               , "to the response sample.")
          gaps <- cbind.data.frame("getCoOccurDataset", paste0("Paired stressor-"

                                                               , bioComm, " data"), 0, gapcomment)

          # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")

          # Write run-time stats to file
          endsite.time <- Sys.time()
          elapsedsite.time <- endsite.time - startsite.time

          df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                         , "Biocomm" = bioComm
                                         , "NumStressors" = NA
                                         , "NumLoE" = numLoE
                                         , "ElapsedTime" = elapsedsite.time))
          # if (site == 1) {
          #     df_runstats <- df_temp
          # } else {
          #     df_runstats <- rbind(df_runstats, df_temp)
          # } ### End gather run stats
          write.table(df_temp, file.path(wd,"Results",fn_runstats)
                      , append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")

          rm(dfTarget)
          next()
        } ### End no stressors statement

        # getQualSites ####
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- paste0(bioComm, "; getQualSites")
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        # Run analyses
        # Identify "quality" sites using different definitions
        list.BioQualSites <- getQualSites(TargetSiteID
                                          , df_sites = data_Sites
                                          , biocomm = bioComm
                                          , df_qual = data_bioCoOccur
                                          , colBio = colBio
                                          , colBioSample = "RespSampID"
                                          , colStressSample = "StressSampID"
                                          , comp_sites = comp_sites
                                          , useBC = useBC
                                          , BioNarBrk = BioNarBrk
                                          , BioNarLab = BioNarLab
                                          , BioDegBrk = BioDegBrk
                                          , BioDegLab = c("Yes", "No")
                                          , dir_results = dir_results)
        # Returns: myQualSites <- list(dfQuality = df_qual
        #                              , allRefBioSites = all.ref
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
        msg <- paste0("getQualSites is complete for ", bioComm, ".")
        message(msg)
        # print(msg)
        # flush.console()

        # getDataSets ####
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- paste0(bioComm, "; getDataSets")
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        # skeleton 832
        # Get data sets for stressors paired with response data, if available
        listPairedStressResp <- getDataSets(TargetSiteID
                                            , compSites = comp_sites
                                            , df_coOccur = data_bioCoOccur
                                            , measParams = measParams
                                            , modelParams = modelParams
                                            , biocomm = bioComm
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
        msg <- "Stressor and response data prepared, for all possible stressors."
        message(msg)
        # print(msg)
        # flush.console()

        compPairedSR <- listPairedStressResp$compBioStress %>%
          select(-StressSampDate, -RespSampDate, -RespSampID)
        sitePairedSR <- listPairedStressResp$siteBioStress %>%
          select(-StressSampDate, -RespSampDate, -RespSampID)
        sitePairedStressors <- as.vector(colnames(sitePairedSR[,3:ncol(sitePairedSR)]))


        # Prepare data sets of all stressors ever detected at the target site
        # 2020-08-26, replace section
        if (removeOutliers == TRUE) {
          siteStressAll <- data_Stress %>%
            dplyr::filter(StationID_Master==TargetSiteID) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(Outlier != "Outlier") %>%
            tidyr::spread(key=StdParamName, value=ResultValue) %>%
            #<<<<<<< 201909_ARL
            dplyr::rename(StressSampID = ChemSampleID
                          , StressSampDate = SampleDate)
          if (ncol(siteStressAll)>7) {
            siteStressAllCore <- siteStressAll[1:6]
            siteStressAllParms <- siteStressAll[,7:ncol(siteStressAll)] %>%
              dplyr::select_if(not_all_na)
            siteStressAll <- cbind(siteStressAllCore, siteStressAllParms)
            rm(siteStressAllCore, siteStressAllParms)
          }
          #===
          #                dplyr::select_if(not_all_na) %>%
          #                dplyr::rename(StressSampID = ChemSampleID
          #                              , StressSampDate = SampleDate)
          #>>>>>>> master
          siteDetectsAll <- as.vector(colnames(siteStressAll[,4:ncol(siteStressAll)]))
          compStressAll <- data_Stress %>%
            dplyr::filter(StationID_Master %in% comp_sites) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(Outlier != "Outlier") %>%
            dplyr::filter(StdParamName %in% siteDetectsAll) %>%
            tidyr::spread(key=StdParamName, value=ResultValue) %>%
            dplyr::rename(StressSampID = ChemSampleID
                          , StressSampDate = SampleDate)
          siteRespAll <- bioMetricData %>%
            dplyr::filter(StationID_Master == TargetSiteID) %>%
            dplyr::rename(RespSampID = eval(colBioSample)
                          , RespSampDate = eval(colBioSampDate))
        } else {
          siteStressAll <- data_Stress %>%
            dplyr::filter(StationID_Master==TargetSiteID) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            #<<<<<<< 201909_ARL
            dplyr::filter(Outlier != "Outlier") %>%
            tidyr::spread(key=StdParamName, value=ResultValue) %>%
            dplyr::rename(StressSampID = ChemSampleID
                          , StressSampDate = SampleDate)
          siteStressAll <- dplyr::select_if(siteStressAll
                                            , not_all_na(siteStressAll[7:ncol(siteStressAll)]))
          #===
          #                tidyr::spread(key=StdParamName, value=ResultValue) %>%
          #                dplyr::select_if(not_all_na) %>%
          #                dplyr::rename(StressSampID = ChemSampleID
          #                              , StressSampDate = SampleDate)
          #>>>>>>> master
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
                          , RespSampDate = eval(colBioSampDate))
        }## IF ~ removeOutliers ~ END

        # Log removed outliers as data gaps
        # skeleton
        data_StressLabeled <- merge(data_Stress, data_stressInfo[,c("Analyte","Label")]
                                    , by.x="StdParamName", by.y="Analyte", all.x= TRUE)
        siteOutliers <- data_StressLabeled %>%
          dplyr::filter(StationID_Master==TargetSiteID) %>%
          dplyr::filter(!is.na(ResultValue)) %>%
          dplyr::filter(Outlier == "Outlier")
        compOutliers <- data_StressLabeled %>%
          dplyr::filter(StationID_Master %in% comp_sites) %>%
          dplyr::filter(!is.na(ResultValue)) %>%
          dplyr::filter(Outlier == "Outlier")
        allOutliers <- data_StressLabeled %>%
          dplyr::filter(!is.na(ResultValue)) %>%
          dplyr::filter(Outlier == "Outlier")

        if (nrow(siteOutliers)>0) {
          for (r in 1:nrow(siteOutliers)) {
            stressor <- siteOutliers$StdParamName[r]
            strLabel <- siteOutliers$Label[r]
            result <- siteOutliers$ResultValue[r]
            siteID <- as.character(siteOutliers$StationID_Master[r])
            gapcomment <- paste0(siteID
                                 , " value removed as an outlier."
                                 , " Transformation applied prior to"
                                 , " identification as necessary.")
            gaps <- cbind.data.frame("Site outliers", strLabel, result
                                     , gapcomment)
            colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
          }
        }## IF ~ siteOutliers ~ END
        if (nrow(compOutliers)>0) {
          for (r in 1:nrow(compOutliers)) {
            stressor <- compOutliers$StdParamName[r]
            strLabel <- compOutliers$Label[r]
            result <- compOutliers$ResultValue[r]
            siteID <- as.character(compOutliers$StationID_Master[r])
            if (siteID != TargetSiteID) {
              gapcomment <- paste0(siteID
                                   , " value removed as an outlier."
                                   , " Transformation applied prior to"
                                   , " identification as necessary.")
              gaps <- cbind.data.frame("Comparator outliers", strLabel, result
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
            }
          }
        }## IF ~ compOutliers ~ END
        if (nrow(allOutliers)>0) {
          for (r in 1:nrow(allOutliers)) {
            stressor <- allOutliers$StdParamName[r]
            strLabel <- allOutliers$Label[r]
            result <- allOutliers$ResultValue[r]
            siteID <- as.character(allOutliers$StationID_Master)[r]
            if (!(siteID %in% comp_sites)) {
              gapcomment <- paste0("Value removed as an outlier for site "
                                   , siteID
                                   , " Transformation applied prior to"
                                   , " identification as necessary.")
              gaps <- cbind.data.frame("All data outliers", strLabel, result
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
            }
          }
        }## IF ~ allOutliers ~ END

        # getStressorList ####
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- paste0(bioComm, "; getStressorList")
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        # skeleton 1003
        # Get Stressor List using all stressors ever detected at the target site
        list.stressors <- getStressorList(TargetSiteID
                                          , siteCluster=list.SiteSummary$ClustID
                                          , chemInfo=data_stressInfo
                                          , clusterChem=compStressAll
                                          , siteQual2Plot=siteQual2Plot
                                          , refSamps=allBioRefStressSamps
                                          , refSites=allBioRefSites
                                          , siteChem=siteStressAll
                                          , probsHigh=probsHigh
                                          , probsLow=probsLow
                                          , DOlim=DOlim
                                          , pHlimLow=pHlimLow
                                          , pHlimHigh=pHlimHigh
                                          , biocomm=bioComm
                                          , bioParmsDEL=bioParmsDEL
                                          , dir_results=dir_results
                                          , dir_sub="CandidateCauses")
        # Returns: myStressors <- list(stressors = stressorlist
        #                     , site.stressor.pctrank = site.pctrank
        #                     , stressors_LogTransf
        #                     , stressors_Excepted)
        stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
        stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
        msg <- "getStressorList is complete."
        message(msg)
        # print(msg))
        # flush.console()

        stressorsNOpairing <- setdiff(stressors, sitePairedStressors)
        stressorsWPairedResponses <- intersect(stressors, sitePairedStressors)

        ### MODIFY siteStressAll to keep all core cols and only stressor cols
        # skeleton

        # If no stressors are identified, no analyses can be performed. Error msg.
        if (length(stressors) == 0) {
          msg <- paste("No stressors identified for", TargetSiteID)
          message(msg)
          # print(msg)
          # flush.console()

          # No identified stressors may be a data gap, but may not be, either
          gapcomment <- paste0("No potential stressors fall outside the specified "
                               , "quantile range (", probsLow, " to ", probsHigh,").")
          gaps <- cbind.data.frame("getStressorList", "Number of stressors", 0
                                   , gapcomment)
          colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")

          # Write run-time stats to file
          endsite.time <- Sys.time()
          elapsedsite.time <- endsite.time - startsite.time

          df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                         , "Biocomm" = bioComm
                                         , "NumStressors" = length(stressors)
                                         , "NumLoE" = numLoE
                                         , "ElapsedTime" = elapsedsite.time))
          # if (site == 1) {
          #     df_runstats <- df_temp
          # } else {
          #     df_runstats <- rbind(df_runstats, df_temp)
          # } ### End gather run stats
          write.table(df_temp, file.path(wd,"Results",fn_runstats)
                      , append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          next()
        } ### End no stressors statement

        if (length(stressorsNOpairing)>0) {
          for (s in 1:length(stressorsNOpairing)) {
            # Candidate causes identified as possible stressors but without
            # paired response data to allow evaluation
            # Grab labels instead of stdparamname
            stressname <- stressorsNOpairing[s]
            strLabel <- unique(as.character(data_stressInfo$Label[data_stressInfo$Analyte==stressname]))
            gapcomment <- paste0("Stressor detected but paired response "
                                 ,"data are not available.")
            gaps <- cbind.data.frame("getStressorList", strLabel, 0
                                     , gapcomment)
            colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
          }
        } ### End unpaired stressors statement

        if (length(stressorsWPairedResponses)==0) {
          NE_true <- TRUE
          # Candidate causes identified as stressors had no response sample
          # obtained within lagdays following the stressor sample collection
          gapcomment <- paste0("No identified possible stressors had a response "
                               , "sample obtained within ", lagdays, " days of "
                               , "stressor sample collection.")
          gaps <- cbind.data.frame("getStressorList", "Paired stresssor/responses"
                                   , 0, gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
        } else {
          NE_true <- FALSE
          stressorsUsed <- as.data.frame(stressorsWPairedResponses)
          colnames(stressorsUsed)[1] <- "Stressor"
          stressorsUsed <- merge(stressorsUsed
                                 , data_stressInfo[,c("Analyte","Label")]
                                 , by.x = "Stressor"
                                 , by.y = "Analyte"
                                 , all.x = TRUE)
          stressorsUsed <- unique(stressorsUsed)
          fn.stressorsUsed <- file.path(dir_results,TargetSiteID
                                        , toupper(bioComm)
                                        , "CandidateCauses/"
                                        , paste0(TargetSiteID, "_"
                                                 ,toupper(bioComm)
                                                 , "_CandCauses_StressorsEvaluated.tab"))
          write.table(stressorsUsed, fn.stressorsUsed, append = FALSE
                      , col.names = TRUE, row.names = FALSE, sep = "\t")
        }

        # Either all are paired or some are
        # skeleton 1122
        stressors_logtransf <- data_stressInfo$LogTransf[data_stressInfo$StdParamName
                                                         %in% stressorsWPairedResponses]
        # 2020-08-26, commented out in Skeleton
        # # getTimeSeq ####
        # prog_cnt <- prog_cnt + 1
        # prog_msg <- paste0("Step ", prog_cnt)
        # prog_det <- paste0(bioComm, "; getTimeSeq")
        # incProgress(prog_inc, message = prog_msg, detail = prog_det)
        # Sys.sleep(mySleepTime)
        # Create time sequence graphics
        # Uses all site stressor and response data, but not paired
        # getTimeSeq(TargetSiteID
        #            , biocomm = bioComm
        #            , BioResp = bioMetricNames
        #            , stressors = stressorsWPairedResponses
        #            , df_stress = siteStressAll   # Not just paired data
        #            , df_resp = siteRespAll       # Not just paired data
        #            , df_stressinfo = df_allStressInfo
        #            , df_respinfo = bioMetricInfo
        #            , dir_results = dir_results
        #            , dir_sub = "TimeSequence")
        # msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
        # message(msg)
        # # print(msg)
        # # flush.console()
        #
        # # NOT WORKING
        # dirTS <- file.path(dir_results, TargetSiteID, toupper(bioComm)
        #                    , "TimeSequence")
        # if (dir.exists(dirTS)==TRUE) {
        #   if (length(list.files(dirTS)) > 0) {
        #     numLoE = numLoE + 1
        #     df_LoE$Completed[df_LoE$LoE == "TS"] <- 1
        #     df_LoE$ResultsDir[df_LoE$LoE == "TS"] <- dirTS
        #
        #   }
        # }
        # 2020-08-26, commented out in Skeleton

        #skeleton 1163
        if (NE_true) { # No paired stressor response data available. Move to next biocomm or site.
          # Write run-time stats to file
          endsite.time <- Sys.time()
          elapsedsite.time <- endsite.time - startsite.time

          df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                         , "Biocomm" = bioComm
                                         , "NumStressors" = length(stressors)
                                         , "NumLoE" = numLoE
                                         , "ElapsedTime" = elapsedsite.time))
          # if (site == 1) {
          #     df_runstats <- df_temp
          # } else {
          #     df_runstats <- rbind(df_runstats, df_temp)
          # } ### End gather run stats
          write.table(df_temp, file.path(wd,"Results",fn_runstats)
                      , append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")


        } else {

          # getTimeSeq ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- "getTimeSeq"
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          # Create time sequence graphics
          # Uses all site stressor and response data, but not paired
          getTimeSeq(TargetSiteID
                     , biocomm = bioComm
                     , BioResp = bioMetricNames
                     , df_stress = siteStressAll
                     , df_resp = siteRespAll
                     , stressors = stressorsWPairedResponses
                     , df_stressinfo = data_stressInfo
                     , df_respinfo = bioMetricInfo
                     , dir_results = dir_results
                     , dir_sub = "TimeSequence")
          msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
          message(msg)

          # NOT WORKING
          dirTS <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                             , "TimeSequence")
          if (dir.exists(dirTS)==TRUE) {
            if (length(list.files(dirTS)) > 0) {
              numLoE = numLoE + 1
              df_LoE$Completed[df_LoE$LoE == "TS"] <- 1
              df_LoE$ResultsDir[df_LoE$LoE == "TS"] <- dirTS

            }
          }

          # getCoOccurr ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- "getCoOccurr"
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Get Response-based co-occurrence
          if (TargetSiteID %in% unique(data_bioCoOccur$StationID_Master)) {
            msg <- ("Starting Co-occurrence")
            message(msg)
            getCoOccur(df_data = data_bioCoOccur
                       , TargetSiteID = TargetSiteID
                       , col_ID = "StationID_Master"
                       , colStressSamp = "StressSampID"
                       , colRespSamp = "RespSampID"
                       , colGroup = "clust"
                       , colBio = colBio
                       , colStressors = stressorsWPairedResponses
                       , df_stressinfo = data_stressInfo
                       , BioNarBrk = BioNarBrk
                       , BioNarLab = BioNarLab
                       , BioDegBrk = BioDegBrk
                       , BioDegLab = c("Yes", "No")
                       , biocomm = bioComm
                       , dir_plots = dir_results
                       , dir_sub = "CoOccurrence"
                       , col_StressInvScore = col_StressInvScore)
          } else {
            # gapcomment <- "Stressor detected but paired response not available"
            # gaps <- cbind.data.frame("getStressorList", stressorsNOpairing[s], 0
            #                          , gapcomment)
            # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            # fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            # write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
            #             , row.names = FALSE, sep = "\t")
          } ### End getCoOccur
          msg <- paste0("getCoOccur for ", bioComm, " is complete.")
          message(msg)

          dirCO <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                             , "CoOccurrence")
          if (dir.exists(dirCO)==TRUE) {
            if ((length(list.files(dirCO)) > 0)==TRUE) {
              numLoE = numLoE + 1
              df_LoE$Completed[df_LoE$LoE == "CO"] <- 1
              df_LoE$ResultsDir[df_LoE$LoE == "CO"] <- dirCO
            }
          }

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
                   , Quality, eval(bioMetricNames))
          cl.b.rsp <- listPairedStressResp$compBioResp %>%
            select(RespSampID, StressSampID, StationID_Master, RespSampDate
                   , Quality, eval(bioMetricNames))
          site.b.rsp <- listPairedStressResp$siteBioResp %>%
            select(RespSampID, StressSampID, StationID_Master, RespSampDate
                   , Quality, eval(bioMetricNames))

          siteStressInfo <- listPairedStressResp$siteStressInfo

          list_MatchBioData <- list("all.b.str" = all.b.str
                                    , "cl.b.str" = cl.b.str
                                    , "site.b.str" = site.b.str
                                    , "all.b.rsp" = all.b.rsp
                                    , "cl.b.rsp" = cl.b.rsp
                                    , "site.b.rsp" = site.b.rsp)

          # getBioStressorResponses ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- "getBioStressorResponses"
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Get Stressor Responses
          getBioStressorResponses(TargetSiteID
                                  , stressors = stressorsWPairedResponses
                                  , stressorInfo = siteStressInfo
                                  , BioResp = bioMetricNames
                                  , BioInfo = bioMetricInfo
                                  , list.MatchBioData = list_MatchBioData
                                  , ref.sites = allBioRefStressSamps
                                  , siteQual2Plot = siteQual2Plot
                                  , biocomm = bioComm
                                  , dir_results = dir_results
                                  , dir_sub = "StressorResponse")
          msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
          message(msg)

          dirSR <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                             , "StressorResponse")
          if (dir.exists(dirSR)==TRUE) {
            if (length(list.files(dirSR)) > 0) {
              numLoE = numLoE + 1
              df_LoE$Completed[df_LoE$LoE == "SR"] <- 1
              df_LoE$ResultsDir[df_LoE$LoE == "SR"] <- dirSR
            }
          }

          # getVerifiedPredictions ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- "getVerifiedPredictions"
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Get Stressor-specific regressions
          if (any(SSTVparms %in% stressorsWPairedResponses)) {
            getVerifiedPredictions(TargetSiteID
                                   , SSTVanalytes = as.character(SSTVparms)
                                   , colBioSample = colBioSample
                                   , stressors = stressorsWPairedResponses
                                   , stressorInfo <- siteStressInfo
                                   , dataBioTaxa = bioTaxaData
                                   , dataMasterTaxa = bioMasterTaxa
                                   , matchedData = list_MatchBioData
                                   , BioIndex_Val = bioIndex
                                   , BioIndex_Nar = "Quality"
                                   , BioIndex_Nar_Deg = "Degraded"
                                   , dir_results=dir_results
                                   , dir_sub="VerifiedPredictions"
                                   , biocomm=bioComm)
          } else {
            print("No possible stressors have stressor-specific tolerance values.")
            flush.console()
            gapcomment <- paste0("Stressors having stressor-specific tolerance "
                                 , "values are not identified at this site.")
            gaps <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0
                                     , gapcomment)
            colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
          } ### End getVP evaluation

          msg <- paste0("getVerifiedPredictions for ", bioComm, " is complete.")
          message(msg)

          dirVP <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                             , "VerifiedPredictions")
          if (dir.exists(dirVP)==TRUE) {
            if (length(list.files(dirVP)) > 0) {
              numLoE = numLoE + 1
              df_LoE$Completed[df_LoE$LoE == "VP"] <- 1
              df_LoE$ResultsDir[df_LoE$LoE == "VP"] <- dirVP
            }
          }

          # # Not enabled yet
          # # getSSDs
          # # getSSDplot(Data, ResponseType, Taxa, Exposure)
          # # myDF <- data_SSD_generator
          # # myRT   <- "ResponseType"
          # # myTaxa <- "Taxa"
          # # myExp  <- "Exposure"
          # # Run function
          # # p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)

          # getWOE ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- "getWOE"
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          getWoE(TargetSiteID
                 , biocomm = bioComm
                 , index = bioIndex
                 , dir_results = dir_results
                 , dfLoE = df_LoE
                 , dfQual = list.BioQualSites$dfQuality
                 , dfStr = list_MatchBioData$site.b.str
                 , dfRank = list.stressors$site.stressor.pctrank
                 , dfStressInfo = siteStressInfo
                 , df_coOccur = data_bioCoOccur
                 , BioResp = bioMetricNames)
          msg <- paste0("getWoE for ", bioComm, " is complete.")
          message(msg)

        }## IF ~ NE_true ~ END

        # Write run-time stats to file
        # skeleton 1387
        endsite.time <- Sys.time()
        elapsedsite.time <- endsite.time - startsite.time

        df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                       , "Biocomm" = bioComm
                                       , "NumStressors" = length(stressors)
                                       , "NumLoE" = numLoE
                                       , "ElapsedTime" = elapsedsite.time))
        #<<<<<<< 201909_ARL
        # if (site == 1) {
        #   df_runstats <- df_temp
        # } else {
        #   df_runstats <- rbind(df_runstats, df_temp)
        # } ### End gather run stats
        # Shiny mod (always 1)
        df_runstats <- df_temp
        write.table(df_temp, file.path(wd,"Results",fn_runstats)
                    , append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")

      } ### End biocomm loop


      # getReport ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "getReport"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # Get final report (Executive Summary style)
      dir_data_abs <- normalizePath(dir_data)
      dir_results_abs <- normalizePath(dir_results)
      # test
      # probsHigh <- 0.75
      # probsLow <- 0.25
      # useBMI <- TRUE
      # useAlg <- TRUE
      # useBC <- TRUE
      # removeOutliers <- TRUE
      # lagdays <- 10
      # bmiIndex <- "CSCI"
      # algIndex <- "MMIhybrid"
      # siteQual2Plot <- "not degraded"
      #
      getReport(TargetSiteID
                , probsHigh=probsHigh
                , probsLow=probsLow
                , useBMI=useBMI
                , useAlg=useAlg
                , useBC=TRUE
                , removeOutliers=removeOutliers
                , lagdays=lagdays
                , bmiIndex=bmiIndex
                , algIndex=algIndex
                , dir_data=dir_data_abs
                , dir_results=dir_results_abs
                , report_type="summary"
                , report_format="html"
                , dir_rmd=file.path(system.file(package = "CASTfxn"), "rmd")
                , siteQual2Plot = siteQual2Plot)

      # rm(list.SiteSummary, list.data, list.stressors, list.ChemBMIData
      #    , chem.info, stressors, stressors_logtransf, data.SSTV.totabund)
      #

      dfGaps <- read.table(file.path(dir_results, TargetSiteID
                                     , paste0(TargetSiteID,"_datagaps.tab"))
                           , header = TRUE, sep="\t")
      dfGaps <- unique(dfGaps)
      write.table(dfGaps, file.path(dir_results, TargetSiteID
                                    , paste0(TargetSiteID,"_datagaps.tab"))
                  , append = FALSE, col.names = TRUE, row.names = FALSE
                  , sep = "\t")

      #} ### End TargetSite loop

      #rm(site) # error, remove 2020-08-17 (part of loop not used)
      #~~~~~~~~~~~~~~~~~~~~~~~~

      # getSummaryAllSites ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "getSummaryAllSites"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      getSummaryAllSites(biocommlist = c("bmi", "algae")
                         , bmiIndex = "CSCI"
                         , algIndex = "MMIhybrid"
                         , dir_data = dir_data
                         , dir_results = dir_results
                         , dir_sub = "WoE"
                         , df_sites = NULL)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # end of Ann's skeleton code, 2020-02-03
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Determine and print elapsed time
      #   end.time <- Sys.time()
      #   elapsed.time <- end.time - start.time
      #  # msg <- paste(site, "sites completed in", elapsed.time)
      # #  msg <- paste(TargetSiteID, "sites completed in", elapsed.time)
      #   message(msg)
      #   # print(msg)
      #   # flush.console()
      #   rm(TargetSiteID)  # Comment out 20200817


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      msgDetail_A <- "Base Data"
      msgDetail_B <- "Load input data"
      incProgress(1/prog_n, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)

      # CopyResults ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Copy Results"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Results"
      msgDetail_B <- "Prepare for display"
      incProgress(1/prog_n, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      # Copy from Results to www/Results
      CopyResults(TargetSiteID)
      
      # Create zip ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Create Zip Download"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      fn_zip <- file.path(".", "Results", paste0(TargetSiteID, ".zip"))
      zip(fn_zip, file.path(dir_results, TargetSiteID))


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Complete ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Complete"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ALL"
      msgDetail_B <- "COMPLETE"
      incProgress(1/prog_n, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #




      #
    }, message = "Run ALL")##withProgress ~ END

    # enable download button ####
    shinyjs::enable("b_downloadData")
    
  }##Run_ALL2~END
  
  Run_ALL <- function(){
    #
   shiny::withProgress({
      #
      start.time <- Sys.time() # Added 2020-08-17 to match with line 2744 (after getSummaryAllSites)
      # Number of increments
      prog_n <- 26 + 7 # confirmed 20200205 (getQS:getWoE repeats for 1661:2447)
      prog_inc <- 1/prog_n
      prog_cnt <- 0
      mySleepTime <- 0.5
      #
      # Remove Zip ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Remove Zip"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
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
      # source(file.path(gitpath, "getCoOccurDataset.R"))
      # source(file.path(gitpath, "getTimeSeq.R"))
      # source(file.path(gitpath, "getDataSets.R"))
      # source(file.path(gitpath, "getComparators.R"))
      # source(file.path(gitpath, "getSiteInfo.R"))
      # source(file.path(gitpath, "getClusterInfo.R"))
      # source(file.path(gitpath, "getStressorList.R"))
      # source(file.path(gitpath, "getCoOccur.R"))
      # source(file.path(gitpath, "getBioStressorResponses.R"))
      # source(file.path(gitpath, "getVerifiedPredictions.R"))
      # source(file.path(gitpath, "getOutliers.R"))
      # source(file.path(gitpath, "getWoE.R"))
      # source(file.path(gitpath, "getQualSites.R"))
      # source(file.path(gitpath, "getSummaryAllSites.R"))
      # source(file.path(gitpath, "getReport.R"))

      # source(file.path(gitpath, "getDataGaps.R"))
      # source(file.path(gitpath, "getSiteBackground.R"))

      # put in global
      #not_all_na <- function(x) {!all(is.na(x))}

      # Timer, Start
      startprep.time <- Sys.time()

      # Required user-designated options
      wd <- file.path(".")
      dir_data <- file.path(wd, "Data")
      dir_results <- file.path(wd, "Results")
      #
      boo_removeOutliers <- TRUE
      useBC <- TRUE # Use Bray-Curtis biological dissimilarity distance matrix
      probsHigh=0.75
      probsLow=0.25
      DOlim=7
      pHlimLow=6.5
      pHlimHigh=9
      lagdays=10
      biocommlist <- c("bmi","algae")
      siteQual2Plot = "not degraded" # options:"reference","better than","not degraded"
      report_format="html"    # word, pdf are the other options


      # Specify Base Filenames # These are the files used to run the analyses
      # Data Files ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Load Data Files"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #fn.targets <- file.path(dir_data,"SMCTestSites.xlsx")
      fn.Sites.Info       <- file.path(dir_data, "SMCSitesFinal.tab")
      fn.SampSummary      <- file.path(dir_data, "SMCSiteSummary.tab")
      fn.cheminfo         <- file.path(dir_data, "SMCMeasStressInfoFinal.tab")
      fn.chemdata         <- file.path(dir_data, "SMCMeasStressDataFinal.tab")
      fn.modelinfo        <- file.path(dir_data, "SMCModelStressInfoFinal.tab")
      fn.modeldata        <- file.path(dir_data, "SMCModelStressDataFinal.tab")
      fn.bmi.metrics      <- file.path(dir_data, "SMCBenthicMetricsFinal.tab")
      fn.bmi.cscicore     <- file.path(dir_data, "SMCBenthicCSCIcore.tab")
      fn.bmi.metrics.info <- file.path(dir_data, "SMCBenthicMetricsInfo.tab")
      fn.bmi.raw          <- file.path(dir_data, "SMCBenthicCountsFinal.tab")
      fn.MT.bmi           <- file.path(dir_data, "SMCBenthicMasterTaxa.tab")
      fn.alg.metrics      <- file.path(dir_data, "SMCAlgaeMetricsFinal.tab")
      fn.alg.metrics.info <- file.path(dir_data, "SMCAlgaeMetricsInfo.tab")
      fn.alg.raw          <- file.path(dir_data, "SMCAlgaeCountsFinal.tab")
      fn.MT.alg           <- file.path(dir_data, "SMCAlgaeMasterTaxa.tab")
      fn.bcdist           <- file.path(dir_data, "SMCBCDist.tab")
      fn.cluster          <- file.path(dir_data, "SMCClusterData.tab")
      fn.clusterinfo      <- file.path(dir_data, "SMCClusterInfo.tab")
      fn.bkgdata          <- file.path(dir_data, "SMCSiteBkgdData.tab")
      fn.bkginfo          <- file.path(dir_data, "SMCSiteBkgdInfo.tab")
      #
      # GIS
      # outline <- rgdal::readOGR(dsn = "Data/SMCBoundary", layer = "SMCBoundary_aea")
      # flowline <- rgdal::readOGR(dsn = "Data/SMCReaches", layer = "SMCReaches_aea")

      # Load GIS files
      message("Loading GIS files.")
      outline <- rgdal::readOGR(dsn = "Data/SMCBoundary", layer = "SMCBoundary_aea", pointDropZ = TRUE)
      flowline <- suppressWarnings(rgdal::readOGR(dsn = "Data/SMCReaches", layer = "SMCReaches_aea", pointDropZ = TRUE))
      # warning z-dimension discarded.  "pointDropZ = TRUE" does not remove the warning

      # Specify user-defined variables
      # Stressors
      meas.stress <- c("ChemSampleID", "PhabSampID", "FldChemSampID")
      chem.stress <- c("ChemSampleID", "FldChemSampID")
      hab.stress <- "PhabSampID"
      mod.stress <- "FlowSampID"

      # BMI responses
      bmi_thresholds <- c(-2, 0.62, 0.799, 0.919, 2)
      bmi_narrative <- c("very likely altered", "likely altered"
                         , "possibly altered", "likely intact")
      bmi_deg_thres <- c(-2, 0.799, 2)
      bmi_deg_text <- c("Yes", "No")
      bmiIndexGp <- c("CSCI", "OoverE", "MMI")
      bmiResp <- "BMISampID"
      bmiRespDate <- "BMISampDate"
      # bmiMetrics <- c(bmiIndex, "MMI", "OoverE", "Taxonomic_Richness"
      #                 , "Intolerant_Percent", "Shredder_Taxa", "Clinger_PercentTaxa"
      #                 , "Coleoptera_PercentTaxa", "EPT_PercentTaxa")


      # Algal responses
      alg_thresholds <- c(-2, 0.82, 2)
      alg_narrative <- c("Degraded", "Not Degraded")
      alg_deg_thres <- c(-2, 0.82, 2)
      alg_deg_text <- c("Yes", "No")
      algIndexGp <- c("MMIhybrid", "MMIdiatom", "MMIsba")
      algResp <- "AlgSampID"
      algRespDate <- "AlgSampDate"
      # algMetrics <- c("MMIdiatom"
      #                 , "propsppOxyReqDO_10_rawdiatom"
      #                 , "cntsppBCG3_rawdiatom"
      #                 , "propCyclotella_rawdiatom"
      #                 , "propSurirella_rawdiatom"
      #                 , "propsppIndicatorClass_TP_low_rawdiatom"
      #                 , "propsppOrgNNHHONF_rawdiatom"
      #                 , "MMIsba"
      #                 , "cntsppBCG3_rawsba"
      #                 , "propsppIndicatorClass_NonRef_rawsba"
      #                 , "propsppGreen_rawsba"
      #                 , "cntsppIndicatorClass_Cu_high_rawsba"
      #                 , "cntsppIndicatorClass_TP_high_rawsba"
      #                 , "cntsppIndicatorClass_DOC_high_rawsba"
      #                 , "propsppmosttol_rawsba"
      #                 , algIndex
      #                 , "cntsppBCG3_rawhybrid"
      #                 , "propCyclotella_rawhybrid"
      #                 , "propsppOxyReqDO_10_rawhybrid"
      #                 , "propSurirella_rawhybrid"
      #                 , "propsppIndicatorClass_DOC_high_rawhybrid"
      #                 , "propsppIndicatorClass_Cu_high_rawhybrid"
      #                 , "propsppOrgNNHHONF_rawhybrid"
      #                 , "propsppIndicatorClass_TN_low_rawhybrid")

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
      data_Sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      rm(fn.Sites.Info)

      # Get sample summary data
      data_SampSummary <- read.delim(fn.SampSummary, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      data_mods        <- data_ReachMod   # Check this
      data_303d  <- data_303d       # Check this
      rm(fn.SampSummary)

      # CAST, Chem & other measured data ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, Chem"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      ## Get metadata for all measured stressors
      data_chemInfo <- read.delim(fn.cheminfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      data_chemInfo <- mutate(data_chemInfo, Analyte = StdParamName)
      colMeasInvScore = as.vector(data_chemInfo$StdParamName[data_chemInfo$DirIncStress=="Dec"])
      SSTVparms <- unique(data_chemInfo$StdParamName[data_chemInfo$SSTV==1])
      rm(fn.cheminfo)

      # Get metadata for modeled stressor data
      data_modelInfo <- read.delim(fn.modelinfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
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
                                 na.strings = "NA", stringsAsFactors = FALSE)
      analytes      <- data_stressInfo$StdParamName[data_stressInfo$UseInStressorID == 1]
      data_chemRaw <- data_chemAll[data_chemAll$StdParamName %in% analytes,]
      data_chemRaw <- data_chemRaw %>%
        mutate(SampleDate = lubridate::mdy(SampDate)) %>%
        select(StationID_Master, ChemSampleID, SampDate, StdParamName
               , ResultValue, SampleDate) %>%
        group_by(StationID_Master, ChemSampleID, SampDate, StdParamName
                 , SampleDate) %>%
        summarize(MeanResultValue = mean(ResultValue), .groups = "drop_last") %>%
        rename(ResultValue = MeanResultValue)
      data_chemRaw <- unique(data_chemRaw)
      data_outliers <- getOutliers(df_data = data_chemRaw
                                   , df_meta = data_chemInfo)
      data_chemRaw <- merge(data_chemRaw, data_outliers
                            , by.x = c("ChemSampleID", "StdParamName", "ResultValue")
                            , by.y = c("ChemSampleID", "StdParamName", "ResultValue")
                            , all.x = TRUE)
      data_chemRaw <- data_chemRaw[,c("StationID_Master", "ChemSampleID", "SampDate"
                                      , "StdParamName", "ResultValue", "SampleDate"
                                      , "IQRmethod", "SDmethod", "Outlier")]
      rm(fn.chemdata, data_chemAll)
      measParams <- as.vector(unique(data_chemRaw$StdParamName))
      algParams <- as.vector(unique(data_chemRaw$StdParamName[grepl("^AFDM|^Chlor_a|^Pheophytin"
                                                                    ,data_chemRaw$StdParamName)]))

      # Get modeled stressor data
      data_modelAll <- read.delim(fn.modeldata, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      useParams      <- data_modelInfo$StdParamName[data_modelInfo$UseInStressorID == 1]
      data_modelRaw <- data_modelAll[data_modelAll$StdParamName %in% useParams,]
      data_modelRaw <- data_modelRaw %>%
        mutate(SampYear = lubridate::year(lubridate::mdy(SampDate))
               , SampleDate =  lubridate::mdy(SampDate)) %>%
        select(StationID_Master, ChemSampleID, SampDate, StdParamName
               , ResultValue, SampleDate)
      data_modoutliers <- getOutliers(df_data = data_modelRaw
                                      , df_meta = data_modelInfo)
      data_modelRaw <- merge(data_modelRaw, data_modoutliers
                             , by.x = c("ChemSampleID", "StdParamName", "ResultValue")
                             , by.y = c("ChemSampleID", "StdParamName", "ResultValue")
                             , all.x = TRUE)
      data_modelRaw <- data_modelRaw[,c("StationID_Master", "ChemSampleID", "SampDate"
                                        , "StdParamName", "ResultValue", "SampleDate"
                                        , "IQRmethod", "SDmethod", "Outlier")]
      rm(fn.modeldata, data_modelAll)

      # Identify modeled parameters to keep or delete (per SCCWRP)
      modelParams <- as.vector(unique(data_modelRaw$StdParamName))
      bmiModelParamsKeep <- c("HighDur_Wet", "HighNum_Dry", "MaxMonthQ_Wet"
                              , "NoDisturb_Average", "Q99_Average", "QmaxIDR_All"
                              , "RBI_Dry")
      bmiModelParamsDEL <- setdiff(modelParams, bmiModelParamsKeep)
      algModelParamsKeep <- c("HighDur_Dry", "HighNum_Dry", "MaxMonthQ_Dry"
                              , "NoDisturb_Dry", "Qmax_Dry", "QmaxIDR_All")
      algModelParamsDEL <- setdiff(modelParams, algModelParamsKeep)
      algParamsDEL <- c(algModelParamsDEL, algParams)

      # Prepare df_allStress file
      data_modeltrim <- as.data.frame(data_modelRaw) %>%
        dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
                      , ResultValue, IQRmethod, SDmethod, Outlier) %>%
        dplyr::mutate(SampleDate = NA)
      data_meastrim <- as.data.frame(data_chemRaw) %>%
        dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
                      , ResultValue, IQRmethod, SDmethod, Outlier)
      data_Stress <- rbind(data_meastrim, data_modeltrim)

      # Combine measured and modeled parameters with inverse scoring
      col_StressInvScore <- c(colMeasInvScore, colModelInvScore)

      # CAST, BMI, taxonomic data ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, BMI, Taxonomic"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      data_BMIcounts <- read.table(fn.bmi.raw, header = TRUE, sep = "\t")

      data_MTbmi <- read.table(fn.MT.bmi, header = TRUE, sep = "\t",
                               stringsAsFactors = FALSE)
      # data_bmiTaxaRaw <- mutate(data_bmiTaxaRaw, BMI.Metrics.SampID = BMISampID)
      rm(fn.bmi.raw, fn.MT.bmi)

      # CAST, BMI, metrics ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, BMI, Metrics"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
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
      data_bmiMetrics <- unique(data_bmiMetrics)
      rm(fn.bmi.metrics)

      data_cscicore <- read.delim(fn.bmi.cscicore, header = TRUE, sep = "\t"
                                  , na.strings = "NA", stringsAsFactors = FALSE)
      data_cscicore <- data_cscicore[,c("stationid", "county", "smcshed", "latitude"
                                        , "longitude", "stationcode", "sampleid"
                                        , "samplemonth", "sampleday", "sampleyear"
                                        , "collectionmethodcode", "fieldreplicate"
                                        , "count", "pcnt_ambiguous_individuals")]
      data_cscicore <- data_cscicore %>%
        mutate(date_text = paste(samplemonth,sampleday,sampleyear,sep="/")
               , BMISampID = paste(stationid, date_text, collectionmethodcode
                                   , fieldreplicate, sep = "_")
               , BMISampFlag = ifelse((count<250) & (pcnt_ambiguous_individuals>50)
                                      , "Insufficient individuals and large percent ambiguity"
                                      , ifelse(count<250, "Insufficient individuals"
                                               , ifelse(pcnt_ambiguous_individuals>50
                                                        , "Large percent ambiguity"
                                                        , NA)))) %>%
        rename(StationID_Master = stationid, BMISampCount = count
               , PctAmbigInd = pcnt_ambiguous_individuals) %>%
        select(StationID_Master, BMISampID, BMISampCount, PctAmbigInd, BMISampFlag)
      data_cscicore <- unique(data_cscicore)

      data_bmiMetrics <- merge(data_bmiMetrics, data_cscicore
                               , by.x = c("StationID_Master", "BMISampID")
                               , by.y = c("StationID_Master", "BMISampID")
                               , all.x = TRUE)

      data_tmpbmicount <- unique(data_BMIcounts[,c("BMISampID","SampleTotAbund")])
      data_bmiMetrics <- data_bmiMetrics %>%
        mutate(BMISampCount = ifelse(is.na(BMISampCount)
                                     , data_tmpbmicount$SampleTotAbund
                                     , BMISampCount)) %>%
        mutate(BMISampFlag = ifelse(is.na(BMISampFlag) & (BMISampCount < 250)
                                    , "Insufficient number of individuals", BMISampFlag))
      rm(data_tmpbmicount)

      data_bmiMetrics <- data_bmiMetrics %>%
        mutate(BMISampFlag = ifelse(is.na(PctAmbigInd) & is.na(BMISampFlag)
                                    , ifelse(BMISampCount >= 250
                                             , paste0("Unknown percent ambiguous individuals")
                                             , paste0("Unknown number of and percent "
                                                      , "ambiguous individuals"))
                                    , ifelse(is.na(PctAmbigInd)
                                             , paste0("Insufficient number of and unknown "
                                                      ,"percent ambiguous individuals")
                                             , BMISampFlag)))

      # CAST, BMI, metrics metadata ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, BMI, Metrics, Metadata"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      data_bmiMetricsInfo <- read.delim(fn.bmi.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      data_bmiMetricsInfo <- data_bmiMetricsInfo[,c("MetricName",	"MetricLabel", "IndexYN")]
      bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
      bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN=="Yes"])

      # Generate co-occurrence data set (same day samples; modeled data match any day)
      data_bmiCoOccur <- getCoOccurDataset(dataDir = dir_data
                                           , df_sites = data_Sites
                                           , df_model = data_modelRaw
                                           , df_meas = data_chemRaw
                                           , biocomm = "BMI"
                                           , df_resp = data_bmiMetrics
                                           , index = bmiIndex
                                           , lagdays = lagdays
                                           , removeOutliers = boo_removeOutliers)
      # returns df_coOccur as data_bmiCoOccur
      bmiParamsKEEP <- setdiff(colnames(data_bmiCoOccur), bmiModelParamsDEL)
      data_bmiCoOccur <- dplyr::select(data_bmiCoOccur, all_of(bmiParamsKEEP))
      # 2020-04-10, add "all_of" to excise tidyverse message.
      # write.table(data_bmiCoOccur, file.path(getwd(),"Results","bmiCoOccur.tab")
      #             ,append=FALSE,col.names = TRUE, row.names = FALSE, sep = "\t")

      # CAST, Alg, metrics ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, Alg, Metrics"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      data_AlgMetrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
      data_AlgMetrics <- data_AlgMetrics %>%
        dplyr::mutate(AlgSampDate = lubridate::mdy(AlgSampDate)) %>%
        dplyr::mutate(AlgSampFlag = NA)
      rm(fn.alg.metrics)

      # CAST, Alg, metrics metadata ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, Alg, Metrics, Metadata"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      data_AlgMetricsInfo <- read.delim(fn.alg.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      algMetrics <- as.vector(data_AlgMetricsInfo$MetricName)
      algIndex <- as.character(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$IndexYN=="Yes"])

      # CAST, Alg taxonomic data ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Data, Alg, Taxonomic"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      data_AlgCounts <- read.table(fn.alg.raw, header = TRUE, sep = "\t")

      data_AlgMasterTaxa <- read.table(fn.MT.alg, header = TRUE, sep = "\t",
                                       stringsAsFactors = FALSE)
      rm(fn.alg.raw, fn.MT.alg)
      #

      # # Generate co-occurrence data set (same day samples; modeled data match any day)
      data_algCoOccur <- getCoOccurDataset(dataDir = dir_data
                                           , df_sites = data_Sites
                                           , df_model = data_modelRaw
                                           , df_meas = data_chemRaw
                                           , biocomm = "Alg"
                                           , df_resp = data_AlgMetrics
                                           , index = algIndex
                                           , lagdays = lagdays
                                           , removeOutliers = boo_removeOutliers)
      # returns df_coOccur as data_algCoOccur
      algParamsKEEP <- setdiff(colnames(data_algCoOccur), algParamsDEL)
      data_algCoOccur <- dplyr::select(data_algCoOccur, all_of(algParamsKEEP))
      # write.table(data_algCoOccur, file.path(getwd(),"Results","algCoOccur.tab")
      #             ,append=FALSE,col.names = TRUE, row.names = FALSE, sep = "\t")

      # Get cluster data
      data_cluster <- read.delim(fn.cluster, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      rm(fn.cluster)

      # Get cluster data metadata
      data_clusterInfo <- read.delim(fn.clusterinfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      rm(fn.clusterinfo)

      # Get background data (StreamCat)
      df_bkgdata <- read.table(fn.bkgdata, header = TRUE, sep = "\t"
                               , na.strings = c("","NA"))

      # Get background metadata
      df_bkginfo <- read.table(fn.bkginfo, header = TRUE, sep = "\t"
                               , na.strings = c("", "NA")
                               , stringsAsFactors = FALSE)

      if (useBC == TRUE) {
        # Get BC dissimilarity distance matrix to subset cluster sites to comparators
        data_BCdist <- read.delim(fn.bcdist, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # RUN CASTool
      # Site Selection ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Site Selection"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      #df_targets <- read_excel(fn.targets, col_names = TRUE, trim_ws = TRUE, skip = 0)

      endprep.time <- Sys.time()
      elapsedprep.time <- round(endprep.time - startprep.time, 2)
      msg <- paste("Prep completed in", elapsedprep.time)
      # print(msg)
      # flush.console()
      message(msg)


      ifelse(!dir.exists(file.path(dir_results))==TRUE
             , dir.create(file.path(dir_results))
             , FALSE)

      fn_runstats <- paste0("RunStats_", format.Date(Sys.Date(),"%Y%m%d"), ".tab")
      df_runstats <- as.data.frame(cbind("TargetSiteID", "Biocomm", "NumStressors"
                                         , "NumLoE", "ElapsedTime"))
      write.table(df_runstats, file.path(dir_results,fn_runstats), append = FALSE
                  , col.names = FALSE, row.names = FALSE, sep = "\t")

      # Main Code ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Main Code Start"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # TargetSiteID = "403S02363"
      # for (site in 1:length(TargetSiteID)) {
    #  for (site in 1:nrow(df_targets)) {
        startsite.time <- Sys.time()
     #   TargetSiteID <- df_targets$TargetSiteID[site] # already defined by Shiny interface
        # if (is.na(TargetSiteID)) {
        #   next()
        # }
        msg <- paste0("Evaluating site: ",TargetSiteID)
        message(msg)
        # print(msg)
        # flush.console()

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Biocomm-independent functions

        # Create high-level results folder structure
        dir_sub2 <- TargetSiteID
        ifelse(!dir.exists(file.path(dir_results, dir_sub2))==TRUE
               , dir.create(file.path(dir_results, dir_sub2))
               , FALSE)

        # Establish data gaps file
        gaps <- cbind.data.frame("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = FALSE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")

        # getComparators ####
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- "getComparators"
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        # Identify comparator sites
        # This is predicated on the fact that BC distance is calculated based on
        # expected benthic macroinvertebrate taxa. If there are ever different
        # BC matrices for different biocomms, then this must move into the biocomm
        # loop or it needs to be run more than once for each biocomm here, since
        # it's used in getSiteInfo immediately afterward.
        list.CompSites <- getComparators(TargetSiteID
                                         , df_sites = data_Sites
                                         , df_bioCoOccur = data_bmiCoOccur
                                         , bioIndex = bmiIndex
                                         , useBC = useBC
                                         , df_bcdist = data_BCdist
                                         , bc_cutoff = 0.05
                                         , dir_results = dir_results
                                         , dir_sub = "SiteInfo")
        # Returns: myCompSites <- list(comp.sites = comp.sites
        #                             , gap.compsites = gap.statement
        comp_sites <- list.CompSites$comp.sites
        msg <- "getComparators is complete."
        message(msg)
        # print(msg)
        # flush.console()

        # getSiteInfo ####
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- "getSiteInfo"
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
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
                                        , data_algMetrics = data_AlgMetrics
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
        msg <- "getSiteInfo is complete."
        message(msg)
        # print(msg)
        # flush.console()

        # getClusterInfo ####
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- "getClusterInfo"
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        # Get Cluster Info
        getClusterInfo(TargetSiteID
                       , siteCOMID=list.SiteSummary$COMID
                       , siteCluster=list.SiteSummary$ClustID
                       , refSiteCOMIDs=list.SiteSummary$refCOMIDs
                       , data_cluster = data_cluster
                       , data_clusterInfo = data_clusterInfo
                       , dir_results=dir_results
                       , dir_sub="ClusterInfo")
        msg <- "getClusterInfo is complete."
        message(msg)
        # print(msg)
        # flush.console()

        # Munge str/resp ####
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- "Munge, Str/Resp"
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Prepare flags for types of stressor and response data to use
        avail.data <- data_SampSummary[data_SampSummary$StationID_Master == TargetSiteID,]
        avail.data <- avail.data[,c(6:ncol(avail.data))]
        avail.data <- avail.data %>% select_if(not_all_na)
        samptypes <- names(avail.data)

        wd <- file.path(".") #2020-02-03

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
            df_allStress <- rbind(df_allStress, data_modelRaw)
          } else {
            df_allStress <- data_modelRaw
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

        if (any(samptypes == algResp)) {
          useAlg = TRUE
          gap.alg.rsp <- cbind.data.frame("general", "useALG", 1, "Algae responses available.")
          colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
        } else {
          useAlg = FALSE
          gap.alg.rsp <- cbind.data.frame("general", "useALG", 0, "No algae responses available.")
          colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
        } ### End If statement for measured stressorsalgal responses

        gaps <- rbind.data.frame(gap.chem.stress, gap.phab.stress, gap.mod.stress
                                 , gap.bmi.rsp, gap.alg.rsp)
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(wd, "Results", TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if ((useMeasStress==FALSE) & (useModStress==FALSE)) {
        #     # No stressor data available
        #     gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0, "No chemistry stressors available.")
        #     colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
        # }

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        for (b in 1:length(biocommlist)) {

          noStressors <- FALSE
          noResponses <- FALSE

          if ((useMeasStress==FALSE) & (useModStress==FALSE)) {
            # No stressor data available
            gap.stress <- cbind.data.frame("general", "Stressors", 0
                                           , "No stressor data available.")
            colnames(gap.stress) <- c("fxnname", "condition", "result"
                                      , "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gap.stress, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            noStressors <- TRUE
          }
          if ((useAlg==FALSE) & (useBMI==FALSE)) {
            # No stressor data available
            gap.resp <- cbind.data.frame("general", "Responses", 0
                                         , "No response data available.")
            colnames(gap.resp) <- c("fxnname", "condition", "result"
                                    , "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gap.resp, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            noResponses <- TRUE
          }
          if ((noStressors==TRUE) | (noResponses==TRUE)) {
            msg <- ifelse((noStressors==TRUE) & (noResponses==TRUE)
                          , paste0("No stressor or response data are available for "
                                   , TargetSiteID)
                          , ifelse(noStressors==TRUE
                                   , paste0("No stressor data are available for "
                                            , TargetSiteID)
                                   , paste0("No response data are available for "
                                            , TargetSiteID)))
            message(msg)
            # print(msg)
            # flush.console()
            next
          }

          numLoE = 0

          LoEs <- c("TS", "CO", "SR", "VP", "SSD")
          df_LoE <- as.data.frame(LoEs)
          colnames(df_LoE) <- "LoE"
          df_LoE <- df_LoE %>%
            mutate(LoE = as.character(LoE)
                   , Completed = as.integer(0)
                   , ResultsDir = as.character(NA))

          # Define biocomm data
          bioComm <- biocommlist[b]
          if ((bioComm=="bmi") && (useBMI==TRUE)) {

            data_bioCoOccur <- data_bmiCoOccur
            bioIndex <- bmiIndex
            bioIndexGp <- bmiIndexGp
            bioMetricNames <- bmiMetrics
            bioMetricData <- data_bmiMetrics
            bioMetricInfo <- data_bmiMetricsInfo
            bioTaxaData <- data_BMIcounts
            bioMasterTaxa <- data_BMIMasterTaxa
            colBio <- bmiIndex
            colBioSample <- bmiResp
            colBioSampDate <- bmiRespDate
            BioNarBrk <- bmi_thresholds
            BioNarLab <- bmi_narrative
            BioDegBrk <- bmi_deg_thres
            BioDegLab <- bmi_deg_text
            modelParams <- bmiModelParamsKeep
            bioParmsDEL <- bmiModelParamsDEL

          } else if ((bioComm=="algae") && (useAlg==TRUE)) {

            data_bioCoOccur <- data_algCoOccur
            bioIndex <- algIndex
            bioIndexGp <- algIndexGp
            bioMetricNames <- algMetrics
            bioMetricData <- data_AlgMetrics
            bioMetricInfo <- data_AlgMetricsInfo
            bioTaxaData <- data_AlgCounts
            bioMasterTaxa <- data_AlgMasterTaxa
            colBio <- algIndex
            colBioSample <- algResp
            colBioSampDate <- algRespDate
            BioNarBrk <- alg_thresholds
            BioNarLab <- alg_narrative
            BioDegBrk <- alg_deg_thres
            BioDegLab <- alg_deg_text
            modelParams <- algModelParamsKeep
            bioParmsDEL <- algParamsDEL

          } else {
            msg <- paste0(bioComm, " is not a valid biological community.")
            message(msg)
            # print(msg)
            # flush.console()
            next()
          }

          # If no paired stressor-response samples for target site, no eval possible
          if (!(TargetSiteID %in% data_bioCoOccur$StationID_Master)) {
            msg <- paste0("No paired stressor-response samples for", TargetSiteID
                          , " for the ", bioComm, " community.")
            message(msg)
            # print(msg)
            # flush.console()

            # No identified stressors may be a data gap, but may not be, either
            gapcomment <- paste0("No paired stressor-", bioComm, " samples are available "
                                 , "for ", TargetSiteID, " within ", lagdays, " days, "
                                 , "with the stressor sample being obtained prior "
                                 , "to the response sample.")
            gaps <- cbind.data.frame("getCoOccurDataset", paste0("Paired stressor-"
                                                                 , bioComm, " data"), 0, gapcomment)
            # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")

            # Write run-time stats to file
            endsite.time <- Sys.time()
            elapsedsite.time <- endsite.time - startsite.time

            df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                           , "Biocomm" = bioComm
                                           , "NumStressors" = NA
                                           , "NumLoE" = numLoE
                                           , "ElapsedTime" = elapsedsite.time))
            # if (site == 1) {
            #     df_runstats <- df_temp
            # } else {
            #     df_runstats <- rbind(df_runstats, df_temp)
            # } ### End gather run stats
            write.table(df_temp, file.path(wd,"Results",fn_runstats)
                        , append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            next()
          } ### End no stressors statement

          # getQualSites ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- paste0(bioComm, "; getQualSites")
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Run analyses
          # Identify "quality" sites using different definitions
          list.BioQualSites <- getQualSites(TargetSiteID
                                            , df_sites = data_Sites
                                            , biocomm = bioComm
                                            , df_qual = data_bioCoOccur
                                            , colBio = colBio
                                            , colBioSample = "RespSampID"
                                            , colStressSample = "StressSampID"
                                            , comp_sites = comp_sites
                                            , useBC = useBC
                                            , BioNarBrk = BioNarBrk
                                            , BioNarLab = BioNarLab
                                            , BioDegBrk = BioDegBrk
                                            , BioDegLab = c("Yes", "No")
                                            , dir_results = dir_results)
          # Returns: myQualSites <- list(dfQuality = df_qual
          #                              , allRefBioSites = all.ref
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
          msg <- paste0("getQualSites is complete for ", bioComm, ".")
          message(msg)
          # print(msg)
          # flush.console()

          # getDataSets ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- paste0(bioComm, "; getDataSets")
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Get data sets for stressors paired with response data, if available
          listPairedStressResp <- getDataSets(TargetSiteID
                                              , compSites = comp_sites
                                              , df_coOccur = data_bioCoOccur
                                              , measParams = measParams
                                              , modelParams = modelParams
                                              , biocomm = bioComm
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
          msg <- "Stressor and response data prepared, for all possible stressors."
          message(msg)
          # print(msg)
          # flush.console()

          compPairedSR <- listPairedStressResp$compBioStress %>%
            select(-StressSampDate, -RespSampDate, -RespSampID)
          sitePairedSR <- listPairedStressResp$siteBioStress %>%
            select(-StressSampDate, -RespSampDate, -RespSampID)
          sitePairedStressors <- as.vector(colnames(sitePairedSR[,3:ncol(sitePairedSR)]))

          # Prepare data sets of all stressors ever detected at the target site
          if (boo_removeOutliers == TRUE) {
            siteStressAll <- data_Stress %>%
              dplyr::filter(StationID_Master==TargetSiteID) %>%
              dplyr::filter(!is.na(ResultValue)) %>%
              dplyr::filter(Outlier != "Outlier") %>%
              tidyr::spread(key=StdParamName, value=ResultValue) %>%
              dplyr::select_if(not_all_na) %>%
              dplyr::rename(StressSampID = ChemSampleID
                            , StressSampDate = SampleDate)
            # colsKeep <- colnames(siteStressAll)[!(colnames(siteStressAll) %in%
            #                                           bioModParmsDEL)]
            # siteStressAll <- dplyr::select(siteStressAll, eval(colsKeep))
            siteDetectsAll <- as.vector(colnames(siteStressAll[,4:ncol(siteStressAll)]))
            compStressAll <- data_Stress %>%
              dplyr::filter(StationID_Master %in% comp_sites) %>%
              dplyr::filter(!is.na(ResultValue)) %>%
              dplyr::filter(Outlier != "Outlier") %>%
              dplyr::filter(StdParamName %in% siteDetectsAll) %>%
              tidyr::spread(key=StdParamName, value=ResultValue) %>%
              dplyr::rename(StressSampID = ChemSampleID
                            , StressSampDate = SampleDate)
            # compStressAll <- dplyr::select(compStressAll, eval(colsKeep))
            siteRespAll <- bioMetricData %>%
              dplyr::filter(StationID_Master == TargetSiteID) %>%
              dplyr::rename(RespSampID = eval(colBioSample)
                            , RespSampDate = eval(colBioSampDate))
          } else {
            siteStressAll <- data_Stress %>%
              dplyr::filter(StationID_Master==TargetSiteID) %>%
              dplyr::filter(!is.na(ResultValue)) %>%
              tidyr::spread(key=StdParamName, value=ResultValue) %>%
              dplyr::select_if(not_all_na) %>%
              dplyr::rename(StressSampID = ChemSampleID
                            , StressSampDate = SampleDate)
            # colsKeep <- colnames(siteStressAll)[!(colnames(siteStressAll) %in%
            #                                           bioModParmsDEL)]
            # siteStressAll <- dplyr::select(siteStressAll, eval(colsKeep))
            siteDetectsAll <- as.vector(colnames(siteStressAll[,4:ncol(siteStressAll)]))
            compStressAll <- data_Stress %>%
              dplyr::filter(StationID_Master %in% comp_sites) %>%
              dplyr::filter(!is.na(ResultValue)) %>%
              dplyr::filter(StdParamName %in% siteDetectsAll) %>%
              tidyr::spread(key=StdParamName, value=ResultValue) %>%
              dplyr::rename(StressSampID = ChemSampleID
                            , StressSampDate = SampleDate)
            # compStressAll <- dplyr::select(compStressAll, eval(colsKeep))
            siteRespAll <- bioMetricData %>%
              dplyr::filter(StationID_Master == TargetSiteID) %>%
              dplyr::rename(RespSampID = eval(colBioSample)
                            , RespSampDate = eval(colBioSampDate))
          }

          # Log removed outliers as data gaps
          siteOutliers <- df_allStress %>%
            dplyr::filter(StationID_Master==TargetSiteID) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(Outlier == "Outlier")
          compOutliers <- data_Stress %>%
            dplyr::filter(StationID_Master %in% comp_sites) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(Outlier == "Outlier")
          allOutliers <- data_Stress %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(Outlier == "Outlier")

          if (nrow(siteOutliers)>0) {
            for (r in 1:nrow(siteOutliers)) {
              stressor <- siteOutliers$StdParamName[r]
              result <- siteOutliers$ResultValue[r]
              siteID <- as.character(siteOutliers$StationID_Master[r])
              gapcomment <- paste0(siteID
                                   , " value removed as an outlier."
                                   , " Transformation applied prior to"
                                   , " identification as necessary.")
              gaps <- cbind.data.frame("Site outliers", stressor, result
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
            }
          }
          if (nrow(compOutliers)>0) {
            for (r in 1:nrow(compOutliers)) {
              stressor <- compOutliers$StdParamName[r]
              result <- compOutliers$ResultValue[r]
              siteID <- as.character(compOutliers$StationID_Master[r])
              if (siteID != TargetSiteID) {
                gapcomment <- paste0(siteID
                                     , " value removed as an outlier."
                                     , " Transformation applied prior to"
                                     , " identification as necessary.")
                gaps <- cbind.data.frame("Comparator outliers", stressor, result
                                         , gapcomment)
                colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                            , row.names = FALSE, sep = "\t")
              }
            }
          }
          if (nrow(allOutliers)>0) {
            for (r in 1:nrow(allOutliers)) {
              stressor <- allOutliers$StdParamName[r]
              result <- allOutliers$ResultValue[r]
              siteID <- as.character(allOutliers$StationID_Master)[r]
              if (!(siteID %in% comp_sites)) {
                gapcomment <- paste0("Value removed as an outlier for site "
                                     , siteID
                                     , " Transformation applied prior to"
                                     , " identification as necessary.")
                gaps <- cbind.data.frame("All data outliers", stressor, result
                                         , gapcomment)
                colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                            , row.names = FALSE, sep = "\t")
              }
            }
          }

          # getStressorList ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- paste0(bioComm, "; getStressorList")
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Get Stressor List using all stressors ever detected at the target site
          list.stressors <- getStressorList(TargetSiteID
                                            , siteCluster=list.SiteSummary$ClustID
                                            , chemInfo=data_stressInfo
                                            , clusterChem=compStressAll
                                            , siteQual2Plot=siteQual2Plot
                                            , refSamps=allBioRefStressSamps
                                            , refSites=allBioRefSites
                                            , siteChem=siteStressAll
                                            , probsHigh=probsHigh
                                            , probsLow=probsLow
                                            , DOlim=DOlim
                                            , pHlimLow=pHlimLow
                                            , pHlimHigh=pHlimHigh
                                            , biocomm=bioComm
                                            , bioParmsDEL=bioParmsDEL
                                            , dir_results=dir_results
                                            , dir_sub="CandidateCauses")
          # Returns: myStressors <- list(stressors = stressorlist
          #                     , site.stressor.pctrank = site.pctrank
          #                     , stressors_LogTransf
          #                     , stressors_Excepted)
          stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
          stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
          msg <- "getStressorList is complete."
          message(msg)
          # print(msg))
          # flush.console()

          stressorsNOpairing <- setdiff(stressors, sitePairedStressors)
          stressorsWPairedResponses <- intersect(stressors, sitePairedStressors)

          ### MODIFY siteStressAll to keep all core cols and only stressor cols

          # If no stressors are identified, no analyses can be performed. Error msg.
          if (length(stressors) == 0) {
            msg <- paste("No stressors identified for", TargetSiteID)
            message(msg)
            # print(msg)
            # flush.console()

            # No identified stressors may be a data gap, but may not be, either
            gapcomment <- paste0("No potential stressors fall outside the specified "
                                 , "quantile range (", probsLow, " to ", probsHigh,").")
            gaps <- cbind.data.frame("getStressorList", "Number of stressors", 0
                                     , gapcomment)
            colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")

            # Write run-time stats to file
            endsite.time <- Sys.time()
            elapsedsite.time <- endsite.time - startsite.time

            df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                           , "Biocomm" = bioComm
                                           , "NumStressors" = length(stressors)
                                           , "NumLoE" = numLoE
                                           , "ElapsedTime" = elapsedsite.time))
            # if (site == 1) {
            #     df_runstats <- df_temp
            # } else {
            #     df_runstats <- rbind(df_runstats, df_temp)
            # } ### End gather run stats
            write.table(df_temp, file.path(wd,"Results",fn_runstats)
                        , append = TRUE, col.names = FALSE
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

          if (length(stressorsWPairedResponses)==0) {
            NE_true <- TRUE
            # Candidate causes identified as stressors had no response sample
            # obtained within lagdays following the stressor sample collection
            gapcomment <- paste0("No identified possible stressors had a response "
                                 , "sample obtained within ", lagdays, " days of "
                                 , "stressor sample collection.")
            gaps <- cbind.data.frame("getStressorList", "Paired stresssor/responses"
                                     , 0, gapcomment)
            colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
          } else {
            NE_true <- FALSE
            stressorsUsed <- as.data.frame(stressorsWPairedResponses)
            colnames(stressorsUsed)[1] <- "Stressor"
            stressorsUsed <- merge(stressorsUsed
                                   , data_stressInfo[,c("Analyte","Label")]
                                   , by.x = "Stressor"
                                   , by.y = "Analyte"
                                   , all.x = TRUE)
            stressorsUsed <- unique(stressorsUsed)
            fn.stressorsUsed <- file.path(dir_results,TargetSiteID
                                          , toupper(bioComm)
                                          , "CandidateCauses/"
                                          , paste0(TargetSiteID, "_"
                                                   ,toupper(bioComm)
                                                   , "_CandCauses_StressorsEvaluated.tab"))
            write.table(stressorsUsed, fn.stressorsUsed, append = FALSE
                        , col.names = TRUE, row.names = FALSE, sep = "\t")
          }

          # Either all are paired or some are
          stressors_logtransf <- data_stressInfo$LogTransf[data_stressInfo$StdParamName
                                                           %in% stressorsWPairedResponses]
          # getTimeSeq ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- paste0(bioComm, "; getTimeSeq")
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Create time sequence graphics
          # Uses all site stressor and response data, but not paired
          getTimeSeq(TargetSiteID
                     , biocomm = bioComm
                     , BioResp = bioMetricNames
                     , stressors = stressorsWPairedResponses
                     , df_stress = siteStressAll   # Not just paired data
                     , df_resp = siteRespAll       # Not just paired data
                     , df_stressinfo = df_allStressInfo
                     , df_respinfo = bioMetricInfo
                     , dir_results = dir_results
                     , dir_sub = "TimeSequence")
          msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
          message(msg)
          # print(msg)
          # flush.console()

          # NOT WORKING
          dirTS <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                             , "TimeSequence")
          if (dir.exists(dirTS)==TRUE) {
            if (length(list.files(dirTS)) > 0) {
              numLoE = numLoE + 1
              df_LoE$Completed[df_LoE$LoE == "TS"] <- 1
              df_LoE$ResultsDir[df_LoE$LoE == "TS"] <- dirTS

            }
          }


          if (NE_true) { # No paired stressor response data available. Move to next biocomm or site.
            # Write run-time stats to file
            endsite.time <- Sys.time()
            elapsedsite.time <- endsite.time - startsite.time

            df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                           , "Biocomm" = bioComm
                                           , "NumStressors" = length(stressors)
                                           , "NumLoE" = numLoE
                                           , "ElapsedTime" = elapsedsite.time))
            # if (site == 1) {
            #     df_runstats <- df_temp
            # } else {
            #     df_runstats <- rbind(df_runstats, df_temp)
            # } ### End gather run stats
            write.table(df_temp, file.path(wd,"Results",fn_runstats)
                        , append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            next()
          }

          # Get Response-based co-occurrence
          if (TargetSiteID %in% unique(data_bioCoOccur$StationID_Master)) {
            msg <- "Starting Co-occurrence"
            message(msg)
            # print(msg)
            # flush.console()
            getCoOccur(df_data = data_bioCoOccur
                       , TargetSiteID = TargetSiteID
                       , col_ID = "StationID_Master"
                       , colStressSamp = "StressSampID"
                       , colRespSamp = "RespSampID"
                       , colGroup = "clust"
                       , colBio = colBio
                       , colStressors = c(stressorsWPairedResponses)
                       , df_stressinfo = data_stressInfo
                       , BioNarBrk = BioNarBrk
                       , BioNarLab = BioNarLab
                       , BioDegBrk = BioDegBrk
                       , BioDegLab = c("Yes", "No")
                       , biocomm = bioComm
                       , dir_plots = dir_results
                       , dir_sub = "CoOccurrence"
                       , col_StressInvScore = col_StressInvScore)
          } else {
            # gapcomment <- "Stressor detected but paired response not available"
            # gaps <- cbind.data.frame("getStressorList", stressorsNOpairing[s], 0
            #                          , gapcomment)
            # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            # fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            # write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
            #             , row.names = FALSE, sep = "\t")
          } ### End getCoOccur
          msg <- paste0("getCoOccur for ", bioComm, " is complete.")
          message(msg)
          # print(msg)
          # flush.console()

          dirCO <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                             , "CoOccurrence")
          if (dir.exists(dirCO)==TRUE) {
            if ((length(list.files(dirCO)) > 0)==TRUE) {
              numLoE = numLoE + 1
              df_LoE$Completed[df_LoE$LoE == "CO"] <- 1
              df_LoE$ResultsDir[df_LoE$LoE == "CO"] <- dirCO
            }
          }

          # Refine all.b.str, cl.b.str, and site.b.str for just identified stressors
          core.cols <- c("StationID_Master", "StressSampDate", "RespSampDate"
                         , "StressSampID", "RespSampID")

          all.b.str <- listPairedStressResp$allBioStress %>%
            select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
            select(StressSampID, RespSampID, StationID_Master, RespSampDate
                   , eval(stressorsWPairedResponses))
          cl.b.str <- listPairedStressResp$compBioStress %>%
            select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
            select(StressSampID, RespSampID, StationID_Master, RespSampDate
                   , eval(stressorsWPairedResponses))
          site.b.str <- listPairedStressResp$siteBioStress %>%
            select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
            select(StressSampID, RespSampID, StationID_Master, RespSampDate
                   , eval(stressorsWPairedResponses))

          all.b.rsp <- listPairedStressResp$allBioResp %>%
            select(RespSampID, StressSampID, StationID_Master, RespSampDate
                   , Quality, eval(bioMetricNames))
          cl.b.rsp <- listPairedStressResp$compBioResp %>%
            select(RespSampID, StressSampID, StationID_Master, RespSampDate
                   , Quality, eval(bioMetricNames))
          site.b.rsp <- listPairedStressResp$siteBioResp %>%
            select(RespSampID, StressSampID, StationID_Master, RespSampDate
                   , Quality, eval(bioMetricNames))

          siteStressInfo <- listPairedStressResp$siteStressInfo

          list_MatchBioData <- list("all.b.str" = all.b.str
                                    , "cl.b.str" = cl.b.str
                                    , "site.b.str" = site.b.str
                                    , "all.b.rsp" = all.b.rsp
                                    , "cl.b.rsp" = cl.b.rsp
                                    , "site.b.rsp" = site.b.rsp)

          # getBioStressorResponses ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- paste0(bioComm, "; getBioStressorResponses")
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Get Stressor Responses

          # TargetSiteID
          # stressors = stressorsWPairedResponses
          # stressorInfo = siteStressInfo
          # BioResp = bioMetricNames
          # BioInfo = bioMetricInfo
          # list.MatchBioData = list_MatchBioData
          # ref.sites = allBioRefStressSamps
          # siteQual2Plot = siteQual2Plot
          # biocomm = bioComm
          # dir_results = dir_results
          # dir_sub = "StressorResponse"
          # boo_pred_warn = TRUE


          getBioStressorResponses(TargetSiteID
                                  , stressors = stressorsWPairedResponses
                                  , stressorInfo = siteStressInfo
                                  , BioResp = bioMetricNames
                                  , BioInfo = bioMetricInfo
                                  , list.MatchBioData = list_MatchBioData
                                  , ref.sites = allBioRefStressSamps
                                  , siteQual2Plot = siteQual2Plot
                                  , biocomm = bioComm
                                  , dir_results = dir_results
                                  , dir_sub = "StressorResponse"
                                  , boo_pred_warn = TRUE)
          msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
          message(msg)
          # print(msg)
          # flush.console()

          dirSR <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                             , "StressorResponse")
          if (dir.exists(dirSR)==TRUE) {
            if (length(list.files(dirSR)) > 0) {
              numLoE = numLoE + 1
              df_LoE$Completed[df_LoE$LoE == "SR"] <- 1
              df_LoE$ResultsDir[df_LoE$LoE == "SR"] <- dirSR
            }
          }

          # getVerifiedPredictions ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- paste0(bioComm, "; getVerifiedPredictions")
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          # Get Stressor-specific regressions
          if (any(SSTVparms %in% stressorsWPairedResponses)) {
            getVerifiedPredictions(TargetSiteID
                                   , SSTVanalytes = as.character(SSTVparms)
                                   , colBioSample = colBioSample
                                   , stressors = stressorsWPairedResponses
                                   , stressorInfo <- siteStressInfo
                                   , dataBioTaxa = bioTaxaData
                                   , dataMasterTaxa = bioMasterTaxa
                                   , matchedData = list_MatchBioData
                                   , BioIndex_Val = bioIndex
                                   , BioIndex_Nar = "Quality"
                                   , BioIndex_Nar_Deg = "Degraded"
                                   , dir_results=dir_results
                                   , dir_sub="VerifiedPredictions"
                                   , biocomm=bioComm)
          } else {
            msg <- "No possible stressors have stressor-specific tolerance values."
            message(msg)
            # print(msg)
            # flush.console()
            gapcomment <- paste0("Stressors having stressor-specific tolerance "
                                 , "values are not identified at this site.")
            gaps <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0
                                     , gapcomment)
            colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
          } ### End getVP evaluation

          msg <- paste0("getVerifiedPredictions for ", bioComm, " is complete.")
          message(msg)
          # print(msg)
          # flush.console()

          dirVP <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                             , "VerifiedPredictions")
          if (dir.exists(dirVP)==TRUE) {
            if (length(list.files(dirVP)) > 0) {
              numLoE = numLoE + 1
              df_LoE$Completed[df_LoE$LoE == "VP"] <- 1
              df_LoE$ResultsDir[df_LoE$LoE == "VP"] <- dirVP
            }
          }

          # # Not enabled yet
          # # getSSDs
          # # getSSDplot(Data, ResponseType, Taxa, Exposure)
          # # myDF <- data_SSD_generator
          # # myRT   <- "ResponseType"
          # # myTaxa <- "Taxa"
          # # myExp  <- "Exposure"
          # # Run function
          # # p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)

          # getWoE ####
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          prog_det <- paste0(bioComm, "; getWoE")
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          getWoE(TargetSiteID
                 , biocomm = bioComm
                 , index = bioIndex
                 , dir_results = dir_results
                 , dfLoE = df_LoE
                 , dfQual = list.BioQualSites$dfQuality
                 , dfStr = list_MatchBioData$site.b.str
                 , dfRank = list.stressors$site.stressor.pctrank
                 , dfStressInfo = siteStressInfo
                 , df_coOccur = data_bioCoOccur
                 , BioResp = bioMetricNames)
          msg <- paste0("getWoE for ", bioComm, " is complete.")
          message(msg)
          # print(msg)
          # flush.console()

          # Write run-time stats to file
          endsite.time <- Sys.time()
          elapsedsite.time <- endsite.time - startsite.time

          df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                         , "Biocomm" = bioComm
                                         , "NumStressors" = length(stressors)
                                         , "NumLoE" = numLoE
                                         , "ElapsedTime" = elapsedsite.time))
          # if (site == 1) {
          #     df_runstats <- df_temp
          # } else {
          #     df_runstats <- rbind(df_runstats, df_temp)
          # } ### End gather run stats
          write.table(df_temp, file.path(wd,"Results",fn_runstats)
                      , append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")

        } ### End biocomm loop


        #***Fails*HERE***###
        # RMD doesn't have all the parts needed.

        # getReport ####
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        prog_det <- "getReport"
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        # Get final report (Executive Summary style)
        getReport(TargetSiteID
                  , probsHigh=probsHigh
                  , probsLow=probsLow
                  , useBMI=useBMI
                  , useAlg=useAlg
                  , useBC = useBC
                  , lagdays = lagdays
                  , removeOutliers=boo_removeOutliers
                  , dir_results=file.path(getwd(), "Results")
                  , report_type="summary"
                  , report_format="html"
                  , dir_rmd=file.path(system.file(package = "CASTfxn"), "rmd")
                  , dir_data = file.path(getwd(), "Data")
                  , bmiIndex = "CSCI"
                  , algIndex = "MMIhybrid")
                  #, dir_rmd="C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd")

        # rm(list.SiteSummary, list.data, list.stressors, list.ChemBMIData
        #    , chem.info, stressors, stressors_logtransf, data.SSTV.totabund)
        #

        dfGaps <- read.table(file.path(dir_results, TargetSiteID
                                       , paste0(TargetSiteID,"_datagaps.tab"))
                             , header = TRUE, sep="\t")
        dfGaps <- unique(dfGaps)
        write.table(dfGaps, file.path(dir_results, TargetSiteID
                                      , paste0(TargetSiteID,"_datagaps.tab"))
                    , append = FALSE, col.names = TRUE, row.names = FALSE
                    , sep = "\t")

      #} ### End TargetSite loop

      #rm(site) # error, remove 2020-08-17
      #~~~~~~~~~~~~~~~~~~~~~~~~

      # getSummaryAllSites ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "getSummaryAllSites"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      getSummaryAllSites(biocommlist = c("bmi", "algae")
                         , bmiIndex = "CSCI"
                         , algIndex = "MMIhybrid"
                         , dir_data = file.path(getwd(),"Data")
                         , dir_results = file.path(getwd(), "Results")
                         , dir_sub = "WoE"
                         , df_sites = NULL)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # end of Ann's skeleton code, 2020-02-03
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # Determine and print elapsed time
    #   end.time <- Sys.time()
    #   elapsed.time <- end.time - start.time
    #  # msg <- paste(site, "sites completed in", elapsed.time)
    # #  msg <- paste(TargetSiteID, "sites completed in", elapsed.time)
    #   message(msg)
    #   # print(msg)
    #   # flush.console()
    #   rm(TargetSiteID)  # Comment out 20200817


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      msgDetail_A <- "Base Data"
      msgDetail_B <- "Load input data"
      incProgress(1/prog_n, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)

      # CopyResults ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Copy Results"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "Results"
      msgDetail_B <- "Prepare for display"
      incProgress(1/prog_n, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      # Copy from Results to www/Results
      CopyResults(TargetSiteID)



      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Complete ####
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Complete"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # Increment the progress bar, and update the detail text.
      msgDetail_A <- "ALL"
      msgDetail_B <- "COMPLETE"
      incProgress(1/prog_n, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)
      #

      # enable download button
      shinyjs::enable("b_downloadData")


      #
    }, message = "Run ALL")##witProgress~END
  }##Run_ALL~END

  # 00RunAll ####

  # observeEvent(input$b_RunAll, {
  #   updateTabsetPanel(session, "tsp_Main", selected = "pan_console")
  #   })

  # observe({
  #   # use tabsetPanel 'id' argument to change tabs
  #   if (input$b_RunAll > 0) {
  #     updateTabsetPanel(session, "tsp_Main", selected = "pan_console")
  #   } else {
  #     updateTabsetPanel(session, "tsp_Main", selected = "pan_disclaimer")
  #   }
  # })
  #
  observeEvent(input$b_RunAll, {
    #
    # Change focus to console tab
    #updateTabsetPanel(session, "tsp_Main", selected = "pan_console")
    withCallingHandlers({
        shinyjs::html(id="text_console_ALL", html="")
        # Run function that shows console output
        Run_ALL2()
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
