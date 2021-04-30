# Server.R, CAST - SMC ####
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
  
  output$Selected_StationIDfromMap <- renderText({
    paste0("Station ID selected from Map: ", input$siteid.select)
  })

  output$fn_Map <- renderText({
    file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, "_map_leaflet.html"))
  })##fn_Map~END

  output$fe_Map <- renderText({
    paste0("Map file exists = ", file.exists(file.path(".", "Results", input$Station, "SiteInfo", paste0(input$Station, "_map_leaflet.html"))))
  })##fe_Map~END

  # HTML Output ####
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

  output$Results_html <- renderUI({
    TargetSiteID <- input$Station
    fn_html <- list.files(file.path(".", "Results", TargetSiteID)
                          , pattern = "Results_Summary"
                          , full.names = TRUE)
    fn_html_len <- length(fn_html)
    fe_html <- ifelse(fn_html_len == 0, FALSE, file.exists(fn_html))
    if(fe_html==TRUE){
      return(includeHTML(fn_html[fn_html_len]))  # in case of multiples
    } else {
      return(NULL)
    }##IF ~ fe_html ~ END
  })##Results_html ~ END

  # Zip ####
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
  # Downloadable zip of selected dataset
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
  )##downloadData ~ Results, ALL ~ END
  
  
  # Results summary report
  output$b_downloadSummary <- downloadHandler(
    
    filename = function() {
      #
      TargetSiteID <- input$Station
      fn_html <- list.files(file.path(".", "Results", TargetSiteID)
                            , pattern = "Results_Summary"
                            , full.names = TRUE)
      fn_html_len <- length(fn_html)
      # get last file
      fe_html <- ifelse(fn_html_len == 0, FALSE, file.exists(fn_html[fn_html_len]))
      #
      if(fe_html == TRUE){
        basename(fn_html[fn_html_len])
      }## IF ~ fe_html ~ END
      
    }## filename ~ END
    , content = function(fname) {
      TargetSiteID <- input$Station
      fn_html <- list.files(file.path(".", "Results", TargetSiteID)
                            , pattern = "Results_Summary"
                            , full.names = TRUE)
      fn_html_len <- length(fn_html)
      # get last file
      fe_html <- ifelse(fn_html_len == 0, FALSE, file.exists(fn_html[fn_html_len]))
      if(fe_html == TRUE){
        file.copy(fn_html[fn_html_len], fname)
      }## IF ~ fe_html ~ END

      
    }##content~END
      # URL_NoResult <- file.path(".", "www", "ShinyNoResult.html")
      # browseURL(URL_NoResult)
  )##downloadData ~ Results, Report ~ END
    
    
    
  
  

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
  
  

  Run_ALL2 <- function(){
    # Updates from Ann's "skeleton" code, 2020-08-25
    # 2020-10-30, Erik, change code below to use skeleton (copy and paste)
    #      modify the skeleton for CAST like did for RPP
    
    boo_Shiny <- TRUE
    #
    shiny::withProgress({
      #
      start.time <- Sys.time() # Added 2020-08-17 to match with line 2744 (after getSummaryAllSites)
      # Number of increments
      prog_n <- 33 + 8 # 33 for single biocom add 8 for 2nd biocomm
      prog_inc <- 1/prog_n
      prog_cnt <- 0
      mySleepTime <- 0.5
      
      
      # 00, Disable Result buttons ----
      # Not a numbered step but numbered for outline
      shinyjs::disable("b_downloadSummary")
      shinyjs::disable("b_downloadData")
      #
      # 01, Remove Zip ####
      # Progress, 01
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Remove Zip"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
      #
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
      # reduce Shiny calc time by omitting plots
      if(boo_Shiny){
        boo_plot_user <- input$usePlots
      } else {
        boo_plot_user <- FALSE
      }## IF ~ boo_Shiny ~ END
      message(paste0("usePlots = ", boo_plot_user))
      
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # **Skeleton**, Start ####
      # external/CASTool_CA.R
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      
      # 02, Set up ####
      # Progress, 02
      if(boo_Shiny == TRUE){
        prog_det <- "Set up"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      boo.debug <- FALSE
      debug.person <- "Erik"
      if(boo_Shiny == TRUE){
        gitpath <- file.path(".", "external", "R")  # used in RPPTool but not CASTool until getReport
        dir_rmd <- file.path(".", "external", "rmd")
        wd <- file.path(".")
        dir_data <- file.path(wd, "Data")
        dir_results <- file.path(wd, "Results")
        printClusterInfo <- TRUE
      } else {
        #
        # in global in shiny
        not_all_na <- function(x) {!all(is.na(x))}
        #
        # Set up required functions ### DO NOT CHANGE! #
        # library(CASTfxn)
        library(readxl)
        library(dplyr)
        library(tidyr)
        library(stringr)
        #
        if (boo.debug==TRUE & debug.person == "Ann") {
          gitpath <- "C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/R"
          dir_rmd <- "C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd"
          localdir <- "C:/Users/ann.lincoln/Documents/SEP_CAST"
          wd <- localdir
          dir_data <- file.path(localdir, "Data")
          dir_results <- file.path(localdir, "Results")
          printClusterInfo <- FALSE
          #if (boo.debug==TRUE & debug.person == "Ann") {
          source(file.path(gitpath, "getCoOccurDataset.R"))
          source(file.path(gitpath, "getTimeSeq.R"))
          source(file.path(gitpath, "getDataSets.R"))
          source(file.path(gitpath, "getComparators.R"))
          source(file.path(gitpath, "getSiteInfo.R"))
          source(file.path(gitpath, "getSiteMap.R"))
          source(file.path(gitpath, "getClusterInfo.R"))
          source(file.path(gitpath, "getStressorList.R"))
          source(file.path(gitpath, "getCoOccur.R"))
          source(file.path(gitpath, "getBioStressorResponses.R"))
          source(file.path(gitpath, "getVerifiedPredictions.R"))
          source(file.path(gitpath, "getOutliers.R"))
          source(file.path(gitpath, "getWoE.R"))
          source(file.path(gitpath, "getQualSites.R"))
          source(file.path(gitpath, "getSummaryAllSites.R"))
          source(file.path(gitpath, "getReport.R"))
          #}
        } else if (boo.debug == TRUE & debug.person == "Erik") {
          library(CASTfxn)
          #gitpath <- file.path(system.file(package = "CASTfxn"), "R")
          dir_rmd <- file.path(system.file(package = "CASTfxn"), "inst", "rmd")
          wd <- "C://Users//Erik.Leppo//OneDrive - Tetra Tech, Inc//MyDocs_OneDrive//GitHub//CASTfxn//inst//shiny-examples//CAST_SMC"
          dir_data <- file.path(wd, "Data")
          dir_results <- file.path(wd, "Results")
          printClusterInfo <- TRUE
          site <- "SMC04134"
          TargetSiteID <- site
          b <- 1
        }
        #
        #
      }## IF ~ boo_Shiny ~ END
      
      msg <- paste0("debug = ", boo.debug, ifelse(boo.debug==FALSE, "", paste0(", person = ", debug.person)))
      message(msg)
      
      startprep.time <- Sys.time()
      
      # Required user-designated options
      #wd <- "C:/Users/ann.lincoln/Documents/SEP_CAST"
      # wd <- getwd()
      # localdir <- "C:/Users/ann.lincoln/Documents/SEP_CAST"
      # dir_data <- file.path(localdir, "Data")
      # dir_results <- file.path(localdir, "Results")
      
      removeOutliers <- TRUE
      useBC     <- TRUE # Use Bray-Curtis biological dissimilarity distance matrix
      probsHigh <- 0.75
      probsLow  <- 0.25
      DOlim     <- 7
      pHlimLow  <- 6.5
      pHlimHigh <- 9
      lagdays   <- 10
      biocommlist   <- c("bmi","algae")
      siteQual2Plot <- "not degraded" # options:"reference","better than","not degraded"
      report_format <- "html"    # word, pdf are the other options
      
      
      # Specify Base Filenames # These are the files used to run the analyses
      # 03, Data Files ####
      # Progress, 03
      if(boo_Shiny == TRUE){
        prog_det <- "Load Data Files"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      # Specify Base Filenames # These are the files used to run the analyses
      fn.targets          <- file.path(dir_data,"SMCTestSites.xlsx")
      fn.Sites.Info       <- file.path(dir_data,"SMCSitesFinal.tab")
      fn.SampSummary      <- file.path(dir_data,"SMCSiteSummary.tab")
      fn.cheminfo         <- file.path(dir_data,"SMCMeasStressInfoFinal.tab")
      fn.chemdata         <- file.path(dir_data,"SMCMeasStressDataFinal.tab")
      fn.modelinfo        <- file.path(dir_data,"SMCModelStressInfoFinal.tab")
      fn.modeldata        <- file.path(dir_data,"SMCModelStressDataFinal.tab")
      fn.bmi.metrics      <- file.path(dir_data,"SMCBenthicMetricsFinal.tab")
      fn.bmi.cscicore     <- file.path(dir_data,"SMCBenthicCSCIcore.tab")
      fn.bmi.metrics.info <- file.path(dir_data,"SMCBenthicMetricsInfo.tab")
      fn.bmi.raw          <- file.path(dir_data, "SMCBenthicCountsFinal.tab")
      fn.MT.bmi           <- file.path(dir_data, "SMCBenthicMasterTaxa.tab")
      fn.alg.metrics      <- file.path(dir_data, "SMCAlgaeMetricsFinal.tab")
      fn.alg.metrics.info <- file.path(dir_data, "SMCAlgaeMetricsInfo.tab")
      fn.alg.raw          <- file.path(dir_data, "SMCAlgaeCountsFinal.tab")
      fn.MT.alg           <- file.path(dir_data, "SMCAlgaeMasterTaxa.tab")
      fn.bcdist           <- file.path(dir_data, "SMCBCDist.tab")
      fn.cluster          <- file.path(dir_data, "SMCClusterData.tab")
      fn.clusterinfo      <- file.path(dir_data,"SMCClusterInfo.tab")
      fn.bkgdata          <- file.path(dir_data, "SMCSiteBkgdData.tab")
      fn.bkginfo          <- file.path(dir_data, "SMCSiteBkgdInfo.tab")
      
      # Load GIS files
      message("Loading GIS files.")
      # outline <- rgdal::readOGR(dsn = file.path(dir_data,"SMCBoundary"), layer = "SMCBoundary_aea")
      # flowline <- rgdal::readOGR(dsn = file.path(dir_data,"SMCReaches"), layer = "SMCReaches_aea")
      dsn_outline         <- file.path(dir_data,"SMCBoundary")
      lyr_outline         <- "SMCBoundary"
      dsn_flowline        <- file.path(dir_data,"SMCReaches")
      lyr_flowline        <- "SMCReaches"
      # 2020-09-09, use RDA saved version
      outline  <- poly.smc.proj
      flowline <- lines.flowline.proj
      
      # Get boundary file for desired region 
      sp_outline <- sf::read_sf(dsn = file.path(dsn_outline)
                                , layer = lyr_outline) %>%
        sf::st_transform(crs=4326) # EPSG identifier for WGS84
      
      # Get all flowlines
      sp_flowline <- sf::read_sf(dsn=file.path(dsn_flowline)
                                 , layer=lyr_flowline) %>%
        sf::st_transform(crs=4326) %>%
        sf::st_zm(drop=TRUE, what="ZM")
      
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
      
      # Algal responses
      alg_thresholds <- c(-2, 0.82, 2)
      alg_narrative  <- c("Degraded", "Not Degraded")
      alg_deg_thres  <- c(-2, 0.82, 2)
      alg_deg_text   <- c("Yes", "No")
      algIndexGp     <- c("MMIhybrid", "MMIdiatom", "MMIsba")
      algResp        <- "AlgSampID"
      algRespDate    <- "AlgSampDate"
      
      # USGS aea for SoCal is below
      socal.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
                +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
                +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      # aea used for AZ is below
      # az.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
      #             +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
      my.aea <- socal.aea
      
      #~~~~~~~~~~~~~~~~~~~~~~~
      # Read datafiles
      ## Get site location info and other metadata (e.g., waterbody name)
      data_Sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      rm(fn.Sites.Info)
      
      # Get sample summary data (No ReachMod file or 303d file available 20200827)
      data_SampSummary <- read.delim(fn.SampSummary, header = TRUE, sep = "\t")
      # data_mods        <- data_ReachMod   # Check this
      # data_303d        <- data_303d       # Check this
      rm(fn.SampSummary)
      
      # 04, CAST, Chem & other measured data ####
      # Progress, 04
      if(boo_Shiny == TRUE){
        prog_det <- "Data, Chem"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      ## Get metadata for all measured stressors
      data_chemInfo   <- read.delim(fn.cheminfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      data_chemInfo   <- mutate(data_chemInfo, Analyte = StdParamName)
      colMeasInvScore <- as.vector(data_chemInfo$StdParamName[data_chemInfo$DirIncStress == "Dec"])
      SSTVparms       <- unique(data_chemInfo$StdParamName[data_chemInfo$SSTV == 1])
      rm(fn.cheminfo)
      
      # Get metadata for modeled stressor data
      data_modelInfo   <- read.delim(fn.modelinfo, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      data_modelInfo   <- mutate(data_modelInfo, Analyte = StdParamName)
      colModelInvScore <- as.vector(data_modelInfo$StdParamName[data_modelInfo$DirIncStress == "Dec"])
      rm(fn.modelinfo)
      
      # Combine metadata for all stressor into one datafile
      chemMetaNames  <- colnames(data_chemInfo)
      modelMetaNames <- colnames(data_modelInfo)
      extraNames     <- chemMetaNames[!(chemMetaNames %in% modelMetaNames)]
      for (e in 1:length(extraNames)) {
        newCol <- extraNames[e]
        data_modelInfo[[newCol]] <- NA
      }
      data_modelInfo  <- data_modelInfo[,chemMetaNames]
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
        summarize(MeanResultValue = mean(ResultValue), .groups="drop_last") %>%
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
      algParams  <- as.vector(unique(data_chemRaw$StdParamName[grepl("^AFDM|^Chlor_a|^Pheophytin"
                                                                     ,data_chemRaw$StdParamName)]))
      # Get modeled stressor data
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
      
      # Identify modeled parameters to keep or delete (per SCCWRP)
      modelParams        <- as.vector(unique(data_modelRaw$StdParamName))
      bmiModelParamsKeep <- c("HighDur_Wet", "HighNum_Dry", "MaxMonthQ_Wet"
                              , "NoDisturb_Average", "Q99_Average", "QmaxIDR_All"
                              , "RBI_Dry")
      bmiModelParamsDEL  <- setdiff(modelParams, bmiModelParamsKeep)
      algModelParamsKeep <- c("HighDur_Dry", "HighNum_Dry", "MaxMonthQ_Dry"
                              , "NoDisturb_Dry", "Qmax_Dry", "QmaxIDR_All")
      algModelParamsDEL  <- setdiff(modelParams, algModelParamsKeep)
      algParamsDEL       <- c(algModelParamsDEL, algParams)
      
      # Prepare df_allStress file (write for RPP use)
      data_modeltrim <- as.data.frame(data_modelRaw) %>%
        dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
                      , ResultValue, IQRmethod, SDmethod, Outlier) %>%
        dplyr::mutate(SampleDate = NA)
      data_meastrim <- as.data.frame(data_chemRaw) %>%
        dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
                      , ResultValue, IQRmethod, SDmethod, Outlier)
      data_Stress <- rbind(data_meastrim, data_modeltrim)
      
      # Write stressor data and metadata for use in RPPTool
      fn.stress4RPP <- file.path(dir_data, "SMC_AllStressData.tab")
      fn.stressmeta4RPP <- file.path(dir_data, "SMC_AllStressInfo.tab")
      write.table(data_Stress, fn.stress4RPP, append = FALSE, col.names = TRUE
                  , row.names = FALSE, sep = "\t")
      write.table(data_stressInfo, fn.stressmeta4RPP, append = FALSE, col.names = TRUE
                  , row.names = FALSE, sep = "\t")
      
      # Combine measured and modeled parameters with inverse scoring
      col_StressInvScore <- c(colMeasInvScore, colModelInvScore)
      
      # 05, CAST, BMI, taxonomic data ####
      # Progress, 05
      if(boo_Shiny == TRUE){
        prog_det <- "Data, BMI, Taxonomic"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      data_BMIcounts <- read.table(fn.bmi.raw, header = TRUE, sep = "\t")
      #, stringsAsFactors = FALSE)
      data_BMIMasterTaxa <- read.table(fn.MT.bmi, header = TRUE, sep = "\t"
                                       , stringsAsFactors = FALSE)
      # data_bmiTaxaRaw <- mutate(data_bmiTaxaRaw, BMI.Metrics.SampID = BMISampID)
      rm(fn.bmi.raw, fn.MT.bmi)
      
      # 06, CAST, BMI, metrics ####
      # Progress, 06
      if(boo_Shiny == TRUE){
        prog_det <- "Data, BMI, Metrics"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
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
      
      # 07, CAST, BMI, metrics metadata ####
      # Progress, 07
      if(boo_Shiny == TRUE){
        prog_det <- "Data, BMI, Metrics, Metadata"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      # 
      message("Read fn.bmi.metrics.info")
      data_bmiMetricsInfo <- read.delim(fn.bmi.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      message("Select data")
      data_bmiMetricsInfo <- data_bmiMetricsInfo[,c("MetricName",	"MetricLabel", "IndexYN")]
      bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
      bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN=="Yes"])
      
      # 08, CAST, BMI, Co-Occurence ####
      # Progress, 08
      if(boo_Shiny == TRUE){
        prog_det <- "Data, BMI, Co-Occurrence"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
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
      message("BMI, Params Keep")
      bmiParamsKEEP <- setdiff(colnames(data_bmiCoOccur), bmiModelParamsDEL)
      message("BMI, Select Params Keep")
      data_bmiCoOccur <- dplyr::select(data_bmiCoOccur, all_of(bmiParamsKEEP))
      
      # 09, CAST, Alg, metrics metadata ####
      # Progress, 09
      if(boo_Shiny == TRUE){
        prog_det <- "Data, Alg, Metrics, Metadata"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      data_AlgMetricsInfo <- read.delim(fn.alg.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      algMetrics <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN==1])
      algMetricsDiscard <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN==0])
      algIndex<- as.character(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$IndexYN=="Yes"])
      
      # 10, CAST, Alg, metrics ####
      # Progress, 10
      if(boo_Shiny == TRUE){
        prog_det <- "Data, Alg, Metrics"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      data_AlgMetrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
      data_AlgMetrics <- data_AlgMetrics %>%
        mutate(AlgSampDate = lubridate::mdy(AlgSampDate)) %>%
        mutate(AlgSampFlag = NA)
      data_AlgMetrics <- dplyr::select(data_AlgMetrics, -all_of(algMetricsDiscard))
      rm(fn.alg.metrics)
      
      # 11, CAST, Alg taxonomic data ####
      # Progress, 11
      if(boo_Shiny == TRUE){
        prog_det <- "Data, Alg, Taxonomic"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      # 
      message("Read fn.alg.raw")
      data_AlgCounts <- read.table(fn.alg.raw, header = TRUE, sep = "\t")
      message("Read fn.MT.alg")
      data_AlgMasterTaxa <- read.table(fn.MT.alg, header = TRUE, sep = "\t",
                                       stringsAsFactors = FALSE)
      rm(fn.alg.raw, fn.MT.alg)
      
      # 12, CAST, Alg, Co-Occurence ####
      # Progress, 12
      if(boo_Shiny == TRUE){
        prog_det <- "Data, Alg, Co-Occurrence"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      # Generate co-occurrence data set (same day samples; modeled data match any day)
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
      algParamsKEEP   <- setdiff(colnames(data_algCoOccur), algParamsDEL)
      data_algCoOccur <- dplyr::select(data_algCoOccur, all_of(algParamsKEEP))
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
      }## IF ~ useBC ~ END
      
      # RUN CASTool
      # 13, Site Selection ####
      # Progress, 13
      if(boo_Shiny == TRUE){
        prog_det <- "Site Selection"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      df_targets <- read_excel(fn.targets, col_names = TRUE, trim_ws = TRUE, skip = 0)
      
      endprep.time <- Sys.time()
      elapsedprep.time <- endprep.time - startprep.time
      msg <- paste("Prep completed in", elapsedprep.time)
      message(msg)
      
      ifelse(!dir.exists(file.path(dir_results))==TRUE
             , dir.create(file.path(dir_results))
             , FALSE)
      
      fn_runstats <- paste0("RunStats_", format.Date(Sys.Date(),"%Y%m%d"), ".tab")
      df_runstats <- as.data.frame(cbind("TargetSiteID", "Biocomm", "NumStressors"
                                         , "NumLoE", "ElapsedTime"))
      write.table(df_runstats, file.path(dir_results,fn_runstats), append = FALSE
                  , col.names = FALSE, row.names = FALSE, sep = "\t")
      
      ### Evaluate each target site
      ## Use this for debugging, and don't run the loop
      if (boo_Shiny == TRUE){
        df_targets <- data.frame("TargetSiteID" = input$Station, "Chosen by" = NA, "Comment" = NA)
        names(df_targets)[2] <- "Chosen by"
      } else if (boo.debug==TRUE & debug.person=="Ann") {
        df_targets <- df_targets[df_targets$TargetSiteID == "SMC04134", ]
      }
      
      # 14, Main Code ####
      # Progress, 14
      if(boo_Shiny == TRUE){
        prog_det <- "Main Code Start"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      # TargetSiteID = "403S02363"
      # for (site in 1:length(TargetSiteID)) {
      #  for (site in 1:nrow(df_targets))
      # FOR ~ site ~ START ####
      for (site in 1:nrow(df_targets)) {
        startsite.time <- Sys.time()
        TargetSiteID <- df_targets$TargetSiteID[site]
        
        if (is.na(TargetSiteID)) {
          next()   
        }
        msg <- paste0("Evaluating site: ",TargetSiteID)
        message(msg)
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Biocomm-independent functions
        
        # Create high-level results folder structure
        dir_sub2 <- TargetSiteID
        ifelse(!dir.exists(file.path(dir_results, dir_sub2))==TRUE
               , dir.create(file.path(dir_results, dir_sub2))
               , FALSE)
        
        # Establish data gaps file
        gaps <- cbind.data.frame("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = FALSE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        
        
        # 15, getComparators ####
        # Progress, 15
        if(boo_Shiny == TRUE){
          prog_det <- "getComparators"
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          message(paste(prog_msg, prog_det, sep = "; "))
        }## IF ~ boo_Shiny ~ END
        #
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
        
        # 16, getSiteInfo ####
        # Progress, 16
        if(boo_Shiny == TRUE){
          prog_det <- "getSiteInfo"
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          message(paste(prog_msg, prog_det, sep = "; "))
        }## IF ~ boo_Shiny ~ END
        # Get site information for general use (map, sample summary, etc)
        
        # Map plots only ref sites, and that's probably for the best
        list.SiteSummary <- getSiteInfo(TargetSiteID = TargetSiteID
                                        , data_Sites = data_Sites
                                        , data_bkgdata = df_bkgdata
                                        , data_bkginfo = df_bkginfo
                                        , data_SampSummary = data_SampSummary
                                        , data_303d = NULL
                                        , data_bmiMetrics = data_bmiMetrics
                                        , bmiIndexGp = bmiIndexGp
                                        , data_algMetrics = data_AlgMetrics
                                        , algIndexGp = algIndexGp
                                        , comp_sites = comp_sites
                                        , data_cluster = data_cluster
                                        , data_mods = NULL
                                        # , map_proj = my.aea
                                        # , map_outline = sp_outline
                                        # , map_flowline = sp_flowline
                                        # , map_flowline2 = NULL
                                        , dir_photo = file.path(dir_data,"Photos")
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
        getSiteMap(sp_outline = sp_outline, sp_flowline=sp_flowline
                   , allSites = data_Sites, TargetSite = TargetSiteID
                   , dir_results = dir_results, dir_sub = "SiteInfo"
                   , dir_map_rmd = dir_rmd)
        # Prints static and leaflet maps (.png and .html)
        msg <- "getSiteInfo is complete."
        message(msg)
        
        # 17, Get Cluster Info ####
        # Progress, 17
        if(boo_Shiny == TRUE){
          prog_det <- "getClusterInfo"
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          message(paste(prog_msg, prog_det, sep = "; "))
        }## IF ~ boo_Shiny ~ END
        #
        # Get Cluster Info
        if (printClusterInfo==TRUE) {
          getClusterInfo(TargetSiteID
                         , siteCOMID=list.SiteSummary$COMID
                         , siteCluster=list.SiteSummary$ClustID
                         , refSiteCOMIDs=list.SiteSummary$refCOMIDs
                         , data_cluster = data_cluster
                         , data_clusterInfo = data_clusterInfo
                         , dir_results=dir_results
                         , dir_sub="ClusterInfo"
                         , boo_plot <- boo_plot_user)
          msg <- "getClusterInfo is complete."
          message(msg)
        }## IF ~ printClusterInfo ~ END
        
        # 18, Munge str/resp ####
        # Progress, 18
        if(boo_Shiny == TRUE){
          prog_det <- "Munge, Str/Resp"
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          message(paste(prog_msg, prog_det, sep = "; "))
        }## IF ~ boo_Shiny ~ END
        #
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Prepare flags for types of stressor and response data to use
        avail.data <- data_SampSummary[data_SampSummary$StationID_Master == TargetSiteID,]
        avail.data <- avail.data[,c(6:ncol(avail.data))]
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
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if ((useMeasStress==FALSE) & (useModStress==FALSE)) {
        #     # No stressor data available
        #     gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0, "No chemistry stressors available.")
        #     colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
        # }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
        
        
        # FOR ~ b ~ START ####
        if (boo.debug == TRUE & debug.person == "Erik"){
          # 1 = bmi, 2 = alg 
          biocommlist <- "alg"
        }
        
        
        for (b in 1:length(biocommlist)) {
          
          noStressors <- FALSE
          noResponses <- FALSE
          
          NE_true <- FALSE
          
          if ((useMeasStress==FALSE) & (useModStress==FALSE)) {
            # No stressor data available
            gap.stress <- cbind.data.frame("general", "Stressors", 0
                                           , "No stressor data available.")
            colnames(gap.stress) <- c("fxnname", "condition", "result"
                                      , "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
            fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
            next()
          } 
          
          # If no paired stressor-response samples for target site, no eval possible
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
          
          # If no paired stressors, write to data gaps file
          if (noStressors==TRUE) {
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
            fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
            write.table(df_temp, file.path(dir_results,fn_runstats)
                        , append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            
            rm(dfTarget)
            next()
          } ### End no stressors statement
          
          # 19, getQualSites ####
          # Progress, 19
          if(boo_Shiny == TRUE){
            prog_det <- paste0(bioComm, "; getQualSites")
            prog_cnt <- prog_cnt + 1
            prog_msg <- paste0("Step ", prog_cnt)
            incProgress(prog_inc, message = prog_msg, detail = prog_det)
            Sys.sleep(mySleepTime)
            message(paste(prog_msg, prog_det, sep = "; "))
          }## IF ~ boo_Shiny ~ END
          #
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
          
          # 20, getDataSets ####
          # Progress, 20
          if(boo_Shiny == TRUE){
            prog_det <- paste0(bioComm, "; getDataSets")
            prog_cnt <- prog_cnt + 1
            prog_msg <- paste0("Step ", prog_cnt)
            incProgress(prog_inc, message = prog_msg, detail = prog_det)
            Sys.sleep(mySleepTime)
            message(paste(prog_msg, prog_det, sep = "; "))
          }## IF ~ boo_Shiny ~ END
          #
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
          
          compPairedSR <- listPairedStressResp$compBioStress %>%
            select(-StressSampDate, -RespSampDate, -RespSampID)
          sitePairedSR <- listPairedStressResp$siteBioStress %>%
            select(-StressSampDate, -RespSampDate, -RespSampID)
          sitePairedStressors <- as.vector(colnames(sitePairedSR[,3:ncol(sitePairedSR)]))
          message("paired")
          
          
          
          
          
          # Prepare data sets of all stressors ever detected at the target site
          if (removeOutliers == TRUE) {
            siteStressAll <- data_Stress %>%
              dplyr::filter(StationID_Master==TargetSiteID) %>%
              dplyr::filter(!is.na(ResultValue)) %>%
              dplyr::filter(Outlier != "Outlier") %>%
              tidyr::spread(key=StdParamName, value=ResultValue) %>%
              dplyr::rename(StressSampID = ChemSampleID
                            , StressSampDate = SampleDate)
            if (ncol(siteStressAll)>7) {
              siteStressAllCore <- siteStressAll[1:6]
              siteStressAllParms <- siteStressAll[,7:ncol(siteStressAll)] %>%
                dplyr::select_if(not_all_na)
              siteStressAll <- cbind(siteStressAllCore, siteStressAllParms)
              rm(siteStressAllCore, siteStressAllParms)
            }
            
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
              dplyr::filter(Outlier != "Outlier") %>%
              tidyr::spread(key=StdParamName, value=ResultValue) %>%
              dplyr::rename(StressSampID = ChemSampleID
                            , StressSampDate = SampleDate)
            siteStressAll <- dplyr::select_if(siteStressAll
                                              , not_all_na(siteStressAll[7:ncol(siteStressAll)]))
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
          message("remove Outliers")
          
          # Log removed outliers as data gaps
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
          message("Log removed outliers as data gaps.")
          
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
              fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
              write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                          , row.names = FALSE, sep = "\t")
            }
          }## IF ~ siteOutliers ~ END
          message("site outliers")
          
          message(paste0("comp outliers, n = ", nrow(compOutliers)))
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
                fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                            , row.names = FALSE, sep = "\t")
              }
            }
          }## IF ~ compOutliers ~ END
          message("comp outliers")
          
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
                fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                            , row.names = FALSE, sep = "\t")
              }
            }
          }## IF ~ allOutliers ~ END
          message("all outliers")
          
          # 21, getStressorList ####
          # Progress, 21
          if(boo_Shiny == TRUE){
            prog_det <- paste0(bioComm, "; getStressorList")
            prog_cnt <- prog_cnt + 1
            prog_msg <- paste0("Step ", prog_cnt)
            incProgress(prog_inc, message = prog_msg, detail = prog_det)
            Sys.sleep(mySleepTime)
            message(paste(prog_msg, prog_det, sep = "; "))
          }## IF ~ boo_Shiny ~ END
          #
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
                                            , dir_sub="CandidateCauses"
                                            , boo_plot <- boo_plot_user)
          # Returns: myStressors <- list(stressors = stressorlist
          #                     , site.stressor.pctrank = site.pctrank
          #                     , stressors_LogTransf
          #                     , stressors_Excepted)
          stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
          stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
          msg <- "getStressorList is complete."
          message(msg)
          
          stressorsNOpairing <- setdiff(stressors, sitePairedStressors)
          stressorsWPairedResponses <- intersect(stressors, sitePairedStressors)
          
          ### MODIFY siteStressAll to keep all core cols and only stressor cols
          
          # If no stressors are identified, no analyses can be performed. Error msg.
          if (length(stressors) == 0) {
            msg <- paste("No stressors identified for", TargetSiteID)
            message(msg)
            
            # No identified stressors may be a data gap, but may not be, either
            gapcomment <- paste0("No potential stressors fall outside the specified "
                                 , "quantile range (", probsLow, " to ", probsHigh,").")
            gaps <- cbind.data.frame("getStressorList", "Number of stressors", 0
                                     , gapcomment)
            colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
            write.table(df_temp, file.path(dir_results,fn_runstats)
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
              fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
            fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
          } # End paired stressors statement
          
          # Either all are paired or some are
          stressors_logtransf <- data_stressInfo$LogTransf[data_stressInfo$StdParamName 
                                                           %in% stressorsWPairedResponses]
          
          # Continue evaluation if data are available
          if (NE_true) { # No paired stressor response data available. Move to next biocomm or site.
            # Write run-time stats to file
            endsite.time <- Sys.time()
            elapsedsite.time <- endsite.time - startsite.time
            
            df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                           , "Biocomm" = bioComm
                                           , "NumStressors" = length(stressors)
                                           , "NumLoE" = numLoE
                                           , "ElapsedTime" = elapsedsite.time))        
            write.table(df_temp, file.path(dir_results,fn_runstats)
                        , append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            
            
          } else {
            
            # 22, getTimeSeq ####
            # Progress, 22
            if(boo_Shiny == TRUE){
              prog_det <- "getTimeSeq"
              prog_cnt <- prog_cnt + 1
              prog_msg <- paste0("Step ", prog_cnt)
              incProgress(prog_inc, message = prog_msg, detail = prog_det)
              message(paste(prog_msg, prog_det, sep = "; "))
            }## IF ~ boo_Shiny ~ END
            #
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
                       , dir_sub = "TimeSequence"
                       , boo_plot = boo_plot_user)
            msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
            message(msg)
            
            dirTS <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                               , "TimeSequence")
            if (dir.exists(dirTS)==TRUE) {
              if (length(list.files(dirTS)) > 0) {
                numLoE = numLoE + 1
                df_LoE$Completed[df_LoE$LoE == "TS"] <- 1
                df_LoE$ResultsDir[df_LoE$LoE == "TS"] <- dirTS
              }## IF ~ length ~ END
            }## IF ~ dir.exists(dirTS) ~ END
            
            # 23, getCoOccur ####
            # Progress, 23
            if(boo_Shiny == TRUE){
              prog_det <- "getCoOccur"
              prog_cnt <- prog_cnt + 1
              prog_msg <- paste0("Step ", prog_cnt)
              incProgress(prog_inc, message = prog_msg, detail = prog_det)
              Sys.sleep(mySleepTime)
              message(paste(prog_msg, prog_det, sep = "; "))
            }## IF ~ boo_Shiny ~ END
            #
            # Get Response-based co-occurrence
            if (TargetSiteID %in% unique(data_bioCoOccur$StationID_Master)) {
              msg <- "Starting Co-occurrence"
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
                         , col_StressInvScore = col_StressInvScore
                         , boo_plot <- boo_plot_user)
            } else {
              # gapcomment <- "Stressor detected but paired response not available"
              # gaps <- cbind.data.frame("getStressorList", stressorsNOpairing[s], 0
              #                          , gapcomment)
              # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              # fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
            
            list_MatchBioData <- list("all.b.str"    = all.b.str
                                      , "cl.b.str"   = cl.b.str
                                      , "site.b.str" = site.b.str
                                      , "all.b.rsp"  = all.b.rsp
                                      , "cl.b.rsp"   = cl.b.rsp
                                      , "site.b.rsp" = site.b.rsp)
            
            # 24, getBioStressorResponses ####
            # Progress, 24
            if(boo_Shiny == TRUE){
              prog_det <- "getBioStressorResponses"
              prog_cnt <- prog_cnt + 1
              prog_msg <- paste0("Step ", prog_cnt)
              incProgress(prog_inc, message = prog_msg, detail = prog_det)
              Sys.sleep(mySleepTime)
              message(paste(prog_msg, prog_det, sep = "; "))
            }## IF ~ boo_Shiny ~ END
            #
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
                                    , dir_sub = "StressorResponse"
                                    , boo_plot <- boo_plot_user)
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
            
            # 25, getVerifiedPredictions ####
            # Progress, 25
            if(boo_Shiny == TRUE){
              prog_det <- "getVerifiedPredictions"
              prog_cnt <- prog_cnt + 1
              prog_msg <- paste0("Step ", prog_cnt)
              incProgress(prog_inc, message = prog_msg, detail = prog_det)
              Sys.sleep(mySleepTime)
              message(paste(prog_msg, prog_det, sep = "; "))
            }## IF ~ boo_Shiny ~ END
            #
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
                                     , biocomm=bioComm
                                     , boo_plot <- boo_plot_user)
            } else {
              msg <- "No possible stressors have stressor-specific tolerance values."
              message(msg)
              gapcomment <- paste0("Stressors having stressor-specific tolerance "
                                   , "values are not identified at this site.")
              gaps <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0
                                       , gapcomment)
              colnames(gaps) <- c("fxnname", "condition", "result", "comment")
              fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
              fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
            
            # 26, getWOE ####
            # Progress, 26
            if(boo_Shiny == TRUE){
              prog_det <- "getWOE"
              prog_cnt <- prog_cnt + 1
              prog_msg <- paste0("Step ", prog_cnt)
              incProgress(prog_inc, message = prog_msg, detail = prog_det)
              Sys.sleep(mySleepTime)
              message(paste(prog_msg, prog_det, sep = "; "))
            }## IF ~ boo_Shiny ~ END
            #
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
          endsite.time <- Sys.time()
          elapsedsite.time <- endsite.time - startsite.time
          
          df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                         , "Biocomm" = bioComm
                                         , "NumStressors" = length(stressors)
                                         , "NumLoE" = numLoE
                                         , "ElapsedTime" = elapsedsite.time))
          
          # if (site == 1) {
          #   df_runstats <- df_temp
          # } else {
          #   df_runstats <- rbind(df_runstats, df_temp)
          # } ### End gather run stats
          # Shiny mod (always 1)
          #df_runstats <- df_temp  # Shiny
          write.table(df_temp, file.path(dir_results,fn_runstats)
                      , append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          
        } ### End biocomm loop 
        # FOR ~ b ~ END ####
        
        # 27, getReport ####
        # Progress, 27
        if(boo_Shiny == TRUE){
          prog_det <- "getReport"
          prog_cnt <- prog_cnt + 1
          prog_msg <- paste0("Step ", prog_cnt)
          incProgress(prog_inc, message = prog_msg, detail = prog_det)
          Sys.sleep(mySleepTime)
          message(paste(prog_msg, prog_det, sep = "; "))
        }## IF ~ boo_Shiny ~ END
        #  
        # Shiny add ons
        dir_data_abs    <- normalizePath(dir_data)
        dir_results_abs <- normalizePath(dir_results)
        dir_rmd         <- normalizePath(dir_rmd)
        report_type     <- "summary"
        strFile_RMD     <- file.path(dir_rmd, paste0("Report_Results_", report_type, ".rmd"))
        message(paste0("file = ", strFile_RMD))
        message(paste0("exists = ", file.exists(strFile_RMD)))
        #
        # test
        # probsHigh      <- 0.75
        # probsLow       <- 0.25
        # useBMI         <- TRUE
        # useAlg         <- TRUE
        # useBC          <- TRUE
        # removeOutliers <- TRUE
        # lagdays        <- 10
        # bmiIndex       <- "CSCI"
        # algIndex       <- "MMIhybrid"
        # siteQual2Plot  <- "not degraded"
        
        # Get final report (Executive Summary style)
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
                  , dir_rmd=dir_rmd
        )
        
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
        
      } ### End TargetSite loop # not used in Shiny 
      # FOR ~ site ~ END ####   
      
      rm(site)
      
      # 28, getSummaryAllSites ####
      # Progress, 28
      if(boo_Shiny == TRUE){
        prog_det <- "getSummaryAllSites"
        prog_cnt <- prog_cnt + 1
        prog_msg <- paste0("Step ", prog_cnt)
        incProgress(prog_inc, message = prog_msg, detail = prog_det)
        Sys.sleep(mySleepTime)
        message(paste(prog_msg, prog_det, sep = "; "))
      }## IF ~ boo_Shiny ~ END
      #
      # getSummaryAllSites
      getSummaryAllSites(biocommlist = c("bmi", "algae")
                         , bmiIndex = "CSCI"
                         , algIndex = "MMIhybrid"
                         , dir_data = dir_data
                         , dir_results = dir_results
                         , dir_sub = "WoE"
                         , df_sites = NULL)
      
      msg <- "getSummaryAllSites is complete."
      message(msg)
      
      # rm(list=ls())
       
        
        
      
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # **Skeleton**, END ####
      # external/RPPTool_CA.R
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      
      
      
      

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      msgDetail_A <- "Base Data"
      msgDetail_B <- "Load input data"
      incProgress(1/prog_n, detail = paste0(msgDetail_A, "; ", msgDetail_B))
      Sys.sleep(mySleepTime)

      # 29, CopyResults ####
      # Progress, 29
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
      
      # 30, CopyInputs ####
      # Progress, 30
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
      # Copy over CASTool and RPPTool inputs
      ## results_CAST (first so don't recopy data input files)
      dir_results_CAST <- file.path(dir_results, TargetSiteID, "_results_CASTool")
      if(!dir.exists(dir_results_CAST)){
        dir.create(dir_results_CAST)
      }## IF ~ !dir.exists(dir_results_CAST) ~ END
      fn_results_from <- list.files(file.path(dir_results, TargetSiteID), "\\.tab$", recursive = TRUE, full.names = TRUE)
      file.copy(from = fn_results_from
                , to = file.path(dir_results_CAST, basename(fn_results_from)))
      ## data_CAST (2nd so don't get caught in above)
      dir_data_CAST <- file.path(dir_results, TargetSiteID, "_data_CASTool")
      if(!dir.exists(dir_data_CAST)){
        dir.create(dir_data_CAST)
      }## IF ~ !dir.exists(dir_data_CAST) ~ END
      fn_input_CASTool <- c("SMC_AllStressData.tab"
                            , "SMC_AllStressInfo.tab"
                            , "SMCBenthicMetricsFinal.tab"
                            , "SMCSitesFinal.tab")
      file.copy(from = file.path(dir_data, fn_input_CASTool)
                , to = file.path(dir_data_CAST, fn_input_CASTool))
      
     
      
      # 31, Create zip ####
      # Progress, 31
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Create Zip Download"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      #
      fn_zip <- file.path(".", "Results", paste0(TargetSiteID, ".zip"))
      zip(fn_zip, file.path(dir_results, TargetSiteID))
      
      # 32, Display Results ####
      # Progress, 32
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Display Results"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      # triggers HTML output for Results file
      # Switch out and then back.
      updateSelectInput(session, inputId = "Station", selected = "NA")
      updateSelectInput(session, inputId = "Station", selected = input$Station)
      
      # 33, Complete ####
      # Progress, 33
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
      
      # 33.5, elapsed time
      end.time <- Sys.time()
      elapsed.time <- end.time - start.time
      message(paste0("Complete; elapsed time = ", format.difftime(elapsed.time)))

      #
    }, message = "Run ALL")##withProgress ~ END

    # 34, enable download button ####
    # Not a numbered step but numbered for outline
    shinyjs::enable("b_downloadSummary")
    shinyjs::enable("b_downloadData")
    
  }##Run_ALL2~END
  
})##server~END
