# Shiny, Server
# RPP - SMC
#

# Packages
#library(shiny)
options(shiny.maxRequestSize=100*1024^2) # increase max file upload to 100 MB

# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {

  # Output ####

  volumes=c(C = 'c:/')
  
  shinyDirChoose(input
                 , 'dir_CAST_Data'
                 , roots=volumes)
  output$dir_txt_CAST_Data <- renderPrint({parseDirPath(volumes, input$dir_CAST_Data)})

  shinyDirChoose(input
                 , 'dir_CAST_Results'
                 , roots=volumes)
  output$dir_txt_CAST_Results <- renderPrint({parseDirPath(volumes, input$dir_CAST_Results)})


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
  
  output$Selected_COMID4SiteID <- renderText({
    paste0("COMID from Map, Stations: "
           , df.sites.map[df.sites.map[, "StationID_Master"] == input$siteid.select, "COMID_NHD2"])
  })
  
  output$Selected_COMIDfromMapStations <- renderText({
    paste0("COMID from Map, Stations: "
           , df.sites.map[df.sites.map[, "StationID_Master"] == input$siteid.select, "COMID_NHD2"])
  })
  
  output$Selected_COMIDfromMapReach <- renderText({
    paste0("COMID from Map, Reach: ", input$comid.select)
  })

  # output$table_wt_stress_count <- renderTable({
  #   cnt_0 <- ifelse(input$wt_hab_evenflow == 0, 1, 0) +
  #     ifelse(input$wt_hab_phi == 0, 1, 0) +
  #     ifelse(input$wt_hab_ripcov == 0, 1, 0) +
  #     ifelse(input$wt_modflow_wetmax == 0, 1, 0) +
  #     ifelse(input$wt_modflow_avg99 == 0, 1, 0) +
  #     ifelse(input$wt_modflow_rbi == 0, 1, 0) +
  #     ifelse(input$wt_nutr_chla == 0, 1, 0) +
  #     ifelse(input$wt_nutr_no2 == 0, 1, 0) +
  #     ifelse(input$wt_nutr_p == 0, 1, 0) +
  #     ifelse(input$wt_nutr_n == 0, 1, 0) +
  #     ifelse(input$wt_wq_alk == 0, 1, 0) +
  #     ifelse(input$wt_wq_do == 0, 1, 0) +
  #     ifelse(input$wt_wq_cond == 0, 1, 0) +
  #     ifelse(input$wt_wq_tds == 0, 1, 0) +
  #     ifelse(input$wt_wq_wtemp == 0, 1, 0) 
  #     # ifelse(input$wt_Pot_BCG == 0, 1, 0) +
  #     # ifelse(input$wt_Pot_CxnBCG == 0, 1, 0) +
  #     # ifelse(input$wt_Pot_Stress == 0, 1, 0) +
  #     # ifelse(input$wt_Pot_CxnStress == 0, 1, 0) +
  #     # ifelse(input$wt_Threat_Fire == 0, 1, 0) +
  #     # ifelse(input$wt_Threat_LU == 0, 1, 0) +
  #     # ifelse(input$wt_Opp_ParksNow == 0, 1, 0) +
  #     # ifelse(input$wt_Opp_MSCPs == 0, 1, 0) +
  #     # ifelse(input$wt_Opp_NASVIBCG == 0, 1, 0) +
  #     # ifelse(input$wt_Opp_UserDefined == 0, 1, 0)
  #   cnt_1 <- ifelse(input$wt_hab_evenflow == 1, 1, 0) +
  #     ifelse(input$wt_hab_phi == 1, 1, 0) +
  #     ifelse(input$wt_hab_ripcov == 1, 1, 0) +
  #     ifelse(input$wt_modflow_wetmax == 1, 1, 0) +
  #     ifelse(input$wt_modflow_avg99 == 1, 1, 0) +
  #     ifelse(input$wt_modflow_rbi == 1, 1, 0) +
  #     ifelse(input$wt_nutr_chla == 1, 1, 0) +
  #     ifelse(input$wt_nutr_no2 == 1, 1, 0) +
  #     ifelse(input$wt_nutr_p == 1, 1, 0) +
  #     ifelse(input$wt_nutr_n == 1, 1, 0) +
  #     ifelse(input$wt_wq_alk == 1, 1, 0) +
  #     ifelse(input$wt_wq_do == 1, 1, 0) +
  #     ifelse(input$wt_wq_cond == 1, 1, 0) +
  #     ifelse(input$wt_wq_tds == 1, 1, 0) +
  #     ifelse(input$wt_wq_wtemp == 1, 1, 0) 
  #     # ifelse(input$wt_Pot_BCG == 1, 1, 0) +
  #     # ifelse(input$wt_Pot_CxnBCG == 1, 1, 0) +
  #     # ifelse(input$wt_Pot_Stress == 1, 1, 0) +
  #     # ifelse(input$wt_Pot_CxnStress == 1, 1, 0) +
  #     # ifelse(input$wt_Threat_Fire == 1, 1, 0) +
  #     # ifelse(input$wt_Threat_LU == 1, 1, 0) +
  #     # ifelse(input$wt_Opp_ParksNow == 1, 1, 0) +
  #     # ifelse(input$wt_Opp_MSCPs == 1, 1, 0) +
  #     # ifelse(input$wt_Opp_NASVIBCG == 1, 1, 0) +
  #     # ifelse(input$wt_Opp_UserDefined == 1, 1, 0)
  #   cnt_2 <- ifelse(input$wt_hab_evenflow == 2, 1, 0) +
  #     ifelse(input$wt_hab_phi == 2, 1, 0) +
  #     ifelse(input$wt_hab_ripcov == 2, 1, 0) +
  #     ifelse(input$wt_modflow_wetmax == 2, 1, 0) +
  #     ifelse(input$wt_modflow_avg99 == 2, 1, 0) +
  #     ifelse(input$wt_modflow_rbi == 2, 1, 0) +
  #     ifelse(input$wt_nutr_chla == 2, 1, 0) +
  #     ifelse(input$wt_nutr_no2 == 2, 1, 0) +
  #     ifelse(input$wt_nutr_p == 2, 1, 0) +
  #     ifelse(input$wt_nutr_n == 2, 1, 0) +
  #     ifelse(input$wt_wq_alk == 2, 1, 0) +
  #     ifelse(input$wt_wq_do == 2, 1, 0) +
  #     ifelse(input$wt_wq_cond == 2, 1, 0) +
  #     ifelse(input$wt_wq_tds == 2, 1, 0) +
  #     ifelse(input$wt_wq_wtemp == 2, 1, 0) 
  #     # ifelse(input$wt_Pot_BCG == 2, 1, 0) +
  #     # ifelse(input$wt_Pot_CxnBCG == 2, 1, 0) +
  #     # ifelse(input$wt_Pot_Stress == 2, 1, 0) +
  #     # ifelse(input$wt_Pot_CxnStress == 2, 1, 0) +
  #     # ifelse(input$wt_Threat_Fire == 2, 1, 0) +
  #     # ifelse(input$wt_Threat_LU == 2, 1, 0) +
  #     # ifelse(input$wt_Opp_ParksNow == 2, 1, 0) +
  #     # ifelse(input$wt_Opp_MSCPs == 2, 1, 0) +
  #     # ifelse(input$wt_Opp_NASVIBCG == 2, 1, 0) +
  #     # ifelse(input$wt_Opp_UserDefined == 2, 1, 0)
  # 
  #   txt_wt_desc <- c("Exclude", "Default", "Double Count")
  #   data.frame("Weights" = c("0", "1", "2")
  #              , "Description" = txt_wt_desc
  #              , "Count" = as.character(c(cnt_0, cnt_1, cnt_2))
  #              , row.names = NULL)
  # 
  # })##table_wt_stress_count
  
  output$table_wt_indicator_count <- renderTable({
    cnt_0 <- ifelse(input$wt_Pot_BCG == 0, 1, 0) +
      ifelse(input$wt_Pot_CxnBCG == 0, 1, 0) +
      ifelse(input$wt_Pot_Stress == 0, 1, 0) +
      ifelse(input$wt_Pot_CxnStress == 0, 1, 0) +
      ifelse(input$wt_Threat_Fire == 0, 1, 0) +
      ifelse(input$wt_Threat_LU == 0, 1, 0) +
      ifelse(input$wt_Opp_ParksNow == 0, 1, 0) +
      ifelse(input$wt_Opp_MSCPs == 0, 1, 0) +
      ifelse(input$wt_Opp_NASVIBCG == 0, 1, 0) +
      ifelse(input$wt_Opp_UserDefined == 0, 1, 0)
    cnt_1 <- ifelse(input$wt_Pot_BCG == 1, 1, 0) +
      ifelse(input$wt_Pot_CxnBCG == 1, 1, 0) +
      ifelse(input$wt_Pot_Stress == 1, 1, 0) +
      ifelse(input$wt_Pot_CxnStress == 1, 1, 0) +
      ifelse(input$wt_Threat_Fire == 1, 1, 0) +
      ifelse(input$wt_Threat_LU == 1, 1, 0) +
      ifelse(input$wt_Opp_ParksNow == 1, 1, 0) +
      ifelse(input$wt_Opp_MSCPs == 1, 1, 0) +
      ifelse(input$wt_Opp_NASVIBCG == 1, 1, 0) +
      ifelse(input$wt_Opp_UserDefined == 1, 1, 0)
    cnt_2 <-ifelse(input$wt_Pot_BCG == 2, 1, 0) +
      ifelse(input$wt_Pot_CxnBCG == 2, 1, 0) +
      ifelse(input$wt_Pot_Stress == 2, 1, 0) +
      ifelse(input$wt_Pot_CxnStress == 2, 1, 0) +
      ifelse(input$wt_Threat_Fire == 2, 1, 0) +
      ifelse(input$wt_Threat_LU == 2, 1, 0) +
      ifelse(input$wt_Opp_ParksNow == 2, 1, 0) +
      ifelse(input$wt_Opp_MSCPs == 2, 1, 0) +
      ifelse(input$wt_Opp_NASVIBCG == 2, 1, 0) +
      ifelse(input$wt_Opp_UserDefined == 2, 1, 0)
    
    txt_wt_desc <- c("Exclude", "Default", "Double Count")
    data.frame("Weights" = c("0", "1", "2")
               , "Description" = txt_wt_desc
               , "Count" = as.character(c(cnt_0, cnt_1, cnt_2))
               , row.names = NULL)
    
  })##table_wt_indicator_count
  
  output$table_wt_subindex_count <- renderTable({
    cnt_0 <- ifelse(input$wt_SubIndex_Pot == 0, 1, 0) +
      ifelse(input$wt_SubIndex_Threat == 0, 1, 0) +
      ifelse(input$wt_SubIndex_Opp == 0, 1, 0)
    cnt_1 <- ifelse(input$wt_SubIndex_Pot == 1, 1, 0) +
      ifelse(input$wt_SubIndex_Threat == 1, 1, 0) +
      ifelse(input$wt_SubIndex_Opp == 1, 1, 0) 
    cnt_2 <- ifelse(input$wt_SubIndex_Pot == 2, 1, 0) +
      ifelse(input$wt_SubIndex_Threat == 2, 1, 0) +
      ifelse(input$wt_SubIndex_Opp == 2, 1, 0)
    
    txt_wt_desc <- c("Exclude", "Default", "Double Count")
    data.frame("Weights" = c("0", "1", "2")
               , "Description" = txt_wt_desc
               , "Count" = as.character(c(cnt_0, cnt_1, cnt_2))
               , row.names = NULL)
    
  })##table_wt_subindex_count
  
  output$table_fn_CAST_data <- renderTable({
    fn_data <- c("SMC_AllStressData.tab"
                 , "SMC_AllStressInfo.tab"
                 , "SMCBenthicMetricsFinal.tab"
                 , "SMCSitesFinal.tab")
    txt_folder <- c(rep("Data", length(fn_data)))
    data.frame("Folder" = txt_folder
               , "FileName" = c(fn_data)
               , row.names = NULL)
  })##tbl_fn_CAST_data

  output$table_fn_CAST_results <- renderTable({
    fn_results <- "list out"
    txt_folder <- c(rep("Results", length(fn_results)))
    data.frame("Folder" = txt_folder
               , "FileName" = c(fn_results)
               , row.names = NULL)
  })##tbl_fn_CAST_results

  getHTML <- function(fn_html){
    #fn_disclaimer_html <- file.path(".", "data", "Disclaimer_Key.html")
    fe_html <- file.exists(fn_html)
    if(fe_html==TRUE){
      return(includeHTML(fn_html))
    } else {
      return(NULL)
    }
  }##getHTML~END

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

  # output$Disclaimer_html <- renderUI({
  #   getHTML(file.path(".", "www", "Disclaimer_Key.html"))
  #   #
  #   # fn_disclaimer_html <- file.path(".", "data", "Disclaimer_Key.html")
  #   # #
  #   # fe_disclaimer_html <- file.exists(fn_disclaimer_html)
  #   #
  #   #if(fe_disclaimer_html==TRUE){
  #   #  includeHTML(file.path(".", "data", "Disclaimer_Key.html"))
  #     #HTML(readLines(fn_map_html))
  #   #} else {
  #   #  NULL
  #   #}
  #   #getHTML(file.path(".", "Data", fn_target_results))
  # })##Disclaimer_html~END

  # Test if zip file exists
  output$boo_zip <- function() {
    fn_zip_boo <- paste0(input$Station, ".zip")
    return(file.exists(file.path(".", "Results", fn_zip_boo)) == TRUE)
  }##boo_zip~END

  # Observe ####

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
      paste0(input$COMID_RPP, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(fname) {##content~START
      # tmpdir <- tempdir()
      #setwd(tempdir())
      file.copy(file.path(".", "Results", paste0(input$COMID_RPP, ".zip")), fname)
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

  observeEvent(input$b_Wts_Str_Reset, {
    # Reset all Weights to '1' (n=25).
    updateSliderInput(session, "wt_hab_evenflow", value = 1)
    updateSliderInput(session, "wt_hab_phi", value = 1)
    updateSliderInput(session, "wt_hab_ripcov", value = 1)
    updateSliderInput(session, "wt_modflow_wetmax", value = 1)
    updateSliderInput(session, "wt_modflow_avg99", value = 1)
    updateSliderInput(session, "wt_modflow_rbi", value = 1)
    updateSliderInput(session, "wt_nutr_chla", value = 1)
    updateSliderInput(session, "wt_nutr_no2", value = 1)
    updateSliderInput(session, "wt_nutr_p", value = 1)
    updateSliderInput(session, "wt_nutr_n", value = 1)
    updateSliderInput(session, "wt_wq_alk", value = 1)
    updateSliderInput(session, "wt_wq_do", value = 1)
    updateSliderInput(session, "wt_wq_cond", value = 1)
    updateSliderInput(session, "wt_wq_tds", value = 1)
    updateSliderInput(session, "wt_wq_wtemp", value = 1)
    # updateSliderInput(session, "wt_Pot_BCG", value = 1)
    # updateSliderInput(session, "wt_Pot_CxnBCG", value = 1)
    # updateSliderInput(session, "wt_Pot_Stress", value = 1)
    # updateSliderInput(session, "wt_Pot_CxnStress", value = 1)
    # updateSliderInput(session, "wt_Threat_Fire", value = 1)
    # updateSliderInput(session, "wt_Threat_LU", value = 1)
    # updateSliderInput(session, "wt_Opp_ParksNow", value = 1)
    # updateSliderInput(session, "wt_Opp_MSCPs", value = 1)
    # updateSliderInput(session, "wt_Opp_NASVIBCG", value = 1)
    # updateSliderInput(session, "wt_Opp_UserDefined", value = 1)
  })##observeEvent~input$b_Wts_Str_Reset~ENDs
  
  observeEvent(input$b_Wts_Ind_Reset, {
    # Reset all Weights to '1' (n=25).
    # updateSliderInput(session, "wt_hab_evenflow", value = 1)
    # updateSliderInput(session, "wt_hab_phi", value = 1)
    # updateSliderInput(session, "wt_hab_ripcov", value = 1)
    # updateSliderInput(session, "wt_modflow_wetmax", value = 1)
    # updateSliderInput(session, "wt_modflow_avg99", value = 1)
    # updateSliderInput(session, "wt_modflow_rbi", value = 1)
    # updateSliderInput(session, "wt_nutr_chla", value = 1)
    # updateSliderInput(session, "wt_nutr_no2", value = 1)
    # updateSliderInput(session, "wt_nutr_p", value = 1)
    # updateSliderInput(session, "wt_nutr_n", value = 1)
    # updateSliderInput(session, "wt_wq_alk", value = 1)
    # updateSliderInput(session, "wt_wq_do", value = 1)
    # updateSliderInput(session, "wt_wq_cond", value = 1)
    # updateSliderInput(session, "wt_wq_tds", value = 1)
    # updateSliderInput(session, "wt_wq_wtemp", value = 1)
    updateSliderInput(session, "wt_Pot_BCG", value = 1)
    updateSliderInput(session, "wt_Pot_CxnBCG", value = 1)
    updateSliderInput(session, "wt_Pot_Stress", value = 1)
    updateSliderInput(session, "wt_Pot_CxnStress", value = 1)
    updateSliderInput(session, "wt_Threat_Fire", value = 1)
    updateSliderInput(session, "wt_Threat_LU", value = 1)
    updateSliderInput(session, "wt_Opp_ParksNow", value = 1)
    updateSliderInput(session, "wt_Opp_MSCPs", value = 1)
    updateSliderInput(session, "wt_Opp_NASVIBCG", value = 1)
    updateSliderInput(session, "wt_Opp_UserDefined", value = 1)
  })##observeEvent ~ input$b_Wts_Ind_Reset ~ ENDs

  observeEvent(input$b_Wts_Subindex_Reset, {
    # Reset all Weights to '1' (n=25).
    updateSliderInput(session, "wt_SubIndex_Pot", value = 1)
    updateSliderInput(session, "wt_SubIndex_Threat", value = 1)
    updateSliderInput(session, "wt_SubIndex_Opp", value = 1)
  })##observeEvent ~ input$b_Wts_SubIndex_Reset ~ ENDs
  
  
  
  
  
  
  observeEvent(input$b_Wts_Import, {
    #
    # Reset all Weights user input file.
    #
    # if file exists

  })##observeEvent~input$b_Wts_Import~ENDs

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

  # Run_ALL ####
  Run_ALL <- function(){
    #
   shiny::withProgress({
      #
      # Number of increments
      prog_n <- 19 # 20200924
      prog_inc <- 1/prog_n
      prog_cnt <- 0
      mySleepTime <- 0.5
      #
      # 01, Remove Zip ####
      # Progress, 01
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Remove Zip"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
      # fn_zip_results <- list.files(file.path(".", "Results"), ".zip", full.names = TRUE)
      # if(length(fn_zip_results)>0){
      #   file.remove(fn_zip_results)
      # }##IF~length(fn_zip_results)~END
      # Remove only the current station's zip file
      TargetCOMID <- input$COMID_RPP
      fn_zip <- file.path(".", "Results", paste0(TargetCOMID, ".zip"))
      if (file.exists(fn_zip)==TRUE){
        file.remove(fn_zip)
      }##IF~file.exists~END
      
      # 02, Copy CAST inputs ####
      # Progress, 01
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Copy CAST input files"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
      # Copy uploaded CAST files
      if(input$useCASTresults == TRUE){
        # CAST Data
        inFile <- input$upload_CAST_Data
        message("Uploaded files; CASTool Data")
        message(paste(inFile$name, collapse = "\n"))
        file.copy(inFile$datapath
                  , file.path(".", "Data", "CASTool_data", inFile$name)
                  , overwrite = TRUE)
        # CAST Results
        inFile <- input$upload_CAST_Results
        message("Uploaded files; CASTool Results")
        message(paste(inFile$name, collapse = "\n"))
        file.copy(inFile$datapath
                  , file.path(".", "Data", "CASTool_results", inFile$name)
                  , overwrite = TRUE)
      }## IF ~ input$useCASTresults ~ END
      
      
      # if useCASTfiles copy
      
      
      # #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # # 02, UpdateAllScores ####
      # # Run UpdateAllScores to account for Shiny user input weights
      # #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # boo_Shiny <- TRUE
      # # dir_CASTdata <- file.path(dir_data, "CASTool_data")
      # # dir_CASTresults <- file.path(dir_data, "CASTool_results")
      # 
      # # Progress, 02
      # prog_cnt <- prog_cnt + 1
      # prog_msg <- paste0("Step ", prog_cnt)
      # prog_det <- "Run UpdateAllScores for Shiny user input"
      # incProgress(prog_inc, message = prog_msg, detail = prog_det)
      # Sys.sleep(mySleepTime)
      # message(paste(prog_msg, prog_det, sep = "; "))
      # # if any "weights" are not "1" then re-run "UpdateAllScores"
      # 
      # # 02, Set RPPTool directories #
      # if(boo_Shiny == TRUE){
      #   sep_rpp_dir <- file.path(".")
      # }## IF ~ boo_Shiny ~ END
      # data_dir <- file.path(sep_rpp_dir,"Data")
      # results_dir <- file.path(sep_rpp_dir,"Results")
      # 
      # # 02.03, Set output file names #
      # fn_allscores   <- file.path(results_dir,paste0("RPPTool_AllScores"))
      # fn_stresswtsOUT   <- file.path(data_dir,"StressorWeights.tab")
      # 
      # # 02.04, User-defined variables #
      # if(boo_Shiny == TRUE){
      #   # Added for this part of of the script
      #   useCASTresults   <- input$useCASTresults
      #   maxYear <- input$year_max
      #   minYear <- input$year_min
      #   # Connectivity variables
      #   cxndist_km        <- input$cxndist_km #5
      #   useHWbonus        <- input$useHWbonus #0 # FALSE (default)
      #   useBCGbonus       <- input$useBCGbonus #0 # FALSE (default)
      #   useDownstream     <- input$useDownstream #0 # FALSE (default)
      #   useModerateFireHazard <- input$useModFireHazard # FALSE (default)
      #   # Indicator weights
      #   wtPot_BCG         <- input$wt_Pot_BCG
      #   wtPot_CxnBCG      <- input$wt_Pot_CxnBCG
      #   wtPot_Stress      <- input$wt_Pot_Stress
      #   wtPot_CxnStress   <- input$wt_Pot_CxnStress
      #   wtThreat_Fire     <- input$wt_Threat_Fire
      #   wtThreat_LU       <- input$wt_Threat_LU # Probably need to separate out categories
      #   wtOpp_ParksNow    <- input$wt_Opp_ParksNow
      #   wtOpp_MSCPs       <- input$wt_Opp_MSCPs
      #   wtOpp_NASVI       <- input$wt_Opp_NASVIBCG
      #   wtOpp_UserDefined <- input$wt_Opp_UserDefined
      #   wtPot_subidx      <- input$wt_SubIndex_Pot
      #   wtThreat_subidx   <- input$wt_SubIndex_Threat
      #   wtOpp_subidx      <- input$wt_SubIndex_Opp
      # # only relevant for Shiny, removed "else".
      # }## IF ~ boo_Shiny ~ END
      # usePrevStressWts <- FALSE
      # if (usePrevStressWts) { # User must supply stress weighting file to use
      #   # fn_stresswtsIN <- ""
      # } else {
      #   fn_stresswtsIN <- fn_stresswtsOUT # Change only if a copy of the weights file is prepared
      # }## IF ~ usePrevStressWts ~ END
      # fn_allstress     <- "SMC_AllStressData.tab" # Change only if file name differs
      # fn_allstressmeta <- "SMC_AllStressInfo.tab" # Change only if file name differs
      # 
      # listWeights <- list(wtPot_Bio=wtPot_BCG, wtPot_CxnBio=wtPot_CxnBCG
      #                     , wtPot_Stress=wtPot_Stress, wtPot_CxnStress=wtPot_CxnStress
      #                     , wtThreat_Fire=wtThreat_Fire, wtThreat_LUdev=wtThreat_LU
      #                     , wtOpp_Recr=wtOpp_ParksNow, wtOpp_MSCPs=wtOpp_MSCPs
      #                     , wtOpp_NASVI=wtOpp_NASVI, wtOpp_UserDefined=wtOpp_UserDefined
      #                     , wt_subPot=wtPot_subidx, wt_subThreat=wtThreat_subidx
      #                     , wt_subOpp=wtOpp_subidx)
      # 
      # 
      # # 02.05, Set input file names #
      # fn_sites <- file.path(data_dir, "SMCSitesFinal.tab")
      # dfSites  <- read.delim(fn_sites, header=TRUE, stringsAsFactors=FALSE, sep="\t")
      # 
      # # 02.06, Get stressor data from CASTool #
      # # 6 (all)
      # # Change useCASTresults to boo_Shiny to force action
      # # modify to use existing data
      # if (boo_Shiny==TRUE) {
      # 
      #     listScaledStr01All <- getScaledStressors(fn_allstress=file.path(dir_data, fn_allstress)
      #                                              , fn_allstressinfo=file.path(dir_data, fn_allstressmeta)
      #                                              , dir_CASTresults=dir_data)
      # 
      #     if (listScaledStr01All$stressorsFound==TRUE) { # Found candidate causes
      # 
      #       dfStressInfo <- as.data.frame(listScaledStr01All$df_allSMCStressInfo)
      # 
      #       # Check for existing stressor weight file #
      #       if (usePrevStressWts==TRUE) {
      #         if (file.exists("fn_stresswtsOUT")) {
      #           # Display existing file to user; ask if it should be used
      #           fn_stresswtsIN = fn_stresswtsOUT
      #         } else {
      #           msg <- "No existing stressor weight in data directory."
      #           message(msg)
      #           write.table(listScaledStr01All$df_allSMCStressInfo
      #                       , fn_stresswtsOUT
      #                       , append=FALSE
      #                       , col.names=TRUE, row.names=FALSE, sep="\t")
      #         } # end No weight file
      # 
      #       } else { # Do not use existing file
      # 
      #         write.table(listScaledStr01All$df_allSMCStressInfo
      #                     , fn_stresswtsOUT
      #                     , append=FALSE
      #                     , col.names=TRUE, row.names=FALSE, sep="\t")
      #       } # End check use existing file
      # 
      #       # Plot number of samples by year, and ask user to select min and max year
      #       dfStressPlot <- listScaledStr01All$df_allSMCStressVals[,c("StationID_Master"
      #                                                                 , "StressSampID"
      #                                                                 , "StressSampleDate")]
      # 
      #     # remove some stuff
      # 
      # 
      #     } else { # No candidate causes found
      #       msg <- paste0("No candidate causes found. "
      #                  , "No stressor or stressor connectivity scores "
      #                  , "will be calculated.")
      #       message(msg)
      #     }## IF ~ listScaledStr01All$stressorsFound==TRUE ~ END
      # 
      # 
      # 
      # }## useCASTresults ~ END
      # 
      # # 02.07, Get stressor scores #
      # # change useCASTresults to boo_Shiny as trying to force calculation of updateAllScoresTable
      # if ((boo_Shiny==TRUE) && (listScaledStr01All$stressorsFound==TRUE)) { # User wants to use CAST results
      # 
      #   listStressScores <- getStressorScores(dfSites = dfSites
      #                                         , dfAllStressVals = listScaledStr01All$df_allSMCStressVals
      #                                         , fnWeights = fn_stresswtsIN
      #                                         , maxYear = maxYear
      #                                         , minYear = minYear)
      #   dfAllScores = listStressScores$dfStrScores
      # } else {
      #   listStressScores <- NULL
      # }## IF ~ useCASTResults & listScaledStr01All$stressorsFound  ~ END
      # 
      # 
      # # New
      # 
      # # Default is all "1".
      # # Values are 0, 1, 2
      # # If multiply all values only get 1 if all 1.
      # listWeights_prod <- Reduce("*", listWeights)
      # message(paste0("listWeights_prod = ", listWeights_prod))
      # # listWeights_prod <- do.call(prod, listWeights) # also works
      # 
      # if(boo_Shiny == TRUE & listWeights_prod != 1){
      #   # 15, Make updateable All Scores table #
      #   # Run UpdateAllScores
      #   listAllScores <- updateAllScoresTable(dfAllScores
      #                                         , listWeights
      #                                         , fn_allscores
      #                                         , BioDegBrk=c(-2, 0.799, 2)
      #                                         , BioDegLab=c("Degraded", "Not degraded"))
      # }## IF ~ listWeights_prod!=1 ~ END


      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # Skeleton, Start ####
      # external/RPPTool_CA.R
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      
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
      
      boo_Shiny <- TRUE
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
      
      
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      # Skeleton, END ####
      # external/RPPTool_CA.R
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      
      # QC, plots ####
      # QC, comid with no data
      # Check for plots to display
      # base plot
      p_nodata <- ggplot() +
        theme_void() + 
        labs(title = paste0("Reach: ", TargetReach)
             , subtitle = "No data for this COMID.") 
      # Create plots if they don't exist
      plot_types <- c("AllScores", "RPPIndex", "ConnectedReaches")
      for (i in plot_types){
        fn_check <- list.files(path = file.path(".", "Results", TargetReach)
                                       , pattern = i, full.names = TRUE)
        if(length(fn_check) == 0){
          p_nodata_i <- p_nodata + 
            labs(caption = i)
          ggsave(file.path(".", "Results", TargetReach, paste0(TargetReach, "_", i, "_NoData.png"))
                 , p_nodata_i)
        }## IF ~ length(fn_check) ~ END
      }## FOR ~ i ~ END
      
      # 19, Create zip ####
      # Progress, 18
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_det <- "Create Zip Download"
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
      # Add directory
      fn_zip <- file.path(results_dir, paste0(TargetCOMID, ".zip"))
      zip(fn_zip, file.path(results_dir, TargetCOMID))
      # add single file
      fn_summary <- list.files(path = results_dir, pattern = "^RPPTool_AllScoresSummary")
      fn_add <- file.path(results_dir, fn_summary[length(fn_summary)]) # get last one
      zip(fn_zip, fn_add)

      # Total time
      elapsed.time2 <- Sys.time() - start.time
      message(paste0("Results ready for download.  Total time = ", format.difftime(elapsed.time2)))
      
      # Enable download button.
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
  
  # watch_results <- reactive({
  #   # trigger for df_results()
  #   paste(input$b_RunAll, )
  # })## watch_results ~ END
  # 
  # df_results <- "x"
  
  # Results ####
  output$df_results_DT <- DT::renderDT({
    #
    TargetCOMID <- input$COMID_RPP
    # should be blank initially and once have data it will appear correctly
    #df_r <- df_results()
    # Need most current file
    fn_sum <- list.files(path = file.path(".", "Results", TargetCOMID)
                         # , pattern = "^RPPTool_AllScoresSummary"
                         #, pattern = "^RPPTool_AllConnectedReaches"
                         , pattern = "_CxnScoresSummary"
                         , full.names = TRUE
                         )
    # get last one
    fn_sum_latest <- fn_sum[length(fn_sum)]
    #
    df_r <- read.delim(fn_sum_latest, stringsAsFactors = FALSE)
    colnames(df_r) <- gsub("\\.", "<br>", colnames(df_r))
    return(df_r)
  }##expression~END
  , filter="top"
  , caption = "Results summary, connected reaches"
  , options=list(scrollX=TRUE
                 , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                 , autoWidth = TRUE
                 )
  , escape = FALSE
  )## output$df_results_DT ~ END
  
  img_width <- 600
  
  img_scores_pn <- reactive({
    TargetCOMID <- input$COMID_RPP
    #TargetCOMID <- "20331434"
    fn_png <- list.files(path = file.path(".", "Results", TargetCOMID)
                             , pattern = "AllScores")
    fn_png_latest <- fn_png[length(fn_png)]
    pn_png <- normalizePath(file.path(".", "Results", TargetCOMID, fn_png_latest))
    #
  })## img_scores_pn
  
  output$img_scores <- renderImage({
    #
    if(file.exists(img_scores_pn())){
      return(list(
        src = img_scores_pn()
        , contentType = "image/png"
        , width = img_width
        , alt = "results all scores"
      ))#return~END
    } else {
      #validate(need(file.exists(img_scores_pn()), message = "Results not ready or no scores available."))
      return(NULL)
    }## IF ~ END
  }, deleteFile = FALSE
  )## output$report_img ~ END
  
  img_map_pn <- reactive({
    TargetCOMID <- input$COMID_RPP
    #TargetCOMID <- "20331434"
    fn_png <- list.files(path = file.path(".", "Results", TargetCOMID)
                         , pattern = "RPPIndex")
    fn_png_latest <- fn_png[length(fn_png)]
    pn_png <- normalizePath(file.path(".", "Results", TargetCOMID, fn_png_latest))
    #
  })## img_map_pn
  
  output$img_map <- renderImage({
    #
    if(file.exists(img_map_pn())){
      return(list(
        src = img_map_pn()
        , contentType = "image/png"
        , width = img_width
        , alt = "map RPP Index"
      ))#return~END
    } else {
      #validate(need(file.exists(img_map_pn()), message = "Results not ready or no scores available."))
      return(NULL)
    }## IF ~ END
  }, deleteFile = FALSE
  )## output$img_map ~ END
  
  img_map_ConnReach <- reactive({
    TargetCOMID <- input$COMID_RPP
    #TargetCOMID <- "20331434"
    fn_png <- list.files(path = file.path(".", "Results", TargetCOMID)
                         , pattern = "ConnectedReaches")
    fn_png_latest <- fn_png[length(fn_png)]
    pn_png <- normalizePath(file.path(".", "Results", TargetCOMID, fn_png_latest))
    #
  })## img_map_ConnReach
  
  output$img_map_cxns <- renderImage({
    #
    if(file.exists(img_map_ConnReach())){
      return(list(
        src = img_map_ConnReach()
        , contentType = "image/png"
        , width = img_width
        , alt = "map connected reaches"
      ))#return~END
    } else {
      #validate(need(file.exists(img_map_ConnReach()), message = "Results not ready or no scores available."))
      return(NULL)
    }## IF ~ END
  }, deleteFile = FALSE
  )## output$img_map_cxns ~ END
  

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

  # Help ####
  output$help_html <- renderUI({
    fn_help_html <- file.path(".", "www", "ShinyHelp.html")
    fe_help_html <- file.exists(fn_help_html)
    if(fe_help_html==TRUE){
      return(includeHTML(fn_help_html))
    } else {
      return(NULL)
    }##IF~fe_help_html~END
  })##help_html~END
  
  # Map, Stations ####
  # Stop Shiny App when close browser
  session$onSessionEnded(stopApp)
  
  # palette
  pal.tidal <- colorBin(palette=c("red", "blue"), domain=lines.flowline.proj$LENGTHKM)
  pal.smc   <- colorFactor(palette = "Set3", domain=poly.smc.proj$CUNAME)
  
  # Map
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

  })##ObserverEvent ~ input$siteid.select ~ END
  
  
  # Map, Reach ####
  output$map_reach <- renderLeaflet({
    #
    leaflet() %>%
      # Groups, Base
      addTiles(group="OSM (default)") %>%  #default tile too cluttered
      addProviderTiles("CartoDB.Positron", group="Positron") %>%
      addProviderTiles(providers$Stamen.TonerLite, group="Toner Lite") %>%
      #addProviderTiles(providers$OpenTopoMap, group="TopoMap (Open)") %>%
      #addProviderTiles(providers$Esri.WorldTopoMap, group="TopoMap (ESRI)") %>%
      #addProviderTiles(providers$Esri.WorldImagery, group="Imagery (ESRI)") %>%
      # # Groups, Overlay
      addPolygons(data=poly.smc.proj
                  , color="green"
                  , fill=FALSE
                  , group="Watersheds") %>%
      addPolylines(data=lines.flowline.proj
                   , color="blue"
                   , popup=~COMID
                   , highlightOptions=highlightOptions(bringToFront=TRUE
                                                       , color="red" )
                   , group="Streams") %>%
      addPolylines(data=lines.flowline.proj[lines.flowline.proj$COMID == "20331944", ]
                   , color="orange"
                   , popup=~COMID
                   #, highlightOptions=highlightOptions(bringToFront=TRUE
                   #                                    , color="red" )
                   , layerId = "layer_Stream_Select"
                   , group="Streams_Select") %>%
      addPolylines(data=lines.flowline.proj
                   , color= ~pal.tidal(LENGTHKM)
                   , layerId = "layer_color"
                   , group="lines_color") %>%
      addCircles(data=df.sites.map
                 , lng=~FinalLongitude
                 , lat=~FinalLatitude
                 , popup=~StationID_Master
                 , color="gray"
                 , group="Sites"
                 #, radius=~CSCI
                 ) %>%
      # # Bounding
      fitBounds(lng1 = poly.smc.proj@bbox[1]
                , lat1 = poly.smc.proj@bbox[4]
                , lng2 = poly.smc.proj@bbox[3]
                , lat2 = poly.smc.proj@bbox[2]) %>%
      # Layers
      addLayersControl(baseGroups=c("OSM (default)", "Positron", "Toner Lite"
                                    , "TopoMap (Open)", "TopoMap (ESRI)", "Imagery (ESRI)")
                       , overlayGroups=c("Watersheds", "Sites", "Streams_Select", "Streams", "lines_color")
                       , options=layersControlOptions(collapsed=TRUE)) %>%
      # Legend
      addLegend("bottomleft", colors=c("green", "blue", "red", "orange", "gray")
                , labels=c("Watersheds", "Streams", "Stream (mouse-over)", "Stream (selected)", "Sites")
                , values=NA) %>%
      addMiniMap(toggleDisplay = TRUE)
    #
  })#output$map_reach ~ END
  
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
  observeEvent(input$comid.select,{
    #
    filteredData <- lines.flowline.proj[lines.flowline.proj$COMID == input$comid.select, ]
    #
    # get centroid
    view.cent <- c(filteredData$CENTROID_X, filteredData$CENTROID_Y)
    #
    # modify map
    leafletProxy("map_reach") %>%
      #clearShapes() %>%  # removes all layers
      removeShape("layer_Stream_Select") %>%
      #addPolylines(data=filteredData()
      addPolylines(data=filteredData
                   , color="orange"
                   , popup=~COMID
                   #, highlightOptions=highlightOptions(bringToFront=TRUE
                   #                                    , color="red" )
                   , group="Streams_Select"
                   , layerId = "layer_Stream_Select") %>%
      #setView(fD.centroid[1], fD.centroid[2], zoom=10)
      #setView(view.cent[1], view.cent[2], zoom=10)
      fitBounds(filteredData@bbox[1], filteredData@bbox[2], filteredData@bbox[3], filteredData@bbox[4])
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
    
    
    
  })##ObserveEvent ~ comid.select ~ END
  
  
  

})##server~END
