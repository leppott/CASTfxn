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
    df.sites.map[df.sites.map[, "StationID_Master"] == input$siteid.select, "COMID_NHD2"]
  })

  output$table_wt_stress_count <- renderTable({
    cnt_0 <- ifelse(input$wt_hab_evenflow == 0, 1, 0) +
      ifelse(input$wt_hab_phi == 0, 1, 0) +
      ifelse(input$wt_hab_ripcov == 0, 1, 0) +
      ifelse(input$wt_modflow_wetmax == 0, 1, 0) +
      ifelse(input$wt_modflow_avg99 == 0, 1, 0) +
      ifelse(input$wt_modflow_rbi == 0, 1, 0) +
      ifelse(input$wt_nutr_chla == 0, 1, 0) +
      ifelse(input$wt_nutr_no2 == 0, 1, 0) +
      ifelse(input$wt_nutr_p == 0, 1, 0) +
      ifelse(input$wt_nutr_n == 0, 1, 0) +
      ifelse(input$wt_wq_alk == 0, 1, 0) +
      ifelse(input$wt_wq_do == 0, 1, 0) +
      ifelse(input$wt_wq_cond == 0, 1, 0) +
      ifelse(input$wt_wq_tds == 0, 1, 0) +
      ifelse(input$wt_wq_wtemp == 0, 1, 0) 
      # ifelse(input$wt_Pot_BCG == 0, 1, 0) +
      # ifelse(input$wt_Pot_CxnBCG == 0, 1, 0) +
      # ifelse(input$wt_Pot_Stress == 0, 1, 0) +
      # ifelse(input$wt_Pot_CxnStress == 0, 1, 0) +
      # ifelse(input$wt_Threat_Fire == 0, 1, 0) +
      # ifelse(input$wt_Threat_LU == 0, 1, 0) +
      # ifelse(input$wt_Opp_ParksNow == 0, 1, 0) +
      # ifelse(input$wt_Opp_MSCPs == 0, 1, 0) +
      # ifelse(input$wt_Opp_NASVIBCG == 0, 1, 0) +
      # ifelse(input$wt_Opp_UserDefined == 0, 1, 0)
    cnt_1 <- ifelse(input$wt_hab_evenflow == 1, 1, 0) +
      ifelse(input$wt_hab_phi == 1, 1, 0) +
      ifelse(input$wt_hab_ripcov == 1, 1, 0) +
      ifelse(input$wt_modflow_wetmax == 1, 1, 0) +
      ifelse(input$wt_modflow_avg99 == 1, 1, 0) +
      ifelse(input$wt_modflow_rbi == 1, 1, 0) +
      ifelse(input$wt_nutr_chla == 1, 1, 0) +
      ifelse(input$wt_nutr_no2 == 1, 1, 0) +
      ifelse(input$wt_nutr_p == 1, 1, 0) +
      ifelse(input$wt_nutr_n == 1, 1, 0) +
      ifelse(input$wt_wq_alk == 1, 1, 0) +
      ifelse(input$wt_wq_do == 1, 1, 0) +
      ifelse(input$wt_wq_cond == 1, 1, 0) +
      ifelse(input$wt_wq_tds == 1, 1, 0) +
      ifelse(input$wt_wq_wtemp == 1, 1, 0) 
      # ifelse(input$wt_Pot_BCG == 1, 1, 0) +
      # ifelse(input$wt_Pot_CxnBCG == 1, 1, 0) +
      # ifelse(input$wt_Pot_Stress == 1, 1, 0) +
      # ifelse(input$wt_Pot_CxnStress == 1, 1, 0) +
      # ifelse(input$wt_Threat_Fire == 1, 1, 0) +
      # ifelse(input$wt_Threat_LU == 1, 1, 0) +
      # ifelse(input$wt_Opp_ParksNow == 1, 1, 0) +
      # ifelse(input$wt_Opp_MSCPs == 1, 1, 0) +
      # ifelse(input$wt_Opp_NASVIBCG == 1, 1, 0) +
      # ifelse(input$wt_Opp_UserDefined == 1, 1, 0)
    cnt_2 <- ifelse(input$wt_hab_evenflow == 2, 1, 0) +
      ifelse(input$wt_hab_phi == 2, 1, 0) +
      ifelse(input$wt_hab_ripcov == 2, 1, 0) +
      ifelse(input$wt_modflow_wetmax == 2, 1, 0) +
      ifelse(input$wt_modflow_avg99 == 2, 1, 0) +
      ifelse(input$wt_modflow_rbi == 2, 1, 0) +
      ifelse(input$wt_nutr_chla == 2, 1, 0) +
      ifelse(input$wt_nutr_no2 == 2, 1, 0) +
      ifelse(input$wt_nutr_p == 2, 1, 0) +
      ifelse(input$wt_nutr_n == 2, 1, 0) +
      ifelse(input$wt_wq_alk == 2, 1, 0) +
      ifelse(input$wt_wq_do == 2, 1, 0) +
      ifelse(input$wt_wq_cond == 2, 1, 0) +
      ifelse(input$wt_wq_tds == 2, 1, 0) +
      ifelse(input$wt_wq_wtemp == 2, 1, 0) 
      # ifelse(input$wt_Pot_BCG == 2, 1, 0) +
      # ifelse(input$wt_Pot_CxnBCG == 2, 1, 0) +
      # ifelse(input$wt_Pot_Stress == 2, 1, 0) +
      # ifelse(input$wt_Pot_CxnStress == 2, 1, 0) +
      # ifelse(input$wt_Threat_Fire == 2, 1, 0) +
      # ifelse(input$wt_Threat_LU == 2, 1, 0) +
      # ifelse(input$wt_Opp_ParksNow == 2, 1, 0) +
      # ifelse(input$wt_Opp_MSCPs == 2, 1, 0) +
      # ifelse(input$wt_Opp_NASVIBCG == 2, 1, 0) +
      # ifelse(input$wt_Opp_UserDefined == 2, 1, 0)

    txt_wt_desc <- c("Exclude", "Default", "Double Count")
    data.frame("Weights" = c("0", "1", "2")
               , "Description" = txt_wt_desc
               , "Count" = as.character(c(cnt_0, cnt_1, cnt_2))
               , row.names = NULL)

  })##table_wt_stress_count
  
  output$table_wt_indicator_count <- renderTable({
    #cnt_0 <- #ifelse(input$wt_hab_evenflow == 0, 1, 0) +
    #   ifelse(input$wt_hab_phi == 0, 1, 0) +
    #   ifelse(input$wt_hab_ripcov == 0, 1, 0) +
    #   ifelse(input$wt_modflow_wetmax == 0, 1, 0) +
    #   ifelse(input$wt_modflow_avg99 == 0, 1, 0) +
    #   ifelse(input$wt_modflow_rbi == 0, 1, 0) +
    #   ifelse(input$wt_nutr_chla == 0, 1, 0) +
    #   ifelse(input$wt_nutr_no2 == 0, 1, 0) +
    #   ifelse(input$wt_nutr_p == 0, 1, 0) +
    #   ifelse(input$wt_nutr_n == 0, 1, 0) +
    #   ifelse(input$wt_wq_alk == 0, 1, 0) +
    #   ifelse(input$wt_wq_do == 0, 1, 0) +
    #   ifelse(input$wt_wq_cond == 0, 1, 0) +
    #   ifelse(input$wt_wq_tds == 0, 1, 0) +
    #   ifelse(input$wt_wq_wtemp == 0, 1, 0) +
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
    # cnt_1 <- #ifelse(input$wt_hab_evenflow == 1, 1, 0) +
      # ifelse(input$wt_hab_phi == 1, 1, 0) +
      # ifelse(input$wt_hab_ripcov == 1, 1, 0) +
      # ifelse(input$wt_modflow_wetmax == 1, 1, 0) +
      # ifelse(input$wt_modflow_avg99 == 1, 1, 0) +
      # ifelse(input$wt_modflow_rbi == 1, 1, 0) +
      # ifelse(input$wt_nutr_chla == 1, 1, 0) +
      # ifelse(input$wt_nutr_no2 == 1, 1, 0) +
      # ifelse(input$wt_nutr_p == 1, 1, 0) +
      # ifelse(input$wt_nutr_n == 1, 1, 0) +
      # ifelse(input$wt_wq_alk == 1, 1, 0) +
      # ifelse(input$wt_wq_do == 1, 1, 0) +
      # ifelse(input$wt_wq_cond == 1, 1, 0) +
      # ifelse(input$wt_wq_tds == 1, 1, 0) +
      # ifelse(input$wt_wq_wtemp == 1, 1, 0) +
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
    # cnt_2 <- #ifelse(input$wt_hab_evenflow == 2, 1, 0) +
      # ifelse(input$wt_hab_phi == 2, 1, 0) +
      # ifelse(input$wt_hab_ripcov == 2, 1, 0) +
      # ifelse(input$wt_modflow_wetmax == 2, 1, 0) +
      # ifelse(input$wt_modflow_avg99 == 2, 1, 0) +
      # ifelse(input$wt_modflow_rbi == 2, 1, 0) +
      # ifelse(input$wt_nutr_chla == 2, 1, 0) +
      # ifelse(input$wt_nutr_no2 == 2, 1, 0) +
      # ifelse(input$wt_nutr_p == 2, 1, 0) +
      # ifelse(input$wt_nutr_n == 2, 1, 0) +
      # ifelse(input$wt_wq_alk == 2, 1, 0) +
      # ifelse(input$wt_wq_do == 2, 1, 0) +
      # ifelse(input$wt_wq_cond == 2, 1, 0) +
      # ifelse(input$wt_wq_tds == 2, 1, 0) +
      # ifelse(input$wt_wq_wtemp == 2, 1, 0) +
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
      ifelse(input$wt_SubIndex_Opp == 0, 1, 0) +
      ifelse(input$wt_modflow_wetmax == 0, 1, 0)
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

  output$table_fn_CAST <- renderTable({
    fn_data <- c("SMC_AllStressData.tab"
                 , "SMC_AllStressInfo.tab"
                 , "SMCBenthicMetricsFinal.tab"
                 , "SMCSitesFinal.tab")
    fn_results <- c("list out")
    txt_folder <- c(rep("Data", length(fn_data)), rep("Results", length(fn_results)))
    data.frame("Folder" = txt_folder
               , "FileName" = c(fn_data, fn_results)
               , row.names = NULL)
  })##tbl_fn_CAST

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
      TargetCOMID <- input$COMID_RPP
      fn_zip <- file.path(".", "Results", paste0(TargetCOMID, ".zip"))
      if (file.exists(fn_zip)==TRUE){
        file.remove(fn_zip)
      }##IF~file.exists~END

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Load Ann's files and such
      #gitpath <- file.path(".", "external") # might need something different
      # source(file.path(gitpath, "getCoOccurDataset.R"))
      # Loaded in global.R


      # Connectivity variables
      cxndist_km    <- input$cxndist_km #5
      useHWbonus    <- input$useHWbonus #0 # FALSE (default)
      useBCGbonus   <- input$useBCGbonus #0 # FALSE (default)
      useDownstream <- input$useDownstream #0 # FALSE (default)

      year_max <- input$year_max # 2020
      year_min <- input$year_min # 2008

      # use 


      # put in global
      #not_all_na <- function(x) {!all(is.na(x))}

      # Timer, Start
      startprep.time <- Sys.time()



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
      CopyResults(TargetCOMID)



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
  
  
  output$df_results_DT <- DT::renderDT({
    # should be blank initially and once have data it will appear correctly
    #df_r <- df_results()
    df_r <- read.delim(file.path(".", "Results", "AllScores.tab")
                       , stringsAsFactors = FALSE)
    colnames(df_r) <- gsub("\\.", "<br>", colnames(df_r))
    return(df_r)
  }##expression~END
  , filter="top"
  , caption = "Results summary."
  , options=list(scrollX=TRUE
                 , lengthMenu = c(5, 10, 25, 50, 100, 1000)
                 , autoWidth = TRUE
                 )
  , escape = FALSE
  )## output$df_results_DT ~ END
  
  output$img_report <- renderImage({
    if(file.exists(file.path(".", "Results", "img_report")) == FALSE){
      return(NULL)
    } else {
      return(list(
        src = "Results/img_report.png",
        contentType = "image/png",
        alt = "report images"
      ))
    }## IF ~ END
  }, deleteFile = FALSE
  )## output$report_img ~ END

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
