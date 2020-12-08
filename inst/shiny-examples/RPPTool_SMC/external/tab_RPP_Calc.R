function(){
  tabPanel("RPPTool-Calc"
           
            # sidebarLayout ####
            # Sidebar with a slider input for number of bins
            , sidebarLayout(
                sidebarPanel(
                  # uiOutput("URL_Shiny_Map") # works but not as nice as a straight link
                  # , p("Dir_input = .Data")
                  # , p("dir_output = .Results")
                  # , p("Input and output folders are show below.")
                  # , p("To change click the buttons below before choosing a station.")
                  # , verbatimTextOutput("dir_user_input_path")
            #     , shinyDirButton('dir_user_input', 'Input directory', 'Select the folder with input files')
                # , shinyDirButton("directory", "Folder select", "Please select a folder")

                  # , verbatimTextOutput("directorypath")

                  #, p(file.path(".", "Data"))
           #      , shinyDirButton('dir_user_output', 'Output directory', "Select the folder to contain 'Results' subfolder")

                  #, p(file.path("."))
                  #, hr()

                   h4("Selection, COMID")
                   , p("If COMID is not known then click the 'Map, Reach' or 'Map, Station' tab above to 
                      use the mapping feature to find your desired COMID.")
                   #, a("Shiny Station ID Selection Map", href="https://leppott.shinyapps.io/CAST_Map_StationID")
                   #, hr()
                   #, a("https://leppott.shinyapps.io/CAST_Map_COMID", href="https://leppott.shinyapps.io/CAST_Map_COMID")
                   , p("COMID selections from maps appear below.")
                   , p(textOutput("Selected_COMIDfromMapStations"))
                   , p(textOutput("Selected_COMIDfromMapReach"))
                   , selectizeInput("COMID_RPP"
                                    , label = "Choose a COMID (stream reach ID) below for which to generate outputs."
                                    , choices = c(targ_COMID, myComID)
                                    , selected = targ_COMID
                                    , multiple = FALSE
                                    , options = list(maxOptions = len_sel_COMID))
                  # , selectInput("COMID_RPP"
                  #             , label = "Choose a COMID (stream reach ID) below for which to generate outputs."
                  #             , choices = c(NA, targ_COMID, myComID)
                  #             , selected = targ_COMID[1]
                  # )
                  , hr()
                  # , p("RPP 'data' and 'results' directories are by default part of the Shiny application.")
                  # , hr()

                  #
                  # , selectInput("BioComm"
                  #               , label = "Choose biological community"
                  #               , choices = c("bmi", "algae")
                  #               , selected = "bmi")
                  , p("Click the button below to generate outputs.")
                  , p("Make any modifications to inputs and settings on the tabs to the right before starting the analysis.")
                  , p("After clicking the button results will appear to the right in tabs by output type.")
                  , actionButton("b_RunAll", "Calculate Results")

                  , hr()

                  #, textOutput("boo_zip")
                  , p("Click the button below to download a zip file of all result outputs.")
                  , p("It will not be active until results are ready.")
                  , shinyjs::useShinyjs() # doesn't work unless in include this line (even though it is in UI.R)
                  , shinyjs::disabled(downloadButton("b_downloadData", "Download Results"))
                  # , downloadButton("b_downloadData", "Download Results")

                  # set size of sidebar (out of 12)
                  #, width=3 # can invoke "ERROR: [uv_write] broken pipe"
                )##sidebarPanel~END

              # mainPanel ####
              # Show a plot of the generated distribution
              , mainPanel(
                # tabsetPanel ####
                  # Main ####
                tabsetPanel(id = "tsp_Main"
                  # 0.0
                  , tabPanel(title = "Console", value = "pan_console"
                             , h3("Console")
                             , p("During the running of the tool any messages or warnings that would be displayed in the R console
                               are displayed below.")
                             ,p("In addition to any text below there is a progress bar in the lower right.")
                             , textOutput("text_console_ALL")
                  )##tabPanel~Console~END
                  # Input, User Crit ####
                  , tabPanel(title = "Input, User Criteria", value = "pan_input_user"
                             , h3("User-Defined Input Criteria")
                             , p("Default criteria are specified below.")
                             , hr()
                             , h4("User Inputs")
                             , numericInput("cxndist_km", "Connectivity distance (km)", value = 5, min = 0)
                             , checkboxInput("useHWbonus", "Use HW bonus?", value = FALSE)
                             , checkboxInput("useBCGbonus", "Use BCG bonus?", value = FALSE)
                             , checkboxInput("useDownstream", "Use downstream reaches?", value = FALSE)
                             , checkboxInput("useModFireHazard", "Use moderate fire hazard?", value = FALSE)
                             
                             , numericInput("year_max", "Maximum year", value = format(Sys.Date(), "%Y"), max = format(Sys.Date(), "%Y"))
                             , numericInput("year_min", "Minimum year", value = lubridate::year(Sys.Date()) - 12)
                             , br()
                             , checkboxInput("useCASTresults", "Use CAST files?", value = FALSE)
                             , p("If using CAST files will need to select and upload files below.")
                             , hr()
                             , h3("User input files from CAST")
                             , p("If want to use CAST files as inputs in the RPP tool then check the box above.")
                             , p("With the buttons below select for upload the directories for CAST 'data' and 'results'.")
                             , br()
                             # , p("Upload, CAST, Data")
                             # , shinyDirButton("dir_CAST_Data", "Choose directory, CAST, Data", "Upload")
                             # , verbatimTextOutput("dir_txt_CAST_Data")
                             , fileInput("upload_CAST_Data", "Select CAST data files", multiple=TRUE
                                         , accept= c("text/tab", ".tab"))
                             , p("Expected files, data")
                             , tableOutput("table_fn_CAST_data")
                             #, hr()
                             # , p("Upload, CAST, Results")
                             # , shinyDirButton("dir_CAST_Results", "Choose directory, CAST, Results", "Upload")
                             # , verbatimTextOutput("dir_txt_CAST_Results")
                             , fileInput("upload_CAST_Results", "Select CAST result files", multiple=TRUE)
                             , p("Expected files, results")
                             , tableOutput("table_fn_CAST_results")
                  )
                  # , tabPanel(title = "Input, CAST files", value = "pan_input_CAST"
                  #            , h3("User input files from CAST")
                  #            , p("If want to use CAST files as inputs in the RPP tool then check the box on the previous tab ('Input, User Criteria').")
                  #            , p("With the buttons below select file locations on C: for CAST 'data' and 'results'.")
                  #            , p("Then click the 'upload' button.")
                  #            , hr()
                  #            , p("Select directory, CAST, Data")
                  #            , shinyDirButton("dir_CAST_Data", "Choose directory, CAST, Data", "Upload")
                  #            , verbatimTextOutput("dir_txt_CAST_Data")
                  #            , hr()
                  #            , p("Select direcory, CAST, Results")
                  #            , shinyDirButton("dir_CAST_Results", "Choose directory, CAST, Results", "Upload")
                  #            , verbatimTextOutput("dir_txt_CAST_Results")
                  #            , hr()
                  #            , p("Expected files by directory")
                  #            , tableOutput("table_fn_CAST")
                  # )
                  # , tabPanel(title = "Input, Weights, Possible Stressors", value = "pan_input_stressors"
                  #           , h3("Weights for Possible Stressors")
                  #           , tableOutput("table_wt_stress_count")
                  #           , actionButton("b_Wts_Str_Reset", "Reset possible stressor weights to '1'")
                  #           , br(), br()
                  #           , actionButton("b_Wts_Import", "Use weights from user import.")
                  #           #, p("Button to use imported file.")
                  #           , hr()
                  #           , h4("Weights, Possible Stressors")
                  #           , sliderInput("wt_hab_evenflow", "Evenness of flow habitat types", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_hab_phi", "Index of physical habitat integrity", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_hab_ripcov", "Riparian cover (sum of three layers)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_modflow_wetmax", "Wet-season maximum mean monthly streamflow (m3/s)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_modflow_avg99", "Average 99th percentile of daily stream flow (m3/s)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_modflow_rbi", "Dry season Richards-Baker Index (flashiness)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_nutr_chla", "Particulate chlorophyll a (mg/m2)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_nutr_no2", "Dissolved nitrate as N (mg/L)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_nutr_p", "Total phosphorus as P (mg/L)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_nutr_n", "Total nitrogen (mg/L)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_wq_alk", "Dissovled alkalinity as calcium carbonate (mg/L)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_wq_do", "Field-measured dissolved oxygen (mg/L", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_wq_cond", "Field-measured specific conductivity (uS/cm)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_wq_tds", "Total dissolved solids (mg/L)", min = 0, max = 2, value = 1)
                  #           , sliderInput("wt_wq_wtemp", "Field-measured water temperature (degrees C)", min = 0, max = 2, value = 1)
                  #           # , h4("Weights, Indicators")
                  #           # , sliderInput("wt_Pot_BCG", label = "wt_Pot_BCG", value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Pot_CxnBCG", label = "wt_Pot_CxnBCG",  value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Pot_Stress", label = "wt_Pot_Stress",  value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Pot_CxnStress", label = "wt_Pot_CxnStress",  value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Threat_Fire", label = "wt_Threat_Fire",  value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Threat_LU", label = "wt_Threat_LU",  value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Opp_ParksNow", label = "wt_Opp_ParksNow",  value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Opp_MSCPs", label = "wt_Opp_MSCPs",  value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Opp_NASVIBCG", label = "wt_Opp_NASVIBCG",  value = 1, min = 0, max = 2)
                  #           # , sliderInput("wt_Opp_UserDefined", label = "wt_Opp_UserDefined",  value = 1, min = 0, max = 2)
                  # 
                  # )
                  # Input, Wts, Ind ####
                  , tabPanel(title = "Input, Weights, Indicators", value = "pan_input_stressors"
                             , h3("Weights for Indicators")
                             , tableOutput("table_wt_indicator_count")
                             , actionButton("b_Wts_Ind_Reset", "Reset indicator weights to '1'")
                             , br(), br()
                             #, actionButton("b_Wts_Import", "Use weights from user import.")
                             #, p("Button to use imported file.")
                             , hr()
                             , fluidRow(column(width = 4
                                               , h4("Potential")
                                               , sliderInput("wt_Pot_BCG", label = "Potential, BCG", value = 1, min = 0, max = 2)
                                               , sliderInput("wt_Pot_CxnBCG", label = "Poential, CxnBCG",  value = 1, min = 0, max = 2)
                                               , sliderInput("wt_Pot_Stress", label = "Potential, Stress",  value = 1, min = 0, max = 2)
                                               , sliderInput("wt_Pot_CxnStress", label = "Potential, CxnStress",  value = 1, min = 0, max = 2)
                                              )## column ~ END
                                        , column(width = 4
                                                 , h4("Threat")
                                                 , sliderInput("wt_Threat_Fire", label = "Threat, Fire",  value = 1, min = 0, max = 2)
                                                 , sliderInput("wt_Threat_LU", label = "Threat, Land Use",  value = 1, min = 0, max = 2))## column ~ END
                                        , column(width = 4
                                                 , h4("Opportunity")
                                                 , sliderInput("wt_Opp_ParksNow", label = "Opportunity, Parks Now",  value = 1, min = 0, max = 2)
                                                 , sliderInput("wt_Opp_MSCPs", label = "Opportunity, MSCPs",  value = 1, min = 0, max = 2)
                                                 , sliderInput("wt_Opp_NASVIBCG", label = "Opportunity, NASVIBCG",  value = 1, min = 0, max = 2)
                                                 , sliderInput("wt_Opp_UserDefined", label = "Opportunity, User Defined",  value = 1, min = 0, max = 2)
                                                )## column ~ END
                                        )## fluidRow ~ END
                            
                  )## tabPanel ~ Indicator Wts ~ END
                  # Input, Wts, SubInd ####
                  # , tabPanel(title = "Input, Weights, Subindices", value = "pan_input_subindices"
                  #            , h3("Weights for Subindices")
                  #            , tableOutput("table_wt_subindex_count")
                  #            , actionButton("b_Wts_Subindex_Reset", "Reset subindex weights to '1'")
                  #            , br(), br()
                  #            #, actionButton("b_Wts_Import", "Use weights from user import.")
                  #            #, p("Button to use imported file.")
                  #            , hr()
                  #            , h4("Weights, Subindices")
                  #            , sliderInput("wt_SubIndex_Pot", label = "Potential Subindex", value = 1, min = 0, max = 2)
                  #            , sliderInput("wt_SubIndex_Threat", label = "Threat Subindex",  value = 1, min = 0, max = 2)
                  #            , sliderInput("wt_SubIndex_Opp", label = "Opportunity Subindex",  value = 1, min = 0, max = 2)
                  #)## tabPanel ~ weights, Sub-Indices ~ END

                  # , tabPanel(title = "Metadata", value = "tab_metadata"
                  #            , h3("RPP Tool Metadata")
                  #            , p("list out terms"))
                  # Results #### 
                  , tabPanel(title = "Results", value = "tab_results"
                             , h3("RPP Tool Results")
                             , p("Use the button in the left side bar to save the Results as a single zip file.")
                             #, p("Not active until calculation has finished.")
                             , p("After calculations are finished the results summary will appear below.")
                             , br()
                             , p("To zoom in to plots use 'Ctrl' and '+' (on Windows) or browser controls.")
                             , hr()
                             # , fluidRow(column(6, imageOutput("imp_map"))
                             #            , column(6, imageOutput("img_scores"))
                             #            )
                             , br(), imageOutput("img_map_cxns")
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br(), imageOutput("img_map")
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , imageOutput("img_scores")
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , br()
                             , hr()
                             , DT::dataTableOutput("df_results_DT")
                             )##tabPanel ~ Results ~ END


                 # , tabPanel("Plot Key", value = "pan_legends"
                 #             #, h3("Disclaimer")
                 #             #, p("Screening tool...and key for plots.")
                 #             #, uiOutput("Disclaimer_html")
                 #             , includeHTML(file.path(".", "www", "Legend_Key.html"))
                 #             #, htmlOutput("Disclaimer_html")
                 #            )##tabPanel~Disclaimer~END
                  # 0.5

                 # # 2
                 # , tabPanel("Results"
                 #            , h3("Results Download")
                 #            , p("Use the button in the left side bar to save the Results as a single zip file.")
                 #            # , actionButton("CreateZip", "Create single zip file")
                 #            # , p("Use the button below to download Results as a zip file.")
                 #            # , downloadButton("downloadData", label = "Download")
                 #            # , downloadButton("downloadData_Test", label = "Download Test")
                 #            , htmlOutput("Results_html")
                 #            )##tabPanel~Results~END
                 #  # 3.1, SiteInfo
                 #  , tabPanel("Site Info"
                 #           , h3("Map")
                 #           , p("If the data exists it will be displayed below.")
                 #           # , textOutput("StationID")
                 #           # , textOutput("fn_Map")
                 #           # , textOutput("fe_Map")
                 #           # , p("Advanced options = none")
                 #           #, actionButton("Create01Map", "Create Map")
                 #           , textOutput("text_console_Map")
                 #           #, includeHTML(file.path(".", "Results", "SRCKN001.61", "SiteInfo", paste0("SRCKN001.61", ".map.leaflet.html")))
                 #           #, includeHTML(output$fn_Map)
                 #           #, uiOutput("Map_html")
                 #           , htmlOutput("Map_html")
                 #           # check MBSS shiny app for ideas on how to display reactively
                 #           )##tabPanel~Site Info~END
                 #  # 3.2,
                 #  , tabPanel("Cluster Info"
                 #             , h3("Cluster Info")
                 #            # , imageOutput("img_Cluster")
                 #            , p("Advanced options = TBD")
                 #           # , actionButton("Create02ClusterInfo", "Create Cluster Info")
                 #            , textOutput("txt_console_Cluster")
                 #             )##tabPanel~Cluster Info~END
                 #  # 3.3, CandidateCauses
                 #  , tabPanel("Candidate Causes"
                 #             , h3("Candidate Causes")
                 #            # , p("Advanced options = probsLow, probsHigh, biocomm")
                 #            # , actionButton("Create03CandidateCauses", "Create Candidate Causes")
                 #             , textOutput("txt_console_Candidate")
                 #             )##tabPanel~Candidate Causes~END
                 #  # 3.4, CoOccurrence
                 #  , tabPanel("Co-Occurrence"
                 #             , h3("Co-Occurrence")
                 #             #, p("Advanced options = Bio, Stressors, biocomm, index labels and break points")
                 #             #, actionButton("Create04CoOccur", "Create Co-Occurrence")
                 #             , textOutput("txt_console_CoOccur")
                 #             )##tabPanel~Co-Occurrence~END
                 #  # 3.5, StressorResponse
                 #  , tabPanel("Stressor Response"
                 #             , h3("Stressor Response")
                 #            # , p("Advanced options = probsLow, probsHigh, biocomm, BioResp")
                 #            # , actionButton("Create05BioStressorResponses", "Create Stressor Responses")
                 #             , textOutput("txt_console_SR")
                 #             )##tabPanel~Stressor Response~END
                 #  # 3.6, VerifiedPredictions
                 #  , tabPanel("Verified Predictions"
                 #             , h3("Verified Predictions")
                 #            # , p("Advanced options = TBD")
                 #            # , actionButton("Create06VerifiedPredictions", "Create Verified Predictions")
                 #             , textOutput("txt_console_VP")
                 #            )##tabPanel~Verified Predictions~END
                 #  # 3.7, TimeSequence
                 #  , tabPanel("Time Sequence"
                 #             , h3("Time Sequence")
                 #            # , p("Advanced options = TBD")
                 #             # , actionButton("Create06VerifiedPredictions", "Create Verified Predictions")
                 #             , textOutput("txt_console_VP")
                 #            )##tabPanel~Time Sequence~END
                 #  # 3.8, WoE
                 #  , tabPanel("Weight of Evidence"
                 #             , h3("Weight of Evidence")
                 #            # , p("Advanced options = TBD")
                 #             # , actionButton("Create06VerifiedPredictions", "Create Verified Predictions")
                 #             , textOutput("txt_console_VP")
                 #            )##tabPanel~WoE~END

                  )##tabsetPanel~END
                )##mainPanel~END
            )##sidebarLayout~END
           
            
           
  )##tabPanel~END
}##FUNCTION~END
