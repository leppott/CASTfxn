#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Packages
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Need for console messages to Shiny
  shinyjs::useShinyjs()
  
  # titlePanel ####
  # Application title
  , titlePanel(HTML("Causal Assessment Screening Tool (CAST) <br/> Test Application, v3")
             ,windowTitle = "Causal Assessment Screening Tool (CAST)")
  
  # sidebarLayout ####
  # Sidebar with a slider input for number of bins 
  , sidebarLayout(
      sidebarPanel(
        selectInput("Station"
                    , label = "Choose a station for which to generate outputs"
                    , choices = LU.Stations
                    , selected = LU.Stations[1]
        )
        #
        , selectInput("BioComm"
                      , label = "Choose biological community"
                      , choices = c("bmi", "algae")
                      , selected = "bmi")
        
        , actionButton("b_RunAll", "Run CAST")
        
        , br()
        #, textOutput("boo_zip")
        , downloadButton("b_downloadData", "Download Results") 
        
        # set size of sidebar (out of 12)
        #, width=3 # can invoke "ERROR: [uv_write] broken pipe"
      )##sidebarPanel~END
     
    # mainPanel ####
    # Show a plot of the generated distribution
    , mainPanel(
      # tabsetPanel ####
      tabsetPanel(
        # 0
        tabPanel("Console"
                 , h3("Console")
                 , textOutput("text_console_ALL")
                 )##tabPanel~Console~END
        # 1
        , tabPanel("Site Info"
                 , h3("Map")
                 , p("If the map file exists it will be displayed below.  If it doesn't then click the button to create it.")
                 , textOutput("StationID")
                 , textOutput("fn_Map")
                 , textOutput("fe_Map")
                 , p("Advanced options = none")
                 , actionButton("Create01Map", "Create Map")
                 , textOutput("text_console_Map")
                 #, includeHTML(file.path(".", "Results", "SRCKN001.61", "SiteInfo", paste0("SRCKN001.61", ".map.leaflet.html")))
                 #, includeHTML(output$fn_Map)
                 , uiOutput("Map_html")
                 # check MBSS shiny app for ideas on how to display reactively
                 )##tabPanel~Site Info~END
        # 2
        , tabPanel("Cluster Info"
                   , h3("Cluster Info")
                  # , imageOutput("img_Cluster")
                  , p("Advanced options = TBD")
                  , actionButton("Create02ClusterInfo", "Create Cluster Info")
                  , textOutput("txt_console_Cluster")
                   )##tabPanel~Cluster Info~END
        # 3
        , tabPanel("Candidate Causes"
                   , h3("Candidate Causes")
                   , p("Advanced options = probsLow, probsHigh, biocomm")
                   , actionButton("Create03CandidateCauses", "Create Candidate Causes")
                   , textOutput("txt_console_Candidate")
                   )##tabPanel~Candidate Causes~END
        # 4
        , tabPanel("Co-Occurrence"
                   , h3("Co-Occurrence")
                   , p("Advanced options = Bio, Stressors, biocomm, index labels and break points")
                   , actionButton("Create04CoOccur", "Create Co-Occurrence")
                   , textOutput("txt_console_CoOccur")
                   )##tabPanel~Co-Occurrence~END
        # 5
        , tabPanel("Stressor Response"
                   , h3("Stressor Response")
                   , p("Advanced options = probsLow, probsHigh, biocomm, BioResp")
                   , actionButton("Create05BioStressorResponses", "Create Stressor Responses")
                   , textOutput("txt_console_SR")
                   )##tabPanel~Stressor Response~END
        # 6
        , tabPanel("Verified Predictions"
                   , h3("Verified Predictions")
                   , p("Advanced options = TBD")
                   , actionButton("Create06VerifiedPredictions", "Create Verified Predictions")
                   , textOutput("txt_console_VP")
                  )##tabPanel~Verified Predictions~END
        # 7
        , tabPanel("Results"
                   # , h3("Results Download")
                   # , p("Use the button below to package the Results as a single zip file.")
                   # , actionButton("CreateZip", "Create single zip file")
                   # , p("Use the button below to download Results as a zip file.")
                   # , downloadButton("downloadData", label = "Download")
                   # , downloadButton("downloadData_Test", label = "Download Test")
                   )##tabPanel~Output~END
        )##tabsetPanel~END
      )##mainPanel~END
  )##sidebarLayout~END
))##shinyUI~END
