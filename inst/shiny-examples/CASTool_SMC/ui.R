#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Packages
#library(shiny)

# Source ####
source("global.R")
# Load files for individual screens
tab_Disclaimer  <- source("external/tab_Disclaimer.R", local=TRUE)$value
tab_Map_Station <- source("external/tab_Map_Station.R", local=TRUE)$value
tab_CAST_Calc   <- source("external/tab_CAST_Calc.R", local=TRUE)$value
tab_Help        <- source("external/tab_Help.R", local=TRUE)$value

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Need for console messages to Shiny
  shinyjs::useShinyjs()
  , navbarPage(title = div(img(src="CASTool.png", height = 35), "Causal Assessment Screening Tool (CAST), SMC")
  #, navbarPage(title = "Causal Assessment Screening Tool (CASTool), SMC 2020-09-01"
  #, navbarPage(paste0("Causal Assessment Screening Tool (CASTool), SMC v0.1.0.9251, Test run time = ", Sys.time())
             , theme = "bootstrap.css"
             , inverse = TRUE
             , tab_Disclaimer()
             , tab_Map_Station()
             , tab_CAST_Calc()
             , tab_Help()
  )## navbarPage~ END
  
 #  # titlePanel ####
 #  # Application title
 #  , titlePanel(HTML("Causal Assessment Screening Tool (CASTool) <br/> version: SMC 2020-06-02")
 #             ,windowTitle = "Causal Assessment Screening Tool (CASTool)")
 #  
 #  # sidebarLayout ####
 #  # Sidebar with a slider input for number of bins 
 #  , sidebarLayout(
 #      sidebarPanel(
 #        helpText("The map takes 20 seconds to load.  Please be patient.")
 #        , br()
 #        , helpText("Use the button (or the map) to select a Station ID.")
 #        , selectInput("siteid.select", "Select Station ID:"
 #                      , choices=c("SMC04134", "905S15201", "907S05514", mySites)
 #                      , selected="SMC04134"
 #                      #, selected="20331944"# diversion but long and easy to spot on map
 #        ) 
 #        
 #        , br()
 #        #, actionButton("zoom.comid", "Zoom to Selected ComID")
 #        , p("After choosing a Station ID to access the CAST app click the link below.
 #          Remember your Station ID as this is not transfered between apps.")
 #        , a("https://leppott.shinyapps.io/CAST_SMC", href="https://leppott.shinyapps.io/CAST_SMC")
 #        
 #        # uiOutput("URL_Shiny_Map") # works but not as nice as a straight link
 #        # , p("Dir_input = .Data")
 #        # , p("dir_output = .Results")
 #        # , p("Input and output folders are show below.")
 #        # , p("To change click the buttons below before choosing a station.")
 #        # , verbatimTextOutput("dir_user_input_path")
 #  #     , shinyDirButton('dir_user_input', 'Input directory', 'Select the folder with input files')
 #      # , shinyDirButton("directory", "Folder select", "Please select a folder")
 #       
 #        # , verbatimTextOutput("directorypath")
 # 
 #        #, p(file.path(".", "Data"))
 # #      , shinyDirButton('dir_user_output', 'Output directory', "Select the folder to contain 'Results' subfolder")
 #        
 #        #, p(file.path("."))
 #        #, hr()
 #        
 #        , h3("Disclaimer")
 #        , p("The CAST is a Causal Assessment Screening Tool.  Its intended to assist 
 #        the City and its agents to rapidly screen and identify the causes of biological 
 #        impairment in a given waterbody, and thereby assist with prioritizing restoration 
 #        and protection actions.  Due the CAST's reliance on currently available and sometimes 
 #        incomplete data, however, it is not intended to be a final arbiter on causal assessments 
 #        analyses, nor does it represent any commitments by the City to perform any specific
 #        projects, studies, or other actions.")
 #        , hr()
 #        , h4("User Selection:")
 #        , selectInput("Station"
 #                    , label = "Choose a Station ID below for which to generate outputs."
 #                    , choices = LU.Stations
 #                    , selected = LU.Stations[1]
 #        )
 # 
 #        , p("If Station ID is not known then click the link below to the mapping app to find your desired Station ID.")
 #        #, a("Shiny Station ID Selection Map", href="https://leppott.shinyapps.io/CAST_Map_StationID")
 #        #, hr()
 #        , a("https://leppott.shinyapps.io/CAST_Map_StationID", href="https://leppott.shinyapps.io/CAST_Map_StationID")
 # 
 #        , hr()
 # 
 #        #
 #        # , selectInput("BioComm"
 #        #               , label = "Choose biological community"
 #        #               , choices = c("bmi", "algae")
 #        #               , selected = "bmi")
 #        , p("Click the button below to generate outputs.")
 #        , p("After clicking the button results will appear to the right in tabs by output type.")
 #        , actionButton("b_RunAll", "Run CAST")
 #        
 #        , hr()
 # 
 #        #, textOutput("boo_zip")
 #        , p("Click the button below to download a zip file of all result outputs.")
 #        , p("It will not be active until results are ready.")
 #        , downloadButton("b_downloadData", "Download Results") 
 # 
 #        # set size of sidebar (out of 12)
 #        #, width=3 # can invoke "ERROR: [uv_write] broken pipe"
 #      )##sidebarPanel~END
 #     
 #    # mainPanel ####
 #    # Show a plot of the generated distribution
 #    , mainPanel(
 #      # tabsetPanel ####
 #      tabsetPanel(id = "tsp_Main"
 #        # 0.0
 #        , tabPanel(title = "Console", value = "pan_console"
 #                   , h3("Console")
 #                   , p("During the running of the tool any messages or warnings that would be displayed in the R console
 #                     are displayed below.")
 #                   ,p("In addition to any text below there is a progress bar in the lower right.")
 #                   , textOutput("text_console_ALL")
 #        )##tabPanel~Console~END
 #       , tabPanel("Plot Key", value = "pan_legends"
 #                   #, h3("Disclaimer")
 #                   #, p("Screening tool...and key for plots.")
 #                   #, uiOutput("Disclaimer_html")
 #                   , includeHTML(file.path(".", "www", "Legend_Key.html"))
 #                   #, htmlOutput("Disclaimer_html")
 #                  )##tabPanel~Disclaimer~END
 #        # 0.5
 #       
 #      
 #       
 #        )##tabsetPanel~END
 #      )##mainPanel~END
 #  )##sidebarLayout~END
))##shinyUI~END
