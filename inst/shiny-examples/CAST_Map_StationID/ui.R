#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#library(shiny)
source("global.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define UI for application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("SEP - CAST - Station ID Selection")

  # Sidebar with a slider input for number of bins 
  , sidebarLayout(
     sidebarPanel(
       # sliderInput("bins",
       #             "Number of bins:",
       #             min = 1,
       #             max = 50,
       #             value = 30)
      helpText("The map takes 20 seconds to load.  Please be patient.")
      #, actionButton("map.streams.add", "Add Streams to Map")
      , br()
      , helpText("Use the button (or the map) to select a Station ID.")
      , selectInput("siteid.select", "Select Station ID:"
                    , choices=c("SMC04134", "905S15201", "907S05514", mySites)
                    , selected="SMC04134"
                    #, selected="20331944"# diversion but long and easy to spot on map
                    ) 
                            
      , br()
      #, actionButton("zoom.comid", "Zoom to Selected ComID")
      , p("After choosing a Station ID to access the CAST app click the link below.
          Remember your Station ID as this is not transfered between apps.")
      , a("https://leppott.shinyapps.io/CAST_SMC", href="https://leppott.shinyapps.io/CAST_SMC")
      
    )##sidebarPanel.END
    
    # Show a plot of the generated distribution
    , mainPanel(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map")
      #, p()
      
    )##mainPanel.END
  )##sidebarLayout.END
))##shinyUI.END
