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
  titlePanel("SEP - CAST")

  # Sidebar with a slider input for number of bins 
  , sidebarLayout(
     sidebarPanel(
       # sliderInput("bins",
       #             "Number of bins:",
       #             min = 1,
       #             max = 50,
       #             value = 30)
      helpText("The map takes 20 seconds to load.")
      #, actionButton("map.streams.add", "Add Streams to Map")
      , br()
      , helpText("Use the button (or the map) to select a COMID for analysis.")
      , selectInput("comid.select", "Select COMID:"
                    , choices=c(NA, myComID)
                    #, selected="20333074")
                    , selected="20331944") # diversion but long and easy to spot on map
                            
      , br()
      #, actionButton("zoom.comid", "Zoom to Selected ComID")
      
    )##sidebarPanel.END
    
    # Show a plot of the generated distribution
    , mainPanel(
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map")
      #, p()
      
    )##mainPanel.END
  )##sidebarLayout.END
))##shinyUI.END
