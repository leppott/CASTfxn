# tab_Map_Station
# RPP, SMC

function(){
  tabPanel("Map, Stations" 
    , sidebarLayout(
      sidebarPanel(
        helpText("The map can take up to 10 seconds to load.  Please be patient.")
        #, actionButton("map.streams.add", "Add Streams to Map")
        , br()
        , helpText("Use the button (or the map) to select a Station ID.")
        , selectInput("siteid.select", "Select Station ID:"
                      , choices=c(targ_SiteID, mySites)
                      , selected="SMC04134"
                      #, selected="20331944"# diversion but long and easy to spot on map
        )##selectInput~END
        
        , br()
        #, actionButton("zoom.comid", "Zoom to Selected ComID")
        , p("After choosing a Station ID above note the Reach ID (COMID) for use with the RPP-Calc tab analysis.
             The COMID will be displayed below and can also be obtained by clicking the stream reach in the map.")
        #, p(paste0("COMID = ", textOutput("Selected_COMID4SiteID")))
        , p(textOutput("Selected_COMID4SiteID"))
        
      )##sidebarPanel.END
      
      # Show a plot of the generated distribution
      , mainPanel(
        #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
        leafletOutput("map_station", height = "85vh")
        #, p()
        
      )##mainPanel.END
    )##sidebarLayout.END
  )##tabPanel ~ END
}##FUNCTION~END

# vertical height of map
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height