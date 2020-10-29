# tab_Map_Station
# CAST, SMC

function(){
  tabPanel("Map, Stations" 
    , sidebarLayout(
      sidebarPanel(
        helpText("The map can takes 10 - 15 seconds to load.  Please be patient.")
        #, actionButton("map.streams.add", "Add Streams to Map")
        , br()
        , helpText("Use the button (or the map) to select a Station ID.")
        , selectInput("siteid.select", "Select Station ID:"
					  , choices=c(targ_SiteID, mySites)
                      , selected = targ_SiteID[1]
                      #, selected="20331944"# diversion but long and easy to spot on map
        )##selectInput~END
        
        , br()
        #, actionButton("zoom.comid", "Zoom to Selected ComID")
        , p("After choosing a Station ID the map will zoom to its location.")
        
      )##sidebarPanel.END
      
      # Show a plot of the generated distribution
      , mainPanel(
        tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
        , leafletOutput("map_station", height = "85vh")
      )##mainPanel.END
    )##sidebarLayout.END
  )##tabPanel ~ END
}##FUNCTION~END

# vertical height of map
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height