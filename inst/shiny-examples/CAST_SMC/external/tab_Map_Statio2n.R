# tab_Map_Station
# CAST, SMC

function(){
  tabPanel("Map, Stations"
    , sidebarLayout(
        sidebarPanel(
               helpText("The map can take up to 20 seconds to load.  Please be patient.")
               #, actionButton("map.streams.add", "Add Streams to Map")
               , br()
               , helpText("Use the button (or the map) to select a Station ID.")
               , selectInput("siteid.select", "Select Station ID:"
                             , choices=c("SMC04134", "905S15201", "907S05514", mySites)
                             , selected="SMC04134"
                             #, selected="20331944"# diversion but long and easy to spot on map
               )##selectInput~END 
               
            #    , br()
            #    #, actionButton("zoom.comid", "Zoom to Selected ComID")
            #    , p("After choosing a Station ID click the stream reach to get the Reach ID (COMID)
            # for use with the RPP-Calc tab analysis.")
               
        )##sidebarPanel.END
             
             # Show a plot of the generated distribution
       , mainPanel(
         #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
         p(getwd())
         , p(paste0("Erik, ", Sys.time()))
         #,leaflet::leafletOutput("map_map_x")
         , plotOutput("map_map_y")
         #, p()
         
       )##mainPanel.END
    )##sidebarLayout.END
  )##tabPanel ~ END
}##FUNCTION~END

# vertical height of map
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height