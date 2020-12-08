function(){
  tabPanel("Map, Reach"
   # Sidebar with a slider input for number of bins 
   , sidebarLayout(
       sidebarPanel(
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30)
         helpText("The map can take up to 60 seconds to load.  Please be patient.")
         #, actionButton("map.streams.add", "Add Streams to Map")
         , br()
         , helpText("Use the button (or the map) to view a COMID on the map.")
         , selectizeInput("comid.select" 
                          , label = "Select COMID:"
                          , choices = c(targ_COMID, myComID)
                          , selected = targ_COMID[1]
                          , multiple = FALSE
                          #, option = list(maxOptions = len_sel_COMID)
                          )
         # , selectInput("comid.select", "Select COMID:"
         #               , choices = c(NA, targ_COMID, myComID)
         #               #, selected="20333074")
         #               , selected = targ_COMID[1]) # diversion but long and easy to spot on map
         , br()
         #, actionButton("zoom.comid", "Zoom to Selected ComID")
         
       )##sidebarPanel.END
       
       # Show a plot of the generated distribution
       , mainPanel(
         tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
         leafletOutput("map_reach", height = "85vh")
         #, p()
         
       )##mainPanel.END
    )##sidebarLayout.END  
           
  )##tabPanel~END
}##FUNCTION~END
