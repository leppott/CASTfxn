#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#library(shiny)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define server logic 
shinyServer(function(input, output, session) {##ShinyServer.START
  
  # Stop Shiny App when close browser
  session$onSessionEnded(stopApp)
  
  # palette
  pal.tidal <- colorBin(palette=c("red", "blue"), domain=lines.flowline.proj$LENGTHKM)
  pal.smc   <- colorFactor(palette = "Set3", domain=poly.smc.proj$CUNAME)
  
  # Map
  output$map <- renderLeaflet({
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
                )
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
    leafletProxy("map") %>%
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
    


  })
  
  
 # x <- 1
  

  
  

})##ShinyServer.END
