#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# load required libraries
library(shiny)

myDir.Data <- "data/"
## Stations - PickList
data.Stations <- read.delim(paste(myDir.Data,"data.Stations.LookUp.tab",sep=""))
LU.Stations <- data.Stations[,"StationID"]


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(HTML("Causal Assessment Screening Tool (CAST) <br/> Pilot Test Application")
             ,windowTitle = "Causal Assessment Screening Tool (CAST)"),
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("Station" 
                  ,label = "Choose a station for which to generate outputs"
                  ,choices = LU.Stations
                  ,selected = LU.Stations[1]
                  )
      
      # , actionButton("Analysis",label="Run Analsysis",onclick="window.open('Results.ABC-01.PDF')")
      # 
      # , actionButton("map",label="map",onclick="window.open('Results.ABC-01.PDF')")
      # , actionButton("table",label="table",onclick="window.open('Results.ABC-01.PDF')")
      # , actionButton("clusters",label="clusters",onclick="window.open('Results.ABC-01.PDF')")
      # , actionButton("stressor",label="list stressors",onclick="window.open('Results.ABC-01.PDF')")
      # 
      # , actionButton("modstreams",label="modified streams",onclick="window.open('Results.ABC-01.PDF')")
      # , actionButton("Listing",label="303(d) Listing with Reason",onclick="window.open('Results.ABC-01.PDF')")
      # 
      # , actionButton("pdf",label="Open Results",onclick=paste("window.open('Results.ABC-01.PDF')")
      
      # set size of sidebar (out of 12)
      ,width=2
      # make file dependant upon input
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Site/Sample Info"
                  ,h3("Site Information")
                  ,tableOutput("Table.SiteInfo")
                  ,h3("Reach Information")
                  ,tableOutput("Table.ReachInfo")
                  ,h3("Samples")
                  ,h4("Samples, Number")
                  ,tableOutput("Table.Samps.N")
                  ,h4("Samples, List")
                  #,dataTableOutput("Table.Samps")
        )
        ,tabPanel("Site Info (ALL)"
                ,h3("Station")
                ,textOutput("StationID")
                ,tableOutput("Table.Station")
                 #,h4(output$StationID)
                 # display site info
                 #data.Stations.Selected <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,]
                 ,h3("COMID (NHD+ v2)")
                ,textOutput("COMID")
                ,dataTableOutput("Table.Eco85")
                )
        #
        ,tabPanel("Location (Site)"
                  ,h3("Map Location, Selected and Reference Site(s)")
                  ,plotOutput("plot.location.wRef")
                  #,plotOutput("plot.location")  # site only
        )
        # Panel 1
        ,tabPanel("Cluster"
                  ,h3("Identify Cluster")
                  ,tableOutput("Table.ClusterIDs")
                  #,imageOutput("image.cluster.char")
                  ,tableOutput("Table.Clusters")
                  #,imageOutput("image.plot.box.clusters")
                  ,plotOutput("plot.cluster.box")
                  )
        # Panel 2
        ,tabPanel("Stressors"
                  ,h3("Potential Stressors")
                  #,plotOutput("plot.box.stressors")
                  #,imageOutput("image.cluster.stressors") # old
                  )

        #
        ,tabPanel("Location (Clusters)"
                  ,h3("Map Location, Clusters")
                  ,imageOutput("image.map.eco85.clusters")
                  ,imageOutput("image.map.ComID")
        )
        #
        ,tabPanel("Taxa Response"
                  ,h3("Calculated Response Values by Taxa Present in Sample")
                  ,plotOutput("plot.bar.TaxResponse")
                  ,dataTableOutput("Table.Taxa")
                  # no matches for selected sites
                  # ,tabPanel("Modified"
                  #           ,h3("CSCI, Modified, and Perennial")
                  #           ,tableOutput("Table.Mod")
        )           
        #
        ,tabPanel("303(d) Listing"
                  ,h3("303d Listings Status")
                  #,h3("table")
                  ,dataTableOutput("Table.303d")
                  )

        ,tabPanel("ChemDataSubsets"
                   ,h3("Chemical Data Subsets")
                  )
        ,tabPanel("StressorList"
                  ,h3("Stressors")
                  )
        ,tabPanel("BMI"
                  ,h3("Samples, Number")
                  ,tableOutput("Table.Samps.N.BMI")
                  ,h3("Metrics")
                  ,dataTableOutput("Table.Metrics.BMI")
                  )
        ,tabPanel("BMIStressorResp"
                  ,h3("BMI Stressor Responses")
                  )
        ,tabPanel("Algae"
                  ,h3("Samples, Number")
                  ,tableOutput("Table.Samps.N.Alg")
                  ,h3("Metrics")
                  ,dataTableOutput("Table.Metrics.Alg")
                  )
        ,tabPanel("AlgStressResp"
                  ,h3("Algae Stressor Responses")
                  )
        ,tabPanel("Modified Status"
                  ,h3("Modified Status")
                   ,h4("Site")
                   ,tableOutput("Table.Status.Modified.Site")
                   ,h4("Reach")
                   ,tableOutput("Table.Status.Modified.Reach")
                   ,h4("Map")
                   ,imageOutput("image.map.modified")
                  )
        ,tabPanel("Flow Status"
                  ,h3("Flow Status")
                   ,h4("Site")
                   ,tableOutput("Table.Status.Flow.Site")
                   ,h4("Reach")
                   ,tableOutput("Table.Status.Flow.Reach")
                   ,h4("Map")
                   ,imageOutput("image.map.flowstatus")
        )
        ,tabPanel("SSD"
                  ,h3("SSD")
                  ,imageOutput("image.SSD")
                  )
        
        # Panel 3
        ,tabPanel("Output",
                  #h3("Results File")
                  #,h4(textOutput("FileName"))
                  #,h4(textOutput("myTXT.onclick"))
                  #, actionButton("pdf",label="Open Results (variable)",onclick=paste("window.open('","FileName","')",sep=""))
                  #, actionButton("pdf",label="Open Results (var)",onclick=paste("window.open('",textOutput("FileName"),"')",sep=""))
                  h3("Results PDF")
                  , actionButton("SavePDF",label="Save Results PDF",onclick=paste("window.open('","Results.PDF","')",sep=""))
                  , actionButton("OpenPDF",label="Open Results PDF",onclick=paste("window.open('","Results.PDF","')",sep=""))
        )
      )
                  
    )
  )
))
