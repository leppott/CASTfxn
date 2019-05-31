#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Load all items for "all sessions" in server.R outside of call to "shinyServer()"
#
# load dependencies
library(shiny)
library(reshape)
library("readxl")
library(rpart)
library(cluster)
library(maps)
library(RColorBrewer)
library(rgdal)
library(maptools)
library(RgoogleMaps)
library(rgeos)
library(raster)
library(dplyr)
library(ggplot2)

# directory
#myDir <- getwd()
#myDir.Data <- paste(myDir,"data",sep="/")
myDir.Data <- "data/"

# data need for operation
## Stations - PickList
data.Stations <- read.delim(paste0(myDir.Data,"data.Stations.LookUp.tab"))
LU.Stations <- data.Stations[,"StationID"]
#myX <- "Results.TEST.pdf"
## Stations - Location Information
data.Stations.Info <- read.delim(paste0(myDir.Data,"data.Stations.Info.tab"))
## Eco85
data.eco85 <- read.delim(paste0(myDir.Data,"data.eco85.tab"))
## Modified Status
data.mod <- read.delim(paste0(myDir.Data,"data.SanDiego.BioIndex.ModPerStatus.tab"))
## Cluster Results
data.results.cluster <- read.delim(paste0(myDir.Data,"data.allcomid.tab"))
## data file
newdata <- read.csv("Results/allcomid_newdata.csv")
#
#chem.tab <- read.csv("Results/chem.csv")  #causing error online.  Comment out.
#
# # Read once and then save
# data.303d.ComID <- read.csv(paste0(myDir.Data,"data.303dcomid.csv"))#,colClasses="character")
# # faster with other packages [data.table and readr] (5.91 sec and 7.41 sec vs. 14.95 sec)
# data.303d.ComID <- fread(paste0(myDir.Data,"data.303dcomid.csv")) # package "data.table", 5.91 sec
# data.303d.ComID <- read_delim(paste0(myDir.Data,"data.303dcomid.csv"),delim=",") # package "readr", 7.41 sec
# # save as RDS (binary so is faster to load (0.21 sec) and saves space 2.95 MB vs. 207 MB)
# saveRDS(data.303d.ComID,paste0(myDir.Data,"data.303dcomid.RDS"))
data.303d.ComID <- readRDS(paste0(myDir.Data,"data.303dcomid.RDS"))
#
# taxa
data.taxa <- read_excel(paste0(myDir.Data,"data.taxa.xlsx"),sheet=1)
# taxa response
data.taxa.response <- read_excel(paste0(myDir.Data,"data.taxa.xlsx"),sheet=2)
#
# Modified and Flow Status (Tt2015)
data.modperstatus <- read.delim(paste0(myDir.Data,"data.ModPerStatus.tab"))
#
## Data summary by site
data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep=""),na.strings = c(""," "))
## all cluster data (COMID, cluster assignments, and predictors)
#data.cluster <- read.delim(paste(myDir.Data,"data.all.clust.tab",sep=""))
data.cluster <- readRDS(paste0(myDir.Data,"data.all.clust.RDS"))
## Stressor data
data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
#data.chem.raw <- readRDS(paste0(myDir.Data,"data.chem.raw.RDS"))
#data.chem.raw <- read.table(file=(paste(myDir.Data,"data.chem.raw.tab",sep="")), header=TRUE, sep="\t", nrows=109266)
#dim(data.chem.raw)
data.phab.raw <- read.delim(paste0(myDir.Data,"data.phab.raw.tab"))
## Stressor group data (lookup values)
#data.taxa.response <- read_excel(paste0(myDir.Data,"data.taxa.xlsx"),sheet=2)
data.chem.info <- read.delim(paste0(myDir.Data,"data.chem.info.tab"))
data.phab.info <- read.delim(paste0(myDir.Data,"data.phab.info.tab"))
## Response data (BMI/Algae Indices/Metrics)
data.algae.metrics <- read.delim(paste0(myDir.Data,"data.algae.metrics.tab"))
data.bmi.metrics <- read.delim(paste0(myDir.Data,"data.bmi.metrics.tab"))
data.SampSummary <- read.delim(paste0(myDir.Data,"data.SampSummary.tab"), na.strings = c(""," "))
## Raw taxa data (abundance in each sample)
data.bmi.taxa.raw <- read.delim(paste0(myDir.Data,"data.bmi.taxa.raw.tab"))
## Taxonomic information (BMI) (Master taxa table)
data.bmi.info <- read.delim(paste0(myDir.Data,"data.bmi.info.tab"))
## Modified Status     ##This is reach-based mod/flow status. Need Site based, too
data.mod.site <- read.delim(paste0(myDir.Data,"data.SanDiego.BioIndex.ModPerStatus.tab"))
data.mod.reach <- read.delim(paste0(myDir.Data,"data.ModPerStatus.tab"))
## SSD data

# Functions
source(paste0("scripts/","CASTfxns_production.R"))

################################
# Define server logic required to draw a histogram
shinyServer(function(input, output) {##ShinyServer.START
  #
  # define LU.Stations for UI
  output$Stations.LU <- reactive({
    #data.Stations <- read.delim(paste(myDir.Data,"data.Stations.LookUp.tab",sep=""))
    #LU.Stations <- data.Stations[,"StationID"]
    })
  #
  output$Stations.LU.Default <- reactive({
    # data.Stations <- read.delim(paste(myDir.Data,"data.Stations.LookUp.tab",sep=""))
    # LU.Stations <- data.Stations[,"StationID"]
    # LU.Stations[1]
  })

  ########
  # PDFs
  # generate PDF filename based on user selection (Station)
  output$FileName <- reactive({
    paste("Results",input$Station,"pdf",sep=".")
  })
  #
  output$myTXT.onclick <- reactive({
    paste("window.open('Results.",input$Station,".pdf')",sep="")
  })
  #
  output$StationID <- reactive({input$Station})

  ##########
  # Plots
  output$plot.location <- renderPlot({
    df.plot <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,]
    myState = "California"
    map('state',region=myState)
    myLatitude <- "FinalLatitude"
    myLongitude <- "FinalLongitude"
    points(df.plot[,myLongitude], df.plot[,myLatitude], col="red", pch=17, cex=1.25)
    }
  )
  #
  output$plot.location.wRef <- renderPlot({
    df.plotSite <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,]
    data.refSites <- subset(data.Stations.Info,CARefSite_2017==1,
                            select= c(StationID_Master,FinalLatitude,FinalLongitude,COMID_NHD2))
    myState = "California"
    map('state',region=myState)
    myLatitude <- "FinalLatitude"
    myLongitude <- "FinalLongitude"
    myLegend <- c("Selected Site","Reference Sites")
    myCol <- c("red","blue")
    myPch <- c(17,19)
    myCex <- 1.25
    # plot Ref first so Selected site is always on top
    points(data.refSites[,"FinalLongitude"], data.refSites[,"FinalLatitude"], col=myCol[2], pch=myPch[2], cex=myCex)
    points(df.plotSite[,myLongitude], df.plotSite[,myLatitude], col=myCol[1], pch=myPch[1], cex=myCex)
    legend("topright",legend=myLegend,col=myCol,pch=myPch)
    }
  )
  #
  output$plot.cluster.box <- renderPlot({
    # newdata subset for mySites
    myCOMID <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,"COMID_NHD2"]
    newdata.mySites <- newdata[newdata$COMID %in% myCOMID,]
    selvar <- c("W_AREA_KM","W_RD_DENSE","W___URBAN","Temp","SLOPE","MAXELEVSMO")
    varnames <- c("lg10 W_Area", "Road Density","%Urban", "Air Temp", "Slope", "Elevation_max")
    par(mfrow = c(2,3), mar = c(2,4,1,1))
    df.plot <- newdata
    df.plot.2 <- newdata.mySites
    for(ii in 1:length(selvar)) {
      myY <- df.plot[,selvar[ii]]
      myX <- df.plot[,"HGroup"]
      boxplot(myY~myX, xlab ="Cluster", ylab = varnames[ii], boxwex = 0.5, col ="lightgray")
      ##########
      # add points to plots for selected sites
      myY <- df.plot.2[,selvar[ii]]
      myX <- df.plot.2[,"HGroup"]
      points(myX,myY,col="red",cex=1.5,pch=19)
      ############
    }
    }
  )
  #
  # output$plot.box.stressors <- renderPlot({
  #   stations <- input$Station
  #   selsub <- subset(chem.tab,  StationID_Master %in% stations)#; dim(selsub) #
  #   for(index in 1:nrow(selsub)) {##FOR.index.START
  #     selsub3 <- subset(selsub, StationID_Master== stations[index])
  #     chem.tab3 <- subset(chem.tab, HGroup ==selsub3$HGroup)#; dim(chem.tab3)
  #     ############# count
  #     allcount <- apply(chem.tab3, 2, function(x) sum(!is.na(x)))
  #     alltype <- unlist(lapply(1:ncol(chem.tab3), function(x) is.numeric(chem.tab3[,x])))
  #     selcount <- apply(selsub3, 2, function(x) sum(!is.na(x)) )
  #     naturalvar <- c("LengthReachTotal_m","LengthSegmentMain_m" )
  #     coolvar <- names(allcount)[allcount>2 & selcount>0&alltype& (!names(allcount)%in% naturalvar)]
  #     #
  #     n <- length(coolvar)# 10
  #     if(n>5) {##FOR.n.START
  #       # png(file = paste0("Results/boxes.example.",stations[index], ".png"),
  #       #     width = 550, height = 480, pointsize = 13)
  #       par(mfrow = c(1,1), mar = c(4,8,1,1))
  #       plot(y= 1:n, x= runif(n,0,1), axes = F, type="n", xlab = "Standardized Values", ylab ="",
  #            xlim = c(0,1), main = paste(stations[index], ": Cluster",selsub3$HGroup))
  #       axis(1, at = seq(0,1, 0.2),labels = seq(0,1, 0.2))
  #       axis(2, at = 1:n, labels = coolvar[1:n], las =1, cex.axis = .6)
  #       for(i in 1:n) {
  #         xvar <- chem.tab3[,coolvar[i]]; dif <- diff(range(xvar, na.rm =T))
  #         newvar <- (xvar-min(xvar, na.rm=T))/dif
  #         boxplot(newvar, at = i,boxwex=.5, horizontal =T, add =T,axes = F)
  #         point1 <- (selsub3[,coolvar[i]]-min(xvar, na.rm=T))/dif #; print(point1)
  #         points(point1, rep(i,length(point1)), col = 1, pch = 22,cex=1.4, bg = 2)
  #       }
  #       box(bty="l")
  #       # graphics.off()
  #     }##IF.n.END
  #   }##FOR.index.END
  # })
  #
  output$plot.bar.TaxResponse <- renderPlot({
    myStation <- input$Station
    data.plot <- data.taxa.response[data.taxa.response$StationID_Master==myStation,]
    #
    myCols <- c("1","2","3","4","5","NA")
    myResponseVars <- unique(data.plot$Response.Variable)
    #
    par(mfrow = c(2,3))#, mar = c(4,8,1,1))
    for (i in 1:5){##FOR.i.START
      #
      data.plot.response <- data.plot[data.plot$Response.Variable==myResponseVars[i],]
      #
      barplot(unlist(as.list(data.plot.response[,myCols]))
              ,main=myResponseVars[i]
              ,xlab="Response Value"
              ,ylab="Number of Taxa")
      #
    }##FOR.i.END
    #
  })

  ##########
  # Images
  # Cluster.Char
  output$image.cluster.char <- renderImage({
    filename <- paste("images/","plot.cluster.chars.jpg",sep="")
                                        #paste('image', input$n, '.jpeg', sep='')))
                                        #'plot.cluster.jpg'))
    list(src=filename,width=800,height=600,alt="plot.cluster.characteristics")
  }, deleteFile = FALSE)
  # Cluster.Stressors
  output$image.cluster.stressors <- renderImage({
    filename <- paste("images/","plot.cluster.stressors.jpg",sep="")
    list(src=filename,width=600,height=600,alt="plot.cluster.stressors")
  }, deleteFile = FALSE)
  # Clusters.Eco85
  output$image.map.eco85.clusters <- renderImage({
    filename <- paste("images/","map.eco85.catch.clusters.jpg",sep="")
    list(src=filename,width=800,height=600,alt="map.eco85.clusters")
  }, deleteFile = FALSE)
  # Map.ComID
  output$image.map.ComID <- renderImage({
    filename <- paste("images/","ComID_Clustering_true.png",sep="")
    list(src=filename,width=800,height=600,alt="comid.clusters")
  }, deleteFile = FALSE)
  # Clusters
  output$image.plot.box.clusters <- renderImage({
    filename <- paste("images/","ClusterBoxPlots.jpg",sep="")
    list(src=filename,width=800,height=600,alt="plot.box.clusters")
  }, deleteFile = FALSE)
  # SSD
  output$image.SSD <- renderImage({
    filename <- paste0("images/","SSD_Permethrin.png")
    list(src=filename,width=800,height=600,alt="SSD.Permethrin")
  }, deleteFile = FALSE)
  # Map.Modified
  output$image.map.modified <- renderImage({
    filename <- paste0("images/","Example_Modified_20331516.png")
    list(src=filename,width=800,height=600,alt="SSD.Permethrin")
  }, deleteFile = FALSE)
  # Map.FlowStatus
  output$image.map.flowstatus <- renderImage({
    filename <- paste0("images/","Example_FlowStatus_20331516.png")
    list(src=filename,width=800,height=600,alt="SSD.Permethrin")
  }, deleteFile = FALSE)

  #######
  # Tables
  output$Table.Station <- renderTable({
    data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,]
  })
  #
  output$COMID <- reactive({
    data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,"COMID_NHD2"]
  })
  #
  output$Table.Eco85 <- renderDataTable({
    myCOMID <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,"COMID_NHD2"]
    data.eco85[data.eco85[,"COMID"]==myCOMID,]
  })
  #
  output$Table.Mod <- renderTable({
    data.mod[data.mod[,"StationCode"]==input$Station,]
  })
  #
  output$Table.Clusters <- renderTable({
    myCOMID <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,"COMID_NHD2"]
    data.results.cluster[data.results.cluster[,"COMID"]==myCOMID,]
  })
  #
  output$Table.ClusterIDs <- renderTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$ClustIDs
  })
  #
  output$Table.303d <- renderDataTable({
    myCOMID <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,"COMID_NHD2"]
    #data.303d.ComID[data.303d.ComID[,"ComID"]==myCOMID,]
    subset(data.303d.ComID,ComID==myCOMID)
  })
  #
  output$Table.Taxa <- renderDataTable(({
    subset(data.taxa,StationID_Master==input$Station)
  }))
  # Site Information (Ann)
  output$Table.SiteInfo <- renderTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$SiteInfo
  })
  # Samples (Ann)
  output$Table.Samps <- renderDataTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$Samps
  })
  # Samples, Number
  output$Table.Samps.N <- renderTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$Samps.N
  })
  # Samples, Number, BMI
  output$Table.Samps.N.BMI <- renderTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$Samps.N[,4]
  })
  # Samples, Number, Alg
  output$Table.Samps.N.Alg <- renderTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$Samps.N[,5]
  })
  # Metrics, BMI
  output$Table.Metrics.BMI <- renderDataTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$MetricsBMI
  })
  # Metrics, Algae
  output$Table.Metrics.Alg <- renderDataTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$MetricsAlg
  })
  # Reach Information
  output$Table.ReachInfo <- renderTable({
    getSiteInfo(input$Station,data.Stations.Info,data.SampSummary,data.bmi.metrics,data.algae.metrics,data.cluster)$ReachInfo
  })
  # Status Modified (Site)
  output$Table.Status.Modified.Site <- renderTable({
    subset(data.mod.site,StationCode==input$Station,select=c("StationCode","Status_Modified_Site"))
  })
  # Status Modified (Reach)
  output$Table.Status.Modified.Reach <- renderTable({
    myCOMID <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,"COMID_NHD2"]
    subset(data.mod.reach,COMID==myCOMID,select=c("COMID","Reach.Mod.Status","Reach.Mod.Status.Reasoning"))
  })
  # Status Flow (Site)
  output$Table.Status.Flow.Site <- renderTable({
    subset(data.mod.site,StationCode==input$Station,select=c("StationCode","Status_Flow"))
  })
  # Status Flow (Reach)
  output$Table.Status.Flow.Reach <- renderTable({
    myCOMID <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==input$Station,"COMID_NHD2"]
    subset(data.mod.reach,COMID==myCOMID,select=c("COMID","Reach.Flow.Status","Reach.Flow.Status.Reasoning"))
  })


  #myResultsPDF <- paste("Results",input$Station,"PDF",sep=".")

 # actionButton("pdf",label="Open Results",onclick=paste("window.open('",myResultsPDF,"')",sep=""))

  # output$distPlot <- renderPlot({
  #
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #
  # })
  
})##ShinyServer.END
