## ----LoadPkg, eval=FALSE-------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("leppott/CASTfxn")

## ----getSiteInfo, echo=TRUE----------------------------------------------
library(CASTfxn)

TargetSiteID <- "SRCKN001.61"
clustertype <- "5"
useLU <- FALSE

CurrentDir<-getwd()
myDir.Data <- paste(CurrentDir,"data/",sep="/")

# data import, example
#data.Stations.Info <- read.delim(paste(myDir.Data,"data.Stations.Info.tab",sep=""))
#data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep="")
#                               , na.strings = c(""," "))
#data.303d.ComID <- readRDS(paste0(myDir.Data,"data.303dcomid.RDS"))
#data.bmi.metrics <- read.delim(paste(myDir.Data,"data.bmi.metrics.tab",sep=""))
#data.algae.metrics <- read.delim(paste(myDir.Data,"data.algae.metrics.tab",sep=""))
#data.cluster <- read.delim(paste(myDir.Data,"data.all.clust.tab",sep=""))
#data.mod <- read.delim(paste(myDir.Data,"data.ModPerStatus.tab",sep=""))

# data, example included with package
data.Stations.Info <- data_Sites
data.SampSummary   <- data_SampSummary
data.303d.ComID    <- data_303d
data.bmi.metrics   <- data_BMIMetrics
data.algae.metrics <- data_AlgMetrics
data.cluster       <- data_Cluster_Hi
data.mod           <- data_ReachMod
 
list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)

## ----OpenJPG, echo=TRUE--------------------------------------------------
#install.packages("imager")
library("imager")
myJPG <- paste0("map.SRCKN001.61.jpg")
img <- load.image(file.path(getwd(), "Results", myJPG))
plot(img)

## ----QCRaw_base, eval=FALSE----------------------------------------------
#  

## ----QCRaw_timeoffset, eval=FALSE----------------------------------------
#  

## ----Aggregate, eval=FALSE-----------------------------------------------
#  
#  

