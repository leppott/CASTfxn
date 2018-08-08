#' @title Site Info
#' 
#' @description Get site info of provided siteID.
#' 
#' @details Summary info including lat/long, ref status, cluster membership, samps from site, maps
#' 
#' Requires package rgdal. 
#' 
#' Required objects:
#' 
#' * data.Stations.Info; StationID_Master, FinalLatitude, FinalLongitude, WaterbodyName, GIS_County, CARefSite_2017, COMID_NHD2
#' 
#' * data.SampSummary; StationID_Master, CollDate, ChemSampleID, PhabSampID, BMI.Metrics.SampID, Algae.Metrics.SampID
#' 
#' * data.303d.ComID; ComID, WATER.BODY.NAME, POLLUTANT, FINAL.LISTING.DECISION
#' 
#' * data.bmi.metrics; StationID_Master, CollDate, CSCI, O_E, MMI_Score
#' 
#' * data.algae.metrics; StationCode, SampleDate, H20, D18, S2
#' 
#' * data.cluster; COMID, H6_noland, H6_land, ElevWs, WsAreaSqKm, PrecipWs, TmeanWs, W___AGRIC, W___URBAN, W___FOREST
#' 
#' * data.mod; COMID, ReachModStatus, ModReason
#' 
#' @param TargetSiteID SiteID
#' @param clustertype Cluster
#' @param useLU Use LandUse.  Default = FALSE.
#' 
#' @return A jpg map to the "Results" directory of the working directory.  And a summary list; SiteInfo, Samps
#' , BMImetrics, AlgMetrics, ReachInfo, COMID, ClustIDs, impair, and mods.
#' 
#' @examples
#' TargetSiteID <- "SRCKN001.61"
#' clustertype <- "5"
#' useLU <- FALSE
#' 
#' CurrentDir<-getwd()
#' myDir.Data <- paste(CurrentDir,"data/",sep="/")
#' 
#' # data import, example
#' #data.Stations.Info <- read.delim(paste(myDir.Data,"data.Stations.Info.tab",sep=""))
#' #data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep="")
#' #                               , na.strings = c(""," "))
#' #data.303d.ComID <- readRDS(paste0(myDir.Data,"data.303dcomid.RDS"))
#' #data.bmi.metrics <- read.delim(paste(myDir.Data,"data.bmi.metrics.tab",sep=""))
#' #data.algae.metrics <- read.delim(paste(myDir.Data,"data.algae.metrics.tab",sep=""))
#' #data.cluster <- read.delim(paste(myDir.Data,"data.all.clust.tab",sep=""))
#' #data.mod <- read.delim(paste(myDir.Data,"data.ModPerStatus.tab",sep=""))
#' 
#' # data, example included with package
#' data.Stations.Info <- data_Sites
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.cluster       <- data_Cluster_Hi
#' data.mod           <- data_ReachMod
#'
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#
#' @export
getSiteInfo <- function(TargetSiteID, clustertype, useLU = FALSE) {
  #
  # check for and create (if necessary) "Results" subdirectory of working directory
  wd <- getwd()
  dir.sub <- "Results"
  dir.sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  #
  mySiteInfo <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
                                   ,c("FinalLatitude","FinalLongitude","WaterbodyName"
                                      ,"GIS_County","CARefSite_2017","COMID_NHD2"
                                      ,"ElevCategory")]
  data.refSites <- subset(data.Stations.Info, CARefSite_2017==1,
                          select= c(StationID_Master,FinalLatitude,
                                    FinalLongitude,COMID_NHD2))
  #nolu.cluster <- paste(clustertype, "_noland", sep="")
  #lu.cluster <- paste(clustertype, "_land", sep="")
  
  col.clust.land.no <- "clust_noland"
  col.clust.land.yes <- "clust_land"
  
  # get sampling info (dates of samples)
  mySamps <- data.SampSummary[data.SampSummary[,"StationID_Master"]==TargetSiteID
                              ,c("CollDate","ChemSampleID","PhabSampID"
                                 ,"BMI.Metrics.SampID","Algae.Metrics.SampID")]
  # get response information (CSCI, H20, etc)
  myBMImetrics <- data.bmi.metrics[data.bmi.metrics[,"StationID_Master"]==TargetSiteID
                                   ,c("CollDate","IBI")]
  myAlgaeMetrics <- data.algae.metrics[data.algae.metrics[,"StationID_Master"]==TargetSiteID
                                       ,c("CollDate","PollTolClass.1.tot")]
  # get COMID 
  myCOMID <- mySiteInfo$COMID_NHD2
  myWBName <- mySiteInfo$WaterbodyName
  # replaced H6_noland and H6_land with "cluster"
  myReachInfo <- data.cluster[data.cluster[,"COMID"]==myCOMID, c(col.clust.land.no, col.clust.land.yes
                                                                ,"ElevWs","WsAreaSqKm","PrecipWs", "TmeanWs"
                                                                ,"W___AGRIC","W___URBAN","W___FOREST")]
  #myClustIDs <- myReachInfo[,c("H6_noland","H6_land")]
  myClustIDs <- myReachInfo[,c(col.clust.land.no, col.clust.land.yes)]
  
  myReachMods <- data.mod[data.mod[,"COMID"]==myCOMID,c("ReachModStatus", "ModReason")]
  
  my303d.COMID <- subset(data.303d.ComID, data.303d.ComID$ComID == myCOMID)
  my303d.COMID.WBName <- subset(my303d.COMID, my303d.COMID$WATER.BODY.NAME %in% myWBName)
  myCurrent303d <- subset(my303d.COMID.WBName, my303d.COMID.WBName$Year == 2012)
  myImpairments <- myCurrent303d[,c("ComID", "WATER.BODY.NAME", "POLLUTANT",
                                    "FINAL.LISTING.DECISION")]
  
  
  all.map.sites <- merge(data.Stations.Info, data.cluster, by.x = "COMID_NHD2", by.y = "COMID")
  # if (useLU == TRUE) {
  #   df.plot.cl <- all.map.sites[all.map.sites[,lu.cluster]==myClustIDs[,2]
  #                               , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]
  # } else {
  #   df.plot.cl <- all.map.sites[all.map.sites[,lu.cluster]==myClustIDs[,1]
  #                               , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]
  # }
  if (useLU == TRUE) {
    df.plot.cl <- all.map.sites[all.map.sites[,col.clust.land.yes]==myClustIDs[,2]
                                , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]
  } else {
    df.plot.cl <- all.map.sites[all.map.sites[,col.clust.land.no]==myClustIDs[,1]
                                , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]
  }
  
  # Read spatial layers for background
  
  # # San Diego
  # flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
  # outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
  # # AZ
  flowline.hi <- data_GIS_Flow_HI
  flowline.lo <- data_GIS_Flow_LO
  outline <- data_GIS_AZ_Outline
  # 

  # # Project site data to USGS Albers Equal Area
  # usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
  #               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
  #               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # projection for outline
  my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m 
               +no_defs +ellps=GRS80 +towgs84=0,0,0"
  

  df.plotSite <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID,]
  proj.mySite <- rgdal::project(cbind(df.plotSite[,"FinalLongitude"],
                               df.plotSite[,"FinalLatitude"]), my.aea)
  proj.plot.cl <- rgdal::project(cbind(df.plot.cl[,"FinalLongitude"],
                                df.plot.cl[,"FinalLatitude"]), my.aea)
  proj.refSites <- rgdal::project(cbind(data.refSites[,"FinalLongitude"],
                                 data.refSites[,"FinalLatitude"]), my.aea)
  proj.allSites <- rgdal::project(cbind(data.Stations.Info[,"FinalLongitude"],
                                 data.Stations.Info[,"FinalLatitude"]), my.aea)
  # 
  # plot map
  ppi <- 300
  grDevices::jpeg(filename = paste0("Results/",TargetSiteID, "/", TargetSiteID, 
                    ".map.jpg"), width = 4*ppi, height = 4*ppi, pointsize = 6,
                    quality=100, bg="white", res=ppi)
    # lines
    sp::plot(outline, col="white", border="black", lwd=1.5, main=TargetSiteID)
    sp::plot(flowline.hi, add = TRUE, col="light blue", lwd=0.5)
    sp::plot(flowline.lo, add = TRUE, col="light blue", lwd=0.5)
    # points
    graphics::points(proj.allSites[,1], proj.allSites[,2], col="darkgray", pch=19, cex=0.3)
    graphics::points(proj.plot.cl[,1], proj.plot.cl[,2], col="cyan3", pch=19, cex=0.9)
    graphics::points(proj.refSites[,1], proj.refSites[,2], col="blue", pch=21, cex=1)
    graphics::points(proj.mySite[,1], proj.mySite[,2], col="red", pch=17, cex=1.2)
    # legend; items not the same size but ok.
    graphics::legend("bottomleft", legend = c("State", "flowline", "all sites", "cluster sites", "ref sites", "target site")
                             , col = c("black", "light blue", "darkgray", "cyan3", "blue", "red")
                             , lty = c(1, 1,  rep(NA, 4))
                             , pch = c(NA, NA, 19, 19, 21, 17)
                             , title = "Legend")
  grDevices::dev.off()
  
  #
  mySiteSummary <- list(SiteInfo = mySiteInfo, Samps = mySamps, BMImetrics = myBMImetrics
                        , AlgMetrics = myAlgaeMetrics, ReachInfo = myReachInfo
                        , COMID = myCOMID, ClustIDs = myClustIDs, impair = myImpairments
                        , mods = myReachMods)
  return(mySiteSummary)
}
