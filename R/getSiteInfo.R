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
#' * data.Stations.Info
#' 
#' * data.SampSumamry
#' 
#' * data.bmi.metrics
#' 
#' * data.algae.metrics
#' 
#' * data.cluster
#' 
#' * data.mod
#' 
#' @param TargetSiteID SiteID
#' @param clustertype Cluster
#' @param useLU Use LandUse.  Default = FALSE.
#' 
#' @return A jpg map to the "Results" directory of the working directory.  And a summary list; SiteInfo, Samps
#' , BMImetrics, AlgMetrics, ReachInfo, COMID, ClustIDs, impair, and mods.
#' 
#' @examples
#' #No example at this time.
#' 
#' @export
getSiteInfo <- function(TargetSiteID, clustertype, useLU = FALSE) {
  #
  mySiteInfo <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
                                   ,c("FinalLatitude","FinalLongitude","WaterbodyName"
                                      ,"GIS_County","CARefSite_2017","COMID_NHD2")]
  data.refSites <- subset(data.Stations.Info,CARefSite_2017==1,
                          select= c(StationID_Master,FinalLatitude,FinalLongitude,COMID_NHD2))
  nolu.cluster <- paste(clustertype, "_noland", sep="")
  lu.cluster <- paste(clustertype, "_land", sep="")
  
  # get sampling info (dates of samples)
  mySamps <- data.SampSummary[data.SampSummary[,"StationID_Master"]==TargetSiteID
                              ,c("CollDate","ChemSampleID","PhabSampID"
                                 ,"BMI.Metrics.SampID","Algae.Metrics.SampID")]
  # get response information (CSCI, H20, etc)
  myBMImetrics <- data.bmi.metrics[data.bmi.metrics[,"StationID_Master"]==TargetSiteID
                                   ,c("CollDate","CSCI","O_E","MMI_Score")]
  myAlgaeMetrics <- data.algae.metrics[data.algae.metrics[,"StationCode"]==TargetSiteID
                                       ,c("SampleDate","H20","D18","S2")]
  # get COMID 
  myCOMID <- mySiteInfo$COMID_NHD2
  myWBName <- mySiteInfo$WaterbodyName
  myReachInfo <- data.cluster[data.cluster[,"COMID"]==myCOMID,c("H6_noland","H6_land"
                                                                ,"ElevWs","WsAreaSqKm","PrecipWs", "TmeanWs"
                                                                ,"W___AGRIC","W___URBAN","W___FOREST")]
  myClustIDs <- myReachInfo[,c("H6_noland","H6_land")]
  
  myReachMods <- data.mod[data.mod[,"COMID"]==myCOMID,c("ReachModStatus", "ModReason")]
  
  my303d.COMID <- subset(data.303d.ComID, data.303d.ComID$ComID == myCOMID)
  my303d.COMID.WBName <- subset(my303d.COMID, my303d.COMID$WATER.BODY.NAME %in% myWBName)
  myCurrent303d <- subset(my303d.COMID.WBName, my303d.COMID.WBName$Year == 2012)
  myImpairments <- myCurrent303d[,c("ComID", "WATER.BODY.NAME", "POLLUTANT",
                                    "FINAL.LISTING.DECISION")]
  
  
  all.map.sites <- merge(data.Stations.Info, data.cluster, by.x = "COMID_NHD2", by.y = "COMID")
  if (useLU == TRUE) {
    df.plot.cl <- all.map.sites[all.map.sites[,lu.cluster]==myClustIDs[,2]
                                , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]
  } else {
    df.plot.cl <- all.map.sites[all.map.sites[,lu.cluster]==myClustIDs[,1]
                                , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]
  }
  
  # Read spatial layers for background
  flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
  outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
  
  # Project site data to USGS Albers Equal Area
  usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
                +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
                +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  df.plotSite <- data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID,]
  proj.mySite <- rgdal::project(cbind(df.plotSite[,"FinalLongitude"], 
                               df.plotSite[,"FinalLatitude"]), usgs.aea)
  proj.plot.cl <- rgdal::project(cbind(df.plot.cl[,"FinalLongitude"], 
                                df.plot.cl[,"FinalLatitude"]), usgs.aea)
  proj.refSites <- rgdal::project(cbind(data.refSites[,"FinalLongitude"], 
                                 data.refSites[,"FinalLatitude"]), usgs.aea)
  proj.allSites <- rgdal::project(cbind(data.Stations.Info[,"FinalLongitude"],
                                 data.Stations.Info[,"FinalLatitude"]), usgs.aea)
  
  # plot map
  ppi <- 300
  grDevices::jpeg(filename = paste0("Results/map.",TargetSiteID, ".jpg"),
       width = 4*ppi, height = 4*ppi, pointsize = 6,
       quality=100, bg="white", res=ppi)
    graphics::plot(outline, col="white", border="black", lwd=1)
    graphics::plot(flowline, add = TRUE, col="light blue", lwd=0.5)
    
    graphics::points(proj.allSites[,1], proj.allSites[,2], col="darkgray", pch=19, cex=0.3)
    graphics::points(proj.plot.cl[,1], proj.plot.cl[,2], col="cyan3", pch=19, cex=0.6)
    graphics::points(proj.refSites[,1], proj.refSites[,2], col="blue", pch=19, cex=0.6)
    graphics::points(proj.mySite[,1], proj.mySite[,2], col="red", pch=17, cex=1.2)
  grDevices::dev.off()
  
  #
  mySiteSummary <- list(SiteInfo = mySiteInfo, Samps = mySamps, BMImetrics = myBMImetrics
                        , AlgMetrics = myAlgaeMetrics, ReachInfo = myReachInfo
                        , COMID = myCOMID, ClustIDs = myClustIDs, impair = myImpairments
                        , mods = myReachMods)
  return(mySiteSummary)
}
