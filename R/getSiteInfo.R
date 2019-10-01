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
#' * data_Sites; StationID_Master, FinalLatitude, FinalLongitude
#' , WaterbodyName, GIS_County, CARefSite_2017, COMID
#' 
#' * data.SampSummary; StationID_Master, CollDate, ChemSampleID, PhabSampID
#' , BMI.Metrics.SampID, Algae.Metrics.SampID
#' 
#' * data.303d.ComID; ComID, WATER.BODY.NAME, POLLUTANT, FINAL.LISTING.DECISION
#' 
#' * data.bmi.metrics; StationID_Master, CollDate, CSCI, O_E, MMI_Score
#' 
#' * data.algae.metrics; StationCode, SampleDate, H20, D18, S2
#' 
#' * data.cluster; COMID, H6_noland, H6_land, ElevWs, WsAreaSqKm, PrecipWs, TmeanWs
#' 
#' * data.mod; COMID, ReachModStatus, ModReason
#' 
#' Will create output folder dir_results if it doesn't already exist.  The default is "Results".  
#' A subdirectory is created for each SiteID.
#' 
#' @param TargetSiteID SiteID
#' @param dir_results Directory for results.  Default = "Results".
#' @param data_Sites data_Sites
#' @param data_SampSummary data_SampSummary
#' @param data_303d data_303d
#' @param data_bmiMetrics data_bmiMetrics
#' @param data_algMetrics data_algMetrics
#' @param data_cluster data_cluster
#' @param data_mods data_mods
#' @param map_proj Map projection.  If no projection is provided an unprojected map is created without flowlines.
#' @param map_outline Outline for map, typically State border.
#' @param map_flowline Typically NHD+ flowline.
#' @param map_flowline2 Typically NHD+ flowline.  Can be more than one but plotted the same.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "SiteInfo"
#' 
#' @return A jpg map to a subdirectory "SiteInfo" in the folder named by the SiteID 
#' in the user supplied dir_results folder (default is "Results" folder in the 
#' working directory).  Also produced is a summary list; SiteInfo, Samps, 
#' BMImetrics, AlgMetrics, ReachInfo, COMID, ClustIDs, impair, and mods.
#' 
#' @examples
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
#' 
#' # Data
#' # data import, example
#' #data_Sites <- read.delim(paste(myDir.Data,"data_Sites.tab",sep=""))
#' #data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep="")
#' #                               , na.strings = c(""," "))
#' #data.303d.ComID <- readRDS(paste0(myDir.Data,"data.303dcomid.RDS"))
#' #data.bmi.metrics <- read.delim(paste(myDir.Data,"data.bmi.metrics.tab",sep=""))
#' #data.algae.metrics <- read.delim(paste(myDir.Data,"data.algae.metrics.tab",sep=""))
#' #data.cluster <- read.delim(paste(myDir.Data,"data.all.clust.tab",sep=""))
#' #data.mod <- read.delim(paste(myDir.Data,"data.ModPerStatus.tab",sep=""))
#' 
#' # Data getSiteInfo
#' # data, example included with package
#' data_Sites <- data_Sites
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.mod           <- data_ReachMod
#' 
#' #' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
#' elev_cat <- toupper(data_Sites[data_Sites[,"StationID_Master"]==TargetSiteID, "ElevCategory"])
#' if(elev_cat=="HI"){
#'    data.cluster <- data_Cluster_Hi
#' } else if(elev_cat=="LO") {
#'    data.cluster <- data_Cluster_Lo
#' }
#' 
#' # Map data
#' # San Diego
#' #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
#' #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
#' # AZ
#' map_flowline  <- data_GIS_Flow_HI
#' map_flowline2 <- data_GIS_Flow_LO
#' if(elev_cat=="HI"){
#'    map_flowline <- data_GIS_Flow_HI
#' } else if(elev_cat=="LO") {
#'    map_flowline <- data_GIS_Flow_LO
#' }
#' map_outline   <- data_GIS_AZ_Outline
#' # Project site data to USGS Albers Equal Area
#' usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#'               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' # projection for outline
#' my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
#'            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' map_proj <- my.aea
#' # 
#' dir_sub <- "SiteInfo"
#'
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID
#'                                 , dir_results
#'                                 , data_Sites
#'                                 , data.SampSummary
#'                                 , data.303d.ComID
#'                                 , data.bmi.metrics
#'                                 , data.algae.metrics
#'                                 , data.cluster
#'                                 , data.mod
#'                                 , map_proj
#'                                 , map_outline
#'                                 , map_flowline
#'                                 , dir_sub=dir_sub)
#
#' @export
getSiteInfo <- function(TargetSiteID
                        , dir_results = file.path(getwd(), "Results")
                        , data_Sites
                        , data_SampSummary
                        , data_303d
                        , data_bmiMetrics=NULL
                        , bmiIndex = "IBI"
                        , data_algMetrics=NULL
                        , algIndex = "IBI"
                        , data_cluster
                        , data_mods
                        , map_proj=NULL
                        , map_outline=NULL
                        , map_flowline=NULL
                        , map_flowline2=NULL
                        , dir_sub="SiteInfo") {
  #
   
    # DEBUG 
    # TargetSiteID
    # dir_results = file.path(wd, "Results")
    # data_Sites = data_Sites
    # data.algae.metrics = data.bmi.metrics
    # map_proj = my.aea
    # map_outline = outline
    # map_flowline = flowline
    # map_flowline2 = NULL
    # dir_sub = "SiteInfo"  

      # check for and create (if necessary) dir_results and SiteID subdirectory
  #wd <- getwd()
  #dir.sub <- "Results"
  dir.sub2 <- TargetSiteID
  dir.sub3 <- dir_sub
  ifelse(!dir.exists(dir_results)==TRUE
         , dir.create(dir_results)
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, dir.sub2))==TRUE
         , dir.create(file.path(dir_results, dir.sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, dir.sub2, dir.sub3))==TRUE
         , dir.create(file.path(dir_results, dir.sub2, dir.sub3))
         , FALSE)
  #
  mySiteInfo <- data_Sites[data_Sites[,"StationID_Master"]==TargetSiteID
                           ,c("FinalLatitude","FinalLongitude","WaterbodyName"
                              ,"GIS_County","CARefSite_2017","COMID"
                              ,"ElevCategory")]
  data_refSites <- subset(data_Sites, CARefSite_2017==1
                          , select= c(StationID_Master, FinalLatitude
                                      , FinalLongitude, COMID))

  # get sampling info (dates of samples)
  mySamps <- data_SampSummary[data_SampSummary[,"StationID_Master"]==TargetSiteID
                              ,c("CollDate","ChemSampleID","PhabSampID"
                                 ,"BMISampID","AlgSampID")]
  # get response information (CSCI, H20, etc)
  if (!is.null("data_bmiMetrics")) {
      myBMImetrics <- data_bmiMetrics[data_bmiMetrics[,"StationID_Master"]==TargetSiteID
                                      ,c("BMISampDate",bmiIndex)]
  }
  if (!is.null("data_algMetrics")) {
      # myAlgaeMetrics <- data.algae.metrics[data.algae.metrics[,"StationID_Master"]==TargetSiteID
      #                                      ,c("CollDate","PollTolClass.1.tot")]
      myAlgaeMetrics <- data_algMetrics[data_algMetrics[,"StationID_Master"]==TargetSiteID,]
  }
  
  # get COMID 
  myCOMID <- mySiteInfo$COMID
  myWBName <- mySiteInfo$WaterbodyName
  myClustID <- as.integer(data_cluster$clust[data_cluster$COMID==myCOMID])

  myReachMods <- data_mods[data_mods[,"COMID"]==myCOMID
                           ,c("ReachModStatus", "ModReason")]
  
  my303d.COMID <- subset(data_303d, data_303d$ComID == myCOMID)
  my303d.COMID.WBName <- subset(my303d.COMID, my303d.COMID$WATER.BODY.NAME %in% myWBName)
  myCurrent303d <- subset(my303d.COMID.WBName, my303d.COMID.WBName$Year == 2012)
  myImpairments <- myCurrent303d[,c("ComID", "WATER.BODY.NAME", "POLLUTANT",
                                    "FINAL.LISTING.DECISION")]
  
  
  all.map.sites <- merge(data_Sites, data_cluster
                         , by.x = c("COMID","clust")
                         , by.y = c("COMID","clust"))
  df.plot.cl <- all.map.sites[all.map.sites[,"clust"]==myClustID
                                , c("FinalLatitude", "FinalLongitude", "CARefSite_2017")]

  # Read spatial layers for background
  
  # # # San Diego
  # # flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
  # # outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
  # # # AZ
  # flowline.hi <- data_GIS_Flow_HI
  # flowline.lo <- data_GIS_Flow_LO
  # outline <- data_GIS_AZ_Outline
  # 

  # # # Project site data to USGS Albers Equal Area
  # # usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
  # #               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
  # #               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # # projection for outline
  # my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m 
  #              +no_defs +ellps=GRS80 +towgs84=0,0,0"
  # map_proj <- my.aea
  

  df.plotSite <- data_Sites[data_Sites[,"StationID_Master"]==TargetSiteID,]
  
  proj.mySite <- rgdal::project(cbind(df.plotSite[,"FinalLongitude"],
                                df.plotSite[,"FinalLatitude"]), map_proj)
  proj.plot.cl <- rgdal::project(cbind(df.plot.cl[,"FinalLongitude"],
                                df.plot.cl[,"FinalLatitude"]), map_proj)
  proj.refSites <- rgdal::project(cbind(data_refSites[,"FinalLongitude"],
                                data_refSites[,"FinalLatitude"]), map_proj)
  proj.allSites <- rgdal::project(cbind(data_Sites[,"FinalLongitude"],
                                data_Sites[,"FinalLatitude"]), map_proj)
  # Unprojected data
  
  
  # plot map
  ppi <- 300
  
  col_outline <- "black"
  col_flowline <- "light blue"
  col_sites_all <- "dark gray"
  col_sites_cl  <- "cyan3"
  col_sites_ref <- "blue"
  col_sites_targ <- "red"
  
  pch_sites_all  <- 19
  pch_sites_cl   <- 19
  pch_sites_ref  <- 21
  pch_sites_targ <- 17
  
  cex_sites_all  <- 0.3
  cex_sites_ref  <- 0.9
  cex_sites_cl   <- 1
  cex_sites_targ <- 1.2
  
  lwd_outline  <- 1.5
  lwd_flowline <- 0.5
  
  #fn_jpg <- paste0("Results/",TargetSiteID, "/", TargetSiteID, ".map.jpg")
  fn_jpg <- file.path(dir_results, TargetSiteID, dir.sub3, paste0(TargetSiteID, ".map.jpg"))
  
  grDevices::jpeg(filename = fn_jpg, width = 4*ppi, height = 4*ppi, pointsize = 6,
                  quality=100, bg="white", res=ppi)
    if(is.null(map_proj)==TRUE){##IF.map_proj.START
      # map with no projection
      graphics::plot(data_Sites[,"FinalLongitude"], data_Sites[,"FinalLatitude"]
           , main=TargetSiteID, xlab="Longitude", ylab="Latitude"
           , col=col_sites_all, pch=pch_sites_all, cex=cex_sites_all
           )
      # points
      graphics::points(df.plot.cl[,"FinalLongitude"], df.plot.cl[,"FinalLatitude"], col=col_sites_cl, pch=pch_sites_cl, cex=cex_sites_cl)
      graphics::points(data.refSites[,"FinalLongitude"], data.refSites[,"FinalLatitude"], col=col_sites_ref, pch=pch_sites_ref, cex=cex_sites_ref)
      graphics::points(df.plotSite[,"FinalLongitude"], df.plotSite[,"FinalLatitude"], col=col_sites_targ, pch=pch_sites_targ, cex=cex_sites_targ)
      # Legend (no flow line)
      graphics::legend("bottomleft", legend = c("State", "all sites", "cluster sites", "ref sites", "target site")
                       , col = c(col_outline, col_sites_all, col_sites_cl, col_sites_ref, col_sites_targ)
                       , lty = c(1, rep(NA, 4))
                       , pch = c(NA, pch_sites_all, pch_sites_cl, pch_sites_ref, pch_sites_targ)
                       , title = "Legend")
      #
      # ggplot alternative (draft)
      # m0 <- ggplot2::ggplot(data_Sites, ggplot2::aes(FinalLongitude, FinalLatitude)) +
      #         ggplot2::geom_point(data=data_Sites, aes(x=FinalLongitude, y=FinalLatitude), size=cex_sites_all, color=col_sites_all ) +
      #         ggplot2::geom_point(data=df.plot.cl, aes(x=FinalLongitude, y=FinalLatitude), size=cex_sites_cl, color=col_sites_cl) +
      #         ggplot2::geom_point(data=data.refSites, aes(x=FinalLongitude, y=FinalLatitude), size=cex_sites_ref, color=col_sites_ref) +
      #         ggplot2::geom_point(data=df.plotSite, aes(x=FinalLongitude, y=FinalLatitude), size=cex_sites_targ, color=col_sites_targ) +
      #         ggplot2::labs(title=TargetSiteID, x="Longitude", y="Latitude") +
      #         ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5))
      #
    } else {
      # Map with Projection
      # lines
      sp::plot(map_outline, col="white", border=col_outline, lwd=lwd_outline, main=TargetSiteID)
      sp::plot(map_flowline, add = TRUE, col=col_flowline, lwd=lwd_flowline)
      if(!is.null(map_flowline2)==TRUE){##IF.null.flowline2.START
        sp::plot(map_flowline2, add = TRUE, col=col_flowline, lwd=lwd_flowline)
      }##IF.null.flowline2.END
      # points
      graphics::points(proj.allSites[,1], proj.allSites[,2], col=col_sites_all, pch=pch_sites_all, cex=cex_sites_all)
      graphics::points(proj.plot.cl[,1], proj.plot.cl[,2], col=col_sites_cl, pch=pch_sites_cl, cex=cex_sites_ref)
      graphics::points(proj.refSites[,1], proj.refSites[,2], col=col_sites_ref, pch=pch_sites_ref, cex=cex_sites_cl)
      graphics::points(proj.mySite[,1], proj.mySite[,2], col=col_sites_targ, pch=pch_sites_targ, cex=cex_sites_targ)
      # legend; items not the same size but ok.
      graphics::legend("bottomleft", legend = c("State", "flowline", "all sites", "cluster sites", "ref sites", "target site")
                               , col = c(col_outline, col_flowline, col_sites_all, col_sites_cl, col_sites_ref, col_sites_targ)
                               , lty = c(1, 1,  rep(NA, 4))
                               , pch = c(NA, NA, pch_sites_all, pch_sites_cl, pch_sites_ref, pch_sites_targ)
                               , title = "Legend")
      # ggplot help with projections
      # http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/
      #
      # m1 <- ggplot2::ggplot() + 
      #         ggplot2::geom_polygon(data=map_outline, ggplot2::aes(x=long, y=lat), fill="white", color=col_outline) +
      #         ggplot2::labs(title=TargetSiteID, x="", y="") +
      #         ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)
      #                        , axis.ticks.x = element_blank(), axis.text.x = element_blank()
      #                        , axis.ticks.y = element_blank(), axis.text.y = element_blank()
      #                        , panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      #                        , panel.background = element_blank()) +
      #         ggplot2::geom_line(data=map_flowline, ggplot2::aes(x=long, y=lat, group=group), col=col_flowline) + 
      #         ggplot2::geom_line(data=map_flowline2, ggplot2::aes(x=long, y=lat, group=group), col=col_flowline) + 
      #         coord_equal(ratio=1) +  #square plot to avoid distortion
      #   
      #   # Good to here
      #         ggplot2::geom_point(data=proj.allSites, ggplot2::aes(x=FinalLongitude, y=FinalLatitude), size=cex_sites_all, color=col_sites_all ) +
      #         ggplot2::geom_point(data=proj.plot.cl, ggplot2::aes(x=FinalLongitude, y=FinalLatitude), size=cex_sites_cl, color=col_sites_cl) +
      #         ggplot2::geom_point(data=proj.refSites, ggplot2::aes(x=FinalLongitude, y=FinalLatitude), size=cex_sites_ref, color=col_sites_ref) +
      #         ggplot2::geom_point(data=proj.mySite, ggplot2::aes(x=FinalLongitude, y=FinalLatitude), size=cex_sites_targ, color=col_sites_targ)
      # # will need to use ggsave()
      #
    }##IF.map_proj.END
  grDevices::dev.off()
  
  #
  # Leaflet Map in Notebook
  report_format <- "html"
  #dir_rmd <- file.path(system.file(package = "CASTfxn"), "rmd")
  #strFile_RMD <- file.path(dir_rmd, "Map_Leaflet.rmd")
  #strFile_RMD <- file.path(file.path(system.file(package = "CASTfxn"), "rmd"), "Map_Leaflet.rmd")
  strFile_out_ext <- paste0(".", report_format)
  strFile_out <- paste0(TargetSiteID,".map.leaflet", strFile_out_ext)
  dir_map <- file.path(dir_results, TargetSiteID, dir.sub3)
  # rmarkdown::render(file.path(file.path(system.file(package = "CASTfxn"), "rmd"), "Map_Leaflet.rmd")
  rmarkdown::render("C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd/Map_Leaflet.rmd"
                    , output_format=paste0(report_format,"_document")
                    , output_file=strFile_out
                    , output_dir=dir_map
                    , quiet=TRUE)
  # place after static map so can insert
  
  #
  mySiteSummary <- list(SiteInfo = mySiteInfo
                        , Samps = mySamps
                        , BMImetrics = myBMImetrics
                        , AlgMetrics = myAlgaeMetrics
                        # , ReachInfo = myReachInfo
                        , COMID = myCOMID
                        , ClustIDs = myClustID
                        , impair = myImpairments
                        , mods = myReachMods)
  return(mySiteSummary)
}
