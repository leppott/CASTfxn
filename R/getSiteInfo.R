#  Copyright 2020 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents 
#  is expressly prohibited without prior written permission of TetraTech.
#
#
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
#' @param dir_map_rmd Directory with Map_Leaflet.RMD.  Default = package RMD.
#' @param boo_plot Boolean value to save plots.  Default = TRUE.
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
                        , data_Sites
                        , data_bkgdata
                        , data_bkginfo
                        , data_SampSummary
                        , data_303d=NULL
                        , data_bmiMetrics=NULL
                        , bmiIndexGp = "IBI"
                        , data_algMetrics=NULL
                        , algIndexGp = "IBI"
                        , comp_sites=NULL
                        , data_cluster
                        , data_mods=NULL
                        , map_proj=NULL
                        , map_outline=NULL
                        , map_flowline=NULL
                        , map_flowline2=NULL
                        , dir_photo = file.path(dir_data,"Photos")
                        , dir_results = dir_results
                        , dir_sub = "SiteInfo"
                        , dir_map_rmd = file.path(system.file(package = "CASTfxn"), "rmd")
                        , boo_plot = TRUE
                        ){
    #
   
    # DEBUG 
    boo_DEBUG <- FALSE
    #
    if (boo_DEBUG == TRUE) {
        TargetSiteID = TargetSiteID
        data_Sites = data_Sites
        data_bkgdata = df_bkgdata
        data_bkginfo = df_bkginfo
        data_SampSummary = data_SampSummary
        data_303d = NULL
        data_bmiMetrics = data_bmiMetrics
        bmiIndexGp = bmiIndexGp
        data_algMetrics = data_AlgMetrics
        algIndexGp = algIndexGp
        comp_sites = comp_sites
        data_cluster = data_cluster
        data_mods = NULL
        map_proj = my.aea
        map_outline = outline
        map_flowline = flowline
        map_flowline2 = NULL
        dir_photo = file.path(dir_data,"Photos")
        dir_results = dir_results
        dir_sub = "SiteInfo"
        dir_map_rmd = "C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd/"
        boo_plot = TRUE
    }

    not_all_na <- function(x) {!all(is.na(x))}
    all_na <- function(x) {all(is.na(x))}
    
    # check for and create (if necessary) dir_results and SiteID subdirectory
    # default structure: Results/TargetSiteID/SiteInfo

    #dir_results = file.path(getwd(), "Results")
    dir_sub2 <- TargetSiteID
    dir_sub3 <- dir_sub
    ifelse(!dir.exists(dir_results)==TRUE
         , dir.create(dir_results)
         , FALSE)
    ifelse(!dir.exists(file.path(dir_results, dir_sub2))==TRUE
         , dir.create(file.path(dir_results, dir_sub2))
         , FALSE)
    ifelse(!dir.exists(file.path(dir_results, dir_sub2, dir_sub3))==TRUE
         , dir.create(file.path(dir_results, dir_sub2, dir_sub3))
         , FALSE)
    
    dir_path <- file.path(dir_results, dir_sub2, dir_sub3)
  
    # Define pipe
    `%>%` <- dplyr::`%>%`
    
    ## Plot, Variables, Output Size (inches)
    plot_H <- 4
    plot_W <- 6
    
    #
    mySiteInfo <- data_Sites[data_Sites[,"StationID_Master"]==TargetSiteID
                       ,c("FinalLatitude","FinalLongitude","WaterbodyName"
                          ,"GIS_County","CARefSite_2017","COMID"
                          ,"clust")]
    data_refSites <- subset(data_Sites, CARefSite_2017==1
                      , select= c(StationID_Master, FinalLatitude
                                  , FinalLongitude, COMID))
    myRefCOMIDs <- as.vector(unique(data_refSites$COMID))
    
    # get sampling info (dates of samples)
    mySamps <- data_SampSummary[data_SampSummary[,"StationID_Master"]==TargetSiteID,]
    
    # get response information (CSCI, H20, etc)
    if (!is.null("data_bmiMetrics")) {
    
        # Prep BMI data for plotting
        compBMImetrics <- data_bmiMetrics[data_bmiMetrics[,"StationID_Master"] 
                                        %in% comp_sites
                                        , c("StationID_Master", "BMISampID"
                                            , "BMISampDate", "Quality"
                                            , bmiIndexGp)]
        compBMImetrics <- compBMImetrics %>%
          tidyr::gather(eval(bmiIndexGp), key="Index", value = "Score") %>%
          dplyr::mutate(Quality = ifelse(StationID_Master==TargetSiteID
                                      , "Target", Quality)
                        , Quality = as.factor(Quality)
                        , Index = as.factor(Index))
        goodBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Good")
        badBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Degraded")
        myBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Target")
        
        gap.good <- cbind.data.frame("getSiteInfo", "quality", nrow(goodBMImetrics)
                                     , "Not degraded comparator samples available.")
        colnames(gap.good) <- c("fxnname", "condition", "result", "comment")
        gap.bad <- cbind.data.frame("getSiteInfo", "quality", nrow(badBMImetrics)
                                     , "Degraded comparator samples available.")
        colnames(gap.bad) <- c("fxnname", "condition", "result", "comment")
        gap.comps <- rbind(gap.good, gap.bad)
        rm(gap.good, gap.bad)
        
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
        write.table(gap.comps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        
        
        ## Plot, Variables, Strings, other Aesthetics
        lab.sub <- paste0("Comparator samples (n = ", nrow(compBMImetrics)
                        , " from ", length(comp_sites)," sites)")
        
        bio_col <- c("dark gray", "blue", "red") # Degraded, Good, Target
        bio_shp <- c(25, 21, 17) # down triangle, circle, and triangle
        bio_alpha <- c(0.3, 0.5, 1)
        
        str_title <- "Benthic macroinvertebrate index scores"
        str_sub <- paste0("Target Site: ", TargetSiteID, ", cluster ", mySiteInfo$clust)
        str_xlab  <- "Index name"
        str_ylab  <- "Score"
        
        ## Plot, Data
        fn_bmiscores <- paste0(TargetSiteID, "_BMI_IndexBoxplots.png")
        fn_bmiscores <- file.path(dir_path,fn_bmiscores)
        pBMI <- ggplot2::ggplot(compBMImetrics, ggplot2::aes(y=Score,x=Index,group=Index)) +
          ggplot2::geom_boxplot(na.rm = TRUE) +
          ggplot2::geom_jitter(size=2, width = 0.2, na.rm=TRUE
                                , ggplot2::aes(color=Quality, fill=Quality
                                , shape=Quality, alpha=Quality)) +
          ggplot2::scale_color_manual(values=bio_col, drop=FALSE) +
          ggplot2::scale_fill_manual(values=bio_col, drop=FALSE) +
          ggplot2::scale_shape_manual(values=bio_shp, drop=FALSE) +
          ggplot2::scale_alpha_manual(values=bio_alpha, drop=FALSE) +
          # ggplot2::geom_jitter(data=filter(compBMImetrics, Quality=="Target")
          #                      , size=2, width=0.2
          #                      , ggplot2::aes(y=Score, x=Index, group=Index
          #                                     , color = "red", fill = "red"
          #                                     , shape = 17))
          ggplot2::labs(title=str_title, subtitle = str_sub, caption=lab.sub) + 
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)
                         , plot.subtitle = ggplot2::element_text(hjust=0.5)) +
          ggplot2::theme(axis.text.y=ggplot2::element_text(color="white")
                         , axis.ticks.y=ggplot2::element_blank())
        if(boo_plot){
          ggplot2::ggsave(fn_bmiscores, pBMI, width=plot_W, height=plot_H, units="in")
        }## IF ~ boo_plot ~ END

    }
    if (!is.null("data_algMetrics")) {
        # Prep Alg data for plotting
        compALGmetrics <- data_algMetrics[data_algMetrics[,"StationID_Master"] 
                                          %in% comp_sites
                                          , c("StationID_Master", "AlgSampID"
                                              , "AlgSampDate", "Quality"
                                              , algIndexGp)]
        compALGmetrics <- compALGmetrics %>%
            tidyr::gather(eval(algIndexGp), key="Index", value = "Score") %>%
            dplyr::mutate(Quality = ifelse(StationID_Master==TargetSiteID
                                           , "Target", Quality)
                          , Quality = as.factor(Quality)
                          , Index = as.factor(Index))
        goodALGmetrics <- dplyr::filter(compALGmetrics, Quality=="Good")
        badALGmetrics <- dplyr::filter(compALGmetrics, Quality=="Degraded")
        myALGmetrics <- dplyr::filter(compALGmetrics, Quality=="Target")
        
        ## Plot, Variables, Strings, other Aesthetics
        lab.sub <- paste0("Comparator samples (n = ", nrow(compALGmetrics)
                          , " from ", length(comp_sites)," sites)")
        
        bio_col <- c("dark gray", "blue", "red") # Degraded, Good, Target
        bio_shp <- c(25, 21, 17) # down triangle, circle, and triangle
        bio_alpha <- c(0.5, 0.5, 1)
        
        str_title <- "Algal community index scores"
        str_sub <- paste0("Target Site: ", TargetSiteID, ", cluster ", mySiteInfo$clust)
        str_xlab  <- "Index name"
        str_ylab  <- "Score"
        
        ## Plot, Data
        fn_algscores <- paste0(TargetSiteID, "_ALGAE_IndexBoxplots.png")
        fn_algscores <- file.path(dir_path,fn_algscores)
        pAlg <- ggplot2::ggplot(compALGmetrics, ggplot2::aes(y=Score,x=Index,group=Index)) +
            ggplot2::geom_boxplot(na.rm = TRUE) +
            ggplot2::geom_jitter(size=2, width = 0.2, na.rm=TRUE
                                 , ggplot2::aes(color=Quality, fill=Quality
                                                , shape=Quality, alpha=Quality)) +
            ggplot2::scale_color_manual(values=bio_col, drop=FALSE) +
            ggplot2::scale_fill_manual(values=bio_col, drop=FALSE) +
            ggplot2::scale_shape_manual(values=bio_shp, drop=FALSE) +
            ggplot2::scale_alpha_manual(values=bio_alpha, drop=FALSE) +
            # ggplot2::geom_jitter(data=filter(compBMImetrics, Quality=="Target")
            #                      , size=2, width=0.2
            #                      , ggplot2::aes(y=Score, x=Index, group=Index
            #                                     , color = "red", fill = "red"
            #                                     , shape = 17))
            ggplot2::labs(title=str_title, subtitle = str_sub, caption=lab.sub) + 
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)
                           , plot.subtitle = ggplot2::element_text(hjust=0.5)) +
            ggplot2::theme(axis.text.y=ggplot2::element_text(color="white")
                           , axis.ticks.y=ggplot2::element_blank())
        if(boo_plot){
          ggplot2::ggsave(fn_algscores, pAlg, width=plot_W, height=plot_H, units="in")
        }## IF ~ boo_plot ~ END
        
    }
    
    # get COMID 
    myCOMID <- mySiteInfo$COMID
    myWBName <- mySiteInfo$WaterbodyName
    myClustID <- as.integer(data_cluster$clust[data_cluster$COMID==myCOMID])
    
    if (exists("data_mods")) {
        myReachMods <- data_mods[data_mods[,"COMID"]==myCOMID
                                 ,c("ReachModStatus", "ModReason")]
    }
    if (exists("data_303d")) {
        my303d.COMID <- subset(data_303d, data_303d$ComID == myCOMID)
        my303d.COMID.WBName <- subset(my303d.COMID, my303d.COMID$WATER.BODY.NAME %in% myWBName)
        myCurrent303d <- subset(my303d.COMID.WBName, my303d.COMID.WBName$Year == 2012)
        myImpairments <- myCurrent303d[,c("ComID", "WATER.BODY.NAME", "POLLUTANT",
                                          "FINAL.LISTING.DECISION")]
    }

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
    fn_map <- file.path(dir_path, paste0(TargetSiteID, "_MAP.png"))
    
    grDevices::png(filename = fn_map, width = 4*ppi, height = 4*ppi, pointsize = 6,
              bg="white", res=ppi)
    if(is.null(map_proj)==TRUE){##IF.map_proj.START
    # map with no projection
    graphics::plot(data_Sites[,"FinalLongitude"], data_Sites[,"FinalLatitude"]
       , main=TargetSiteID, xlab="Longitude", ylab="Latitude"
       , col=col_sites_all, pch=pch_sites_all, cex=cex_sites_all
       )
    # points
    graphics::points(df.plot.cl[,"FinalLongitude"]
                     , df.plot.cl[,"FinalLatitude"]
                     , col=col_sites_cl, pch=pch_sites_cl, cex=cex_sites_cl)
    graphics::points(data.refSites[,"FinalLongitude"]
                     , data.refSites[,"FinalLatitude"]
                     , col=col_sites_ref, pch=pch_sites_ref, cex=cex_sites_ref)
    graphics::points(df.plotSite[,"FinalLongitude"]
                     , df.plotSite[,"FinalLatitude"]
                     , col=col_sites_targ, pch=pch_sites_targ, cex=cex_sites_targ)
    # Legend (no flow line)
    graphics::legend("bottomleft", legend = c("State", "all sites"
                                              , "cluster sites", "ref sites"
                                              , "target site")
                   , col = c(col_outline, col_sites_all, col_sites_cl
                             , col_sites_ref, col_sites_targ)
                   , lty = c(1, rep(NA, 4))
                   , pch = c(NA, pch_sites_all, pch_sites_cl, pch_sites_ref
                             , pch_sites_targ)
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
    sp::plot(map_outline, col="white", border=col_outline, lwd=lwd_outline
             , main=TargetSiteID)
    sp::plot(map_flowline, add = TRUE, col=col_flowline, lwd=lwd_flowline)
    if(!is.null(map_flowline2)==TRUE){##IF.null.flowline2.START
    sp::plot(map_flowline2, add = TRUE, col=col_flowline, lwd=lwd_flowline)
    }##IF.null.flowline2.END
    # points
    graphics::points(proj.allSites[,1], proj.allSites[,2]
                     , col=col_sites_all, pch=pch_sites_all, cex=cex_sites_all)
    graphics::points(proj.plot.cl[,1], proj.plot.cl[,2]
                     , col=col_sites_cl, pch=pch_sites_cl, cex=cex_sites_ref)
    graphics::points(proj.refSites[,1], proj.refSites[,2]
                     , col=col_sites_ref, pch=pch_sites_ref, cex=cex_sites_cl)
    graphics::points(proj.mySite[,1], proj.mySite[,2], col=col_sites_targ
                     , pch=pch_sites_targ, cex=cex_sites_targ)
    # legend; items not the same size but ok.
    graphics::legend("bottomleft", legend = c("State", "flowline"
                                              , "all sites", "cluster sites"
                                              , "ref sites", "target site")
                           , col = c(col_outline, col_flowline, col_sites_all
                                     , col_sites_cl, col_sites_ref, col_sites_targ)
                           , lty = c(1, 1,  rep(NA, 4))
                           , pch = c(NA, NA, pch_sites_all, pch_sites_cl
                                     , pch_sites_ref, pch_sites_targ)
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
    strFile_out <- paste0(TargetSiteID,"_MAP_leaflet", strFile_out_ext)
    # dir_map <- file.path(dir_results, TargetSiteID, dir_sub3)
    
    rmarkdown::render(file.path(dir_map_rmd, "Map_Leaflet.rmd")
                      , output_format=paste0(report_format,"_document")
                      , output_file=strFile_out
                      , output_dir=dir_path
                      , quiet=TRUE)

    # place after static map so can insert
    
    
    # Check for presence of Photos in data directory. If not present, skip.
    if (dir.exists(dir_photo)==TRUE & length(list.files(dir_photo)) > 0) {
        photofiles <- list.files(dir_photo)
        have.photos <- FALSE
        for (l in 1:length(photofiles)) {
            ifelse(!dir.exists(file.path(dir_path, "Photos"))==TRUE
                   , dir.create(file.path(dir_path, "Photos"))
                   , FALSE)
            photoname <- photofiles[l]
            if (str_detect(photoname, eval(TargetSiteID))==TRUE) {
                file.copy(file.path(dir_photo,photoname)
                          , file.path(dir_path,"Photos",photoname))
                message(paste0(photoname, " copied."))
                have.photos <- TRUE
            }
        }## FOR ~ l ~ END
    } else { 
        have.photos <- FALSE
        msg <- "Photo directory does not exist."
        message(msg)
    }## IF ~ dir.exists(dir_photo)==TRUE & length(list.files(dir_photo)) > 0 ~ END
    
    if (!have.photos) {
        
        message(paste0("No site photos are available for ", TargetSiteID))
        
        gap.photos <- cbind.data.frame("getSiteInfo", "quality", 0
                                    , "Site photos are not available.")
        colnames(gap.photos) <- c("fxnname", "condition", "result", "comment")

        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
        write.table(gap.photos, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
    }## IF ~ !have.photos ~ END

    message("Completed transferring any available site files.")
    
    # Get background data from fn_bkgdata; use COMID to select single row
    data_bkgdata <- dplyr::filter(data_bkgdata, COMID == myCOMID)
    
    # Check for data to plot
    data_bkgcheck <- dplyr::select_if(data_bkgdata, not_all_na)
    
    if (ncol(data_bkgcheck)<=1) { # If only COMID column, then no data
        # NO data in streamcat for the reach.
        gapcomment <- paste0("No background data are available for site "
                             , TargetSiteID, "on reach with COMID = ", myCOMID)
        gaps <- cbind.data.frame("getSiteInfo", "Background Data", 0
                                 , gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results, TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")    
    } else {  # Background data exists
        data_bkgdata2 <- tidyr::gather(data_bkgdata, -COMID, key = "ColName"
                                       , value = "val")
        data_bkgdata2 <- dplyr::select(data_bkgdata2, -COMID)
        
        fn_bkg <- paste0(TargetSiteID,"_BKGDATA.tab")
        write.table(data_bkgdata, file.path(dir_path,fn_bkg), append = FALSE
                    , sep = "\t", col.names = TRUE, row.names = FALSE)
        
        # Get metadata from fn_bkginfo
        df.bkg2plot <- dplyr::left_join(data_bkginfo, data_bkgdata2)
        
        rm(data_bkgdata, data_bkgdata2, data_bkginfo)
        
        # Determine appropriate graphics
        # Bar charts, faceted with catchment on left, watershed on right
        cat.sub <- unique(df.bkg2plot[,c("Category","Subcategory","Units","AbbrFN")])
        
        for (i in 1:nrow(cat.sub)) { # Plot each subcategory
            # pull out temp data set to plot
            df.temp <- df.bkg2plot %>%
                dplyr::filter(Category == cat.sub$Category[i]
                              , Subcategory == cat.sub$Subcategory[i])
            
            xlab <- paste0(cat.sub$Category[i],": ",cat.sub$Subcategory[i]
                           ,", ",cat.sub$Units[i])
            fn.plot <- file.path(dir_path, paste0(TargetSiteID, "_BKGD_"
                                                  , cat.sub[i,4], ".png"))
            p.title <- paste0(TargetSiteID, ": Site background")
            p.subtitle <- "Potential anthropogenic alterations"
            numcols <- length(unique(df.temp$Scale))/2
            
            message(xlab)
            
            if (is.na(df.temp$StudyYear)) {  # No study year
                p.bkg <- ggplot2::ggplot(df.temp, ggplot2::aes(x = ShortName
                                                               , y = signif(val, digits = 2))) +
                    ggplot2::geom_bar(stat = "identity", width = 0.5, fill = "darkred") +
                    ggplot2::geom_text(ggplot2::aes(label = signif(val, digits = 2)
                                                    , vjust=-0.2), color = "black", size=3) +
                    ggplot2::ylim(0, max(df.temp$val)*1.2) +
                    ggplot2::facet_wrap(Scale~.)
                p.bkg <- p.bkg + ggplot2::theme_bw() +
                ggplot2::theme(legend.position = "none") +
                    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)
                                   , strip.text.y = ggplot2::element_text(size = 8)) +
                    ggplot2::labs(title = p.title, subtitle = p.subtitle
                                  , x = xlab, y = "Value")
                p.bkg <- p.bkg + 
                    ggplot2::theme(axis.text.x = ggplot2::element_text(size=8
                                                                       ,angle=45,hjust=1)
                                   , axis.text.y = ggplot2::element_text(size=7)
                                   , axis.title.x = ggplot2::element_text(size=9, face="bold")
                                   , axis.title.y = ggplot2::element_text(size=9, face="bold")
                                   , plot.title = ggplot2::element_text(size=12, face="bold")
                                   , plot.subtitle = ggplot2::element_text(size=10, face="bold"))
                if(boo_plot){
                  ggplot2::ggsave(fn.plot, p.bkg, dpi=ppi, width=plot_W*1.5, height=plot_H*1.5)
                }## IF ~ boo_plot ~ END
                
            } else {  # Separate study year to consider in faceting
                
                p.bkg <- ggplot2::ggplot(df.temp, ggplot2::aes(x = ShortName
                                                               , y = signif(val, digits = 2)
                                                               , group = StudyYear)) +
                    ggplot2::geom_bar(position="dodge", stat = "identity", width = 0.5
                                      , fill = "darkred") +
                    ggplot2::geom_text(ggplot2::aes(label = signif(val, digits=2)
                                                    , vjust=-0.2), color = "black", size=3) +
                    ggplot2::ylim(0, max(df.temp$val)*1.2) +
                    ggplot2::facet_grid(stringr::str_wrap(Scale,10)~StudyYear, margins = FALSE)
                p.bkg <- p.bkg + ggplot2::theme_bw() +
                    ggplot2::theme(legend.position = "none") +
                    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)
                                   , strip.text.y = ggplot2::element_text(size = 8)) +
                    ggplot2::labs(title = p.title, subtitle = p.subtitle
                                  , x = xlab, y = "Value")
                p.bkg <- p.bkg +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8
                                                                       , angle = 45, hjust = 1)
                                   , axis.text.y = ggplot2::element_text(size=7)
                                   , axis.title.x = ggplot2::element_text(size=9, face="bold")
                                   , axis.title.y = ggplot2::element_text(size=9, face="bold")
                                   , plot.title = ggplot2::element_text(size=12, face="bold")
                                   , plot.subtitle = ggplot2::element_text(size=10, face="bold"))
                if(boo_plot){
                  ggplot2::ggsave(fn.plot, p.bkg, dpi=ppi, width=plot_W*1.5, height=plot_H*1.5)
                }## IF ~ boo_plot ~ END
                
            }  # End creating background plot
            
        }  # End iteration over subcategories
        
    }  # End background data portion

    #
    mySiteSummary <- list(SiteInfo = mySiteInfo
                    , Samps = mySamps
                    , BMImetrics = myBMImetrics
                    , AlgMetrics = myALGmetrics
                    , COMID = myCOMID
                    , ClustIDs = myClustID
                    , impair = myImpairments
                    , mods = myReachMods
                    , refCOMIDs = myRefCOMIDs)
    return(mySiteSummary)
}

