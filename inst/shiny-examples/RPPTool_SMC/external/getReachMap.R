# getReachMap (Specific for SMC)
# Ann.RoseberryLincoln@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 
# Plots a map of the target reach, the reaches within the connectivity distance,
# and any sites on those reaches.
# Reaches color-coded by predicted median CSCI score (SCAPE)
# Sites color-coded by observed recent CSCI score



getReachMap <- function(proj, dsn_boundary, lyr_boundary, dsn_reaches
                        , lyr_reaches, allSites, allCxns, TargetCOMID) {
    
    
    boo_DEBUG <- FALSE
    
    if (boo_DEBUG==TRUE) {
        dsn_boundary <- dsn_outline
        lyr_boundary <- lyr_outline
        proj <- proj_wgs84
        dsn_reaches <- dsn_flowline
        lyr_reaches <- lyr_flowline
        allSiteBCGs = listBCGdata$obsSiteBCG
        allReachBCGs = listBCGdata$predReachBCG
        allCxns = dfCxnsALL
        allStressData = listStressScores
        TargetCOMID = TargetCOMID
    }
    
    not_all_na <- function(x) {!all(is.na(x))}
    
    # Prep outline and reachlines ####
    sp_outline <- rgdal::readOGR(dsn = "Data/SMCBoundary", layer = "SMCBoundary_aea")
    sp_outline_wgs <- spTransform(sp_outline, CRS("+proj=longlat +datum=WGS84 +no_def"))
    sp_flowline <- rgdal::readOGR(dsn = "Data/SMCReaches", layer = "SMCReaches_aea")
    sp_flowline_wgs <- spTransform(sp_flowline, CRS("+proj=longlat +datum=WGS84 +no_def"))
    rm(sp_outline, sp_flowline)

    # Need to identify things to plot
    # 1. All reaches (light blue, thin line width) [sp_flowline]
    # 2. Connected reaches (dark blue, thin line width)
    # 3. Target reach (darkest blue, thicker line width) [TargetCOMID]
    # 4. Sites on any connected reaches [sp_siteCxns]
    
    # Get median predicted BCG level for the reach (BCGqt50)
    dfReachPredBCG50 <- siteCxns[, c("COMID", "BCGqt50")]
    dfReachPredBCG50$BCGqt50 <- as.factor(dfReachPredBCG50$BCGqt50)
    
    # Prepare connected reaches ####
    useReaches <- unique(siteCxns$COMID)
    sp_reachCxns_wgs <- sp_flowline_wgs[sp_flowline_wgs@data$COMID %in% useReaches,]
    comsInShpFile <- sp_reachCxns_wgs@data$COMID
    dfReachPredBCG50 <- unique(dfReachPredBCG50[dfReachPredBCG50$COMID %in% comsInShpFile,])
    sp_reachCxns_wgs <- sp::merge(sp_reachCxns_wgs, dfReachPredBCG50
                               , by.x = "COMID", by.y = "COMID", all.x = TRUE)

    # Select target COMID
    sp_targetCOMID_wgs <- sp_flowline_wgs[sp_flowline_wgs@data$COMID==TargetCOMID,]

    # Get sites on connected reaches ####
    siteCxns <- siteCxns[!is.na(siteCxns$StationID_Master),]
    
    # Get scaled stressors ####
    siteStressors <- dplyr::select(as.data.frame(siteStressors), COMID
                                   , Stressor, StressSampleDate
                                   , StressorValue, AdjStressorValue)
    siteStrParamAdjVal <- siteStressors %>%
        dplyr::mutate(AdjStressor = paste0("Adj_", Stressor)) %>%
        dplyr::select(TargetSite, AdjStressor, AdjStressorValue) %>%
        tidyr::spread(key=AdjStressor, value=AdjStressorValue, drop=TRUE)
    siteStrParamVal <- siteStressors %>%
        dplyr::select(TargetSite, Stressor, StressorValue) %>%
        tidyr::spread(key=Stressor, value=StressorValue, drop=TRUE)
    siteStrParamDate <- siteStressors %>%
        dplyr::mutate(DateStressor = paste0("Date_", Stressor)) %>%
        dplyr::select(TargetSite, DateStressor, StressSampleDate) %>%
        tidyr::spread(key=DateStressor, value=StressSampleDate, drop=TRUE)    
    siteStrFinal <- merge(siteStrParamDate, siteStrParamAdjVal
                          , by.x = "TargetSite"
                          , by.y = "TargetSite")
    siteStrFinal <- merge(siteStrFinal, siteStrParamVal
                          , by.x = "TargetSite"
                          , by.y = "TargetSite")
    
    allSiteStrFinal <- merge(allSites, siteStrFinal
                           , by.x = "StationID_Master", by.y = "TargetSite"
                           , all.x = TRUE)
    cxnSiteStrFinal <- merge(siteCxns[,c("StationID_Master","FinalLatitude"
                                         , "FinalLongitude", "BCGLevel")]
                             , siteStrFinal
                             , by.x = "StationID_Master", by.y = "TargetSite"
                             , all.x = TRUE)
    cxnSiteStrFinal <- dplyr::select_if(cxnSiteStrFinal, not_all_na)

    # siteCxns <- unique(siteCxns[, c("StationID_Master","FinalLongitude"
    #                                 ,"FinalLatitude")])
    
    # Prepare spatial point data for connected sites ####
    # sp_siteCxns is albers equal area spatial points data frame
    # sp_siteCxns_f is fortified aea data frame
    # sp_siteCxns_transf is WGS84 version (for leaflet)
    # sp_siteCxns <- sp::SpatialPointsDataFrame(coords = siteCxns[,c("FinalLongitude"
    #                                                                , "FinalLatitude")]
    #                                           , data = siteCxns
    #                                           , proj4string = CRS(sp_proj))
    # Merge in most recent BCG category and corresponding CSCI score #
    
    # sp_siteCxns_f <- left_join(siteCxns, sp_siteCxns@data)    
    # sp_siteCxns_transf <- spTransform(sp_siteCxns
    #                                  , CRS("+proj=longlat +datum=WGS84 +no_def"))
    

    # NOTE: Lines aren't working. Maybe need basic plotting here instead
    # ggplot2::ggplot() +
    #     ggplot2::geom_polygon(data=sp_outline_f, ggplot2::aes(x=long, y=lat)
    #                           , fill="white", color=col_outline) +
    #     ggplot2::expand_limits(x=sp_outline_f$long, y=sp_outline_f$lat) +
    #     ggplot2::coord_sf(crs=sp_proj) +
    #     ggplot2::labs(x="", y = "") +
    #     ggplot2::ggtitle(paste0("Target Reach: ",TargetCOMID)) +
    #     ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5, size = 10)
    #                    , axis.text.x = ggplot2::element_text(size=6)
    #                    , axis.text.y = ggplot2::element_text(size=6)) #+
    # 
    #     ggplot2::geom_line(data=sp_flowline_f, ggplot2::aes(x=long, y=lat)
    #                        , col=col_reaches)
    BCGlevels = c(1,2,3,4,5,6)
    pal <- colorFactor(palette = viridis(n=6, option = "D"), domain = BCGlevels)

    leaflet::leaflet(data = sp_outline_wgs) %>%
        # Groups, Base
        leaflet::addTiles(group = "OSM (default)") %>%
        leaflet::addProviderTiles(leaflet::providers$Stamen.Terrain, group = "Terrain") %>%
        # Groups, Overlay
        leaflet::addPolygons(data = sp_outline_wgs, color="black"
                             , group = "SMC Region") %>%
        leaflet::addPolylines(data = sp_flowline_wgs
                              , color = "lightblue"
                              , group = "All streams"
                              , opacity = 0.6
                              , popup = ~paste(COMID)) %>%
#        leaflet::addPolylines(data=sp_reachCxns_aea, color)
        leaflet::addPolylines(data = sp_targetCOMID_wgs
                              , color = "black"
                              , stroke = TRUE
                              , weight = 10
                              , opacity = 1) %>%
        leaflet::addPolylines(data = sp_reachCxns_wgs
                              , group = "Target plus connected streams"
                              , color = ~pal(BCGqt50)
                              , stroke = TRUE
                              , weight = 8
                              , opacity = 0.8
                              , popup = ~paste(COMID, as.character("<br>")
                                               , "BCG Level ="
                                               , BCGqt50,sep = " ")) %>%
        # leaflet::addPolylines(data=sp_targetCOMID_wgs, color="yellow") %>%
        leaflet::addCircleMarkers(data=allSites
                                  , lng = ~FinalLongitude
                                  , lat = ~FinalLatitude
                                  , group = "All sites"
                                  , color = "black"
                                  , fillColor = ~pal(BCGLevel)
                                  , radius = 4
                                  , stroke = TRUE
                                  , weight = 1
                                  , fillOpacity = 0.6
                                  , opacity = 0.6
                                  , popup = ~paste0(StationID_Master, as.character("<br>")
                                                    , "CSCI =", CSCI, as.character("<br>")
                                                    , "BCG Level =", BCGLevel)) %>%
        leaflet::addCircleMarkers(data = siteCxns
                                  , lng = ~FinalLongitude
                                  , lat = ~FinalLatitude
                                  , group = "Connected sites"
                                  , color = "black"
                                  , fillColor = ~pal(BCGLevel)
                                  , radius = 4
                                  , stroke = TRUE
                                  , weight = 1
                                  , fillOpacity = 1
                                  , opacity = 1
                                  , popup = ~paste0(StationID_Master, as.character("<br>")
                                                    , "CSCI =", CSCI, as.character("<br>")
                                                    , "BCG Level =", BCGLevel)) %>%
        leaflet::addCircleMarkers(data = allSiteStrFinal[!is.na(allSiteStrFinal$SpecificConductivity_fld_uS_cm),]
                                  , lng = ~FinalLongitude
                                  , lat = ~FinalLatitude
                                  , group = "Specific conductivity (uS/cm)"
                                  , color = "red"
                                  , fillColor = ~pal(BCGLevel)
                                  , radius = 4
                                  , stroke = TRUE
                                  , weight = 1
                                  , fillOpacity = 1
                                  , opacity = 1
                                  , label = ~paste0(" ", StationID_Master, "; "
                                                    , SpecificConductivity_fld_uS_cm
                                                    , "; "
                                                    , Adj_SpecificConductivity_fld_uS_cm
                                                    , " (Adj)")
                                  , labelOptions = labelOptions(noHide = TRUE
                                                    , textOnly = TRUE
                                                    , direction = "bottomright"
                                                    , style = list("color"="red"
                                                                   , "font-family" = "serif"
                                                                   , "font-style" = "italic"))) %>%
                                  # , popup = ~paste0(StationID_Master
                                  #                   , as.character("<br>")
                                  #                   , "Sample Date = "
                                  #                   , Date_SpecificConductivity_fld_uS_cm
                                  #                   , as.character("<br>")
                                  #                   , "Specific Conductivity (uS/cm) = "
                                  #                   , SpecificConductivity_fld_uS_cm
                                  #                   , as.character("<br>")
                                  #                   , "Scaled Specific Conductivity (uS/cm) = "
                                  #                   , Adj_SpecificConductivity_fld_uS_cm)) %>%
        # Bounding (to connected reaches)
        fitBounds(lng1 = sp_reachCxns_wgs@bbox[1]
                  , lat1 = sp_reachCxns_wgs@bbox[4]
                  , lng2 = sp_reachCxns_wgs@bbox[3]
                  , lat2 = sp_reachCxns_wgs@bbox[2]) %>%
        # Layers
        leaflet::addLayersControl( 
            baseGroups = c("OSM (default)", "Terrain")
            , overlayGroups = c("All streams", "Target plus connected streams"
                                , "Specific conductivity (uS/cm)", "Connected sites")) %>%
        # Legend
        leaflet::addLegend("bottomleft", pal = pal, values = ~BCGlevels
                           , title = "BCG levels", opacity = 1) %>%
        leaflet::addMiniMap("bottomleft")
    
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # ppi <- 600
    # 
    # col_outline <- "black"
    # col_allreach <- "light blue"
    # col_cxnreach <- ""
    # 
    # p <- ggplot(sp_flowline@data, aes())
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # dfReachSites <- map_sites[map_sites$COMID==TargetCOMID,]
    # projReachSites <- rgdal::project(cbind(dfReachSites[,"FinalLongitude"]
    #                                        , dfReachSites[,"FinalLatitude"])
    #                                  , map_proj)
    # projAllSites <- rgdal::project(cbind(map_sites[,"FinalLongitude"]
    #                                      , map_sites[,"FinalLatitude"])
    #                                , map_proj)
    # 
    # # dfReachSites <- dfReachSites[,c("StationID_Master", "COMID"
    # #                                 , "FinalLatitude", "FinalLongitude")]
    # 
    # plot(projAllSites)
    # plot(projReachSites)
    # 
    # 
    # map1 <- ggplot2::ggplot(projAllSites, aes(fill="light gray")) +
    #     geom_map(map=map_outline)
    # 
    # 
    # 
    # +
    #     geom_line(map_flowline, aes(x=long, y=lat)) +
    #     coord_map(map_proj)
    # 
    
    
}