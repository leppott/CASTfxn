# getReachMap (Specific for SMC)
# Ann.RoseberryLincoln@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 
# Plots a map of the target reach, the reaches within the connectivity distance,
# and any sites on those reaches.
# Reaches color-coded by predicted median CSCI score (SCAPE)
# Sites color-coded by observed recent CSCI score
# Reaches should already have been clipped to the outline. This code does not clip.


getReachMap <- function(dsn_outline, lyr_outline, dsn_flowline, lyr_flowline
                        , allSites, allCxns, allScores, cxndist_km
                        , TargetCOMID=NULL, results_dir) {
    
    boo_DEBUG <- TRUE
    
    if (boo_DEBUG==TRUE) {
        dsn_outline <- file.path(data_dir,"SMCBoundary")
        lyr_outline <- "SMCBoundary"
        dsn_flowline <- file.path(data_dir,"SMCReaches")
        lyr_flowline <- "SMCReachesNHDv2"
        allSites = listBCGdata$obsSiteBCGxy
        allCxns = dfCxnsALL
        allScores = listAllScores$dfAllScoresSummary
        cxndist_km = cxndist_km
        TargetCOMID = 20331434
        results_dir = results_dir
    }
    
    not_all_na <- function(x) {!all(is.na(x))}
    
    # Create results dir, if it doesn't exist
    ifelse(!dir.exists(file.path(results_dir))==TRUE
           , dir.create(file.path(results_dir))
           , FALSE)
    ifelse(!dir.exists(file.path(results_dir, TargetCOMID))==TRUE
           , dir.create(file.path(results_dir, TargetCOMID))
           , FALSE)
    
    # Get filename for saving
    myTime <- lubridate::now()
    myDate <- stringr::str_replace_all(stringr::str_extract(myTime,"\\d{4}-\\d{2}-\\d{2}")
                                       , "-", "")
    myTime <- stringr::str_replace_all(stringr::str_extract(myTime,"\\d{2}:\\d{2}:\\d{2}")
                                       , ":", "")
    base_TargetCOMIDMap <- file.path(results_dir, TargetCOMID
                                     ,paste0("AllScores"))
    rm(myDate, myTime)
    
    # Get reaches connected to target reach
    cxnReaches <- unique(allCxns$COMID[allCxns$TargetCOMID==TargetCOMID])

    # Get sites (NAD27 coordinates in dataset, transform to WGS84) with most recent obs.
    allSites <- allSites %>%
        dplyr::group_by(StationID_Master, COMID, FinalLongitude, FinalLatitude) %>%
        dplyr::filter(BMISampleDate==max(BMISampleDate)) %>%
        dplyr::select(StationID_Master, COMID, FinalLongitude, FinalLatitude
                      , CSCI, BCGLevel)
        
    sp_sites <- sf::st_as_sf(allSites, crs=4267
                             , coords=c("FinalLongitude","FinalLatitude"))
    sp_sites <- sf::st_transform(sp_sites, crs=4326) %>%
        mutate(lon=purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]])
               , lat=purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
    
    # Get boundary file for desired region ####
    sp_outline <- sf::read_sf(dsn = file.path(dsn_outline)
                              , layer = lyr_outline) %>%
        sf::st_transform(crs=4326) # EPSG identifier for WGS84

    # Get all flowlines ####
    sp_flowline <- sf::read_sf(dsn=file.path(dsn_flowline)
                               , layer=lyr_flowline) %>%
        sf::st_transform(crs=4326) %>%
        sf::st_zm(drop=TRUE, what="ZM")
    # Alternate: use handyFunctions repository for getNHDfunctions
    # sp_flowline <- get_flowlines(1, sp_outline, crs=4326) # get flowline from web
    # Note: If get data from web, comid is lowercase, not uppercase
    # sp_flowline <- sf::st_intersection(sp_flowline, sp_outline) # clip to boundary (long)
    
    # Add scores to flowlines
    sp_flowline <- merge(sp_flowline, allScores, by.x="COMID"
                         , by.y="COMID", all.x=TRUE)
    sp_flowline <- dplyr::select(sp_flowline, COMID, CSCI, BCGTier, BioType
                                 , IndexType, RPPIndex, RankByIndexType
                                 , PotSubindex, BioCondnInd, BioCxnInd
                                 , StressorInd, StressorCxnInd, ThreatSubindex
                                 , FireHazardInd, PlannedDevInd, OppSubindex
                                 , RecreationInd, MSCPInd, NASVIInd, UserAppliedInd
                                 , geometry)
    
    # Get connected reaches and target reach ####
    sp_cxns <- sp_flowline[sp_flowline$COMID %in% cxnReaches,] %>%
        dplyr::mutate(lon=purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]])
               , lat=purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
    sp_target <- sp_flowline[sp_flowline$COMID==TargetCOMID,] %>%
        dplyr::mutate(lon=purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]])
               , lat=purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
    sp_cxnsTarg <- rbind(sp_cxns, sp_target)
    
    # Generate static maps ####
    bestdpi <- 600
    if (nrow(sp_cxns)==0) {
        sp_bbox <-sp::bbox(sf::as_Spatial(sp_flowline))
        
        ggmap_bbox <- setNames(sf::st_bbox(sp_flowline),c("left","bottom","right","top"))
        basemap_toner <- ggmap::get_map(source="stamen", maptype="toner-lite"
                                        , location=ggmap_bbox, messaging=FALSE)
        toner_map <- ggmap::ggmap(basemap_toner)
        caption2 <- "No connected reaches identified"
        plotScoreMaps <- FALSE
    } else {
        sp_bbox <-sp::bbox(sf::as_Spatial(sp_cxns))
        
        ggmap_bbox <- setNames(sf::st_bbox(sp_cxns),c("left","bottom","right","top"))
        basemap_toner <- ggmap::get_map(source="stamen", maptype="toner-lite"
                                        , location=ggmap_bbox, messaging=FALSE)
        toner_map <- ggmap::ggmap(basemap_toner)
        caption2 <- ""
        plotScoreMaps <- TRUE
    }
    loc_map <- toner_map + 
        ggplot2::geom_sf(data=sp_flowline, inherit.aes=FALSE, color="deepskyblue"
                         , lwd=0.5) +
        ggplot2::geom_sf(data=sp_cxns, inherit.aes=FALSE, color="darkblue", lwd=1.5) +
        ggplot2::geom_sf(data=sp_target, inherit.aes=FALSE, color="red", lwd=2) +
        ggplot2::geom_sf(data=sp_outline, inherit.aes=FALSE, fill=NA, color="black"
                         , lwd=1) +
        ggrepel::geom_label_repel(data=sp_cxns, aes(label=COMID, x=lon, y=lat)
                                  , hjust=0.5, vjust=0.5#, nudge_x=-0.05, nudge_y=-0.01
                                  , color="black", size=2) +
        ggrepel::geom_label_repel(data=sp_target, aes(label=COMID, x=lon, y=lat)
                                  , hjust=0.5, vjust=0.5#, nudge_x=-0.05, nudge_y=-0.01
                                  , color="black", size=2) +
        ggplot2::theme_bw() +
        ggplot2::labs(x="Longitude", y="Latitude"
                      , title=paste0(TargetCOMID, " Connected Reaches within "
                                     , cxndist_km, " km")
                      , caption = paste("Red line is the target reach", caption2
                                        , sep="\n")) +
        ggplot2::theme(title = element_text(size=12, face="bold", hjust=0.5)
                       , axis.text = element_text(size=8)
                       , axis.title = element_text(size=10)
                       , plot.caption = element_text(size=6, face="italic"))
    # locmap2 <- loc_map + ggplot2::coord_sf(crs=4326)
        
    fn_locmap <- paste0(base_TargetCOMIDMap,"_",TargetCOMID,"_ConnectedReaches.png")
    ggplot2::ggsave(fn_locmap, loc_map, width=7, height=7, units="in", dpi=bestdpi)
    
    # Plot locator map with sites, not COMIDs
    sites_map <- toner_map + 
        ggplot2::geom_sf(data=sp_flowline, inherit.aes=FALSE, color="deepskyblue", lwd=0.5) +
        ggplot2::geom_sf(data=sp_cxns, inherit.aes=FALSE, color="darkblue", lwd=1.5) +
        ggplot2::geom_sf(data=sp_target, inherit.aes=FALSE, color="red", lwd=2) +
        ggplot2::geom_sf(data=sp_outline, inherit.aes=FALSE, fill=NA, color="black"
                         , lwd=1) +
        ggplot2::geom_sf(data=sp_sites,color="black",pch=21,fill="#F97C5DFF",size=3) +
        ggrepel::geom_label_repel(data=sp_sites[sp_sites$COMID %in% sp_cxnsTarg$COMID,]
                                  , aes(label=StationID_Master, x=lon, y=lat)
                                  , hjust=1, vjust=0.5, color="black", size=2) +
        ggplot2::theme_bw() +
        ggplot2::labs(x="Longitude", y="Latitude"
                      , title=paste0("Bioassessment sites near ", TargetCOMID)
                      , caption = paste("Red line is the target reach", caption2
                                        , sep="\n")) +
        ggplot2::theme(title=element_text(size=12, face="bold", hjust=0.5)
                       , axis.text=element_text(size=8)
                       , axis.title=element_text(size=10)
                       , plot.caption=element_text(size=6, face="italic"))
    fn_sitesmap <- paste0(base_TargetCOMIDMap,"_",TargetCOMID,"_Sites.png")
    ggplot2::ggsave(fn_sitesmap, sites_map, width=7, height=7, units="in", dpi=bestdpi)
    
    if (plotScoreMaps) {
        # Iterate over all scores and other mappable columns
        # Plot RankByIndexType only for IndexType == TargetReach IndexType
        # mapCols <- c("CSCI", "BCGTier", "RPPIndex", "RankByIndexType", "PotSubindex"
        #              , "BioCondnInd", "BioCxnInd", "StressorInd", "StressorCxnInd"
        #              , "ThreatSubindex", "FireHazardInd", "PlannedDevInd", "OppSubindex"
        #              , "RecreationInd", "MSCPInd", "NASVIInd", "UserAppliedInd")
        # Do no plot CxnInd as they are not useful...only show the score at the target reach
        mapCols <- c("CSCI", "BCGTier", "RPPIndex", "RankByIndexType", "PotSubindex"
                     , "BioCondnInd", "StressorInd", "ThreatSubindex", "FireHazardInd"
                     , "PlannedDevInd", "OppSubindex", "RecreationInd", "MSCPInd"
                     , "NASVIInd", "UserAppliedInd")
        
        for (m in 1:length(mapCols)) {
            
            val = mapCols[m]
            sp_cxnsTargPlot <- sp_cxnsTarg[,c("COMID",val,"lon","lat","geometry")]
            sp_cxnsTargPlot$value <- NA
            
            print(paste0("Mapping ", val))
            flush.console()
            
            if (all(is.na(sp_cxnsTarg[[val]]))) {
                next()
            }
            
            sp_cxnsTargPlot$value <- sp_cxnsTarg[[val]]
            
            score_map <- toner_map + 
                ggplot2::geom_sf(data=sp_flowline,inherit.aes=FALSE,color="deepskyblue"
                                 , lwd=0.5) +
                ggplot2::geom_sf(data=sp_cxns,inherit.aes=FALSE,color="darkblue",lwd=1.5) +
                ggplot2::geom_sf(data=sp_target, inherit.aes=FALSE, color="red"
                                 , lwd=2) +
                ggplot2::geom_sf(data=sp_outline, inherit.aes=FALSE, fill=NA, color="black"
                                 , lwd=1) +
                ggplot2::geom_sf(data=sp_cxnsTargPlot, aes(color=value)
                                 , inherit.aes = FALSE
                                 , lwd=1.5) +
                ggplot2::scale_color_viridis_c(name=val, option="D", direction=-1
                                               , na.value="white") +
                # I thought this was going to work, but it may not
                ggplot2::geom_sf_label(data=sp_cxnsTargPlot
                                       , aes(label=value)
                                       , inherit.aes = FALSE
                                       , hjust=0.5, vjust=0.5, size=2) +
                ggplot2::theme_bw() +
                ggplot2::labs(x="Longitude", y="Latitude"
                              , title=paste0(TargetCOMID
                                             , " and connected reaches within "
                                             , cxndist_km, " km")
                              , caption=paste("Target reach shown with red outline."
                                              , "NA values shown as white reaches"
                                              , sep="\n")) +
                ggplot2::theme(title=element_text(size=12, face="bold", hjust=0.5)
                               , axis.text=element_text(size=8)
                               , axis.title=element_text(size=10)
                               , plot.caption=element_text(size=6,face="italic")
                               , legend.title = element_text(size=8))
            
            fn_scoremap <- paste0(base_TargetCOMIDMap,"_",TargetCOMID,"_", val
                                  , ".png")
            ggplot2::ggsave(fn_scoremap, score_map, width=7, height=7, units="in"
                            , dpi=bestdpi)
            
        } # End if (static)
    } else {
        # Do not plot score maps
    }
    
# START HERE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Currently have:
    # sp_outline -- simple polygon outline of SMC region
    # sp_flowline
    # sp_cxns
    # sp_target
    # sp_cxnsTarget
    # sp_sites
    
    # all flowline layers have the following fields:
    # "COMID", "CSCI", "BCGTier", "BioType", IndexType", "RPPIndex"       
    # , "RankByIndexType", "PotSubindex", "BioCondnInd", "BioCxnInd"
    # , "StressorInd", "StressorCxnInd", "ThreatSubindex", "FireHazardInd"
    # , "PlannedDevInd", "OppSubindex", "RecreationInd", "MSCPInd" 
    # , "NASVIInd", UserAppliedInd", "lon", "lat", "geometry"
    
    # Do not bother to plot BioCxnInd or StressorCxnInd
    # Do not plot (but use to label) BioType and IndexType
    
    # Sites data have the following fields (most recent only):
    # "StationID_Master", "COMID", "CSCI", "BCGLevel", "geometry", "lon", "lat"             
    # 
    # # Set palettes
    BCGlevels = c(1,2,3,4,5,6)
    palBCG <- colorFactor(palette = viridis(n=6, option = "C")
                          , domain = BCGlevels, reverse = TRUE
                          , na.color="white")
    palScores <- colorNumeric(palette = "viridis", domain = c(0,1)
                              , na.color = "white")
    palCSCI <- colorNumeric(palette = "viridis", domain = c(0, 1.5))
    palRank <- colorNumeric(palette = colorRamp(c("#d8b365", "#fab4ac"))
                                                , sp_flowline$RankByIndexType)
    # 
    if (is.null(TargetCOMID)) { # No TargetCOMID (Just base version)
        lmap <- leaflet::leaflet(data = sp_outline) %>%
            # Groups, Base
            leaflet::addTiles(group = "OSM (default)") %>%
            leaflet::addProviderTiles(leaflet::providers$Stamen.Terrain, group = "Terrain") %>%
            leaflet::addProviderTiles(leaflet::providers$Stamen.TonerLite, group="TonerLite") %>%
            # Groups, Overlay
            leaflet::addPolygons(data = sp_outline, color="black" # Boundary
                                 , group = "SMC Region") %>%
            leaflet::addPolylines(data = sp_flowline # All reaches
                                  , color = "lightblue"
                                  , group = "All streams"
                                  , opacity = 0.6
                                  , popup = ~paste(COMID)) %>%
            leaflet::addPolylines(data = sp_flowline # All reaches, Rank
                                  , group = "Rank"
                                  , color = ~palRank(RankByIndexType)
                                  , weight = 8
                                  , opacity = 0.8
                                  , popup = ~paste(COMID, as.character("<br>")
                                                   , ifelse(is.na(BioType),""
                                                            , BioType)
                                                   , "Rank ="
                                                   , RankByIndexType,sep=" ")) %>%
            leaflet::addPolylines(data = sp_flowline # All reaches, CSCI
                                  , group = "CSCI"
                                  , color = ~palCSCI(CSCI)
                                  , weight = 8
                                  , opacity = 0.8
                                  , popup = ~paste(COMID, as.character("<br>")
                                                   , ifelse(is.na(BioType),""
                                                            , BioType)
                                                   , " CSCI ="
                                                   , CSCI,sep = " ")) %>%
            leaflet::addPolylines(data = sp_flowline # All reaches, BCG Tier
                                  , group = "BCG"
                                  , color = ~palBCG(BCGTier)
                                  , weight = 8
                                  , opacity = 0.8
                                  , popup = ~paste(COMID, as.character("<br>")
                                                   , ifelse(is.na(BioType),""
                                                            , BioType)
                                                   , " BCG Tier ="
                                                   , BCGTier,sep = " ")) %>%
            leaflet::addPolylines(data = sp_flowline # All reaches RPP Index (both types)
                                  , group = "RPP Index"
                                  , color = ~palScores(RPPIndex)
                                  , weight = 8
                                  , opacity = 0.8
                                  , popup = ~paste(COMID, as.character("<br>")
                                                   , ifelse(is.na(IndexType),""
                                                            , IndexType)
                                                   , "Potential ="
                                                   , RPPIndex,sep = " ")) %>%
            leaflet::addPolylines(data = sp_flowline # All reaches Potential Subindex (both types)
                                  , group = "Potential Subindex"
                                  , color = ~palScores(PotSubindex)
                                  , weight = 8
                                  , opacity = 0.8
                                  , popup = ~paste(COMID, as.character("<br>")
                                                   , ifelse(is.na(IndexType),""
                                                            , IndexType)
                                                   , "Potential Subindex ="
                                                   , PotSubindex,sep = " ")) %>%
            # leaflet::addPolylines(data = sp_flowline # All reaches Biological Condition (both types)
            #                       , group = "Biological Condition"
            #                       , color = ~palScores(BioCondnInd)
            #                       , weight = 8
            #                       , opacity = 0.8
            #                       , popup = ~paste(COMID, as.character("<br>")
            #                                        , ifelse(is.na(IndexType),""
            #                                                 , IndexType)
            #                                        , "Biological Condition ="
            #                                        , BioCondnInd,sep = " ")) %>%
            # leaflet::addPolylines(data = sp_flowline # All reaches Stressor Indicator
            #                       , group = "Stressor Indicator"
            #                       , color = ~palScores(StressorInd)
            #                       , weight = 8
            #                       , opacity = 0.8
            #                       , popup = ~paste(COMID, as.character("<br>")
            #                                        , "Stressor Indicator ="
            #                                        , StressorInd,sep = " ")) %>%
            leaflet::addPolylines(data = sp_flowline # All reaches Threat Subindex
                                  , group = "Threat Subindex"
                                  , color = ~palScores(ThreatSubindex)
                                  , weight = 8
                                  , opacity = 0.8
                                  , popup = ~paste(COMID, as.character("<br>")
                                                   , "Threat Subindex ="
                                                   , ThreatSubindex,sep = " ")) %>%
            # leaflet::addPolylines(data = sp_flowline # All reaches Fire Hazard
            #                       , group = "Fire Hazard"
            #                       , color = ~palScores(FireHazardInd)
            #                       , weight = 8
            #                       , opacity = 0.8
            #                       , popup = ~paste(COMID, as.character("<br>")
            #                                        , "Fire Hazard ="
            #                                        , FireHazardInd,sep = " ")) %>%
            # leaflet::addPolylines(data = sp_flowline # All reaches Planned Development
            #                       , group = "Planned Development"
            #                       , color = ~palScores(PlannedDevInd)
            #                       , weight = 8
            #                       , opacity = 0.8
            #                       , popup = ~paste(COMID, as.character("<br>")
            #                                        , "Planned Development ="
            #                                        , PlannedDevInd,sep = " ")) %>%
            leaflet::addPolylines(data = sp_flowline # All reaches Opportunity Subindex
                                  , group = "Opportunity Subindex"
                                  , color = ~palScores(OppSubindex)
                                  , weight = 8
                                  , opacity = 0.8
                                  , popup = ~paste(COMID, as.character("<br>")
                                                   , "Opportunity Subindex ="
                                                   , OppSubindex,sep = " ")) %>%
            # leaflet::addPolylines(data = sp_flowline # All reaches Recreation
            #                       , group = "Recreation Indicator"
            #                       , color = ~palScores(RecreationInd)
            #                       , weight = 8
            #                       , opacity = 0.8
            #                       , popup = ~paste(COMID, as.character("<br>")
            #                                        , "Recreation Use ="
            #                                        , RecreationInd,sep = " ")) %>%
            # leaflet::addPolylines(data = sp_flowline # All reaches MSCP
            #                       , group = "MSCP Indicator"
            #                       , color = ~palScores(MSCPInd)
            #                       , weight = 8
            #                       , opacity = 0.8
            #                       , popup = ~paste(COMID, as.character("<br>")
            #                                        , "MSCP Indictor ="
            #                                        , MSCPInd,sep = " ")) %>%
            # leaflet::addPolylines(data = sp_flowline # All reaches NASVI
            #                       , group = "NASVI Indicator"
            #                       , color = ~palScores(NASVIInd)
            #                       , weight = 8
            #                       , opacity = 0.8
            #                       , popup = ~paste(COMID, as.character("<br>")
            #                                        , "NASVI Indicator ="
            #                                        , NASVIInd,sep = " ")) %>%
            # leaflet::addPolylines(data = sp_flowline # All reaches User-applied
            #                       , group = "User-applied Indicator"
            #                       , color = ~palScores(UserAppliedInd)
            #                       , weight = 8
            #                       , opacity = 0.8
            #                       , popup = ~paste(COMID, as.character("<br>")
            #                                        , "User-applied Indicator ="
            #                                        , UserAppliedInd,sep = " ")) %>%
            leaflet::addCircleMarkers(data=sp_sites # All Sites
                                      , group = "All sites"
                                      , color = "white"
                                      , fillColor = ~palCSCI(CSCI)
                                      , radius = 4
                                      , stroke = TRUE
                                      , weight = 1
                                      , fillOpacity = 0.6
                                      , opacity = 0.6
                                      , popup = ~paste0(StationID_Master, as.character("<br>")
                                                        , "CSCI =", CSCI, as.character("<br>")
                                                        , "BCG Tier =", BCGLevel)) %>%
            # Add Layer Control
            leaflet::addLayersControl(
                baseGroups = c("OSM (default)", "Terrain", "TonerLite")
                , overlayGroups = c("All sites", "Rank", "CSCI", "BCG", "RPP Index"
                                    , "Potential Subindex"#, "Biological Condition"
                                    # , "Stressor Indicator"
                                    , "Threat Subindex"
                                    # , "Fire Hazard", "Planned Development"
                                    , "Opportunity Subindex"#, "Recreation Indicator"
                                    # , "MSCP Indicator", "NASVI Indicator"
                                    # , "User-applied Indicator"
                                    , "All streams")
                , options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) %>%
            # leaflet::hideGroup(c("CSCI", "BCG", "Potential Subindex"
            #                      , "Biological Condition", "Stressor Indicator"
            #                      , "Threat Subindex", "Fire Hazard"
            #                      , "Planned Development", "Opportunity Subindex"
            #                      , "Recreation Indicator", "MSCP Indicator"
            #                      , "NASVI Indicator", "User-applied Indicator")) %>%
            # Legend
            leaflet::addLegend("bottomleft", pal = palBCG, values = ~BCGlevels
                               , title = "BCG Tiers", opacity = 1) %>%
            leaflet::addLegend("bottomleft", pal = palCSCI, values = c(0, 1.5)
                               , bins=5, title = "Site CSCI Scores"
                               , opacity = 1) %>%
            leaflet::addLegend("bottomleft", pal = palScores, values = c(0,1)
                               , bins=4, title = "RPPTool Scores", opacity = 1) %>%
            leaflet::addMiniMap("bottomleft")
        
        # Save map
        mapview::mapshot(lmap, file = file.path(results_dir,"SMC_lmap.png"))
        
    } else { # Target reach is specified ####

        # Get connected reach shapefile ####
        # reachCxns <- as.numeric(dfCxnsALL$COMID[dfCxnsALL$TargetCOMID==TargetCOMID])
        # reachCxns <- c(TargetCOMID, reachCxns)
        # sp_reachCxns <- sp_flowline[sp_flowline@data$COMID %in% reachCxns,]
        # comsInShpFile <- sp_reachCxns@data$COMID
        # 
        # # Add predicted BCG
        # dfReachPredBCG50 <- unique(dfReachPredBCG50[dfReachPredBCG50$COMID %in% comsInShpFile,])
        # sp_reachCxnBCGs <- sp::merge(sp_reachCxns, dfReachPredBCG50
        #                              , by.x = "COMID", by.y = "COMID", all.x = TRUE)
        # 
        # # Add connected reach scores ####
        # reachCxnScores <- unique(allScores[allScores$COMID %in% comsInShpFile,])
        # sp_allreachscores <- sp::merge(sp_reachCxns, reachCxnScores
        #                                , by.x = "COMID", by.y = "COMID", all.x = TRUE)
        # 
        # # ID target reach separately ####
        # sp_target <- sp_flowline[sp_flowline@data$COMID==TargetCOMID,]
        # 
        # lmap <- leaflet::leaflet(data = sp_outline) %>%
        #     # Groups, Base
        #     leaflet::addTiles(group = "OSM (default)") %>%
        #     leaflet::addProviderTiles(leaflet::providers$Stamen.Terrain, group = "Terrain") %>%
        #     # Groups, Overlay
        #     leaflet::addPolygons(data = sp_outline, color="black"
        #                          , group = "SMC Region") %>%
        #     leaflet::addPolylines(data = sp_flowline
        #                           , color = "lightblue"
        #                           , group = "All streams"
        #                           , opacity = 0.6
        #                           , popup = ~paste(COMID)) %>%
        #     leaflet::addPolylines(data = sp_target
        #                           , color = "black"
        #                           , stroke = TRUE
        #                           , weight = 10
        #                           , opacity = 1
        #                           , label = TargetCOMID) %>%
        #     leaflet::addPolylines(data = sp_reachCxnBCGs
        #                           , group = "Streams with predicted BCG"
        #                           , color = ~palBCG(BCGqt50)
        #                           , stroke = TRUE
        #                           , weight = 8
        #                           , opacity = 0.8
        #                           , popup = ~paste(COMID, as.character("<br>")
        #                                            , "BCG (predicted) ="
        #                                            , BCGqt50,sep = " ")) %>%
        #     leaflet::addPolylines(data = sp_allreachscores
        #                           , group = "Protection RPPIndex"
        #                           , color = ~palScores(idx_RPPIndex_Prot)
        #                           , stroke = TRUE
        #                           , weight = 8
        #                           , opacity = 0.8
        #                           , popup = ~paste(COMID, as.character("<br>")
        #                                            , "Protection Potential ="
        #                                            , idx_RPPIndex_Prot, sep = " ")) %>%
        #     leaflet::addPolylines(data = sp_allreachscores
        #                           , group = "Restoration Potential"
        #                           , color = ~palScores(idx_RPPIndex_Rest)
        #                           , stroke = TRUE
        #                           , weight = 8
        #                           , opacity = 0.8
        #                           , popup = ~paste(COMID, as.character("<br>")
        #                                            , "Restoration Potential ="
        #                                            , idx_RPPIndex_Rest, sep = " ")) %>%
        #     leaflet::addCircleMarkers(data=allSites
        #                               , lng = ~FinalLongitude
        #                               , lat = ~FinalLatitude
        #                               , group = "All sites"
        #                               , color = "black"
        #                               , fillColor = ~palBCG(BCGLevel)
        #                               , radius = 4
        #                               , stroke = TRUE
        #                               , weight = 1
        #                               , fillOpacity = 0.6
        #                               , opacity = 0.6
        #                               , popup = ~paste0(StationID_Master, as.character("<br>")
        #                                                 , "CSCI =", CSCI, as.character("<br>")
        #                                                 , "BCG Level =", BCGLevel)) %>%
        # # leaflet::addCircleMarkers(data = siteCxns
        # #                           , lng = ~FinalLongitude
        # #                           , lat = ~FinalLatitude
        # #                           , group = "Connected sites"
        # #                           , color = "black"
        # #                           , fillColor = ~pal(BCGLevel)
        # #                           , radius = 4
        # #                           , stroke = TRUE
        # #                           , weight = 1
        # #                           , fillOpacity = 1
        # #                           , opacity = 1
        # #                           , popup = ~paste0(StationID_Master, as.character("<br>")
        # #                                             , "CSCI =", CSCI, as.character("<br>")
        # #                                             , "BCG Level =", BCGLevel)) %>%
        # # Bounding (to connected reaches)
        # fitBounds(lng1 = sp_reachCxnBCGs@bbox[1]
        #           , lat1 = sp_reachCxnBCGs@bbox[4]
        #           , lng2 = sp_reachCxnBCGs@bbox[3]
        #           , lat2 = sp_reachCxnBCGs@bbox[2]) %>%
        #     # Layers
        #     leaflet::addLayersControl(
        #         baseGroups = c("OSM (default)", "Terrain")
        #         , overlayGroups = c("Streams with predicted BCG"
        #                             , "Protection Potential"
        #                             , "Restoration Potential"
        #                             , "All sites")) %>%
        #     # Legend
        #     leaflet::addLegend("bottomleft", pal = palBCG, values = ~BCGlevels
        #                        , title = "BCG levels", opacity = 1) %>%
        #     leaflet::addLegend("bottomleft", pal = palScores, values = c(0,1)
        #                        , title = "Scores", opacity = 1) %>%
        #     leaflet::addMiniMap("bottomleft")
        # 
        # mapview::mapshot(lmap, file = file.path(results_dir,"TEST_MAP.png"))

    } # End target network map
    
}
