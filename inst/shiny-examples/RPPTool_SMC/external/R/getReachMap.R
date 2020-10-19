#  Copyright 2020 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents 
#  is expressly prohibited without prior written permission of TetraTech.
#
#  You can contact the author at:
#  - RPPTool R package source repository : https://github.com/ALincolnTt/RPPTool


# Ann.RoseberryLincoln@tetratech.com
# Erik.Leppo@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.0.2
# 
# library(devtools)
# install_github("ALincolnTt/RPPTool")
#
# Add Shiny code for use in Shiny App
# 2020-09-10, Erik
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 
# Plots a map of the target reach, the reaches within the connectivity distance,
# and any sites on those reaches.
# Reaches color-coded by predicted median CSCI score (SCAPE)
# Sites color-coded by observed recent CSCI score
# Reaches should already have been clipped to the outline. This code does not clip.


getReachMap <- function(dsn_outline, lyr_outline, dsn_flowline, lyr_flowline
                        , allSites, allCxns, allScores, cxndist_km
                        , TargetCOMID=NULL, results_dir, plotLMAP=FALSE) {
    
    boo_DEBUG <- FALSE
    
    if (boo_DEBUG==TRUE) {
        dsn_outline <- file.path(data_dir,"SMCBoundary")
        lyr_outline <- "SMCBoundary"
        dsn_flowline <- file.path(data_dir,"SMCReaches")
        lyr_flowline <- "SMCReachesNHDv2"
        allSites = dfAllSites
        allCxns = dfCxnsALL
        allScores = dfAllScoresSummary
        cxndist_km = cxndist_km
        TargetCOMID = TargetReach
        results_dir = results_dir
        plotLMAP = FALSE
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
                                     ,paste0(TargetCOMID,"_"))
    myDT <- paste0(myDate, "_", myTime)
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
    
    # Graphics parameters
    bestdpi <- 600
    GIS_offset <- 0.1

    # Generate static maps ####
    message(paste0(TargetCOMID, " has ", nrow(sp_cxns), " connected reaches."))
    if (nrow(sp_cxns)==0) {
        sp_bbox <-sp::bbox(sf::as_Spatial(sp_flowline))
        ggmap_bbox <- setNames(sf::st_bbox(sp_flowline),c("left","bottom","right","top"))
        
        caption2 <- "No connected reaches identified"
        maptype <- "SMCRegionReaches"
        maptype2 <- "SMCRegionSites"
        plotScoreMaps <- FALSE
    } else {
        sp_bbox <-sp::bbox(sf::as_Spatial(sp_cxnsTarg))
        ggmap_bbox <- setNames(sf::st_bbox(sp_cxnsTarg),c("left","bottom","right","top"))
        diffLat <- as.numeric(ggmap_bbox[4]-ggmap_bbox[2])
        diffLong <- as.numeric(ggmap_bbox[3]-ggmap_bbox[1])
        if ((diffLat<0.1) | (diffLong<0.1)) {
            GIS_offset <- GIS_offset * 10
        }

        ggmap_bbox[1] <- ggmap_bbox[1] - GIS_offset*(diffLat)
        ggmap_bbox[2] <- ggmap_bbox[2] - GIS_offset*(diffLong)
        ggmap_bbox[3] <- ggmap_bbox[3] + GIS_offset*(diffLat)
        ggmap_bbox[4] <- ggmap_bbox[4] + GIS_offset*(diffLong)

        sp_bbox[1,1] <- sp_bbox[1,1] - GIS_offset*(diffLong)
        sp_bbox[2,1] <- sp_bbox[2,1] - GIS_offset*(diffLat)
        sp_bbox[1,2] <- sp_bbox[1,2] + GIS_offset*(diffLong)
        sp_bbox[2,2] <- sp_bbox[2,2] + GIS_offset*(diffLat)
        
        caption2 <- "Blue lines are connected reaches"
        maptype <- "ConnectedReaches"
        maptype2 <- "ConnectedSites"
        plotScoreMaps <- TRUE
    }
    
    # Plot, loc_map ####
    message("Plot, loc_map")
    basemap_toner <- ggmap::get_map(source="stamen", maptype="toner-lite"
                                    , location=ggmap_bbox, messaging=FALSE)
    toner_map <- ggmap::ggmap(basemap_toner)
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

    fn_locmap <- paste0(base_TargetCOMIDMap,"_",maptype,"_",myDT,".png")
    ggplot2::ggsave(fn_locmap, loc_map, width=7, height=7, units="in", dpi=bestdpi)
    rm(fn_locmap)
    
    # Plot, sites_map ####
    # Plot locator map with sites, not COMIDs
    message("Plot, sites_map")
    if (nrow(sp_sites[sp_sites$COMID %in% sp_cxnsTarg$COMID,])<1) {
        sites_map <- toner_map + 
            ggplot2::geom_sf(data=sp_flowline, inherit.aes=FALSE, color="deepskyblue", lwd=0.5) +
            ggplot2::geom_sf(data=sp_cxns, inherit.aes=FALSE, color="darkblue", lwd=1.5) +
            ggplot2::geom_sf(data=sp_target, inherit.aes=FALSE, color="red", lwd=2) +
            ggplot2::geom_sf(data=sp_outline, inherit.aes=FALSE, fill=NA, color="black"
                             , lwd=1) +
            # ggplot2::geom_sf(data=sp_sites,color="black",pch=21,fill="#F97C5DFF",size=3, na.rm = TRUE) +
            # ggrepel::geom_label_repel(data=sp_sites[sp_sites$COMID %in% sp_cxnsTarg$COMID,]
            #                           , aes(label=StationID_Master, x=lon, y=lat)
            #                           , hjust=1, vjust=0.5, color="black", size=2) +
            ggplot2::theme_bw() +
            ggplot2::labs(x="Longitude", y="Latitude"
                          , title=paste0("Bioassessment sites near ", TargetCOMID)
                          , caption = paste("Red line is the target reach", caption2
                                            , "No sites are located on target or connected reaches."
                                            , sep="\n")) +
            ggplot2::theme(title=element_text(size=12, face="bold", hjust=0.5)
                           , axis.text=element_text(size=8)
                           , axis.title=element_text(size=10)
                           , plot.caption=element_text(size=6, face="italic"))
    } else {
        sites_map <- toner_map + 
            ggplot2::geom_sf(data=sp_flowline, inherit.aes=FALSE, color="deepskyblue", lwd=0.5) +
            ggplot2::geom_sf(data=sp_cxns, inherit.aes=FALSE, color="darkblue", lwd=1.5) +
            ggplot2::geom_sf(data=sp_target, inherit.aes=FALSE, color="red", lwd=2) +
            ggplot2::geom_sf(data=sp_outline, inherit.aes=FALSE, fill=NA, color="black"
                             , lwd=1) +
            ggplot2::geom_sf(data=sp_sites,color="black",pch=21,fill="#F97C5DFF",size=3, na.rm = TRUE) +
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
    }
    fn_sitesmap <- paste0(base_TargetCOMIDMap,"_",maptype2,"_",myDT,".png")
    ggplot2::ggsave(fn_sitesmap, sites_map, width=7, height=7, units="in", dpi=bestdpi)
    rm(fn_sitesmap, sites_map)
    
    # Plot, score_map ####
    message("Plot, score_map")
    if (plotScoreMaps) {
        # Iterate over all scores and other mappable columns
        mapCols <- c("CSCI", "BCGTier", "RPPIndex", "RankByIndexType", "PotSubindex"
                     , "BioCondnInd", "StressorInd", "ThreatSubindex", "FireHazardInd"
                     , "PlannedDevInd", "OppSubindex", "RecreationInd", "MSCPInd"
                     , "NASVIInd", "UserAppliedInd")
        mapTitles <- c("CSCI", "BCG Tier", "RPP Index", "Rank", "Potential Subindex"
                       , "Biological Condition Indicator", "Stressor Indicator"
                       , "Threat Subindex", "Fire Hazard Indicator"
                       , "Planned Development Indicator", "Opportunity Subindex"
                       , "Recreation Indicator", "MSCP Indicator", "NASVI Indicator"
                       , "User-Applied Opportunity Indicator")
        
        for (m in 1:length(mapCols)) {
            
            m_len <- length(mapCols)
            val = mapCols[m]
            m_type <- sp_cxnsTarg$IndexType
            m_title <- mapTitles[m]
            if (val=="RPPIndex" | val=="RankByIndexType") {
                m_title <- paste(sp_cxnsTarg$IndexType[sp_cxnsTarg$COMID==TargetCOMID]
                                 , m_title, sep=" ")
            }
            
            sp_cxnsTargPlot <- sp_cxnsTarg[,c("COMID",val,"lon","lat","geometry")]
            sp_cxnsTargPlot$value <- NA
            
            message(paste0("Mapping ", m, "/", m_len, "; ", val))
            
            if (all(is.na(sp_cxnsTarg[[val]]))) {
                next()
            }
            
            sp_cxnsTargPlot$value <- sp_cxnsTarg[[val]]
            # fullTitle <- paste0(m_title," = ",sp_cxnsTargPlot$value[sp_cxnsTarg$COMID==TargetCOMID])
            score_map <- ggplot2::ggplot(data=sp_flowline, color="deepskyblue", lwd=0.5) +
                ggplot2::geom_sf(data=sp_cxns, inherit.aes=FALSE, color="darkblue", lwd=1.5) +
                ggplot2::geom_sf(data=sp_target, inherit.aes=FALSE, color="red", lwd=2) +
                ggplot2::geom_sf(data=sp_outline, inherit.aes=FALSE, fill=NA, color="black", lwd=1) +
                ggplot2::geom_sf(data=sp_cxnsTargPlot, aes(color=value), inherit.aes = FALSE, lwd=1.5) +
                ggplot2::scale_color_viridis_c(name=val, option="D", direction=-1, na.value="white") +
                ggplot2::coord_sf(datum=4326, xlim = c(ggmap_bbox["left"],ggmap_bbox["right"])
                                  , ylim = c(ggmap_bbox["bottom"], ggmap_bbox["top"])) +
                ggrepel::geom_label_repel(data=sp_cxnsTargPlot
                                          , aes(label=value, x=lon, y=lat)
                                          , inherit.aes = FALSE
                                          , hjust=1, vjust=0.5, size=2
                                          , color=ifelse(m_type=="Restore","red","black")) +
                ggplot2::theme_gray() +
                ggplot2::labs(x="Longitude", y="Latitude"
                              , title=paste0(m_title," = "
                                             , sp_cxnsTargPlot$value[sp_cxnsTarg$COMID==TargetCOMID])
                              , subtitle=paste0(TargetCOMID
                                             , " and connected reaches within "
                                             , cxndist_km, " km")
                              , caption=paste("Target reach shown with red outline."
                                              , "NA values shown as white reaches."
                                              , "Red labels indicate degraded reaches."
                                              , "Black labels indicate not degraded reaches."
                                              , sep="\n")) +
                ggplot2::theme(plot.title=element_text(size=12, face="bold", hjust=0)
                               , plot.subtitle=element_text(size=10, hjust=0)
                               , axis.text=element_text(size=8)
                               , axis.title=element_text(size=10)
                               , plot.caption=element_text(size=6,face="italic")
                               , legend.title = element_text(size=8))
            
            # score_map <- toner_map + 
            #     ggplot2::geom_sf(data=sp_flowline,inherit.aes=FALSE,color="deepskyblue"
            #                      , lwd=0.5) +
            #     ggplot2::geom_sf(data=sp_cxns,inherit.aes=FALSE,color="darkblue",lwd=1.5) +
            #     ggplot2::geom_sf(data=sp_target, inherit.aes=FALSE, color="red"
            #                      , lwd=2) +
            #     ggplot2::geom_sf(data=sp_outline, inherit.aes=FALSE, fill=NA, color="black"
            #                      , lwd=1) +
            #     ggplot2::geom_sf(data=sp_cxnsTargPlot, aes(color=value)
            #                      , inherit.aes = FALSE
            #                      , lwd=1.5) +
            #     ggplot2::scale_color_viridis_c(name=val, option="D", direction=-1
            #                                    , na.value="white") +
            #     # I thought this was going to work, but it may not
            #     ggplot2::geom_sf_label(data=sp_cxnsTargPlot
            #                            , aes(label=value)
            #                            , inherit.aes = FALSE
            #                            , hjust=0.5, vjust=0.5, size=2) +
            #     ggplot2::theme_bw() +
            #     ggplot2::labs(x="Longitude", y="Latitude"
            #                   , title=val
            #                   , subtitle=paste0(TargetCOMID
            #                                  , " and connected reaches within "
            #                                  , cxndist_km, " km")
            #                   , caption=paste("Target reach shown with red outline."
            #                                   , "NA values shown as white reaches"
            #                                   , sep="\n")) +
            #     ggplot2::theme(plot.title=element_text(size=12, face="bold", hjust=0)
            #                    , plot.subtitle=element_text(size=10, hjust=0)
            #                    , axis.text=element_text(size=8)
            #                    , axis.title=element_text(size=10)
            #                    , plot.caption=element_text(size=6,face="italic")
            #                    , legend.title = element_text(size=8))
            # 
            fn_scoremap <- paste0(base_TargetCOMIDMap,"_", val, ".png")
            ggplot2::ggsave(fn_scoremap, score_map, width=7, height=7, units="in"
                            , dpi=bestdpi)
            
        } # End if (static)
        
        rm(fn_scoremap, score_map)
        
    } else { # potScoreMaps==FALSE
        # Do not plot score maps
    }

    if (plotLMAP) {
        BCGlevels = c(1,2,3,4,5,6)
        palBCG <- leaflet::colorFactor(palette = viridis::viridis(n=6, option = "C")
                                       , domain = BCGlevels, reverse = TRUE, na.color="white")
        palScores <- leaflet::colorNumeric(palette = "viridis", domain = c(0,1)
                                           , na.color = "white")
        palCSCI <- leaflet::colorNumeric(palette = "viridis", domain = c(0, 1.5))
        palRank <- leaflet::colorNumeric(palette = grDevices::colorRamp(c("#d8b365", "#fab4ac"))
                                         , sp_flowline$RankByIndexType)
        
        # Establish popup text
        popupText <- paste(as.character("<b>"),"COMID:", sp_flowline$COMID, as.character("</b>")
                           , as.character("<br/>")
                           , ifelse(is.na(sp_flowline$BioType),"", sp_flowline$BioType)
                           , "Biology", as.character("<br>")
                           , "Rank =", sp_flowline$RankByIndexType, as.character("<br>")
                           , ifelse(is.na(sp_flowline$IndexType),"", sp_flowline$IndexType)
                           , "RPP Index =", sp_flowline$RPPIndex, as.character("<br>")
                           , "Potential Subindex =", sp_flowline$PotSubindex, as.character("<br>")
                           , "Bio. Condition =", sp_flowline$BioCondnInd, as.character("<br>")
                           , "Bio. Connectivity =", sp_flowline$BioCxnInd, as.character("<br>")
                           , "Stressor Indicator =", sp_flowline$StressorInd, as.character("<br>")
                           , "Stress. Connectivity =", sp_flowline$StressorCxnInd, as.character("<br>")
                           , "Threat Subindex =", sp_flowline$ThreatSubindex, as.character("<br>")
                           , "Fire Hazard =", sp_flowline$FireHazardInd, as.character("<br>")
                           , "Planned Devel. =", sp_flowline$PlannedDevInd, as.character("<br>")
                           , "Opportunity Subindex =", sp_flowline$OppSubindex, as.character("<br>")
                           , "Recreation =", sp_flowline$RecreationInd, as.character("<br>")
                           , "MSCP =", sp_flowline$MSCPInd, as.character("<br>")
                           , "NASVI =", sp_flowline$NASVIInd, as.character("<br>")
                           , "User-applied =", sp_flowline$UserAppliedInd
                           , sep=" ")
        
        # Plot, lmap ####
        # Draw leaflet map
        message("Plot, lmap")
        if (nrow(sp_cxns)==0) { # No TargetCOMID (Just base version)
            
            lmap <- leaflet::leaflet(data = sp_outline) %>%
                # Groups, Base
                # leaflet::addTiles(group = "OSM (default)") %>%
                leaflet::addProviderTiles(leaflet::providers$Stamen.TonerLite
                                          , group="TonerLite (default)") %>%
                leaflet::addProviderTiles(leaflet::providers$Stamen.Terrain
                                          , group = "Terrain") %>%
                # Groups, Overlay
                leaflet::addPolygons(data = sp_outline, color="black" # Boundary
                                     , group = "SMC Region") %>%
                leaflet::addPolylines(data = sp_flowline # All reaches
                                      , color = "lightblue"
                                      , group = "All streams"
                                      , opacity = 0.6
                                      , popup = ~popupText) %>%
                leaflet::addPolylines(data = sp_target # Target reach only
                                      , group = "Target stream"
                                      , color = "red"
                                      , weight = 10
                                      , opacity = 1
                                      , popup = ~popupText) %>%
                leaflet::addPolylines(data = sp_flowline # All reaches, Rank
                                      , group = "Rank"
                                      , color = ~palRank(RankByIndexType)
                                      , weight = 8
                                      , opacity = 0.8
                                      , popup = ~popupText) %>%
                leaflet::addPolylines(data = sp_flowline # All reaches RPP Index (both types)
                                      , group = "RPP Index"
                                      , color = ~palScores(RPPIndex)
                                      , weight = 8
                                      , opacity = 0.8
                                      , popup = ~popupText) %>%
                leaflet::addCircleMarkers(data=sp_sites # All Sites
                                          , group = "All sites"
                                          , color = "white"
                                          , fillColor = ~palCSCI(CSCI)
                                          , radius = 4
                                          , stroke = TRUE
                                          , weight = 1
                                          , fillOpacity = 0.6
                                          , opacity = 0.6
                                          , popup = ~paste0(as.character("<b>")
                                                            ,StationID_Master
                                                            , as.character("</b>")
                                                            , as.character("<br/>")
                                                            , "CSCI = ", CSCI
                                                            , as.character("<br/>")
                                                            , "BCG Tier = ", BCGLevel)) %>%
                # Add Layer Control
                leaflet::addLayersControl(
                    baseGroups = c("TonerLite (default)", "Terrain")
                    , overlayGroups = c("All sites","Rank","RPP Index"
                                        ,"Target stream","All streams")
                    , options = leaflet::layersControlOptions(collapsed = TRUE
                                                              , autoZIndex = TRUE)) %>%
                
                # Legend
                leaflet::addLegend("bottomleft", pal = palBCG, values = ~BCGlevels
                                   , title = "BCG Tiers", opacity = 1) %>%
                leaflet::addLegend("bottomleft", pal = palCSCI, values = c(0, 1.5)
                                   , bins=5, title = "Site CSCI Scores"
                                   , opacity = 1) %>%
                leaflet::addLegend("bottomleft", pal = palScores, values = c(0,1)
                                   , bins=4, title = "RPPTool Scores", opacity = 1) %>%
                leaflet::addMiniMap("bottomleft")
            
        } else {  # Zoom to connected reaches bounding box
            
            lmap <- leaflet::leaflet(data = sp_outline) %>%
                # Groups, Base
                # leaflet::addTiles(group = "OSM (default)") %>%
                leaflet::addProviderTiles(leaflet::providers$Stamen.TonerLite
                                          , group="TonerLite (default)") %>%
                leaflet::addProviderTiles(leaflet::providers$Stamen.Terrain
                                          , group = "Terrain") %>%
                # leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron
                #                           , group = "Carto Light") %>%
                # Groups, Overlay
                leaflet::addPolygons(data = sp_outline, color="black" # Boundary
                                     , group = "SMC Region") %>%
                leaflet::addPolylines(data = sp_target # Target reach only
                                      , group = "Target stream"
                                      , color = "red"
                                      , weight = 10
                                      , opacity = 1
                                      , popup = ~popupText) %>%
                leaflet::addPolylines(data = sp_cxnsTarg # Connected reaches, Rank
                                      , group = "Rank"
                                      , color = ~palRank(RankByIndexType)
                                      , weight = 8
                                      , opacity = 0.8
                                      , popup = ~popupText) %>%
                leaflet::addPolylines(data = sp_cxnsTarg # Connected reaches RPP Index (both types)
                                      , group = "RPP Index"
                                      , color = ~palScores(RPPIndex)
                                      , weight = 8
                                      , opacity = 0.8
                                      , popup = ~popupText) %>%
                leaflet::addPolylines(data = sp_flowline # All reaches
                                      , group = "All streams"
                                      , color = "lightblue"
                                      , weight = 2
                                      , opacity = 0.6
                                      , popup = ~popupText) %>%
                leaflet::addCircleMarkers(data=sp_sites # All Sites
                                          , group = "All sites"
                                          , color = "white"
                                          , fillColor = ~palCSCI(CSCI)
                                          , radius = 4
                                          , stroke = TRUE
                                          , weight = 1
                                          , fillOpacity = 0.6
                                          , opacity = 0.6
                                          , popup = ~paste0(as.character("<b>")
                                                            ,StationID_Master
                                                            , as.character("</b>")
                                                            , as.character("<br/>")
                                                            , "CSCI = ", CSCI
                                                            , as.character("<br/>")
                                                            , "BCG Tier = ", BCGLevel)) %>%
                # Add Layer Control
                leaflet::addLayersControl(
                    baseGroups = c("TonerLite (default)", "Terrain")
                    # baseGroups = c("Carto Light")
                    , overlayGroups = c("All sites","Rank","RPP Index"
                                        ,"Target stream","All streams")
                    , options = leaflet::layersControlOptions(collapsed = TRUE
                                                              , autoZIndex = TRUE)) %>%
                # Bounding (to connected reaches)
                leaflet::fitBounds(lng1 = sp_bbox[1]
                                   , lat1 = sp_bbox[4]
                                   , lng2 = sp_bbox[3]
                                   , lat2 = sp_bbox[2]) %>%
                # Legend
                leaflet::addLegend("bottomleft", pal = palBCG, values = ~BCGlevels
                                   , title = "BCG Tiers", opacity = 1) %>%
                leaflet::addLegend("bottomleft", pal = palCSCI, values = c(0, 1.5)
                                   , bins=5, title = "Site CSCI Scores"
                                   , opacity = 1) %>%
                leaflet::addLegend("bottomleft", pal = palScores, values = c(0,1)
                                   , bins=4, title = "RPPTool Scores", opacity = 1) %>%
                leaflet::addMiniMap("bottomleft")
            
        }  
        
        return(lmap)
        
    } else {
        
        return(loc_map)
        
    }
    
    # # Leaflet Map in Notebook
    # report_format <- "html"
    # strFile_out_ext <- paste0(".", report_format)
    # strFile_out <- paste0(TargetCOMID,"_MAP_leaflet", strFile_out_ext)
    # ### CHANGE THIS BEFORE PUBLISHING...
    # rmarkdown::render("C:/Users/ann.lincoln/Documents/GitHub/RPPTool/inst/rmd/Map_Leaflet.rmd"
    #                   , output_format=paste0(report_format,"_document")
    #                   , output_file=strFile_out
    #                   , output_dir=file.path(results_dir,TargetCOMID)
    #                   , params = list(sites=sp_sites, boundary=sp_outline
    #                                   , reaches=sp_flowline, cxns=sp_cxns
    #                                   , targ=sp_target)
    #                   , quiet=TRUE)

}
