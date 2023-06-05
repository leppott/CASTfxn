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
#' @title Site Map
#'
#' @description Plot site map of provided TargetSite.
#'
#' @details Plots a map of the region, indicating the target site in red, cluster sites
#' in teal, reference sites in dark blue, cluster reference sites in teal outlined
#' in blue, and the rest of the sites (not same cluster as target & not reference)
#' in grey.
#'
#' @param sp_outline Outline for map, typically State border.
#' @param sp_flowline ypically NHD+ flowline.
#' @param allSites allSites
#' @param TargetSiteID SiteID
#' @param dir_results Directory for results.  Default = "Results".
#' @param dir_sub Subdirectory for outputs from this function.  Default = "SiteInfo"
#' @param dir_map_rmd Directory with Map_Leaflet.RMD.  Default = package RMD.
#'
#' @return A jpg map to a subdirectory "SiteInfo" in the folder named by the SiteID
#' in the user supplied dir_results folder (default is "Results" folder in the
#' working directory).  Also produced is a summary list; SiteInfo, Samps,
#' BMImetrics, AlgMetrics, ReachInfo, COMID, ClustIDs, impair, and mods.
#
# no examples
#
#' @export
getSiteMap <- function(sp_outline, sp_flowline, allSites, TargetSiteID
                       , dir_results, dir_sub, dir_map_rmd) {

  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    sp_outline <- sp_outline
    sp_flowline <- sp_flowline
    allSites = data_Sites
    TargetSiteID = TargetSiteID
    dir_results = dir_results
    dir_sub = "SiteInfo"
    dir_map_rmd = "C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd/"
    # plotLMAP = FALSE
  }

  not_all_na <- function(x) {!all(is.na(x))}

  # Create results dir, if it doesn't exist
  #dir_results = file.path(getwd(), "Results")
  dir_sub2 <- TargetSiteID
  dir_sub3 <- dir_sub
  ifelse(!dir.exists(dir_results) == TRUE
         , dir.create(dir_results)
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, dir_sub2)) == TRUE
         , dir.create(file.path(dir_results, dir_sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, dir_sub2, dir_sub3)) == TRUE
         , dir.create(file.path(dir_results, dir_sub2, dir_sub3))
         , FALSE)

  dir_path <- file.path(dir_results, dir_sub2, dir_sub3)

  # Get filename for saving
  fn_Map <- file.path(dir_path, paste0(TargetSiteID,"_map.png"))

  # Get sites (NAD27 coordinates in dataset, transform to WGS84)
  # Subset ref sites, cluster sites, target site
  allSites <- allSites %>%
    dplyr::select(StationID_Master, FinalLongitude, FinalLatitude
                  , CARefSite_2017, COMID, clust)

  sp_sites <- sf::st_as_sf(allSites, crs = 4267
                           , coords = c("FinalLongitude", "FinalLatitude"))
  sp_sites <- sf::st_transform(sp_sites, crs = 4326) %>%
    mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]])
           , lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))

  sp_refsites <- dplyr::filter(sp_sites, CARefSite_2017 == 1)

  sp_targetsite <- dplyr::filter(sp_sites, StationID_Master == TargetSiteID)

  targetCluster <- as.numeric(sp_targetsite$clust)

  sp_clustsites <- dplyr::filter(sp_sites, clust == targetCluster)

  # Graphics parameters
  bestdpi <- 600
  GIS_offset <- 0.1
  maptype <- "SMCRegionSites"
  maptype2 <- "SMC Region Bioassessment Sites"

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

  cex_sites_all  <- 0.5
  cex_sites_ref  <- 0.9
  cex_sites_cl   <- 1
  cex_sites_targ <- 1.4

  lwd_outline  <- 1.0
  lwd_flowline <- 0.25

  size_legtitle <- 3
  size_legelement <- 2.5

  # Generate static maps ####
  sp_bbox <- sp::bbox(sf::as_Spatial(sp_flowline))
  ggmap_bbox <- setNames(sf::st_bbox(sp_flowline)
                         , c("left","bottom","right","top"))

  diffLat <- as.numeric(ggmap_bbox[4] - ggmap_bbox[2])
  diffLong <- as.numeric(ggmap_bbox[3] - ggmap_bbox[1])

  ggmap_bbox[1] <- ggmap_bbox[1] - GIS_offset*(diffLat)
  ggmap_bbox[2] <- ggmap_bbox[2] - GIS_offset*(diffLong)
  ggmap_bbox[3] <- ggmap_bbox[3] + GIS_offset*(diffLat)
  ggmap_bbox[4] <- ggmap_bbox[4] + GIS_offset*(diffLong)

  site_map <- ggplot2::ggplot(data = sp_outline, fill = NA
                              , color = col_outline, lwd = lwd_outline) +
    ggplot2::geom_sf(data = sp_flowline, inherit.aes = FALSE
                     , color = col_flowline, lwd = lwd_flowline) +
    ggplot2::geom_sf(data = sp_sites, inherit.aes = FALSE
                     , color = col_sites_all, pch = pch_sites_all
                     , size = cex_sites_all) +
    ggplot2::geom_sf(data = sp_clustsites, inherit.aes = FALSE
                     , color = col_sites_cl, pch = pch_sites_cl
                     , size = cex_sites_cl) +
    ggplot2::geom_sf(data = sp_refsites, inherit.aes = FALSE
                     , color = col_sites_ref, pch = pch_sites_ref
                     , size = cex_sites_ref) +
    ggplot2::geom_sf(data = sp_targetsite, inherit.aes = FALSE
                     , color = col_sites_targ, pch = pch_sites_targ
                     , size = cex_sites_targ) +
    ggplot2::geom_sf(data = sp_outline, inherit.aes = FALSE, fill = NA
                     , color = col_outline, lwd = lwd_outline) +
    ggplot2::coord_sf(datum = 4326
                      , xlim = c(ggmap_bbox["left"], ggmap_bbox["right"])
                      , ylim = c(ggmap_bbox["bottom"], ggmap_bbox["top"])) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Longitude", y = "Latitude", title = TargetSiteID
                  , subtitle = maptype2) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold"
                                                      , hjust = 0)
                   , plot.subtitle = ggplot2::element_text(size = 10
                                                           , hjust = 0)
                   , axis.text = ggplot2::element_text(size = 8)
                   , axis.title = ggplot2::element_text(size = 10)
                   , legend.title = ggplot2::element_text(size = 8))
  site_map <- site_map +   # Build legend
    ggplot2::geom_rect(ggplot2::aes(xmin = -119.5, xmax = -118.5
                                    , ymin = 32.5, ymax = 33.5)
                       , color = col_outline, fill = "white"
                       , lwd = lwd_outline) +
    ggplot2::annotate(geom = "text", x = -119.25, y = 33.4
                      , label = "Legend", size = size_legtitle) +
    ggplot2::annotate(geom = "segment", x = -119.4, xend = -119.3
                      , y = 33.2, yend = 33.2, color = col_outline
                      , lwd = lwd_outline) +
    ggplot2::annotate(geom = "segment", x = -119.4, xend = -119.3
                      , y = 33.09, yend = 33.09, color = col_flowline
                      , lwd = lwd_outline) +
    ggplot2::geom_point(ggplot2:::aes(x = -119.35, y = 32.972)
                        , color = col_sites_all, pch = pch_sites_all
                        , size = 1.4) +
    ggplot2::geom_point(ggplot2:::aes(x = -119.35, y = 32.867)
                        , color = col_sites_cl, pch = pch_sites_cl
                        , size = 1.4) +
    ggplot2::geom_point(ggplot2:::aes(x = -119.35, y = 32.760)
                        , color = col_sites_ref, pch = pch_sites_ref
                        , size = 1.4) +
    ggplot2::geom_point(ggplot2:::aes(x = -119.35, y = 32.653)
                        , color = col_sites_targ, pch = pch_sites_targ
                        , size = 1.4) +
    ggplot2::annotate(geom = "text", x = -119.03, y = 33.2
                      , label = "Study boundary", size = size_legelement) +
    ggplot2::annotate(geom = "text", x = -119.12, y = 33.09
                      , label = "Reaches", size = size_legelement) +
    ggplot2::annotate(geom = "text", x = -119.135, y = 32.972
                      , label = "All sites", size = size_legelement) +
    ggplot2::annotate(geom = "text", x = -119.07, y = 32.865
                      , label = "Cluster sites", size = size_legelement) +
    ggplot2::annotate(geom = "text", x = -119.0256, y = 32.763
                      , label = "Reference sites", size = size_legelement) +
    ggplot2::annotate(geom = "text", x = -119.095, y = 32.662
                      , label = "Target site", size = size_legelement)

  ggplot2::ggsave(fn_Map, site_map, width = 7, height = 7, units = "in"
                  , dpi = bestdpi)

  #
  # Leaflet Map in Notebook
  report_format <- "html"
  strFile_out_ext <- paste0(".", report_format)
  strFile_out <- paste0(TargetSiteID,"_MAP_leaflet", strFile_out_ext)

  rmarkdown::render(file.path(dir_map_rmd, "Map_Leaflet2.rmd")
                    , output_format = paste0(report_format,"_document")
                    , output_file = strFile_out
                    , output_dir = dir_path
                    , quiet = TRUE)

  # place after static map so can insert static map into report

}
