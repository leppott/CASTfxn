#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#  You can contact the author at:
#  - RPPTool R package source repository : https://github.com/ALincolnTt/RPPTool


# Ann.RoseberryLincoln@tetratech.com
# Erik.Leppo@tetratech.com
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.3.1
#
# library(devtools)
# install_github("ALincolnTt/RPPTool")
#
# requires dplyr, ggplot2, purrr, sf
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
#' @param sp_outline Spatial dataframe representing region (or subregion) boundary.
#' @param sp_flowline Spatial dataframe representing stream reaches.
#' @param region Region or regulatory organization to which the data apply (e.g., AZ, SMC, WA, OR)
#' @param df_sites dataframe containing site data, including both "inside the case"
#'                 and "outside the case" identifiers.
#' @param outcaseID Identifier for "outside the case" sites
#' @param incaseID Identifier for "inside the case" sites
#' @param TargetSiteID site identifier for the site being evaluated (the Target Site)
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = "FALSE"
#' @param dir_results Directory containing all results. Default is file.path(getwd(),"Results")
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo".
#' @param dir_map_rmd Directory containing the leaflet map template.
#'
#' @return A png map to a subdirectory "SiteInfo" in the folder named by the SiteID
#' in the user supplied dir_results folder (default is "Results" folder in the
#' working directory).  Also produced is a summary list; SiteInfo, Samps,
#' BMImetrics, AlgMetrics, ReachInfo, COMID, ClustIDs, impair, and mods.
#
# no examples
#
#' @export
getSiteMap <- function(sp_outline
                       , sp_flowline
                       , region
                       , df_sites
                       , allSites
                       , compSites
                       , TargetSiteID
                       , useBC = FALSE
                       , dir_results = file.path(getwd(), "Results")
                       , dir_sub = "SiteInfo"
                       , dir_map_rmd
                       ) {

  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    sp_outline = STATE.shp
    sp_flowline = NHD.STATE
    region = regionName
    df_sites = data_Sites
    allSites = all_sites
    compSites = comp_sites
    TargetSiteID = TargetSiteID
    useBC = TRUE
    dir_results = dir_results
    dir_sub = "SiteInfo"
    dir_map_rmd = "C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd/"
    # plotLMAP = FALSE
  }

  # Check function arguments
  if (is.null(sp_outline)) {
    msg("Please provide boundary (sp_outline).")
    stop()
  }
  if (is.null(sp_flowline)) {
    msg("Please provide reach file (sp_flowline).")
    stop()
  }
  if (is.null(df_sites)) {
    msg("Please provide sites file (df_sites).")
    stop()
  }
  if (is.null(TargetSiteID)) {
    msg("Please provide Target site identifier (TargetSiteID).")
    stop()
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
  fn_Map <- file.path(dir_path, paste0(TargetSiteID, "_map.png"))

  # Create color vector (range 2 to 6) for reaches
  if (useBC == TRUE) {
    maxClusterID <- max(as.numeric(df_sites$OutcaseCol), na.rm = TRUE)
    if (maxClusterID == 6) {
      mag.vec <- viridis::viridis(23)[c(3,7,11,15,19,23)]
    } else if (maxClusterID == 5) {
      mag.vec <- viridis::viridis(19)[c(3,7,11,15,19)]
    } else if (maxClusterID == 4) {
      mag.vec <- viridis::viridis(15)[c(3,7,11,15)]
    } else if (maxClusterID == 3) {
      mag.vec <- viridis::viridis(11)[c(3,7,11)]
    } else { # max clusters = 2
      mag.vec <- viridis::viridis(7)[c(3,7)]
    }
  } else {
    maxClusterID <- max(as.numeric(df_sites$IncaseCol), na.rm = TRUE)
    if (maxClusterID == 6) {
      mag.vec <- viridis::viridis(23)[c(3,7,11,15,19,23)]
    } else if (maxClusterID == 5) {
      mag.vec <- viridis::viridis(19)[c(3,7,11,15,19)]
    } else if (maxClusterID == 4) {
      mag.vec <- viridis::viridis(15)[c(3,7,11,15)]
    } else if (maxClusterID == 3) {
      mag.vec <- viridis::viridis(11)[c(3,7,11)]
    } else { # max clusters = 2
      mag.vec <- viridis::viridis(7)[c(3,7)]
    }
  }

  # Get sites (if datum is specified in the metadata, transform to WGS84)
  # Subset ref sites, outside case sites, inside case sites, and target site
  df_sites <- df_sites %>%
    dplyr::select(StationID, FinalLongitude, FinalLatitude, RefSiteFlag, COMID
                  , IncaseCol, OutcaseCol, US_L3CODE) %>%
    dplyr::rename(Latitude = FinalLatitude, Longitude = FinalLongitude) %>%
    dplyr::mutate(Case = dplyr::case_when(StationID == TargetSiteID ~ "Target"
                                          , StationID %in% compSites ~ "Inside the case"
                                          , StationID %in% allSites ~ "Outside the case"
                                          , TRUE ~ "Outside the case")
                  , Case = factor(Case, levels = c("Outside the case"
                                                   , "Inside the case"
                                                   , "Target")))
  if (is.na(datum)) {
    message("Datum assumed to be WGS84.")
   sp_sites <- sf::st_as_sf(df_sites, crs = 4326, coords = c("Longitude", "Latitude"))
  } else if (datum == "NAD27") {
    sp_sites <- sf::st_as_sf(df_sites, crs = 4267
                             , coords = c("FinalLongitude", "FinalLatitude"))
    sp_sites <- sf::st_transform(sp_sites, crs = 4326) %>%
      dplyr::mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]])
                    , lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
  } else if (datum == "NAD83") {
    sp_sites <- sf::st_as_sf(df_sites, crs = 4268
                             , coords = c("FinalLongitude", "FinalLatitude"))
    sp_sites <- sf::st_transform(sp_sites, crs = 4326) %>%
      dplyr::mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]])
                    , lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
  } else {
    message("CRS is not identified")
    sp_sites <- sf::st_as_sf(df_sites, crs = 4326
                             , coords = c("FinalLongitude", "FinalLatitude"))
  }

  # Get region orientation
  if (region %in% c("Arizona", "Arkansas", "Colorado", "Connecticut", "Georgia"
                    , "Iowa", "Kansas", "Louisiana", "Maryland", "Massachusetts"
                    , "Nebraska", "New Mexico", "North Dakota", "Ohio", "Oklahoma"
                    , "Oregon", "Pennsylvania", "South Carolina", "South Dakota"
                    , "Utah", "Washington", "West Virginia", "Wisconsin", "Wyoming")) {
    map.width = 7
    map.height = 7
    map.units = "in"
  }

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Prepare map
  # Get state bounding box
  GIS_offset <- 0.01
  ggmap_bbox <- setNames(sf::st_bbox(sp_flowline)
                         , c("left","bottom","right","top"))

  diffLat <- as.numeric(ggmap_bbox[4] - ggmap_bbox[2])
  diffLong <- as.numeric(ggmap_bbox[3] - ggmap_bbox[1])

  # ggmap_bbox[1] <- ggmap_bbox[1] - GIS_offset*(diffLat)  # Left
  # ggmap_bbox[2] <- ggmap_bbox[2] - GIS_offset*(diffLong) # Bottom
  # ggmap_bbox[3] <- ggmap_bbox[3] + GIS_offset*(diffLat)  # Right
  ggmap_bbox[4] <- ggmap_bbox[4] + GIS_offset*(diffLong) # Top

  NHD.clust <- dplyr::right_join(sp_flowline
                                  , df_clusters[, c("COMID", "US_L3CODE", "ClusterID")])
  sp_refsites <- subset(sp_sites, RefSiteFlag == 1)
  sp_outside <- subset(sp_sites, Case == "Outside the case")
  sp_inside <- subset(sp_sites, Case == "Inside the case")
  sp_target <- subset(sp_sites, Case == "Target")

  state.map <- tmap::tm_shape(sp_outline, bbox = ggmap_bbox) +
    tmap::tm_polygons(fill = "grey80") +
    tmap::tm_shape(NHD.clust) +
    tmap::tm_lines("ClusterID", palette = mag.vec, legend.col.show = FALSE
                   , lwd =0.5) +
    tmap::tm_shape(sp_outside) +
    tmap::tm_symbols(col = "gray25", shape = 25, size = 0.1, border.col = NA) +
    tmap::tm_shape(sp_inside) +
    tmap::tm_symbols(col = "blue", shape = 21, size = 0.15, border.col = NA) +
    tmap::tm_shape(sp_target) +
    tmap::tm_symbols(col = "red", shape = 17, size = 0.2, border.col = NA) +
    tmap::tm_shape(sp_outline) +
    tmap::tm_borders(col = "black", lwd = 1) +
    tmap::tm_add_legend('symbol'
                        , col = c("gray25", "blue", "red"), border.col = NA
                        # , shape = c(25, 21, 17)
                        , labels = c("Outside the case", "Inside the case", "Target")
                        , title = "", is.portrait = FALSE, reverse = TRUE) +
    tmap::tm_layout(frame = FALSE, legend.show = TRUE, legend.outside = TRUE
                    , main.title = region, legend.text.size = 0.5
                    , legend.outside.position = c("bottom", "center"))
  tmap::tmap_save(state.map, fn_Map, width = map.width, height = map.height
                  , units = "in", dpi = 600)
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Graphics parameters for ggplot2
  # bestdpi <- 600
  # maptype2 <- paste0(region, " Bioassessment Sites")
  #
  # col_outline <- "black"
  # col_flowline <- "light blue"
  # col_sites_out <- "dark gray"
  # col_sites_in  <- "cyan3"
  # col_sites_ref <- "blue"
  # col_sites_targ <- "red"
  #
  # pch_sites_out  <- 19
  # pch_sites_in   <- 19
  # pch_sites_ref  <- 21
  # pch_sites_targ <- 17
  #
  # cex_sites_out  <- 0.5
  # cex_sites_ref  <- 0.9
  # cex_sites_in   <- 1
  # cex_sites_targ <- 1.4
  #
  # lwd_outline  <- 1.0
  # lwd_flowline <- 0.25
  #
  # size_legtitle <- 3
  # size_legelement <- 2.5
  #
  # site_map <- ggplot2::ggplot(data = sp_outline, fill = "lightgray"
  #                             , color = col_outline, lwd = lwd_outline) +
  #   ggplot2::geom_sf(data = NHD.clust, inherit.aes = FALSE
  #                    , color = col_flowline, lwd = lwd_flowline) +
  #   ggplot2::geom_sf(data = sp_sites, inherit.aes = FALSE
  #                    , color = col_sites_all, pch = pch_sites_all
  #                    , size = cex_sites_all) +
  #   ggplot2::geom_sf(data = sp_outside, inherit.aes = FALSE
  #                    , color = col_sites_out, pch = pch_sites_out
  #                    , size = cex_sites_out) +
  #   ggplot2::geom_sf(data = sp_inside, inherit.aes = FALSE
  #                    , color = col_sites_in, pch = pch_sites_in
  #                    , size = cex_sites_in) +
  #   # ggplot2::geom_sf(data = sp_refsites, inherit.aes = FALSE
  #   #                  , color = col_sites_ref, pch = pch_sites_ref
  #   #                  , size = cex_sites_ref) +
  #   ggplot2::geom_sf(data = sp_target, inherit.aes = FALSE
  #                    , color = col_sites_targ, pch = pch_sites_targ
  #                    , size = cex_sites_targ) +
  #   ggplot2::geom_sf(data = sp_outline, inherit.aes = FALSE, fill = NA
  #                    , color = col_outline, lwd = lwd_outline) +
  #   ggplot2::coord_sf(datum = 4326
  #                     , xlim = c(ggmap_bbox["left"], ggmap_bbox["right"])
  #                     , ylim = c(ggmap_bbox["bottom"], ggmap_bbox["top"])) +
  #   ggplot2::theme_bw() +
  #   ggplot2::labs(x = "Longitude", y = "Latitude", title = TargetSiteID
  #                 , subtitle = maptype2) +
  #   ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold"
  #                                                     , hjust = 0)
  #                  , plot.subtitle = ggplot2::element_text(size = 10
  #                                                          , hjust = 0)
  #                  , axis.text = ggplot2::element_text(size = 8)
  #                  , axis.title = ggplot2::element_text(size = 10)
  #                  , legend.title = ggplot2::element_text(size = 8))
  #
  # ggplot2::ggsave(fn_Map, site_map, width = 7, height = 7, units = "in"
  #                 , dpi = bestdpi)

  #
  # Leaflet Map in Notebook
  report_format <- "html"
  strFile_out_ext <- paste0(".", report_format)
  strFile_out <- paste0(TargetSiteID, "_MAP_leaflet", strFile_out_ext)

  rmarkdown::render(file.path(dir_map_rmd, "Map_Leaflet2.rmd")
                    , output_format = paste0(report_format, "_document")
                    , output_file = strFile_out
                    , output_dir = dir_path
                    , quiet = TRUE)

  # place after static map so can insert static map into report

}
