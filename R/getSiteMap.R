#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#  You can contact the authors at
#  Ann.RoseberryLincoln@tetratech.com
#  Erik.Leppo@tetratech.com
#
#  requires dplyr, ggplot2, purrr, rmarkdown, sf, tmap, viridis
#
#  Add Shiny code for use in Shiny App
#  2020-09-10, Erik
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#' @title Site Map
#'
#' @description Plot site map of provided TargetSite.
#'
#' @details Plots a map of the region, indicating the target site in red,
#' inside-the-case (comparator) sites in teal, outside-the-case sites in grey,
#' and both inside/outside the case reference sites outlined in dark blue.
#'
#' @param sp_outline Spatial dataframe representing region (or subregion) boundary.
#' @param sp_flowline Spatial dataframe representing stream reaches *MUST* include
#'                    inside-the-case ID; may include outside-the-case ID.
#' @param region Region or regulatory organization to which the data apply
#'               (e.g., AZ, SMC, WA, OR)
#' @param datum x
#' @param df_sites dataframe containing site data, including both "inside the case"
#'                 and "outside the case" identifiers.
#' @param allSites a vector of outside-the-case site identifiers
#' @param compSites a vector of inside-the-case site identifiers
#' @param TargetSiteID site identifier for the site being evaluated (the Target Site)
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = "FALSE"
#' @param plotvars Standardized plot sizes, fills, transparencies, and shapes by type
#' @param refOutline Standardized reference site outline color
#' @param dir_results Directory containing all results. Default is file.path(getwd(),"Results")
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo".
#' @param dir_map_rmd Directory containing the leaflet map template.
#'
#' @return A png map to a subdirectory "SiteInfo" in the folder named by the SiteID
#' in the user supplied dir_results folder (default is "Results" folder in the
#' working directory).
#
# no examples
#
#' @export
getSiteMap <- function(sp_outline,
                       sp_flowline,
                       region,
                       datum,
                       df_sites,
                       allSites,
                       compSites,
                       TargetSiteID,
                       useBC = FALSE,
                       plotvars,
                       refOutline,
                       dir_results = file.path(getwd(), "Results"),
                       dir_sub = "SiteInfo",
                       dir_map_rmd) {

  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    sp_outline <- STATE.shp
    sp_flowline <- NHD.STATE
    region <- region
    datum <- datum
    df_sites <- data_Sites
    allSites <- list.CompSites$all.sites
    compSites <- list.CompSites$comp.sites
    TargetSiteID <- TargetSiteID
    useBC <- FALSE
    plotvars = data_plotvars
    refOutline = refOutline_col
    dir_results <- dir_results
    dir_sub <- "SiteInfo"
    dir_map_rmd <- "C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd/"
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}

  # Write results directory ----
  out.dir <- dirname(dir_results)
  out.folders <- c(out.dir, basename(dir_results), TargetSiteID, dir_sub)

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      dir_path <- file.path(out.folders[i])
    } else {
      dir_path <- file.path(dir_path, out.folders[i])
    }
    if (dir.exists(dir_path) == FALSE) {
      dir.create(dir_path)
    }
  }

  # Get filename for saving
  fn_Map <- file.path(dir_path, paste0(TargetSiteID, "_map.png"))

  # Create color vector (range 2 to 6) for reaches
  if (useBC == TRUE) {
    maxClusterID <- max(as.numeric(df_sites$OutcaseCol), na.rm = TRUE)
  } else {
    maxClusterID <- max(as.numeric(df_sites$IncaseCol), na.rm = TRUE)
  }

  # Select colors from viridis palette with good separation (for reaches)
  if (maxClusterID == 6) {
    mag.vec <- viridis::viridis(23)[c(3, 7, 11, 15, 19, 23)]
  } else if (maxClusterID == 5) {
    mag.vec <- viridis::viridis(19)[c(3, 7, 11, 15, 19)]
  } else if (maxClusterID == 4) {
    mag.vec <- viridis::viridis(15)[c(3, 7, 11, 15)]
  } else if (maxClusterID == 3) {
    mag.vec <- viridis::viridis(11)[c(3, 7, 11)]
  } else { # max clusters = 2
    mag.vec <- viridis::viridis(7)[c(3, 7)]
  }
  mag.vec <- paste0(mag.vec, collapse = ",")

  ## Plot colors, sizes, etc  ----
  targetFill   <- plotvars$Fill[plotvars$Type == "target"]
  targetShape  <- plotvars$Shape[plotvars$Type == "target"]
  targetSize   <- plotvars$Size[plotvars$Type == "target"]/2
  insideFill   <- plotvars$Fill[plotvars$Type == "insideND"]
  insideShape  <- plotvars$Shape[plotvars$Type == "insideND"]
  insideSize   <- plotvars$Size[plotvars$Type == "insideND"]/2
  outsideFill  <- plotvars$Fill[plotvars$Type == "outsideND"]
  outsideShape <- plotvars$Shape[plotvars$Type == "outsideND"]
  outsideSize  <- plotvars$Size[plotvars$Type == "outsideND"]/2
  # refSiteCol is reference site outline color

  # Ensure flowline shapefile contains "ClusterID"
  sp_outline <- sf::st_transform(sp_outline, crs = sf::st_crs(sp_flowline))

  # Get sites (if datum is specified in the metadata, transform to WGS84,
  # otherwise, assume wGS84)
  # Subset ref sites, outside case sites, inside case sites, and target site
  df_sites <- df_sites %>%
    dplyr::select(StationID, Longitude, Latitude, RefSiteFlag, COMID,
                  IncaseCol, OutcaseCol) %>%
    dplyr::mutate(Case = dplyr::case_when(StationID == TargetSiteID ~ "Target",
                                          StationID %in% compSites ~ "Inside the case",
                                          StationID %in% allSites ~ "Outside the case",
                                          TRUE ~ "Outside the case"),
                  Case = factor(Case, levels = c("Outside the case",
                                                 "Inside the case",
                                                 "Target")))
  if (is.na(datum)) {
    message("CRS is not identified. Assumed to be WGS84.")
    sp_sites <- sf::st_as_sf(df_sites, crs = 4326,
                             coords = c("Longitude", "Latitude"))
  } else if (datum == "WGS84") {
    sp_sites <- sf::st_as_sf(df_sites, crs = 4326,
                             coords = c("Longitude", "Latitude")) %>%
      dplyr::mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                    lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
  } else if (datum == "NAD27") {
    sp_sites <- sf::st_as_sf(df_sites, crs = 5069,
                             coords = c("Longitude", "Latitude")) %>%
      dplyr::mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                    lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
  } else if (datum == "NAD83") {
    sp_sites <- sf::st_as_sf(df_sites, crs = 5070,
                             coords = c("Longitude", "Latitude")) %>%
      dplyr::mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                    lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
  } else if (grepl("\\d*", datum)) {
    if(class(datum)=="character"){
      datum <- as.numeric(datum)
    }
    
    sp_sites <- sf::st_as_sf(df_sites, crs = datum,
                             coords = c("Longitude", "Latitude")) %>%
      dplyr::mutate(lon = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
                    lat = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]]))
  } else {
    message("Datum does not match any of the allowed values.")
    stop()
  }
  sp_sites <- sf::st_transform(sp_sites, crs = sf::st_crs(sp_flowline))

  # Get region orientation
    map.width = 7
    map.height = 7
    map.units = "in"

  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Prepare map
  # Get state bounding box
  GIS_offset <- 0.01
  ggmap_bbox <- stats::setNames(sf::st_bbox(sp_flowline),
                         c("left","bottom","right","top"))

  diffLat <- as.numeric(ggmap_bbox[4] - ggmap_bbox[2])
  diffLong <- as.numeric(ggmap_bbox[3] - ggmap_bbox[1])

  # ggmap_bbox[1] <- ggmap_bbox[1] - GIS_offset*(diffLat)  # Left
  # ggmap_bbox[2] <- ggmap_bbox[2] - GIS_offset*(diffLong) # Bottom
  # ggmap_bbox[3] <- ggmap_bbox[3] + GIS_offset*(diffLat)  # Right
  ggmap_bbox[4] <- ggmap_bbox[4] + GIS_offset*(diffLong) # Top

  sp_refsites <- subset(sp_sites, !is.na(RefSiteFlag) & RefSiteFlag == 1)

  sp_outside <- subset(sp_sites, Case == "Outside the case")
  sp_inside <- subset(sp_sites, Case == "Inside the case")
  sp_targetsite <- subset(sp_sites, Case == "Target")

  state.map <- tmap::tm_shape(sp_outline, bbox = ggmap_bbox) +
    tmap::tm_polygons(fill = "white") + # LCN changed fill color from grey80
    tmap::tm_shape(sp_flowline) +
    tmap::tm_lines(lwd = 0.5, #palette = mag.vec,
                   col = "ClusterID",
                   col.scale = tmap::tm_scale_discrete(values = "viridis"),
                   col.legend = tmap::tm_legend(title = "ClusterID",
                                          orientation = "portrait")) +
    tmap::tm_shape(sp_outside) +
    tmap::tm_symbols(fill = outsideFill, col = "grey15", shape = outsideShape,
                     size = outsideSize) +
    tmap::tm_shape(sp_inside) +
    tmap::tm_symbols(fill = insideFill, col = "grey15", shape = insideShape,
                     size = insideSize)

  if (nrow(sp_refsites) > 0) {
    state.map <- state.map +
      tmap::tm_shape(sp_refsites) +
      tmap::tm_symbols(col = refOutline, fill = "grey40", fill_alpha = 0, size = 0.4) +
      # LCN added fill_alpha = 0 and changed size from 0.25
      tmap::tm_add_legend(type = 'symbols',
                          col = c("grey15", "grey15", refOutline, "grey15"),
                          fill = c(outsideFill, insideFill, "grey40", targetFill),
                          fill_alpha = c(1,1,0,1), # LCN added for consistency with plot
                          shape = c(outsideShape, insideShape, 21, targetShape),
                          labels = c("Outside the case", "Inside the case"
                                       , "Reference", "Target site"),
                          title = "", orientation = "portrait", reverse = TRUE)
  } else {
    state.map <- state.map +
      tmap::tm_add_legend(type = 'symbols', col = "grey15",
                          fill = c(outsideFill, insideFill, targetFill),
                          shape = c(outsideShape, insideShape, targetShape),
                          title = "Sites", orientation = "portrait",
                          labels = c("Outside case ", "Inside case", "Target site"),
                          reverse = TRUE)
  }
  state.map <- state.map  +
    tmap::tm_shape(sp_targetsite) +
    tmap::tm_symbols(fill = targetFill, col = "grey15", shape = targetShape,
                     size = targetSize) +
    tmap::tm_shape(sp_outline) +
    tmap::tm_borders(col = "black", lwd = 1) +
    tmap::tm_layout(frame = FALSE, legend.show = TRUE, legend.text.size = 0.5,
                    legend.title.size = 0.8, legend.stack = "horizontal",
                    legend.outside = TRUE, legend.outside.position = "bottom") +
    tmap::tm_title(region)

  tmap::tmap_save(state.map, fn_Map, , width = map.width, height = map.height,
                  units = "in", dpi = 600)
  # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # TODO
  # Leaflet Map in Notebook
  # report_format <- "html"
  # strFile_out_ext <- paste0(".", report_format)
  # strFile_out <- paste0(TargetSiteID, "_MAP_leaflet", strFile_out_ext)
  #
  # rmarkdown::render(file.path(dir_map_rmd, "Map_Leaflet2.rmd")
  #                   , output_format = paste0(report_format, "_document")
  #                   , output_file = strFile_out
  #                   , output_dir = dir_path
  #                   , quiet = TRUE)

  # place after static map so can insert static map into report

}
