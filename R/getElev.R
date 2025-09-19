#  Copyright 2020 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents 
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Elevation
#' 
#' @description Get location elevation.
#' 
#' @details Uses the elevatr package to acccess USGS Elevation Point Query Service (EPQS).
#' 
#' The EPQS is based on the National Map.  The vertical accuracy is 3.04 meters.
#' 
#' https://www.usgs.gov/faqs/what-vertical-accuracy-seamless-3dep-dems
#' 
#' getElev assumes WGS84 but any projections can be entered.  
#' 
#' Requires package elevatr and sp.
#' 
#' Requires a data frame with latitude and longitude.  
#' Other arguments are desired units and a projection.
#' 
#' The output is the input data frame with the elevation and units columns appended to it.
#' 
#' @param df_loc Data frame with location information
#' @param Lat df_loc column name; Latitude (decimal degrees).  Default = Latitude
#' @param Long df_loc column name; Longitude (decimal degrees).  Default = Longitude
#' @param elev_units Requested elevation units; meters or feet.  Default = meters.
#' @param proj_loc Projection.  Default is WGS84.
#' 
#' @return A data frame with elevation and units columns appended to it.
#' 
#' @examples
#' #Function Arguments
#' df_loc <- head(data_Sites)
#' Lat <- "FinalLatitude"
#' Long <- "FinalLongitude"
#' elev_units <- "feet"
#' proj_loc <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#'
#' # Run Function
#' df_elev <- getElev(df_loc, Lat, Long, elev_units, proj_loc)
#' 
#' /dontrun{
#' # Save Results
#' utils::write.table(df_elev, "Elev.tsv", sep="\t", row.names=FALSE, col.names = TRUE)
#' }
#
#' @export
getElev <- function(df_loc, Lat, Long, elev_units="meters"
                    , proj_loc="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"){##FUNCTION.START
  # 
  # Rename columns
  df_get <- df_loc[, c(Long, Lat)]
  names(df_get) <- c("x","y")
  
  # Create sp object
  Loc_sp <- sp::SpatialPoints(sp::coordinates(df_get), 
                                proj4string = sp::CRS(proj_loc)) 
  
  # Get Elevation
  ## epqs = USGS Elevation Point Query Service
  df_elev <- elevatr::get_elev_point(locations=Loc_sp, src="epqs", units=elev_units, prj=Loc_sp)

  # Create Output
  df_results <- cbind(df_loc, as.data.frame(df_elev@data))
  
  # Return Output
  return(df_results)
  #
}##FUNCTION.END





