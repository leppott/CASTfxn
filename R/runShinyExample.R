#' @title run Shiny Example
#'
#' @description Launches Shiny app.
#'
#' @details The Shiny app based on the R package CASTfxn is included in the R package.
#' This function launches in the users default web browser the app referenced.  
#' There are multiple apps stored in the package:
#' 
#' * CAST_Map_COMID
#' 
#'     + Selection of COMID on a map or by pull down.
#'     
#'     + https://leppott.shinyapps.io/CAST_Map/
#'     
#' * CAST_v1
#'     
#'     + No longer in the package, kept for reference only.
#' 
#'     + Original design for Shiny App.
#'     
#'     + https://leppott.shinyapps.io/CAST/
#'     
#' * CAST_AZ
#' 
#'     + Updated Shiny App with AZ data.
#' 
#' @param appname Name of the Shiny app to run.  Valid values are 
#' "CAST_Map_COMID", "CAST_Map_SiteID", "CAST_AZ", and "CAST_SMC".  Default = "CAST_AZ".

#'
#' @examples
#' \dontrun{
#' # AZ
#' runShinyExample("CAST_AZ")
#' 
#' # Map
#' runShinyExample("CAST_Map_COMID")
#' }
#
#' @export
runShinyExample <- function(appname="CAST_AZ"){##FUNCTION.START
  #
  #appDir <- system.file("shiny-examples", appname, package = "CASTfxn")
  #if (appDir == "") {
  if (system.file("shiny-examples", appname, package = "CASTfxn") == "") {
    stop("Could not find example directory. Try re-installing `CASTfxn`.", call. = FALSE)
  }

  #shiny::runApp(appDir, display.mode = "normal")
  shiny::runApp(system.file("shiny-examples", appname, package = "CASTfxn"), display.mode = "normal")
  #
}##FUNCTION.END
