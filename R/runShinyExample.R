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
#'     + Selection of StationID on a map or by pull down.
#'     
#'     + https://leppott.shinyapps.io/CAST_Map_StationID/
#'     
#' * CAST_SMC
#'     
#'     + https://leppott.shinyapps.io/CAST_SMC/
#'     
#' * CAST_AZ
#' 
#'     + Updated Shiny App with AZ data.
#' 
#' @param appname Name of the Shiny app to run.  Valid values are 
#' "CAST_Map_COMID", "CAST_Map_StationID", "CAST_AZ", and "CAST_SMC".  Default = "CAST_SMC".

#'
#' @examples
#' \dontrun{
#' # SMC
#' runShinyExample("CAST_SMC")
#' 
#' # Map
#' runShinyExample("CAST_Map_StationID")
#' }
#
#' @export
runShinyExample <- function(appname="CAST_SMC"){##FUNCTION.START
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
