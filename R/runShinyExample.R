#' @title run Shiny Example
#'
#' @description Launches Shiny app.
#'
#' @details The Shiny app based on the R package CASTfxn is included in the R package.
#' This function launches in the users default web browser the app referenced.  
#' There are multiple apps stored in the package:
#' 
#' * CAST_Map
#' 
#'     + Selection of COMID on a map or by pull down.
#'     
#'     + https://leppott.shinyapps.io/CAST_Map/
#'     
#' * CAST_v1
#' 
#'     + Original design for Shiny App.  Kept for reference only.
#'     
#'     + https://leppott.shinyapps.io/CAST/
#'     
#' * CAST_v2
#' 
#'     + Updated Shiny App.
#' 
#' @param appname Name of the Shiny app to run.  Valid values are 
#' "CAST_Map", "CAST_v1", and "CAST_v2".  Default = "CAST_v2".

#'
#' @examples
#' \dontrun{
#' # Run Function
#' runShinyExample()
#' }
#
#' @export
runShinyExample <- function(appname="CAST_v2"){##FUNCTION.START
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
