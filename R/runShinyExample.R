#' @title run Shiny Example
#'
#' @description Launches Shiny app.
#'
#' @details The Shiny app based on the R package CASTfxn is included in the R package.
#' This function launches that app.
#'
#' The Shiny app is online at:
#' https://leppott.shinyapps.io/CAST/
#' 
#' A demo map (also Shiny) is at:
#' https://leppott.shinyapps.io/CAST_Map/
#'
#' @examples
#' \dontrun{
#' # Run Function
#' runShinyExample()
#' }
#
#' @export
runShinyExample <- function(){##FUNCTION.START
  #
  #appDir <- system.file("shiny-examples", "CAST", package = "CASTfxn")
  #if (appDir == "") {
  if (system.file("shiny-examples", "CAST", package = "CASTfxn") == "") {
    stop("Could not find example directory. Try re-installing `CASTfxn`.", call. = FALSE)
  }

  #shiny::runApp(appDir, display.mode = "normal")
  shiny::runApp(system.file("shiny-examples", "CAST", package = "CASTfxn"), display.mode = "normal")
  #
}##FUNCTION.END
