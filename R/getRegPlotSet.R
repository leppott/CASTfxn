#' @title Regression Plot Settings
#' 
#' @description Get regression plot settings
#' 
#' @details Get regression plot settings for Inset, Spacer, and LegOpp based on LegLoc.
#' 
#' @param varLegLoc varLegLoc
#' 
#' @return A 3 part character string.
#' 
#' @examples
#' varLegLoc <- "topright"
#' RegPlotSet <- getRegPlotSet(varLegLoc)
#' varInset  <- RegPlotSet[1]
#' varSpacer <- RegPlotSet[2]
#' varLegOpp <- RegPlotSet[3]
#'  
#' @export
getRegPlotSet <- function(varLegLoc){
  #
  if (varLegLoc == "topleft") {
    varInset = 0.01       #top inset = 0.05
    varSpacer = "\n\n\n"
    varLegOpp = "bottomright"
  }
  if (varLegLoc == "topright") {
    varInset = 0.01
    varSpacer = "\n\n\n\n"
    varLegOpp = "bottomleft"
  }
  if (varLegLoc == "bottomleft") {
    varInset = 0.01
    varSpacer = ""
    varLegOpp = "topright"
  }
  if (varLegLoc == "bottomright") {
    varInset = 0.01
    varSpacer = ""
    varLegOpp = "topleft"
  }
  #
  Result <- c(varInset, varSpacer, varLegOpp)
  return(Result)
}
