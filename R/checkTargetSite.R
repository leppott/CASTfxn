#' Title
#'
#' @return
#' @export
#'
#' @examples
checkTargetSite <- function(TargetSiteID,
                            SiteData){

  temp_status <- data.frame()

  if (is.na(TargetSiteID)) {
    temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID),
                              status = "Failed",
                              reason = "TargetSiteID is NA")
    msg <- "TargetSiteID is NA."
    message(msg)

  } else if(SiteData |> dplyr::filter(StationID == TargetSiteID) |> nrow() == 0){
    temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID),
                              status = "Failed",
                              reason = "TargetSiteID not found in Sites file")

    msg <- "TargetSiteID not found in the Sites file."
    message(msg)

  } else if (is.na(SiteData$IncaseCol[SiteData$StationID == TargetSiteID])) {
    temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID),
                              status = "Failed",
                              reason = "TargetSiteID not assigned an inside-the-case identifier")

    msg <- "No inside-the-case identifier available."
    message(msg)
  }

  return(temp_status)
}
