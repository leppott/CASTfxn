#' Retrieve watershed stressor data
#'
#' @return
#' @export
#'
#' @examples
getWSStressData <- function(boo_ws = NULL,
                            boo_helper = NULL,
                            out_dir = NULL,
                            dn_checked_sk = NULL,
                            region = NULL,
                            #meta = NULL,
                            config = NULL){

  # if(is.null(meta) == FALSE){
  #   boo_ws <- meta$exploreWSStressor
  #   boo_helper <- meta$helperImport
  # }

  if(is.null(config) == FALSE){
    out_dir <- config$out_dir
    dn_checked_sk <- config$dn_checked_sk
    region <- config$region
  }

  if(boo_ws == FALSE){
    data_stressorWS <- NULL
    data_stressorinfoWS <- NULL

  } else if (boo_ws == TRUE & boo_helper == FALSE) {
    data_stressorWS     <- readRDS(file.path(out_dir, dn_checked_sk, "data_stressorWS.rds"))
    data_stressorinfoWS <- readRDS(file.path(out_dir, dn_checked_sk,
                                             "data_stressorinfoWS.rds"))
    if(exists("data_stressorWS") & exists("data_stressorinfoWS")){
      message("Watershed stressor data provided by user loaded.")
    } else{
      message("Watershed stressor data provided by user not found.")
    }

  } else if(boo_ws == TRUE & boo_helper == TRUE){
    conusStates <- setdiff(state.name, c("Alaska", "Hawaii"))
    regionAvailable <- region %in% conusStates

    if(regionAvailable == TRUE){
      message("Downloading watershed stressor data from helper package.")

      data_stressorWS <- CASToolHelperPckg::getWSStressorData(region)
      data_stressorinfoWS <- CASToolHelperPckg::getWSStressorInfo()

      if("comid" %in% names(data_stressorWS)){
        data_stressorWS <- data_stressorWS |> dplyr::rename("COMID" = "comid")
      }

    } else{
      message("Watershed stressor data not available for the specified region. ")
    }
  }

  return(list(data_stressorWS = data_stressorWS,
              data_stressorinfoWS = data_stressorinfoWS))
}
