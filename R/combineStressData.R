#' Combine stressor data
#'
#' @return
#' @export
#'
#' @examples

combineStressData <- function(#meta = NULL,
                              boo_meas = NULL,
                              boo_model = NULL,
                              boo_outliers = NULL,
                              meas_stress,
                              mod_stress){

  # if(is.null(meta)==FALSE){
  #   boo_meas <- meta$boo_meas
  #   boo_model <- meta$boo_model
  #   boo_outliers <- meta$removeOutliers
  # }

  if (isTRUE(boo_meas) && isTRUE(boo_model)) {
    # Combine metadata
    data_stressInfo <- rbind(meas_stress[["data_chemInfo"]], mod_stress[["data_modelInfo"]])

    # Combine data
    data_stress     <- rbind(meas_stress[["data_chemRaw"]], mod_stress[["data_modelRaw"]])

    # Combine outliers
    if (isTRUE(boo_outliers)) {
      data_stressOutliers <- rbind(meas_stress[["data_measoutliers"]], mod_stress[["data_modeloutliers"]])
    }

  } else if (isTRUE(boo_meas)) {
    data_stressInfo <- meas_stress[["data_chemInfo"]]
    data_stress     <- meas_stress[["data_chemRaw"]]

    if (isTRUE(boo_outliers)) {
      data_stressOutliers <- meas_stress[["data_measoutliers"]]
    } else {
      data_stressOutliers <- NULL
    }

  } else {
    data_stressInfo <- mod_stress[["data_modelInfo"]]
    data_stress     <- mod_stress[["data_modelRaw"]]

    if (isTRUE(boo_outliers)) {
      data_stressOutliers <- mod_stress[["data_modeloutliers"]]
    } else {
      data_stressOutliers <- NULL
    }
  }

  return(list(data_stressInfo = data_stressInfo,
              data_stress = data_stress,
              data_stressOutliers = data_stressOutliers))
}
