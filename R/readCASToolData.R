#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.2
#
#' @title Read CASTool input data files
#'
#' @description Reads varying CASTool input data using the correct function
#' for the file type based on it's extension.
#'
#' @details Input data may be either Microsoft Excel (extensions xls or xlsx),
#' tab-delimited text (tab or txt), or comma-separated values (csv).
#' No other options are allowed currently.
#'
#' Uses the librares readxl and tools.
#'
#' @param fn Filename with path specified in the CASTool_Metadata.xlsx file.
#' @param NAs vector of values to be interpreted as NA.
#'
#' @return A dataframe containing the parsed file.
#'
#' @keywords internal
#' @examples
#' # None at this time
#' @export
readCASToolData <- function(fn, NAs) {##FUNCTION.START

  if (tolower(tools::file_ext(fn)) == "csv" ) {
    df <- utils::read.csv(fn,
                          header = TRUE,
                          na.strings = NAs,
                          strip.white = TRUE,
                          stringsAsFactors = FALSE)
    df <- readr::type_convert(df, col_types = readr::cols(
      TargetSiteID = readr::col_character(),
      StationID = readr::col_character(),
      COMID = readr::col_number(),
      Latitude = readr::col_number(),
      Longitude = readr::col_number(),
      RefSiteFlag = readr::col_number(),
      StreamCatVar = readr::col_character(),
      Label = readr::col_character(),
      WatershedValue = readr::col_number(),
      Year = readr::col_number(),
      StdParamName = readr::col_character(),
      SourceGroup = readr::col_character(),
      LogTransf = readr::col_number(),
      UseInStressorID = readr::col_number(),
      DirIncStress = readr::col_character(),
      StressSampleID = readr::col_character(),
      ResultValue = readr::col_number(),
      RespSampleID = readr::col_character(),
      TaxonID = readr::col_character(),
      NumInd = readr::col_number(),
      MetricName = readr::col_character(),
      MetricLabel = readr::col_character(),
      IndexYN = readr::col_character(),
      UseYN = readr::col_character(),
      TrendWIncStress = readr::col_character(),
      CutoffValue = readr::col_number(),
      InclusiveIndicator = readr::col_character(),
      SSIndex = readr::col_character(),
      SSTVname.bmi = readr::col_character(),
      SSTVname.alg = readr::col_character(),
      SSTVname.fish = readr::col_character(),
      SensMax.bmi = readr::col_character(),
      SensMin.bmi = readr::col_character(),
      SensMax.alg = readr::col_character(),
      SensMin.alg = readr::col_character(),
      SensMax.fish = readr::col_character(),
      SensMin.fish = readr::col_character()
    ))
    return(df)
  } else if (tolower(tools::file_ext(fn)) %in% c("txt", "tab")) {
    df <- utils::read.delim(fn,
                            header = TRUE,
                            na.strings = NAs,
                            strip.white = TRUE,
                            stringsAsFactors = FALSE)
    df <- readr::type_convert(df, col_types = readr::cols(
      TargetSiteID = readr::col_character(),
      StationID = readr::col_character(),
      COMID = readr::col_number(),
      Latitude = readr::col_number(),
      Longitude = readr::col_number(),
      RefSiteFlag = readr::col_number(),
      StreamCatVar = readr::col_character(),
      Label = readr::col_character(),
      WatershedValue = readr::col_number(),
      Year = readr::col_number(),
      StdParamName = readr::col_character(),
      SourceGroup = readr::col_character(),
      LogTransf = readr::col_number(),
      UseInStressorID = readr::col_number(),
      DirIncStress = readr::col_character(),
      StressSampleID = readr::col_character(),
      ResultValue = readr::col_number(),
      RespSampleID = readr::col_character(),
      TaxonID = readr::col_character(),
      NumInd = readr::col_number(),
      MetricName = readr::col_character(),
      MetricLabel = readr::col_character(),
      IndexYN = readr::col_character(),
      UseYN = readr::col_character(),
      TrendWIncStress = readr::col_character(),
      CutoffValue = readr::col_number(),
      InclusiveIndicator = readr::col_character(),
      SSIndex = readr::col_character(),
      SSTVname.bmi = readr::col_character(),
      SSTVname.alg = readr::col_character(),
      SSTVname.fish = readr::col_character(),
      SensMax.bmi = readr::col_character(),
      SensMin.bmi = readr::col_character(),
      SensMax.alg = readr::col_character(),
      SensMin.alg = readr::col_character(),
      SensMax.fish = readr::col_character(),
      SensMin.fish = readr::col_character()
    ))
    return(df)
  } else if (tolower(tools::file_ext(fn) %in% c("xls", "xlsx"))) {
    df <- readxl::read_excel(fn,
                             col_names = TRUE,
                             skip = 0,
                             trim_ws = TRUE,
                             na = NAs)
    df <- readr::type_convert(df, col_types = readr::cols(
      TargetSiteID = readr::col_character(),
      StationID = readr::col_character(),
      COMID = readr::col_number(),
      Latitude = readr::col_number(),
      Longitude = readr::col_number(),
      RefSiteFlag = readr::col_number(),
      StreamCatVar = readr::col_character(),
      Label = readr::col_character(),
      WatershedValue = readr::col_number(),
      Year = readr::col_number(),
      StdParamName = readr::col_character(),
      SourceGroup = readr::col_character(),
      LogTransf = readr::col_number(),
      UseInStressorID = readr::col_number(),
      DirIncStress = readr::col_character(),
      StressSampleID = readr::col_character(),
      ResultValue = readr::col_number(),
      RespSampleID = readr::col_character(),
      TaxonID = readr::col_character(),
      NumInd = readr::col_number(),
      MetricName = readr::col_character(),
      MetricLabel = readr::col_character(),
      IndexYN = readr::col_character(),
      UseYN = readr::col_character(),
      TrendWIncStress = readr::col_character(),
      CutoffValue = readr::col_number(),
      InclusiveIndicator = readr::col_character(),
      SSIndex = readr::col_character(),
      SSTVname.bmi = readr::col_character(),
      SSTVname.alg = readr::col_character(),
      SSTVname.fish = readr::col_character(),
      SensMax.bmi = readr::col_character(),
      SensMin.bmi = readr::col_character(),
      SensMax.alg = readr::col_character(),
      SensMin.alg = readr::col_character(),
      SensMax.fish = readr::col_character(),
      SensMin.fish = readr::col_character()
    ))
    return(df)
  } else if (tolower(tools::file_ext(fn)) == "rda"){
    #load(fn, envir = .GlobalEnv)
    load(fn, envir = parent.frame())
  } else if(tolower(tools::file_ext(fn)) %in% c("png", "jpg", "jpeg")){
    df <- magick::image_read(fn)
    return(df)
  } else {
    message("File format not recognized.")
  }
}

