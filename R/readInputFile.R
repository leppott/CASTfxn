#  Copyright 2023 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#  R version 4.3.1
#
#
#' @title Identifies Stressor Outliers
#'
#' @description Flags stressor outliers using two methods: 3 times IQR and
#'              6 times sd. If both suggest a given value as an outlier, the
#'              value is identified as an outlier.
#'
#' @details Generates faceted time sequence graphics (stressor/response, one
#' atop the other). All stressor/response data are graphed.
#' Improvements: Add scoring.
#'
#' Uses the library dplyr.
#'
#' @param filename filename to read
#' @param extension file type to read
#'
#' @return Dataframe containing data corresponding to filename
#'
#' @keywords internal
#'
#' @export
#'
readInputFile <- function(filename, extension = "tab") {

  fn <- filename
  ext <- extension

  if (ext == "tab" | ext == "txt") {                              # tab-delimited text
    df <- read.delim(fn, header = TRUE
                     , na.strings = c("NA", "N/A", "")
                     , sep = "\t", stringsAsFactors = FALSE)
  } else if (ext == "csv") {
    df <- read.csv(fn, na.strings = c("NA", "N/A", "")            # csv
                     , stringsAsFactors = FALSE)
  } else if (ext == "xlsx") {
    df <- readxl::read_excel(fn, na.strings = c("NA", "N/A", "")) # MS Excel file
  } else {
    msg <- "File extension unclear"
    message(msg)
    stop()
  }

  return(df)

}
