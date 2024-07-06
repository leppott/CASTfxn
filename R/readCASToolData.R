#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#' @title Read CASTool input data files
#'
#' @description Reads varying CASTool input data using the correct function
#' for the file type based on it's extension.
#'
#' @details Input data may be either Microsoft Excel (extensions xls or xlsx),
#' tab-delimited text (tab or txt), or comma-separated values (csv).
#' No other options are allowde.
#'
#' Uses the librares readxl and tools.
#'
#' @param fn Filename with path specified in the CASTool_Metadata.xlsx file.
#'
#' @return A dataframe containing the parsed file.
#'
#' @keywords internal
#'
#' @export
#'
readCASToolData <- function(fn, NAs) {##FUNCTION.START

  if (tolower(tools::file_ext(fn)) == "csv" ) {
    df <- read.csv(fn, header = TRUE, na.strings = NAs, strip.white = TRUE
                   , stringsAsFactors = FALSE)
  } else if (tolower(tools::file_ext(fn)) %in% c("txt", "tab")) {
    df <- read.delim(fn, header = TRUE, na.strings = NAs, strip.white = TRUE
                     , stringsAsFactors = FALSE)
  } else if (tolower(tools::file_ext(fn) %in% c("xls", "xlsx"))) {
    df <- readxl::read_excel(fn, col_names = TRUE, skip = 0, trim_ws = TRUE
                             , na = NAs)
  } else {
    message("File format not recognized.")
  }

  return(df)

}

