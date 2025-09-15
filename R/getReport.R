#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Report Generation
#'
#' @description Generate report
#'
#' @details Generates a report based on a Target SiteID in a given directory.
#' The output from other `CASTfxn` functions are stored in a "Results" folder
#' with the provided Target SiteID as a subdirectory.
#' Report format can be "html" (recommended default) or "word" (docx).
#'
#' Only the "summary" report is active.
#'
#' dir_data and dir_results should be absolute and not relative paths.
#' This is because these directories are passed to an RMD file.
#' Within the RMD each code chunk assumes a relative path based on the RMD location
#' and not the working directory of the calling function.
#' The function `normalizePath` can be used to convert from relative to absolute path.
#'
#' @param TargetSiteID SiteID
#' @param probsHigh Default = 0.75
#' @param probsLow Default = 0.25
#' @param useBMI boolean to use Benthic Macroinvertebrates.
#' @param useAlg boolean to use Algae.
#' @param useBC Default = TRUE
#' @param removeOutliers Default = TRUE
#' @param lagdays Default = 10, 10
#' @param bmiIndex Default = bmiIndex
#' @param algIndex Default = algIndex
#' @param fishIndex Default = fishIndex
#' @param dir_data Absolute path to data.  Default = /Data
#' @param dir_results Absoluthe path with subfolders named by SiteID.  Default = Results folder in working directory.
#' @param report_type Requested report type (all or summary).  Default = summary
#' @param report_format Requested report output format (html or word).  Default = html
#' @param dir_rmd Directory with template RMD for report.  Default = package rmd folder.
#' @param siteQual2Plot Site quality to print.
#'
#' @return A report for the provided SiteID in the provided format (html or word)
#' in the results directory.
#'
#' @examples
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
#' report_type <- "summary"
#' report_format <- "html"
#'
#' \dontrun{
#' # Run Function
#' getReport(TargetSiteID, dir_results, report_type, report_format)
#' }
#
#' @export
getReport <- function(TargetSiteID,
                      biocomms,
                      primeIndex = bmiIndexGp,
                      removeOutliers,
                      samplim,
                      r2_cutoff,
                      p.val_cutoff,
                      useBC,
                      lagdays,
                      DOlim,
                      pHlimLow,
                      pHlimHigh,
                      bmiIndex,
                      algIndex,
                      fishIndex,
                      useBMI,
                      useAlg,
                      useFish,
                      dir_data = normalizePath(file.path(".", "Data")),
                      dir_results = normalizePath(file.path(".", "Results")),
                      report_type = "full",
                      report_format = "html",
                      dir_rmd = file.path(system.file(package = "CASTfxn"), "rmd")
                      ) { ##FUNCTION.START
  #
  boo_DEBUG <- FALSE
  DEBUG_person <- "Ann"
  if (boo_DEBUG) {
    TargetSiteID = TargetSiteID
    biocomms = biocommlist
    useBMI = useBMI
    useAlg = useAlg
    useFish = useFish
    useBC = useBC
    removeOutliers = removeOutliers
    samplim = samplim
    r2_cutoff = r2_cutoff
    p.val_cutoff = p.val_cutoff
    lagdays = lagdays
    DOlim = DOlim
    pHlimLow = pHlimLow
    pHlimHigh = pHlimHigh
    bmiIndex = bmiIndex
    algIndex = algIndex
    fishIndex = fishIndex
    report_type = "full"
    report_format = "html"
    if (DEBUG_person == "Ann") {
      dir_data = dir_data
      dir_results = dir_results
      dir_rmd = file.path("C:", "Users", "ann.lincoln", "Documents", "GitHub"
                          , "CASTfxn", "inst", "rmd")
    } else {
      dir_data = normalizePath(file.path(".", "Data"))
      dir_results = normalizePath(file.path(".", "Results"))
      dir_rmd = file.path(system.file(package = "CASTfxn"), "rmd")
    }
    # siteQual2Plot = NULL
  }## IF ~ boo_DEBUG ~ END

  # Date and Time for output
  myDate <- format(Sys.Date(),"%Y%m%d")
  myTime <- format(Sys.time(),"%H%M%S")
  #
  # QC, ensure inputs are in the proper case
  report_type   <- tolower(report_type)
  report_format <- tolower(report_format)
  #
  # Report parts
  strFile_RMD <- file.path(dir_rmd, paste0("Report_Results_", report_type, ".rmd"))
  # strFile_RMD <- NULL
  strFile_out_ext <- paste0(".", ifelse(report_format == "word", "docx", report_format)) #".docx" # ".html"
  strFile_out_RMD <- paste0(paste(TargetSiteID, "Results", myDate, myTime,
                                  "FromRMD", sep = "_"), strFile_out_ext)

  strFile_QMD <- file.path(dir_rmd, "CASToolFullReport", "CASToolFullReport.qmd")
  strFile_out_QMD <- paste0(paste(TargetSiteID, "FullReport", myDate, myTime,
                                  "FromQMD", sep = "_"), strFile_out_ext)
  #
  # Generate Report
  # Test if RMD file exists
  if (!is.null(strFile_RMD)) {##IF.file.exists.START

    rmarkdown::render(strFile_RMD,
                      output_format = paste0(report_format, "_document"),
                      output_file   = file.path(dir_results, TargetSiteID, strFile_out_RMD),
                      params = list(TargetSiteID   = TargetSiteID,
                                    regionName     = regionName,
                                    biocommlist    = biocommlist,
                                    useBMI         = useBMI,
                                    useAlg         = useAlg,
                                    useFish        = useFish,
                                    useBC          = useBC,
                                    removeOutliers = removeOutliers,
                                    samplim        = samplim,
                                    r2_cutoff      = r2_cutoff,
                                    p.val_cutoff   = p.val_cutoff,
                                    lagdays        = lagdays,
                                    DOlim          = DOlim,
                                    pHlimLow       = pHlimLow,
                                    pHlimHigh      = pHlimHigh,
                                    bmiIndex       = bmiIndex,
                                    algIndex       = algIndex,
                                    fishIndex      = fishIndex,
                                    clusterfile    = basename(fn.cluster),
                                    dir_results    = dir_results),
                      quiet = TRUE)
  } else {
    Msg.Line0 <- "~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    Msg.Line1 <- "Provided report template file directory does not include the necessary RMD file to generate the report.  So no report will be generated."
    #Msg.Line2 <- "The default report directory can be modified in config.R (ContData.env$myReport.Dir) and used as input to the function (fun.myConfig)."
    Msg.Line3 <- paste0("file = ", basename(strFile_RMD))
    Msg.Line4 <- paste0("directory = ", dir_rmd)
    Msg <- paste(Msg.Line0, Msg.Line1
                 #, Msg.Line2
                 , Msg.Line3, Msg.Line4, Msg.Line0, sep = "\n\n")
    message(Msg)
    # cat(Msg)
    # utils::flush.console()
  }##IF.file.exists.END
  #
  # User Feedback
  message("Task COMPLETE.  Report generated.")
  message(paste0("    User defined parameters: SiteID (", TargetSiteID, "), Report Type ("
               , report_type, "), Report Format (", report_format, ")."))
  #utils::flush.console()
  #
}##FUNCTION.END





