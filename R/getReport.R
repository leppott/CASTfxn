#' @title Report Generation
#' 
#' @description Generate report
#' 
#' @details Generates a report based on a Target SiteID in a given directory.  
#' The output from other `CASTfxn` functions are stored in a "Results" folder with the provided Target SiteID as a subdirectory.  
#' Report format can be "html" (recommended default) or "word" (docx). 
#' 
#' Only the "summary" report is active. 
#' 
#' @param TargetSiteID SiteID
#' @param dir_results Directory with subfolders named by SiteID.  Default = Results folder in working directory.
#' @param report_type Requested report type (all or summary).  Default = summary
#' @param report_format Requested report output format (html or word).  Default = HTML
#' @param dir_rmd Directory with template RMD for report.  Default = package rmd folder.
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
getReport <- function(TargetSiteID, dir_results=file.path(getwd(), "Results")
                      , report_type="summary", report_format="html"
                      , dir_rmd=file.path(system.file(package = "CASTfxn"), "rmd")){##FUNCTION.START
  #
  # Date and Time for output
  myDate <- format(Sys.Date(),"%Y%m%d")
  myTime <- format(Sys.time(),"%H%M%S")
  #
  # QC, ensure inputs are in the proper case
  report_type   <- tolower(report_type)
  report_format <- tolower(report_format)
  #
  # 20181205
  report_type_valid <- c("summary", "overall")
  if(!(tolower(report_type) %in% report_type_valid)){
    Msg <- "Only the 'summary' report_type is active."
    stop(Msg)
  }
  #
  # Report parts
  strFile_RMD <- file.path(dir_rmd, paste0("Report_Results_", report_type, ".rmd"))
  strFile_out_ext <- paste0(".", ifelse(report_format=="word", "docx", report_format)) #".docx" # ".html"
  strFile_out <- paste0(paste("Report_Results", report_type, TargetSiteID, myDate, myTime, sep="_"), strFile_out_ext)
  #
    # Generate Report
  # Test if RMD file exists
  if (file.exists(strFile_RMD)){##IF.file.exists.START
    #suppressWarnings(
    rmarkdown::render(strFile_RMD, output_format=paste0(report_format,"_document"), output_file=strFile_out
                      , output_dir=file.path(dir_results, TargetSiteID), quiet=TRUE)
    #)
  } else {
    Msg.Line0 <- "~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    Msg.Line1 <- "Provided report template file directory does not include the necessary RMD file to generate the report.  So no report will be generated."
    #Msg.Line2 <- "The default report directory can be modified in config.R (ContData.env$myReport.Dir) and used as input to the function (fun.myConfig)."
    Msg.Line3 <- paste0("file = ", paste0("Report_Results_", report_type, ".rmd"))
    Msg.Line4 <- paste0("directory = ", dir_rmd)
    Msg <- paste(Msg.Line0, Msg.Line1
                 #, Msg.Line2
                 , Msg.Line3, Msg.Line4, Msg.Line0, sep="\n\n")
    cat(Msg)
    utils::flush.console()
  }##IF.file.exists.END
  # 
  # User Feedback
  print("Task COMPLETE.  Report generated.")
  print(paste0("    User defined parameters: SiteID (", TargetSiteID, "), Report Type ("
               , report_type, "), Report Format (", report_format, ")."))
  utils::flush.console()
  #
}##FUNCTION.END





