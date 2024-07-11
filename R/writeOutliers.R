#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#' @title Write Outliers
#'
#' @description Writes identified outliers for target site, inside-the-case sites,
#' and outside-the-case sites to the datagaps file.
#'
#' @details Writes information about sample outliers to the target site data gaps
#' file, including site samples, inside-the-case (comparator) samples, and
#' outside-the-case samples.
#'
#' Uses the library dplyr.
#'
#' @param TargetSiteID Site ID
#' @param df_outliers Dataframe containing any stressor values identified outliers. Default = "data_outliers"
#' @param df_stressInfo Dataframe containing stressor metadata. Default = data_stressInfo
#' @param siteDetects All stressors ever detected in any target site samples.
#' @param compSites Vector containing comparator site identifiers (inside the case).
#' @param allSites Vector containing all "outside the case" identifiers
#' @param dir_results directory for results; Default = ./Results
#'
#' @keywords internal
#'
#' @export
writeOutliers <- function(TargetSiteID
                          , df_outliers
                          , df_stressInfo
                          , siteDetects
                          , compSites
                          , allSites
                          , dir_results = file.path(getwd(), "Results")) {

  boo.debug = FALSE

  if (boo.debug) {
    df_outliers = data_outliers
    df_stressInfo = data_stressInfo
    TargetSiteID = TargetSiteID
    siteDetects = siteDetectsAll
    compSites = comp_sites
    allSites = all_sites
    dir_results = dir_results
  }

  # Log removed or not removed outliers as data gaps
  data_OutliersLabeled <- merge(data_outliers
                                , data_stressInfo[, c("StdParamName", "Label")]
                                , by = "StdParamName", all.x =  TRUE)
  siteOutliers <- data_OutliersLabeled %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::filter(Outlier == "Outlier")
  compOutliers <- data_OutliersLabeled %>%
    dplyr::filter(StationID %in% compSites) %>%
    dplyr::filter(StationID != TargetSiteID) %>%
    dplyr::filter(StdParamName %in% siteDetectsAll) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::filter(Outlier == "Outlier")
  allOutliers <- data_OutliersLabeled %>%
    dplyr::filter(StationID %in% allSites) %>%
    dplyr::filter(StationID != TargetSiteID) %>%
    dplyr::filter(StdParamName %in% siteDetectsAll) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::filter(Outlier == "Outlier")

  if (nrow(siteOutliers) > 0) {
    for (r in 1:nrow(siteOutliers)) {
      stressor <- siteOutliers$StdParamName[r]
      strLabel <- siteOutliers$Label[r]
      result <- siteOutliers$ResultValue[r]
      status <- siteOutliers$Outlier[r]
      statusMsg <- ifelse(status == "Outlier"
                          , paste0("n ", tolower(status))
                          , status)
      siteID <- as.character(siteOutliers$StationID[r])
      gapcomment <- paste0(siteID, " value identified as a", statusMsg
                           , ". Transformation applied prior to"
                           , " identification as necessary.")
      gaps <- cbind.data.frame("Site outliers", strLabel, result
                               , gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
    }
  }## IF ~ siteOutliers ~ END
  message(paste0("Identified ", nrow(siteOutliers), " site outliers"))

  # message(paste0("comp outliers, n = ", nrow(compOutliers)))
  if (nrow(compOutliers) > 0) {
    for (r in 1:nrow(compOutliers)) {
      stressor <- compOutliers$StdParamName[r]
      strLabel <- compOutliers$Label[r]
      result <- compOutliers$ResultValue[r]
      status <- compOutliers$Outlier[r]
      statusMsg <- ifelse(status == "Outlier"
                          , paste0("n ", tolower(status))
                          , status)
      siteID <- as.character(compOutliers$StationID[r])
      if (siteID != TargetSiteID) {
        gapcomment <- paste0(siteID, " value identified as a", statusMsg
                             , ". Transformation applied prior to"
                             , " identification as necessary.")
        gaps <- cbind.data.frame("Comparator (inside the case) outliers", strLabel, result
                                 , gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
      }
    }
  }## IF ~ compOutliers ~ END
  message(paste0("Identified ", nrow(compOutliers)
                 , " comparator site outliers"))

  if (nrow(allOutliers) > 0) {
    for (r in 1:nrow(allOutliers)) {
      stressor <- allOutliers$StdParamName[r]
      strLabel <- allOutliers$Label[r]
      result <- allOutliers$ResultValue[r]
      status <- allOutliers$Outlier[r]
      statusMsg <- ifelse(status == "Outlier"
                          , paste0("n ", tolower(status))
                          , status)
      siteID <- as.character(allOutliers$StationID)[r]
      if (!(siteID %in% compSites)) {
        gapcomment <- paste0(siteID, " value identified as a", statusMsg
                             , ". Transformation applied prior to"
                             , " identification as necessary.")
        gaps <- cbind.data.frame("Outside-the-case data outliers", strLabel, result
                                 , gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
        fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
      }
    }
  }## IF ~ allOutliers ~ END
  message(paste0("Identified ", nrow(allOutliers)
                 , " outside the case site outliers"))

}
