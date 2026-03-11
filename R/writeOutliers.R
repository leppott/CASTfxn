#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.2
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
#' @param df_Sites x
#' @param siteDetectsAll All stressors ever detected in any target site samples.
#' @param compSites Vector containing comparator site identifiers (inside the case).
#' @param useBC x
#' @param allSites Vector containing all "outside the case" identifiers
#' @param dir_results directory for results; Default = ./Results
#'
#' @keywords internal
#' @examples
#' # None at this time
#' @export
writeOutliers <- function(TargetSiteID,
                          df_outliers,
                          df_stressInfo,
                          df_Sites,
                          siteDetectsAll,
                          compSites,
                          useBC = FALSE,
                          allSites,
                          dir_results = file.path(getwd(), "Results")) {

  # Global Bindings
  data_outliers <- data_stressInfo <- list.CompSites <-
    StationID <- TransfResult <- Outlier <- StdParamName <- IncaseCol <-  NULL

  # Debug
  boo.debug = FALSE
  #
  if (boo.debug) {
    df_outliers = data_outliers
    df_stressInfo = data_stressInfo
    TargetSiteID = TargetSiteID
    siteDetects = siteDetectsAll
    compSites = list.CompSites$comp.sites
    allSites = list.CompSites$all.sites
    dir_results = dir_results
  }

  # Initialize gaps df
  df_gap <- data.frame(fxnname = character(), condition = character(), result = character(), comment = character())


  # LCN patch fix to remove dependence on hardcoded bmi_dataBioCoOccur
  if(useBC == TRUE){
    compSites <- compSites
    allSites <- allSites
  } else{
    TargetSiteCluster <- df_Sites %>%
      dplyr::filter(StationID == TargetSiteID) %>%
      dplyr::pull(IncaseCol)

    compSites <- df_Sites %>%
      dplyr::filter(IncaseCol == TargetSiteCluster) %>%
      dplyr::pull(StationID)

    allSites <- df_Sites %>%
      dplyr::filter(is.na(IncaseCol) | IncaseCol != TargetSiteCluster) %>%
      dplyr::pull(StationID)
  }

  # define pipe
  `%>%` <- dplyr::`%>%`

  # add case identifier to outliers dataframe and add stressor parameter label
  df_outliers <- df_outliers |>
    dplyr::mutate(StationIDType = dplyr::case_when(
      StationID == TargetSiteID ~ "Target site",
      StationID %in% compSites ~ "Inside-the-case",
      TRUE ~ "Outside-the-case")) |>
    dplyr::left_join(df_stressInfo |> dplyr::select("StdParamName", "Label"), by = "StdParamName") |>
    dplyr::select(StationID, StressSampleID, StressSampleDate, StationIDType, StdParamName, Label, TransfResult,
                  IQRmethod, SDmethod)

  file.outliers <- paste0(TargetSiteID, "_outliers.csv")
  fn.outliers <- file.path(dir_results, TargetSiteID, file.outliers)

  write.csv(df_outliers, fn.outliers, row.names=FALSE)

  n_target_outliers <- df_outliers |>
    dplyr::filter(StationIDType == "Target site") |>
    nrow()

  n_inside_outliers <- df_outliers |>
    dplyr::filter(StationIDType == "Inside-the-case") |>
    nrow()

  n_outside_outliers <- df_outliers |>
    dplyr::filter(StationIDType == "Outside-the-case") |>
    nrow()

  gap.statement.target <- data.frame(
    fxnname = "writeOutliers",
    condition = "Target site outliers",
    result = as.character(n_target_outliers),
    comment = paste0("See ", file.outliers, " in the ", TargetSiteID, " results folder for detailed description of outliers.")
  )

  gap.statement.inside <- data.frame(
    fxnname = "writeOutliers",
    condition = "Inside-the-case outliers",
    result = as.character(n_inside_outliers),
    comment = paste0("See ", file.outliers, " in the ", TargetSiteID, " results folder for detailed description of outliers.")
  )

  gap.statement.outside <- data.frame(
    fxnname = "writeOutliers",
    condition = "Outside-the-case outliers",
    result = as.character(n_outside_outliers),
    comment = paste0("See ", file.outliers, " in the ", TargetSiteID, " results folder for detailed description of outliers.")
  )

  df_gap <- df_gap |>
    dplyr::bind_rows(gap.statement.target) |>
    dplyr::bind_rows(gap.statement.inside) |>
    dplyr::bind_rows(gap.statement.outside)

  # Log removed or not removed outliers as data gaps
  # data_OutliersLabeled <- merge(df_outliers,
  #                               df_stressInfo[, c("StdParamName", "Label")],
  #                               by = "StdParamName", all.x =  TRUE)
  # siteOutliers <- data_OutliersLabeled %>%
  #   dplyr::filter(StationID == TargetSiteID) %>%
  #   dplyr::filter(!is.na(TransfResult)) %>%
  #   dplyr::filter(Outlier == "Outlier")
  # compOutliers <- data_OutliersLabeled %>%
  #   dplyr::filter(StationID %in% compSites) %>%
  #   dplyr::filter(StationID != TargetSiteID) %>%
  #   dplyr::filter(StdParamName %in% siteDetectsAll) %>%
  #   dplyr::filter(!is.na(TransfResult)) %>%
  #   dplyr::filter(Outlier == "Outlier")
  # allOutliers <- data_OutliersLabeled %>%
  #   dplyr::filter(StationID %in% allSites) %>%
  #   dplyr::filter(StationID != TargetSiteID) %>%
  #   dplyr::filter(StdParamName %in% siteDetectsAll) %>%
  #   dplyr::filter(!is.na(TransfResult)) %>%
  #   dplyr::filter(Outlier == "Outlier")

  # if (nrow(siteOutliers) > 0) {
  #   for (r in 1:nrow(siteOutliers)) {
  #     stressor <- siteOutliers$StdParamName[r]
  #     strLabel <- siteOutliers$Label[r]
  #     result <- siteOutliers$TransfResult[r]
  #     status <- siteOutliers$Outlier[r]
  #     statusMsg <- ifelse(status == "Outlier",
  #                         paste0("n ", tolower(status)),
  #                         status)
  #     siteID <- as.character(siteOutliers$StationID[r])
  #     gapcomment <- paste0(siteID, " value identified as a", statusMsg,
  #                          ". Transformation applied prior to",
  #                          " identification as necessary.")
  #     gaps <- cbind.data.frame("Site outliers", strLabel, result, gapcomment)
  #     colnames(gaps) <- c("fxnname", "condition", "result", "comment")
  #     #fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
  #     fn.gaps <- paste0(TargetSiteID, "_datagaps.csv")
  #     fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
  #     # utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
  #     #             row.names = FALSE, sep = "\t")
  #     utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
  #                        row.names = FALSE, sep = ",")
  #   }
  # }## IF ~ siteOutliers ~ END
  # message(paste0("Identified ", nrow(siteOutliers), " site outliers"))
  #
  # # message(paste0("comp outliers, n = ", nrow(compOutliers)))
  # if (nrow(compOutliers) > 0) {
  #   for (r in 1:nrow(compOutliers)) {
  #     stressor <- compOutliers$StdParamName[r]
  #     strLabel <- compOutliers$Label[r]
  #     result <- compOutliers$TransfResult[r]
  #     status <- compOutliers$Outlier[r]
  #     statusMsg <- ifelse(status == "Outlier",
  #                         paste0("n ", tolower(status)),
  #                         status)
  #     siteID <- as.character(compOutliers$StationID[r])
  #     if (siteID != TargetSiteID) {
  #       gapcomment <- paste0(siteID, " value identified as a", statusMsg,
  #                            ". Transformation applied prior to",
  #                            " identification as necessary.")
  #       gaps <- cbind.data.frame("Comparator (inside the case) outliers",
  #                                strLabel, result, gapcomment)
  #       colnames(gaps) <- c("fxnname", "condition", "result", "comment")
  #       #fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
  #       fn.gaps <- paste0(TargetSiteID, "_datagaps.csv")
  #       fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
  #       # utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
  #       #                    row.names = FALSE, sep = "\t")
  #       utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
  #                          row.names = FALSE, sep = ",")
  #     }
  #   }
  # }## IF ~ compOutliers ~ END
  # message(paste0("Identified ", nrow(compOutliers), " comparator site outliers"))
  #
  # if (nrow(allOutliers) > 0) {
  #   for (r in 1:nrow(allOutliers)) {
  #     stressor <- allOutliers$StdParamName[r]
  #     strLabel <- allOutliers$Label[r]
  #     result <- allOutliers$TransfResult[r]
  #     status <- allOutliers$Outlier[r]
  #     statusMsg <- ifelse(status == "Outlier",
  #                         paste0("n ", tolower(status)),
  #                         status)
  #     siteID <- as.character(allOutliers$StationID)[r]
  #     if (!(siteID %in% compSites)) {
  #       gapcomment <- paste0(siteID, " value identified as a", statusMsg,
  #                            ". Transformation applied prior to",
  #                            " identification as necessary.")
  #       gaps <- cbind.data.frame("Outside-the-case data outliers",
  #                                strLabel, result, gapcomment)
  #       colnames(gaps) <- c("fxnname", "condition", "result", "comment")
  #       # fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
  #       fn.gaps <- paste0(TargetSiteID, "_datagaps.csv")
  #       fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
  #       # utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
  #       #                    row.names = FALSE, sep = "\t")
  #       utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
  #                          row.names = FALSE, sep = ",")
  #     }
  #   }
  # }## IF ~ allOutliers ~ END
  # message(paste0("Identified ", nrow(allOutliers), " outside the case site outliers"))
  return(list(df_gap = df_gap))
}
