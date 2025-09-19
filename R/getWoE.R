#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Weight-of-Evidence summary
#'
#' @description Summarize weight of evidence using scores from other functions.
#'
#' @details Stressor-based weight of evidence for the stressor as a cause of impairment
#'          for the specified biological community. Combines information from
#'          the co-occurrence, stressor-response using data from or outside the
#'          case, verified predictions, and stressor-response from laboratory data.
#'
#' Uses the packages dplyr and tidyr.
#'
#' @param TargetSiteID Site ID
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param dfLoE Dataframe containing the lines of evidence scores for each
#'              stressor and sample in long form.
#' @param dir_results Directory to save tables. Default = working directory and Results.
#'
#' @return Two tab-delimited tables containing weight of evidence information:
#'         one detailed, and one summary. The detailed table includes stressors
#'         and the lines of evidence for or against them. The summary table
#'         includes the number of lines that support, refute, are equivalent,
#'         or were not evaluated.
#'
#' @keywords internal
#'
#' @export
getWoE <- function(TargetSiteID,
                   biocomm,
                   dfLoE,
                   dfStress = df_stressorMetadata,
                   dir_results = file.path(getwd(), "Results"),
                   dir_WoE = "_WoE") {##FUNCTION.START

  # QC data
  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    TargetSiteID = TargetSiteID
    biocomm = bioComm
    dfLoE = df_LoE
    dfStress = df_stressorMetadata
    dir_results = dir_results
    dir_WoE = "_WoE"
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  biocomm <- toupper(biocomm)

  # Write results directory ----
  out.dir <- dirname(dir_results)
  out.folders <- c(out.dir, basename(dir_results), TargetSiteID, biocomm,
                   dir_WoE)

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      dir.path <- file.path(out.folders[i])
    } else {
      dir.path <- file.path(dir.path, out.folders[i])
    }
    if (dir.exists(dir.path) == FALSE) {
      dir.create(dir.path)
    }
  }

  dfLoE_summary <- dfLoE %>%
    dplyr::mutate(Score = ifelse(Score == "NE", NA, Score),
                  Score = suppressWarnings(as.numeric(Score)),
                  StressorValue = round(StressorValue, 3),
                  bioComm = toupper(bioComm)) %>%
    dplyr::group_by(StationID, StressSampleID, StressSampleDate,
                    RespSampleID, RespSampleDate, bioComm, Stressor,
                    StressorValue) %>%
    dplyr::summarize(NumSupport = sum(Score > 0, na.rm = TRUE),
                     NumRefute = sum(Score < 0, na.rm = TRUE),
                     NumIndeterminate = sum(Score == 0, na.rm = TRUE),
                     NumNotEvaluated = sum(is.na(Score)),
                     .groups = "drop_last") %>%
    dplyr::arrange(StationID, Stressor, StressSampleDate)

  utils::write.table(dfLoE_summary,
              file.path(dir.path, paste0(TargetSiteID, "_LoESummary.tab")),
              col.names = TRUE, row.names = FALSE, sep = "\t")

  dfLoE <- dfLoE %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, RespSampleID,
                  RespSampleDate, bioComm, Stressor, StressorValue, LoE, Score) %>%
    dplyr::mutate(StressorValue = round(StressorValue, 3),
                  bioComm = toupper(bioComm)) %>%
    tidyr::pivot_wider(id_cols = c(StationID, StressSampleID, StressSampleDate,
                                   RespSampleID, RespSampleDate, bioComm,
                                   Stressor, StressorValue), names_from = LoE,
                       values_from = Score) %>%
    # dplyr::mutate(CO = tidyr::replace_na(CO, "NE"),
    #               Suff = tidyr::replace_na(Suff, "NE"),
    #               `Gradient (inside)` = tidyr::replace_na(`Gradient (inside)`,
    #                                                       "NE"),
    #               VP_SSTV = tidyr::replace_na(VP_SSTV, "NE"),
    #               VP_SSIbox = tidyr::replace_na(VP_SSIbox, "NE"),
    #               VP_SSIlog = tidyr::replace_na(VP_SSIlog, "NE"),
    #               TS = tidyr::replace_na(TS, "NE"),
    #               `Gradient (outside)` = tidyr::replace_na(`Gradient (outside)`,
    #                                                        "NE"))
    dplyr::arrange(StationID, Stressor, StressSampleDate)

  utils::write.table(dfLoE, file.path(dir.path, paste0(TargetSiteID, "_LoEs.tab")),
              col.names = TRUE, row.names = FALSE, sep = "\t")

}

