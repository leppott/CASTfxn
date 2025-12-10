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
#' @param dfStress x
#' @param dir_results Directory to save tables. Default = working directory and Results.
#' @param dir_WOE x
#' @param plotdpi x
#' @param plotH x
#' @param plotW x
#' @param plotunits x
#' @param boo_plot x
#'
#' @return Two tab-delimited tables containing weight of evidence information:
#'         one detailed, and one summary. The detailed table includes stressors
#'         and the lines of evidence for or against them. The summary table
#'         includes the number of lines that support, refute, are equivalent,
#'         or were not evaluated.
#'
#' @keywords internal
#' @examples
#' # None at this time
#' @export
getWoE <- function(TargetSiteID,
                   biocomm,
                   dfLoE,
                   dfStress,
                   dir_results,
                   dir_WoE = "_WoE",
                   plotdpi,
                   plotH,
                   plotW,
                   plotunits,
                   boo_plot = TRUE) {##FUNCTION.START

  # Global Bindings
  bioComm <- df_LoE <- Score <- StressorValue <- StationID <- StressSampleID <-
    StressSampleDate <- RespSampleID <- RespSampleDate <- Stressor <- LoE <-
    NumSupport <- NumRefute <- NumIndeterminate <- NumNotEvaluated <-
    Evidence <- Count <- max_count <- df_stressorMetadata <- NULL

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


  dfLoESummary_pivot <- dfLoE_summary %>%
    dplyr::select(Stressor, StressSampleID, StressSampleDate, NumSupport,
                  NumRefute, NumIndeterminate, NumNotEvaluated) %>%
    dplyr::arrange(Stressor, StressSampleDate) %>%
    dplyr::rename("Support" = "NumSupport", "Refute" = "NumRefute", "Indeterminate" = "NumIndeterminate") %>%
    dplyr::select(-NumNotEvaluated) %>%
    tidyr::pivot_longer(cols = c("Support", "Refute", "Indeterminate"), names_to = "Evidence", values_to = "Count") %>%
    dplyr::mutate(Evidence = forcats::fct_relevel(as.factor(Evidence), "Support", "Refute", "Indeterminate")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Stressor = stringr::str_wrap(Stressor, width = 40),
                  StressSampleID = dplyr::if_else(nchar(StressSampleID)>20,
                                           paste0(stringr::str_sub(StressSampleID, 1, ceiling(nchar(StressSampleID)/2)), "\n", stringr::str_sub(StressSampleID, ceiling(nchar(StressSampleID)/2) + 1, nchar(StressSampleID)), "\n"),
                  StressSampleID))


  stressor_order <- dfLoESummary_pivot %>%
    dplyr::filter(Evidence == "Support") %>%
    dplyr::group_by(Stressor) %>%
    dplyr::summarize(max_count = max(Count)) %>%
    dplyr::arrange(max_count) %>%
    dplyr::pull(Stressor)

  sample_order <- dfLoESummary_pivot %>%
    dplyr::distinct(StressSampleID, StressSampleDate) %>%
    dplyr::arrange(dplyr::desc(StressSampleDate)) %>%
    dplyr::pull(StressSampleID)


  dfLoESummary_pivot <- dfLoESummary_pivot %>%
    dplyr::mutate(
      Stressor = as.factor(Stressor),
      Stressor = forcats::fct_relevel(Stressor, stressor_order),
      StressSampleID = as.factor(StressSampleID),
      StressSampleID = forcats::fct_relevel(StressSampleID, sample_order)
      )

  p1 <- ggplot2::ggplot(dfLoESummary_pivot,
                        ggplot2::aes(x = Stressor, y = Count, fill = StressSampleID))+
    ggplot2::geom_bar(width = 0.5, stat = "identity", position = "dodge")+
    ggplot2::scale_fill_brewer(palette = "Paired")+
    ggplot2::coord_flip()+
    ggplot2::facet_wrap(~Evidence, ncol = 3)+
    ggplot2::theme_bw()+
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))+
    ggplot2::theme(axis.text = ggplot2::element_text(size = 10), legend.text = ggplot2::element_text(size = 10),
                   strip.text = ggplot2::element_text(size = 11), axis.title = ggplot2::element_text(size = 12))

  fn_png_p1 <- paste0(TargetSiteID, "_",
                      biocomm, "_LoESummaryFig.png")

  if ((boo_plot) == TRUE) {
    ggplot2::ggsave(filename = file.path(dir.path, fn_png_p1),
                    plot = p1, dpi = plotdpi, width = plotW, height = plotH,
                    units = plotunits)
  }

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

