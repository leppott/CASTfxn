#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Site Info
#'
#' @description Get site info of provided siteID.
#'
#' @details Summary info including lat/long, ref status, cluster membership,
#' samples from site
#'
#' Requires packages dplyr, ggplot2, tidyr
#'
#' Required objects:
#'
#' * data_Sites; StationID, COMID, FinalLatitude, FinalLongitude
#'
#' * data.SampSummary; StationID, CollDate, ChemSampleID, PhabSampID
#' , BMI.Metrics.SampID, Algae.Metrics.SampID
#'
#' * data.bmi.metrics; StationID, CollDate, CSCI, O_E, MMI_Score
#'
#' * data.algae.metrics; StationCode, SampleDate, H20, D18, S2
#'
#' * data.cluster; COMID, H6_noland, H6_land, ElevWs, WsAreaSqKm, PrecipWs, TmeanWs
#'
#' Will create output folder dir_results if it doesn't already exist.  The default is "Results".
#' A subdirectory is created for each SiteID.
#'
#' @param TargetSiteID site identifier for the site being evaluated (the Target Site)
#' @param TargetCOMID common identifier for the reach on which the site is located
#' @param df_Sites dataframe containing site data, including "inside the case"
#' and "outside the case" identifiers. If useBC == TRUE, "outside the case" will
#' be cluster; if useBC == FALSE, "inside the case" will be cluster.
#' @param df_SampSummary dataframe containing sample IDs for samples collected
#' at the target site, organized by sample date (rows) and type (columns)
#' @param biocommlist vector of all biological response communities to be evaluated
#' @param df_BMIMetrics dataframe containing BMI sample index and metric values
#' @param BMIIndexGp vector containing one or more BMI indices for display purpose only
#' @param df_ALGMetrics dataframe containing algae sample index and metric values.
#' Default is NULL.
#' @param ALGIndexGp vector containing one or more algal indices for display purpose only
#' @param df_FishMetrics dataframe containing fish sample index and metric values
#' @param FishIndexGp vector containing one or more fish indices for display purpose only
#' @param comp.sites vector containing inside-the-case (comparator) site IDs
#' @param all.sites vector containing all outside-the-case site IDs
#' @param OutcaseLabel Label for the "outside the case" identifier. Default = NULL.
#' @param IncaseLabel Label for the "inside the case" identifier. Default = NULL.
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = FALSE.
#' @param plot_vars Colors, fills, shapes, transparencies for each type (target,
#'                 not degraded, degraded, inside-the-case, outside-the-case).
#'                 Default = data_plotvars.
#' @param refSiteCol Default color outline for reference sites, used for standardization.
#' Default = refOutline_col.
#' @param plot_dpi Default dpi for plots, used for standardization. Default = plot_dpi.
#' @param plot_H Default height for plots, used for standardization. Default = plot_H.
#' @param plot_W Default width for plots, used for standardization. Default = plot_W.
#' @param plot_units Default units for plots, used for standardization. Default = plot_units.
#' @param dir_photo directory containing all site photos (for every site in the data set).
#' Default is file.path(getwd(), "Data", "Photos").
#' @param dir_results Directory containing all results. Default is file.path(getwd(),"Results").
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo".
#' @param boo_plot Boolean value to save plots. Default = TRUE.
#'
#' @return A list containing a dataframe of site information (site ID, geographic
#'         coordinates, waterbody name, county name, reach identifier, cluster
#'         identifier, and reference site flag); a dataframe of any samples obtained
#'         from the site location by sample date; a dataframe of bmi response
#'         indices and metric values; a dataframe of algae response indices and
#'         metric values; the reach identifier for the target site location; the
#'         cluster identifier for the target site; and a vector of reference reaches.
#'         Also outputs graphics depicting background variables by category,
#'         response indices, and tabular site background data. Creates a subfolder
#'         for site photos within the directory "~/Results/TargetSiteID/SiteInfo".
#'
#' @examples
#' \dontrun{
#' }
#' @export
getSiteInfo <- function(TargetSiteID,
                        TargetCOMID,
                        df_Sites,
                        df_SampSummary,
                        biocommlist,
                        df_BMIMetrics = NULL,
                        BMIIndexGp = NULL,
                        df_ALGMetrics = NULL,
                        ALGIndexGp = NULL,
                        df_FishMetrics = NULL,
                        FishIndexGp = NULL,
                        comp.sites,
                        all.sites,
                        IncaseLabel = NULL,
                        OutcaseLabel = NULL,
                        useBC = FALSE,
                        plot_vars = data_plotvars,
                        refSiteCol = refOutline_col,
                        plot_dpi = plot_dpi,
                        plot_H = plot_H,
                        plot_W = plot_W,
                        plot_units = plot_units,
                        dir_photo = file.path(getwd(), "Data", "Photos"),
                        dir_results = file.path(getwd(), "Results"),
                        dir_sub = "SiteInfo",
                        boo_plot = TRUE) {##FUNCTION.START

  # DEBUG
  boo_DEBUG <- FALSE
  #
  if (boo_DEBUG == TRUE) {
    TargetSiteID = TargetSiteID
    TargetCOMID = list.CompSites$TargetCOMID
    df_Sites = data_Sites
    df_SampSummary = data_sampSummary
    biocommlist = biocommlist
    df_BMIMetrics = data_bmiMetrics
    BMIIndexGp = bmiIndexGp
    df_ALGMetrics = data_algMetrics
    ALGIndexGp = algIndexGp
    df_FishMetrics = data_fishMetrics
    FishIndexGp = fishIndexGp
    comp.sites = list.CompSites$comp.sites
    all.sites = list.CompSites$all.sites
    OutcaseLabel = outcaseLabel
    IncaseLabel = incaseLabel
    useBC = FALSE
    plot_vars = data_plotvars
    plot_dpi = plot_dpi
    plot_H = plot_H
    plot_W = plot_W
    plot_units = plot_units
    refSiteCol = refOutline_col
    dir_photo = file.path(dir_data, "Photos")
    dir_results = dir_results
    dir_sub = "SiteInfo"
    boo_plot = TRUE
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}
  all_na <- function(x) {all(is.na(x))}
  biocommlist <- toupper(biocommlist)

  # Write results directory ----
  out.dir <- dirname(dir_results)
  out.folders <- c(out.dir, basename(dir_results), TargetSiteID, dir_sub)

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      dir_path <- file.path(out.folders[i])
    } else {
      dir_path <- file.path(dir_path, out.folders[i])
    }
    if (dir.exists(dir_path) == FALSE) {
      dir.create(dir_path)
    }
  }

  ## Plot colors, sizes, etc  ----
  # Switched around to account for showing reference ND & D with 2 boxes for
  # inside-the-case and outside-the-case (for fill, col, & alpha)
  bio_shp <- c(plot_vars$Shape[plot_vars$Type == "target"],
               plot_vars$Shape[plot_vars$Type == "insideND"],
               plot_vars$Shape[plot_vars$Type == "insideD"],
               plot_vars$Shape[plot_vars$Type == "outsideND"],
               plot_vars$Shape[plot_vars$Type == "outsideD"])
  bio_fill <- c(plot_vars$Fill[plot_vars$Type == "target"],
                plot_vars$Fill[plot_vars$Type == "insideND"],
                plot_vars$Fill[plot_vars$Type == "outsideD"],
                plot_vars$Fill[plot_vars$Type == "outsideND"],
                plot_vars$Fill[plot_vars$Type == "insideD"])
  bio_alpha <- c(plot_vars$Alpha[plot_vars$Type == "target"],
                 plot_vars$Alpha[plot_vars$Type == "insideND"],
                 1,
                 plot_vars$Alpha[plot_vars$Type == "outsideND"],
                 plot_vars$Alpha[plot_vars$Type == "outsideD"])
  bio_size <- c(plot_vars$Size[plot_vars$Type == "target"]*2.25,
                plot_vars$Size[plot_vars$Type == "insideND"]*1.5,
                plot_vars$Size[plot_vars$Type == "insideD"]+0.2,
                plot_vars$Size[plot_vars$Type == "outsideND"],
                plot_vars$Size[plot_vars$Type == "outsideD"])
  bio_col <- c(plot_vars$Fill[plot_vars$Type == "target"],
               plot_vars$Fill[plot_vars$Type == "outsideND"],
               plot_vars$Fill[plot_vars$Type == "outsideD"],
               plot_vars$Fill[plot_vars$Type == "insideND"],
               plot_vars$Fill[plot_vars$Type == "insideD"])

  mySiteInfo <- df_Sites %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::select(FinalLatitude, FinalLongitude, RefSiteFlag, COMID,
                  OutcaseCol, IncaseCol)
  myIncaseID = as.vector(unlist(mySiteInfo$IncaseCol))
  myOutcaseID = as.vector(unlist(mySiteInfo$OutcaseCol))

  data_refSites <- df_Sites %>%
    dplyr::filter(RefSiteFlag == 1) %>%
    dplyr::select(StationID, FinalLatitude, FinalLongitude, RefSiteFlag,
                  COMID, OutcaseCol, IncaseCol)
  myRefSites <- unique(as.vector(unlist(data_refSites$StationID)))

  # get sampling info (dates of samples)
  mySamps <- dplyr::filter(df_SampSummary, StationID == TargetSiteID) %>%
    dplyr::mutate(SampYear = lubridate::year(SampleDate))
  mySampsYears <- sort(as.integer(unique(mySamps$SampYear)))

  # ID RespSampleDates & Types ----
  myRespSampDates <- mySamps %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("SampleID"),
                        names_to = "SampleType", values_to = "ID") %>%
    dplyr::mutate(SampleType = sub("SampleID$", "", SampleType)) %>%
    dplyr::select(SampleDate, SampleType)
  myRespSampDates <- myRespSampDates[myRespSampDates$SampleType %in% biocommlist, ]
  myRespSampDates$yLoc <- NA_real_

  allRespSampTypes <- unique(myRespSampDates$SampleType)

  for (t in seq_along(allRespSampTypes)) {
    type <- allRespSampTypes[t]
    myRespSampDates <- myRespSampDates %>%
      dplyr::mutate(yLoc = ifelse(SampleType == type, -5*t, yLoc))
  }

  # Resp indices ----
  # get response information (CSCI, MMIhybrid, FIBI, etc.)
  if (useBC == TRUE) {
    str_sub <- paste0("Target Site: ", TargetSiteID, ", ", OutcaseLabel, " ",
                      myOutcaseID)
  } else {
    str_sub <- paste0("Target Site: ", TargetSiteID, "; Outside the case: ",
                      OutcaseLabel, "; Inside the case: ", IncaseLabel,
                      " ", myIncaseID)
  }

  if (!is.null(df_BMIMetrics)) {

    # Prep BMI data for plotting
    allBMImetrics <- df_BMIMetrics %>%
      dplyr::mutate(Quality = as.character(Quality),
                    Case = "Outside the case")
    allBMImetrics <- allBMImetrics %>%
      tidyr::pivot_longer(cols = all_of(BMIIndexGp), names_to = "Index",
                          values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                    Quality = factor(Quality, levels = c("Target",
                                                         "Not degraded",
                                                         "Degraded")),
                    Index = factor(Index),
                    Case = "Outside the case",
                    RefSite = ifelse(StationID %in% myRefSites, "Reference", NA),
                    QualityRef = dplyr::case_when(Quality == "Target" ~ "Target",
                                                  !is.na(RefSite) ~
                                                    paste0(RefSite, ", ", tolower(Quality)),
                                                  TRUE ~ Quality),
                    QualityRef = factor(QualityRef, levels = c("Target",
                                                               "Reference, not degraded",
                                                               "Reference, degraded",
                                                               "Not degraded",
                                                               "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index,
                    Score, Case, QualityRef)

    compBMImetrics <- df_BMIMetrics %>%
      dplyr::filter(StationID %in% comp.sites)%>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, all_of(BMIIndexGp),
                    Quality) %>%
      dplyr::mutate(Quality = as.character(Quality))

    compBMImetrics <- compBMImetrics %>%
      tidyr::pivot_longer(cols = all_of(BMIIndexGp), names_to = "Index",
                          values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                    Quality = factor(Quality, levels = c("Target",
                                                         "Not degraded",
                                                         "Degraded")),
                    Index = factor(Index),
                    Case = "Inside the case",
                    RefSite = ifelse(StationID %in% myRefSites, "Reference", NA),
                    QualityRef = dplyr::case_when(Quality == "Target" ~ "Target",
                                                  !is.na(RefSite) ~
                                                    paste0(RefSite, ", ", tolower(Quality)),
                                                  TRUE ~ Quality),
                    QualityRef = factor(QualityRef, levels = c("Target",
                                                               "Reference, not degraded",
                                                               "Reference, degraded",
                                                               "Not degraded",
                                                               "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index,
                    Score, Case, QualityRef)

    goodBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Not degraded")
    badBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Degraded")
    myBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Target")

    gap.good <- cbind.data.frame("getSiteInfo", "quality", nrow(goodBMImetrics),
                                 "Not degraded comparator samples available.")
    colnames(gap.good) <- c("fxnname", "condition", "result", "comment")
    gap.bad <- cbind.data.frame("getSiteInfo", "quality", nrow(badBMImetrics),
                                "Degraded comparator samples available.")
    colnames(gap.bad) <- c("fxnname", "condition", "result", "comment")
    gap.comps <- rbind(gap.good, gap.bad)
    rm(gap.good, gap.bad, goodBMImetrics, badBMImetrics)

    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gap.comps, fn.gaps, append = TRUE, col.names = FALSE,
                row.names = FALSE, sep = "\t")

    # ## Plot, Variables, Strings, other Aesthetics
    myBMISamps <- dplyr::filter(mySamps, !is.na(BMISampleID))
    lab.sub <- paste0("Comparator samples (n = ",
                      (nrow(compBMImetrics) - nrow(myBMISamps)),
                      " from ", (length(comp.sites) - 1), " sites)")

    str_title <- "Benthic macroinvertebrate index scores"
    str_ylab  <- "Score"

    ## Plot, BMI data by case ----
    allsamplesByCase <- rbind(compBMImetrics, allBMImetrics)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, !is.na(Score))
    targetSamples <- dplyr::filter(allsamplesByCase, StationID == TargetSiteID)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, StationID != TargetSiteID)

    fn_bmiscoresByCase <- paste0(TargetSiteID, "_BMI_IndexBoxplotsByCase.png")
    fn_bmiscoresByCase <- file.path(dir_path, fn_bmiscoresByCase)
    pBMIbyCase <- ggplot2::ggplot(allsamplesByCase,
                                  ggplot2::aes(y = round(Score, 3), x = Case,
                                               group = Case)) +
      ggplot2::geom_boxplot(na.rm = TRUE, staplewidth = 0.5) +
      ggplot2::geom_jitter(width = 0.2, height = 0.05, na.rm = TRUE,
                           ggplot2::aes(color = QualityRef, fill = QualityRef,
                                        shape = QualityRef, alpha = QualityRef,
                                        size = QualityRef))
    pBMIbyCase <- pBMIbyCase +
      ggplot2::geom_jitter(data = targetSamples, width = 0.2, na.rm = TRUE,
                           ggplot2::aes(color = QualityRef, fill = QualityRef,
                                        shape = QualityRef, alpha = QualityRef,
                                        size = QualityRef)) +
      ggplot2::scale_color_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_fill_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_shape_manual(values = bio_shp, drop = FALSE) +
      ggplot2::scale_alpha_manual(values = bio_alpha, drop = FALSE) +
      ggplot2::scale_size_manual(values = bio_size, drop = FALSE) +
      ggplot2::labs(title = str_title, subtitle = str_sub, caption = lab.sub,
                    y = str_ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                        size = ggplot2::rel(0.8)),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                           size = ggplot2::rel(0.5))) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black"),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
    if (boo_plot) {
      ggplot2::ggsave(fn_bmiscoresByCase, pBMIbyCase, width = plot_W,
                      height = plot_H, units = plot_units, dpi = plot_dpi)
    }## IF ~ boo_plot_by_case ~ END

  } else {
    myBMIMetrics <- NULL
  } ## IF ~ BMI_by_case ~ END

  if (!is.null(df_ALGMetrics)) {
    # Prep Alg data for plotting
    allALGmetrics <- df_ALGMetrics %>%
      dplyr::mutate(Quality = as.character(Quality),
                    Case = "Outside the case")
    allALGmetrics <- allALGmetrics %>%
      tidyr::pivot_longer(cols = all_of(ALGIndexGp), names_to = "Index",
                          values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                    Quality = factor(Quality, levels = c("Target",
                                                         "Not degraded",
                                                         "Degraded")),
                    Index = factor(Index),
                    Case = "Outside the case",
                    RefSite = ifelse(StationID %in% myRefSites, "Reference", NA),
                    QualityRef = dplyr::case_when(Quality == "Target" ~ "Target",
                                                  !is.na(RefSite) ~
                                                    paste0(RefSite, ", ", tolower(Quality)),
                                                  TRUE ~ Quality),
                    QualityRef = factor(QualityRef, levels = c("Target",
                                                               "Reference, not degraded",
                                                               "Reference, degraded",
                                                               "Not degraded",
                                                               "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index,
                    Score, Case, QualityRef)

    compALGmetrics <- df_ALGMetrics %>%
      dplyr::filter(StationID %in% comp.sites)%>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, all_of(ALGIndexGp),
                    Quality) %>%
      dplyr::mutate(Quality = as.character(Quality))

    compALGmetrics <- compALGmetrics %>%
      tidyr::pivot_longer(cols = all_of(ALGIndexGp), names_to = "Index",
                          values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                    Quality = factor(Quality, levels = c("Target",
                                                         "Not degraded",
                                                         "Degraded")),
                    Index = factor(Index),
                    Case = "Inside the case",
                    RefSite = ifelse(StationID %in% myRefSites, "Reference", NA),
                    QualityRef = dplyr::case_when(Quality == "Target" ~ "Target",
                                                  !is.na(RefSite) ~
                                                    paste0(RefSite, ", ", tolower(Quality)),
                                                  TRUE ~ Quality),
                    QualityRef = factor(QualityRef, levels = c("Target",
                                                               "Reference, not degraded",
                                                               "Reference, degraded",
                                                               "Not degraded",
                                                               "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index,
                    Score, Case, QualityRef)

    goodALGmetrics <- dplyr::filter(compALGmetrics, Quality=="Not degraded")
    badALGmetrics <- dplyr::filter(compALGmetrics, Quality=="Degraded")
    myALGmetrics <- dplyr::filter(compALGmetrics, Quality=="Target")

    gap.good <- cbind.data.frame("getSiteInfo", "quality", nrow(goodALGmetrics),
                                 "Not degraded comparator samples available.")
    colnames(gap.good) <- c("fxnname", "condition", "result", "comment")
    gap.bad <- cbind.data.frame("getSiteInfo", "quality", nrow(badALGmetrics),
                                "Degraded comparator samples available.")
    colnames(gap.bad) <- c("fxnname", "condition", "result", "comment")
    gap.comps <- rbind(gap.good, gap.bad)
    rm(gap.good, gap.bad, goodALGmetrics, badALGmetrics)

    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gap.comps, fn.gaps, append = TRUE, col.names = FALSE,
                row.names = FALSE, sep = "\t")

    # ## Plot, Variables, Strings, other Aesthetics
    myALGSamps <- dplyr::filter(mySamps, !is.na(ALGSampleID))
    lab.sub <- paste0("Comparator samples (n = ",
                      (nrow(compALGmetrics) - nrow(myALGSamps)),
                      " from ", (length(comp.sites) - 1), " sites)")

    str_title <- "Algal index scores"
    str_ylab  <- "Score"

    ## Plot, Alg data by case ----
    allsamplesByCase <- rbind(compALGmetrics, allALGmetrics)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, !is.na(Score))
    targetSamples <- dplyr::filter(allsamplesByCase, StationID == TargetSiteID)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, StationID != TargetSiteID)

    fn_algscoresByCase <- paste0(TargetSiteID, "_ALG_IndexBoxplotsByCase.png")
    fn_algscoresByCase <- file.path(dir_path, fn_algscoresByCase)
    pALGbyCase <- ggplot2::ggplot(allsamplesByCase,
                                  ggplot2::aes(y = round(Score, 3), x = Case,
                                               group = Case)) +
      ggplot2::geom_boxplot(na.rm = TRUE, staplewidth = 0.5) +
      ggplot2::geom_jitter(width = 0.2, height = 0.05, na.rm = TRUE,
                           ggplot2::aes(color = QualityRef, fill = QualityRef,
                                        shape = QualityRef, alpha = QualityRef,
                                        size = QualityRef))
    pALGbyCase <- pALGbyCase +
      ggplot2::geom_jitter(data = targetSamples, width = 0.2, na.rm = TRUE,
                           ggplot2::aes(color = QualityRef, fill = QualityRef,
                                        shape = QualityRef, alpha = QualityRef,
                                        size = QualityRef)) +
      ggplot2::scale_color_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_fill_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_shape_manual(values = bio_shp, drop = FALSE) +
      ggplot2::scale_alpha_manual(values = bio_alpha, drop = FALSE) +
      ggplot2::scale_size_manual(values = bio_size, drop = FALSE) +
      ggplot2::labs(title = str_title, subtitle = str_sub, caption = lab.sub,
                    y = str_ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                        size = ggplot2::rel(0.8)),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                           size = ggplot2::rel(0.5))) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black"),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
    if (boo_plot) {
      ggplot2::ggsave(fn_algscoresByCase, pALGbyCase, width = plot_W,
                      height = plot_H, units = plot_units, dpi = plot_dpi)
    }## IF ~ boo_plot_by_case ~ END

  } else {
    myALGmetrics <- NULL
  } ## IF ~ alg_by_case ~ END

  # TODO: adapt for fish
  if (!is.null(df_FishMetrics)) {
    # Prep Fish data for plotting
    allFISHmetrics <- df_FISHMetrics %>%
      dplyr::mutate(Quality = as.character(Quality),
                    Case = "Outside the case")
    allFISHmetrics <- allFISHmetrics %>%
      tidyr::pivot_longer(cols = all_of(FISHIndexGp), names_to = "Index",
                          values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                    Quality = factor(Quality, levels = c("Target",
                                                         "Not degraded",
                                                         "Degraded")),
                    Index = factor(Index),
                    Case = "Outside the case",
                    RefSite = ifelse(StationID %in% myRefSites, "Reference", NA),
                    QualityRef = dplyr::case_when(Quality == "Target" ~ "Target",
                                                  !is.na(RefSite) ~
                                                    paste0(RefSite, ", ", tolower(Quality)),
                                                  TRUE ~ Quality),
                    QualityRef = factor(QualityRef, levels = c("Target",
                                                               "Reference, not degraded",
                                                               "Reference, degraded",
                                                               "Not degraded",
                                                               "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index,
                    Score, Case, QualityRef)

    compFISHmetrics <- df_FISHMetrics %>%
      dplyr::filter(StationID %in% comp.sites)%>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, all_of(FISHIndexGp),
                    Quality) %>%
      dplyr::mutate(Quality = as.character(Quality))

    compFISHmetrics <- compFISHmetrics %>%
      tidyr::pivot_longer(cols = all_of(FISHIndexGp), names_to = "Index",
                          values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                    Quality = factor(Quality, levels = c("Target",
                                                         "Not degraded",
                                                         "Degraded")),
                    Index = factor(Index),
                    Case = "Inside the case",
                    RefSite = ifelse(StationID %in% myRefSites, "Reference", NA),
                    QualityRef = dplyr::case_when(Quality == "Target" ~ "Target",
                                                  !is.na(RefSite) ~
                                                    paste0(RefSite, ", ", tolower(Quality)),
                                                  TRUE ~ Quality),
                    QualityRef = factor(QualityRef, levels = c("Target",
                                                               "Reference, not degraded",
                                                               "Reference, degraded",
                                                               "Not degraded",
                                                               "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index,
                    Score, Case, QualityRef)

    goodFISHmetrics <- dplyr::filter(compFISHmetrics, Quality=="Not degraded")
    badFISHmetrics <- dplyr::filter(compFISHmetrics, Quality=="Degraded")
    myFISHmetrics <- dplyr::filter(compFISHmetrics, Quality=="Target")

    gap.good <- cbind.data.frame("getSiteInfo", "quality", nrow(goodFISHmetrics),
                                 "Not degraded comparator samples available.")
    colnames(gap.good) <- c("fxnname", "condition", "result", "comment")
    gap.bad <- cbind.data.frame("getSiteInfo", "quality", nrow(badFISHmetrics),
                                "Degraded comparator samples available.")
    colnames(gap.bad) <- c("fxnname", "condition", "result", "comment")
    gap.comps <- rbind(gap.good, gap.bad)
    rm(gap.good, gap.bad, goodFISHmetrics, badFISHmetrics)

    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gap.comps, fn.gaps, append = TRUE, col.names = FALSE,
                row.names = FALSE, sep = "\t")

    # ## Plot, Variables, Strings, other Aesthetics
    myFISHSamps <- dplyr::filter(mySamps, !is.na(FISHSampleID))
    lab.sub <- paste0("Comparator samples (n = ",
                      (nrow(compFISHmetrics) - nrow(myFISHSamps)),
                      " from ", (length(comp.sites) - 1), " sites)")

    str_title <- "Benthic macroinvertebrate index scores"
    str_ylab  <- "Score"

    ## Plot, Fish data by case ----
    allsamplesByCase <- rbind(compFISHmetrics, allFISHmetrics)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, !is.na(Score))
    targetSamples <- dplyr::filter(allsamplesByCase, StationID == TargetSiteID)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, StationID != TargetSiteID)

    fn_FISHscoresByCase <- paste0(TargetSiteID, "_FISH_IndexBoxplotsByCase.png")
    fn_FISHscoresByCase <- file.path(dir_path, fn_FISHscoresByCase)
    pFISHbyCase <- ggplot2::ggplot(allsamplesByCase,
                                  ggplot2::aes(y = round(Score, 3), x = Case,
                                               group = Case)) +
      ggplot2::geom_boxplot(na.rm = TRUE, staplewidth = 0.5) +
      ggplot2::geom_jitter(width = 0.2, height = 0.05, na.rm = TRUE,
                           ggplot2::aes(color = QualityRef, fill = QualityRef,
                                        shape = QualityRef, alpha = QualityRef,
                                        size = QualityRef))
    pFISHbyCase <- pFISHbyCase +
      ggplot2::geom_jitter(data = targetSamples, width = 0.2, na.rm = TRUE,
                           ggplot2::aes(color = QualityRef, fill = QualityRef,
                                        shape = QualityRef, alpha = QualityRef,
                                        size = QualityRef)) +
      ggplot2::scale_color_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_fill_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_shape_manual(values = bio_shp, drop = FALSE) +
      ggplot2::scale_alpha_manual(values = bio_alpha, drop = FALSE) +
      ggplot2::scale_size_manual(values = bio_size, drop = FALSE) +
      ggplot2::labs(title = str_title, subtitle = str_sub, caption = lab.sub,
                    y = str_ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                        size = ggplot2::rel(0.8)),
                     plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                           size = ggplot2::rel(0.5))) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black"),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
    if (boo_plot) {
      ggplot2::ggsave(fn_FISHscoresByCase, pFISHbyCase, width = plot_W,
                      height = plot_H, units = plot_units, dpi = plot_dpi)
    }## IF ~ boo_plot_by_case ~ END

  } else {
    myFISHmetrics <- NULL
  } ## IF ~ fish_by_case ~ END

  # Site photos ----
  # Check for presence of Photos in data directory. If not present, skip.
  if (dir.exists(dir_photo) == TRUE & length(list.files(dir_photo)) > 0) {
    photofiles <- list.files(dir_photo)
    ifelse(!dir.exists(file.path(dir_path, "Photos")) == TRUE,
           dir.create(file.path(dir_path, "Photos")), FALSE)
    have.photos <- FALSE
    for (l in 1:length(photofiles)) {
      photoname <- photofiles[l]
      if (str_detect(photoname, all_of(TargetSiteID)) == TRUE) {
        file.copy(file.path(dir_photo, photoname),
                  file.path(dir_path, "Photos", photoname))
        message(paste0(photoname, " copied."))
        have.photos <- TRUE
      }
    }## FOR ~ l ~ END
  } else {
    have.photos <- FALSE
    msg <- "Photo directory does not exist."
    message(msg)
  }## IF ~ dir.exists(dir_photo)==TRUE & length(list.files(dir_photo)) > 0 ~ END

  if (!have.photos) {
    message(paste0("No site photos are available for ", TargetSiteID))

    gap.photos <- cbind.data.frame("getSiteInfo", "photos", 0,
                                   "Site photos are not available.")
    colnames(gap.photos) <- c("fxnname", "condition", "result", "comment")

    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gap.photos, fn.gaps, append = TRUE, col.names = FALSE,
                row.names = FALSE, sep = "\t")
  }## IF ~ !have.photos ~ END

  message("Completed transferring any available site files.")

  # nothing returned; only graphics written to "SiteInfo" folder

}

