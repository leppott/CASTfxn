#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.2
#
#' @title Site Info
#'
#' @description Get site info of provided siteID.
#'
#' @details Summary info including lat/long, ref status, cluster membership,
#' samples from site
#'
#' Requires packages dplyr, sf, tidyr
#'
#' Required objects:
#'
#' * data_Sites; StationID, FinalLatitude, FinalLongitude
#' , WaterbodyName, GIS_County, CARefSite_2017, COMID
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
#'                 and "outside the case" identifiers. If useBC == TRUE, "outside
#'                 the case" will be cluster; if useBC == FALSE, "inside the case"
#'                 will be cluster.
#' @param df_BkgData dataframe containing watershed-scale stressor variables from StreamCat
#' @param df_BkgInfo dataframe containing metadata for the variables in df_BkgData
#' @param df_SampSummary dataframe containing sample IDs for samples collected
#'                       at the target site, organized by sample date (rows)
#'                       and type (columns)
#' @param df_BMIMetrics dataframe containing BMI sample index and metric values
#' @param BMIIndexGp vector containing one or more BMI indices for display purpose only
#' @param df_ALGMetrics dataframe containing algae sample index and metric values.
#'                      Default is NULL.
#' @param ALGIndexGp vector containing one or more algal indices for display purpose only
#' @param df_FishMetrics dataframe containing fish sample index and metric values
#' @param FishIndexGp vector containing one or more fish indices for display purpose only
#' @param comp.sites vector containing inside-the-case (comparator) site IDs
#' @param all.sites vector containing all outside-the-case site IDs
#' @param OutcaseLabel Label for the "outside the case" identifier. Default = NULL.
#' @param IncaseLabel Label for the "inside the case" identifier. Default = NULL.
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = FALSE.
#' @param UseAllCompReaches TRUE to use all inside-the-case reaches, even those
#'                          without sites; FALSE to use only inside-the-case reaches
#'                          with sites. DEFAULT = FALSE.
#' @param dir_photo directory containing all site photos (for every site in the data set).
#'                  Default is file.path(getwd(), "Data", "Photos").
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
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
#'
#' # Data
#' # data import, example
#' #data_Sites <- read.delim(paste(myDir.Data,"data_Sites.tab",sep=""))
#' #data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep="")
#' #                               , na.strings = c(""," "))
#' #data.bmi.metrics <- read.delim(paste(myDir.Data,"data.bmi.metrics.tab",sep=""))
#' #data.algae.metrics <- read.delim(paste(myDir.Data,"data.algae.metrics.tab",sep=""))
#'
#' # Data getSiteInfo
#' # data, example included with package
#' data_Sites <- data_Sites
#' data.SampSummary   <- data_SampSummary
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#'
#' dir_sub <- "SiteInfo"
#'
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID,
#'                                 TargetCOMID,
#'                                 df_Sites,
#'                                 df_BkgData,
#'                                 df_BkgInfo,
#'                                 df_SampSummary,
#'                                 df_BMIMetrics = NULL,
#'                                 BMIIndexGp = NULL,
#'                                 df_ALGMetrics = NULL,
#'                                 ALGIndexGp = NULL,
#'                                 df_FishMetrics = NULL,
#'                                 FishIndexGp = NULL,
#'                                 comp.sites,
#'                                 comp.reaches,
#'                                 all.sites,
#'                                 IncaseLabel = NULL,
#'                                 OutcaseLabel = NULL,
#'                                 UseBC = FALSE,
#'                                 UseAllCompReaches = FALSE,
#'                                 dir_photo = file.path(getwd(), "Data", "Photos"),
#'                                 dir_results = file.path(getwd(), "Results"),
#'                                 dir_sub = "SiteInfo",
#'                                 boo_plot = TRUE)
#' }
#' @export
getSiteInfo <- function(TargetSiteID,
                        TargetCOMID,
                        df_Sites,
                        df_BkgData,
                        df_BkgInfo,
                        df_SampSummary,
                        df_BMIMetrics = NULL,
                        BMIIndexGp = NULL,
                        df_ALGMetrics = NULL,
                        ALGIndexGp = NULL,
                        df_FishMetrics = NULL,
                        FishIndexGp = NULL,
                        comp.sites,
                        comp.reaches,
                        all.sites,
                        IncaseLabel = NULL,
                        OutcaseLabel = NULL,
                        UseBC = FALSE,
                        UseAllCompReaches = FALSE,
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
    df_BkgData = data_bkgdata
    df_BkgInfo = data_bkginfo
    df_SampSummary = data_sampSummary
    df_BMIMetrics = data_bmiMetrics
    BMIIndexGp = bmiIndexGp
    df_ALGMetrics = data_algMetrics
    ALGIndexGp = algIndexGp
    df_FishMetrics = data_fishMetrics
    FishIndexGp = fishIndexGp
    comp.sites = list.CompSites$comp.sites
    comp.reaches = list.CompSites$comp.reaches
    all.sites = list.CompSites$all.sites
    OutcaseLabel = outcaseLabel
    IncaseLabel = incaseLabel
    UseBC = FALSE
    UseAllCompReaches = FALSE
    dir_photo = file.path(dir_data, "Photos")
    dir_results = dir_results
    dir_sub = "SiteInfo"
    boo_plot = TRUE
  }

  # define pipe
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}
  all_na <- function(x) {all(is.na(x))}

  # Write results directory ----
  out.dir <- dirname(dir_plots)
  out.folders <- c(out.dir, basename(dir_plots), TargetSiteID, dir_sub)

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

  ## Plot, Variables, Output Size (inches)
  plot_H <- 4
  plot_W <- 6
  ppi <- 300

  #
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

    # Specify symbology for target, reference not degraded, reference degraded,
    # not degraded, and degraded in that order
    # Outline color
    bio_col <- c("red", "blue", "blue", "steelblue2", "gray25")
    # Fill color
    bio_fill <- c("red", "steelblue2", "gray25", "steelblue2", "gray25")
    # Shape (triangle, circle, down triangle, circle, down triangle)
    bio_shp <- c(17, 21, 25, 21, 25)
    # Transparency
    bio_alpha <- c(1, 0.5, 0.3, 0.5, 0.3)
    # Size
    bio_size <- c(1.5, 1, 1, 1, 1)

    str_title <- "Benthic macroinvertebrate index scores"
    str_ylab  <- "Score"

    ## Plot, Data by case
    allsamplesByCase <- rbind(compBMImetrics, allBMImetrics)
    # allsamplesByCase <- rbind(refBMImetrics, compBMImetrics, allBMImetrics)
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
                      height = plot_H, units = "in")
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

    # Specify symbology for target, reference not degraded, reference degraded,
    # not degraded, and degraded in that order
    # Outline color
    bio_col <- c("red", "blue", "blue", "steelblue2", "gray25")
    # Fill color
    bio_fill <- c("red", "steelblue2", "gray25", "steelblue2", "gray25")
    # Shape (triangle, circle, down triangle, circle, down triangle)
    bio_shp <- c(17, 21, 25, 21, 25)
    # Transparency
    bio_alpha <- c(1, 0.5, 0.3, 0.5, 0.3)
    # Size
    bio_size <- c(1.5, 1, 1, 1, 1)

    str_title <- "Algal index scores"
    str_ylab  <- "Score"

    ## Plot, Data by case
    allsamplesByCase <- rbind(compALGmetrics, allALGmetrics)
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
                      height = plot_H, units = "in")
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

    # Specify symbology for target, reference not degraded, reference degraded,
    # not degraded, and degraded in that order
    # Outline color
    bio_col <- c("red", "blue", "blue", "steelblue2", "gray25")
    # Fill color
    bio_fill <- c("red", "steelblue2", "gray25", "steelblue2", "gray25")
    # Shape (triangle, circle, down triangle, circle, down triangle)
    bio_shp <- c(17, 21, 25, 21, 25)
    # Transparency
    bio_alpha <- c(1, 0.5, 0.3, 0.5, 0.3)
    # Size
    bio_size <- c(1.5, 1, 1, 1, 1)

    str_title <- "Benthic macroinvertebrate index scores"
    str_ylab  <- "Score"

    ## Plot, Data by case
    allsamplesByCase <- rbind(compFISHmetrics, allFISHmetrics)
    # allsamplesByCase <- rbind(refFISHmetrics, compFISHmetrics, allFISHmetrics)
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
                      height = plot_H, units = "in")
    }## IF ~ boo_plot_by_case ~ END

  } else {
    myFISHmetrics <- NULL
  } ## IF ~ fish_by_case ~ END

  # Check for presence of Photos in data directory. If not present, skip.
  if (dir.exists(dir_photo) == TRUE & length(list.files(dir_photo)) > 0) {
    photofiles <- list.files(dir_photo)
    have.photos <- FALSE
    for (l in 1:length(photofiles)) {
      ifelse(!dir.exists(file.path(dir_path, "Photos")) == TRUE,
             dir.create(file.path(dir_path, "Photos")), FALSE)
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

  if (!is.null(df_BkgData)) {
    # Get background data from df_BkgData; use COMID to select single reach
    data_compbkgd <- dplyr::filter(data_bkgdata, COMID %in% comp.reaches)
    data_sitebkgdata <- dplyr::filter(data_compbkgd, COMID == TargetCOMID)
    naVars.site <- unique(data_sitebkgdata$Metric[is.na(data_sitebkgdata$WatershedValue)])
    data_sitebkgdata <- dplyr::filter(data_sitebkgdata, !is.na(WatershedValue))
    vars.site <- unique(data_sitebkgdata$StreamCatVar[!is.na(data_sitebkgdata$WatershedValue)])

    if (length(naVars.site) > 0) { # if any NA values, then missing data for site
      # Missing one or more values in StreamCat for the target reach.
      naVars.site <- paste(naVars.site, collapse = "; ")
      gapcomment <- paste0("Missing background data for site ",
                           TargetSiteID, "on reach with COMID = ", TargetCOMID)
      gaps <- cbind.data.frame("getSiteInfo", "Background Data", naVars.site,
                               gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")
    }

    if (length(vars.site) > 0) { # Background data exists
      fn_bkg <- paste0(TargetSiteID, "_WSstressorData.tab")
      write.table(data_sitebkgdata, file.path(dir_path, fn_bkg), append = FALSE,
                  sep = "\t", col.names = TRUE, row.names = FALSE)

      # If EPA wants to use all comparator reaches, make sure to set
      # useAllCompReaches to TRUE in the CASTool_Metadata.xlsx file.
      if (UseAllCompReaches) { # use all comparator reaches, even those not having sites
        if (useBC == TRUE) {
          outcaseID <- mySiteInfo$OutcaseCol # this represents cluster ID
          data_compbkgd <- df_BkgData[df_BkgData$ClusterID == outcaseID, ]
        } else { # useBC == FALSE; cluster ID is the inside the case ID
          incaseID <- mySiteInfo$IncaseCol # this represents cluster ID
          data_compbkgd <- df_BkgData[df_BkgData$ClusterID == incaseID, ]
        }
        str_caption <- paste0("Target reach (", TargetCOMID, ") relative to ",
                              "distribution of values for all comparator reaches")
      } else { # use only comparator reaches having sites
        data_compbkgd <- df_BkgData[df_BkgData$COMID %in% comp.reaches, ]
        str_caption <- paste0("Target reach (", TargetCOMID, ") relative to ",
                              "distribution of values for all comparator sites' reaches")
      }

      # Get metadata from fn_bkginfo
      data_compbkgd <- data_compbkgd %>%
        dplyr::mutate(PctRank = round(dplyr::percent_rank(WatershedValue)*100, 0),
                      Scaled = scale(WatershedValue, scale = TRUE, center = TRUE))
      # Note that this results in a column name "Scaled[, 1]"
      # which is a column with attributes of center and scale

      # Draw boxplots
      # TODO: Ask EPA if they want to group variables that do not have years

      # Prepare boxplot main elements
      str_title <- paste0(TargetSiteID, ": Site watershed-scale stressors")

      for (i in seq_along(vars.site)) { # StreamCatVar (no year--Metric includes year)
        print(paste0("Prepping ", vars.site[i]))
        plotvar <- vars.site[i]
        fn.bkgplot <- file.path(dir_path, paste0(TargetSiteID, "_WSstress_",
                                                 plotvar, ".png"))
        if (plotvar == "WSAREASQKM") {
          str_sub <- "Watershed area, km2"
        } else {
          str_sub <- unique(df_BkgInfo$Label[df_BkgInfo$StreamCatVar == plotvar])
        }

        df.plot.comp <- dplyr::filter(data_compbkgd, StreamCatVar == plotvar)
        dataYears <- sort(unique(df.plot.comp$Year))

        if (length(dataYears) > 0) {
          # TODO: Figure out matching samp years to data years to allow plotting
          # Probably add a SampYears column to df.plot.site to add as labels

          xmin <- min(df.plot.comp$Year) - min(df.plot.comp$Year) %% 10
          xmax <- max(df.plot.comp$Year) - (max(df.plot.comp$Year) %% 10) + 10
          numYears <- xmax - xmin

          p.box <- ggplot2::ggplot(data = df.plot.comp,
                                   ggplot2::aes(x = Year, y = WatershedValue,
                                                group = Year)) +
            ggplot2::geom_boxplot(outliers = TRUE, outlier.size = 0.75, na.rm = TRUE,
                                  staplewidth = 0.5, linewidth = 0.1) +
            ggplot2::geom_jitter(data = df.plot.comp, width = 0.1, height = 0,
                                 ggplot2::aes(x = Year, y = WatershedValue),
                                 size = 0.25, na.rm = TRUE, color = "cyan4") +
            ggplot2::scale_x_continuous(limits = c(xmin, xmax),
                                        breaks = scales::breaks_width(1)) +
            ggplot2::labs(title = str_title, subtitle = str_sub,
                          caption = str_caption) +
            ggplot2::ylab("Watershed Value") +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5),
                           plot.caption = ggplot2::element_text(size = 5)) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black",
                                                               size = 6,
                                                               angle = 90,
                                                               vjust = 0.6,
                                                               hjust = 0.5),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 6),
                           axis.title.x = ggplot2::element_text(color = "black",
                                                                size = 8),
                           axis.title.y = ggplot2::element_text(color = "black",
                                                                size = 8),
                           legend.position = "none")

          p.box <- p.box +
            ggplot2::geom_point(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                                ggplot2::aes(x = Year, y = WatershedValue, group = Year),
                                color = "red", shape = 17) +
            ggplot2::geom_text(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                               ggplot2::aes(x = Year, y = WatershedValue,
                                            group = Year,
                                            label = formatC(WatershedValue,
                                                            format = "fg",
                                                            digits = 3)),
                               size = 2.1, color = "red", nudge_x = 0.75,
                               nudge_y = 0.75)
          if(boo_plot){
            ggplot2::ggsave(fn.bkgplot, p.box, width = plot_W, height = plot_H,
                            units = "in")
          }## IF ~ boo_plot ~ END

        } else { # no years to consider
          p.box <- ggplot2::ggplot(data = df.plot.comp,
                                   ggplot2::aes(x = StreamCatVar, y = WatershedValue)) +
            ggplot2::geom_boxplot(outliers = TRUE, outlier.size = 0.75, na.rm = TRUE,
                                  staplewidth = 0.5, linewidth = 0.1) +
            ggplot2::geom_jitter(data = df.plot.comp, width = 0.1, height = 0,
                                 ggplot2::aes(x = StreamCatVar, y = WatershedValue),
                                 size = 0.25, na.rm = TRUE, color = "cyan4") +
            ggplot2::labs(title = str_title, subtitle = str_sub,
                          caption = str_caption) +
            ggplot2::xlab(str_sub) +
            ggplot2::ylab("Watershed Value")

          p.box <- p.box +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5),
                           plot.caption = ggplot2::element_text(size = 5)) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black",
                                                               size = 6,
                                                               # angle = 90,
                                                               vjust = 0.6,
                                                               hjust = 0.5),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 6),
                           axis.title.x = ggplot2::element_text(color = "black",
                                                                size = 8),
                           axis.title.y = ggplot2::element_text(color = "black",
                                                                size = 8),
                           legend.position = "none")

          p.box <- p.box +
            ggplot2::geom_point(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                                ggplot2::aes(x = StreamCatVar, y = WatershedValue),
                                color = "red", shape = 17) +
            ggplot2::geom_text(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                               ggplot2::aes(x = StreamCatVar, y = WatershedValue,
                                            label = formatC(WatershedValue,
                                                            format = "fg",
                                                            digits = 3)),
                               size = 2.3, color = "red", nudge_x = 0.15,
                               nudge_y = 0.15)
          if(boo_plot){
            ggplot2::ggsave(fn.bkgplot, p.box, width = plot_W, height = plot_H,
                            units = "in")
          }## IF ~ boo_plot ~ END
        }## If/else for graphs ends
      }## for loop over variables ends
    }  # End background data portion
  }

  #
  # nothing returned; only graphics written to "SiteInfo" folder

}

