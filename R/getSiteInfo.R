#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
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
#' @param df_Sites dataframe containing site data, including "inside the case"
#'                   and "outside the case" identifiers. If useBC == TRUE, "outside
#'                   the case" will be cluster; if useBC == FALSE, "inside the case"
#'                   will be cluster.
#' @param df_BkgData dataframe containing anthropogenically-influenced variables from StreamCat
#' @param df_BkgInfo dataframe containing metadata for the variables in df_BkgData
#' @param df_SampSummary dataframe containing sample IDs for samples collected
#'                         at the target site, organized by sample date (rows)
#'                         and type (columns)
#' @param df_BMIMetrics dataframe containing BMI sample index and metric values
#' @param BMIIndexGp vector containing one or more BMI indices for display purpose only
#' @param df_ALGMetrics dataframe containing algae sample index and metric values.
#'                        Default is NULL.
#' @param ALGIndexGp vector containing one or more algal indices for display purpose only
#' @param df_FishMetrics dataframe containing fish sample index and metric values
#' @param FishIndexGp vector containing one or more fish indices for display purpose only
#' @param comp.sites vector containing comparator site IDs
#' @param OutcaseLabel Label for the "outside the case" identifier. Default = NULL.
#' @param IncaseLabel Label for the "inside the case" identifier. Default = NULL.
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = "FALSE"
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
#' list.SiteSummary <- getSiteInfo(TargetSiteID
#'                                 , dir_results
#'                                 , data_Sites
#'                                 , data.SampSummary
#'                                 , data.303d.ComID
#'                                 , data.bmi.metrics
#'                                 , data.algae.metrics
#'                                 , map_proj
#'                                 , map_outline
#'                                 , map_flowline
#'                                 , dir_sub=dir_sub)
#' }
#' @export
getSiteInfo <- function(TargetSiteID
                        , df_Sites
                        , df_BkgData
                        , df_BkgInfo
                        , df_SampSummary
                        , df_BMIMetrics = NULL
                        , BMIIndexGp
                        , df_ALGMetrics = NULL
                        , ALGIndexGp
                        , df_FishMetrics = NULL
                        , FishIndexGp
                        , comp.sites
                        , all.sites
                        , OutcaseLabel = NULL
                        , IncaseLabel = NULL
                        , UseBC = FALSE
                        # , data_cluster
                        # , data_mods = NULL
                        # , data_303d = NULL
                        , dir_photo = file.path(getwd(), "Data", "Photos")
                        , dir_results = file.path(getwd(), "Results")
                        , dir_sub = "SiteInfo"
                        , boo_plot = TRUE
) {##FUNCTION.START

  # DEBUG
  boo_DEBUG <- FALSE
  #
  if (boo_DEBUG == TRUE) {
    TargetSiteID = TargetSiteID
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
    comp.sites = comp_sites
    all.sites = all_sites
    OutcaseLabel = outcaseLabel
    IncaseLabel = incaseLabel
    useBC = FALSE
    # data_cluster = NULL
    # data_mods = NULL
    # data_303d = NULL
    dir_photo = file.path(dir_data, "Photos")
    dir_results = dir_results
    dir_sub = "SiteInfo"
    boo_plot = TRUE
  }

  not_all_na <- function(x) {!all(is.na(x))}
  all_na <- function(x) {all(is.na(x))}

  # check for and create (if necessary) dir_results and SiteID subdirectory
  # default structure: Results/TargetSiteID/SiteInfo

  #dir_results = file.path(getwd(), "Results")
  dir_sub2 <- TargetSiteID
  dir_sub3 <- dir_sub
  ifelse(!dir.exists(dir_results) == TRUE
         , dir.create(dir_results)
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, dir_sub2)) == TRUE
         , dir.create(file.path(dir_results, dir_sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, dir_sub2, dir_sub3)) == TRUE
         , dir.create(file.path(dir_results, dir_sub2, dir_sub3))
         , FALSE)

  dir_path <- file.path(dir_results, dir_sub2, dir_sub3)

  # Define pipe
  `%>%` <- dplyr::`%>%`

  ## Plot, Variables, Output Size (inches)
  plot_H <- 4
  plot_W <- 6
  ppi <- 300

  #
  if (useBC == TRUE) {
    mySiteInfo <- df_Sites %>%
      dplyr::filter(StationID == TargetSiteID) %>%
      dplyr::select(FinalLatitude, FinalLongitude, WaterbodyName
                    , RefSiteFlag, COMID, OutcaseCol)
    outcaseID <- mySiteInfo$OutcaseCol # this represents cluster ID
    data_compbkgd <- df_BkgData[df_BkgData$ClusterID == outcaseID, ]
  } else { # useBC == FALSE; cluster ID is inside the case ID
    mySiteInfo <- df_Sites %>%
      dplyr::filter(StationID == TargetSiteID) %>%
      dplyr::select(FinalLatitude, FinalLongitude, WaterbodyName
                    , RefSiteFlag, COMID, OutcaseCol, IncaseCol)
    incaseID <- mySiteInfo$IncaseCol # this represents cluster ID
    data_compbkgd <- df_BkgData[df_BkgData$ClusterID == incaseID, ]
  }
  myCOMID <- mySiteInfo$COMID
  data_refSites <- df_Sites %>%
    dplyr::filter(RefSiteFlag == 1) %>%
    dplyr::select(StationID, FinalLatitude, FinalLongitude, COMID)
  myRefCOMIDs <- as.vector(unique(data_refSites$COMID))

  data_compsites <- dplyr::filter(df_Sites, StationID %in% comp.sites)
  myCompCOMIDs <- as.vector(unique(data_compsites$COMID))

  # get sampling info (dates of samples)
  mySamps <- dplyr::filter(df_SampSummary, StationID == TargetSiteID) %>%
    dplyr::mutate(SampYear = lubridate::year(SampleDate))
  mySampsYears <- sort(as.integer(unique(mySamps$SampYear)))

  # get response information (CSCI, MMIhybrid, FIBI, etc.)
  if (useBC == TRUE) {
    str_sub <- paste0("Target Site: ", TargetSiteID, ", ", OutcaseLabel, " "
                      , outcaseID)
  } else {
    str_sub <- paste0("Target Site: ", TargetSiteID, "; Outside the case: "
                      , OutcaseLabel, "; Inside the case: ", IncaseLabel
                      , " ", incaseID)
  }

  if (!is.null(df_BMIMetrics)) {
    # Prep BMI data for plotting
    OutSamples <- df_BMIMetrics %>%
      dplyr::mutate(Quality = as.character(Quality)
                    , Case = "Outside the case")
    OutSamples <- OutSamples %>%
      tidyr::pivot_longer(cols = all_of(BMIIndexGp), names_to = "Index"
                          , values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID
                                     , "Target", Quality)
                    , Quality = factor(Quality, levels = c("Target"
                                                           , "Not degraded"
                                                           , "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index
                    , Score, Case)

    compBMImetrics <- df_BMIMetrics %>%
      dplyr::filter(StationID %in% comp.sites)%>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, all_of(BMIIndexGp)
                    , Quality) %>%
      dplyr::mutate(Quality = as.character(Quality))
    compBMImetrics <- compBMImetrics %>%
      tidyr::pivot_longer(cols = all_of(BMIIndexGp), names_to = "Index"
                          , values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID
                                     , "Target", Quality)
                    , Quality = factor(Quality, levels = c("Target"
                                                           , "Not degraded"
                                                           , "Degraded"))
                    , Index = factor(Index)
                    , Case = "Inside the case")
    goodBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Not degraded")
    badBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Degraded")
    myBMImetrics <- dplyr::filter(compBMImetrics, Quality=="Target")

    gap.good <- cbind.data.frame("getSiteInfo", "quality", nrow(goodBMImetrics)
                                 , "Not degraded comparator samples available.")
    colnames(gap.good) <- c("fxnname", "condition", "result", "comment")
    gap.bad <- cbind.data.frame("getSiteInfo", "quality", nrow(badBMImetrics)
                                , "Degraded comparator samples available.")
    colnames(gap.bad) <- c("fxnname", "condition", "result", "comment")
    gap.comps <- rbind(gap.good, gap.bad)
    rm(gap.good, gap.bad)

    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gap.comps, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")

    ## Plot, Variables, Strings, other Aesthetics
    myBMISamps <- dplyr::filter(mySamps, !is.na(BMISampleID))
    lab.sub <- paste0("Comparator samples (n = "
                      , (nrow(compBMImetrics) - nrow(myBMISamps))
                      , " from ", (length(comp.sites) - 1), " sites)")

    bio_col <- c("red", "steelblue2", "gray25") # Target, Not degraded, Degraded
    bio_shp <- c(17, 21, 25) # triangle, circle, and down triangle
    bio_alpha <- c(1, 0.5, 0.3)
    bio_size <- c(1.5, 1, 1)

    str_title <- "Benthic macroinvertebrate index scores"
    str_ylab  <- "Score"

    ## Plot, Data
    fn_bmiscores <- paste0(TargetSiteID, "_BMI_IndexBoxplots.png")
    fn_bmiscores <- file.path(dir_path,fn_bmiscores)
    pBMI <- ggplot2::ggplot(compBMImetrics, ggplot2::aes(y = round(Score, 3)
                                                         , x = Index
                                                         , group = Index)) +
      ggplot2::geom_boxplot(na.rm = TRUE) +
      ggplot2::geom_jitter(width = 0.2, na.rm = TRUE
                           , ggplot2::aes(color = Quality, fill = Quality
                                          , shape = Quality, alpha = Quality
                                          , size = Quality)) +
      ggplot2::scale_color_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_fill_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_shape_manual(values = bio_shp, drop = FALSE) +
      ggplot2::scale_alpha_manual(values = bio_alpha, drop = FALSE) +
      ggplot2::scale_size_manual(values = bio_size, drop = FALSE) +
      ggplot2::labs(title = str_title, subtitle = str_sub, caption = lab.sub
                    , y = str_ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                        size = ggplot2::rel(0.8))
                     , plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                             size = ggplot2::rel(0.5))) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black")
                     , axis.ticks.y = ggplot2::element_blank()
                     , axis.title.x = ggplot2::element_blank())
    if (boo_plot) {
      ggplot2::ggsave(fn_bmiscores, pBMI, width = plot_W, height = plot_H
                      , units = "in")
    }## IF ~ boo_plot ~ END

    ## Plot, Data by case
    allsamplesByCase <- rbind(compBMImetrics, OutSamples)
    targetSamples <- dplyr::filter(allsamplesByCase, StationID == TargetSiteID)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, StationID != TargetSiteID)

    fn_bmiscoresByCase <- paste0(TargetSiteID, "_BMI_IndexBoxplotsByCase.png")
    fn_bmiscoresByCase <- file.path(dir_path, fn_bmiscoresByCase)
    pBMIbyCase <- ggplot2::ggplot(allsamplesByCase, ggplot2::aes(y = round(Score, 3)
                                                                 , x = Case
                                                                 , group = Case)) +
      ggplot2::geom_boxplot(na.rm = TRUE) +
      ggplot2::geom_jitter(width = 0.2, na.rm = TRUE
                           , ggplot2::aes(color = Quality, fill = Quality
                                          , shape = Quality, alpha = Quality
                                          , size = Quality)) +
      ggplot2::geom_jitter(data = targetSamples, width = 0.2, na.rm = TRUE
                           , ggplot2::aes(color = Quality, fill = Quality
                                          , shape = Quality, alpha = Quality
                                          , size = Quality)) +
      ggplot2::scale_color_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_fill_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_shape_manual(values = bio_shp, drop = FALSE) +
      ggplot2::scale_alpha_manual(values = bio_alpha, drop = FALSE) +
      ggplot2::scale_size_manual(values = bio_size, drop = FALSE) +
      ggplot2::labs(title = str_title, subtitle = str_sub, caption = lab.sub
                    , y = str_ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                        size = ggplot2::rel(0.8))
                     , plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                             size = ggplot2::rel(0.5))) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black")
                     , axis.ticks.y = ggplot2::element_blank()
                     , axis.title.x = ggplot2::element_blank())
    if (boo_plot) {
      ggplot2::ggsave(fn_bmiscoresByCase, pBMIbyCase, width = plot_W
                      , height = plot_H, units = "in")
    }## IF ~ boo_plot_by_case ~ END

  } else {
    myBMIMetrics <- NULL
  }


  if (!is.null(df_ALGMetrics)) {
    # Prep Alg data for plotting
    compALGmetrics <- df_ALGMetrics %>%
      dplyr::filter(StationID %in% comp.sites)%>%
      dplyr::select(StationID, AlgSampID, AlgSampDate, Quality
                    , all_of(ALGIndexGp))
    compALGmetrics <- compALGmetrics %>%
      tidyr::pivot_longer(cols = all_of(ALGIndexGp), names_to = "Index"
                          , values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID
                                     , "Target", Quality)
                    , Quality = as.factor(Quality)
                    , Index = as.factor(Index))
    goodALGmetrics <- dplyr::filter(compALGmetrics, Quality == "Good")
    badALGmetrics <- dplyr::filter(compALGmetrics, Quality == "Degraded")
    myALGmetrics <- dplyr::filter(compALGmetrics, Quality == "Target")

    ## Plot, Variables, Strings, other Aesthetics
    myAlgSamps <- dplyr::filter(mySamps, !is.na(AlgSampID))
    lab.sub <- paste0("Comparator samples (n = ", (nrow(compALGmetrics) - nrow(myAlgSamps))
                      , " from ", (length(comp.sites) - 1), " sites)")

    bio_col <- c("dark gray", "blue", "red") # Degraded, Good, Target
    bio_shp <- c(25, 21, 17) # down triangle, circle, and triangle
    bio_alpha <- c(0.5, 0.5, 1)

    str_title <- "Algal community index scores"
    # str_sub <- paste0("Target Site: ", TargetSiteID, ", cluster ", mySiteInfo$clust)
    str_xlab  <- "Index"
    str_ylab  <- "Score"

    ## Plot, Data
    fn_algscores <- paste0(TargetSiteID, "_ALGAE_IndexBoxplots.png")
    fn_algscores <- file.path(dir_path, fn_algscores)
    pAlg <- ggplot2::ggplot(compALGmetrics, ggplot2::aes(y = round(Score, 3)
                                                         , x = Index
                                                         , group = Index)) +
      ggplot2::geom_boxplot(na.rm = TRUE) +
      ggplot2::geom_jitter(size = 2, width = 0.2, na.rm = TRUE
                           , ggplot2::aes(color = Quality, fill = Quality
                                          , shape = Quality, alpha  = Quality)) +
      ggplot2::scale_color_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_fill_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_shape_manual(values = bio_shp, drop = FALSE) +
      ggplot2::scale_alpha_manual(values = bio_alpha, drop = FALSE) +
      ggplot2::labs(title = str_title, subtitle = str_sub, caption = lab.sub
                    , x = str_xlab, y = str_ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)
                     , plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black")
                     , axis.ticks.y = ggplot2::element_blank())
    if(boo_plot){
      ggplot2::ggsave(fn_algscores, pAlg, width = plot_W, height = plot_H
                      , units = "in")
    }## IF ~ boo_plot ~ END

  } else {
    myALGmetrics <- NULL
  }
  if (!is.null(df_FishMetrics)) {
    # Prep Fish data for plotting
    compFISHmetrics <- df_FishMetrics %>%
      dplyr::filter(StationID %in% comp.sites)%>%
      dplyr::select(StationID, FishSampID, FishSampDate, Quality
                    , all_of(FishIndexGp))
    compFISHmetrics <- compFISHmetrics %>%
      tidyr::pivot_longer(cols = all_of(FishIndexGp), names_to = "Index"
                          , values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID==TargetSiteID
                                     , "Target", Quality)
                    , Quality = as.factor(Quality)
                    , Index = as.factor(Index))
    goodFISHmetrics <- dplyr::filter(compFISHmetrics, Quality=="Good")
    badFISHmetrics <- dplyr::filter(compFISHmetrics, Quality=="Degraded")
    myFISHmetrics <- dplyr::filter(compFISHmetrics, Quality=="Target")

    gap.good <- cbind.data.frame("getSiteInfo", "quality", nrow(goodFISHmetrics)
                                 , "Not degraded comparator samples available.")
    colnames(gap.good) <- c("fxnname", "condition", "result", "comment")
    gap.bad <- cbind.data.frame("getSiteInfo", "quality", nrow(badFISHmetrics)
                                , "Degraded comparator samples available.")
    colnames(gap.bad) <- c("fxnname", "condition", "result", "comment")
    gap.comps <- rbind(gap.good, gap.bad)
    rm(gap.good, gap.bad)

    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gap.comps, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")

    ## Plot, Variables, Strings, other Aesthetics
    lab.sub <- paste0("Comparator samples (n = ", nrow(compBMImetrics)
                      , " from ", length(comp.sites)," sites)")

    bio_col <- c("dark gray", "blue", "red") # Degraded, Good, Target
    bio_shp <- c(25, 21, 17) # down triangle, circle, and triangle
    bio_alpha <- c(0.3, 0.5, 1)

    str_title <- "Fish index scores"
    # str_sub <- paste0("Target Site: ", TargetSiteID, ", cluster ", mySiteInfo$clust)
    str_xlab  <- "Index"
    str_ylab  <- "Score"

    ## Plot, Data
    fn_fishscores <- paste0(TargetSiteID, "_Fish_IndexBoxplots.png")
    fn_fishscores <- file.path(dir_path, fn_fishscores)
    pFish <- ggplot2::ggplot(compFISHmetrics
                             , ggplot2::aes(y = round(Score, 3), x = Index
                                            , group = Index)) +
      ggplot2::geom_boxplot(na.rm = TRUE) +
      ggplot2::geom_jitter(size = 2, width = 0.2, na.rm = TRUE
                           , ggplot2::aes(color = Quality, fill = Quality
                                          , shape = Quality, alpha = Quality)) +
      ggplot2::scale_color_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_fill_manual(values = bio_col, drop = FALSE) +
      ggplot2::scale_shape_manual(values = bio_shp, drop = FALSE) +
      ggplot2::scale_alpha_manual(values = bio_alpha, drop = FALSE) +
      ggplot2::labs(title = str_title, subtitle = str_sub, caption = lab.sub
                    , x = str_xlab, y = str_ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)
                     , plot.subtitle = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black")
                     , axis.ticks.y = ggplot2::element_blank())
    if(boo_plot){
      ggplot2::ggsave(fn_fishscores, pFish, width = plot_W, height = plot_H
                      , units = "in")
    }## IF ~ boo_plot ~ END

  } else {
    myFISHmetrics <- NULL
  }

  # Check for presence of Photos in data directory. If not present, skip.
  if (dir.exists(dir_photo) == TRUE & length(list.files(dir_photo)) > 0) {
    photofiles <- list.files(dir_photo)
    have.photos <- FALSE
    for (l in 1:length(photofiles)) {
      ifelse(!dir.exists(file.path(dir_path, "Photos")) == TRUE
             , dir.create(file.path(dir_path, "Photos"))
             , FALSE)
      photoname <- photofiles[l]
      if (str_detect(photoname, all_of(TargetSiteID)) == TRUE) {
        file.copy(file.path(dir_photo, photoname)
                  , file.path(dir_path, "Photos", photoname))
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

    gap.photos <- cbind.data.frame("getSiteInfo", "photos", 0
                                   , "Site photos are not available.")
    colnames(gap.photos) <- c("fxnname", "condition", "result", "comment")

    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gap.photos, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
  }## IF ~ !have.photos ~ END

  message("Completed transferring any available site files.")

  if (!is.null(df_BkgData)) {
    # Get background data from df_BkgData; use COMID to select single reach
    data_sitebkgdata <- dplyr::filter(data_compbkgd, COMID == myCOMID)
    naVars.site <- unique(data_sitebkgdata$Metric[is.na(data_sitebkgdata$WatershedValue)])
    data_sitebkgdata <- dplyr::filter(data_sitebkgdata, !is.na(WatershedValue))
    vars.site <- unique(data_sitebkgdata$StreamCatVar[!is.na(data_sitebkgdata$WatershedValue)])

    if (length(naVars.site) > 0) { # if any NA values, then missing data for site
      # Missing one or more values in StreamCat for the target reach.
      naVars.site <- paste(naVars.site, collapse = "; ")
      gapcomment <- paste0("Missing background data for site "
                           , TargetSiteID, "on reach with COMID = ", myCOMID)
      gaps <- cbind.data.frame("getSiteInfo", "Background Data", naVars.site
                               , gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
    }

    if (length(vars.site) > 0) { # Background data exists
      fn_bkg <- paste0(TargetSiteID, "_BKGDATA.tab")
      write.table(data_sitebkgdata, file.path(dir_path, fn_bkg), append = FALSE,
                  sep = "\t", col.names = TRUE, row.names = FALSE)

      # If EPA wants to use only comparator reaches having site data:
      # data_compbkgd <- dplyr::filter(data_compbkgd, COMID %in% myCompCOMIDs)

      # Get metadata from fn_bkginfo
      data_compbkgd <- data_compbkgd %>%
        dplyr::mutate(Scaled = scale(WatershedValue, scale = TRUE, center = TRUE))
      # Note that this results in a column name "Scaled[, 1]"
      # which is a column with attributes of center and scale)

      data_compbkgd <- data_compbkgd %>%
        dplyr::mutate(PctRank = round(dplyr::percent_rank(WatershedValue)*100, 0))


      # Draw boxplots
      # TODO: Ask EPA if they want to group variables that do not have years

      # Prepare boxplot main elements
      str_title <- paste0(TargetSiteID, ": Site background")

      for (i in seq_along(vars.site)) { # StreamCatVar (no year--Metric includes year)
        print(paste0("Prepping ", vars.site[i]))
        plotvar <- vars.site[i]
        fn.bkgplot <- file.path(dir_path, paste0(TargetSiteID, "_BKGD_"
                                                 , plotvar, ".png"))
        str_sub <- unique(df_BkgInfo$Label[df_BkgInfo$StreamCatVar == plotvar])
        str_caption <- paste0("Target reach (", myCOMID, ") relative to ",
                              "distribution of values for all comparator reaches")

        # OR, If EPA wants to use only comparator reaches having site data:
        # str_caption <- paste0("Target reach (", myCOMID, ") relative to ",
        #                       "distribution of values for all comparator sites' reaches")

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
            ggplot2::geom_jitter(data = df.plot.comp, width = 0.1, height = 0
                                 , ggplot2::aes(x = Year, y = WatershedValue)
                                 , size = 0.25, na.rm = TRUE, color = "cyan4") +
            # ggplot2::scale_x_continuous(limits = c(xmin, xmax),
            #                             breaks = scales::breaks_extended(numYears)) +
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
            ggplot2::geom_point(data = dplyr::filter(df.plot.comp, COMID == myCOMID),
                                ggplot2::aes(x = Year, y = WatershedValue, group = Year),
                                color = "red", shape = 17) +
            ggplot2::geom_text(data = dplyr::filter(df.plot.comp, COMID == myCOMID),
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
            ggplot2::geom_jitter(data = df.plot.comp, width = 0.1, height = 0
                                 , ggplot2::aes(x = StreamCatVar, y = WatershedValue)
                                 , size = 0.25, na.rm = TRUE, color = "cyan4") +
            ggplot2::labs(title = str_title, subtitle = str_sub,
                          caption = str_caption) +
            ggplot2::xlab(str_sub) +
            ggplot2::ylab("Watershed Value")

          # if (plotvar == "WSAREASQKM") {
          #   p.box <- p.box +
          #     ggplot2::scale_y_log10() +
          #     ggplot2::ylab("Log10 Watershed Value")
          # } else {
          #   p.box <- p.box +
          #     ggplot2::ylab("Watershed Value")
          # }
          #
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
            ggplot2::geom_point(data = dplyr::filter(df.plot.comp, COMID == myCOMID),
                                ggplot2::aes(x = StreamCatVar, y = WatershedValue),
                                color = "red", shape = 17) +
            ggplot2::geom_text(data = dplyr::filter(df.plot.comp, COMID == myCOMID),
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
  mySiteSummary <- list(SiteInfo = mySiteInfo
                        , Samps = mySamps
                        , BMImetrics = myBMImetrics
                        , AlgMetrics = myALGmetrics
                        , FishMetrics = myFISHmetrics
                        , COMID = myCOMID
                        # , outcaseID = outcaseID
                        # , impair = myImpairments
                        # , mods = myReachMods
                        , refCOMIDs = myRefCOMIDs)
  return(mySiteSummary)

}

