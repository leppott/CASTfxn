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
#' @param data_Sites dataframe containing site data, including "inside the case"
#'                   and "outside the case" identifiers. If useBC == TRUE, "outside
#'                   the case" will be cluster; if useBC == FALSE, "inside the case"
#'                   will be cluster.
#' @param data_bkgdata dataframe containing anthropogenically-influenced variables from StreamCat
#' @param data_bkginfo dataframe containing metadata for the variables in data_bkgdata
#' @param data_SampSummary dataframe containing sample IDs for samples collected
#'                         at the target site, organized by sample date (rows)
#'                         and type (columns)
#' @param data_bmiMetrics dataframe containing BMI sample index and metric values
#' @param bmiIndexGp vector containing one or more BMI indices for display purpose only
#' @param data_algMetrics dataframe containing algae sample index and metric values.
#'                        Default is NULL.
#' @param algIndexGp vector containing one or more algal indices for display purpose only
#' @param data_fishMetrics dataframe containing fish sample index and metric values
#' @param fishIndexGp vector containing one or more fish indices for display purpose only
#' @param comp_sites vector containing comparator site IDs
#' @param outcaseLabel Label for the "outside the case" identifier. Default = NULL.
#' @param incaseLabel Label for the "inside the case" identifier. Default = NULL.
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
                        , data_Sites
                        , data_bkgdata
                        , data_bkginfo
                        , data_SampSummary
                        , data_bmiMetrics = NULL
                        , bmiIndexGp
                        , data_algMetrics = NULL
                        , algIndexGp
                        , data_fishMetrics = NULL
                        , fishIndexGp
                        , comp_sites
                        , all_sites
                        , outcaseLabel = NULL
                        , incaseLabel = NULL
                        , useBC = FALSE
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
    data_Sites = data_Sites
    data_bkgdata = NULL
    data_bkginfo = NULL
    data_SampSummary = data_sampSummary
    data_bmiMetrics = data_bmiMetrics
    bmiIndexGp = bmiIndexGp
    data_algMetrics = data_algMetrics
    algIndexGp = algIndexGp
    data_fishMetrics = data_fishMetrics
    fishIndexGp = fishIndexGp
    comp_sites = comp_sites
    all_sites = all_sites
    outcaseLabel = outcaseLabel
    incaseLabel = incaseLabel
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
    mySiteInfo <- data_Sites %>%
      dplyr::filter(StationID == TargetSiteID) %>%
      dplyr::select(FinalLatitude, FinalLongitude, WaterbodyName
                    , RefSiteFlag, COMID, OutcaseCol)
    outcaseID <- mySiteInfo$OutcaseCol
  } else { # useBC == FALSE; cluster ID is inside the case ID
    mySiteInfo <- data_Sites %>%
      dplyr::filter(StationID == TargetSiteID) %>%
      dplyr::select(FinalLatitude, FinalLongitude, WaterbodyName
                    , RefSiteFlag, COMID, OutcaseCol, IncaseCol)
    incaseID <- mySiteInfo$IncaseCol
  }
  myCOMID <- mySiteInfo$COMID
  data_refSites <- data_Sites %>%
    dplyr::filter(RefSiteFlag == 1) %>%
    dplyr::select(StationID, FinalLatitude, FinalLongitude, COMID)
  myRefCOMIDs <- as.vector(unique(data_refSites$COMID))

  # get sampling info (dates of samples)
  mySamps <- dplyr::filter(data_SampSummary, StationID == TargetSiteID)

  # get response information (CSCI, MMIhybrid, FIBI, etc.)
  if (useBC == TRUE) {
    str_sub <- paste0("Target Site: ", TargetSiteID, ", ", outcaseLabel, " "
                      , outcaseID)
  } else {
    str_sub <- paste0("Target Site: ", TargetSiteID, "; Outside the case: "
                      , outcaseLabel, "; Inside the case: ", incaseLabel
                      , " ", incaseID)

  }

  if (!is.null(data_bmiMetrics)) {
    # Prep BMI data for plotting
    OutSamples <- data_bmiMetrics %>%
      dplyr::mutate(Quality = as.character(Quality)
                    , Case = "Outside the case")
    OutSamples <- OutSamples %>%
      tidyr::pivot_longer(cols = all_of(bmiIndexGp), names_to = "Index"
                          , values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID
                                     , "Target", Quality)
                    , Quality = factor(Quality, levels = c("Target"
                                                           , "Not degraded"
                                                           , "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index
                    , Score, Case)

    compBMImetrics <- data_bmiMetrics %>%
      dplyr::filter(StationID %in% comp_sites)%>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, all_of(bmiIndexGp)
                    , Quality) %>%
      dplyr::mutate(Quality = as.character(Quality))
    compBMImetrics <- compBMImetrics %>%
      tidyr::pivot_longer(cols = all_of(bmiIndexGp), names_to = "Index"
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
                      , " from ", (length(comp_sites) - 1), " sites)")

    bio_col <- c("red", "blue", "gray25") # Degraded, Good, Target
    bio_shp <- c(17, 21, 25) # down triangle, circle, and triangle
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


if (!is.null(data_algMetrics)) {
    # Prep Alg data for plotting
    compALGmetrics <- data_algMetrics %>%
      dplyr::filter(StationID %in% comp_sites)%>%
      dplyr::select(StationID, AlgSampID, AlgSampDate, Quality
                    , all_of(algIndexGp))
    compALGmetrics <- compALGmetrics %>%
      tidyr::pivot_longer(cols = all_of(algIndexGp), names_to = "Index"
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
                      , " from ", (length(comp_sites) - 1), " sites)")

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
      # ggplot2::geom_jitter(data=filter(compBMImetrics, Quality=="Target")
      #                      , size=2, width=0.2
      #                      , ggplot2::aes(y=Score, x=Index, group=Index
      #                                     , color = "red", fill = "red"
      #                                     , shape = 17))
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
  if (!is.null(data_fishMetrics)) {
    # Prep Fish data for plotting
    compFISHmetrics <- data_fishMetrics %>%
      dplyr::filter(StationID %in% comp_sites)%>%
      dplyr::select(StationID, FishSampID, FishSampDate, Quality
                    , all_of(fishIndexGp))
    compFISHmetrics <- compFISHmetrics %>%
      tidyr::pivot_longer(cols = all_of(fishIndexGp), names_to = "Index"
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
                      , " from ", length(comp_sites)," sites)")

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

  # get COMID
  # myCOMID <- mySiteInfo$COMID
  # myWBName <- mySiteInfo$WaterbodyName
  # myClustID <- as.integer(data_cluster$clust[data_cluster$COMID==myCOMID])
  #
  # if (exists("data_mods")) {
  #   myReachMods <- data_mods[data_mods[,"COMID"]==myCOMID
  #                            ,c("ReachModStatus", "ModReason")]
  # }
  # if (exists("data_303d")) {
  #   my303d.COMID <- subset(data_303d, data_303d$ComID == myCOMID)
  #   my303d.COMID.WBName <- subset(my303d.COMID, my303d.COMID$WATER.BODY.NAME %in% myWBName)
  #   myCurrent303d <- subset(my303d.COMID.WBName, my303d.COMID.WBName$Year == 2012)
  #   myImpairments <- myCurrent303d[,c("ComID", "WATER.BODY.NAME", "POLLUTANT",
  #                                     "FINAL.LISTING.DECISION")]
  # }

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

  if (!is.null(data_bkgdata)) {
    # Get background data from fn_bkgdata; use COMID to select single row
    data_bkgdata <- dplyr::filter(data_bkgdata, COMID == myCOMID)

    # Check for data to plot
    data_bkgcheck <- dplyr::select_if(data_bkgdata, not_all_na)

    if (ncol(data_bkgcheck) <= 1) { # If only COMID column, then no data
      # NO data in streamcat for the reach.
      gapcomment <- paste0("No background data are available for site "
                           , TargetSiteID, "on reach with COMID = ", myCOMID)
      gaps <- cbind.data.frame("getSiteInfo", "Background Data", 0
                               , gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
    } else {  # Background data exists
      data_bkgdata2 <- tidyr::pivot_longer(data_bkgdata, -COMID
                                           , names_to = "ColName"
                                           , values_to = "val")
      data_bkgdata2 <- dplyr::select(data_bkgdata2, -COMID)

      fn_bkg <- paste0(TargetSiteID,"_BKGDATA.tab")
      write.table(data_bkgdata, file.path(dir_path,fn_bkg), append = FALSE
                  , sep = "\t", col.names = TRUE, row.names = FALSE)

      # Get metadata from fn_bkginfo
      df.bkg2plot <- merge(data_bkginfo, data_bkgdata2, by = "ColName")

      rm(data_bkgdata, data_bkgdata2, data_bkginfo)

      # Determine appropriate graphics
      # Bar charts, faceted with catchment on left, watershed on right
      cat.sub <- unique(df.bkg2plot[, c("Category", "Subcategory", "Units", "AbbrFN")])

      for (i in 1:nrow(cat.sub)) { # Plot each subcategory
        # pull out temp data set to plot
        df.temp <- df.bkg2plot %>%
          dplyr::filter(Category == cat.sub$Category[i]
                        , Subcategory == cat.sub$Subcategory[i])
        maxYear <- max(df.temp$StudyYear)

        xlab <- paste0(cat.sub$Category[i], ": ", cat.sub$Subcategory[i]
                       , ", ", cat.sub$Units[i])
        fn.plot <- file.path(dir_path, paste0(TargetSiteID, "_BKGD_"
                                              , cat.sub[i, 4], ".png"))
        p.title <- paste0(TargetSiteID, ": Site background")
        p.subtitle <- "Potential anthropogenic alterations"
        numcols <- length(unique(df.temp$Scale))/2

        message(xlab)

        if (is.na(maxYear)) {  # No study year
          p.bkg <- ggplot2::ggplot(df.temp, ggplot2::aes(x = ShortName
                                                         , y = signif(val, digits = 2))) +
            ggplot2::geom_bar(stat = "identity", width = 0.5, fill = "darkred") +
            ggplot2::geom_text(ggplot2::aes(label = signif(val, digits = 2)
                                            , vjust = -0.2)
                               , color = "black", size=3) +
            ggplot2::ylim(0, max(df.temp$val) * 1.2) +
            ggplot2::facet_wrap(Scale ~ .)
          p.bkg <- p.bkg + ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "none") +
            ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)
                           , strip.text.y = ggplot2::element_text(size = 8)) +
            ggplot2::labs(title = p.title, subtitle = p.subtitle
                          , x = xlab, y = "Value")
          p.bkg <- p.bkg +
            ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8
                                                               , angle = 45
                                                               , hjust = 1)
                           , axis.text.y = ggplot2::element_text(size = 7)
                           , axis.title.x = ggplot2::element_text(size = 9
                                                                  , face = "bold")
                           , axis.title.y = ggplot2::element_text(size = 9
                                                                  , face = "bold")
                           , plot.title = ggplot2::element_text(size = 12
                                                                , face = "bold")
                           , plot.subtitle = ggplot2::element_text(size = 10
                                                                   , face = "bold"))
          if(boo_plot){
            ggplot2::ggsave(fn.plot, p.bkg, dpi = ppi, width = plot_W * 1.5
                            , height = plot_H * 1.5)
          }## IF ~ boo_plot ~ END

        } else {  # Separate study year to consider in faceting

          p.bkg <- ggplot2::ggplot(df.temp, ggplot2::aes(x = ShortName
                                                         , y = signif(val, digits = 2)
                                                         , group = StudyYear)) +
            ggplot2::geom_bar(position="dodge", stat = "identity", width = 0.5
                              , fill = "darkred") +
            ggplot2::geom_text(ggplot2::aes(label = signif(val, digits = 2)
                                            , vjust = -0.2)
                               , color = "black", size = 3) +
            ggplot2::ylim(0, max(df.temp$val) * 1.2) +
            ggplot2::facet_grid(stringr::str_wrap(Scale, 10) ~ StudyYear
                                , margins = FALSE)
          p.bkg <- p.bkg + ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "none") +
            ggplot2::theme(strip.text.x = ggplot2::element_text(size = 9)
                           , strip.text.y = ggplot2::element_text(size = 8)) +
            ggplot2::labs(title = p.title, subtitle = p.subtitle
                          , x = xlab, y = "Value")
          p.bkg <- p.bkg +
            ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8
                                                               , angle = 45, hjust = 1)
                           , axis.text.y = ggplot2::element_text(size = 7)
                           , axis.title.x = ggplot2::element_text(size = 9, face = "bold")
                           , axis.title.y = ggplot2::element_text(size = 9, face = "bold")
                           , plot.title = ggplot2::element_text(size = 12, face = "bold")
                           , plot.subtitle = ggplot2::element_text(size = 10, face = "bold"))
          if(boo_plot){
            ggplot2::ggsave(fn.plot, p.bkg, dpi = ppi, width = plot_W * 1.5
                            , height = plot_H * 1.5)
          }## IF ~ boo_plot ~ END

        }  # End creating background plot

      }  # End iteration over subcategories

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

