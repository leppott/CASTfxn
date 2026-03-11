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
#' Requires packages dplyr, ggplot2, lubridate, tidyr
#'
#' @param TargetSiteID site identifier for the site being evaluated (the Target Site)
#' @param TargetCOMID x
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
#' @param IncaseLabel Label for the "inside the case" identifier. Default = NULL.
#' @param OutcaseLabel Label for the "outside the case" identifier. Default = NULL.
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = FALSE.
#' @param plotvars Colors, fills, shapes, transparencies for each type (target,
#' not degraded, degraded, inside-the-case, outside-the-case). Default = data_plotvars.
#' @param refSiteCol Default color outline for reference sites, used for standardization.
#' Default = refOutline_col.
#' @param plotdpi Default dpi for plots, used for standardization.
#' Default = plot_dpi. 600
#' @param plotH Default height for plots, used for standardization.
#' Default = plot_H. 6
#' @param plotW Default width for plots, used for standardization.
#' Default = plot_W. 8
#' @param plotunits Default units for plots, used for standardization.
#' Default = plot_units. "in"
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
#' # None at this time
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
                        plotvars,
                        refSiteCol,
                        plotdpi = 600,
                        plotH = 6,
                        plotW = 8,
                        plotunits = "in",
                        dir_photo = file.path(getwd(), "Data", "Photos"),
                        dir_results = file.path(getwd(), "Results"),
                        dir_sub = "SiteInfo",
                        boo_plot = TRUE) {##FUNCTION.START

  # Global Bindings
   list.CompSites <- data_Sites <- data_sampSummary <-
    data_bmiMetrics <- bmiIndexGp <- data_algMetrics <- algIndexGp <-
    data_fishMetrics <- fishIndexGp <- outcaseLabel <- incaseLabel <- in.dir <-
    region <- StationID <- Latitude <- Longitude <- RefSiteFlag <- COMID <-
    OutcaseCol <- IncaseCol <- SampleDate <- SampleType <- yLoc <-
    df_algMetrics <- df_fishMetrics <- Quality <- Index <- Samples <-
    RespSampleID <- RespSampleDate <- Score <- Case <- NULL

  # DEBUG
  boo_DEBUG <- FALSE
  #
  if (boo_DEBUG == TRUE) {
    TargetSiteID   = TargetSiteID
    TargetCOMID    = list.CompSites$TargetCOMID
    df_Sites       = data_Sites
    df_SampSummary = data_sampSummary
    biocommlist    = biocommlist
    df_BMIMetrics  = data_bmiMetrics
    BMIIndexGp     = bmiIndexGp
    df_ALGMetrics  = data_algMetrics
    ALGIndexGp     = algIndexGp
    df_FishMetrics = data_fishMetrics
    FishIndexGp    = fishIndexGp
    comp.sites     = list.CompSites$comp.sites
    all.sites      = list.CompSites$all.sites
    OutcaseLabel   = outcaseLabel
    IncaseLabel    = incaseLabel
    useBC          = FALSE
    plotvars       = plotvars
    plotdpi        = 600
    plotH          = 6
    plotW          = 8
    plotunits      = "in"
    refSiteCol     = refSiteCol
    dir_photo      = file.path(in.dir, region, "Photos")
    dir_results    = dir_results
    dir_sub        = "SiteInfo"
    boo_plot       = TRUE
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

  # Initialize gap df
  df_gap <- data.frame(fxnname = character(), condition = character(), result = character(), comment = character())

  ## Plot colors, sizes, etc  ----
  # Switched around to account for showing reference ND & D with 2 boxes for
  # inside-the-case and outside-the-case (for fill, col, & alpha)
  bio_shp <- c(plotvars$Shape[plotvars$Type == "target"],
               plotvars$Shape[plotvars$Type == "insideND"],
               plotvars$Shape[plotvars$Type == "insideD"],
               plotvars$Shape[plotvars$Type == "outsideND"],
               plotvars$Shape[plotvars$Type == "outsideD"])
  bio_fill <- c(plotvars$Fill[plotvars$Type == "target"],
                plotvars$Fill[plotvars$Type == "insideND"],
                plotvars$Fill[plotvars$Type == "outsideD"],
                plotvars$Fill[plotvars$Type == "outsideND"],
                plotvars$Fill[plotvars$Type == "insideD"])
  bio_alpha <- c(plotvars$Alpha[plotvars$Type == "target"],
                 plotvars$Alpha[plotvars$Type == "insideND"],
                 1,
                 plotvars$Alpha[plotvars$Type == "outsideND"],
                 plotvars$Alpha[plotvars$Type == "outsideD"])
  bio_size <- c(plotvars$Size[plotvars$Type == "target"]*1.8,
                plotvars$Size[plotvars$Type == "insideND"]*1.5,
                plotvars$Size[plotvars$Type == "insideD"]+0.2,
                plotvars$Size[plotvars$Type == "outsideND"],
                plotvars$Size[plotvars$Type == "outsideD"])
  bio_col <- c(plotvars$Fill[plotvars$Type == "target"],
               plotvars$Fill[plotvars$Type == "outsideND"],
               plotvars$Fill[plotvars$Type == "outsideD"],
               plotvars$Fill[plotvars$Type == "insideND"],
               plotvars$Fill[plotvars$Type == "insideD"])

  # Details about the site (Lat, Long, RefSiteFlag, COMID, OutcaseCol, IncaseCol)
  mySiteInfo <- df_Sites %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::select(Latitude, Longitude, RefSiteFlag, COMID, OutcaseCol, IncaseCol)
  myIncaseID  <- as.vector(unlist(mySiteInfo$IncaseCol))
  myOutcaseID <- as.vector(unlist(mySiteInfo$OutcaseCol))

  data_refSites <- df_Sites %>%
    dplyr::filter(RefSiteFlag == 1) %>%
    dplyr::select(StationID, Latitude, Longitude, RefSiteFlag, COMID,
                  OutcaseCol, IncaseCol)
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

  for (b in seq_along(biocommlist)) { # Only tested for BMI

    # Define biocomm data
    bioComm <- tolower(biocommlist[b])
    if (bioComm == "bmi") {
      bioIndexGp <- BMIIndexGp
      bioMetricData <- df_BMIMetrics
      bioSampleID <- "BMISampleID"
    } else if (bioComm == "alg") {
      bioIndexGp <- ALGIndexGp
      bioMetricData <- df_ALGMetrics
      bioSampleID <- "AlgSampleID"
    } else if (bioComm == "fish") {
      bioIndexGp <- FishIndexGp
      bioMetricData <- df_FishMetrics
      bioSampleID <- "FishSampleID"
    } else {
      msg <- paste0(bioComm, " is not a valid biological community.")
      message(msg)
      next()
    }

    # Prep bio data for plotting
    targetBioMetrics <- dplyr::filter(bioMetricData, StationID == TargetSiteID)

    # LCN 9/23/25 patch to remove dependence on hardcoded data_bmiCoOccur
    if(useBC == TRUE){
      allBioMetrics <- bioMetricData %>%
        dplyr::filter(!(StationID %in% comp.sites))
    } else{
      allBioMetrics <- bioMetricData %>%
        dplyr::left_join(df_Sites %>% dplyr::select(StationID, IncaseCol)) %>%
        dplyr::filter(is.na(IncaseCol) | IncaseCol != myIncaseID) %>%
        dplyr::select(-IncaseCol)
    }

    allBioMetrics <- rbind(targetBioMetrics, allBioMetrics)

    allBioMetrics <- allBioMetrics %>%
      dplyr::mutate(Quality = as.character(Quality),
                    Case = "Outside the case")
    allBioMetrics <- allBioMetrics %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(bioIndexGp),
                          names_to = "Index",
                          values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                    Quality = factor(Quality, levels = c("Target",
                                                         "Not degraded",
                                                         "Degraded")),
                    Index = factor(Index),
                    Case = "Outside the case",
                    RefSite = ifelse(StationID %in% myRefSites, "Reference", NA),
                    Samples = dplyr::case_when(Quality == "Target" ~ "Target",
                                               !is.na(RefSite) ~
                                                 paste0(RefSite, ", ", tolower(Quality)),
                                               TRUE ~ Quality),
                    Samples = factor(Samples, levels = c("Target",
                                                         "Reference, not degraded",
                                                         "Reference, degraded",
                                                         "Not degraded",
                                                         "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index,
                    Score, Case, Samples)

    # LCN 9/23/25 patch to remove dependence on hardcoded data_bmiCoOccur
    if(useBC == TRUE){
      compBioMetrics <- bioMetricData %>%
        dplyr::filter(StationID %in% comp.sites)%>%
        dplyr::select(StationID, RespSampleID, RespSampleDate, tidyselect::all_of(bioIndexGp),
                      Quality) %>%
        dplyr::mutate(Quality = as.character(Quality))
    } else{
      compBioMetrics <- bioMetricData %>%
        dplyr::left_join(df_Sites %>% dplyr::select(StationID, IncaseCol)) %>%
        dplyr::filter(IncaseCol == myIncaseID) %>%
        dplyr::select(StationID, RespSampleID, RespSampleDate, tidyselect::all_of(bioIndexGp),
                      Quality) %>%
        dplyr::mutate(Quality = as.character(Quality))
    }



    compBioMetrics <- compBioMetrics %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(bioIndexGp), names_to = "Index",
                          values_to = "Score") %>%
      dplyr::mutate(Quality = ifelse(StationID == TargetSiteID, "Target", Quality),
                    Quality = factor(Quality, levels = c("Target",
                                                         "Not degraded",
                                                         "Degraded")),
                    Index = factor(Index),
                    Case = "Inside the case",
                    RefSite = ifelse(StationID %in% myRefSites, "Reference", NA),
                    Samples = dplyr::case_when(Quality == "Target" ~ "Target",
                                               !is.na(RefSite) ~
                                                 paste0(RefSite, ", ", tolower(Quality)),
                                               TRUE ~ Quality),
                    Samples = factor(Samples, levels = c("Target",
                                                         "Reference, not degraded",
                                                         "Reference, degraded",
                                                         "Not degraded",
                                                         "Degraded"))) %>%
      dplyr::select(StationID, RespSampleID, RespSampleDate, Quality, Index,
                    Score, Case, Samples)

    # LCN 3/10/26 commented out because duplicative of the Comparator sample summary table and not strictly a gap
    # goodBioMetrics <- dplyr::filter(compBioMetrics, Quality=="Not degraded")
    # badBioMetrics <- dplyr::filter(compBioMetrics, Quality=="Degraded")
    # myBioMetrics <- dplyr::filter(compBioMetrics, Quality=="Target")
    #
    # gap.good <- cbind.data.frame("getSiteInfo", "quality", nrow(goodBioMetrics),
    #                              "Not degraded comparator samples available.")
    # colnames(gap.good) <- c("fxnname", "condition", "result", "comment")
    # gap.bad <- cbind.data.frame("getSiteInfo", "quality", nrow(badBioMetrics),
    #                             "Degraded comparator samples available.")
    # colnames(gap.bad) <- c("fxnname", "condition", "result", "comment")
    # gap.comps <- rbind(gap.good, gap.bad)
    # rm(gap.good, gap.bad, goodBioMetrics, badBioMetrics)
    #
    # # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    # fn.gaps <- paste0(TargetSiteID,"_datagaps.csv")
    # fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    # # utils::write.table(gap.comps, fn.gaps, append = TRUE, col.names = FALSE,
    # #                    row.names = FALSE, sep = "\t")
    # utils::write.table(gap.comps, fn.gaps, append = TRUE, col.names = FALSE,
    #                    row.names = FALSE, sep = ",")

    # ## Plot, Variables, Strings, other Aesthetics
    myBioSamps <- dplyr::filter(mySamps, !is.na(bioSampleID))
    lab.sub <- paste0("Comparator samples (n = ",
                      (nrow(compBioMetrics) - nrow(myBioSamps)),
                      " from ", (length(unique(compBioMetrics$StationID)) - 1), " sites)") # :CN 9/23/25 changed from length(comp.sites)



    if(bioComm == "bmi"){
      str_title <- "Benthic macroinvertebrate index scores"
    }
    if(bioComm == "alg"){
      str_title <- "Algae index scores"
    }
    if(bioComm == "fish"){
      str_title <- "Fish index scores"
    }
    str_ylab  <- "Score"

    ## Plot, BMI data by case ----
    allsamplesByCase <- rbind(compBioMetrics, allBioMetrics)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, !is.na(Score))
    targetSamples <- dplyr::filter(allsamplesByCase, StationID == TargetSiteID)
    allsamplesByCase <- dplyr::filter(allsamplesByCase, StationID != TargetSiteID)

    fn_bioscoresByCase <- paste0(TargetSiteID, "_", biocommlist[b], "_IndexBoxplotsByCase.png")
    fn_bioscoresByCase <- file.path(dir_path, fn_bioscoresByCase)
    pBiobyCase <- ggplot2::ggplot(allsamplesByCase,
                                  ggplot2::aes(y = round(Score, 3), x = Case,
                                               group = Case)) +
      ggplot2::geom_boxplot(na.rm = TRUE, staplewidth = 0.5) +
      ggplot2::geom_jitter(width = 0.2, height = 0.05, na.rm = TRUE,
                           ggplot2::aes(color = Samples, fill = Samples,
                                        shape = Samples, alpha = Samples,
                                        size = Samples))
    pBiobyCase <- pBiobyCase +
      ggplot2::geom_jitter(data = targetSamples, width = 0.2, height = 0, na.rm = TRUE,
                           ggplot2::aes(color = Samples, fill = Samples,
                                        shape = Samples, alpha = Samples,
                                        size = Samples)) +
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
                                                           size = ggplot2::rel(0.5)),
                     plot.caption = ggplot2::element_text(size = ggplot2::rel(0.6)),
                     legend.title = ggplot2::element_text(size = ggplot2::rel(0.6)),
                     legend.text = ggplot2::element_text(size = ggplot2::rel(0.5))) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black"),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank())
    if (boo_plot) {
      ggplot2::ggsave(fn_bioscoresByCase, pBiobyCase, width = plotW,
                      height = plotH, units = plotunits, dpi = plotdpi)
    }## IF ~ boo_plot_by_case ~ END

  } # End loop over biocomms

  # Site photos ----
  # Check for presence of Photos in data directory. If not present, skip.
  if (dir.exists(dir_photo) == TRUE & length(list.files(dir_photo)) > 0) {
    photofiles <- list.files(dir_photo)
    ifelse(!dir.exists(file.path(dir_path, "Photos")) == TRUE,
           dir.create(file.path(dir_path, "Photos")), FALSE)
    have.photos <- FALSE
    for (l in 1:length(photofiles)) {
      photoname <- photofiles[l]
      if (stringr::str_detect(photoname, tidyselect::all_of(TargetSiteID)) == TRUE) {
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

    gap.statement <- data.frame(
      fxnname = "getSiteInfo",
      condition = "Photos",
      result = "0",
      comment = "Site photos are not available"
    )

    df_gap <- df_gap |> dplyr::bind_rows(gap.statement)

    # gap.photos <- cbind.data.frame("getSiteInfo", "photos", 0,
    #                                "Site photos are not available.")
    # colnames(gap.photos) <- c("fxnname", "condition", "result", "comment")

    # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    # fn.gaps <- paste0(TargetSiteID,"_datagaps.csv")
    # fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    # # utils::write.table(gap.photos, fn.gaps, append = TRUE, col.names = FALSE,
    # #             row.names = FALSE, sep = "\t")
    # utils::write.table(gap.photos, fn.gaps, append = TRUE, col.names = FALSE,
    #                    row.names = FALSE, sep = ",")
  }## IF ~ !have.photos ~ END

  message("Completed transferring any available site files.")

  # only gap df return; graphics written to "SiteInfo" folder
  return(list(df_gap = df_gap))

}

