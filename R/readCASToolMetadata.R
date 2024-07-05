#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#  R version 4.3.1
#
#
#' @title Identifies Stressor Outliers
#'
#' @description Flags stressor outliers using two methods: 3 times IQR and
#'              6 times sd. If both suggest a given value as an outlier, the
#'              value is identified as an outlier.
#'
#' @details Generates faceted time sequence graphics (stressor/response, one
#' atop the other). All stressor/response data are graphed.
#' Improvements: Add scoring.
#'
#' Uses the library dplyr.
#'
#' @param filename filename to read with path
#' @param region  state or region column to read
#'
#' @return List containing metadata for the specified region
#'
#' @keywords internal
#'
#' @export
#'
readCASToolMetadata <- function(filename, region) {

  data_CASTmeta <- readxl::read_excel(filename, na = "", trim_ws = TRUE)

  data_CASTmetaDUPS <- data_CASTmeta %>%
    dplyr::select(Variable, all_of(region)) %>%
    dplyr::group_by(Variable) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L)
  if (nrow(data_CASTmetaDUPS) == 0) {
    rm(data_CASTmetaDUPS)
  } else {
    message("Duplicate variable names in CASTool metadata file.")
    msg(message)
  }

  data_CASTmeta <- data_CASTmeta %>%
    dplyr::select(Variable, all_of(region)) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = all_of(region))

  # Specify Base Filenames # These are the files used to run the analyses
  fn.targets           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.targets))
  fn.Sites.Info        <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.Sites.Info))
  # stressors
  fn.measdata          <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.measdata))
  fn.measinfo          <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.measinfo))
  fn.modeldata         <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.modeldata))
  fn.modelinfo         <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.modelinfo))
  # responses
  fn.bmi.metrics       <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.metrics))
  fn.bmi.metrics.info  <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.metrics.info))
  fn.bmi.qualifiers    <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.qualifiers))
  fn.bmi.raw           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bmi.raw))
  fn.MT.bmi            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.MT.bmi))
  fn.alg.metrics       <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.metrics))
  fn.alg.metrics.info  <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.metrics.info))
  fn.alg.raw           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.alg.raw))
  fn.MT.alg            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.MT.alg))
  fn.fish.metrics      <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.metrics))
  fn.fish.metrics.info <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.metrics.info))
  fn.fish.raw          <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.fish.raw))
  fn.MT.fish           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.MT.fish))
  # optional files
  fn.bcdist            <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bcdist))
  fn.cluster           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.cluster))
  fn.clusterinfo       <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.clusterinfo))
  fn.bkgdata           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bkgdata))
  fn.bkginfo           <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.bkginfo))
  # GIS files
  datum                <- as.character(dplyr::select(data_CASTmeta, datum))
  dsn_outline          <- file.path(dir_data, dplyr::select(data_CASTmeta, dsn_outline))
  lyr_outline          <- as.character(dplyr::select(data_CASTmeta, lyr_outline))
  dsn_flowline         <- file.path(dir_data, dplyr::select(data_CASTmeta, dsn_flowline))
  lyr_flowline         <- as.character(dplyr::select(data_CASTmeta, lyr_flowline))

  # Required user-designated options
  refColName     <- as.character(dplyr::select(data_CASTmeta, refColName))
  outcaseColName <- as.character(dplyr::select(data_CASTmeta, outcaseColName))
  # outcaseLabel   <- as.character(dplyr::select(data_CASTmeta, outcaseLabel))
  incaseColName  <- as.character(dplyr::select(data_CASTmeta, incaseColName))
  # incaseLabel    <- as.character(dplyr::select(data_CASTmeta, incaseLabel))
  removeOutliers       <- as.logical(dplyr::select(data_CASTmeta, removeOutliers))
  useBC                <- as.logical(dplyr::select(data_CASTmeta, useBC))
  samplim              <- as.integer(dplyr::select(data_CASTmeta, samplim))
  probsHigh            <- as.numeric(dplyr::select(data_CASTmeta, probsHigh))
  probsLow             <- as.numeric(dplyr::select(data_CASTmeta, probsLow))
  DOlim                <- as.numeric(dplyr::select(data_CASTmeta, DOlim))
  pHlimLow             <- as.numeric(dplyr::select(data_CASTmeta, pHlimLow))
  pHlimHigh            <- as.numeric(dplyr::select(data_CASTmeta, pHlimHigh))
  lagdays              <- as.integer(unlist(stringr::str_split(dplyr::select(data_CASTmeta, lagdays), ", ")))
  biocommlist          <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, biocommlist), ", "))
  siteQual2Plot        <- as.character(dplyr::select(data_CASTmeta, siteQual2Plot))
  printClusterInfo     <- as.logical(dplyr::select(data_CASTmeta, printClusterInfo))
  printBkgdInfo        <- as.logical(dplyr::select(data_CASTmeta, printBkgdInfo))
  # report_format <- as.character(data_CASTmeta["report_format", region])

  ## Check for required files ####
  if (basename(fn.Sites.Info) == "NA") {
    missingFiles <- "Sites filename is missing"
  }
  if (basename(fn.measdata) == "NA" & basename(fn.modeldata) == "NA") {
    if (exists("missingFiles")) {
      missingFiles <- c(missingFiles, "Stressor data filename is missing")
    } else {
      missingFiles <- "Stressor data filename is missing"
    }
  }
  if (basename(fn.measinfo) == "NA" & basename(fn.modelinfo) == "NA") {
    if (exists("missingFiles")) {
      missingFiles <- c(missingFiles, "Stressor metadata filename is missing")
    } else {
      missingFiles <- "Stressor metadata filename is missing"
    }
  }
  if (basename(fn.bmi.metrics) == "NA" & basename(fn.alg.metrics) == "NA" &
      basename(fn.fish.metrics) == "NA") {
    if (exists("missingFiles")) {
      missingFiles <- c(missingFiles, "Response data filename is missing")
    } else {
      missingFiles <- "Response data filename is missing"
    }
  }
  if (basename(fn.bmi.metrics.info) == "NA" & basename(fn.alg.metrics.info) == "NA" &
      basename(fn.fish.metrics.info) == "NA") {
    if (exists("missingFiles")) {
      missingFiles <- c(missingFiles, "Response metadata filename(s) is(are) missing")
    } else {
      missingFiles <- "Response metadata filename(s) is(are) missing"
    }
  }

  if (exists("missingFiles")) {
    message(missingFiles)
    stop()
  }

  CASTmeta <- list(fn.targets = fn.targets
                   , fn.Sites.Info = fn.Sites.Info
                   , refColName = refColName
                   , outcaseColName = outcaseColName
                   # outcaseLabel   <- as.character(dplyr::select(data_CASTmeta, outcaseLabel))
                   , incaseColName = incaseColName
                   # incaseLabel    <- as.character(dplyr::select(data_CASTmeta, incaseLabel))
                   # stressors
                   , fn.measdata = fn.measdata
                   , fn.measinfo = fn.measinfo
                   , fn.modeldata = fn.modeldata
                   , fn.modelinfo = fn.modelinfo
                   # responses
                   , fn.bmi.metrics = fn.bmi.metrics
                   , fn.bmi.metrics.info = fn.bmi.metrics.info
                   , fn.bmi.qualifiers = fn.bmi.qualifiers
                   , fn.bmi.raw = fn.bmi.raw
                   , fn.MT.bmi = fn.MT.bmi
                   , fn.alg.metrics = fn.alg.metrics
                   , fn.alg.metrics.info = fn.alg.metrics.info
                   , fn.alg.raw = fn.alg.raw
                   , fn.MT.alg = fn.MT.alg
                   , fn.fish.metrics = fn.fish.metrics
                   , fn.fish.metrics.info = fn.fish.metrics.info
                   , fn.fish.raw = fn.fish.raw
                   , fn.MT.fish = fn.MT.fish
                   # optional files
                   , fn.bcdist = fn.bcdist
                   , fn.cluster = fn.cluster
                   , fn.clusterinfo = fn.clusterinfo
                   , fn.bkgdata = fn.bkgdata
                   , fn.bkginfo = fn.bkginfo
                   # GIS files
                   , datum = datum
                   , dsn_outline = dsn_outline
                   , lyr_outline = lyr_outline
                   , dsn_flowline = dsn_flowline
                   , lyr_flowline = lyr_flowline
                   # Required user-designated options
                   , removeOutliers = removeOutliers
                   , useBC = useBC
                   , samplim = samplim
                   , probsHigh = probsHigh
                   , probsLow = probsLow
                   , DOlim = DOlim
                   , pHlimLow = pHlimLow
                   , pHlimHigh = pHlimHigh
                   , lagdays = lagdays
                   , biocommlist = biocommlist
                   , siteQual2Plot = siteQual2Plot
                   , printClusterInfo = printClusterInfo
                   , printBkgdInfo = printBkgdInfo)


  # Bio responses
  for (b in seq_along(biocommlist)) {
    bio <- tolower(biocommlist[b])
    if (bio == "bmi") {
      bmi_thresholds <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_thresholds)
                                                             , ", ")))
      bmi_narrative  <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_narrative), ", "))
      bmi_deg_thres  <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_deg_thres)
                                                             , ", ")))
      bmi_deg_text   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmi_deg_text), ", "))
      bmiIndexGp     <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmiIndexGp), ", "))
      # bmiResp        <- as.character(dplyr::select(data_CASTmeta, bmiResp))
      # bmiRespDate    <- as.character(dplyr::select(data_CASTmeta, bmiRespDate))
      bmiModParams   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmiModParams), ", "))
      bmiSuffInds    <- as.numeric(dplyr::select(data_CASTmeta, bmiSuffInds))
      bmiPctAmbInds  <- as.numeric(dplyr::select(data_CASTmeta, bmiPctAmbInds))
      bmiCounts      <- as.character(dplyr::select(data_CASTmeta, bmiCounts))
      bmiTaxon       <- as.character(dplyr::select(data_CASTmeta, bmiTaxon))
      CASTmeta       <- append(CASTmeta, list(bmi_thresholds = bmi_thresholds
                                              , bmi_narrative = bmi_narrative
                                              , bmi_deg_thres = bmi_deg_thres
                                              , bmi_deg_text = bmi_deg_text
                                              , bmiIndexGp = bmiIndexGp
                                              , bmiModParams = bmiModParams
                                              , bmiSuffInds = bmiSuffInds
                                              , bmiPctAmbInds = bmiPctAmbInds
                                              , bmiCounts = bmiCounts
                                              , bmiTaxon = bmiTaxon))
    }
    if (bio == "algae") {
      alg_thresholds <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_thresholds)
                                                             , ", ")))
      alg_narrative  <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_narrative), ", "))
      alg_deg_thres  <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_deg_thres)
                                                             , ", ")))
      alg_deg_text   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, alg_deg_text), ", "))
      algIndexGp     <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, algIndexGp), ", "))
      # algResp        <- as.character(dplyr::select(data_CASTmeta, algResp))
      # algRespDate    <- as.character(dplyr::select(data_CASTmeta, algRespDate))
      algModParams   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, algModParams), ", "))
      algCounts      <- as.character(dplyr::select(data_CASTmeta, algCounts))
      algTaxon       <- as.character(dplyr::select(data_CASTmeta, algTaxon))
      CASTmeta       <- append(CASTmeta, list(alg_thresholds = alg_thresholds
                                              , alg_narrative = alg_narrative
                                              , alg_deg_thres = alg_deg_thres
                                              , alg_deg_text = alg_deg_text
                                              , algIndexGp = algIndexGp
                                              , algModParams = algModParams
                                              , algCounts = algCounts
                                              , algTaxon = algTaxon))
    }
    if (bio == "fish") {
      fish_thresholds <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_thresholds)
                                                              , ", ")))
      fish_narrative  <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_narrative), ", "))
      fish_deg_thres  <- as.numeric(unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_deg_thres)
                                                              , ", ")))
      fish_deg_text   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fish_deg_text), ", "))
      fishIndexGp     <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fishIndexGp), ", "))
      # fishResp        <- as.character(dplyr::select(data_CASTmeta, fishResp))
      # fishRespDate    <- as.character(dplyr::select(data_CASTmeta, fishRespDate))
      fishModParams   <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fishModParams), ", "))
      fishCounts      <- as.character(dplyr::select(data_CASTmeta, fishCounts))
      fishTaxon       <- as.character(dplyr::select(data_CASTmeta, fishTaxon))
      CASTmeta       <- append(CASTmeta, list(fish_thresholds = fish_thresholds
                                              , fish_narrative = fish_narrative
                                              , fish_deg_thres = fish_deg_thres
                                              , fish_deg_text = fish_deg_text
                                              , fishIndexGp = fishIndexGp
                                              , fishModParams = fishModParams
                                              , fishCounts = fishCounts
                                              , fishTaxon = fishTaxon))
    }
  }
  rm(b, bio, data_CASTmeta)

  return(CASTmeta)

}
