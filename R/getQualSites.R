#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Get Quality Sites
#'
#' @description Get quality sites where quality is defined in any of three ways:
#' reference, not degraded, or having better biology than the worst target site sample.
#'
#' @details Generates a vector for each measure of quality for sites and samples
#' in the entire data set and separately in the comparator data set, and also
#' vectors for the samples having better biology in the entire data set and
#' separately in the comparator data set.
#' Improvements: Add data gap analysis. In particular, how many samples are
#' better than the min target sample score, and is that enough?
#'
#' Uses the library dplyr.
#'
#' @param TargetSiteID Site ID
#' @param df_sites Sites table containing site ids and reference flags.
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param df_qual Biological index data for the specified biocomm.
#' @param colBio Name of the column for the biological response index measure.
#' @param colBioSample Column name for biological sample.
#' @param ColStressSample Column name for Stress sample ID.
#' @param compSites Vector containing "inside the case" site identifiers.
#' @param allSites Vector containing "outside the case" site identifiers.
#' @param useBC Boolean for using Bray Curtis model. Default = FALSE
#' @param outcaseColName Column name for "outside the case" values.
#' @param outcaseID value used to identify "outside the case" sites.
#' @param BioNarBrk Breaks for cut function for biological index. CA CSCI Default is
#' c(-2, 0.62, 0.799, 0.919, 2).
#' @param BioNarLab Labels for biological index. CA CSCI Default is
#' c("very likely altered", "likely altered", "possibly altered", "likely intact").
#' @param BioDegBrk Biological assessment degraded status, cut function breaks.
#' Should be in order from bad (low) to good (high). CA CSCI Default is
#' c(-2, 0.799, 2).
#' @param BioDegLab Biological assessment degraded status, cut function labels.
#' Should be in order from bad (low) to good (high).
#' Defaults are referenced in the code so if change the code will break.
#' Default = c("Yes", "No").
#' @param dir_results directory for results; Default = file.path(getwd(), "Results").
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo".
#'
#' @return A list with vectors containing all reference sites, response samples,
#'         and stressor samples; all not degraded sites, response samples, and
#'         stressor samples; and all "better than" target site quality sites,
#'         response samples; and stressor samples. Writes a table indicating
#'         numbers of reference, not degraded, or better than target samples in
#'         the comparator sample set (inside the case) and the all sample set
#'         (outside the case).
#'
#' @keywords internal
#'
#' @export
getQualSites <- function(TargetSiteID,
                         biocomm,
                         df_qual,
                         colBio,
                         compSites,
                         allSites,
                         refSites,
                         stressors,
                         dir_results = file.path(getwd(), "Results"),
                         dir_sub = "SiteInfo") {##FUNCTION.START

  # For QC purposes
  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    TargetSiteID = TargetSiteID
    biocomm = bioComm
    df_qual = data_bioCoOccur
    colBio = bioIndex
    compSites = list.CompSites$comp.sites
    allSites = list.CompSites$all.sites
    refSites = refSites
    stressors = stressors
    dir_results = dir_results
    dir_sub = "SiteInfo"
  }
  #
  # Define pipe
  `%>%` <- dplyr::`%>%`

  # Declare name of column to hold biodegradation flag value
  biocomm <- tolower(biocomm)

  # Add reference site, inside case, and outside case flags to samples
  df_qual <- df_qual %>%
    dplyr::mutate(RefSiteFlag = ifelse(StationID %in% refSites, 1, 0),
                  IncaseYN = ifelse(StationID %in% compSites, 1, 0),
                  OutcaseYN = ifelse(StationID %in% allSites, 1, 0))

  # Get bio samples and chem sample where bio is better than target
  # First get max(degraded) index value or, if site isn't degraded, min index value
  df_targqual <- as.character(df_qual$Quality[df_qual$StationID == TargetSiteID])
  if ("Degraded" %in% df_targqual) {
    qual.targ <- max(df_qual[, colBio][df_qual$StationID == TargetSiteID & df_qual$Quality == "Degraded"])
  } else {
    qual.targ <- min(df_qual[, colBio][df_qual$StationID == TargetSiteID])
  }

  df_qual[, "BetterThan"] <- ifelse(df_qual[, colBio] >= qual.targ, 1, 0)

  df_qual <- dplyr::select(df_qual, StationID, IncaseCol, OutcaseCol, StressSampleDate,
                           RespSampleDate, StressSampleID, RespSampleID, BioComm,
                           RefSiteFlag, IncaseYN, OutcaseYN, BetterThan, all_of(colBio),
                           Quality, all_of(stressors))

  # Get vector of all "reference" sites in data_sites
  # all.ref <- df_sites %>%
  #   dplyr::filter(RefSiteFlag == 1) %>%
  #   dplyr::select(StationID)
  # all.ref <- unlist(all.ref)

  # Get vector of "reference" reaches in data_sites
  # all.ref.reaches <- df_sites %>%
  #   dplyr::filter(RefSiteFlag == 1) %>%
  #   dplyr::select(COMID)
  # all.ref.reaches <- unlist(all.ref.reaches)

  # Get vector of "reference" samples in data_bioCoOccur
  # all.ref.samps.bio <- df_qual %>%
  #   dplyr::filter(StationID %in% all.ref) %>%
  #   dplyr::select(RespSampleID)
  # all.ref.samps.bio <- unlist(all.ref.samps.bio)
  # all.ref.samps.stress <- df_qual %>%
  #   dplyr::filter(StationID %in% all.ref) %>%
  #   dplyr::select(StressSampleID)
  # all.ref.samps.stress <- unlist(all.ref.samps.stress)

  # Get vector of "not degraded" samples in data_bioCoOccur
  # all.good <- df_qual %>%
  #   dplyr::filter(Quality == "Not degraded") %>%
  #   dplyr::select(StationID)
  # all.good <- unlist(all.good)
  # all.good.samps.bio <- df_qual %>%
  #   dplyr::filter(StationID %in% all.good) %>%
  #   dplyr::select(RespSampleID)
  # all.good.samps.bio <- unlist(all.good.samps.bio)
  # all.good.samps.stress <- df_qual %>%
  #   dplyr::filter(StationID %in% all.good) %>%
  #   dplyr::select(StressSampleID)
  # all.good.samps.stress <- unlist(all.good.samps.stress)

  # Get vector of "not degraded" reaches in data_sites
  # all.good.reaches <- df_sites %>%
  #   dplyr::filter(StationID %in% all.good) %>%
  #   dplyr::select(COMID)
  # all.good.reaches <- unlist(all.good.reaches)

  # Mark response samples with "better than" biology
  # all.better <- as.vector(unique(df_qual$StationID[df_qual[, colBio] > qual.targ]))
  # all.better.samps.bio <- as.vector(unique(df_qual[, "RespSampleID"][df_qual[, colBio] > qual.targ]))
  # all.better.samps.stress <- as.vector(unique(df_qual[, "StressSampleID"][df_qual[, colBio] > qual.targ]))
  # all.better.reaches <- as.vector(df_sites$COMID[df_sites$StationID %in% all.better])
  #
  # all.ref <- all.ref[!is.na(all.ref)]
  # all.ref.samps.bio <- all.ref.samps.bio[!is.na(all.ref.samps.bio)]
  # all.ref.samps.stress <- all.ref.samps.stress[!is.na(all.ref.samps.stress)]
  # all.ref.reaches <- all.ref.reaches[!is.na(all.ref.reaches)]
  # all.good <- all.good[!is.na(all.good)]
  # all.good.samps.bio <- all.good.samps.bio[!is.na(all.good.samps.bio)]
  # all.good.samps.stress <- all.good.samps.stress[!is.na(all.good.samps.stress)]
  # all.good.reaches <- all.good.reaches[!is.na(all.good.reaches)]
  # all.better <- all.better[!is.na(all.better)]
  # all.better.samps.bio <- all.better.samps.bio[!is.na(all.better.samps.bio)]
  # all.better.samps.stress <- all.better.samps.stress[!is.na(all.better.samps.stress)]
  # all.better.reaches <- all.better.reaches[!is.na(all.better.reaches)]

  # Get matrix all samples, quality vs. Inside the case (Comparator)/Outside the case/Total
  # better than samples, quality vs. Inside the case (Comparator)/Outside the case/Total

  df_qualstats <- df_qual %>%
    dplyr::mutate(IncaseSamples = ifelse(IncaseYN == 1, 1, 0),
                  IncaseGood = ifelse((IncaseYN == 1) & (Quality == "Not degraded"), 1, 0),
                  IncaseBad = ifelse((IncaseYN == 1) & (Quality == "Degraded"), 1, 0),
                  IncaseBT = ifelse((IncaseYN == 1) & (BetterThan == 1), 1, 0),
                  IncaseBTGood = ifelse((IncaseBT == 1) & (Quality == "Not degraded"), 1, 0),
                  IncaseBTBad = ifelse((IncaseBT == 1) & (Quality == "Degraded"), 1, 0),
                  OutcaseSamples = ifelse((OutcaseYN == 1), 1, 0),
                  OutcaseGood = ifelse((OutcaseYN == 1) & (Quality == "Not degraded"), 1, 0),
                  OutcaseBad = ifelse((OutcaseYN == 1) & (Quality == "Degraded"), 1, 0),
                  OutcaseBT = ifelse((OutcaseYN == 1) & (BetterThan == 1), 1, 0),
                  OutcaseBTGood = ifelse((OutcaseBT == 1) & (Quality == "Not degraded"), 1, 0),
                  OutcaseBTBad = ifelse((OutcaseBT == 1) & (Quality == "Degraded"), 1, 0),
                  AllSamples = 1,
                  AllSamplesGood = ifelse(Quality == "Not degraded", 1, 0),
                  AllSamplesBad = ifelse(Quality == "Degraded", 1, 0),
                  AllSamplesBT = ifelse(BetterThan == 1, 1, 0),
                  AllSamplesBTGood = ifelse((AllSamplesBT == 1) & (Quality == "Not degraded"), 1, 0),
                  AllSamplesBTBad = ifelse((AllSamplesBT == 1) & (Quality == "Degraded"), 1, 0),
                  RefSamples = ifelse(RefSiteFlag == 1, 1, 0),
                  RefSamplesGood = ifelse((RefSiteFlag == 1) & (Quality == "Not degraded"), 1, 0),
                  RefSamplesBad = ifelse((RefSiteFlag == 1) & (Quality == "Degraded"), 1, 0),
                  RefSamplesBT = ifelse((RefSiteFlag == 1) & (BetterThan == 1), 1, 0),
                  RefSamplesBTGood = ifelse((RefSamplesBT == 1) & (Quality == "Not degraded"), 1, 0),
                  RefSamplesBTBad = ifelse((RefSamplesBT == 1) & (Quality == "Degraded"), 1, 0)) %>%
    dplyr::select(IncaseSamples, IncaseGood, IncaseBad, IncaseBT, IncaseBTGood, IncaseBTBad,
                  OutcaseSamples, OutcaseGood, OutcaseBad, OutcaseBT, OutcaseBTGood, OutcaseBTBad,
                  AllSamples, AllSamplesGood, AllSamplesBad, AllSamplesBT, AllSamplesBTGood, AllSamplesBTBad,
                  RefSamples, RefSamplesGood, RefSamplesBad, RefSamplesBT, RefSamplesBTGood, RefSamplesBTBad)

  df_qualstats <- df_qualstats %>%
    dplyr::summarise(across(where(is.numeric), sum)) %>%
    t()
  df_qualstats <- as.data.frame(df_qualstats) %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(Label = rowname, Count = V1)
  df_qualstats <- df_qualstats %>%
    dplyr::mutate(Quality = dplyr::case_when(stringr::str_detect(Label, "Good") ~ "Not degraded",
                                             stringr::str_detect(Label, "Bad") ~ "Degraded",
                                             TRUE ~ "All qualities"),
                  Group = ifelse(stringr::str_detect(Label, "BT"), "Better than", "All"),
                  Samples = dplyr::case_when(stringr::str_detect(Label, "Ref") ~ "ReferenceSamples",
                                             stringr::str_detect(Label, "Incase") ~ "InsideCaseSamples",
                                             stringr::str_detect(Label, "Outcase") ~ "OutsideCaseSamples",
                                             TRUE ~ "AllSamples"),
                  BioComm = toupper(biocomm))

  df_qualstats <- df_qualstats %>%
    dplyr::select(-Label) %>%
    dplyr::group_by(BioComm, Group, Quality) %>%
    tidyr::pivot_wider(names_from = "Samples", values_from = "Count") %>%
    dplyr::select(BioComm, Group, Quality, InsideCaseSamples, OutsideCaseSamples,
                  ReferenceSamples, AllSamples) %>%
    dplyr::arrange(Group, Quality)

  dirSiteInfo <- file.path(dir_results, TargetSiteID, dir_sub)
  fnQualStats <- paste0(TargetSiteID, "_", toupper(biocomm), "_SiteQualities.tab")
  write.table(df_qualstats, file.path(dirSiteInfo, fnQualStats)
              , append = FALSE, col.names = TRUE, row.names = FALSE
              , sep = "\t")

  numcompsfinal <- as.numeric(df_qualstats[1, "InsideCaseSamples"])
  if (numcompsfinal < length(compSites)) {
    gapcomment <- paste0("Inside case sites do not have paired "
                         , "stressor-response data for comparison.")
    gaps <- cbind.data.frame("getQualSites", "Number of inside case samples"
                             , length(compSites) - numcompsfinal
                             , gapcomment)
    colnames(gaps) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
  }

  return(df_qual)

}
