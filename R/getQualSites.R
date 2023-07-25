#  Copyright 2023 TetraTech. All rights reserved.
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
#' @param df_qual Biological index and metrics data for the specified biocomm.
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
getQualSites <- function(TargetSiteID
                        , df_sites
                        , biocomm
                        , df_qual
                        , colBio
                        , colBioSample
                        , colStressSample
                        , compSites
                        , allSites
                        , useBC = FALSE
                        , outcaseColName
                        , outcaseID
                        , BioNarBrk = c(-2, 0.62, 0.799, 0.919, 2)
                        , BioNarLab = c("very likely altered", "likely altered"
                                        , "possibly altered", "likely intact")
                        , BioDegBrk = c(-2, 0.799, 2)
                        , BioDegLab = c("Yes", "No")
                        , dir_results = file.path(getwd(), "Results")
                        , dir_sub = "SiteInfo"
                        ) {##FUNCTION.START

  # For QC purposes
  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    TargetSiteID = TargetSiteID
    df_sites = data_Sites
    biocomm = bioComm
    df_qual = data_bioCoOccur
    colBio = bioIndex
    colBioSample = "RespSampID"
    colStressSample = "StressSampID"
    compSites = comp_sites
    allSites = all_sites
    useBC = TRUE
    outcaseColName = "OutcaseCol"
    outcaseID = outcaseID
    BioNarBrk = BioNarBrk
    BioNarLab = BioNarLab
    BioDegBrk = BioDegBrk
    BioDegLab = BioDegLab
    dir_sub = "SiteInfo"
  }
  #
  # Define pipe
  `%>%` <- dplyr::`%>%`

  # Declare name of column to hold biodegradation flag value
  biocomm <- tolower(biocomm)
  colBioDeg = "BioDeg"
  colBioNar = "BioNarrative"

  # Subset bio index data frame to just site, sample, index score
  df_qual <- df_qual  %>%
    dplyr::select(StationID_Master, all_of(outcaseColName), all_of(colStressSample)
                  , all_of(colBioSample), all_of(colBio))
  df_qual <- df_qual[!is.na(df_qual[, colBio]), ]

  # Get vector of all "reference" sites in data_sites
  all.ref <- df_sites %>%
    dplyr::filter(RefSiteFlag == 1) %>%
    dplyr::select(StationID_Master)
  all.ref <- unlist(all.ref)

  # Get vector of "reference" reaches in data_sites
  all.ref.reaches <- df_sites %>%
    dplyr::filter(RefSiteFlag == 1) %>%
    dplyr::select(COMID)
  all.ref.reaches <- unlist(all.ref.reaches)

  # Get vector of "reference" samples in data_bioCoOccur
  all.ref.samps.bio <- df_qual %>%
    dplyr::filter(StationID_Master %in% all.ref) %>%
    dplyr::select(all_of(colBioSample))
  all.ref.samps.bio <- unlist(all.ref.samps.bio)
  all.ref.samps.stress <- df_qual %>%
    dplyr::filter(StationID_Master %in% all.ref) %>%
    dplyr::select(all_of(colStressSample))
  all.ref.samps.stress <- unlist(all.ref.samps.stress)

  # Flag quality of samples based on degradation threshold
  df_qual[, colBioDeg] <- cut(df_qual[, colBio]
                              , breaks = BioDegBrk
                              , labels = BioDegLab)
  df_qual[, colBioNar] <- cut(df_qual[, colBio]
                              , breaks = BioNarBrk
                              , labels = BioNarLab)

  # Get vector of "not degraded" samples in data_bioCoOccur
  all.good <- df_qual %>%
    dplyr::filter(BioDeg == "No") %>%
    dplyr::select(StationID_Master)
  all.good <- unlist(all.good)

  all.good.samps.bio <- df_qual %>%
    dplyr::filter(StationID_Master %in% all.good) %>%
    dplyr::select(all_of(colBioSample))
  all.good.samps.bio <- unlist(all.good.samps.bio)
  all.good.samps.stress <- df_qual %>%
    dplyr::filter(StationID_Master %in% all.good) %>%
    dplyr::select(all_of(colStressSample))
  all.good.samps.stress <- unlist(all.good.samps.stress)

  # Get vector of "not degraded" reaches in data_sites
  all.good.reaches <- df_sites %>%
    dplyr::filter(StationID_Master %in% all.good) %>%
    dplyr::select(COMID)
  all.good.reaches <- unlist(all.good.reaches)

  # Get vector of sites with samples having index > min target site index
  # Get bio samples and chem sample where bio is better than target
  min.targ <- min(df_qual[, colBio][df_qual$StationID_Master == TargetSiteID])
  all.better <- as.vector(unique(df_qual$StationID_Master[df_qual[, colBio] > min.targ]))
  all.better.samps.bio <- as.vector(unique(df_qual[, colBioSample][df_qual[, colBio] > min.targ]))
  all.better.samps.stress <- as.vector(unique(df_qual[, colStressSample][df_qual[, colBio] > min.targ]))
  all.better.reaches <- as.vector(df_sites$COMID[df_sites$StationID_Master %in% all.better])

  all.ref <- all.ref[!is.na(all.ref)]
  all.ref.samps.bio <- all.ref.samps.bio[!is.na(all.ref.samps.bio)]
  all.ref.samps.stress <- all.ref.samps.stress[!is.na(all.ref.samps.stress)]
  all.ref.reaches <- all.ref.reaches[!is.na(all.ref.reaches)]
  all.good <- all.good[!is.na(all.good)]
  all.good.samps.bio <- all.good.samps.bio[!is.na(all.good.samps.bio)]
  all.good.samps.stress <- all.good.samps.stress[!is.na(all.good.samps.stress)]
  all.good.reaches <- all.good.reaches[!is.na(all.good.reaches)]
  all.better <- all.better[!is.na(all.better)]
  all.better.samps.bio <- all.better.samps.bio[!is.na(all.better.samps.bio)]
  all.better.samps.stress <- all.better.samps.stress[!is.na(all.better.samps.stress)]
  all.better.reaches <- all.better.reaches[!is.na(all.better.reaches)]

  # Get matrix all samples, quality vs. Inside the case (Comparator)/Outside the case/Total
  # better than samples, quality vs. Inside the case (Comparator)/Outside the case/Total

  # First get max(degraded) site index value
  maxDegSiteIndexVal <- max(df_qual[, colBio][df_qual$StationID_Master == TargetSiteID])

  df_qual[, "ComparatorYN"] <- ifelse(df_qual$StationID_Master %in% compSites, "Yes", "No")
  df_qual[, "OutsideCaseYN"] <- ifelse(df_qual$StationID_Master %in% allSites, "Yes", "No")
  df_qual[, "BetterThan"] <- ifelse(df_qual[, colBio] > maxDegSiteIndexVal, "Yes", "No")

  df_qualstats <- df_qual %>%
    dplyr::mutate(CompSites = ifelse(ComparatorYN == "Yes", 1, 0)
                  , CompGood = ifelse((ComparatorYN == "Yes") & (BioDeg == "No"), 1, 0)
                  , CompBad = ifelse((ComparatorYN == "Yes") & (BioDeg == "Yes"), 1, 0)
                  , CompBT = ifelse((ComparatorYN == "Yes") & (BetterThan == "Yes"), 1, 0)
                  , CompBTGood = ifelse((CompBT == 1) & (BioDeg == "No"), 1, 0)
                  , CompBTBad = ifelse((CompBT == 1) & (BioDeg == "Yes"), 1, 0)
                  , OutcaseSites = ifelse((OutcaseCol == outcaseID), 1, 0)
                  , OutcaseGood = ifelse((OutcaseCol == outcaseID) & (BioDeg == "No"), 1, 0)
                  , OutcaseBad = ifelse((OutcaseCol == outcaseID) & (BioDeg == "Yes"), 1, 0)
                  , OutcaseBT = ifelse((OutcaseCol == outcaseID) & (BetterThan == "Yes"), 1, 0)
                  , OutcaseBTGood = ifelse((OutcaseBT == 1) & (BioDeg == "No"), 1, 0)
                  , OutcaseBTBad = ifelse((OutcaseBT == 1) & (BioDeg == "Yes"), 1, 0)
                  , AllSites = 1
                  , AllSitesGood = ifelse(BioDeg == "No", 1, 0)
                  , AllSitesBad = ifelse(BioDeg == "Yes", 1, 0)
                  , AllSitesBT = ifelse(BetterThan == "Yes", 1, 0)
                  , AllSitesBTGood = ifelse((AllSitesBT == 1) & (BioDeg == "No"), 1, 0)
                  , AllSitesBTBad = ifelse((AllSitesBT == 1)&(BioDeg == "Yes"), 1, 0)) %>%
    dplyr::select(CompSites, CompGood, CompBad, CompBT, CompBTGood
                  , CompBTBad, OutcaseSites, OutcaseGood, OutcaseBad
                  , OutcaseBT, OutcaseBTGood, OutcaseBTBad, AllSites
                  , AllSitesGood, AllSitesBad, AllSitesBT, AllSitesBTGood
                  , AllSitesBTBad)

  df_qualstats <- as.data.frame(colSums(df_qualstats), na.rm = TRUE)
  df_qualstats <- cbind(rownames(df_qualstats)
                        , data.frame(df_qualstats, row.names = NULL))
  colnames(df_qualstats) <- c("Label", "Count")
  df_qualstats <- df_qualstats %>%
    dplyr::mutate(Quality = ifelse(stringr::str_detect(Label, "Good")
                                   , "Not degraded"
                                   , ifelse(stringr::str_detect(Label
                                                                , "Bad")
                                            , "Degraded"
                                            , "All qualities"))
                  , Group = ifelse(stringr::str_detect(Label, "BT")
                                   , "Better than"
                                   , "All")
                  , Sites = ifelse(stringr::str_detect(Label, "Comp")
                                   , "ComparatorSamples"
                                   , ifelse(stringr::str_detect(Label
                                                                , "Outcase")
                                            , "OutsideCaseSamples"
                                            , "AllSamples"))
                  , BioComm = biocomm)
  df_qualstats <- df_qualstats %>%
    dplyr::select(-Label) %>%
    dplyr::group_by(BioComm, Group, Quality) %>%
    tidyr::pivot_wider(names_from = "Sites", values_from = "Count") %>%
    dplyr::select(BioComm, Group, Quality, ComparatorSamples
                  , OutsideCaseSamples, AllSamples) %>%
    dplyr::arrange(Group, Quality)

  dirSiteInfo <- file.path(dir_results, TargetSiteID, "SiteInfo")
  fnQualStats <- paste0(TargetSiteID, "_", toupper(biocomm), "_SiteQualities.tab")
  write.table(df_qualstats, file.path(dirSiteInfo, fnQualStats)
              , append = FALSE, col.names = TRUE, row.names = FALSE
              , sep = "\t")

  numcompsfinal <- as.numeric(df_qualstats[1, 4])
  if (numcompsfinal < length(compSites)) {
    gapcomment <- paste0("Comparator sites do not have paired "
                         , " stressor-response data for comparison.")
    gaps <- cbind.data.frame("getQualSites", "Number of Comparators"
                             , length(compSites) - numcompsfinal
                             , gapcomment)
    colnames(gaps) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
  }

  # Return data as a list of vector
  myQualSites <- list(dfQuality = df_qual
                      , allRefBioSites = all.ref
                      , allRefBioRespSamps = all.ref.samps.bio
                      , allRefBioStressSamps = all.ref.samps.stress
                      , allRefBioReaches = all.ref.reaches
                      , allGoodBioSites = all.good
                      , allGoodBioRespSamps = all.good.samps.bio
                      , allGoodBioStressSamps = all.good.samps.stress
                      , allGoodBioReaches = all.good.reaches
                      , allBTBioSites = all.better
                      , allBTBioRespSamps = all.better.samps.bio
                      , allBTBioStressSamps = all.better.samps.stress
                      , allBTBioReaches = all.better.reaches)
  # myQualSites <- list(dfQuality = df_qual
  #                     , allRefBioSites = all.ref
  #                     , allRefBioRespSamps = all.ref.samps.bio
  #                     , allRefBioStressSamps = all.ref.samps.stress
  #                     , allRefBioReaches = all.ref.reaches
  #                     , allGoodBioSites = all.good
  #                     , allGoodBioRespSamps = all.good.samps.bio
  #                     , allGoodBioStressSamps = all.good.samps.stress
  #                     , allGoodBioReaches = all.good.reaches
  #                     , allBTBioSites = all.better
  #                     , allBTBioRespSamps = all.better.samps.bio
  #                     , allBTBioStressSamps = all.better.samps.stress
  #                     , allBTBioReaches = all.better.reaches)

  return(myQualSites)

}
