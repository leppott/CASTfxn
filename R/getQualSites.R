#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Get Quality Sites
#'
#' @description Get quality sites where quality is defined in any of three ways:
#' reference, not degraded, or having better biology than the worst target site sample.
#'
#' @details Modify existing dataframe with stressor-response paired data and
#' having columns StationID, IncaseCol, OutcaseCol, StressSampleDate,
#' RespSampleDate, StressSampleID, RespSampleID, BioComm, BioIndex, Quality,
#' and all_of(stressors).
#' The output dataframe inserts RefSiteFlag, IncaseYN, OutcaseYN, and BetterThan
#' betwee BioCom and BioIndex. It retains all the existing columns from the
#' input dataframe. By incorporating boolean columns, it becomes easier to
#' subset the dataframe according to the requirements of each evaluation.
#'
#' Improvements: Add data gap analysis. In particular, how many samples are
#' better than the min target sample score, and is that enough?
#'
#' Uses the library dplyr, stringr, tibble, and tidyr.
#'
#' @param TargetSiteID Site ID
#' @param biocomm Biological community; fish, algae or BMI.  Default = "BMI".
#' @param df_qual Biological index data for the specified biocomm.
#' @param colBio Name of the column for the biological response index measure.
#' @param compSites Vector containing "inside the case" site identifiers.
#' @param allSites Vector containing "outside the case" site identifiers.
#' @param refSites Vector containing all reference site identifiers.
#' @param stressors Vector containing
#' @param dir_results directory for results; Default = file.path(getwd(), "Results").
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo".
#'
#' @return A dataframe containing the columns StationID, IncaseCol, OutcaseCol,
#'         StressSampleDate, RespSampleDate, StressSampleID, RespSampleID, BioComm,
#'         RefSiteFlag (0 or 1), IncaseYN (0 or 1), OutcaseYN (0 or 1), BetterThan
#'         (0 or 1), BioIndex, Quality (not degraded or degraded), all_of(stressors)
#'         where stressors represent candidate causes from the target site.
#'
#'         Better than is defined as all samples having a biological index
#'         value >= the maximum degraded target site sample (if any) or the
#'         minumum not degraded target site sample.
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
                         dir_results = dir_results,
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
  target.quals <- as.character(df_qual$Quality[df_qual$StationID == TargetSiteID])
  if ("Degraded" %in% target.quals) {
    qual.targ <- df_qual %>%
      dplyr::filter(StationID == TargetSiteID & Quality == "Degraded") %>%
      dplyr::summarise(max := max(.data[[colBio]], na.rm = TRUE), .groups = "drop_last")
    qual.targ <- as.numeric(qual.targ$max)
  } else { #
    qual.targ <- df_qual %>%
      dplyr::filter(StationID == TargetSiteID) %>%
      dplyr::summarise(min := min(.data[[colBio]], na.rm = TRUE), .groups = "drop_last")
    qual.targ <- as.numeric(qual.targ$min)
  }

  df_qual[, "BetterThan"] <- ifelse(df_qual[, colBio] >= qual.targ, 1, 0)

  df_qual <- dplyr::select(df_qual, StationID, IncaseCol, OutcaseCol, StressSampleDate,
                           RespSampleDate, StressSampleID, RespSampleID, BioComm,
                           RefSiteFlag, IncaseYN, OutcaseYN, BetterThan, all_of(colBio),
                           Quality, all_of(stressors))

  # Create dataframe for all possible cases
  df_qualstats <- df_qual %>%
    dplyr::filter(StationID != TargetSiteID) %>%
    dplyr::mutate(IncaseSamples = ifelse(IncaseYN == 1, 1, 0),
                  IncaseGood = ifelse((IncaseYN == 1) & (Quality == "Not degraded"),
                                      1, 0),
                  IncaseBad = ifelse((IncaseYN == 1) & (Quality == "Degraded"),
                                     1, 0),
                  OutcaseSamples = ifelse((OutcaseYN == 1 & IncaseYN == 0), 1, 0),
                  OutcaseGood = ifelse((OutcaseYN == 1) & (IncaseYN == 0) &
                                         (Quality == "Not degraded"), 1, 0),
                  OutcaseBad = ifelse((OutcaseYN == 1) & (IncaseYN == 0) &
                                        (Quality == "Degraded"), 1, 0),
                  TotalSamples = 1,
                  TotalSamplesGood = ifelse(Quality == "Not degraded", 1, 0),
                  TotalSamplesBad = ifelse(Quality == "Degraded", 1, 0),
                  RefSamples = ifelse(RefSiteFlag == 1, 1, 0),
                  RefSamplesGood = ifelse((RefSiteFlag == 1) & (Quality == "Not degraded"),
                                          1, 0),
                  RefSamplesBad = ifelse((RefSiteFlag == 1) & (Quality == "Degraded"),
                                         1, 0)) %>% # ,
    dplyr::select(IncaseSamples, IncaseGood, IncaseBad, OutcaseSamples, OutcaseGood,
                  OutcaseBad, TotalSamples, TotalSamplesGood, TotalSamplesBad,
                  RefSamples, RefSamplesGood, RefSamplesBad)

  df_qualstats <- df_qualstats %>%
    dplyr::summarise(across(where(is.numeric), sum)) %>%
    t()
  df_qualstats <- as.data.frame(df_qualstats) %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(Label = rowname, Count = V1)
  df_qualstats <- df_qualstats %>%
    dplyr::mutate(Quality = dplyr::case_when(stringr::str_detect(Label, "Good") ~ "Not degraded",
                                             stringr::str_detect(Label, "Bad") ~ "Degraded",
                                             TRUE ~ "Total"),
                  Samples = dplyr::case_when(stringr::str_detect(Label, "Ref") ~ "ReferenceSamples",
                                             stringr::str_detect(Label, "Incase") ~ "InsideCaseSamples",
                                             stringr::str_detect(Label, "Outcase") ~ "OutsideCaseSamples",
                                             TRUE ~ "AllSamples"),
                  BioComm = toupper(biocomm))

  df_qualstats <- df_qualstats %>%
    dplyr::select(-Label) %>%
    dplyr::group_by(BioComm, Quality) %>%
    tidyr::pivot_wider(names_from = "Samples", values_from = "Count") %>%
    dplyr::select(BioComm, Quality, InsideCaseSamples, OutsideCaseSamples,
                  AllSamples, ReferenceSamples) %>%
    dplyr::arrange(Quality)

  dirSiteInfo <- file.path(dir_results, TargetSiteID, dir_sub)
  fnQualStats <- paste0(TargetSiteID, "_", toupper(biocomm), "_SiteQualities.tab")
  write.table(df_qualstats, file.path(dirSiteInfo, fnQualStats),
              append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")

  numcompsfinal <- as.numeric(df_qualstats[1, "InsideCaseSamples"])

  if (numcompsfinal < length(compSites)) {
    gapcomment <- paste0("Inside case sites do not have paired ",
                         "stressor-response data for comparison.")
    gaps <- cbind.data.frame("getQualSites", "Number of inside case samples",
                             length(compSites) - numcompsfinal,
                             gapcomment)
    colnames(gaps) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                row.names = FALSE, sep = "\t")
  }

  return(df_qual)

}
