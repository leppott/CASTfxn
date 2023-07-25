#  Copyright 2023 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Get Data Sets
#'
#' @description Subsets a dataframe of matched stressor-response data for
#'              a single target site (with one or more matched samples),
#'              comparator samples (inside the case), and all samples (outside
#'              the case)
#'
#' @details Subsets the data frame output by the function getCoOccurDataset
#'          first for stressors detected at the target site (in any sample),
#'          then produces multiple subsets: two for the target site samples,
#'          two for comparator site samples, and two for all site samples.
#'
#' Uses the libraries dplyr, tidyr, and fuzzyjoin.
#'
#' @param TargetSiteID site identifier for the site being evaluated (the Target Site)
#' @param compSites vector containing comparator site IDs
#' @param allSites vector containing all "outside the case" site IDs
#' @param df_coOccur dataframe of matched stressor response samples for the
#' desired biological community
#' @param siteStressors Vector containing stressors identified as candidate causes
#' for the target site
#' @param bioParamsDEL vector of modeled parameters to delete based on lack of
#' applicability to the biological response community
#' @param colBioSample column name for the column containing the response sample ID
#' @param colBioSampDate column name for the column containing the response sample date
#' @param df_stressInfo dataframe containing stressor metadata, specifically "Label".
#' @param df_biometrics dataframe containing the biological response samples'
#' index and metric values
#'
#' @return List containing six dataframes: 1) stressor data from all samples
#'         (outside the case), 2) stressor data from comparator samples (inside
#'         the case), 3) stressor data from only the target samples, 4) response
#'         data from all samples (outside the case), 5) response data from
#'         comparator samples (inside the case), and 6) response data from only
#'         the target samples.
#'
#' @keywords internal
#'
#' @export
getDataSets <- function(TargetSiteID
                        , compSites # inside the case
                        , allSites  # outside the case
                        , df_coOccur
                        , siteStressors
                        , bioParmsDEL = NULL
                        , colBioSample
                        , colBioSampDate
                        , df_biometrics
                        , df_stressinfo
                        ) {##FUNCTION.START

  # For QC purposes
  boo_DEBUG <- FALSE

  if (boo_DEBUG==TRUE) {
    TargetSiteID = TargetSiteID
    compSites = comp_sites
    allSites = all_sites
    df_coOccur = data_bioCoOccur
    siteStressors = stressors
    bioParmsDEL = bioParmsDEL
    colBioSample = colBioSample
    colBioSampDate = colBioSampDate
    df_biometrics = bioMetricData
    df_stressinfo = data_stressInfo
  }

  # Define pipe
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}

  # biocomm <- tolower(biocomm)

  # Get dataframe of parameters detected at target site (meas & mod)
  df_detects <- df_coOccur %>% dplyr::select_if(not_all_na)
  useParams <- setdiff(stressors, bioParmsDEL)
  coreCols <- c("StationID_Master", "StressSampID", "StressSampDate"
                , "RespSampID", "RespSampDate")
  useCols <- c(coreCols, useParams)

  # Subset big dataset to only detected stressors at target site
  # Create stressor data sets for target, inside the case, and outside the case
  siteBioStressData <- df_detects[, useCols] %>%
    dplyr::filter(StationID_Master == TargetSiteID)
  allBioStressData <- df_detects[, useCols] %>%
    dplyr::filter(StationID_Master %in% allSites)
  compBioStressData <- df_detects[, useCols] %>%
    dplyr::filter(StationID_Master %in% compSites)

  # Create response data sets for target, inside the case, and outside the case
  df_core <- dplyr::select(df_detects, all_of(coreCols))
  allBioRespData <- merge(df_core, df_biometrics
                          , by.x = c("StationID_Master", "RespSampID"
                                     , "RespSampDate")
                          , by.y = c("StationID_Master", colBioSample
                                     , colBioSampDate))
  allBioRespData  <- dplyr::filter(allBioRespData, StationID_Master %in% allSites)
  compBioRespData <- dplyr::filter(allBioRespData, StationID_Master %in% compSites)
  siteBioRespData <- dplyr::filter(allBioRespData, StationID_Master == TargetSiteID)

  # Identify "no match" data
  # Do this when generating coOccurrence dataset

  # Subset stressor table for just detected stressors at target site
  df_stressinfo <- df_stressinfo %>%
    dplyr::filter(StdParamName %in% useParams)

  # Return both unmatched and matched data as output
  mySubsets <- list(siteStressInfo = df_stressinfo
                    , allBioStress = allBioStressData
                    , compBioStress = compBioStressData
                    , siteBioStress = siteBioStressData
                    , allBioResp = allBioRespData
                    , compBioResp = compBioRespData
                    , siteBioResp = siteBioRespData)

  return(mySubsets)

}
