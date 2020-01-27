#' @title Get Data Sets
#' 
#' @description Get Data Sets identifies stressor-response paired samples with
#' a lag time that is provided as a passed parameter. 
#' 
#' @details This function applies fuzzyjoin using a lag time in days, where the 
#' stressor sample must have been collected between lag time days before or 
#' on the same day as the response sample. 
#' 
#' Uses the libraries dplyr, tidyr, and fuzzyjoin.
#' 
#' @param TargetSiteID Site ID
#' @param compSites Vector of stressors.
#' @param stresstype Type of stressor (measured or modeled). Default = "meas".
#' Valid options are "meas" for measured data or "mod" for modeled data.
#' @param df_stress Stressor values.
#' @param df_stressinfo Metadata about the stressors.
#' @param biocomm Biological community; algae or BMI. Default = "BMI".
#' @param df_biometrics Response metrics values for the specified biocomm.
#' 
#' @return One or more jpgs in SiteID/TemporalSequence/Biocomm subfolder of the 
#'        "Results" folder of working directory. No scores are currently generated.
#' 
#' @keywords internal
#' 
#' @export
getDataSets <- function(TargetSiteID
                        , compSites
                        , df_coOccur = data_bmiCoOccur
                        , measParams = measParams
                        , modelParams = modelParams
                        , biocomm = "bmi"
                        , bioIndex = "CSCI"
                        , colBioSample = "BMISampID"
                        , colBioSampDate = "BMISampDate"
                        , df_biometrics = data_bmiMetrics
                        , df_stressinfo = data_stressInfo) {
  
  # For QC purposes
  # TargetSiteID
  # compSites = comp_sites
  # df_coOccur = data_bioCoOccur
  # measParams = measParams
  # modelParams = modelParams
  # biocomm = "bmi"
  # bioIndex = colBio
  # colBioSample = colBioSample
  # colBioSampDate = colBioSampDate
  # df_biometrics = data_bmiMetrics
  # df_stressinfo = data_stressInfo
  
  # Define pipe
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}
  
  biocomm <- tolower(biocomm)
  
  # Get datafrane of parameters detected at target site (meas & mod)
  df_detects <- df_coOccur %>% dplyr::select_if(not_all_na)
  useCols <- colnames(df_detects) %in% c("StationID_Master", "StressSampID"
                                         , "StressSampDate", "RespSampID"
                                         , "RespSampDate", modelParams
                                         , measParams)
  
  siteBioStressData <- df_detects[,useCols] %>%
    dplyr::filter(StationID_Master == TargetSiteID) %>%
    dplyr::select_if(not_all_na)
  useColsDetects <- colnames(siteBioStressData)
  allBioStressData <- df_detects[,useColsDetects]
  compBioStressData <- df_detects[,useColsDetects] %>%
    dplyr::filter(StationID_Master %in% compSites)
  
  df_core <- dplyr::select(df_detects, StationID_Master, StressSampID
                           , StressSampDate, RespSampID, RespSampDate)
  useParams <- colnames(dplyr::select(df_detects, -StationID_Master
                                      , -StressSampID, -StressSampDate
                                      , -RespSampID, -RespSampDate, -clust
                                      , -BioComm, -Quality))
  useParams <- useParams[useParams != bioIndex]
  
  allBioRespData <- merge(df_core, df_biometrics
                          , by.x = c("StationID_Master", "RespSampID"
                                     , "RespSampDate")
                          , by.y = c("StationID_Master", colBioSample
                                     , colBioSampDate))
  compBioRespData <- allBioRespData[allBioRespData$StationID_Master %in% compSites,]
  siteBioRespData <- allBioRespData[allBioRespData$StationID_Master==TargetSiteID,]
  
  # Identify "no match" data
  # Do this when generating coOccurrence dataset
  
  # Subset stressor table for just detected stressors at target site
  df_stressinfo <- df_stressinfo %>% 
    dplyr::filter(StdParamName %in% useParams) %>%
    dplyr::select(-ANALYSIS_TYPE, -CHEMICAL_NAME, -FinalUnit)
  
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