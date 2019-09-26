#' @title Get CoOccurrence Data
#' 
#' @description Prepare matched stressor/response data as a single dataframe
#' 
#' @details Generates a dataframe and a table that pairs stressor and response
#' data for all stressor and response samples obtained within a specified time
#' of each other. User specified lag time between stressor sample (prior) and 
#' response sample (after) may be employed, with a default of same day samples.
#' 
#' Uses the libraries dplyr and tidyr.
#' 
#' @param dataDir Directory containing the CAST dataset for analysis.
#' @param df_sites Dataframe containing site data for all sites. 
#' Default = "data_Sites".
#' @param df_mod Dataframe containing modeled stressor data for all sites that 
#' have it. Default = "data.model.raw".
#' @param df_meas Dataframe containing measured stressor data for all sites that 
#' have them. Default = "data.chem.raw".
#' @param biocomm Biological community; algae or BMI. Default = "BMI".
#' @param df_resp Dataframe containing biological response metrics. 
#' Default = "data.bmi.metrics".
#' @param index Name of the response index column. Default = "CSCI".
#' @param respColnames Names of the core response columns (date, sample ID, 
#' quality, and index score). Default = c("BMISampDate", "BMISampID", 
#' "Quality", "CSCI").
#' @param lagdays The number of days allowed between the stressor sample date
#' and the response sample date, where stressor must always be sampled prior to 
#' the response sample collection. Default = 0 (same day).
#' 
#' @return A dataframe containing matched stressor response data based on 
#' same-day matching.
#' Improvements: Add flexible matching to account for lag time.
#' Improvements: Add algae capability.
#' 
#' 
#' @keywords internal
#' 
#' @export
getCoOccurDataset <- function(dataDir = file.path(getwd(),"Data")
                              , df_sites = data_Sites
                              , df_model = data.model.raw
                              , df_meas = data.chem.raw
                              , biocomm = "BMI"
                              , df_resp = data.bmi.metrics
                              , index = "CSCI"
                              , respColnames = c("BMISampDate", "BMISampID"
                                                  , "Quality", "CSCI")
                              , lagdays = 0) {
    
    # Debug
    # dataDir = file.path(getwd(),"Data")
    # df_sites = data_Sites
    # df_model = data.model.raw
    # df_meas = data.chem.raw
    # biocomm = "BMI"
    # df_resp = data.bmi.metrics
    # index = "CSCI"
    # respColnames = c("BMISampDate", "BMISampID", "Quality", "CSCI")
    # lagdays = 10
    
    biocomm <- tolower(biocomm)
    
    # define pipe
    `%>%` <- dplyr::`%>%`
    
    # Read data files (stressor and response)
    df_resp <- df_resp[,c("StationID_Master", "CollDate", "BMI.Metrics.SampID"
                        , "Quality", "IBI")]
    colnames(df_resp) <- c("StationID_Master", respColnames)
    
    # Clean up modeled data
    df_model <- df_model %>%
        dplyr::select(StationID_Master, clust, StdParamName, ResultValue) %>%
        tidyr::spread(key = StdParamName, value = ResultValue)
    modColnames <- names(df_model)
    modColnames <- modColnames[!(modColnames %in% c("StationID_Master","clust"))]
    
    # Merge modeled stressor data and response data
    df_modbmi <- merge(df_resp, df_model, by.x = "StationID_Master"
                       , by.y = "StationID_Master", all = TRUE)
    df_modbmi <- df_modbmi %>% 
        dplyr::mutate(BioSampleDate = lubridate::mdy(BMISampDate)) %>%
        dplyr::mutate(LagDate = BioSampleDate - lagdays) %>%
        dplyr::select(StationID_Master
                      , BioSampleDate
                      , LagDate
                      , eval(respColnames)
                      , eval(modColnames)
                      , - clust
                      , - BMISampDate)
    
    rm(df_model, df_resp)
    respColnames <- respColnames[!respColnames %in% "BMISampDate"]

    # Clean up measured data and convert to wide format
    df_meas <- df_meas[!is.na(df_meas$ResultValue),]
    df_meas <- df_meas %>% 
        dplyr::select(StationID_Master, ChemSampleID, SampDate
               , StdParamName, ResultValue) %>%
        tidyr::spread(key = StdParamName, value = ResultValue) %>%
        dplyr::mutate(StressSampleDate = lubridate::mdy(SampDate))
    measColnames <- names(df_meas)
    measColnames <- measColnames[!(measColnames %in% c("StationID_Master"
                                                       , "ChemSampleID"
                                                       , "StressSampleDate"))]
    
    # Merge site/bmi data with measure data by station & date
    
    df_coOccur <- fuzzyjoin::fuzzy_left_join(df_modbmi, df_meas
                            , by = c("StationID_Master" = "StationID_Master"
                            , "BioSampleDate" = "StressSampleDate"
                            , "LagDate" = "StressSampleDate")
                            , match_fun = list(`==`, `>=`, `<=`)) %>%
        dplyr::filter(!is.na(StationID_Master.y)) %>%
        dplyr::rename(StationID_Master = StationID_Master.x) %>%
        dplyr::rename(RespSampleDate = BioSampleDate) %>%
        dplyr::mutate(BioComm = eval(biocomm)) %>%
        dplyr::select(StationID_Master, StressSampleDate, RespSampleDate
                      , ChemSampleID, eval(respColnames), eval(modColnames)
                      , eval(measColnames), -SampDate)
    df_sites <- df_sites[,c("StationID_Master", "clust")]
    df_coOccur <- merge(df_sites, df_coOccur)
    
    fn <- paste0("SMC",biocomm,"CoOccurFinal.tab")
    write.table(df_coOccur, file.path(dataDir,fn)
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
    
    return(df_coOccur)
    
}







