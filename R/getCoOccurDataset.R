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
#' "Quality", index).
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
                              , df_model = data_modelRaw
                              , df_meas = data_chemRaw
                              , biocomm = "BMI"
                              , df_resp = data_bmiMetrics
                              , index = "CSCI"
                              , lagdays = 0
                              , removeOutliers = TRUE) {
    
    # Debug
    
    boo_DEBUG <- FALSE
    
    if  (boo_DEBUG == TRUE) {
        dataDir = dir_data
        df_sites = data_Sites
        df_model = data_modelRaw
        df_meas = data_chemRaw
        biocomm = "Alg"
        df_resp = data_AlgMetrics
        index = algIndex
        lagdays = lagdays
        removeOutliers = TRUE
    }
    
    # define pipe
    `%>%` <- dplyr::`%>%`
    not_all_na <- function(x) {!all(is.na(x))}
    biocomm <- tolower(biocomm)
    
    # Read data files (stressor and response)
    if (biocomm == "bmi") {
        df_resp <- df_resp[,c("StationID_Master", "BMISampDate", "BMISampID"
                              , "Quality", index, "BMISampFlag")] %>%
            dplyr::rename(RespSampDate = BMISampDate) %>%
            dplyr::rename(RespSampID = BMISampID) %>%
            dplyr::rename(RespSampFlag = BMISampFlag)
    } else if (biocomm == "alg") {
        df_resp <- df_resp[,c("StationID_Master", "AlgSampDate", "AlgSampID"
                              , "Quality", index)] %>%
            dplyr::rename(RespSampDate = AlgSampDate) %>%
            dplyr::rename(RespSampID = AlgSampID) #%>%
            dplyr::mutate(RespSampFlag = NA)
    } else {
        print("Biological community type not used.")
        flush.console()
    }
    # colnames(df_resp) <- c("StationID_Master", respColnames)
    
    # Clean up modeled data
    df_model <- df_model %>%
        dplyr::select(StationID_Master, StdParamName, ResultValue) %>%
        tidyr::spread(key = StdParamName, value = ResultValue)
    modColnames <- names(df_model)
    modColnames <- modColnames[!(modColnames %in% "StationID_Master")]
    
    # Merge modeled stressor data and response data
    df_modresp <- merge(df_resp, df_model, by.x = "StationID_Master"
                       , by.y = "StationID_Master", all = TRUE)
    df_modresp <- df_modresp %>% 
        # dplyr::mutate(RespSampDate = lubridate::mdy(RespSampDate)) %>%
        dplyr::mutate(LagDate = RespSampDate - lagdays) %>%
        dplyr::select(StationID_Master
                      , RespSampDate
                      , LagDate
                      , RespSampID
                      , Quality
                      , eval(index)
                      , RespSampFlag
                      , eval(modColnames))
    
    rm(df_model, df_resp)
    respColnames <- c("RespSampID", "Quality", index, "RespSampFlag")

    # Clean up measured data and convert to wide format
    df_meas <- df_meas[!is.na(df_meas$ResultValue),]
    if (removeOutliers == TRUE) {
        df_meas <- df_meas[df_meas$Outlier != "Outlier",]
    }
    df_meas <- as.data.frame(df_meas)
    df_meas <- dplyr::select(df_meas, -SampDate)
    df_meas <- df_meas %>% 
        dplyr::select(StationID_Master, ChemSampleID, SampleDate
                      , StdParamName, ResultValue) %>%
        dplyr::group_by(StationID_Master, ChemSampleID, SampleDate
                        , StdParamName) %>%
        dplyr::summarise(meanResult = mean(ResultValue)) %>%
        dplyr::rename(ResultValue = meanResult) %>%
        tidyr::spread(key = StdParamName, value = ResultValue) %>%
        dplyr::rename(StressSampDate = SampleDate)
    measColnames <- names(df_meas)
    measColnames <- measColnames[!(measColnames %in% c("StationID_Master"
                                                       , "ChemSampleID"
                                                       , "StressSampDate"))]
    
    # Merge site/bmi data with measure data by station & date
    
    df_coOccur <- fuzzyjoin::fuzzy_left_join(df_modresp, df_meas
                            , by = c("StationID_Master" = "StationID_Master"
                            , "RespSampDate" = "StressSampDate"
                            , "LagDate" = "StressSampDate")
                            , match_fun = list(`==`, `>=`, `<=`)) %>%
        dplyr::filter(!is.na(StationID_Master.y)) %>%
        dplyr::rename(StationID_Master = StationID_Master.x) %>%
        dplyr::rename(StressSampID = ChemSampleID) %>%
        dplyr::mutate(BioComm = eval(biocomm)) %>%
        dplyr::select(StationID_Master, StressSampDate, RespSampDate
                      , StressSampID, BioComm, eval(respColnames)
                      , eval(modColnames), eval(measColnames)) %>%
        dplyr::select_if(not_all_na)
    df_coOccur <- df_coOccur[not_all_na(df_coOccur)]
    df_sites <- df_sites[,c("StationID_Master", "clust")]
    df_coOccur <- merge(df_sites, df_coOccur)
    
    fn <- paste0("SMC",biocomm,"CoOccurFinal.tab")
    write.table(df_coOccur, file.path(dataDir,fn)
                , append = FALSE, col.names = TRUE, row.names = FALSE
                , sep = "\t")
    
    return(df_coOccur)
    
}







