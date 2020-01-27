#' @title Time Sequence Graphics
#'
#' @description Graph time-specific stressor-response values.
#'
#' @details Generates faceted time sequence graphics (stressor/response, one
#' atop the other). All stressor/response data are graphed.
#' Improvements: Add scoring.
#'
#' Uses the libraries dplyr, tidyr, ggplot2, and ggrepel.
#'
#' @param dataDir Directory containing all data
#' @param dir_results Directory containing all results. Default = "file.path(getwd(),"Results")"
#' @param stressors stressors
#' @param biocomm Biological community; algae or BMI. Default = "BMI".
#' @param BioResp Biological response variable names. For example, BMI metrics
#' or Algae metrics.
#' @param df_stress Stressor values.
#' @param df_resp Response values for the specified biological community and metrics.
#' @param colname.SampID Name of the column for the response sample identifier.
#'
#' @return One or more jpgs in SiteID/TemporalSequence/Biocomm subfolder of the
#'        "Results" folder of working directory. No scores are currently generated.
#'
#' @keywords internal
#'
#' @export
getOutliers <- function(df_data, df_meta) {
    
    # Debug
    boo_DEBUG <- FALSE
    if (boo_DEBUG==TRUE) {
        df_data = data_modelRaw
        df_meta = data_modelInfo
    }

    # Define pipe
    `%>%` <- dplyr::`%>%`
    # not_all_na <- function(x) {!all(is.na(x))}
    
    # Ensure uniqueness of df_meta
    df_meta <- df_meta %>%
        dplyr::select(StdParamName, LogTransf) %>%
        dplyr::group_by(StdParamName) %>%
        dplyr::summarise(LogTransf = max(LogTransf))
    # LogTransform data that need to be transformed
    df_data <- merge(df_data, df_meta[,c("StdParamName","LogTransf")]
                     , by.x = "StdParamName", by.y = "StdParamName"
                     , all.x = TRUE)
    df_data <- df_data %>%
        dplyr::filter(!is.na(ResultValue)) %>%
        dplyr::mutate(TransfResult = ifelse(LogTransf==1
                                            , log10(ResultValue)
                                            , ResultValue))
    params <- unique(as.character(df_data$StdParamName))
    
    for (p in 1:length(params)) { # Iterate over parameters
        
        paramName <- params[p]
        
        df_sub <- df_data %>%  # Subset for just one parameter at a time
            dplyr::filter(StdParamName == paramName)
        
        # 3*IQR method for identifying outliers
        iqr <- quantile(df_sub$TransfResult, probs=c(0.25,0.75), na.rm = TRUE)
        iqr1.5 <- 1.5*IQR(df_sub$TransfResult, na.rm = TRUE)
        outlowlim <- iqr[1] - iqr1.5
        outhilim <- iqr[2] + iqr1.5
        
        df_sub <- df_sub %>%
            dplyr::mutate(IQRmethod = ifelse((TransfResult < outlowlim)
                                             , "Outlier low"
                                             , ifelse((TransfResult > outhilim)
                                              , "Outlier high", "Good")))
        
        # 6*sd method for identifying outliers
        paramMean <- mean(df_sub$TransfResult[is.finite(df_sub$TransfResult)]
                          , na.rm = TRUE)
        paramSD <- sd(df_sub$TransfResult[is.finite(df_sub$TransfResult)]
                      , na.rm = TRUE)
        df_sub <- df_sub %>%
            dplyr::mutate(SDmethod = ifelse((abs(TransfResult - paramMean) > 
                                                 (6*paramSD)), "Outlier", "Good"))
        
        # Combine the findings
        df_sub <- df_sub %>%
            dplyr::mutate(Outlier = ifelse((IQRmethod=="Good") | (SDmethod=="Good")
                                           ,"Good", "Outlier"))

        if (p == 1) {
            df_temp <- df_sub
        } else {
            df_temp <- rbind(df_temp, df_sub)
        }

    } # End parameter iteration
    
    df_temp <- dplyr::select(df_temp, ChemSampleID, StdParamName, ResultValue
                             , IQRmethod, SDmethod, Outlier)
    # Return what?
    myOutliers <- df_temp

}