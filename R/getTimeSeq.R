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
#' @param TargetSiteID Site ID
#' @param stressors stressors
#' @param biocomm Biological community; algae or BMI. Default = "BMI".
#' @param BioResp Biological response variable names. For example, BMI metrics 
#' or Algae metrics.
#' @param df_stress Stressor values.
#' @param df_resp Response values for the specified biological community and metrics.
#' @param colname.SampID Name of the column for the response sample identifier.
#' @param dir_results Directory containing all results. Default = "file.path(getwd(),"Results")"
#' @param dir_sub Subdirectory for outputs from this function. Default = "TemporalSequence"
#' 
#' @return One or more jpgs in SiteID/TemporalSequence/Biocomm subfolder of the 
#'        "Results" folder of working directory. No scores are currently generated.
#' 
#' @keywords internal
#' 
#' @export
getTimeSeq <- function(TargetSiteID
                       , stressors
                       , biocomm = "BMI"
                       , BioResp
                       , df_stress
                       , df_resp
                       , colSampID
                       , dir_results = file.path(getwd(),"Results")
                       , dir_sub = "TemporalSequence") {

    # Define pipe
    `%>%` <- dplyr::`%>%`

    not_all_na <- function(x) {!all(is.na(x))}
    biocomm <- toupper(biocomm)
    
    # Check for presence of TemporalSequence directory. If not present, create
    ifelse(!dir.exists(file.path(dir_results, TargetSiteID))==TRUE
           , dir.create(file.path(dir_results, TargetSiteID))
           , FALSE)
    ifelse(!dir.exists(file.path(dir_results, TargetSiteID, dir_sub))==TRUE
           , dir.create(file.path(dir_results, TargetSiteID, dir_sub))
           , FALSE)
    ifelse(!dir.exists(file.path(dir_results, TargetSiteID, dir_sub, biocomm))==TRUE
           , dir.create(file.path(dir_results, TargetSiteID, dir_sub, biocomm))
           , FALSE)
    
    path <- file.path(dir_results, TargetSiteID, dir_sub, biocomm)
    
    skipflag = FALSE
    
    # Prep measured stressor data
    df_stress <- df_stress %>%
        dplyr::select_if(not_all_na) %>%
        dplyr::select(-StationID_Master
               , -RespSampID
               , -RespSampDate) %>%
        tidyr::gather(key = StdParamName, value = ResultValue
               , -StressSampID, -StressSampDate) %>%
        # dplyr::filter(!is.na(ResultValue)) %>%
        # dplyr::mutate(variable = StdParamName
        #        , value = ResultValue
        #        , SampID = ChemSampleID) %>%
        # dplyr::select(BioQuality, SampleDate, variable, value) %>%
        dplyr::group_by(StressSampDate, StdParamName) %>%
        dplyr::summarize(meanval = signif(mean(ResultValue),digits=3)) %>%
        dplyr::rename(SampDate = StressSampDate, variable = StdParamName)

    # Prep response data
    df_resp <- df_resp %>%
        dplyr::select_if(not_all_na) %>%
        dplyr::select(-StationID_Master
               , -StressSampID
               , -StressSampDate
               , -Quality) %>%
        tidyr::gather(key = Biometric, value = ResultValue
               , -RespSampID, -RespSampDate) %>%
        dplyr::filter(!is.na(ResultValue)
               , Biometric %in% BioResp) %>%
        dplyr::group_by(RespSampDate, Biometric) %>%
        dplyr::summarize(meanval = signif(mean(ResultValue),digits=3)) %>%
        dplyr::rename(SampDate = RespSampDate, variable = Biometric)

        skipflag <- ifelse(nrow(df_resp)==0,TRUE, FALSE)
    
    if (skipflag == FALSE) {
        
        # Ensure all data in one dataframe
        df.data <- rbind(df_stress, df_resp)
        
        minDate <- as.Date(min(df.data$SampDate)-30)
        maxDate <- as.Date(max(df.data$SampDate)+30)
        diffDate <- paste(round((maxDate - minDate)/10, 2),"days")
        # print(diffDate)
        # flush.console()

        # Loop over each stressor
        ppi = 300
        for (s in 1:length(stressors)) {
            stressName = stressors[s]
            
            # Plot time series for stressor & bio response
            for (r in 1:length(BioResp)) {
                respName = BioResp[r]
                
                fn = paste0(TargetSiteID,".TS.",stressName,".",respName,".jpg")
                fpath = file.path(path, fn)
                
                df.plot <- df.data %>%
                    dplyr::filter(variable %in% c(stressName,respName))
                df.plot$variable <- factor(df.plot$variable
                                           , levels = c(stressName, respName))
                maxStress <- max(df.plot$meanval[df.plot$variable==stressName])
                maxResp <- max(df.plot$meanval[df.plot$variable==respName])
                
                print(paste("Plotting bar graphs for", stressName, "and", respName))
                flush.console()

                p_ts <- ggplot2::ggplot(df.plot, ggplot2::aes(x=SampDate
                                            , y=as.numeric(meanval)))
                p_ts <- p_ts + ggplot2::geom_col(fill = "black", width = 2
                             , position = ggplot2::position_dodge(preserve = "single"))
                p_ts <- p_ts + ggrepel::geom_text_repel(ggplot2::aes(label=meanval)
                                        , hjust= 2, vjust = 0, size=2.5)
                p_ts <- p_ts + ggplot2::facet_wrap(~ variable, ncol=1, scales="free_y")
                p_ts <- p_ts + ggplot2::theme_bw()
                p_ts <- p_ts + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                       , hjust = 1, size = 8)
                                       , panel.grid.minor = ggplot2::element_blank())
                p_ts <- p_ts + ggplot2::scale_x_date(limits=c(minDate,maxDate)
                                 , date_labels = "%m/%d/%Y", date_breaks = diffDate)
                p_ts <- p_ts + ggplot2::labs(title = paste(TargetSiteID
                                       ,"Stressor/Response Time Series")
                         , x = "Sample Date", y = "Value")
                p_ts <- p_ts + ggplot2::ggsave(filename=fpath, dpi = ppi, width=8
                                    , height=6, units="in")
            }
        }
    } else {
        print(paste("No ",biocomm,"response data available for", TargetSiteID))
        flush.console()
    }
    
}
