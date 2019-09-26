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
                       , colname.SampID
                       , dir_results = file.path(getwd(),"Results")
                       , dir_sub = "TemporalSequence") {

    # TargetSiteID
    # stressors
    # biocomm = "BMI"
    # BioResp = BMImetrics
    # df_stress = site.b.str
    # df_resp = site.b.rsp
    # colname.SampID = "BMI.Metrics.SampID"
    # dir_results = file.path(getwd(),"Results")
    # dir_sub = "TemporalSequence"
    
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
        dplyr::mutate(BioQuality = as.factor(Quality)) %>%
        dplyr::select(-StationID_Master
               , -BMI.Metrics.SampID
               , -SampDate
               , -LagDate
               , -Quality) %>%
        tidyr::gather(key = StdParamName, value = ResultValue
               , -ChemSampleID, -SampleDate, -BioQuality) %>%
        dplyr::filter(!is.na(ResultValue)) %>%
        dplyr::mutate(variable = StdParamName
               , value = ResultValue
               , SampID = ChemSampleID) %>%
        dplyr::select(BioQuality, SampleDate, variable, value) %>%
        dplyr::group_by(BioQuality, SampleDate, variable) %>%
        dplyr::summarize(meanval = formatC(signif(mean(value),digits=3)
                                , digits=3,format="fg", flag="#"))

    # Prep response data
    if (biocomm == "BMI") {
        df_resp <- df_resp %>%
            dplyr::select_if(not_all_na) %>%
            dplyr::mutate(BioQuality = as.factor(Quality)) %>%
            dplyr::select(-StationID_Master
                   , -BMISampID
                   , -ChemSampleID
                   , -SampDate
                   , -LagDate
                   , -CollDate
                   , -Quality) %>%
            tidyr::gather(key = BMImetric, value = ResultValue
                   , -BMI.Metrics.SampID, -SampleDate, -BioQuality) %>%
            dplyr::filter(!is.na(ResultValue)
                   , BMImetric %in% BioResp) %>%
            dplyr::mutate(variable = BMImetric
                   , value = ResultValue
                   , SampID = BMI.Metrics.SampID) %>%
            dplyr::select(BioQuality, SampleDate, variable, value) %>%
            dplyr::group_by(BioQuality, SampleDate, variable) %>%
            dplyr::summarize(meanval = formatC(signif(mean(value),digits=3)
                                        , digits=3,format="fg", flag="#"))
    } else if (biocomm == "ALGAE") {
        df_resp <- df_resp %>%
            dplyr::select_if(not_all_na) %>%
            dplyr::mutate(BioQuality = as.factor(Quality)) %>%
            dplyr::select(-StationID_Master
                   , -ChemSampleID
                   , -SampDate
                   , -LagDate
                   , -Quality) %>%
            tidyr::gather(key = Algmetric, value = ResultValue
                   , -Alg.Metrics.SampID, -SampleDate, -BioQuality) %>%
            dplyr::filter(!is.na(ResultValue)
                   , Algmetric %in% BioResp) %>%
            dplyr::mutate(variable = Algmetric
                   , value = ResultValue
                   , SampID = Alg.Metrics.SampID) %>%
            dplyr::select(BioQuality, SampleDate, variable, value) %>%
            dplyr::group_by(BioQuality, SampleDate, variable) %>%
            dplyr::summarize(meanval = formatC(signif(mean(value),digits=3)
                                        , digits=3,format="fg", flag="#"))
    } else {
        warn(paste(biocomm,"is not a valid option."))
    }
    skipflag <- ifelse(nrow(df_resp)==0,TRUE, FALSE)
    
    if (skipflag == FALSE) {
        
        # Ensure all data in one dataframe
        df.data <- rbind(df_stress, df_resp)
        
        minDate <- min(df.data$SampleDate)-30
        maxDate <- max(df.data$SampleDate)+30
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

                ggplot2::ggplot(df.plot, ggplot2::aes(x=SampleDate, y=as.numeric(meanval))) +
                    ggplot2::geom_col(fill = "black", width = 2
                             , position = ggplot2::position_dodge(preserve = "single")) +
                    ggrepel::geom_text_repel(ggplot2::aes(label=meanval), hjust= 2, vjust = 0
                                    , size=2.5) +
                    ggplot2::facet_wrap(~ variable, ncol=1, scales="free_y") +
                    ggplot2::theme_bw() + 
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                       , hjust = 1, size = 8)
                                       , panel.grid.minor = ggplot2::element_blank()) +
                    ggplot2::scale_x_date(limits=c(minDate,maxDate)
                                 , date_labels = "%m/%d/%Y"
                                 , date_breaks = diffDate) +
                    ggplot2::labs(title = paste(TargetSiteID
                                       ,"Stressor/Response Time Series")
                         , x = "Sample Date", y = "Value") +
                    ggplot2::ggsave(filename=fpath, dpi = ppi, width=8
                                    , height=6, units="in")
            }
        }
    } else {
        print(paste("No ",biocomm,"response data available for", TargetSiteID))
        flush.console()
    }
    
}
