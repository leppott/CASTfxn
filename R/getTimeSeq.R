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
#' @param biocomm Biological community; algae or BMI. Default = "BMI".
#' @param BioResp Biological response variable names. For example, BMI metrics or Algae metrics.
#' @param stressors stressors
#' @param df_stress Stressor values.
#' @param df_resp Response values for the specified biological community and metrics.
#' @param df_stressinfo data frame, stress info
#' @param df_respinfo data frame, response info
#' @param dir_results Directory containing all results. Default = "file.path(getwd(),"Results")"
#' @param dir_sub Subdirectory for outputs from this function. Default = "TimeSequence"
# @param stressors stressors

# @param colname.SampID Name of the column for the response sample identifier.
#'
#' @return One or more jpgs in SiteID/TemporalSequence/Biocomm subfolder of the
#'        "Results" folder of working directory. No scores are currently generated.
#'
#' @keywords internal
#'
#' @export
getTimeSeq <- function(TargetSiteID
                       , biocomm = "BMI"
                       , BioResp
                       , stressors
                       , df_stress
                       , df_resp
                       , df_stressinfo
                       , df_respinfo
                       , dir_results = file.path(getwd(),"Results")
                       , dir_sub = "TimeSequence") {

    # Debug
    boo_DEBUG <- FALSE
    
    if (boo_DEBUG == TRUE) {
        TargetSiteID
        biocomm = bioComm
        BioResp = bioMetricNames
        df_stress = siteStressAll
        df_resp = siteRespAll
        stressors = stressorsWPairedResponses
        df_stressinfo = data_stressInfo
        df_respinfo = data_bmiMetricsInfo
        dir_results = file.path(getwd(),"Results")
        dir_sub = "TimeSequence"
    }


    # Define pipe
    `%>%` <- dplyr::`%>%`

    not_all_na <- function(x) {!all(is.na(x))}
    biocomm <- toupper(biocomm)

    # Check for presence of TemporalSequence directory. If not present, create
    ifelse(!dir.exists(file.path(dir_results, TargetSiteID))==TRUE
           , dir.create(file.path(dir_results, TargetSiteID))
           , FALSE)
    ifelse(!dir.exists(file.path(dir_results, TargetSiteID, biocomm))==TRUE
           , dir.create(file.path(dir_results, TargetSiteID, biocomm))
           , FALSE)
    ifelse(!dir.exists(file.path(dir_results, TargetSiteID, biocomm, dir_sub))==TRUE
           , dir.create(file.path(dir_results, TargetSiteID, biocomm, dir_sub))
           , FALSE)

    path <- file.path(dir_results, TargetSiteID, biocomm, dir_sub)

    skipflag = FALSE

    # Prep measured stressor data
    df_stress <- df_stress %>%
        dplyr::select(-StationID_Master, -IQRmethod, -SDmethod, -Outlier) %>%
        dplyr::select_if(not_all_na) %>%
        tidyr::gather(key = StdParamName, value = ResultValue
               , -StressSampID, -StressSampDate) %>%
        dplyr::filter(!is.na(ResultValue)) %>%
        dplyr::group_by(StressSampDate, StdParamName) %>%
        dplyr::summarize(meanval = signif(mean(ResultValue,na.rm=TRUE),digits=3)
                         , .groups = "drop_last") %>%
        dplyr::rename(SampDate = StressSampDate, variable = StdParamName) %>%
        dplyr::filter(variable %in% stressors)

    if (any(is.na(df_stress$SampDate))) {
        msg <- "NA values in Sample Date indicative of modeled stressor data."
        message(msg)
        # print(msg)
        # flush.console()
        df_NAs <- as.data.frame(dplyr::filter(df_stress, is.na(SampDate))) %>%
            dplyr::select(variable)
        df_stress <- dplyr::filter(df_stress, !is.na(SampDate)) # Removes modeled stressors, which have not date

        for (i in 1:nrow(df_NAs)) {
            stressNA <- df_NAs$variable[i]
            gapcomment <- "No date is available for modeled stressors."
            df.temp <- cbind.data.frame("getTimeSeq", stressNA, 0
                                     , gapcomment)
            if (i==1) {
                gaps <- df.temp
            } else {
                gaps <- rbind(gaps, df.temp)
            }
        }
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results, TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")

    }
    df_stressinfo <- unique(df_stressinfo[,c("Analyte","Label")])
    df_stress <- merge(df_stress, df_stressinfo, by.x = "variable", by.y = "Analyte")

    # Prep response data
    df_resp <- df_resp %>%
        dplyr::select_if(not_all_na) %>%
        dplyr::select(-StationID_Master
               , -Quality) %>%
        tidyr::gather(key = Biometric, value = ResultValue
               , -RespSampID, -RespSampDate) %>%
        dplyr::filter(!is.na(ResultValue)
               , Biometric %in% BioResp) %>%
        dplyr::group_by(RespSampDate, Biometric) %>%
        dplyr::summarize(meanval = signif(mean(ResultValue),digits=3), .groups = "drop_last") %>%
        dplyr::rename(SampDate = RespSampDate, variable = Biometric)
    df_respinfo <- unique(df_respinfo[,c("MetricName","MetricLabel")])
    df_resp <- merge(df_resp, df_respinfo, by.x = "variable", by.y = "MetricName")
    df_resp <- dplyr::rename(df_resp, Label=MetricLabel)
    
    skipflag <- ifelse(nrow(df_resp)==0,TRUE, FALSE)

    if (skipflag == FALSE) {

        # Ensure all data in one dataframe
        df.data <- rbind(as.data.frame(df_stress)
                         , as.data.frame(df_resp))

        minDate <- as.Date(min(df.data$SampDate)-30)
        maxDate <- as.Date(max(df.data$SampDate)+30)
        diffDate <- paste(round((maxDate - minDate)/10, 2),"days")
        # print(diffDate)
        # flush.console()

        # Loop over each stressor
        ppi = 300
        stresses <- unique(df_stress[,c("variable","Label")])
        count = 1
        
        for (s in 1:nrow(stresses)) {

            stressName = stresses[s,"variable"]
            stressLabel = as.character(stresses[s,"Label"])
            # print(paste0("s=",s," stressor is "))

            # Plot time series for stressor & bio response
            responses <- unique(df_resp[,c("variable","Label")])
            totplots <- nrow(stresses)*nrow(responses)
            for (r in 1:nrow(responses)) {

                respName = responses[r,"variable"]
                respLabel = as.character(responses[r,"Label"])

                fn = paste0(TargetSiteID, "_", biocomm, "_TS_", stressName, "_"
                            , respName, ".png")
                fpath = file.path(path, fn)

                df.plot <- df.data %>%
                    dplyr::filter(variable %in% c(stressName,respName))
                df.plot$variable <- factor(df.plot$variable
                                           , levels = c(stressName, respName))
                maxStress <- max(df.plot$meanval[df.plot$variable==stressName])
                maxResp <- max(df.plot$meanval[df.plot$variable==respName])
                
                msg <- paste0("Plotting bar graphs (", count, "/", totplots, ") ", stressName, " and "
                              , respName)
                message(msg)
                # print(msg)
                # flush.console()
                
                colwid =  nrow(df.plot) * 2

                p_ts <- ggplot2::ggplot(df.plot, ggplot2::aes(x=SampDate
                                            , y=as.numeric(meanval)))
                p_ts <- p_ts + ggplot2::geom_col(fill = "black", width = colwid
                             , position = ggplot2::position_dodge(preserve = "single"), na.rm = TRUE)
                p_ts <- p_ts + ggrepel::geom_text_repel(ggplot2::aes(label=meanval)
                                        , hjust= 2, vjust = 0, size=2.5, na.rm = TRUE)
                p_ts <- p_ts + ggplot2::facet_wrap(~ Label, ncol=1, scales="free_y")
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
                count = count + 1
            } # End loop over responses

        } # End loop over stressors
    } else {
        # print(paste("No ",biocomm,"response data available for", TargetSiteID))
        # flush.console()
        msg <- paste("No ",biocomm,"response data available for", TargetSiteID)
        message(msg)
    }

}
