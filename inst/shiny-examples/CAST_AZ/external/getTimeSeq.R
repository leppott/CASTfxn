# Ann.Lincoln@tetratech.com, 20190430
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 

# This function will get site stressor and response data and prepare
# faceted time sequence graphics (stressor/response, on atop the other)
# All stressor/response data will be graphed, not just paired data

# Requirements: target site stressor data with date; 
#               target site response data with dates;
#               target site stressor list

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggrepel)

getTimeSeq <- function(TargetSiteID
                       , stressors
                       , biocomm = "BMI"
                       , BioResp = BMImetrics
                       , df.stress = data.chem.raw
                       , df.resp = data.bmi.metrics
                       , colname.SampID = "BMI.Metrics.SampID"
                       , dir_results = file.path(getwd(),"Results")
                       , dir_sub = "TemporalSequence") {

    
    # TargetSiteID
    # stressors
    # biocomm = "BMI"
    # BioResp = BMImetrics
    # df.stress = data.chem.raw
    # df.resp = data.bmi.metrics
    # colname.SampID = "BMI.Metrics.SampID"
    # dir_results = file.path(getwd(),"Results")
    # dir_sub = "TemporalSequence"    

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
    
    # Prep stressor data ### AZ used SampDate at this point, CA uses CollDate
    df.stress <- df.stress %>%
        filter(StationID_Master == TargetSiteID) %>%
        filter(StdParamName %in% stressors) %>%
        mutate(variable = StdParamName
               , value = ResultValue
               , SampID = ChemSampleID
               , SampleDate = mdy(SampDate)) %>%
        select(SampleDate, variable, value) %>%
        group_by(SampleDate, variable) %>%
        summarize(meanval = formatC(signif(mean(value),digits=3)
                                , digits=3,format="fg", flag="#"))
    df.stress$variable <- as.character(df.stress$variable)
    
    # Prep response data
    if (biocomm == "BMI") {
        df.resp <- df.resp %>%
            filter(StationID_Master == TargetSiteID) %>%
            mutate(SampID = BMI.Metrics.SampID
                   , SampleDate = mdy(CollDate))
    } else if (biocomm == "Algae") {
        df.resp <- df.resp %>%
            filter(StationID_Master == TargetSiteID) %>%
            mutate(SampID = Algae.Metrics.SampID
                   , SampleDate = mdy(CollDate))
    } else {
        warn(paste(biocomm,"is not a valid option."))
    }
    skipflag <- ifelse(nrow(df.resp)==0,TRUE, FALSE)
    
    if (skipflag == FALSE) {
        df.resp <- df.resp[,c("SampleDate", BioResp)]
        df.resp <- df.resp %>%
            gather(key = "variable", value = "value", -SampleDate) %>%
            group_by(SampleDate, variable) %>%
            summarize(meanval = formatC(signif(mean(value),digits=3)
                                        , digits=3,format="fg", flag="#"))
        df.resp$variable <- as.character(df.resp$variable)

        # Ensure all data in one dataframe
        df.data <- rbind(df.stress, df.resp)
        
        minDate <- min(df.data$SampleDate)-30
        maxDate <- max(df.data$SampleDate)+30
        diffDate <- paste(round((maxDate - minDate)/10, 2),"days")
        print(diffDate)
        flush.console()

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
                    filter(variable %in% c(stressName,respName))
                df.plot$variable <- factor(df.plot$variable
                                           , levels = c(stressName, respName))
                maxStress <- max(df.plot$meanval[df.plot$variable==stressName])
                maxResp <- max(df.plot$meanval[df.plot$variable==respName])
                
                print(paste("Plotting bar graphs for", stressName, "and", respName))
                flush.console()

                ggplot(df.plot, aes(x=SampleDate, y=as.numeric(meanval))) +
                    geom_col(fill = "black", width = 2
                             , position = position_dodge(preserve = "single")) +
                    geom_text_repel(aes(label=meanval), hjust= 2, vjust = 0
                                    , size=2.5) +
                    facet_wrap(~ variable, ncol=1, scales="free_y") +
                    theme_bw() + theme(axis.text.x = element_text(angle = 90
                                       , hjust = 1, size = 8)
                                       , panel.grid.minor = element_blank()) +
                    scale_x_date(limits=c(minDate,maxDate)
                                 , date_labels = "%m/%d/%Y"
                                 , date_breaks = diffDate) +
                    labs(title = paste(TargetSiteID
                                       ,"Stressor/Response Time Series")
                         , x = "Sample Date", y = "Value") +
                ggsave(filename=fpath, dpi = ppi, width=8, height=6, units="in")
            }
        }
    } else {
        print(paste("No ",biocomm,"response data available for", TargetSiteID))
        flush.console()
    }
    
}
