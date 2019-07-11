## Ann Roseberry Lincoln
### Tetra Tech
### June 2019

### Objective: Combine stressor and response samples. 
### Keep both sample IDs, the response index, and stressor measurements.
### Create two files: one for measured data and one for modeled data.

rm(list=ls())

# library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

wd <- getwd()
dataDir <- "Data"

getCoOccurDataset <- function(df.sites = data_Sites
                              , df.mod = data.model.raw
                              , df.bmi = data.bmi.metrics
                              , df.meas = data.chem.raw
                              , index = "CSCI") {
    
    bmi.colnames <- c("BMISampDate", "BMISampID", "Quality", index)
    
    # Debug
    # df.mod <- data.model.raw
    # df.bmi <- data.bmi.metrics
    # df.meas <- data.chem.raw
    # index = "CSCI"
    
    # Read data files (stressor and response)
    df.bmi <- df.bmi[,c("StationID_Master", "SampDate", "BMISampID"
                        , "Quality", "IBI")]
    colnames(df.bmi) <- c("StationID_Master", bmi.colnames)
    
    # Clean up modeled data
    df.mod <- df.mod %>%
        select(StationID_Master, clust, StdParamName, ResultValue) %>%
        spread(key = StdParamName, value = ResultValue)
    mod.colnames <- names(df.mod)
    mod.colnames <- mod.colnames[!(mod.colnames %in% c("StationID_Master"
                                                       , "clust"))]
    
    # Merge Modeled data and response data, then with measured data
    df.modbmi <- merge(df.bmi, df.mod, by.x = "StationID_Master"
                       , by.y = "StationID_Master", all = TRUE)
    df.modbmi <- df.modbmi[,c("StationID_Master", "clust", bmi.colnames
                              , mod.colnames)]
    rm(df.mod, df.bmi)
    
    # Clean up measured data and convert to wide format
    df.meas <- df.meas[!is.na(df.meas$ResultValue),]
    df.meas <- df.meas %>% 
        select(StationID_Master, ChemSampleID, SampDate
               , StdParamName, ResultValue) %>%
        spread(key = StdParamName, value = ResultValue) %>%
        mutate(SampleDate = mdy(SampDate))
    meas.colnames <- names(df.meas)
    meas.colnames <- meas.colnames[!(meas.colnames %in% c("StationID_Master"
                                                          , "ChemSampleID"
                                                          , "SampleDate"))]
    
    # Merge site/bmi data with measure data by station & date
    df.coOccur <- merge(df.modbmi, df.meas
                        , by.x = c("StationID_Master", "BMISampDate")
                        , by.y = c("StationID_Master", "SampleDate")
                        , all = TRUE)
    df.coOccur <- df.coOccur[,c("StationID_Master", "ChemSampleID"
                                , "SampDate", bmi.colnames, mod.colnames
                                , meas.colnames)]
    df.sites <- df.sites[,c("StationID_Master", "clust")]
    df.coOccur <- merge(df.sites, df.coOccur, all = TRUE)
    # colnames(df.coOccur) <- c(bmi.colnames, ChemSampleID, SampleDate
    #                           , mod.colnames, meas.colnames)
    # df.coOccur <- df.coOccur %>%
    #     mutate(StressSampleDate = mdy(SampDate))
    # df.coOccur <- df.coOccur[,c("StationID_Master", "ChemSampleID"
    #                             , "SampDate", bmi.colnames
    #                             , mod.colnames, meas.colnames)]
    # df.coOccur <- select(df.coOccur, -SampDate)
    
    write.table(df.coOccur, file.path(wd,dataDir,"SMCCoOccurFinal.tab")
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
    
    return(df.coOccur)
    
}







