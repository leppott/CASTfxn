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
                        , stresstype = "meas"
                        , df_stress
                        , df_stressinfo
                        , biocomm = "bmi"
                        , df_biometrics
                        , lagdays = 0) {

    # For QC purposes
    # TargetSiteID = TargetSiteID
    # compSites = list.CompSites$comp.sites
    # stresstype = "meas"
    # df_stress = data.chem.raw
    # df_stressinfo = data.chem.info
    # OR
    # stresstype = "mod"
    # df_stress = data.model.raw
    # df_stressinfo = data.model.info
    # AND
    # biocomm = "bmi"
    # df_biometrics = data.bmi.metrics
    # lagdays = 90
    # OR
    # biocomm = "alg"
    # df_biometrics = data.alg.metrics
    # lagdays = 0
    
    # Define pipe
    `%>%` <- dplyr::`%>%`
    
    # Get vector of parameters detected at target site
    detect.site <- df_stress %>%
        dplyr::filter(StationID_Master == TargetSiteID
               , !is.na(ResultValue)) %>%
        dplyr::select(StdParamName)
    detect.site <- as.vector(detect.site$StdParamName)
    
    # Convert data to wide format
    if (stresstype == "meas") {
        all.stress <- df_stress %>%
            dplyr::select(StationID_Master, ChemSampleID, SampleDate, StdParamName
                   , ResultValue) %>%
            dplyr::filter(StdParamName %in% detect.site) %>%
            tidyr::spread(key = StdParamName, value = ResultValue)
        cl.stress <- all.stress[all.stress$StationID_Master %in% compSites,]
        site.stress <- all.stress[all.stress$StationID_Master == TargetSiteID,]
        
        all.str.core <- all.stress[,c("StationID_Master", "ChemSampleID"
                                      , "SampleDate")]
        
        # Get bmi matches if bmi are specified
        if (biocomm == "bmi") {
            bio.metr.core <- df_biometrics %>%
                dplyr::select(StationID_Master, BMI.Metrics.SampID, Quality, SampDate) %>%
                dplyr::mutate(LagDate = SampDate - lagdays)
            all.match.str <- fuzzyjoin::fuzzy_right_join(all.str.core, bio.metr.core
                                              , by = c("StationID_Master" = "StationID_Master"
                                                       , "SampleDate" = "SampDate"
                                                       , "SampleDate" = "LagDate")
                                              , match_fun = list(`==`, `<=`, `>=`)) %>%
                dplyr::filter(!is.na(StationID_Master.x)) %>%
                dplyr::rename(StationID_Master = StationID_Master.y)
            all.match.str <- all.match.str[,c("StationID_Master", "ChemSampleID"
                                              , "SampleDate", "BMI.Metrics.SampID"
                                              , "SampDate", "LagDate", "Quality")]
            all.b.str <- unique(merge(all.match.str, all.stress
                               , by.x = c("StationID_Master", "ChemSampleID"
                                          , "SampleDate")
                               , by.y = c("StationID_Master", "ChemSampleID"
                                          , "SampleDate")
                               , all = TRUE))
            all.b.str <- all.b.str[!is.na(all.b.str$BMI.Metrics.SampID),]
            cl.b.str <- all.b.str[all.b.str$StationID_Master %in% compSites,]
            site.b.str <- all.b.str[all.b.str$StationID_Master == TargetSiteID,]
            
            all.b.rsp <- unique(merge(all.match.str, df_biometrics
                               , by.x = c("StationID_Master", "BMI.Metrics.SampID"
                                          , "SampDate", "Quality")
                               , by.y = c("StationID_Master", "BMI.Metrics.SampID"
                                          , "SampDate", "Quality")
                               , all = TRUE))
            all.b.rsp <- all.b.rsp[!is.na(all.b.rsp$ChemSampleID),]
            cl.b.rsp <- all.b.rsp[all.b.rsp$StationID_Master %in% compSites,]
            site.b.rsp <- all.b.rsp[all.b.rsp$StationID_Master == TargetSiteID,]
            
            # Identify detects for which no matched biology is available
            nomatch.b.gap <- dplyr::setdiff(colnames(site.b.str), colnames(site.b.str))
            
            # Identify comparator samples
            cl.samps <- cl.b.str[,c("StationID_Master", "ChemSampleID")]
            cl.samps <- cl.samps[cl.samps$StationID_Master %in% compSites,]
        }
        # Get algal matches, if algae are specified
        if (biocomm == "alg") {
            # Stuff goes here
        }  
        
    } else if (stresstype == "mod") {
        
        # If stresstype = mod, then lagdays must be zero, as date is not used
        lagdays = 0
            
        all.stress <- df_stress %>%
            dplyr::select(StationID_Master, ChemSampleID, StdParamName, ResultValue) %>%
            dplyr::filter(StdParamName %in% detect.site) %>%
            tidyr::spread(key = StdParamName, value = ResultValue)
        cl.stress <- all.stress[all.stress$StationID_Master %in% compSites,]
        site.stress <- all.stress[all.stress$StationID_Master == TargetSiteID,]
        
        all.str.core <- all.stress[,c("StationID_Master", "ChemSampleID")]

        # Get bmi matches if bmi are specified
        if (biocomm == "bmi") {
            bio.metr.core <- df_biometrics %>%
                dplyr::select(StationID_Master, BMI.Metrics.SampID, Quality, SampDate) %>%
                dplyr::mutate(LagDate = SampDate - lagdays)
            all.match.str <- merge(all.str.core, bio.metr.core
                                   , by.x = "StationID_Master"
                                   , by.y = "StationID_Master"
                                   , all = TRUE)
            all.match.str <- unique(all.match.str)
            all.match.str <- all.match.str %>%
                dplyr::mutate(SampleDate = SampDate) %>%
                dplyr::select(StationID_Master, ChemSampleID, SampleDate
                       , BMI.Metrics.SampID, SampDate, LagDate, Quality)
            all.b.str <- unique(merge(all.match.str, all.stress
                               , by.x = c("StationID_Master", "ChemSampleID")
                               , by.y = c("StationID_Master", "ChemSampleID")
                               , all = TRUE))
            all.b.str <- all.b.str[!is.na(all.b.str$BMI.Metrics.SampID),]
            cl.b.str <- all.b.str[all.b.str$StationID_Master %in% compSites,]
            site.b.str <- all.b.str[all.b.str$StationID_Master == TargetSiteID,]
            
            all.b.rsp <- unique(merge(all.match.str, df_biometrics
                               , by.x = c("StationID_Master", "BMI.Metrics.SampID"
                                          , "SampDate", "Quality")
                               , by.y = c("StationID_Master", "BMI.Metrics.SampID"
                                          , "SampDate", "Quality")
                               , all = TRUE))
            all.b.rsp <- all.b.rsp[!is.na(all.b.rsp$ChemSampleID),]
            cl.b.rsp <- all.b.rsp[all.b.rsp$StationID_Master %in% compSites,]
            site.b.rsp <- all.b.rsp[all.b.rsp$StationID_Master == TargetSiteID,]

            # Identify detects for which no matched biology is available
            nomatch.b.gap <- setdiff(colnames(site.b.str), colnames(site.b.str))
            
            # Identify comparator samples
            cl.samps <- cl.b.str[,c("StationID_Master", "ChemSampleID")]
            cl.samps <- cl.samps[cl.samps$StationID_Master %in% compSites,]
        }
        # Get algal matches, if algae are specified
        if (biocomm == "alg") {
            # Stuff goes here
        }
        
    }
    
    df_stressinfo <- df_stressinfo %>% 
        dplyr::rename(ConvertTo = Analyte) %>%
        dplyr::filter(StdParamName %in% detect.site) %>%
        dplyr::select(ConvertTo, ANALYSIS_TYPE, CHEMICAL_NAME, FinalUnit, StdParamName
               , GroupNum, GroupName, LogTransf, SSD, SSTV, SensMin, SensMax
               , TolMin, TolMax, UseInStressorID, DirIncStress)
    
    
    # Return both unmatched and matched data as output
    if (biocomm == "bmi") {
        mySubsets <- list(cluster.samps = cl.samps
                          , chem.info = df_stressinfo
                          , all.b.str = all.b.str
                          , all.b.rsp = all.b.rsp
                          , nomatch.b.meas = nomatch.b.gap)
    } else if (biocomm == "alg") {
        mySubsets <- list(cluster.samps = cl.samps
                          , chem.info = df_stressinfo
                          , all.a.str = all.a.str
                          , all.a.rsp = all.a.rsp
                          , nomatch.a.meas = nomatch.a.gap)
    }

    return(mySubsets)
    
}
