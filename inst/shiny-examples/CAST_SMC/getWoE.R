# Ann.Lincoln@tetratech.com, 20190430
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 

# Lines of evidence evaluated (possible):
# CO: Spatial/temporal co-occurrence 
# SR: Stressor-response relationships from the field
# VP: Verified predictions
# SR: Stressor-response relationships from other field studies
# SSD: Stressor-response relationships from laboratory studies

getWoE <- function(TargetSiteID
                   , df.rank = list.stressors$site.stressor.pctrank
                   , df.coOccur = data.bmi.coOccur
                   , biocomm = "bmi"
                   , index = "IBI"
                   , dir_results = file.path(getwd(), "Results")
                   , CO_sub = "CoOccurrence"
                   , SR_sub = "StressorResponse"
                   , VP_sub = "VerifiedPredictons"
                   , SSD_sub = "SSD") {

    subdir = TargetSiteID
    
    LoEcols <- c("StationID_Master", "ChemSampleID", "Bio.Deg", "Stressor"
                 , "StressorValue", "Response", "ResponseValue", "n", "LoEtrim"
                 , "LoE", "Analysis", "InOut", "Score", "biocomm")
    
    # Filenames for all files containing scores
    fn.COScores <- paste0(TargetSiteID, ".CoOccurrence.",tolower(biocomm)
                          ,".Scores.txt")
    fn.SRScores <- paste0(TargetSiteID, ".SR.",toupper(biocomm),".Scores.txt")
    fn.VPScores <- paste0(TargetSiteID, ".VP.",tolower(biocomm),".Scores.txt")
    # fn.SSDScores <- paste0(TargetSiteID, ".SSD.",toupper(biocomm),".Scores.txt")
    
    # Read all existing BMI score files first
    # Co-occurrence
    if (file.exists(file.path(dir_results, subdir, "CoOccurrence"
                              , fn.COScores))==TRUE) {
        df.CO <- read.table(file.path(dir_results, subdir, "CoOccurrence"
                                      , fn.COScores), header = TRUE, sep = "\t"
                            , stringsAsFactors = FALSE)
        colnames(df.CO) <- c("StationID_Master", "Cluster", "ResponseValue"
                             , "Bio.Nar", "Bio.Deg", "Stressor", "StressorValue"
                             , "n", "q25", "q50", "q75", "Sc_Box", "SR_pred_Deg"
                             , "SC_SR", "biocomm")
        df.CO <- df.CO[!is.na(df.CO$StressorValue),]
        df.CO <- unique(df.CO)
        
        keep.stress <- unique(df.CO$Stressor)
        
        data.coOccurTarget <- df.coOccur %>%
            select(StationID_Master, ChemSampleID, CSCI, keep.stress) %>%
            mutate(Index = CSCI) %>%
            filter(StationID_Master == TargetSiteID) %>%
            gather(keep.stress, key = "Stressor", value = "StressorValue") %>%
            filter(!is.na(StressorValue)) %>%
            select(StationID_Master, ChemSampleID, Index, Stressor
                   , StressorValue)
        data.coOccurTarget <- unique(data.coOccurTarget)
        
        df.CO <- merge(df.CO, data.coOccurTarget, by.x = c("StationID_Master"
                            , "ResponseValue", "Stressor", "StressorValue")
                       , by.y =  c("StationID_Master", "Index", "Stressor"
                                   , "StressorValue"))
        df.CO <- unique(df.CO)
        df.CO <- df.CO[,c("StationID_Master", "ChemSampleID", "Bio.Nar"
                          , "Bio.Deg", "ResponseValue", "Stressor"
                          , "StressorValue", "n", "Sc_Box", "SC_SR", "biocomm")]
        
        # Pull out co-occurrence scores from co-occurrence file
        df.CO.1 <- df.CO %>%
            select(StationID_Master, ChemSampleID, Bio.Deg, ResponseValue
                   , Stressor, StressorValue, n, Sc_Box, biocomm) %>%
            mutate(Response = index
                   , LoEtrim = "CO_boxplot"
                   , LoE = "Co-occurrence"
                   , Analysis = "Box plot"
                   , InOut = "Inside the case")
        
        df.CO.1 <- df.CO.1[!is.na(df.CO.1$Sc_Box),]
        colnames(df.CO.1)[8] <- "Score"
        df.CO.1 <- df.CO.1[,LoEcols]
        
        # Pull out the SR logistic regression scores from co-occurrence file
        df.SRlog <- df.CO %>%
            select(StationID_Master, ChemSampleID, Bio.Deg, ResponseValue
                   , Stressor, StressorValue, n, SC_SR, biocomm) %>%
            mutate(Response = index, LoEtrim = "SR_InCase_LogRegr"
                   , LoE = "Stressor-response in the case"
                   , Analysis = "Logistic regression"
                   , InOut = "Inside the case")
        
        df.SRlog <- df.SRlog[!is.na(df.SRlog$SC_SR),]
        colnames(df.SRlog)[8] <- "Score"
        df.SRlog <- df.SRlog[,LoEcols]
        
    } else {
        print(paste(biocomm, "co-occurrence scores unavailable."))
        flush.console()
    } # Co-occurrence scores collated.
    
    # Stressor response (inside & outside case, from field)
    if (file.exists(file.path(dir_results, subdir, "StressorResponse"
                              , fn.SRScores))==TRUE) {
        df.SR <- read.table(file.path(dir_results, subdir, "StressorResponse"
                                      , fn.SRScores), header = TRUE, sep = "\t"
                            , stringsAsFactors = FALSE)
        colnames(df.SR) <- c("StationID_Master", "biocomm", "Stressor", "Response"
                             , "n_site", "n_all", "SRscore.all", "n_cluster"
                             , "SRscore.cluster")
        df.SR <- merge(df.SR, unique(df.CO.1[,c("StationID_Master", "ChemSampleID"
                                                , "ResponseValue", "Bio.Deg"
                                                , "Stressor", "StressorValue")])
                       , by.x = c("StationID_Master", "Stressor")
                       , by.y = c("StationID_Master", "Stressor"))
        df.SR <- unique(df.SR)
        
        # Get unique responses to determine if index name exists
        responses <- as.vector(unique(df.SR$Response))
        if (index %in% responses) {
            # Pull out the stressor response linear regression scores for the IBI
            # NOTE that I've hardcoded IBI, but it really should be a variable w/value=IBI
            df.SRin <- df.SR %>%
                select(StationID_Master, ChemSampleID, Bio.Deg, ResponseValue
                       , Stressor, StressorValue, Response, n_cluster
                       , SRscore.cluster, biocomm) %>%
                filter(Response == index) %>%
                mutate(LoEtrim = "SR_InCase_LinRegr"
                       , LoE = "Stressor-response in the case"
                       , Analysis = "Linear regression"
                       , InOut = "Inside the case")
            df.SRin <- df.SRin[!is.na(df.SRin$SRscore.cluster),]
            colnames(df.SRin)[8:9] <- c("n", "Score")
            df.SRin <- df.SRin[,LoEcols]
            
            df.SRout <- df.SR %>%
                select(StationID_Master, ChemSampleID, Bio.Deg, ResponseValue
                       , Stressor, StressorValue, Response, n_all, SRscore.all
                       , biocomm) %>%
                filter(Response == index) %>%
                mutate(LoEtrim = "SR_OutCase_LinRegr"
                       , LoE = "Stressor-response outside the case"
                       , Analysis = "Linear regression"
                       , InOut = "Outside the case")
            df.SRout <- df.SRout[!is.na(df.SRout$SRscore.all),]
            colnames(df.SRout)[8:9] <- c("n", "Score")
            df.SRout <- df.SRout[,LoEcols]
            
            # Pull out the stressor response linear regression scores for the metrics
            # NOTE that I've hardcoded IBI, but it really should be a variable w/value=IBI
            df.SRin.met <- df.SR %>%
                select(StationID_Master, ChemSampleID, Bio.Deg, ResponseValue
                       , Stressor, StressorValue, Response, n_cluster
                       , SRscore.cluster, biocomm) %>%
                filter(Response != index) %>%
                mutate(LoEtrim = "SR_InCase_LinRegr"
                       , LoE = "Stressor-response in the case"
                       , Analysis = "Linear regression"
                       , InOut = "Inside the case")
            df.SRin.met <- df.SRin.met[!is.na(df.SRin.met$SRscore.cluster),]
            colnames(df.SRin.met)[8:9] <- c("n", "Score")
            df.SRin.met <- df.SRin.met[,LoEcols]
            
            df.SRout.met <- df.SR %>%
                select(StationID_Master, ChemSampleID, Bio.Deg, ResponseValue
                       , Stressor, StressorValue, Response, n_all, SRscore.all
                       , biocomm) %>%
                filter(Response != index) %>%
                mutate(LoEtrim = "SR_OutCase_LinRegr"
                       , LoE = "Stressor-response outside the case"
                       , Analysis = "Linear regression"
                       , InOut = "Outside the case")
            df.SRout.met <- df.SRout.met[!is.na(df.SRout.met$SRscore.all),]
            colnames(df.SRout.met)[8:9] <- c("n", "Score")
            df.SRout.met <- df.SRout.met[,LoEcols]
        } else {
            print("No index value.")
            flush.console()
        }
    } else {
        print(paste(biocomm, "stressor-response scores unavailable."))
        flush.console()
    } # Stressor-response scores collated.
    
    # Verified prediction (using stressor-specific tol vals)
    if (file.exists(file.path(dir_results, subdir, "VerifiedPredictions"
                              , fn.VPScores))==TRUE) {
        df.VP <- read.table(file.path(dir_results, subdir, "VerifiedPredictions"
                                      , fn.VPScores), sep = "\t"
                            , stringsAsFactors = FALSE, header = TRUE)
        df.VP <- df.VP %>%
            mutate(LoEtrim = "VP_boxplot"
                   , LoE = "Verified prediction"
                   , Analysis = "Box plot"
                   , InOut = "Inside the case"
                   , n = n_BetterBio
                   , Stressor = Param_Name
                   , StressorValue = Param_Value
                   , Response = variable
                   , ResponseValue = value) %>%
            select(StationID_Master, ChemSampleID, Stressor, StressorValue
                   , Response, ResponseValue, n, LoEtrim, LoE, Analysis, InOut
                   , Score, biocomm)
        df.VP <- merge(df.VP, unique(df.CO.1[,c("ChemSampleID", "Bio.Deg")]))
        df.VP <- df.VP[,LoEcols]
        df.VP <- df.VP[!is.na(df.VP$Score),]
        df.VP <- unique (df.VP)
        
    } else {
        print(paste(biocomm, "verified prediction scores unavailable."))
        flush.console()
    } # Verified prediction scores collated.
    
    # # Stressor-specific relationships from lab studies (SSDs)
    # if (file.exists(file.path(dir_results,subdir,"SSDs",fn.SSDScores))==TRUE) {
    #     df.SSD <- read.table(file.path(dir_results,subdir,"SSDs",fn.SSDScores)
    #                          , header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    #     
    #     # Pull out SSD scores from SSD file
    #     # Response should be name for taxa both expected to be extirpated 
    #     # and expected to be observed
    #     # n should maybe be the relative abundance (or absolute?) when found (score = -1)
    #     df.SSD1 <- df.SSD %>%
    #         select(StationID_Master, Stressor, Response, n, Sc_SSD, biocomm) %>%
    #         mutate(LoEtrim = "SR_lab_SSD"
    #                , LoE = "Stressor response from lab studies"
    #                , Analysis = "SSD curve"
    #                , InOut = "Outside the case")
    #     
    #     df.SSD1 <- df.SSD1[!is.na(df.SSD1$Sc_SSD),]
    #     colnames(df.SSD1)[4] <- "Score"
    #     df.SSD1 <- df.SSD1[,LoEcols]
    # } else {
    #     print(paste(biocomm, "SSD scores unavailable."))
    #     flush.console()
    # }
    
    # Combine all index-level scores into one data frame
    if (exists("df.CO.1")==TRUE) {
        if (exists("df.scores")==TRUE) {
            df.scores <- rbind(df.scores, df.CO.1)
        } else {
            df.scores <- df.CO.1
        }
    }
    
    if (exists("df.SRlog")==TRUE) {
        if (exists("df.scores")==TRUE) {
            df.scores <- rbind(df.scores, df.SRlog)
        } else {
            df.scores <- df.SRlog
        }
    }
    
    if (exists("df.SRin")==TRUE) {
        if (exists("df.scores")==TRUE) {
            df.scores <- rbind(df.scores, df.SRin)
        } else {
            df.scores <- df.SRin
        }
    }
    
    if (exists("df.SRout")==TRUE) {
        if (exists("df.scores")==TRUE) {
            df.scores <- rbind(df.scores, df.SRout)
        } else {
            df.scores <- df.SRout
        }
    }
    
    if (exists("df.VP")==TRUE) {
        if (exists("df.scores")==TRUE) {
            df.scores <- rbind(df.scores, df.VP)
        } else {
            df.scores <- df.VP
        }
    }
    
    if (exists("df.SSD")==TRUE) {
        if (exists("df.scores")==TRUE) {
            df.scores <- rbind(df.scores, df.SSD)
        } else {
            df.scores <- df.SSD
        }
    }
    
    # Get Chem Info for all possible stressors
    data.chem.info.trim <- data.chem.info %>% 
        mutate(Analyte = StdParamName) %>% 
        select(StdParamName, GroupNum, GroupName)
    data.chem.info.trim <- unique(data.chem.info.trim)
    
    df.scores <- merge(df.scores, data.chem.info.trim, by.x = "Stressor"
                       , by.y = "StdParamName")
    df.scores <- df.scores[,c("StationID_Master", "Bio.Deg", "ChemSampleID"
                              , "GroupNum", "GroupName", "Stressor", "StressorValue"
                              , "Response", "ResponseValue", "n", "LoEtrim", "LoE"
                              , "Analysis", "InOut","Score", "biocomm")]
    
    df.LoEs <- unique(df.scores[,c("LoEtrim", "LoE", "Analysis", "InOut")])
    write.table(df.LoEs, file.path(dir_results, subdir, "Lkp_LOEdescriptions.tab")
                , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
    
    # Create subset of data (to not destroy full dataset)
    df.scores2 <- df.scores %>%
        select(StationID_Master, Bio.Deg, GroupNum, GroupName, ChemSampleID
               , Stressor, StressorValue, Response, ResponseValue, LoEtrim
               , Score, biocomm) %>%
        arrange(ChemSampleID, GroupNum, Stressor, LoEtrim)
    
    # Get % rank info for all stressors
    keep.stress <- unique(df.scores2$Stressor)
    df.rank <- df.rank[,c("StationID_Master", "ChemSampleID", keep.stress)]
    df.rank <- df.rank %>%
        gather(key = "Stressor"
               , value = "PctRank"
               , -StationID_Master
               , -ChemSampleID)
    
    df.scores3 <- merge(df.rank, df.scores2
                        , by.x = c("StationID_Master", "ChemSampleID", "Stressor")
                        , by.y = c("StationID_Master", "ChemSampleID", "Stressor")
                        , all.y = TRUE)
    df.scores3 <- unique(df.scores3)

    # Cast data into wide format, as opposed to long
    df.scores.wide.all <- df.scores3 %>%
        spread(key = LoEtrim, value = Score) %>%
        arrange(ChemSampleID, GroupNum, Stressor, PctRank)
    
    if ("VP_boxplot" %in% colnames(df.scores.wide.all)) {
        df.scores.wide.all <- df.scores.wide.all %>%
            mutate(VP_boxplot = ifelse(!is.na(VP_boxplot)==TRUE
                                       , VP_boxplot, -9)) %>%
            group_by(StationID_Master, Bio.Deg, GroupNum, GroupName, ChemSampleID
                     , Stressor, StressorValue, PctRank, Response, ResponseValue) %>%
            summarize(CO_boxplot = sum(CO_boxplot, na.rm = TRUE)
                      , SR_InCase_LogRegr = sum(SR_InCase_LogRegr, na.rm = TRUE)
                      , SR_InCase_LinRegr = sum(SR_InCase_LinRegr, na.rm = TRUE)
                      , SR_OutCase_LinRegr = sum(SR_OutCase_LinRegr, na.rm = TRUE)
                      , VP_boxplot = sum(VP_boxplot)) %>%
            mutate(VP_boxplot = ifelse(VP_boxplot == -7, 2, VP_boxplot)
                   , VP_boxplot = ifelse(VP_boxplot == -8, 1, VP_boxplot)
                   , VP_boxplot = ifelse(VP_boxplot == -10, -1, VP_boxplot)
                   , VP_boxplot = ifelse(VP_boxplot == -11, -2, VP_boxplot)
                   , VP_boxplot = ifelse(VP_boxplot == -18, "NE", VP_boxplot))
        # NOTE: VP_boxplot = -9 means either indeterminate or NE. Cannot distinguish)
    } else {
        df.scores.wide.all <- df.scores.wide.all %>%
             group_by(StationID_Master, Bio.Deg, GroupNum, GroupName
                      , ChemSampleID, Stressor, StressorValue, PctRank
                      , Response, ResponseValue) %>%
            summarize(CO_boxplot = sum(CO_boxplot, na.rm = TRUE)
                      , SR_InCase_LogRegr = sum(SR_InCase_LogRegr, na.rm = TRUE)
                      , SR_InCase_LinRegr = sum(SR_InCase_LinRegr, na.rm = TRUE)
                      , SR_OutCase_LinRegr = sum(SR_OutCase_LinRegr
                                                     , na.rm = TRUE))
    }
    
    # Acquire LoEs for inside & outside case in separate vectors
    InsideCaseLoE <- as.vector(df.LoEs$LoEtrim[df.LoEs$InOut=="Inside the case"])
    OutsideCaseLoE <- as.vector(df.LoEs$LoEtrim[df.LoEs$InOut=="Outside the case"])
    
    # Add columns and calculate total LoEs support/refute/indeterminate/NE
    df.scores.wide.all <- df.scores.wide.all %>%
        mutate(NumLoEIn_Support = NA
               , NumLoEIn_Refute = NA
               , NumLoEIn_Indeterm = NA
               , NumLoEIn_NE = NA
               , NumLoEOut_Support = NA
               , NumLoEOut_Refute = NA
               , NumLoEOut_Indeterm = NA
               , NumLoEOut_NE = NA)
    
    for (r in 1:nrow(df.scores.wide.all)) {
        NumSupportInside = 0
        NumRefuteInside = 0
        NumIndeterminateInside = 0
        NumNEInside = 0
        
        NumSupportOutside = 0
        NumRefuteOutside = 0
        NumIndeterminateOutside = 0
        NumNEOutside = 0
        
        for (i in 1:length(InsideCaseLoE)) {
            namecol = InsideCaseLoE[i]
            if (df.scores.wide.all[r,namecol] == 1) {
                NumSupportInside = NumSupportInside + 1
            } else if (df.scores.wide.all[r,namecol] == 2) {
                NumSupportInside = NumSupportInside + 1
            } else if (df.scores.wide.all[r,namecol] == -1) {
                NumRefuteInside = NumRefuteInside + 1
            } else if (df.scores.wide.all[r,namecol] == -2) {
                NumRefuteInside = NumRefuteInside + 1
            } else if (df.scores.wide.all[r,namecol] == 0) {
                NumIndeterminateInside = NumIndeterminateInside + 1
            } else {
                NumNEInside = NumNEInside + 1
            }
        }
        for (i in 1:length(OutsideCaseLoE)) {
            namecol = OutsideCaseLoE[i]
            if ((df.scores.wide.all[r,namecol] == 1) ||
                (df.scores.wide.all[r,namecol] == 2)) {
                NumSupportOutside = NumSupportOutside + 1
            } else if ((df.scores.wide.all[r,namecol] == -1) ||
                       (df.scores.wide.all[r,namecol] == -2)) {
                NumRefuteOutside = NumRefuteOutside + 1
            } else if (df.scores.wide.all[r,namecol] == 0) {
                NumIndeterminateOutside = NumIndeterminateOutside + 1
            } else {
                NumNEOutside = NumNEOutside + 1
            }
        }
        
        df.scores.wide.all$NumLoEIn_Support[r] <- NumSupportInside
        df.scores.wide.all$NumLoEIn_Refute[r] <- NumRefuteInside
        df.scores.wide.all$NumLoEIn_Indeterm[r] <- NumIndeterminateInside
        df.scores.wide.all$NumLoEIn_NE[r] <- NumNEInside
        
        df.scores.wide.all$NumLoEOut_Support[r] <- NumSupportOutside
        df.scores.wide.all$NumLoEOut_Refute[r] <- NumRefuteOutside
        df.scores.wide.all$NumLoEOut_Indeterm[r] <- NumIndeterminateOutside
        df.scores.wide.all$NumLoEOut_NE[r] <- NumNEOutside
    }
    
    # Combine LoE (sample/stressor level) into WoE
    df.scores.wide.all <- df.scores.wide.all %>%
        mutate(TotSupport = abs(NumLoEIn_Support + NumLoEOut_Support)
               , TotRefute = abs(NumLoEIn_Refute + NumLoEOut_Refute)
               , TotIndeterminate = abs(NumLoEIn_Indeterm +
                                            NumLoEOut_Indeterm)
               , TotNE = abs(NumLoEIn_NE + NumLoEOut_NE)
               , WoE = ifelse(TotIndeterminate > (TotSupport + TotRefute)
                              , "Indeterminate"
                              , ifelse(TotSupport > TotRefute, "Supports"
                                       , ifelse(TotRefute > TotSupport, "Refutes"
                                                , "Indeterminate"))))
    df.scores.wide.all <- unique(df.scores.wide.all)
    
    # Write table containing all index-level scores (sample/stressor level)
    fn.detail.scores <- paste0(TargetSiteID,".WoEdetail.",biocomm,".", index, ".tab")
    write.table(df.scores.wide.all, file = file.path(dir_results, subdir
                                                     , fn.detail.scores)
                , append = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)
    
    df.scores.stressor <- as.data.frame(df.scores.wide.all) %>%
        select(StationID_Master, Bio.Deg, GroupName, Stressor, StressorValue, WoE) %>%
        group_by(StationID_Master, Bio.Deg, GroupName, Stressor, WoE) %>%
        summarize(nSamples = n()
                  , minStressor = min(StressorValue, na.rm = TRUE)
                  , meanStressor = mean(StressorValue, na.rm = TRUE)
                  , maxStressor = max(StressorValue, na.rm = TRUE)) 
    
    # Write table containing all index-level scores (sample/stressor level)
    fn.stressor.scores <- paste0(TargetSiteID, ".WoEstressor.", biocomm, "."
                                 , index, ".tab")
    write.table(df.scores.stressor, file = file.path(dir_results, subdir
                                                     , fn.stressor.scores)
                , append = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)
    
    # Gather metric-level scores into one data frame
    df.SRin.met.gps <- merge(df.SRin.met, data.chem.info, by.x = "Stressor"
                             , by.y = "StdParamName")
    df.SRin.met.gps <- df.SRin.met.gps[,c(2,9:10,1,3:8)]
    df.SRout.met.gps <- merge(df.SRout.met, data.chem.info, by.x = "Stressor"
                              , by.y = "StdParamName")
    df.SRout.met.gps <- df.SRout.met.gps[,c(2,9:10,1,3:8)]
    df.SR.met.gps <- rbind(df.SRin.met.gps, df.SRout.met.gps)
    
    # Write table containing all metric-level scores
    fn.metric.scores <- paste0(TargetSiteID,".WoE.metrics.",biocomm,".tab")
    write.table(df.SR.met.gps, file = file.path(dir_results, subdir, fn.metric.scores)
                , append = FALSE, sep = "\t", col.names = TRUE, row.names = FALSE)
}
# 
# rm(list = ls())

