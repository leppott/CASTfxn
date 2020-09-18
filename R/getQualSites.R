#  Copyright 2020 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents 
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Get Quality Sites
#' 
#' @description Get quality sites where quality is defined in any of three ways:
#' reference, not degraded, or having better biology than the worst target site sample.
#' 
#' @details Generates a vector for each measure of quality for sites and reaches
#' in the entire data set and separately in the comparator data set, and also 
#' vectors for the samples having better biology in the entire data set and 
#' separately in the comparatory data set.
#' Inprovements: Add data gap analysis. In particular, how many samples are 
#' better than the min target sample score, and is that enough?
#' 
#' Uses the library dplyr.
#' 
#' @param TargetSiteID Site ID
#'  @param df_sites Sites table containing site ids and reference flags. 
#' Default = "data_Sites".
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param df_qual Biological index and metrics data for the specified biocomm. 
#' Default = "data.bmi.metrics". 
#' @param colBio Name of the column for the biological response index measure; Default = "CSCI".
#' @param colBioSample Column name for biological sample; Default = "RespSampID" 
#' @param ColStressSample Column name for Stress sample ID; Default = StressSampID 
#' @param comp_sites Vector containing comparator site identifiers.
#' @param useBC Boolean for using Bray Curtis model.  Default = FALSE
#' @param BioNarBrk Breaks for cut function for biological index.  CA CSCI Default is =  
#' c(-2, 0.62, 0.799, 0.919, 2)
#' @param BioNarLab Labels for biological index.  CA CSCI Default =  c("very likely altered"
#' , "likely altered", "possibly altered", "likely intact") 
#' @param BioDegBrk Biological assessment degraded status, cut function breaks. 
#' Should be in order from bad (low) to good (high). 
#' Default = c(-2, 0.799, 2)
#' @param BioDegLab Biological assessment degraded status, cut function labels. 
#' Should be in order from bad (low) to good (high).
#' Defaults are referenced in the code so if change the code will break. 
#' Default = c("Yes", "No").
#' @param dir_results directory for results; Default = ./Results
#' 
#' @return A list with vectors containing all sites or comparator sites that are
#' reference, not degraded ("good"), or having samples with index scores better 
#' than the minimum target site index score. Also, vectors containing similar 
#' data for reaches, and all samples or comparator samples with better index scores. 
#' 
#' @keywords internal
#' 
#' @export
getQualSites<- function(TargetSiteID
                        , df_sites = data_Sites
                        , biocomm = "bmi"
                        , df_qual = data_bmiCoOccur
                        , colBio = "CSCI"
                        , colBioSample = "RespSampID"
                        , colStressSample = "StressSampID"
                        , comp_sites = NULL
                        , useBC = FALSE
                        , BioNarBrk = c(-2, 0.62, 0.799, 0.919, 2)
                        , BioNarLab = c("very likely altered", "likely altered"
                                        , "possibly altered", "likely intact")
                        , BioDegBrk = c(-2, 0.799, 2)
                        , BioDegLab = c("Yes", "No")
                        , dir_results = file.path(getwd(), "Results")) {
    
    # For QC purposes
    boo_DEBUG <- FALSE
    
    if (boo_DEBUG == TRUE) {
        TargetSiteID
        df_sites = data_Sites
        biocomm = bioComm
        df_qual = data_bioCoOccur
        colBio = bioIndex
        colBioSample = "RespSampID"
        colStressSample = "StressSampID"
        comp_sites = comp_sites
        useBC = TRUE
        BioNarBrk = BioNarBrk
        BioNarLab = BioNarLab
        BioDegBrk = BioDegBrk
        BioDegLab = BioDegLab
    }
    # 
    # Define pipe
    `%>%` <- dplyr::`%>%`

    # Declare name of column to hold biodegradation flag value
    biocomm <- tolower(biocomm)
    colBioDeg = "BioDeg"
    colBioNar = "BioNarrative"
    
    # Subset bio index data frame to just site, sample, index score
    df_qual <- df_qual  %>%
        dplyr::select(StationID_Master, clust, eval(colStressSample)
                      , eval(colBioSample), eval(colBio))
    df_qual <- df_qual[!is.na(df_qual[,colBio]),]
    
    # Get vector of "reference" sites (all & cluster/comparator)
    all.ref <- as.vector(df_sites$StationID_Master[df_sites$CARefSite_2017 == 1])

    # Get vector of "reference" reaches (all & cluster/comparator)
    all.ref.reaches <- as.vector(df_sites$COMID[df_sites$CARefSite_2017 == 1])

    # Get vector of "reference" samples (all & cluster/comparator)
    all.ref.samps.bio <- as.vector(unique(df_qual[,colBioSample][df_qual[,"StationID_Master"] %in% all.ref]))
    all.ref.samps.stress <- as.vector(unique(df_qual[,colStressSample][df_qual[,"StationID_Master"] %in% all.ref]))
    
    # Flag quality of sites based on degradation threshold
    df_qual[, colBioDeg] <- cut(df_qual[,colBio]
                                  , breaks=BioDegBrk
                                  , labels=BioDegLab)
    df_qual[, colBioNar] <- cut(df_qual[,colBio]
                                , breaks=BioNarBrk
                                , labels=BioNarLab)
    all.good <- as.vector(unique(df_qual$StationID_Master[df_qual[,colBioDeg]=="No"]))

    all.samp.good.bio <- as.vector(unique(df_qual[,colBioSample][df_qual[,colBioDeg]=="No"]))
    all.samp.good.stress <- as.vector(unique(df_qual[,colStressSample][df_qual[,colBioDeg]=="No"]))

    all.good.reaches <- as.vector(df_sites$COMID[df_sites$StationID_Master %in% all.good])

    # Get vector of sites with samples having index > min target site index
    # Get bio samples and chem sample where bio is better than target
    min.targ <- min(df_qual[,colBio][df_qual$StationID_Master == TargetSiteID])
    all.better <- as.vector(unique(df_qual$StationID_Master[df_qual[,colBio] > min.targ]))
    all.samp.better.bio <- as.vector(unique(df_qual[,colBioSample][df_qual[,colBio] > min.targ]))
    all.samp.better.stress <- as.vector(unique(df_qual[,colStressSample][df_qual[,colBio] > min.targ]))
    all.better.reaches <- as.vector(df_sites$COMID[df_sites$StationID_Master %in% all.better])
   
    all.ref <- all.ref[!is.na(all.ref)]
    all.ref.samps.bio <- all.ref.samps.bio[!is.na(all.ref.samps.bio)]
    all.ref.samps.stress <- all.ref.samps.stress[!is.na(all.ref.samps.stress)]
    all.ref.reaches <- all.ref.reaches[!is.na(all.ref.reaches)]
    all.good <- all.good[!is.na(all.good)]
    all.samp.good.bio <- all.samp.good.bio[!is.na(all.samp.good.bio)]
    all.samp.good.stress <- all.samp.good.stress[!is.na(all.samp.good.stress)]
    all.good.reaches <- all.good.reaches[!is.na(all.good.reaches)]
    all.better <- all.better[!is.na(all.better)]
    all.samp.better.bio <- all.samp.better.bio[!is.na(all.samp.better.bio)]
    all.samp.better.stress <- all.samp.better.stress[!is.na(all.samp.better.stress)]
    all.better.reaches <- all.better.reaches[!is.na(all.better.reaches)]
    
    # Generate matrix of Quality vs. Comparator/Cluster/All, and same but only better than
    # First get max(degraded) site index value
    maxDegSiteIndexVal <- max(df_qual[,colBio][df_qual$StationID_Master==TargetSiteID])
    myCluster <- unique(df_qual[,"clust"][df_qual$StationID_Master==TargetSiteID])
    
    if (useBC == TRUE) { # There are comparator sites defined by biosimilarity
        df_qual[, "ComparatorYN"] <- ifelse(df_qual$StationID_Master %in% comp_sites, "Yes", "No")
        df_qual[, "BetterThan"] <- ifelse(df_qual[,colBio]>maxDegSiteIndexVal, "Yes", "No")
        
        df_qualstats <- df_qual %>%
            dplyr::mutate(CompSites = ifelse(ComparatorYN=="Yes", 1, 0)
                          , CompGood = ifelse((ComparatorYN=="Yes")&(BioDeg=="No"), 1, 0)
                          , CompBad = ifelse((ComparatorYN=="Yes")&(BioDeg=="Yes"), 1, 0)
                          , CompBT = ifelse((ComparatorYN=="Yes")&(BetterThan=="Yes"), 1, 0)
                          , CompBTGood = ifelse((CompBT==1)&(BioDeg=="No"),1,0)
                          , CompBTBad = ifelse((CompBT==1)&(BioDeg=="Yes"),1,0)
                          , ClustSites = ifelse((clust==myCluster), 1, 0)
                          , ClustGood = ifelse((clust==myCluster)&(BioDeg=="No"), 1, 0)
                          , ClustBad = ifelse((clust==myCluster)&(BioDeg=="Yes"), 1, 0)
                          , ClustBT = ifelse((clust==myCluster)&(BetterThan=="Yes"), 1, 0)
                          , ClustBTGood = ifelse((ClustBT==1)&(BioDeg=="No"),1,0)
                          , ClustBTBad = ifelse((ClustBT==1)&(BioDeg=="Yes"),1,0)
                          , AllSites = 1
                          , AllSitesGood = ifelse(BioDeg=="No", 1, 0)
                          , AllSitesBad = ifelse(BioDeg=="Yes", 1, 0)
                          , AllSitesBT = ifelse(BetterThan=="Yes", 1, 0)
                          , AllSitesBTGood = ifelse((AllSitesBT==1)&(BioDeg=="No"),1,0)
                          , AllSitesBTBad = ifelse((AllSitesBT==1)&(BioDeg=="Yes"),1,0)) %>%
            dplyr::select(CompSites, CompGood, CompBad, CompBT, CompBTGood
                          , CompBTBad, ClustSites, ClustGood, ClustBad, ClustBT
                          , ClustBTGood, ClustBTBad, AllSites, AllSitesGood
                          , AllSitesBad, AllSitesBT, AllSitesBTGood, AllSitesBTBad)
        
            df_qualstats <- as.data.frame(colSums(df_qualstats), na.rm = TRUE)
            df_qualstats <- cbind(rownames(df_qualstats)
                                  , data.frame(df_qualstats, row.names = NULL))
            colnames(df_qualstats) <- c("Label", "Count")
            df_qualstats <- df_qualstats %>%
                dplyr::mutate(Quality = ifelse(stringr::str_detect(Label, "Good")
                                               , "Not degraded"
                                               , ifelse(stringr::str_detect(Label
                                                                    , "Bad")
                                                    , "Degraded"
                                                    , "All qualities"))
                              , Group = ifelse(stringr::str_detect(Label, "BT")
                                               , "Better than"
                                               , "All")
                              , Sites = ifelse(stringr::str_detect(Label, "Comp")
                                               , "ComparatorSamples"
                                               , ifelse(stringr::str_detect(Label
                                                                  , "Clust")
                                                    , "ClusterSamples"
                                                    , "AllSamples"))
                              , BioComm = biocomm)
            df_qualstats <- df_qualstats %>%
                dplyr::select(-Label) %>%
                dplyr::group_by(BioComm, Group, Quality) %>%
                tidyr::spread(key = "Sites", value = "Count") %>%
                dplyr::select(BioComm, Group, Quality, ComparatorSamples
                              , ClusterSamples, AllSamples) %>%
                dplyr::arrange(Group, Quality)
            
    } else { # Comparator sites = cluster sites

        df_qual[, "BetterThan"] <- ifelse(df_qual[,colBio]>maxDegSiteIndexVal, "Yes", "No")
        
        df_qualstats <- df_qual %>%
            dplyr::mutate(ClustSites = ifelse((clust==myCluster), 1, 0)
                          , ClustGood = ifelse((clust==myCluster)&(BioDeg=="No"), 1, 0)
                          , ClustBad = ifelse((clust==myCluster)&(BioDeg=="Yes"), 1, 0)
                          , ClustBT = ifelse((clust==myCluster)&(BetterThan=="Yes"), 1, 0)
                          , ClustBTGood = ifelse((ClustBT==1)&(BioDeg=="No"),1,0)
                          , ClustBTBad = ifelse((ClustBT==1)&(BioDeg=="Yes"),1,0)
                          , AllSites = 1
                          , AllSitesGood = ifelse(BioDeg=="No", 1, 0)
                          , AllSitesBad = ifelse(BioDeg=="Yes", 1, 0)
                          , AllSitesBT = ifelse(BetterThan=="Yes", 1, 0)
                          , AllSitesBTGood = ifelse((AllSitesBT==1)&(BioDeg=="No"),1,0)
                          , AllSitesBTBad = ifelse((AllSitesBT==1)&(BioDeg=="Yes"),1,0)) %>%
            dplyr::select(ClustSites, ClustGood, ClustBad, ClustBT
                          , ClustBTGood, ClustBTBad, AllSites, AllSitesGood
                          , AllSitesBad, AllSitesBT, AllSitesBTGood, AllSitesBTBad)
        
        df_qualstats <- as.data.frame(colSums(df_qualstats), na.rm = TRUE)
        df_qualstats <- cbind(rownames(df_qualstats)
                              , data.frame(df_qualstats, row.names = NULL))
        colnames(df_qualstats) <- c("Label", "Count")
        dfcomps <- dfcomps %>%
            dplyr::mutate(Quality = ifelse(stringr::str_detect(Label, "Good")
                                           , "Not degraded"
                                           , ifelse(stringr::str_detect(Label,"Bad")
                                                    , "Degraded"
                                                    , "All qualities"))
                          , Group = ifelse(stringr::str_detect(Label,"BT")
                                           , "Better than"
                                           , "All")
                          , Sites = ifelse(stringr::str_detect(Label, "Clust")
                                           , "Cluster samples"
                                           , "All samples")
                          , BioComm = biocomm)
        
    }

    dirSiteInfo <- file.path(dir_results,TargetSiteID,"SiteInfo")
    fnQualStats <- paste0(TargetSiteID,"_",toupper(biocomm),"_SiteQualities.tab")
    write.table(df_qualstats, file.path(dirSiteInfo,fnQualStats)
                , append = FALSE, col.names = TRUE, row.names = FALSE
                , sep = "\t")
    
    numcompsfinal <- as.numeric(df_qualstats[1,4])
    if (numcompsfinal < length(comp_sites)) {
        gapcomment <- paste0("Comparator sites do not have paired "
                             , " stressor-response data for comparison.")
        gaps <- cbind.data.frame("getQualSites", "Number of Comparators"
                                 , length(comp_sites) - numcompsfinal
                                 , gapcomment)
        colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
    }
        
    # Return data as a list of vector
    myQualSites <- list(dfQuality = df_qual
                        , allRefBioSites = all.ref
                        , allRefBioRespSamps = all.ref.samps.bio
                        , allRefBioStressSamps = all.ref.samps.stress
                        , allRefBioReaches = all.ref.reaches
                        , allGoodBioSites = all.good
                        , allGoodBioRespSamps = all.samp.good.bio
                        , allGoodBioStressSamps = all.samp.good.stress
                        , allGoodBioReaches = all.good.reaches
                        , allBTBioSites = all.better
                        , allBTBioRespSamps = all.samp.better.bio
                        , allBTBioStressSamps = all.samp.better.stress
                        , allBTBioReaches = all.better.reaches
                        , dfQualStats = df_qualstats)
    
    return(myQualSites)

}
