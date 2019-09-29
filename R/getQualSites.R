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
#' @param comp_sites Vector containing comparator site identifiers.
#' @param df_sites Sites table containing site ids and reference flags. 
#' Default = "data_Sites".
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param df_qual Biological index and metrics data for the specified biocomm. 
#' Default = "data.bmi.metrics".
#' @param colBio Name of the column for the biological response index measure. 
#' Default = "CSCI".
#' @param colSample Name of the column for the response sample identifier. 
#' Default = "BMI.Metrics.SampID".
#' @param Bio.Deg.Brk Biological assessment degraded status, cut function breaks. 
#' Should be in order from bad (low) to good (high). 
#' Default = c(-2, 0.799, 2)
#' @param Bio.Deg.Lab Biological assessment degraded status, cut function labels. 
#' Should be in order from bad (low) to good (high).
#' Defaults are referenced in the code so if change the code will break. 
#' Default = c("Yes", "No").
#' @param siteQual2Plot The quality of site desired for plotting on graphs. 
#' Default = reference. Allowed values are "reference", "not degraded", or 
#' "better than" the minimum target sample quality.
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
                        , comp_sites
                        , df_sites = data_Sites
                        , biocomm = "bmi"
                        , df_qual = data.bmi.metrics
                        , colBio = "CSCI"
                        , colSample = "BMI.Metrics.SampID"
                        , BioDegBrk = c(-2, 0.799, 2)
                        , BioDegLab = c("Yes", "No")) {
    
    # For QC purposes
    # TargetSiteID
    # comp_sites
    # df_sites = data_Sites
    # biocomm = "bmi"
    # df_qual = data.bmi.metrics
    # colBio = "IBI"
    # colSample = "BMI.Metrics.SampID"
    # BioDegBrk = c(-2, 0.799, 2)
    # BioDegLab = c("Yes", "No")
    
    # Define pipe
    `%>%` <- dplyr::`%>%`

    # Declare name of column to hold biodegradation flag value
    biocomm <- tolower(biocomm)
    colBioDeg = "BioDeg"
    
    # Subset df_sites for comparators only
    comp.sitedata <- df_sites[df_sites$StationID_Master %in% comp_sites,]
    
    # Get vector of "reference" sites (all & cluster/comparator)
    all.ref <- as.vector(df_sites$StationID_Master[df_sites$CARefSite_2017 == 1])
    comp.ref <- all.ref[all.ref %in% comp_sites]
    
    # Get vector of "reference" reaches (all & cluster/comparator)
    ref.reaches <- as.vector(df_sites$COMID_NHD2[df_sites$CARefSite_2017 == 1])
    comp.ref.reaches <- as.vector(comp.sitedata$COMID_NHD2[comp.sitedata$CARefSite_2017 == 1])
    
    # Get vector of not degraded sites
    # Subset bio index data frame to just site, sample, index score
    df_qual <- df_qual  %>%
        dplyr::select(StationID_Master, eval(colSample), eval(colBio))
    comp.samps <- df_qual[,colSample][df_qual$StationID_Master %in% comp_sites]
    
    df_qual[, colBioDeg] <- cut(df_qual[,colBio]
                                  , breaks=BioDegBrk
                                  , labels=BioDegLab)
    all.good <- as.vector(unique(df_qual$StationID_Master[df_qual[,colBioDeg]=="No"]))
    comp.good <- all.good[all.good %in% comp_sites]

    all.samp.good <- as.vector(unique(df_qual[,colSample][df_qual[,colBioDeg]=="No"]))
    comp.samp.good <- all.samp.better[all.samp.better %in% comp.samps]
    
    good.reaches <- as.vector(df_sites$COMID_NHD2[df_sites$StationID_Master %in% all.good])
    comp.good.reaches <- as.vector(comp.sitedata$COMID_NHD2[comp.sitedata$StationID_Master %in% all.good])
    
    # Get vector of sites with samples having index > min target site index
    min.targ <- min(df_qual[,colBio][df_qual$StationID_Master == TargetSiteID])
    all.better <- as.vector(unique(df_qual$StationID_Master[df_qual[,colBio] > min.targ]))
    all.samp.better <- as.vector(unique(df_qual[,colSample][df_qual[,colBio] > min.targ]))
    all.better.reaches <- as.vector(df_sites$COMID_NHD2[df_sites$StationID_Master %in% all.better])
    comp.better <- all.better[all.better %in% comp_sites]
    comp.samp.better <- all.samp.better[all.samp.better %in% comp.samps]
    comp.better.reaches <- as.vector(comp.sitedata$COMID_NHD2[comp.sitedata$StationID_Master %in% comp.better])
    
    # Assess data gaps
    
    # Return data as a list of vectors
    if (biocomm == "bmi") {
        myQualSites <- list(all.b.ref = all.ref
                            , all.b.ref.reaches = ref.reaches
                            , all.b.good = all.good
                            , all.b.good.samps = all.samp.good
                            , all.b.good.reaches = good.reaches
                            , all.b.bt.sites = all.better
                            , all.b.bt.samps = all.samp.better
                            , all.b.bt.reaches = all.better.reaches
                            , comp.b.ref = comp.ref
                            , comp.b.ref.reaches = comp.ref.reaches
                            , comp.b.good = comp.good
                            , comp.b.good.samps = comp.samp.good
                            , comp.b.good.reaches = comp.good.reaches
                            , comp.b.bt.sites = comp.better
                            , comp.b.bt.samps = comp.samp.better
                            , comp.b.bt.reaches = comp.better.reaches)
    } else if (biocomm == "alg") {
        myQualSites <- list(all.a.ref = all.ref
                            , all.a.ref.reaches = ref.reaches
                            , all.a.good = all.good
                            , all.a.good.samps = all.samp.good
                            , all.a.good.reaches = good.reaches
                            , all.a.bt.sites = all.better
                            , all.a.bt.samps = all.samp.better
                            , all.a.bt.reaches = all.better.reaches
                            , comp.a.ref = comp.ref
                            , comp.a.ref.reaches = comp.ref.reaches
                            , comp.a.good = comp.good
                            , comp.a.good.samps = comp.samp.good
                            , comp.a.good.reaches = comp.good.reaches
                            , comp.a.bt.sites = comp.better
                            , comp.a.bt.samps = comp.samp.better
                            , comp.a.bt.reaches = comp.better.reaches)
    } else {
        QualMsg <- paste0("Biological community ", biocomm, " not supported.")
        Msg(QualMsg)
    }
    
    return(myQualSites)

}