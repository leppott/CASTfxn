#' @title Chemical Data Subsets
#' 
#' @description Get chemical data subsets.
#' 
#' @details Summary information about chems.
#' 
#' Uses package reshape.
#' 
#' Required objects:
#' 
#' * data.cluster; COMID, H6_noland, H6_land, ElevWs, WsAreaSqKm, PrecipWs, TmeanWs, W___AGRIC, W___URBAN, W___FOREST
#' 
#' * data.Stations.Info; StationID_Master, FinalLatitude, FinalLongitude, WaterbodyName, GIS_County, CARefSite_2017, COMID_NHD2
#' 
#' * data.chem.raw; StationID_Master, ChemSampleID
#' 
#' * data.chem.info; Analyte
#' 
#' @param TargetSiteID SiteID
#' @param comid NHD+ COMID
#' @param cluster cluster information for TargetSiteID.  getSiteInfo list output 'COMID'.
#' @param data.cluster data.cluster
#' @param data.Stations.Info data.Stations.Info
#' @param data.chem.raw Chemistry data.
#' @param data.chem.info data.chem.info
#'  
#' @return A summary list; ref.sites, ref.reaches, cluster.samps, chem.info
#' , all.chems, cluster.chem, and site.chem.
#' 
#' @examples
#' TargetSiteID <- "SRCKN001.61"
#' 
# CurrentDir<-getwd()
# myDir.Data <- paste(CurrentDir, "data/", sep="/")
# 
#' # Data getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites       # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.cluster       <- data_Cluster_Hi  # need for getSiteInfo and getChemDataSubsets
#' data.mod           <- data_ReachMod
#'
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID)
#' 
#' # Data getChemDataSubset
#' # data import, example 
#' # data.chem.raw <- read.delim(paste(myDir.Data, "data.chem.raw.tab", sep=""), na.strings = c(""," "))
#' # data.chem.info <- read.delim(paste(myDir.Data, "data.chem.info.tab", sep=""))
#' # data, example included with package
#' #
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' data.chem.raw      <- data_Chem
#' data.chem.info     <- data_ChemInfo
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
#'                                 , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
#'                                 , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
#~~~~~~~~~~~~~~~~~~~~~~~~~
# QC
# TargetSiteID <- "UGBLR028.77"
# comid <- site.COMID
# cluster <- site.Clusters
#~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
getChemDataSubsets <- function(TargetSiteID, comid, cluster, data.cluster
                               , data.Stations.Info, data.chem.raw, data.chem.info) {
  #
  useLU <- FALSE
  #
  # Debug ####
  boo.DEBUG <- FALSE
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    comid <- site.COMID
    cluster <- site.Clusters
  }##IF.boo.DEBUG.END
  
  # check for and create (if necessary) "Results" subdirectory of working directory
  wd <- getwd()
  dir.sub <- "Results"
  dir.sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  #
  #Create subsets for target sites, ref sites in cluster, all sites in cluster
  site.COMID <- comid
  site.Clusters <- cluster
  # nolu.cluster <- paste(clustertype, "_noland", sep="")
  # lu.cluster <- paste(clustertype, "_land", sep="")
  nolu.cluster <- "clust_noland"
  lu.cluster <- "clust_land"
  
  #Create a vector of Reference Site IDs
  data.refSites <- subset(data.Stations.Info,CARefSite_2017==1,
                          select= c(StationID_Master,FinalLatitude,FinalLongitude,COMID_NHD2))
  refSiteIDs <- as.vector(unique(data.refSites[,"StationID_Master"]))
  refSiteCOMIDs <- as.vector(unique(data.refSites[,"COMID_NHD2"]))
  
  data.clusterIDs <- data.cluster[,c("COMID", nolu.cluster, lu.cluster)]
  data.Stations.Clusters <- merge(data.Stations.Info, data.clusterIDs, by.x="COMID_NHD2",by.y="COMID")
  data.Stations.ClustIDs <- data.Stations.Clusters[,c("StationID_Master", nolu.cluster, lu.cluster)]
  data.chem.raw <- merge(data.chem.raw, data.Stations.ClustIDs, by.x="StationID_Master", by.y="StationID_Master")
  
  #Create stressor data cross-tabs
  site.chem <- subset(data.chem.raw, StationID_Master %in% TargetSiteID)
  
  # site.chem2 contains the chemicals detected at the target site
  site.chem2 <- subset(site.chem, !is.na(site.chem["ResultValue"]))
  chems <- unique(site.chem2["ConvertTo"])
  chems.groups <- merge(chems, data.chem.info, by.x="ConvertTo", by.y="Analyte")
  
  # chems.groups.sort is the list of chems detected at the target site, in group sort order
  chems.groups.sort <- chems.groups[order(chems.groups$GroupNum, chems.groups$ConvertTo),]
  numgps <- length(unique(chems.groups$GroupName))
  groupnames <- unique(chems.groups.sort$GroupName)
  site.lu <- site.Clusters[lu.cluster]
  site.nolu <- site.Clusters[nolu.cluster]
  
  # all.chems is the list of target site chems across all sites in dataset (all clusters)
  all.chems <- subset(data.chem.raw, ConvertTo %in% chems$ConvertTo)
  # Erik, 20180615  (same result) all.chems <- data.chem.raw[data.chem.raw$ConvertTo %in% chems$ConvertTo, ]
  all.chems2 <- all.chems[,c("ChemSampleID","ConvertTo","ResultValue")]
  all.chems3 <- reshape::cast(all.chems2, ChemSampleID ~ ConvertTo, mean, value="ResultValue", na.rm=TRUE)
  
  
  # chem.tab2 is the list of target site chems at sites in the target site cluster
  if (useLU == TRUE) {
    # QC, 20180612
    if (is.na(site.lu[,1])) {
      # do not proceed
      # no cluster assignment
      stop(paste0("No cluster assignment for ", TargetSiteID))
    }
    cluster.chem.tab2 <- subset(all.chems, all.chems[, lu.cluster]==site.lu[,1])
  } else {
    # QC, 20180612
    if (is.na(site.nolu[,1])) {
      # do not proceed
      # no cluster assignment
      stop(paste0("No cluster assignment for ", TargetSiteID))
    }
    cluster.chem.tab2 <- subset(all.chems, all.chems[, nolu.cluster]==site.nolu[,1])
  }
  # Get error if no cluster info (i.e., if is.na(site.lu[,1]))
  
  cluster.chem.tab3 <- cluster.chem.tab2[,c("ChemSampleID","ConvertTo","ResultValue")]
  # Remove NA values, 20190205
  cluster.chem.tab3 <- cluster.chem.tab3[!is.na(cluster.chem.tab3$ResultValue), ]
  #
  cluster.chem.samps <- unique(cluster.chem.tab2[,c("StationID_Master","ChemSampleID")])
  cluster.chem.tab4 <- reshape::cast(cluster.chem.tab3, ChemSampleID ~ ConvertTo, mean, value="ResultValue", na.rm=TRUE)
  cluster.chem.tab5 = merge(cluster.chem.samps, cluster.chem.tab4, by.x = "ChemSampleID", by.y = "ChemSampleID")
  site.chem3 <- site.chem2[,c("ChemSampleID", "ConvertTo", "ResultValue")]
  site.chem4 <- reshape::cast(site.chem3, ChemSampleID ~ ConvertTo, mean, value="ResultValue", na.rm=TRUE)
  
  mySubsets <- list(ref.sites = refSiteIDs, ref.reaches = refSiteCOMIDs, cluster.samps = cluster.chem.samps
                    , chem.info = chems.groups.sort, all.chems = all.chems3
                    , cluster.chem = cluster.chem.tab5, site.chem = site.chem4)
  # Key
  # chem.tab2  = cluster.chem
  # chem.tab3  =
  # chem.tab4  =
  # chem.tab4  =
  # chem.tab5  = cluster.chem
  # site.chem3 = 
  # site.chem4 = site.chem
  # all.chems  = all.chem (target chemicals, all columns, converted units)
  # all.chems2 = all.chem (limited columns)
  # all.chems3 = all.chems (mean values)
  
  return(mySubsets)
}
