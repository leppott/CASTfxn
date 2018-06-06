#' @title Chemical Data Subsets
#' 
#' @description Get chemical data subsets.
#' 
#' @details Summary information about chems
#' 
#' @param TargetSiteID SiteID
#' @param comid NHD+ COMID
#' @param clustertype Cluster
#' @param useLU Use LandUse.
#' 
#' @return A summary list; ref.sites, ref.reaches, cluster.samps, chem.info
#' , all.chems, cluster.chem, and site.chem.
#' 
#' @examples
#' #No example at this time.
#' 
#' @export
getChemDataSubsets <- function(TargetSiteID, comid, cluster, clustertype, useLU) {
  #Create subsets for target sites, ref sites in cluster, all sites in cluster
  site.COMID <- comid
  site.Clusters <- cluster
  nolu.cluster <- paste(clustertype, "_noland", sep="")
  lu.cluster <- paste(clustertype, "_land", sep="")
  
  #Create a vector of Reference Site IDs
  data.refSites <- subset(data.Stations.Info,CARefSite_2017==1,
                          select= c(StationID_Master,FinalLatitude,FinalLongitude,COMID_NHD2))
  refSiteIDs <- as.vector(unique(data.refSites[,"StationID_Master"]))
  refSiteCOMIDs <- as.vector(unique(data.refSites[,"COMID_NHD2"]))
  
  data.clusterIDs <- data.cluster[,c("COMID",nolu.cluster,lu.cluster)]
  data.Stations.Clusters <- merge(data.Stations.Info, data.clusterIDs, by.x="COMID_NHD2",by.y="COMID")
  data.Stations.ClustIDs <- data.Stations.Clusters[,c("StationID_Master",nolu.cluster,lu.cluster)]
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
  all.chems2 <- all.chems[,c("ChemSampleID","ConvertTo","ResultValue")]
  all.chems3 = cast(all.chems2, ChemSampleID ~ ConvertTo, mean)
  
  
  # chem.tab2 is the list of target site chems at sites in the target site cluster
  if (useLU == TRUE) {
    cluster.chem.tab2 <- subset(all.chems, all.chems[,lu.cluster]==site.lu[,1])
  } else {
    cluster.chem.tab2 <- subset(all.chems, all.chems[,nolu.cluster]==site.nolu[,1])
  }
  
  cluster.chem.tab3 <- cluster.chem.tab2[,c("ChemSampleID","ConvertTo","ResultValue")]
  cluster.chem.samps <- unique(cluster.chem.tab2[,c("StationID_Master","ChemSampleID")])
  cluster.chem.tab4 = cast(cluster.chem.tab3, ChemSampleID ~ ConvertTo, mean)
  cluster.chem.tab5 = merge(cluster.chem.samps, cluster.chem.tab4, by.x = "ChemSampleID", by.y = "ChemSampleID")
  site.chem3 <- site.chem2[,c("ChemSampleID", "ConvertTo", "ResultValue")]
  site.chem4 = cast(site.chem3, ChemSampleID ~ ConvertTo, mean)
  
  mySubsets <- list(ref.sites = refSiteIDs, ref.reaches = refSiteCOMIDs, cluster.samps = cluster.chem.samps
                    , chem.info = chems.groups.sort, all.chems = all.chems3
                    , cluster.chem = cluster.chem.tab5, site.chem = site.chem4)
  return(mySubsets)
}