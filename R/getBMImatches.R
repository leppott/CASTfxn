#' @title BMI matches.
#' 
#' @description Get BMI and chem sample matches.
#' 
#' @details Matched chem/bmi samples.
#' 
#' Required objects:
#' 
#' * data.SampSumamry; StationID_Master, CollDate, ChemSampleID, PhabSampID, BMI.Metrics.SampID, Algae.Metrics.SampID
#' 
#' * data.chem.raw; StationID_Master, ChemSampleID
#' 
#' @param stressors stressors
#' @param list.data data list
#' 
#' @return A summary list; all.b.str, cl.b.str, site.b.str, all.b.rsp, cl.b.rsp
#' , and site.b.rsp.
#' 
#' @examples
#' TargetSiteID <- "SRCKN001.61"
#' clustertype <- "5"
#' useLU <- FALSE
#' 
#' CurrentDir<-getwd()
#' myDir.Data <- paste(CurrentDir,"data/",sep="/")
#' 
#' # datasets getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.cluster       <- data_Cluster_Hi
#' data.mod           <- data_ReachMod
#' #
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#' 
#' # datasets getChemDataSubsets
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' 
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#' 
#' # datasets getStressorList
#' chem.info <- list.data$chem.info
#' cluster.chem <- list.data$cluster.chem
#' cluster.samps <- list.data$cluster.samps
#' ref.sites <- list.data$ref.sites
#' site.chem <- list.data$site.chem
#' 
#' # set cutoff for possible stressor identification
#' probsLow <- 0.10
#' probsHigh <- 0.90 
#' 
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, site.chem
#'                                  , probsHigh, probsLow)
#'                                  
#' # datasets getBMIMatches
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' 
#' 
#' # Run getBMIMatches
#' list.MatchBMIData <- getBMIMatches(stressors, list.data)
# 
#' @export
getBMIMatches <- function(stressors, list.data) {
  
  all.chems <- list.data[["all.chems"]]
  cl.chems <- list.data[["cluster.chem"]]
  site.chem <- list.data[["site.chem"]]

  if (nrow(list.SiteSummary$BMImetrics)==0) {
      # No BMI Responses Found
      print(paste0("No BMI response data available for ", TargetSiteID,
                  ". Regression data illustrate cluster relationships only."))
      flush.console()
  }

  # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
  # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
  # These aren't in all.chems, because they don't have data corresponding to the site data
  useChemSamps <- all.chems$ChemSampleID
  mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
  
  mbmi.Samps <- stats::na.omit(data.SampSummary[,c("ChemSampleID","BMI.Metrics.SampID")])
  mbmi.use.samps <- subset(mbmi.Samps, mbmi.Samps$ChemSampleID %in% mUseSamps)
  
  # bmi stressor data to use: all.mbmi.stress, cl.mbmi.stress, and site.stress
  all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
  all.str.samps[is.na(all.str.samps)] <- NA
  all.stress <- merge(unique(data.chem.raw[,c("StationID_Master", "ChemSampleID")])
                      , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
  all.stress <- all.stress[,colSums(is.na(all.stress)) < nrow(all.stress)]
  all.mbmi.stress <- subset(all.stress, ChemSampleID %in% mbmi.use.samps$ChemSampleID)
  all.mbmi.stress <- merge(mbmi.use.samps, all.mbmi.stress, by.x = "ChemSampleID", 
                           by.y = "ChemSampleID")
  cl.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% cl.chems$ChemSampleID)
  site.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% site.chem$ChemSampleID)
  
  # bmi response data to use: all.mbmi.resp, cl.mbmi.resp, and site.mbmi.resp
  all.resp <- subset(data.bmi.metrics, BMISampID %in% mbmi.use.samps$BMI.Metrics.SampID)
  all.mbmi.resp <- merge(mbmi.use.samps, all.resp, by.x = "BMI.Metrics.SampID", 
                         by.y = "BMI.Metrics.SampID")
  cl.mbmi.resp <- subset(all.mbmi.resp, ChemSampleID %in% cl.chems$ChemSampleID)
  site.mbmi.resp <- subset(all.mbmi.resp, ChemSampleID %in% site.chem$ChemSampleID)
  
  myMatchData <- list(all.b.str = all.mbmi.stress
                      , cl.b.str = cl.mbmi.stress
                      , site.b.str = site.mbmi.stress
                      , all.b.rsp = all.mbmi.resp
                      , cl.b.rsp = cl.mbmi.resp
                      , site.b.rsp = site.mbmi.resp)
  return(myMatchData)
}
