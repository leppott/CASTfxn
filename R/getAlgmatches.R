#' @title Algae matches.
#' 
#' @description Get Algae and chem sample matches.
#' 
#' @details Matched chem/algae samples.
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
#' TargetSiteID <- "LCBEN002.57"
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
#' # datasets getAlgMatches
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#'
#' # Run getAlgMatches
#' list.MatchAlgData <- getAlgMatches(stressors, list.data)
#' 
#' @export
getAlgMatches <- function(stressors, list.data) {
  
  all.chems <- list.data[["all.chems"]]
  cl.chems <- list.data[["cluster.chem"]]
  site.chem <- list.data[["site.chem"]]
  ref.sites <- list.data[["ref.sites"]]
  
  if (nrow(list.SiteSummary$AlgMetrics)==0) {
      # No Algae Responses Found
      print(paste("No algae response data available for ", TargetSiteID,
                  ". Regression data illustrate cluster relationships only.",
                  sep = ""))
      utils::flush.console()
  }
  
  # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
  # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
  # These aren't in all.chems, because they don't have data corresponding to the site data
  useChemSamps <- all.chems$ChemSampleID
  mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
  
  malg.Samps <- stats::na.omit(data.SampSummary[,c("ChemSampleID",
                                                   "Algae.Metrics.SampID")])
  malg.use.samps <- subset(malg.Samps, malg.Samps$ChemSampleID %in% mUseSamps)
  malg.use.samps$Algae.Metrics.SampID <- 
      stringr::str_remove(malg.use.samps$Algae.Metrics.SampID, "_EMAP")
  malg.use.samps$Algae.Metrics.SampID <- 
      stringr::str_remove(malg.use.samps$Algae.Metrics.SampID, "_Multihabitat")

  # bmi stressor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
  all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
  all.stress <- merge(unique(data.chem.raw[,c("StationID_Master","ChemSampleID")])
                      , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
  
  # alg stresor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
  all.malg.stress <- subset(all.stress, ChemSampleID %in% malg.use.samps$ChemSampleID)
  all.malg.stress <- merge(malg.use.samps, all.malg.stress, 
                           by.x = "ChemSampleID", by.y = "ChemSampleID")
  cl.malg.stress <- subset(all.malg.stress, ChemSampleID %in% cl.chems$ChemSampleID)
  site.malg.stress <- subset(all.malg.stress, ChemSampleID %in% site.chem$ChemSampleID)
  
  # alg response data to use: all.malg.resp, cl.malg.resp, and site.malg.resp
  all.malg.resp <- subset(data.algae.metrics, Algae.Metrics.SampID %in% 
                              malg.use.samps$Algae.Metrics.SampID)
  cl.chems1 <- merge(cl.chems, all.malg.resp[,c("StationID_Master", 
                     "Algae.Metrics.SampID")], by.x = "StationID_Master", 
                     by.y = "StationID_Master")
  cl.malg.resp <- subset(all.malg.resp, Algae.Metrics.SampID %in% 
                             cl.chems1$Algae.Metrics.SampID)
  site.chem1 <- as.data.frame(stringr::str_remove(site.chem$ChemSampleID, 
                                                  "_\\d{4}\\-\\d{2}\\-\\d{2}"))
  colnames(site.chem1)[1] <- "StationID_Master"
  site.chem1 <- merge(site.chem1, all.malg.resp[,c("StationID_Master",
                      "Algae.Metrics.SampID")], by.x = "StationID_Master",
                      by.y = "StationID_Master")
  site.chem1 <- unique(site.chem1)
  site.malg.resp <- subset(all.malg.resp, Algae.Metrics.SampID %in% 
                             site.chem1$Algae.Metrics.SampID)
  
  myMatchData <- list(all.a.str = all.malg.stress
                      , cl.a.str = cl.malg.stress
                      , site.a.str = site.malg.stress
                      , all.a.rsp = all.malg.resp
                      , cl.a.rsp = cl.malg.resp
                      , site.a.rsp = site.malg.resp )
  return(myMatchData)
}