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
#' 
#' TargetSiteID <- "SDR-MLS"
#' clustertype <- "H6"
#' useLU <- FALSE
#' 
#' \dontrun{
#' CurrentDir<-getwd()
#' myDir.Data <- paste(CurrentDir,"data/",sep="/")
#' 
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#' 
#' # Run getChemDataSubsets
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#' chem.info <- list.data$chem.info
#' cluster.chem <- list.data$cluster.chem
#' cluster.samps <- list.data$cluster.samps
#' ref.sites <- list.data$ref.sites
#' site.chem <- list.data$site.chem
#' 
#' # set cutoff for possible stressor identification
#' probsLow <- 0.10
#' probsHigh <- 0.90#' 
#' 
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, site.chem
#'                                  , probsHigh, probsLow)
#' stressors <- list.stressors$stressors
#' 
#' data.SampSummary <- read.delim(paste(myDir.Data,"data.SampSummary.tab",sep="")
#'                                , na.strings = c(""," "))
#' data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
#'
#' list.MatchAlgData <- getAlgMatches(stressors, list.data)
#' }
#' 
#' @export
getAlgMatches <- function(stressors, list.data) {
  
  all.chems <- list.data[["all.chems"]]
  cl.chems <- list.data[["cluster.chem"]]
  site.chem <- list.data[["site.chem"]]
  ref.sites <- list.data[["ref.sites"]]
  
  # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
  # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
  # These aren't in all.chems, because they don't have data corresponding to the site data
  useChemSamps <- all.chems$ChemSampleID
  mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
  
  malg.Samps <- stats::na.omit(data.SampSummary[,c("ChemSampleID","Algae.Metrics.SampID")])
  malg.use.samps <- subset(malg.Samps, malg.Samps$ChemSampleID %in% mUseSamps)
  
  # bmi stressor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
  all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
  all.stress <- merge(unique(data.chem.raw[,c("StationID_Master", "ChemSampleID")])
                      , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
  
  # alg stresor data to use: all.malg.stress, cl.malg.stress, and site.malg.stress
  all.malg.stress <- subset(all.stress, ChemSampleID %in% malg.use.samps$ChemSampleID)
  all.malg.stress <- merge(malg.use.samps, all.malg.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
  cl.malg.stress <- subset(all.malg.stress, ChemSampleID %in% cl.chems$ChemSampleID)
  site.malg.stress <- subset(all.malg.stress, ChemSampleID %in% site.chem$ChemSampleID)
  
  # alg response data to use: all.malg.resp, cl.malg.resp, and site.malg.resp
  all.malg.resp <- subset(data.algae.metrics, StationDateRep %in% malg.use.samps$Algae.Metrics.SampID)
  cl.malg.resp <- subset(all.malg.resp, StationDateRep %in% cl.chems$ChemSampleID)
  site.malg.resp <- subset(all.malg.resp, StationDateRep %in% site.chem$ChemSampleID)
  
  myMatchData <- list(all.a.str = all.malg.stress
                      , cl.a.str = cl.malg.stress
                      , site.a.str = site.malg.stress
                      , all.a.rsp = all.malg.resp
                      , cl.a.rsp = cl.malg.resp
                      , site.a.rsp = site.malg.resp )
  return(myMatchData)
}