#' @title BMI matches.
#' 
#' @description Get BMI and chem sample matches.
#' 
#' @details Matched chem/bmi samples.
#' 
#' @param stressors stressors
#' @param list.data data list
#' 
#' @return A summary list; all.b.str, cl.b.str, site.b.str, all.b.rsp, cl.b.rsp
#' , and site.b.rsp.
#' 
#' @examples
#' #No example at this time.
#' 
#' @export
getBMIMatches <- function(stressors, list.data) {
  
  all.chems <- list.data[["all.chems"]]
  cl.chems <- list.data[["cluster.chem"]]
  site.chem <- list.data[["site.chem"]]
  
  # get sample matches mbmi indicates match betw chem & bmi; malg indicates match betw chem and algae
  # need to omit ChemSampleIDs not in all.chems from mbmi.Samps and malg.Samps
  # These aren't in all.chems, because they don't have data corresponding to the site data
  useChemSamps <- all.chems$ChemSampleID
  mUseSamps <- intersect(useChemSamps, data.SampSummary$ChemSampleID)
  
  mbmi.Samps <- na.omit(data.SampSummary[,c("ChemSampleID","BMI.Metrics.SampID")])
  mbmi.use.samps <- subset(mbmi.Samps, mbmi.Samps$ChemSampleID %in% mUseSamps)
  
  # bmi stressor data to use: all.mbmi.stress, cl.mbmi.stress, and site.stress
  all.str.samps <- all.chems[,c("ChemSampleID", stressors)]
  all.stress <- merge(unique(data.chem.raw[,c("StationID_Master", "ChemSampleID")])
                      , all.str.samps, by.x = "ChemSampleID", by.y = "ChemSampleID")
  all.mbmi.stress <- subset(all.stress, ChemSampleID %in% mbmi.use.samps$ChemSampleID)
  all.mbmi.stress <- merge(mbmi.use.samps, all.mbmi.stress, by.x = "ChemSampleID", by.y = "ChemSampleID")
  cl.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% cl.chems$ChemSampleID)
  site.mbmi.stress <- subset(all.mbmi.stress, ChemSampleID %in% site.chem$ChemSampleID)
  
  # bmi response data to use: all.mbmi.resp, cl.mbmi.resp, and site.mbmi.resp
  all.resp <- subset(data.bmi.metrics, BMISampleID %in% mbmi.use.samps$BMI.Metrics.SampID)
  all.mbmi.resp <- merge(mbmi.use.samps, all.resp, by.x = "BMI.Metrics.SampID", by.y = "BMISampleID")
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
