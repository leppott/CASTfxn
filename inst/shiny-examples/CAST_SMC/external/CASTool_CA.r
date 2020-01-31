# CASTfxn (Specific for Arizona DEQ)
# Erik.Leppo@tetratech.com, 20180710
# Ann.Lincoln@tetratech.com, 20190430
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 
# library(devtools)
# install_github("leppott/CASTfxn")

rm(list=ls())

# Set up required functions ### DO NOT CHANGE! #
library(CASTfxn)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
source(file.path(getwd(), "getCoOccurDataset.R"))
source(file.path(getwd(), "getTimeSeq.R"))
source(file.path(getwd(), "getWoE.R"))

start.time <- Sys.time()

wd <- setwd(getwd())
dir_data <- file.path(wd, "Data")

# Specify Base Filenames ####
fn.Sites.Info <- file.path(dir_data,"SMCSitesFinal.tab")
fn.SampSummary <- file.path(dir_data,"SMCSiteSummary.tab")
fn.cheminfo <- file.path(dir_data,"SMCStressInfoFinal.tab")
fn.chemdata <- file.path(dir_data,"SMCStressDataFinal.tab")
fn.modelinfo <- file.path(dir_data,"SMCModelStressInfoFinal.tab")
fn.modeldata <- file.path(dir_data,"SMCModelStressDataFinal.tab")
fn.bmi.metrics <- file.path(dir_data,"SMCBenthicMetricsFinal.tab")
fn.bmi.raw <- file.path(dir_data, "SMCBenthicCountsFinal.tab")
fn.MT.bmi <- file.path(dir_data, "SMCBenthicMasterTaxa.tab")
# fn.alg.metrics <- file.path(dir_data, "AZAlgaeMetrics.tab")
# fn.alg.raw <- file.path(dir_data, "AZAlgaeCountsFinal.tab")
# fn.MT.alg <- file.path(dir_data, "AZAlgaeMasterTaxa.tab")
fn.cluster <- "SMCClusters.tab"
fn.targets <- "SMCTestSites.xlsx"
# flowline <- rgdal::readOGR(dsn = "Data/SMCBoundary", layer = "SMCBoundary")
# outline <- rgdal::readOGR(dsn = "Data/SMCReaches", layer = "SMCReaches")

# Specify user-defined variables
ibi.thresholds <- c(-2, 0.62, 0.799, 0.919, 2)
ibi.deg.thres <- c(-2, 0.799, 2)
BMImetrics <- c("IBI", "MMI", "OoverE", "Taxonomic_Richness"
                , "Intolerant_Percent", "Shredder_Taxa", "Clinger_PercentTaxa"
                , "Coleoptera_PercentTaxa", "EPT_PercentTaxa")
# USGS aea is below
# my.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0
#             +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# aea used for AZ is below
my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
probsHigh=0.75
probsLow=0.25
report_format="html"    # word, pdf are the other options

# Read Base datafiles
data_Sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t")
rm(fn.Sites.Info)

data.SampSummary <- read.delim(fn.SampSummary, header = TRUE, sep = "\t")
data.mod         <- data_ReachMod   # Check this
data.303d.ComID  <- data_303d       # Check this
rm(fn.SampSummary)

# CAST, Chem ####
## Get Chem Info for all possible stressors
data.chem.info <- read.delim(fn.cheminfo, header = TRUE, sep = "\t")
data.chem.info <- mutate(data.chem.info, Analyte = StdParamName)
col.meas.InvSc = as.vector(data.chem.info$StdParamName[data.chem.info$DirIncStress=="Dec"])
SSTVparms <- data.chem.info$StdParamName[data.chem.info$SSTV==1]
rm(fn.cheminfo)

## Use data rather than example file
data.chem.all <- read.delim(fn.chemdata, header = TRUE, sep = "\t",
                            na.strings = "NA")
analytes      <- data.chem.info$StdParamName[data.chem.info$UseInStressorID == 1]
data.chem.raw <- data.chem.all[data.chem.all$StdParamName %in% analytes,]
data.chem.raw <- mutate(data.chem.raw, SampleDate = mdy(SampDate))
rm(fn.chemdata, data.chem.all)

# Get modeled flow data and metadata
data.model.info <- read.delim(fn.modelinfo, header = TRUE, sep = "\t")
data.model.info <- mutate(data.model.info, Analyte = StdParamName)
col.model.InvSc = as.vector(data.model.info$StdParamName[data.model.info$DirIncStress=="Dec"])
rm(fn.modelinfo)

# Combine measured and modeled parameters with inverse scoring
col.Stressors.InvSc <- c(col.meas.InvSc, col.model.InvSc)

data.model.all <- read.delim(fn.modeldata, header = TRUE, sep = "\t")
useParams      <- data.model.info$StdParamName[data.model.info$UseInStressorID == 1]
data.model.raw <- data.model.all[data.model.all$StdParamName %in% useParams,]
data.model.raw <- data.model.raw %>%
    mutate(SampYear = year(mdy(SampDate))) %>%
    select(StationID_Master, ChemSampleID, SampDate, SampYear, StdParamName
           , ResultValue, ConvertTo, COMID_NHD2, clust)
rm(fn.modeldata, data.model.all)

# CAST, BMI, metrics ####
data.bmi.metrics.all <- read.delim(fn.bmi.metrics, header = TRUE, sep = "\t",
                               na.strings = "NA", stringsAsFactors = FALSE)
data.bmi.metrics.all <- data.bmi.metrics.all[,c("StationID_Master","SampleID"
                                                ,"SampleDate","Quality","CSCI"
                                                ,"MMI","OoverE"
                                                ,"Taxonomic_Richness"
                                                , "Intolerant_Percent"
                                                , "Shredder_Taxa"
                                                , "Clinger_PercentTaxa"
                                                , "Coleoptera_PercentTaxa"
                                                , "EPT_PercentTaxa"
                                                , "BMISampID")]
data.bmi.metrics <- data.bmi.metrics.all[, unlist(lapply(data.bmi.metrics.all,
                                            function(x) !all(is.na(x))))]
colnames(data.bmi.metrics) <- c("StationID_Master","BMI.Metrics.SampID"
                                ,"CollDate","Quality","IBI"
                                ,"MMI","OoverE"
                                ,"Taxonomic_Richness"
                                , "Intolerant_Percent"
                                , "Shredder_Taxa"
                                , "Clinger_PercentTaxa"
                                , "Coleoptera_PercentTaxa"
                                , "EPT_PercentTaxa"
                                , "BMISampID")
data.bmi.metrics <- data.bmi.metrics %>%
    mutate(SampDate = mdy(CollDate))
rm(fn.bmi.metrics, data.bmi.metrics.all)

# CAST, BMI taxonomic data ####
data.bmi.taxa.raw <- read.table(fn.bmi.raw, header = TRUE, sep = "\t")

data.MT.bmi <- read.table(fn.MT.bmi, header = TRUE, sep = "\t",
                          stringsAsFactors = FALSE)
data.bmi.taxa.raw <- mutate(data.bmi.taxa.raw, BMI.Metrics.SampID = BMISampID)
rm(fn.bmi.raw, fn.MT.bmi)

# Generate co-occurrence data set (same day samples; modeled data match any day)
data.bmi.coOccur <- getCoOccurDataset(df.sites = data_Sites
                                      , df.mod = data.model.raw
                                      , df.bmi = data.bmi.metrics
                                      , df.meas = data.chem.raw
                                      , index = "CSCI")

# CAST, Alg, metrics ####
# data.algae.metrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t",
#                                  stringsAsFactors = FALSE)
# AlgMetrics <- colnames(data.algae.metrics)[6:52]
# rm(fn.alg.metrics)

# Add extra legacy fields
data.cluster <- read.delim(file.path(dir_data,fn.cluster), header = TRUE
                           , sep = "\t")
data.cluster <- data.cluster %>%
    mutate(PrecipWs = Precip8110Ws
           , TmeanWs = Tmean8110Ws
           , clust_noland = clust
           , clust_land = clust)
rm(fn.cluster)

# Site Selection ####
df.targets <- read_excel(file.path(dir_data,fn.targets))

# RUN CASTool
for (site in 1:nrow(df.targets)) {
    TargetSiteID <- df.targets$TargetSiteID[site]

    print(paste(paste0(site,"."),"Evaluating",TargetSiteID, sep = " "))
    flush.console()
    
    list.SiteSummary <- getSiteInfo(TargetSiteID
                                    , dir_results = file.path(wd, "Results")
                                    , data.Stations.Info = data_Sites
                                    , data.SampSummary
                                    , data.303d.ComID
                                    , data.bmi.metrics
                                    , data.algae.metrics = data.bmi.metrics
                                    , data.cluster
                                    , data.mod
                                    , map_proj = NULL
                                    , map_outline = outline
                                    , map_flowline = flowline
                                    , map_flowline2 = NULL
                                    , dir_sub = "SiteInfo")
    # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo, Samps = mySamps, 
    #                                BMImetrics = myBMImetrics, 
    #                                AlgMetrics = myAlgaeMetrics, 
    #                                ReachInfo = myReachInfo, 
    #                                COMID = myCOMID, 
    #                                ClustIDs = myClustIDs,
    #                                impair = myImpairments,
    #                                mods = myReachMods)
    print("getSiteInfo is complete.")
    flush.console()
    
    # Get matched data for analyses
    list.data <- getChemDataSubsets(TargetSiteID
                                    , comid=list.SiteSummary$COMID
                                    , cluster=list.SiteSummary$ClustIDs
                                    , data.cluster
                                    , data.Stations.Info = data_Sites
                                    , data.chem.raw
                                    , data.chem.info)
    # Returns: mySubsets <- list(ref.sites = refSiteIDs, 
    #                            ref.reaches = refSiteCOMIDs, 
    #                            cluster.samps = cluster.chem.samps, 
    #                            chem.info = chems.groups.sort, 
    #                            all.chems = all.chems3, 
    #                            cluster.chem = cluster.chem.tab5, 
    #                            site.chem = site.chem4)
    print("getChemDataSubsets is complete.")
    flush.console()
    
    # Get Cluster Info
    getClusterInfo(site.COMID=list.SiteSummary$COMID
                   , site.Clusters=list.SiteSummary$ClustIDs
                   , refSiteCOMIDs=list.data$ref.reaches
                   , dir_results=file.path(wd,"Results")
                   , dir_sub="ClusterInfo")
    print("getClusterInfo is complete.")
    flush.console()
    
    # Get Stressor List
    list.stressors <- getStressorList(TargetSiteID
                                      , site.Clusters=list.SiteSummary$ClustIDs
                                      , chem.info=list.data$chem.info
                                      , cluster.chem=list.data$cluster.chem
                                      , cluster.samps=list.data$cluster.samps
                                      , ref.sites=list.data$ref.sites
                                      , site.chem=list.data$site.chem
                                      , probsHigh=probsHigh
                                      , probsLow=probsLow
                                      , dir_results=file.path(wd, "Results"))
    # Returns: myStressors <- list(stressors = stressorlist
    #                     , site.stressor.pctrank = site.pctrank
    #                     , stressors_LogTransf)
    stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
    stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
    print("getStressorList is complete.")
    flush.console()
    
    # If no stressors are identified, no analyses can be performed. Error msg.
    if (length(stressors) == 0) {
        print(paste("No stressors identified for", TargetSiteID))
        flush.console()
        next()
    }
    
    # Create time sequence graphics
    getTimeSeq(TargetSiteID
               , stressors
               , biocomm = "BMI"
               , BioResp = BMImetrics
               , df.stress = data.chem.raw
               , df.resp = data.bmi.metrics
               , dir_results = file.path(getwd(),"Results")
               , dir_sub = "TemporalSequence")
    print("getTimeSeq for benthic macroinvertebrates is complete.")
    flush.console()
    
    # Create time sequence graphics
    # getTimeSeq(TargetSiteID
    #            , stressors
    #            , biocomm = "Algae"
    #            , BioResp = AlgMetrics
    #            , df.stress = data.chem.raw
    #            , df.resp = data.algae.metrics
    #            , dir_results = file.path(getwd(),"Results")
    #            , dir_sub = "TemporalSequence")
    # print("getTimeSeq for algae is complete.")
    # flush.console()
    
    # Get BMI matches
    list.ChemBMIData <- getBioMatches(stressors
                                      , list.data
                                      , list.SiteSummary
                                      , data.SampSummary
                                      , data.chem.raw
                                      , data.bio.metrics = data.bmi.metrics
                                      , biocomm = "BMI")
    # Returns: myBMIMatchData <- list(all.b.str = all.mbmi.stress
    #                        , cl.b.str = cl.mbmi.stress
    #                        , site.b.str = site.mbmi.stress
    #                        , all.b.rsp = all.mbmi.resp
    #                        , cl.b.rsp = cl.mbmi.resp
    #                        , site.b.rsp = site.mbmi.resp)
    print("getBioMatches for benthic macroinvertebrates is complete.")
    flush.console()
    
    # Get BMI Stressor Responses
    getBioStressorResponses(TargetSiteID
                            , stressors = stressors
                            , BioResp = BMImetrics
                            , list.MatchBioData = list.ChemBMIData
                            , LogTransf=stressors_logtransf
                            , ref.sites=list.data$ref.sites
                            , biocomm="BMI"
                            , dir_results=file.path(wd, "Results"))
    print("getBioStressorResponses for benthic macroinvertebrates is complete.")
    flush.console()
    
    # Get BMI-based co-occurrence
    if (TargetSiteID %in% unique(data.bmi.coOccur$StationID_Master)) {
        print("Starting Co-occurrence")
        flush.console()
        getCoOccur(df.data=data.bmi.coOccur
                   , TargetSiteID=TargetSiteID
                   , col.ID="StationID_Master"
                   , col.Group="clust"
                   , col.Bio="CSCI"
                   , col.Stressors=c(stressors)
                   , Bio.Nar.Brk=ibi.thresholds
                   , Bio.Nar.Lab=c("very likely altered", "likely altered"
                                   ,"possibly altered","likely intact")
                   , Bio.Deg.Brk=ibi.deg.thres
                   , Bio.Deg.Lab=c("Yes", "No")
                   , biocomm = "bmi"
                   , dir.plots = file.path(wd, "Results")
                   , dir_sub = "CoOccurrence"
                   , col.Stressors.InvSc = col.Stressors.InvSc)
    }
    print("getCoOccur for benthic macroinvertebrates is complete.")
    flush.console()
    
    # Get Stressor-specific regressions
    data.SSTV.totabund <- data.bmi.taxa.raw[,c("BMI.Metrics.SampID", "FinalID"
                                               ,"RelAbundInds")]
    if (SSTVparms %in% stressors) {
        targ_bio_bad <- list.ChemBMIData$site.b.rsp[list.ChemBMIData$site.b.rsp[
            , "Quality"]=="Degraded", "IBI"]
        if (length(targ_bio_bad)==0) {
            print("There are no 'worse' bio sites for comparison for this site.")
            flush.console()
        } else {
            chem.info <- list.data$chem.info
            getVerifiedPredictions(TargetSiteID
                                   , data.SampSummary
                                   , data.bio.taxa.raw = data.bmi.taxa.raw
                                   , data.chem.info
                                   , data.SSTV.totabund
                                   , data.MT.bio = data.MT.bmi
                                   , matchedData = list.ChemBMIData
                                   , ref.sites = list.data$ref.sites
                                   , BioIndex_Val = "IBI"
                                   , BioIndex_Nar = "Quality"
                                   , BioIndex_Nar_Deg = "Degraded"
                                   , dir_results=file.path(wd, "Results"))
            }
    } else {
        print("No possible stressors have stressor-specific tolerance values.")
        flush.console()
    }

    print("getVerifiedPredictions for benthic macroinvertebrates is complete.")
    flush.console()
    # Need to capture lack of SSTV data (either no stressor with SSTVs or no
    # SSTV values for response taxa at site) & not process in that case.
    
    # Get Algae matches
    # list.ChemAlgData <- getBioMatches(stressors
    #                                   , list.data
    #                                   , list.SiteSummary
    #                                   , data.SampSummary
    #                                   , data.chem.raw
    #                                   , data.bio.metrics = data.algae.metrics
    #                                   , biocomm = "algae")
    # Returns: myAlgMatchData <- list(all.a.str = all.malg.stress
    #                        , cl.a.str = cl.malg.stress
    #                        , site.a.str = site.malg.stress
    #                        , all.a.rsp = all.malg.resp
    #                        , cl.a.rsp = cl.malg.resp
    #                        , site.a.rsp = site.malg.resp)
    # print("getBioMatches for algae is complete.")
    # flush.console()
    
    # Test for presence of paired site stress/response data
    # If at least one pair exists, evaluate line of evidence
    # if ((nrow(list.ChemAlgData$site.a.str) > 0)
    #     && (nrow(list.ChemAlgData$site.a.rsp) > 0)) {
    #     getBioStressorResponses(TargetSiteID
    #                             , stressors = stressors
    #                             , BioResp = AlgMetrics
    #                             , list.MatchBioData = list.ChemAlgData
    #                             , LogTransf = stressors_logtransf
    #                             , ref.sites=list.data$ref.sites
    #                             , biocomm = "algae"
    #                             , dir_results=file.path(wd, "Results"))
    #     print("getBioStressorResponses for algae is complete.")
    #     flush.console()
    # }
    
    # Not enabled for Arizona 
    # getSSDs
    # getSSDplot(Data, ResponseType, Taxa, Exposure)
    # myDF <- data_SSD_generator
    # myRT   <- "ResponseType"
    # myTaxa <- "Taxa"
    # myExp  <- "Exposure"
    # Run function
    # p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)
    
    getWoE(TargetSiteID
           , df.rank = list.stressors$site.stressor.pctrank
           , df.coOccur = data.bmi.coOccur
           , biocomm = "bmi"
           , index = "IBI"
           , dir_results = file.path(wd, "Results")
           , CO_sub = "CoOccurrence"
           , SR_sub = "StressorResponse"
           , VP_sub = "VerifiedPredictons"
           , SSD_sub = "SSD")

    getReport(TargetSiteID, dir_results=file.path(wd, "Results")
              , report_type="summary", report_format=report_format
              , dir_rmd=file.path(system.file(package = "CASTfxn"), "rmd"))

    rm(list.SiteSummary, list.data, list.stressors, list.ChemBMIData
       , chem.info, stressors, stressors_logtransf, data.SSTV.totabund)
}

# Determine and print elapsed time
end.time <- Sys.time()
elapsed.time <- end.time - start.time
print(paste(site, "sites completed in", elapsed.time))
flush.console()
rm(site)


