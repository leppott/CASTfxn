# CASTfxn (Specific for SMC)
# Erik.Leppo@tetratech.com, 20180710
# Ann.Lincoln@tetratech.com, 20190630
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v3.5.1
# 
# library(devtools)
# install_github("leppott/CASTfxn")

#rm(list=ls())

#gitpath <- "C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/R"
gitpath <- file.path(system.file(package = "CASTfxn"), "R")

# Set up required functions ### DO NOT CHANGE! #
library(CASTfxn)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
source(file.path(gitpath, "getCoOccurDataset.R"))
source(file.path(gitpath, "getTimeSeq.R"))
source(file.path(gitpath, "getDataSets.R"))
source(file.path(gitpath, "getComparators.R"))
source(file.path(gitpath, "getSiteInfo.R"))
source(file.path(gitpath, "getClusterInfo.R"))
source(file.path(gitpath, "getStressorList.R"))
source(file.path(gitpath, "getCoOccur.R"))
source(file.path(gitpath, "getBioStressorResponses.R"))
source(file.path(gitpath, "getVerifiedPredictions.R"))
source(file.path(gitpath, "getOutliers.R"))
source(file.path(gitpath, "getWoE.R"))
source(file.path(gitpath, "getQualSites.R"))
source(file.path(gitpath, "getSummaryAllSites.R"))
source(file.path(gitpath, "getReport.R"))

not_all_na <- function(x) {!all(is.na(x))}

startprep.time <- Sys.time()

# Required user-designated options
#wd <- "C:/Users/ann.lincoln/Documents/SEP_CAST"
wd <- getwd()
dir_data <- file.path(wd, "Data")
dir_results <- file.path(wd, "Results")

printClusterInfo <- FALSE

removeOutliers <- TRUE
useBC <- TRUE # Use Bray-Curtis biological dissimilarity distance matrix
probsHigh=0.75
probsLow=0.25
DOlim=7
pHlimLow=6.5
pHlimHigh=9
lagdays=10
biocommlist <- c("bmi","algae")
siteQual2Plot = "not degraded" # options:"reference","better than","not degraded"
report_format="html"    # word, pdf are the other options

# Specify Base Filenames # These are the files used to run the analyses
fn.targets <- file.path(dir_data,"SMCTestSites.xlsx")
fn.Sites.Info <- file.path(dir_data,"SMCSitesFinal.tab")
fn.SampSummary <- file.path(dir_data,"SMCSiteSummary.tab")
fn.cheminfo <- file.path(dir_data,"SMCMeasStressInfoFinal.tab")
fn.chemdata <- file.path(dir_data,"SMCMeasStressDataFinal.tab")
fn.modelinfo <- file.path(dir_data,"SMCModelStressInfoFinal.tab")
fn.modeldata <- file.path(dir_data,"SMCModelStressDataFinal.tab")
fn.bmi.metrics <- file.path(dir_data,"SMCBenthicMetricsFinal.tab")
fn.bmi.cscicore <- file.path(dir_data,"SMCBenthicCSCIcore.tab")
fn.bmi.metrics.info <- file.path(dir_data,"SMCBenthicMetricsInfo.tab")
fn.bmi.raw <- file.path(dir_data, "SMCBenthicCountsFinal.tab")
fn.MT.bmi <- file.path(dir_data, "SMCBenthicMasterTaxa.tab")
fn.alg.metrics <- file.path(dir_data, "SMCAlgaeMetricsFinal.tab")
fn.alg.metrics.info <- file.path(dir_data, "SMCAlgaeMetricsInfo.tab")
fn.alg.raw <- file.path(dir_data, "SMCAlgaeCountsFinal.tab")
fn.MT.alg <- file.path(dir_data, "SMCAlgaeMasterTaxa.tab")
fn.bcdist <- file.path(dir_data, "SMCBCDist.tab")
fn.cluster <- file.path(dir_data, "SMCClusterData.tab")
fn.clusterinfo <- file.path(dir_data,"SMCClusterInfo.tab")
fn.bkgdata <- file.path(dir_data, "SMCSiteBkgdData.tab")
fn.bkginfo <- file.path(dir_data, "SMCSiteBkgdInfo.tab")

outline <- rgdal::readOGR(dsn = file.path(dir_data,"SMCBoundary"), layer = "SMCBoundary_aea")
flowline <- rgdal::readOGR(dsn = file.path(dir_data,"SMCReaches"), layer = "SMCReaches_aea")

# Specify user-defined variables
# Stressors
meas.stress <- c("ChemSampleID", "PhabSampID", "FldChemSampID")
chem.stress <- c("ChemSampleID", "FldChemSampID")
hab.stress <- "PhabSampID"
mod.stress <- "FlowSampID"

# BMI responses
bmi_thresholds <- c(-2, 0.62, 0.799, 0.919, 2)
bmi_narrative <- c("very likely altered", "likely altered"
                   , "possibly altered", "likely intact")
bmi_deg_thres <- c(-2, 0.799, 2)
bmi_deg_text <- c("Yes", "No")
bmiIndexGp <- c("CSCI", "OoverE", "MMI")
bmiResp <- "BMISampID"
bmiRespDate <- "BMISampDate"

# Algal responses
alg_thresholds <- c(-2, 0.82, 2)
alg_narrative <- c("Degraded", "Not Degraded")
alg_deg_thres <- c(-2, 0.82, 2)
alg_deg_text <- c("Yes", "No")
algIndexGp <- c("MMIhybrid", "MMIdiatom", "MMIsba")
algResp <- "AlgSampID"
algRespDate <- "AlgSampDate"

# USGS aea for SoCal is below
socal.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
                +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
                +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# aea used for AZ is below
# az.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
#             +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
my.aea = socal.aea

# Read datafiles
## Get site location info and other metadata (e.g., waterbody name)
data_Sites <- read.delim(fn.Sites.Info, header = TRUE, sep = "\t")
rm(fn.Sites.Info)

# Get sample summary data
data_SampSummary <- read.delim(fn.SampSummary, header = TRUE, sep = "\t")
data_mods        <- data_ReachMod   # Check this
data_303d        <- data_303d       # Check this
rm(fn.SampSummary)

# CAST, Chem & other measured data ####
## Get metadata for all measured stressors
data_chemInfo <- read.delim(fn.cheminfo, header = TRUE, sep = "\t")
data_chemInfo <- mutate(data_chemInfo, Analyte = StdParamName)
colMeasInvScore = as.vector(data_chemInfo$StdParamName[data_chemInfo$DirIncStress=="Dec"])
SSTVparms <- unique(data_chemInfo$StdParamName[data_chemInfo$SSTV==1])
rm(fn.cheminfo)

# Get metadata for modeled stressor data
data_modelInfo <- read.delim(fn.modelinfo, header = TRUE, sep = "\t")
data_modelInfo <- mutate(data_modelInfo, Analyte = StdParamName)
colModelInvScore = as.vector(data_modelInfo$StdParamName[data_modelInfo$DirIncStress=="Dec"])
rm(fn.modelinfo)

# Combine metadata for all stressor into one datafile
chemMetaNames <- colnames(data_chemInfo)
modelMetaNames <- colnames(data_modelInfo)
extraNames <- chemMetaNames[!(chemMetaNames %in% modelMetaNames)]
for (e in 1:length(extraNames)) {
    newCol <- extraNames[e]
    data_modelInfo[[newCol]] <- NA
}
data_modelInfo <- data_modelInfo[,chemMetaNames]
data_stressInfo <- rbind(data_chemInfo, data_modelInfo)

## Get measured stressor values
data_chemAll <- read.delim(fn.chemdata, header = TRUE, sep = "\t",
                            na.strings = "NA")
analytes      <- data_stressInfo$StdParamName[data_stressInfo$UseInStressorID == 1]
data_chemRaw <- data_chemAll[data_chemAll$StdParamName %in% analytes,]
data_chemRaw <- data_chemRaw %>%
    mutate(SampleDate = lubridate::mdy(SampDate)) %>%
    select(StationID_Master, ChemSampleID, SampDate, StdParamName
           , ResultValue, SampleDate) %>%
    group_by(StationID_Master, ChemSampleID, SampDate, StdParamName
             , SampleDate) %>%
    summarize(MeanResultValue = mean(ResultValue)) %>%
    rename(ResultValue = MeanResultValue)
data_chemRaw <- unique(data_chemRaw)
data_outliers <- getOutliers(df_data = data_chemRaw
                             , df_meta = data_chemInfo)
data_chemRaw <- merge(data_chemRaw, data_outliers
                      , by.x = c("ChemSampleID", "StdParamName", "ResultValue")
                      , by.y = c("ChemSampleID", "StdParamName", "ResultValue")
                      , all.x = TRUE)
data_chemRaw <- data_chemRaw[,c("StationID_Master", "ChemSampleID", "SampDate"
                                , "StdParamName", "ResultValue", "SampleDate"
                                , "IQRmethod", "SDmethod", "Outlier")]
rm(fn.chemdata, data_chemAll)
measParams <- as.vector(unique(data_chemRaw$StdParamName))
algParams <- as.vector(unique(data_chemRaw$StdParamName[grepl("^AFDM|^Chlor_a|^Pheophytin"
                                                              ,data_chemRaw$StdParamName)]))

# Get modeled stressor data
data_modelAll <- read.delim(fn.modeldata, header = TRUE, sep = "\t")
useParams      <- data_modelInfo$StdParamName[data_modelInfo$UseInStressorID == 1]
data_modelRaw <- data_modelAll[data_modelAll$StdParamName %in% useParams,]
data_modelRaw <- data_modelRaw %>%
    mutate(SampYear = lubridate::year(lubridate::mdy(SampDate))
           , SampleDate =  lubridate::mdy(SampDate)) %>%
    select(StationID_Master, ChemSampleID, SampDate, StdParamName
           , ResultValue, SampleDate)
data_modoutliers <- getOutliers(df_data = data_modelRaw
                             , df_meta = data_modelInfo)
data_modelRaw <- merge(data_modelRaw, data_modoutliers
                      , by.x = c("ChemSampleID", "StdParamName", "ResultValue")
                      , by.y = c("ChemSampleID", "StdParamName", "ResultValue")
                      , all.x = TRUE)
data_modelRaw <- data_modelRaw[,c("StationID_Master", "ChemSampleID", "SampDate"
                                , "StdParamName", "ResultValue", "SampleDate"
                                , "IQRmethod", "SDmethod", "Outlier")]
rm(fn.modeldata, data_modelAll)
rm(data_chemInfo, data_modelInfo)

# Identify modeled parameters to keep or delete (per SCCWRP)
modelParams <- as.vector(unique(data_modelRaw$StdParamName))
bmiModelParamsKeep <- c("HighDur_Wet", "HighNum_Dry", "MaxMonthQ_Wet"
                        , "NoDisturb_Average", "Q99_Average", "QmaxIDR_All"
                        , "RBI_Dry")
bmiModelParamsDEL <- setdiff(modelParams, bmiModelParamsKeep)
algModelParamsKeep <- c("HighDur_Dry", "HighNum_Dry", "MaxMonthQ_Dry"
                        , "NoDisturb_Dry", "Qmax_Dry", "QmaxIDR_All")
algModelParamsDEL <- setdiff(modelParams, algModelParamsKeep)
algParamsDEL <- c(algModelParamsDEL, algParams)

# Prepare df_allStress file (write for RPP use)
data_modeltrim <- as.data.frame(data_modelRaw) %>%
    dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
                  , ResultValue, IQRmethod, SDmethod, Outlier) %>%
    dplyr::mutate(SampleDate = NA)
data_meastrim <- as.data.frame(data_chemRaw) %>%
    dplyr::select(StationID_Master, ChemSampleID, StdParamName, SampleDate
                  , ResultValue, IQRmethod, SDmethod, Outlier)
data_Stress <- rbind(data_meastrim, data_modeltrim)
fn.stress4RPP <- file.path(dir_data,"SMC_AllStressData.tab")
fn.stressmeta4RPP <- file.path(dir_data,"SMC_AllStressInfo.tab")
write.table(data_Stress, fn.stress4RPP, append = FALSE, col.names = TRUE
            , row.names = FALSE, sep = "\t")
write.table(data_stressInfo, fn.stressmeta4RPP, append = FALSE, col.names = TRUE
            , row.names = FALSE, sep = "\t")

# Combine measured and modeled parameters with inverse scoring
col_StressInvScore <- c(colMeasInvScore, colModelInvScore)

# CAST, BMI taxonomic data ####
data_BMIcounts <- read.table(fn.bmi.raw, header = TRUE, sep = "\t")

data_MTbmi <- read.table(fn.MT.bmi, header = TRUE, sep = "\t",
                         stringsAsFactors = FALSE)
# data_bmiTaxaRaw <- mutate(data_bmiTaxaRaw, BMI.Metrics.SampID = BMISampID)
rm(fn.bmi.raw, fn.MT.bmi)

# CAST, BMI, metrics ####
data_bmiMetrics <- read.delim(fn.bmi.metrics, header = TRUE, sep = "\t",
                               na.strings = "NA", stringsAsFactors = FALSE)
data_bmiMetrics <- data_bmiMetrics[,c("StationID_Master", "BMISampID"
                                      , "BMISampDate", "Quality", "CSCI"
                                      , "MMI", "OoverE", "Taxonomic_Richness"
                                      , "Intolerant_Percent", "Shredder_Taxa"
                                      , "Clinger_PercentTaxa"
                                      , "Coleoptera_PercentTaxa"
                                      , "EPT_PercentTaxa")]
data_bmiMetrics <- data_bmiMetrics[, unlist(lapply(data_bmiMetrics,
                                            function(x) !all(is.na(x))))]
colnames(data_bmiMetrics) <- c("StationID_Master","BMISampID"
                               , "CollDate", "Quality", "CSCI", "MMI"
                               , "OoverE", "Taxonomic_Richness"
                               , "Intolerant_Percent", "Shredder_Taxa"
                               , "Clinger_PercentTaxa", "Coleoptera_PercentTaxa"
                               , "EPT_PercentTaxa")
data_bmiMetrics <- data_bmiMetrics %>%
    mutate(BMISampDate = lubridate::mdy(CollDate)) %>%
    select(-CollDate)
data_bmiMetrics <- unique(data_bmiMetrics)
rm(fn.bmi.metrics)

data_cscicore <- read.delim(fn.bmi.cscicore, header = TRUE, sep = "\t"
                            , na.strings = "NA", stringsAsFactors = FALSE)
data_cscicore <- data_cscicore[,c("stationid", "county", "smcshed", "latitude"
                                  , "longitude", "stationcode", "sampleid"
                                  , "samplemonth", "sampleday", "sampleyear"
                                  , "collectionmethodcode", "fieldreplicate"
                                  , "count", "pcnt_ambiguous_individuals")]
data_cscicore <- data_cscicore %>%
    mutate(date_text = paste(samplemonth,sampleday,sampleyear,sep="/")
           , BMISampID = paste(stationid, date_text, collectionmethodcode
                             , fieldreplicate, sep = "_")
           , BMISampFlag = ifelse((count<250) & (pcnt_ambiguous_individuals>50)
                                  , "Insufficient individuals and large percent ambiguity"
                                  , ifelse(count<250, "Insufficient individuals"
                                           , ifelse(pcnt_ambiguous_individuals>50
                                                    , "Large percent ambiguity"
                                                    , NA)))) %>%
    rename(StationID_Master = stationid, BMISampCount = count
           , PctAmbigInd = pcnt_ambiguous_individuals) %>%
    select(StationID_Master, BMISampID, BMISampCount, PctAmbigInd, BMISampFlag)
data_cscicore <- unique(data_cscicore)

data_bmiMetrics <- merge(data_bmiMetrics, data_cscicore
                         , by.x = c("StationID_Master", "BMISampID")
                         , by.y = c("StationID_Master", "BMISampID")
                         , all.x = TRUE)

data_tmpbmicount <- unique(data_BMIcounts[,c("BMISampID","SampleTotAbund")])
data_bmiMetrics <- data_bmiMetrics %>%
    mutate(BMISampCount = ifelse(is.na(BMISampCount)
                                 , data_tmpbmicount$SampleTotAbund
                                 , BMISampCount)) %>%
    mutate(BMISampFlag = ifelse(is.na(BMISampFlag) & (BMISampCount < 250)
                                , "Insufficient number of individuals", BMISampFlag))
rm(data_tmpbmicount)

data_bmiMetrics <- data_bmiMetrics %>%
    mutate(BMISampFlag = ifelse(is.na(PctAmbigInd) & is.na(BMISampFlag)
                                , ifelse(BMISampCount >= 250
                                         , paste0("Unknown percent ambiguous individuals")
                                         , paste0("Unknown number of and percent "
                                                  , "ambiguous individuals"))
                                , ifelse(is.na(PctAmbigInd)
                                         , paste0("Insufficient number of and unknown "
                                                  ,"percent ambiguous individuals")
                                         , BMISampFlag)))

# CAST, BMI, metrics metadata ####
data_bmiMetricsInfo <- read.delim(fn.bmi.metrics.info, header = TRUE, sep = "\t",
                              na.strings = "NA", stringsAsFactors = FALSE)
data_bmiMetricsInfo <- data_bmiMetricsInfo[,c("MetricName",	"MetricLabel", "IndexYN")]
bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN=="Yes"])

# Generate co-occurrence data set (same day samples; modeled data match any day)
data_bmiCoOccur <- getCoOccurDataset(dataDir = dir_data
                                     , df_sites = data_Sites
                                     , df_model = data_modelRaw
                                     , df_meas = data_chemRaw
                                     , biocomm = "BMI"
                                     , df_resp = data_bmiMetrics
                                     , index = bmiIndex
                                     , lagdays = lagdays
                                     , removeOutliers = removeOutliers)
# returns df_coOccur as data_bmiCoOccur
bmiParamsKEEP <- setdiff(colnames(data_bmiCoOccur), bmiModelParamsDEL)
data_bmiCoOccur <- dplyr::select(data_bmiCoOccur, bmiParamsKEEP)
# write.table(data_bmiCoOccur, file.path(getwd(),"Results","bmiCoOccur.tab")
#             ,append=FALSE,col.names = TRUE, row.names = FALSE, sep = "\t")

# CAST, Alg, metrics metadata ####
data_AlgMetricsInfo <- read.delim(fn.alg.metrics.info, header = TRUE, sep = "\t",
                                  na.strings = "NA", stringsAsFactors = FALSE)
algMetrics <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN==1])
algMetricsDiscard <- as.vector(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$UseYN==0])
algIndex <- as.character(data_AlgMetricsInfo$MetricName[data_AlgMetricsInfo$IndexYN=="Yes"])

# CAST, Alg, metrics ####
data_AlgMetrics <- read.table(fn.alg.metrics, header = TRUE, sep = "\t",
                                 stringsAsFactors = FALSE)
data_AlgMetrics <- data_AlgMetrics %>%
    mutate(AlgSampDate = lubridate::mdy(AlgSampDate)) %>%
    mutate(AlgSampFlag = NA)
data_AlgMetrics <- dplyr::select(data_AlgMetrics, -algMetricsDiscard)
rm(fn.alg.metrics)

# CAST, Alg taxonomic data ####
data_AlgCounts <- read.table(fn.alg.raw, header = TRUE, sep = "\t")

data_AlgMasterTaxa <- read.table(fn.MT.alg, header = TRUE, sep = "\t",
                         stringsAsFactors = FALSE)
rm(fn.alg.raw, fn.MT.alg)
# 
# # Generate co-occurrence data set (same day samples; modeled data match any day)
data_algCoOccur <- getCoOccurDataset(dataDir = dir_data
                                     , df_sites = data_Sites
                                     , df_model = data_modelRaw
                                     , df_meas = data_chemRaw
                                     , biocomm = "Alg"
                                     , df_resp = data_AlgMetrics
                                     , index = algIndex
                                     , lagdays = lagdays
                                     , removeOutliers = removeOutliers)
# returns df_coOccur as data_algCoOccur
algParamsKEEP <- setdiff(colnames(data_algCoOccur), algParamsDEL)

data_algCoOccur <- dplyr::select(data_algCoOccur, all_of(algParamsKEEP))

# write.table(data_algCoOccur, file.path(getwd(),"Results","algCoOccur.tab")
#             ,append=FALSE,col.names = TRUE, row.names = FALSE, sep = "\t")

# Get cluster data
data_cluster <- read.delim(fn.cluster, header = TRUE, sep = "\t")
rm(fn.cluster)

# Get cluster data metadata
data_clusterInfo <- read.delim(fn.clusterinfo, header = TRUE, sep = "\t")
rm(fn.clusterinfo)

# Get background data (StreamCat)
df_bkgdata <- read.table(fn.bkgdata, header = TRUE, sep = "\t"
                         , na.strings = c("","NA"))

# Get background metadata
df_bkginfo <- read.table(fn.bkginfo, header = TRUE, sep = "\t"
                         , na.strings = c("", "NA")
                         , stringsAsFactors = FALSE)

if (useBC == TRUE) {
    # Get BC dissimilarity distance matrix to subset cluster sites to comparators
    data_BCdist <- read.delim(fn.bcdist, header = TRUE, sep = "\t")
}

# RUN CASTool
# Site Selection ####
df_targets <- read_excel(fn.targets, col_names = TRUE, trim_ws = TRUE, skip = 0)

endprep.time <- Sys.time()
elapsedprep.time <- endprep.time - startprep.time
print(paste("Prep completed in", elapsedprep.time))
flush.console()

ifelse(!dir.exists(file.path(dir_results))==TRUE
       , dir.create(file.path(dir_results))
       , FALSE)

fn_runstats <- paste0("RunStats_", format.Date(Sys.Date(),"%Y%m%d"), ".tab")
df_runstats <- as.data.frame(cbind("TargetSiteID", "Biocomm", "NumStressors"
                                   , "NumLoE", "ElapsedTime"))
write.table(df_runstats, file.path(dir_results,fn_runstats), append = FALSE
            , col.names = FALSE, row.names = FALSE, sep = "\t")


### Evaluate each target site

# TargetSiteID = "905S015201"
# site = 1
# TargetSiteID = "902S01097"
# site = 1
# for (site in 1:length(TargetSiteID)) {

for (site in 1:nrow(df_targets)) {
    startsite.time <- Sys.time()
    TargetSiteID <- df_targets$TargetSiteID[site]
    if (is.na(TargetSiteID)) {
        next()   
    }
    print(paste0("Evaluating site: ",TargetSiteID))
    flush.console()
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Biocomm-independent functions
    
    # Create high-level results folder structure
    dir_sub2 <- TargetSiteID
    ifelse(!dir.exists(file.path(dir_results, dir_sub2))==TRUE
           , dir.create(file.path(dir_results, dir_sub2))
           , FALSE)

    # Establish data gaps file
    gaps <- cbind.data.frame("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
    write.table(gaps, fn.gaps, append = FALSE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
    
    # Identify comparator sites
    # This is predicated on the fact that BC distance is calculated based on
    # expected benthic macroinvertebrate taxa. If there are ever different 
    # BC matrices for different biocomms, then this must move into the biocomm
    # loop or it needs to be run more than once for each biocomm here, since 
    # it's used in getSiteInfo immediately afterward.
    list.CompSites <- getComparators(TargetSiteID
                                     , df_sites = data_Sites
                                     , df_bioCoOccur = data_bmiCoOccur
                                     , bioIndex = bmiIndex
                                     , useBC = useBC
                                     , df_bcdist = data_BCdist
                                     , bc_cutoff = 0.05
                                     , dir_results = dir_results
                                     , dir_sub = "SiteInfo")
    # Returns: myCompSites <- list(comp.sites = comp.sites
    #                             , gap.compsites = gap.statement
    comp_sites <- list.CompSites$comp.sites
    print("getComparators is complete.")
    flush.console()
    
    # Get site information for general use (map, sample summary, etc)
    # Map plots only ref sites, and that's probably for the best
    list.SiteSummary <- getSiteInfo(TargetSiteID = TargetSiteID
                                    , data_Sites = data_Sites
                                    , data_bkgdata = df_bkgdata
                                    , data_bkginfo = df_bkginfo
                                    , data_SampSummary = data_SampSummary
                                    , data_303d = data_303d
                                    , data_bmiMetrics = data_bmiMetrics
                                    , bmiIndexGp = bmiIndexGp
                                    , data_algMetrics = data_AlgMetrics
                                    , algIndexGp = algIndexGp
                                    , comp_sites = comp_sites
                                    , data_cluster = data_cluster
                                    , data_mods = data_mods
                                    , map_proj = my.aea
                                    , map_outline = outline
                                    , map_flowline = flowline
                                    , map_flowline2 = NULL
                                    , dir_photo = file.path(getwd(),"Data","Photos")
                                    , dir_results = dir_results
                                    , dir_sub = "SiteInfo")
    # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo, 
    #                                Samps = mySamps, 
    #                                BMImetrics = myBMImetrics, 
    #                                AlgMetrics = myAlgaeMetrics, 
    #                                COMID = myCOMID, 
    #                                ClustID = myClustID,
    #                                impair = myImpairments,
    #                                mods = myReachMods
    #                                refCOMIDs = myRefCOMIDs)
    print("getSiteInfo is complete.")
    flush.console()
    
    # Get Cluster Info

    if (printClusterInfo==TRUE) {
        getClusterInfo(TargetSiteID
                       , siteCOMID=list.SiteSummary$COMID
                       , siteCluster=list.SiteSummary$ClustID
                       , refSiteCOMIDs=list.SiteSummary$refCOMIDs
                       , data_cluster = data_cluster
                       , data_clusterInfo = data_clusterInfo
                       , dir_results=dir_results
                       , dir_sub="ClusterInfo")
        print("getClusterInfo is complete.")
        flush.console()
    }

    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Prepare flags for types of stressor and response data to use
    avail.data <- data_SampSummary[data_SampSummary$StationID_Master == TargetSiteID,]
    avail.data <- avail.data[,c(6:ncol(avail.data))]
    avail.data <- avail.data %>% select_if(not_all_na)
    samptypes <- names(avail.data)

    if (any(samptypes %in% meas.stress)) { # Either chem or phab samps exist
        useMeasStress = TRUE
        if (!any(samptypes %in% chem.stress)) {         # No chem samps
            gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0
                                                , "No chemistry stressors available.")
            colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")

            gap.phab.stress <- cbind.data.frame("general", "HabStress", 1
                                                , "Habitat stressors available.")
            colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
            
        } else if (!any(samptypes %in% hab.stress)) {   # No habitat samps
            gap.phab.stress <- cbind.data.frame("general", "HabStress", 0
                                                , "No habitat stressors available.")
            colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
            
            gap.chem.stress <- cbind.data.frame("general", "ChemStress", 1
                                                , "Chemistry stressors available.")
            colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
        } else {
            gap.phab.stress <- cbind.data.frame("general", "HabStress", 1
                                                , "Habitat stressors available.")
            colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
            
            gap.chem.stress <- cbind.data.frame("general", "ChemStress", 1
                                                , "Chemistry stressors available.")
            colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
        }
        df_allStress <- data_chemRaw
    } else {    # No measured stressors at all
        useMeasStress = FALSE
        gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0, "No chemistry stressors available.")
        colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
        
        gap.phab.stress <- cbind.data.frame("general", "HabStress", 0, "No habitat stressors available.")
        colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
    } ### End If statement for measured stressors
    
    if (any(samptypes %in% mod.stress)) {
        useModStress = TRUE
        gap.mod.stress <- cbind.data.frame("general", "useModStress", 1, "Modeled stressors available.")
        colnames(gap.mod.stress) <- c("fxnname", "condition", "result", "comment")
        if (exists("df_allStress")==TRUE) {
            df_allStress <- rbind(df_allStress, data_modelRaw)
        } else {
            df_allStress <- data_modelRaw
        }
    } else { 
        useModStress = FALSE 
        gap.mod.stress <- cbind.data.frame("general", "useModStress", 0, "No modeled stressors available.")
        colnames(gap.mod.stress) <- c("fxnname", "condition", "result", "comment")
    } ### End If statement for modeled stressors
    
    if (any(samptypes == bmiResp)) {
        useBMI = TRUE
        gap.bmi.rsp <- cbind.data.frame("general", "useBMI", 1, "BMI responses available.")
        colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
    } else{
        useBMI = FALSE
        gap.bmi.rsp <- cbind.data.frame("general", "useBMI", 0, "No BMI responses available.")
        colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
    } ### End If statement for benthic macroinvertebrate responses
    
    if (any(samptypes == algResp)) {
        useAlg = TRUE
        gap.alg.rsp <- cbind.data.frame("general", "useALG", 1, "Algae responses available.")
        colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
    } else {
        useAlg = FALSE
        gap.alg.rsp <- cbind.data.frame("general", "useALG", 0, "No algae responses available.")
        colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
    } ### End If statement for measured stressorsalgal responses
    
    gaps <- rbind.data.frame(gap.chem.stress, gap.phab.stress, gap.mod.stress
                             , gap.bmi.rsp, gap.alg.rsp)
    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if ((useMeasStress==FALSE) & (useModStress==FALSE)) {
    #     # No stressor data available
    #     gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0, "No chemistry stressors available.")
    #     colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
    # }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
    
    for (b in 1:length(biocommlist)) {
        
        noStressors <- FALSE
        noResponses <- FALSE

        NE_true <- FALSE


        if ((useMeasStress==FALSE) & (useModStress==FALSE)) {
            # No stressor data available
            gap.stress <- cbind.data.frame("general", "Stressors", 0
                                           , "No stressor data available.")
            colnames(gap.stress) <- c("fxnname", "condition", "result"
                                           , "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gap.stress, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            noStressors <- TRUE
        }
        if ((useAlg==FALSE) & (useBMI==FALSE)) {
            # No stressor data available
            gap.resp <- cbind.data.frame("general", "Responses", 0
                                           , "No response data available.")
            colnames(gap.resp) <- c("fxnname", "condition", "result"
                                           , "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gap.resp, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            noResponses <- TRUE
        }
        if ((noStressors==TRUE) | (noResponses==TRUE)) {
            msg <- ifelse((noStressors==TRUE) & (noResponses==TRUE)
                          , paste0("No stressor or response data are available for "
                                   , TargetSiteID)
                          , ifelse(noStressors==TRUE
                                   , paste0("No stressor data are available for "
                                            , TargetSiteID)
                                   , paste0("No response data are available for "
                                            , TargetSiteID)))
            print(msg)
            flush.console()
            next
        }
        
        numLoE = 0
        
        LoEs <- c("TS", "CO", "SR", "VP", "SSD")
        df_LoE <- as.data.frame(LoEs)
        colnames(df_LoE) <- "LoE"
        df_LoE <- df_LoE %>%
            mutate(LoE = as.character(LoE)
                   , Completed = as.integer(0)
                   , ResultsDir = as.character(NA))

        # Define biocomm data
        bioComm <- biocommlist[b]
        if ((bioComm=="bmi") && (useBMI==TRUE)) {
            
            data_bioCoOccur <- data_bmiCoOccur
            bioIndex <- bmiIndex
            bioIndexGp <- bmiIndexGp
            bioMetricNames <- bmiMetrics
            bioMetricData <- data_bmiMetrics
            bioMetricInfo <- data_bmiMetricsInfo
            bioTaxaData <- data_BMIcounts
            bioMasterTaxa <- data_BMIMasterTaxa
            colBio <- bmiIndex
            colBioSample <- bmiResp
            colBioSampDate <- bmiRespDate
            BioNarBrk <- bmi_thresholds
            BioNarLab <- bmi_narrative
            BioDegBrk <- bmi_deg_thres
            BioDegLab <- bmi_deg_text
            modelParams <- bmiModelParamsKeep
            bioParmsDEL <- bmiModelParamsDEL

        } else if ((bioComm=="algae") && (useAlg==TRUE)) {
            
            data_bioCoOccur <- data_algCoOccur
            bioIndex <- algIndex
            bioIndexGp <- algIndexGp
            bioMetricNames <- algMetrics
            bioMetricData <- data_AlgMetrics
            bioMetricInfo <- data_AlgMetricsInfo
            bioTaxaData <- data_AlgCounts
            bioMasterTaxa <- data_AlgMasterTaxa
            colBio <- algIndex
            colBioSample <- algResp
            colBioSampDate <- algRespDate
            BioNarBrk <- alg_thresholds
            BioNarLab <- alg_narrative
            BioDegBrk <- alg_deg_thres
            BioDegLab <- alg_deg_text
            modelParams <- algModelParamsKeep
            bioParmsDEL <- algParamsDEL
            
        } else {
            print(paste0(bioComm, " is not a valid biological community."))
            flush.console()
            next()
        }
        
        # If no paired stressor-response samples for target site, no eval possible
#<<<<<<< 201909_ARL
        if (!(TargetSiteID %in% data_bioCoOccur$StationID_Master)) { # Not in data_bioCoOccur
            noStressors = TRUE
        } else {
            dfTarget <- dplyr::filter(data_bioCoOccur, StationID_Master==TargetSiteID)
            if (all(is.na(dfTarget[,11:ncol(dfTarget)]))) { # In data_bioCoOccur but all values NA
                noStressors = TRUE
            } else {
            noStressors = FALSE
            }
        }
        if (noStressors==TRUE) {
#=======
#        if (!(TargetSiteID %in% data_bioCoOccur$StationID_Master)) {
#>>>>>>> master
            print(paste0("No paired stressor-response samples for", TargetSiteID
                         , " for the ", bioComm, " community."))
            flush.console()
            
            # No identified stressors may be a data gap, but may not be, either
            gapcomment <- paste0("No paired stressor-", bioComm, " samples are available "
                                 , "for ", TargetSiteID, " within ", lagdays, " days, "
                                 , "with the stressor sample being obtained prior "
                                 , "to the response sample.")
            gaps <- cbind.data.frame("getCoOccurDataset", paste0("Paired stressor-"

                                                                 , bioComm, " data"), 0, gapcomment)

            # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            
            # Write run-time stats to file
            endsite.time <- Sys.time()
            elapsedsite.time <- endsite.time - startsite.time
            
            df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                           , "Biocomm" = bioComm
                                           , "NumStressors" = NA
                                           , "NumLoE" = numLoE
                                           , "ElapsedTime" = elapsedsite.time))        
            # if (site == 1) {
            #     df_runstats <- df_temp
            # } else {
            #     df_runstats <- rbind(df_runstats, df_temp)
            # } ### End gather run stats
            write.table(df_temp, file.path(wd,"Results",fn_runstats)
                        , append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")

            rm(dfTarget)
            next()
        } ### End no stressors statement

        # Run analyses
        # Identify "quality" sites using different definitions
        list.BioQualSites <- getQualSites(TargetSiteID
                                          , df_sites = data_Sites
                                          , biocomm = bioComm
                                          , df_qual = data_bioCoOccur
                                          , colBio = colBio
                                          , colBioSample = "RespSampID"
                                          , colStressSample = "StressSampID"
                                          , comp_sites = comp_sites
                                          , useBC = useBC
                                          , BioNarBrk = BioNarBrk
                                          , BioNarLab = BioNarLab
                                          , BioDegBrk = BioDegBrk
                                          , BioDegLab = c("Yes", "No")
                                          , dir_results = dir_results)
        # Returns: myQualSites <- list(dfQuality = df_qual
        #                              , allRefBioSites = all.ref
        #                              , allRefBioRespSamps = all.ref.samps.bio
        #                              , allRefBioStressSamps = all.ref.samps.stress
        #                              , allRefBioReaches = all.ref.reaches
        #                              , allGoodBioSites = all.good
        #                              , allGoodBioRespSamps = all.samp.good.bio
        #                              , allGoodBioStressSamps = all.samp.good.stress
        #                              , allGoodBioReaches = all.good.reaches
        #                              , allBTBioSites = all.better
        #                              , allBTBioRespSamps = all.samp.better.bio
        #                              , allBTBioStressSamps = all.samp.better.stress
        #                              , allBTBioReaches = all.better.reaches)

        allBioRefSites <- switch(siteQual2Plot
                                 , "reference"=list.BioQualSites$allRefBioSites
                                 , "not degraded"=list.BioQualSites$allGoodBioSites
                                 , "better than"=list.BioQualSites$allBTBioSites)
        allBioRefRespSamps <- switch(siteQual2Plot
                                     , "reference"=list.BioQualSites$allRefBioRespSamps
                                     , "not degraded"=list.BioQualSites$allGoodBioRespSamps
                                     , "better than"=list.BioQualSites$allBTBioRespSamps)
        allBioRefStressSamps <- switch(siteQual2Plot
                                       , "reference"=list.BioQualSites$allRefBioStressSamps
                                       , "not degraded"=list.BioQualSites$allGoodBioStressSamps
                                       , "better than"=list.BioQualSites$allBTBioStressSamps)
        allBioRefReaches <- switch(siteQual2Plot
                                   , "reference"=list.BioQualSites$allRefBioReaches
                                   , "not degraded"=list.BioQualSites$allGoodBioReaches
                                   , "better than"=list.BioQualSites$allBTBioReaches)
        print(paste0("getQualSites is complete for ", bioComm, "."))
        flush.console()        
        
        # Get data sets for stressors paired with response data, if available
        listPairedStressResp <- getDataSets(TargetSiteID
                                            , compSites = comp_sites
                                            , df_coOccur = data_bioCoOccur
                                            , measParams = measParams
                                            , modelParams = modelParams
                                            , biocomm = bioComm
                                            , bioIndex = bioIndex
                                            , colBioSample = colBioSample
                                            , colBioSampDate = colBioSampDate
                                            , df_biometrics = bioMetricData
                                            , df_stressinfo = data_stressInfo)
        # Returns: mySubsets <- list(siteStressInfo = df_stressinfo
        #                   , allBioStress = allBioStressData
        #                   , compBioStress = compBioStressData
        #                   , siteBioStress = siteBioStressData
        #                   , allBioResp = allBioRespData
        #                   , compBioResp = compBioRespData
        #                   , siteBioResp = siteBioRespData)
        print("Stressor and response data prepared, for all possible stressors.")
        flush.console()
        
        compPairedSR <- listPairedStressResp$compBioStress %>%
            select(-StressSampDate, -RespSampDate, -RespSampID)
        sitePairedSR <- listPairedStressResp$siteBioStress %>%
            select(-StressSampDate, -RespSampDate, -RespSampID)
        sitePairedStressors <- as.vector(colnames(sitePairedSR[,3:ncol(sitePairedSR)]))
        
        # Prepare data sets of all stressors ever detected at the target site
        if (removeOutliers == TRUE) {
            siteStressAll <- data_Stress %>%
                dplyr::filter(StationID_Master==TargetSiteID) %>%
                dplyr::filter(!is.na(ResultValue)) %>%
                dplyr::filter(Outlier != "Outlier") %>%
                tidyr::spread(key=StdParamName, value=ResultValue) %>%
#<<<<<<< 201909_ARL
                dplyr::rename(StressSampID = ChemSampleID
                              , StressSampDate = SampleDate)
            if (ncol(siteStressAll)>7) {
                siteStressAllCore <- siteStressAll[1:6]
                siteStressAllParms <- siteStressAll[,7:ncol(siteStressAll)] %>%
                    dplyr::select_if(not_all_na)
                siteStressAll <- cbind(siteStressAllCore, siteStressAllParms)
                rm(siteStressAllCore, siteStressAllParms)
            }
#=======
#                dplyr::select_if(not_all_na) %>%
#                dplyr::rename(StressSampID = ChemSampleID
#                              , StressSampDate = SampleDate)
#>>>>>>> master
            siteDetectsAll <- as.vector(colnames(siteStressAll[,4:ncol(siteStressAll)]))
            compStressAll <- data_Stress %>%
                dplyr::filter(StationID_Master %in% comp_sites) %>%
                dplyr::filter(!is.na(ResultValue)) %>%
                dplyr::filter(Outlier != "Outlier") %>%
                dplyr::filter(StdParamName %in% siteDetectsAll) %>%
                tidyr::spread(key=StdParamName, value=ResultValue) %>%
                dplyr::rename(StressSampID = ChemSampleID
                              , StressSampDate = SampleDate)
            siteRespAll <- bioMetricData %>%
                dplyr::filter(StationID_Master == TargetSiteID) %>%
                dplyr::rename(RespSampID = eval(colBioSample)
                              , RespSampDate = eval(colBioSampDate))
        } else {
            siteStressAll <- data_Stress %>%
                dplyr::filter(StationID_Master==TargetSiteID) %>%
                dplyr::filter(!is.na(ResultValue)) %>%
#<<<<<<< 201909_ARL
                dplyr::filter(Outlier != "Outlier") %>%
                tidyr::spread(key=StdParamName, value=ResultValue) %>%
                dplyr::rename(StressSampID = ChemSampleID
                              , StressSampDate = SampleDate)
            siteStressAll <- dplyr::select_if(siteStressAll
                                              , not_all_na(siteStressAll[7:ncol(siteStressAll)]))
#=======
#                tidyr::spread(key=StdParamName, value=ResultValue) %>%
#                dplyr::select_if(not_all_na) %>%
#                dplyr::rename(StressSampID = ChemSampleID
#                              , StressSampDate = SampleDate)
#>>>>>>> master
            siteDetectsAll <- as.vector(colnames(siteStressAll[,4:ncol(siteStressAll)]))
            compStressAll <- data_Stress %>%
                dplyr::filter(StationID_Master %in% comp_sites) %>%
                dplyr::filter(!is.na(ResultValue)) %>%
                dplyr::filter(StdParamName %in% siteDetectsAll) %>%
                tidyr::spread(key=StdParamName, value=ResultValue) %>%
                dplyr::rename(StressSampID = ChemSampleID
                              , StressSampDate = SampleDate)
            siteRespAll <- bioMetricData %>%
                dplyr::filter(StationID_Master == TargetSiteID) %>%
                dplyr::rename(RespSampID = eval(colBioSample)
                              , RespSampDate = eval(colBioSampDate))
        }

        # Log removed outliers as data gaps
        data_StressLabeled <- merge(data_Stress, data_stressInfo[,c("Analyte","Label")]
                           , by.x="StdParamName", by.y="Analyte", all.x= TRUE)
        siteOutliers <- data_StressLabeled %>%
            dplyr::filter(StationID_Master==TargetSiteID) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(Outlier == "Outlier")
        compOutliers <- data_StressLabeled %>%
            dplyr::filter(StationID_Master %in% comp_sites) %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(Outlier == "Outlier")
        allOutliers <- data_StressLabeled %>%
            dplyr::filter(!is.na(ResultValue)) %>%
            dplyr::filter(Outlier == "Outlier")

        if (nrow(siteOutliers)>0) {
            for (r in 1:nrow(siteOutliers)) {
                stressor <- siteOutliers$StdParamName[r]
                strLabel <- siteOutliers$Label[r]
                result <- siteOutliers$ResultValue[r]
                siteID <- as.character(siteOutliers$StationID_Master[r])
                gapcomment <- paste0(siteID
                                     , " value removed as an outlier."
                                     , " Transformation applied prior to"
                                     , " identification as necessary.")
                gaps <- cbind.data.frame("Site outliers", strLabel, result
                                         , gapcomment)
                colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                            , row.names = FALSE, sep = "\t")
            }
        }
        if (nrow(compOutliers)>0) {
            for (r in 1:nrow(compOutliers)) {
                stressor <- compOutliers$StdParamName[r]
                strLabel <- compOutliers$Label[r]
                result <- compOutliers$ResultValue[r]
                siteID <- as.character(compOutliers$StationID_Master[r])
                if (siteID != TargetSiteID) {
                    gapcomment <- paste0(siteID
                                         , " value removed as an outlier."
                                         , " Transformation applied prior to"
                                         , " identification as necessary.")
                    gaps <- cbind.data.frame("Comparator outliers", strLabel, result
                                             , gapcomment)
                    colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                    fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                                , row.names = FALSE, sep = "\t")
                }
            }
        }
        if (nrow(allOutliers)>0) {
            for (r in 1:nrow(allOutliers)) {
                stressor <- allOutliers$StdParamName[r]
                strLabel <- allOutliers$Label[r]
                result <- allOutliers$ResultValue[r]
                siteID <- as.character(allOutliers$StationID_Master)[r]
                if (!(siteID %in% comp_sites)) {
                    gapcomment <- paste0("Value removed as an outlier for site "
                                         , siteID
                                         , " Transformation applied prior to"
                                         , " identification as necessary.")
                    gaps <- cbind.data.frame("All data outliers", strLabel, result
                                             , gapcomment)
                    colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                    fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                                , row.names = FALSE, sep = "\t")
                }
            }
        }

        # Get Stressor List using all stressors ever detected at the target site
        list.stressors <- getStressorList(TargetSiteID
                                          , siteCluster=list.SiteSummary$ClustID
                                          , chemInfo=data_stressInfo
                                          , clusterChem=compStressAll
                                          , siteQual2Plot=siteQual2Plot
                                          , refSamps=allBioRefStressSamps
                                          , refSites=allBioRefSites
                                          , siteChem=siteStressAll
                                          , probsHigh=probsHigh
                                          , probsLow=probsLow
                                          , DOlim=DOlim
                                          , pHlimLow=pHlimLow
                                          , pHlimHigh=pHlimHigh
                                          , biocomm=bioComm
                                          , bioParmsDEL=bioParmsDEL
                                          , dir_results=dir_results
                                          , dir_sub="CandidateCauses")
        # Returns: myStressors <- list(stressors = stressorlist
        #                     , site.stressor.pctrank = site.pctrank
        #                     , stressors_LogTransf
        #                     , stressors_Excepted)
        stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
        stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
        print("getStressorList is complete.")
        flush.console()
        
        stressorsNOpairing <- setdiff(stressors, sitePairedStressors)
        stressorsWPairedResponses <- intersect(stressors, sitePairedStressors)
        
        ### MODIFY siteStressAll to keep all core cols and only stressor cols

        # If no stressors are identified, no analyses can be performed. Error msg.
        if (length(stressors) == 0) {
            print(paste("No stressors identified for", TargetSiteID))
            flush.console()
            
            # No identified stressors may be a data gap, but may not be, either
            gapcomment <- paste0("No potential stressors fall outside the specified "
                                 , "quantile range (", probsLow, " to ", probsHigh,").")
            gaps <- cbind.data.frame("getStressorList", "Number of stressors", 0
                                               , gapcomment)
            colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            
            # Write run-time stats to file
            endsite.time <- Sys.time()
            elapsedsite.time <- endsite.time - startsite.time
            
            df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                           , "Biocomm" = bioComm
                                           , "NumStressors" = length(stressors)
                                           , "NumLoE" = numLoE
                                           , "ElapsedTime" = elapsedsite.time))        
            write.table(df_temp, file.path(wd,"Results",fn_runstats)
                        , append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
            next()
        } ### End no stressors statement
        
        if (length(stressorsNOpairing)>0) {
            for (s in 1:length(stressorsNOpairing)) {
                # Candidate causes identified as possible stressors but without
                # paired response data to allow evaluation
                # Grab labels instead of stdparamname
                stressname <- stressorsNOpairing[s]
                strLabel <- unique(as.character(data_stressInfo$Label[data_stressInfo$Analyte==stressname]))
                gapcomment <- paste0("Stressor detected but paired response "
                                     ,"data are not available.")
                gaps <- cbind.data.frame("getStressorList", strLabel, 0
                                                   , gapcomment)
                colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                            , row.names = FALSE, sep = "\t")
            }
        } ### End unpaired stressors statement
        
        if (length(stressorsWPairedResponses)==0) {
            NE_true <- TRUE
            # Candidate causes identified as stressors had no response sample
            # obtained within lagdays following the stressor sample collection
            gapcomment <- paste0("No identified possible stressors had a response "
                                 , "sample obtained within ", lagdays, " days of "
                                 , "stressor sample collection.")
            gaps <- cbind.data.frame("getStressorList", "Paired stresssor/responses"
                                     , 0, gapcomment)
            colnames(gaps) <- c("fxnname", "condition", "result", "comment")
            fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
            fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
            write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")
        } else {
            NE_true <- FALSE
            stressorsUsed <- as.data.frame(stressorsWPairedResponses)
            colnames(stressorsUsed)[1] <- "Stressor"
            stressorsUsed <- merge(stressorsUsed
                                   , data_stressInfo[,c("Analyte","Label")]
                                   , by.x = "Stressor"
                                   , by.y = "Analyte"
                                   , all.x = TRUE)
            stressorsUsed <- unique(stressorsUsed)
            fn.stressorsUsed <- file.path(dir_results,TargetSiteID
                                          , toupper(bioComm)
                                          , "CandidateCauses/"
                                          , paste0(TargetSiteID, "_"
                                                   ,toupper(bioComm)
                                                   , "_CandCauses_StressorsEvaluated.tab"))
            write.table(stressorsUsed, fn.stressorsUsed, append = FALSE
                        , col.names = TRUE, row.names = FALSE, sep = "\t")

        } # End paired stressors statement

        
        # Either all are paired or some are
        stressors_logtransf <- data_stressInfo$LogTransf[data_stressInfo$StdParamName 
                                                         %in% stressorsWPairedResponses]
        
#<<<<<<< 201909_ARL

#=======
        # Adjust siteStressAll to reflect only stressors used
        # siteStressAllData <- cbind(siteStressAll[,1:6]
        #                            , siteStressAll[[stressors]])
        # siteStressPaired <- cbind(siteStressAll[,1:6]
        #                           , siteStressAll[[stressorsWPairedResponses]])
#
#        # Create time sequence graphics
#        # Uses all site stressor and response data, but not paired
#        getTimeSeq(TargetSiteID
#                   , biocomm = bioComm
#                   , BioResp = bioMetricNames
#                   , df_stress = siteStressAll
#                   , df_resp = siteRespAll
#                   , stressors = stressorsWPairedResponses
#                   , df_stressinfo = data_stressInfo
#                   , df_respinfo = bioMetricInfo
#                   , dir_results = dir_results
#                   , dir_sub = "TimeSequence")
#        print(paste0("getTimeSeq for ", bioComm, " is complete."))
#        flush.console()
#        
#        # NOT WORKING
#        dirTS <- file.path(dir_results, TargetSiteID, toupper(bioComm)
#                           , "TimeSequence")
#        if (dir.exists(dirTS)==TRUE) {
#            if (length(list.files(dirTS)) > 0) {
#                numLoE = numLoE + 1
#                df_LoE$Completed[df_LoE$LoE == "TS"] <- 1
#                df_LoE$ResultsDir[df_LoE$LoE == "TS"] <- dirTS
#                
#            }
#        }
#        
#        
#>>>>>>> master
        if (NE_true) { # No paired stressor response data available. Move to next biocomm or site.
            # Write run-time stats to file
            endsite.time <- Sys.time()
            elapsedsite.time <- endsite.time - startsite.time
            
            df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                           , "Biocomm" = bioComm
                                           , "NumStressors" = length(stressors)
                                           , "NumLoE" = numLoE
                                           , "ElapsedTime" = elapsedsite.time))        
            # if (site == 1) {
            #     df_runstats <- df_temp
            # } else {
            #     df_runstats <- rbind(df_runstats, df_temp)
            # } ### End gather run stats
            write.table(df_temp, file.path(wd,"Results",fn_runstats)
                        , append = TRUE, col.names = FALSE
                        , row.names = FALSE, sep = "\t")


        } else {
            
            # Create time sequence graphics
            # Uses all site stressor and response data, but not paired
            getTimeSeq(TargetSiteID
                       , biocomm = bioComm
                       , BioResp = bioMetricNames
                       , df_stress = siteStressAll
                       , df_resp = siteRespAll
                       , stressors = stressorsWPairedResponses
                       , df_stressinfo = data_stressInfo
                       , df_respinfo = bioMetricInfo
                       , dir_results = dir_results
                       , dir_sub = "TimeSequence")
            print(paste0("getTimeSeq for ", bioComm, " is complete."))
            flush.console()
            
            # NOT WORKING
            dirTS <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                               , "TimeSequence")
            if (dir.exists(dirTS)==TRUE) {
                if (length(list.files(dirTS)) > 0) {
                    numLoE = numLoE + 1
                    df_LoE$Completed[df_LoE$LoE == "TS"] <- 1
                    df_LoE$ResultsDir[df_LoE$LoE == "TS"] <- dirTS
                    
                }
            }
            
            # Get Response-based co-occurrence
            if (TargetSiteID %in% unique(data_bioCoOccur$StationID_Master)) {
                print("Starting Co-occurrence")
                flush.console()
                getCoOccur(df_data = data_bioCoOccur
                           , TargetSiteID = TargetSiteID
                           , col_ID = "StationID_Master"
                           , colStressSamp = "StressSampID"
                           , colRespSamp = "RespSampID"
                           , colGroup = "clust"
                           , colBio = colBio
                           , colStressors = stressorsWPairedResponses
                           , df_stressinfo = data_stressInfo
                           , BioNarBrk = BioNarBrk
                           , BioNarLab = BioNarLab
                           , BioDegBrk = BioDegBrk
                           , BioDegLab = c("Yes", "No")
                           , biocomm = bioComm
                           , dir_plots = dir_results
                           , dir_sub = "CoOccurrence"
                           , col_StressInvScore = col_StressInvScore)
            } else {
                # gapcomment <- "Stressor detected but paired response not available"
                # gaps <- cbind.data.frame("getStressorList", stressorsNOpairing[s], 0
                #                          , gapcomment)
                # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                # fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                # write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                #             , row.names = FALSE, sep = "\t")
            } ### End getCoOccur
            print(paste0("getCoOccur for ", bioComm, " is complete."))
            flush.console()
            
            dirCO <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                               , "CoOccurrence")
            if (dir.exists(dirCO)==TRUE) {
                if ((length(list.files(dirCO)) > 0)==TRUE) {
                    numLoE = numLoE + 1
                    df_LoE$Completed[df_LoE$LoE == "CO"] <- 1
                    df_LoE$ResultsDir[df_LoE$LoE == "CO"] <- dirCO
                }
            }
            
            # Refine all.b.str, cl.b.str, and site.b.str for just identified stressors
            core.cols <- c("StationID_Master", "StressSampDate", "RespSampDate"
                           , "StressSampID", "RespSampID")
            
            all.b.str <- listPairedStressResp$allBioStress %>%
                select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
                select(StressSampID, RespSampID, StationID_Master
                       , eval(stressorsWPairedResponses))
            cl.b.str <- listPairedStressResp$compBioStress %>%
                select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
                select(StressSampID, RespSampID, StationID_Master
                       , eval(stressorsWPairedResponses))
            site.b.str <- listPairedStressResp$siteBioStress %>%
                select(eval(core.cols), eval(stressorsWPairedResponses)) %>%
                select(StressSampID, RespSampID, StationID_Master
                       , eval(stressorsWPairedResponses))
            
            all.b.rsp <- listPairedStressResp$allBioResp %>%
                select(RespSampID, StressSampID, StationID_Master, RespSampDate
                       , Quality, eval(bioMetricNames))
            cl.b.rsp <- listPairedStressResp$compBioResp %>%
                select(RespSampID, StressSampID, StationID_Master, RespSampDate
                       , Quality, eval(bioMetricNames))
            site.b.rsp <- listPairedStressResp$siteBioResp %>%
                select(RespSampID, StressSampID, StationID_Master, RespSampDate
                       , Quality, eval(bioMetricNames))
            
            siteStressInfo <- listPairedStressResp$siteStressInfo
            
            list_MatchBioData <- list("all.b.str" = all.b.str
                                      , "cl.b.str" = cl.b.str
                                      , "site.b.str" = site.b.str
                                      , "all.b.rsp" = all.b.rsp
                                      , "cl.b.rsp" = cl.b.rsp
                                      , "site.b.rsp" = site.b.rsp)
            
            # Get Stressor Responses
            getBioStressorResponses(TargetSiteID
                                    , stressors = stressorsWPairedResponses
                                    , stressorInfo = siteStressInfo
                                    , BioResp = bioMetricNames
                                    , BioInfo = bioMetricInfo
                                    , list.MatchBioData = list_MatchBioData
                                    , ref.sites = allBioRefStressSamps
                                    , siteQual2Plot = siteQual2Plot
                                    , biocomm = bioComm
                                    , dir_results = dir_results
                                    , dir_sub = "StressorResponse")
            print(paste0("getBioStressorResponses for ", bioComm, " is complete."))
            flush.console()
            
            dirSR <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                               , "StressorResponse")
            if (dir.exists(dirSR)==TRUE) {
                if (length(list.files(dirSR)) > 0) {
                    numLoE = numLoE + 1
                    df_LoE$Completed[df_LoE$LoE == "SR"] <- 1
                    df_LoE$ResultsDir[df_LoE$LoE == "SR"] <- dirSR
                }
            }
            
            # Get Stressor-specific regressions
            if (any(SSTVparms %in% stressorsWPairedResponses)) {
                getVerifiedPredictions(TargetSiteID
                                       , SSTVanalytes = as.character(SSTVparms)
                                       , colBioSample = colBioSample
                                       , stressors = stressorsWPairedResponses
                                       , stressorInfo <- siteStressInfo
                                       , dataBioTaxa = bioTaxaData
                                       , dataMasterTaxa = bioMasterTaxa
                                       , matchedData = list_MatchBioData
                                       , BioIndex_Val = bioIndex
                                       , BioIndex_Nar = "Quality"
                                       , BioIndex_Nar_Deg = "Degraded"
                                       , dir_results=dir_results
                                       , dir_sub="VerifiedPredictions"
                                       , biocomm=bioComm)
            } else {
                print("No possible stressors have stressor-specific tolerance values.")
                flush.console()
                gapcomment <- paste0("Stressors having stressor-specific tolerance "
                                     , "values are not identified at this site.")
                gaps <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0
                                         , gapcomment)
                colnames(gaps) <- c("fxnname", "condition", "result", "comment")
                fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
                fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
                write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                            , row.names = FALSE, sep = "\t")
            } ### End getVP evaluation
            
            print(paste0("getVerifiedPredictions for ", bioComm, " is complete."))
            flush.console()
            
            dirVP <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                               , "VerifiedPredictions")
            if (dir.exists(dirVP)==TRUE) {
                if (length(list.files(dirVP)) > 0) {
                    numLoE = numLoE + 1
                    df_LoE$Completed[df_LoE$LoE == "VP"] <- 1
                    df_LoE$ResultsDir[df_LoE$LoE == "VP"] <- dirVP
                }
            }
            
            # # Not enabled yet
            # # getSSDs
            # # getSSDplot(Data, ResponseType, Taxa, Exposure)
            # # myDF <- data_SSD_generator
            # # myRT   <- "ResponseType"
            # # myTaxa <- "Taxa"
            # # myExp  <- "Exposure"
            # # Run function
            # # p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)
            
            getWoE(TargetSiteID
                   , biocomm = bioComm
                   , index = bioIndex
                   , dir_results = dir_results
                   , dfLoE = df_LoE
                   , dfQual = list.BioQualSites$dfQuality
                   , dfStr = list_MatchBioData$site.b.str
                   , dfRank = list.stressors$site.stressor.pctrank
                   , dfStressInfo = siteStressInfo
                   , df_coOccur = data_bioCoOccur
                   , BioResp = bioMetricNames)
            print(paste0("getWoE for ", bioComm, " is complete."))
            flush.console()
            
        }


        # Write run-time stats to file
        endsite.time <- Sys.time()
        elapsedsite.time <- endsite.time - startsite.time
        
        df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                       , "Biocomm" = bioComm
                                       , "NumStressors" = length(stressors)
                                       , "NumLoE" = numLoE
                                       , "ElapsedTime" = elapsedsite.time))        
#<<<<<<< 201909_ARL
        if (site == 1) {
            df_runstats <- df_temp
        } else {
            df_runstats <- rbind(df_runstats, df_temp)
        } ### End gather run stats
        write.table(df_temp, file.path(wd,"Results",fn_runstats)
                    , append = TRUE, col.names = FALSE
                    , row.names = FALSE, sep = "\t")
        
#=======
 #       # if (site == 1) {
 #       #     df_runstats <- df_temp
 #       # } else {
 #       #     df_runstats <- rbind(df_runstats, df_temp)
#        # } ### End gather run stats
#        write.table(df_temp, file.path(wd,"Results",fn_runstats)
#                               , append = TRUE, col.names = FALSE
#                               , row.names = FALSE, sep = "\t")
#
#>>>>>>> master
    } ### End biocomm loop
    
    # Get final report (Executive Summary style)
    getReport(TargetSiteID
              , probsHigh=probsHigh
              , probsLow=probsLow
              , useBMI=useBMI
              , useAlg=useAlg
#<<<<<<< 201909_ARL
              , useBC=TRUE
              , removeOutliers=removeOutliers
              , lagdays=lagdays
              , bmiIndex=bmiIndex
              , algIndex=algIndex
              , dir_data=dir_data
              , dir_results=dir_results
              , report_type="summary"
              , report_format="html"
               , dir_rmd=file.path(system.file(package = "CASTfxn"), "rmd")
              #, dir_rmd="C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd")
#=======
#              , removeOutliers=removeOutliers
#              , dir_results=file.path(getwd(), "Results")
#              , report_type="summary"
#              , report_format="html"
#               , dir_rmd=file.path(system.file(package = "CASTfxn"), "rmd"))
#             # , dir_rmd="C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/inst/rmd")
#>>>>>>> master

    # rm(list.SiteSummary, list.data, list.stressors, list.ChemBMIData
    #    , chem.info, stressors, stressors_logtransf, data.SSTV.totabund)
    # 
    
    dfGaps <- read.table(file.path(dir_results, TargetSiteID
                                   , paste0(TargetSiteID,"_datagaps.tab"))
                         , header = TRUE, sep="\t")
    dfGaps <- unique(dfGaps)
    write.table(dfGaps, file.path(dir_results, TargetSiteID
                                  , paste0(TargetSiteID,"_datagaps.tab"))
                , append = FALSE, col.names = TRUE, row.names = FALSE
                , sep = "\t")

} ### End TargetSite loop

rm(site)

getSummaryAllSites(biocommlist = c("bmi", "algae")
                   , bmiIndex = "CSCI"
                   , algIndex = "MMIhybrid"

                   , dir_data = dir_data
                   , dir_results = dir_results
                   , dir_sub = "WoE"
                   , df_sites = NULL)

rm(list=ls())

