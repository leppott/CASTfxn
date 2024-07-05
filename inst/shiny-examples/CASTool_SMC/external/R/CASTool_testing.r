# Copyright 2024 TetraTech. All rights reserved.
# Use, copying, modification, or distribution of this file or any of its contents
# is expressly prohibited without prior written permission of TetraTech.
#
#
# CASTfxn (Generic)
# Erik.Leppo@tetratech.com, 20180710
# Ann.RoseberryLincoln@tetratech.com, 20230605
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.3.1
#
# library(devtools)
# install_github("leppott/CASTfxn")
# requires packages: dplyr, ggplot2, lubridate, purrr, readxl, sf, shiny,
#                    stringr, tibble, tidyr
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Shiny code for use in Shiny App
# 2020-10-30, Erik
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#rm(list=ls())

boo_Shiny <- FALSE

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, Start ####
# external/CASTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# 02, Set up ####
# Progress, 02
if (boo_Shiny == TRUE) {
  prog_det <- "Set up"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
boo.debug <- TRUE
debug.person <- "Ann"
if (boo_Shiny == TRUE) {
  gitpath <- file.path(".", "external", "R")  # used in RPPTool but not CASTool until getReport
  dir_rmd <- file.path(".", "external", "rmd")
  wd <- file.path(".")
  dir_data <- file.path(wd, "Data")
  dir_results <- file.path(wd, "Results")
  printClusterInfo <- TRUE
} else {# Not using shiny app
  #
  # in global in shiny
  not_all_na <- function(x) {!all(is.na(x))}
  #
  # Set up required functions ### DO NOT CHANGE! #
  # library(CASTfxn)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  #
  if (boo.debug == TRUE & debug.person == "Ann") {
    # wd <- getwd() # "C:/Users/ann.lincoln/Documents" ARL 2023-05-22
    wd <- "C:/Users/ann.lincoln/Documents" # ARL 2023-06-29
    gitpath <- file.path(wd, "GitHub", "CASTfxn", "R") # ARL 2023-05-22
    dir_rmd <- file.path(wd, "GitHub", "CASTfxn", "inst", "rmd") # ARL 2023-05-22
    localdir <- "C:/Users/ann.lincoln/Documents/CASTool_DATA" # ARL 2024-07-05
    region <- "WA" # options: SMC, AZ, WA, OR
    dir_data <- file.path(localdir, region, "Data")
    dir_results <- file.path(localdir, region, "Results")
    printClusterInfo <- FALSE
    boo_plot_user <- TRUE
    # NOTE: to run all sites, comment out line 639
    #if (boo.debug == TRUE & debug.person == "Ann") {
    source(file.path(gitpath, "readCASToolMetadata.R"))
    source(file.path(gitpath, "readInputFile.R"))
    source(file.path(gitpath, "getCoOccurDataset.R"))
    source(file.path(gitpath, "getTimeSeq.R"))
    source(file.path(gitpath, "getDataSets.R"))
    source(file.path(gitpath, "getComparators.R"))
    source(file.path(gitpath, "getAvailableDataTypes.R"))
    source(file.path(gitpath, "writeOutliers.R"))
    source(file.path(gitpath, "getSiteInfo.R"))
    source(file.path(gitpath, "getSiteMap.R"))
    source(file.path(gitpath, "getClusterInfo.R"))
    source(file.path(gitpath, "getStressorList.R"))
    source(file.path(gitpath, "getCoOccur.R"))
    source(file.path(gitpath, "getSufficiency.R"))
    source(file.path(gitpath, "getBioStressorResponses.R"))
    source(file.path(gitpath, "getVerifiedPredictions.R"))
    source(file.path(gitpath, "getOutliers.R"))
    source(file.path(gitpath, "getWoE.R"))
    source(file.path(gitpath, "getQualSites.R"))
    source(file.path(gitpath, "getSummaryAllSites.R"))
    source(file.path(gitpath, "getReport.R"))
    #}
  } else if (boo.debug == TRUE & debug.person == "Erik") {
    library(CASTfxn)
    #gitpath <- file.path(system.file(package = "CASTfxn"), "R")
    dir_rmd <- file.path(system.file(package = "CASTfxn"), "inst", "rmd")
    wd <- "C://Users//Erik.Leppo//OneDrive - Tetra Tech, Inc//MyDocs_OneDrive//GitHub//CASTfxn//inst//shiny-examples//CAST_SMC"
    dir_data <- file.path(wd, "Data")
    dir_results <- file.path(wd, "Results")
    printClusterInfo <- TRUE
    site <- "SMC04134"
    TargetSiteID <- site
    b <- 1
  } else if (boo.debug == TRUE) {
    # This should be an error condition, because Ann & Erik are only people
  } else {#boo.debug == FALSE
    # Install CASTfxn package
    library(CASTfxn)
    # Set local directory info
    wd <- file.path(".")
    dir_data <- file.path(wd, "Data")
    dir_results <- file.path(wd, "Results")
    boo_plot_user <- TRUE
  }
  #
}## IF ~ boo_Shiny ~ END

msg <- paste0("debug = ", boo.debug
              , ifelse(boo.debug == FALSE, ""
                       , paste0(", person = ", debug.person)))
message(msg)

startprep.time <- Sys.time()

#~~~~~~~~~~~~~~~~~~~~~~~
# 03, Select region variables ####
## modify to be console-input ####
# Progress, 03
fn.CASTmeta   <- file.path(dir_data, "CASTool_Metadata.xlsx")
list.meta <- readCASToolMetadata(fn.CASTmeta, region)

# Load GIS files
message("Loading GIS files.")
if (boo_Shiny == TRUE) {
  # 2020-09-09, use RDA saved version
  # NOT sure how to handle this
  outline  <- poly.smc.proj
  flowline <- lines.flowline.proj
} else {
  # Get boundary file for desired region
  sp_outline <- sf::read_sf(dsn = file.path(list.meta$dsn_outline)
                            , layer = list.meta$lyr_outline) %>%
    sf::st_transform(crs = 4269) # EPSG identifier for NAD83

  sp_flowline <- sf::read_sf(dsn = file.path(list.meta$dsn_flowline)
                             , layer = list.meta$lyr_flowline) %>%
    suppressWarnings(sf::st_transform(crs = 4269)) %>%
    sf::st_zm(drop = TRUE, what = "ZM")
}## IF ~ boo_Shiny ~ END

#~~~~~~~~~~~~~~~~~~~~~~~
# 04, Site data files ####
# Progress, 04
if (boo_Shiny == TRUE) {
  prog_det <- "Load Site Data Files"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

## Get site location info and other metadata (e.g., waterbody name)
if (!is.na(basename(list.meta$fn.Sites.Info))) {
  data_Sites <- read.delim(list.meta$fn.Sites.Info, header = TRUE, sep = "\t"
                           , stringsAsFactors = FALSE)

  if (!is.na(list.meta$outcaseColName)) {
    data_Sites <- data_Sites %>%
      dplyr::rename(StationID_Master = all_of(list.meta$siteName)
                    , RefSiteFlag = all_of(list.meta$refColName)
                    , OutcaseCol = all_of(list.meta$outcaseColName))
  } else {
    data_Sites <- data_Sites %>%
      dplyr::rename(StationID_Master = all_of(list.meta$siteName)
                    , RefSiteFlag = all_of(list.meta$refColName)) %>%
      dplyr::mutate(OutcaseCol = 1)
    outcaseColName <- "OutcaseCol"
  }
  if (!is.na(list.meta$incaseColName)) {
    data_Sites <- data_Sites %>%
      dplyr::rename(StationID_Master = all_of(list.meta$siteName)
                    , IncaseCol = all_of(list.meta$incaseColName))
    incaseColName = "IncaseCol"
  }

  refSites <- data_Sites$StationID_Master[data_Sites$RefSiteFlag == 1]
} else {
  msg <- "fn.Sites.Info is NA. This file is mandatory."
  message(msg)
  stop()
}
rm(fn.Sites.Info, refColName)

## Get cluster data (StreamCat) ####
if (list.meta$printClusterInfo == TRUE) {
  if (!is.na(basename(list.meta$fn.cluster))) {
    data_cluster <- read.table(list.meta$fn.cluster, header = TRUE, sep = "\t"
                               , na.strings = c("", "NA"), stringsAsFactors = FALSE)
  } else {
    msg <- "fn.cluster is NA"
    message(msg)
  }
  if (!is.na(basename(list.meta$fn.clusterinfo))) {
    data_clusterinfo <- read.table(list.meta$fn.clusterinfo, header = TRUE, sep = "\t"
                               , na.strings = c("", "NA"), stringsAsFactors = FALSE)
  } else {
    msg <- "fn.clusterinfo is NA"
    message(msg)
  }
} else {
  msg <- "printClusterInfo is FALSE"
  message(msg)
}

## Get background data (StreamCat) ####
if (list.meta$printBkgdInfo == TRUE) {
  if (!is.na(basename(list.meta$fn.bkgdata))) {
    data_bkgdata <- read.table(list.meta$fn.bkgdata, header = TRUE, sep = "\t"
                               , na.strings = c("", "NA"), stringsAsFactors = FALSE)
  } else {
    msg <- "fn.bkgdata is NA"
    message(msg)
  }
  if (!is.na(basename(list.meta$fn.bkginfo))) {
    data_bkginfo <- read.table(list.meta$fn.bkginfo, header = TRUE, sep = "\t"
                               , na.strings = c("", "NA"), stringsAsFactors = FALSE)
  } else {
    msg <- "fn.bkginfo is NA"
    message(msg)
  }
} else {
  msg <- "printBkgdInfo is FALSE"
  message(msg)
}

## Get Bray-Curtis dissimilarity matrix ####
if (list.meta$useBC == TRUE) {
  useBC <- list.meta$useBC
  if (!is.na(basename(list.meta$fn.bcdist))) {
    data_BCdist <- read.table(list.meta$fn.bcdist, header = TRUE, sep = "\t"
                               , na.strings = c("", "NA"), stringsAsFactors = FALSE)
  } else {
    msg <- "fn.bcdist is NA"
    message(msg)
  }
} else {
  msg <- "useBC is FALSE"
  message(msg)
}

#~~~~~~~~~~~~~~~~~~~~~~~
# 05, Measured data and metadata ####
# Progress, 05
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Chem"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

## Get metadata for all measured stressors
if (!is.na(basename(list.meta$fn.measinfo))) {
  data_measInfo   <- read.delim(list.meta$fn.measinfo, header = TRUE, sep = "\t"
                                , stringsAsFactors = FALSE)
  data_measInfo   <- data_measInfo %>%
    # dplyr::mutate(Analyte = StdParamName) %>%
    dplyr::filter(UseInStressorID == 1)
} else {
  msg <- "fn.measinfo is NA"
  message(msg)
}
rm(fn.measinfo)

## Get measured stressor values
## This will only include stressors in the raw data that are also defined
## in the metadata file. Should we print a unique list of any that aren't?
if (!is.na(basename(list.meta$fn.measdata))) {
  data_measAll <- read.delim(list.meta$fn.measdata, header = TRUE, sep = "\t"
                             , na.strings = c("NA", "N/A", "")
                             , stringsAsFactors = FALSE)
  analytes     <- sort(as.character(data_measInfo$StdParamName))
  params       <- sort(as.character(unique(data_measAll$StdParamName)))
  inDataNotMeta <- setdiff(params, analytes)
  if (length(inDataNotMeta) > 0) {
    inDataNotMeta <- toString(inDataNotMeta)
    msg <- "The following parameters in the measured stressor data will not be evauated:"
    message(paste(msg, inDataNotMeta, sep = " "))
  }
  rm(params, inDataNotMeta)
  data_measRaw <- data_measAll[data_measAll$StdParamName %in% analytes,]

  ## Average duplicate data
  ## This part assumes that any NA values will be removed.
  ## If one of two duplicates is NA, the value will be retained.
  ## If both are NA, the sample-analyte combo will be removed.
  ## This will likely throw a warning. Is it better to remove NAs first?
  data_measRaw <- data_measRaw %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    dplyr::mutate(SampleDate = lubridate::mdy(SampleDate)) %>%
    dplyr::select(StationID_Master, SampleID, SampleDate, StdParamName, ResultValue) %>%
    dplyr::group_by(StationID_Master, SampleID, SampleDate, StdParamName) %>%
    dplyr::summarize(MeanResultValue = mean(ResultValue, na.rm = TRUE)
                     , .groups = "drop_last") %>%
    dplyr::rename(ResultValue = MeanResultValue)
  data_measRaw <- unique(data_measRaw) # should not be necessary

  ## Get measured parameter names and separately, algal parameter names
  measParams <- as.vector(unique(data_measRaw$StdParamName))
  algParams  <- as.vector(unique(data_measRaw$StdParamName[grepl("^AFDM|^Chlor_a|^Pheophytin"
                                                                 , data_measRaw$StdParamName)]))
  ## getOutliers returns a dataframe with ChemSampleID, StdParamName, ResultValue,
  ## IQRmethod, SDmethod, Outlier
  ## Nonsensical values are flagged as possible data entry errors
  data_measoutliers <- getOutliers(df_data = data_measRaw
                                   , df_meta = data_measInfo)
  ## Merge outlier flags with raw data by sample ID
  data_measRaw <- merge(data_measRaw, data_measoutliers
                        , by.x = c("SampleID", "StdParamName", "ResultValue")
                        , by.y = c("SampleID", "StdParamName", "ResultValue")
                        , all.x = TRUE)
  data_measRaw <- data_measRaw %>%
    dplyr::select(StationID_Master, SampleID, SampleDate, StdParamName
                  , ResultValue, IQRmethod, SDmethod, Outlier) %>%
    dplyr::mutate(Outlier = ifelse(is.na(Outlier), "Possible data entry error"
                                   , Outlier))
  # Clean up
  rm(data_measAll, data_measoutliers)

  # Remove measured outliers here!
  if (list.meta$removeOutliers) {
    data_measoutliers <- data_measRaw %>%
      dplyr::filter(!(Outlier %in% c("Good", "NE")))
    data_measRaw <- data_measRaw %>%
      dplyr::filter(Outlier %in% c("Good", "NE"))
  } else {
    # don't do anything differently
  }

} else {
  msg <- "fn.measdata is NA"
  message(msg)
  data_measRaw <- NULL
}
rm(fn.measdata, analytes)

#~~~~~~~~~~~~~~~~~~~~~~~
# 06, Modeled data and metadata ####
# Progress, 06
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Model"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# Get metadata for modeled stressor data
if (!is.na(basename(list.meta$fn.modelinfo))) {
  data_modelInfo   <- read.delim(list.meta$fn.modelinfo, header = TRUE, sep = "\t"
                                 , stringsAsFactors = FALSE)
  data_modelInfo   <- data_modelInfo %>%
    # dplyr::mutate(Analyte = StdParamName) %>%
    dplyr::filter(UseInStressorID == 1)
} else {
  message <- "fn.modelinfo is NA"
  msg(message)
}
rm(fn.modelinfo)

# Get modeled stressor data
if (!is.na(basename(list.meta$fn.modeldata))) {
  data_modelAll <- read.delim(list.meta$fn.modeldata, header = TRUE, sep = "\t"
                              , stringsAsFactors = FALSE)
  useParams     <- as.character(data_modelInfo$StdParamName)
  params       <- sort(as.character(unique(data_modelAll$StdParamName)))
  inDataNotMeta <- setdiff(params, useParams)
  if (length(inDataNotMeta) > 0) {
    inDataNotMeta <- toString(inDataNotMeta)
    msg <- "The following parameters in the measured stressor data will not be evauated:"
    message(paste(msg, inDataNotMeta, sep = " "))
  }
  rm(params, inDataNotMeta)
  data_modelRaw <- data_modelAll[data_modelAll$StdParamName %in% useParams, ]

  ## Obtain SampleYear -- but SampDate is all NA, so this is meaningless
  data_modelRaw <- data_modelRaw %>%
    dplyr::mutate(SampleDate = NA) %>%
    # dplyr::rename(SampleID = ChemSampleID) %>%
    dplyr::select(StationID_Master, SampleID, SampleDate, StdParamName, ResultValue)

  ## getOutliers returns a dataframe with ChemSampleID, StdParamName, ResultValue,
  ## IQRmethod, SDmethod, Outlier
  data_modoutliers <- getOutliers(df_data = data_modelRaw
                                  , df_meta = data_modelInfo)

  ## Merge outlier flags with raw data by sample ID (should be all.y not all.x)
  data_modelRaw <- merge(data_modelRaw, data_modoutliers
                         , by.x = c("SampleID", "StdParamName", "ResultValue")
                         , by.y = c("SampleID", "StdParamName", "ResultValue")
                         , all.x = TRUE)
  data_modelRaw <- data_modelRaw %>%
    dplyr::select(StationID_Master, SampleID, SampleDate, StdParamName
                  , ResultValue, IQRmethod, SDmethod, Outlier) %>%
    dplyr::mutate(Outlier = ifelse(is.na(Outlier), "NE", Outlier))
  modelParams <- as.vector(unique(data_modelRaw$StdParamName))

  # Clean up
  rm(data_modelAll, useParams, data_modoutliers)
} else {
  message <- "fn.modeldata is NA"
  msg(message)
  data_modelRaw <- NULL
}
rm(fn.modeldata)

# Remove modeled outliers here
if (list.meta$removeOutliers) {
  data_modeloutliers <- data_modelRaw %>%
    dplyr::filter(!(Outlier %in% c("Good", "NE")))
  data_modelRaw <- data_modelRaw %>%
    dplyr::filter(Outlier %in% c("Good", "NE"))
} else {
  # don't do anything differently
}

#~~~~~~~~~~~~~~~~~~~~~~~
# 07, Combine stressor data ####
# Progress, 07
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Combine stressor data and metadata"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# Combine metadata for all stressors into one datafile
if (exists("data_measInfo") & exists("data_modelInfo")) {
  measMetaNames  <- colnames(data_measInfo)
  modelMetaNames <- colnames(data_modelInfo)
  extraNames     <- measMetaNames[!(measMetaNames %in% modelMetaNames)]
  for (e in 1:length(extraNames)) {
    newCol <- extraNames[e]
    data_modelInfo[[newCol]] <- NA
  }
  data_modelInfo  <- data_modelInfo[, measMetaNames]
  data_stressInfo <- rbind(data_measInfo, data_modelInfo)
  rm(data_measInfo, data_modelInfo)
} else if (exists("data_measInfo")) {
  data_stressInfo <- data_measInfo
  rm(data_measInfo)
} else if (exists("data_modelInfo")) {
  data_stressInfo <- data_modelInfo
  rm(data_modelInfo)
} else {
  msg <- "Neither measured nor modeled metadata are available"
  message(msg)
}
rm(measMetaNames, modelMetaNames, extraNames, newCol, e)

# Modify stressor info metadata; id direction of stress; id SSTVs
# Select only necessary columns -- ARL 2023-05-25
data_stressInfo <- dplyr::distinct(data_stressInfo, StdParamName, GroupNum
                                   , GroupName, LogTransf, SSD, SSTV, SensMin
                                   , SensMax, TolMin, TolMax, UseInStressorID
                                   , DirIncStress, SSTVname, Label)

# Combine measured and modeled parameters with inverse scoring
col_StressInvScore <- as.vector(data_stressInfo$StdParamName[data_stressInfo$DirIncStress == "Dec"])
SSTVparms <- unique(data_stressInfo$StdParamName[data_stressInfo$SSTV == 1])

# Combine raw data for all stressors into one datafile
if (exists("data_measRaw") & exists("data_modelRaw")) {
  data_Stress <- rbind(data_measRaw, data_modelRaw)
} else if (exists("data_measRaw")) {
  data_Stress <- data_measRaw
} else if (exists("data_modelRaw")) {
  data_Stress <- data_modelRaw
} else {
  msg <- "Neither measured nor modeled metadata are available"
  message(msg)
}

data_Stress <- data_Stress %>%
  dplyr::rename(StressSampID = SampleID
                , StressSampDate = SampleDate)

# Combine outliers for all stressors into one datafile
if (exists("data_measoutliers") & exists("data_modeloutliers")) {
  data_outliers <- rbind(data_measoutliers, data_modeloutliers)
} else if (exists("data_measoutliers")) {
  data_outliers <- data_measoutliers
} else if (exists("data_modeloutliers")) {
  data_outliers <- data_modeloutliers
} else {
  msg <- "Neither measured nor modeled data contain outliers"
  message(msg)
}

data_outliers <- data_outliers %>%
  dplyr::rename(StressSampID = SampleID, StressSampDate = SampleDate)
rm(data_measoutliers, data_modeloutliers)

# Bio responses
boo.bmi <- FALSE
boo.alg <- FALSE
boo.fish <- FALSE
list.bioParamsDEL <- list() # initialize an empty list

biocommlist <- list.meta$biocommlist

for (b in seq_along(biocommlist)) {
  bio <- tolower(biocommlist[b])
  #~~~~~~~~~~~~~~~~~~~~~~~
  # 08-10, Bio response data ####
  # Progress, 08-10
  if (boo_Shiny == TRUE) {
    prog_det <- paste0("Data, ", bio, ", Response data")
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END

  if ((bio == "bmi") & !boo.bmi) {
    # Read bmi data files
    message("Reading BMI data files")
    boo.bmi <- TRUE

    # Get raw BMI data
    if (!is.na(basename(list.meta$fn.bmi.raw))) {
      data_BMIcounts <- read.table(list.meta$fn.bmi.raw, header = TRUE, sep = "\t"
                                   , stringsAsFactors = FALSE)
      data_BMISampTotAbund <- unique(data_BMIcounts %>%
        dplyr::select(BMISampleID, SampleTotAbund))

    } else {
      msg <- "fn.bmi.raw filename is NA"
      message(msg)
    }

    # Get csci core data (qualifiers)
    # This strictly applies to SMC data -- Rework entirely to be agnostic
    if (!is.na(basename(list.meta$fn.bmi.qualifiers))) {
      data_qualifiers <- read.delim(list.meta$fn.bmi.qualifiers, header = TRUE, sep = "\t"
                                  , na.strings = "NA", stringsAsFactors = FALSE)
      data_qualifiers <- data_qualifiers[, c("stationid", "samplemonth", "sampleday"
                                             , "sampleyear", "collectionmethodcode"
                                             , "fieldreplicate", "count"
                                             , "pcnt_ambiguous_individuals")]
      data_qualifiers <- data_qualifiers %>%
        dplyr::mutate(date_text = paste(samplemonth, sampleday, sampleyear, sep = "/")
                      , BMISampleID = paste(stationid, date_text, collectionmethodcode
                                          , fieldreplicate, sep = "_")) %>%
        dplyr::rename(StationID_Master = stationid
                      , SampleTotAbund = count
                      , PctAmbigInd = pcnt_ambiguous_individuals) %>%
        dplyr::select(StationID_Master, BMISampleID, PctAmbigInd)
      data_qualifiers <- unique(data_qualifiers)
    } else {
      msg <- "fn.bmi.qualifiers filename is NA"
      message(msg)
    }

    # Get BMI master taxa data
    if (!is.na(basename(list.meta$fn.MT.bmi))) {
      data_BMIMasterTaxa <- read.table(list.meta$fn.MT.bmi, header = TRUE, sep = "\t"
                                       , stringsAsFactors = FALSE)
    } else {
      msg <- "fn.MT.bmi filename is NA"
      message(msg)
    }

    # Get BMI metric data                                   # This is very specific to San Diego
    if (!is.na(basename(list.meta$fn.bmi.metrics))) {
      data_bmiMetrics <- read.delim(list.meta$fn.bmi.metrics, header = TRUE, sep = "\t",
                                    na.strings = "NA", stringsAsFactors = FALSE)
      data_bmiMetrics <- data_bmiMetrics %>%
        dplyr::select_if(not_all_na) %>%
        dplyr::mutate(BMISampleDate = lubridate::mdy(BMISampleDate))
      data_bmiMetrics <- unique(data_bmiMetrics)

      data_bmiMetrics <- merge(data_bmiMetrics, data_BMISampTotAbund
                               , by = "BMISampleID", all = TRUE)

      if (exists("data_qualifiers")) {
        data_bmiMetrics <- merge(data_bmiMetrics, data_qualifiers
                                 , by.x = c("StationID_Master", "BMISampleID")
                                 , by.y = c("StationID_Master", "BMISampleID")
                                 , all = TRUE)
        data_bmiMetrics <- data_bmiMetrics %>%
          dplyr::mutate(BMISampFlag = case_when(SampleTotAbund < list.meta$bmiPctAmbInds &
                                                  PctAmbigInd > list.meta$bmiSuffInds ~
                                                  "Insufficient individuals and large percent ambiguity"
                                                , SampleTotAbund < list.meta$bmiPctAmbInds ~
                                                  "Insufficient individuals"
                                                , PctAmbigInd > list.meta$bmiSuffInds ~
                                                  "Large percent ambiguity"
                                                , TRUE ~ NA))
        rm(data_qualifiers)
      } else {
        data_bmiMetrics <- data_bmiMetrics %>%
          dplyr::mutate(PctAmbigInd = NA, BMISampFlag = NA)
      }
      rm(data_BMISampTotAbund)
    } else {
      msg <- "fn.bmi.metrics filename is NA"
      message(msg)
    }

    # Get BMI metric data
    if (!is.na(basename(list.meta$fn.bmi.metrics.info))) {
      data_bmiMetricsInfo <- read.delim(list.meta$fn.bmi.metrics.info, header = TRUE, sep = "\t"
                                        , na.strings = "NA", stringsAsFactors = FALSE)
      bmiMetrics <- as.vector(data_bmiMetricsInfo$MetricName)
      bmiIndex <- as.character(data_bmiMetricsInfo$MetricName[data_bmiMetricsInfo$IndexYN == "Yes"])
    } else {
      msg <- "fn.bmi.metrics.info filename is NA"
      message(msg)
    }

    # Generate co-occurrence data set (within lag day samples; modeled data match any day)
    # SMC version writes a co-occur data file to dataDir (Data directory) -- 20230711 Removed dataDir ARL
    data_bmiCoOccur <- getCoOccurDataset(df_sites = data_Sites
                                         , df_model = data_modelRaw
                                         , df_meas = data_measRaw
                                         , biocomm = "BMI"
                                         , df_resp = data_bmiMetrics
                                         , index = list.meta$bmiIndex
                                         , lagdays = list.meta$lagdays)
    # returns df_coOccur as data_bmiCoOccur

    # Identify modeled parameters to keep or delete (per client)
    bmiModelParamsDEL  <- setdiff(modelParams, list.meta$bmiModParams)
    # modelParams: all modeled Params;
    # bmiModParams: input data from client re which modeled parameters to use when evaluating bmi responses
    data_bmiCoOccur <- data_bmiCoOccur %>%
      dplyr::select(!all_of(bmiModelParamsDEL)) %>%
      dplyr::select_if(not_all_na)
    list.bioParamsDEL <- append(list.bioParamsDEL, list(BMI = bmiModelParamsDEL))

    # rm(bmiModParams)
  } else { # NO BMI data
    # message("No BMI data available")
  }

  if ((bio == "algae") & !boo.alg) {
    # Read alg data files
    message("Reading algae data files")
    boo.alg <- TRUE

    # Get raw algal data
    if (!is.na(basename(list.meta$fn.alg.raw))) {
      data_algCounts <- read.table(list.meta$fn.alg.raw, header = TRUE, sep = "\t")
    } else {
      msg <- "fn.alg.raw filename is NA"
      message(msg)
    }

    # Get algal master taxa data
    if (!is.na(basename(list.meta$fn.MT.alg))) {
      data_algMasterTaxa <- read.table(list.meta$fn.MT.alg, header = TRUE, sep = "\t",
                                       stringsAsFactors = FALSE)
    } else {
      msg <- "fn.MT.alg filename is NA"
      message(msg)
    }

    # Get algal metrics data
    if (!is.na(basename(list.meta$fn.alg.metrics))) {
      data_algMetrics <- read.table(list.meta$fn.alg.metrics, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
      data_algMetrics <- data_algMetrics %>%
        dplyr::mutate(AlgSampleDate = lubridate::mdy(AlgSampleDate)) %>%
        dplyr::mutate(AlgSampFlag = NA)
    } else {
      msg <- "fn.alg.metrics filename is NA"
      message(msg)
    }

    # Get algal metrics metadata
    if (!is.na(basename(list.meta$fn.alg.metrics.info))) {
      data_algMetricsInfo <- read.delim(list.meta$fn.alg.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      algMetrics <- as.vector(data_algMetricsInfo$MetricName[data_algMetricsInfo$UseYN == 1])
      algIndex <- as.character(data_algMetricsInfo$MetricName[data_algMetricsInfo$IndexYN == "Yes"])

    } else {
      msg <- "fn.alg.metrics.info filename is NA"
      message(msg)
    }

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    # SMC version writes a co-occur data file to dataDir (Data directory) -- 20230711 Removed dataDir ARL
    data_algCoOccur <- getCoOccurDataset(df_sites = data_Sites
                                         , df_model = data_modelRaw
                                         , df_meas = data_measRaw
                                         , biocomm = "Alg"
                                         , df_resp = data_algMetrics
                                         , index = list.meta$algIndex
                                         , lagdays = list.meta$lagdays)
    # returns df_coOccur as data_algCoOccur

    # Identify modeled parameters to keep or delete (per SCCWRP)
    algModelParamsDEL  <- setdiff(modelParams, list.meta$algModParams)
    data_algCoOccur <- data_algCoOccur %>%
      dplyr::select(!all_of(algModelParamsDEL)) %>%
      dplyr::select_if(not_all_na)
    list.bioParamsDEL <- append(list.bioParamsDEL, list(ALG = algModelParamsDEL))

    # rm(algModParams)
  } else { # NO algae data
    # message("No algae data available")
  }

  # Read fish data
  if ((bio == "fish") & !boo.fish) {
    # Read alg data files
    message("Reading fish data files")
    boo.fish <- TRUE

    # Get raw fish data
    if (!is.na(basename(list.meta$fn.fish.raw))) {
      data_fishCounts <- read.table(list.meta$fn.fish.raw, header = TRUE, sep = "\t")
    } else {
      msg <- "fn.fish.raw filename is NA"
      message(msg)
    }

    # Get fish master taxa data
    if (!is.na(basename(list.meta$fn.MT.fish))) {
      data_fishMasterTaxa <- read.table(list.meta$fn.MT.fish, header = TRUE, sep = "\t",
                                       stringsAsFactors = FALSE)
    } else {
      msg <- "fn.MT.fish filename is NA"
      message(msg)
    }

    # Get fish metrics data
    if (!is.na(basename(list.meta$fn.fish.metrics))) {
      data_fishMetrics <- read.table(list.meta$fn.fish.metrics, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
      data_fishMetrics <- data_fishMetrics %>%
        dplyr::mutate(FishSampleDate = lubridate::mdy(FishSampleDate)) %>%
        dplyr::mutate(FishSampFlag = NA)
    } else {
      msg <- "fn.fish.metrics filename is NA"
      message(msg)
    }

    # Get fish metrics metadata
    if (!is.na(basename(list.meta$fn.fish.metrics.info))) {
      data_fishMetricsInfo <- read.delim(list.meta$fn.fish.metrics.info, header = TRUE, sep = "\t",
                                        na.strings = "NA", stringsAsFactors = FALSE)
      fishMetrics <- as.vector(data_fishMetricsInfo$MetricName[data_fishMetricsInfo$UseYN == 1])
      fishIndex <- as.character(data_fishMetricsInfo$MetricName[data_fishMetricsInfo$IndexYN == "Yes"])

    } else {
      msg <- "fn.fish.metrics.info filename is NA"
      message(msg)
    }

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    # SMC version writes a co-occur data file to dataDir (Data directory) -- 20230711 Removed dataDir ARL
    data_fishCoOccur <- getCoOccurDataset(df_sites = data_Sites
                                         , df_model = data_modelRaw
                                         , df_meas = data_measRaw
                                         , biocomm = "Fish"
                                         , df_resp = data_fishMetrics
                                         , index = list.meta$fishIndex
                                         , lagdays = list.meta$lagdays)
    # returns df_coOccur as data_fishCoOccur

    # Identify modeled parameters to keep or delete (per SCCWRP)
    fishModelParamsDEL  <- setdiff(modelParams, fishModParams)
    data_fishCoOccur <- data_fishCoOccur %>%
      dplyr::select(!all_of(fishModelParamsDEL)) %>%
      dplyr::select_if(not_all_na)
    list.bioParamsDEL <- append(list.bioParamsDEL, list(FISH = fishModelParamsDEL))

    rm(fishModParams)
  } else { # NO fish data
    # message("No fish data available")
  }
}

if (boo.bmi == FALSE) {
  message("No BMI data available")
  bmiResp <- NULL
  bmiIndexGp <- NULL
  data_bmiMetrics <- NULL
  data_bmiMetricsInfo <- NULL
  data_bmiCoOccur <- NULL
}
if (boo.alg == FALSE) {
  message("No algae data available")
  algResp <- NULL
  algIndexGp <- NULL
  data_algMetrics <- NULL
  data_algMetricsInfo <- NULL
  data_algCoOccur <- NULL
}
if (boo.fish == FALSE) {
  message("No fish data available")
  fishResp <- NULL
  fishIndexGp <- NULL
  data_fishMetrics <- NULL
  data_fishMetricsInfo <- NULL
  data_fishCoOccur <- NULL
}
# Clean up
rm(b, bio, boo.bmi, boo.alg, boo.fish)
# rm(fn.bmi.raw, fn.bmi.metrics, fn.bmi.metrics.info, fn.MT.bmi, fn.bmi.qualifiers)
# rm(fn.alg.raw, fn.alg.metrics, fn.alg.metrics.info, fn.MT.alg)
# rm(fn.fish.raw, fn.fish.metrics, fn.fish.metrics.info, fn.MT.fish)

#~~~~~~~~~~~~~~~~~~~~~~~
# 11, Sample summary ####
# Progress, 11
# NOTE: This must use all of the data, including outliers
if (boo_Shiny == TRUE) {
  prog_det <- "Data, Sample Summary"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# Prepare stressor data
# Identify field, lab, phab, model types
if (exists("data_modelRaw") & exists("data_measRaw")) {
  data_modeltrim <- as.data.frame(data_modelRaw) %>%
    dplyr::distinct(StationID_Master, SampleID) %>%
    dplyr::rename(ModelSampID = SampleID)

  data_meastrim <- as.data.frame(data_measRaw) %>%
    dplyr::distinct(StationID_Master, SampleID, StdParamName, SampleDate)
  data_meastrim <- merge(data_meastrim, data_stressInfo, by = "StdParamName") %>%
    dplyr::select(StationID_Master, SampleID, GroupName, StdParamName
                  , SampleDate, Label) %>%
    dplyr::mutate(Type = case_when(grepl("Habitat", GroupName, ignore.case) ~ "PhabSampID"
                                   , grepl("Field-measured", Label, ignore.case) ~ "FldChemSampID"
                                   , TRUE ~ "ChemSampID")) %>%
    dplyr::distinct(StationID_Master, SampleID, SampleDate, Type)  %>%
    tidyr::pivot_wider(id_cols = c(StationID_Master, SampleDate), names_from = Type
                       , values_from = SampleID, values_fill = NA)

  data_sampSummary <- merge(data_meastrim, data_modeltrim
                            , by = "StationID_Master", all = TRUE)

  rm(data_modeltrim, data_meastrim)

} else if (exists("data_measRaw")) {
  data_sampSummary <- as.data.frame(data_measRaw) %>%
    dplyr::distinct(StationID_Master, SampleID, StdParamName, SampleDate)
  data_sampSummary <- merge(data_sampSummary, data_stressInfo, by = "StdParamName") %>%
    dplyr::select(StationID_Master, SampleID, GroupName, StdParamName
                  , SampleDate, Label) %>%
    dplyr::mutate(Type = case_when(grepl("Habitat", GroupName, ignore.case) ~ "PhabSampID"
                                   , grepl("Field-measured", Label, ignore.case) ~ "FldChemSampID"
                                   , TRUE ~ "ChemSampID")) %>%
    dplyr::distinct(StationID_Master, SampleID, SampleDate, Type)  %>%
    tidyr::pivot_wider(id_cols = c(StationID_Master, SampleDate), names_from = Type
                       , values_from = SampleID, values_fill = NA)

  msg <- "No modeled stressor data available"
  message(msg)

} else if (exists("data_modelRaw")) {
  data_modeltrim <- as.data.frame(data_modelRaw) %>%
    dplyr::distinct(StationID_Master, SampleID) %>%
    dplyr::rename(ModelSampID = SampleID)

  msg <- "No measured stressor data available"
  message(msg)

} else {
  msg <- "No stressor data available"
  message(msg)
  stop()
}

# Identify response samples
if (!is.null(data_bmiMetrics) & exists("data_sampSummary")) {
  data_bmiMetricsTrim <- data_bmiMetrics %>%
    dplyr::distinct(StationID_Master, BMISampID, BMISampDate) %>%
    dplyr::group_by(StationID_Master, BMISampDate) %>%
    dplyr::summarise(BMISampID = stringr::str_flatten(BMISampID, collapse = "\n")
                     , .groups = "drop_last")
  data_sampSummary <- merge(data_sampSummary, data_bmiMetricsTrim
                            , by.x = c("StationID_Master", "SampleDate")
                            , by.y = c("StationID_Master", "BMISampDate")
                            , all = TRUE)
  rm(data_bmiMetricsTrim)
} else if (!is.null(data_bmiMetrics) & exists("data_modeltrim")) {
  data_bmiMetricsTrim <- data_bmiMetrics %>%
    dplyr::distinct(StationID_Master, BMISampID, BMISampDate) %>%
    dplyr::group_by(StationID_Master, BMISampDate) %>%
    dplyr::summarise(BMISampID = stringr::str_flatten(BMISampID, collapse = "\n")
                     , .groups = "drop_last")
  data_sampSummary <- merge(data_modeltrim, data_bmiMetricsTrim
                            , by.x = c("StationID_Master")
                            , by.y = c("StationID_Master")
                            , all = TRUE)
  rm(data_bmiMetricsTrim)
} else {
  msg <- "No BMI data available"
  message(msg)
}
if (!is.null(data_algMetrics) & exists("data_sampSummary")) {
  data_algMetricsTrim <- data_algMetrics %>%
    dplyr::distinct(StationID_Master, AlgSampleID, AlgSampleDate) %>%
    dplyr::group_by(StationID_Master, AlgSampleDate) %>%
    dplyr::summarise(AlgSampleID = stringr::str_flatten(AlgSampleID, collapse = "\n")
                     , .groups = "drop_last")
  data_sampSummary <- merge(data_sampSummary, data_algMetricsTrim
                            , by.x = c("StationID_Master", "SampleDate")
                            , by.y = c("StationID_Master", "AlgSampleDate")
                            , all = TRUE)
  rm(data_algMetricsTrim)
} else if (!is.null(data_algMetrics) & exists("data_modeltrim")) {
  data_algMetricsTrim <- data_algMetrics %>%
    dplyr::distinct(StationID_Master, AlgSampleID, AlgSampleDate) %>%
    dplyr::group_by(StationID_Master, AlgSampleDate) %>%
    dplyr::summarise(AlgSampleID = stringr::str_flatten(AlgSampleID, collapse = "\n")
                     , .groups = "drop_last")
  data_sampSummary <- merge(data_modeltrim, data_algMetricsTrim
                            , by.x = c("StationID_Master")
                            , by.y = c("StationID_Master")
                            , all = TRUE)
  rm(data_algMetricsTrim, data_modeltrim)
} else {
  msg <- "No Algal data available"
  message(msg)
}
if (!is.null(data_fishMetrics) & exists("data_sampSummary")) {
  data_fishMetricsTrim <- data_fishMetrics %>%
    dplyr::distinct(StationID_Master, FishSampleID, FishSampleDate) %>%
    dplyr::group_by(StationID_Master, FishSampleDate) %>%
    dplyr::summarise(FishSampleID = stringr::str_flatten(FishSampleID, collapse = "\n")
                     , .groups = "drop_last")
  data_sampSummary <- merge(data_sampSummary, data_fishMetricsTrim
                            , by.x = c("StationID_Master", "SampleDate")
                            , by.y = c("StationID_Master", "FishSampleDate")
                            , all = TRUE)
  rm(data_fishMetricsTrim)
} else if (!is.null(data_fishMetrics) & exists("data_modeltrim")) {
  data_fishMetricsTrim <- data_fishMetrics %>%
    dplyr::distinct(StationID_Master, FishSampleID, FishSampleDate) %>%
    dplyr::group_by(StationID_Master, FishSampleDate) %>%
    dplyr::summarise(FishSampleID = stringr::str_flatten(FishSampleID, collapse = "\n")
                     , .groups = "drop_last")
  data_sampSummary <- merge(data_modeltrim, data_fishMetricsTrim
                            , by.x = c("StationID_Master")
                            , by.y = c("StationID_Master")
                            , all = TRUE)
  rm(data_fishMetricsTrim)
} else {
  msg <- "No Fish data available"
  message(msg)
}

if (is.na(fn.CASTmeta$incaseColName)) {
  data_sampSummary <- merge(data_Sites[, c("StationID_Master", "COMID", "OutcaseCol")]
                            , data_sampSummary, by = "StationID_Master"
                            , all = TRUE)
  data_sampSummary <- unique(data_sampSummary)
  # data_sampSummary <- data_sampSummary %>%
  #   dplyr::distinct(StationID_Master, COMID, OutcaseCol, SampleDate, ChemSampID
  #                   , FldChemSampID, PhabSampID, ModelSampID, BMISampID, AlgSampleID)
} else if (fn.CASTmeta$outcaseColName == "State" | is.na(fn.CASTmeta$outcaseColName)) {
  data_sampSummary <- merge(data_Sites[, c("StationID_Master", "COMID", "IncaseCol")]
                            , data_sampSummary, by = "StationID_Master"
                            , all = TRUE)
  data_sampSummary <- unique(data_sampSummary)
  # data_sampSummary <- data_sampSummary %>%
  #   dplyr::distinct(StationID_Master, COMID, IncaseCol, SampleDate, ChemSampID
  #                   , FldChemSampID, PhabSampID, ModelSampID, BMISampID, AlgSampleID)
} else {
  data_sampSummary <- merge(data_Sites[, c("StationID_Master", "COMID", "OutcaseCol"
                                           , "IncaseCol")]
                            , data_sampSummary, by = "StationID_Master"
                            , all = TRUE)
  data_sampSummary <- unique(data_sampSummary)
  # data_sampSummary <- data_sampSummary %>%
  #   dplyr::distinct(StationID_Master, COMID, OutcaseCol, IncaseCol, SampleDate
  #                   , ChemSampID, FldChemSampID, PhabSampID, ModelSampID, BMISampID, AlgSampleID)
}

# FOR TESTING ONLY
# write.table(data_sampSummary, file.path(dir_data, "TESTSummarySiteSamples.tab")
#             , append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")

rm(fn.CASTmeta)

#~~~~~~~~~~~~~~~~~~~~~~~
# RUN CASTool
# 12, Target site selection ####
# Progress, 12
if (boo_Shiny == TRUE) {
  prog_det <- "Site Selection"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
df_targets <- read_excel(fn.targets, col_names = TRUE, trim_ws = TRUE, skip = 0)
rm(fn.targets)

endprep.time <- Sys.time()
elapsedprep.time <- endprep.time - startprep.time
msg <- paste("Prep completed in", elapsedprep.time)
message(msg)

ifelse(!dir.exists(file.path(dir_results)) == TRUE
       , dir.create(file.path(dir_results))
       , FALSE)

fn_runstats <- paste0("RunStats_", format.Date(Sys.Date(),"%Y%m%d"), ".tab")
df_runstats <- as.data.frame(cbind("TargetSiteID", "Biocomm", "NumStressors"
                                   , "NumLoE", "ElapsedTime"))
write.table(df_runstats, file.path(dir_results,fn_runstats), append = FALSE
            , col.names = FALSE, row.names = FALSE, sep = "\t")

rm(startprep.time, endprep.time, elapsedprep.time, df_runstats)

### Evaluate each target site
## Use this for debugging
if (boo_Shiny == TRUE) {
  df_targets <- data.frame("TargetSiteID" = input$Station, "Chosen by" = NA, "Comment" = NA)
  names(df_targets)[2] <- "Chosen by"
} else if (boo.debug == TRUE & debug.person == "Ann") {
  # df_targets <- dplyr::filter(df_targets, TargetSiteID  == "SMC04134")
  df_targets <- dplyr::filter(df_targets, TargetSiteID %in% c("SMC04134", "402BA0031"))
  msg <- paste0("Number of target sites = ", nrow(df_targets))
  message(msg)
}
rm(msg)

#~~~~~~~~~~~~~~~~~~~~~~~
# 13, Main Code ####
# Progress, 13
if (boo_Shiny == TRUE) {
  prog_det <- "Main Code Start"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
# FOR ~ site ~ START ####
for (site in seq_along(df_targets)) {
  startsite.time <- Sys.time()
  TargetSiteID <- df_targets$TargetSiteID[site]

  if (is.na(TargetSiteID)) {
    next()
  }
  msg <- paste0("Evaluating site: ", TargetSiteID)
  message(msg)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Biocomm-independent functions

  # Create high-level results folder structure
  dir_sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(dir_results, dir_sub2)) == TRUE
         , dir.create(file.path(dir_results, dir_sub2))
         , FALSE)

  # Establish data gaps file
  gaps <- cbind.data.frame("fxnname", "condition", "result", "comment")
  fn.gaps <- file.path(dir_results, TargetSiteID
                       , paste0(TargetSiteID, "_datagaps.tab"))
  write.table(gaps, fn.gaps, append = FALSE, col.names = FALSE
              , row.names = FALSE, sep = "\t")

  # 14, getComparators ####
  # Progress, 14
  if (boo_Shiny == TRUE) {
    prog_det <- "getComparators"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Identify comparator sites & write # comparators to data gaps file
  # This is predicated on the fact that BC distance is calculated based on
  # expected benthic macroinvertebrate taxa. If there are ever different
  # BC matrices for different biocomms, then this must move into the biocomm
  # loop or it needs to be run more than once for each biocomm here, since
  # it's used in getSiteInfo immediately afterward.
  list.CompSites <- getComparators(TargetSiteID = TargetSiteID
                                   , df_sites = data_Sites
                                   , df_bioCoOccur = data_bmiCoOccur
                                   , bioIndex = bmiIndex
                                   , useBC = useBC
                                   , outcaseColName = outcaseColName
                                   , incaseColName = incaseColName
                                   , df_bcdist = data_BCdist
                                   , bc_cutoff = 0.05
                                   , dir_results = dir_results
                                   , dir_sub = "SiteInfo")
  # If useBC == TRUE
  # Returns: myCompSites <- list(comp.sites = comp.sites ("inside the case")
  #                              , all.sites = cluster.sites) ("outside the case")
  # If useBC == FALSE
  # Returns: myCompSites <- list(comp.sites = cluster.sites ("inside the case")
  #                              , all.sites = elig.sites)
  # (all sites in ecoregion or other geographic region = "outside the case")
  comp_sites <- list.CompSites$comp.sites # inside the case sites (if useBC == FALSE, cluster sites)
  all_sites <- list.CompSites$all.sites   # outside the case sites (<= all sites in dataframe)
  outcaseID <- list.CompSites$outcaseID   # outside the case identifier (value)
  rm(list.CompSites)
  msg <- "getComparators is complete."
  message(msg)

  # 15, getSiteInfo, getSiteMap, writeOutliers ####
  # Progress, 15
  if (boo_Shiny == TRUE) {
    prog_det <- "getSiteInfo, getSiteMap, writeOutliers"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  # Get site information for general use (map, sample summary, etc)

  # Create site info folder with background bar graphs, boxplots for bio indices,
  # and folder for photos
  list.SiteSummary <- getSiteInfo(TargetSiteID = TargetSiteID
                                  , data_Sites = data_Sites
                                  , data_SampSummary = data_sampSummary
                                  , data_bmiMetrics = data_bmiMetrics
                                  , bmiIndexGp = bmiIndexGp
                                  , data_algMetrics = data_algMetrics
                                  , algIndexGp = algIndexGp
                                  , data_fishMetrics = data_fishMetrics
                                  , fishIndexGp = fishIndexGp
                                  , comp_sites = comp_sites
                                  , outcaseLabel = outcaseLabel
                                  , incaseLabel = incaseLabel
                                  , useBC = useBC
                                  , printBkgdInfo = printBkgdInfo
                                  , data_bkgdata = data_bkgdata
                                  , data_bkginfo = data_bkginfo
                                  # , data_cluster = data_cluster
                                  # , data_mods = NULL
                                  # , data_303d = NULL
                                  , dir_photo = file.path(dir_data,"Photos")
                                  , dir_results = dir_results
                                  , dir_sub = "SiteInfo")
  # Returns: mySiteSummary <- list(SiteInfo = mySiteInfo,
  #                                Samps = mySamps,
  #                                BMImetrics = myBMImetrics,
  #                                AlgMetrics = myAlgaeMetrics,
  #                                COMID = myCOMID,
  #                                refCOMIDs = myRefCOMIDs)
  # clustID <- list.SiteSummary$ClustID

  # Create site map
  # Q: Map entire state or only ecoregion? ####
  getSiteMap(sp_outline = sp_outline
             , sp_flowline = sp_flowline
             , datum = datum
             , region = region
             , df_sites = data_Sites
             , allSites = all_sites
             , compSites = comp_sites
             , TargetSiteID = TargetSiteID
             , useBC = useBC
             , dir_results = dir_results
             , dir_sub = "SiteInfo"
             , dir_map_rmd = dir_rmd)
  # Prints static and leaflet maps (.png and .html)

  # Prepare data sets of all stressors ever detected at the target site
  # This requires data_Stress, which either includes or excludes outliers
  # used in getStressorList and getTimeSeq
  siteStressAll <- data_Stress %>%
    dplyr::select(!c(IQRmethod, SDmethod, Outlier)) %>%
    dplyr::filter(StationID_Master == TargetSiteID) %>%
    dplyr::filter(!is.na(ResultValue)) %>%
    tidyr::pivot_wider(names_from = StdParamName
                       , values_from = ResultValue) %>%
    dplyr::select_if(not_all_na)
  siteDetectsAll <- as.vector(colnames(siteStressAll))
  siteDetectsAll <- siteDetectsAll[!(siteDetectsAll %in%
                                       c("StationID_Master", "StressSampID"
                                         , "StressSampDate"))]

  # Write target site outliers, comparator site outliers (inside the case),
  # and all outliers (outside the case)
  writeOutliers(TargetSiteID = TargetSiteID
                , df_outliers = data_outliers
                , df_stressInfo = data_stressInfo
                , siteDetects = siteDetectsAll
                , compSites = comp_sites
                , allSites = all_sites
                , dir_results = dir_results)
  # Writes outliers to data gaps file

  msg <- "getSiteInfo, getSiteMap, and writeOutliers are complete."
  message(msg)

  # 16, getClusterInfo ####
  # Progress, 16
  if (boo_Shiny == TRUE) {
    prog_det <- "getClusterInfo"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Get Cluster Info
  ## NEEDS TO BE TESTED ####
  if (printClusterInfo) {
    if (is.na(subregion)) {
      #OutcaseCol = Cluster; subregion = ecoregion or other division; useBC = TRUE
      clusterID = outcaseID
      subregion <- NULL
    } else {
      #OutcaseCol = ecoregion or other division; IncaseCol = Cluster; useBC = FALSE
      clusterID = incaseID
    }
    #
    getClusterInfo(TargetSiteID = TargetSiteID
                   , siteCOMID = list.SiteSummary$COMID
                   , siteCluster = clusterID
                   , refSiteCOMIDs = list.SiteSummary$refCOMIDs
                   , subregion = subregion
                   , data_cluster = data_cluster
                   , data_clusterInfo = data_clusterInfo
                   , dir_results = dir_results
                   , dir_sub = "ClusterInfo")
    msg <- "getClusterInfo is complete."
    message(msg)
  }## IF ~ printClusterInfo ~ END
  rm(list.SiteSummary)

  # 17, getAvailableDataTypes ####
  # Progress, 17
  if (boo_Shiny == TRUE) {
    prog_det <- "getAvailableDataTypes"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Prepare flags for types of stressor and response data to use
  list.AvailData <- getAvailableDataTypes(TargetSiteID = TargetSiteID
                                          , df_SampSummary = data_sampSummary
                                          , measStressSamps = meas.stress
                                          , modStressSamps = mod.stress
                                          , chemStressSamps = chem.stress
                                          , habStressSamps = hab.stress
                                          , bmiRespSamps = bmiResp
                                          , algRespSamps = algResp
                                          , fishRespSamps = fishResp
                                          , dir_results = dir_results)
  # Returns: myAvailData <- list(useBMI = useBMI
  #                              , useAlg = useAlg
  #                              , useFish = useFish
  #                              , noStressors = noStressors
  #                              , noResponses = noResponses)
  noStressors <- list.AvailData$noStressors
  noResponses <- list.AvailData$noResponses
  useBMI      <- list.AvailData$useBMI
  useAlg      <- list.AvailData$useAlg
  useFish     <- list.AvailData$useFish

  if ((noStressors == TRUE) | (noResponses == TRUE)) {
    msg <- ifelse((noStressors == TRUE) & (noResponses == TRUE)
                  , paste0("No stressor or response data are available for "
                           , TargetSiteID)
                  , ifelse(noStressors == TRUE
                           , paste0("No stressor data are available for "
                                    , TargetSiteID)
                           , paste0("No response data are available for "
                                    , TargetSiteID)))
    message(msg)
    next
  }
  rm(list.AvailData, noStressors, noResponses)

  # 18, getStressorList ####
  # Progress, 18
  if (boo_Shiny == TRUE) {
    prog_det <- paste0(bioComm, "; getStressorList")
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #

  # Get Stressor List using all stressors ever detected at the target site
  # regardless of match status
  list.stressors <- getStressorList(TargetSiteID = TargetSiteID
                                    , outcaseLabel = outcaseColLabel # used for subtitle
                                    , outcaseID = outcaseID          # used for title
                                    , outcaseSites = all_sites       # vector
                                    , incaseSites = comp_sites       # vector
                                    , refSites = refSites            # vector
                                    , siteChem = siteDetectsAll      # dataframe
                                    , df_Stress = data_Stress        # dataframe
                                    , chemInfo = data_stressInfo     # dataframe
                                    , samplim = samplim              # integer (#samps < which can't id)
                                    , probsHigh = probsHigh          # numeric
                                    , probsLow = probsLow            # numeric
                                    , DOlim = DOlim                  # numeric
                                    , pHlimLow = pHlimLow            # numeric
                                    , pHlimHigh = pHlimHigh          # numeric
                                    , biocommlist = biocommlist      # character
                                    , listbioParamsDEL = list.bioParamsDEL # list of vectors
                                    , dir_results = dir_results      # vector
                                    , dir_sub = "CandidateCauses")
  # Returns: myStressors <- list(stressors = stressorlist
  #                     , site.stressor.pctrank = site.pctrank
  #                     , stressors_LogTransf)
  stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
  stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
  msg <- "getStressorList is complete."
  message(msg)

  # Evaluate possible error conditions
  # If no stressors are identified, no analyses can be performed. Error msg.
  if (length(stressors) == 0) {
    msg <- paste("No candidate causes identified for", TargetSiteID)
    message(msg)

    # No identified stressors may be a data gap, but may not be, either
    gapcomment <- paste0("No potential stressors fall outside the specified "
                         , "quantile range (", probsLow, " to ", probsHigh,").")
    gaps <- cbind.data.frame("getStressorList", "Number of stressors", 0
                             , gapcomment)
    colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
    write.table(df_temp, file.path(dir_results, fn_runstats)
                , append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
    next()
  } ### End no stressors statement GO TO NEXT SITE


  # FOR ~ b ~ START ####
  if (boo.debug == TRUE & debug.person == "Erik") {
    # 1 = bmi, 2 = alg, 3 = fish
    biocommlist <- "alg"
  }

  for (b in seq_along(biocommlist)) {

    NE_true <- FALSE
    numLoE = 0

    LoEs <- c("TS", "CO", "SRLog", "SRLin", "VP", "SSD")
    df_LoE <- as.data.frame(LoEs)
    colnames(df_LoE) <- "LoE"
    df_LoE <- df_LoE %>%
      dplyr::mutate(LoE = as.character(LoE)
                    , Completed = as.integer(0)
                    , ResultsDir = as.character(NA))

    # Define biocomm data
    bioComm <- tolower(biocommlist[b])
    if ((bioComm == "bmi") && (useBMI == TRUE)) {
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
      bioParmsDEL <- bmiModelParamsDEL
    } else if ((bioComm == "algae") && (useAlg == TRUE)) {
      data_bioCoOccur <- data_algCoOccur
      bioIndex <- algIndex
      bioIndexGp <- algIndexGp
      bioMetricNames <- algMetrics
      bioMetricData <- data_algMetrics
      bioMetricInfo <- data_algMetricsInfo
      bioTaxaData <- data_algCounts
      bioMasterTaxa <- data_algMasterTaxa
      colBio <- algIndex
      colBioSample <- algResp
      colBioSampDate <- algRespDate
      BioNarBrk <- alg_thresholds
      BioNarLab <- alg_narrative
      BioDegBrk <- alg_deg_thres
      BioDegLab <- alg_deg_text
      bioParmsDEL <- algModelParamsDEL
    } else if ((bioComm == "fish") && (useFish == TRUE)) {
      data_bioCoOccur <- data_fishCoOccur
      bioIndex <- fishIndex
      bioIndexGp <- fishIndexGp
      bioMetricNames <- fishMetrics
      bioMetricData <- data_fishMetrics
      bioMetricInfo <- data_fishMetricsInfo
      bioTaxaData <- data_fishCounts
      bioMasterTaxa <- data_fishMasterTaxa
      colBio <- fishIndex
      colBioSample <- fishResp
      colBioSampDate <- fishRespDate
      BioNarBrk <- fish_thresholds
      BioNarLab <- fish_narrative
      BioDegBrk <- fish_deg_thres
      BioDegLab <- fish_deg_text
      bioParmsDEL <- fishModelParamsDEL
    } else {
      msg <- paste0(bioComm, " is not a valid biological community.")
      message(msg)
      next()
    }

    # If no paired stressor-response samples for target site, no eval possible
    # First 10 colnames of data_bioCoOccur are:
    # "StationID_Master", "OutcaseCol", "StressSampDate", "RespSampDate",
    # "StressSampID", "BioComm", "RespSampID", "Quality", "CSCI", "RespSampFlag"
    # Remaining columns are stressors/responses
    if (!(TargetSiteID %in% data_bioCoOccur$StationID_Master)) { # Not in data_bioCoOccur
      noStressors = TRUE
    } else {
      dfTarget <- dplyr::filter(data_bioCoOccur, StationID_Master == TargetSiteID)
      if (all(is.na(dfTarget[, 11:ncol(dfTarget)]))) { # In data_bioCoOccur but all values NA
        noStressors = TRUE
      } else {
        noStressors = FALSE
      }
    }

    # If no paired stressors, write to data gaps file
    if (noStressors == TRUE) {
      msg <- paste0("No paired stressor-response samples for", TargetSiteID
                    , " for the ", bioComm, " community.")
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      gapcomment <- paste0("No stressor samples are available for ", TargetSiteID
                           , " within ", lagdays[1], " days before, and ", lagdays[2]
                           , " after the ", biocomm, " sample(s) was(were) obtained.")
      gaps <- cbind.data.frame("getCoOccurDataset", paste0("Paired stressor-"
                                                           , bioComm, " data"), 0, gapcomment)

      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
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
      write.table(df_temp, file.path(dir_results, fn_runstats)
                  , append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

      rm(dfTarget)
      next()
    } ### End no stressors statement

    # 19, getQualSites ####
    # Progress, 19
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; getQualSites")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Run analyses
    # Identify "quality" samples using different definitions
    # These are identified only from paired stressor-response samples
    # Data gaps statements from getSiteInfo aren't necessarily paired
    list.BioQualSites <- getQualSites(TargetSiteID = TargetSiteID
                                      , df_sites = data_Sites
                                      , biocomm = bioComm
                                      , df_qual = data_bioCoOccur
                                      , colBio = colBio
                                      , colBioSample = "RespSampID"
                                      , colStressSample = "StressSampID"
                                      , compSites = comp_sites # inside the case
                                      , allSites = all_sites # outside the case
                                      , useBC = useBC
                                      , outcaseColName = "OutcaseCol"
                                      , outcaseID = outcaseID
                                      , BioNarBrk = BioNarBrk
                                      , BioNarLab = BioNarLab
                                      , BioDegBrk = BioDegBrk
                                      , BioDegLab = c("Yes", "No")
                                      , dir_results = dir_results
                                      , dir_sub = "SiteInfo")
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

    allQual2PlotSamps <- switch(siteQual2Plot
                                   , "reference" = list.BioQualSites$allRefBioStressSamps
                                   , "not degraded" = list.BioQualSites$allGoodBioStressSamps
                                   , "better than" = list.BioQualSites$allBTBioStressSamps)
    msg <- paste0("getQualSites is complete for ", bioComm, ".")
    message(msg)

    # Prepare data set of all response index values ever determined for the
    # target site for use in getTimeSeq
    siteRespAll <- bioMetricData %>%
      dplyr::filter(StationID_Master == TargetSiteID) %>%
      dplyr::rename(RespSampID = eval(colBioSample), RespSampDate = eval(colBioSampDate)) %>%
      dplyr::select(StationID_Master, RespSampID, RespSampDate, Quality, eval(bioIndex))

    # 20, getDataSets ####
    # Progress, 20
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; getDataSets")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get data sets for stressors paired with response data, if available
    listPairedStressResp <- getDataSets(TargetSiteID = TargetSiteID
                                        , compSites = comp_sites
                                        , allSites = all_sites
                                        , df_coOccur = data_bioCoOccur
                                        , siteStressors = stressors
                                        , bioParmsDEL = bioParmsDEL
                                        , colBioSample = colBioSample
                                        , colBioSampDate = colBioSampDate
                                        , df_biometrics = bioMetricData
                                        , df_stressinfo = data_stressInfo)
    # Returns: mySubsets <- list(siteStressInfo = df_stressinfo
    #                            , allBioStress = allBioStressData
    #                            , compBioStress = compBioStressData
    #                            , siteBioStress = siteBioStressData
    #                            , allBioResp = allBioRespData
    #                            , compBioResp = compBioRespData
    #                            , siteBioResp = siteBioRespData)
    msg <- "Stressor and response data prepared, for all possible stressors."
    message(msg)

    # 22, getTimeSeq ####
    # Progress, 22
    if (boo_Shiny == TRUE) {
      prog_det <- "getTimeSeq"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Create time sequence graphics for ONLY target site
    # Uses all site stressor and response data, but not necessarily paired
    getTimeSeq(TargetSiteID
               , biocomm = bioComm
               , BioResp = bioMetricNames
               , df_stress = siteStressAll
               , df_resp = siteRespAll
               , stressors = stressors
               , df_stressinfo = data_stressInfo
               , df_respinfo = bioMetricInfo
               , dir_results = dir_results
               , dir_sub = "TimeSequence"
               , boo_plot = boo_plot_user)
    msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
    message(msg)

    dirTS <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                       , "TimeSequence")
    if (dir.exists(dirTS) == TRUE) {
      if (length(list.files(dirTS)) > 0) {
        numLoE = numLoE + 1
        df_LoE$Completed[df_LoE$LoE == "TS"] <- 1
        df_LoE$ResultsDir[df_LoE$LoE == "TS"] <- dirTS
      }## IF ~ length ~ END
    }## IF ~ dir.exists(dirTS) ~ END

    # rm(siteStressAll, siteRespAll)

    # 23, getCoOccur ####
    # Progress, 23
    if (boo_Shiny == TRUE) {
      prog_det <- "getCoOccurr"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get Co-occurrence from comparator sites with better than biology
    if (TargetSiteID %in% unique(data_bioCoOccur$StationID_Master)) {
      msg <- "Starting Co-occurrence"
      message(msg)
      getCoOccur(TargetSiteID = TargetSiteID
                 , df_data = data_bioCoOccur[data_bioCoOccur$StationID_Master %in% comp_sites, ]
                 , col_ID = "StationID_Master"
                 , colStressSamp = "StressSampID"
                 , colRespSamp = "RespSampID"
                 , colGroup = "OutcaseCol"
                 , colBio = colBio
                 , colStressors = stressors
                 , df_stressinfo = data_stressInfo
                 , BioNarBrk = BioNarBrk
                 , BioNarLab = BioNarLab
                 , BioDegBrk = BioDegBrk
                 , BioDegLab = c("Yes", "No")
                 , biocomm = bioComm
                 , dir_plots = dir_results
                 , dir_sub = "CoOccurrence"
                 , col_StressInvScore = col_StressInvScore
                 , boo_plot = boo_plot_user)
    } else {
      # gapcomment <- "Stressor detected but paired response not available"
      # gaps <- cbind.data.frame("getStressorList", stressorsNOpairing[s], 0
      #                          , gapcomment)
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      # fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      # write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
      #             , row.names = FALSE, sep = "\t")
    } ### End getCoOccur
    msg <- paste0("getCoOccur for ", bioComm, " is complete.")
    message(msg)

    dirCO <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                       , "CoOccurrence")
    if (dir.exists(dirCO) == TRUE) {
      if ((length(list.files(dirCO)) > 0) == TRUE) {
        numLoE = numLoE + 1
        df_LoE$Completed[df_LoE$LoE == "CO"] <- 1
        df_LoE$ResultsDir[df_LoE$LoE == "CO"] <- dirCO
      }
    }

    # 24, getSufficiency ####
    # Progress, 24
    if (boo_Shiny == TRUE) {
      prog_det <- "getCoOccurr"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get stressors sufficient to cause biological impairment using all comparator samples
    if (TargetSiteID %in% unique(data_bioCoOccur$StationID_Master)) {
      msg <- "Starting Sufficiency"
      message(msg)
      getSufficiency(TargetSiteID = TargetSiteID
                     , df_data = data_bioCoOccur
                     , compSites = comp_sites
                     , stressors = stressors
                     , df_stressinfo = data_stressInfo
                     , biocomm = bioComm
                     , colBio = bioIndex
                     , BioDegBrk = BioDegBrk
                     , BioDegLab = c("Yes", "No")
                     , dir_plots = dir_results
                     , dir_sub = "Sufficiency"
                     , boo_plot = boo_plot_user)
    } else {
      # gapcomment <- "Stressor detected but paired response not available"
      # gaps <- cbind.data.frame("getStressorList", stressorsNOpairing[s], 0
      #                          , gapcomment)
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      # fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      # fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      # write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
      #             , row.names = FALSE, sep = "\t")
    } ### End getCoOccur
    msg <- paste0("getSufficiency for ", bioComm, " is complete.")
    message(msg)

    dirSRlog <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                       , "Sufficiency")
    if (dir.exists(dirSRlog) == TRUE) {
      if ((length(list.files(dirSRlog)) > 0) == TRUE) {
        numLoE = numLoE + 1
        df_LoE$Completed[df_LoE$LoE == "SRLog"] <- 1
        df_LoE$ResultsDir[df_LoE$LoE == "SRLog"] <- dirSRlog
      }
    }

    # Refine all.b.str, cl.b.str, and site.b.str for just identified stressors
    core.cols <- c("StationID_Master", "StressSampDate", "RespSampDate"
                   , "StressSampID", "RespSampID")

    all.b.str <- listPairedStressResp$allBioStress %>%
      dplyr::select(eval(core.cols), eval(stressors)) %>%
      dplyr::select(StressSampID, RespSampID, StationID_Master, eval(stressors))
    cl.b.str <- listPairedStressResp$compBioStress %>%
      dplyr::select(eval(core.cols), eval(stressors)) %>%
      dplyr::select(StressSampID, RespSampID, StationID_Master, eval(stressors))
    site.b.str <- listPairedStressResp$siteBioStress %>%
      dplyr::select(eval(core.cols), eval(stressors)) %>%
      dplyr::select(StressSampID, RespSampID, StationID_Master, eval(stressors))

    all.b.rsp <- listPairedStressResp$allBioResp %>%
      dplyr::select(RespSampID, StressSampID, StationID_Master, RespSampDate
             , Quality, eval(bioMetricNames))
    cl.b.rsp <- listPairedStressResp$compBioResp %>%
      dplyr::select(RespSampID, StressSampID, StationID_Master, RespSampDate
             , Quality, eval(bioMetricNames))
    site.b.rsp <- listPairedStressResp$siteBioResp %>%
      dplyr::select(RespSampID, StressSampID, StationID_Master, RespSampDate
             , Quality, eval(bioMetricNames))

    siteStressInfo <- listPairedStressResp$siteStressInfo

    list_MatchBioData <- list("all.b.str"    = all.b.str
                              , "cl.b.str"   = cl.b.str
                              , "site.b.str" = site.b.str
                              , "all.b.rsp"  = all.b.rsp
                              , "cl.b.rsp"   = cl.b.rsp
                              , "site.b.rsp" = site.b.rsp)

    rm(listPairedStressResp, all.b.str, cl.b.str, site.b.str, all.b.rsp
       , cl.b.rsp, site.b.rsp)

    # 25, getBioStressorResponses ####
    # Progress, 25
    if (boo_Shiny == TRUE) {
      prog_det <- "getBioStressorResponses"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get Stressor Responses inside (comparators) and outside (all) the case
    getBioStressorResponses(TargetSiteID = TargetSiteID
                            , stressors = stressors
                            , df_stressinfo = siteStressInfo
                            , BioResp = bioMetricNames
                            , df_respinfo = bioMetricInfo
                            , list.MatchBioData = list_MatchBioData
                            , qual2plotSamps = allQual2PlotSamps
                            , siteQual2Plot = siteQual2Plot
                            , biocomm = bioComm
                            , dir_plots = dir_results
                            , dir_sub = "StressorResponse"
                            , boo_plot = boo_plot_user)
    msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
    message(msg)

    dirSRLin <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                       , "StressorResponse")
    if (dir.exists(dirSRLin) == TRUE) {
      if (length(list.files(dirSRLin)) > 0) {
        numLoE = numLoE + 1
        df_LoE$Completed[df_LoE$LoE == "SRLin"] <- 1
        df_LoE$ResultsDir[df_LoE$LoE == "SRLin"] <- dirSRLin
      } else {
        numLoE = numLoE + 1
        df_LoE$Completed[df_LoE$LoE == "SRLin"] <- 0
        df_LoE$ResultsDir[df_LoE$LoE == "SRLin"] <- NA
        unlink(dirSRLin, recursive = TRUE)
      }
    }

    # 26, getVerifiedPredictions ####
    # Progress, 26
    if (boo_Shiny == TRUE) {
      prog_det <- "getVerifiedPredictions"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END
    #
    # Get Stressor-specific regressions using comparator sites
    if (any(SSTVparms %in% stressors)) {
      getVerifiedPredictions(TargetSiteID = TargetSiteID
                             , stressors = stressors
                             , df_stressinfo = siteStressInfo
                             , SSTVanalytes = as.character(SSTVparms)
                             , list.MatchBioData = list_MatchBioData
                             , biocomm = bioComm
                             , colBioSample = colBioSample
                             , df_BioTaxaRelAbund = bioTaxaData
                             , df_MasterTaxa = bioMasterTaxa
                             , colBio = bioIndex
                             , BioIndex_Nar = "Quality"
                             , BioIndex_Nar_Deg = "Degraded"
                             , dir_plots = dir_results
                             , dir_sub = "VerifiedPredictions"
                             , boo_plot = boo_plot_user)
    } else {
      msg <- "No possible stressors have stressor-specific tolerance values."
      message(msg)
      gapcomment <- paste0("Stressors having stressor-specific tolerance "
                           , "values are not identified at this site.")
      gaps <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0
                               , gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
    } ### End getVP evaluation

    msg <- paste0("getVerifiedPredictions for ", bioComm, " is complete.")
    message(msg)

    dirVP <- file.path(dir_results, TargetSiteID, toupper(bioComm)
                       , "VerifiedPredictions")
    if (dir.exists(dirVP) == TRUE) {
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

    # 27, getWOE ####
    # Progress, 27
    if (boo_Shiny == TRUE) {
      prog_det <- "getWOE"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    getWoE(TargetSiteID = TargetSiteID
           , outcaseLabel = outcaseLabel
           , biocomm = bioComm
           , index = bioIndex
           , BioResp = bioMetricNames
           , dfQual = list.BioQualSites$dfQuality
           , dfStr = list_MatchBioData$site.b.str
           , dfRank = list.stressors$site.stressor.pctrank
           , dfStressInfo = siteStressInfo
           , df_coOccur = data_bioCoOccur
           , dfLoE = df_LoE
           , dir_results = dir_results
           , dir_WoE = "WoE")
    msg <- paste0("getWoE for ", bioComm, " is complete.")
    message(msg)

    # Write run-time stats to file
    endsite.time <- Sys.time()
    elapsedsite.time <- endsite.time - startsite.time

    df_temp <- as.data.frame(cbind("TargetSiteID" = TargetSiteID
                                   , "Biocomm" = bioComm
                                   , "NumStressors" = length(stressors)
                                   , "NumLoE" = numLoE
                                   , "ElapsedTime" = elapsedsite.time))

    write.table(df_temp, file.path(dir_results, fn_runstats)
                , append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")

  } ### End biocomm loop
  # FOR ~ b ~ END ####

  # 28, getReport ####
  # Progress, 28
  if (boo_Shiny == TRUE) {
    prog_det <- "getReport"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(mySleepTime)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ boo_Shiny ~ END
  #
  # Shiny add ons
  if (boo_Shiny == TRUE) {
    report_type     <- "preliminary"
  } else {
    report_type     <- "summary"
  }
  # dir_data_abs    <- normalizePath(dir_data)
  # dir_results_abs <- normalizePath(dir_results)
  # dir_rmd         <- normalizePath(dir_rmd)
  strFile_RMD     <- file.path(dir_rmd, paste0("Report_Results_", report_type, ".rmd"))
  message(paste0("file = ", strFile_RMD))
  message(paste0("exists = ", file.exists(strFile_RMD)))
  #
  # Get final report (Executive Summary style)
  # Report (rmd file) is not working, but since it will change, I'm simply commenting
  # out the code so it doesn't run  --- ARL 2023-05-26
  # getReport(TargetSiteID
  #           , probsHigh = probsHigh
  #           , probsLow = probsLow
  #           , useBMI = useBMI
  #           , useAlg = useAlg
  #           , useBC = useBC
  #           , removeOutliers = removeOutliers
  #           , DOlim = DOlim
  #           , pHlimHigh = pHlimHigh
  #           , pHlimLow = pHlimLow
  #           , lagdays = lagdays
  #           , bmiIndex = bmiIndex
  #           , algIndex = algIndex
  #           , dir_data = dir_data
  #           , dir_results = dir_results
  #           , report_type = report_type
  #           , report_format = "html"
  #           , dir_rmd = dir_rmd
  #           , siteQual2Plot = siteQual2Plot)

  dfGaps <- read.table(file.path(dir_results, TargetSiteID
                                 , paste0(TargetSiteID,"_datagaps.tab"))
                       , header = TRUE, sep = "\t")
  dfGaps <- unique(dfGaps)
  write.table(dfGaps, file.path(dir_results, TargetSiteID
                                , paste0(TargetSiteID,"_datagaps.tab"))
              , append = FALSE, col.names = TRUE, row.names = FALSE
              , sep = "\t")

} ### End TargetSite loop # not used in Shiny
# FOR ~ site ~ END ####

rm(site)

# 29, getSummaryAllSites ####
# Progress, 29
if (boo_Shiny == TRUE) {
  prog_det <- "getSummaryAllSites"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END
#
# getSummaryAllSites
getSummaryAllSites(biocommlist = c("bmi", "algae")
                   , bmiIndex = "CSCI"
                   , algIndex = "MMIhybrid"
                   , fishIndex = NULL
                   , dir_data = dir_data
                   , dir_results = dir_results
                   , dir_sub = "WoE"
                   , df_sites = NULL)

msg <- "getSummaryAllSites is complete."
message(msg)

# rm(list=ls())

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, END ####
# external/RPPTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
