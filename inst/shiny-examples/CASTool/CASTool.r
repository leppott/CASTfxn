# Copyright 2025 TetraTech. All rights reserved.
# Use, copying, modification, or distribution of this file or any of its contents
# is expressly prohibited without prior written permission of TetraTech.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.4.3
#
# CASTfxn
# Erik.Leppo@tetratech.com, 20180710
# Ann.RoseberryLincoln@tetratech.com, 20230605
#
# library(devtools)
# install_github("leppott/CASTfxn")
# requires packages: dplyr, ggplot2, lubridate, nhdplusTools, purrr, readxl, sf,
#                    shiny, stringr, tibble, tidyr, tmap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Shiny code for use in Shiny App
# 2020-10-30, Erik
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2025-09-24, Erik, start mods for updated Shiny App
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, Start ####
# external/CASTool.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Packages ----
# library(tidyverse) #LCN added
# library(CASTfxn)
# if(require(CASToolClusterPckg)!=TRUE){
#   if(require(pak)!=TRUE){
#     install.packages("pak")
#   }
#   pak::pak("laura-naslund/CASToolClusterPckg")
# }
# library(CASToolClusterPckg)

# Global Variables ----
# boo_Shiny <- TRUE # Comment out and define in Shiny App
# boo.debug <- TRUE # Comment out and define in Shiny App
# debug.person <- "Erik" # Ann, Erik, Laura

if (boo_Shiny == FALSE) {
  # prompt user for path to input/output data directories
  # ** Run one line at a time **
  # Else next line of code is taken as the response and it fails
  # in.dir        <- readline(prompt = "Enter input data file directory path: ")
  # out.dir       <- readline(prompt = "Enter output file directory path: ")
  # region        <- readline(prompt = "Enter region name: ")
  # Use tcltk instead
  in.dir <- tcltk::tk_choose.dir(default = getwd(),
                                 caption = "Enter input data file directory path:")
  out.dir <- tcltk::tk_choose.dir(default = getwd(),
                                 caption = "Enter output file directory path: ")
  # get user value for region
  # code help from Bing CoPilot, 20250924
  ## Create a variable to store the input
  input_var <- tcltk::tclVar("")
  ## Create a top-level window
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "Enter a Value")
  ## Create label and entry widgets
  tcltk::tkgrid(tcltk::tklabel(tt, text = "Enter region name: "), padx = 10, pady = 5)
  entry_widget <- tcltk::tkentry(tt, textvariable = input_var, width = 30)
  tcltk::tkgrid(entry_widget, padx = 10, pady = 5)
  ## Function to close the window
  onOK <- function() {
    tcltk::tkdestroy(tt)
  }
  ## OK button
  ok_button <- tcltk::tkbutton(tt, text = "OK", command = onOK)
  tcltk::tkgrid(ok_button, padx = 10, pady = 10)
  ## Wait for user to respond
  tcltk::tkwait.window(tt)
  ## Get the value
  region <- tcltk::tclvalue(input_var)
  #
  in.dir        <- gsub("\\\\", "/", in.dir)
  out.dir       <- gsub("\\\\", "/", out.dir)
  boo.plot.user <- TRUE
}## IF ~ boo_Shiny

`%>%` <- dplyr::`%>%`

## Color assignments ####
# Based on ito_seven from ggpubfigs
data_plotvars <- data.frame("Type" = c("target", "insideND", "insideD", "outsideND", "outsideD"),
                            "Fill" = c("#CC79A7", "#56B4E9", "#E69F00", "#0072B2", "#D55E00"),
                            "Shape" = c(24, 21, 25, 21, 25),
                            "Size" = c(1.75, 0.8, 1, 0.8, 1),
                            "Alpha" = c(1, 0.5, 0.7, 0.5, 0.7))
refOutline_col <- "#26F7FD" # LCN changed from "#009E73"
# Note: this change may not be colorblind-friendly
# check site map using https://www.color-blindness.com/coblis-color-blindness-simulator/

## Other plot variables ####
plot_dpi <- 600
plot_H <- 6
plot_W <- 8
plot_units <- "in"

# 01, Set up ####
# Progress, 02
## Shiny ----
if (boo_Shiny == TRUE) {
  prog_det <- "Set up"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
  #
  gitpath     <- file.path(".", "external", "R")  # used in getReport
  dir_rmd     <- file.path(".", "external", "rmd")
  wd          <- file.path(".")
  dir_data    <- file.path(wd, "Data")
  dir_results <- file.path(wd, "Results")
} else {# Not using shiny app
  #
  # in global in shiny
  # not_all_na <- function(x) {!all(is.na(x))}
  ## Ann ----
  if (boo.debug == TRUE & debug.person == "Ann") {

    wd <- "C:/Users/ann.lincoln/Documents" # ARL 2025-01-13
    gitpath <- file.path(wd, "GitHub", "CASTfxn", "R") # ARL 2023-05-22
    dir_rmd <- file.path(wd, "GitHub", "CASTfxn", "inst", "rmd") # ARL 2023-05-22

    # localdir <- file.path(wd, "CASTool_DATA")
    # in.dir <- file.path(localdir, "UploadedData_Test")

    ### source functions ----
    # sourcing so can use updates without reinstalling the package
    ## All data
    source(file.path(gitpath, "readCASToolData.R"))
    source(file.path(gitpath, "checkInputs.R"))
    source(file.path(gitpath, "prepSiteData.R"))
    source(file.path(gitpath, "prepMeasStressorData.R"))
    source(file.path(gitpath, "prepModStressorData.R"))
    source(file.path(gitpath, "getOutliers.R"))
    source(file.path(gitpath, "prepRespData.R"))
    source(file.path(gitpath, "getCoOccurDataset.R"))
    source(file.path(gitpath, "getAllSamplesTable.R"))
    ## Target site & inside/outside case
    source(file.path(gitpath, "getComparators.R"))
    source(file.path(gitpath, "getSiteInfo.R"))
    source(file.path(gitpath, "getWSStressorFigs.R"))
    source(file.path(gitpath, "getSiteMap.R"))
    source(file.path(gitpath, "writeOutliers.R"))
    # source(file.path(gitpath, "getClusterInfo.R")) # no longer used
    source(file.path(gitpath, "getAvailableDataTypes.R"))
    # source(file.path(gitpath, "getStressorList.R")) # no longer used
    source(file.path(gitpath, "getQualSites.R"))
    # source(file.path(gitpath, "getDataSets.R")) # no longer used
    ### Evaluate lines of evidence
    source(file.path(gitpath, "getCoOccur.R"))
    source(file.path(gitpath, "getTimeSeq.R"))
    source(file.path(gitpath, "getSufficiency.R"))
    source(file.path(gitpath, "getBioStressorResponses.R"))
    source(file.path(gitpath, "getVerifiedPredictions.R"))
    source(file.path(gitpath, "getVPSSI.R"))
    ### Summarize findings
    source(file.path(gitpath, "getWoE.R"))
    source(file.path(gitpath, "getReport.R"))
    ## Summarize findings for all test sites
    # source(file.path(gitpath, "getSummaryAllSites.R"))
    #}
    # Erik ----
  } else if (boo.debug == TRUE & debug.person == "Erik") {
    # library(CASTfxn)
    # gitpath <- file.path(system.file(package = "CASTfxn"), "R")
    dir_rmd <- file.path(system.file(package = "CASTfxn"), "inst", "rmd")
    # wd <- "C:/Users/Erik.Leppo/OneDrive - Tetra Tech, Inc/MyDocs_OneDrive/GitHub/CASTfxn/inst/shiny-examples/CAST_SMC"
    wd <- "C:\\Users\\Erik.Leppo\\Documents\\GitHub\\CAST_Shiny\\apps\\CASTool_USEPA"
    dir_data <- file.path(wd, "Data")
    dir_results <- file.path(wd, "Results")
    site <- "SMC04134"
    TargetSiteID <- site
    b <- 1
    ## Laura ----
  } else if (boo.debug == TRUE & debug.person == "Laura") {
    #LCN file paths
    # region <- "WA"
    wd <-  "C:/Users/lnaslund/Documents"
    gitpath <- file.path(wd, "CASTfxn_AnnFinal" , "R")
    dir_rmd <- file.path(wd, "CASTfxn_AnnFinal",  "inst", "rmd")
    localdir <- file.path(wd, "CASTool_Data", "20250711_FinalInputDataFormat")
    dir_data <- file.path(localdir, "Data")
    dir_results <- file.path(localdir, "Results")

    source(file.path(gitpath, "readCASToolData.R"))
    source(file.path(gitpath, "checkInputs.R"))
    source(file.path(gitpath, "prepSiteData.R"))
    source(file.path(gitpath, "prepMeasStressorData.R"))
    source(file.path(gitpath, "prepModStressorData.R"))
    source(file.path(gitpath, "getOutliers.R"))
    source(file.path(gitpath, "prepRespData.R"))
    source(file.path(gitpath, "getCoOccurDataset.R"))
    source(file.path(gitpath, "getAllSamplesTable.R"))
    ## Target site & inside/outside case
    source(file.path(gitpath, "getComparators.R"))
    source(file.path(gitpath, "getSiteInfo.R"))
    source(file.path(gitpath, "getWSStressorFigs.R"))
    source(file.path(gitpath, "getSiteMap.R"))
    source(file.path(gitpath, "writeOutliers.R"))
    # source(file.path(gitpath, "getClusterInfo.R")) # no longer used
    source(file.path(gitpath, "getAvailableDataTypes.R"))
    # source(file.path(gitpath, "getStressorList.R")) # no longer used
    source(file.path(gitpath, "getQualSites.R"))
    # source(file.path(gitpath, "getDataSets.R")) # no longer used
    ### Evaluate lines of evidence
    source(file.path(gitpath, "getCoOccur.R"))
    source(file.path(gitpath, "getTimeSeq.R"))
    source(file.path(gitpath, "getSufficiency.R"))
    source(file.path(gitpath, "getBioStressorResponses.R"))
    source(file.path(gitpath, "getVerifiedPredictions.R"))
    source(file.path(gitpath, "getVPSSI.R"))
    ### Summarize findings
    source(file.path(gitpath, "getWoE.R"))
    source(file.path(gitpath, "getReport.R"))
  } else {#boo.debug == FALSE
    # Install CASTfxn package
    # library(CASTfxn)
    # Set local directory info
    wd <- file.path(".")
    dir_data <- file.path(wd, "Data")
    dir_results <- file.path(wd, "Results")
    boo.plot.user <- TRUE
    
    #C:/Users/lnaslund/Documents/CASTool_Data/20250711_FinalInputDataFormat/Data
  }
  #
}## IF ~ boo_Shiny ~ END

msg <- paste0("debug = ", boo.debug
              , ifelse(boo.debug == FALSE, ""
                       , paste0(", person = ", debug.person)))
message(msg)

# define pipe
# `%>%` <- dplyr::`%>%`
# not_all_na <- function(x) {!all(is.na(x))}

#~~~~~~~~~~~~~~~~~~~~~~~
# 02, Check inputs ####
# Progress, 02

if (boo_Shiny == TRUE) {
  prog_det <- "Data, Model"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
} else {
  # # prompt user for path to input/output data directories
  # in.dir        <- readline(prompt = "Enter input data file directory path: ")
  # out.dir       <- readline(prompt = "Enter output file directory path: ")
  # region        <- readline(prompt = "Enter region name: ")
  # in.dir        <- gsub("\\\\", "/", in.dir)
  # out.dir       <- gsub("\\\\", "/", out.dir)
  # boo.plot.user <- TRUE
}## IF ~ boo_Shiny ~ END

list.Tables <- checkInputs(dir.uploaded = in.dir,
                           dir.out = out.dir)
TableOne    <- list.Tables$TableOne
write.table(TableOne, file.path(out.dir, region, "Results", "TableOne.tab"),
            sep = "\t", col.names = TRUE, row.names = FALSE, append = FALSE)
TableTwo    <- list.Tables$TableTwo
write.table(TableTwo, file.path(out.dir, region, "Results", "TableTwo.tab"),
            sep = "\t", col.names = TRUE, row.names = FALSE, append = FALSE)
rm(list.Tables, TableOne, TableTwo)

#~~~~~~~~~~~~~~~~~~~~~~~
# 03, Select region variables ####
# Progress, 03
out.dir <- file.path(out.dir, region, "Results")

## Load CASTool_Metadata ####
data_CASTmeta <- readRDS(file.path(out.dir, "_CheckedInputs", "CASTmetadata.rds"))
data_CASTmeta <- data_CASTmeta %>%
  tidyr::pivot_wider(names_from = Variable, values_from = Value)

## Read loaded.rds ####
data_loaded <- readRDS(file.path(out.dir, "_CheckedInputs", "loaded.rds"))
loaded      <- as.character(data_loaded$Object)

# Set up booleans for different data types available
boo.meas  <- FALSE
boo.model <- FALSE
boo.WS    <- FALSE
for (l in seq_along(loaded)) {
  object <- loaded[l]
  if (grepl("chem", object) == TRUE)  {
    boo.meas  <- TRUE
    if (grepl("Info", object) == TRUE) {
      meta.meas <- object
    } else {
      data.meas <- object
    }
  }
  if (grepl("model", object) == TRUE) {
    boo.model <- TRUE
    if (grepl("Info", object) == TRUE) {
      meta.mod <- object
    } else {
      data.mod <- object
    }
  }
  if (grepl("WS", object) == TRUE) { boo.WS <- TRUE }
}
rm(l, object, data_loaded)

## Get variables ####
### Response data ####
biocommlist <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, biocommlist), ", "))
# Bio responses
for (b in seq_along(biocommlist)) {
  bio <- tolower(biocommlist[b])
  calcRelAbund       <- as.logical(dplyr::select(data_CASTmeta, calcRelAbund))
  if (bio == "bmi") {
    bmiIndexGp       <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, bmiIndexGp), ", "))
    useBC            <- as.logical(dplyr::select(data_CASTmeta, useBC))
  }
  if (bio == "algae") {
    algIndexGp       <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, algIndexGp), ", "))
  }
  if (bio == "fish") {
    fishIndexGp      <- unlist(stringr::str_split(dplyr::select(data_CASTmeta, fishIndexGp), ", "))
  }
}

### Stressor data ####
removeOutliers  <- as.logical(dplyr::select(data_CASTmeta, removeOutliers))
samplim         <- as.integer(dplyr::select(data_CASTmeta, samplim))
r2_cutoff       <- as.numeric(dplyr::select(data_CASTmeta, r2_cutoff))
p.val_cutoff    <- as.numeric(dplyr::select(data_CASTmeta, p.val_cutoff))

if (boo.meas) {
  DOlim         <- as.numeric(dplyr::select(data_CASTmeta, DOlim))
  pHlimLow      <- as.numeric(dplyr::select(data_CASTmeta, pHlimLow))
  pHlimHigh     <- as.numeric(dplyr::select(data_CASTmeta, pHlimHigh))
  lagdays       <- as.integer(unlist(stringr::str_split(dplyr::select(data_CASTmeta,
                                                                      lagdays), ", ")))
}

if (boo.WS) {
  useAllCompReaches  <- as.logical(dplyr::select(data_CASTmeta, useAllCompReaches))
}

### Site variables ####
datum          <- as.character(dplyr::select(data_CASTmeta, datum))
outcaseColName <- as.character(dplyr::select(data_CASTmeta, outcaseColName))
outcaseLabel   <- as.character(dplyr::select(data_CASTmeta, outcaseLabel))
incaseColName  <- as.character(dplyr::select(data_CASTmeta, incaseColName))
incaseLabel    <- as.character(dplyr::select(data_CASTmeta, incaseLabel))

rm(b, bio, data_CASTmeta)

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

list.SiteData <- prepSiteData(out.dir = file.path(out.dir, "_CheckedInputs"))
data_Sites    <- list.SiteData$site
data_cluster  <- list.SiteData$cluster
refSites      <- list.SiteData$refSites
rm(list.SiteData)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Get GIS files ####
# TODO: Decide exactly how this is going to happen.
# fn.outline <- file.path(localdir, "NHDPlus", "gadm41_USA_shp", "gadm41_USA_1.shp")

# Required user-designated options
# LCN this code only handles if region is a state name or abbreviation
# TODO: is this necessary anymore? Region is in the _CASTool_Metadata.xlsx file
# Determine how to obtain the shapefile
# if (region %in% state.abb) {
#   regionName        <- state.name[which(state.abb == region)]
# } else if (region %in% state.name) {
#   regionName        <- region
#   region            <- state.abb[which(state.name == regionName)]
# } else if (region == "WA_LCN") {
#   regionName <- "WA"
# } else {
#   # region is not a standard, accepted region (e.g., SMC)
#   # this will affect watershed-scale stressors and maps
#   if (region == "SMC") {
#     outline  <- poly.smc.proj
#     flowline <- lines.flowline.proj
#   }
# }
# message("Loading GIS files.")
# if (boo_Shiny == TRUE) {
#   # 2020-09-09, use RDA saved version
#   # NOT sure how to handle this # ARL 2025-04-13
#   outline  <- poly.smc.proj
#   flowline <- lines.flowline.proj
# } else {
#   # fn.outline moved to the top with other hard-coded file locations
#   STATE.shp <- sf::read_sf(fn.outline) %>%
#     dplyr::filter(NAME_1 == regionName) %>%
#     sf::st_transform(crs = 5070) %>%
#     sf::st_buffer(300)
#   NHD.STATE <- nhdplusTools::get_nhdplus(AOI = STATE.shp) %>%
#     dplyr::filter(ftype %in% c("StreamRiver", "ArtificialPath", "Connector",
#                                "CanalDitch", "Drainageway")) %>%
#     dplyr::select(comid, geometry) %>%
#     dplyr::rename(COMID = comid)
# NHD.STATE <- dplyr::left_join(NHD.STATE, data_cluster, by = "COMID")
# ## Remove reaches without clusterIDs
# NHD.STATE <- NHD.STATE[!is.na(NHD.STATE$ClusterID), ]
# ## Select only required columns
# NHD.STATE <- dplyr::select(NHD.STATE, COMID, ClusterID, geometry)
# }## IF ~ boo_Shiny ~ END
# rm(fn.outline)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

if (boo.meas) {
  list.measStress   <- prepMeasStressorData(in.dir = file.path(out.dir, "_CheckedInputs"),
                                            out.dir = out.dir,
                                            fn.data = paste0(data.meas, ".rds"),
                                            fn.meta = paste0(meta.meas, ".rds"),
                                            removeOutliers = removeOutliers,
                                            sub.dir = "_Histograms")
  data_chemInfo     <- list.measStress$data_chemInfo
  data_chemRaw      <- list.measStress$data_chemRaw
  data_measoutliers <- list.measStress$data_measoutliers
  rm(list.measStress)
}

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

if (boo.model) {
  list.modStress     <- prepModStressorData(in.dir = file.path(out.dir, "_CheckedInputs"),
                                            out.dir = out.dir,
                                            fn.data = paste0(data.meas, ".rds"),
                                            fn.meta = paste0(meta.meas, ".rds"),
                                            removeOutliers = removeOutliers,
                                            sub.dir = "_Histograms")
  data_modelInfo     <- list.modStress$data_modelInfo
  data_modelRaw      <- list.modStress$data_modelAll %>%
    dplyr::mutate(StressSampleDate = NA) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, TransfValue)
  data_modeloutliers <- list.modStress$data_modeloutliers
  rm(list.modStress)
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
if (boo.meas && boo.model) {
  # Combine metadata
  chemMetaNames   <- colnames(data_chemInfo)
  modelMetaNames  <- colnames(data_modelInfo)
  extraNames      <- chemMetaNames[!(chemMetaNames %in% modelMetaNames)]
  for (e in 1:length(extraNames)) {
    newCol        <- extraNames[e]
    data_modelInfo[[newCol]] <- NA
  }
  data_modelInfo  <- data_modelInfo[, chemMetaNames]
  data_stressInfo <- rbind(data_chemInfo, data_modelInfo)
  rm(data_chemInfo, data_modelInfo)
  rm(chemMetaNames, modelMetaNames, extraNames, newCol, e)

  data_stressInfo <- dplyr::distinct(data_stressInfo, StdParamName, Label,
                                     LogTransf, UseInStressorID, DirIncStress,
                                     SSTVname.bmi, SensMax.bmi, SensMin.bmi,
                                     SSTVname.alg, SensMax.alg, SensMin.alg,
                                     SSTVname.fish, SensMax.fish, SensMin.fish,
                                     SSIndex, SourceGroup)
  # Combine data
  data_Stress     <- rbind(data_chemRaw, data_modelRaw)
  rm(data_chemRaw, data_modelRaw)
  # Combine outliers
  if (removeOutliers) {
    data_stressoutliers <- rbind(data_measoutliers, data_modeloutliers)
    rm(data_measoutliers, data_modeloutliers)
  }
} else if (boo.meas) {
  data_stressInfo <- data_chemInfo
  data_Stress     <- data_chemRaw
  rm(data_chemInfo, data_chemRaw)
  if (removeOutliers) {
    data_stressoutliers <- data_measoutliers
    rm(data_measoutliers)
  }
} else {
  data_stressInfo <- data_modelInfo
  data_Stress     <- data_modelRaw
  rm(data_modelInfo, data_modelRaw)
  if (removeOutliers) {
    data_stressoutliers <- data_modeloutliers
    rm(data_modeloutliers)
  }
}

# If using, get WS stressor data
if (boo.WS) {
  data_stressorWS     <- readRDS(file.path(out.dir, "_CheckedInputs", "data_stressorWS.rds"))
  data_stressorinfoWS <- readRDS(file.path(out.dir, "_CheckedInputs",
                                           "data_stressorinfoWS.rds"))
}

# Bio responses
boo.bmi <- FALSE
boo.alg <- FALSE
boo.fish <- FALSE
data_respTrim <- data.frame()

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

  if (bio == "bmi") {
    # Read bmi data files
    message("Reading BMI data files")
    boo.bmi <- TRUE
    list.bmiData <- prepRespData(out.dir  = file.path(out.dir, "_CheckedInputs"),
                                 bio      = "bmi",
                                 loaded   = loaded,
                                 useBC    = useBC,
                                 bioIndex = bmiIndexGp)
    data_bmiMetrics     <- list.bmiData$data_bioMetrics
    data_bmiMetricsInfo <- list.bmiData$data_bioMetricsInfo
    data_bmiCounts      <- list.bmiData$data_bioCounts
    data_bmiMasterTaxa  <- list.bmiData$data_bioMasterTaxa

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_bmiCoOccur <- getCoOccurDataset(df_sites  = data_Sites,
                                         df_stress = data_Stress,
                                         biocomm   = "BMI",
                                         df_resp   = data_bmiMetrics,
                                         index     = bmiIndexGp,
                                         lagdays   = lagdays)
    data_respTrim <- rbind(data_respTrim,
                           data_bmiCoOccur[, c("StationID", "RespSampleID",
                                               "RespSampleDate", "BioComm")]) %>%
      dplyr::mutate(biocomm = "BMISampleID")
  } # end BMI
  if (bio == "alg") {
    # Read alg data files
    message("Reading alg data files")
    boo.alg <- TRUE
    list.algData <- prepRespData(out.dir  = file.path(out.dir, "_CheckedInputs"),
                                 bio      = "alg",
                                 loaded   = loaded,
                                 useBC    = useBC,
                                 bioIndex = algIndexGp)
    data_algMetrics     <- list.algData$data_bioMetrics
    data_algMetricsInfo <- list.algData$data_bioMetricsInfo
    data_algCounts      <- list.algData$data_bioCounts
    data_algMasterTaxa  <- list.algData$data_bioMasterTaxa

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_algCoOccur <- getCoOccurDataset(df_sites  = data_Sites,
                                         df_stress = data_Stress,
                                         biocomm   = "alg",
                                         df_resp   = data_bmiMetrics,
                                         index     = algIndexGp,
                                         lagdays   = lagdays)
    data_respTrim <- rbind(data_respTrim,
                           data_algMetrics[, c("StationID", "RespSampleID",
                                               "RespSampleDate", "BioComm")]) %>%
      dplyr::mutate(biocomm = "AlgSampleID")
  } # end ALG
  if (bio == "fish") {
    # Read fish data files
    message("Reading fish data files")
    boo.fish <- TRUE
    list.fishData <- prepRespData(out.dir  = file.path(out.dir, "_CheckedInputs"),
                                  bio      = "fish",
                                  loaded   = loaded,
                                  useBC    = useBC,
                                  bioIndex = fishIndexGp)
    data_fishMetrics     <- list.fishData$data_bioMetrics
    data_fishMetricsInfo <- list.fishData$data_bioMetricsInfo
    data_fishCounts      <- list.fishData$data_bioCounts
    data_fishMasterTaxa  <- list.fishData$data_bioMasterTaxa

    # Generate co-occurrence data set (same day samples; modeled data match any day)
    data_fishCoOccur <- getCoOccurDataset(df_sites = data_Sites,
                                         df_stress = data_Stress,
                                         biocomm   = "FISH",
                                         df_resp   = data_bmiMetrics,
                                         index     = fishIndexGp,
                                         lagdays   = lagdays)
    data_respTrim <- rbind(data_respTrim,
                           data_fishMetrics[, c("StationID", "RespSampleID",
                                                "RespSampleDate", "BioComm")]) %>%
      dplyr::mutate(biocomm = "FishSampleID")
  } # end FISH

}
data_respTrim <- data_respTrim %>%
  dplyr::distinct(StationID, RespSampleID, RespSampleDate, biocomm)

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

data_sampSummary <- getAllSamplesTable(df.stress     = data_Stress,
                                       df.stressInfo = data_stressInfo,
                                       df.resp       = data_respTrim,
                                       df.sites      = data_Sites)
# Returns: data_sampSummary (df.sampSummary)
# Colnames include: StationID, COMID, OutcaseCol, IncaseCol, SampleDate,
# ChemistrySampleID, FieldSampleID, HabitatSampleID, ModeledSampleID,
# BMISampleID, AlgSampleID, FishSampleID (assuming all sample types are available)
rm(data_respTrim)
# Data prep completed
#~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~
# RUN CASTool ####
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
df_targets <- readRDS(file.path(out.dir, "_CheckedInputs", "df_targets.rds"))

### Evaluate each target site
## Use this for debugging
if (boo_Shiny == TRUE) {
  df_targets <- data.frame("TargetSiteID" = input$Station,
                           "Chosen by"    = NA, "Comment" = NA)
  names(df_targets)[2] <- "Chosen by"
} else if (boo.debug == TRUE & debug.person == "Ann") {
  # df_targets <- dplyr::filter(df_targets, TargetSiteID == "BIO06600_BURP15")
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
for (site in seq_along(1:nrow(df_targets))) {
  TargetSiteID <- df_targets$TargetSiteID[site]
  # if (boo.debug == TRUE & debug.person == "Ann") {
  # For debugging purposes only # these need to be - in LCN version of files
  # if (region == "WA") {
  # TargetSiteID <- "BIO06600_BURP15"   # No degraded samples (6 of them)
  # TargetSiteID <- "ERR06600_005995"   # Assigned new COMID; removed all detected stressors
  # TargetSiteID <- "PSS05515_007726"   # All samples degraded; low DO
  # TargetSiteID <- "RSM06600_007971"   # No degraded samples
  # TargetSiteID <- "WAM06600_000586"   # All samples degraded; Temp (tests getVerifiedPredictions.R)
  # TargetSiteID <- "WAM06600_012453"   # Two of two samples degraded; extremely low DO
  # TargetSiteID <- "WAM06600_005424"   # One sample, degraded, response sample date > stress sample date
  # TargetSiteID <- "WAM06600_034707"   # One sample, degraded, stress sample date > response sample date
  # TargetSiteID <- "WAM06600_003688"   # One of two samples degraded
  # TargetSiteID <- "WAM06600-003688"   # One of two samples degraded
  # LCN version of files uses - instead of _
  # } else if (region == "OR") {
  #
  # } else {
  # Different state here
  #   }
  # }

  if (is.na(TargetSiteID)) {
    next
  } else if(data_Sites %>% dplyr::filter(StationID == TargetSiteID) %>% nrow() == 0){ # LCN added 20250917
    msg <- "TargetSiteID not found in the sites file. Skipping to the next TargetSiteID."
    next
  } else if (is.na(data_Sites$IncaseCol[data_Sites$StationID == TargetSiteID])) {
    msg <- "No inside-the-case identifier available"
    message(msg)
    next
  }
  msg <- paste0("Evaluating site: ", TargetSiteID)
  message(msg)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Biocomm-independent functions ####

  # Create high-level results folder structure
  dir_results <- out.dir
  dir_sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(dir_results, dir_sub2)) == TRUE,
         dir.create(file.path(dir_results, dir_sub2)), FALSE)

  # Define datagaps data frame ####
  gaps    <- data.frame(fxnname = character(), condition = character(),
                        result = character(), comment = character())
  fn.gaps <- file.path(dir_results, TargetSiteID,
                       paste0(TargetSiteID, "_datagaps.tab"))
  write.table(gaps, fn.gaps, append = FALSE, col.names = TRUE,
              row.names = FALSE, sep = "\t")

  # 14, getComparators ####
  ## Progress, 14
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
  list.CompSites <- getComparators(TargetSiteID = TargetSiteID,
                                   df_sites = data_Sites,
                                   df_cluster = data_cluster,
                                   df_bioCoOccur = data_bmiCoOccur,
                                   bioIndex = bmiIndexGp,
                                   useBC = useBC,
                                   df_bcdist = data_BCdist,
                                   bc_cutoff = 0.05,
                                   outcaseColName = outcaseColName,
                                   outcaseLabel = outcaseLabel,
                                   incaseColName = incaseColName,
                                   incaseLabel = incaseLabel,
                                   useAllCompReaches = useAllCompReaches,
                                   dir_results = dir_results,
                                   dir_sub = "SiteInfo")
  # Returns: list.CompSites$TargetCOMID (Reach on which target site is located)
  #          list.CompSites$comp.sites (vector of unique inside-the-case sites
  #          regardless of useBC)
  #          list.CompSites$comp.reaches (vector of unique inside-the-case reaches
  #          having sites on them if useAllReaches == FALSE, else all
  #          inside-the-case reaches)
  #          list.CompSites$all.sites (vector of unique outside-the-case sites,
  #          regardless of useBC)
  #          list.CompSites$all.reaches (vector of unique outside-the-case reaches
  #          having sites on them)
  #          list.CompSites$incaseID (inside-the-case identifier, NULL if useBC
  #          is TRUE)
  #          list.CompSites$outcaseID (outside-the-case identifier)
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

  # Create site info folder with watershed-scale stressor boxplots,
  # boxplots for bio indices, and folder for photos
  getSiteInfo(TargetSiteID   = TargetSiteID,
              TargetCOMID    = list.CompSites$TargetCOMID,
              df_Sites       = data_Sites,
              df_SampSummary = data_sampSummary,
              biocommlist    = biocommlist,
              df_BMIMetrics  = data_bmiMetrics,
              BMIIndexGp     = bmiIndexGp,
              df_ALGMetrics  = data_algMetrics,
              ALGIndexGp     = algIndexGp,
              df_FishMetrics = data_fishMetrics,
              FishIndexGp    = fishIndexGp,
              comp.sites     = list.CompSites$comp.sites,
              all.sites      = list.CompSites$all.sites,
              IncaseLabel    = incaseLabel,
              OutcaseLabel   = outcaseLabel,
              useBC          = useBC,
              plotvars       = data_plotvars,
              refSiteCol     = refOutline_col,
              plotdpi        = plot_dpi,
              plotH          = plot_H,
              plotW          = plot_W,
              plotunits      = plot_units,
              dir_photo      = file.path(in.dir, region, "Photos"),
              dir_results    = out.dir,
              dir_sub        = "SiteInfo",
              boo_plot       = TRUE)

  # if (boo.WS) {
  #   getWSStressorFigs(TargetSiteID      = TargetSiteID,
  #                     df_WSData         = data_stressorWS,
  #                     df_WSInfo         = data_stressorinfoWS,
  #                     comp.reaches      = list.CompSites$comp.reaches,
  #                     TargetCOMID       = list.CompSites$TargetCOMID,
  #                     useAllCompReaches = useAllCompReaches,
  #                     dir_sub           = "SiteInfo",
  #                     df_SampSummary    = data_sampSummary,
  #                     biocommlist       = biocommlist,
  #                     boo_plot          = TRUE,
  #                     plotdpi           = plot_dpi,
  #                     plotH             = plot_H,
  #                     plotW             = plot_W,
  #                     plotunits         = plot_units)
  # }

  
  # Create site map
  if(debug.person == "Laura"){
    # if(require(CASToolClusterPckg)!=TRUE){
    #   if(require(pak)!=TRUE){
    #     install.packages("pak")
    #   }
    #   pak::pak("laura-naslund/CASToolClusterPckg")
    # }
    # 
    # library(CASToolClusterPckg)
    
    STATE.shp <- retrieve_boundary(region)
    
    NHD.STATE <- nhdplusTools::get_nhdplus(AOI = STATE.shp) %>%
          dplyr::filter(ftype %in% c("StreamRiver", "ArtificialPath", "Connector",
                                     "CanalDitch", "Drainageway")) %>%
          dplyr::select(comid, geometry) %>%
          dplyr::rename(COMID = comid)
      NHD.STATE <- dplyr::left_join(NHD.STATE, data_cluster, by = "COMID")
      ## Remove reaches without clusterIDs
      NHD.STATE <- NHD.STATE[!is.na(NHD.STATE$ClusterID), ]
      ## Select only required columns
      NHD.STATE <- dplyr::select(NHD.STATE, COMID, ClusterID, geometry)

      getSiteMap(sp_outline   = STATE.shp,
                 sp_flowline  = NHD.STATE,
                 region       = region,
                 datum        = datum,
                 df_sites     = data_Sites,
                 allSites     = list.CompSites$all.sites,
                 compSites    = list.CompSites$comp.sites,
                 TargetSiteID = TargetSiteID,
                 useBC        = useBC,
                 plotvars     = data_plotvars,
                 refOutline   = refOutline_col,
                 dir_results  = dir_results,
                 dir_sub      = "SiteInfo",
                 dir_map_rmd  = dir_rmd)
  }
  
  # getSiteMap(sp_outline   = STATE.shp,
  #            sp_flowline  = NHD.STATE,
  #            region       = regionName,
  #            datum        = datum,
  #            df_sites     = data_Sites,
  #            allSites     = list.CompSites$all.sites,
  #            compSites    = list.CompSites$comp.sites,
  #            TargetSiteID = TargetSiteID,
  #            useBC        = useBC,
  #            plotvars     = data_plotvars,
  #            refOutline   = refOutline_col,
  #            dir_results  = dir_results,
  #            dir_sub      = "SiteInfo",
  #            dir_map_rmd  = dir_rmd)
  # Prints static map (.png)

  msg <- "getSiteInfo, getWSstressorFigs, and getSiteMap are complete."
  message(msg)

  # 16, getAvailableDataTypes ####
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
  list.AvailData <- getAvailableDataTypes(TargetSiteID   = TargetSiteID,
                                          df_stress      = data_Stress,
                                          df_SampSummary = data_sampSummary,
                                          biocommlist    = biocommlist,
                                          dir_results    = dir_results)
  # Returns: myAvailData <- list(useBMI      = useBMI,
  #                              useAlg      = useAlg,
  #                              useFish     = useFish,
  #                              noStressors = noStressors,
  #                              noResponses = noResponses)
  noStressors    <- list.AvailData$noStressors
  noResponses    <- list.AvailData$noResponses
  useBMI         <- list.AvailData$useBMI
  useAlg         <- list.AvailData$useAlg
  useFish        <- list.AvailData$useFish
  siteDetectsAll <- list.AvailData$siteDetectsAll
  rm(list.AvailData)

  if ((noStressors == TRUE) | (noResponses == TRUE)) {

    msg <- ifelse((noStressors == TRUE) & (noResponses == TRUE),
                  paste0("No stressor or response data are available for ",
                         TargetSiteID),
                  ifelse(noStressors == TRUE,
                         paste0("No stressor data are available for ",
                                TargetSiteID),
                         paste0("No response data are available for ",
                                TargetSiteID)))
    message(msg)

    gaps <- cbind.data.frame("getAvailData", "Number detects/responses", 0, msg)
    colnames(gaps) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
    fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                row.names = FALSE, sep = "\t")

    next

  } ### End no stressors statement GO TO NEXT SITE
  rm(noStressors, noResponses)

  if (boo.WS) {
    getWSStressorFigs(TargetSiteID      = TargetSiteID,
                      df_WSData         = data_stressorWS,
                      df_WSInfo         = data_stressorinfoWS,
                      comp.reaches      = list.CompSites$comp.reaches,
                      TargetCOMID       = list.CompSites$TargetCOMID,
                      useAllCompReaches = useAllCompReaches,
                      dir_sub           = "SiteInfo",
                      df_SampSummary    = data_sampSummary,
                      biocommlist       = biocommlist,
                      boo_plot          = TRUE,
                      plotdpi           = plot_dpi,
                      plotH             = plot_H,
                      plotW             = plot_W,
                      plotunits         = plot_units)
  }
  
  # Write target site outliers, comparator site outliers (inside the case),
  # and all outliers (outside the case)
  writeOutliers(TargetSiteID  = TargetSiteID,
                df_outliers   = data_stressoutliers,
                df_stressInfo = data_stressInfo,
                siteDetects   = siteDetectsAll,
                compSites     = list.CompSites$comp.sites,
                allSites      = list.CompSites$all.sites,
                dir_results   = dir_results)
  # Writes outliers to data gaps file

  msg <- "getAvailableDataTypes and writeOutliers are complete."
  message(msg)

  # FOR ~ b ~ START ####
  if (boo.debug == TRUE & debug.person == "Erik") {
    # 1 = bmi, 2 = alg, 3 = fish
    biocommlist <- "alg"
  }

  for (b in seq_along(biocommlist)) {

    # Define biocomm data
    bioComm <- tolower(biocommlist[b])
    if ((bioComm == "bmi") && (useBMI == TRUE)) {
      data_bioCoOccur <- data_bmiCoOccur
      bioIndexGp      <- bmiIndexGp
      bioMetricData   <- data_bmiMetrics
      bioMetricInfo   <- data_bmiMetricsInfo
      bioTaxaData     <- data_bmiCounts
      bioMasterTaxa   <- data_bmiMasterTaxa
      # colBio <- bmiIndex
    } else if ((bioComm == "algae") && (useAlg == TRUE)) {
      data_bioCoOccur <- data_algCoOccur
      bioIndexGp      <- algIndexGp
      bioMetricData   <- data_algMetrics
      bioMetricInfo   <- data_algMetricsInfo
      bioTaxaData     <- data_algCounts
      bioMasterTaxa   <- data_algMasterTaxa
      # colBio <- algIndex
    } else if ((bioComm == "fish") && (useFish == TRUE)) {
      data_bioCoOccur <- data_fishCoOccur
      bioIndexGp      <- fishIndexGp
      bioMetricData   <- data_fishMetrics
      bioMetricInfo   <- data_fishMetricsInfo
      bioTaxaData     <- data_fishCounts
      bioMasterTaxa   <- data_fishMasterTaxa
      # colBio <- fishIndex
    } else {
      msg <- paste0(bioComm, " is not a valid biological community.")
      message(msg)
      next
    }

    ### Define LoE dataframe ----
    df_LoE <- data.frame(StationID        = character(),
                         StressSampleID   = character(),
                         StressSampleDate = as.Date(character()),
                         RespSampleID     = character(),
                         RespSampleDate   = as.Date(character()),
                         bioComm          = character(),
                         bioIndexName     = character(),
                         bioIndex         = numeric(),
                         Quality          = character(),
                         Stressor         = character(),
                         StressorValue    = numeric(),
                         LoE              = character(),
                         Score            = character(),
                         stringsAsFactors = FALSE)

    # Site-specific paired SR
    # If no paired stressor-response samples for target site, no eval possible
    # First 10 colnames of data_bioCoOccur are:
    # "StationID", "IncaseCol", "OutcaseCol", "StressSampleDate", "RespSampleDate",
    # "StressSampleID", "RespSampleID, "BioComm", bioIndex, "Quality"
    # Remaining columns (11:ncol) are stressors/responses
    if (!(TargetSiteID %in% data_bioCoOccur$StationID)) { # Not in data_bioCoOccur
      noPairedSamps = TRUE
    } else {
      dfTarget <- dplyr::filter(data_bioCoOccur, StationID == TargetSiteID)
      if (all(is.na(dfTarget[, siteDetectsAll]))) { # In data_bioCoOccur but all values NA
        noPairedSamps = TRUE
      } else {
        noPairedSamps = FALSE
      }
    }

    # If no paired stressors, write to data gaps file and output to runstats file
    # Proceed to next target site
    if (noPairedSamps == TRUE) {
      msg <- paste0("No paired stressor-response samples for", TargetSiteID,
                    " for the ", bioComm, " community within specified lag days.")

      # No identified stressors may be a data gap, but may not be, either
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      gapcomment <- paste0("No stressor samples are available for ", TargetSiteID,
                           " within ", lagdays[1], " days before, and ", lagdays[2],
                           " after the ", biocomm, " sample(s) was(were) obtained.")
      gaps       <- cbind.data.frame("getCoOccurDataset",
                                     paste0("Paired stressor-", bioComm, " data"),
                                     0, gapcomment)

      fn.gaps    <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps    <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")

      msg <- paste0(msg, "\nProceeding to next response community or site, ",
                    "as appropriate.")
      message(msg)

      next
    } ### End no stressors statement
    rm(dfTarget)

    ## 17, getQualSites ####
    # Progress, 18
    if (boo_Shiny == TRUE) {
      prog_det <- paste0(bioComm, "; getQualSites")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Run analyses
    # Identify "quality" samples using different definitions
    # These are identified only from paired stressor-response samples
    # Data gaps statements from getSiteInfo aren't necessarily paired

    # IMPORTANT
    # This step adds "RefSiteFlag", BetterThan", "IncaseYN", and "OutcaseYN" to
    # the dataframe, data_bioCoOccur, allowing subsets to be created as needed.
    df_PairedStressResp <- getQualSites(TargetSiteID = TargetSiteID,
                                        biocomm      = bioComm,
                                        df_qual      = data_bioCoOccur,
                                        colBio       = bioIndexGp,
                                        refSites     = refSites,
                                        compSites    = list.CompSites$comp.sites, # inside the case
                                        allSites     = list.CompSites$all.sites, # outside the case
                                        stressors    = siteDetectsAll,
                                        dir_results  = dir_results,
                                        dir_sub      = "SiteInfo")
    # Returns: df_PairedStressResp, a dataframe with the following columns:
    # [1] "StationID"          "IncaseCol"          "OutcaseCol"         "StressSampleDate"
    # [5] "RespSampleDate"     "StressSampleID"     "RespSampleID"       "BioComm"
    # [9] "RefSiteFlag"        "IncaseYN"           "OutcaseYN"          "BetterThan"
    # [13] colBio              "Quality"            all_of(stressors)
    # The rows in this dataframe may be subset as desired to either inside-the-case or
    # outside-the-case with the boolean columns "IncaseYN" and "OutcaseYN"

    # Remove any stressors with fewer than samplim comparator samples
    df_PairedStressResp.stats <- df_PairedStressResp %>%
      dplyr::filter(StationID %in% list.CompSites$comp.sites) %>%
      dplyr::select(StationID, IncaseCol, OutcaseCol, StressSampleDate,
                    RespSampleDate, StressSampleID, RespSampleID, BioComm,
                    all_of(bioIndexGp), Quality, all_of(siteDetectsAll)) %>%
      tidyr::pivot_longer(cols = !(StationID:Quality), names_to = "Stressor",
                          values_to = "StressorValue", values_drop_na = TRUE) %>%
      dplyr::group_by(Stressor) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop_last")

    insuffSamples <- df_PairedStressResp.stats$Stressor[which(df_PairedStressResp.stats$n < samplim)]

    df_PairedStressResp <- df_PairedStressResp %>%
      dplyr::select(!all_of(insuffSamples))

    rm(df_PairedStressResp.stats)

    # Write these to data gaps file
    if (length(insuffSamples) == 0) {
      for (i in seq_along(insuffSamples)) {
        str <- insuffSamples[i]
        msg <- paste0("Insufficient numbers of samples are available for ", str, ".")
        message(msg)

        # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        gapcomment <- "This stressor will not be evaluated."
        gaps       <- cbind.data.frame("samplim comparison", str,
                                       paste0("<", samplim), gapcomment)

        fn.gaps    <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps    <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")
      } #End loop over stressors
    } #End if

    msg <- paste0("getQualSites is complete for ", bioComm, ".")
    message(msg)

    ## 18, getCoOccur ####
    # Progress, 21
    if (boo_Shiny == TRUE) {
      prog_det <- "getCoOccur"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get Co-occurrence from comparator samples with not degraded samples
    # Answers the question: "Are the observed stressor levels consistent
    # with impairment where and when it occurs"?
    if (TargetSiteID %in% unique(data_bioCoOccur$StationID)) {
      msg <- "Starting Co-occurrence"
      message(msg)

      list.StressorMetaData <- getCoOccur(TargetSiteID  = TargetSiteID,
                                          df_data       = df_PairedStressResp,
                                          detects       = siteDetectsAll,
                                          df_stressinfo = data_stressInfo,
                                          compsites     = list.CompSites$comp.sites,
                                          biocomm       = bioComm,
                                          colBio        = bioIndexGp,
                                          pHlimLow      = pHlimLow,
                                          pHlimHigh     = pHlimHigh,
                                          DOlim         = DOlim,
                                          plotvars      = data_plotvars,
                                          plotdpi       = plot_dpi,
                                          plotH         = plot_H,
                                          plotW         = plot_W,
                                          plotunits     = plot_units,
                                          dir_plots     = dir_results,
                                          dir_sub       = "_WoE",
                                          boo_plot      = boo.plot.user)

      df_stressorMetadata <- list.StressorMetaData$df_stressorMetadata
      notEvaluated <- c(insuffSamples, list.StressorMetaData$notEvaluated)
      df_COscores  <- list.StressorMetaData$df_COscores
    }

    if (nrow(df_stressorMetadata) == 0) {
      msg <- paste0("No candidate causes to evaluate further for ",
                    TargetSiteID, " for the ", bioComm, " community.")
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      gaps    <- cbind.data.frame("getCoOccur", msg, 0, gapcomment)
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")
      next
    }

    if (nrow(df_COscores) != 0) {
      df_LoE <- df_COscores
    }
    rm(df_COscores, list.StressorMetaData)

    msg <- paste0("getCoOccur for ", bioComm, " is complete.")
    message(msg)

    ## 19, getTimeSeq ####
    # Progress, 20
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
    df_TS_scores <- getTimeSeq(TargetSiteID,
                               biocomm       = bioComm,
                               bioindex      = bioIndexGp,
                               df_stress     = data_Stress,
                               df_resp       = bioMetricData[bioMetricData$StationID == TargetSiteID, ],
                               df_respinfo   = bioMetricInfo,
                               df_stressinfo = df_stressorMetadata,
                               df_paired     = df_PairedStressResp,
                               plotdpi       = plot_dpi,
                               plotH         = plot_H,
                               plotW         = plot_W,
                               plotunits     = plot_units,
                               dir_results   = dir_results,
                               dir_sub       = "_WoE",
                               boo_plot      = boo.plot.user)
    # TODO: why are there missing values or values outside the scale range in getTimeSeq?
    # Getting many warnings (45 for site WAM06600_000586)

    if (nrow(df_TS_scores) != 0) {
      df_LoE <- rbind(df_LoE, df_TS_scores)
    }
    rm(df_TS_scores)

    msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
    message(msg)

    ## 20, getSufficiency ####
    # Progress, 24
    if (boo_Shiny == TRUE) {
      prog_det <- "getSufficiency"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get stressors sufficient to cause biological impairment using all comparator samples
    msg <- "Starting Sufficiency"
    message(msg)

    df_SuffScores <- getSufficiency(TargetSiteID  = TargetSiteID,
                                    df_data       = df_PairedStressResp,
                                    compSites     = list.CompSites$comp.sites,
                                    df_stressinfo = df_stressorMetadata,
                                    biocomm       = bioComm,
                                    colBio        = bioIndexGp,
                                    plotvars      = data_plotvars,
                                    plotdpi       = plot_dpi,
                                    plotH         = plot_H,
                                    plotW         = plot_W,
                                    plotunits     = plot_units,
                                    dir_plots     = dir_results,
                                    dir_sub       = "_WoE",
                                    boo_plot      = boo.plot.user)

    if (nrow(df_SuffScores) != 0) {
      df_LoE <- rbind(df_LoE, df_SuffScores)
    }
    rm(df_SuffScores)

    msg <- paste0("getSufficiency for ", bioComm, " is complete.")
    message(msg)

    ## 21, getBioStressorResponses ####
    # Progress, 25
    if (boo_Shiny == TRUE) {
      prog_det <- "getBioStressorResponses"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    # Get Stressor Responses inside (comparators) and outside (all) the case
    df_gradscores <- getBioStressorResponses(TargetSiteID  = TargetSiteID,
                                             df_stressinfo = df_stressorMetadata,
                                             df_respinfo   = bioMetricInfo,
                                             df_respdata   = bioMetricData,
                                             df_datapaired = df_PairedStressResp,
                                             biocomm       = bioComm,
                                             bioindex      = bioIndexGp,
                                             min_cases     = samplim,
                                             p.val_cutoff  = p.val_cutoff,
                                             r2_cutoff     = r2_cutoff,
                                             plotvars      = data_plotvars,
                                             refOutline    = refOutline_col,
                                             plotdpi       = plot_dpi,
                                             plotH         = plot_H,
                                             plotW         = plot_W,
                                             plotunits     = plot_units,
                                             dir_plots     = dir_results,
                                             dir_sub       = "_WoE",
                                             boo_pred_warn = TRUE,
                                             boo_plot      = boo.plot.user)

    if (nrow(df_gradscores != 0)) {
      df_LoE <- rbind(df_LoE, df_gradscores)
    }
    rm(df_gradscores)

    msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
    message(msg)

    ## 22, getVerifiedPredictions ####
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
    sstv.name      <- paste0("SSTVname.", bioComm)
    stressors.sstv <- as.vector(unlist(df_stressorMetadata$Stressor[!is.na(df_stressorMetadata[[sstv.name]])]))
    stressors.ssi  <- as.vector(unlist(df_stressorMetadata$Stressor[!is.na(df_stressorMetadata$SSIndex)]))

    if (length(stressors.ssi) == 0 & length(stressors.sstv) == 0) {

      msg <- "No site stressors have stressor-specific tolerance values or stressor-specific indices."
      message(msg)
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      gaps    <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0, msg)
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")

    } else {

      if (length(stressors.sstv) > 0) { # one or more stressors.sstv

        df_VPscores <- getVerifiedPredictions(TargetSiteID   = TargetSiteID,
                                              stressors.sstv = stressors.sstv,
                                              df_stressinfo  = df_stressorMetadata,
                                              df_paired      = df_PairedStressResp,
                                              biocomm        = bioComm,
                                              df_bioTaxaData = bioTaxaData,
                                              df_MasterTaxa  = bioMasterTaxa,
                                              colBio         = bioIndexGp,
                                              plotvars       = data_plotvars,
                                              plotdpi        = plot_dpi,
                                              plotH          = plot_H,
                                              plotW          = plot_W,
                                              plotunits      = plot_units,
                                              dir_plots      = dir_results,
                                              dir_sub        = "_WoE",
                                              boo_plot       = boo.plot.user)

        if (nrow(df_VPscores)!= 0) { # LCN changed 20250917
          df_LoE <- rbind(df_LoE, df_VPscores)
        }
        rm(df_VPscores)

      } else { # no sstvs

        msg <- "No site stressors have stressor specific tolerance values"
        message(msg)

        # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        gaps    <- cbind.data.frame("getVerifiedPredictions", TargetSiteID, 0, msg)
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")

      }

      if (length(stressors.ssi) > 0) { # one or more stressors.ssi

        df_VPSSIscores <- getVPSSI(TargetSiteID     = TargetSiteID,
                                   stressors.ssi    = stressors.ssi,
                                   df_stressinfo    = df_stressorMetadata,
                                   df_paired        = df_PairedStressResp,
                                   biocomm          = bioComm,
                                   df_bioMetricData = bioMetricData,
                                   df_bioMetricInfo = bioMetricInfo,
                                   colBio           = bioIndexGp,
                                   plotvars         = data_plotvars,
                                   plotdpi          = plot_dpi,
                                   plotH            = plot_H,
                                   plotW            = plot_W,
                                   plotunits        = plot_units,
                                   dir_plots        = dir_results,
                                   dir_sub          = "_WoE",
                                   boo_plot         = boo.plot.user) 

        if (nrow(df_VPSSIscores) != 0) { # LCN changed 20250917
          df_LoE <- rbind(df_LoE, df_VPSSIscores)
        }
        rm(df_VPSSIscores)

      } else { # no ssis

        msg <- "No site stressors have stressor specific indices"
        message(msg)

        # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
        gaps    <- cbind.data.frame("getVPSSIscores", TargetSiteID, 0, msg)
        fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
        fn.gaps <- file.path(dir_results,TargetSiteID,fn.gaps)
        write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                    row.names = FALSE, sep = "\t")

      }

    } ### End getVP evaluation

    msg <- paste0("getVerifiedPredictions for ", bioComm, " is complete.")
    message(msg)

    ## 23, getWOE ####
    # Progress, 27
    if (boo_Shiny == TRUE) { # needs updating
      prog_det <- "getWOE"
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(mySleepTime)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ boo_Shiny ~ END

    getWoE(TargetSiteID = TargetSiteID,
           biocomm      = bioComm,
           dfLoE        = df_LoE,
           dfStress     = df_stressorMetadata,
           dir_results  = dir_results,
           dir_WoE      = "_WoE")
    msg <- paste0("getWoE for ", bioComm, " is complete.")
    message(msg)

  } ### End biocomm loop
  ## FOR ~ b ~ END ####

  ## 24, getReport ####
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
    report_type <- "preliminary"
  } else {
    report_type <- "summary"
  }

  getReport(TargetSiteID,
            biocomms,
            primeIndex     = bmiIndex,
            removeOutliers = removeOutliers,
            samplim        = samplim,
            r2_cutoff      = r2_cutoff,
            p.val_cutoff   = p.val_cutoff,
            useBC          = useBC,
            lagdays        = lagdays,
            DOlim          = DOlim,
            pHlimLow       = pHlimLow,
            pHlimHigh      = pHlimHigh,
            bmiIndex       = bmiIndex,
            algIndex       = algIndex,
            fishIndex      = fishIndex,
            useBMI         = useBMI,
            useAlg         = useAlg,
            useFish        = useFish,
            dir_data       = dir_data,
            dir_results    = dir_results,
            report_type    = "full",
            report_format  = "html",
            dir_rmd        = dir_rmd)

  dfGaps <- read.table(file.path(dir_results, TargetSiteID,
                                 paste0(TargetSiteID,"_datagaps.tab")),
                       header = TRUE, sep = "\t")
  dfGaps <- unique(dfGaps)
  write.table(dfGaps, file.path(dir_results, TargetSiteID,
                                paste0(TargetSiteID,"_datagaps.tab")),
              append = FALSE, col.names = TRUE, row.names = FALSE,
              sep = "\t")

} ### End TargetSite loop # not used in Shiny
# FOR ~ site ~ END ####

rm(site)

# 25, getSummaryAllSites ####
# Progress, 29
if (boo_Shiny == TRUE) {
  prog_det <- "getSummaryAllSites"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(mySleepTime)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ boo_Shiny ~ END

# getSummaryAllSites(biocommlist = biocommlist,
#                    bmiIndex    = bmiIndex,
#                    algIndex    = NULL,
#                    fishIndex   = NULL,
#                    dir_data    = dir_data,
#                    dir_results = dir_results,
#                    dir_sub     = "WoE",
#                    df_sites    = NULL)

# msg <- "getSummaryAllSites is complete."
# message(msg)

# rm(list=ls())

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, END ####
# external/RPPTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
