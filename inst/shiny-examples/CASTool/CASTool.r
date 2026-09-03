# Copyright 2025 TetraTech. All rights reserved.
# Use, copying, modification, or distribution of this file or any of its contents
# is expressly prohibited without prior written permission of TetraTech.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R v4.4.3
#
# CASTfxn
# Erik.Leppo@tetratech.com, 20180710
# Ann.RoseberryLincoln@tetratech.com, 20230605
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Shiny code for use in Shiny App
# 2020-10-30, Erik
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2025-09-24, Erik, start mods for updated Shiny App
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2026-04-07, Laura, clean up
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2026-07-27, Laura, additional clean up
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, Start ####
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

tic <- Sys.time()

# 01, Set up ####
## Define global variables ####
cfg <- setGlobals(
  boo_shiny = TRUE, # Whether to run the code in Shiny mode (set to FALSE if running script outside of the app)
  boo_debug = FALSE # Whether to run the code in debug mode
)

## R console program set up ####
if(cfg$boo_shiny == FALSE) {

  cfg <- setUpRConsole(
      # File path of data directory
      in_dir = "C:/Users/lnaslund/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/3-projects/14-biocriteria/Bioindicator_Workshop/CASTool Output",
      # File path of results directory
      out_dir = "C:/Users/lnaslund/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/3-projects/14-biocriteria/Bioindicator_Workshop/CASTool Results",
      region = "Rhode Island"
    ) |>
    append(cfg, values = _)
}

## Shiny set up----
if (cfg$boo_shiny == TRUE) {

  dir_rmd     <- file.path(system.file(package = "CASTfxn"), "rmd")
  wd          <- getwd()
  dir_data    <- dn_data
  dir_results <- dn_results

  data_CASTmeta_prog <- readRDS(file.path(dir_data, dn_checked_sk, "CASTmetadata.rds"))
  data_CASTmeta_prog <- data_CASTmeta_prog |>
    tidyr::pivot_wider(names_from = Variable, values_from = Value)

  biocommlist_prog <- data_CASTmeta_prog |>
    dplyr::pull(biocommlist) |>
    stringr::str_split(", |,")  |>
    unlist()
  n_biocomm_prog <- length(biocommlist_prog)

  cfg[["in_dir"]] <- dir_data
  cfg[["out_dir"]] <- dir_results
  cfg[["region"]] <- data_CASTmeta_prog$region

  prog_cnt <- 0

  # Number of increments
  prog_n <- 16 + (7 * n_biocomm_prog)
  prog_sleep <- 0.25

  prog_det <- "Set up output structure"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
} # IF ~ cfg$boo_shiny ~ END


#~~~~~~~~~~~~~~~~~~~~~~~
# 02, Check inputs ####
# Progress, 02

if (cfg$boo_shiny == TRUE) {
  prog_det <- "Check input data files"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
} else {
  list.Tables <- checkInputs(dir_uploaded = cfg$in_dir,
                             dir_out = cfg$out_dir)

  write.csv(list.Tables$TableOne, file.path(cfg$out_dir, cfg$region, "TableOne.csv"), row.names = FALSE)
  write.csv(list.Tables$TableTwo, file.path(cfg$out_dir, cfg$region, "TableTwo.csv"), row.names = FALSE)
}## IF ~ cfg$boo_shiny ~ END

#~~~~~~~~~~~~~~~~~~~~~~~
# 03, Get metadata params ####
# Progress, 03
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Pull values from metadata"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END

cfg$out_dir <- file.path(cfg$out_dir, cfg$region)

meta <- getMetadataParams(config = cfg)

#~~~~~~~~~~~~~~~~~~~~~~~
# 04, Site data files ####
# Progress, 04
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Load site data"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END

list.SiteData <- prepSiteData(outcaseLabel = meta$outcaseLabel,
                              incaseColName = meta$incaseColName,
                              outcaseColName = meta$outcaseColName,
                              config = cfg)

#~~~~~~~~~~~~~~~~~~~~~~~
# 05, Measured data and metadata ####
# Progress, 05
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Load measured stressor data"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END

if (isTRUE(meta$boo_meas)) {
  list.measStress   <- prepMeasStressorData(fn_data = "data_chemAll.rds",
                                            fn_meta = "data_chemInfo.rds",
                                            sub_dir = "_Histograms",
                                            removeOutliers = meta$removeOutliers,
                                            config = cfg)
} else {list.measStress <- NULL}

#~~~~~~~~~~~~~~~~~~~~~~~
# 06, Modeled data and metadata ####
# Progress, 06
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Load modeled stressor data"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END

if (isTRUE(meta$boo_model)) {
  list.modStress     <- prepModStressorData(fn_data = "data_modelAll.rds",
                                            fn_meta = "data_modelInfo.rds",
                                            sub_dir = "_Histograms",
                                            removeOutliers = meta$removeOutliers,
                                            config = cfg)
} else {list.modStress <- NULL}

#~~~~~~~~~~~~~~~~~~~~~~~
# 07, Combine stressor data ####
# Progress, 07
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Combine stressor data and metadata"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END

list.Stress <- combineStressData(meas_stress = list.measStress,
                                 mod_stress = list.modStress,
                                 boo_meas = meta$boo_meas,
                                 boo_model = meta$boo_model,
                                 boo_outliers = meta$removeOutliers)

rm(list.measStress, list.modStress)

# 08, Get WS stressor data ####
list.WSStress <- getWSStressData(boo_ws = meta$exploreWSStressor,
                                 boo_helper = meta$helperImport,
                                 config = cfg)

# 09, Bio response data ####
data_respTrim <- data.frame()

responsesOutput <- data.frame(MetricName = character(),
                              MetricLabel = character(),
                              BioComm = character(),
                              IndexYN = character())
list.bmiData <- list()
list.algData <- list()
list.fishData <- list()

for (b in seq_along(meta$biocommlist)) {
  bio <- meta$biocommlist[b]
  #~~~~~~~~~~~~~~~~~~~~~~~

  if (cfg$boo_shiny == TRUE) {
    prog_det <- paste0("Load ", bio, ", response data")
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ cfg$boo_shiny ~ END

  message(paste0("Reading ", bio, " data files"))
  meta[[paste0("boo_", bio)]] <- TRUE

  temp <- prepRespData(bio      = bio,
                       loaded   = meta$loaded,
                       bioIndex = meta[[paste0(bio, "IndexGp")]],
                       calcRelAbund = meta$calcRelAbund,
                       config   = cfg)

  assign(paste0("list.", bio, "Data"), temp)

  responsesOutput <- responsesOutput |>
    dplyr::bind_rows(get(paste0("list.", bio, "Data"))[["data_bioMetricsInfo"]] |>
                       dplyr::select(MetricName, MetricLabel, IndexYN) |>
                       dplyr::mutate(BioComm = bio))

  ## Perform stressor-response observation matching
  temp <- getCoOccurDataset(df_sites  = list.SiteData$site,
                            df_stress = list.Stress$data_stress,
                            biocomm   = bio,
                            df_resp   = get(paste0("list.", bio, "Data"))[["data_bioMetrics"]],
                            index     = meta[[paste0(bio, "IndexGp")]],
                            lagdays   = meta$lagdays)

  assign(paste0("data_", bio, "CoOccur"), temp)

  ## Generate summary of biological response samples for later use in getAllSamplesTable
  data_respTrim <- rbind(data_respTrim,
                         temp[, c("StationID",
                                  "RespSampleID",
                                  "RespSampleDate",
                                  "BioComm")] |>
                           dplyr::mutate(biocomm = paste0(toupper(bio), "SampleID"))) # TODO change this name if possible

  rm(temp)
}## FOR ~ b

#~~~~~~~~~~~~~~~~~~~~~~~
# 10, Sample summary ####
# Progress, 11
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Generate summary of sample types"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END

data_sampSummary <- getAllSamplesTable(df_stress     = list.Stress$data_stress,
                                       df_stressInfo = list.Stress$data_stressInfo,
                                       df_resp       = data_respTrim,
                                       df_sites      = list.SiteData$site,
                                       incaseColName = meta$incaseColName)
rm(data_respTrim)
# Data prep completed
#~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~
# RUN CASTool ####
# 11, Target site selection ####
# Progress, 12
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Select target site"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END
#
df_targets <- readRDS(file.path(cfg$out_dir, cfg$dn_checked_sk, "df_targets.rds"))

### Evaluate each target site
## Use this for debugging
if (cfg$boo_shiny == TRUE) {
  df_targets <- data.frame("TargetSiteID" = input$si_checked_sites_targ,
                           "Chosen by" = NA,
                           "Comment" = NA)
  names(df_targets)[2] <- "Chosen by"
  # ok since Shiny only works on 1 sites
}

#~~~~~~~~~~~~~~~~~~~~~~~
# 12, Main Code ####
# Progress, 13
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Check target site data availability"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END

status_df <- data.frame(TargetSiteID = character(),
                        status = character(),
                        reason = character())

# Site loop ####
for (site in seq_len(nrow(df_targets))) {
  TargetSiteID <- df_targets$TargetSiteID[site]
  dir_results <- cfg$out_dir

  ## Check TargetSiteID
  temp_status <- checkTargetSite(TargetSiteID = TargetSiteID,
                                 SiteData = list.SiteData$sites)

  if(nrow(temp_status)!= 0){
    status_df <- rbind(status_df, temp_status)
    next
  }

  msg <- paste0("Evaluating site: ", TargetSiteID)
  message(msg)

  rm(temp_status)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Biocomm-independent functions ####

  # Create high-level results folder structure
  #dir_results <- out.dir
  dir_sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(dir_results, dir_sub2)) == TRUE,
         dir.create(file.path(dir_results, dir_sub2)), FALSE)

  # Define datagaps data frame ####
  gaps    <- data.frame(fxnname = character(),
                        condition = character(),
                        result = character(),
                        comment = character())

  # Initialize stressor elimination data frame
  df_stressorElim <- data.frame(
    Stressor = character(),
    Biocomm = character(),
    Reason = character()
  )

  # 13, getComparators ####
  ## Progress, 14
  if (cfg$boo_shiny == TRUE) {
    prog_det <- "Get comparator site data"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ cfg$boo_shiny ~ END

  compSitesList <- list()

  for(b in seq_along(meta$biocommlist)){
    bio <- tolower(meta$biocommlist[b])

    temp_list <- getComparators(TargetSiteID = TargetSiteID,
                                df_sites =list.SiteData$site,
                                df_cluster =list.SiteData$cluster,
                                df_bioCoOccur = get(paste0("data_", bio, "CoOccur")),
                                bioIndex = meta[[paste0(bio, "IndexGp")]],
                                bio = bio,
                                outcaseColName = meta$outcaseColName,
                                outcaseLabel = meta$outcaseLabel,
                                incaseColName = meta$incaseColName,
                                incaseLabel = meta$incaseLabel,
                                useAllCompReaches = meta$useAllCompReaches,
                                dir_results = dir_results,
                                dir_sub = "SiteInfo",
                                config = cfg)

    assign(paste0("list.CompSites.", bio), temp_list)
    rm(temp_list)

    compSitesList[[bio]] <- get(paste0("list.CompSites.", bio))
  }

  ## Choose first biocomm list for purposes of plotting non-biological community specific figures (e.g., map)
  ## Shouldn't matter which selected unless potentially useBC = TRUE
  list.CompSites <- compSitesList[[1]]
  rm(compSitesList)

  msg <- "getComparators is complete."
  message(msg)

  # 14, getSiteInfo, getSiteMap, writeOutliers ####
  # Progress, 15
  if (cfg$boo_shiny == TRUE) {
    prog_det <- "Generate index boxplots and create site map"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ cfg$boo_shiny ~ END
  # Get site information for general use (map, sample summary, etc)

  # Create site info folder with watershed-scale stressor boxplots,
  # boxplots for bio indices, and folder for photos
  list.SiteInfo <- getSiteInfo(TargetSiteID   = TargetSiteID,
                              TargetCOMID    = list.CompSites$TargetCOMID,
                              df_Sites       = list.SiteData$site,
                              df_SampSummary = data_sampSummary,
                              biocommlist    = meta$biocommlist,
                              df_BMIMetrics  = list.bmiData$data_bioMetrics,
                              df_ALGMetrics  = list.algData$data_bioMetrics,
                              df_FishMetrics = list.fishData$data_bioMetrics,
                              comp.sites     = list.CompSites$comp.sites,
                              all.sites      = list.CompSites$all.sites,
                              BMIIndexGp     = meta$bmiIndexGp,
                              ALGIndexGp     = meta$algIndexGp,
                              FishIndexGp    = meta$fishIndexGp,
                              IncaseLabel    = meta$incaseLabel,
                              OutcaseLabel   = meta$outcaseLabel,
                              config         = cfg,
                              dir_sub        = "SiteInfo")

  if (meta$exploreWSStressor) {
    list.WSStressorFigs <- getWSStressorFigs(TargetSiteID      = TargetSiteID,
                      df_WSData         = list.WSStress$data_stressorWS,
                      df_WSInfo         = list.WSStress$data_stressorinfoWS,
                      df_Sites          = list.SiteData$site,
                      comp.reaches      = list.CompSites$comp.reaches,
                      TargetCOMID       = list.CompSites$TargetCOMID,
                      biocommlist       = meta$biocommlist,
                      useAllCompReaches = meta$useAllCompReaches,
                      dir_sub           = "SiteInfo",
                      df_SampSummary    = data_sampSummary,
                      config = cfg)
  }


  # Create site map
  boundary <- readRDS(file.path(cfg$out_dir, cfg$dn_checked_sk, "boundary.rds"))
  reaches <- readRDS(file.path(cfg$out_dir, cfg$dn_checked_sk, "reaches.rds")) |>
    dplyr::mutate(COMID = as.character(COMID))
  flowline <- reaches |>
    dplyr::left_join(list.SiteData$cluster |>
                       dplyr::mutate(COMID = as.character(COMID),
                              ClusterID = as.factor(ClusterID)),
                     by = "COMID")

  getSiteMap(sp_outline   = boundary,
             sp_flowline  = flowline,
             config       = cfg,
             datum        = meta$datum,
             df_sites     = list.SiteData$site,
             allSites     = list.CompSites$all.sites,
             compSites    = list.CompSites$comp.sites,
             TargetSiteID = TargetSiteID,
             dir_results  = dir_results,
             dir_sub      = "SiteInfo",
             dir_map_rmd  = file.path(system.file(package = "CASTfxn"), "rmd"))

  rm(boundary, reaches, flowline)

  msg <- "getSiteInfo, getWSstressorFigs, and getSiteMap are complete."
  message(msg)

  # 15, getAvailableDataTypes ####
  # Progress, 17
  if (cfg$boo_shiny == TRUE) {
    prog_det <- "Identify outliers"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ cfg$boo_shiny ~ END
  #
  # Prepare flags for types of stressor and response data to use
  list.AvailData <- getAvailableDataTypes(TargetSiteID   = TargetSiteID,
                                          df_stress      = list.Stress$data_stress,
                                          df_SampSummary = data_sampSummary,
                                          biocommlist    = meta$biocommlist,
                                          dir_results    = dir_results)

  list.StressorElim <- getInitialStressors(stress_info    = list.Stress$data_stressInfo,
                                           stress_data    = list.Stress$data_stress)

  list.StressorElim <-  reportNoStressResponse(TargetSiteID = TargetSiteID,
                                               avail_list = list.AvailData)|>
    append(list.StressorElim, values = _)

  # If there is no stressor or response data
  if(is.null(list.StressorElim$msg_no_sr)==FALSE){
    temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID),
                              status = "Failed",
                              reason = list.StressorElim$msg_no_sr)
    status_df <- status_df |> dplyr::bind_rows(temp_status)
    next
  }

  # Write target site outliers, comparator site outliers (inside the case),
  # and all outliers (outside the case)
  list.Outliers <- writeOutliers(TargetSiteID  = TargetSiteID,
                df_outliers   = list.Stress$data_stressOutliers,
                df_stressInfo = list.Stress$data_stressInfo,
                df_Sites      = list.SiteData$site,
                siteDetectsAll= list.AvailData$siteDetectsAll,
                compSites     = list.CompSites$comp.sites,
                allSites      = list.CompSites$all.sites,
                dir_results   = dir_results)

  msg <- "getAvailableDataTypes and writeOutliers are complete."
  message(msg)

  # Biocomm loop ####
  for (b in seq_along(meta$biocommlist)) {

    # Define biocomm data
    bioComm <- tolower(meta$biocommlist[b])

    if(list.AvailData[[paste0("use_", bioComm)]]){
      currentDataList <- get(paste0("list.", bioComm, "Data"))
      data_bioCoOccur <- get(paste0("data_", bioComm, "CoOccur"))
      bioIndexGp      <- meta[[paste0(bioComm, "IndexGp")]]
      list.CompSites <- get(paste0("list.CompSites.", bioComm))
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

    # Evaluate if site has no stressor-response data
    list.StressorElim <- reportNoSiteStressResponse(TargetSiteID = TargetSiteID,
                                                    avail_list = list.AvailData,
                                                    data_bioCoOccur = data_bioCoOccur,
                                                    lagdays = meta$lagdays,
                                                    bioComm = bioComm) |>
      append(list.StressorElim, values = _)

    if(is.null(list.StressorElim$msg_no_site_sr)==FALSE){
      next
    }

    # Create dataframe with paired stressor response data at target site
    data_target <- dplyr::filter(data_bioCoOccur, StationID == TargetSiteID)

    # Evaluate stressors not measured at target site
    list.StressorElim <- getTargetNotMeas(stress_elim = list.StressorElim,
                                          data_target = data_target,
                                          bioComm = bioComm) |>
      append(list.StressorElim, values = _)

    df_stressorElim <- df_stressorElim |>
      dplyr::bind_rows(list.StressorElim$df_notMeasElim)

    ## 16, getQualSites ####
    # Progress, 18
    if (cfg$boo_shiny == TRUE) {
      prog_det <- paste0(bioComm, "; summarize index values")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ cfg$boo_shiny ~ END

    # Run analyses
     list.QualSites <- getQualSites(TargetSiteID = TargetSiteID,
                                        biocomm      = bioComm,
                                        df_qual      = data_bioCoOccur,
                                        colBio       = bioIndexGp,
                                        refSites     = list.SiteData$refSite,
                                        compSites    = list.CompSites$comp.sites, # inside the case
                                        allSites     = list.CompSites$all.sites, # outside the case
                                        stressors = list.StressorElim$targMeasStress,
                                        dir_results  = dir_results,
                                        dir_sub      = "SiteInfo")

    df_PairedStressResp <- list.QualSites$df_qual

    # Remove any stressors with fewer than samplim comparator samples
    list.StressorElim <- reportInsuffStress(df_PairedStressResp = df_PairedStressResp,
                                           list.CompSites = list.CompSites,
                                           bioIndexGp = bioIndexGp,
                                           stress_elim = list.StressorElim,
                                           samplim = meta$samplim,
                                           bioComm = bioComm,
                                           targMeasStress = list.StressorElim$targMeasStress) |>
    append(list.StressorElim, values = _)

    df_PairedStressResp <- df_PairedStressResp |>
      dplyr::select(!all_of(list.StressorElim$insuffSamples))

    df_stressorElim <- df_stressorElim |>
             dplyr::bind_rows(list.StressorElim$df_suffElim)

    priorElim <- df_stressorElim |>
      dplyr::filter(Biocomm == bioComm) |>
      dplyr::distinct(Stressor) |>
      dplyr::pull(Stressor)

    list.StressorElim$stressorMeasSuff <- setdiff(list.StressorElim$initialStress, priorElim)

    rm(priorElim)

    # Exit loop if no remaining stressors
    if(length(list.StressorElim$stressorMeasSuff) == 0){
      temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID),
                                status = "Failed",
                                reason = "No stressors remaining after removing stressors with no paired stressor-response samples at the target site and stressors with insufficient number of samples at comparator sites. ")
      status_df <- status_df |> dplyr::bind_rows(temp_status)

      next
    }

    msg <- paste0("getQualSites is complete for ", bioComm, ".")
    message(msg)

    ## 17, getCoOccur ####
    # Progress, 21
    if (cfg$boo_shiny == TRUE) {
      prog_det <-  paste0(bioComm, "; run co-occurrence line of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ cfg$boo_shiny ~ END

    # Get Co-occurrence from comparator samples with not degraded samples
      msg <- "Starting Co-occurrence"
      message(msg)

      list.StressorMetaData <- getCoOccur(TargetSiteID  = TargetSiteID,
                                          df_data       = df_PairedStressResp,
                                          detects       = list.StressorElim$stressorMeasSuff,
                                          df_stressinfo = list.Stress$data_stressInfo,
                                          compsites     = list.CompSites$comp.sites,
                                          biocomm       = bioComm,
                                          colBio        = bioIndexGp,
                                          config        = cfg,
                                          pHlimLow      = meta$pHlimLow,
                                          pHlimHigh     = meta$pHlimHigh,
                                          DOlim         = meta$DOlim,
                                          incaseLabel   = meta$incaseLabel,
                                          targetSampleLabels = meta$targetSampleLabels,
                                          dir_plots     = dir_results,
                                          dir_sub       = "_WoE")

      list.StressorElim$coOccurElim <- list.StressorMetaData$notEvaluated

      if(length(list.StressorMetaData$notEvaluated)>0){
        tempElim <- data.frame(Stressor = list.StressorMetaData$notEvaluated,
                               Biocomm = bioComm,
                               Reason = "Co-occurrence")

        df_stressorElim <- df_stressorElim |>
          dplyr::bind_rows(tempElim)

        rm(tempElim)
      }


    if (nrow(list.StressorMetaData$df_stressorMetadata) == 0) {
      msg <- paste0("No candidate causes to evaluate further for ",
                    TargetSiteID, " for the ", bioComm, " community.")
      message(msg)

      # No identified stressors may be a data gap, but may not be, either
      gapcomment <- paste0(bioComm, ": All candidate causes were eliminated by the co-occurrence line of evidence for ",
                           TargetSiteID)

      gap.statement <- data.frame(
        fxnname = "getCoOccur",
        condition = msg,
        result = as.character(0),
        comment = gapcomment)

      gaps <- gaps |>
        dplyr::bind_rows(gap.statement)

      temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID),
                                status = "Failed",
                                reason = "No stressors remaining after removing stressors with no paired stressor-response samples at the target site,  stressors with insufficient number of samples at comparator sites, and co-occurrence screening. ")
      status_df <- status_df |> dplyr::bind_rows(temp_status)

      next
    }

    if (nrow(list.StressorMetaData$df_COscores) != 0) {
      df_LoE <- list.StressorMetaData$df_COscores
    }

    msg <- paste0("getCoOccur for ", bioComm, " is complete.")
    message(msg)

    ## 18, getTimeSeq ####
    # Progress, 20
    if (cfg$boo_shiny == TRUE) {
      prog_det <- paste0(bioComm, "; run time sequence line of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ cfg$boo_shiny ~ END
    #
    # Create time sequence graphics for ONLY target site
    # Uses all site stressor and response data, but not necessarily paired
    df_TS_scores <- getTimeSeq(TargetSiteID,
                               biocomm       = bioComm,
                               bioindex      = bioIndexGp,
                               df_stress     = list.Stress$data_stress,
                               df_resp       = currentDataList$data_bioMetrics[currentDataList$data_bioMetrics$StationID == TargetSiteID, ],
                               df_respinfo   = currentDataList$data_bioMetricsInfo,
                               df_stressinfo = list.StressorMetaData$df_stressorMetadata,
                               df_paired     = df_PairedStressResp,
                               dir_results   = dir_results,
                               dir_sub       = "_WoE")

    if (nrow(df_TS_scores) != 0) {
      df_LoE <- rbind(df_LoE, df_TS_scores)
    }
    rm(df_TS_scores)

    msg <- paste0("getTimeSeq for ", bioComm, " is complete.")
    message(msg)

    ## 19, getSufficiency ####
    # Progress, 24
    if (cfg$boo_shiny == TRUE) {
      prog_det <-  paste0(bioComm, "; run sufficiency line of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ cfg$boo_shiny ~ END

    # Get stressors sufficient to cause biological impairment using all comparator samples
    msg <- "Starting Sufficiency"
    message(msg)

    df_SuffScores <- getSufficiency(TargetSiteID  = TargetSiteID,
                                    df_data       = df_PairedStressResp,
                                    compSites     = list.CompSites$comp.sites,
                                    df_stressinfo = list.StressorMetaData$df_stressorMetadata,
                                    biocomm       = bioComm,
                                    colBio        = bioIndexGp,
                                    config        = cfg,
                                    dir_plots     = dir_results,
                                    dir_sub       = "_WoE",
                                    targetSampleLabels = meta$targetSampleLabels)

    if (nrow(df_SuffScores) != 0) {
      df_LoE <- rbind(df_LoE, df_SuffScores)
    }
    rm(df_SuffScores)

    msg <- paste0("getSufficiency for ", bioComm, " is complete.")
    message(msg)

    ## 20, getBioStressorResponses ####
    # Progress, 25
    if (cfg$boo_shiny == TRUE) {
      prog_det <-  paste0(bioComm, "; run biological gradient line of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ cfg$boo_shiny ~ END

    # Get Stressor Responses inside (comparators) and outside (all) the case
    list.BioStressorResponses <- getBioStressorResponses(TargetSiteID  = TargetSiteID,
                                             df_stressinfo = list.StressorMetaData$df_stressorMetadata,
                                             df_respinfo   = currentDataList$data_bioMetricsInfo, #bioMetricInfo,
                                             df_respdata   = currentDataList$data_bioMetrics, #bioMetricData,
                                             df_datapaired = df_PairedStressResp,
                                             config        = cfg,
                                             biocomm       = bioComm,
                                             bioindex      = bioIndexGp,
                                             min_cases     = meta$samplim,
                                             p.val_cutoff  = meta$p.val_cutoff,
                                             r2_cutoff     = meta$r2_cutoff,
                                             dir_plots     = dir_results,
                                             dir_sub       = "_WoE",
                                             boo_pred_warn = TRUE,
                                             targetSampleLabels = meta$targetSampleLabels)

    df_gradscores <- list.BioStressorResponses$df.scores

    if (nrow(df_gradscores != 0)) {
      df_LoE <- rbind(df_LoE, df_gradscores)
    }
    rm(df_gradscores)

    msg <- paste0("getBioStressorResponses for ", bioComm, " is complete.")
    message(msg)

    ## 21, getVerifiedPredictions ####
    # Progress, 26
    if (cfg$boo_shiny == TRUE) {
      prog_det <-  paste0(bioComm, "; verified predictions lines of evidence")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ cfg$boo_shiny ~ END
    #
    # Get Stressor-specific regressions using comparator sites
    sstv.name      <- paste0("SSTVname.", bioComm)

    stressors.sstv <- list.StressorMetaData$df_stressorMetadata |> # I think you could add split on delimiter here
      dplyr::filter(!is.na(.data[[sstv.name]])) |>
      dplyr::pull(Stressor)

    stressors.ssi  <- list.StressorMetaData$df_stressorMetadata |>
      dplyr::filter(!is.na(SSIndex)) |>
      dplyr::pull(Stressor)

    if (length(stressors.ssi) != 0 | length(stressors.sstv) != 0) {

      if (length(stressors.sstv) > 0) { # one or more stressors.sstv

        list.VerifiedPredictions <- getVerifiedPredictions(TargetSiteID   = TargetSiteID,
                                              stressors.sstv = stressors.sstv,
                                              df_stressinfo  = list.StressorMetaData$df_stressorMetadata,
                                              df_paired      = df_PairedStressResp,
                                              biocomm        = bioComm,
                                              df_bioTaxaData = currentDataList$data_bioCounts,
                                              df_MasterTaxa  = currentDataList$data_bioMasterTaxa,
                                              colBio         = bioIndexGp,
                                              config         = cfg,
                                              dir_plots      = dir_results,
                                              dir_sub        = "_WoE",
                                              targetSampleLabels = meta$targetSampleLabels)

        df_VPscores <- list.VerifiedPredictions$df.scores

        if (nrow(df_VPscores)!= 0) {
          df_LoE <- rbind(df_LoE, df_VPscores)
        }
        rm(df_VPscores)

      } else { # no sstvs

        msg <- "No site stressors have stressor-specific tolerance values"
        message(msg)

        gap.statement <- data.frame(
          fxnname = "getVerifiedPredictions",
          condition = TargetSiteID,
          result = "0",
          comment = msg
        )

        gaps <- gaps |>
          dplyr::bind_rows(gap.statement)

      }

      if (length(stressors.ssi) > 0) { # one or more stressors.ssi

        info.stress.ssi <- list.StressorMetaData$df_stressorMetadata |>
          dplyr::filter(Stressor %in% stressors.ssi) |>
          dplyr::select(Stressor, SSIndex, Label, LogTransf)

        info.ssi <- bioMetricInfo |>
          dplyr::filter(MetricName %in% unique(info.stress.ssi$SSIndex))

        if(nrow(info.ssi)==0){
          msg <- paste0(paste(unique(info.stress.ssi$SSIndex), collapse = ", "), " are listed as stressor-specific index in stressor metadata but are not found as a metric to be included in ", bioComm, " response metadata.")
          message(msg)

          gap.statement <- data.frame(
            fxnname = "getVPSSIscores",
            condition = "Missing stressor-specific index information",
            result = unique(info.stress.ssi$SSIndex),
            comment = msg
          )

          gaps <- gaps |>
            dplyr::bind_rows(gap.statement)

        } else{
          list.VPSSIscores <- getVPSSI(TargetSiteID     = TargetSiteID,
                                       stressors.ssi    = stressors.ssi,
                                       df_stressinfo    = list.StressorMetaData$df_stressorMetadata,
                                       df_paired        = df_PairedStressResp,
                                       biocomm          = bioComm,
                                       df_bioMetricData = currentDataList$data_bioMetrics,
                                       df_bioMetricInfo = currentDataList$data_bioMetricsInfo,
                                       colBio           = bioIndexGp,
                                       config           = cfg,
                                       dir_plots        = dir_results,
                                       dir_sub          = "_WoE",
                                       targetSampleLabels = meta$targetSampleLabels)

          df_VPSSIscores <- list.VPSSIscores$df.scores

          if (nrow(df_VPSSIscores) != 0) { # LCN changed 20250917
            df_LoE <- rbind(df_LoE, df_VPSSIscores)
          }
          rm(df_VPSSIscores)
        }



      } else { # no ssis

        msg <- "No site stressors have stressor-specific indices"
        message(msg)

        gap.statement <- data.frame(
          fxnname = "getVPSSIscores",
          condition = TargetSiteID,
          result = "0",
          comment = msg
        )

        gaps <- gaps |>
          dplyr::bind_rows(gap.statement)

      }

    } ### End getVP evaluation

    msg <- paste0("getVerifiedPredictions for ", bioComm, " is complete.")
    message(msg)

    ## 22, getWOE ####
    # Progress, 27
    if (cfg$boo_shiny == TRUE) { # needs updating
      prog_det <-  paste0(bioComm, "; get weight of evidence table")
      prog_cnt <- prog_cnt + 1
      prog_msg <- paste0("Step ", prog_cnt)
      prog_inc <- 1 / prog_n
      incProgress(prog_inc, message = prog_msg, detail = prog_det)
      Sys.sleep(prog_sleep)
      message(paste(prog_msg, prog_det, sep = "; "))
    }## IF ~ cfg$boo_shiny ~ END

    # LCN addition 3/10/26 to get rid of observations with only BioGrad scores
    df_LoE <- df_LoE |> dplyr::filter(is.na(StressorValue)==FALSE)

    getWoE(TargetSiteID = TargetSiteID,
           biocomm      = bioComm,
           dfLoE        = df_LoE,
           dfStress     = list.StressorMetaData$df_stressorMetadata,
           dir_results  = dir_results,
           config       = cfg,
           dir_WoE      = "_WoE")
    msg <- paste0("getWoE for ", bioComm, " is complete.")
    message(msg)

  } ### End biocomm loop
  ## FOR ~ b ~ END ####

  ## 23, getReport ####
  # Progress, 28
  if (cfg$boo_shiny == TRUE) {
    prog_det <- "Get report"
    prog_cnt <- prog_cnt + 1
    prog_msg <- paste0("Step ", prog_cnt)
    prog_inc <- 1 / prog_n
    incProgress(prog_inc, message = prog_msg, detail = prog_det)
    Sys.sleep(prog_sleep)
    message(paste(prog_msg, prog_det, sep = "; "))
  }## IF ~ cfg$boo_shiny ~ END
  #

  df_stressorElim <- df_stressorElim |>
    dplyr::left_join(list.Stress$data_stressInfo |>
                       dplyr::select(StdParamName, Label),
                     by = c("Stressor" = "StdParamName"))

  write.csv(df_stressorElim,
            file.path(dir_results,
                      TargetSiteID,
                      paste0(TargetSiteID,
                             "_StressorsEliminated.csv")),
            row.names = FALSE)

  write.csv(list.StressorElim$df_initialStress,
            file.path(dir_results,
                      TargetSiteID,
                      paste0(TargetSiteID,
                             "_InitialStressors.csv")),
            row.names = FALSE)

  if(exists("info.ssi")){
    responsesOutput <- responsesOutput |>
      dplyr::mutate(SSI = dplyr::if_else(MetricName %in% info.ssi$MetricName, "Y", "N"))
  } else{
    responsesOutput <- responsesOutput |>
      dplyr::mutate(SSI = "N")
  }


  write.csv(responsesOutput,
            file.path(dir_results,
                      TargetSiteID,
                      paste0(TargetSiteID,
                             "_Responses.csv")),
            row.names = FALSE)

  # Shiny add ons
  if (cfg$boo_shiny == TRUE) {
    report_type <- "full" # summary preliminary full
    # browser()
    # getwd()
    # list.files()

    # copy RMD so works in Shiny
    ## render switches working directory to location of RMD
    rmd2copy <- list.files(file.path(system.file(package = "CASTfxn"), "rmd"),
                           pattern = "\\.rmd$",
                           full.names = TRUE)
    file.copy(rmd2copy, ".", overwrite = TRUE)

    # need graphic as well
    svg2copy <- list.files(file.path(system.file(package = "CASTfxn"), "rmd"),
                           pattern = "\\.svg$",
                           full.names = TRUE)
    file.copy(svg2copy, ".", overwrite = TRUE)

    # browser()
    # not found, added to function call

    # report
    getReport(TargetSiteID = TargetSiteID,
              biocommlist    = meta$biocommlist,
              regionName     = cfg$region,
              primeIndex     = meta$bmiIndexGp,
              removeOutliers = meta$removeOutliers,
              samplim        = meta$samplim,
              r2_cutoff      = meta$r2_cutoff,
              p.val_cutoff   = meta$p.val_cutoff,
              useBC          = cfg$useBC,
              lagdays        = meta$lagdays,
              DOlim          = meta$DOlim,
              pHlimLow       = meta$meta$pHlimLow,
              pHlimHigh      = meta$pHlimHigh,
              bmiIndex       = meta$bmiIndexGp,
              algIndex       = meta$algIndexGp,
              fishIndex      = meta$fishIndexGp,
              useBMI         = list.AvailData$use_bmi,
              useAlg         = list.AvailData$use_alg,
              useFish        = list.AvailData$use_fish,
              dir_data       = normalizePath(dir_data),
              dir_results    = normalizePath(dir_results),
              report_type    = report_type, # full, preliminary, summary
              report_format  = "html",
              dir_rmd = ".", # added for Shiny after copy RMD
              boo.WS = meta$exploreWSStressor,
              data_sampSummary = data_sampSummary,
              data_bmiMetrics = list.bmiData$data_bioMetrics,
              data_algMetrics = list.algData$data_bioMetrics,
              data_fishMetrics = list.fishData$data_bioMetrics,
              data_stressInfo = list.Stress$data_stressInfo,
              siteDetectsAll = list.StressorElim$targMeasStress # TODO see if this right
              )

  } else {
    report_type <- "full"

    getReport(TargetSiteID = TargetSiteID,
              biocommlist    = meta$biocommlist,
              regionName     = cfg$region,
              primeIndex     = meta$bmiIndexGp,
              removeOutliers = meta$removeOutliers,
              samplim        = meta$samplim,
              r2_cutoff      = meta$r2_cutoff,
              p.val_cutoff   = meta$p.val_cutoff,
              useBC          = cfg$useBC,
              lagdays        = meta$lagdays,
              DOlim          = meta$DOlim,
              pHlimLow       = meta$pHlimLow,
              pHlimHigh      = meta$pHlimHigh,
              bmiIndex       = meta$bmiIndexGp,
              algIndex       = meta$algIndexGp,
              fishIndex      = meta$fishIndexGp,
              useBMI         = list.AvailData$use_bmi,
              useAlg         = list.AvailData$use_alg,
              useFish        = list.AvailData$use_fish,
              dir_data       = normalizePath(cfg$in_dir),
              dir_results    = normalizePath(dir_results),
              report_type    = "full",
              report_format  = "html",
              boo.WS = meta$exploreWSStressor,
              data_sampSummary = data_sampSummary,
              data_bmiMetrics = list.bmiData$data_bioMetrics,
              data_algMetrics = list.algData$data_bioMetrics,
              data_fishMetrics = list.fishData$data_bioMetrics,
              data_stressInfo = list.Stress$data_stressInfo,
              siteDetectsAll = list.StressorElim$targMeasStress,
              dn_checked_sk = cfg$dn_checked_sk
              )

  }## IF ~ cfg$boo_shiny

  gaps <- gaps |>
    dplyr::bind_rows(tryCatch(list.CompSites.alg$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.CompSites.fish$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.CompSites.bmi$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.SiteInfo$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.WSStressorFigs$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.AvailData$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.Outliers$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.QualSites$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.StressorMetadata$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.BioStressorResponses$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.VerifiedPredictions$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.VPSSIscores$df_gap, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.StressorElim$gap_no_sr, error = function(e) NULL)) |>
    dplyr::bind_rows(tryCatch(list.StressorElim$gap_no_site_sr, error = function(e) NULL))

  write.csv(gaps, file.path(dir_results,
                                TargetSiteID,
                                paste0(TargetSiteID, "_datagaps.csv")),
              row.names = FALSE)

# status
  temp_status <- data.frame(TargetSiteID = as.character(TargetSiteID), status = "Passed", reason = "")
  status_df <- status_df |> dplyr::bind_rows(temp_status)

  } # End TargetSite loop

# FOR ~ site ~ END ####

fn_status <- file.path(dir_results,
                       paste0("TargetSiteID_Status_",
                              format(Sys.Date(),"%Y%m%d"),
                              "_",
                              format(Sys.time(),"%H%M%S"),
                              ".csv"))
write.csv(status_df,
          fn_status,
          row.names = FALSE)

rm(site)

toc <- Sys.time()
msg <- paste0("report time (min): ",
              round(difftime(toc, tic, units = "min"), 2))
message(msg)

# 24, Clean Up ----
# Clean up operations
if (cfg$boo_shiny == TRUE) {
  prog_det <- "Clean Up"
  prog_cnt <- prog_cnt + 1
  prog_msg <- paste0("Step ", prog_cnt)
  prog_inc <- 1 / prog_n
  incProgress(prog_inc, message = prog_msg, detail = prog_det)
  Sys.sleep(prog_sleep)
  message(paste(prog_msg, prog_det, sep = "; "))
}## IF ~ cfg$boo_shiny ~ END

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Skeleton, END ####
# external/RPPTool_CA.R
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
