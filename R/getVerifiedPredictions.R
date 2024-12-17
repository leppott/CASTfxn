#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Verified Predictions
#'
#' @description Get verified predictions.
#'
#' @details
#'
#' Required packages: dplyr, ggplot2, stringr, tidyr
#'
#' @param TargetSiteID Site ID
#' @param stressors vector of stressors identified as candidate causes
#' @param df_stressinfo dataframe of stressor metadata
#' @param SSTVanalytes vector containing StdParamNames for stressors with stressor-specific tolerance values
#' @param list.MatchBioData list_MatchBioData
#' @param biocomm default = "bmi"
#' @param colBioSample column name for the response sample ID
#' @param df_BioTaxaRelAbund dataframe of raw response data (counts or relative abundance)
#' @param df_MasterTaxa dataframe of master taxa with SSTV values determined for individual taxa
#' @param colBio default = "IBI"
#' @param BioIndex_Nar default = "Quality"
#' @param BioIndex_Nar_Deg default = "Degraded"
#' @param dir_plots default = file.path(getwd(), "Results")
#' @param dir_sub default = "VerifiedPredictions"
#' @param boo_plot = TRUE
#'
#' @return Results text file and png files to "Results" "VerifiedPredictions" folder
#' in working directory of box plots
#'
#' @examples
#' \dontrun{
#' TargetSiteID <- "SRCKN001.61"
#' dir_plots  <- file.path(getwd(), "Results")
#'
#' # Data getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites          # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.mod           <- data_ReachMod
#' df_MasterTaxa        <- data_BMIMasterTaxa
#'
#' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID"]==TargetSiteID
#'                     , "ElevCategory"])
#' if(elev_cat=="HI"){
#'    data.cluster <- data_Cluster_Hi
#' } else if(elev_cat=="LO") {
#'    data.cluster <- data_Cluster_Lo
#' }
#'
#' # Map data
#' # San Diego
#' #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
#' #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
#' # AZ
#' map_flowline  <- data_GIS_Flow_HI
#' map_flowline2 <- data_GIS_Flow_LO
#' if(elev_cat=="HI"){
#'    map_flowline <- data_GIS_Flow_HI
#' } else if(elev_cat=="LO") {
#'    map_flowline <- data_GIS_Flow_LO
#' }
#' map_outline   <- data_GIS_AZ_Outline
#' # Project site data to USGS Albers Equal Area
#' usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#'               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' # projection for outline
#' my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
#'            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' map_proj <- my.aea
#' #
#' dir_sub <- "SiteInfo"
#'
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, dir_plots, data.Stations.Info
#'                                 , data.SampSummary, data.303d.ComID
#'                                 , data.bmi.metrics, data.algae.metrics
#'                                 , data.cluster, data.mod
#'                                 , map_proj, map_outline, map_flowline
#'                                 , dir_sub=dir_sub)
#'
#' # Data getChemDataSubsets
#' # data, example included with package
#' data.chem.raw  <- data_Chem
#' data_stressInfo <- data_ChemInfo
#' site.COMID     <- list.SiteSummary$COMID
#' site.Clusters  <- list.SiteSummary$ClustIDs
#'
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
#'                                 , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
#'                                 , data.chem.raw=data.chem.raw, data_stressInfo=data_stressInfo)
#'
#' # Data getStressorList
#' chem.info     <- list.data$chem.info
#' cluster.chem  <- list.data$cluster.chem
#' cluster.samps <- list.data$cluster.samps
#' ref.sites     <- list.data$ref.sites
#' site.chem     <- list.data$site.chem
#' dir_sub <- "CandidateCauses"
#'
#' # set cutoff for possible stressor identification
#' probsLow  <- 0.10
#' probsHigh <- 0.90
#' biocomm <- "bmi"
#'
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, site.chem
#'                                  , probsHigh, probsLow, biocomm, dir_plots
#'                                  , dir_sub)
#'
#' # Data getBMIMatches
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
#'
#' # Run getBioMatches
#' biocomm <- "BMI"
#' data.bio.metrics <- data_BMIMetrics
#' list.MatchBioData<- getBioMatches(stressors, list.data, list.SiteSummary, data.SampSummary
#'                                   , data.chem.raw, data.bio.metrics, biocomm)
#'
#' # Data getVerifiedPredictions
#' # data import, example
#' # df_BioTaxaRelAbund  <- read.delim(paste(myDir.Data,"data.bmi.taxa.raw.tab",sep=""))
#' # data.SSTV.totabund <- read.delim(paste(myDir.Data,"data.totabund.bySample.tab",sep=""))
#' #
#' # data, example included with package
#' df_BioTaxaRelAbund  <- data_BMIcounts
#' data.SSTV.totabund <- data_BMIRelAbund
#' colBio       <- "IBI"
#' BioIndex_Nar       <- "NarRat"
#' BioIndex_Nar_Deg   <- "Violates"
#' dir_sub            <- "VerifiedPredictions"
#' biocomm <- "bmi"
#'
#' # Run getVerifiedPredictions
#' getVerifiedPredictions(TargetSiteID
#'                        , data.SampSummary
#'                        , df_BioTaxaRelAbund
#'                        , data_stressInfo
#'                        , data.SSTV.totabund
#'                        , df_MasterTaxa
#'                        , list.MatchBioData
#'                        , ref.sites
#'                        , colBio
#'                        , BioIndex_Nar
#'                        , BioIndex_Nar_Deg
#'                        , dir_plots
#'                        , dir_sub)
#' }
#~~~~~~~~~~~~~~~~
#' @export
getVerifiedPredictions <- function(TargetSiteID
                                   , stressors
                                   , df_stressinfo
                                   , SSTVanalytes
                                   , list.MatchBioData
                                   , biocomm
                                   , df_BioTaxaRelAbund
                                   , df_MasterTaxa
                                   , colBio
                                   , BioIndex_Nar
                                   , BioIndex_Nar_Deg
                                   , dir_plots = file.path(getwd(), "Results")
                                   , dir_sub = "VerifiedPredictions"
                                   , boo_plot = TRUE
                                   ) {##FUNCTION.START



  # Debugging
  boo.DEBUG <- FALSE
  #
  if (boo.DEBUG == TRUE) {##IF.boo.DEBUG.START
    TargetSiteID = TargetSiteID
    stressors = stressors
    df_stressinfo = siteStressInfo
    SSTVanalytes = as.character(SSTVparms)
    list.MatchBioData = list_MatchBioData
    biocomm = bioComm
    df_BioTaxaRelAbund = bioTaxaData
    df_MasterTaxa = bioMasterTaxa
    colBio = bioIndex
    BioIndex_Nar = "Quality"
    BioIndex_Nar_Deg = "Degraded"
    dir_plots = dir_results
    dir_sub = "VerifiedPredictions"
    boo_plot = TRUE
    tv <- 1
  }##IF.boo.DEBUG.END

  # define pipe
  `%>%` <- dplyr::`%>%`
  col.Bio.Deg   <- "Quality"
  # QC, biocomm ####
  biocomm <- toupper(biocomm)

  if (exists("keepMTcol")) {rm(keepMTcol)}
  if (exists("deleteSSTVnames")) {rm(deleteSSTVnames)}
  if (exists("mtcols")) {rm(mtcols)}

  # Write results directory ----
  wd <- dirname(dir_plots)
  dir.sub <- basename(dir_plots)
  dir.sub2 <- TargetSiteID
  dir.sub3 <- biocomm
  dir.sub4 <- dir_sub
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2)) == TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3)) == TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3))
         , FALSE)
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4)) == TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4))
         , FALSE)
  dir_path <- file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4)

  # Intersect SSTVanalytes with stressors ----
  # These are the stressors that need evaluating. Other stressors will be
  # logged as data gaps?
  SSTVanalytes <- SSTVanalytes[SSTVanalytes %in% stressors]
  otherAnalytes <- stressors[!(stressors %in% SSTVanalytes)]

  # Write data gaps ----
  for (o in seq_along(otherAnalytes)) {
    otherName <- otherAnalytes[o]
    gapcomment <- paste0("No stressor-specific tolerance values for "
                         , otherName, ".")
    gaps <- cbind.data.frame("getVerifiedPredictions", "No VP data", 0
                             , gapcomment)
    colnames(gaps) <- c("fxnname", "condition", "result", "comment")
    fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
    fn.gaps <- file.path(dir_plots, TargetSiteID,fn.gaps)
    # write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
    #             , row.names = FALSE, sep = "\t")
  }

  # Iterate over stressors with SSTVs ----
  df_SSTV <- df_stressinfo %>%
    dplyr::filter(StdParamName %in% SSTVanalytes) %>%
    dplyr::select(StdParamName, SSTVname, SensMin, SensMax, TolMin, TolMax)
  df_SSTV <- unique(df_SSTV)
  df_SSTV <- merge(df_SSTV, df_stressinfo[, c("StdParamName", "Label")])

  SSTVnames <- as.vector(unique(df_SSTV$SSTVname))
  mtcols <- colnames(df_MasterTaxa)

  # Check whether master taxa file contains SSTVname (tol vals for that stressor)

  # Check for SSTV column names in master taxa file ----
  for (n in seq_along(SSTVnames)) {  # If more than one SSTV, then must iterate
    name <- SSTVnames[n]
    SSTVlabel <- as.character(df_SSTV$Label[df_SSTV$SSTVname == name])

    if (name %in% mtcols) {  # Check if TV data in Master Taxa file
      if (exists("keepMTcol")) {
        keepMTcol <- c(keepMTcol, name)
      } else {
        keepMTcol <- name
      }
    } else {
      # no taxa in MT taxa are assigned tol values for this stressor
      gapcomment <- paste0("No ", biocomm, " taxa have tolerance "
                           , "values for this stressor.")
      gaps <- cbind.data.frame("getVerifiedPredictions", SSTVlabel, 0, gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_plots, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
      if (exists("deleteSSTVname")) {
        deleteSSTVnames <- c(deleteSSTVnames, name)
      } else {
        deleteSSTVnames <- name
      }
    }
  }

  # Merge biotaxa results with master taxa file ----
  df_MT_SSTVs <- df_MasterTaxa %>%
    dplyr::select(TaxonID, all_of(keepMTcol))

  allDetectedTaxa <- df_BioTaxaRelAbund %>%
    dplyr::distinct(TaxonID)
  allDetectedTaxa <- as.character(unlist(allDetectedTaxa))

  # Is this necessary?
  siteDetectedTaxa <- df_BioTaxaRelAbund %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::distinct(TaxonID)
  siteDetectedTaxa <- as.character(unlist(siteDetectedTaxa))

  # boo.continue = FALSE  # default value; only flips to true if data available
  #
  # if (exists("deleteSSTVnames") == TRUE) { # Some SSTV stressors not used
  #   if (all(SSTVnames %in% deleteSSTVnames)) { # No SSTV stressors in master taxa
  #     gapcomment <- paste0("No stressor-specific tolerance values for "
  #                          , "potential site stressors exist in the master "
  #                          , "taxa file for ", biocomm, ".")
  #     gaps <- cbind.data.frame("getVerifiedPredictions", "No VP data", 0
  #                              , gapcomment)
  #     colnames(gaps) <- c("fxnname", "condition", "result", "comment")
  #     fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
  #     fn.gaps <- file.path(dir_plots, TargetSiteID,fn.gaps)
  #     write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
  #                 , row.names = FALSE, sep = "\t")
  #
  #     msg <- gapcomment
  #     message(msg)
  #
  #     boo.continue = FALSE
  #   } # NO SSTV stressors are used; exit function cleanly
  # }
  #

  if (exists("keepMTcol") == TRUE) { # Some stressors have SSTV vals in master taxa file

    keepMTcol <- as.character(keepMTcol)
    df_SSTVtaxa <- df_MasterTaxa %>%
      dplyr::select(TaxonID, all_of(keepMTcol))

    # Keep taxa with SSTValues, discard those without
    if (length(keepMTcol) == 1) {
      msg <- "Got only 1 SSTV stressor!"
      message(msg)
      df_SSTVtaxa <- df_SSTVtaxa[!is.na(df_SSTVtaxa[, keepMTcol]), ]

    } else { # Two or more SSTV stressors exist ### NOT TESTED WITH DATA
      msg <- "Got 2 or more SSTV stressors!"
      message(msg)

      msg <- keepMTcol
      message(msg)
      df_SSTVtaxa <- df_SSTVtaxa[rowSums(!is.na(df_SSTVtaxa[, -1])) >= 1, ]

    }

    SSTVtaxanames <- unique(as.character(df_SSTVtaxa$TaxonID))
    reportedtaxa <- unique(as.vector(df_BioTaxaRelAbund$TaxonID))

    if (any(reportedtaxa %in% SSTVtaxanames) == TRUE) {
      df_SSTVrelabund <- df_BioTaxaRelAbund %>%
      # dplyr::rename(RelAbund = RelAbundInds) %>%
      dplyr::select(RespSampleID, TaxonID, RelAbund) %>%
      dplyr::filter(TaxonID %in% SSTVtaxanames)
      boo.continue = TRUE
    } else {
      gapcomment <- paste0("No stressor-specific tolerance values for "
                           , "potential site stressors exist in the master "
                           , "taxa file for ", biocomm, ".")
      gaps <- cbind.data.frame("getVerifiedPredictions", "No VP data", 0
                               , gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
      fn.gaps <- file.path(dir_plots, TargetSiteID,fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")

      msg <- gapcomment
      message(msg)

      boo.continue = FALSE
    }
  } else {
    boo.continue = FALSE
  }

  if (boo.continue == TRUE) { # Have
    # check for and create (if necessary) "Results" subdirectory of working directory
    # wd <- dirname(dir_plots)
    # dir.sub <- basename(dir_plots)
    # dir.sub2 <- TargetSiteID
    # dir.sub3 <- biocomm
    # dir.sub4 <- dir_sub
    # ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2)) == TRUE
    #        , dir.create(file.path(wd, dir.sub, dir.sub2))
    #        , FALSE)
    # ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3)) == TRUE
    #        , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3))
    #        , FALSE)
    # ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4)) == TRUE
    #        , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4))
    #        , FALSE)
    # dir_path <- file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4)

    # 20190513, remove scores file if exists
    fn_scores <-  file.path(dir_path, paste0(TargetSiteID, "_", biocomm
                                             , "_VP_Scores.tab"))
    if (file.exists(fn_scores)) { file.remove(fn_scores) }

    # plots.tvr <- vector(10, mode="list")
    plots.tv <- vector(10, mode = "list")
    ppi<-300
    plot_H <- 4
    plot_W <- 8

    # boo.pryr <- FALSE

    # Target Site Bio Scores
    targ_bio <- list.MatchBioData$site.b.rsp[, colBio]
    targ_bio_bad <- list.MatchBioData$site.b.rsp[list.MatchBioData$site.b.rsp[
      , BioIndex_Nar] == BioIndex_Nar_Deg, colBio]
    targ_bio_good <- list.MatchBioData$site.b.rsp[list.MatchBioData$site.b.rsp[
      , BioIndex_Nar] != BioIndex_Nar_Deg, colBio]
    targ_bio_min <- min(targ_bio, na.rm = TRUE)
    targ_bio_max <- max(targ_bio, na.rm = TRUE)

    # skip to next if no "bad" bio scores for this site
    msg_stop_NoBadBio <- paste0("There are no '", BioIndex_Nar_Deg
                                , "' bio sites for comparison for this site.")
    # Use minimum good or maximum bad for "better than" threshold
    if (length(targ_bio_bad) == 0) {
      targ_bio_good_min <- min(targ_bio_good, na.rm = TRUE)
      targ_bio_good_max <- max(targ_bio_good, na.rm = TRUE)
      # bio threshold to use for "better"
      bio_better_thresh <- targ_bio_good_min
    } else {
      targ_bio_bad_min <- min(targ_bio_bad, na.rm = TRUE)
      targ_bio_bad_max <- max(targ_bio_bad, na.rm = TRUE)
      # bio threshold to use for "better"
      bio_better_thresh <- targ_bio_bad_max
    }

    # skip to next if no "bad" bio scores for this site
    # This should never be triggered, because there is a bio index score
    if(is.na(bio_better_thresh)){
      #next
      stop(msg_stop_NoBadBio)
    }

    # IF ####
    if (nrow(df_SSTV) != 0) {##IF.SSTV.START
      #
      stressor.SSTV <- subset(df_SSTV, Analyte %in% stressors)

      tv.len <- nrow(stressor.SSTV)

      #
      if (nrow(stressor.SSTV) != 0) {##IF.stressor.SSTV.START
        #
        # Loop tv (stressor) ####
        for (tv in 1:nrow(stressor.SSTV)) {##FOR.tv.START
          # Currently only valid for SpecCond
          #
          SSTV.analyte <- as.vector(stressor.SSTV$Analyte)[tv]
          SSTV.name <- as.vector(stressor.SSTV$SSTVname)[tv]
          SSTV.label <- df_stressinfo$Label[df_stressinfo$StdParamName == SSTV.analyte]
          SSTV.label <- unique(as.character(SSTV.label))

          if (boo.DEBUG == TRUE) {##IF.boo.DEBUG.START
            varFlag <- 0
            #if(tv==1){tv=20}
          }##IF.boo.DEBUG.END
          #

          tv.len <- nrow(stressor.SSTV)
          msg <- paste0("Item (", tv, "/", tv.len,"); Stressor = ", SSTV.analyte)
          message(msg)
          # print(msg)
          # utils::flush.console()

          # skip if SSTV = ""
          ## 20181211
          if (is.na(SSTV.name) == TRUE | SSTV.name == "") {
            msg <- "No data; SKIP"
            message(msg)
            # print(msg)
            # utils::flush.console()
            next
          }

          # 20190111, get LogTransf (0 = FALSE; 1 = TRUE)
          # need to use max (default of 1) in case of duplicates
          chem.info_LogTransf <- df_stressinfo %>%
            dplyr::group_by(StdParamName) %>%
            dplyr::summarise(max_LogTransf = max(LogTransf, na.rm = TRUE)
                             , .groups = "drop_last")
          LogTransf <- chem.info_LogTransf$max_LogTransf[chem.info_LogTransf[, "StdParamName"] == SSTV.analyte]
          log.yn <- as.logical(LogTransf)

          # get all the matched sample data for this stressor
          # 20180620, match names
          col_keep <- c("StationID", "StressSampleID", "RespSampleID")
          SSTV.analyte.match.all.b.str <- SSTV.analyte[SSTV.analyte %in%
                                                         names(list.MatchBioData$all.b.str)]
          all.match.b.str <- list.MatchBioData$all.b.str[, c(col_keep, SSTV.analyte.match.all.b.str)]
          cl.match.b <- list.MatchBioData$cl.b.str[, c(col_keep, SSTV.analyte.match.all.b.str)]

          bmi.taxa.raw <- df_BioTaxaRelAbund[df_BioTaxaRelAbund$StationID %in%
                                        unique(all.match.b.str$StationID), ]
          bmi.taxa.raw <- merge(bmi.taxa.raw
                                , df_MasterTaxa[, c("TaxonID", SSTV.name)]
                                , by.x = "TaxonID", by.y = "TaxonID")

          minTolVal <- min(df_MasterTaxa[,SSTV.name], na.rm = TRUE)
          maxTolVal <- max(df_MasterTaxa[,SSTV.name], na.rm = TRUE)

          bmi.taxa.raw$SensTaxa <- ifelse(bmi.taxa.raw[, SSTV.name] == minTolVal |
                                            bmi.taxa.raw[, SSTV.name] == minTolVal + 1
                                          , bmi.taxa.raw$RelAbund, NA)

          bmi.taxa.raw$TolTaxa <- ifelse(bmi.taxa.raw[,SSTV.name] == maxTolVal |
                                           bmi.taxa.raw[,SSTV.name] == maxTolVal - 1
                                         , bmi.taxa.raw$RelAbund, NA)

          bmi.taxa.raw <- dplyr::group_by(bmi.taxa.raw, StationID
                                          , BMISampID) %>%
            dplyr::summarize(SensRelAbund = sum(SensTaxa, na.rm = TRUE)
                             , TolRelAbund = sum(TolTaxa, na.rm = TRUE)
                             , .groups = "drop_last")
          bmi.taxa.raw <- dplyr::rename(bmi.taxa.raw, RespSampleID = BMISampID)

          all.match.b.resp <- bmi.taxa.raw[bmi.taxa.raw$RespSampleID %in%
                                             unique(all.match.b.str$RespSampleID), ]

          col_by <- c("StationID", "RespSampleID")
          all.SSTV.abund <- merge(all.match.b.str
                                  , all.match.b.resp
                                  , by.x = col_by
                                  , by.y = col_by
                                  , all = TRUE)

          # Add Bio Index (value and Narrative Rating) (20190305)
          all.SSTV.abund <- merge(all.SSTV.abund
                                  , list.MatchBioData$all.b.rsp[, c(col_by
                                                              , BioIndex_Nar
                                                              , colBio)]
                                  , by.x = col_by
                                  , by.y = col_by
                                  , all.x = TRUE)

          good.SSTV.abund    <- all.SSTV.abund[stats::complete.cases(all.SSTV.abund), ]
          cl.SSTV.abund      <- subset(good.SSTV.abund
                                       , good.SSTV.abund$StressSampleID %in%
                                         cl.match.b$StressSampleID)
          site.SSTV.abund    <- subset(good.SSTV.abund
                                       , good.SSTV.abund$StationID
                                       %in% TargetSiteID)
          SSTV.Resp          <- c("SensRelAbund", "TolRelAbund")

          varFlag <- 1

          # Generate data for plotting (1 = all complete cases, 3 = comparators
          # 5 = target site)
          df.plot1 <- good.SSTV.abund[, c(SSTV.analyte, SSTV.Resp)]
          df.plot3 <- cl.SSTV.abund[, c(SSTV.analyte, SSTV.Resp)]
          df.plot5 <- site.SSTV.abund[, c(SSTV.analyte, SSTV.Resp)]

          # Log transform if indicated
          if (log.yn == TRUE) {
            df.plot1[, SSTV.analyte] <- log1p(df.plot1[, SSTV.analyte])
            df.plot3[, SSTV.analyte] <- log1p(df.plot3[, SSTV.analyte])
            df.plot5[, SSTV.analyte] <- log1p(df.plot5[, SSTV.analyte])
          }

          ## ALL
          # 20190305, drop added Bio Index value and narrative
          # 20230529, convert reshape2::melt to tidyr::pivot_longer ARL
          df_plot_all <- good.SSTV.abund[, 1:6] %>%
            tidyr::pivot_longer(cols = c(SensRelAbund, TolRelAbund)
                                , names_to = "variable", values_to = "value")
          df_plot_all[, "Param_Name"] <- SSTV.analyte
          colnames(df_plot_all)[colnames(df_plot_all) == SSTV.analyte] <- "Param_Value"
          df_plot_all <- df_plot_all[, c(1:3, 7, 4:6)]

          ## BETTER BIO
          # 20230529, convert reshape2::melt to tidyr::pivot_longer ARL
          # 20190305, switch to "better" bio from all
          df_plot_betterbio <- good.SSTV.abund[good.SSTV.abund[, colBio]
                                               > bio_better_thresh, 1:6]
          df_plot_betterbio <- tidyr::pivot_longer(df_plot_betterbio
                                                   , cols = c(SensRelAbund, TolRelAbund)
                                                   , names_to = "variable"
                                                   , values_to = "value") %>%
            dplyr::mutate(variable = ifelse(variable == "SensRelAbund"
                                            , "Sensitive Taxa", "Tolerant Taxa")
                          , Param_Name = SSTV.analyte) %>%
            dplyr::rename(Param_Value = eval(SSTV.analyte)) %>%
            dplyr::select(StationID:StressSampleID, Param_Name
                          , Param_Value:value)

          n_records_better_bio <- nrow(df_plot_betterbio)

          ## TARGET
          # 20230529, convert reshape2::melt to tidyr::pivot_longer ARL
          df_plot_targ <- site.SSTV.abund[, 1:6]
          df_plot_targ <- tidyr::pivot_longer(site.SSTV.abund[, 1:6]
                                              , cols = c(SensRelAbund, TolRelAbund)
                                              , names_to = "variable"
                                              , values_to = "value") %>%
            dplyr::mutate(variable = ifelse(variable == "SensRelAbund"
                                            , "Sensitive Taxa", "Tolerant Taxa")
                          , Param_Name = SSTV.analyte) %>%
            dplyr::rename(Param_Value = eval(SSTV.analyte)) %>%
            dplyr::select(StationID:StressSampleID, Param_Name
                          , Param_Value:value)

          # 20190510, new data frame for better sites AND bio.deg = No
          # IBI scores (drop variable and value from good.SSTV.abund)
          df_IBI <- unique(good.SSTV.abund[, c(1:4, 7:8)])
          # Add IBI scores to "better" sites
          df_plot_betterbio_IBI <- merge(df_plot_betterbio, df_IBI, all.x = TRUE)
          # Add Bio.Deg
          df_plot_betterbio_IBI[, col.Bio.Deg] <-
            ifelse(df_plot_betterbio_IBI[, BioIndex_Nar] == BioIndex_Nar_Deg
                   , "Yes", "No")
          df_plot_betterbio_BioDegNo <-
            df_plot_betterbio_IBI[df_plot_betterbio_IBI[, col.Bio.Deg] == "No", ]

          n_records_betterbio_BioDegNo <- nrow(df_plot_betterbio_BioDegNo)

          # Scoring ####
          # Get percentiles by taxa group
          # 20230530 ARL; Converted to tidyverse
          df_quantiles <- df_plot_betterbio %>%
            dplyr::select(variable, value) %>%
            dplyr::group_by(variable) %>%
            dplyr::summarise(q25 = quantile(value, probs = 0.25)
                             , q50 = quantile(value, probs = 0.50)
                             , q75 = quantile(value, probs = 0.75)
                             , .groups = "drop_last")
          q_Sens_lo <- df_quantiles %>% filter(variable == "Sensitive Taxa") %>%
            select(q25) %>% as.numeric(.)
          q_Sens_hi <- df_quantiles %>% filter(variable == "Sensitive Taxa") %>%
            select(q50) %>% as.numeric(.)
          q_Tol_lo <- df_quantiles %>% filter(variable == "Tolerant Taxa") %>%
            select(q50) %>% as.numeric(.)
          q_Tol_hi <- df_quantiles %>% filter(variable == "Tolerant Taxa") %>%
            select(q75) %>% as.numeric(.)

          # Add scoring thresholds to target site data frame
          # 20230530 ARL; converted to tidyverse
          df_plot_targ <- df_plot_targ %>%
            dplyr::mutate(better_bio_varval_qLo = ifelse(variable == "Sensitive Taxa"
                                                         , q_Sens_lo, q_Tol_lo)
                          , better_biovarval_qHi = ifelse(variable == "Sensitive Taxa"
                                                          , q_Sens_hi, q_Tol_hi))
          # Add scores to target site data frame
          df_plot_targ <- df_plot_targ %>%
            dplyr::mutate(Score = case_when((variable == "Sensitive Taxa") &
                                              (value < better_bio_varval_qLo) ~ 1
                                            , (variable == "Sensitive Taxa") &
                                              (value > better_biovarval_qHi) ~ -1
                                            , (variable == "Tolerant Taxa") &
                                              (value > better_biovarval_qHi) ~ 1
                                            , (variable == "Tolerant Taxa") &
                                              (value > better_bio_varval_qLo) ~ -1
                                            , TRUE ~ 0))


          # Add other variables
          df_plot_targ[, "biocomm"] <- biocomm
          df_plot_targ[, "n_BetterBio"] <- n_records_better_bio
          df_plot_targ[, "n_BetterBioDegNo"] <- n_records_betterbio_BioDegNo
          df_tbl_scores <- merge(df_plot_targ
                                 , site.SSTV.abund[,c("RespSampleID"
                                                      ,"StressSampleID"
                                                      , colBio
                                                      , "Quality")]
                                 , by.x = c("RespSampleID","StressSampleID")
                                 , by.y = c("RespSampleID","StressSampleID")
                                 , all.x = TRUE)
          df_tbl_scores <- merge(df_tbl_scores
                                 , unique(df_stressinfo[,c("StdParamName", "Label")])
                                 , by.x = "Param_Name"
                                 , by.y = "StdParamName"
                                 , all.x = TRUE)
          df_tbl_scores <- dplyr::select(df_tbl_scores, StationID
                                         , RespSampleID, eval(colBio)
                                         , Quality, StressSampleID, Label
                                         , Param_Name, Param_Value
                                         , variable, value, better_bio_varval_qLo
                                         , better_biovarval_qHi, Score
                                         , biocomm, n_BetterBio
                                         , n_BetterBioDegNo) %>%
            dplyr::rename(Stressor = Param_Name
                        , StressorValue = Param_Value
                        , Response = variable
                        , ResponseValue = value
                        , qLoValue_Cutoff = better_bio_varval_qLo
                        , qHiValue_Cutoff = better_biovarval_qHi)

          # Save
          # fn_scores <-  file.path(dir.sub, dir.sub2, dir.sub3
          #                         , paste0(TargetSiteID, ".SR.SSTV.Scores.txt"))
          boo_append <- TRUE
          boo_colnames <- FALSE
          if(file.exists(fn_scores)==FALSE){##IF~file.exists(fn_scores)~START
            # invert for 1st instance
            boo_append <- !boo_append
            boo_colnames <- !boo_colnames
          }##IF~file.exists(fn_scores)~END

          utils::write.table(df_tbl_scores, file = fn_scores
                             , col.names = boo_colnames, row.names = FALSE
                             , sep="\t", append = boo_append)

          # ggplot ####

          ##PLOT VARIABLES ~ START
            ## Plot, Variables, Strings
            str_title <- paste0(TargetSiteID, ": Verified prediction "
                                ,"line of evidence for ", SSTV.label)
            str_title <- stringr::str_wrap(str_title, 100)
            str_subtitle <- paste0("Do the data support the prediction"
                                   , " that the abundance of sensitive"
                                   , " taxa will be lower and tolerant"
                                   , " taxa will be higher than that"
                                   , " observed at comparator sites with"
                                   , " better biology?")
            str_subtitle <- stringr::str_wrap(str_subtitle, 100)
            str_xlab  <- ""
            str_ylab  <- "Relative Abundance"
            # df_plot_targ_sortvalue <- df_plot_targ[order(df_plot_targ[,"value"]), ]
            str_score_sens <- paste(df_plot_targ[df_plot_targ[, "variable"] == "Sensitive Taxa", "Score"], collapse = ", ")
            str_score_tol <- paste(df_plot_targ[df_plot_targ[, "variable"] == "Tolerant Taxa", "Score"], collapse = ", ")
            str_caption <- paste0("Score = Tolerant Taxa (", str_score_tol
                                  , "), Sensitive Taxa ("
                                  , str_score_sens
                                  , ")\nNumber of samples with better biology (n="
                                  , n_records_better_bio
                                  , "); better biology and not degraded (n="
                                  , n_records_betterbio_BioDegNo, ")"
                                  , "\nSamples with better biology have "
                                  , colBio, " > "
                                  , signif(bio_better_thresh, 3))

            ## Plot, Variables, Colors
            col_sites_targ    <- "red"

            ## Plot, Variables, Fill
            fill_sites_targ    <- col_sites_targ

            ## Plot, Variables, Points
            pch_sites_targ    <- 17 # triangle

            ## Plot, Variables, Sizes
            cex_mod <- 2
            cex_sites_targ    <- cex_mod*1.2

            ## Plot, Variables, Target Site Line
            targ_line_col <- col_sites_targ
            targ_line_lty <- 2
            targ_line_lwd <- 1

            ## Plot, Variables, Legend
            leg_name   <- "Sites"
            leg_labels <- c("target")
            leg_shape  <- c(pch_sites_targ)
            leg_col    <- c(col_sites_targ)
            leg_fill   <- c(fill_sites_targ)

          ##PLOT VARIABLES ~ END

          ## Plot, Variables, Bio.Deg
          bio_col <- c("gray20", "blue")
          bio_shp <- c(21, 25) # circle and down triangle
          bio_size <- c(2, 1)

          col.SiteTypeQuality <- col.Bio.Deg

          display_target <- "lines"  # "lines", "points"

          p_SSTV <- ggplot2::ggplot(df_plot_betterbio_IBI, ggplot2::aes(variable, value)) +
            ggplot2::geom_boxplot(ggplot2::aes(group = variable)) +
            ggplot2::labs(title = str_title
                          , subtitle = str_subtitle
                          , y = str_ylab
                          , caption = str_caption) +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 12)
                           , plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10)
                           , plot.caption = ggplot2::element_text(size = 8)
                           , legend.title = ggplot2::element_text(size = 8)
                           , legend.text = ggplot2::element_text(size = 6)
                           , axis.title.x = ggplot2::element_text(size = 10)
                           , axis.title.y = ggplot2::element_blank()) +
            ggplot2::coord_flip() +

            # Add degraded y/n for better bio sites
            # 20230530 ARL; Convert geom_jitter(aes_string) to
            # geom_point(position = "jitter", aes())
            ggplot2::geom_point(ggplot2::aes(color=Bio.Deg, shape=Bio.Deg
                                             , fill=Bio.Deg, size=Bio.Deg)
                                , alpha=0.45, na.rm = TRUE, position = "jitter") +
            # ggplot2::geom_jitter(data = df_plot_betterbio_IBI
            #                      # , size = 1
            #                      , alpha = 0.45
            #                      , na.rm = TRUE
            #                      , ggplot2::aes_string(color = col.SiteTypeQuality
            #                                            , shape = col.SiteTypeQuality
            #                                            , fill = col.SiteTypeQuality)) +
            # redo box with no fill (can't change alpha of just the box if do 2nd and want to keep gray background)
            ggplot2::geom_boxplot(fill = NA, ggplot2::aes(group = variable)) +
            # scoring thresholds
            # ggplot2::geom_errorbar(data = df_plot_targ
            #                        , ggplot2::aes(group = variable
            #                                       , ymin = betterbio_varval_qLO
            #                                       , ymax = betterbio_varval_qHI)
            #                        , lty = 2
            #                        , lwd = 1
            #                        , color = "black"
            #                        , show.legend = FALSE
            #                        , na.rm = TRUE) +
            # Legend, Points
            ggplot2::scale_color_manual(name="Degraded"
                                        , breaks = c("Yes", "No")
                                        , values = bio_col
                                        , drop = FALSE) +
            ggplot2::scale_fill_manual(name="Degraded"
                                       , breaks = c("Yes", "No")
                                       , values = bio_col
                                       , drop = FALSE) +
            ggplot2::scale_shape_manual(name="Degraded"
                                        , breaks = c("Yes", "No")
                                        , values = bio_shp
                                        , drop = FALSE) +
            ggplot2::scale_size_manual(name = "Degraded"
                                     , breaks = c("Yes", "No")
                                     , values = bio_size
                                     , drop = FALSE)

          # target site, line (no legend - color outside of aes)
          p_SSTV <- p_SSTV + ggplot2::geom_errorbar(data = df_plot_targ
                                                    , ggplot2::aes(group = variable
                                                                   , ymin = value
                                                                   , ymax = value)
                                                    , lty = targ_line_lty
                                                    , lwd = targ_line_lwd
                                                    , color = targ_line_col
                                                    , show.legend = FALSE)

          #
          # print(p_SSTV)
          # # plots.tvr[[tvr]] <- grDevices::recordPlot()
          # plots.tv[[tv]] <- grDevices::recordPlot()
          #
          fn_png <- paste0(TargetSiteID, "_", biocomm, "_VP_"
                           , make.names(SSTV.analyte), ".png")
          if(boo_plot){
            ggplot2::ggsave(file.path(dir_path, fn_png), p_SSTV
                            , width = plot_W, height = plot_H, units = "in")
          }## IF ~ boo_plot ~ END

          varFlag <- 0

          #}##FOR.r.END  # End For loop over responses
          #grDevices::graphics.off()

        }##FOR.tv.END  # End For loop over stressors
        # SSTVfile <- paste("Results/",TargetSiteID, "/", TargetSiteID, ".SSTVCorrs.txt", sep="")
        # utils::write.table(df.CorrTable, file=SSTVfile, sep= "\t",quote=FALSE,
        #                    row.names=FALSE,col.names=TRUE)
      }##IF.stressor.SSTV.END
    }##IF.SSTV.END

    # Create PDF from list
    # fn_pdf <- paste0(TargetSiteID, "_", biocomm, "_VP_AllStressors.pdf")
    # grDevices::pdf(file.path(dir_path, fn_pdf), width=8)
    # for (tv in plots.tv){##FOR.gp.START
    #     #grDevices::replayPlot(g.plot)
    #     if(is.null(tv)==TRUE) {next}
    #     grDevices::replayPlot(tv)
    # }##FOR.gp.END
    # grDevices::dev.off()
    # rm(plots.tv)

  }

}##FUNCTION.END

