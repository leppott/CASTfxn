#' @title Verified Predictions
#' 
#' @description Get verified predictions.
#' 
#' @details 
#' 
#' Required objects:
#' 
#' * data.SampSummary; StationID_Master, CollDate, StressSampID, PhabSampID, BMI.Metrics.SampID, Algae.Metrics.SampID
#' 
#' * dataBioTaxa; BMISampID
#' 
#' * data_stressInfo; SSTV, Analyte, SSTV, SensMin, SensMax, TolMin, TolMax
#' 
#' * data.SSTV.totabund; BMISampID, StationID_Master, StressSampID, SSTV.analyte
#' , SensRelAbund, TolRelAbund, SensTaxa, SampleAbundance, TolTaxa
#' 
#' * TargetSiteID
#' 
#' @param TargetSiteID Site ID
#' @param data.SampSummary x
#' @param dataBioTaxa x
#' @param data_stressInfo x
#' @param data.SSTV.totabund x
#' @param dataMasterTaxa Master Taxa list for biological data
#' @param matchedData matched biological and chemical stressor data.
#' @param ref.sites Vector of reference sites IDs.
#' @param BioIndex_Val Column name for biological index value; list.MatchBioData$site.b.rsp
#' @param BioIndex_Nar Column name for biological index narrative rating; list.MatchBioData$site.b.rsp
#' @param BioIndex_Nar_Deg Biological index degraded narrative text; list.MatchBioData$site.b.rsp
#' @param dir_results Directory to save plots.  Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "VerifiedPredictions"
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' 
#' @return Results text file and jpeg files to "Results" "VerifiedPredictions" folder 
#' in working directory of box plots and a single PDF of all plots.
#' 
# @importFrom pryr "%<a-%"
#' 
#' @examples
#' TargetSiteID <- "SRCKN001.61"
#' dir_results  <- file.path(getwd(), "Results")
#' 
#' # Data getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites          # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.mod           <- data_ReachMod
#' dataMasterTaxa        <- data_BMIMasterTaxa
#' 
#' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
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
#' list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
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
#'                                  , probsHigh, probsLow, biocomm, dir_results
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
#' # dataBioTaxa  <- read.delim(paste(myDir.Data,"data.bmi.taxa.raw.tab",sep=""))
#' # data.SSTV.totabund <- read.delim(paste(myDir.Data,"data.totabund.bySample.tab",sep=""))
#' #
#' # data, example included with package
#' dataBioTaxa  <- data_BMIcounts
#' data.SSTV.totabund <- data_BMIRelAbund
#' BioIndex_Val       <- "IBI"
#' BioIndex_Nar       <- "NarRat"
#' BioIndex_Nar_Deg   <- "Violates"
#' dir_sub            <- "VerifiedPredictions"
#' biocomm <- "bmi"
#' 
#' # Run getVerifiedPredictions
#' getVerifiedPredictions(TargetSiteID
#'                        , data.SampSummary
#'                        , dataBioTaxa
#'                        , data_stressInfo
#'                        , data.SSTV.totabund
#'                        , dataMasterTaxa
#'                        , list.MatchBioData
#'                        , ref.sites
#'                        , BioIndex_Val
#'                        , BioIndex_Nar
#'                        , BioIndex_Nar_Deg
#'                        , dir_results
#'                        , dir_sub)
#~~~~~~~~~~~~~~~~
#' @export
getVerifiedPredictions <- function(TargetSiteID
                                   , SSTVanalytes
                                   , colBioSample
                                   , stressors
                                   , stressorInfo
                                   , dataBioTaxa
                                   , dataMasterTaxa
                                   , matchedData
                                   , BioIndex_Val="IBI"
                                   , BioIndex_Nar="NarRat"
                                   , BioIndex_Nar_Deg="Violates"
                                   , dir_results=file.path(getwd(), "Results")
                                   , dir_sub="VerifiedPredictions"
                                   , biocomm="bmi"
                                   ) {##FUNCTION.START
    
    
    
  # Debugging
  boo.DEBUG <- FALSE
  #
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
      TargetSiteID
      SSTVanalytes = as.character(SSTVparms) # Used
      colBioSample = colBioSample            # Used
      stressors = stressorsWPairedResponses  # Used
      stressorInfo <- siteStressInfo         # Used
      dataBioTaxa = bioTaxaData              # Used
      dataMasterTaxa = bioMasterTaxa         # Used
      matchedData = list_MatchBioData        # Used
      BioIndex_Val = bioIndex                # Used
      BioIndex_Nar = "Quality"               # Used
      BioIndex_Nar_Deg = "Degraded"          # Used
      dir_results=dir_results                # Used
      dir_sub="VerifiedPredictions"          # Used
      biocomm=bioComm                        # Used
      tv <- 1
  }##IF.boo.DEBUG.END
  
  wd <- getwd() #2020-02-05
  
  # define pipe
  `%>%` <- dplyr::`%>%`
  col.Bio.Deg   <- "Bio.Deg"
  # QC, biocomm ####
  biocomm <- toupper(biocomm)
  
  if (exists("keepMTcol")) {rm(keepMTcol)}
  if (exists("deleteSSTVnames")) {rm(deleteSSTVnames)}
  if (exists("mtcols")) {rm(mtcols)}
  
  # Pull only SSTVanalytes that are also in the list of paired stressors (at least one is)
  SSTVanalytes <- SSTVanalytes[SSTVanalytes %in% stressors]

  # Pull stressor names having SSTVs (from stressor metadata) & in paired samp data
  df_SSTV <- stressorInfo %>%
      dplyr::filter(StdParamName %in% SSTVanalytes) %>%
      dplyr::select(StdParamName,SSTV,SSTVname,SensMin,SensMax,TolMin,TolMax)
  df_SSTV <- unique(df_SSTV)
  colnames(df_SSTV)[1] <- "Analyte"

  SSTVnames <- as.vector(unique(df_SSTV$SSTVname))
  # SSTVnames <- as.character(unique(SSTVnames))
  mtcols <- colnames(dataMasterTaxa)
  # Check whether master taxa file contains SSTVname (tol vals for that stressor)
  for (name in 1:length(SSTVnames)) {  # If more than one SSTV, then must iterate
      SSTVlabel <- as.character(stressorInfo$Label[stressorInfo$StdParamName==name])
      
      if (SSTVnames[name] %in% mtcols) {  # Check if TV data in Master Taxa file

          if (exists("keepMTcol")) {
              keepMTcol <- c(keepMTcol, SSTVnames[name])
          } else {
              keepMTcol <- SSTVnames[name]
          }
      } else { 
          # no taxa in MT taxa are assigned tol values for this stressor
          gapcomment <- paste0("No ", biocomm, " taxa have tolerance "
                               , "values for this stressor.")
          gaps <- cbind.data.frame("getVerifiedPredictions", SSTVnames[name], 0
                                   , gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(dir_results, TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          if (exists("deleteSSTVname")) {
              deleteSSTVnames <- c(deleteSSTVnames, SSTVnames[name])
          } else {
              deleteSSTVnames <- SSTVnames[name]
          }
      }
  }
  
  boo.continue = FALSE  # default value; only flips to true if data available
  
  if (exists("deleteSSTVnames")==TRUE) { # Some SSTV stressors not used
      if (all(SSTVnames %in% deleteSSTVnames)) { # No SSTV stressors in master taxa
          gapcomment <- paste0("No stressor-specific tolerance values for "
                               , "potential site stressors exist in the master "
                               , "taxa file for ", biocomm, ".")
          gaps <- cbind.data.frame("getVerifiedPredictions", "No VP data", 0
                                   , gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(dir_results, TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          
          msg <- gapcomment
          message(msg)
          # print(msg)
          # flush.console()
          
          boo.continue = FALSE
      } # NO SSTV stressors are used; exit function cleanly
  }
  
  if (exists("keepMTcol")==TRUE) { # Some stressors have SSTV vals in master taxa file
      keepMTcol <- as.character(keepMTcol)

      df_SSTVtaxa <- dataMasterTaxa %>%
      dplyr::select(FinalID, eval(keepMTcol))

      # Keep taxa with SSTValues, discard those without
      if (length(keepMTcol)==1) {
          msg <- "Got only 1 SSTV stressor!"
          message(msg)
          # print(msg)
          # flush.console()
          df_SSTVtaxa <- df_SSTVtaxa[!is.na(df_SSTVtaxa[,keepMTcol]),]
          
      } else { # Two or more SSTV stressors exist ### NOT TESTED WITH DATA
          msg <- "Got 2 or more SSTV stressors!"
          message(msg)
          # print(msg)
          # flush.console()
          msg <- keepMTcol
          message(msg)
          # print(msg)
          # flush.console()
          df_SSTVtaxa <- df_SSTVtaxa[rowSums(!is.na(df_SSTVtaxa[,-1]))>=1,]
          
      }
          
      SSTVtaxanames <- unique(as.character(df_SSTVtaxa$FinalID))
      reportedtaxa <- unique(as.vector(dataBioTaxa$FinalID))

      if (any(reportedtaxa %in% SSTVtaxanames)==TRUE) {
          df_SSTVrelabund <- dataBioTaxa %>%
              dplyr::select(eval(colBioSample), FinalID, RelAbund) %>% 
              dplyr::filter(FinalID %in% SSTVtaxanames)
          boo.continue = TRUE
          
      } else {
          gapcomment <- paste0("No stressor-specific tolerance values for "
                               , "potential site stressors exist in the master "
                               , "taxa file for ", biocomm, ".")
          gaps <- cbind.data.frame("getVerifiedPredictions", "No VP data", 0
                                   , gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(dir_results, TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")
          
          msg <- gapcomment
          message(msg)
          # print(msg)
          # flush.console()          
          boo.continue = FALSE
      }
  } else {
      boo.continue = FALSE
  }
      
  if (boo.continue == TRUE) { # Have
      # check for and create (if necessary) "Results" subdirectory of working directory
      wd <- dirname(dir_results)
      dir.sub <- basename(dir_results)
      dir.sub2 <- TargetSiteID
      dir.sub3 <- biocomm
      dir.sub4 <- dir_sub
      ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
             , dir.create(file.path(wd, dir.sub, dir.sub2))
             , FALSE)
      ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3))==TRUE
             , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3))
             , FALSE)
      ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4))==TRUE
             , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4))
             , FALSE)
      dir_path <- file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4)
      
      # 20190513, remove scores file if exists
      fn_scores <-  file.path(dir_path, paste0(TargetSiteID, "_", biocomm
                                               , "_VP_Scores.tab"))
      if(file.exists(fn_scores)){file.remove(fn_scores)}        
      
      # plots.tvr <- vector(10, mode="list")
      plots.tv <- vector(10, mode="list")
      ppi<-300
      plot_H <- 4
      plot_W <- 9
      
      # boo.pryr <- FALSE

      # Target Site Bio Scores
      targ_bio <- matchedData$site.b.rsp[, BioIndex_Val]
      targ_bio_bad <- matchedData$site.b.rsp[matchedData$site.b.rsp[
          , BioIndex_Nar]==BioIndex_Nar_Deg, BioIndex_Val]
      targ_bio_good <- matchedData$site.b.rsp[matchedData$site.b.rsp[
          , BioIndex_Nar]!=BioIndex_Nar_Deg, BioIndex_Val]
      targ_bio_min <- min(targ_bio, na.rm=TRUE)
      targ_bio_max <- max(targ_bio, na.rm=TRUE)
      
      # skip to next if no "bad" bio scores for this site
      msg_stop_NoBadBio <- paste0("There are no '", BioIndex_Nar_Deg
                                  , "' bio sites for comparison for this site.")
      # Use minimum good or maximum bad for "better than" threshold
      if(length(targ_bio_bad)==0){
          targ_bio_good_min <- min(targ_bio_good, na.rm=TRUE)
          targ_bio_good_max <- max(targ_bio_good, na.rm=TRUE)
          # bio threshold to use for "better"
          bio_better_thresh <- targ_bio_good_min
      } else {
          targ_bio_bad_min <- min(targ_bio_bad, na.rm=TRUE)
          targ_bio_bad_max <- max(targ_bio_bad, na.rm=TRUE)
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
                  SSTV.label <- stressorInfo$Label[stressorInfo$Analyte==SSTV.analyte]
                  SSTV.label <- unique(as.character(SSTV.label))
                  
                  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
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
                  if(is.na(SSTV.name)==TRUE | SSTV.name==""){
                      msg <- "No data; SKIP"
                      message(msg)
                      # print(msg)
                      # utils::flush.console()
                      next
                  }
                  
                  # 20190111, get LogTransf (0 = FALSE; 1 = TRUE)
                  # need to use max (default of 1) in case of duplicates
                  chem.info_LogTransf <- stressorInfo %>% 
                      dplyr::group_by(StdParamName) %>% 
                      dplyr::summarise(max_LogTransf=max(LogTransf, na.rm=TRUE))
                  LogTransf <- chem.info_LogTransf[chem.info_LogTransf[,"StdParamName"]==SSTV.analyte
                                                   , "max_LogTransf"]
                  LogTransf <- ifelse(is.na(LogTransf), "TRUE", as.logical(LogTransf))
                  log.yn <- LogTransf
                  
                  # get all the matched sample data for this stressor
                  # 20180620, match names
                  col_keep <- c("StationID_Master", "StressSampID", "RespSampID")
                  SSTV.analyte.match.all.b.str <- SSTV.analyte[SSTV.analyte %in% names(matchedData$all.b.str)]
                  all.match.b.str <- matchedData$all.b.str[,c(col_keep, SSTV.analyte.match.all.b.str)]
                  cl.match.b <- matchedData$cl.b.str[,c(col_keep, SSTV.analyte.match.all.b.str)]
                  
                  bmi.taxa.raw <- dataBioTaxa[dataBioTaxa$StationID_Master %in% 
                                                  unique(all.match.b.str$StationID_Master),]
                  bmi.taxa.raw <- merge(bmi.taxa.raw, dataMasterTaxa[,c("FinalID", SSTV.name)], 
                                        by.x = "FinalID", by.y = "FinalID")
                  
                  minTolVal <- min(dataMasterTaxa[,SSTV.name], na.rm = TRUE)
                  maxTolVal <- max(dataMasterTaxa[,SSTV.name], na.rm = TRUE)
                  
                  bmi.taxa.raw$SensTaxa <- ifelse(bmi.taxa.raw[,SSTV.name]==minTolVal | 
                                                      bmi.taxa.raw[,SSTV.name]==minTolVal+1, 
                                                  bmi.taxa.raw$RelAbund, NA)
                  
                  bmi.taxa.raw$TolTaxa <- ifelse(bmi.taxa.raw[,SSTV.name]==maxTolVal |
                                                     bmi.taxa.raw[,SSTV.name]==maxTolVal-1, 
                                                 bmi.taxa.raw$RelAbund, NA)
                  
                  bmi.taxa.raw <- dplyr::group_by(bmi.taxa.raw, StationID_Master
                                                  , BMISampID) %>%
                      dplyr::summarize(SensRelAbund = sum(SensTaxa, na.rm = TRUE)
                                       , TolRelAbund = sum(TolTaxa, na.rm = TRUE))
                  bmi.taxa.raw <- dplyr::rename(bmi.taxa.raw, RespSampID = BMISampID)
                  
                  all.match.b.resp <- bmi.taxa.raw[bmi.taxa.raw$RespSampID %in%
                                                       unique(all.match.b.str$RespSampID), ]
                  
                  col_by <- c("StationID_Master", "RespSampID")
                  all.SSTV.abund <- merge(all.match.b.str
                                          , all.match.b.resp
                                          , by.x = col_by
                                          , by.y = col_by
                                          , all = TRUE)
                  
                  # Add Bio Index (value and Narrative Rating) (20190305)
                  all.SSTV.abund <- merge(all.SSTV.abund
                                          , matchedData$all.b.rsp[, c(col_by
                                                                      , BioIndex_Nar
                                                                      , BioIndex_Val)]
                                          , by.x = col_by
                                          , by.y = col_by
                                          , all.x = TRUE)
                  
                  good.SSTV.abund    <- all.SSTV.abund[stats::complete.cases(all.SSTV.abund),]
                  cl.SSTV.abund      <- subset(good.SSTV.abund, good.SSTV.abund$StressSampID %in% cl.match.b$StressSampID)
                  site.SSTV.abund    <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% TargetSiteID)
                  SSTV.Resp          <- c("SensRelAbund", "TolRelAbund")
                  
                  varFlag <- 1
                  
                  
                  # Generate data for plotting (1 = all complete cases, 3 = comparators
                  # 5 = target site)
                  df.plot1 <- good.SSTV.abund[,c(SSTV.analyte, SSTV.Resp)]
                  df.plot3 <- cl.SSTV.abund[,c(SSTV.analyte, SSTV.Resp)]
                  df.plot5 <- site.SSTV.abund[,c(SSTV.analyte, SSTV.Resp)]
                  
                  # Log transform if indicated
                  if (log.yn == TRUE) {
                      df.plot1[, SSTV.analyte] <- log10(df.plot1[, SSTV.analyte])
                      df.plot3[, SSTV.analyte] <- log10(df.plot3[, SSTV.analyte])
                      df.plot5[, SSTV.analyte] <- log10(df.plot5[, SSTV.analyte])
                  }
                  
                  # There should never be a case where either x or y are always NA for all data
                  # WHAT DOES THIS DO???
                  if (length(df.plot1) > 0) {
                      # graphics::plot(df.plot1[,2]~df.plot1[,1],main=varMain, 
                      #      xlab=varxlab,ylab=respText, col="darkgrey", 
                      #      pch=1, cex = 0.8, cex.lab=0.6, cex.main = 0.8, 
                      #      font.main = 2, font.lab = 2, mar = c(6,4,4,2)+0.1)
                  } else {
                      next
                  }
                  
                  # 20190305, drop added Bio Index value and narrative
                  df_plot_all <- reshape2::melt(good.SSTV.abund[, 1:6]
                                                , id.vars=colnames(good.SSTV.abund)[1:4])
                  #df_plot_all$SSTV.analyte <- df_plot_all[, SSTV.analyte]
                  df_plot_all[, "Param_Name"] <- SSTV.analyte
                  colnames(df_plot_all)[colnames(df_plot_all)==SSTV.analyte] <- "Param_Value"
                  df_plot_all <- df_plot_all[, c(1:3,7,4:6)]
                  levels(df_plot_all$variable) <- c("Sensitive Taxa", "Tolerant Taxa")
                  
                  # 20190305, switch to "better" bio from all
                  df_plot_betterbio <- good.SSTV.abund[good.SSTV.abund[, BioIndex_Val] > bio_better_thresh, 1:6]
                  df_plot_betterbio <- reshape2::melt(df_plot_betterbio
                                                      , id.vars=colnames(df_plot_betterbio)[1:4])
                  #df_plot_betterbio$SSTV.analyte <- df_plot_betterbio[, SSTV.analyte]
                  df_plot_betterbio[, "Param_Name"] <- SSTV.analyte
                  colnames(df_plot_betterbio)[colnames(df_plot_betterbio)==SSTV.analyte] <- "Param_Value"
                  df_plot_betterbio <- df_plot_betterbio[, c(1:3,7,4:6)]
                  levels(df_plot_betterbio$variable) <- c("Sensitive Taxa", "Tolerant Taxa")
                  n_records_better_bio <- nrow(df_plot_betterbio)
                  
                  df_plot_targ <- reshape2::melt(site.SSTV.abund[, 1:6]
                                                 , id.vars=colnames(site.SSTV.abund)[1:4])
                  #df_plot_targ$SSTV.analyte <- df_plot_targ[, SSTV.analyte]
                  # chem var and value to columns (20190513)
                  df_plot_targ[, "Param_Name"] <- SSTV.analyte
                  colnames(df_plot_targ)[colnames(df_plot_targ)==SSTV.analyte] <- "Param_Value"
                  df_plot_targ <- df_plot_targ[, c(1:3,7,4:6)]
                  levels(df_plot_targ$variable) <- c("Sensitive Taxa", "Tolerant Taxa")
                  # factors by default are alphebetical so should be ok that every plot will be in the same order
                  
                  # 20190510, new data frame for better sites AND bio.deg = No
                  # IBI scores (drop variable and value from good.SSTV.abund)
                  df_IBI <- unique(good.SSTV.abund[, c(1:4,7:8)])
                  # Add IBI scores to "better" sites
                  df_plot_betterbio_IBI <- merge(df_plot_betterbio, df_IBI, all.x=TRUE)
                  # Add Bio.Deg
                  df_plot_betterbio_IBI[, col.Bio.Deg] <- ifelse(df_plot_betterbio_IBI[, BioIndex_Nar] == BioIndex_Nar_Deg
                                                                 , "Yes", "No")
                  df_plot_betterbio_BioDegNo <- df_plot_betterbio_IBI[df_plot_betterbio_IBI[, col.Bio.Deg] == "No", ]
                  n_records_betterbio_BioDegNo <- nrow(df_plot_betterbio_BioDegNo)
                  
                  
                  # Scoring ####
                  # Get percentiles by taxa group
                  myProbs <- c(10, 20, 25, 50, 75, 80, 90)*0.01
                  df_quantiles <- aggregate(value ~ variable, data=df_plot_betterbio
                                            , FUN = function(x) {quantile(x, probs=myProbs, na.rm=TRUE)})
                  q_Sens_lo <- as.vector(df_quantiles[df_quantiles[, 1]=="Sensitive Taxa"
                                                      , "value"][, "25%"])
                  q_Sens_hi <- as.vector(df_quantiles[df_quantiles[, 1]=="Sensitive Taxa"
                                                      , "value"][, "50%"])
                  q_Tol_lo <- as.vector(df_quantiles[df_quantiles[, 1]=="Tolerant Taxa"
                                                     , "value"][, "50%"])
                  q_Tol_hi <- as.vector(df_quantiles[df_quantiles[, 1]=="Tolerant Taxa"
                                                     , "value"][, "75%"])
                  # Add scoring thresholds to target siteID data frame
                  df_plot_targ[, paste0("betterbio_varval_q", c("LO", "HI"))] <- NA
                  df_plot_targ[df_plot_targ[, "variable"]=="Sensitive Taxa"
                               , "betterbio_varval_qLO"] <- q_Sens_lo
                  df_plot_targ[df_plot_targ[, "variable"]=="Sensitive Taxa"
                               , "betterbio_varval_qHI"] <- q_Sens_hi
                  df_plot_targ[df_plot_targ[, "variable"]=="Tolerant Taxa"
                               , "betterbio_varval_qLO"] <- q_Tol_lo
                  df_plot_targ[df_plot_targ[, "variable"]=="Tolerant Taxa"
                               , "betterbio_varval_qHI"] <- q_Tol_hi
                  # Scoring (tolerant than flip for sensitive)
                  df_plot_targ[, "Score"] <- ifelse(df_plot_targ[, "value"] > df_plot_targ[, "betterbio_varval_qHI"], 1
                                                    , ifelse(df_plot_targ[, "value"] < df_plot_targ[, "betterbio_varval_qLO"], -1, 0))
                  df_plot_targ[df_plot_targ[, "variable"]=="Sensitive Taxa", "Score"]  <- -1 * df_plot_targ[df_plot_targ[, "variable"]=="Sensitive Taxa", "Score"]
                  
                  # Add other variables
                  df_plot_targ[, "biocomm"] <- biocomm
                  df_plot_targ[, "n_BetterBio"] <- n_records_better_bio
                  df_plot_targ[, "n_BetterBioDegNo"] <- n_records_betterbio_BioDegNo
                  df_tbl_scores <- merge(df_plot_targ
                                         , site.SSTV.abund[,c("RespSampID"
                                                              ,"StressSampID"
                                                              ,"CSCI", "Quality")]
                                         , by.x = c("RespSampID","StressSampID")
                                         , by.y = c("RespSampID","StressSampID")
                                         , all.x = TRUE)
                  df_tbl_scores <- merge(df_tbl_scores
                                         , unique(stressorInfo[,c("Analyte", "Label")])
                                         , by.x = "Param_Name"
                                         , by.y = "Analyte"
                                         , all.x = TRUE)
                  df_tbl_scores <- dplyr::select(df_tbl_scores, StationID_Master
                                                  , RespSampID, eval(BioIndex_Val)
                                                  , Quality, StressSampID, Label
                                                  , Param_Name, Param_Value
                                                  , variable, value, betterbio_varval_qLO
                                                  , betterbio_varval_qHI, Score
                                                  , biocomm, n_BetterBio
                                                  , n_BetterBioDegNo) %>%
                      dplyr::rename(Stressor = Param_Name
                                    , StressorValue = Param_Value
                                    , Response = variable
                                    , ResponseValue = value
                                    , qLoValue_Cutoff = betterbio_varval_qLO
                                    , qHiValue_Cutoff = betterbio_varval_qHI)

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
                  
                  utils::write.table(df_tbl_scores, file=fn_scores
                                     , col.names = boo_colnames, row.names=FALSE, sep="\t"
                                     , append=boo_append)
                  
                  
                  
                  
                  # ggplot ####
                  
                  {##PLOT VARIABLES ~ START
                      ## Plot, Variables, Strings
                      str_title <- paste0(TargetSiteID, ": Verified prediction "
                                          ,"line of evidence for ", SSTV.label)
                      str_title <- stringr::str_wrap(str_title,100)
                      str_subtitle <- paste0("Do the data support the prediction "
                                             , " that the abundance of sensitive"
                                             , " taxa will be lower and tolerant"
                                             , " taxa will be higher than observed at "
                                             , " comparator sites with better biology?")
                      str_subtitle <- stringr::str_wrap(str_subtitle, 100)
                      str_xlab  <- ""
                      str_ylab  <- "Relative Abundance"
                      df_plot_targ_sortvalue <- df_plot_targ[order(df_plot_targ[,"value"]), ]
                      str_score_sens <- paste(df_plot_targ_sortvalue[df_plot_targ_sortvalue[
                          , "variable"]=="Sensitive Taxa", "Score"], collapse=", ")
                      str_score_tol <- paste(df_plot_targ_sortvalue[df_plot_targ_sortvalue[
                          , "variable"]=="Tolerant Taxa", "Score"], collapse=", ")
                      str_caption <- paste0("Score = Tolerant Taxa (", str_score_tol
                                            , "), Sensitive Taxa ("
                                            , str_score_sens
                                            , ")\nNumber of samples with better biology (n="
                                            , n_records_better_bio
                                            , "); better biology and not degraded (n="
                                            , n_records_betterbio_BioDegNo, ")"
                                            , "\nSamples with better biology have "
                                            , BioIndex_Val, " > "
                                            , signif(bio_better_thresh, 3))
                      
                      ## Plot, Variables, Colors
                      # col_sites_all     <- "dark gray"
                      # col_sites_all_ref <- "blue"
                      # col_sites_cl      <- "cyan3"
                      # col_sites_cl_ref  <- col_sites_all_ref
                      col_sites_targ    <- "red"
                      # col_line          <- "black"
                      
                      ## Plot, Variables, Fill
                      # fill_sites_all     <- col_sites_all
                      # fill_sites_all_ref <- fill_sites_all
                      # fill_sites_cl      <- col_sites_cl
                      # fill_sites_cl_ref  <- fill_sites_cl 
                      fill_sites_targ    <- col_sites_targ
                      
                      ## Plot, Variables, Points
                      # pch_sites_all     <- 19 # solid circle
                      # pch_sites_all_ref <- 21 # circle outline
                      # pch_sites_cl      <- 19
                      # pch_sites_cl_ref  <- pch_sites_all_ref
                      pch_sites_targ    <- 17 # triangle
                      
                      ## Plot, Variables, Sizes
                      cex_mod <- 2
                      # cex_sites_all     <- 1 #cex_mod*0.3
                      # cex_sites_all_ref <- cex_sites_all
                      # cex_sites_cl      <- cex_mod*0.95
                      # cex_sites_cl_ref  <- cex_sites_cl
                      cex_sites_targ    <- cex_mod*1.2
                      
                      ## Plot, Variables, Target Site Line
                      targ_line_col <- col_sites_targ
                      targ_line_lty <- 2
                      targ_line_lwd <- 1
                      
                      ## Plot, Variables, Legend
                      leg_name   <- "Sites"
                      # leg_labels <- c("all", "all ref", "cluster", "cluster ref", "target")
                      # leg_shape  <- c(pch_sites_all, pch_sites_all_ref, pch_sites_cl, pch_sites_cl_ref, pch_sites_targ)
                      # leg_col    <- c(col_sites_all, col_sites_all_ref, col_sites_cl, col_sites_cl_ref, col_sites_targ)
                      # leg_fill   <- c(fill_sites_all, fill_sites_all_ref, fill_sites_cl, fill_sites_cl_ref, fill_sites_targ)
                      leg_labels <- c("target")
                      leg_shape  <- c(pch_sites_targ)
                      leg_col    <- c(col_sites_targ)
                      leg_fill   <- c(fill_sites_targ)
                      
                  }##PLOT VARIABLES ~ END
                  
                  ## Plot, Variables, Bio.Deg
                  bio_col <- c("blue", "dark gray")
                  bio_shp <- c(21, 25) # circle and down triangle
                  # col.Bio.Deg   <- "Bio.Deg"
                  col.SiteTypeQuality <- col.Bio.Deg
                  
                  display_target <- "lines"  # "lines", "points"
                  
                  p_SSTV <- ggplot2::ggplot(df_plot_betterbio, ggplot2::aes(variable, value)) + 
                      ggplot2::geom_boxplot(ggplot2::aes(group = variable)) + 
                      ggplot2::labs(title = str_title
                                    , subtitle = str_subtitle
                                    , y = str_ylab
                                    , caption = str_caption) +
                      ggplot2::theme_bw() +
                      ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)
                                     , plot.subtitle = ggplot2::element_text(hjust=0.5)
                                     , axis.title.y = ggplot2::element_blank()) +
                      ggplot2::coord_flip() + 
                      # Add degraded y/n for better bio sites
                      ggplot2::geom_jitter(data=df_plot_betterbio_IBI
                                           , size = 1
                                           , alpha = 0.45
                                           , na.rm = TRUE
                                           , ggplot2::aes_string(color = col.SiteTypeQuality
                                                                 , shape = col.SiteTypeQuality
                                                                 , fill = col.SiteTypeQuality)) +
                      # redo box with no fill (can't change alpha of just the box if do 2nd and want to keep gray background)
                      ggplot2::geom_boxplot(fill=NA, ggplot2::aes(group=variable)) + 
                      # scoring thresholds
                      ggplot2::geom_errorbar(data=df_plot_targ
                                             , ggplot2::aes(group = variable
                                                            , ymin = betterbio_varval_qLO
                                                            , ymax = betterbio_varval_qHI
                                             )
                                             , lty = 2
                                             , lwd = 1
                                             , color = "black"
                                             , show.legend = FALSE
                                             , na.rm = TRUE) +
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
                                                  , drop = FALSE)
                  
                  # target site, line (no legend - color outside of aes)
                  p_SSTV <- p_SSTV + ggplot2::geom_errorbar(data = df_plot_targ
                                                            , ggplot2::aes(group = variable
                                                                           , ymin = value
                                                                           , ymax = value
                                                            )
                                                            , lty=targ_line_lty
                                                            , lwd=targ_line_lwd
                                                            , color=targ_line_col
                                                            , show.legend = FALSE)
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  # target site, points or line
                  #  if(display_target=="points"){##IF~display_target~START
                  #    p_SSTV <- p_SSTV + ggplot2::geom_jitter(data=df_plot_targ
                  #                      , ggplot2::aes(group=variable, y=value, color="target"
                  #                                     , shape="target", fill="target"), size=2, width=0.1)
                  #      # ggplot2::scale_shape_manual(name=leg_name, labels=leg_labels, values=leg_shape)  + 
                  #      # ggplot2::scale_color_manual(name=leg_name, labels=leg_labels, values=leg_col) + 
                  #      # ggplot2::scale_fill_manual(name=leg_name, labels=leg_labels, values=leg_fill)
                  # ###*****Needs work but since hard coded to lines is ok *****
                  #       } else if(display_target=="lines"){
                  #    p_SSTV <- p_SSTV + ggplot2::geom_errorbar(data=df_plot_targ
                  #                      , ggplot2::aes(group=variable, ymin=value, ymax=value
                  #                                     , color=targ_line_col)
                  #                      , lty=targ_line_lty, lwd=targ_line_lwd)# +
                  #      #ggplot2::scale_color_manual(name=leg_name, labels=leg_labels, values=targ_line_col)
                  #      # ggplot2::scale_shape_manual(breaks=c("Yes", "No"), values=bio_shp, drop=FALSE) + 
                  #      # ggplot2::scale_color_manual(values=c("blue", "red", "gray")
                  #      #                             , guide=guide_legend(override.aes = list()))
                  #    
                  #    # Legend off
                  #   #p_SSTV +  theme(legend.position = "none")
                  #    
                  #    
                  #    p_SSTV + scale_color_manual(name="LegCol", values=c("gray", "red", "blue")) +
                  #             scale_shape_manual(name="LegShp", values=bio_shp)
                  #    
                  #    p_SSTV + scale_shape_manual(name="LegShp", values=bio_shp)
                  #    
                  #    
                  #    
                  #    
                  #    p_SSTV + scale_color_manual(name="myLegend", values=c("blue", "red", "gray")
                  #                                , guide=TRUE
                  #                                )
                  #    
                  #    
                  #    p_SSTV <- p_SSTV + scale_color_manual(name="Sites", values=c("blue", "red")
                  #                       , guide=guide_legend(override.aes = list(
                  #                         linetype=c("blank", "dashed", "blank")
                  #                         , shape=c(21, NA, 25))))
                  #  }##IF~display_target~START
                  # 
                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  
                  #
                  print(p_SSTV)
                  # plots.tvr[[tvr]] <- grDevices::recordPlot()
                  plots.tv[[tv]] <- grDevices::recordPlot()
                  #
                  fn_png <- paste0(TargetSiteID, "_", biocomm, "_VP_", make.names(SSTV.analyte), ".png")
                  ggplot2::ggsave(file.path(dir_path, fn_png), p_SSTV, width=plot_W, height=plot_H, units="in")
                  
                  
                  #~~~~~~~~~~old code~~~~~~~~~~~~~~~~~
                  
                  
                  
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
      fn_pdf <- paste0(TargetSiteID, "_", biocomm, "_VP_AllStressors.pdf")
      grDevices::pdf(file.path(dir_path, fn_pdf), width=8)
      for (tv in plots.tv){##FOR.gp.START
          #grDevices::replayPlot(g.plot)
          if(is.null(tv)==TRUE) {next}
          grDevices::replayPlot(tv)
      }##FOR.gp.END
      grDevices::dev.off()
      rm(plots.tv)
      
  }
  
}##FUNCTION.END

