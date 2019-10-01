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
#' * data.bio.taxa.raw; BMI.Metrics.SampID
#' 
#' * data.chem.info; SSTV, Analyte, SSTV, SensMin, SensMax, TolMin, TolMax
#' 
#' * data.SSTV.totabund; BMI.Metrics.SampID, StationID_Master, StressSampID, SSTV.analyte
#' , SensRelAbund, TolRelAbund, SensTaxa, SampleAbundance, TolTaxa
#' 
#' * TargetSiteID
#' 
#' @param TargetSiteID Site ID
#' @param data.SampSummary x
#' @param data.bio.taxa.raw x
#' @param data.chem.info x
#' @param data.SSTV.totabund x
#' @param data.MT.bio Master Taxa list for biological data
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
#' data.MT.bio        <- data_BMIMasterTaxa
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
#' data.chem.info <- data_ChemInfo
#' site.COMID     <- list.SiteSummary$COMID
#' site.Clusters  <- list.SiteSummary$ClustIDs
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
#'                                 , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
#'                                 , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
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
#' # data.bio.taxa.raw  <- read.delim(paste(myDir.Data,"data.bmi.taxa.raw.tab",sep=""))
#' # data.SSTV.totabund <- read.delim(paste(myDir.Data,"data.totabund.bySample.tab",sep=""))
#' #
#' # data, example included with package
#' data.bio.taxa.raw  <- data_BMIcounts
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
#'                        , data.bio.taxa.raw
#'                        , data.chem.info
#'                        , data.SSTV.totabund
#'                        , data.MT.bio
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
                                   , data.bio.taxa.raw
                                   , data.chem.info
                                   , data.SSTV.totabund
                                   , data.MT.bio
                                   , matchedData
                                   , ref.sites
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
  col.Bio.Deg   <- "Bio.Deg"
  #
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    matchedData <- list.MatchBioData
    tv <- 1
  }##IF.boo.DEBUG.END
  
  # QC, biocomm ####
  biocomm <- tolower(biocomm)
  # Check for no data
  if(biocomm=="bmi"){##IF.biocomm.START
    #
    #
  } else if(biocomm=="algae"){
    #
    #
  } else {
    # Non Valid biological community
    Msg_Stop <- print(paste0("Non-valid biological community specified ("
                    , biocomm,"). Only values of 'bmi' and 'algae' are valid."))
    stop(Msg_Stop)
  }##IF.biocomm.END
  
  
  # Extra, 20181211
  ## Add RelAbundInds to data.bmi.raw
  # col.by <- c("BMI.Metrics.SampID", "FinalID")
  # data.bio.taxa.raw <- merge(data.bio.taxa.raw
  #                            , data.SSTV.totabund[, c(col.by, "RelAbundInds")]
  #                            , by=col.by
  #                            , all.x=TRUE)
  # 20190304, remove, data.bio.taxa.raw now has "RelAbundInds"
  
  # check for and create (if necessary) "Results" subdirectory of working directory
  # wd <- getwd()
  # dir.sub <- "Results"
  wd <- dirname(dir_results)
  dir.sub <- basename(dir_results)
  dir.sub2 <- TargetSiteID
  dir.sub3 <- dir_sub
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3))
         , FALSE)
  
  # 20190513, remove scores file if exists
  fn_scores <-  file.path(dir.sub, dir.sub2, dir.sub3
                          , paste0(TargetSiteID, ".VP.", biocomm, ".Scores.txt"))
  if(file.exists(fn_scores)){file.remove(fn_scores)}
  
  #
  # Comment out, 20190423, when remove varLegLoc as input
  # helper
  # RegPlotSet <- getRegPlotSet(varLegLoc)
  # varInset   <- RegPlotSet[1]
  # varSpacer  <- RegPlotSet[2]
  # varLegOpp  <- RegPlotSet[3]
  
  df.SSTV <- subset(data.chem.info, SSTV != 0 & !is.na(SSTV) & SSTV!=""
                    , c("StdParamName", "SSTV", "SensMin", "SensMax", "TolMin", "TolMax"))
  # duplicate entry, use unique to limit
  df.SSTV <- unique(df.SSTV)
  colnames(df.SSTV)[1] <- "Analyte"

  if (nrow(list.SiteSummary$BMImetrics)==0) {
    # No BMI Responses Found
    print(paste0("No biological response data available for ", TargetSiteID, 
                 ". Regression data illustrate cluster relationships only."))
    utils::flush.console()
  }
  
  # boo.pryr <- FALSE
  
  # plots.tvr <- vector(10, mode="list")
  plots.tv <- vector(10, mode="list")
  ppi<-300
  varFileOut = paste0("Results/",TargetSiteID,"/", dir.sub3, "/", TargetSiteID,".SR.SSTV.")
  plot_H <- 4
  plot_W <- 9
  
  fn_SSTVfile <- paste0(TargetSiteID, ".SR.SSTV.Corrs.txt")
  boo.file.exists <- file.exists(file.path(wd, dir.sub, dir.sub2, fn_SSTVfile))
  if(boo.file.exists){
    file.remove(file.path(wd, dir.sub, dir.sub2, dir.sub3, fn_SSTVfile))
  }
  
  
  # Target Site Bio Scores
  targ_bio <- matchedData$site.b.rsp[, BioIndex_Val]
  targ_bio_bad <- matchedData$site.b.rsp[matchedData$site.b.rsp[, BioIndex_Nar]==BioIndex_Nar_Deg, BioIndex_Val]
  targ_bio_min <- min(targ_bio, na.rm=TRUE)
  targ_bio_max <- max(targ_bio, na.rm=TRUE)
  
  # skip to next if no "bad" bio scores for this site
  msg_stop_NoBadBio <- "There are no 'worse' bio sites for comparison for this site.  Quitting analysis."
  if(length(targ_bio_bad)==0){
    #next
    stop(msg_stop_NoBadBio)
  }
  targ_bio_bad_min <- min(targ_bio_bad, na.rm=TRUE)
  targ_bio_bad_max <- max(targ_bio_bad, na.rm=TRUE)
  # bio threshold to use for "better"
  bio_better_thresh <- targ_bio_bad_max
  # skip to next if no "bad" bio scores for this site
  if(is.na(bio_better_thresh)){
    #next
    stop(msg_stop_NoBadBio)
  }
  
  
  # IF ####
  if (nrow(df.SSTV) != 0) {##IF.SSTV.START
    #
    stressor.SSTV <- subset(df.SSTV, Analyte %in% stressors)
    
    tv.len <- nrow(stressor.SSTV)
    
    #
    if (nrow(stressor.SSTV) != 0) {##IF.stressor.SSTV.START
      #
      # Loop tv (stressor) ####
      for (tv in 1:nrow(stressor.SSTV)) {##FOR.tv.START
      # Currently only valid for SpecCond
        #
        SSTV.analyte <- as.vector(stressor.SSTV$Analyte)[tv]
        SSTV.name <- as.vector(stressor.SSTV$SSTV)[tv]
        
        if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
          varFlag <- 0
          #if(tv==1){tv=20}
        }##IF.boo.DEBUG.END
        #
        
        tv.len <- nrow(stressor.SSTV)
        print(paste0("Item (", tv, "/", tv.len,"); Stressor = ", SSTV.analyte))
        utils::flush.console()
        
        # skip if SSTV = ""
        ## 20181211
        if(is.na(SSTV.name)==TRUE | SSTV.name==""){
          print("No data; SKIP")
          utils::flush.console()
          next
        }
        
        # 20190111, get LogTransf (mod for single parameter)
        # LogTransf ####
        # 20190110, get log transformation code from chem.info
        # define pipe
        `%>%` <- dplyr::`%>%`
        #x <- unique(chem.info[chem.info$StdParamName %in% stressorlist, c("StdParamName", "LogTransf")])
        # need to use max (default of 1) in case of duplicates
        chem.info_LogTransf <- chem.info %>% 
          dplyr::group_by(StdParamName) %>% 
          dplyr::summarise(max_LogTransf=max(LogTransf, na.rm=TRUE))
        LogTransf <- chem.info_LogTransf[chem.info_LogTransf[,"StdParamName"]==SSTV.analyte, "max_LogTransf"]
        LogTransf <- ifelse(is.na(LogTransf), "TRUE", as.logical(LogTransf))
        # # 20180620, more than one (add sum)
        # if (sum(SSTV.analyte %in% c("DO_uf_mg_L", "pH_SU", "Temp_degC", "Flow_cfs",
        #                             "Flow_calc_cfs"))>0) {
        #   log.yn <- FALSE
        # } else {
        #   log.yn <- TRUE
        # }
        log.yn <- LogTransf
        
        # get all the matched sample data for this stressor
        # 20180620, match names
        col_keep <- c("StationID_Master", "StressSampID", "RespSampID")
        SSTV.analyte.match.all.b.str <- SSTV.analyte[SSTV.analyte %in% names(matchedData$all.b.str)]
        all.match.b.str <- matchedData$all.b.str[,c(col_keep, SSTV.analyte.match.all.b.str)]
        cl.match.b <- matchedData$cl.b.str[,c(col_keep, SSTV.analyte.match.all.b.str)]
        
        bmi.taxa.raw <- data.bio.taxa.raw[data.bio.taxa.raw$StationID_Master %in% 
                                              unique(all.match.b.str$StationID_Master),]
        bmi.taxa.raw <- merge(bmi.taxa.raw, data.MT.bio[,c("GenusFinal", 
                                "FinalID", SSTV.name)], 
                                by.x = "FinalID", by.y = "FinalID")

        minTolVal <- min(data.MT.bio[,SSTV.name], na.rm = TRUE)
        maxTolVal <- max(data.MT.bio[,SSTV.name], na.rm = TRUE)
        
        bmi.taxa.raw$SensTaxa <- ifelse(bmi.taxa.raw[,SSTV.name]==minTolVal | 
                                        bmi.taxa.raw[,SSTV.name]==minTolVal+1, 
                                        bmi.taxa.raw$RelAbundInds, NA)

        bmi.taxa.raw$TolTaxa <- ifelse(bmi.taxa.raw[,SSTV.name]==maxTolVal |
                                        bmi.taxa.raw[,SSTV.name]==maxTolVal-1, 
                                        bmi.taxa.raw$RelAbundInds, NA)

        # bmi.taxa.raw2 <- dplyr::group_by(bmi.taxa.raw, StationID_Master, BMI.Metrics.SampID)
        # bmi.taxa.raw2 <- dplyr::summarize(bmi.taxa.raw2, 
        #                                   SensRelAbund = sum(SensTaxa, na.rm = TRUE), 
        #                                   TolRelAbund = sum(TolTaxa, na.rm = TRUE))
        bmi.taxa.raw2 <- dplyr::group_by(bmi.taxa.raw, StationID_Master, BMI.Metrics.SampID) %>%
                                         dplyr::summarize(SensRelAbund = sum(SensTaxa, na.rm = TRUE)
                                                          , TolRelAbund = sum(TolTaxa, na.rm = TRUE))
        
        all.match.b.resp <- bmi.taxa.raw2[bmi.taxa.raw2$BMI.Metrics.SampID %in%
                                    unique(all.match.b.str$BMI.Metrics.SampID), ]
        
        col_by <- c("StationID_Master", "RespSampID")
        all.SSTV.abund <- merge(all.match.b.str
                                , all.match.b.resp
                                , by.x = col_by
                                , by.y = col_by
                                , all = TRUE)
        
        # Add Bio Index (value and Narrative Rating) (20190305)
        all.SSTV.abund <- merge(all.SSTV.abund
                                , matchedData$all.b.rsp[, c(col_by, BioIndex_Nar, BioIndex_Val)]
                                , by.x = col_by
                                , by.y = col_by
                                , all.x = TRUE)

        good.SSTV.abund    <- all.SSTV.abund[stats::complete.cases(all.SSTV.abund),]
        all.ref.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% ref.sites)
        cl.SSTV.abund      <- subset(good.SSTV.abund, good.SSTV.abund$StressSampID %in% cl.match.b$StressSampID)
        cl.ref.SSTV.abund  <- subset(cl.SSTV.abund, cl.SSTV.abund$StationID_Master %in% ref.sites)
        site.SSTV.abund    <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% TargetSiteID)
        SSTV.Resp          <- c("SensRelAbund", "TolRelAbund")
        
        varFlag <- 1
        
        # r.len  <- length(SSTV.Resp)
        # 
        # if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
        #   r <- 1
        # }##IF.boo.DEBUG.END
        # 
        # # Loop r (response) ####
        # for (r in 1:length(SSTV.Resp)) {##FOR.r.START
          # tvr <- r.len*(tv-1)+r
          # tvr.len <- tv.len * r.len
          
        #  respName <- SSTV.Resp[r]
          
        #  print(paste0("Response = ",respName))
        #  utils::flush.console()
          
          # df.plot1 <- good.SSTV.abund[,c(SSTV.analyte, respName)]
          # df.plot2 <- all.ref.SSTV.abund[,c(SSTV.analyte, respName)]
          # df.plot3 <- cl.SSTV.abund[,c(SSTV.analyte, respName)]
          # df.plot4 <- cl.ref.SSTV.abund[,c(SSTV.analyte, respName)]
          # df.plot5 <- site.SSTV.abund[,c(SSTV.analyte, respName)]
        df.plot1 <- good.SSTV.abund[,c(SSTV.analyte, SSTV.Resp)]
        df.plot2 <- all.ref.SSTV.abund[,c(SSTV.analyte, SSTV.Resp)]
        df.plot3 <- cl.SSTV.abund[,c(SSTV.analyte, SSTV.Resp)]
        df.plot4 <- cl.ref.SSTV.abund[,c(SSTV.analyte, SSTV.Resp)]
        df.plot5 <- site.SSTV.abund[,c(SSTV.analyte, SSTV.Resp)]
          
          # PLOTS ####
          # Capture each plot in a list for the PDF
          # plots.tvr <- vector(length(SSTV.Resp), mode="list")
          # ppi<-300
          #varFileOut = paste0("Results/",TargetSiteID,"/",TargetSiteID,".SR.SSTV.")
          
          # 
          # grDevices::jpeg(filename = paste(varFileOut, SSTV.analyte, "_", 
          #                       respName, ".jpg", sep = ""), 
          #      width = 4*ppi, height = 3*ppi, quality=100, 
          #      pointsize=8, res = ppi)
          
         # plot.pryr %<a-% {##pryr.START
           # graphics::par(cex.main=0.8,cex.lab=0.6,font.main=2, font.lab=2)
            if (log.yn == TRUE) {
              # df.plot1 <- cbind(log10(df.plot1[, 1]), df.plot1[, 2])
              # df.plot2 <- cbind(log10(df.plot2[, 1]), df.plot2[, 2])
              # df.plot3 <- cbind(log10(df.plot3[, 1]), df.plot3[, 2])
              # df.plot4 <- cbind(log10(df.plot4[, 1]), df.plot4[, 2])
              # df.plot5 <- cbind(log10(df.plot5[, 1]), df.plot5[, 2])
              df.plot1[, SSTV.analyte] <- log10(df.plot1[, SSTV.analyte])
              df.plot2[, SSTV.analyte] <- log10(df.plot2[, SSTV.analyte])
              df.plot3[, SSTV.analyte] <- log10(df.plot3[, SSTV.analyte])
              df.plot4[, SSTV.analyte] <- log10(df.plot4[, SSTV.analyte])
              df.plot5[, SSTV.analyte] <- log10(df.plot5[, SSTV.analyte])
            }
            
            # if (respName == "SensRelAbund") {
            #   respText <- "Sensitive Taxa Relative Abundance"
            # } else if (respName == "SensTotAbund") {
            #   respText <- "Sensitive Taxa Abundance"
            # } else if (respName == "TolRelAbund") {
            #   respText <- "Tolerant Taxa Relative Abundance"
            # } else {
            #   respText <- "Tolerant Taxa Abundance"
            # }
            
            # varMain <- paste("Linear regression of", SSTV.analyte, "on", respText
            #                  , "\n", "for", TargetSiteID,"with", paste(predint*100, "th", sep= "")
            #                  , "percentile prediction interval", sep = " ")
            # if (log.yn == TRUE) {
            #   varxlab <- paste("Log10", SSTV.analyte)
            # } else {
            #   varxlab <- SSTV.analyte
            # }
            # There should never be a case where either x or y are always NA for all data
            if (length(df.plot1) > 0) {
              # graphics::plot(df.plot1[,2]~df.plot1[,1],main=varMain, 
              #      xlab=varxlab,ylab=respText, col="darkgrey", 
              #      pch=1, cex = 0.8, cex.lab=0.6, cex.main = 0.8, 
              #      font.main = 2, font.lab = 2, mar = c(6,4,4,2)+0.1)
            } else {
              next
            }
          #   if (length(df.plot2) > 0) {
          #     graphics::points(df.plot2[,2]~df.plot2[,1], 
          #            col="blue", pch=16, cex = 0.8) # blue solid dots
          #   }
          #   if (length(df.plot3) > 0) {
          #     graphics::points(df.plot3[,2]~df.plot3[,1], 
          #            col="cyan4", pch=2, cex = 0.8) # Cyan open triangles
          #   }
          #   if (length(df.plot4) > 0) {
          #     graphics::points(df.plot4[,2]~df.plot4[,1], 
          #            col="blue", pch=17, cex = 0.8) # Solid blue triangles
          #   }
          #   if (length(df.plot5) > 0) {
          #     graphics::points(df.plot5[,2]~df.plot5[,1], 
          #            col="red", pch=19, cex = 1.0) # Red solid dots
          #   }
          #   
          #   cl.x.sd <- stats::sd(df.plot3[,1])
          #   cl.y.sd <- stats::sd(df.plot3[,2])
          #   
          #   # fix from df.plot3 to sum(df.plot3) for vert and horiz
          #   # and !is.na to sum(is.na)
          #   #Check for vertical line
          #   if (sum(!is.na(df.plot3))==0) {
          #     if (sum(df.plot3) == 0) {
          #       print(paste("Vertical line for", SSTV.analyte, respName, sep=" "))
          #       utils::flush.console()
          #       next     #It's okay to plot the points, but not the regression line
          #     }
          #   }
          #   #Check for horizontal line
          #   if (sum(!is.na(df.plot3))==0) {
          #     if (sum(df.plot3) == 0) {
          #       print(paste("Horizontal line for", SSTV.analyte, respName, sep=" "))
          #       utils::flush.console()
          #       next     #It's okay to plot the points, but not the regression line
          #     }
          #   }    
          #   
          #   #Linear Regression (uses cluster data -- all sites in the cluster)
          #   varY <- df.plot3[,2]
          #   varX <- df.plot3[,1]
          #   fit = stats::lm(varY~varX)
          #   pred.int = stats::predict(fit,interval="prediction",level=predint)
          #   fitted.values = pred.int[,1]
          #   pred.lower = pred.int[,2]
          #   pred.upper = pred.int[,3]
          #   
          #   graphics::abline(stats::lm(varY~varX), col="cyan4", lwd=1.5)
          #   graphics::abline(stats::lm(pred.lower~varX), col="cyan4", lwd=1)
          #   graphics::abline(stats::lm(pred.upper~varX), col="cyan4", lwd=1)
          #   # 
          #   slope <- summary(stats::lm(varY~varX))[[4]][[2]]
          #   intercept <- summary(stats::lm(varY~varX))[[4]][[1]]
          #   pval_intercept <- summary(stats::lm(varY~varX))[[4]][[7]]
          #   pval_slope <- summary(stats::lm(varY~varX))[[4]][[8]]
          #   slope = signif(slope, 3)
          #   intercept = signif(intercept, 3)
          #   pval_intercept = signif(pval_intercept, 3)
          #   pval = signif(pval_slope, 3)
          #   # # r? text and legend
          #   r = stats::cor(varX, varY, method="pearson",use="pairwise.complete.obs")
          #   r2 = formatC(r^2,format="f",digits=3)
          #   # 
          #   c1S <- (stats::cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
          #   df.corr = data.frame(cbind(SSTV.analyte, respName, signif(c1S$statistic,2)
          #                              , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
          #   names(df.corr) <- c("stressName", "respName", "statistic", "p.value", "estimate", "r2")
          #   # # Create results data frame
          #   # Correlations ####
          #   # fn_SSTVfile <- paste0(TargetSiteID, ".SR.SSTV.Corrs.txt")
          #   boo.file.exists <- file.exists(file.path(wd, dir.sub, dir.sub2, fn_SSTVfile))
          #   boo.Append    <- TRUE
          #   boo.col.names <- FALSE
          #   if (boo.file.exists==FALSE) {  #First time through loop
          #     # df.CorrTable <- df.corr
          #     boo.Append    <- !boo.Append
          #     boo.col.names <- !boo.col.names
          #   } else {
          #     #df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
          #   }# IF, END
          #   df.CorrTable <- df.corr
          #   if(boo.pryr==TRUE){
          #     utils::write.table(df.CorrTable
          #                        , file.path(wd, dir.sub, dir.sub2, fn_SSTVfile)
          #                        , sep="\t", quote=FALSE, row.names=FALSE
          #                        , col.names=boo.col.names, append=boo.Append) 
          #   }
          #   pval.corr = signif(c1S$p.value,2)
          #   
          #   #Print equation, r2, and p-value
          #   if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
          #     eqn <- paste("Cluster regression: "
          #                  , "y =", slope, "x +", intercept, "; ", "r2 = ",r2,"; "
          #                  ,"p-value = ",pval.corr,"; ","n = ",length(varX))
          #     symbshape <- c(1, 16, 2, 17, 19)
          #     symbcol <- c("grey", "blue", "cyan4", "blue", "red")
          #     symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", TargetSiteID)
          #     graphics::mtext(eqn, side=1, line = 4, bty="n", col = c("black"), cex=0.6)
          #     graphics::legend(varLegOpp, inset=as.numeric(varInset), symbname, pch=symbshape, col=symbcol, cex=0.6)
          #   }
          # }##plot.pryr.END
          
          # # PDF, capture plot in list
          # boo.pryr <- TRUE
          #   plot.pryr
          # boo.pryr <- FALSE
          # plots.tvr[[tvr]] <- grDevices::recordPlot()
          
          # # JPG
          # grDevices::jpeg(filename = paste(varFileOut, SSTV.analyte, "_", 
          #                                 respName, ".jpg", sep = ""),
          #                 width = 4*ppi, height = 3*ppi, quality=100,
          #                 pointsize=8, res = ppi)
          #   plot.pryr
          # grDevices::dev.off()
          
          
          #~~~~~~~~~~new code~~~~~~~~~~~~~~~~
          
          # stressor.SSTV has tolval for sensitive and tolerant taxa
          # sstv_sens_min <- stressor.SSTV[tv, "SensMin"]
          # sstv_sens_max <- stressor.SSTV[tv, "SensMax"]
          # sstv_tol_min  <- stressor.SSTV[tv, "TolMin"]
          # sstv_tol_max  <- stressor.SSTV[tv, "TolMax"]
        
          # 20190305, drop added Bio Index value and narrative
          df_plot_all <- reshape2::melt(good.SSTV.abund[, 1:6], id.vars=colnames(good.SSTV.abund)[1:4])
          #df_plot_all$SSTV.analyte <- df_plot_all[, SSTV.analyte]
          df_plot_all[, "Param_Name"] <- SSTV.analyte
          colnames(df_plot_all)[colnames(df_plot_all)==SSTV.analyte] <- "Param_Value"
          df_plot_all <- df_plot_all[, c(1:3,7,4:6)]
          levels(df_plot_all$variable) <- c("Sensitive Taxa", "Tolerant Taxa")
          
          # 20190305, switch to "better" bio from all
          df_plot_betterbio <- good.SSTV.abund[good.SSTV.abund[, BioIndex_Val] > bio_better_thresh, 1:6]
          df_plot_betterbio <- reshape2::melt(df_plot_betterbio, id.vars=colnames(df_plot_betterbio)[1:4])
          #df_plot_betterbio$SSTV.analyte <- df_plot_betterbio[, SSTV.analyte]
          df_plot_betterbio[, "Param_Name"] <- SSTV.analyte
          colnames(df_plot_betterbio)[colnames(df_plot_betterbio)==SSTV.analyte] <- "Param_Value"
          df_plot_betterbio <- df_plot_betterbio[, c(1:3,7,4:6)]
          levels(df_plot_betterbio$variable) <- c("Sensitive Taxa", "Tolerant Taxa")
          n_records_better_bio <- nrow(df_plot_betterbio)
      
          df_plot_targ <- reshape2::melt(site.SSTV.abund[, 1:6], id.vars=colnames(site.SSTV.abund)[1:4])
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
          q_Sens_lo <- as.vector(df_quantiles[df_quantiles[, 1]=="Sensitive Taxa", "value"][, "25%"])
          q_Sens_hi <- as.vector(df_quantiles[df_quantiles[, 1]=="Sensitive Taxa", "value"][, "50%"])
          q_Tol_lo <- as.vector(df_quantiles[df_quantiles[, 1]=="Tolerant Taxa", "value"][, "50%"])
          q_Tol_hi <- as.vector(df_quantiles[df_quantiles[, 1]=="Tolerant Taxa", "value"][, "75%"])
          # Add scoring thresholds to target siteID data frame
          df_plot_targ[, paste0("betterbio_varval_q", c("LO", "HI"))] <- NA
          df_plot_targ[df_plot_targ[, "variable"]=="Sensitive Taxa", "betterbio_varval_qLO"] <- q_Sens_lo
          df_plot_targ[df_plot_targ[, "variable"]=="Sensitive Taxa", "betterbio_varval_qHI"] <- q_Sens_hi
          df_plot_targ[df_plot_targ[, "variable"]=="Tolerant Taxa", "betterbio_varval_qLO"] <- q_Tol_lo
          df_plot_targ[df_plot_targ[, "variable"]=="Tolerant Taxa", "betterbio_varval_qHI"] <- q_Tol_hi
          # Scoring (tolerant than flip for sensitive)
          df_plot_targ[, "Score"] <- ifelse(df_plot_targ[, "value"] > df_plot_targ[, "betterbio_varval_qHI"], 1
                                            , ifelse(df_plot_targ[, "value"] < df_plot_targ[, "betterbio_varval_qLO"], -1, 0))
          df_plot_targ[df_plot_targ[, "variable"]=="Sensitive Taxa", "Score"]  <- -1 * df_plot_targ[df_plot_targ[, "variable"]=="Sensitive Taxa", "Score"]
          
          # Add other variables
          df_plot_targ[, "biocomm"] <- biocomm
          df_plot_targ[, "n_BetterBio"] <- n_records_better_bio
          df_plot_targ[, "n_BetterBioDegNo"] <- n_records_betterbio_BioDegNo
          
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

          utils::write.table(df_plot_targ, file=fn_scores
                             , col.names = boo_colnames, row.names=FALSE, sep="\t", append=boo_append)
          
          
          
          
          # ggplot ####
          
          {##PLOT VARIABLES ~ START
          ## Plot, Variables, Strings
          str_title <- paste(TargetSiteID, SSTV.analyte, sep=" ~ ")
          str_subtitle <- paste0("Samples with better biology (index > ", signif(bio_better_thresh, 3), ")")
          str_xlab  <- ""
          str_ylab  <- "Relative Abundance"
          df_plot_targ_sortvalue <- df_plot_targ[order(df_plot_targ[,"value"]), ]
          str_score_sens <- paste(df_plot_targ_sortvalue[df_plot_targ_sortvalue[, "variable"]=="Sensitive Taxa", "Score"], collapse=", ")
          str_score_tol <- paste(df_plot_targ_sortvalue[df_plot_targ_sortvalue[, "variable"]=="Tolerant Taxa", "Score"], collapse=", ")
          str_caption <- paste0("Score = Tolerant (", str_score_tol
                                , "), Sensitive ("
                                , str_score_sens
                                , ").\nNumber of samples = better biology (n="
                                , n_records_better_bio
                                , "), better biology and not degraded (n="
                                , n_records_betterbio_BioDegNo, ").")
          
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
                    ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)
                                   , plot.subtitle = ggplot2::element_text(hjust=0.5)
                                   , axis.title.y = ggplot2::element_blank()) +
                    ggplot2::coord_flip() + 
                    # Add degraded y/n for better bio sites
                    ggplot2::geom_jitter(data=df_plot_betterbio_BioDegNo
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
                    ggplot2::scale_color_manual(breaks = c("Yes", "No"), values = bio_col, drop = FALSE) +
                    ggplot2::scale_fill_manual(breaks = c("Yes", "No"), values = bio_col, drop = FALSE) +
                    ggplot2::scale_shape_manual(breaks = c("Yes", "No"), values = bio_shp, drop = FALSE)
                    
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
          #fn_jpg <- paste0(varFileOut, SSTV.analyte, "_", respName, ".jpg")
          fn_jpg <- paste0(varFileOut, make.names(SSTV.analyte), ".jpg")
          ggplot2::ggsave(fn_jpg, p_SSTV, width=plot_W, height=plot_H, units="in")
          
          # ggplot save
          
          
          
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
  fn_pdf <- file.path(getwd(), "Results", TargetSiteID, dir.sub3, paste0(TargetSiteID,".SR.SSTV.ALL.pdf"))
  grDevices::pdf(file=fn_pdf, width=8)
    # for (tvr in plots.tvr){##FOR.gp.START
    #   #grDevices::replayPlot(g.plot)
    #   if(is.null(tvr)==TRUE) {next}
    #   grDevices::replayPlot(tvr)
    # }##FOR.gp.END
    for (tv in plots.tv){##FOR.gp.START
      #grDevices::replayPlot(g.plot)
      if(is.null(tv)==TRUE) {next}
      grDevices::replayPlot(tv)
    }##FOR.gp.END
  grDevices::dev.off()
#  rm(plots.tvr)
  rm(plots.tv)
  
  # # CorrPlot ####
  # ## read
  # fn_corr <- paste0(TargetSiteID,".SR.SSTV.Corrs.txt")
  # df_corr <- utils::read.delim(file.path(wd,dir.sub,dir.sub2,fn_corr))
  # ## transpose
  # df_corr_r <- reshape2::dcast(df_corr, stressName ~ respName, value.var="estimate")
  # df_corrplot <- t(df_corr_r[,-1])
  # colnames(df_corrplot) <- df_corr_r[,1]
  # ## jpg
  # fn_jpg_cp <- file.path(wd, dir.sub, dir.sub2, paste0(TargetSiteID, ".SR.SSTV.CorrPlot.jpg"))
  # grDevices::jpeg(filename = fn_jpg_cp
  #                 , width = 4 * ppi
  #                 , height = 3 * ppi
  #                 , quality=100
  #                 )
  #     corrplot::corrplot(df_corrplot, method="circle")
  # grDevices::dev.off()
  #
  
}##FUNCTION.END
