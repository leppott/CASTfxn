# 20190226, modifications to function and output.  
# Mothball the old function
#' @title Stressor Specific Regressions
#' 
#' @description Get stressor specific regressions.
#' 
#' @details Percent fines.
#' 
#' Required objects:
#' 
#' * data.SampSummary; StationID_Master, CollDate, ChemSampleID, PhabSampID, BMI.Metrics.SampID, Algae.Metrics.SampID
#' 
#' * data.bmi.taxa.raw; BMI.Metrics.SampID
#' 
#' * data.chem.info; SSTV, Analyte, SSTV, SensMin, SensMax, TolMin, TolMax
#' 
#' * data.SSTV.totabund; BMI.Metrics.SampID, StationID_Master, ChemSampleID, SSTV.analyte
#' , SensRelAbund, TolRelAbund, SensTaxa, SampleAbundance, TolTaxa
#' 
#' * TargetSiteID
#' 
#' @param TargetSiteID x
#' @param data.SampSummary x
#' @param data.bmi.taxa.raw x
#' @param data.chem.info x
#' @param data.SSTV.totabund x
#' @param data.MT.bmi x
#' @param matchedData matched biological and chemical stressor data.
#' @param predint x
#' @param varLegLoc Legend location; "bottomright", "bottom", "bottomleft", 
#' "left", "topleft", "top", "topright", "right" and "center".  Default = "topright"
#' 
#' @keywords internal
#' 
#' @return Jpeg files to "Results" folder in working directory.  And a tab-delimited text file.
#' 
#' @importFrom pryr "%<a-%"
#' 
#' @examples
#' predint <- 0.75
#' varLegLoc <- "topright"
#' 
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
#' 
#' # Data getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites          # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.cluster       <- data_Cluster_Hi     # need for getSiteInfo and getChemDataSubsets
#' data.mod           <- data_ReachMod
#' data.MT.bmi        <- data_BMIMasterTaxa
#' 
#' # Map data
#' # San Diego
#' #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
#' #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
#' # AZ
#' map_flowline  <- data_GIS_Flow_HI
#' map_flowline2 <- data_GIS_Flow_LO
#' map_outline   <- data_GIS_AZ_Outline
#' # Project site data to USGS Albers Equal Area
#' usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#'               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' # projection for outline
#' my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 
#'            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' map_proj <- my.aea
#' 
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
#'                                 , data.SampSummary, data.303d.ComID
#'                                 , data.bmi.metrics, data.algae.metrics
#'                                 , data.cluster, data.mod
#'                                 , map_proj, map_outline, map_flowline)
#' 
#' # Data getChemDataSubsets
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
#'                                 , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
#'                                 , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
#'
#' # Data getStressorList
#' chem.info <- list.data$chem.info
#' cluster.chem <- list.data$cluster.chem
#' cluster.samps <- list.data$cluster.samps
#' ref.sites <- list.data$ref.sites
#' site.chem <- list.data$site.chem
#' 
#' # set cutoff for possible stressor identification
#' probsLow <- 0.10
#' probsHigh <- 0.90 
#' 
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, site.chem
#'                                  , probsHigh, probsLow)
#'                                  
#' # Data getBMIMatches
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
#' 
#' # Run getBMIMatches
#' list.MatchBMIData <- getBMIMatches(stressors, list.data)  
#'   
#' # Data getStressorSpecificRegressions
#' # data import, example
#' # data.bmi.taxa.raw <- read.delim(paste(myDir.Data,"data.bmi.taxa.raw.tab",sep=""))
#' # data.SSTV.totabund <- read.delim(paste(myDir.Data,"data.totabund.bySample.tab",sep=""))
#' #
#' # data, example included with package
#' data.bmi.taxa.raw <- data_BMIcounts
#' data.SSTV.totabund <- data_BMIRelAbund
#' 
#' # Run getStressorSpecificRegressions
#' getStressorSpecificRegressions(TargetSiteID
#'                                , data.SampSummary
#'                                , data.bmi.taxa.raw
#'                                , data.chem.info
#'                                , data.SSTV.totabund
#'                                , data.MT.bmi
#'                                , list.MatchBMIData)
#~~~~~~~~~~~~~~~~
# QC
# matchedData <- list.MatchBMIData
#
#' @export
z_getStressorSpecificRegressions <- function(TargetSiteID
                                           , data.SampSummary
                                           , data.bmi.taxa.raw
                                           , data.chem.info
                                           , data.SSTV.totabund
                                           , data.MT.bmi
                                           , matchedData
                                           , predint=0.75
                                           , varLegLoc="topright"
                                           ) {##FUNCTION.START
  # Debugging
  boo.DEBUG <- FALSE
  #
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    matchedData <- list.MatchBMIData
    tv <- 1
  }##IF.boo.DEBUG.END
  
  # Extra, 20181211
  ## Add RelAbundInds to data.bmi.raw
  col.by <- c("BMI.Metrics.SampID", "FinalID")
  data.bmi.taxa.raw <- merge(data.bmi.taxa.raw
                             , data.SSTV.totabund[, c(col.by, "RelAbundInds")]
                             , by=col.by
                             , all.x=TRUE)
  
  # check for and create (if necessary) "Results" subdirectory of working directory
  wd <- getwd()
  dir.sub <- "Results"
  dir.sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  #
  # helper
  RegPlotSet <- getRegPlotSet(varLegLoc)
  varInset   <- RegPlotSet[1]
  varSpacer  <- RegPlotSet[2]
  varLegOpp  <- RegPlotSet[3]
  
  df.SSTV <- subset(data.chem.info, SSTV != 0 & !is.na(SSTV) & SSTV!=""
                    , c("StdParamName", "SSTV", "SensMin", "SensMax", "TolMin", "TolMax"))
  # duplicate entry, use unique to limit
  df.SSTV <- unique(df.SSTV)
  colnames(df.SSTV)[1] <- "Analyte"

  if (nrow(list.SiteSummary$BMImetrics)==0) {
    # No BMI Responses Found
    print(paste0("No BMI response data available for ", TargetSiteID, 
                 ". Regression data illustrate cluster relationships only."))
    utils::flush.console()
  }
  
  boo.pryr <- FALSE
  
  plots.tvr <- vector(10, mode="list")
  ppi<-300
  varFileOut = paste0("Results/",TargetSiteID,"/",TargetSiteID,".SR.SSTV.")
  
  fn_SSTVfile <- paste0(TargetSiteID, ".SR.SSTV.Corrs.txt")
  boo.file.exists <- file.exists(file.path(wd, dir.sub, dir.sub2, fn_SSTVfile))
  if(boo.file.exists){
    file.remove(file.path(wd, dir.sub, dir.sub2, fn_SSTVfile))
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
        flush.console()
        
        # skip if SSTV = ""
        ## 20181211
        if(is.na(SSTV.name)==TRUE | SSTV.name==""){
          print("No data; SKIP")
          flush.console()
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
        SSTV.analyte.match.all.b.str <- SSTV.analyte[SSTV.analyte %in% names(matchedData$all.b.str)]
        all.match.b.str <- matchedData$all.b.str[,c("StationID_Master"
                        , "ChemSampleID", "BMI.Metrics.SampID", SSTV.analyte.match.all.b.str)]
        cl.match.b <- matchedData$cl.b.str[,c("StationID_Master", "ChemSampleID",
                            "BMI.Metrics.SampID", SSTV.analyte.match.all.b.str)]
        
        bmi.taxa.raw <- data.bmi.taxa.raw[data.bmi.taxa.raw$StationID_Master %in% 
                                              unique(all.match.b.str$StationID_Master),]
        bmi.taxa.raw <- merge(bmi.taxa.raw, data.MT.bmi[,c("GenusFinal", 
                                "FinalID", SSTV.name)], 
                                by.x = "FinalID", by.y = "FinalID")

        minTolVal <- min(data.MT.bmi[,SSTV.name], na.rm = TRUE)
        maxTolVal <- max(data.MT.bmi[,SSTV.name], na.rm = TRUE)
        
        bmi.taxa.raw$SensTaxa <- ifelse(bmi.taxa.raw[,SSTV.name]==minTolVal |
                                        bmi.taxa.raw[,SSTV.name]==minTolVal+1,
                                        bmi.taxa.raw$RelAbundInds, NA)
        bmi.taxa.raw$TolTaxa <- ifelse(bmi.taxa.raw[,SSTV.name]==maxTolVal |
                                        bmi.taxa.raw[,SSTV.name]==maxTolVal-1,
                                        bmi.taxa.raw$RelAbundInds, NA)

        bmi.taxa.raw2 <- dplyr::group_by(bmi.taxa.raw, StationID_Master, BMI.Metrics.SampID)
        bmi.taxa.raw2 <- dplyr::summarize(bmi.taxa.raw2, 
                                          SensRelAbund = sum(SensTaxa, na.rm = TRUE), 
                                          TolRelAbund = sum(TolTaxa, na.rm = TRUE))

        all.match.b.resp <- bmi.taxa.raw2[bmi.taxa.raw2$BMI.Metrics.SampID %in%
                                    unique(all.match.b.str$BMI.Metrics.SampID),]

        all.SSTV.abund <- merge(all.match.b.str, all.match.b.resp, 
                                by.x = c("StationID_Master", "BMI.Metrics.SampID"),
                                by.y = c("StationID_Master", "BMI.Metrics.SampID"),
                                all = TRUE)

        good.SSTV.abund <- all.SSTV.abund[stats::complete.cases(all.SSTV.abund),]
        all.ref.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% ref.sites)
        cl.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$ChemSampleID %in% cl.match.b$ChemSampleID)
        cl.ref.SSTV.abund <- subset(cl.SSTV.abund, cl.SSTV.abund$StationID_Master %in% ref.sites)
        site.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% TargetSiteID)
        SSTV.Resp <- c("SensRelAbund", "TolRelAbund")
        
        varFlag <- 1
        
        r.len  <- length(SSTV.Resp)
        
        if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
          r <- 1
        }##IF.boo.DEBUG.END
        
        # Loop r (response) ####
        for (r in 1:length(SSTV.Resp)) {##FOR.r.START
          tvr <- r.len*(tv-1)+r
          tvr.len <- tv.len * r.len
          
          respName <- SSTV.Resp[r]
          
          print(paste0("Response = ",respName))
          flush.console()
          
          df.plot1 <- good.SSTV.abund[,c(SSTV.analyte,respName)]
          df.plot2 <- all.ref.SSTV.abund[,c(SSTV.analyte,respName)]
          df.plot3 <- cl.SSTV.abund[,c(SSTV.analyte,respName)]
          df.plot4 <- cl.ref.SSTV.abund[,c(SSTV.analyte,respName)]
          df.plot5 <- site.SSTV.abund[,c(SSTV.analyte,respName)]
          
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
          
          plot.pryr %<a-% {##pryr.START
            graphics::par(cex.main=0.8,cex.lab=0.6,font.main=2, font.lab=2)
            if (log.yn == TRUE) {
              df.plot1 <- cbind(log10(df.plot1[,1]),df.plot1[,2])
              df.plot2 <- cbind(log10(df.plot2[,1]),df.plot2[,2])
              df.plot3 <- cbind(log10(df.plot3[,1]),df.plot3[,2])
              df.plot4 <- cbind(log10(df.plot4[,1]),df.plot4[,2])
              df.plot5 <- cbind(log10(df.plot5[,1]),df.plot5[,2])
            }
            
            if (respName == "SensRelAbund") {
              respText <- "Sensitive Taxa Relative Abundance"
            } else if (respName == "SensTotAbund") {
              respText <- "Sensitive Taxa Abundance"
            } else if (respName == "TolRelAbund") {
              respText <- "Tolerant Taxa Relative Abundance"
            } else {
              respText <- "Tolerant Taxa Abundance"
            }
            
            varMain <- paste("Linear regression of", SSTV.analyte, "on", respText
                             , "\n", "for", TargetSiteID,"with", paste(predint*100, "th", sep= "")
                             , "percentile prediction interval", sep = " ")
            if (log.yn == TRUE) {
              varxlab <- paste("Log10", SSTV.analyte)
            } else {
              varxlab <- SSTV.analyte
            }
            # There should never be a case where either x or y are always NA for all data
            if (length(df.plot1) > 0) {
              graphics::plot(df.plot1[,2]~df.plot1[,1],main=varMain, 
                   xlab=varxlab,ylab=respText, col="darkgrey", 
                   pch=1, cex = 0.8, cex.lab=0.6, cex.main = 0.8, 
                   font.main = 2, font.lab = 2, mar = c(6,4,4,2)+0.1)
            } else {
              next
            }
            if (length(df.plot2) > 0) {
              graphics::points(df.plot2[,2]~df.plot2[,1], 
                     col="blue", pch=16, cex = 0.8) # blue solid dots
            }
            if (length(df.plot3) > 0) {
              graphics::points(df.plot3[,2]~df.plot3[,1], 
                     col="cyan4", pch=2, cex = 0.8) # Cyan open triangles
            }
            if (length(df.plot4) > 0) {
              graphics::points(df.plot4[,2]~df.plot4[,1], 
                     col="blue", pch=17, cex = 0.8) # Solid blue triangles
            }
            if (length(df.plot5) > 0) {
              graphics::points(df.plot5[,2]~df.plot5[,1], 
                     col="red", pch=19, cex = 1.0) # Red solid dots
            }
            
            cl.x.sd <- stats::sd(df.plot3[,1])
            cl.y.sd <- stats::sd(df.plot3[,2])
            
            # fix from df.plot3 to sum(df.plot3) for vert and horiz
            # and !is.na to sum(is.na)
            #Check for vertical line
            if (sum(!is.na(df.plot3))==0) {
              if (sum(df.plot3) == 0) {
                print(paste("Vertical line for", SSTV.analyte, respName, sep=" "))
                utils::flush.console()
                next     #It's okay to plot the points, but not the regression line
              }
            }
            #Check for horizontal line
            if (sum(!is.na(df.plot3))==0) {
              if (sum(df.plot3) == 0) {
                print(paste("Horizontal line for", SSTV.analyte, respName, sep=" "))
                utils::flush.console()
                next     #It's okay to plot the points, but not the regression line
              }
            }    
            
            #Linear Regression (uses cluster data -- all sites in the cluster)
            varY <- df.plot3[,2]
            varX <- df.plot3[,1]
            fit = stats::lm(varY~varX)
            pred.int = stats::predict(fit,interval="prediction",level=predint)
            fitted.values = pred.int[,1]
            pred.lower = pred.int[,2]
            pred.upper = pred.int[,3]
            
            graphics::abline(stats::lm(varY~varX), col="cyan4", lwd=1.5)
            graphics::abline(stats::lm(pred.lower~varX), col="cyan4", lwd=1)
            graphics::abline(stats::lm(pred.upper~varX), col="cyan4", lwd=1)
            # 
            slope <- summary(stats::lm(varY~varX))[[4]][[2]]
            intercept <- summary(stats::lm(varY~varX))[[4]][[1]]
            pval_intercept <- summary(stats::lm(varY~varX))[[4]][[7]]
            pval_slope <- summary(stats::lm(varY~varX))[[4]][[8]]
            slope = signif(slope, 3)
            intercept = signif(intercept, 3)
            pval_intercept = signif(pval_intercept, 3)
            pval = signif(pval_slope, 3)
            # # r? text and legend
            r = stats::cor(varX, varY, method="pearson",use="pairwise.complete.obs")
            r2 = formatC(r^2,format="f",digits=3)
            # 
            c1S <- (stats::cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
            df.corr = data.frame(cbind(SSTV.analyte, respName, signif(c1S$statistic,2)
                                       , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
            names(df.corr) <- c("stressName", "respName", "statistic", "p.value", "estimate", "r2")
            # # Create results data frame
            # correlations ####
            # fn_SSTVfile <- paste0(TargetSiteID, ".SR.SSTV.Corrs.txt")
            boo.file.exists <- file.exists(file.path(wd, dir.sub, dir.sub2, fn_SSTVfile))
            boo.Append    <- TRUE
            boo.col.names <- FALSE
            if (boo.file.exists==FALSE) {  #First time through loop
              # df.CorrTable <- df.corr
              boo.Append    <- !boo.Append
              boo.col.names <- !boo.col.names
            } else {
              #df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
            }# IF, END
            df.CorrTable <- df.corr
            if(boo.pryr==TRUE){
              utils::write.table(df.CorrTable
                                 , file.path(wd, dir.sub, dir.sub2, fn_SSTVfile)
                                 , sep="\t", quote=FALSE, row.names=FALSE
                                 , col.names=boo.col.names, append=boo.Append) 
            }
            pval.corr = signif(c1S$p.value,2)
            
            #Print equation, r2, and p-value
            if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
              eqn <- paste("Cluster regression: "
                           , "y =", slope, "x +", intercept, "; ", "r2 = ",r2,"; "
                           ,"p-value = ",pval.corr,"; ","n = ",length(varX))
              symbshape <- c(1, 16, 2, 17, 19)
              symbcol <- c("grey", "blue", "cyan4", "blue", "red")
              symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", TargetSiteID)
              graphics::mtext(eqn, side=1, line = 4, bty="n", col = c("black"), cex=0.6)
              graphics::legend(varLegOpp, inset=as.numeric(varInset), symbname, pch=symbshape, col=symbcol, cex=0.6)
            }
          }##plot.pryr.END
          
          # PDF, capture plot in list
          boo.pryr <- TRUE
            plot.pryr
          boo.pryr <- FALSE
          plots.tvr[[tvr]] <- grDevices::recordPlot()
          
          # JPG
          grDevices::jpeg(filename = paste(varFileOut, SSTV.analyte, "_", 
                                          respName, ".jpg", sep = ""),
                          width = 4*ppi, height = 3*ppi, quality=100,
                          pointsize=8, res = ppi)
            plot.pryr
          grDevices::dev.off()

          varFlag <- 0
          
        }##FOR.r.END  # End For loop over responses
        grDevices::graphics.off()
        
      }##FOR.tv.END  # End For loop over stressors
      # SSTVfile <- paste("Results/",TargetSiteID, "/", TargetSiteID, ".SSTVCorrs.txt", sep="")
      # utils::write.table(df.CorrTable, file=SSTVfile, sep= "\t",quote=FALSE,
      #                    row.names=FALSE,col.names=TRUE)
    }##IF.stressor.SSTV.END
  }##IF.SSTV.END
  
  # Create PDF from list
  fn_pdf <- file.path(getwd(), "Results", TargetSiteID, paste0(TargetSiteID,".SR.SSTV.ALL.pdf"))
  pdf(file=fn_pdf, width=8)
  for (tvr in plots.tvr){##FOR.gp.START
    #grDevices::replayPlot(g.plot)
    if(is.null(tvr)==TRUE) {next}
    grDevices::replayPlot(tvr)
  }##FOR.gp.END
  grDevices::dev.off()
  rm(plots.tvr)
  
  # CorrPlot ####
  ## read
  fn_corr <- paste0(TargetSiteID,".SR.SSTV.Corrs.txt")
  df_corr <- read.delim(file.path(wd,dir.sub,dir.sub2,fn_corr))
  ## transpose
  df_corr_r <- reshape2::dcast(df_corr, stressName ~ respName, value.var="estimate")
  df_corrplot <- t(df_corr_r[,-1])
  colnames(df_corrplot) <- df_corr_r[,1]
  ## jpg
  fn_jpg_cp <- file.path(wd, dir.sub, dir.sub2, paste0(TargetSiteID, ".SR.SSTV.CorrPlot.jpg"))
  grDevices::jpeg(filename = fn_jpg_cp
                  , width = 4 * ppi
                  , height = 3 * ppi
                  , quality=100
                  )
      corrplot::corrplot(df_corrplot, method="circle")
  grDevices::dev.off()
  #
  
}##FUNCTION.END
