#' @title Stressor Specific Regressions
#' 
#' @description Get stressor specific regressions.
#' 
#' @details Percent fines.
#' 
#' Required objects:
#' 
#' * data.SampSumamry; StationID_Master, CollDate, ChemSampleID, PhabSampID, BMI.Metrics.SampID, Algae.Metrics.SampID
#' 
#' * data.bmi.taxa.raw; BMI.Metrics.SampID
#' 
#' * data.chem.info; SSTV, Analyte, SSTV, SensMin, SensMax, TolMin, TolMax
#' 
#' * data.SSTV.totabund; BMI.Metrics.SampID, StationID_Master, ChemSampleID, SSTV.analyte
#' , SensRelAbund, TolRelAbund, SensTaxa, SampleAbundance, TolTaxa
#' 
#' * myDir.Data
#' 
#' * TargetSiteID
#' 
#' @param matchedData matched biological and chemical stressor data.
#' 
#' @return Jpeg files to "Results" folder in working directory.  And a tab-delimited text file.
#' 
#' @examples
#' predint <- 0.75
#' varLegLoc <- "topright"
#' 
#' TargetSiteID <- "SRCKN001.61"
#' clustertype <- "5"
#' useLU <- FALSE
#' 
#' CurrentDir <- getwd()
#' myDir.Data <- paste(CurrentDir,"data/",sep="/")
#' 
#' # datasets getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.cluster       <- data_Cluster_Hi
#' data.mod           <- data_ReachMod
#' #
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#' 
#' # datasets getChemDataSubsets
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' 
#' # data import, example 
#' # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
#' # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
#' 
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#'
#' # datasets getStressorList
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
#' # datasets getBMIMatches
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' 
#' # Run getBMIMatches
#' list.MatchBMIData <- getBMIMatches(stressors, list.data)  
#'   
#' # datasets getStressorSpecificRegressions
#' # data import, example
#' # data.bmi.taxa.raw <- read.delim(paste(myDir.Data,"data.bmi.taxa.raw.tab",sep=""))
#' # data.SSTV.totabund <- read.delim(paste(myDir.Data,"data.totabund.bySample.tab",sep=""))
#' #
#' # data, example included with package
#' data.bmi.taxa.raw <- data_BMIcounts
#' data.SSTV.totabund <- data_BMIRelAbund
#' 
#' # Run getStressorSpecificRegressions
#' getStressorSpecificRegressions(list.MatchBMIData)
#~~~~~~~~~~~~~~~~
# QC
# matchedData <- list.MatchBMIData
#
#' @export
getStressorSpecificRegressions <- function(matchedData, predint=0.75, 
                                           varLegLoc="topright") {
  #
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
  varInset  <- RegPlotSet[1]
  varSpacer <- RegPlotSet[2]
  varLegOpp <- RegPlotSet[3]
  
  df.SSTV <- subset(data.chem.info, SSTV != 0, c("StdParamName", "SSTV", "SensMin"
                                              , "SensMax", "TolMin", "TolMax"))
  colnames(df.SSTV)[1] <- "Analyte"

  if (nrow(list.SiteSummary$BMImetrics)==0) {
    # No BMI Responses Found
    print(paste0("No BMI response data available for ", TargetSiteID, 
                 ". Regression data illustrate cluster relationships only."))
    flush.console()
  }
    
  if (nrow(df.SSTV) != 0) {##IF.SSTV.START
    stressor.SSTV <- subset(df.SSTV, Analyte %in% stressors)

    if (nrow(stressor.SSTV) != 0) {##IF.stressor.SSTV.START
      
      for (tv in 1:nrow(stressor.SSTV)) {        # Currently only valid for SpecCond
        SSTV.analyte <- as.vector(stressor.SSTV$Analyte)[tv]
        SSTV.name <- as.vector(stressor.SSTV$SSTV)[tv]
        # 20180620, more than one (add sum)
        if (sum(SSTV.analyte %in% c("DO_uf_mg_L", "pH_SU", "Temp_degC", "Flow_cfs",
                                    "Flow_calc_cfs"))>0) {
          log.yn <- FALSE
        } else {
          log.yn <- TRUE
        }
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

        bmi.taxa.raw2 <- dplyr::group_by(bmi.taxa.raw, StationID_Master, SampleID)
        bmi.taxa.raw2 <- dplyr::summarize(bmi.taxa.raw2, 
                                          SensRelAbund = sum(SensTaxa, na.rm = TRUE), 
                                          TolRelAbund = sum(TolTaxa, na.rm = TRUE))

        all.match.b.resp <- bmi.taxa.raw2[bmi.taxa.raw2$SampleID %in%
                                    unique(all.match.b.str$BMI.Metrics.SampID),]

        all.SSTV.abund <- merge(all.match.b.str, all.match.b.resp, 
                                by.x = c("StationID_Master", "BMI.Metrics.SampID"),
                                by.y = c("StationID_Master", "SampleID"),
                                all = TRUE)

        good.SSTV.abund <- all.SSTV.abund[stats::complete.cases(all.SSTV.abund),]
        all.ref.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% ref.sites)
        cl.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$ChemSampleID %in% cl.match.b$ChemSampleID)
        cl.ref.SSTV.abund <- subset(cl.SSTV.abund, cl.SSTV.abund$StationID_Master %in% ref.sites)
        site.SSTV.abund <- subset(good.SSTV.abund, good.SSTV.abund$StationID_Master %in% TargetSiteID)
        SSTV.Resp <- c("SensRelAbund", "TolRelAbund")
        
        varFlag <- 1
        for (r in 1:length(SSTV.Resp)) {
          respName <- SSTV.Resp[r]
          df.plot1 <- good.SSTV.abund[,c(SSTV.analyte,respName)]
          df.plot2 <- all.ref.SSTV.abund[,c(SSTV.analyte,respName)]
          df.plot3 <- cl.SSTV.abund[,c(SSTV.analyte,respName)]
          df.plot4 <- cl.ref.SSTV.abund[,c(SSTV.analyte,respName)]
          df.plot5 <- site.SSTV.abund[,c(SSTV.analyte,respName)]
          
          ppi<-300
          varFileOut = paste0("Results/",TargetSiteID,"/",TargetSiteID,"SSTV.SR.")
          grDevices::jpeg(filename = paste(varFileOut, SSTV.analyte, "_", 
                                respName, ".jpg", sep = ""), 
               width = 4*ppi, height = 3*ppi, quality=100, 
               pointsize=8, res = ppi)
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
          # # Create results data frame
          if (varFlag==1) {  #First time through loop
            df.CorrTable <- c(df.corr)
          } # IF, END
          df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
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
          
          grDevices::dev.off()

          varFlag <- 0
          
        }  # End For loop over responses
        grDevices::graphics.off()
        
      }  # End For loop over stressors
      SSTVfile <- paste("Results/",TargetSiteID, "/", TargetSiteID, ".SSTVCorrs.txt", sep="")
      utils::write.table(df.CorrTable, file=SSTVfile, sep= "\t",quote=FALSE,
                         row.names=FALSE,col.names=TRUE)
    }##IF.stressor.SSTV.END
  }##IF.SSTV.END    
}##FUNCTION.END
