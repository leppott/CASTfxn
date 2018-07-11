#' @title BMI Stressor Responses
#' 
#' @description Get BMI stressor responses.
#' 
#' @details BMI stressor regressions.
#' 
#' Required objects:
#' 
#' * BMIresp
#' 
#' * TargetSiteID
#' 
#' @param stressors stressors
#' @param list.MatchBMIData list of matched BMI and stressor data.
#' 
#' @return A jpg in "Results" folder of working directory.  And a tab-delimited text file of stressor correlations.
#' 
#' @examples
#' predint <- 0.75
#' varLegLoc <- "topright"
#' BMIresp <- c("CSCI", "MMI_Score", "TotalTaxSPL_Sc", "DipTaxSPL_Sc"
#'              , "IntolTaxSPL_Sc", "HBISPL_Sc", "PlecoPct_Sc", "ScrapPctSPL_Sc"
#'              , "TrichTax_Sc", "EphemTax_Sc", "EphemPct_Sc", "Dom01PctSPL_Sc")
#'              
#' TargetSiteID <- "SRCKN001.61"
#' clustertype <- "5"
#' useLU <- FALSE
#' 
#' CurrentDir<-getwd()
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
#' # Run getBMIStressorResponses           
#' getBMIStressorResponses(stressors, list.MatchBMIData)
#
#' @export
getBMIStressorResponses <- function(stressors, list.MatchBMIData
                                    , predint=0.75, varLegLoc="topright") {

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
  
  BMIresp <- colnames(list.MatchBMIData[["all.b.rsp"]])[8:ncol(list.MatchBMIData[["all.b.rsp"]])]
  
  for (p in 1:length(stressors)) {
    # QC
    # print(p)
    p.len <- length(stressors)
    stressName <- stressors[p]
    varFlag <- 1
    varFlag.b <- 1
    if (stressName %in% c("DO_uf_mg_L", "pH_SU", "Temp_degC", "Flow_calc_cfs",
                          "Flow_cfs")) {
      log.yn <- FALSE
    } else {
      log.yn <- TRUE
    }
    for (q in 1:length(BMIresp)) { 
      respName <- BMIresp[q]
      # QC
      # print (q)
      q.len <- length(BMIresp)
      # get all data to plot
      all.xvar<- list.MatchBMIData[["all.b.str"]][,c("StationID_Master","BMI.Metrics.SampID", stressName)]
      all.yvar<- list.MatchBMIData[["all.b.rsp"]][,c("StationID_Master","BMI.Metrics.SampID", respName)]
      df.plot1 <- merge(all.xvar[,2:3],all.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
      if (nrow(df.plot1[stats::complete.cases(df.plot1),2:3]) < 20) { next }
      all.df.plot <- df.plot1[stats::complete.cases(df.plot1),2:3]

      #get all ref data to plot
      all.ref.xvar <- subset(all.xvar, all.xvar$StationID_Master %in% ref.sites)
      all.ref.yvar <- subset(all.yvar, all.yvar$StationID_Master %in% ref.sites)
      df.plot2 <- merge(all.ref.xvar[,2:3],all.ref.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
      all.ref.df.plot <- df.plot2[stats::complete.cases(df.plot2),2:3]
      
      #get all cluster data to plot
      cl.xvar<- list.MatchBMIData[["cl.b.str"]][,c("StationID_Master","BMI.Metrics.SampID", stressName)]
      cl.yvar<- list.MatchBMIData[["cl.b.rsp"]][,c("StationID_Master","BMI.Metrics.SampID", respName)]
      df.plot3 <- merge(cl.xvar[,2:3],cl.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
      cl.df.plot <- df.plot3[stats::complete.cases(df.plot3),2:3]
      
      #get all cluster ref data to plot
      cl.ref.xvar <- subset(cl.xvar, cl.xvar$StationID_Master %in% ref.sites)
      cl.ref.yvar <- subset(cl.yvar, cl.yvar$StationID_Master %in% ref.sites)
      df.plot4 <- merge(cl.ref.xvar[,2:3],cl.ref.yvar[,2:3], by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
      cl.ref.df.plot <- df.plot4[stats::complete.cases(df.plot4),2:3]
      
      #get target site data to plot
      site.xvar<- list.MatchBMIData[["site.b.str"]][,c("BMI.Metrics.SampID", stressName)]
      site.yvar<- list.MatchBMIData[["site.b.rsp"]][,c("BMI.Metrics.SampID", respName)]
      df.plot5 <- merge(site.xvar,site.yvar, by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
      site.df.plot <- df.plot5[stats::complete.cases(df.plot5),2:3]
      
      ppi<-300
      varFileOut = paste0("Results/",TargetSiteID, "/", TargetSiteID, "BMI.SR.")
      grDevices::jpeg(filename = paste(varFileOut, stressName, "_", respName,
                            ".jpg", sep = ""), width = 4 * ppi, 
           height = 3 * ppi, quality=100, pointsize=8, res = ppi)
      graphics::par(cex.main=0.8,cex.lab=0.7,font.main=2, font.lab=2
                    , mar=c(6,4,4,2)+0.1)
      if (log.yn == TRUE) {
        all.df.plot     <- cbind(log10(all.df.plot[,1]), all.df.plot[,2])
        all.ref.df.plot <- cbind(log10(all.ref.df.plot[,1]), all.ref.df.plot[,2])
        cl.df.plot      <- cbind(log10(cl.df.plot[,1]), cl.df.plot[,2])
        cl.ref.df.plot  <- cbind(log10(cl.ref.df.plot[,1]), cl.ref.df.plot[,2])
        site.df.plot    <- cbind(log10(site.df.plot[,1]), site.df.plot[,2])
      }
      
      varMain <- paste("Linear regression of", stressName, "on", respName
                       , "for", TargetSiteID, "\n","with", paste(predint*100, "th", sep= "")
                       , "percentile prediction interval", sep = " ")
      if (log.yn == TRUE) {
        varxlab <- paste("Log10", stressName)
      } else {
        varxlab <- stressName
      }
      # There should never be a case where either x or y are always NA for all data
      if (length(all.ref.df.plot) > 0) {
        graphics::plot(all.df.plot[,2]~all.df.plot[,1],main=varMain,
             xlab=varxlab,ylab=respName, col="darkgrey", pch=1, cex = 0.8,
             cex.axis = 0.8)
      } else {
        next
      }
      if (length(all.ref.df.plot) > 0) {
        graphics::points(all.ref.df.plot[,2]~all.ref.df.plot[,1], 
               col="blue", pch=16, cex = 0.8) # blue solid dots
      }
      if (length(cl.df.plot) > 0) {
        graphics::points(cl.df.plot[,2]~cl.df.plot[,1], 
               col="cyan4", pch=2, cex = 0.8) # Cyan open triangles
      }
      if (length(cl.ref.df.plot) > 0) {
        graphics::points(cl.ref.df.plot[,2]~cl.ref.df.plot[,1], 
               col="blue", pch=17, cex = 0.8) # Solid blue triangles
      }
      if (length(site.df.plot) > 0) {
        graphics::points(site.df.plot[,2]~site.df.plot[,1], 
               col="red", pch=19, cex = 1.0)  # Red solid dots
      }
      
      cl.x.sd <- stats::sd(cl.df.plot[,1])
      cl.y.sd <- stats::sd(cl.df.plot[,2])
      #Check for vertical line
      if (!is.na(cl.x.sd)) {
        if (cl.x.sd == 0) {
          print(paste("Vertical line for", stressName, respName, sep=" "))
          utils::flush.console()
          next     #It's okay to plot the points, but not the regression line
        }
      }
      #Check for horizontal line
      if (!is.na(cl.y.sd)) {
        if (cl.y.sd == 0) {
          print(paste("Horizontal line for", stressName, respName, sep=" "))
          utils::flush.console()
          next     #It's okay to plot the points, but not the regression line
        }
      }    
      
      #Linear Regression (uses cluster data -- all sites in the cluster)
      varY <- cl.df.plot[,2]
      varX <- cl.df.plot[,1]
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
      # r2 text and legend
      r = stats::cor(varX, varY, method="pearson",use="pairwise.complete.obs")
      r2 = formatC(r^2,format="f",digits=3)

      #
      c1S <- (stats::cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
      df.corr = data.frame(cbind(stressName, respName, signif(c1S$statistic,2)
                                 , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
      # # Create results data frame
      if (varFlag==1) {  #First time through loop
        df.CorrTable <- c(df.corr)
      } # IF, END
      df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
      pval.corr = signif(c1S$p.value,2)
      
      #Print equation, r2, and p-value
      if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
        eqn <- paste("Cluster regression: ", "y =", slope, "x +", intercept
                       , "; ", "r2 =", r2, "; ", "p-value =", pval.corr
                       ,"; ","n =",length(varX))
        symbshape <- c(1, 16, 2, 17, 19)
        symbcol <- c("darkgrey", "blue", "cyan4", "blue", "red")
        symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", 
                      TargetSiteID)
        graphics::mtext(eqn, side=1, line=4, bty="n", col=c("black"), cex=0.6)
        graphics::legend(varLegOpp, symbname, pch=symbshape, col=symbcol
                         , cex=0.6, lwd="1", bg="white")
      }##IF.length.END
      #
      
      # 20180621, scoring
      slope.dir <- sign(slope) #1 = positive, -1 = negative
      # exp.dir <- data.lkp.dir[stressName,respName]
      exp.dir <- -1

      for (f in 1:length(site.df.plot)) {
        # Generate scores based on slope, significance value, and r2
        if ((length(cl.df.plot)>=5) && (abs(pval.corr)<=0.1) && (r2>=0.1)) {
          # print to console p (stressName) and q (respName)
            if (slope.dir == exp.dir) {
                print(paste0(stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 1"))
                sr.score = 1
            } else if (slope.dir != exp.dir) {
                print(paste0(stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = -1"))
                sr.score = -1
            } else {
                print(paste0(stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = inconclusive"))
                sr.score = 1
            }
        } else {
            print(paste0(stressName, " (", p, "/", p.len, "), ", respName, " (", q, "/", q.len, "); score = 0"))
            sr.score = 0
        }
        df.temp2 <- as.data.frame(cbind("StationID_Master"=TargetSiteID, # "Group" = cluster,
                        "Param_Name"=stressName,"BMI_Metric"=respName,
                        "n"=length(site.df.plot),#"Param_Value"=varXprime[f],
                        #"BMI_MetricValue"=varYprime[f],
                        "SR_Score"=sr.score))
        if (varFlag.b==1) { # First time through this loop
            df.sc.sr <- rbind(df.temp2)
        } else {
            df.sc.sr <- rbind(df.sc.sr, df.temp2)
        }
        varFlag.b <- 0 # Set varFlag.b to zero
      }##FOR.f.END
      #
      grDevices::dev.off()
      #
      varFlag <- 0
    }##FOR.q.END
    grDevices::graphics.off()
  }##FOR.p.END
  utils::write.table(df.CorrTable,file="StressRespCorrs.BMI.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)  
  # utils::write.table(df.sc.sr,file="StressRespScores.BMI.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=TRUE)  
}##FUNCTION.END
