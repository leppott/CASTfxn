#' @title BMI Stressor Responses
#' 
#' @description Get BMI stressor responses.
#' 
#' @details BMI stressor regressions.
#' 
#' @param TargetSiteID Site ID
#' @param stressors stressors
#' @param BMIresp Benthic macroinvertebrate response variables.
#' @param list.MatchBMIData list of matched BMI and stressor data.
#' @param predint x. Default = 0.75
#' @param varLegLoc Plot legend location.  For regressions this will be opposite. Default="topright"
#' 
#' @return A jpg in SiteID subfoler of the "Results" folder of working directory.  
#' And two tab-delimited text files; stressor correlations and scores.
#' 
#' @importFrom pryr "%<a-%"
#' 
#' @examples
#' \dontrun{
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
# CurrentDir<-getwd()
# myDir.Data <- paste(CurrentDir,"data/",sep="/")
# 
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
#' getBMIStressorResponses(TargetSiteID, stressors, BMIresp, list.MatchBMIData)
#' }
#
#' @export
getBMIStressorResponses <- function(TargetSiteID, stressors, BMIresp, list.MatchBMIData
                                    , predint=0.75, varLegLoc="topright") {
  # QC
  boo.QC <- FALSE
  ## Trigger QC actions below for when debugging.

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
  
  #BMIresp <- colnames(list.MatchBMIData[["all.b.rsp"]])[16:ncol(list.MatchBMIData[["all.b.rsp"]])]
  
  #QC
  if(boo.QC==TRUE){##IF.boo.QC.START
    ##p
    stressors <- stressors[1:2]
    ##q 
    BMIresp <- BMIresp[1:3]
  }##IF.boo.QC.END

  
  # move from plotting section
  #p
  p.len <- length(stressors)
  #q
  q.len <- length(BMIresp)
  
  boo.pryr <- FALSE
  
  # Capture each plot in a list for the PDF
  #plots.pq <- vector(length(BMIresp), mode="list")
  plots.pq <- vector(q.len*p.len, mode="list")
  ppi<-300
  varFileOut = paste0("Results/",TargetSiteID, "/", TargetSiteID, ".SR.BMI.")
  
  # FOR.p ####
  for (p in 1:length(stressors)) {
    stressName <- stressors[p]
    varFlag <- 1
    varFlag.b <- 1

    if (stressName %in% c("DO_uf_mg_L", "pH_SU", "Temp_degC", "Flow_calc_cfs",
                          "Flow_cfs")) {##IF.stressName.START
        log.yn <- FALSE
      } else {
        log.yn <- TRUE
    }##IF.stressName.END
    # QC
    if(boo.QC==TRUE){##IF.boo.QC.START
      print(paste0("p; ",p))
      flush.console()
    }##IF.boo.QC.END

    # FOR.q ####
    for (q in 1:length(BMIresp)) { 
      varFlag <- 1
      varFlag.b <- 1
      respName <- BMIresp[q]
      pq <- q.len*(p-1)+q
      pq.len <- p.len * q.len
      
      # QC
      if(boo.QC==TRUE){##IF.boo.QC.START
        print(paste0("Item (", pq, "/", pq.len, ")"))
        print(paste0("q; ", respName))
        flush.console()
      }##IF.boo.QC.END
      


      {##NoIssues.START
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
      df.plot5 <- merge(site.xvar, site.yvar, by.x = "BMI.Metrics.SampID", by.y = "BMI.Metrics.SampID")
      site.df.plot <- df.plot5[stats::complete.cases(df.plot5),2:3]
      }##NoIssues.END
      
      # Plots ####

      # Plot parts
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
      
      ## Create Plot
      plot.pryr %<a-% {##pryr.START
        {##NoIssue.pryr
        graphics::par(cex.main=0.8,cex.lab=0.7,font.main=2, font.lab=2
                      , mar=c(6,4,4,2)+0.1)
        
        # moved out parts
          
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
        }##NoIssue.pryr
        
        # Correlation ####
        c1S <- (stats::cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
        df.corr = data.frame(cbind(stressName, respName, signif(c1S$statistic,2)
                                   , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
        names(df.corr) <- c("stressName", "respName", "statistic", "p.value", "estimate", "r2")
        # # Create results data frame
        if (varFlag==1) {  #First time through loop
          df.CorrTable <- df.corr
        } else {
          df.CorrTable=rbind(df.CorrTable,df.corr)  #  if not first iteration then append
        } # IF, END
        boo.Append    <- TRUE
        boo.col.names <- FALSE
        if (pq==1){
          boo.Append    <- !boo.Append
          boo.col.names <- !boo.col.names
        }
        if(boo.pryr==TRUE){
          fn_corr <- paste0(TargetSiteID,".SR.BMI.Corrs.txt")
          utils::write.table(df.CorrTable
                             , file.path(wd,dir.sub,dir.sub2,fn_corr)
                             , sep="\t", quote=FALSE, row.names=FALSE
                             , col.names=boo.col.names, append=boo.Append)  
        }
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
        
         # Scoring ####
        # 20180621, scoring
        slope.dir <- sign(slope) #1 = positive, -1 = negative
        # exp.dir <- data.lkp.dir[stressName,respName]
        exp.dir <- -1
  
        for (f in 1:nrow(site.df.plot)) {
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
         
        }##FOR.f.END
        #
        df.temp2 <- as.data.frame(cbind("StationID_Master"=TargetSiteID, # "Group" = cluster,
                                        "Param_Name"=stressName,"BMI_Metric"=respName,
                                        "n"=length(site.df.plot),#"Param_Value"=varXprime[f],
                                        #"BMI_MetricValue"=varYprime[f],
                                        "SR_Score"=sr.score))
        if (varFlag.b==1) { # First time through this loop
          df.sc.sr <- df.temp2
        } else {
          df.sc.sr <- rbind(df.sc.sr, df.temp2)
        }
        boo.Append    <- TRUE
        boo.col.names <- FALSE
        if (pq==1){
          boo.Append    <- !boo.Append
          boo.col.names <- !boo.col.names
        }
        if(boo.pryr==TRUE){
          fn_scores <- paste0(TargetSiteID,".SR.BMI.Scores.txt")
          utils::write.table(df.sc.sr
                             , file.path(wd,dir.sub,dir.sub2,fn_scores)
                             , sep="\t", quote=FALSE, row.names=FALSE
                             , col.names=boo.col.names, append=boo.Append) 
        }
        # Moved from inside FOR.f
      }##plot.pryr.END
      
      
      ## PDF, capture plot in list
      ### Need to run plot.pryr as is only created above
      boo.pryr <- TRUE
        plot.pryr
      boo.pryr <- FALSE
      #pq <- q.len*(p-1)+q
      plots.pq[[pq]] <- grDevices::recordPlot()
      
      ## JPG, Create
      grDevices::jpeg(filename = paste(varFileOut, stressName, "_", respName,
                                       ".jpg", sep = ""), width = 4 * ppi, 
                      height = 3 * ppi, quality=100, pointsize=8, res = ppi)
        plot.pryr
      grDevices::dev.off()
      #
      varFlag <- 0
      varFlag.b <- 0 # Set varFlag.b to zero
    }##FOR.q.END
    #grDevices::graphics.off()
  }##FOR.p.END
  
  # END ####
  # Create PDF from list
  fn_pdf <- file.path(getwd(), "Results", TargetSiteID, paste0(TargetSiteID,".SR.BMI.ALL.pdf"))
  pdf(file=fn_pdf, width=8)
  for (pq in plots.pq){##FOR.gp.START
    #grDevices::replayPlot(g.plot)
    if(is.null(pq)==TRUE) {next}
    grDevices::replayPlot(pq)
  }##FOR.gp.END
  grDevices::dev.off()
 # rm(plots.pq)
  #
  # utils::write.table(df.CorrTable
  #                    , file.path(wd,dir.sub,dir.sub2,"StressRespCorrs.BMI.txt")
  #                    , sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
  # utils::write.table(df.sc.sr
  #                    , file.path(wd,dir.sub,dir.sub2,"StressRespScores.BMI.txt")
  #                   , sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE) 
  #
  # CorrPlot ####
  ## read
  fn_corr <- paste0(TargetSiteID,".SR.BMI.Corrs.txt")
  df_corr <- read.delim(file.path(wd,dir.sub,dir.sub2,fn_corr))
  ## transpose
  df_corr_r <- reshape2::dcast(df_corr, stressName ~ respName, value.var="estimate")
  df_corrplot <- t(df_corr_r[,-1])
  colnames(df_corrplot) <- df_corr_r[,1]
  ## jpg
  fn_jpg_cp <- file.path(wd, dir.sub, dir.sub2, paste0(TargetSiteID, ".SR.BMI.CorrPlot.jpg"))
  grDevices::jpeg(filename = fn_jpg_cp
                  , width = 4 * ppi
                  , height = 3 * ppi
                  , quality=100
                  )
    corrplot::corrplot(df_corrplot, method="circle")
  grDevices::dev.off()
  #
}##FUNCTION.END
