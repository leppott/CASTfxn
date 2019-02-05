#' @title Algae Stressor Responses
#' 
#' @description Get Algae stressor responses.
#' 
#' @details Algae stressor regressions.
#' 
#' Required objects:
#' 
#' * AlgResp
#' 
#' * TargetSiteID
#' 
#' @param TargetSiteID Site ID
#' @param stressors stressors
#' @param AlgResp Algae response variables.
#' @param list.MatchAlgData list of matched Algae and stressor data.
#' @param LogTransf Value for if stressor variables should be log10 transformed; 1=TRUE, 0=FALSE.
#' @param predint Prediction interval. Default = 0.75
#' @param varLegLoc Plot legend location.  For regressions this will be opposite. Default="topright"
#' 
#' @keywords internal
#' 
#' @return A jpg in "Results" folder of working directory.  And a tab-delimited text file of stressor correlations.
#' 
#' @importFrom pryr "%<a-%"
#' 
#' @examples
#' \dontrun{
#' predint <- 0.75
#' varLegLoc <- "topright"
#' 
#' TargetSiteID <- "LCBEN002.57"
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
#' list.SiteSummary <- getSiteInfo(TargetSiteID)
#' 
#' # datasets getChemDataSubsets
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' 
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters)
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
#' # datasets getAlgMatches
#' ## remove "none"
#' stressors <- list.stressors$stressors[list.stressors$stressors != "none"]
#' stressors_logtransf <- list.stressors$stressors_LogTransf[list.stressors$stressors != "none"]
#'
#' # Run getAlgMatches
#' list.MatchAlgData <- getAlgMatches(stressors, list.data)
#' 
#' # data getAlgStressorResponses
#' data.algae.metrics <- data_AlgMetrics
#' AlgResp <- colnames(data.algae.metrics[6:ncol(data.algae.metrics)])
#' 
#' # Run getAlgStressorResponses
#' getAlgStressorResponses(TargetSiteID, stressors, AlgResp, list.MatchAlgData, stressors_logtransf)
#' }
#
#' @export
getAlgStressorResponses <- function(TargetSiteID, stressors, AlgResp, list.MatchAlgData
                                    , LogTransf, predint=0.75, varLegLoc="topright") {
  
  
  # QC
  boo.QC <- FALSE
  ## Trigger QC actions below for when debugging.
  
  # check for and create (if necessary) "Results" subdirectory of working directory
  wd <- getwd()
  dir.sub <- "Results"
  dir.sub2 <- TargetSiteID
  fp_dir <- file.path(wd, dir.sub, dir.sub2)
  ifelse(dir.exists(fp_dir)==FALSE
         , dir.create(fp_dir)
         , FALSE)
  #
  # helper
  RegPlotSet <- getRegPlotSet(varLegLoc)
  varInset  <- RegPlotSet[1]
  varSpacer <- RegPlotSet[2]
  varLegOpp <- RegPlotSet[3]
  
  #AlgResp <- colnames(list.MatchAlgData[["all.a.resp"]])[1:ncol(list.MatchAlgData[["all.a.resp"]])]

  #QC
  if(boo.QC==TRUE){##IF.boo.QC.START
    ##p
    stressors <- stressors[1:2]
    p<-1
    ##r
    AlgResp <- AlgResp[1:3]
    r<-1
  }##IF.boo.QC.END
  #p
  p.len <- length(stressors)
  #r
  r.len <- length(AlgResp)
  
  boo.pryr <- FALSE
  
  # Capture each plot in a list for the PDF
  #plots.pq <- vector(length(BMIresp), mode="list")
  plots.pr <- vector(p.len*r.len, mode="list")
  ppi<-300
  varFileOut = paste0("Results/",TargetSiteID, "/", TargetSiteID, ".SR.Alg.")
  
  LogTransf <- as.logical(LogTransf)
  
  # FOR.p ####
  for (p in 1:length(stressors)) {
    stressName <- stressors[p]
    
    varFlag <- 1
    
    # if (stressName %in% c("DO_uf_mg_L", "pH", "Temp_degC", "Flow_cfs", 
    #                       "Flow_calc_cfs")) {
    #   log.yn <- FALSE
    # } else {
    #   log.yn <- TRUE
    # }
    log.yn <- LogTransf[p]
    
    varFlag <- 1
    
    # QC
    if(boo.QC==TRUE){##IF.boo.QC.START
      # print(paste0("p (",p,"/",p.len,") ", stressName))
      # flush.console()
    }##IF.boo.QC.END
    
    # FOR.r ####
    for (r in 1:length(AlgResp)) {
      respName <- AlgResp[r]
      
      varFlag <- 1
      
      pr <- r.len*(p-1)+r
      pr.len <- p.len * r.len
      print(paste0("Item (", pr, "/", pr.len, "); p (",p,"/",p.len,") ", stressName,"; r (",r,"/",r.len,") ",respName))
      flush.console()
      # QC
      if(boo.QC==TRUE){##IF.boo.QC.START
        # noting
      }##IF.boo.QC.END
      

      {##NoIssues.START
      #get all data to plot
      all.xvar<- list.MatchAlgData[["all.a.str"]][,c("StationID_Master","Algae.Metrics.SampID", stressName)]
      all.yvar<- list.MatchAlgData[["all.a.rsp"]][,c("StationID_Master","Algae.Metrics.SampID", respName)]
      df.plot1 <- merge(all.xvar[,2:3],all.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "Algae.Metrics.SampID")
      all.df.plot <- df.plot1[stats::complete.cases(df.plot1),2:3]
      
      #get all ref   data to plot
      all.ref.xvar <- subset(all.xvar, all.xvar$StationID_Master %in% ref.sites)
      all.ref.yvar <- subset(all.yvar, all.yvar$Algae.Metrics.SampID %in% ref.sites)
      df.plot2 <- merge(all.ref.xvar[,2:3],all.ref.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "Algae.Metrics.SampID")
      all.ref.df.plot <- df.plot2[stats::complete.cases(df.plot2),2:3]
      
      #get all cluster data to plot
      cl.xvar<- list.MatchAlgData[["cl.a.str"]][,c("StationID_Master","Algae.Metrics.SampID", stressName)]
      cl.yvar<- list.MatchAlgData[["cl.a.rsp"]][,c("StationID_Master","Algae.Metrics.SampID", respName)]
      df.plot3 <- merge(cl.xvar[,2:3],cl.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "Algae.Metrics.SampID")
      cl.df.plot <- df.plot3[stats::complete.cases(df.plot3),2:3]
      
      #get all cluster ref data to plot
      cl.ref.xvar <- subset(cl.xvar, cl.xvar$Algae.Metrics.SampID %in% ref.sites)
      cl.ref.yvar <- subset(cl.yvar, cl.yvar$Algae.Metrics.SampID %in% ref.sites)
      df.plot4 <- merge(cl.ref.xvar[,2:3],cl.ref.yvar[,2:3], by.x = "Algae.Metrics.SampID", by.y = "Algae.Metrics.SampID")
      cl.ref.df.plot <- df.plot4[stats::complete.cases(df.plot4),2:3]
      
      #get target site data to plot
      site.xvar<- list.MatchAlgData[["site.a.str"]][,c("Algae.Metrics.SampID", stressName)]
      site.yvar<- list.MatchAlgData[["site.a.rsp"]][,c("Algae.Metrics.SampID", respName)]
      df.plot5 <- merge(site.xvar,site.yvar, by.x = "Algae.Metrics.SampID", by.y = "Algae.Metrics.SampID")
      site.df.plot <- df.plot5[stats::complete.cases(df.plot5),2:3]
      }##NoIssues.END
      
      # Plots ####
      # ppi<-300
      # varFileOut = paste0("Results/",TargetSiteID,"/", TargetSiteID, ".SR.Alg.")
      
      #20181218, move IF here so doesn't plot if no data.
      if (length(all.ref.df.plot) == 0) {
        print("SKIP, no data")
        flush.console()
        next
      }
      
      # plot.pryr ####
      plot.pryr %<a-% {##pryr.START
        # grDevices::jpeg(filename = paste(varFileOut, stressName, "_", respName, ".jpg", 
        #                   sep = ""), width = 4*ppi, height = 3*ppi, 
        #                 quality=100, pointsize=8, res = ppi)
        graphics::par(cex.main=1.0,cex.lab=0.9,font.main=2, font.lab=2
                      , mar=c(6,4,4,2)+0.1)
        if (log.yn == TRUE) {
          all.df.plot <- cbind(log10(all.df.plot[,1]),all.df.plot[,2])
          all.ref.df.plot <- cbind(log10(all.ref.df.plot[,1]),all.ref.df.plot[,2])
          cl.df.plot <- cbind(log10(cl.df.plot[,1]),cl.df.plot[,2])
          cl.ref.df.plot <- cbind(log10(cl.ref.df.plot[,1]),cl.ref.df.plot[,2])
          site.df.plot <- cbind(log10(site.df.plot[,1]),site.df.plot[,2])
        }
        
        varMain <- paste(TargetSiteID, "\n","with", paste(predint*100, "th", sep= "")
                         , "percentile prediction interval", sep = " ")
        if (log.yn == TRUE) {
          varxlab <- paste("Log10", stressName)
        } else {
          varxlab <- stressName
        }
        
        # There should never be a case where either x or y are always NA for all data
        #if (length(all.ref.df.plot) > 0) {
          graphics::plot(all.df.plot[,2]~all.df.plot[,1],
               main=varMain, xlab=varxlab,ylab=respName, 
               col="darkgrey", pch=1, cex=0.8, cex.axis=0.8)
        # } else {
        #   # Create empty plot, should have outside of graphics device
        #   par(mar = c(0,0,0,0))
        #   plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
        #   text(x = 0.5, y = 0.5, paste0("No data to plot.\n", stressName,"\n", respName)
        #        , cex = 1.6, col = "black")
        #   dev.off()
        #   next
        # }
        
        if (length(all.ref.df.plot) > 0) {
          graphics::points(all.ref.df.plot[,2]~all.ref.df.plot[,1], 
                 col="blue", pch=16, cex=0.8) # blue solid dots
        }
        
        if (length(cl.df.plot) > 0) {
          graphics::points(cl.df.plot[,2]~cl.df.plot[,1], 
                 col="cyan4", pch=2, cex=0.8) # Red open triangles
        }
        
        if (length(cl.ref.df.plot) > 0) {
          graphics::points(cl.ref.df.plot[,2]~cl.ref.df.plot[,1], 
                 col="blue", pch=17, cex=0.8) # Solid blue triangles
        }
        
        if (length(site.df.plot) > 0) {
          graphics::points(site.df.plot[,2]~site.df.plot[,1], 
                 col="red", pch=19, cex = 1.0) # black solid dots
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
            #next     #It's okay to plot the points, but not the regression line
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
        graphics::abline(stats::lm(pred.lower~varX), col="cyan4", lwd=1.0)
        graphics::abline(stats::lm(pred.upper~varX), col="cyan4", lwd=1.0)
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
        
        # Correlation ####
        c1S <- (stats::cor.test(varX,varY,method="pearson",use="pairwise.complete.obs"))
        df.corr = data.frame(cbind(stressName, respName, signif(c1S$statistic,2)
                                   , signif(c1S$p.value,2), signif(c1S$estimate,2), r2))
        names(df.corr) <- c("stressName", "respName", "statistic", "p.value", "estimate", "r2")
       
         # # Create results data frame
        if (varFlag==1) {  #First time through loop
          df.CorrTable <- df.corr
        } else {
           df.CorrTable <- rbind(df.CorrTable, df.corr)  #  if not first iteration then append
        }# IF, END
        #df.CorrTable <- rbind(df.CorrTable,df.corr)  #  if not first iteration then append
        boo.Append    <- TRUE
        boo.col.names <- FALSE
        if (pr==1){
          boo.Append    <- !boo.Append
          boo.col.names <- !boo.col.names
        }
        if(boo.pryr==TRUE){
          fn_corr <- paste0(TargetSiteID,".SR.Alg.Corrs.txt")
          utils::write.table(df.CorrTable
                             , file.path(wd,dir.sub,dir.sub2,fn_corr)
                             , sep="\t", quote=FALSE, row.names=FALSE
                             , col.names=boo.col.names, append=boo.Append)  
        }
        pval.corr <- signif(c1S$p.value,2)
        
        #Print equation, r2, and p-value
        if ((length(varX[!is.na(varX)]) > 2) || (length(varY[!is.na(varY)])) > 2) {
          eqn <- paste("Cluster regression: "
                       , "y =", slope, "x +", intercept, "; ", "r2 = ",r2,"; "
                       ,"p-value =",pval.corr,"; ","n =",length(varX),"\n")
          symbshape <- c(1, 16, 2, 17, 19)
          symbcol <- c("darkgrey", "blue", "cyan4", "blue", "red")
          symbname <- c("All data", "All reference", "Cluster data", "Cluster reference", TargetSiteID)
          graphics::mtext(eqn, side=1, line=5, bty="n", col = c("black"), cex=0.6)
          #graphics::legend(varLegOpp, symbname, inset=varInset, pch=symbshape, col=symbcol, cex=0.6)
          graphics::legend(varLegOpp, symbname, pch=symbshape, col=symbcol, cex=0.6)
        }
        #grDevices::dev.off()
      }##pryr.END
      
      ## Plot, Single JPG ####
      boo.pryr <- TRUE
        plot.pryr
      boo.pryr <- FALSE
      #
      plots.pr[[pr]] <- grDevices::recordPlot()
      
      ## JPG 
      grDevices::jpeg(filename = paste0(varFileOut, stressName, "_", respName, ".jpg")
                      , width = 4*ppi, height = 3*ppi, quality=100, pointsize=8, res = ppi)
        plot.pryr
      grDevices::dev.off()
      
      
      
      
      varFlag <- 0
    }##FOR.r.END
    #grDevices::graphics.off()
  }##FOR.p.END
  
    # END ####
    ## PDF ####
    # Create PDF from list
    fn_pdf <- file.path(getwd(), "Results", TargetSiteID, paste0(TargetSiteID,".SR.Alg.ALL.pdf"))
    pdf(file=fn_pdf, width=8)
    for (pr in plots.pr){##FOR.gp.START
      #grDevices::replayPlot(g.plot)
      if(is.null(pr)==TRUE) {next}
      grDevices::replayPlot(pr)
    }##FOR.gp.END
    grDevices::dev.off()
  
  
   #if(exists("df.CorrTable")==TRUE){##IF.exists.START
    #   fn_CorrTable <- paste0(TargetSiteID,".SR.Alg.Corrs.txt")
    #   utils::write.table(df.CorrTable
    #                      ,file=file.path(wd,dir.sub,dir.sub2, fn_CorrTable)
    #                      ,sep="\t"
    #                      ,quote=FALSE
    #                      ,row.names=FALSE
    #                      ,col.names=TRUE
    #   )  
    
    ## CorrPlot ####
    ## read
    fn_corr <- paste0(TargetSiteID,".SR.Alg.Corrs.txt")
    df_corr <- read.delim(file.path(wd,dir.sub,dir.sub2,fn_corr))
    ## transpose
    df_corr_r <- reshape2::dcast(df_corr, stressName ~ respName, value.var="estimate")
    df_corrplot <- t(df_corr_r[,-1])
    colnames(df_corrplot) <- df_corr_r[,1]
    ## jpg
    fn_jpg_cp <- file.path(wd, dir.sub, dir.sub2, paste0(TargetSiteID, ".SR.Alg.CorrPlot.jpg"))
    grDevices::jpeg(filename = fn_jpg_cp
                    , width = 4 * ppi
                    , height = 3 * ppi
                    , quality=100
    )
    corrplot::corrplot(df_corrplot, method="circle")
    grDevices::dev.off()
  #}##IF.exists.END
 
  #
  
}##FUNCTION.END
