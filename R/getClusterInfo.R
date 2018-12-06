#' @title Cluster Info
#' 
#' @description Get cluster information.
#' 
#' @details Summary cluster information
#' 
#' Required objects:
#' 
#' * TargetSiteID
#' 
#' * data.cluster; COMID, H6_noland, H6_land, ElevWs, WsAreaSqKm, PrecipWs, TmeanWs, W___AGRIC, W___URBAN, W___FOREST
#'  
#' @param site.COMID SiteID
#' @param clustertype Cluster type.
#' @param siteClusters site clusters.
#' @param refSiteCOMIDs reference site COMIDs
#' @param useLU Use LandUse.  Default = FALSE.
#' 
#' @return A jpeg in the "Results" subdirectory of the working directory.
#' 
#' @importFrom pryr "%<a-%"
#' 
#' @examples
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
#' 
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#'  
#' # datasets getChemDataSubsets
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#'
#' #
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#' 
#' # datasets getClusterInfo
#' ref.reaches <- list.data$ref.reaches
#' refSiteCOMIDs <- list.data$ref.reaches
#' 
#' # Run getClusterInfo
#' getClusterInfo(site.COMID, clustertype, site.Clusters, ref.reaches, useLU)
#' 
#' @export
getClusterInfo <- function(site.COMID, clustertype, siteClusters, refSiteCOMIDs, 
                           useLU = FALSE) {##FUNCTION.START
  # check for and create (if necessary) "Results" subdirectory of working directory
  wd <- getwd()
  dir.sub <- "Results"
  dir.sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  #
  if (length(site.Clusters)==0) {
    # do not proceed
    # no cluster assignment
    stop(paste("No cluster assignment for", TargetSiteID, sep = " "))
  }
  
  if (useLU == FALSE) {##IF.useLU.START
    varMain = "Clusters w/o Land Use"
    cluster <- "clust_noland"
  } else {
    varMain = "Clusters w/ Land Use"
    cluster <- "clust_land"
  }
  
  data.cluster.mySites <- data.cluster[data.cluster$COMID %in% site.COMID,]
  df.plot.3 <- data.cluster[data.cluster$COMID %in% refSiteCOMIDs,]
  df.plot.2 <- data.cluster.mySites
  df.plot <- data.cluster

  # Plots ####
  # Capture each plot in a list for the PDF
  plots.i <- vector(ncol(data.cluster.mySites)-1, mode="list")
  ppi<-300
  for (i in 2:ncol(data.cluster.mySites)) {##FOR.i.START
    #
    varYlab <- colnames(data.cluster.mySites)[i]
    #
    # QC
    i.num <- i -1
    i.len <- ncol(data.cluster.mySites) - 1
    i.var <- varYlab
    print(paste0("Processing item, ", i.num, "/", i.len, "; ", i.var))
    utils::flush.console()
    #
    myY <- df.plot[,i]
    myX <- df.plot[,cluster]
    #
    # QC
    if(sum(!is.na(myY))==0 || is.numeric(myY)==FALSE){##IF.myY.START
      print("No data, next")
      utils::flush.console()
      next
    }##IF.myY.END
    #
    plot.pryr %<a-% {##pryr.START
      #
      graphics::boxplot(myY~myX, main = varMain, xlab ="Cluster"
                        , ylab = varYlab, medlwd = 0.8, boxwex = 0.5, boxlty = 1
                        , boxlwd = 0.8, col ="lightgray")
      #~~~~~~~~~~~~~
      # add points to plots for reference sites
      myY <- df.plot.3[,i]
      myX <- df.plot.3[,cluster]
      graphics::points(myX,myY,col="blue",cex=0.7,pch=19)
      #~~~~~~~~~~~~~
      # add points to plots for selected sites
      myY <- df.plot.2[,i]
      myX <- df.plot.2[,cluster]
      graphics::points(myX,myY,col="red",cex=0.8,pch=19)
      #
    }##pryr.END
    #
    # PDF, capture plot in list
    plot.pryr
    plots.i[[i-1]] <- grDevices::recordPlot()
    #
    # JPG, Create
    grDevices::jpeg(filename = paste0("Results/",TargetSiteID,"/",
                                      TargetSiteID,".cluster.",varYlab,".jpg"),
                width = 4*ppi, height = 3*ppi, pointsize = 8,
                quality = 100, bg = "white", res = ppi)
      plot.pryr  
    grDevices::dev.off() ##JPEG.END
    #
  }##FOR.i.END
  #
  #grDevices::graphics.off() 
  # Create PDF from list
  fn_pdf <- file.path(getwd(), "Results", TargetSiteID, paste0(TargetSiteID,".cluster.AllGroups.pdf"))
  pdf(file=fn_pdf)
  for (ii in plots.i){##FOR.gp.START
    #grDevices::replayPlot(g.plot)
    if(is.null(ii)==TRUE) {next}
    grDevices::replayPlot(ii)
  }##FOR.gp.END
  grDevices::dev.off()
  rm(plots.i)
  #
}##FUNCTION.END
