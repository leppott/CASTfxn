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
#' @examples
#' 
#' TargetSiteID <- "SRCKN001.61"
#' clustertype <- "5"
#' useLU <- FALSE
#' 
#' CurrentDir<-getwd()
#' myDir.Data <- paste(CurrentDir,"data/",sep="/")
#' 
#' # Run getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.cluster       <- data_Cluster_Hi
#' data.mod           <- data_ReachMod
#' 
#' list.SiteSummary <- getSiteInfo(TargetSiteID, clustertype, useLU)
#'  
#' # Run getChemDataSubsets
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' #
#' list.data <- getChemDataSubsets(TargetSiteID, site.COMID, site.Clusters, clustertype, useLU)
#' #
#' ref.reaches <- list.data$ref.reaches
#' 
#' getClusterInfo(site.COMID, clustertype, site.Clusters, ref.reaches, useLU)
#' 
#' @export
getClusterInfo <- function(site.COMID, clustertype, siteClusters, refSiteCOMIDs, useLU = FALSE) {
  nolu.cluster <- paste(clustertype, "_noland", sep="")
  lu.cluster <- paste(clustertype, "_land", sep="")
  if (length(siteClusters)==0) {
    # do not proceed
    # no cluster assignment
    stop(paste("No cluster assignment for", TargetSiteID, sep = " "))
  }
  
  # check for and create (if necessary) "Results" subdirectory of working directory
  wd <- getwd()
  dir.sub <- "Results"
  ifelse(!dir.exists(file.path(wd, dir.sub))==TRUE
         , dir.create(file.path(wd, dir.sub))
         , FALSE)
  #
  
  data.cluster.mySites <- data.cluster[data.cluster$COMID %in% site.COMID,]
  df.plot.3 <- data.cluster[data.cluster$COMID %in% refSiteCOMIDs,]
  
  ppi<-300
  grDevices::jpeg(filename = paste0("Results/cluster.example.",TargetSiteID, ".jpg"),
       width = 4*ppi, height = 3*ppi, pointsize = 8,
       quality = 100, bg = "white", res = ppi)
    #
    if (useLU == FALSE) {##IF.useLU.START
      selvar <- c("WsAreaSqKm","PrecipWs","TmeanWs","SLOPE","MAXELEVSMO")
      varnames <- c("WS Area", "WS Precipitation","Mean Temp", "Slope", "Max Elevation")
      graphics::par(mfrow = c(2,3), mar = c(2,4,1,1))
      df.plot <- data.cluster
      df.plot.2 <- data.cluster.mySites
      for(ii in 1:length(selvar)) {##FOR.ii.START
        myY <- df.plot[,selvar[ii]]
        myX <- df.plot[,nolu.cluster]
        graphics::boxplot(myY~myX, main = "Clusters w/o Land Use", xlab ="Cluster"
                , ylab = varnames[ii], medlwd = 0.8, boxwex = 0.5, boxlty = 1
                , boxlwd = 0.8, col ="lightgray")
        #~~~~~~~~~~~~~
        # add points to plots for reference sites
        myY <- df.plot.3[,selvar[ii]]
        myX <- df.plot.3[,nolu.cluster]
        graphics::points(myX,myY,col="blue",cex=0.7,pch=19)
        #~~~~~~~~~~~~~
        # add points to plots for selected sites
        myY <- df.plot.2[,selvar[ii]]
        myX <- df.plot.2[,nolu.cluster]
        graphics::points(myX,myY,col="red",cex=0.8,pch=19)
        #~~~~~~~~~~~~~
      }##FOR.ii.END
    } else {
      data.cluster.mySites <- data.cluster[data.cluster$COMID %in% site.COMID,]
      selvar <- c("WsAreaSqKm","PrecipWs","TmeanWs","SLOPE","MAXELEVSMO")
      varnames <- c("W_Area", "WS Precipitation","Mean Temp", "Slope", "Max Elevation")
      graphics::par(mfrow = c(2,3), mar = c(2,4,1,1))
      df.plot <- data.cluster
      df.plot.2 <- data.cluster.mySites
      for(ii in 1:length(selvar)) {##FOR.ii.START
        myY <- df.plot[,selvar[ii]]
        myX <- df.plot[,lu.cluster]
        graphics::boxplot(myY~myX, main = "Clusters w/Land Use", xlab ="Cluster"
                , ylab = varnames[ii], medlwd = 0.8, boxwex = 0.5, boxlty = 1
                , boxlwd = 0.8, col ="lightgray")
        #~~~~~~~~~~~~~
        # add points to plots for reference sites
        myY <- df.plot.3[,selvar[ii]]
        myX <- df.plot.3[,lu.cluster]
        graphics::points(myX,myY,col="blue",cex=0.7,pch=19)
        #~~~~~~~~~~~~~
        # add points to plots for selected sites
        myY <- df.plot.2[,selvar[ii]]
        myX <- df.plot.2[,lu.cluster]
        graphics::points(myX,myY,col="red",cex=0.8,pch=19)
        #~~~~~~~~~~~~~
      }##FOR.ii.END
      #
    }##IF.useLU.END
    #
  grDevices::dev.off()  ##JPEG.END
}