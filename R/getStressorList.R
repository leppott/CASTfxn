#' @title Stressor List
#' 
#' @description Get stressor list.
#' 
#' @details Box plots of each stressor, grouped by category.
#' 
#' Required objects:
#' 
#' * clustertype
#' 
#' * useLU
#' 
#' @param TargetSiteID Site ID
#' @param site.Clusters Clusters
#' @param chem.info chem information
#' @param cluster.chem chem data cluster.
#' @param cluster.samps sample cluster.
#' @param ref.sites reference sites
#' @param site.chem Chem sites
#' @param probsHigh probabilities, high
#' @param probsLow probabilities, low
#' 
#' @return A jpeg in the "Results" subdirectory of the working directory with box plots.
#' Also returns a list of stressors; stressors and site.stressor.pctrank.
#' 
#' @examples
#' #No example at this time. 
#' 
#' @export
getStressorList <- function(TargetSiteID, site.Clusters, chem.info, cluster.chem
                            , cluster.samps, ref.sites, site.chem
                            , probsHigh, probsLow) {
  
  stations <- TargetSiteID
  nolu.cluster <- paste(clustertype, "_noland", sep="")
  lu.cluster <- paste(clustertype, "_land", sep="")
  
  cluster.chem.data <- cluster.chem[3:ncol(cluster.chem)]
  cluster.ref.chem <- subset(cluster.chem, cluster.chem$StationID_Master %in% ref.sites)
  cluster.ref.chem.data <- cluster.ref.chem[3:ncol(cluster.ref.chem)]
  chemnames <- names(cluster.chem[,3:ncol(cluster.chem.data)])
  allcount <- apply(cluster.chem.data, 2, function(x) sum(!is.na(x)))
  alltype <- unlist(lapply(1:ncol(cluster.chem.data), function(x) is.numeric(cluster.chem[,x])))
  coolvar <- names(allcount)[allcount>2 & alltype]
  
  groupnames <- unique(subset(chem.info, chem.info$ConvertTo %in% chemnames, select = "GroupName"))
  numgps <- length(groupnames[,1])
  
  ppi <- 300
  
  for (g in 1:numgps) {    # Generate 1 box plot for each group, ref sites in blue, target site in red
    gpchems <- subset(chem.info, GroupName == groupnames[g,], select = "ConvertTo")
    gpcoolvar <- subset(coolvar, coolvar %in% gpchems$ConvertTo)
    n <- length(gpcoolvar)
    if(n>0) { ##FOR.n.START
      grDevices::jpeg(filename = paste0("Results/boxes.example.",TargetSiteID
                             , ".", groupnames[g,], ".jpg"), width = 4*ppi
           , height = 3*ppi, pointsize = 8
           , quality = 100, bg = "white", res = ppi)
      maintitle <- paste(groupnames[g,], "Standardized values, All sites in cluster", sep=", ")
      graphics::par(mfrow = c(1,1), mar = c(4,8,1,1))
      if (useLU == TRUE) {
        labmain = paste(stations, ": Cluster", site.Clusters[1,lu.cluster])
      } else {
        labmain = paste(stations, ": Cluster", site.Clusters[1,nolu.cluster])
      }
      labx = paste(maintitle, labmain, sep = "\n")
      graphics::plot(y= 1:n, x= stats::runif(n,0,1), axes = F, type="n", xlab = "", ylab ="",
           xlim = c(0,1), cex.lab = 0.8)
      graphics::title(xlab=labx, line = 1, cex.lab = 0.8)
      # axis(1, at = seq(0,1, 0.2),labels = seq(0,1, 0.2))
      graphics::axis(2, at = 1:n, labels = gpcoolvar[1:n], las =1, cex.axis = 0.6)
      for(i in 1:n) {
        xvar <- cluster.chem[,gpcoolvar[i]]; dif <- diff(range(xvar, na.rm =T))
        newvar <- (xvar-min(xvar, na.rm=T))/dif
        graphics::boxplot(newvar, at = i,boxwex=0.5, horizontal =T, add =T,axes = F
                , outcex = 0.6, staplewex = 1, medlwd = 0.9, boxlwd = 0.8)
        good.ref.data <- cluster.ref.chem.data[,gpcoolvar[i]][!is.na(cluster.ref.chem[,gpcoolvar[i]])]
        if (length(good.ref.data) != 0) {
          point2 <- (cluster.ref.chem.data[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif 
          graphics::points(point2, rep(i,length(point2)), col = "blue", pch = 15,cex=0.6, bg = 2)
        }
        point1 <- (site.chem[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif 
        graphics::points(point1, rep(i,length(point1)), col = "red", pch = 19,cex=0.6, bg = 2)
      }
      graphics::box(bty="l")
      grDevices::dev.off()
    }
    
  }
  chem.pctrank <- apply(cluster.chem[,3:ncol(cluster.chem)], 2, function(x) dplyr::percent_rank(x))
  data.chem.pctrank <- as.data.frame(chem.pctrank)
  data.chem.pctrank <- cbind(cluster.chem$StationID_Master,
                             cluster.chem$ChemSampleID,data.chem.pctrank)
  colnames(data.chem.pctrank)[1] <- "StationID_Master"
  colnames(data.chem.pctrank)[2] <- "ChemSampleID"
  row.names(data.chem.pctrank) <- NULL
  utils::write.table(data.chem.pctrank, file = paste("Results/chem.pctrank.",
                                              TargetSiteID,".txt", sep=""),sep="\t", col.names=TRUE)
  site.pctrank <- subset(data.chem.pctrank, StationID_Master==TargetSiteID)
  stressor <- c("none")
  for (c in 3:ncol(site.pctrank)) {
    chemname <- colnames(site.pctrank)[c]
    bad <- is.na(site.pctrank[,c])
    check <- site.pctrank[,c]
    good <- check[!bad]
    maxSiteVal <- max(good)
    minSiteVal <- min(good)
    if ((chemname == "DO_uf_mg_L") || (chemname == "pH")) {
      if (minSiteVal <= probsLow) {
        stressor <- c(stressor, chemname)
      }
    }
    if ((chemname != "DO_uf_mg_L") && (maxSiteVal >= probsHigh)) {
      stressor <- c(stressor, chemname)
    }
  }
  stressorlist <- stressor
  myStressors <- list(stressors = stressorlist, site.stressor.pctrank = site.pctrank)
  #
  return(myStressors)
} # FUN end