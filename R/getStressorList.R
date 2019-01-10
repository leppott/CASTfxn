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
#' @importFrom pryr "%<a-%"
#' 
#' @examples
#' TargetSiteID <- "SRCKN001.61"
#' clustertype <- "5"
#' useLU <- FALSE
#' 
# CurrentDir<-getwd()
#  myDir.Data <- paste(CurrentDir,"data/",sep="/")
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
#                                  
#' @export
getStressorList <- function(TargetSiteID, site.Clusters, chem.info, cluster.chem
                            , cluster.samps, ref.sites, site.chem
                            , probsHigh, probsLow, useLU=FALSE) {
  #
  # check for and create (if necessary) "Results" subdirectory of working directory
  wd <- getwd()
  dir.sub <- "Results"
  dir.sub2 <- TargetSiteID
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  #
  stations <- TargetSiteID
  nolu.cluster <- "clust_noland"
  lu.cluster <- "clust_land"
  
  cluster.chem.data <- cluster.chem[3:ncol(cluster.chem)]
  cluster.ref.chem <- subset(cluster.chem, cluster.chem$StationID_Master %in% ref.sites)
  cluster.ref.chem.data <- cluster.ref.chem[3:ncol(cluster.ref.chem)]
  chemnames <- names(cluster.chem[,3:ncol(cluster.chem.data)])
  allcount <- apply(cluster.chem.data, 2, function(x) sum(!is.na(x)))
  alltype <- unlist(lapply(1:ncol(cluster.chem.data), function(x) is.numeric(cluster.chem[,x])))
  coolvar <- names(allcount)[allcount>2 & alltype]
  
  groupnames <- unique(subset(chem.info, chem.info$ConvertTo %in% chemnames, select = "GroupName"))
  numgps <- length(groupnames[,1])
  
  # Plots ####
  ppi <- 300
  # Capture each plot in a list for the PDF
  ## https://stackoverflow.com/questions/13273611/how-to-append-a-plot-to-an-existing-pdf-file
  ## https://www.andrewheiss.com/blog/2016/12/08/save-base-graphics-as-pseudo-objects-in-r/
  plots.g <- vector(numgps, mode="list")
  # Generate 1 box plot for each group, ref sites in blue, target site in red
  for (g in 1:numgps) {##FOR.g.START
    gpchems <- subset(chem.info, GroupName == groupnames[g,], select = "ConvertTo")
    gpcoolvar <- subset(coolvar, coolvar %in% gpchems$ConvertTo)
    n <- length(gpcoolvar)
    if(n>0) { ##FOR.n.START
      plot.pryr %<a-% {##pryr.START
        maintitle <- paste(groupnames[g,], "Standardized values, All sites in cluster", sep=", ")
        graphics::par(mfrow = c(1,1), mar = c(4,8,1,1))
        if (useLU == TRUE) {##IF.useLU.START
          labmain = paste(stations, ": Cluster", site.Clusters[1,lu.cluster])
        } else {
          labmain = paste(stations, ": Cluster", site.Clusters[1,nolu.cluster])
        }##IF.useLU.END
        labx = paste(maintitle, labmain, sep = "\n")
        graphics::plot(y= 1:n, x= stats::runif(n,0,1), axes = F, type="n", xlab = "", ylab ="",
             xlim = c(0,1), cex.lab = 0.8)
        graphics::title(xlab=labx, line = 1, cex.lab = 0.8)
        graphics::axis(2, at = 1:n, labels = gpcoolvar[1:n], las =1, cex.axis = 0.6)
        for(i in 1:n) {##FOR.i.START
          xvar <- cluster.chem[,gpcoolvar[i]]; dif <- diff(range(xvar, na.rm =T))
          newvar <- (xvar-min(xvar, na.rm=T))/dif
          graphics::boxplot(newvar, at = i,boxwex=0.5, horizontal =T, add =T,axes = F
                  , outcex = 0.6, staplewex = 1, medlwd = 0.9, boxlwd = 0.8)
          good.ref.data <- cluster.ref.chem.data[,gpcoolvar[i]][!is.na(cluster.ref.chem[,gpcoolvar[i]])]
          if (length(good.ref.data) != 0) {##IF.length.START
            point2 <- (cluster.ref.chem.data[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif 
            graphics::points(point2, rep(i,length(point2)), col = "blue", pch = 15,cex=0.6, bg = 2)
          }##IF.length.END
          point1 <- (site.chem[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif 
          graphics::points(point1, rep(i,length(point1)), col = "red", pch = 19,cex=0.6, bg = 2)
        }##FOR.i.NED
        graphics::box(bty="l")  
      }##pryr.END

      # PDF, capture plot in list
      #lst.plots.g[[g]] <- grDevices::recordPlot()
      #plots.g[[g]] <- plot.pryr
      #assign(paste0("plot_",g),plot.pryr)
      plot.pryr
      plots.g[[g]] <- grDevices::recordPlot()
      
      # JPG, create
      grDevices::jpeg(filename = paste0("Results/",TargetSiteID,"/",TargetSiteID,
                                        ".boxes.", make.names(groupnames[g,]), ".jpg"), width = 4*ppi,
                      height = 3*ppi, pointsize = 8, quality = 100, bg = "white",
                      res = ppi)
        plot.pryr
      grDevices::dev.off()
    }##IF.n.END
  }##FOR.g.END
  
  # Create PDF from list
  fn_pdf <- file.path(getwd(), "Results", TargetSiteID, paste0(TargetSiteID,".boxes.ALL.pdf"))
  pdf(file=fn_pdf)
  for (i in plots.g){##FOR.gp.START
    #grDevices::replayPlot(g.plot)
    if(is.null(i)==TRUE) {next}
    grDevices::replayPlot(i)
  }##FOR.gp.END
  grDevices::dev.off()
  rm(plots.g)
  
  # Data File ####
  chem.pctrank <- apply(cluster.chem[,3:ncol(cluster.chem)], 2, function(x) dplyr::percent_rank(x))
  data.chem.pctrank <- as.data.frame(chem.pctrank)
  data.chem.pctrank <- cbind(cluster.chem$StationID_Master,
                             cluster.chem$ChemSampleID,data.chem.pctrank)
  colnames(data.chem.pctrank)[1] <- "StationID_Master"
  colnames(data.chem.pctrank)[2] <- "ChemSampleID"
  row.names(data.chem.pctrank) <- NULL
  utils::write.table(data.chem.pctrank, file = paste("Results/",TargetSiteID,
                    "/",TargetSiteID,".chem.pctrank.txt", sep=""),sep="\t", 
                    col.names=TRUE)
  site.pctrank <- subset(data.chem.pctrank, StationID_Master==TargetSiteID)
  stressor <- c("none")
  for (c in 3:ncol(site.pctrank)) {
    chemname <- colnames(site.pctrank)[c]
    bad <- is.na(site.pctrank[,c])
    check <- site.pctrank[,c]
    good <- check[!bad]
    maxSiteVal <- max(good, na.rm = TRUE)
    minSiteVal <- min(good, na.rm = TRUE)
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
  
  # LogTransf ####
  # 20190110, get log transformation code from chem.info
  # define pipe
  `%>%` <- dplyr::`%>%`
  #x <- unique(chem.info[chem.info$StdParamName %in% stressorlist, c("StdParamName", "LogTransf")])
  # need to use max (default of 1) in case of duplicates
  chem.info_LogTransf <- chem.info %>% 
                             group_by(StdParamName) %>% 
                                summarise(max_LogTransf=max(LogTransf))
  stressorlist4merge <- data.frame(StdParamName=stressorlist, Sort=1:length(stressorlist))
  # merge
  LogTransf_merge <- merge(stressorlist4merge, chem.info_LogTransf, all.x=TRUE)
  # sort 
  LogTransf_merge <- LogTransf_merge[order(LogTransf_merge$Sort), ]
  # NA to 0
  LogTransf_merge[is.na(LogTransf_merge[,"max_LogTransf"]), "max_LogTransf"] <- 0
  
  # create output ####
  myStressors <- list(stressors = stressorlist, site.stressor.pctrank = site.pctrank
                      , stressors_LogTransf=LogTransf_merge$max_LogTransf)
  #
  return(myStressors)
} # FUN end