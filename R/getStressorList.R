#' @title Stressor List
#' 
#' @description Get stressor list.
#' 
#' @details Box plots of each stressor, grouped by category.
#' 
#' Required objects:  all specified as inputs.
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
# @importFrom pryr "%<a-%"
#' 
#' @examples
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
#' 
#' # Data getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites        # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.cluster       <- data_Cluster_Hi   # need for getSiteInfo and getChemDataSubsets
#' data.mod           <- data_ReachMod
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
#' # data import, example 
#' # data.chem.raw <- read.delim(paste(myDir.Data,"data.chem.raw.tab",sep=""),na.strings = c(""," "))
#' # data.chem.info <- read.delim(paste(myDir.Data,"data.chem.info.tab",sep=""))
#' # data, example included with package
#' site.COMID <- list.SiteSummary$COMID
#' site.Clusters <- list.SiteSummary$ClustIDs
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' 
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=site.Clusters
#'                                 , data.cluster=data.cluster, data.Stations.Info=data.Stations.Info
#'                                 , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
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
                            , probsHigh, probsLow) {
  #
  useLU <- FALSE
  
  # DEBUGGING ####
  boo.DEBUG <- FALSE
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    g <- 2
  }##IF.boo.DEBUG.END
  
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
  
  # First 2 columns are ChemSampID and StationID_Master
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
  plot_H <- 4
  plot_W <- 9
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
      # plot.pryr %<a-% {##pryr.START
      #   maintitle <- paste(groupnames[g,], "Standardized values, All sites in cluster", sep=", ")
      #   graphics::par(mfrow = c(1,1), mar = c(4,8,1,1))
      #   if (useLU == TRUE) {##IF.useLU.START
      #     labmain = paste(stations, ": Cluster", site.Clusters[1,lu.cluster])
      #   } else {
      #     labmain = paste(stations, ": Cluster", site.Clusters[1,nolu.cluster])
      #   }##IF.useLU.END
      #   labx = paste(maintitle, labmain, sep = "\n")
      #   graphics::plot(y= 1:n, x= stats::runif(n,0,1), axes = F, type="n", xlab = "", ylab ="",
      #        xlim = c(0,1), cex.lab = 0.8)
      #   graphics::title(xlab=labx, line = 1, cex.lab = 0.8)
      #   graphics::axis(2, at = 1:n, labels = gpcoolvar[1:n], las =1, cex.axis = 0.6)
      #   for(i in 1:n) {##FOR.i.START
      #     xvar <- cluster.chem[,gpcoolvar[i]]; dif <- diff(range(xvar, na.rm =T))
      #     newvar <- (xvar-min(xvar, na.rm=T))/dif
      #     graphics::boxplot(newvar, at = i,boxwex=0.5, horizontal =T, add =T,axes = F
      #             , outcex = 0.6, staplewex = 1, medlwd = 0.9, boxlwd = 0.8)
      #     good.ref.data <- cluster.ref.chem.data[,gpcoolvar[i]][!is.na(cluster.ref.chem[,gpcoolvar[i]])]
      #     if (length(good.ref.data) != 0) {##IF.length.START
      #       point2 <- (cluster.ref.chem.data[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif 
      #       graphics::points(point2, rep(i,length(point2)), col = "blue", pch = 15,cex=0.6, bg = 2)
      #     }##IF.length.END
      #     point1 <- (site.chem[,gpcoolvar[i]]-min(xvar, na.rm=T))/dif 
      #     graphics::points(point1, rep(i,length(point1)), col = "red", pch = 19,cex=0.6, bg = 2)
      #   }##FOR.i.END
      #   graphics::box(bty="l") 
      # }##pryr.END
      # 
      # # PDF, capture plot in list
      # #lst.plots.g[[g]] <- grDevices::recordPlot()
      # #plots.g[[g]] <- plot.pryr
      # #assign(paste0("plot_",g),plot.pryr)
      # plot.pryr
      # plots.g[[g]] <- grDevices::recordPlot()
      # 
      # # JPG, create
      # grDevices::jpeg(filename = paste0("Results/",TargetSiteID,"/",TargetSiteID,
      #                                   ".boxes.", make.names(groupnames[g,]), ".jpg"), width = 4*ppi,
      #                 height = 3*ppi, pointsize = 8, quality = 100, bg = "white",
      #                 res = ppi)
      #   plot.pryr
      # grDevices::dev.off()
      #
      
      
      # ggplot ####
      
      ## Plot, Data, Cluster
      df_plot_wide <- as.data.frame(cluster.chem[,gpcoolvar])
      colnames(df_plot_wide) <- gpcoolvar 
      # need as.data.frame and colnames for groups with only 1 parameter
      df_plot_wide_min <- apply(df_plot_wide, 2, min, na.rm=TRUE)
      df_plot_wide_range <- apply(df_plot_wide, 2, range, na.rm=TRUE)
      df_plot_wide_diff <- apply(df_plot_wide_range, 2, diff)
      #df_plot_wide_mod <- (df_plot_wide - df_plot_wide_min) / df_plot_wide_diff
      df_plot_wide_valminusmin <- sweep(df_plot_wide, 2, df_plot_wide_min, FUN="-")
      df_plot_wide_mod <- sweep(df_plot_wide_valminusmin, 2, df_plot_wide_diff, FUN="/")
      # reshape from wide to long
      df_plot_long <- reshape2::melt(df_plot_wide_mod, measure.vars=gpcoolvar, variable.name = "GrpNm")
      # Remove NaN so get rid of error message?
      df_plot_long <- df_plot_long[!is.na(df_plot_long$value), ]

      ## Plot, Data, Cluster_Ref
      # QC for nrow
      if(nrow(cluster.ref.chem.data)>0){##IF~nrow(cluster.ref.chem.data)~START
        df_plot_ref_wide <- as.data.frame(cluster.ref.chem.data[, gpcoolvar])
        colnames(df_plot_ref_wide) <- gpcoolvar 
        df_plot_ref_wide_valminusmin <- sweep(df_plot_ref_wide, 2, df_plot_wide_min, FUN="-")
        df_plot_ref_wide_mod <- sweep(df_plot_ref_wide_valminusmin, 2, df_plot_wide_diff, FUN="/")
        df_plot_long_ref <- reshape2::melt(df_plot_ref_wide_mod, measure.vars=gpcoolvar, variable.name = "GrpNm")
        df_plot_long_ref <- df_plot_long_ref[!is.na(df_plot_long_ref$value), ] 
      }##IF~nrow(cluster.ref.chem.data)~END
      
      ## Plot, Data, Target Site
      df_plot_targ_wide <- as.data.frame(site.chem[, gpcoolvar])
      colnames(df_plot_targ_wide) <- gpcoolvar
      df_plot_targ_wide_valminusmin <- sweep(df_plot_targ_wide, 2, df_plot_wide_min, FUN="-")
      df_plot_targ_wide_mod <- sweep(df_plot_targ_wide_valminusmin, 2, df_plot_wide_diff, FUN="/")
      df_plot_long_targ <- reshape2::melt(df_plot_targ_wide_mod, measure.vars=gpcoolvar, variable.name = "GrpNm")
      df_plot_long_targ <- df_plot_long_targ[!is.na(df_plot_long_targ$value), ]
      
      
      ## Plot, Variables, Strings
      str_Group <- as.character(groupnames[g,1])
      str_title <- TargetSiteID
      str_subtitle <- paste0("Cluster ", site.Clusters[1,nolu.cluster], " (all sites)")
      str_xlab <- "Standardized Values"
      str_ylab <- str_Group
      
      ## Plot, Variables, Colors
      col_sites_all     <- "dark gray"
      col_sites_all_ref <- "blue"
      col_sites_cl      <- "cyan3"
      col_sites_cl_ref  <- col_sites_all_ref
      col_sites_targ    <- "red"
      col_line          <- "black"
      
      ## Plot, Variables, Fill
      fill_sites_all     <- col_sites_all
      fill_sites_all_ref <- fill_sites_all
      fill_sites_cl      <- col_sites_cl
      fill_sites_cl_ref  <- fill_sites_cl 
      fill_sites_targ    <- col_sites_targ
      
      ## Plot, Variables, Points
      pch_sites_all     <- 19 # solid circle
      pch_sites_all_ref <- 21 # circle outline
      pch_sites_cl      <- 19
      pch_sites_cl_ref  <- pch_sites_all_ref
      pch_sites_targ    <- 17 # triangle
      
      ## Plot, Variables, Sizes
      cex_mod <- 3
      cex_sites_all     <- cex_mod*1
      cex_sites_all_ref <- cex_sites_all
      cex_sites_cl      <- cex_mod*0.95
      cex_sites_cl_ref  <- cex_sites_cl
      cex_sites_targ    <- cex_mod*1.2
      
      ## Plot, Variables, Legend
      leg_name   <- "Sites"
      leg_labels <- c("cluster ref", "target")
      leg_shape  <- c(pch_sites_cl_ref, pch_sites_targ)
      leg_col    <- c(col_sites_cl_ref, col_sites_targ)
      leg_fill   <- c(fill_sites_cl_ref, fill_sites_targ)
      
      # ggplot, main
      p_SL <- ggplot2::ggplot(data=df_plot_long) + 
                ggplot2::geom_boxplot(ggplot2::aes(y=value, x=GrpNm))  + 
                ggplot2::coord_flip() + 
                ggplot2::labs(title=str_title, subtitle=str_subtitle, y=str_xlab, x=str_ylab) + 
                ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5), plot.subtitle=ggplot2::element_text(hjust=0.5), axis.text.x = ggplot2::element_blank(), axis.ticks.x=ggplot2::element_blank())
      #
      # ggplot, points subsets
      ## Cluster, Ref
      if(nrow(cluster.ref.chem.data)!=0){##IF~nrow.df_plot_long_ref~START
        p_SL <- p_SL + ggplot2::geom_jitter(data=df_plot_long_ref, width=0.1, ggplot2::aes(x=GrpNm, y=value, color="cl_ref", shape="cl_ref", fill="cl_ref"), size=1)
      } else {
        p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color="cl_ref", shape="cl_ref", fill="cl_ref")) 
      }##IF~nrow.df_plot_long_ref~END
      ## Target Site
      if(nrow(site.chem)!=0){##IF~nrow.df_plot_long_targ~START
        p_SL <- p_SL + ggplot2::geom_jitter(data=df_plot_long_targ, width=0.1, ggplot2::aes(x=GrpNm, y=value, color="targ", shape="targ", fill="targ"), size=1)
      } else {
        p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color="targ", shape="targ", fill="targ"))
      }##IF~nrow.df_plot_long_targ~END
      #
      # ggplot, Legend
      p_SL <- p_SL + ggplot2::scale_shape_manual(name=leg_name, labels=leg_labels, values=leg_shape)  + 
                ggplot2::scale_color_manual(name=leg_name, labels=leg_labels, values=leg_col) +
                ggplot2::scale_fill_manual(nam=leg_name, labels=leg_labels, values=leg_fill)
      
      
      #
      print(p_SL)
      plots.g[[g]] <- grDevices::recordPlot()
      #
      fn_jpg <- file.path(wd, dir.sub, dir.sub2, paste0(TargetSiteID, ".boxes.", make.names(groupnames[g,]), ".jpg"))
      ggplot2::ggsave(fn_jpg, p_SL, width=plot_W, height=plot_H, units="in")
      
    }##IF.n.END
  }##FOR.g.END
  
  # PDF ####
  # Create PDF from list
  fn_pdf <- file.path(wd, dir.sub, dir.sub2, paste0(TargetSiteID,".boxes.ALL.pdf"))
  pdf(file=fn_pdf, width=plot_W, height=plot_H)
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
                            dplyr::group_by(StdParamName) %>% 
                              dplyr::summarise(max_LogTransf=max(LogTransf, na.rm=TRUE))
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