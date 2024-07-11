#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Cluster Info
#'
#' @description Get cluster information.
#'
#' @details Summary cluster information
#'
#' @param TargetSiteID SiteID
#' @param siteCOMID Site COMID from NHDPlus v2
#' @param siteCluster Site cluster.
# @param siteQual2Plot Type of quality sites. Allowed values are c("reference",
# "not degraded", "better than")
#' @param refSiteCOMIDs reference site COMIDs
#' @param data_cluster The StreamCat data for each reach with cluster assignments.
#' @param data_clusterInfo data file for cluster info.
#' @param dir_results Directory to save plots.  Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "ClusterInfo"
#'
#' @return A jpeg in the "Results" "ClusterInfo" subdirectory of the working directory.
#'
# @importFrom pryr "%<a-%"
#'
#' @examples
#' \dontrun{
#' TargetSiteID <- "SRCKN001.61"
#' dir_results <- file.path(getwd(), "Results")
#'
#' # Data getSiteInfo
#' # data, example included with package
#' data.Stations.Info <- data_Sites       # need for getSiteInfo and getChemDataSubsets
#' data.SampSummary   <- data_SampSummary
#' data.303d.ComID    <- data_303d
#' data.bmi.metrics   <- data_BMIMetrics
#' data.algae.metrics <- data_AlgMetrics
#' data.mod           <- data_ReachMod
#'
#' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID"]==TargetSiteID
#'                    , "ElevCategory"])
#' if(elev_cat=="HI"){
#'    data_cluster <- data_Cluster_Hi
#' } else if(elev_cat=="LO") {
#'    data_cluster <- data_Cluster_Lo
#' }
#'
#' # Map data
#' # San Diego
#' #flowline <- rgdal::readOGR(dsn = "data_gis/NHDv2_Flowline_Ecoreg85", layer = "NHDv2_eco85_Project")
#' #outline <- rgdal::readOGR(dsn = "data_gis/Eco85", layer = "Ecoregion85")
#' # AZ
#' map_flowline  <- data_GIS_Flow_HI
#' map_flowline2 <- data_GIS_Flow_LO
#' if(elev_cat=="HI"){
#'    map_flowline <- data_GIS_Flow_HI
#' } else if(elev_cat=="LO") {
#'    map_flowline <- data_GIS_Flow_LO
#' }
#' map_outline   <- data_GIS_AZ_Outline
#' # Project site data to USGS Albers Equal Area
#' usgs.aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
#'               +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'               +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' # projection for outline
#' my.aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
#'            +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' map_proj <- my.aea
#' #
#' dir_sub <- "SiteInfo"
#'
#' # Run getSiteInfo
#' list.SiteSummary <- getSiteInfo(TargetSiteID, dir_results, data.Stations.Info
#'                                 , data.SampSummary, data.303d.ComID
#'                                 , data.bmi.metrics, data.algae.metrics
#'                                 , data_cluster, data.mod
#'                                 , map_proj, map_outline, map_flowline
#'                                 , dir_sub=dir_sub)
#'
#' # Data getChemDataSubsets
#' # data, example included with package
#' data.chem.raw <- data_Chem
#' data.chem.info <- data_ChemInfo
#' site.COMID <- list.SiteSummary$COMID
#' siteCluster <- list.SiteSummary$ClustIDs
#'
#' #
#' # Run getChemDataSubsets
#' list.data <- getChemDataSubsets(TargetSiteID, comid=site.COMID, cluster=siteCluster
#'                                 , data_cluster=data_cluster, data.Stations.Info=data.Stations.Info
#'                                 , data.chem.raw=data.chem.raw, data.chem.info=data.chem.info)
#'
#' # Data getClusterInfo
#' ref.reaches <- list.data$ref.reaches
#' refSiteCOMIDs <- list.data$ref.reaches
#' dir_sub <- "ClusterInfo"
#'
#' # Run getClusterInfo
#' getClusterInfo(TargetSiteID, site.COMID, siteCluster, ref.reaches,
#'                data_cluster, dir_results, dir_sub)
#' }
#' @export
#'
getClusterInfo <- function(TargetSiteID
                           , siteCOMID
                           , siteCluster
                           , refSiteCOMIDs
                           , subregion = NULL
                           , data_cluster
                           , data_clusterInfo
                           , dir_results=file.path(getwd(), "Results")
                           , dir_sub="ClusterInfo"
                           ) {##FUNCTION.START
  #
  boo_DEBUG <- FALSE
  #
  if (boo_DEBUG == TRUE) {
    TargetSiteID
    siteCOMID = list.SiteSummary$COMID
    siteCluster = outcaseID
    refSiteCOMIDs = list.SiteSummary$refCOMIDs
    subregion = NULL
    data_cluster = data_cluster
    data_clusterInfo = data_clusterInfo
    dir_results = file.path(localdir, "Results")
    dir_sub = "ClusterInfo"
  }##IF~boo_DEBUG~END
  #


  # check for and create (if necessary) "Results" subdirectory of working directory
  # wd <- getwd()
  # dir.sub <- "Results"
  wd <- dirname(dir_results)
  dir.sub <- basename(dir_results)
  dir.sub2 <- TargetSiteID
  dir.sub3 <- dir_sub
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2)) == TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3)) == TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3))
         , FALSE)
  #
  if (length(siteCluster) == 0) {
    # do not proceed
    # no cluster assignment
    stop(paste("No cluster assignment for", TargetSiteID, sep = " "))
  }

  if (!is.null(subregion)) {
    data_cluster <- data_cluster[data_cluster$subregion == subregion, ]
  }
  df.plot.target <- data_cluster[data_cluster$COMID == siteCOMID,  ]
  df.plot.ref <- data_cluster[data_cluster$COMID %in% refSiteCOMIDs, ]
  df.plot <- data_cluster
  clusterLevels <- dplyr::distinct(data_cluster, Cluster) %>%
    dplyr::arrange(Cluster)
  clusterLevels <- unlist(clusterLevels)

  # Plots ####
  # Capture each plot in a list for the PDF
  # plots.i <- vector(ncol(df.plot.target)-1, mode="list")
  ppi<-300
  for (i in 2:ncol(df.plot.target)) {##FOR.i.START
    #
    varYlab <- colnames(df.plot.target)[i]
    varYunits <- as.character(data_clusterInfo$Units[data_clusterInfo$ColName == varYlab])
    varYgrandscale <- as.character(data_clusterInfo$Scale[data_clusterInfo$ColName == varYlab])
    varYtext <- as.character(data_clusterInfo$ShortName[data_clusterInfo$ColName == varYlab])
    varMain <- paste0(varYtext, " (", varYunits,") in the "
                      , tolower(varYgrandscale), " by cluster")
    qualtext <- "Reference"
    str_caption <- ""
    #
    # QC
    i.num <- i - 1
    i.len <- ncol(df.plot.target) - 1
    i.var <- varYlab
    message(paste0("Processing item, ", i.num, "/", i.len, "; ", i.var))
    utils::flush.console()
    #
    myY <- df.plot[, i]
    myX <- df.plot[, "Cluster"]
    #
    # QC
    if (sum(!is.na(myY)) == 0 || is.numeric(myY) == FALSE) {##IF.myY.START
      message("No data, next")
      utils::flush.console()
      next
    } ##IF.myY.END

    if (varYlab %in% c("Cluster")) {
      message(paste0("Skipping ", varYlab))
      utils::flush.console()
      next
    }
    #
    # ggplot ####

    ## Plot, Variables, Strings
    str_title <- varMain
    str_sub <- TargetSiteID
    str_xlab  <- "Cluster"
    str_ylab  <- colnames(df.plot)[i]

    ## Plot, Data
    df_ggplot_all  <- as.data.frame(cbind(df.plot[, i], df.plot[, "Cluster"]))
    df_ggplot_ref  <- as.data.frame(cbind(df.plot.ref[, i], df.plot.ref[, "Cluster"]))
    df_ggplot_targ <- as.data.frame(cbind(df.plot.target[, i], df.plot.target[, "Cluster"]))
    colnames(df_ggplot_all)  <- c("var", str_xlab)
    colnames(df_ggplot_ref)  <- c("var", str_xlab)
    colnames(df_ggplot_targ) <- c("var", str_xlab)
    df_ggplot_all <- mutate(df_ggplot_all, Cluster = factor(Cluster
                                                            , levels = clusterLevels))
    df_ggplot_ref <- mutate(df_ggplot_ref, Cluster = factor(Cluster
                                                            , levels = clusterLevels))
    df_ggplot_targ <- mutate(df_ggplot_targ, Cluster = factor(Cluster
                                                            , levels = clusterLevels))

    ## Plot, portions
    boo_plot_ref  <- ifelse(nrow(df_ggplot_ref) != 0, TRUE, FALSE)
    boo_plot_targ <- ifelse(nrow(df_ggplot_targ) != 0, TRUE, FALSE)

    ## Plot, Variables, Output Size (inches)
    plot_H <- 4
    plot_W <- 9

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
    cex_mod <- 2.5
    cex_sites_all     <- cex_mod*1
    cex_sites_all_ref <- cex_sites_all
    cex_sites_cl      <- cex_mod*0.95
    cex_sites_cl_ref  <- cex_sites_cl
    cex_sites_targ    <- cex_mod*1.2

    ## Plot, Variables, Legend
    leg_name   <- "Sites"
    leg_labels <- c(qualtext, "Target")
    leg_shape  <- c(pch_sites_all_ref, pch_sites_targ)
    leg_col    <- c(col_sites_all_ref, col_sites_targ)
    leg_fill   <- c(fill_sites_all_ref, fill_sites_targ)

    # ggplot, basic
    p_cl <- ggplot2::ggplot(df_ggplot_all, ggplot2::aes(Cluster, var)) +
      ggplot2::geom_boxplot(ggplot2::aes(group = Cluster, y = var))

    # ggplot, point subsets
    ## Ref
    if(boo_plot_ref==TRUE){##IF~boo_plot_ref~START
      p_cl <- p_cl + ggplot2::geom_jitter(data = df_ggplot_ref, width = 0.1
                                          , ggplot2::aes(group = Cluster
                                                         , y = var
                                                         , color = "ref_all"
                                                         , shape = "ref_all"
                                                         , fill = "ref_all")
                                          , size = cex_sites_all_ref)
    } else {
      p_cl <- p_cl + ggplot2::geom_blank(ggplot2::aes(color = "ref_all"
                                                      , shape = "ref_all"
                                                      , fill = "ref_all"))
    }##IF~boo_plot_ref~END
    ## Target Site
    if(boo_plot_targ==TRUE){##IF~boo_plot_targ~START
      p_cl <- p_cl + ggplot2::geom_jitter(data = df_ggplot_targ, width = 0.1
                                          , ggplot2::aes(group = Cluster
                                                         , y = var
                                                         , color = "target"
                                                         , shape = "target"
                                                         , fill = "target")
                                          , size = cex_sites_targ)
    } else {
      p_cl <- p_cl + ggplot2::geom_blank(ggplot2::aes(color = "target"
                                                      , shape = "target"
                                                      , fill = "target"))
    }##IF~boo_plot_targ~END

    # ggplot, Legend and other
    p_cl <- p_cl + ggplot2::scale_shape_manual(name = leg_name
                                               , labels = leg_labels
                                               , values = leg_shape) +
      ggplot2::scale_color_manual(name = leg_name, labels = leg_labels
                                  , values = leg_col) +
      ggplot2::scale_fill_manual(nam = leg_name, labels = leg_labels
                                 , values = leg_fill) +
      ggplot2::scale_x_discrete(limits = levels(df_ggplot_all$Cluster)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)
                     , plot.subtitle=ggplot2::element_text(hjust=0.5)
                     , plot.caption=ggplot2::element_text(size=8)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size=8)
                     , axis.text.y = ggplot2::element_text(size=8)) +
      ggplot2::labs(title=str_title, subtitle = str_sub, x=str_xlab
                    , y=str_ylab, caption=str_caption)
    #
    # print(p_cl)

    # PDF, capture plot in list
    # plots.i[[i-1]] <- grDevices::recordPlot()

    # Save to JPG
    fn_png <- file.path(wd, dir.sub, dir.sub2, dir.sub3
                        , paste0(TargetSiteID, "_Cluster_", make.names(varYlab), ".png"))
    ggplot2::ggsave(fn_png, p_cl, width=plot_W, height=plot_H, units="in")
    #
  }##FOR.i.END
  #
  #grDevices::graphics.off()
  # Create PDF from list
  # fn_pdf <- file.path(wd, dir.sub, dir.sub2, dir.sub3, paste0(TargetSiteID,".cluster.ALL.pdf"))
  # grDevices::pdf(file=fn_pdf, width=plot_W, height=plot_H)
  # for (ii in plots.i){##FOR.gp.START
  #   #grDevices::replayPlot(g.plot)
  #   if(is.null(ii)==TRUE) {next}
  #   grDevices::replayPlot(ii)
  # }##FOR.gp.END
  # grDevices::dev.off()
  # rm(plots.i)
  #
}##FUNCTION.END
