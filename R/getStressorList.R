#' @title Stressor List
#' 
#' @description Get stressor list.
#' 
#' @details Box plots of each stressor, grouped by category.
#' 
#' Required objects:  all specified as inputs.
#' 
#' chem.info need to include DirIncStress.  Valid values are 'inc' or 'dec'.
#' 
#' @param TargetSiteID Site ID
#' @param site.Cluster Clusters
#' @param chem.info chem information
#' @param cluster.chem chem data cluster.
#' @param cluster.samps sample cluster.
#' @param ref.sites reference sites
#' @param siteChem Chem sites
#' @param probsHigh probabilities, high
#' @param probsLow probabilities, low
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param dir_results Directory to save plots.  Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "SiteInfo"
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
#' data.mod           <- data_ReachMod
#' 
#' # Cluster based on elevation category  # need for getSiteInfo and getChemDataSubsets
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID_Master"]==TargetSiteID
#'                     , "ElevCategory"])
#' if(elev_cat=="HI"){
#'    data.cluster <- data_Cluster_Hi
#' } else if(elev_cat=="LO") {
#'    data.cluster <- data_Cluster_Lo
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
#'                                 , data.cluster, data.mod
#'                                 , map_proj, map_outline, map_flowline
#'                                 , dir_sub=dir_sub)
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
#' siteChem <- list.data$siteChem
#' dir_sub <- "CandidateCauses"
#' 
#' # set cutoff for possible stressor identification
#' probsLow <- 0.10
#' probsHigh <- 0.90 
#' biocomm <- "bmi"
#' 
#' # Run getStressorList
#' list.stressors <- getStressorList(TargetSiteID, site.Clusters, chem.info, cluster.chem
#'                                  , cluster.samps, ref.sites, siteChem
#'                                  , probsHigh, probsLow, biocomm, dir_results
#'                                  , dir_sub)
#                                  
#' @export
getStressorList <- function(TargetSiteID
                            , siteCluster
                            , chemInfo
                            , clusterChem
                            , siteQual2Plot
                            , refSamps
                            , refSites
                            , siteChem
                            , probsHigh
                            , probsLow
                            , DOlim=7
                            , pHlimLow=6.5
                            , pHlimHigh=9
                            , biocomm="bmi"
                            , bioParmsDEL
                            , dir_results=file.path(getwd(), "Results")
                            , dir_sub="CandidateCauses") {##FUNCTION.START
  # DEBUGGING ####
  boo.DEBUG <- FALSE
  #
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    g <- 1
    TargetSiteID
    siteCluster=list.SiteSummary$ClustID
    chemInfo=data_stressInfo
    clusterChem=compStressAll
    siteQual2Plot=siteQual2Plot
    refSamps=allBioRefStressSamps
    refSites=allBioRefSites
    siteChem=siteStressAll
    probsHigh=probsHigh
    probsLow=probsLow
    DOlim=7
    pHlimLow=6.5
    pHlimHigh=9
    biocomm=bioComm
    bioParmsDEL=bioParmsDEL
    dir_results=dir_results
    dir_sub="CandidateCauses"
    # all other function inputs defined in example.
  }##IF.boo.DEBUG.END
  #
  # 
  # QC, 20190905
  # chem.info$DirIncStress to lower case
  chemInfo$DirIncStress <- tolower(chemInfo$DirIncStress)
  biocomm <- toupper(biocomm)
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}
  plot_ext <- ".png"
  outliercols <- c("IQRmethod", "SDmethod", "Outlier")
  
  # check for and create (if necessary) "Results" subdirectory of working directory
  # wd <- getwd()
  # dir.sub <- "Results"
  wd <- dirname(dir_results)
  dir.sub <- basename(dir_results)
  dir.sub2 <- TargetSiteID
  dir.sub3 <- biocomm
  dir.sub4 <- dir_sub
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2))
         , FALSE)
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3))
         , FALSE)
  ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4))==TRUE
         , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4))
         , FALSE)
  
  dir_path <- file.path(wd, dir.sub, dir.sub2, dir.sub3, dir.sub4)
  
  # First 2 columns are ChemSampID and StationID_Master
  clusterChemData <- clusterChem[4:ncol(clusterChem)]
  clusterChemData <- dplyr::select_if(clusterChemData, not_all_na)
  clustChemCols <- colnames(clusterChemData)
  clustChemCols <- clustChemCols[!(clustChemCols %in% outliercols)]
  
  # Identify all cluster "reference" samples, including modeled ones
  clusterRefChem <- subset(clusterChem, clusterChem$StressSampID %in% refSamps)
  clusterRefModSamps <- clusterChem %>%
      dplyr::filter(stringr::str_detect(StressSampID, "_modeledflow")) %>%
      dplyr::filter(StationID_Master %in% refSites)
  clusterRefChem <- rbind(clusterRefChem, clusterRefModSamps)
  rm(clusterRefModSamps)
  if (nrow(clusterRefChem)==0) {
      # No reference sites in the comparator set
  } else {
      clusterRefChem <- select_if(clusterRefChem, not_all_na)
      clusterRefChemData <- clusterRefChem[4:ncol(clusterRefChem)]
      clustRefChemCols <- colnames(clusterRefChemData)
      clustRefChemCols <- clustRefChemCols[!(clustRefChemCols %in% outliercols)]
      addcols <- setdiff(clustChemCols, clustRefChemCols)
      if (length(addcols)>0) {
          for (add in 1:length(addcols)) {
              addcolname <- addcols[add]
              clusterRefChemData[[addcolname]] <- NA
          }
          clusterRefChemData <- dplyr::select(clusterRefChemData, clustChemCols)
      }
  }
  rm(clusterRefChem)
  
  chemnames <- colnames(clusterChemData)
  chemnames <- chemnames[!(chemnames %in% c("IQRmethod", "SDmethod", "Outlier"))]
  allcount <- apply(clusterChemData, 2, function(x) sum(!is.na(x)))
  alltype <- unlist(lapply(1:ncol(clusterChemData)
                           , function(x) is.numeric(clusterChemData[,x])))
  coolvar <- names(allcount)[allcount>2 & alltype]
  groupnames <- unique(subset(chemInfo, chemInfo$Analyte %in% chemnames
                              , select = "GroupName"))
  numgps <- length(groupnames[,1])
  
  # Get data having <=2 samples in cluster, write to data gaps & add to eliminated
  uncoolvar <- setdiff(chemnames, coolvar)
  if (length(uncoolvar)>0) {
      df_allcount <- as.data.frame(allcount)
      df_allcount <- cbind(rownames(df_allcount), df_allcount, row.names=NULL)
      colnames(df_allcount)[1] <- "Stressor"
      df_allcount <- dplyr::filter(df_allcount, Stressor %in% uncoolvar)
      df_labels <- unique(as.data.frame(dplyr::select(chemInfo, Analyte, Label)))
      df_allcount <- merge(df_allcount, df_labels, by.x = "Stressor"
                           , by.y = "Analyte", all.x = TRUE)
      for (s in 1:nrow(df_allcount)) {
          elimName <- as.character(df_allcount$Label[s])
          gapcomment <- paste0("Number of comparator samples is too few for analysis.")
          gaps <- cbind.data.frame("getStressorList", elimName
                                   , df_allcount$allcount[s]
                                   , gapcomment)
          colnames(gaps) <- c("fxnname", "condition", "result", "comment")
          fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
          fn.gaps <- file.path(wd,"Results",TargetSiteID,fn.gaps)
          write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                      , row.names = FALSE, sep = "\t")          
          if (!exists("tmpParmDEL")){ tmpParmDEL <- elimName } 
          else { tmpParmDEL <- c(tmpParmDEL, elimName) }
      }
          
  }
  
  # Plots ####
  ppi <- 300
  plot_H <- 6
  plot_W <- 9
  # Capture each plot in a list for the PDF
  ## https://stackoverflow.com/questions/13273611/how-to-append-a-plot-to-an-existing-pdf-file
  ## https://www.andrewheiss.com/blog/2016/12/08/save-base-graphics-as-pseudo-objects-in-r/
  plots.g <- vector(numgps, mode="list")
  # Generate 1 box plot for each group, ref sites in blue, target site in red
  for (g in 1:numgps) {##FOR.g.START
    gpchems <- subset(chemInfo, GroupName == groupnames[g,]
                      , select = c("Analyte", "Label"))
    gpcoolvar <- subset(coolvar, coolvar %in% gpchems$Analyte)
    n <- length(gpcoolvar)
    #
    if(boo.DEBUG==TRUE){##IF~boo.DEBUG~START
      print(paste0("Item (", g, "/", numgps, ")"))
      utils::flush.console()
    }##IF~boo.DEBUG~START
    #
    if(n>0) { ##FOR.n.START

      ## Plot, Data, Cluster
        #In next line changed clusterChem to ClusterChemData
      df_plot_wide <- as.data.frame(clusterChemData[,gpcoolvar])
      colnames(df_plot_wide) <- gpcoolvar 
      # need as.data.frame and colnames for groups with only 1 parameter
      df_plot_wide_min <- apply(df_plot_wide, 2, min, na.rm=TRUE)
      df_plot_wide_range <- apply(df_plot_wide, 2, range, na.rm=TRUE)
      df_plot_wide_diff <- apply(df_plot_wide_range, 2, diff)
      #df_plot_wide_mod <- (df_plot_wide - df_plot_wide_min) / df_plot_wide_diff
      df_plot_wide_valminusmin <- sweep(df_plot_wide, 2, df_plot_wide_min, FUN="-")
      df_plot_wide_mod <- sweep(df_plot_wide_valminusmin, 2, df_plot_wide_diff, FUN="/")
      # reshape from wide to long
      df_plot_long <- reshape2::melt(df_plot_wide_mod, measure.vars=gpcoolvar
                                     , variable.name="GrpNm")
      # Remove NaN so get rid of error message?
      df_plot_long <- df_plot_long[!is.na(df_plot_long$value), ]
      df_plot_long <- merge(gpchems, df_plot_long, by.x="Analyte", by.y="GrpNm")

      ## Plot, Data, Cluster_Ref
      # QC for nrow
      boo_plot_ref <- FALSE
      if(exists("clusterRefChemData")){##IF~nrow(cluster.ref.chem.data)~START
        df_plot_ref_wide <- as.data.frame(clusterRefChemData[, gpcoolvar])
        # colnames(df_plot_ref_wide) <- gpcoolvar 
        df_plot_ref_wide_valminusmin <- sweep(df_plot_ref_wide, 2, df_plot_wide_min, FUN="-")
        df_plot_ref_wide_mod <- sweep(df_plot_ref_wide_valminusmin, 2, df_plot_wide_diff, FUN="/")
        df_plot_long_ref <- reshape2::melt(df_plot_ref_wide_mod, measure.vars=gpcoolvar, variable.name = "GrpNm")
        # df_plot_long_ref <- df_plot_long_ref[!is.na(df_plot_long_ref$value), ] 
        df_plot_long_ref <- merge(gpchems, df_plot_long_ref, by.x="Analyte", by.y="GrpNm")
        boo_plot_ref <- ifelse(nrow(df_plot_long_ref)>0, TRUE, FALSE)
        boo_plot_ref <- ifelse(all(is.na(df_plot_long_ref$value)), FALSE, TRUE)
      }##IF~nrow(cluster.ref.chem.data)~END
      
      ## Plot, Data, Target Site
      df_plot_targ_wide <- as.data.frame(siteChem[, gpcoolvar])
      colnames(df_plot_targ_wide) <- gpcoolvar
      df_plot_targ_wide_valminusmin <- sweep(df_plot_targ_wide, 2, df_plot_wide_min, FUN="-")
      df_plot_targ_wide_mod <- sweep(df_plot_targ_wide_valminusmin, 2, df_plot_wide_diff, FUN="/")
      df_plot_long_targ <- reshape2::melt(df_plot_targ_wide_mod, measure.vars=gpcoolvar, variable.name = "GrpNm")
      df_plot_long_targ <- df_plot_long_targ[!is.na(df_plot_long_targ$value), ]
      df_plot_long_targ <- merge(gpchems, df_plot_long_targ, by.x="Analyte", by.y="GrpNm")
      boo_plot_targ <- ifelse(nrow(siteChem)!=0, TRUE, FALSE)
      
      # Get proper labels to describe "good quality" sites
      if (siteQual2Plot=="not degraded") {
          qualtext <- "Not degraded*"
          if (biocomm=="BMI") {
              str_caption <- paste0("*Stressor samples paired with benthic "
                                    , "macroinvertebrate samples rated not degraded.")
          } else if (biocomm=="ALGAE") {
              str_caption <- paste0("*Stressor samples paired with algae "
                                    , "samples rated not degraded.")
          }
      } else if (siteQual2Plot=="better than") {
          qualtext <- "Better quality*"
              str_caption <- paste("*Stressor samples with paired response samples having biological"
                                   ,"quality better than the mimum target site quality.", sep = "\n")
      } else { 
          qualtext <- "Reference"
          str_caption <- ""
      }
      
      ## Plot, Variables, Strings
      str_Group <- stringr::str_to_sentence(as.character(groupnames[g,1]))
      str_title <- paste0(TargetSiteID, ": Selection of detected stressors for"
                          ," evaluation as causes of impairment")
      str_title <- stringr::str_wrap(str_title,100)
      str_subtitle <- paste0("Comparator samples from cluster ", siteCluster)
      str_xlab <- "Standardized values"
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
      cex_sites_targ    <- cex_mod*1.5
      
      ## Plot, Variables, Legend
      leg_name   <- "Samples"
      leg_labels <- c(qualtext, "Target")
      leg_shape  <- c(pch_sites_cl_ref, pch_sites_targ)
      leg_col    <- c(col_sites_cl_ref, col_sites_targ)
      leg_fill   <- c(fill_sites_cl_ref, fill_sites_targ)
      
      if (n>8) {
          yaxistextsize = 6
          wrap_length = 35
      } else {
          yaxistextsize = 7
          wrap_length <- 27
      }
      
      # ggplot, main
      p_SL <- ggplot2::ggplot(data=df_plot_long) + 
                ggplot2::geom_boxplot(ggplot2::aes(x=stringr::str_wrap(Label, wrap_length)
                                                   , y=value))  + 
                ggplot2::coord_flip() + 
                ggplot2::labs(title=str_title, subtitle=str_subtitle
                              , y=str_xlab, x=str_ylab, caption = str_caption) + 
                ggplot2::theme_bw() +
                ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5,size=10)
                               , plot.subtitle=ggplot2::element_text(hjust=0.5,size=10)
                               , axis.text.x = ggplot2::element_blank()
                               , axis.text.y = ggplot2::element_text(size=yaxistextsize)
                               , axis.ticks.x=ggplot2::element_blank()
                               , plot.caption = ggplot2::element_text(size=8))
      #
      # ggplot, points subsets
      ## Cluster, Ref
      if(boo_plot_ref==TRUE){##IF~boo_plot_ref~START
        p_SL <- p_SL + ggplot2::geom_jitter(data=df_plot_long_ref, width=0.1
                                , ggplot2::aes(x=stringr::str_wrap(Label, wrap_length)
                                               , y=value, color="cl_ref"
                                               , shape="cl_ref", fill="cl_ref")
                                , size=1)
      } else {
        p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color="cl_ref"
                                                        , shape="cl_ref"
                                                        , fill="cl_ref")) 
      }##IF~boo_plot_ref~END
      ## Target Site
      if(boo_plot_targ==TRUE){##IF~boo_plot_targ~START
        p_SL <- p_SL + ggplot2::geom_jitter(data=df_plot_long_targ
                                            , width=0.1
                                            , ggplot2::aes(x=stringr::str_wrap(Label, wrap_length)
                                                           , y=value
                                                           , color="targ"
                                                           , shape="targ"
                                                           , fill="targ")
                                            , size=1.5)
      } else {
        p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color="targ"
                                                        , shape="targ"
                                                        , fill="targ"))# + 
            # ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5,size=10)
            #                , plot.subtitle=ggplot2::element_text(hjust=0.5,size=10)
            #                , axis.text.x = ggplot2::element_text(size=8)
            #                , axis.text.y = ggplot2::element_text(size=yaxistextsize)
            #                , axis.ticks.x=ggplot2::element_blank()
            #                , plot.caption = ggplot2::element_text(size=8))
      }##IF~boo_plot_targ~END
      #
      # ggplot, Legend
      p_SL <- p_SL + ggplot2::scale_shape_manual(name=leg_name
                                                 , labels=leg_labels
                                                 , values=leg_shape)  + 
                ggplot2::scale_color_manual(name=leg_name, labels=leg_labels
                                            , values=leg_col) +
                ggplot2::scale_fill_manual(nam=leg_name, labels=leg_labels
                                           , values=leg_fill)
      
      
      #
      print(p_SL)
      plots.g[[g]] <- grDevices::recordPlot()
      #
      # fn_title <- make.names(groupnames[g,])
      fn_title <- stringr::str_to_title(str_Group)
      fn_title <- gsub("\\s","",fn_title)
      fn_plot <- file.path(dir_path, paste0(TargetSiteID, "_", biocomm
                                            , "_CandCauses_", fn_title, plot_ext))
      # fn_plot <- file.path(dir_path, paste0(TargetSiteID, "_PossStressors_"
      #                                       , make.names(groupnames[g,]), plot_ext))
      ggplot2::ggsave(fn_plot, p_SL, width=plot_W, height=plot_H, units="in")
      
    }##IF.n.END
  }##FOR.g.END
  
  # PDF ####
  # Create PDF from list
  fn_pdf <- file.path(dir_path, paste0(TargetSiteID,"_",biocomm,"_"
                                       ,"CandCauses_ALL.pdf"))
  grDevices::pdf(file=fn_pdf, width=plot_W, height=plot_H)
    for (i in plots.g){##FOR.gp.START
      #grDevices::replayPlot(g.plot)
      if(is.null(i)==TRUE) {next}
      grDevices::replayPlot(i)
    }##FOR.gp.END
  grDevices::dev.off()
  rm(plots.g)

  # Percentile Data File ####
  chem.pctrank <- apply(clusterChem[,4:ncol(clusterChem)], 2
                        , function(x) dplyr::percent_rank(x))
  data.chem.pctrank <- cbind(clusterChem[,1:3], as.data.frame(chem.pctrank))
  fn.pctrank <- file.path(dir_path, paste0(TargetSiteID,"_",biocomm,"_"
                                           ,"CandCauses_ChemPctRank.tab"))
  utils::write.table(data.chem.pctrank, fn.pctrank, sep="\t", col.names=TRUE
                     , row.names = FALSE, append=FALSE)
  site.pctrank <- subset(data.chem.pctrank, StationID_Master==TargetSiteID)
  stressor <- c("none")
  # 
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    c <- 7
  }##IF.boo.DEBUG.END
  
  # Handle exceptions from standard stressor list ID
  for (c in 7:ncol(site.pctrank)) {
    # print(c)
    chemname <- colnames(site.pctrank)[c]
    bad <- is.na(site.pctrank[,c])
    check <- site.pctrank[,c]
    good <- check[!bad]
    maxSiteRank <- max(good, na.rm = TRUE)
    minSiteRank <- min(good, na.rm = TRUE)
    maxSiteVal <- max(as.data.frame(siteChem[, chemname]), na.rm = TRUE)
    minSiteVal <- min(as.data.frame(siteChem[, chemname]), na.rm = TRUE)
    # DirIncStress ####
    # (not all in chem.info)
    if(chemname %in% chemInfo[, "StdParamName"]){
      ExpDirIncStress <- tolower((chemInfo[chemInfo[,"StdParamName"]==chemname
                                           ,"DirIncStress"])[1])
    } else {
      ExpDirIncStress <- "unk"
    }
    if (grepl("^pH_", chemname, perl=TRUE, ignore.case=FALSE)==TRUE) {
        if ((minSiteVal < pHlimLow) | (maxSiteVal > pHlimHigh)) {
            if ((minSiteRank <= probsLow) | (maxSiteRank >= probsHigh)) {
                stressor <- c(stressor, chemname)
            }
        } else {
            if (!exists("tmpParmDEL")){ tmpParmDEL <- chemname } 
            else { tmpParmDEL <- c(tmpParmDEL, chemname) }
            print("pH is not a stressor.")
            flush.console()
        }
        next()
    }
    if (ExpDirIncStress == "dec") {
      if (grepl("^DO_", chemname, perl=TRUE, ignore.case=FALSE)==TRUE) {

          if ((minSiteVal < DOlim) & (minSiteRank <= probsLow)) {
              print("DO is a stressor.")
              flush.console()
              stressor <- c(stressor, chemname)
          } else {
              if (!exists("tmpParmDEL")){ tmpParmDEL <- chemname } 
              else { tmpParmDEL <- c(tmpParmDEL, chemname) }
              print("DO is not a stressor.")
              flush.console()
          }
          
      } else if (minSiteRank <= probsLow) {
          stressor <- c(stressor, chemname)
      }
    } else if ((ExpDirIncStress == "inc") && (maxSiteRank >= probsHigh)) {
      stressor <- c(stressor, chemname)
    }
  }##FOR~c~END
  
  # if (exists("tmpParmDEL")) {
  #     bioParmsDEL <- c(bioParmsDEL, tmpParmDEL)
  #     bioParmsDEL <- unique(bioParmsDEL)
  # }
  
  # Stressor list contains stressors to proceed in analysis
  # bioParmsDEL contains parameters that don't apply for this biocomm
  # tmpParmDEL contains parameters with <= only 2 sample points for cluster data
  stressorlist <- stressor
  stressorlist <- setdiff(stressorlist, bioParmsDEL)
  stressorsExcepted <- intersect(stressorlist, bioParmsDEL)
  if (exists("tmpParmDEL")) { 
      stressorsExcepted<-unique(c(stressorsExcepted, tmpParmDEL)) 
      stressorlist <- setdiff(stressorlist, tmpParmDEL)
  }
  stressorsExcepted <- as.data.frame(stressorsExcepted) %>%
      dplyr::mutate(Biocomm = biocomm)
  colnames(stressorsExcepted)[1] <- "Stressor"
  stressorsExcepted <- merge(stressorsExcepted, chemInfo[,c("Analyte", "Label")]
                             , by.x = "Stressor", by.y = "Analyte", all.x = TRUE)
  if (nrow(stressorsExcepted)==0) {
      stressorsExcepted <- rbind(stressorsExcepted,(cbind("None",biocomm,"None")))
  }
  colnames(stressorsExcepted) <- c("Stressor","BioComm","Label")
  stressorsExcepted <- unique(stressorsExcepted)
  # Write stressors excepted table
  fn.stressorsExc <- file.path(dir_path, paste0(TargetSiteID,"_",biocomm,"_"
                                           ,"CandCauses_StressorsExcluded.tab"))
  utils::write.table(stressorsExcepted, fn.stressorsExc, sep="\t", col.names=TRUE
                     , row.names = FALSE, append=FALSE)
  
  # LogTransf ####
  # 20190110, get log transformation code from chem.info
  # define pipe
  `%>%` <- dplyr::`%>%`
  #x <- unique(chem.info[chem.info$StdParamName %in% stressorlist, c("StdParamName", "LogTransf")])
  # need to use max (default of 1) in case of duplicates
  chemInfo_LogTransf <- chemInfo %>% 
      dplyr::group_by(StdParamName) %>%
      dplyr::summarise(max_LogTransf=max(LogTransf, na.rm=TRUE))
  stressorlist4merge <- data.frame(StdParamName=stressorlist, Sort=1:length(stressorlist))
  # merge
  LogTransf_merge <- merge(stressorlist4merge, chemInfo_LogTransf, all.x=TRUE)
  # sort 
  LogTransf_merge <- LogTransf_merge[order(LogTransf_merge$Sort), ]
  # NA to 0
  LogTransf_merge[is.na(LogTransf_merge[,"max_LogTransf"]), "max_LogTransf"] <- 0
  
  
  # # Data File ####
  stressorlist_trim <- stressorlist[stressorlist != "none"]
  data.chemVals <- clusterChem %>%
      dplyr::select(StationID_Master, StressSampID, StressSampDate, IQRmethod
                    , SDmethod, Outlier, eval(stressorlist_trim))
  fn.chemVals <- file.path(dir_path, paste0(TargetSiteID,"_",biocomm,"_"
                                           ,"CandCauses_ChemValues.tab"))
  utils::write.table(data.chemVals, fn.chemVals, sep="\t", col.names=TRUE
                     , row.names = FALSE, append=FALSE)

  # create output ####
  myStressors <- list(stressors = stressorlist, site.stressor.pctrank = site.pctrank
                      , stressors_LogTransf=LogTransf_merge$max_LogTransf)
  #
  return(myStressors)
} # FUN end
