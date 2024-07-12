#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#
#' @title Stressor List
#'
#' @description Get stressor list.
#'
#' @details Box plots of each stressor, grouped by category.
#'
#' Required objects:  all specified as inputs.
#'
#' Required packages: dplyr, ggplot2, stringr, tidyr
#'
#' chem.info need to include DirIncStress.  Valid values are 'inc' or 'dec'.
#'
#' @param TargetSiteID Site ID
#' @param outcaseLabel Label for the "outside the case" identifier
#' @param outcaseID Name of column in sites file that indicates the "outside
#' the case" id
#' @param outcaseSites Vector containing "outside the case" site IDs
#' @param incaseSites Vector containing "inside the case" site IDs (comparators)
#' @param refSites all reference sites
#' @param siteChem dataframe containing any detected stressor data from target
#' site samples at any time
#' @param df_Stress dataframe containing stressor data
#' @param cheminfo dataframe containing stressor metadata, specifically "Label",
#' "DirIncStress", and "LogTransformYN"
#' @param samplim minimum number of samples required to id stressors as
#' candidate causes
#' @param probsHigh probabilities, high
#' @param probsLow probabilities, low
#' @param DOlim Dissolved Oxygen limit, default = 7
#' @param pHlimLow  pH limit, low, default = 6.5
#' @param pHlimHigh pH limit, high, default = 9
#' @param biocommlist vector of each biological response community available
#' ("bmi", "alg", or "fish")
#' @param listbioParamsDEL list of vectors corresponding to biocommlist of
#' stressors not considered relevant for evaluation
#' @param dir_results Directory to save plots. Default = file.path(getwd(), "Results").
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo"
#'
#' @return One or more png files containing normalized stressor boxplots by stressor
#' group; a correlation matrix representing stressor correlations; a file of stressors
#' evaluated; a file of stressors excluded; stressor values and site.stressor.pctrank.
#'
#' @examples
#' \dontrun{
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
#' elev_cat <- toupper(data.Stations.Info[data.Stations.Info[,"StationID"]==TargetSiteID
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
#' }
#' @export
#'
getStressorList <- function(TargetSiteID
                            , outcaseLabel
                            , outcaseID
                            , outcaseSites
                            , incaseSites
                            , refSites
                            , siteChem
                            , df_Stress
                            , chemInfo
                            , samplim = 10
                            , probsHigh = 0.75
                            , probsLow = 0.25
                            , DOlim = 7
                            , pHlimLow = 6.5
                            , pHlimHigh = 9
                            , biocommlist
                            , listbioParamsDEL
                            , dir_results = file.path(getwd(), "Results")
                            , dir_sub = "CandidateCauses"
                            ) {##FUNCTION.START
  # DEBUGGING ####
  boo.DEBUG <- FALSE
  #
  if (boo.DEBUG == TRUE) {##IF.boo.DEBUG.START
    TargetSiteID = TargetSiteID
    outcaseLabel = outcaseLabel
    outcaseID = outcaseID
    outcaseSites = all_sites
    incaseSites = comp_sites
    refSites = refSites # vector
    siteChem = siteDetectsAll # dataframe
    df_Stress = data_Stress
    chemInfo = data_stressInfo
    samplim = 10
    probsHigh = probsHigh
    probsLow = probsLow
    DOlim = DOlim
    pHlimLow = pHlimLow
    pHlimHigh = pHlimHigh
    biocommlist = biocommlist
    listbioParamsDEL = list.bioParamsDEL
    dir_results = dir_results
    dir_sub = "CandidateCauses"
  }##IF.boo.DEBUG.END
  #
  #
  # QC, 20190905
  # chem.info$DirIncStress to lower case
  chemInfo$DirIncStress <- tolower(chemInfo$DirIncStress)
  biocommlist <- toupper(biocommlist)
  outcaseLabel <- tolower(outcaseLabel)
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}
  plot_ext <- ".png"

  # Works with Shiny server
  if (Sys.getenv('SHINY_PORT') != "") { # Running on Shiny server
    is_local <- FALSE
    wd <- "."
    dir.sub <- basename(dir_results)
  } else {
    is_local <- TRUE
    dir.sub <- dir_results
  }
  # wd <- "."
  # dir.sub <- basename(dir_results)
  dir.sub2 <- TargetSiteID
  dir.sub3 <- dir_sub

  if (!is_local) { # Is shiny
    ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
           , dir.create(file.path(wd, dir.sub, dir.sub2))
           , FALSE)
    ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3))==TRUE
           , dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3))
           , FALSE)
    dir_path <- file.path(wd, dir.sub, dir.sub2, dir.sub3)
  } else {
    ifelse(!dir.exists(file.path(dir.sub, dir.sub2))==TRUE
           , dir.create(file.path(dir.sub, dir.sub2))
           , FALSE)
    ifelse(!dir.exists(file.path(dir.sub, dir.sub2, dir.sub3))==TRUE
           , dir.create(file.path(dir.sub, dir.sub2, dir.sub3))
           , FALSE)
    dir_path <- file.path(dir.sub, dir.sub2, dir.sub3)
  }

  # Create dataset for outside the case, from which inside the case, reference
  # and target site data can be subset
  outcaseChemLONG <- df_Stress %>%
    dplyr::filter(StationID %in% outcaseSites) %>%
    dplyr::filter(StdParamName %in% siteChem) %>%
    dplyr::select(!c(IQRmethod, SDmethod, Outlier))

  outcaseChemLONG <- merge(outcaseChemLONG
                           , chemInfo[, c("StdParamName", "LogTransf")]
                           , by = "StdParamName")
  outcaseChemLONG <- outcaseChemLONG %>%
    dplyr::mutate(TransfValue = ifelse(LogTransf == 1
                                       , suppressWarnings(log1p(ResultValue))
                                       , ResultValue)) %>%
    dplyr::mutate(TransfValue = ifelse(!is.finite(TransfValue), ResultValue
                                       , TransfValue))

  # Use this dataframe for chemvalues table
  outcaseChemVals <- outcaseChemLONG %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName
                  , ResultValue) %>%
    tidyr::pivot_wider(names_from = StdParamName, values_from = ResultValue)

  # Use this dataframe for stressor id visualization & percentile rank
  outcaseChemData <- outcaseChemLONG %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName
                  , TransfValue) %>%
    dplyr::mutate(RefSiteFlag = ifelse(StationID %in% refSites, 1, 0)) %>%
    tidyr::pivot_wider(names_from = StdParamName, values_from = TransfValue)

  # ID all "reference" samples
  outcaseRefChemData <- outcaseChemData %>%
    dplyr::filter(RefSiteFlag == 1) %>%
    dplyr::select(!RefSiteFlag)

  # ID "comparator" samples
  incaseChemData <- outcaseChemData %>%
    dplyr::filter(StationID %in% incaseSites) %>%
    dplyr::select(!RefSiteFlag)

  # ID all "comparator reference" samples
  incaseRefChemData <- outcaseChemData %>%
    dplyr::filter(StationID %in% incaseSites) %>%
    dplyr::filter(RefSiteFlag == 1) %>%
    dplyr::select(!RefSiteFlag)

  # ID "target" samples
  siteChemData <- outcaseChemData %>%
    dplyr::filter(StationID == TargetSiteID) %>%
    dplyr::select(!RefSiteFlag)

  # clean up unnecessary objects
  rm(df_Stress, outcaseChemLONG)

  # This loop is not necessary now?
  # if (nrow(clusterRefChem)==0) {
  #   # No reference sites in the comparator set
  # } else {
  #   clusterRefChemData <- clusterRefChem %>%
  #     dplyr::select(!c(StationID, StressSampleID, StressSampleDate)) %>%
  #     dplyr::select_if(not_all_na)
  #   # clusterRefChemData <- clusterRefChem[7:ncol(clusterRefChem)]
  #   clustRefChemCols <- colnames(clusterRefChemData)
  #   # clustRefChemCols <- clustRefChemCols[!(clustRefChemCols %in% outliercols)]
  #   addcols <- setdiff(chemnames, clustRefChemCols)
  #   if (length(addcols)>0) {
  #     for (add in 1:length(addcols)) {
  #       addcolname <- addcols[add]
  #       clusterRefChemData[[addcolname]] <- NA
  #     }
  #     clusterRefChemData <- dplyr::select(clusterRefChemData, all_of(chemnames))
  #   }
  # }
  # rm(clusterRefChem)

  # Select parameters with > 2 samples for candidate cause identification
  # Adjusted (mostly) to use tidyverse syntax ARL 2023-05-27
  # chemnames <- colnames(outcaseChemData)
  # chemnames <- outcaseChemData %>%
  #   dplyr::select(!c(StressSampleDate, IQRmethod, SDmethod, Outlier))
  # chemnames <- colnames(chemnames)

  # Remove any parameters having all NA values and use only chemnames
  allcount <- outcaseChemData %>%
    dplyr::select_if(not_all_na) %>%
    dplyr::select(any_of(siteChem)) %>%
    dplyr::summarise(across(.cols = everything(), .fns = ~sum(!is.na(.))))

  # identify parameter names with <= samplim # samples for data gap identification
  uncoolvar <- as.character(colnames(allcount %>% dplyr::select_if(~ any(. <= samplim))))
  # identify parameter names with > samplim # samples
  allcountnames <- as.character(colnames(allcount %>% dplyr::select_if(~ any(. > samplim))))
  alltype <- dplyr::select_if(outcaseChemData, is.numeric) %>%
    dplyr::select(!RefSiteFlag)
  coolvar <- alltype[, allcountnames]
  groupnames <- unique(subset(chemInfo, chemInfo$StdParamName %in% siteChem
                              , select = "GroupName"))
  numgps <- length(groupnames[, 1])

  # Get data having <=2 samples in cluster, write to data gaps & add to eliminated
  # uncoolvar <- setdiff(chemnames, colnames(coolvar))
  if (length(uncoolvar) > 0) {
    df_allcount <- allcount %>%
      dplyr::select(all_of(uncoolvar))

    for (s in 1:ncol(df_allcount)) {
      elimName <- colnames(df_allcount)[s]
      gapcomment <- paste0("Number of outside-the-case samples is too few for analysis.")
      gaps <- cbind.data.frame("getStressorList", elimName
                               , df_allcount[[elimName]][1]
                               , gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                  , row.names = FALSE, sep = "\t")
      if (!exists("tmpParmDEL")) { tmpParmDEL <- elimName }
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
  # plots.g <- vector(numgps, mode="list")
  # Generate 1 box plot for each group, ref sites in blue, target site in red
  for (g in 1:numgps) {##FOR.g.START
    gpchems <- subset(chemInfo, GroupName == groupnames[g, ]
                      , select = c("StdParamName", "Label"))
    # gpcoolvar <- subset(coolvar, coolvar %in% gpchems$StdParamName)
    gpcoolvar <- coolvar[, colnames(coolvar) %in% gpchems$StdParamName]
    n <- length(gpcoolvar)
    #
    if(boo.DEBUG==TRUE){##IF~boo.DEBUG~START
      message(paste0("Item (", g, "/", numgps, ")"))
    }##IF~boo.DEBUG~START
    #
    if (n > 0) { ##FOR.n.START

      ## Plot, Data, Outside the case
      df_plot_wide <- gpcoolvar
      # need as.data.frame and colnames for groups with only 1 parameter
      df_plot_wide_min <- apply(df_plot_wide, 2, min, na.rm = TRUE)
      df_plot_wide_range <- apply(df_plot_wide, 2, range, na.rm = TRUE)
      df_plot_wide_diff <- apply(df_plot_wide_range, 2, diff)
      df_plot_wide_valminusmin <- sweep(df_plot_wide, 2, df_plot_wide_min, FUN = "-")
      df_plot_wide_mod <- sweep(df_plot_wide_valminusmin, 2, df_plot_wide_diff, FUN = "/")
      df_plot_long <- df_plot_wide_mod %>%
        tidyr::pivot_longer(cols = everything(), names_to = "GrpNm", values_to = "value") %>%
        dplyr::filter(!is.na(value))
      df_plot_long <- merge(gpchems, df_plot_long, by.x = "StdParamName", by.y = "GrpNm")

      if (nrow(df_plot_long) > 0) {boo_plot <- TRUE} else {boo_plot <- FALSE}

      ## Plot, Data, Comparator (inside the case)
      boo_plot_comp <- FALSE
      if (exists("incaseChemData")) {##IF~nrow(cluster.ref.chem.data)~START
        df_plot_comp_wide <- as.data.frame(incaseChemData[, colnames(gpcoolvar)])
        df_plot_comp_wide_valminusmin <- sweep(df_plot_comp_wide, 2, df_plot_wide_min, FUN = "-")
        df_plot_comp_wide_mod <- sweep(df_plot_comp_wide_valminusmin, 2, df_plot_wide_diff, FUN = "/")
        compchemcolnames <- colnames(df_plot_comp_wide_mod)

        if (any(colnames(gpcoolvar) %in% compchemcolnames)) { # Should this be ANY? --ARL CHECK
          df_plot_long_comp <- df_plot_comp_wide_mod %>%
            tidyr::pivot_longer(cols = everything(), names_to = "GrpNm"
                                , values_to = "value") %>%
            dplyr::filter(!is.na(value))
          df_plot_long_comp <- merge(gpchems, df_plot_long_comp, by.x="StdParamName", by.y = "GrpNm")
          boo_plot_comp <- ifelse(nrow(df_plot_long_comp) > 0, TRUE, FALSE)
          boo_plot_comp <- ifelse(all(is.na(df_plot_long_comp$value)), FALSE, TRUE)
        } else {
          boo_plot_comp <- FALSE
        }
      }##IF~nrow(cluster.ref.chem.data)~END

      ## Plot, Data, Reference
      boo_plot_ref <- FALSE
      if (exists("outcaseRefChemData")) {##IF~nrow(cluster.ref.chem.data)~START
        df_plot_ref_wide <- as.data.frame(outcaseRefChemData[, colnames(gpcoolvar)])
        df_plot_ref_wide_valminusmin <- sweep(df_plot_ref_wide, 2, df_plot_wide_min, FUN = "-")
        df_plot_ref_wide_mod <- sweep(df_plot_ref_wide_valminusmin, 2, df_plot_wide_diff, FUN = "/")
        refchemcolnames <- colnames(df_plot_ref_wide_mod)

        if (any(colnames(gpcoolvar) %in% refchemcolnames)) { # Should this be ANY? --ARL CHECK
          df_plot_long_ref <- df_plot_ref_wide_mod %>%
            tidyr::pivot_longer(cols = everything(), names_to = "GrpNm", values_to = "value") %>%
            dplyr::filter(!is.na(value))
          df_plot_long_ref <- merge(gpchems, df_plot_long_ref, by.x = "StdParamName", by.y = "GrpNm")
          boo_plot_ref <- ifelse(nrow(df_plot_long_ref) > 0, TRUE, FALSE)
          boo_plot_ref <- ifelse(all(is.na(df_plot_long_ref$value)), FALSE, TRUE)
        } else {
          boo_plot_ref <- FALSE
        }
      }##IF~nrow(cluster.ref.chem.data)~END

      ## Plot, Data, Target Site
      df_plot_targ_wide <- as.data.frame(siteChemData[, colnames(gpcoolvar)])
      df_plot_targ_wide_valminusmin <- sweep(df_plot_targ_wide, 2, df_plot_wide_min, FUN = "-")
      df_plot_targ_wide_mod <- sweep(df_plot_targ_wide_valminusmin, 2, df_plot_wide_diff, FUN = "/")
      df_plot_long_targ <- df_plot_targ_wide_mod %>%
        tidyr::pivot_longer(cols = everything(), names_to = "GrpNm", values_to = "value") %>%
        dplyr::filter(!is.na(value))
      df_plot_long_targ <- merge(gpchems, df_plot_long_targ, by.x="StdParamName", by.y="GrpNm")
      boo_plot_targ <- ifelse(nrow(siteChemData) != 0, TRUE, FALSE)

      qualtext <- "Reference"
      str_caption <- ""

      ## Plot, Variables, Strings
      str_Group <- stringr::str_to_sentence(as.character(groupnames[g, 1]))
      str_title <- paste0(TargetSiteID, ": Selection of detected stressors for"
                          , " evaluation as causes of impairment")
      str_title <- stringr::str_wrap(str_title,100)
      str_subtitle <- paste0("All stressor samples from outside the case ("
                            , outcaseLabel, " ", outcaseID, ")")
      str_xlab <- "Standardized values"
      str_ylab <- str_Group

      ## Plot, Variables, Colors
      col_sites_all     <- "dark gray"     # outside the case
      # col_sites_all_ref <- "blue"
      col_sites_cl      <- "cyan3"         # inside the case
      col_sites_cl_ref  <- "blue"          # reference sites inside the case
      col_sites_targ    <- "red"           # target site
      col_line          <- "black"

      ## Plot, Variables, Fill
      fill_sites_all     <- col_sites_all
      # fill_sites_all_ref <- col_sites_all_ref
      fill_sites_cl      <- col_sites_cl
      fill_sites_cl_ref  <- col_sites_all
      fill_sites_targ    <- col_sites_targ

      ## Plot, Variables, Points
      pch_sites_all     <- 19 # solid circle
      # pch_sites_all_ref <- 21 # circle outline
      pch_sites_cl      <- 19
      pch_sites_cl_ref  <- 21 # circle outline
      pch_sites_targ    <- 17 # triangle

      ## Plot, Variables, Sizes
      cex_mod <- 3
      cex_sites_all     <- cex_mod * 1
      # cex_sites_all_ref <- cex_mod * 1
      cex_sites_cl      <- cex_mod * 0.95
      cex_sites_cl_ref  <- cex_mod * 0.9
      cex_sites_targ    <- cex_mod * 2

      ## Plot, Variables, Legend
      leg_name   <- "Samples"
      leg_labels <- c("Outside the case", "Inside the case", qualtext, "Target")
      leg_shape  <- c(pch_sites_all, pch_sites_cl, pch_sites_cl_ref, pch_sites_targ)
      leg_col    <- c(col_sites_all, col_sites_cl, col_sites_cl_ref, col_sites_targ)
      leg_fill   <- c(fill_sites_all, fill_sites_cl, fill_sites_cl_ref, fill_sites_targ)

      if (n > 8) {
        yaxistextsize = 6
        wrap_length = 35
      } else {
        yaxistextsize = 7
        wrap_length = 27
      }

      if (boo_plot == TRUE) { # No rows in df_plot_long
        # ggplot, main (outside the case)
        p_SL <- ggplot2::ggplot(data=df_plot_long) +
          ggplot2::geom_boxplot(ggplot2::aes(x = stringr::str_wrap(Label, wrap_length)
                                             , y = value))  +
          ggplot2::geom_jitter(data=df_plot_long, width = 0.1
                               , ggplot2::aes(x = stringr::str_wrap(Label, wrap_length)
                                              , y = value, color = "col_sites_all"
                                              , shape = "pch_sites_all"
                                              , fill = "fill_sites_all")
                               , size = 1, na.rm = TRUE) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = str_title, subtitle = str_subtitle
                        , y = str_xlab, x = str_ylab, caption = str_caption) +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10)
                         , plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10)
                         , axis.text.x = ggplot2::element_blank()
                         , axis.text.y = ggplot2::element_text(size = yaxistextsize)
                         , axis.ticks.x = ggplot2::element_blank()
                         , plot.caption = ggplot2::element_text(size = 8))
        #
        # ggplot, points subsets
        ## Comparators (inside the case)
        if (boo_plot_comp == TRUE) {##IF~boo_plot_comp~START
          p_SL <- p_SL + ggplot2::geom_jitter(data = df_plot_long_comp, width = 0.1
                                              , ggplot2::aes(x = stringr::str_wrap(Label, wrap_length)
                                                             , y = value, color = "col_sites_cl"
                                                             , shape = "pch_sites_cl"
                                                             , fill = "fill_sites_cl")
                                              , size = 1, na.rm = TRUE)
        } else if (boo_plot_comp == FALSE) {
          p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color = "col_sites_cl"
                                                          , shape = "pch_sites_cl"
                                                          , fill = "fill_sites_cl"))
        }##IF~boo_plot_comp~END
        ## Comp, Ref
        if (boo_plot_ref == TRUE) {##IF~boo_plot_ref~START
          p_SL <- p_SL + ggplot2::geom_jitter(data = df_plot_long_ref, width = 0.1
                                              , ggplot2::aes(x = stringr::str_wrap(Label, wrap_length)
                                                             , y = value, color = "col_sites_cl_ref"
                                                             , shape = "pch_sites_cl_ref"
                                                             , fill = "fill_sites_cl_ref")
                                              , size = 1.5, na.rm = TRUE)
        } else if (boo_plot_ref == FALSE) {
          p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color = "col_sites_cl_ref"
                                                          , shape = "pch_sites_cl_ref"
                                                          , fill = "fill_sites_cl_ref"))
        }##IF~boo_plot_ref~END
        ## Target Site
        p_SL <- p_SL + ggplot2::geom_jitter(data = df_plot_long_targ, width = 0.1
                                            , ggplot2::aes(x = stringr::str_wrap(Label, wrap_length)
                                                           , y = value
                                                           , color = "col_sites_targ"
                                                           , shape = "pch_sites_targ"
                                                           , fill = "fill_sites_targ")
                                            , size = 1.5, na.rm = TRUE)
        #
        # ggplot, Legend
        p_SL <- p_SL + ggplot2::scale_shape_manual(name = leg_name
                                                   , labels = leg_labels
                                                   , values = leg_shape)  +
          ggplot2::scale_color_manual(name = leg_name, labels = leg_labels
                                      , values = leg_col) +
          ggplot2::scale_fill_manual(name = leg_name, labels = leg_labels
                                     , values = leg_fill)


        #
        if (!is_local) {message(p_SL)}
        # ENABLE THIS LATER
        # plots.g[[g]] <- grDevices::recordPlot()
        #
        # fn_title <- make.names(groupnames[g,])
        fn_title <- stringr::str_to_title(str_Group)
        fn_title <- gsub("\\s","",fn_title)
        fn_plot <- file.path(dir_path, paste0(TargetSiteID, "_CandCauses_"
                                              , fn_title, plot_ext))
        ggplot2::ggsave(fn_plot, p_SL, width = plot_W, height = plot_H, units = "in")
      }##IF.boo_plot==TRUE
    }##IF.n.END
  }##FOR.g.END

  # PDF ####
  # Create PDF from list
  # ENABLE THIS LATER
  # fn_pdf <- file.path(dir_path, paste0(TargetSiteID,"_",biocomm,"_"
  #                                      ,"CandCauses_ALL.pdf"))
  # grDevices::pdf(file=fn_pdf, width=plot_W, height=plot_H)
  #   for (i in plots.g){##FOR.gp.START
  #     #grDevices::replayPlot(g.plot)
  #     if(is.null(i)==TRUE) {next}
  #     grDevices::replayPlot(i)
  #   }##FOR.gp.END
  # grDevices::dev.off()
  # rm(plots.g)

  # Percentile Data File ####
  if (nrow(outcaseChemData) > 1) { # more than one sample from target site exists for cluster
    chem.clusterCore <- as.data.frame(outcaseChemData %>%
      dplyr::select(StationID, StressSampleID, StressSampleDate, RefSiteFlag))
    chem.pctrank <- as.data.frame(apply(outcaseChemData[, 5:ncol(outcaseChemData)]
                                        , 2, function(x) dplyr::percent_rank(x)))
    data.chem.pctrank <- cbind(chem.clusterCore, as.data.frame(chem.pctrank))
    fn.pctrank <- file.path(dir_path, paste0(TargetSiteID, "_CandCauses_ChemPctRank.tab"))
  } else { # only the target sample exists
    data.chem.pctrank <- cbind(outcaseChemData[, c("StationID", "StressSampleID"
                                               , "StressSampleDate")]
                               , outcaseChemData[, 5:ncol(outcaseChemData)])
    data.chem.pctrank[, 5:ncol(data.chem.pctrank)] <- 1
    fn.pctrank <- file.path(dir_path, paste0(TargetSiteID, "_CandCauses_ChemPctRank.tab"))
  }
  utils::write.table(data.chem.pctrank, fn.pctrank, sep = "\t", col.names = TRUE
                     , row.names = FALSE, append = FALSE)
  site.pctrank <- subset(data.chem.pctrank, StationID == TargetSiteID)
  stressor <- "none"
  #
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    c <- 5
  }##IF.boo.DEBUG.END

  # Handle exceptions from standard stressor list ID
  for (c in 5:ncol(site.pctrank)) {
    # print(c)
    chemname <- colnames(site.pctrank)[c]
    bad <- is.na(site.pctrank[, c])
    check <- site.pctrank[, c]
    good <- check[!bad]
    maxSiteRank <- max(good, na.rm = TRUE)
    minSiteRank <- min(good, na.rm = TRUE)
    maxSiteVal <- max(as.data.frame(siteChemData[, chemname]), na.rm = TRUE)
    minSiteVal <- min(as.data.frame(siteChemData[, chemname]), na.rm = TRUE)
    # DirIncStress ####
    # (not all in chem.info)
    if(chemname %in% chemInfo[, "StdParamName"]){
      ExpDirIncStress <- tolower((chemInfo[chemInfo[,"StdParamName"] == chemname
                                           ,"DirIncStress"])[1])
    } else {
      ExpDirIncStress <- "unk"
    }
    if (grepl("^pH[_]?", chemname, perl = TRUE, ignore.case = FALSE) == TRUE) {
      if ((minSiteVal < pHlimLow) | (maxSiteVal > pHlimHigh)) {
        if ((minSiteRank <= probsLow) | (maxSiteRank >= probsHigh)) {
          message("pH is a stressor.")
          stressor <- c(stressor, chemname)
        }
      } else {
        if (!exists("tmpParmDEL")) { tmpParmDEL <- chemname }
        else { tmpParmDEL <- c(tmpParmDEL, chemname) }
        message("pH is not a stressor.")
      }
      next()
    }
    if (ExpDirIncStress == "dec") {
      if (grepl("^DO_", chemname, perl = TRUE, ignore.case = FALSE) == TRUE) {

        if ((minSiteVal < DOlim) & (minSiteRank <= probsLow)) {
          message("DO is a stressor.")
          stressor <- c(stressor, chemname)
        } else {
          if (!exists("tmpParmDEL")) { tmpParmDEL <- chemname }
          else { tmpParmDEL <- c(tmpParmDEL, chemname) }
          message("DO is not a stressor.")
        }

      } else if (minSiteRank <= probsLow) {
        stressor <- c(stressor, chemname)
      }
    } else if ((ExpDirIncStress == "inc") && (maxSiteRank >= probsHigh)) {
      stressor <- c(stressor, chemname)
    }
  }##FOR~c~END

  # Stressor list contains identified candidate causes
  # bioParmsDEL contains parameters that don't apply for this biocomm
  # tmpParmDEL contains parameters with <= only 2 sample points for cluster data
  fn.stressorsExc <- file.path(dir_path, paste0(TargetSiteID
                                                , "_CandCauses_StressorsExcluded.tab"))
  fn.stressorsEval <- file.path(dir_path, paste0(TargetSiteID
                                                , "_CandCauses_StressorsEvaluated.tab"))

  for (b in seq_along(biocommlist)) {
    biocomm = biocommlist[b]
    bioParmsDEL = unlist(listbioParamsDEL[b])

    stressorlist <- setdiff(stressor, bioParmsDEL)
    stressorsExcepted <- intersect(stressor, bioParmsDEL)
    if (exists("tmpParmDEL")) {
      stressorsExcepted<-unique(c(stressorsExcepted, tmpParmDEL))
      stressorlist <- setdiff(stressorlist, tmpParmDEL)
    }
    # ID and write stressors evaluated by biocomm
    stressorsEvaluated <- setdiff(stressor, stressorsExcepted)
    stressorsEvaluated <- as.data.frame(stressorsEvaluated) %>%
      dplyr::mutate(Biocomm = biocomm)
    colnames(stressorsEvaluated)[1] <- "Stressor"
    stressorsEvaluated <- merge(stressorsEvaluated
                                , chemInfo[,c("StdParamName", "Label")]
                               , by.x = "Stressor", by.y = "StdParamName"
                               , all.x = TRUE)
    if (nrow(stressorsEvaluated)==0) {
      stressorsEvaluated <- rbind(stressorsEvaluated
                                  , (cbind("None", biocomm, "None")))
    }
    colnames(stressorsEvaluated) <- c("Stressor","BioComm","Label")
    stressorsEvaluated <- unique(stressorsEvaluated)
    stressorsEvaluated <- dplyr::filter(stressorsEvaluated, Stressor != "none")
    # Write stressors evaluated table
    if (file.exists(fn.stressorsEval)) {
      utils::write.table(stressorsEvaluated, fn.stressorsEval, sep="\t"
                         , col.names=FALSE, row.names = FALSE, append=TRUE)
    } else {
      utils::write.table(stressorsEvaluated, fn.stressorsEval, sep="\t"
                         , col.names=TRUE, row.names = FALSE, append=FALSE)
    }
    # ID and write stressors evaluated by biocomm
    stressorsExcepted <- as.data.frame(stressorsExcepted) %>%
      dplyr::mutate(Biocomm = biocomm)
    colnames(stressorsExcepted)[1] <- "Stressor"
    stressorsExcepted <- merge(stressorsExcepted
                               , chemInfo[,c("StdParamName", "Label")]
                               , by.x = "Stressor", by.y = "StdParamName"
                               , all.x = TRUE)
    if (nrow(stressorsExcepted)==0) {
      stressorsExcepted <- rbind(stressorsExcepted
                                 , (cbind("None", biocomm, "None")))
    }
    colnames(stressorsExcepted) <- c("Stressor","BioComm","Label")
    stressorsExcepted <- unique(stressorsExcepted)
    # Write stressors excepted table
    if (file.exists(fn.stressorsExc)) {
      utils::write.table(stressorsExcepted, fn.stressorsExc, sep="\t"
                         , col.names=FALSE, row.names = FALSE, append=TRUE)
    } else {
      utils::write.table(stressorsExcepted, fn.stressorsExc, sep="\t"
                         , col.names=TRUE, row.names = FALSE, append=FALSE)
    }
  }

  # LogTransf ####
  # 20190110, get log transformation code from chem.info
  # x <- unique(chem.info[chem.info$StdParamName %in% stressorlist, c("StdParamName", "LogTransf")])
  # need to use max (default of 1) in case of duplicates
  chemInfo_LogTransf <- chemInfo %>%
    dplyr::group_by(StdParamName) %>%
    dplyr::summarise(max_LogTransf = max(LogTransf, na.rm = TRUE)
                     , .groups = "drop_last")
  stressorlist4merge <- data.frame(StdParamName = stressorlist
                                   , Sort = 1:length(stressorlist))
  # merge
  LogTransf_merge <- merge(stressorlist4merge, chemInfo_LogTransf, all.x = TRUE)
  # sort
  LogTransf_merge <- LogTransf_merge[order(LogTransf_merge$Sort), ]
  # NA to 0
  LogTransf_merge[is.na(LogTransf_merge[,"max_LogTransf"]), "max_LogTransf"] <- 0


  # Data File ####
  stressorlist_trim <- stressorlist[stressorlist != "none"]
  data.chemVals <- outcaseChemVals %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate
                  , eval(stressorlist_trim))
  fn.chemVals <- file.path(dir_path, paste0(TargetSiteID,
                                            "_CandCauses_ChemValues.tab"))
  utils::write.table(data.chemVals, fn.chemVals, sep = "\t", col.names = TRUE
                     , row.names = FALSE, append = FALSE)

  # create output ####
  myStressors <- list(stressors = stressorlist
                      , site.stressor.pctrank = site.pctrank
                      , stressors_LogTransf = LogTransf_merge$max_LogTransf)
  #
  return(myStressors)

} # FUN end
