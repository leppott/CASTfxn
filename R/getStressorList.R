#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.2
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
#'                  the case" id
#' @param outcaseSites Vector containing "outside the case" site IDs
#' @param incaseSites Vector containing "inside the case" site IDs (comparators)
#' @param refSites all reference sites
#' @param siteChem dataframe containing any detected stressor data from target
#'                 site samples at any time
#' @param df_Stress dataframe containing stressor data
#' @param cheminfo dataframe containing stressor metadata, specifically "Label",
#'                 "DirIncStress", and "LogTransformYN"
#' @param samplim minimum number of samples required to id stressors as
#' candidate causes. Defaults to 10.
#' @param probsHigh probabilities, high. Defaults to 0.75. For stressors considered
#'                  to be increasers (stress increases with increasing concentration),
#'                  values above this are considered to be candidate causes.
#' @param probsLow probabilities, low, Defaults to 0.25.For stressors considered
#'                 to be decreasers (stress increases with decreasing concentration),
#'                 values above this are considered to be candidate causes.
#' @param DOlim Dissolved Oxygen limit, default = 7. Used to exclude values
#'              < probsLow if they are also > 7 mg/L.
#' @param pHlimLow  pH limit, low, default = 6.5. Used to exclude values
#'                  < probsLow if they are also between 6.5 and 9 pH units.
#' @param pHlimHigh pH limit, high, default = 9. Used to exclude values
#'                  > probsHigh if they are also between 6.5 and 9 pH units.
#' @param biocommlist vector of each biological response community available
#'                    ("bmi", "alg", or "fish")
#' @param listbioParamsDEL list of vectors corresponding to biocommlist of
#'                         stressors not considered relevant for evaluation
#' @param dir_results Directory to save plots. Default = file.path(getwd(), "Results").
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo"
#'
#' @return One or more png files containing normalized stressor boxplots by stressor
#' group; a correlation matrix representing stressor correlations; a file of stressors
#' evaluated; a file of stressors excluded; stressor values and site.stressor.pctrank.
#'
#' @examples
#' \dontrun{
#' }
#' @export
#'
getStressorList <- function(TargetSiteID,
                            outcaseLabel,
                            outcaseID,
                            outcaseSites,
                            incaseSites,
                            refSites,
                            siteChem,
                            df_Stress,
                            chemInfo,
                            samplim = 10,
                            probsHigh = 0.75,
                            probsLow = 0.25,
                            DOlim = 7,
                            pHlimLow = 6.5,
                            pHlimHigh = 9,
                            biocommlist,
                            listbioParamsDEL,
                            dir_results = file.path(getwd(), "Results"),
                            dir_sub = "CandidateCauses") {##FUNCTION.START
  # DEBUGGING ####
  boo.DEBUG <- FALSE
  #
  if (boo.DEBUG == TRUE) {##IF.boo.DEBUG.START
    TargetSiteID = TargetSiteID
    outcaseLabel = outcaseLabel
    outcaseID = list.CompSites$outcaseID
    outcaseSites = list.CompSites$all.sites
    incaseSites = list.CompSites$comp.sites
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
  if (is.character(outcaseID)) {
    outcaseID <- tolower(outcaseID)
  }
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
    ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2)) == TRUE,
           dir.create(file.path(wd, dir.sub, dir.sub2)),
           FALSE)
    ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2, dir.sub3)) == TRUE,
           dir.create(file.path(wd, dir.sub, dir.sub2, dir.sub3)),
           FALSE)
    dir_path <- file.path(wd, dir.sub, dir.sub2, dir.sub3)
  } else {
    ifelse(!dir.exists(file.path(dir.sub, dir.sub2)) == TRUE,
           dir.create(file.path(dir.sub, dir.sub2)),
           FALSE)
    ifelse(!dir.exists(file.path(dir.sub, dir.sub2, dir.sub3)) == TRUE,
           dir.create(file.path(dir.sub, dir.sub2, dir.sub3)),
           FALSE)
    dir_path <- file.path(dir.sub, dir.sub2, dir.sub3)
  }

  # Create dataset for outside the case, from which inside the case, reference
  # and target site data can be subset
  outcaseChemLONG <- df_Stress %>%
    dplyr::filter(StationID %in% outcaseSites) %>%
    dplyr::filter(StdParamName %in% siteChem) %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate, StdParamName,
                  ResultValue, TransfResult)

  # Use this dataframe for chemvalues table
  outcaseChemVals <- outcaseChemLONG %>%
    dplyr::select(!TransfResult) %>%
    tidyr::pivot_wider(names_from = StdParamName, values_from = ResultValue)

  # Use this dataframe for stressor id visualization & percentile rank
  outcaseChemData <- outcaseChemLONG %>%
    dplyr::select(!ResultValue) %>%
    dplyr::mutate(RefSiteFlag = ifelse(StationID %in% refSites, 1, 0)) %>%
    tidyr::pivot_wider(names_from = StdParamName, values_from = TransfResult)

  # ID all "reference" samples
  outcaseRefChemData <- outcaseChemData %>%
    dplyr::filter(RefSiteFlag == 1)

  # ID "comparator" samples
  incaseChemData <- outcaseChemData %>%
    dplyr::filter(StationID %in% incaseSites)

  # ID all "comparator reference" samples
  incaseRefChemData <- outcaseChemData %>%
    dplyr::filter(StationID %in% incaseSites) %>%
    dplyr::filter(RefSiteFlag == 1)

  # ID "target" samples
  siteChemData <- outcaseChemData %>%
    dplyr::filter(StationID == TargetSiteID)

  # clean up unnecessary objects
  rm(df_Stress, outcaseChemLONG)

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
  groupnames <- unique(subset(chemInfo, chemInfo$StdParamName %in% siteChem,
                              select = "GroupName"))
  numgps <- length(groupnames[, 1])

  # Get data having <=2 samples in cluster, write to data gaps & add to eliminated
  # uncoolvar <- setdiff(chemnames, colnames(coolvar))
  if (length(uncoolvar) > 0) {
    df_allcount <- allcount %>%
      dplyr::select(all_of(uncoolvar))

    for (s in 1:ncol(df_allcount)) {
      elimName <- colnames(df_allcount)[s]
      gapcomment <- paste0("Number of outside-the-case samples is too few for analysis.")
      gaps <- cbind.data.frame("getStressorList", elimName,
                               df_allcount[[elimName]][1],
                               gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")
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
    gpchems <- subset(chemInfo, GroupName == groupnames[g, ],
                      select = c("StdParamName", "Label"))
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
      df_plot_wide_valminusmin <- sweep(df_plot_wide, 2,
                                        df_plot_wide_min, FUN = "-")
      df_plot_wide_mod <- sweep(df_plot_wide_valminusmin, 2,
                                df_plot_wide_diff, FUN = "/")
      df_plot_long <- df_plot_wide_mod %>%
        tidyr::pivot_longer(cols = everything(), names_to = "GrpNm",
                            values_to = "value") %>%
        dplyr::filter(!is.na(value))
      df_plot_long <- merge(gpchems, df_plot_long, by.x = "StdParamName",
                            by.y = "GrpNm")

      if (nrow(df_plot_long) > 0) {boo_plot <- TRUE} else {boo_plot <- FALSE}

      ## Plot, Data, Comparator (inside the case)
      boo_plot_comp <- FALSE
      if (exists("incaseChemData")) {##IF~nrow(cluster.ref.chem.data)~START
        df_plot_comp_wide <- as.data.frame(incaseChemData[, colnames(gpcoolvar)])
        df_plot_comp_wide_valminusmin <- sweep(df_plot_comp_wide, 2,
                                               df_plot_wide_min, FUN = "-")
        df_plot_comp_wide_mod <- sweep(df_plot_comp_wide_valminusmin, 2,
                                       df_plot_wide_diff, FUN = "/")
        compchemcolnames <- colnames(df_plot_comp_wide_mod)

        if (any(colnames(gpcoolvar) %in% compchemcolnames)) { # Should this be ANY? --ARL CHECK
          df_plot_long_comp <- df_plot_comp_wide_mod %>%
            tidyr::pivot_longer(cols = everything(), names_to = "GrpNm",
                                values_to = "value") %>%
            dplyr::filter(!is.na(value))
          df_plot_long_comp <- merge(gpchems, df_plot_long_comp,
                                     by.x="StdParamName", by.y = "GrpNm")
          boo_plot_comp <- ifelse(nrow(df_plot_long_comp) > 0, TRUE, FALSE)
          boo_plot_comp <- ifelse(all(is.na(df_plot_long_comp$value)), FALSE, TRUE)
        } else {
          boo_plot_comp <- FALSE
        }
      }##IF~nrow(cluster.ref.chem.data)~END

      ## Plot, Data, Reference outside-the-case
      boo_plot_ref_out <- FALSE
      if (exists("outcaseRefChemData")) {##IF~nrow(cluster.ref.chem.data)~START
        df_plot_ref_out_wide <- as.data.frame(outcaseRefChemData[, colnames(gpcoolvar)])
        df_plot_ref_out_wide_valminusmin <- sweep(df_plot_ref_out_wide, 2,
                                                  df_plot_wide_min, FUN = "-")
        df_plot_ref_out_wide_mod <- sweep(df_plot_ref_out_wide_valminusmin, 2,
                                          df_plot_wide_diff, FUN = "/")
        refchemcolnames <- colnames(df_plot_ref_out_wide_mod)

        if (any(colnames(gpcoolvar) %in% refchemcolnames)) { # Should this be ANY? --ARL CHECK
          df_plot_long_ref_out <- df_plot_ref_out_wide_mod %>%
            tidyr::pivot_longer(cols = everything(), names_to = "GrpNm",
                                values_to = "value") %>%
            dplyr::filter(!is.na(value))
          df_plot_long_ref_out <- merge(gpchems, df_plot_long_ref_out,
                                        by.x = "StdParamName", by.y = "GrpNm")
          boo_plot_ref_out <- ifelse(nrow(df_plot_long_ref_out) > 0, TRUE, FALSE)
          boo_plot_ref_out <- ifelse(all(is.na(df_plot_long_ref_out$value)), FALSE, TRUE)
        } else {
          boo_plot_ref_out <- FALSE
        }
      }##IF~nrow(cluster.ref.chem.data)~END

      ## Plot, Data, Reference inside-the-case
      boo_plot_ref_in <- FALSE
      if (exists("incaseRefChemData")) {##IF~nrow(cluster.ref.chem.data)~START
        df_plot_ref_in_wide <- as.data.frame(incaseRefChemData[, colnames(gpcoolvar)])
        df_plot_ref_in_wide_valminusmin <- sweep(df_plot_ref_in_wide, 2,
                                                 df_plot_wide_min, FUN = "-")
        df_plot_ref_in_wide_mod <- sweep(df_plot_ref_in_wide_valminusmin, 2,
                                         df_plot_wide_diff, FUN = "/")
        refchemcolnames <- colnames(df_plot_ref_in_wide_mod)

        if (any(colnames(gpcoolvar) %in% refchemcolnames)) { # Should this be ANY? --ARL CHECK
          df_plot_long_ref_in <- df_plot_ref_in_wide_mod %>%
            tidyr::pivot_longer(cols = everything(), names_to = "GrpNm",
                                values_to = "value") %>%
            dplyr::filter(!is.na(value))
          df_plot_long_ref_in <- merge(gpchems, df_plot_long_ref_in,
                                       by.x = "StdParamName", by.y = "GrpNm")
          boo_plot_ref_in <- ifelse(nrow(df_plot_long_ref_in) > 0, TRUE, FALSE)
          boo_plot_ref_in <- ifelse(all(is.na(df_plot_long_ref_in$value)), FALSE, TRUE)
        } else {
          boo_plot_ref_in <- FALSE
        }
      }##IF~nrow(cluster.ref.chem.data)~END

      ## Plot, Data, Target Site
      df_plot_targ_wide <- as.data.frame(siteChemData[, colnames(gpcoolvar)])
      df_plot_targ_wide_valminusmin <- sweep(df_plot_targ_wide, 2,
                                             df_plot_wide_min, FUN = "-")
      df_plot_targ_wide_mod <- sweep(df_plot_targ_wide_valminusmin, 2,
                                     df_plot_wide_diff, FUN = "/")
      df_plot_long_targ <- df_plot_targ_wide_mod %>%
        tidyr::pivot_longer(cols = everything(), names_to = "GrpNm",
                            values_to = "value") %>%
        dplyr::filter(!is.na(value))
      df_plot_long_targ <- merge(gpchems, df_plot_long_targ,
                                 by.x="StdParamName", by.y="GrpNm")
      boo_plot_targ <- ifelse(nrow(siteChemData) != 0, TRUE, FALSE)

      qualtext <- "Reference"
      str_caption <- ""

      ## Plot, Variables, Strings
      str_Group <- stringr::str_to_sentence(as.character(groupnames[g, 1]))
      str_title <- paste0(TargetSiteID, ": Selection of detected stressors for"
                          , " evaluation as causes of impairment")
      str_title <- stringr::str_wrap(str_title, 100)
      if (outcaseLabel == outcaseID) {
        str_subtitle <- paste0("All stressor samples from outside the case ("
                               , outcaseLabel, ")")
      } else {
        str_subtitle <- paste0("All stressor samples from outside the case ("
                               , outcaseLabel, " ", outcaseID, ")")
      }
      # message(str_subtitle)
      str_xlab <- "Standardized values"
      str_ylab <- str_Group

      ## Plot, Variables, Colors
      col_sites_all     <- "gray40"        # outside the case
      col_sites_all_ref <- "blue"         # reference sites outside the case
      col_sites_cl      <- "cyan4"        # inside the case
      col_sites_cl_ref  <- "blue"         # reference sites outside the case
      col_sites_targ    <- "red"          # target site
      col_line          <- "black"

      ## Plot, Variables, Fill
      fill_sites_all     <- "gray40"
      fill_sites_all_ref <- "gray40"
      fill_sites_cl      <- "cyan4"
      fill_sites_cl_ref  <- "cyan4"
      fill_sites_targ    <- "red"

      ## Plot, Variables, Points
      pch_sites_all     <- 21 # circle with outline
      pch_sites_all_ref <- 21 # circle outline
      pch_sites_cl      <- 21 # circle outline
      pch_sites_cl_ref  <- 21 # circle outline
      pch_sites_targ    <- 17 # triangle

      ## Plot, Variables, Sizes
      cex_mod <- 2.5
      cex_sites_all     <- cex_mod * 1
      cex_sites_all_ref <- cex_mod * 1
      cex_sites_cl      <- cex_mod * 0.95
      cex_sites_cl_ref  <- cex_mod * 0.9
      cex_sites_targ    <- cex_mod * 2

      ## Plot, Variables, Legend
      leg_name   <- "Samples"
      leg_labels <- c("Outside the case", "Inside the case",
                      "Outside the case, reference",
                      "Inside the case, reference", "Target")
      leg_shape  <- c(pch_sites_all, pch_sites_cl, pch_sites_all_ref,
                      pch_sites_cl_ref, pch_sites_targ)
      leg_col    <- c(col_sites_all, col_sites_cl, col_sites_all_ref,
                      col_sites_cl_ref, col_sites_targ)
      leg_fill   <- c(fill_sites_all, fill_sites_cl, fill_sites_all_ref,
                      fill_sites_cl_ref, fill_sites_targ)

      if (n > 8) {
        yaxistextsize = 6
        wrap_length = 35
      } else {
        yaxistextsize = 7
        wrap_length = 27
      }

      if (boo_plot == TRUE) { # No rows in df_plot_long
        # ggplot, main (outside the case)
        p_SL <- ggplot2::ggplot(data = df_plot_long) +
          ggplot2::geom_boxplot(ggplot2::aes(x = stringr::str_wrap(Label, wrap_length),
                                             y = value), staplewidth = 0.5)  +
          ggplot2::geom_jitter(data = df_plot_long, width = 0.1,
                               ggplot2::aes(x = stringr::str_wrap(Label, wrap_length),
                                            y = value, color = "col_sites_all",
                                            stroke = 0.5,
                                            shape = "pch_sites_all",
                                            fill = "fill_sites_all"),
                               size = 1, na.rm = TRUE, show.legend = TRUE) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = str_title, subtitle = str_subtitle,
                        y = str_xlab, x = str_ylab, caption = str_caption) +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                         plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
                         axis.text.x = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(size = yaxistextsize),
                         axis.ticks.x = ggplot2::element_blank(),
                         plot.caption = ggplot2::element_text(size = 8),
                         legend.title.position = "top",
                         legend.title = ggplot2::element_text(size = 8),
                         legend.text = ggplot2::element_text(size = 6))
        #
        # ggplot, points subsets
        ## Comparators (inside the case)
        if (boo_plot_comp == TRUE) {##IF~boo_plot_comp~START
          p_SL <- p_SL + ggplot2::geom_jitter(data = df_plot_long_comp, width = 0.1,
                                              ggplot2::aes(x = stringr::str_wrap(Label, wrap_length),
                                                           y = value, color = "col_sites_cl",
                                                           shape = "pch_sites_cl",
                                                           fill = "fill_sites_cl"),
                                              size = 1, na.rm = TRUE, show.legend = TRUE)
        } else if (boo_plot_comp == FALSE) {
          p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color = "col_sites_cl",
                                                          shape = "pch_sites_cl",
                                                          fill = "fill_sites_cl"))
        }##IF~boo_plot_comp~END
        ## Out, Ref
        if (boo_plot_ref_out == TRUE) {##IF~boo_plot_ref~START
          p_SL <- p_SL + ggplot2::geom_jitter(data = df_plot_long_ref_out, width = 0.1,
                                              ggplot2::aes(x = stringr::str_wrap(Label, wrap_length),
                                                           y = value, color = "col_sites_all_ref",
                                                           shape = "pch_sites_all_ref",
                                                           fill = "fill_sites_all_ref"),
                                              size = 1.5, na.rm = TRUE, show.legend = TRUE)
        } else if (boo_plot_ref == FALSE) {
          p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color = "col_sites_all_ref",
                                                          shape = "pch_sites_all_ref",
                                                          fill = "fill_sites_all_ref"))
        }##IF~boo_plot_ref~END
        ## In, Ref
        if (boo_plot_ref_in == TRUE) {##IF~boo_plot_ref_in~START
          p_SL <- p_SL + ggplot2::geom_jitter(data = df_plot_long_ref_in, width = 0.1,
                                              ggplot2::aes(x = stringr::str_wrap(Label, wrap_length),
                                                           y = value, color = "col_sites_cl_ref",
                                                           shape = "pch_sites_cl_ref",
                                                           fill = "fill_sites_cl_ref"),
                                              size = 1.5, na.rm = TRUE, show.legend = TRUE)
        } else if (boo_plot_ref == FALSE) {
          p_SL <- p_SL + ggplot2::geom_blank(ggplot2::aes(color = "col_sites_cl_ref",
                                                          shape = "pch_sites_cl_ref",
                                                          fill = "fill_sites_cl_ref"))
        }##IF~boo_plot_ref~END
        ## Target Site
        p_SL <- p_SL + ggplot2::geom_jitter(data = df_plot_long_targ, width = 0.1,
                                            ggplot2::aes(x = stringr::str_wrap(Label, wrap_length),
                                                         y = value,
                                                         color = "col_sites_targ",
                                                         shape = "pch_sites_targ",
                                                         fill = "fill_sites_targ"),
                                            size = 1.5, na.rm = TRUE, show.legend = TRUE)
        #
        # ggplot, Legend
        p_SL <- p_SL + ggplot2::scale_shape_manual(name = leg_name,
                                                   labels = leg_labels,
                                                   values = leg_shape)  +
          ggplot2::scale_color_manual(name = leg_name, labels = leg_labels,
                                      values = leg_col) +
          ggplot2::scale_fill_manual(name = leg_name, labels = leg_labels,
                                     values = leg_fill)
        #
        if (!is_local) {message(p_SL)}

        fn_title <- stringr::str_to_title(str_Group)
        fn_title <- gsub("\\s", "", fn_title)
        fn_plot <- file.path(dir_path, paste0(TargetSiteID, "_CandCauses_",
                                              fn_title, plot_ext))
        ggplot2::ggsave(fn_plot, p_SL, width = plot_W, height = plot_H, units = "in")
      }##IF.boo_plot==TRUE
    }##IF.n.END
  }##FOR.g.END

  # Percentile Data File ####
  # NOTE: This is based on outside the case data, which is what the boxplots use
  # If EPA prefers to identify candidate causes relative to comparators, then the
  # identification below must change, and the plots should also change
  if (nrow(outcaseChemData) > 1) { # more than one sample from target site exists for cluster
    chem.clusterCore <- as.data.frame(outcaseChemData %>%
      dplyr::select(StationID, StressSampleID, StressSampleDate))
    chem.pctrank <- as.data.frame(apply(outcaseChemData[, 5:ncol(outcaseChemData)],
                                        2, function(x) dplyr::percent_rank(x)))
    data.chem.pctrank <- cbind(chem.clusterCore, as.data.frame(chem.pctrank))
    fn.pctrank <- file.path(dir_path, paste0(TargetSiteID, "_CandCauses_ChemPctRank.tab"))
  } else { # only the target sample exists
    data.chem.pctrank <- cbind(outcaseChemData[, c("StationID", "StressSampleID",
                                                   "StressSampleDate")],
                               outcaseChemData[, 5:ncol(outcaseChemData)])
    data.chem.pctrank[, 5:ncol(data.chem.pctrank)] <- 1
    fn.pctrank <- file.path(dir_path, paste0(TargetSiteID, "_CandCauses_ChemPctRank.tab"))
  }
  utils::write.table(data.chem.pctrank, fn.pctrank, sep = "\t",
                     col.names = TRUE, row.names = FALSE, append = FALSE)
  site.pctrank <- subset(data.chem.pctrank, StationID == TargetSiteID)
  stressor <- "none"
  #
  if(boo.DEBUG==TRUE){##IF.boo.DEBUG.START
    c <- 4
  }##IF.boo.DEBUG.END

  # Handle exceptions from standard stressor list ID
  for (c in 4:ncol(site.pctrank)) {
    chemname <- colnames(site.pctrank)[c]
    # msg <- paste0("Evaluating ", chemname, " as a potential candidate cause.")
    # message(msg)
    bad <- is.na(site.pctrank[, c])
    check <- site.pctrank[, c]
    good <- check[!bad]
    maxSiteRank <- max(good, na.rm = TRUE)
    minSiteRank <- min(good, na.rm = TRUE)
    maxSiteVal <- max(as.data.frame(siteChemData[, chemname]), na.rm = TRUE)
    minSiteVal <- min(as.data.frame(siteChemData[, chemname]), na.rm = TRUE)
    # DirIncStress ####
    # (not all in chem.info)
    if (chemname %in% chemInfo[, "StdParamName"]) {
      ExpDirIncStress <- tolower((chemInfo[chemInfo[,"StdParamName"] == chemname,
                                           "DirIncStress"])[1])
    } else {
      ExpDirIncStress <- "unk"
    }
    if (grepl("^pH[_]?", chemname, perl = TRUE, ignore.case = FALSE) == TRUE) {
      if ((minSiteVal < pHlimLow) | (maxSiteVal > pHlimHigh)) {
        if ((minSiteRank <= probsLow) | (maxSiteRank >= probsHigh)) {
          message("pH is a candidate cause")
          stressor <- c(stressor, chemname)
        }
      } else {
        if (!exists("tmpParmDEL")) { tmpParmDEL <- chemname }
        else { tmpParmDEL <- c(tmpParmDEL, chemname) }
        message("pH is not a candidate cause")
      }
      next()
    }
    if (ExpDirIncStress == "dec") {
      if (grepl("^DO_", chemname, perl = TRUE, ignore.case = FALSE) == TRUE) {

        if ((minSiteVal < DOlim) & (minSiteRank <= probsLow)) {
          message("DO is a candidate cause")
          stressor <- c(stressor, chemname)
        } else {
          if (!exists("tmpParmDEL")) { tmpParmDEL <- chemname }
          else { tmpParmDEL <- c(tmpParmDEL, chemname) }
          message("DO is not a candidate cause")
        }

      } else if (minSiteRank <= probsLow) {
        stressor <- c(stressor, chemname)
        msg <- paste0(chemname, " is a candidate cause")
        message(msg)
      }
    } else if ((ExpDirIncStress == "inc") && (maxSiteRank >= probsHigh)) {
      stressor <- c(stressor, chemname)
      msg <- paste0(chemname, " is a candidate cause")
      message(msg)
    }
  }##FOR~c~END

  # Stressor variable contains identified candidate causes
  # bioParmsDEL contains parameters that don't apply for this biocomm
  # tmpParmDEL contains parameters with <= only 2 sample points for cluster data
  fn.stressorsExc <- file.path(dir_path,
                               paste0(TargetSiteID, "_CandCauses_StressorsExcluded.tab"))
  fn.stressorsEval <- file.path(dir_path,
                                paste0(TargetSiteID, "_CandCauses_StressorsEvaluated.tab"))

  for (b in seq_along(biocommlist)) {
    biocomm = biocommlist[b]
    bioParmsDEL = unlist(listbioParamsDEL[b])

    stressorlist <- setdiff(stressor, bioParmsDEL)
    stressorsExcepted <- intersect(stressor, bioParmsDEL)
    if (exists("tmpParmDEL")) {
      stressorsExcepted <- unique(c(stressorsExcepted, tmpParmDEL))
      stressorlist <- setdiff(stressorlist, tmpParmDEL)
    }
    # ID and write stressors evaluated by biocomm
    stressorsEvaluated <- setdiff(stressor, stressorsExcepted)
    stressorsEvaluated <- as.data.frame(stressorsEvaluated) %>%
      dplyr::mutate(Biocomm = biocomm)
    colnames(stressorsEvaluated)[1] <- "Stressor"
    stressorsEvaluated <- merge(stressorsEvaluated,
                                chemInfo,
                                by.x = "Stressor", by.y = "StdParamName",
                                all.x = TRUE)
    stressorsEvaluated <- stressorsEvaluated %>%
      dplyr::select(Biocomm, Stressor, GroupName, Label, LogTransf, SSTV, SSI,
                    SensMin, SensMax, TolMin, TolMax, DirIncStress, SSTVname,
                    SSIndex)
    if (nrow(stressorsEvaluated)==0) {
      stressorsEvaluated <- rbind(stressorsEvaluated,
                                  (cbind(biocomm, "None", "None", "None", "None",
                                         "None", "None", "None", "None", "None",
                                         "None", "None", "None", "None")))
    }
    stressorsEvaluated <- unique(stressorsEvaluated)
    stressorsEvaluated <- dplyr::filter(stressorsEvaluated, Stressor != "none")
    # Write stressors evaluated table
    if (file.exists(fn.stressorsEval)) {
      utils::write.table(stressorsEvaluated, fn.stressorsEval, sep = "\t",
                         col.names = FALSE, row.names = FALSE, append = TRUE)
    } else {
      utils::write.table(stressorsEvaluated, fn.stressorsEval, sep = "\t",
                         col.names = TRUE, row.names = FALSE, append = FALSE)
    }
    # ID and write stressors evaluated by biocomm
    stressorsExcepted <- as.data.frame(stressorsExcepted) %>%
      dplyr::mutate(Biocomm = biocomm)
    colnames(stressorsExcepted)[1] <- "Stressor"
    stressorsExcepted <- merge(stressorsExcepted,
                               chemInfo,
                               by.x = "Stressor", by.y = "StdParamName",
                               all.x = TRUE)
    stressorsExcepted <- stressorsExcepted %>%
      dplyr::select(Biocomm, Stressor, GroupName, Label, LogTransf, SSTV, SSI,
                    SensMin, SensMax, TolMin, TolMax, DirIncStress, SSTVname,
                    SSIndex)
    if (nrow(stressorsExcepted) == 0) {
      stressorsExcepted <- rbind(stressorsExcepted,
                                 (cbind(biocomm, "None", "None", "None", "None",
                                        "None", "None", "None", "None", "None",
                                        "None", "None", "None", "None")))
    }
    stressorsExcepted <- unique(stressorsExcepted)
    # Write stressors excepted table
    if (file.exists(fn.stressorsExc)) {
      utils::write.table(stressorsExcepted, fn.stressorsExc, sep = "\t",
                         col.names = FALSE, row.names = FALSE, append = TRUE)
    } else {
      utils::write.table(stressorsExcepted, fn.stressorsExc, sep = "\t",
                         col.names = TRUE, row.names = FALSE, append = FALSE)
    }
  }

  # Data File ####
  stressor_trim <- stressor[stressor != "none"]
  data.chemVals <- outcaseChemVals %>%
    dplyr::select(StationID, StressSampleID, StressSampleDate,
                  eval(stressor_trim))
  fn.chemVals <- file.path(dir_path, paste0(TargetSiteID,
                                            "_CandCauses_ChemValues.tab"))
  utils::write.table(data.chemVals, fn.chemVals, sep = "\t", col.names = TRUE,
                     row.names = FALSE, append = FALSE)

  # create output ####
  myStressors <- list(stressors = stressorsEvaluated,
                      site.stressor.pctrank = site.pctrank)
  #
  return(myStressors)

} # FUN end
