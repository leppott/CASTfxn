getWSStressorFigs <- function(df_WSData = NULL,
                              df_WSInfo = NULL,
                              comp.reaches =  list.CompSites$comp.reaches,
                              TargetCOMID = list.CompSites$TargetCOMID,
                              dir_sub = "SiteInfo",
                              df_SampSummary = data_sampSummary,
                              biocommlist = biocommlist,
                              boo_plot = TRUE) { # FUN: Start

  boo.debug <- FALSE

  if (boo.debug) {
    df_WSData <- data_stressorWS
    df_WSInfo <- data_stressorinfoWS
    comp.reaches <-  list.CompSites$comp.reaches
    TargetCOMID <- list.CompSites$TargetCOMID
    dir_sub <- "SiteInfo"
    df_SampSummary <- data_sampSummary
    biocommlist <- biocommlist
    boo_plot <- TRUE
  }

  biocommlist <- toupper(biocommlist)

  # Write results directory ----
  out.dir <- dirname(dir_results)
  out.folders <- c(out.dir, basename(dir_results), TargetSiteID, dir_sub)

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      dir_path <- file.path(out.folders[i])
    } else {
      dir_path <- file.path(dir_path, out.folders[i])
    }
    if (dir.exists(dir_path) == FALSE) {
      dir.create(dir_path)
    }
  }

  # get sampling info (dates of samples)
  mySamps <- dplyr::filter(df_SampSummary, StationID == TargetSiteID) %>%
    dplyr::mutate(SampYear = lubridate::year(SampleDate))
  mySampsYears <- sort(as.integer(unique(mySamps$SampYear)))

  # ID RespSampleDates & Types ----
  myRespSampDates <- mySamps %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("SampleID"),
                        names_to = "SampleType", values_to = "ID") %>%
    dplyr::mutate(SampleType = sub("SampleID$", "", SampleType)) %>%
    dplyr::select(SampleDate, SampleType)
  myRespSampDates <- myRespSampDates[myRespSampDates$SampleType %in% biocommlist, ]
  myRespSampDates$yLoc <- NA_real_

  allRespSampTypes <- unique(myRespSampDates$SampleType)

  # for (t in seq_along(allRespSampTypes)) {
  #   type <- allRespSampTypes[t]
  #   myRespSampDates <- myRespSampDates %>%
  #     dplyr::mutate(yLoc = ifelse(SampleType == type, -5*t, yLoc))
  # to get better timelines, this is what you need to alter and probably
  # put it after reading in the date range
  # }

  if (!is.null(df_WSData)) {
    # Get background data from df_WSData; use COMID to select single reach
    data_compbkgd <- dplyr::filter(df_WSData, COMID %in% comp.reaches)
    data_sitebkgdata <- dplyr::filter(data_compbkgd, COMID == TargetCOMID)
    naVars.site <- unique(data_sitebkgdata$StreamCatVar[is.na(data_sitebkgdata$WatershedValue)])
    data_sitebkgdata <- dplyr::filter(data_sitebkgdata, !is.na(WatershedValue))
    vars.site <- unique(data_sitebkgdata$StreamCatVar[!is.na(data_sitebkgdata$WatershedValue)])

    if (length(naVars.site) > 0) { # if any NA values, then missing data for site
      # Missing one or more values in StreamCat for the target reach.
      naVars.site <- paste(naVars.site, collapse = "; ")
      gapcomment <- paste0("Missing background data for site ",
                           TargetSiteID, "on reach with COMID = ", TargetCOMID)
      gaps <- cbind.data.frame("getSiteInfo", "Background Data", naVars.site,
                               gapcomment)
      colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
                  row.names = FALSE, sep = "\t")
    }

    if (length(vars.site) > 0) { # Wastershed-scale stressor data exists

      # If EPA wants to use all comparator reaches, make sure to set
      # useAllCompReaches to TRUE in the CASTool_Metadata.xlsx file.
      if (useAllCompReaches) { # use all comparator reaches, even those not having sites
        if (useBC == TRUE) {
          # If useAllCompReaches == T & useBC == T, this means use all
          # outside-the-case reaches because filtering happens on a site basis,
          # meaning that inside the case is by definition reaches having sites.
          outcaseID <- mySiteInfo$OutcaseCol
          data_compbkgd <- df_WSData[df_WSData$ClusterID == outcaseID, ]
        } else { # useBC == FALSE; cluster ID is the inside the case ID
          incaseID <- mySiteInfo$IncaseCol # this represents cluster ID
          data_compbkgd <- df_WSData[df_WSData$ClusterID == incaseID, ]
        }
        str_caption <- paste0("Target reach (", TargetCOMID, ") relative to ",
                              "distribution of values for all comparator reaches")
      } else { # use only comparator reaches having sites
        data_compbkgd <- df_WSData[df_WSData$COMID %in% comp.reaches, ]
        str_caption <- paste0("Target reach (", TargetCOMID, ") relative to ",
                              "distribution of values for all comparator sites' reaches")
      }

      ## Draw boxplots ----
      # Prepare boxplot main elements
      str_title <- paste0(TargetSiteID, ": Site watershed-scale stressors")

      for (i in seq_along(vars.site)) { # StreamCatVar (no year--Metric includes year)
        print(paste0("Prepping ", vars.site[i]))
        plotvar <- vars.site[i]
        fn.bkgplot <- file.path(dir_path, paste0(TargetSiteID, "_WSstress_",
                                                 plotvar, ".png"))
        if (plotvar == "wsareasqkm") {
          str_sub <- "Watershed area, km2"
        } else {
          str_sub <- unique(df_WSInfo$Label[df_WSInfo$StreamCatVar == plotvar])
        }

        df.plot.comp <- dplyr::filter(data_compbkgd, StreamCatVar == plotvar) %>%
          dplyr::filter(!is.na(WatershedValue))
        dataYears <- sort(unique(df.plot.comp$Year))

        if (length(dataYears) > 0) { # Plot data with years

          xmin <- min(df.plot.comp$Year)
          numTypes <- length(allRespSampTypes)
          ymax <- max(df.plot.comp$WatershedValue)
          ymin <- min(df.plot.comp$WatershedValue)

          for (t in 1:numTypes) {
            type <- allRespSampTypes[t]

            if(ymin >= 0){
              myRespSampDates <- myRespSampDates %>%
                dplyr::mutate(yLoc = ifelse(SampleType == type,
                                            -0.05 * ymax * t,
                                            yLoc))
            } else{
              myRespSampDates <- myRespSampDates %>%
                dplyr::mutate(yLoc = ifelse(SampleType == type,
                                            ymin + (-0.05 * ymax * t),
                                            yLoc))
            }

          }

          # Boxplots with years ----
          p.box <- ggplot2::ggplot(data = df.plot.comp,
                                   ggplot2::aes(x = Year, y = WatershedValue,
                                                group = Year)) +
            ggplot2::geom_boxplot(outliers = FALSE, na.rm = TRUE,
                                  staplewidth = 0.5, linewidth = 0.1) +
            ggplot2::geom_jitter(data = df.plot.comp, width = 0.1, height = 0,
                                 ggplot2::aes(x = Year, y = WatershedValue),
                                 size = 0.25, na.rm = TRUE, color = "cyan4") +
            ggplot2::scale_x_continuous( #limits = c(xmin, xmax),
                                        breaks = scales::breaks_width(1)) +
            ggplot2::labs(title = str_title, subtitle = str_sub) +
            ggplot2::ylab("Watershed Value") +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5),
                           plot.caption = ggplot2::element_text(size = 5)) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black",
                                                               size = 6,
                                                               angle = 90,
                                                               vjust = 0.6,
                                                               hjust = 0.5),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 6),
                           axis.title.x = ggplot2::element_blank(),
                           axis.title.y = ggplot2::element_text(color = "black",
                                                                size = 8),
                           legend.position = "none")

          p.box <- p.box +
            ggplot2::geom_point(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                                ggplot2::aes(x = Year, y = WatershedValue, group = Year),
                                color = "red", shape = 17) +
            ggplot2::geom_label(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                               ggplot2::aes(x = Year, y = WatershedValue,
                                            group = Year,
                                            label = round(WatershedValue,
                                                            digits = 1)),
                               size = 2.1, color = "red", nudge_x = 0,
                               nudge_y = 0.02 * ymax) #0.75)

          p.boxtime <- p.box +
            ggplot2::geom_point(data = myRespSampDates, inherit.aes = FALSE,
                                ggplot2::aes(x = lubridate::decimal_date(SampleDate),
                                             y = yLoc), size = 1) #+
          for (l in 1:numTypes) {
            p.boxtime <- p.boxtime +
              ggplot2::geom_hline(color = "black",
                                  yintercept = ifelse(ymin >= 0,
                                                      (-0.05 * ymax * l),
                                                      (ymin + (-0.05 * ymax * l))),
                                  linewidth = 0.2) +
              ggplot2::geom_text(x = xmin,
                                 y = ifelse(ymin >= 0,
                                            (-0.05 * ymax * l),
                                            (ymin + (-0.05 * ymax * l))) ,
                                 label = allRespSampTypes[l],
                                 size = 2)
          }

          if (boo_plot) {
            ggplot2::ggsave(fn.bkgplot, p.boxtime, dpi = plot_dpi, width = plot_W,
                            height = 1.5 * plot_H, units = plot_units)
          }## IF ~ boo_plot ~ END

        } else { # no years to consider

          ymax <- max(df.plot.comp$WatershedValue)

          p.box <- ggplot2::ggplot(data = df.plot.comp,
                                   ggplot2::aes(x = StreamCatVar, y = WatershedValue)) +
            ggplot2::geom_boxplot(outliers = FALSE, na.rm = TRUE,
                                  staplewidth = 0.5, linewidth = 0.1) +
            ggplot2::geom_jitter(data = df.plot.comp, width = 0.1, height = 0,
                                 ggplot2::aes(x = StreamCatVar, y = WatershedValue),
                                 size = 0.25, na.rm = TRUE, color = "cyan4") +
            ggplot2::labs(title = str_title, subtitle = str_sub,
                          caption = str_caption) +
            ggplot2::ylab("Watershed Value")

          p.box <- p.box +
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5),
                           plot.caption = ggplot2::element_text(size = 5)) +
            ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 6),
                           axis.title.x = ggplot2::element_blank(),
                           axis.title.y = ggplot2::element_text(color = "black",
                                                                size = 8),
                           legend.position = "none")

          p.box <- p.box +
            ggplot2::geom_point(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                                ggplot2::aes(x = StreamCatVar, y = WatershedValue),
                                color = "red", shape = 17) +
            ggplot2::geom_label(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                               ggplot2::aes(x = StreamCatVar, y = WatershedValue,
                                            label = round(WatershedValue,
                                                            digits = 1)),
                               size = 2.3, color = "red", nudge_y = ymax * 0.02,
                               nudge_x = 0)
          if(boo_plot){
            ggplot2::ggsave(fn.bkgplot, p.box, dpi = plot_dpi, width = plot_W,
                            height = plot_H, units = plot_units)
          }## IF ~ boo_plot ~ END
        }## If/else for graphs ends
      }## for loop over variables ends
    } # End background data portion
  } # End WS data check
} # End FUN
