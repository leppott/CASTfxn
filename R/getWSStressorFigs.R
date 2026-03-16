#' @title Watershed Stressor Figures
#'
#' @description Get watershed stressor figures
#'
#' @details Create watershed stressor figures
#'
#' @param TargetSiteID Site ID
#' @param df_WSData xyz
#' @param df_WSInfo xyz
#' @param df_Sites xyz
#' @param comp.reaches xyz
#' @param TargetCOMID xyz
#' @param useAllCompReaches xyz
#' @param useBC xyz
#' @param dir_sub Subdirectory for outputs from this function.
#' Default = "SiteInfo"
#' @param df_SampSummary default = data_sampSummary
#' @param biocommlist xyz
#' @param boo_plot Flag declaring whether the plots should be saved or not.
#' Default = TRUE.
#' @param plotdpi standardized plot dpi. Default = 600
#' @param plotH standardized plot height. Default = 6
#' @param plotW standardized plot width. Default = 8
#' @param plotunits units for plot height and width. Default = "in"
#' @param dir_results xyz
#'
#' @return xyz
#'
#' @examples
#' # None at this time
#'
#' @export
getWSStressorFigs <- function(TargetSiteID = TargetSiteID,
                              df_WSData = NULL,
                              df_WSInfo = NULL,
                              df_Sites = NULL,
                              comp.reaches = list.CompSites$comp.reaches,
                              TargetCOMID = list.CompSites$TargetCOMID,
                              useAllCompReaches = useAllCompReaches,
                              useBC = FALSE,
                              dir_sub = "SiteInfo",
                              df_SampSummary = data_sampSummary,
                              biocommlist = biocommlist,
                              boo_plot = TRUE,
                              plotdpi = 600,
                              plotH = 6,
                              plotW = 8,
                              plotunits = "in",
                              dir_results) { # FUN: Start

  # Global Bindings
  data_stressorWS <- data_stressorinfoWS <-
    StationID <- SampleDate <- SampleType <- COMID <- WatershedValue <-
    mySiteInfo <- StreamCatVar <- TargetValue <- Year <- WatershedValueMedian <-
    yLoc <- Label <- TargetAboveMedian <- list.CompSites <- data_sampSummary <-
    data_Sites <- IncaseCol <- NULL

  # Debug
  boo.debug <- FALSE
  if (boo.debug) {
    TargetSiteID      <- TargetSiteID
    df_WSData         <- data_stressorWS
    df_WSInfo         <- data_stressorinfoWS
    df_Sites          <- data_Sites
    comp.reaches      <- list.CompSites$comp.reaches
    TargetCOMID       <- list.CompSites$TargetCOMID
    useAllCompReaches <- useAllCompReaches
    useBC             <- FALSE
    dir_sub           <- "SiteInfo"
    df_SampSummary    <- data_sampSummary
    biocommlist       <- biocommlist
    boo_plot          <- TRUE
    plotdpi           <- 600
    plotH             <- 6
    plotW             <- 8
    plotunits         <- "in"
  }

  # define pipe
  `%>%` <- dplyr::`%>%`

  biocommlist <- toupper(biocommlist)

  # # Write results directory ----
  # out.dir <- dirname(dir_results)
  # out.folders <- c(out.dir, basename(dir_results), TargetSiteID, dir_sub)
  #
  # for (i in 1:length(out.folders)) {
  #   if (i == 1) {
  #     dir_path <- file.path(out.folders[i])
  #   } else {
  #     dir_path <- file.path(dir_path, out.folders[i])
  #   }
  #   if (dir.exists(dir_path) == FALSE) {
  #     dir.create(dir_path)
  #   }
  # }

  dir_path <- file.path(dir_results, TargetSiteID, dir_sub)

  # Initialize gap df
  df_gap <- data.frame(fxnname = character(), condition = character(), result = character(), comment = character())

  # LCN 9/23/25 patch fix to remove dependency on hard coded data_bmiCoOccur
  # if(useBC == TRUE){
  #   comp.reaches <- comp.reaches
  # } else{
  #   TargetSiteCluster <- df_Sites %>%
  #     dplyr::filter(StationID == TargetSiteID) %>%
  #     dplyr::pull(IncaseCol)
  #
  #   comp.reaches <- df_Sites %>%
  #     dplyr::filter(IncaseCol == TargetSiteCluster) %>%
  #     dplyr::distinct(COMID) %>%
  #     dplyr::pull(COMID)
  # }

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

  ## Initialize multi-year legend
  legend_items_yrs <- data.frame(
    # label = factor(c("Target reach", "Comparator reach", "Biological community sample"),
    #                levels= c("Target reach", "Comparator reach", "Biological community sample")),
    label = c("Target reach", "Comparator reach", "Biological community sample"),
    color = c("red", "cyan4", "black"),
    shape = c(17, 16, 16)
  )

  # Build a legend-only plot
  legend_plot_yrs <- ggplot2::ggplot(legend_items_yrs, ggplot2::aes(x = label, y = 1)) +
    ggplot2::geom_point(ggplot2::aes(color = label, shape = label), size = 1) +
    ggplot2::scale_color_manual(
      name   = "",
      values = setNames(legend_items_yrs$color, legend_items_yrs$label)
    ) +
    ggplot2::scale_shape_manual(
      name   = "",
      values = setNames(legend_items_yrs$shape, legend_items_yrs$label)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1, byrow = TRUE,  override.aes = list(size = 4), reverse = TRUE),  # horizontal
      shape = ggplot2::guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = ggplot2::element_text(size = 10),
      legend.background    = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key           = ggplot2::element_rect(fill = "white", colour = NA),
      legend.box.background= ggplot2::element_rect(fill = "white", colour = NA)
    )

  # Extract the legend as a grob
  legend_grob_yrs <- cowplot::get_legend(legend_plot_yrs)

  ## Initialize a single year legend
  # Initialize single year legend
  legend_items_single <- legend_items_yrs |>
    dplyr::filter(label != "Biological community sample")

  # Build a legend-only plot
  legend_plot_single <- ggplot2::ggplot(legend_items_single, ggplot2::aes(x = label, y = 1)) +
    ggplot2::geom_point(ggplot2::aes(color = label, shape = label), size = 1) +
    ggplot2::scale_color_manual(
      name   = "",
      values = setNames(legend_items_single$color, legend_items_single$label)
    ) +
    ggplot2::scale_shape_manual(
      name   = "",
      values = setNames(legend_items_single$shape, legend_items_single$label)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1, byrow = TRUE,  override.aes = list(size = 4), reverse = TRUE),  # horizontal
      shape = ggplot2::guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = ggplot2::element_text(size = 10),
      legend.background    = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key           = ggplot2::element_rect(fill = "white", colour = NA),
      legend.box.background= ggplot2::element_rect(fill = "white", colour = NA)
    )

  # Extract the legend as a grob
  legend_grob_single <- cowplot::get_legend(legend_plot_single)


  if (!is.null(df_WSData)) {
    # Get background data from df_WSData; use COMID to select single reach
    data_compbkgd <- dplyr::filter(df_WSData, COMID %in% comp.reaches)
    data_sitebkgdata <- dplyr::filter(data_compbkgd, COMID == TargetCOMID)
    naVars.site <- unique(data_sitebkgdata$StreamCatVar[is.na(data_sitebkgdata$WatershedValue)])
    data_sitebkgdata <- dplyr::filter(data_sitebkgdata, !is.na(WatershedValue))
    vars.site <- unique(data_sitebkgdata$StreamCatVar[!is.na(data_sitebkgdata$WatershedValue)])

    # Limit variables evaluated to only those in the WS stressor metadata
    vars.site <- intersect(vars.site, df_WSInfo$StreamCatVar)
    naVars.site <- intersect(naVars.site, df_WSInfo$StreamCatVar)

    if (length(naVars.site) > 0) { # if any NA values, then missing data for site
      # Missing one or more values in StreamCat for the target reach.

      gap.statement <- data.frame(
        fxnname = "getWSStressorFigs",
        condition = "Missing WS stressor data",
        result = naVars.site,
        comment = paste0("Missing background data for site ",
                         TargetSiteID, "on reach with COMID = ", TargetCOMID)
      )

      df_gap <- df_gap |> dplyr::bind_rows(gap.statement)

      # naVars.site <- paste(naVars.site, collapse = "; ")
      # gapcomment <- paste0("Missing background data for site ",
      #                      TargetSiteID, "on reach with COMID = ", TargetCOMID)
      # gaps <- cbind.data.frame("getSiteInfo", "Background Data", naVars.site,
      #                          gapcomment)
      # colnames(gaps) <- c("fxnname", "condition", "result", "comment")
      # fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
      # fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
      # utils::write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE,
      #             row.names = FALSE, sep = "\t")
    }

    if (length(vars.site) > 0) { # Wastershed-scale stressor data exists

      # If EPA wants to use all comparator reaches, make sure to set
      # useAllCompReaches to TRUE in the CASTool_Metadata.xlsx file.
      if (useAllCompReaches) { # use all comparator reaches, even those not having sites
        if (useBC == TRUE) {
          # If useAllCompReaches == T & useBC == T, this means use all
          # outside-the-case reaches because filtering happens on a site basis,
          # meaning that inside the case is by definition reaches having sites.
          # Will not work 1/15/26
          outcaseID <- mySiteInfo$OutcaseCol
          data_compbkgd <- df_WSData[df_WSData$ClusterID == outcaseID, ]
        } else { # useBC == FALSE; cluster ID is the inside the case ID
          # incaseID <- mySiteInfo$IncaseCol # this represents cluster ID
          # data_compbkgd <- df_WSData[df_WSData$ClusterID == incaseID, ]

          data_compbkgd <- df_WSData[df_WSData$COMID %in% comp.reaches,]
        }
        str_sub <- paste0("Watershed summaries: target (", TargetCOMID, ") and all comparator reaches")
      } else { # use only comparator reaches having sites
          comp.reaches_sites <- df_Sites %>%
            dplyr::filter(COMID %in% comp.reaches) %>%
            dplyr::pull(COMID)

        data_compbkgd <- df_WSData[df_WSData$COMID %in% comp.reaches_sites, ]
        str_sub <- paste0("Watershed summaries: target (", TargetCOMID, ") and comparator reaches with sampled sites")
      }

      ## Draw boxplots ----
      # Prepare boxplot main elements


      high_stress <- NULL # LCN added 20250918

      for (i in seq_along(vars.site)) { # StreamCatVar (no year--Metric includes year)
        print(paste0("Prepping ", vars.site[i]))
        plotvar <- vars.site[i]
        fn.bkgplot <- file.path(dir_path, paste0(TargetSiteID, "_WSstress_",
                                                 plotvar, ".png"))
        # if (plotvar == "wsareasqkm") {
        #   str_sub <- "Watershed area, km2"
        # } else {
        #   str_sub <- unique(df_WSInfo$Label[df_WSInfo$StreamCatVar == plotvar])
        # }

        str_title <- paste(TargetSiteID, unique(df_WSInfo$Label[df_WSInfo$StreamCatVar == plotvar]), sep = ": ")

        df.plot.comp <- dplyr::filter(data_compbkgd, StreamCatVar == plotvar) %>%
          dplyr::filter(!is.na(WatershedValue))
        dataYears <- sort(unique(df.plot.comp$Year))

        # LCN added 20250918
        target_val <- df.plot.comp %>%
          dplyr::filter(COMID == TargetCOMID) %>%
          dplyr::rename("TargetValue" = "WatershedValue") %>%
          dplyr::select(TargetValue, Year)

        options(dplyr.summarise.inform = FALSE)

        median_temp <- df.plot.comp %>%
          dplyr::group_by(StreamCatVar, Year) %>%
          dplyr::summarize(WatershedValueMedian = stats::median(WatershedValue))  %>%
          dplyr::ungroup() %>%
          dplyr::full_join(target_val, by = "Year") %>%
          dplyr::mutate(TargetAboveMedian = TargetValue > WatershedValueMedian)

        high_stress <- high_stress %>% dplyr::bind_rows(median_temp)


        if (length(dataYears) > 0) { # Plot data with years

          xmin <- min(df.plot.comp$Year)
          numTypes <- length(allRespSampTypes)
          ymax <- max(df.plot.comp$WatershedValue)
          ymin <- min(df.plot.comp$WatershedValue)

          for (t in 1:numTypes) {
            type <- allRespSampTypes[t]

            if(ymin == 0 & ymax == 0){
              myRespSampDates <- myRespSampDates %>%
                dplyr::mutate(yLoc = ifelse(SampleType == type,
                                            -0.05 * t,
                                            yLoc))
            }
            else if(ymin >= 0){
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
                                 size = 1, na.rm = TRUE, color = "cyan4") +
            ggplot2::scale_x_continuous( #limits = c(xmin, xmax),
                                        breaks = scales::breaks_width(1)) +
            ggplot2::labs(title = str_title, subtitle = str_sub) +
            ggplot2::ylab("Watershed Value") +
            ggplot2::theme_classic() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                              size = 15),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                                 size = 12),
                           plot.caption = ggplot2::element_text(size = 5)) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(color = "black",
                                                               size = 10,
                                                               angle = 90,
                                                               vjust = 0.6,
                                                               hjust = 0.5),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 10),
                           axis.title.x = ggplot2::element_blank(),
                           axis.title.y = ggplot2::element_text(color = "black",
                                                                size = 14),
                           legend.position = "none")

          p.box <- p.box +
            ggplot2::geom_point(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                                ggplot2::aes(x = Year, y = WatershedValue, group = Year),
                                color = "red", shape = 17, size = 3) #+
            # ggplot2::geom_label(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
            #                    ggplot2::aes(x = Year, y = WatershedValue,
            #                                 group = Year,
            #                                 label = round(WatershedValue,
            #                                                 digits = 1)),
            #                    size = 2.1, color = "red", nudge_x = 0,
            #                    nudge_y = 0.02 * ymax) #0.75)

          p.boxtime <- p.box +
            ggplot2::geom_point(data = myRespSampDates, inherit.aes = FALSE,
                                ggplot2::aes(x = lubridate::decimal_date(SampleDate),
                                             y = yLoc), size = 2)

          for (l in 1:numTypes) {
            p.boxtime <- p.boxtime +
              ggplot2::geom_hline(color = "black",
                                  yintercept = dplyr::case_when(
                                    (ymin == 0 & ymax == 0) ~ (-0.05 * l),
                                    ymin >= 0 ~ (-0.05 * ymax * l),
                                    TRUE ~  (ymin + (-0.05 * ymax * l))
                                  ),



                                    # ifelse(ymin >= 0,
                                    #                   (-0.05 * ymax * l),
                                    #                   (ymin + (-0.05 * ymax * l))),
                                  linewidth = 0.2) +
              ggplot2::geom_text(x = xmin,
                                 y = dplyr::case_when(
                                     (ymin == 0 & ymax == 0) ~ (-0.05 * l),
                                     ymin >= 0 ~ (-0.05 * ymax * l),
                                     TRUE ~  (ymin + (-0.05 * ymax * l))
                                   ),

                                   # ifelse(ymin >= 0,
                                   #          (-0.05 * ymax * l),
                                   #          (ymin + (-0.05 * ymax * l))) ,
                                 label = allRespSampTypes[l],
                                 size = 5)
          }

          p.boxtime <- cowplot::ggdraw() +
            cowplot::draw_plot(cowplot::plot_grid(
              p.boxtime,
              legend_grob_yrs,
              ncol = 1,
              rel_heights = c(1, 0.05)
            )) +
            ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))


          if (boo_plot) {
            ggplot2::ggsave(filename = fn.bkgplot,
                            plot = p.boxtime,
                            dpi = plotdpi,
                            width = plotW,
                            height = 1.5 * plotH,
                            units = plotunits)
          }## IF ~ boo_plot ~ END

        } else { # no years to consider

          ymax <- max(df.plot.comp$WatershedValue)

          p.box <- ggplot2::ggplot(data = df.plot.comp,
                                   ggplot2::aes(x = StreamCatVar, y = WatershedValue)) +
            ggplot2::geom_boxplot(outliers = FALSE, na.rm = TRUE,
                                  staplewidth = 0.5, linewidth = 0.1) +
            ggplot2::geom_jitter(data = df.plot.comp, width = 0.1, height = 0,
                                 ggplot2::aes(x = StreamCatVar, y = WatershedValue),
                                 size = 1, na.rm = TRUE, color = "cyan4") +
            ggplot2::labs(title = str_title, subtitle = str_sub) +
            ggplot2::ylab("Watershed Value")

          p.box <- p.box +
            ggplot2::theme_classic() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                              size = 15),
                           plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                                 size = 12),
                           plot.caption = ggplot2::element_text(size = 5)) +
            ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 10),
                           axis.title.x = ggplot2::element_blank(),
                           axis.title.y = ggplot2::element_text(color = "black",
                                                                size = 12),
                           legend.position = "none")

          p.box <- p.box +
            ggplot2::geom_point(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
                                ggplot2::aes(x = StreamCatVar, y = WatershedValue),
                                color = "red", shape = 17, size = 3) #+
            # ggplot2::geom_label(data = dplyr::filter(df.plot.comp, COMID == TargetCOMID),
            #                    ggplot2::aes(x = StreamCatVar, y = WatershedValue,
            #                                 label = round(WatershedValue,
            #                                                 digits = 1)),
            #                    size = 2.3, color = "red", nudge_y = ymax * 0.02,
            #                    nudge_x = 0)

          p.box <- cowplot::ggdraw() +
            cowplot::draw_plot(cowplot::plot_grid(
              p.box,
              legend_grob_single,
              ncol = 1,
              rel_heights = c(1, 0.05)
            )) +
            ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))

          if(boo_plot){
            ggplot2::ggsave(filename = fn.bkgplot,
                            plot = p.box,
                            dpi = plotdpi,
                            width = plotW,
                            height = plotH,
                            units = plotunits)
          }## IF ~ boo_plot ~ END
        }## If/else for graphs ends
      }## for loop over variables ends
    } # End background data portion
  } # End WS data check

  high_stress <- high_stress %>%
    dplyr::left_join(df_WSInfo, by = c("StreamCatVar", "Year")) %>%
    dplyr::select(Label, Year, WatershedValueMedian, TargetValue, TargetAboveMedian) %>%
    dplyr::rename("Watershed Stressor" = "Label", "Comparator Median" = "WatershedValueMedian", "Target Site Value" = "TargetValue") %>%
    dplyr::filter(TargetAboveMedian == TRUE) %>%
    dplyr::select(-TargetAboveMedian)

  utils::write.csv(high_stress, file.path(dir_path, paste0(TargetSiteID, "WSStressHigh.csv")), row.names = FALSE)

  return(list(df_gap = df_gap))
} # End FUN
