#  Copyright statement here
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Get StreamCat data
#' @description
#' Returns StreamCat watershed summary data for reaches in the specified state
#' and, if provided in the cluster data file, in the 300 m buffer around the
#' state outline.
#'
#' @details Uses API to query StreamCat data for available parameter names,
#' which are matched to previously identified anthropogenically-influenced
#' parameters. Next uses the API to aquire data corresponding to each of these
#' parameters within each reach's watershed.
#'
#' Uses the library dplyr, readxl, StreamCatTools, stringr, and tidyr.
#'
#' @param localdir Directory containing CASTool metadata
#' @param dir_data Directory containing cluster data
#' @param region x
#' @param state Two letter state abbreviation of region of interest
#'
#' Will need to change this function if extrapolating to a region beyond a state
#' @examples
#' # None at this time 
#' @return Writes StreamCat_data_region.csv and StreamCat_stressor-info_region.csv
#' to the local directory if the download is successful. If unsuccessful returns NULL.

getStreamCatData <- function(localdir = "",
                             dir_data = "",
                             region = "",
                             state = ""){

  # Global Bindings
  Variable <- StreamCatVar <- Year <- ReachLoc <- COMID <- ClusterID <- 
      Label <- NULL
  
  # define pipe
  `%>%` <- dplyr::`%>%`
  
  tryCatch({
    fn.CASTmeta   <- file.path(localdir, "CASTool_Metadata.xlsx")
    data_CASTmeta <- readxl::read_excel(fn.CASTmeta, na = "", trim_ws = TRUE)
    data_CASTmeta <- data_CASTmeta %>%
      dplyr::select(Variable, dplyr::all_of(region)) %>%
      tidyr::pivot_wider(names_from = Variable, values_from = dplyr::all_of(region))

    fn.SC.WSvars  <- file.path(localdir, "SelectedStreamCatStressors.csv")
    fn.cluster <- file.path(dir_data, dplyr::select(data_CASTmeta, fn.cluster))

    # Read in SC parameters
    data_bkginfo <- readCASToolData(fn = fn.SC.WSvars, NAs = c("", "na", "NA", "N/A"))

    # Read in cluster assignments
    data_cluster <- readCASToolData(fn = fn.cluster, NAs = c("", "na", "NA", "N/A"))

    # Get all SC parameter names
    SCmetrics <- StreamCatTools::sc_get_params(param = 'metric_names')
    data_stressorinfoWS <- data.frame(SCmetrics)

    data_stressorinfoWS <- data_stressorinfoWS %>%
      dplyr::mutate(Year = suppressWarnings(
        dplyr::case_when(
          SCmetrics == "popden2010" ~ NA_integer_,
          grepl("^\\w*\\d{4}$", SCmetrics) ~ as.integer(sub("^\\w*(\\d{4})$",
                                                            "\\1", SCmetrics)),
          TRUE ~ NA_integer_)))

    data_stressorinfoWS <- data_stressorinfoWS %>%
      dplyr::mutate(StreamCatVar = suppressWarnings(
        dplyr::case_when(SCmetrics == "popden2010" ~ "popden2010",
                         grepl("\\d{4}$", SCmetrics) ~ sub("\\d{4}", "####", SCmetrics),
                         TRUE ~ SCmetrics))) %>%
      dplyr::select(SCmetrics, StreamCatVar, Year)

    # Match metric names in StreamCat to metric names desired
    data_stressorinfoWS <- merge(data_stressorinfoWS, data_bkginfo,
                                 by.x = "StreamCatVar", by.y = "variable",
                                 all.y = TRUE)
    data_stressorinfoWS <- data_stressorinfoWS %>%
      dplyr::mutate(StreamCatVar = sub("####", "", StreamCatVar),
                    Label = dplyr::case_when(grepl("^NABD", description) ~ description,
                                             grepl("^NPDES", description) ~ description,
                                             TRUE ~ stringr::str_to_sentence(description)))



    # Obtain actual data
    SCmetrics_nni <- data_stressorinfoWS %>%
      dplyr::filter(stringr::str_detect(SCmetrics, "_")) %>%
      dplyr::pull(SCmetrics) %>%
      paste(collapse = ",")
    SCmetrics_other <- data_stressorinfoWS %>%
      dplyr::filter(stringr::str_detect(SCmetrics, "_")== FALSE) %>%
      dplyr::pull(SCmetrics) %>%
      paste(collapse = ",")

    message("Downloading StreamCat data")

    data_stressorWS_nni <- StreamCatTools::sc_get_data(metric = SCmetrics_nni,
                                                       aoi = 'watershed',
                                                       state = state)
    data_stressorWS_other <- StreamCatTools::sc_get_data(metric = SCmetrics_other,
                                                         aoi = 'watershed',
                                                         state = state)

    data_stressorWS <- dplyr::full_join(data_stressorWS_nni,
                                        data_stressorWS_other,
                                        by = "comid")

    if("ReachLoc" %in% names(data_cluster)){
      buffer <- data_cluster %>%
        dplyr::filter(ReachLoc == "Buffer") %>%
        dplyr::pull(COMID) %>%
        paste(collapse = ",")

      buffer_nni <- StreamCatTools::sc_get_data(metric = SCmetrics_nni,
                                                aoi = 'watershed',
                                                comid = buffer)

      buffer_other <- StreamCatTools::sc_get_data(metric = SCmetrics_other,
                                                  aoi = 'watershed',
                                                  comid = buffer)
      buffer_WS <- dplyr::full_join(buffer_nni, buffer_other, by = "comid")

      data_stressorWS <- data_stressorWS %>%
        dplyr::bind_rows(buffer_WS)

      data_cluster <- data_cluster %>%
        dplyr::select(-ReachLoc)
    }


    data_stressorWS <- merge(data_cluster, data_stressorWS,
                             by.x = "COMID", by.y = "comid",
                             all.x = TRUE)
    # need to add by.x and by.y because of names returned by StreamCatTools


    data_stressorWS <- data_stressorWS %>%
      dplyr::rename_with(~sub("ws$", "", .)) %>%
      tidyr::pivot_longer(cols = !c(COMID, ClusterID),
                          names_to = "StreamCatVar",
                          values_to = "WatershedValue")

    data_stressorWS <- data_stressorWS %>%
      dplyr::mutate(Year = suppressWarnings(dplyr::case_when(
        StreamCatVar == "popden2010" ~ NA_integer_,
        grepl("^\\w*\\d{4}$", StreamCatVar) ~
          as.integer(sub("^\\w*(\\d{4})$", "\\1", StreamCatVar)),
        TRUE ~ NA_integer_)),
        StreamCatVar = suppressWarnings(dplyr::case_when(
          StreamCatVar == "popden2010" ~ "popden2010",
          grepl("\\d{4}$", StreamCatVar) ~ sub("\\d{4}", "", StreamCatVar),
          TRUE ~ StreamCatVar)))

    # Trim data_stressorinfoWS to StreamCatVar and Label only
    data_stressorinfoWS <- dplyr::distinct(data_stressorinfoWS, StreamCatVar,
                                           SCmetrics, Year, Label)

    utils::write.csv(data_stressorWS,
              file.path(dir_data, paste0("StreamCat_data_", state, ".csv")),
              row.names = FALSE)
    utils::write.csv(data_stressorinfoWS,
              file.path(dir_data, paste0("StreamCat_stressor-info_", state, ".csv")),
              row.names = FALSE)

    if (nrow(data_stressorWS) != 0 & nrow(data_stressorinfoWS) != 0) {
      message("StreamCat data successfully written to localdir")
    }

  }, error = function(err) {
    message(paste0("Error downloading StreamCat data: ", err))
    return(NULL)
  })

}
