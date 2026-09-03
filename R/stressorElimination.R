#' Title
#'
#' @param stress_info
#' @param stress_data
#'
#' @return
#' @export
#'
#' @examples
getInitialStressors <- function(stress_info,
                                stress_data){

  stressUse <- stress_info |>
    dplyr::filter(UseInStressorID == 1) |>
    dplyr::pull(StdParamName)

  initialStress <- stress_data |>
    dplyr::filter(is.na(TransfResult)==FALSE) |>
    dplyr::filter(StdParamName %in% stressUse) |>
    dplyr::distinct(StdParamName) |>
    dplyr::pull(StdParamName)

  df_initialStress <- data.frame(Stressor = initialStress) |>
    dplyr::left_join(stress_info |> dplyr::select(StdParamName, Label), by = c("Stressor" = "StdParamName"))

return(list(
  initialStress = initialStress,
  df_initialStress = df_initialStress))
}

#' Report on no stressor-response data
#'
#' @param TargetSiteID
#' @param avail_list
#'
#' @return
#' @export
#'
#' @examples
reportNoStressResponse<- function(TargetSiteID,
                                  avail_list){
  msg <- NULL
  gap.statement <- NULL

  if ((avail_list$noStressors == TRUE) | (avail_list$noResponses == TRUE)) {

    if(avail_list$noStressors == TRUE){
      cond <- "Number of stressor samples"
      msg <- paste0("No stressor data are available for ",
                    TargetSiteID)
    }
    if(avail_list$noResponses == TRUE){
      cond <- "Number of response samples"
      msg <- paste0("No response data are available for ",
                    TargetSiteID)
    }

    message(msg)

    gap.statement <- data.frame(
      fxnname = "getAvailableDataTypes",
      condition = cond,
      result = "0",
      comment = msg
    )
  }

  return(list(msg_no_sr = msg,
              gap_no_sr = gap.statement))
}

#' Report on no stressor response data for a particular site
#'
#' @param TargetSiteID
#' @param avail_list
#' @param data_bioCoOccur
#' @param meta
#' @param bioComm
#'
#' @return
#' @export
#'
#' @examples
reportNoSiteStressResponse <- function(TargetSiteID,
                                       avail_list,
                                       data_bioCoOccur,
                                       lagdays,
                                       bioComm){

  dfTarget <- NULL

  # Site-specific paired SR
  if (!(TargetSiteID %in% data_bioCoOccur$StationID)) { # Not in data_bioCoOccur
    noPairedSamps = TRUE
  } else {
    dfTarget <- dplyr::filter(data_bioCoOccur, StationID == TargetSiteID)

    if (all(is.na(dfTarget[, avail_list$siteDetectsAll]))) { # In data_bioCoOccur but all values NA
      noPairedSamps = TRUE
    } else {
      noPairedSamps = FALSE
    }
  }

  msg <- NULL
  gap.statement <- NULL

  if (noPairedSamps == TRUE) {

    msg <- paste0("No paired stressor-response samples for", TargetSiteID,
                  " for the ", bioComm, " community within specified lag days.")

    gapcomment <- paste0("No stressor samples are available for ", TargetSiteID,
                         " within ", lagdays[1], " days before, and ", lagdays[2],
                         " after the ", bioComm, " sample(s) was(were) obtained.")

    gap.statement <- data.frame(
      fxnname = "getCoOccurDataset",
      condition = paste0("Paired stressor-", bioComm, " data"),
      result = "0",
      comment = gapcomment
    )

    msg <- paste0(msg, "\nProceeding to next response community or site, ",
                  "as appropriate.")
    message(msg)

  } ### End no stressors statement

  return(list(
    msg_no_site_sr = msg,
    gap_no_site_sr = gap.statement))
}

#' Get stressors not measured at a target site
#'
#' @param stress_elim
#' @param bioComm
#'
#' @return
#' @export
#'
#' @examples
getTargetNotMeas <- function(stress_elim,
                             data_target,
                              bioComm){
  possibleStressors <- intersect(stress_elim$initialStress, names(data_target))

  targMeasStress <- data_target |>
    dplyr::select(dplyr::all_of(possibleStressors)) |>
    tidyr::pivot_longer(cols = everything()) |>
    dplyr::filter(is.na(value)==FALSE) |>
    dplyr::distinct(name) |>
    dplyr::pull(name)

  # Note stressors in dataset but not measured at target site
  targNotMeasStress <- setdiff(stress_elim$initialStress, targMeasStress)

  if(length(targNotMeasStress) > 0){
    df_notMeasElim <- data.frame(Stressor = targNotMeasStress,
                           Biocomm = bioComm,
                           Reason = "Not measured at target site")
  } else{df_notMeasElim <- NULL}

  return(list(
    targMeasStress = targMeasStress,
    df_notMeasElim = df_notMeasElim
  ))
}

#' Report on stressors with insufficent comparator samples
#'
#' @return
#' @export
#'
#' @examples
reportInsuffStress <- function(df_PairedStressResp,
                               list.CompSites,
                               bioIndexGp,
                               stress_elim,
                               samplim,
                               bioComm,
                               targMeasStress){

  # Remove any stressors with fewer than samplim comparator samples
  df_PairedStressResp.stats <- df_PairedStressResp |>
    dplyr::filter(StationID %in% list.CompSites$comp.sites) |>
    dplyr::select(StationID, IncaseCol, OutcaseCol, StressSampleDate,
                  RespSampleDate, StressSampleID, RespSampleID, BioComm,
                  all_of(bioIndexGp), Quality,
                  all_of(targMeasStress)
    ) |>
    tidyr::pivot_longer(cols = !(StationID:Quality), names_to = "Stressor",
                        values_to = "StressorValue", values_drop_na = TRUE) |>
    dplyr::group_by(Stressor) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last")

  insuffSamples <- df_PairedStressResp.stats$Stressor[which(df_PairedStressResp.stats$n < samplim)]

  df_suffElim <- data.frame()
  # Write these to data gaps file
  if (length(insuffSamples) != 0) { # changed 3/10/26 LCN was previously == 0, which is incorrect
    for (i in seq_along(insuffSamples)) {
      str <- paste(bioComm, insuffSamples[i], sep = ": ")
      msg <- paste0("Insufficient number of paired ", bioComm, " samples are available for ", str, ". This stressor will not be evaluated.")
      message(msg)

      gap.statement <- data.frame(
        fxnname = "getQualSites",
        condition = str,
        result = paste0("<", samplim),
        comment = msg)

      tempElim <- data.frame(Stressor = insuffSamples[i],
                             Biocomm = bioComm,
                             Reason = "Insufficient paired samples")

      df_suffElim <- df_suffElim |>
        dplyr::bind_rows(tempElim)
    } #End loop over stressors
  } #End if

  return(list(
    insuffSamples = insuffSamples,
    df_suffElim = df_suffElim
  ))
}
