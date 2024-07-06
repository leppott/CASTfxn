#  Copyright 2024 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#' @title Get Available Data
#'
#' @description Identifies which data are available for the target site.
#'
#' @details Using the entire possible set of data types, which are differentiated by their sample identifiers, this function determines which sample types are available for the target site. Sample ID column names are required to end with "SampID".
#'
#' Uses the library dplyr
#'
#' @param TargetSiteID Site ID for the site being evaluated
#' @param df_SampSummary dataframe containing sample IDs for samples collected
#'                       at the target site, organized by sample date (rows) and
#'                       type (columns)
#' @param measStressSamps vector of all measured stressor sample types, each must
#'                        end in "SampID".
#' @param modStressSamps vector of all modeled stressor sample types, each must
#'                       end in "SampID". Default = NULL.
#' @param chemStressSamps vector of all field or lab chemistry sample types, each
#'                        must end in "SampID".
#' @param habStressSamps vector of all habitat sample types, each must end in
#'                       "SampID". Default = NULL.
#' @param bmiRespSamps vector of all BMI response sample types, each must end
#'                     in "SampID".
#' @param algRespSamps vector of all Algae response sample types, each must end
#'                     in "SampID". Default = NULL.
#' @param fishRespSamps vector of all Fish response sample types, each must end
#'                      in "SampID". Default = NULL.
#' @param dir_results Directory containing all results. Default is file.path(getwd(),"Results")
#'
#' @return A list containing five boolean values 1) useBMI, 2) useAlg, 3) useFish,
#'         4) noStressors, and 5) noResponses.
#'
#' @keywords internal
#'
#' @export
getAvailableDataTypes <- function(TargetSiteID
                                  , df_SampSummary
                                  , measStressSamps
                                  , modStressSamps = NULL
                                  , chemStressSamps
                                  , habStressSamps = NULL
                                  , bmiRespSamps
                                  , algRespSamps = NULL
                                  , fishRespSamps = NULL
                                  , dir_results = file.path(getwd(), "Results")
                                  ) {##FUNCTION.START

  boo.DEBUG <- FALSE

  if(boo.DEBUG) {
    TargetSiteID = TargetSiteID
    df_SampSummary = data_sampSummary
    measStressSamps = meas.stress
    modStressSamps = mod.stress
    chemStressSamps = chem.stress
    habStressSamps = hab.stress
    bmiRespSamps = bmiResp
    algRespSamps = algResp
    fishRespSamps = fishResp
    dir_results = dir_results
  }

  # define pipe and not_all_na function
  `%>%` <- dplyr::`%>%`
  not_all_na <- function(x) {!all(is.na(x))}

  # Check for directory, if not existing, create
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID)) == TRUE
         , dir.create(file.path(dir_results, TargetSiteID))
         , FALSE)

  fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
  fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)

  avail.data <- data_sampSummary %>%
    dplyr::filter(StationID_Master == TargetSiteID) %>%
    dplyr::select(ends_with("SampID")) %>%
    dplyr::select_if(not_all_na)
  samptypes <- names(avail.data)

  if (!is.null(measStressSamps)) {
    if (any(samptypes %in% measStressSamps)) { # Either chem or phab samps exist
      useMeasStress = TRUE
      if (!any(samptypes %in% chemStressSamps)) {         # No chem samps
        gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0
                                            , "No chemistry stressors available.")
        colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")

        gap.phab.stress <- cbind.data.frame("general", "HabStress", 1
                                            , "Habitat stressors available.")
        colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")

      } else if (!any(samptypes %in% habStressSamps)) {   # No habitat samps
        gap.phab.stress <- cbind.data.frame("general", "HabStress", 0
                                            , "No habitat stressors available.")
        colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")

        gap.chem.stress <- cbind.data.frame("general", "ChemStress", 1
                                            , "Chemistry stressors available.")
        colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
      } else {
        gap.phab.stress <- cbind.data.frame("general", "HabStress", 1
                                            , "Habitat stressors available.")
        colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")

        gap.chem.stress <- cbind.data.frame("general", "ChemStress", 1
                                            , "Chemistry stressors available.")
        colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")
      }
      df_allStress <- data_chemRaw
    } else {# No measured stressors at all
      useMeasStress <- FALSE
      gap.chem.stress <- cbind.data.frame("general", "ChemStress", 0
                                          , "No chemistry stressors available.")
      colnames(gap.chem.stress) <- c("fxnname", "condition", "result", "comment")

      gap.phab.stress <- cbind.data.frame("general", "HabStress", 0
                                          , "No habitat stressors available.")
      colnames(gap.phab.stress) <- c("fxnname", "condition", "result", "comment")
    } ### End If statement for measured stressors
  }

  if (!is.null(modStressSamps)) {
    if (any(samptypes %in% modStressSamps)) {
      useModStress <- TRUE
      gap.mod.stress <- cbind.data.frame("general", "useModStress", 1
                                         , "Modeled stressors available.")
      colnames(gap.mod.stress) <- c("fxnname", "condition", "result", "comment")
      if (exists("df_allStress") == TRUE) {
        df_allStress <- rbind(df_allStress, data_modelRaw)
      } else {
        df_allStress <- data_modelRaw
      }
    } else {
      useModStress <- FALSE
      gap.mod.stress <- cbind.data.frame("general", "useModStress", 0
                                         , "No modeled stressors available.")
      colnames(gap.mod.stress) <- c("fxnname", "condition", "result", "comment")
    } ### End If statement for modeled stressors
  }

  if (!is.null(bmiRespSamps)) {
    if (any(samptypes == bmiRespSamps)) {
      useBMI <- TRUE
      gap.bmi.rsp <- cbind.data.frame("general", "useBMI", 1, "BMI responses available.")
      colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
    }
  } else{
    useBMI <- FALSE
    gap.bmi.rsp <- cbind.data.frame("general", "useBMI", 0, "No BMI responses available.")
    colnames(gap.bmi.rsp) <- c("fxnname", "condition", "result", "comment")
  } ### End If statement for benthic macroinvertebrate responses

  if (!is.null(algRespSamps)) {
    if (any(samptypes == algRespSamps)) {
      useAlg <- TRUE
      gap.alg.rsp <- cbind.data.frame("general", "useALG", 1, "Algae responses available.")
      colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
    }
  } else {
    useAlg <- FALSE
    gap.alg.rsp <- cbind.data.frame("general", "useALG", 0, "No algae responses available.")
    colnames(gap.alg.rsp) <- c("fxnname", "condition", "result", "comment")
  } ### End If statement for measured stressorsalgal responses

  if (!is.null(fishRespSamps)) {
    if (any(samptypes == fishRespSamps)) {
      useFish <- TRUE
      gap.fish.rsp <- cbind.data.frame("general", "useFISH", 1, "Fish responses available.")
      colnames(gap.fish.rsp) <- c("fxnname", "condition", "result", "comment")
    }
  } else {
    useFish <- FALSE
    gap.fish.rsp <- cbind.data.frame("general", "useFISH", 0, "No fish responses available.")
    colnames(gap.fish.rsp) <- c("fxnname", "condition", "result", "comment")
  } ### End If statement for measured stressorsalgal responses

  gaps <- rbind.data.frame(gap.chem.stress, gap.phab.stress, gap.mod.stress
                           , gap.bmi.rsp, gap.alg.rsp)
  if (file.exists(fn.gaps)) {
    write.table(gaps, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
  } else {
    write.table(gaps, fn.gaps, append = FALSE, col.names = TRUE
                , row.names = FALSE, sep = "\t")
  }

  # Clean up unnecessary objects
  rm(avail.data, samptypes)

  noStressors <- FALSE
  noResponses <- FALSE

  if ((useMeasStress == FALSE) & (useModStress == FALSE)) {
    # No stressor data available
    gap.stress <- cbind.data.frame("general", "Stressors", 0
                                   , "No stressor data available.")
    colnames(gap.stress) <- c("fxnname", "condition", "result"
                              , "comment")
    write.table(gap.stress, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
    noStressors <- TRUE
  }

  if ((useAlg == FALSE) & (useBMI == FALSE) & (useFish == FALSE)) {
    # No response data available
    gap.resp <- cbind.data.frame("general", "Responses", 0
                                 , "No response data available.")
    colnames(gap.resp) <- c("fxnname", "condition", "result"
                            , "comment")
    write.table(gap.resp, fn.gaps, append = TRUE, col.names = FALSE
                , row.names = FALSE, sep = "\t")
    noResponses <- TRUE
  }

  myAvailData <- list(useBMI = useBMI
                      , useAlg = useAlg
                      , useFish = useFish
                      , noStressors = noStressors
                      , noResponses = noResponses)

  return(myAvailData)

}
