getMetadataParams <- function(out_dir = NULL,
                              dn_checked_sk = NULL,
                              config = NULL){

  if(is.null(config) == FALSE){
    out_dir <- config$out_dir
    dn_checked_sk <- config$dn_checked_sk
  }

  ## Load CASTool_Metadata ####
  data_CASTmeta <- readRDS(file.path(out_dir, dn_checked_sk, "CASTmetadata.rds"))

  ## Set correct classes
  out <- setNames(as.list(data_CASTmeta$Value), data_CASTmeta$Variable)

  out[c("DOlim",
        "pHlimLow",
        "pHlimHigh",
        "samplim",
        "r2_cutoff",
        "p.val_cutoff")] <- lapply(out[c("DOlim",
                                         "pHlimLow",
                                         "pHlimHigh",
                                         "samplim",
                                         "r2_cutoff",
                                         "p.val_cutoff")], as.numeric)

  out[c("helperImport",
        "exploreWSStressor",
        "calcRelAbund",
        "targetSampleLabels",
        "removeOutliers",
        "useAllCompReaches")] <- lapply(out[c("helperImport",
                                              "exploreWSStressor",
                                              "calcRelAbund",
                                              "targetSampleLabels",
                                              "removeOutliers",
                                              "useAllCompReaches")], as.logical)

  ## Make comma separated strings into lists
  out$biocommlist <- data_CASTmeta |>
    dplyr::filter(Variable == "biocommlist") |>
    dplyr::pull(Value) |>
    tolower() |>
    stringr::str_split(", |,") |>
    unlist()

  out$lagdays <- data_CASTmeta |>
    dplyr::filter(Variable == "lagdays") |>
    dplyr::pull(Value) |>
    stringr::str_split(",") |>
    unlist() |>
    stringr::str_trim() |>
    as.integer()

  ## Read loaded.rds ####
  ### Describes which input files have been loaded
  data_loaded <- readRDS(file.path(out_dir, dn_checked_sk, "loaded.rds"))
  out$loaded <- as.character(data_loaded$Object)

  ## Describe whether measured and/or modeled data loaded
  if(stringr::str_detect(out$loaded, "chem") |> any()){
    out$boo_meas <- TRUE
  } else{
    out$boo_meas <- FALSE
  }

  if(stringr::str_detect(out$loaded, "model") |> any()){
    out$boo_model <- TRUE
  } else{
    out$boo_model <- FALSE
  }

  return(out)
}

# ## Load CASTool_Metadata ####
# data_CASTmeta <- readRDS(file.path(out.dir, dn_checked_sk, "CASTmetadata.rds"))
# data_CASTmeta <- data_CASTmeta |>
#   tidyr::pivot_wider(names_from = Variable, values_from = Value)
#
# ## Read loaded.rds ####
# ### Describes which input files have been loaded
# data_loaded <- readRDS(file.path(out.dir, dn_checked_sk, "loaded.rds"))
# loaded      <- as.character(data_loaded$Object)
#
# ## Set up booleans ####
# ### Helper import boolean
# helperImport <- data_CASTmeta |> dplyr::pull(helperImport) |>  as.logical()
# helperImport <- ifelse(is.na(helperImport), FALSE, helperImport)
#
# ### WS boolean
# boo.WS <- data_CASTmeta |> dplyr::pull(exploreWSStressor) |> as.logical()
# boo.WS <- ifelse(is.na(boo.WS), FALSE, boo.WS)
#
# ### Target sample label boolean
# targetSampleLabels <- data_CASTmeta |> dplyr::pull(targetSampleLabels)|>  as.logical()
# targetSampleLabels <- ifelse(is.na(targetSampleLabels), FALSE, targetSampleLabels)
#
# boo.meas  <- FALSE
# boo.model <- FALSE
#
# # LCN could be condensed
# # Sets up booleans for measured and modeled data and creates meta.meas/mod and data.meas/mod with object names for measured and modeled (meta)data
# for (l in seq_along(loaded)) {
#   msg <- paste0("booleans, ", l, "/", length(loaded))
#   message(msg)
#   object <- loaded[l]
#   if (grepl("chem", object) == TRUE)  {
#     boo.meas  <- TRUE
#     if (grepl("Info", object) == TRUE) {
#       meta.meas <- object
#     } else {
#       data.meas <- object
#     }
#   }
#   if (grepl("model", object) == TRUE) {
#     boo.model <- TRUE
#     if (grepl("Info", object) == TRUE) {
#       meta.mod <- object
#     } else {
#       data.mod <- object
#     }
#   }
# }
# rm(l, object, data_loaded)
#
# ## Get variables ####
# ### Response data ####
# biocommlist <- data_CASTmeta |> dplyr::pull(biocommlist) |> stringr::str_split(", |,") |> unlist()
#
# # Define the metric to use as the index for each biocommunity
# for (b in seq_along(biocommlist)) {
#   bio <- tolower(biocommlist[b])
#   calcRelAbund       <- as.logical(dplyr::select(data_CASTmeta, calcRelAbund))
#   if (bio == "bmi") {
#     bmiIndexGp       <- data_CASTmeta |> dplyr::pull(bmiIndexGp) |> stringr::str_split(", |,") |> unlist()
#   }
#   if (bio == "alg") {
#     algIndexGp       <- data_CASTmeta |> dplyr::pull(algIndexGp) |> stringr::str_split(", |,") |> unlist()
#   }
#   if (bio == "fish") {
#     fishIndexGp       <- data_CASTmeta |> dplyr::pull(fishIndexGp) |> stringr::str_split(", |,") |> unlist()
#   }
# }## FOR ~ b
#
# ### Stressor data ####
# removeOutliers  <- as.logical(dplyr::select(data_CASTmeta, removeOutliers))
# samplim         <- as.integer(dplyr::select(data_CASTmeta, samplim))
# r2_cutoff       <- as.numeric(dplyr::select(data_CASTmeta, r2_cutoff))
# p.val_cutoff    <- as.numeric(dplyr::select(data_CASTmeta, p.val_cutoff))
#
# if (boo.meas) {
#   DOlim         <- as.numeric(dplyr::select(data_CASTmeta, DOlim))
#   pHlimLow      <- as.numeric(dplyr::select(data_CASTmeta, pHlimLow))
#   pHlimHigh     <- as.numeric(dplyr::select(data_CASTmeta, pHlimHigh))
#
#   lagdays <- dplyr::pull(data_CASTmeta, lagdays) |>
#     stringr::str_split(",") |>
#     unlist() |>
#     stringr::str_trim() |>
#     as.integer()
#
# }## IF ~ boo.meas
#
# useAllCompReaches  <- as.logical(dplyr::select(data_CASTmeta, useAllCompReaches))
#
# ### Site variables ####
# # if (cfg$boo_shiny) {
# #   # should be single entries so should be ok
# datum          <- as.character(dplyr::pull(data_CASTmeta, datum))
# outcaseColName <- as.character(dplyr::pull(data_CASTmeta, outcaseColName))
# outcaseLabel   <- as.character(dplyr::pull(data_CASTmeta, outcaseLabel))
# incaseColName  <- as.character(dplyr::pull(data_CASTmeta, incaseColName))
# incaseLabel    <- as.character(dplyr::pull(data_CASTmeta, incaseLabel))
# # } else {
# #   datum          <- as.character(dplyr::select(data_CASTmeta, datum))
# #   outcaseColName <- as.character(dplyr::select(data_CASTmeta, outcaseColName))
# #   outcaseLabel   <- as.character(dplyr::select(data_CASTmeta, outcaseLabel))
# #   incaseColName  <- as.character(dplyr::select(data_CASTmeta, incaseColName))
# #   incaseLabel    <- as.character(dplyr::select(data_CASTmeta, incaseLabel))
# # }## IF ~ cfg$boo_shiny
#
# rm(b, bio)
