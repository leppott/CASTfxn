#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#
#  R version 4.4.3
#
#' @title checkInputs
#'
#' @description Check input files for existence, followed by checks for required
#'  columns, types, and relationships
#'
#' @details Reviews each uploaded file against both user-input data (either via 
#' the shiny app or contained in the CASToolMetadata.xlsx file) to evaluate 1) 
#' if the file exists, 2) if it contains the required columns (which requires 
#' that they be named correctly), 3) whether datatypes meet requirements.
#'
#' If any required files do not exist, return the list of missing files and shut
#' down.  If all required files exist, proceed to internal checks of the columns
#' and data. Lastly, perform minimal joins to determine missing values.
#'
#' dir_data and dir_results should be absolute and not relative paths.
#' The function `normalizePath` can be used to convert from relative to absolute
#' path.
#'
#' @param dir.uploaded directory of input files to be checked
#' @param dir.out directory for output
#' @param fn.inputcheck path filename for the MSExcel file describing required 
#' files, columns, types, and relationships.  
#' Default is extdata/CASTool_InputCheck.xlsx
#' @param df_targets data frame with single column (TargetSiteID) of target 
#' sites
#'
#' @return A list of objects to be used in the CASTool.
#' @examples
#' # None at this time 
#' @export
checkInputs <- function(dir.uploaded,
                        dir.out,
                        fn.inputcheck = system.file("extdata", 
                                                    "CASTool_InputCheck.xlsx",
                                                    package = "CASTfxn"),
                        df_targets = NULL) {
  
  # define pipe
  `%>%` <- dplyr::`%>%`
  
  # Global Bindings
  Variable <- Value <- Type <- Uploaded <- Object <- FilePath <- 
    ObjectData <- DataType <- DataFN <- DataFileUploaded <- ObjectMetadata <- 
    MetadataType <- MetadataFN <- DataFile <- MetadataFile <- sstv.alg <- 
    ExpectedDatatypes <- errors <- ExpectedColumns <- StationID <- 
    RespSampleID <- RespSampleDate <- NumUniquePKs <- Observations <- 
    PrimaryKey <- FileOne <- FileTwo <- MetadataFileUploaded <- 
    data_bmiMetricInfo <- NULL
  
  # Set debug status ----
  debug <- FALSE
  if (debug) {
    # Uploaded files
    dir.uploaded <- "C:/Users/ann.lincoln/Documents/CASTool_DATA/UploadedData_Test"
    # Output dir
    dir.out <- "C:/Users/ann.lincoln/Documents/CASTool_DATA/WA/Results/_CheckedInputs"
    # Included functions
    dir.git <- "C:/Users/ann.lincoln/Documents/GitHub/CASTfxn/R"
    source(file.path(dir.git, "readCASToolData.R"))
  }## IF ~ debug

  # QC----
  qc_dir_uploaded <- !dir.exists(dir.uploaded)
  if (qc_dir_uploaded) {
    stop("ERROR: 'dir.uploaded' does not exist.")
  } ## IF ~ qc_dir_uploaded
  #
  qc_dir_out <- !dir.exists(dir.out)
  if (qc_dir_out) {
    stop("ERROR: 'dir.out' does not exist.")
  } ## IF ~ qc_dir_out
  #
  qc_inputcheck <- !file.exists(fn.inputcheck)
  if (qc_inputcheck) {
    stop("ERROR: 'fn.inputcheck' does not exist.")
  } ## IF ~ qc_inputcheck
  #
  qc_df_targets <- is.null(df_targets)
  if (qc_df_targets) {
    stop("ERROR: 'df_targets' does not exist.")
  } ## IF ~ qc_df_targets
  
  # dir.uploaded <- in.dir

  # Declare internal functions ----
  compare.fk.pk <- function(fk, pk) {
    if (length(intersect(fk, pk)) != length(fk)) {
      result <- setdiff(fk, pk)
    } else {
      result <- NULL
    }
  }

  compare.colnames <- function(required, actual) {
    result <- paste0("Missing columns: ", paste(setdiff(required, actual),
                                                  collapse = ", "))
    return(result)
  }

  compare.coltypes <- function(object, required.cols) {

    df.obj <- get(object)
    df.obj <- dplyr::select(df.obj, dplyr::any_of(required.cols))
    obj.cols <- colnames(df.obj)
    df.coltypechecks <- data.frame("object" = as.character(),
                                   "col" = as.character(),
                                   "errors" = as.character())

    text.cols <- c("TargetSiteID", "StationID", "StreamCatVar", "Label",
                   "StdParamName", "SourceGroup", "SSIndex", "SSTVname.bmi",
                   "SensMax.bmi", "SensMin.bmi", "SSTVname.alg", "SensMax.alg",
                   "SensMin.alg", "SSTVname.fish", "SensMax.fish", "SensMin.fish",
                   "DirIncStress", "StressSampleID", "RespSampleID", "TaxonID",
                   "MetricName", "MetricLabel", "IndexYN", "UseYN", "TrendWIncStress",
                   "InclusiveIndicator", "StreamCatVar")
    num.cols <- c("COMID", "Latitude", "Longitude", "RefSiteFlag", "ClusterID",
                  "WatershedValue", "Year", "LogTransf", "UseInStressorID",
                  "ResultValue", "NumInd", "CutoffValue", "WatershedValue", "Year")
    date.cols <- c("StressSampleDate", "RespSampleDate")

    for (c in seq_along(obj.cols)) {

      col <- obj.cols[c]
      col.class <- class(df.obj[[col]])

      if (col.class == "character" && col %in% text.cols) {
        # object col type matches
        errors <- "pass"
      } else if (col.class %in% c("integer", "numeric") && col %in% num.cols) {
        # object col type matches
        errors <- "pass"
      } else {
        # By default, either a number or a date stored as text, possibly erroneously
        if (col %in% num.cols) {
          message(paste0(col, " is expected to be numeric, but is not"))
          errors <- "column is not numeric"
        } else {
          # TODO: check this thoroughly!
          result <- tryCatch(
            {
              df.obj <- df.obj |>
                dplyr::mutate({{col}} := lubridate::parse_date_time(dplyr::.data[[col]],
                                         orders = c("ymd", "mdy", "dmy")) |> # LCN note: we may want to accomodate date times in the future, but for now we can instruct users to only include date. 
                            lubridate::date())
            },
            error = function(e) {
              message(e$message)
              return("Column cannot be converted to date.")
            }
          )

          if (is.character(result)) {
            message(paste0(col, " cannot be converted to date"))
            errors <- "column cannot be converted to date"
          } else {
            df.obj <- result
            errors <- "column can be converted to date"
          }
        }
      }
      df.coltypechecks <- rbind(df.coltypechecks, cbind(object, col, errors))
    } # end for cols

    return(df.coltypechecks)

  }

  # Create empty dataframes for results ----
  df.relational.checks <- data.frame("FileOne" = as.character(),
                                     "FileTwo" = as.character(),
                                     "JoinCols" = as.character(),
                                     "IntegrityIssues" = as.character(),
                                     "OtherConditions" = as.character())

  # Read inputcheck and CASTool_Metadata files ----
  # input_check includes variables meant to hold filenames (without paths),
  # the objects into which they are read, and whether or not they are required.
  # Regardless of whether or not the file is required, if it is provided, certain
  # columns are required. These are included in input_check, too. Relationships
  # are sort of included in input_check, but mostly that logic is in this code.
  # fn.inputcheck <- file.path(dir.uploaded, "CASTool_InputCheck.xlsx")
  #    defined in input of function
  input_check <- readxl::read_xlsx(fn.inputcheck, sheet = "data", na = "")
  fn.paired <- file.path(dir.uploaded, "CASTool_InputCheck.xlsx")
  paired_check <- readxl::read_xlsx(fn.inputcheck, sheet = "paired", na = "") |>
    tibble::rowid_to_column("UID")

  # CASTmetadata contains the expected filenames. Either this file should
  # be available in the region's data folder (if running at the command-line),
  # or it will be uploaded in zip format along with all other data files.
  fn.metadata <- file.path(dir.uploaded, "_CASTool_Metadata.xlsx")
  if (file.exists(fn.metadata)) {
    CASTmetadata <- readCASToolData(fn = fn.metadata, NAs = c("", "NA"))
    pf.cols.CASTmeta <- compare.colnames(c("Variable", "Value"),
                                         colnames(CASTmetadata))
    #if (length(pf.cols.CASTmeta) == 0) { # LCN removed 20250916
    if(length(setdiff(c("Variable", "Value"), colnames(CASTmetadata))) != 0){
      stop(pf.cols.CASTmeta)
    } else {
      CASTmetadata <- dplyr::select(CASTmetadata, Variable, Value)
    }
    rm(pf.cols.CASTmeta)
  } else {
    msg <- "_CASTool_Metadata.xlsx is not uploaded or otherwise does not exist."
    stop(msg)
  }
  input_check <- merge(input_check, CASTmetadata, by.x = "FilePath",
                       by.y = "Variable", all.x = TRUE)
  input_check$Uploaded <- 0

  # Check fatal errors ----
  ## Check definition status ----
  # All required files must be defined. If not, generate a fatal error.
  not.defined <- dplyr::filter(input_check, is.na(Value))
  if (any(grepl("^required$", not.defined$Type))) {
    fns.reqd.not.defined <- which(grepl("^required$", not.defined$Type))
    req.not.defined <- not.defined[fns.reqd.not.defined, "FilePath"]
    req.not.defined <- paste(req.not.defined, collapse = ", ")
    msg <- paste0(req.not.defined, " are not defined and likely not uploaded.")
    stop(msg)
  }

  ## Check load status ----
  loaded <- input_check[0, ]
  not.loaded <- input_check[0, ]
  input_check <- dplyr::filter(input_check, !is.na(Value)) # Minus not defined
  for (i in 1:nrow(input_check)) {
    fn <- input_check$Value[i]
    if (file.exists(file.path(dir.uploaded, fn))) {
      input_check$Uploaded[i] <- 1
      loaded <- rbind(loaded, dplyr::filter(input_check, Value == fn))
    } else {
      input_check$Uploaded[i] <- 0
      not.loaded <- rbind(not.loaded,
                          dplyr::filter(input_check, Value == fn))
    }
  }

  ## Check req'd files loaded ----
  # All required files must be loaded. If not, generate a fatal error.
  # This might be due to
  if (any(grepl("^required$", not.loaded$Type))) {
    fns.reqd.not.loaded <- which(grepl("^required$", not.loaded$Type))
    req.not.loaded <- not.loaded[fns.reqd.not.loaded, "FilePath"]
    req.not.loaded <- paste(req.not.loaded, collapse = ", ")
    msg <- paste0(req.not.loaded, " are not loaded.")
    stop(msg)
  } else if (any(grepl("^optional$", not.loaded$Type))) {
    fns.opt.not.loaded <- which(grepl("^required$", not.loaded$Type))
    opt.not.loaded <- not.loaded[fns.opt.not.loaded, "FilePath"]
    opt.not.loaded <- paste(opt.not.loaded, collapse = ", ")
    warning(paste0(opt.not.loaded, " are not loaded."))
  }

  ## Check contingent files (paired) ----
  # These are paired files (data & metadata) of which at least one pair must be both
  # defined and loaded for stressors and responses. If not, generate a fatal error.

  # ID contingent files
  cont.fns <- input_check |>
    dplyr::filter(grepl("contingent", Type))

  # ID paired data files
  paired.datafns <- cont.fns |>
    dplyr::filter(grepl("paired data", Type)) |>
    dplyr::rename(DataFileUploaded = Uploaded, ObjectData = Object,
                  DataType = Type, DataFN = Value) |>
    dplyr::select(FilePath, ObjectData, DataType, DataFN, DataFileUploaded)
  paired.datafns <- merge(paired_check, paired.datafns, by.x = "DataFile",
                          by.y = "FilePath")

  # ID paired metadata files
  paired.metafns <- cont.fns |>
    dplyr::filter(grepl("paired metadata", Type)) |>
    dplyr::rename(MetadataFileUploaded = Uploaded, ObjectMetadata = Object,
                  MetadataType = Type, MetadataFN = Value) |>
    dplyr::select(FilePath, ObjectMetadata, MetadataType, MetadataFN,
                  MetadataFileUploaded)
  paired.metafns <- merge(paired_check, paired.metafns, by.x = "MetadataFile",
                          by.y = "FilePath")

  ## Match paired data/metadata files ----
  paired.fns <- merge(paired.datafns, paired.metafns)

  paired.fns <- paired.fns |>
    dplyr::mutate(cat = sub("^fn\\.(\\w*)(\\..*$)", "\\1", DataFile),
                  desc = paste0(cat, " ", Type)) |>
    dplyr::select(ObjectData, DataFile, DataFN, DataFileUploaded, ObjectMetadata,
                  MetadataFile, MetadataFN, MetadataFileUploaded, cat, Type, desc) |>
    dplyr::mutate(TotalFiles = DataFileUploaded + MetadataFileUploaded,
                  MissingType = dplyr::case_when(TotalFiles == 2 ~ "None",
                                                 DataFileUploaded == 0 ~
                                                   paste0(desc,
                                                          " data file missing"),
                                                 MetadataFileUploaded == 0 ~
                                                   paste0(desc,
                                                          " metadata file missing"),
                                                 TRUE ~ paste0("Both ", desc,
                                                               " data and metadata files missing")))

  rm(paired.datafns, paired.metafns)

  # If any of the contingent paired data/metadata files have only one of the pair
  # loaded, generate a fatal error.
  if (any(paired.fns$TotalFiles < 2)) {
    missing.data <- as.character(paired.fns$MissingType[paired.fns$TotalFiles < 2])
    missing.data <- paste(missing.data, collapse = ", ")
    if (grepl("stressor", missing.data) | grepl("response", missing.data)) {
      msg <- paste0("Missing: ", missing.data)
      stop(msg)
    } else {
      warning(paste0("Missing: ", missing.data))
    }
  }

  # End fatal error checks ----
  rm(paired.fns, paired_check, fn, fn.inputcheck, fn.metadata, fn.paired,
     not.loaded, i, cont.fns, not.defined)

  # Read all loaded files ----
  df.reqd.obj.cols <- data.frame("Object" = as.character(),
                        "Result" = as.character())

  for (j in 1:nrow(loaded)) {
    
    object <- loaded$Object[j]
    fn <- loaded$Value[j]
    desc <- loaded$Description[j]
    assign(object, readCASToolData(file.path(dir.uploaded, fn),
                                   NAs = c("", "NA", "na", "N/A", "n/a")))
    ## Check required columns ----
    reqd.cols <- stringr::str_split_1(loaded$ReqCols[loaded$Object == object], ", ")
    if ("*SSTV" %in% reqd.cols) {
      next # These will be checked below
    }
    if ("!IncaseCol" %in% reqd.cols) {
      reqd.cols <- sub("!IncaseCol",
                       CASTmetadata$Value[CASTmetadata$Variable == "incaseColName"],
                       reqd.cols)
    }
    act.cols <- colnames(get(object))
    reqd.v.act.cols <- compare.colnames(reqd.cols, act.cols)
    if (object %in% c("data_stressorWS", "data_stressorinfoWS")) {
      df.coltypes.tmp <- cbind("object" = object,
                               "col" = "all columns",
                               "errors" = "were generated by code")
    } else {
      df.coltypes.tmp <- compare.coltypes(object, reqd.cols)
    }
    if (j == 1) {
      df.reqd.obj.cols <- data.frame(cbind(object, reqd.v.act.cols))
      df.reqd.coltypes <- df.coltypes.tmp
    } else {
      df.reqd.obj.cols <- rbind(df.reqd.obj.cols,
                                data.frame(cbind(object, reqd.v.act.cols)))
      df.reqd.coltypes <- rbind(df.reqd.coltypes, df.coltypes.tmp)
    }

  }

  ### Setup for loop over files as needed ----
  # stress.meta <- as.character(loaded$Object[loaded$Description == "stressor metadata"])
  # resp.meta <- as.character(loaded$Object[loaded$Description == "response metadata"])
  # stress.data <- as.character(loaded$Object[loaded$Description == "stressor data"])
  # resp.data <- as.character(loaded$Object[loaded$Description %in% c("response metrics",
  #                                                                    "response counts")])
  # resp.MT <- as.character(loaded$Object[loaded$Description == "response MT"])

  ### Check SSTV values ----
  # TODO: streamline code using loops
  # for (s in seq_along(stress.meta)) {
  #   obj.stress <- stress.meta[s]
  #   df.obj.s <- get(obj.stress)
  #     for (r in seq_along(resp.MT)) {
  #       obj.resp <- resp.MT[r]
  #       df.obj.r <- get(obj.resp)
  #
  #     }
  # }

  if (exists("data_chemInfo")) {
    if (exists("data_bmiMasterTaxa")) {
      object <- "data_bmiMasterTaxa"
      meas.sstv.bmi <- c("TaxonID", unique(data_chemInfo$SSTVname.bmi))
      meas.sstv.bmi <- meas.sstv.bmi[!is.na(meas.sstv.bmi)]
      act.mt <- colnames(data_bmiMasterTaxa)
      reqd.v.act.cols <- compare.colnames(meas.sstv.bmi, act.mt)
      df.reqd.obj.cols <- rbind(df.reqd.obj.cols,
                                data.frame(cbind(object, reqd.v.act.cols)))
      rm(object, meas.sstv.bmi, act.mt)
    }
    if (exists("data_algMasterTaxa")) {
      object <- "data_algMasterTaxa"
      meas.sstv.alg <- c("TaxonID", unique(data_chemInfo$SSTVname.alg))
      meas.sstv.alg <- meas.sstv.alg[!is.na(meas.sstv.alg)]
      act.mt <- colnames(data_algMasterTaxa)
      reqd.v.act.cols <- compare.colnames(meas.sstv.alg, act.mt)
      df.reqd.obj.cols <- rbind(df.reqd.obj.cols,
                                data.frame(cbind(object, reqd.v.act.cols)))
      rm(object, meas.sstv.alg, act.mt)
    }
    if (exists("data_fishMasterTaxa")) {
      object <- "data_fishMasterTaxa"
      meas.sstv.fish <- c("TaxonID", unique(data_chemInfo$SSTVname.fish))
      meas.sstv.fish <- meas.sstv.fish[!is.na(meas.sstv.fish)]
      act.mt <- colnames(data_fishMasterTaxa)
      reqd.v.act.cols <- compare.colnames(meas.sstv.fish, act.mt)
      df.reqd.obj.cols <- rbind(df.reqd.obj.cols,
                                data.frame(cbind(object, reqd.v.act.cols)))
      rm(object, meas.sstv.fish, act.mt)
    }
  }
  if (exists("data_modelInfo")) {
    if (exists("data_bmiMasterTaxa")) {
      object <- "data_bmiMasterTaxa"
      mod.sstv.bmi <- c("TaxonID", unique(data_chemInfo$SSTVname.bmi))
      mod.sstv.bmi <- mod.sstv.bmi[!is.na(mod.sstv.bmi)]
      act.mt <- colnames(data_bmiMasterTaxa)
      act.mt <- act.mt[act.mt != "TaxonID"]
      reqd.v.act.cols <- compare.colnames(mod.sstv.bmi, act.mt)
      df.reqd.obj.cols <- rbind(df.reqd.obj.cols,
                                data.frame(cbind(object, reqd.v.act.cols)))
      rm(object, mod.sstv.bmi, act.mt)
    }
    if (exists("data_algMasterTaxa")) {
      object <- "data_algMasterTaxa"
      mod.sstv.alg <- c("TaxonID", unique(data_chemInfo$SSTVname.alg))
      mod.sstv.alg <- sstv.alg[!is.na(mod.sstv.alg)]
      act.mt <- colnames(data_algMasterTaxa)
      reqd.v.act.cols <- compare.colnames(mod.sstv.alg, act.mt)
      df.reqd.obj.cols <- rbind(df.reqd.obj.cols,
                                data.frame(cbind(object, reqd.v.act.cols)))
      rm(object, mod.sstv.alg, act.mt)
    }
    if (exists("data_fishMasterTaxa")) {
      object <- "data_fishMasterTaxa"
      mod.sstv.fish <- c("TaxonID", unique(data_chemInfo$SSTVname.fish))
      mod.sstv.fish <- mod.sstv.fish[!is.na(mod.sstv.fish)]
      act.mt <- colnames(data_fishMasterTaxa)
      reqd.v.act.cols <- compare.colnames(mod.sstv.fish, act.mt)
      df.reqd.obj.cols <- rbind(df.reqd.obj.cols,
                                data.frame(cbind(object, reqd.v.act.cols)))
      rm(object, mod.sstv.fish, act.mt)
    }
  }

  # Complete expected columns and datatypes ----
  # TODO: group results for passing columns as "Pass: col1, col2, col3" and
  # other columns by error description (IMPROVEMENT)
  df.reqd.coltypes <- df.reqd.coltypes |>
    tidyr::unite(ExpectedDatatypes, col, errors, sep = " ") |>
    dplyr::filter(grepl("pass$", ExpectedDatatypes) == FALSE)

  df.reqd.obj.cols <- merge(df.reqd.obj.cols, df.reqd.coltypes, all.x = TRUE)
  df.reqd.obj.cols <- df.reqd.obj.cols |>
    dplyr::rename(ExpectedColumns = reqd.v.act.cols) |>
    dplyr::mutate(ExpectedColumns = ifelse(ExpectedColumns == "Missing columns: ",
                                           "No missing columns", ExpectedColumns),
                  ExpectedDatatypes = ifelse(is.na(ExpectedDatatypes),
                                             "All expected datatypes confirmed",
                                             ExpectedDatatypes))
  df.reqd.obj.cols <- merge(loaded[, c("Object", "FilePath")], df.reqd.obj.cols,
                            by.x = "Object", by.y = "object", all.y = TRUE)

  rm(act.cols, desc, df.coltypes.tmp, fn, j, reqd.v.act.cols, reqd.cols,
     df.reqd.coltypes)

  # Check primary keys/foreign keys ----
  ### Station checks ----
  # Target sites should be unique
  df_targets   <- unique(df_targets)
  nrow.targets <- nrow(df_targets)
  nrow.targets <- paste(nrow.targets, "unique rows")
  unq.targets  <- unique(df_targets$TargetSiteID)
  nunq.targets <- length(unq.targets)
  nunq.targets <- paste(nunq.targets, "StationIDs")
  # Combine results into dataframe
  df.uniquePKs <- data.frame("Object" = "df_targets",
                             "NumUniquePKs" = nunq.targets)
  df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "df_targets",
                                            "NumUniquePKs" = nrow.targets))
  rm(nrow.targets, nunq.targets)

  # StationIDs should be unique
  data_Sites <- unique(data_Sites)
  nrow.sites <- nrow(data_Sites)
  nrow.sites <- paste(nrow.sites, "unique rows")
  unq.sites  <- unique(data_Sites$StationID)
  nunq.sites <- length(unq.sites)
  nunq.sites <- paste(nunq.sites, "StationIDs")
  unq.sites.COMIDs <- unique(data_Sites$COMID)
  nunq.sites.COMIDs <- length(unq.sites.COMIDs)
  nunq.sites.COMIDs <- paste(nunq.sites.COMIDs, "COMIDs")
  # All StationIDs should have COMIDs
  # Sites w/o COMIDs
  sites.noCOMID <- data_Sites$StationID[is.na(data_Sites$COMID)]
  nsites.noCOMID <- length(sites.noCOMID)
  nsites.noCOMID <- paste(nsites.noCOMID, "StationIDs w/o COMIDs")
  # Combine results into dataframe
  df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_Sites",
                                            "NumUniquePKs" = nunq.sites))
  df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_Sites",
                                            "NumUniquePKs" = nrow.sites))

  df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_Sites",
                                            "NumUniquePKs" = nsites.noCOMID))
  rm(nrow.sites, nunq.sites, nunq.sites.COMIDs, nsites.noCOMID)

  # Target sites should be a subset of all sites
  targ.not.sites <- compare.fk.pk(unq.targets, unq.sites)
  ntarg.not.sites <- length(targ.not.sites)
  sites.not.target <- compare.fk.pk(unq.sites, unq.targets)
  nsites.not.target <- length(sites.not.target)

  if (nsites.not.target == 0) {
    pk.issues <- "All StationIDs are in FileTwo"
  } else {
    pk.issues <- paste0(nsites.not.target, " StationIDs are not in FileTwo")
  }
  if (ntarg.not.sites == 0) {
    fk.issues <- "All StationIDs are in FileOne"
  } else {
    fk.issues <- paste0(ntarg.not.sites, " StationIDs are not in FileOne")
  }

  df.relational.checks <- rbind(df.relational.checks,
                                cbind("FileOne" = "df_Targets",
                                      "FileTwo" = "data_Sites",
                                      "JoinCols" = "StationID",
                                      "IntegrityIssues" = pk.issues,
                                      "OtherConditions" = fk.issues))
  rm(ntarg.not.sites, nsites.not.target, targ.not.sites, sites.not.target)

  # All COMIDs in sites file should be in cluster file
  data_cluster <- unique(data_cluster)
  unq.COMIDs   <- unique(data_cluster$COMID)
  nunq.COMIDs  <- length(unq.COMIDs)
  nunq.COMIDs  <- paste(nunq.COMIDs, "COMIDs")
  unq.cluster  <- unique(data_cluster$ClusterID)
  nunq.cluster <- length(unq.cluster)
  nunq.cluster <- paste(nunq.cluster, "clusters")
  # Combine results into dataframe
  df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_cluster",
                                            "NumUniquePKs" = nunq.COMIDs))
  df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_cluster",
                                            "NumUniquePKs" = nunq.cluster))
  rm(nunq.COMIDs, nunq.cluster)

  # Check relational integrity
  site.COMIDs.not.COMIDs <- compare.fk.pk(unq.sites.COMIDs, unq.COMIDs)
  nsite.COMIDs.not.COMIDs <- length(site.COMIDs.not.COMIDs)
  COMIDs.not.site.COMIDs <- compare.fk.pk(unq.COMIDs, unq.sites.COMIDs)
  nCOMIDs.not.site.COMIDs <- length(COMIDs.not.site.COMIDs)

  if (nsite.COMIDs.not.COMIDs == 0) {
    pk.issues <- "All COMIDs are in FileOne"
  } else {
    pk.issues <- paste0(nsite.COMIDs.not.COMIDs, " COMIDs are not in FileOne")
  }
  if (nCOMIDs.not.site.COMIDs == 0) {
    fk.issues <- "All COMIDs are in FileTwo"
  } else {
    fk.issues <- paste0(nCOMIDs.not.site.COMIDs, " COMIDs are not in FileTwo (expected)")
  }

  df.relational.checks <- rbind(df.relational.checks,
                                cbind("FileOne" = "data_cluster",
                                      "FileTwo" = "data_Sites",
                                      "JoinCols" = "COMID",
                                      "IntegrityIssues" = pk.issues,
                                      "OtherConditions" = fk.issues))
  rm(nsite.COMIDs.not.COMIDs, nCOMIDs.not.site.COMIDs, site.COMIDs.not.COMIDs,
     COMIDs.not.site.COMIDs)

  ### Stressor data checks ----
  #### Watershed stressors ----
  #### DO NOT IMPLEMENT Takes too long to run, and the metadata file is used
  # to generate the data file, so it should be perfectly fine
  # if (all(c("data_stressorWS", "data_stressorinfoWS") %in% loaded$Object)) {
  #   # data_stressorWS COMIDs, clusters, StreamCatVars
  #   data_stressorWS <- unique(data_stressorWS)
  #   unq.WS.COMIDs <- unique(data_stressorWS$COMID)
  #   nunq.WS.COMIDs <- length(unq.WS.COMIDs)
  #   nunq.WS.COMIDs <- paste(nunq.WS.COMIDs, "COMIDs")
  #   unq.WS.clusters <- unique(data_stressorWS$ClusterID)
  #   nunq.WS.clusters <- length(unq.WS.clusters)
  #   nunq.WS.clusters <- paste(nunq.WS.clusters, "clusters")
  #   unq.WS.data.SCvars <- unique(data_stressorWS$StreamCatVar)
  #   nunq.WS.data.SCvars <- length(unq.WS.data.SCvars)
  #   nunq.WS.data.SCvars <- paste(nunq.WS.data.SCvars, "StreamCat variables")
  #   # data_stressorinfoWS
  #   data_stressorinfoWS <- unique(data_stressorinfoWS)
  #   unq.WS.meta.SCvars <- unique(data_stressorinfoWS$StreamCatVar)
  #   nunq.WS.meta.SCvars <- length(unq.WS.meta.SCvars)
  #   nunq.WS.meta.SCvars <- paste(nunq.WS.meta.SCvars, "StreamCat variables")
  #   # Combine results in data frame
  #   df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_stressorWS",
  #                                             "NumUniquePKs" = nunq.WS.COMIDs))
  #   df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_stressorWS",
  #                                             "NumUniquePKs" = nunq.WS.clusters))
  #   df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_stressorWS",
  #                                             "NumUniquePKs" = nunq.WS.data.SCvars))
  #   df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_stressorinfoWS",
  #                                             "NumUniquePKs" = nunq.WS.meta.SCvars))
  #   rm(nunq.WS.COMIDs, nunq.WS.clusters, nunq.WS.data.SCvars, nunq.WS.meta.SCvars)
  #
  #   # Check relational integrity (data_stressorWS vs data_Sites)
  #   WS.data.not.sites  <- compare.fk.pk(unq.WS.COMIDs, unq.sites.COMIDs)
  #   nWS.data.not.sites <- length(WS.data.not.sites)
  #   sites.not.WS.data  <- compare.fk.pk(unq.sites.COMIDs, unq.WS.COMIDs)
  #   nsites.not.WS.data <- length(sites.not.WS.data)
  #
  #   if (nsites.not.WS.data == 0) {
  #     pk.issues <- "All COMIDs are in FileOne"
  #   } else {
  #     pk.issues <- paste0(nsites.not.WS.data, " COMIDs are not in FileOne")
  #   }
  #   if (nWS.data.not.sites == 0) {
  #     fk.issues <- "All COMIDs are in FileTwo"
  #   } else {
  #     fk.issues <- paste0(nWS.data.not.sites, " COMIDs are not in FileTwo")
  #   }
  #
  #   df.relational.checks <- rbind(df.relational.checks,
  #                                 cbind("FileOne" = "data_stressorWS",
  #                                       "FileTwo" = "data_Sites",
  #                                       "JoinCols" = "COMID",
  #                                       "IntegrityIssues" = pk.issues,
  #                                       "OtherConditions" = fk.issues))
  #   rm(nsites.not.WS.data, nWS.data.not.sites, sites.not.WS.data,
  #      WS.data.not.sites)
  #
  #   # Check relational integrity (data_stressorinfoWS vs data_stressorWS)
  #   # unq.WS.data.SCvars (Many)     nunq.WS.meta.SCvars (One)
  #   SCvars.data.not.meta  <- compare.fk.pk(unq.WS.data.SCvars, nunq.WS.meta.SCvars)
  #   nSCvars.data.not.meta <- length(SCvars.data.not.meta)
  #   SCvars.meta.not.data  <- compare.fk.pk(nunq.WS.meta.SCvars, unq.WS.data.SCvars)
  #   nSCvars.meta.not.data <- length(SCvars.meta.not.data)
  #
  #   if (nSCvars.data.not.meta == 0) {
  #     pk.issues <- "All COMIDs are in FileOne"
  #   } else {
  #     pk.issues <- paste0(nSCvars.data.not.meta, " COMIDs are not in FileOne")
  #   }
  #   if (nSCvars.meta.not.data == 0) {
  #     fk.issues <- "All COMIDs are in FileTwo"
  #   } else {
  #     fk.issues <- paste0(nSCvars.meta.not.data, " COMIDs are not in FileTwo")
  #   }
  #
  #   df.relational.checks <- rbind(df.relational.checks,
  #                                 cbind("FileOne" = "data_stressorWS",
  #                                       "FileTwo" = "data_stressorinfoWS",
  #                                       "JoinCols" = "StreamCat variable",
  #                                       "IntegrityIssues" = pk.issues,
  #                                       "OtherConditions" = fk.issues))
  #   rm(nSCvars.data.not.meta, nSCvars.meta.not.data, SCvars.data.not.meta,
  #      SCvars.meta.not.data)
  #
  # }

  #### Measured stressors ----
  if (all(c("data_chemAll", "data_chemInfo") %in% loaded$Object)) {
    # data_chemAll unique stations, samples, and parameters
    data_chemAll <- unique(data_chemAll)
    nunq.meas.samples <- nrow(unique(data_chemAll[, c("StationID",
                                                      "StressSampleID",
                                                      "StressSampleDate")]))

    nunq.meas.samples    <- paste(nunq.meas.samples, "samples")
    unq.meas.data.sites  <- unique(data_chemAll$StationID)
    nunq.meas.data.sites <- length(unq.meas.data.sites)
    nunq.meas.data.sites <- paste(nunq.meas.data.sites, "StationIDs")
    unq.meas.data.param  <- unique(data_chemAll$StdParamName)
    nunq.meas.data.param <- length(unq.meas.data.param)
    nunq.meas.data.param <- paste(nunq.meas.data.param, "parameters")
    # data_chemInfo unique parameters
    data_chemInfo <- unique(data_chemInfo)
    unq.meas.meta.param  <- unique(data_chemInfo$StdParamName)
    nunq.meas.meta.param <- length(unq.meas.meta.param)
    nunq.meas.meta.param <- paste(nunq.meas.meta.param, "parameters")
    # Combine results in data frame
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_chemAll",
                                              "NumUniquePKs" = nunq.meas.data.sites))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_chemAll",
                                              "NumUniquePKs" = nunq.meas.samples))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_chemAll",
                                              "NumUniquePKs" = nunq.meas.data.param))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_chemInfo",
                                              "NumUniquePKs" = nunq.meas.meta.param))
    rm(nunq.meas.samples, nunq.meas.data.sites, nunq.meas.data.param, nunq.meas.meta.param)

    # Check relational integrity (data_chemAll vs data_Sites)
    meas.data.not.sites  <- compare.fk.pk(unq.meas.data.sites, unq.sites)
    nmeas.data.not.sites <- length(meas.data.not.sites)
    sites.not.meas.data  <- compare.fk.pk(unq.sites, unq.meas.data.sites)
    nsites.not.meas.data <- length(sites.not.meas.data)

    if (nmeas.data.not.sites == 0) {
      pk.issues <- "All StationIDs are in FileOne"
    } else {
      pk.issues <- paste0(nmeas.data.not.sites, " StationIDs are not in FileOne")
    }
    if (nsites.not.meas.data == 0) {
      fk.issues <- "All StationIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nsites.not.meas.data, " StationIDs are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_Sites",
                                        "FileTwo" = "data_chemAll",
                                        "JoinCols" = "StationID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nmeas.data.not.sites, nsites.not.meas.data, meas.data.not.sites,
       sites.not.meas.data)

    # Check relational integrity (data_chemInfo vs data_chemAll)
    meas.param.not.meta  <- compare.fk.pk(unq.meas.data.param, unq.meas.meta.param)
    nmeas.param.not.meta <- length(meas.param.not.meta)
    meta.param.not.meas  <- compare.fk.pk(unq.meas.meta.param, unq.meas.data.param)
    nmeta.param.not.meas <- length(meta.param.not.meas)

    if (nmeas.param.not.meta == 0) {
      pk.issues <- "All StdParamNames are in FileOne"
    } else {
      pk.issues <- paste0(nmeas.param.not.meta, " StdParamNames are not in FileOne")
    }
    if (nmeta.param.not.meas == 0) {
      fk.issues <- "All StdParamNames are in File Two"
    } else {
      fk.issues <- paste0(nmeta.param.not.meas, " StdParamNames are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_chemInfo",
                                        "FileTwo" = "data_chemAll",
                                        "JoinCols" = "StdParamName",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nmeas.param.not.meta, nmeta.param.not.meas, meas.param.not.meta,
       meta.param.not.meas)
  }

  #### Modeled stressors ----
  if (all(c("data_modelAll", "data_modelInfo") %in% loaded$Object)) {
    # data_modelAll unique stations, samples, and parameters
    data_modelAll       <- unique(data_modelAll)
    nunq.mod.samples    <- nrow(unique(data_modelAll[, c("StationID",
                                                         "StressSampleID")]))
    nunq.mod.samples    <- paste(nunq.mod.samples, "samples")
    unq.mod.data.sites  <- unique(data_modelAll$StationID)
    nunq.mod.data.sites <- length(unq.mod.data.sites)
    nunq.mod.data.sites <- paste(nunq.mod.data.sites, "StationIDs")
    unq.mod.data.param  <- unique(data_modelAll$StdParamName)
    nunq.mod.data.param <- length(unq.mod.data.param)
    nunq.mod.data.param <- paste(nunq.mod.data.param, "parameters")
    # data_modelInfo unique parameters
    data_modelInfo      <- unique(data_modelInfo)
    unq.mod.meta.param  <- unique(data_modelInfo$StdParamName)
    nunq.mod.meta.param <- length(unq.mod.meta.param)
    nunq.mod.meta.param <- paste(nunq.mod.meta.param, "parameters")
    # Combine results in data frame
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_modelAll",
                                              "NumUniquePKs" = nunq.mod.data.sites))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_modelAll",
                                              "NumUniquePKs" = nunq.mod.samples))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_modelAll",
                                              "NumUniquePKs" = nunq.mod.data.param))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_modelInfo",
                                              "NumUniquePKs" = nunq.mod.meta.param))
    rm(nunq.mod.data.sites, nunq.mod.samples, nunq.mod.data.param, nunq.mod.meta.param)

    # Check relational integrity (data_modelAll vs data_Sites)
    mod.data.not.sites  <- compare.fk.pk(unq.mod.data.sites, unq.sites)
    nmod.data.not.sites <- length(mod.data.not.sites)
    sites.not.mod.data  <- compare.fk.pk(unq.sites, unq.mod.data.sites)
    nsites.not.mod.data <- length(sites.not.mod.data)

    if (nmod.data.not.sites == 0) {
      pk.issues <- "All StationIDs are in FileOne"
    } else {
      pk.issues <- paste0(nmod.data.not.sites, " StationIDs are not in FileOne")
    }
    if (nsites.not.mod.data == 0) {
      fk.issues <- "All StationIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nsites.not.mod.data, " StationIDs are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_Sites",
                                        "FileTwo" = "data_modelAll",
                                        "JoinCols" = "StationID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nmod.data.not.sites, nsites.not.mod.data, mod.data.not.sites,
       sites.not.mod.data)

    # Check relational integrity (data_modelInfo vs data_modelAll)
    mod.param.not.meta  <- compare.fk.pk(unq.mod.data.param, unq.mod.meta.param)
    nmod.param.not.meta <- length(mod.param.not.meta)
    meta.param.not.mod  <- compare.fk.pk(unq.mod.meta.param, unq.mod.data.param)
    nmeta.param.not.mod <- length(meta.param.not.mod)

    if (nmod.param.not.meta == 0) {
      pk.issues <- "All StdParamNames are in FileOne"
    } else {
      pk.issues <- paste0(nmod.param.not.meta, " StdParamNames are not in FileOne")
    }
    if (nmeta.param.not.mod == 0) {
      fk.issues <- "All StdParamNames are in File Two"
    } else {
      fk.issues <- paste0(nmeta.param.not.mod, " StdParamNames are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_modelInfo",
                                        "FileTwo" = "data_modelAll",
                                        "JoinCols" = "StdParamName",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nmod.param.not.meta, nmeta.param.not.mod, mod.param.not.meta,
       meta.param.not.mod)
  }

  ### Response data checks ----
  ##### BMI ----
  # Benthic macroinvertebrate metric data checks
  if (all(c("data_bmiMetrics", "data_bmiMetricsInfo") %in% loaded$Object)) {
    # data_bmiMetrics unique stations, samples, and metrics
    data_bmiMetrics      <- unique(data_bmiMetrics)
    nunq.bmi.data.samps  <- nrow(unique(data_bmiMetrics[, c("RespSampleID",
                                                     "RespSampleDate")]))
    nunq.bmi.data.samps  <- paste(nunq.bmi.data.samps, "samples")
    unq.bmi.data.sites   <- unique(data_bmiMetrics$StationID)
    nunq.bmi.data.sites  <- length(unq.bmi.data.sites)
    nunq.bmi.data.sites  <- paste(nunq.bmi.data.sites, "StationIDs")
    data_bmiMetrics.long <- data_bmiMetrics |>
      dplyr::select(-dplyr::any_of(c("Study_ID", "Latitude", "Longitude"))) %>% # LCN added 20250916
      tidyr::pivot_longer(cols = !c(StationID, RespSampleID, RespSampleDate),
                          names_to = "MetricName", values_to = "Value",
                          values_transform = list(Value = as.character))
    unq.bmi.data.metric  <- unique(data_bmiMetrics.long$MetricName)
    nunq.bmi.data.metric <- length(unq.bmi.data.metric)
    nunq.bmi.data.metric <- paste(nunq.bmi.data.metric, "metrics")
    # data_bmiMetricsInfo unique metrics
    unq.bmi.meta.metric  <- unique(data_bmiMetricsInfo$MetricName)
    nunq.bmi.meta.metric <- length(unq.bmi.meta.metric)
    nunq.bmi.meta.metric <- paste(nunq.bmi.meta.metric, "metrics")
    # Combine results in data frame
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_bmiMetrics",
                                              "NumUniquePKs" = nunq.bmi.data.sites))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_bmiMetrics",
                                              "NumUniquePKs" = nunq.bmi.data.samps))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_bmiMetrics",
                                              "NumUniquePKs" = nunq.bmi.data.metric))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_bmiMetricsInfo",
                                              "NumUniquePKs" = nunq.bmi.meta.metric))
    rm(nunq.bmi.data.sites, nunq.bmi.data.samps, nunq.bmi.data.metric, nunq.bmi.meta.metric)

    # Check relational integrity (data_Sites vs. data_bmiMetrics)
    bmi.data.not.sites  <- compare.fk.pk(unq.bmi.data.sites, unq.sites)
    nbmi.data.not.sites <- length(bmi.data.not.sites)
    sites.not.bmi.data  <- compare.fk.pk(unq.sites, unq.bmi.data.sites)
    nsites.not.bmi.data <- length(sites.not.bmi.data)

    # File1 = data_Sites; File2 = data_bmiMetrics
    if (nbmi.data.not.sites == 0) {
      pk.issues <- "All StationIDs are in FileOne"
    } else {
      pk.issues <- paste0(nbmi.data.not.sites, " StationIDs are not in FileOne")
    }
    if (nsites.not.bmi.data == 0) {
      fk.issues <- "All StationIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nsites.not.bmi.data, " StationIDs are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_Sites",
                                        "FileTwo" = "data_bmiMetrics",
                                        "JoinCols" = "StationID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nbmi.data.not.sites, nsites.not.bmi.data, bmi.data.not.sites,
       sites.not.bmi.data)

    # Check relational integrity (data_bmiMetricsInfo vs. data_bmiMetrics)
    bmi.data.not.meta  <- compare.fk.pk(unq.bmi.data.metric, unq.bmi.meta.metric)
    nbmi.data.not.meta <- length(bmi.data.not.meta)
    bmi.meta.not.data  <- compare.fk.pk(unq.bmi.meta.metric, unq.bmi.data.metric)
    nbmi.meta.not.data <- length(bmi.meta.not.data)

    # File1 = data_bmiMetricsInfo; File2 = data_bmiMetrics
    if (nbmi.data.not.meta == 0) {
      pk.issues <- "All MetricNames are in FileOne"
    } else {
      pk.issues <- paste0(nbmi.data.not.meta, " MetricNames are not in FileOne")
    }
    if (nbmi.meta.not.data == 0) {
      fk.issues <- "All MetricNames are in FileTwo"
    } else {
      fk.issues <- paste0(nbmi.meta.not.data, " MetricNames are not in FileTwo")
    }
    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_bmiMetricsInfo",
                                        "FileTwo" = "data_bmiMetrics",
                                        "JoinCols" = "MetricName",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nbmi.data.not.meta, nbmi.meta.not.data, bmi.data.not.meta,
       bmi.meta.not.data)
  }

  if (all(c("data_bmiCounts", "data_bmiMasterTaxa") %in% loaded$Object)) {
    # data_bmiCounts
    data_bmiCounts        <- unique(data_bmiCounts)
    nunq.bmi.counts.samps <- nrow(unique(data_bmiCounts[, c("RespSampleID",
                                                            "RespSampleDate")]))
    nunq.bmi.counts.samps <- paste(nunq.bmi.counts.samps, "samples")
    unq.bmi.count.sites   <- unique(data_bmiCounts$StationID)
    nunq.bmi.count.sites  <- length(unq.bmi.count.sites)
    nunq.bmi.count.sites  <- paste(nunq.bmi.count.sites, "StationIDs")
    unq.bmi.count.taxa    <- unique(data_bmiCounts$TaxonID)
    nunq.bmi.count.taxa   <- length(unq.bmi.count.taxa)
    nunq.bmi.count.taxa   <- paste(nunq.bmi.count.taxa, "TaxonIDs")
    # data_bmiMasterTaxa
    data_bmiMasterTaxa    <- unique(data_bmiMasterTaxa)
    unq.bmi.mastertaxa    <- unique(data_bmiMasterTaxa$TaxonID)
    nunq.bmi.mastertaxa   <- length(unq.bmi.mastertaxa)
    nunq.bmi.mastertaxa   <- paste(nunq.bmi.mastertaxa, "TaxonIDs")
    # Combine results in data frame
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_bmiCounts",
                                              "NumUniquePKs" = nunq.bmi.count.sites))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_bmiCounts",
                                              "NumUniquePKs" = nunq.bmi.counts.samps))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_bmiCounts",
                                              "NumUniquePKs" = nunq.bmi.count.taxa))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_bmiMasterTaxa",
                                              "NumUniquePKs" = nunq.bmi.mastertaxa))
    rm(nunq.bmi.count.sites, nunq.bmi.counts.samps, nunq.bmi.count.taxa, nunq.bmi.mastertaxa)

    # Check relational integrity (data_Sites vs data_bmiCounts)
    bmi.count.not.sites  <- compare.fk.pk(unq.bmi.count.sites, unq.sites)
    nbmi.count.not.sites <- length(bmi.count.not.sites)
    sites.not.bmi.count  <- compare.fk.pk(unq.sites, unq.bmi.count.sites)
    nsites.not.bmi.count <- length(sites.not.bmi.count)

    if (nbmi.count.not.sites == 0) {
      pk.issues <- "All StationIDs are in FileOne"
    } else {
      pk.issues <- paste0(nbmi.count.not.sites, " StationIDs are not in FileOne")
    }
    if (nsites.not.bmi.count == 0) {
      fk.issues <- "All StationIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nsites.not.bmi.count, " StationIDs are not in FileTwo")
    }
    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_Sites",
                                        "FileTwo" = "data_bmiCounts",
                                        "JoinCols" = "StationID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nbmi.count.not.sites, nsites.not.bmi.count, bmi.count.not.sites,
       sites.not.bmi.count)

    # Check relational integrity (data_bmiMasterTaxa vs data_bmiCounts)
    bmi.count.not.mt  <- compare.fk.pk(unq.bmi.count.taxa, unq.bmi.mastertaxa)
    nbmi.count.not.mt <- length(bmi.count.not.mt)
    bmi.mt.not.count  <- compare.fk.pk(unq.bmi.mastertaxa, unq.bmi.count.taxa)
    nbmi.mt.not.count <- length(bmi.mt.not.count)

    if (nbmi.count.not.mt == 0) {
      pk.issues <- "All TaxonIDs are in FileOne"
    } else {
      pk.issues <- paste0(nbmi.count.not.mt, " TaxonIDs are not in FileOne")
    }
    if (nbmi.mt.not.count == 0) {
      fk.issues <- "All TaxonIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nbmi.mt.not.count, " TaxonIDs are not in FileTwo")
    }
    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_bmiMasterTaxa",
                                        "FileTwo" = "data_bmiCounts",
                                        "JoinCols" = "TaxonID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nbmi.count.not.mt, nbmi.mt.not.count, bmi.count.not.mt, bmi.mt.not.count)
  }

  ##### Algae ----
  if (all(c("data_algMetrics", "data_algMetricsInfo") %in% loaded$Object)) {
    # data_algMetrics
    data_algMetrics      <- unique(data_algMetrics)
    nunq.alg.data.samps  <- nrow(unique(data_algMetrics[, c("RespSampleID",
                                                           "RespSampleDate")]))
    nunq.alg.data.samps  <- paste(nunq.alg.data.samps, "samples")
    unq.alg.data.sites   <- unique(data_algMetrics$StationID)
    nunq.alg.data.sites  <- length(unq.alg.data.sites)
    nunq.alg.data.sites  <- paste(nunq.alg.data.sites, "StationIDs")
    data_algMetrics.long <- data_algMetrics |>
      tidyr::pivot_longer(cols = !c(StationID, RespSampleID, RespSampleDate),
                          names_to = "MetricName", values_to = "Value",
                          values_transform = list(Value = as.character))
    unq.alg.data.metric  <- unique(data_algMetrics.long$MetricName)
    nunq.alg.data.metric <- length(unq.alg.data.metric)
    nunq.alg.data.metric <- paste(nunq.alg.data.metric, "metrics")
    # data_algMetricInfo
    data_algMetricsInfo   <- unique(data_algMetricsInfo)
    unq.alg.meta.metric  <- unique(data_algMetricsInfo$MetricName)
    nunq.alg.meta.metric <- length(unq.alg.meta.metric)
    nunq.alg.meta.metric <- paste(nunq.alg.meta.metric, "metrics")
    # Combine results in data frame
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_algMetrics",
                                              "NumUniquePKs" = nunq.alg.data.sites))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_algMetrics",
                                              "NumUniquePKs" = nunq.alg.data.samps))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_algMetrics",
                                              "NumUniquePKs" = nunq.alg.data.metric))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_algMetricsInfo",
                                              "NumUniquePKs" = nunq.alg.meta.metric))
    rm(nunq.alg.data.sites, nunq.alg.data.samps, nunq.alg.data.metric, nunq.alg.meta.metric)

    # Check relational integrity (data_Sites vs data_algMetrics)
    alg.data.not.sites  <- compare.fk.pk(unq.alg.data.sites, unq.sites)
    nalg.data.not.sites <- length(alg.data.not.sites)
    sites.not.alg.data  <- compare.fk.pk(unq.sites, unq.alg.data.sites)
    nsites.not.alg.data <- length(sites.not.alg.data)

    if (nalg.data.not.sites == 0) {
      pk.issues <- "All StationIDs are in FileOne"
    } else {
      pk.issues <- paste0(nalg.data.not.sites, " StationIDs are not in FileOne")
    }
    if (nsites.not.alg.data == 0) {
      fk.issues <- "All StationIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nsites.not.alg.data, " StationIDs are not in FileTwo")
    }
    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_Sites",
                                        "FileTwo" = "data_algMetrics",
                                        "JoinCols" = "StationID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nalg.data.not.sites, nsites.not.alg.data, alg.data.not.sites,
       sites.not.alg.data)

    # Check relational integrity (dataAlgMetricsInfo vs data_algMetrics)
    alg.data.not.meta  <- compare.fk.pk(unq.alg.data.metric, unq.alg.meta.metric)
    nalg.data.not.meta <- length(alg.data.not.meta)
    alg.meta.not.data  <- compare.fk.pk(unq.alg.meta.metric, unq.alg.data.metric)
    nalg.meta.not.data <- length(alg.meta.not.data)

    if (nalg.data.not.meta == 0) {
      pk.issues <- "All MetricNames are in FileOne"
    } else {
      pk.issues <- paste0(nalg.data.not.meta, " MetricNames are not in FileOne")
    }
    if (nalg.meta.not.data == 0) {
      fk.issues <- "All MetricNames are in FileTwo"
    } else {
      fk.issues <- paste0(nalg.meta.not.data, " MetricNames are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_algMetricsInfo",
                                        "FileTwo" = "data_algMetrics",
                                        "JoinCols" = "MetricName",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nalg.data.not.meta, nalg.meta.not.data, alg.data.not.meta,
       alg.meta.not.data)
  }

  # If alg count data and master taxa are available
  if (all(c("data_algCounts", "data_algMasterTaxa") %in% loaded$Object)) {
    # data_algCounts
    data_algCounts        <- unique(data_algCounts)
    nunq.alg.counts.samps <- nrow(unique(data_algCounts[, c("RespSampleID",
                                                      "RespSampleDate")]))
    nunq.alg.counts.samps <- paste(nunq.alg.counts.samps, "samples")
    unq.alg.count.sites   <- unique(data_algCounts$StationID)
    nunq.alg.count.sites  <- length(unq.alg.count.sites)
    nunq.alg.count.sites  <- paste(nunq.alg.count.sites, "StationIDs")
    unq.alg.count.taxa    <- unique(data_algCounts$TaxonID)
    nunq.alg.count.taxa   <- length(unq.alg.count.taxa)
    nunq.alg.count.taxa   <- paste(nunq.alg.count.taxa, "TaxonIDs")
    # data_algMasterTaxa
    data_algMasterTaxa    <- unique(data_algMasterTaxa)
    unq.alg.mastertaxa    <- unique(data_algMasterTaxa$TaxonID)
    nunq.alg.mastertaxa   <- length(unq.alg.mastertaxa)
    nunq.alg.mastertaxa   <- paste(nunq.alg.mastertaxa, "TaxonIDs")
    # Combine results in data frame
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_algCounts",
                                              "NumUniquePKs" = nunq.alg.count.sites))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_algCounts",
                                              "NumUniquePKs" = nunq.alg.counts.samps))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_algCounts",
                                              "NumUniquePKs" = nunq.alg.count.taxa))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_algMasterTaxa",
                                              "NumUniquePKs" = nunq.alg.mastertaxa))
    rm(nunq.alg.count.sites, nunq.alg.counts.samps, nunq.alg.count.taxa, nunq.alg.mastertaxa)

    # Check relational integrity (data_Sites vs data_algCounts)
    alg.count.not.sites <- compare.fk.pk(unq.alg.count.sites, unq.sites)
    nalg.count.not.sites <- length(alg.count.not.sites)
    sites.not.alg.count <- compare.fk.pk(unq.sites, unq.alg.count.sites)
    nsites.not.alg.count <- length(sites.not.alg.count)

    if (nalg.count.not.sites == 0) {
      pk.issues <- "All StationIDs are in FileOne"
    } else {
      pk.issues <- paste0(nalg.count.not.sites, " StationIDs are not in FileOne")
    }
    if (nsites.not.alg.count == 0) {
      fk.issues <- "All StationIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nsites.not.alg.count, " StationIDs are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_Sites",
                                        "FileTwo" = "data_algCounts",
                                        "JoinCols" = "StationID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nalg.count.not.sites, nsites.not.alg.count, alg.count.not.sites,
       sites.not.alg.count)

    # Check relational integrity (data_algMasterTaxa vs data_algCounts)
    alg.count.not.mt  <- compare.fk.pk(unq.alg.count.taxa, unq.alg.mastertaxa)
    nalg.count.not.mt <- length(alg.count.not.mt)
    alg.mt.not.count  <- compare.fk.pk(unq.alg.mastertaxa, unq.alg.count.taxa)
    nalg.mt.not.count <- length(alg.mt.not.count)

    if (nalg.count.not.mt == 0) {
      pk.issues <- "All TaxonIDs are in FileOne"
    } else {
      pk.issues <- paste0(nalg.count.not.mt, " TaxonIDs are not in FileOne")
    }
    if (nalg.mt.not.count == 0) {
      fk.issues <- "All TaxonIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nalg.mt.not.count, " TaxonIDs are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_algMasterTaxa",
                                        "FileTwo" = "data_algCounts",
                                        "JoinCols" = "TaxonID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nalg.count.not.mt, nalg.mt.not.count, alg.count.not.mt, alg.mt.not.count)
  }

  ##### Fish ----
  # Benthic macroinvertebrate metric data checks
  if (all(c("data_fishMetrics", "data_fishMetricsInfo") %in% loaded$Object)) {
    # data_fishMetrics
    data_fishMetrics      <- unique(data_fishMetrics)
    nunq.fish.data.samps  <- nrow(unique(data_fishMetrics[, c("RespSampleID",
                                                           "RespSampleDate")]))
    nunq.fish.data.samps  <- paste(nunq.fish.data.samps, "samples")
    unq.fish.data.sites   <- unique(data_fishMetrics$StationID)
    nunq.fish.data.sites  <- length(unq.fish.data.sites)
    nunq.fish.data.sites  <- paste(nunq.fish.data.sites, "StationIDs")
    data_fishMetrics.long <- data_fishMetrics |>
      tidyr::pivot_longer(cols = !c(StationID, RespSampleID, RespSampleDate),
                          names_to = "MetricName", values_to = "Value",
                          values_transform = list(Value = as.character))
    unq.fish.data.metric  <- unique(data_fishMetrics.long$MetricName)
    nunq.fish.data.metric <- length(unq.fish.data.metric)
    nunq.fish.data.metric <- paste(nunq.fish.data.metric, "metrics")
    # data_fishMetricsInfo
    data_fishMetricsInfo  <- unique(data_fishMetricsInfo)
    unq.fish.meta.metric  <- unique(data_fishMetricsInfo$MetricName)
    nunq.fish.meta.metric <- length(unq.fish.meta.metric)
    nunq.fish.meta.metric <- paste(nunq.fish.meta.metric, "metrics")
    # Combine results in data frame
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_fishMetrics",
                                              "NumUniquePKs" = nunq.fish.data.sites))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_fishMetrics",
                                              "NumUniquePKs" = nunq.fish.data.samps))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_fishMetrics",
                                              "NumUniquePKs" = nunq.fish.data.metric))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_fishMetricsInfo",
                                              "NumUniquePKs" = nunq.fish.meta.metric))
    rm(nunq.fish.data.sites, nunq.fish.data.samps, nunq.fish.data.metric, nunq.fish.meta.metric)

    # Check relational integrity (data_Sites vs data_fishMetrics)
    fish.data.not.sites  <- compare.fk.pk(unq.fish.data.sites, unq.sites)
    nfish.data.not.sites <- length(fish.data.not.sites)
    sites.not.fish.data  <- compare.fk.pk(unq.sites, unq.fish.data.sites)
    nsites.not.fish.data <- length(sites.not.fish.data)

    if (nfish.data.not.sites == 0) {
      pk.issues <- "All StationIDs are in FileOne"
    } else {
      pk.issues <- paste0(nfish.data.not.sites, " StationIDs are not in FileOne")
    }
    if (nsites.not.fish.data == 0) {
      fk.issues <- "All StationIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nsites.not.fish.data, " StationIDs are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_Sites",
                                        "FileTwo" = "data_fishMetrics",
                                        "JoinCols" = "StationID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nfish.data.not.sites, nsites.not.fish.data, fish.data.not.sites,
       sites.not.fish.data)

    # Check relational integrity (data_fishMetricsInfo vs data_fishMetrics)
    fish.data.not.meta  <- compare.fk.pk(unq.fish.data.metric, unq.fish.meta.metric)
    nfish.data.not.meta <- length(fish.data.not.meta)
    fish.meta.not.data  <- compare.fk.pk(unq.fish.meta.metric, unq.fish.data.metric)
    nfish.meta.not.data <- length(fish.meta.not.data)

   if (nfish.data.not.meta == 0) {
      pk.issues <- "All MetricNames are in FileOne"
    } else {
      pk.issues <- paste0(nfish.data.not.meta, " MetricNames are not in FileOne")
    }
    if (nfish.meta.not.data == 0) {
      fk.issues <- "All MetricNames are in FileTwo"
    } else {
      fk.issues <- paste0(nfish.meta.not.data, " MetricNames are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_fishMetricsInfo",
                                        "FileTwo" = "data_fishMetrics",
                                        "JoinCols" = "MetricName",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nfish.data.not.meta, nfish.meta.not.data, fish.data.not.meta,
       fish.meta.not.data)
  }

  # If fish count data and master taxa are available
  if (all(c("data_fishCounts", "data_fishMasterTaxa") %in% loaded$Object)) {
    # data_fishCounts
    data_fishCounts <- unique(data_fishCounts)
    nunq.fish.counts.samps <- nrow(unique(data_fishCounts[, c("RespSampleID",
                                                              "RespSampleDate")]))
    nunq.fish.counts.samps <- paste(nunq.fish.counts.samps, "samples")
    unq.fish.count.sites   <- unique(data_fishCounts$StationID)
    nunq.fish.count.sites  <- length(unq.fish.count.sites)
    nunq.fish.count.sites  <- paste(nunq.fish.count.sites, "StationIDs")
    unq.fish.count.taxa    <- unique(data_fishCounts$TaxonID)
    nunq.fish.count.taxa   <- length(unq.fish.count.taxa)
    nunq.fish.count.taxa   <- paste(nunq.fish.count.taxa, "TaxonIDs")
    # data_fishMasterTaxa
    data_fishMasterTaxa    <- unique(data_fishMasterTaxa)
    unq.fish.mastertaxa    <- unique(data_fishMasterTaxa$TaxonID)
    nunq.fish.mastertaxa   <- length(unq.fish.mastertaxa)
    nunq.fish.mastertaxa   <- paste(nunq.fish.mastertaxa, "TaxonIDs")
    # Combine results in data frame
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_fishCounts",
                                              "NumUniquePKs" = nunq.fish.count.sites))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_fishCounts",
                                              "NumUniquePKs" = nunq.fish.counts.samps))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_fishCounts",
                                              "NumUniquePKs" = nunq.fish.count.taxa))
    df.uniquePKs <- rbind(df.uniquePKs, cbind("Object" = "data_fishMasterTaxa",
                                              "NumUniquePKs" = nunq.fish.mastertaxa))

    # Check relational integrity (data_Sites vs data_fishCounts)
    fish.count.not.sites  <- compare.fk.pk(unq.fish.count.sites, unq.sites)
    nfish.count.not.sites <- length(fish.count.not.sites)
    sites.not.fish.count  <- compare.fk.pk(unq.sites, unq.fish.count.sites)
    nsites.not.fish.count <- length(sites.not.fish.count)

    if (nfish.count.not.sites == 0) {
      pk.issues <- "All StationIDs are in FileOne"
    } else {
      pk.issues <- paste0(nfish.count.not.sites, " StationIDs are not in FileOne")
    }
    if (nsites.not.fish.count == 0) {
      fk.issues <- "All StationIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nsites.not.fish.count, " StationIDs are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_Sites",
                                        "FileTwo" = "data_fishCounts",
                                        "JoinCols" = "StationID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nfish.count.not.sites, nsites.not.fish.count, fish.count.not.sites,
       sites.not.fish.count)

    # Check relational integrity (data_fishMasterTaxa vs data_fishCounts)
    fish.count.not.mt  <- compare.fk.pk(unq.fish.count.taxa, unq.fish.mastertaxa)
    nfish.count.not.mt <- length(fish.count.not.mt)
    fish.mt.not.count  <- compare.fk.pk(unq.fish.mastertaxa, unq.fish.count.taxa)
    nfish.mt.not.count <- length(fish.mt.not.count)

    if (nfish.count.not.mt == 0) {
      pk.issues <- "All TaxonIDs are in FileOne"
    } else {
      pk.issues <- paste0(nfish.count.not.mt, " TaxonIDs are not in FileOne")
    }
    if (nfish.mt.not.count == 0) {
      fk.issues <- "All TaxonIDs are in FileTwo"
    } else {
      fk.issues <- paste0(nfish.mt.not.count, " TaxonIDs are not in FileTwo")
    }

    df.relational.checks <- rbind(df.relational.checks,
                                  cbind("FileOne" = "data_fishMasterTaxa",
                                        "FileTwo" = "data_fishCounts",
                                        "JoinCols" = "TaxonID",
                                        "IntegrityIssues" = pk.issues,
                                        "OtherConditions" = fk.issues))
    rm(nfish.count.not.mt, nfish.mt.not.count, fish.count.not.mt,
       fish.mt.not.count)
  }

  # Clean up vectors
  # rm(fk.issues, pk.issues, sites.noCOMID, unq.bmi.count.sites,
  #    unq.bmi.count.taxa, unq.bmi.data.metric, unq.bmi.data.sites,
  #    unq.bmi.mastertaxa, unq.bmi.meta.metric, unq.cluster, unq.COMIDs,
  #    unq.meas.data.param, unq.meas.data.sites, unq.meas.meta.param,
  #    unq.sites, unq.sites.COMIDs, unq.targets)

  # Prepare outputs ----
  ## TableOne: Summary of file inputs ----
  df.reqd.obj.cols <- merge(df.reqd.obj.cols,
                            input_check[, c("Object", "PrimaryKey")],
                            all.x = TRUE)

  df.uniquePKs <- df.uniquePKs |>
    dplyr::group_by(Object) |>
    dplyr::summarize(Observations = paste(NumUniquePKs, collapse = "; "),
                     .groups = "drop_last")
  df.reqd.obj.cols <- merge(df.reqd.obj.cols, df.uniquePKs, by = "Object",
                            all.x = TRUE)
  rm(df.uniquePKs)

  df.TableOne <- df.reqd.obj.cols |>
    dplyr::mutate(Observations = ifelse(is.na(Observations),
                                        "Not checked", Observations)) |>
    dplyr::select(FilePath, Object, ExpectedColumns, ExpectedDatatypes,
                  PrimaryKey, Observations)
  rm(df.reqd.obj.cols)

  ## TableTwo: Relational integrity ----
  df.TableTwo <- df.relational.checks |>
    dplyr::rename(`FileOne (One)` = FileOne,
                  `FileTwo (Many)` = FileTwo)

  rm(input_check)

  # Write results directory ----
  region <- as.character(CASTmetadata$Value[CASTmetadata$Variable == "region"])
  out.folders <- c(dir.out, region, "Results", "_CheckedInputs")

  for (i in 1:length(out.folders)) {
    if (i == 1) {
      dir.path <- file.path(out.folders[i])
    } else {
      dir.path <- file.path(dir.path, out.folders[i])
    }
    if (dir.exists(dir.path) == FALSE) {
      dir.create(dir.path)
    }
  }

  dir.out <- dir.path

  # Save objects ----
  saveRDS(CASTmetadata, file.path(dir.out, "CASTmetadata.rds"))
  saveRDS(loaded, file.path(dir.out, "loaded.rds"))

  objects <- df.TableOne$Object
  for (o in seq_along(objects)) {
    objName <- objects[o]
    fn <- paste0(objName, ".rds")
    saveRDS(get(objName), file.path(dir.out, fn))
  }

  myTables <- list(TableOne = df.TableOne, TableTwo = df.TableTwo)
  return(myTables)

}
