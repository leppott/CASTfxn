#  Copyright 2025 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Get Comparator Sites
#'
#' @description Generates a list of comparator sites for the target site.
#'
#' @details Obtains the cluster members for the cluster containing the target
#'          site and, if useBC = TRUE (default is FALSE), subsets the cluster
#'          based on a biological dissimilarity distance matrix (Bray-Curtis)
#'          at the specified level of dissimilarity.
#'
#' Uses the library dplyr
#'
#' @param TargetSiteID Site ID for the site being evaluated
#' @param df_sites Sites table with cluster membership. Default = "data_Sites".
#' @param df_cluster Dataframe containing reach IDs and their associated clusters
#' @param df_bioCoOccur Dataframe containing paired stressor/response sample data
#' @param bioIndex Character vector corresponding to the biological index column name.
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = "FALSE".
#' @param outcaseColName If useBC == FALSE, all the sites in the sites file (NULL).
#'                       If useBC == TRUE, the name of the column that indicates
#'                       the "outside the case". Default = NULL.
#' @param outcaseLabel A label used to describe the "outside the case" region.
#'                     Examples include "Entire state" or "Omernik Level 3 Ecoregion."
#' @param incaseColName If useBC == FALSE, the name of the column in the sites
#'                      file that indicates "inside the case". Default = NULL.
#'                      If NULL, df_bcdist and bc_cutoff must be defined.
#' @param incaseLabel A label used to describe the "inside the case" region.
#'                    Examples include "Cluster" or "Sites with expected biologic
#'                    similarity from cluster x" where x refers to the cluster
#'                    membership of the target site.
#' @param useAllCompReaches Whether all comparator reaches should be used (FALSE)
#'                          or only those with sites on them (TRUE).
#' @param df_bcdist Dataframe containing the biological dissimilarity distance matrix.
#'                  Default = NULL. Must be defined if incaseColName is not.
#' @param bc_cutoff Cutoff value below which will be considered similar to the
#'                  target site. Default = "0.05".
#' @param dir_results Directory containing all results. Default = "file.path(getwd(),"Results")"
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo"
#'
#' @return A list containing 1) a vector of comparator site identifiers (comp.sites),
#'         2) a vector of all sites representing "outside the case", and 3) a dataframe
#'         containing gap details (function name, condition, result, and comment);
#'         and a tab-delimited text file containing the list of comparator sites with
#'         additional information relating to their similarity to the target site.
#'
#' @keywords internal
#'
#' @export
getComparators<- function(TargetSiteID,
                          df_sites,
                          df_cluster,
                          df_bioCoOccur,
                          bioIndex,
                          useBC = FALSE,
                          outcaseColName = NULL,
                          outcaseLabel = outcaseLabel,
                          incaseColName = NULL,
                          incaseLabel = incaseLabel,
                          useAllCompReaches = FALSE,
                          df_bcdist = NULL,
                          bc_cutoff = 0.05,
                          dir_results,
                          dir_sub = "SiteInfo") {##FUNCTION.START

  # For QC purposes
  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) { # Specific to WA state
    TargetSiteID = TargetSiteID
    df_sites = data_Sites
    df_cluster = data_cluster # need if useAllCompReaches == TRUE
    df_bioCoOccur = data_bmiCoOccur
    bioIndex = bmiIndex
    useBC = useBC
    outcaseColName = "OutcaseCol"
    outcaseLabel = "Entire state"
    incaseColName = "IncaseCol"
    incaseLabel = "ClusterID"
    useAllCompReaches = useAllCompReaches
    df_bcdist = NULL
    bc_cutoff = 0.05
    dir_results = dir_results
    dir_sub = "SiteInfo"
  }

  # define pipe
  `%>%` <- dplyr::`%>%`

  fn.compsites <- file.path(dir_results, TargetSiteID, dir_sub,
                            paste0(TargetSiteID, "_COMPARATORS.tab"))

  # Write results directory ----
  out.dir <- dirname(dir_results)
  out.folders <- c(out.dir, basename(dir_results), TargetSiteID, dir_sub)

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

  TargetCOMID <- df_sites$COMID[df_sites$StationID == TargetSiteID]

  if (useBC == TRUE) { # UseBC == TRUE not tested ----

    # TODO: revisit this when another state using a biological filter wants
    # to use the CASTool for its water quality program

    # Outside the case = cluster; Inside the case uses BC distance matrix
    # eligsites are those sites having paired stressor/response samples
    eligsites <- as.vector(unique(df_bioCoOccur$StationID))

    # Get cluster to which the target site belongs (outside the case identifier)
    outcaseNum <- as.numeric(df_sites$OutcaseCol[df_sites$StationID == TargetSiteID])

    # Get vector of cluster sites (outside the case sites); subset for paired SR samples
    outcaseSites <- as.vector(df_sites$StationID[df_sites$OutcaseCol == outcaseNum])
    outcaseSites <- outcaseSites[outcaseSites %in% eligsites]

    # Subset the BC file for sites in rows = cluster sites; target site is 2nd column
    # If site ids start with a number, prepend an X so they can match colnames
    ## NOTE: might need to change this for OR/WA site ids! ####
    if (grepl("^\\d+\\w*$", TargetSiteID) == TRUE) {
      TargetColName <- paste0("X", TargetSiteID)
    } else if (grepl("-", TargetSiteID)) {
      TargetColName <- stringr::str_replace(TargetSiteID, "-", "\\.")
    } else {
      TargetColName <- TargetSiteID
    }

    df_bcdist <- df_bcdist[, c("StationID", TargetColName)]
    df_bcdist <- df_bcdist %>% filter(StationID %in% outcaseSites)
    df_bcdist <- as.data.frame(df_bcdist[order(df_bcdist[, TargetColName]), ])
    df_bcdist.temp <- df_bcdist[df_bcdist[, TargetColName] <= bc_cutoff, ]

    CompMsg1 <- paste("Number of comparator sites is", nrow(df_bcdist.temp) - 1)
    message(CompMsg1)

    # If using bc_cutoff gives <30 comp sites + target site, then take
    # the most similar 30. Offset by 1 because target site included
    num.good <- nrow(df_bcdist.temp) - 1
    if (num.good < 30) {
      df_bcdist.temp <- dplyr::top_n(df_bcdist, -31)
      gap.statement <- cbind.data.frame("getComparators",
                                        paste0("bc.dist <= ", bc_cutoff),
                                        num.good,
                                        paste("max bc.dist for ",
                                              nrow(df_bcdist.temp) - 1,
                                              "comparators = ",
                                              max(df_bcdist.temp[, TargetColName])))
      colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")
    } else {
      gap.statement <- cbind.data.frame("getComparators",
                                        paste0("bc.dist <= ", bc_cutoff),
                                        num.good,
                                        paste("max bc.dist = ",
                                              round(max(df_bcdist.temp[, TargetColName]), 4)))
      colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")
    }

    bc_cutofftxt <- ifelse((bc_cutoff * 100) < 10, paste0(0, bc_cutoff * 100),
                            bc_cutoff * 100)
    comp.sites.info <- df_bcdist.temp
    comp.sites.info$Comment <- ifelse(comp.sites.info[, TargetColName] <= bc_cutoff,
                                      paste0("OutCase_LTEQ", bc_cutofftxt),
                                      paste0("OutCase_GT", bc_cutofftxt))

    df_bioCoOccurTrim <- df_bioCoOccur[, c("StationID", "RespSampID",
                                           bioIndex, "Quality", "RespSampFlag")]
    comp.samps <- merge(comp.sites.info, df_bioCoOccurTrim)
    comp.samps <- dplyr::rename(comp.samps, BCdistance = all_of(TargetColName))
    write.table(comp.samps, fn.compsites, append = FALSE, col.names = TRUE,
                row.names = FALSE, sep = "\t")

    # Convert to vector that can be returned in the list generated
    comp.sites <- as.vector(df_bcdist.temp$StationID)
    if (useAllCompReaches == FALSE) {
      comp.reaches <- unique(as.vector(df_sites$COMID[df_sites$StationID %in% comp.sites]))
    } else {
      comp.reaches <- unique(as.vector(df_cluster$COMID[df_cluster$ClusterID == outcaseNum]))
    }
    all.sites <- outcaseSites
    all.reaches <- unique(as.vector(df_sites$COMID[df_sites$StationID %in% all.sites]))
    outcaseID <- outcaseNum
    incaseID <- NULL

  } else { # UseBC == FALSE (tested) ----

    # Outside the case = outcaseColName; Inside the case uses cluster
    outcaseValue <- df_sites$OutcaseCol[df_sites$StationID == TargetSiteID]
    df_outcase <- dplyr::select(df_sites, StationID, OutcaseCol, IncaseCol)
    outcaseSites <- as.vector(df_outcase$StationID[df_outcase$OutcaseCol == outcaseValue])

    # Get cluster to which the target site belongs (inside the case identifier)
    incaseValue <- as.numeric(df_outcase$IncaseCol[df_outcase$StationID == TargetSiteID])
    incaseSites <- unique(as.vector(df_outcase$StationID[df_outcase$IncaseCol == incaseValue]))

    # Get cluster sites also having paired stressor/response samples
    eligsites <- as.vector(unique(df_bioCoOccur$StationID))
    outcaseSites <- outcaseSites[outcaseSites %in% eligsites]
    incaseSites <- incaseSites[incaseSites %in% eligsites]

    comp.sites <- unique(incaseSites)
    if (useAllCompReaches == FALSE) { # use only comparator reaches having sites
      comp.reaches <- unique(as.vector(df_sites$COMID[df_sites$StationID %in% comp.sites]))
    } else {
      comp.reaches <- unique(as.vector(df_cluster$COMID[df_cluster$ClusterID == incaseValue]))
    }
    all.sites <- unique(outcaseSites)
    all.reaches <- unique(as.vector(df_sites$COMID[df_sites$StationID %in% all.sites]))
    statement <- paste0("All '", incaseLabel, "=", incaseValue, "' sites from '",
                        outcaseLabel, "' are used as comparators.")
    gap.statement <- cbind.data.frame("getComparators",
                                      "bc.dist not used",
                                      paste0("Inside the case: ", incaseLabel,
                                             " = ", length(incaseSites)),
                                      paste(statement))
    gap.statement <- cbind.data.frame("getComparators",
                                      "bc.dist not used",
                                      paste0("Outside the case: ", outcaseLabel,
                                             " = ", length(outcaseSites)),
                                      paste(statement))
    colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")

  }

  fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
  fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
  write.table(gap.statement, fn.gaps, append = TRUE, col.names = FALSE,
              row.names = FALSE, sep = "\t")

  CompMsg2 <- paste("Using final number of comparators =", length(comp.sites) - 1)
  message(CompMsg2)

  myCompSites <- list(TargetCOMID = TargetCOMID,
                      comp.sites = comp.sites,
                      comp.reaches = comp.reaches,
                      all.sites = all.sites,
                      all.reaches = all.reaches,
                      incaseID = incaseValue,
                      outcaseID = outcaseValue)

  return(myCompSites)

}
