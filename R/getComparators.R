#  Copyright 2023 TetraTech. All rights reserved.
#  Use, copying, modification, or distribution of this file or any of its contents
#  is expressly prohibited without prior written permission of TetraTech.
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
#' @param df_bioCoOccur Dataframe containing paired stressor/response sample data
#' @param bioIndex Character vector corresponding to the biological index column name.
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = "FALSE".
#' @param outcaseColName If useBC == FALSE, the name of the column in the sites
#'                       file that indicates "outside the case". Default = NULL.
#' @param incaseColName If useBC == FALSE, the name of the column in the sites
#'                      file that indicates "inside the case". Default = NULL.
#' @param df_bcdist Dataframe containing the biological dissimilarity distance matrix. Default = NULL.
#' @param bc_cutoff Cutoff value below which will be considered similar to the target site. Default = "0.05".
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
getComparators<- function(TargetSiteID
                          , df_sites
                          , df_bioCoOccur
                          , bioIndex
                          , useBC = FALSE
                          , outcaseColName = NULL
                          , incaseColName = NULL
                          , df_bcdist = NULL
                          , bc_cutoff = 0.05
                          , dir_results = file.path(getwd(), "Results")
                          , dir_sub = "SiteInfo"
                          ) {##FUNCTION.START

  # For QC purposes
  boo_DEBUG <- FALSE

  if (boo_DEBUG == TRUE) {
    TargetSiteID = TargetSiteID
    df_sites = data_Sites
    df_bioCoOccur = data_bmiCoOccur
    bioIndex = bmiIndex
    useBC = useBC
    outcaseColName = "OutcaseCol"
    incaseColName = NULL
    df_bcdist = data_BCdist
    bc_cutoff = 0.05
    dir_results = dir_results
    dir_sub = "SiteInfo"
  }

  # define pipe
  `%>%` <- dplyr::`%>%`

  fn.compsites <- file.path(dir_results, TargetSiteID, dir_sub
                            , paste0(TargetSiteID, "_COMPARATORS.tab"))

  ifelse(!dir.exists(file.path(dir_results, TargetSiteID)) == TRUE
         , dir.create(file.path(dir_results, TargetSiteID))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID, dir_sub)) == TRUE
         , dir.create(file.path(dir_results, TargetSiteID, dir_sub))
         , FALSE)

  if (useBC == TRUE) {

    # Outside the case = cluster; Inside the case uses BC distance matrix
    eligsites <- as.vector(unique(df_bioCoOccur$StationID_Master))

    # Get cluster to which the target site belongs (outside the case identifier)
    outcaseNum <- as.numeric(df_sites$OutcaseCol[df_sites$StationID_Master == TargetSiteID])

    # Get vector of cluster sites (outside the case sites); subset for paired SR samples
    outcaseSites <- as.vector(df_sites$StationID_Master[df_sites$OutcaseCol == outcaseNum])
    outcaseSites <- outcaseSites[outcaseSites %in% eligsites]

    # Subset the BC file for sites in rows = cluster sites; target site is 2nd column
    # If site ids start with a number, prepend an X so they can match colnames
    # NOTE: might need to change this for OR/WA site ids! ####
    if (grepl("^\\d+\\w*$", TargetSiteID) == TRUE) {
      TargetColName <- paste0("X", TargetSiteID)
    } else if (grepl("-", TargetSiteID)) {
      TargetColName <- stringr::str_replace(TargetSiteID, "-", "\\.")
    } else {
      TargetColName <- TargetSiteID
    }

    df_bcdist <- df_bcdist[, c("StationID_Master", TargetColName)]
    df_bcdist <- df_bcdist %>% filter(StationID_Master %in% outcaseSites)
    df_bcdist <- as.data.frame(df_bcdist[order(df_bcdist[, TargetColName]), ])
    df_bcdist.temp <- df_bcdist[df_bcdist[, TargetColName] <= bc_cutoff, ]

    CompMsg1 <- paste("Number of comparator sites is", nrow(df_bcdist.temp) - 1)
    message(CompMsg1)

    # If using bc_cutoff gives <30 comp sites + target site, then take
    # the most similar 30. Offset by 1 because target site included
    num.good <- nrow(df_bcdist.temp) - 1
    if (num.good < 30) {
      df_bcdist.temp <- dplyr::top_n(df_bcdist, -31)
      gap.statement <- cbind.data.frame("getComparators"
                                        , paste0("bc.dist <= ", bc_cutoff)
                                        , num.good
                                        , paste("max bc.dist for"
                                                , nrow(df_bcdist.temp) - 1
                                                , "comparators ="
                                                , max(df_bcdist.temp[, TargetColName])))
      colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")
    } else {
      gap.statement <- cbind.data.frame("getComparators"
                                        , paste0("bc.dist <= ", bc_cutoff)
                                        , num.good
                                        , paste("max bc.dist ="
                                                , round(max(df_bcdist.temp[, TargetColName]), 4)))
      colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")
    }

    bc_cutofftxt <- ifelse((bc_cutoff * 100) < 10, paste0(0, bc_cutoff * 100)
                           , bc_cutoff * 100)
    comp.sites.info <- df_bcdist.temp
    comp.sites.info$Comment <- ifelse(comp.sites.info[,TargetColName] <= 0.05
                                      , paste0("OutCase_LTEQ", bc_cutofftxt)
                                      , paste0("OutCase_GT", bc_cutofftxt))

    df_bioCoOccurTrim <- df_bioCoOccur[,c("StationID_Master", "RespSampID"
                                          , bioIndex, "Quality", "RespSampFlag")]
    comp.samps <- merge(comp.sites.info, df_bioCoOccurTrim)
    comp.samps <- dplyr::rename(comp.samps, BCdistance = all_of(TargetColName))
    write.table(comp.samps, fn.compsites, append = FALSE
                , col.names = TRUE, row.names = FALSE, sep = "\t")

    # Convert to vector that can be returned in the list generated
    comp.sites <- as.vector(df_bcdist.temp$StationID_Master)
    all.sites <- outcaseSites
    outcaseID <- outcaseNum

  } else {

    # Outside the case = outcaseColName; Inside the case uses cluster
    outcaseNum <- as.numeric(df_sites$outcaseColName[df_sites$StationID_Master == TargetSiteID])
    df_outcase <- dplyr::select(df_sites, StationID_Master, all_of(outcaseColName), all_of(incaseColName))
    outcaseSites <- as.vector(df_outcase$StationID_Master[df_outcase$outcaseColName == outcaseNum])

    # Get cluster to which the target site belongs (inside the case identifier)
    incaseNum <- as.numeric(df_outcase$incaseColName[df_outcase$StationID_Master == TargetSiteID])
    incaseSites <- as.vector(df_outcase$StationID_Master[df_outcase$incaseColName == incaseNum])

    # Get cluster sites also having paired stressor/response samples
    eligsites <- as.vector(unique(df_bioCoOccur$StationID_Master))
    outcaseSites <- outcaseSites[outcaseSites %in% eligsites]
    incaseSites <- incaseSites[incaseSites %in% eligsites]
    outcaseSites <- !(outcaseSites %in% incaseSites)

    comp.sites <- incaseSites
    all.sites <- outcaseSites
    outcaseID <- outcaseNum
    statement <- "All cluster sites are used as comparators."
    gap.statement <- cbind.data.frame("getComparators"
                                      , "bc.dist not used"
                                      , length(incaseSites)
                                      , paste(statement))
    colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")

  }

  fn.gaps <- paste0(TargetSiteID, "_datagaps.tab")
  fn.gaps <- file.path(dir_results, TargetSiteID, fn.gaps)
  write.table(gap.statement, fn.gaps, append = TRUE, col.names = FALSE
              , row.names = FALSE, sep = "\t")

  CompMsg2 <- paste("Using final number of comparators =", length(comp.sites) - 1)
  message(CompMsg2)

  myCompSites <- list(comp.sites = comp.sites
                      , all.sites = all.sites
                      , outcaseID = outcaseID)

  return(myCompSites)

}
