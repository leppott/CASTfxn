#' @title Get Comparator Sites
#' 
#' @description Generates a list of comparator sites for the target site.
#' 
#' @details Obtains the cluster members for the cluster containing the target site
#'          and, if useBC = TRUE (default is FALSE), subsets the cluster
#'          based on a biological dissimilarity distance matrix (Bray-Curtis)
#'          at the specified level of dissimilarity.
#' 
#' Uses the library dplyr
#' 
#' @param TargetSiteID Site ID
#' @param df_sites Sites table with cluster membership. Default = "data_Sites".
#' @param useBC TRUE to use biological similarity; FALSE to not use. Default = "TRUE".
#' @param df_bcdist Dataframe containing the biological dissimilarity distance matrix. Default = "data_BCdist".
#' @param bc_cutoff Cutoff value below which will be considered similar to the target site. Default = "0.05".
#' @param dir_results Directory containing all results. Default = "file.path(getwd(),"Results")"
#' @param dir_sub Subdirectory for outputs from this function.  Default = "SiteInfo"
#' 
#' @return A list containing a vector of comparator site identifiers (comp.sites) 
#'         and a dataframe of containing the function name, condition, result, and 
#'         comment. Also a tab-delimited text file containing the list of comparator
#'         sites with additional information relating to how they were chosen.
#' 
#' @keywords internal
#' 
#' @export
getComparators<- function(TargetSiteID
                          , df_sites = data_Sites
                          , useBC = FALSE
                          , df_bcdist = data.BCdist
                          , bc_cutoff = 0.05
                          , dir_results = file.path(getwd(), "Results")
                          , dir_sub = "SiteInfo") {
  # For QC purposes
  # TargetSiteID = TargetSiteID
  # df_sites = data_Sites
  # useBC = FALSE
  # df_bcdist = data.BCdist
  # bc_cutoff = 0.05
  # dir_results = file.path(wd, "Results")
  # dir_sub = "SiteInfo"
  
  # define pipe
  `%>%` <- dplyr::`%>%`
  
  fn.compsites <- file.path(dir_results, TargetSiteID, dir_sub
                            ,paste0(TargetSiteID,"_Compsites.tab"))
  
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID))==TRUE
         , dir.create(file.path(dir_results, TargetSiteID))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID, dir_sub))==TRUE
         , dir.create(file.path(dir_results, TargetSiteID, dir_sub))
         , FALSE)
  
  # Get cluster to which the target site belongs
  clustnum <- as.numeric(df_sites$clust[df_sites$StationID_Master == TargetSiteID])
  
  # Get vector of cluster sites
  clustsites <- as.vector(df_sites$StationID_Master[df_sites$clust == clustnum])
  
  # a Bray-Curtis dissimilarity matrix is provided, use it
  if (useBC == TRUE) {
    
    # Subset the BC file for sites in rows = cluster sites; target site is 2nd column
    if (grepl("^\\d+\\w*$", TargetSiteID)==TRUE) {
      TargetColName <- paste0("X",TargetSiteID)
    } else {
      TargetColName <- TargetSiteID
    }
    
    df_bcdist <- df_bcdist[,c("StationID_Master",TargetColName)]
    df_bcdist <- df_bcdist %>% filter(StationID_Master %in% clustsites)
    df_bcdist <- as.data.frame(df_bcdist[order(df_bcdist[,TargetColName]),])
    df_bcdist.temp <- df_bcdist[df_bcdist[,TargetColName] <= bc_cutoff,]
    
    CompMsg1 <- paste("Number of comparator sites is", nrow(df_bcdist.temp)-1)
    message(CompMsg1)
    
    # If using bc_cutoff gives <30 comp sites + target site, then take 
    # the most similar 30. Offset by 1 because target site included
    num.good <- nrow(df_bcdist.temp)-1
    if (num.good<30) {
      df_bcdist.temp <- dplyr::top_n(df_bcdist, -31)
      gap.statement <- cbind.data.frame("getComparators"
                                        , "bc.dist <= 0.05"
                                        , num.good
                                        , paste("max bc.dist for", nrow(df_bcdist.temp)-1
                                                , "comparators ="
                                                , max(df_bcdist.temp[,TargetColName])))
      colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")
    } else {
      gap.statement <- cbind.data.frame("getComparators"
                                        , "bc.dist <= 0.05"
                                        , num.good
                                        , paste("max bc.dist ="
                                                , max(df_bcdist.temp[,TargetColName])))
      colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")
    }
    
    comp.sites.info <- df_bcdist.temp
    comp.sites.info$Comment <- ifelse(comp.sites.info[,TargetColName] <= 0.05
                                      , "Cluster_LTEQ05", "Cluster_GT05")
    write.table(comp.sites.info, fn.compsites, append = FALSE
                , col.names = TRUE, row.names = FALSE, sep = "\t")
    
    # Convert to vector that can be returned in the list generated
    comp.sites <- as.vector(df_bcdist.temp$StationID_Master)
    
  } else {
    comp.sites <- clustsites
    statement <- "All cluster sites are used as comparators."
    gap.statement <- cbind.data.frame("getComparators"
                                      , "bc.dist not used"
                                      , length(clustsites)
                                      , paste(statement))
    colnames(gap.statement) <- c("fxnname", "condition", "result", "comment")
  }
  
  fn.gaps <- paste0(TargetSiteID,"_datagaps.tab")
  fn.gaps <- file.path(getwd(),"Results",TargetSiteID,fn.gaps)
  write.table(gap.statement, fn.gaps, append = TRUE, col.names = FALSE
              , row.names = FALSE, sep = "\t")
  
  CompMsg2 <- paste("Using final number of comparators =", length(comp.sites)-1)
  message(CompMsg2)
  
  myCompSites <- list(comp.sites= comp.sites
                      , gap.compsites = gap.statement)
  
  return(myCompSites)
  
}