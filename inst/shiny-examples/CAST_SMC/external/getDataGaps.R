#' @title Get Data Gaps
#' 
#' @description Assembles data gaps available from other functions in the package.
#' 
#' @details Each analytical function identifies data gaps, if possible. This 
#' function pulls together those results into one tab-delimited text file.
#' Improvements: Merge gap_chemstress and gap_phabstress into gapMeasStress
#' 
#' Uses the libraries dplyr and tidyr.
#' 
#' @param TargetSiteID Site ID
#' @param gap_compsites Data gaps identified by getComparators
#' @param gap_chemstress Data gaps in chemistry samples
#' @param gap_phabstress Data gaps in phab samples
#' @param gap_modstress Data gaps in modeled stressors
#' @param cluster_chem Measured stressor data for all comparator samples
#' @param stressors List of stressors identified for additional analysis
#' @param biocomm Biological community; algae or BMI. Default = "BMI".
#' @param list_biomatches List containing vectors of matched stressor-response 
#' data for the entire data set, the comparator data set, and the target site only.
#' @param dir_results Directory containing all results. Default = "file.path(getwd(),"Results")".
#' @param dir_sub Subdirectory for outputs from this function. Default = "SiteInfo".
#' 
#' @return A tab-delimited data file containing any identified data gaps.
#' 
#' @keywords internal
#' 
#' @export
getDataGaps<- function(TargetSiteID
                       , gap_compsites = list.CompSites$gap.compsites
                       , gap_chemstress
                       , gap_phabstress
                       , gap_modstress
                       , cluster_chem
                       , stressors
                       , biocomm = "BMI"
                       , list_biomatches = list.bmi.matches
                       , dir_results = file.path(getwd(), "Results")
                       , dir_sub = "SiteInfo") {
  
  # QC
  # TargetSiteID
  # gap_compsites = list.CompSites$gap.compsites
  # gap_chemstress = gap.chem.stress
  # gap_phabstress = gap.phab.stress
  # gap.meas.stress = gap.meas.stress
  # gap_modstress = gap.mod.stress
  # cluster_chem = cluster.chem
  # stressors = stressors
  # biocomm = "bmi"
  # list_biomatches = list.bmi.matches
  # list.alg.matches
  # dir_results = file.path(wd, "Results")
  # dir_sub = "SiteInfo"
  
  # Define pipe
  `%>%` <- dplyr::`%>%`
  
  biocomm <- tolower(biocomm)
  
  # Check for presence of SiteInfo directory. If not present, create
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID))==TRUE
         , dir.create(file.path(dir_results, TargetSiteID))
         , FALSE)
  ifelse(!dir.exists(file.path(dir_results, TargetSiteID, dir_sub))==TRUE
         , dir.create(file.path(dir_results, TargetSiteID, dir_sub))
         , FALSE)
  
  path <- file.path(dir_results, TargetSiteID, dir_sub)
  fn.gaps <- paste0(TargetSiteID,".datagaps.tab")
  
  # Identify data gaps due to insufficient sample data in the comparator set
  # This uses cluster_chem (all detects, regardless of response data)
  cl.chem.count <- as.data.frame(colSums(!is.na(cluster_chem)))
  cl.chem.count <- as.data.frame(cbind(row.names(cl.chem.count)
                                       , cl.chem.count))
  colnames(cl.chem.count) <- c("Stressor","SampCount")
  rownames(cl.chem.count) <- NULL
  cl.chem.count <- filter(cl.chem.count, SampCount < 20)
  if (nrow(cl.chem.count) < 3) {
    comm = "There are sufficient samples in the comparator set."
    gap.temp <- cbind.data.frame(fxnname = "general dataset"
                                 , condition = "stressor samples"
                                 , result = 1
                                 , comment = comm)
    gap.str.data <- as.data.frame(gap.temp)
  } else {
    cl.chem.count <- cl.chem.count[3:nrow(cl.chem.count),]
    if (nrow(cl.chem.count)>0) {
      for (chem in 1:nrow(cl.chem.count)) {
        comm = "Insufficient samples in the comparator set."
        gap.temp <- cbind.data.frame(fxnname = "general dataset"
                                     , condition = cl.chem.count[chem,1]
                                     , result = cl.chem.count[chem,2]
                                     , comment = comm)
        if (chem == 1) {
          gap.str.data <- as.data.frame(gap.temp)
        } else {
          gap.str.data <- rbind(gap.str.data
                                , as.data.frame(gap.temp))
        }
      }
    }
  }
  if (exists("gap.temp")) { rm(gap.temp) }
  
  # If no stressors are identified, no analyses can be performed. Error msg.
  if ((nrow(gap.str.data)==1) && 
      (gap.str.data$comm == "There are sufficient samples in the comparator set")) {
    # No need to evaluate whether or not identified stressors have enough
    # samples in comparator set, because all detected ones have enough
    comm = paste("All stressors listed as candidate causes"
                 , "have sufficient comparator data.")
    gap.temp <- cbind.data.frame(fxnname = "getStressorList"
                                 , condition = "candidate causes"
                                 , result = 1
                                 , comment = comm)
    gap.stressor <- as.data.frame(gap.temp)
  } else {
    if (length(stressors) > 0) {
      # Identify if any stressors have too few samples to be reliable
      stressor.count <- cl.chem.count %>%
        filter(Stressor %in% stressors)
      if (nrow(stressor.count)>0) {
        for (chem in 1:nrow(stressor.count)) {
          comm = paste("Identified possible stressor from uncertain"
                       , "distribution (insufficient samples).")
          gap.temp <- cbind.data.frame(fxnname = "getStressorList"
                                       , condition = stressor.count[chem,1]
                                       , result = stressor.count[chem,2]
                                       , comment = comm)
          if (chem == 1) {
            gap.stressor <- as.data.frame(gap.temp)
          } else {
            gap.stressor <- rbind(gap.stressor
                                  , as.data.frame(gap.temp))
          }
        }
      }
    } else {    # No stressors identified
      comm = paste("No stressors are listed as potential candidate causes.")
      gap.temp <- cbind.data.frame(fxnname = "getStressorList"
                                   , condition = "candidate causes"
                                   , result = 0
                                   , comment = comm)
      gap.stressor <- as.data.frame(gap.temp)
    }
  } 
  if (exists("gap.temp")) { rm(gap.temp) }
  
  # Identify data gaps due to insufficient sample data in the comparator set
  # This uses paired stressor/response data (just the stressor is checked)
  if (biocomm == "bmi") {
    cl.b.str <- list_biomatches$cl.b.str
    cl.b.count <- as.data.frame(colSums(!is.na(cl.b.str)))
    cl.b.count <- as.data.frame(cbind(row.names(cl.b.count)
                                      , cl.b.count))
    colnames(cl.b.count) <- c("Stressor","SampCount")
    rownames(cl.b.count) <- NULL
    if (any(cl.b.count$SampCount<20)==TRUE) {
      cl.b.count <- filter(cl.b.count, Stressor %in% stressors)
      cl.b.count <- filter(cl.b.count, SampCount < 20)
      for (chem in 1:nrow(cl.b.count)) {
        comm = "Insufficient paired samples in the comparator set."
        gap.temp <- cbind.data.frame(fxnname = "getBioMatches--bmi"
                                     , condition = cl.b.count[chem,1]
                                     , result = cl.b.count[chem,2]
                                     , comment = comm)
        if (chem == 1) {
          gap.b.data <- as.data.frame(gap.temp)
        } else {
          gap.b.data <- rbind(gap.b.data
                              , as.data.frame(gap.temp))
        }
        if (exists("gap.temp")) { rm(gap.temp) }
      }
    } else {    # No candidate cause has <20 paired samples
      comm = paste("All candidate causes have sufficient paired"
                   , "stressor/bmi response data.")
      gap.temp <- cbind.data.frame(fxnname = "getBioMatches--bmi"
                                   , condition = "paired stress/bmi data"
                                   , result = 1
                                   , comment = comm)
      gap.b.data <- as.data.frame(gap.temp)
    }
  } else {
    # list_biomatches does not exist
  }
  if (exists("gap.temp")) { rm(gap.temp) }
  
  if (biocomm == "alg") {
    cl.a.str <- list_biomatches$cl.a.str
    cl.a.count <- as.data.frame(colSums(!is.na(cl.a.str)))
    cl.a.count <- as.data.frame(cbind(row.names(cl.a.count)
                                      , cl.a.count))
    colnames(cl.a.count) <- c("Stressor","SampCount")
    rownames(cl.a.count) <- NULL
    if (any(cl.a.count$SampCount<20)==TRUE) {
      cl.a.count <- filter(cl.a.count, SampCount < 20)
      cl.a.count <- cl.a.count[3:nrow(cl.a.count),]
      for (chem in 1:nrow(cl.a.count)) {
        comm = "Insufficient paired samples in the comparator set."
        gap.temp <- cbind.data.frame(fxnname = "getBioMatches--algae"
                                     , condition = cl.a.count[chem,1]
                                     , result = cl.a.count[chem,2]
                                     , comment = comm)
        if (chem == 1) {
          gap.a.data <- as.data.frame(gap.temp)
        } else {
          gap.a.data <- rbind(gap.a.data
                              , as.data.frame(gap.temp))
        }
        if (exists("gap.temp")) { rm(gap.temp) }
      }
    } else {    # No candidate cause has <20 paired samples
      comm = paste("All candidate causes have sufficient paired"
                   , "stressor/alg response data.")
      gap.temp <- cbind.data.frame(fxnname = "getBioMatches--algae"
                                   , condition = "paired stress/algae data"
                                   , result = 1
                                   , comment = comm)
      gap.a.data <- as.data.frame(gap.temp)
    }
  } else {
    # list_biomatches does not exist
  }
  if (exists("gap.temp")) { rm(gap.temp) }
  
  # Combine all data gap info into one data frame
  data.gaps <- gap_compsites
  
  # If no chemistry stressors
  if (exists("gap_chemstress")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap.meas.stress)
    } else {
      data.gaps <- gap.meas.stress
    }
  }
  
  # If no habitat stressors
  if (exists("gap_phabstress")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap.meas.stress)
    } else {
      data.gaps <- gap.meas.stress
    }
  }
  
  # If no modeled stressors
  if (exists("gap_modstress")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap_modstress)
    } else {
      data.gaps <- gap_modstress
    }
  }
  
  # If no BMI responses
  if (exists("gap.bmi.resp")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap.bmi.resp)
    } else {
      data.gaps <- gap.bmi.resp
    }
  }
  
  # If no Alg responses
  if (exists("gap.alg.resp")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap.bmi.resp)
    } else {
      data.gaps <- gap.bmi.resp
    }
  }
  
  # If stressors w/LT20 samples, regardless of pairing
  if (exists("gap.str.data")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap.str.data)
    } else {
      data.gaps <- gap.str.data
    }
  }
  
  # If stressors w/LT20 samples selected as candidate causes
  if (exists("gap.stressor")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap.stressor)
    } else {
      data.gaps <- gap.stressor
    }
  }
  
  # If candidate causes w/LT20 paired samples among comparators w/bmi resp
  if (exists("gap.b.data")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap.b.data)
    } else {
      data.gaps <- gap.b.data
    }
  }
  
  # If candidate causes w/LT20 paired samples among comparators w/alg resp
  if (exists("gap.a.data")==TRUE) {
    if (exists("data.gaps")==TRUE) {
      data.gaps <- rbind(data.gaps, gap.b.data)
    } else {
      data.gaps <- gap.b.data
    }
  }
  
  write.table(data.gaps, file.path(path, fn.gaps)
              , append = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")   
  
}