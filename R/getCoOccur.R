# library(dplyr)
# library(ggplot2)
# library(gridExtra)
#
#' @title Co-Occurrence Plots
#' 
#' @description Generates a box plots and stressor response plots (individually as jpg 
#' and together as a PDF) as well as scores for co-occurence.
#' 
#' @details \strong{Derive evidence fo spatial/temporal co-occurrence.}
#' 
#' Are higher levels of the stressor observed where and when the biological 
#' effect occurs?
#' 
#' Box plots are used to show the distribution of the stressor levels at compartor 
#' sites with better biological condition.  If a site has multiple biological 
#' condition scores the lowest score is used to determine "better" sites.
#' 
#' Samples are scored:
#' 
#' 1. Supports the case for candidate cause.  Stressor levels at the test sites 
#' are above the 75th percentile of comparator sites having higher biological 
#' quality.
#' 
#' 0. Indeterminate.  Stressor levels at the test site are below the 50th 
#' percentile of comparator sites having higher biological quality.
#' 
#' -1. Weakens the case for the candidate cause.  Stressor levels at the test 
#' sites are between the 50th and 75th percentile of comparator sites having 
#' higher biological quality.
#' 
#' \strong{Derive Evidence for Stressor-Response Relationships from Field Observational 
#' Studies.}
#' 
#' Stressor-response from field observational studies:  Is the level of the 
#' stressor sufficient to explain the level of biological effect observed at the
#'  site?  
#' 
#' Using all comparator sites, fit logistical regression curve of the probability 
#' of poor condition (i.e., poor California index score) as a function of 
#' stressor level.  Compare stressor levels from test site to levels 
#' corresponding to median (50%) and low (20%) probabilities of observing poor 
#' condition.
#' 
#' 1. Supports the case for the candidate cause.  Stressor levels at the test 
#' site are above the lower confidence limit  (LCL) corresponding to 50% 
#' probability of observing poor condition 
#' 
#' 0. Indeterminate.  Stressor levels at the test site are between the LCL 
#' corresponding to 50% probability of observing poor condition and the UCL 
#' corresponding to 20% probability of observing poor condition. 
#' 
#' -1. Weakens the case for the candidate cause.  Stressor levels at the test 
#' site are below the upper confidence limit (UCL) corresponding to 20% 
#' probability of observing poor condition. 
#' 
#' Cut function is used to assign narrative categories and degraded status based 
#' on provided biological score.
#' Ensures criteria are applied the same across all sites.
#' 
#' The Bio.Deg.Lab has to remain as the default values of Yes and No.  
#' Other values will break the code.
#' 
#' Only a single biological measurement is used.  But multiple stressors can be
#'  used.
#' 
#' Uses the libraries dplyr, wrapr, ggplot2, and gridExtra.
#' 
#' @param df.data data frame with data.
#' @param TargetSiteID ID of station/sample to plot; can be single or multiple.  
#' Default is first entry in df.data[, col.ID]
#' @param col.ID df.data column with unique Station/Sample identifier.
#' @param col.Group df.data column with grouping variable.
#' @param col.Bio df.data column with biological numeric value.
#' @param col.Stressors df.data column(s) with stressor variable(s); can be 
#' single or multiple.
#' @param Bio.Nar.Brk Biological assessment narrative, cut function breaks.
#' Should be in order from bad (low) to good (high). 
#' Default = c(-2, 0.62, 0.799, 0.919, 2)
#' @param Bio.Nar.Lab Biological assessment narrative, cut function labels.
#' Should be in order from bad (low) to good (high).  
#' Default = c("very likely altered", "likely altered", "possibly altered ", 
#' "likely intact")
#' @param Bio.Deg.Brk Biological assessment degraded status, cut function breaks. 
#' Should be in order from bad (low) to good (high). 
#' Default = c(-2, 0.799, 2)
#' @param Bio.Deg.Lab Biological assessment degraded status, cut function labels. 
#' Should be in order from bad (low) to good (high).
#' Defaults are referenced in the code so if change the code will break. 
#' Default = c("Yes", "No").
#' @param biocomm Biological community; algae or BMI.  Default = "BMI".
#' @param dir.plots Directory to save plots.  Default = working directory and Results.
#' @param dir_sub Subdirectory for outputs from this function.  Default = "CoOccurrence"
#' @param col.Stressor.InvSc Stressors as columns of df.data that have inverse scoring for box plots.  
#' Default = pH and DO; c("DO_f_.", "DO_f_mg_L", "DO_f_unk", "DOSat_f_."
#' , "DOSat_f_unk", "DO_uf_mg_L", "pH", "pH_SU")
#'
#' @return Saves a single PDF of all plots, individual plots as JPGs, and a 
#' scores files (tab separated text file) to a user defined 'Results' directory 
#' in a 'CoOccurrence subdirectory.  A sub-directory is created under 'Results' 
#' for each SiteID in TargetSiteID.
#' 
#' @examples
#' # Example #1, CA data (multiple sites)
#' #
#' #Load Data
#' df.data <- data_CoOccur_CA
#' #
#' col.Group     <- "Group"
#' col.Bio       <- "CSCI"
#' col.Stressors <- c("DO_uf_mg_L", "TN_uf_mg_L", "TP_mg_L")
#' col.ID        <- "StationID_Master"
#' #
#' Bio.Nar.Brk <- c(-2, 0.62, 0.799, 0.919, 2)
#' Bio.Nar.Lab <- c("very likely altered", "likely altered"
#'                 , "possibly altered ", "likely intact")
#' Bio.Deg.Brk <- c(-2, 0.799, 2)
#' Bio.Deg.Lab <- c("Yes", "No")
#' biocomm <- "bmi"
#' dir.plots <- file.path(getwd(), "Results")
#' dir_sub <- "CoOccurrence"
#' #
#' TargetSiteID <- c("SMC08335", "901SJSJC9", "911TCAM01", "403STC004")
#' #
#' # Specify stressors by name
#' col.Stressors.InvSc <- c("DO_uf_mg_L", "pH")
#' 
#' #
#' getCoOccur(df.data, TargetSiteID, col.ID, col.Group, col.Bio, col.Stressors
#'         , Bio.Nar.Brk, Bio.Nar.Lab, Bio.Deg.Brk, Bio.Deg.Lab 
#'         , biocomm, dir.plots, dir_sub, col.Stressors.InvSc
#'         )
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' # Example #2, AZ data (single site)
#' #
#' TargetSiteID <- c("SRCKN001.61")
#' #
#' # Cluster Data based on elevation category
#' boo_Lo <- TargetSiteID %in% data_CoOccur_AZ_Lo$StationID_Master
#' if(boo_Lo==TRUE){
#'    df.data <- data_CoOccur_AZ_Lo
#' } else {
#'    df.data <- data_CoOccur_AZ_Hi
#' }
#' #
#' col.Group     <- "Group"
#' col.Bio       <- "IBI"
#' col.Stressors <- c("Calcium_uf_mg_L", "Copper_uf_ug_L", "DO_f_mg_L", "SpecCond_umhos_cm")
#' col.ID        <- "StationID_Master"
#' #
#' Bio.Nar.Brk <- c(0, 45, 52, 100)
#' Bio.Nar.Lab <- c("Most Disturbed", "Intermediate", "Least Disturbed")
#' Bio.Deg.Brk <- c(0, 45, 100)
#' Bio.Deg.Lab <- c("Yes", "No")
#' biocomm <- "bmi"
#' dir.plots <- file.path(getwd(), "Results")
#' dir_sub <- "CoOccurrence"
#' 
#' # Specify stressors by name
#' #col.Stressors.InvSc <- c("DO_f_.", "DO_f_mg_L", "DO_f_unk", "DOSat_f_.", "DOSat_f_unk", "pH_SU")
#' # Get stressors from chem.info
#' col.Stressors.InvSc <- data_ChemInfo[data_ChemInfo[, "DirIncStress"] == "Dec", "StdParamName"] 
#' 
#' #
#' getCoOccur(df.data, TargetSiteID, col.ID, col.Group, col.Bio, col.Stressors
#'         , Bio.Nar.Brk, Bio.Nar.Lab, Bio.Deg.Brk, Bio.Deg.Lab
#'         , biocomm, dir.plots, dir_sub, col.Stressors.InvSc
#'         )
#' 
#~~~~~~~~~~~~~
#' @export
getCoOccur <- function(df.data
                       , TargetSiteID=NULL
                       , col.ID
                       , col.Group
                       , col.Bio
                       , col.Stressors
                       , Bio.Nar.Brk=c(-2, 0.62, 0.799, 0.919, 2)
                       , Bio.Nar.Lab=c("very likely altered", "likely altered"
                                       , "possibly altered ", "likely intact")
                       , Bio.Deg.Brk=c(-2, 0.799, 2)
                       , Bio.Deg.Lab=c("Yes", "No")
                       , biocomm="bmi"
                       , dir.plots=file.path(getwd(), "Results")
                       , dir_sub="CoOccurrence"
                       , col.Stressors.InvSc=c("DO_f_."
                                               , "DO_f_mg_L"
                                               , "DO_f_unk"
                                               , "DOSat_f_."
                                               , "DOSat_f_unk"
                                               , "DO_uf_mg_L"
                                               , "pH"
                                               , "pH_SU")
                       ) {##FUNCTION.START
  #
  boo_DEBUG <- FALSE
  
  # define pipe
  `%>%` <- dplyr::`%>%`
  
  # QC, 20190418
  col.Stressors <- unique(col.Stressors)
  
  # QC, 20190418
  col.Stressors.NotPresent <- col.Stressors[!(col.Stressors %in% names(df.data))]
  if(length(col.Stressors.NotPresent)!=0){##IF~bad stressors~START
    msg.warning <- paste0("Stressors listed below are not present in the provided data frame (df.data) and were not analyzed: \n"
                           , paste(col.Stressors.NotPresent, collapse="\n"), "\n\n")
    message(msg.warning)
    utils::flush.console()
    col.Stressors <- col.Stressors[col.Stressors %in% names(df.data)]
  }##IF~bad stressors~END
  
  # # Parameters (stressors) with inverse scoring
  # # 2019-05-20 (same day added as input variables)
  # col.Stressors.InvSc <- c("DO_f_."
  #                          , "DO_f_mg_L"
  #                          , "DO_f_unk"
  #                          , "DOSat_f_."
  #                          , "DOSat_f_unk"
  #                          , "pH_SU")

  #
  myDateTime    <- format(Sys.time(),"%Y%m%d_%H%M%S")
  col.Bio.Nar   <- "Bio.Nar"
  col.Bio.Deg   <- "Bio.Deg"
  #
  col.KEEP      <- c(col.ID, col.Group, col.Bio, col.Bio.Nar, col.Bio.Deg, col.Stressors)
  #
  # Assign Bio Narrative and Status
  df.data[, col.Bio.Nar] <- cut(df.data[,col.Bio]
                                , breaks=Bio.Nar.Brk
                                , labels=Bio.Nar.Lab)
  df.data[, col.Bio.Deg] <- cut(df.data[,col.Bio]
                                , breaks=Bio.Deg.Brk
                                , labels=Bio.Deg.Lab)
  
  # Change Levels (factors) as 1=No and 2=Yes
  ## Used to later convert to 0=No (not degraded) and 1=Yes (degraded)
  df.data$Bio.Deg <- factor(df.data$Bio.Deg, c("No", "Yes"))
  
  # Add missing variable
  col.SiteTypeQuality <- col.Bio.Deg
  #
  # default sample ID
  if(is.null(TargetSiteID)){##IF.isnull.ID.START
    TargetSiteID <- as.character(sort(unique(df.data[,col.ID])))[1]
  }##IF.isnull.ID.END
  
  
  # Create Score Output File
  df.scores <- df.data[, col.KEEP]
  # # Add necessary Fields
  # for (jj in col.Stressors){##FOR.jj.START
  #   df.scores[,paste0("n_",jj)]       <- as.character(NA)
  #   df.scores[,paste0("q25_",jj)]     <- as.character(NA)
  #   df.scores[,paste0("q50_",jj)]     <- as.character(NA)
  #   df.scores[,paste0("q75_",jj)]     <- as.character(NA)
  #   df.scores[,paste0("Sc_Comp_",jj)] <- as.character(NA)
  # }##FOR.jj.END
  # could use apply
  #
  # Add columns
  df.scores[, "Param_Name"]  <- as.character(NA)
  df.scores[, "Param_Value"] <- as.numeric(NA)
  df.scores[, "n"]           <- as.character(NA)
  df.scores[, "q25"]         <- as.character(NA)
  df.scores[, "q50"]         <- as.character(NA)
  df.scores[, "q75"]         <- as.character(NA)
  df.scores[, "Sc_Box"]      <- as.character(NA)
  df.scores[, "SR_pred_Deg"] <- as.character(NA)
  df.scores[, "Sc_SR"]       <- as.character(NA)
  df.scores[, "biocomm"]     <- as.character(NA)
  # Remove columns
  col.remove <- names(df.scores) %in% col.Stressors
  df.scores <- df.scores[, !col.remove]
  #
  # remove all rows
  df.scores <- df.scores[0, ]
  
  

  #par
#  par.orig <- par(no.readonly=TRUE)
  # reset with "par(par.orig)"
  # using ggplot so don't need par
  
  
  # QC Test

  # num.ID        <- sum(TargetSiteID %in% df.data[,col.ID])
  # num.Stressors <- sum(col.Stressors %in% names(df.data))
  # num.Groups    <- length(unique(df.data[,col.Group]))
  # 
  # print(paste0("Items to process; Samples/Stations (n=",num.ID,")."))
  # print(paste0("Items to process; Stressors (n=",num.Stressors,")."))
  # print(paste0("Items to process; Groups (n=",num.Groups,")."))

  #
  if(boo_DEBUG==TRUE){##IF.boo_DEBUG.START
    i <- TargetSiteID[1] 
  }##IF.boo_DEBUG.END
  # outside loop just in case forget to turn off debug flag

  # Analysis for each "test" sample
  # Loop, i ####
  for (i in TargetSiteID){##FOR.i.START
    #
    i_TargetSiteID <- i
    #
    # QC (site in data) ####
    boo_QC_site <- i_TargetSiteID %in% df.data[, col.ID]
    if(boo_QC_site==FALSE){##IF~boo_QC_site~START
      name_df <- deparse(substitute(df.data))
      name_col <- deparse(substitute(col.ID))
      name_df_col <- paste0(name_df, name_col)
      msg_NoSite <- paste0("Target site (", i_TargetSiteID, ") was *not* found in the function inputs (df.data, TargetSiteID, and col.ID).")
      stop(msg_NoSite)
    }##IF~boo_QC_site~END
    #
    #wd <- getwd()
    #dir.sub <- "Results"
    dir.sub2 <- i_TargetSiteID
    dir.sub3 <- dir_sub
    ifelse(!dir.exists(file.path(dir.plots, dir.sub2))==TRUE
           , dir.create(file.path(dir.plots, dir.sub2))
           , FALSE)
    ifelse(!dir.exists(file.path(dir.plots, dir.sub2, dir.sub3))==TRUE
           , dir.create(file.path(dir.plots, dir.sub2, dir.sub3))
           , FALSE)
    # PDF, old ####
    #fn.pdf    <- paste0(TargetSiteID, ".CoOccurrence.ALL.", myDateTime,".pdf")
    # fn.pdf    <- paste0(TargetSiteID, ".CoOccurrence.ALL.pdf")
    # if(boo_DEBUG==FALSE){##IF.boo_DEBUG.START
    #   grDevices::pdf(file=file.path(wd, dir.sub, dir.sub2, fn.pdf), width=6, height=8)
    # }##IF.boo_DEBUG.END
    plots_pdf <- vector(1, mode="list")
    plots_jpg <- vector(2, mode="list")
    # #
    # Save scores file (append to later)
    # fn.scores <- file.path(wd, dir.sub, dir.sub2, paste0(TargetSiteID,".CoOccurrence.Scores.", myDateTime,".txt"))
    fn.scores <- file.path(dir.plots, dir.sub2, dir.sub3, paste0(i_TargetSiteID, ".CoOccurrence.", biocomm, ".Scores.txt"))
    utils::write.table(df.scores, file=fn.scores
                , col.names = TRUE, row.names=FALSE, sep="\t")
    #
    i.num <- match(i, TargetSiteID)
    i.len <- length(TargetSiteID)
    #
    df.i <- df.data[df.data[,col.ID]==i, col.KEEP]
    i.Group <- df.i[,col.Group][1]
    i.Bio <- min(df.data[df.data[, col.ID]==i, col.Bio], na.rm=TRUE)

    # Filter for selected variables
    
    mapping <- c(COL.GROUP=col.Group, COL.BIO=col.Bio)
    # Comparator Site Data
    wrapr::let(alias=mapping
        , expr={
          df.comp <- df.data[, col.KEEP] %>% dplyr::filter(COL.GROUP==i.Group)
        })
    # Better Bio Comparator Site Data
    wrapr::let(alias=mapping
        , expr={
          df.comp.bio.better <- df.comp %>% dplyr::filter(COL.BIO>i.Bio)
        })
  
     #
    if(boo_DEBUG==TRUE){##IF.boo_DEBUG.START
      j <- col.Stressors[3]
      #par(mfrow=c(3,2))
    }##IF.boo_DEBUG.END
    # outside loop just in case forget to turn off debug flag
    
     # Calculate quantiles on Comparator Sites
     # Loop, j ####
     for (j in col.Stressors){##FOR.j.START
       #
       j.num <- match(j, col.Stressors)
       j.len <- length(col.Stressors)
       #
       ij.num <- ((i.num-1)*j.len) + j.num
       ij.len <- i.len * j.len
       #
       message(paste0("Processing item (",ij.num,"/",ij.len,"); ID (", i.num, "/", i.len, ") ", i
                    , "; Stressors (", j.num, "/", j.len, ") ", j, ".\n"))
       utils::flush.console()
       #
       df.i[ ,paste0("n_", j)] <- sum(!is.na(df.comp.bio.better[, j]))
       #df.i[, paste0("q20_", j)] <- stats::quantile(df.comp.bio.better[, j], probs=0.20, na.rm=TRUE)
       df.i[, paste0("q25_", j)] <- stats::quantile(df.comp.bio.better[, j], probs=0.25, na.rm=TRUE)
       df.i[, paste0("q50_", j)] <- stats::quantile(df.comp.bio.better[, j], probs=0.50, na.rm=TRUE)
       df.i[, paste0("q75_", j)] <- stats::quantile(df.comp.bio.better[, j], probs=0.75, na.rm=TRUE)
       # Comp Score for box plot
       ## Use different criteria for some parameters
       if(j %in% col.Stressors.InvSc){##IF~j_in_InvSc~START
         # Inverse Scoring
         df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q50_", j)], -1
                                                , ifelse(df.i[, j] < df.i[, paste0("q25_",j)], 1, 0)) 
       } else {
         # Regular Scoring
         df.i[, paste0("Sc_Box_", j)] <- ifelse(df.i[, j] > df.i[,paste0("q75_", j)], 1
                                                , ifelse(df.i[, j] < df.i[, paste0("q50_",j)], -1, 0))
       }##IF~j_in_InvSc~END
       df.i[is.na(df.i[, j]), paste0("Sc_Box_", j)] <- NA
       
       # Plots
       # Need to filter df.i to get rid of NA for "j" (stressor)
       # order values by j then get multiple comp scores
       df.i.n <- df.i[!is.na(df.i[, j]), ]
       df.i.n <- df.i.n[order(df.i.n[, j]), ]
       
       if (nrow(df.i.n)!=0){##IF.nrow.START
         # Save to Score/Results file
         df.i.n[, "Param_Name"]  <- j
         df.i.n[, "Param_Value"] <- df.i.n[, j]
         df.i.n[, "n"]           <- df.i.n[, paste0("n_", j)]
         df.i.n[, "q25"]         <- df.i.n[, paste0("q25_", j)]
         df.i.n[, "q50"]         <- df.i.n[, paste0("q50_", j)]
         df.i.n[, "q75"]         <- df.i.n[, paste0("q75_", j)]
         df.i.n[, "Sc_Box"]      <- df.i.n[, paste0("Sc_Box_", j)]
         # df.i.n append to output (only keep matching columns)
         df.scores.i.n <- merge(df.scores, df.i.n[, (names(df.i.n) %in% names(df.scores))], all.y=TRUE)
         # 2019-05-20, sort by score
         df.scores.i.n <- df.scores.i.n[order(df.scores.i.n[, "Param_Value"]), ]
        #  # Save
        #  utils::write.table(df.scores.i.n, file=fn.scores
        #              , col.names = FALSE, row.names=FALSE, sep="\t", append=TRUE)
        #  # Remove
        # # rm(df.scores.i.n)
         
         ## Box Plot of Comparator Sites (with better bio)
         lab.Score <- paste0("Score = ", paste0(df.i.n[, paste0("Sc_Box_", j)], collapse=", "))
         lab.N     <- paste0("n = ",df.i[,paste0("n_",j)][1])
         
         # # plot, R
         # boxplot(df.comp[,j], xlab=j
         #         , ylab="Comp sites with higher CSCI scores"
         #         , main=i
         #         , sub=lab.N
         #         , cex.lab=1.25
         #         , horizontal=TRUE)
         # abline(v=df.i[,j], col="red", lty=2, lwd=2.5)
         # mtext(lab.Score, 3, 0.25)
         # # 20180511, multiple scores in df.i, fixed 20180605
         
         # plots ####
         # File Names
         #fn.pdf    <- paste0(TargetSiteID, ".CoOccurrence.ALL.", myDateTime,".pdf")
         fn_jpg_p1 <- paste0(i_TargetSiteID, ".CoOccurrence.Box.", make.names(j), ".jpg")
         fn_jpg_p2 <- paste0(i_TargetSiteID, ".CoOccurrence.SR.", make.names(j), ".jpg")
         ppi       <- 300
         
         # Create (ggplot)
         lab.sub <- paste0("Cluster sites with higher ", col.Bio, " scores and ", j, " (", lab.N, ").\n "
                           , lab.Score,".")
         
         bio_col <- c("blue", "dark gray")
         bio_shp <- c(21, 25) # circle and down triangle
         lab_comp <- paste0("Cluster = ",i.Group)
         
         # scoring lines
         if(j %in% col.Stressors.InvSc){##IF~j_in_InvSc~START
           # Inverse Scoring
           box_qHI <- df.scores.i.n$q50[1]
           box_qLO <- df.scores.i.n$q25[1]
         } else {
           # Regular Scoring
           box_qHI <- df.scores.i.n$q75[1]
           box_qLO <- df.scores.i.n$q50[1]
         }##IF~j_in_InvSc~END

         ## Plot, Variables, Target Site Line
         targ_line_col <- "red"
         targ_line_lty <- 2
         targ_line_lwd <- 1
         
         # if non-empty
         #if(sum(is.na(df.comp.bio.better[,j]))!=nrow(df.comp.bio.better)){##IF~non-empty~START
           # plot1, ggplot ####
           p1<- ggplot2::ggplot(df.comp.bio.better, ggplot2::aes_string(y=as.name(j), x=col.Group, group=col.Group)) +
             ggplot2::geom_boxplot(na.rm = TRUE) +
             ggplot2::coord_flip() + 
             ggplot2::geom_jitter(size=2, alpha=0.5, na.rm=TRUE
                                  , ggplot2::aes_string(color=col.SiteTypeQuality, shape=col.SiteTypeQuality, fill=col.SiteTypeQuality)) + 
             ggplot2::geom_hline(yintercept = df.i[,j], color=targ_line_col, lty=targ_line_lty, lwd=targ_line_lwd, na.rm = TRUE) + 
             # ggplot2::scale_fill_brewer(palette = "Set2", name=NULL, breaks=NULL, labels=NULL) +
             #ggplot2::scale_color_manual(values = c("black", "lightskyblue", "red", "darkgreen")) +
             ggplot2::scale_color_manual(breaks=c("Yes", "No"), values=bio_col, drop=FALSE) +
             ggplot2::scale_fill_manual(breaks=c("Yes", "No"), values=bio_col, drop=FALSE) +
             ggplot2::scale_shape_manual(breaks=c("Yes", "No"), values=bio_shp, drop=FALSE) + 
             ggplot2::labs(title=i, caption=lab.sub) + 
             ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5), plot.subtitle = ggplot2::element_text(hjust=0.5)) +
             ggplot2::theme(axis.text.y=ggplot2::element_text(color="white"), axis.ticks.y=ggplot2::element_blank()) +
             ggplot2::labs(y=j, x=lab_comp) + 
             ggplot2::geom_hline(yintercept = c(box_qLO, box_qHI), color="black", lty=2, na.rm = TRUE)
           # Capture plot (jpg)
           # Capture most recent plot to a list
           # print(p1)
           # plots_pdf[[ij.num]] <- grDevices::recordPlot()
           # p1
           # plots_jpg[[1]] <- grDevices::recordPlot()
           ggplot2::ggsave(filename=file.path(dir.plots, dir.sub2, dir.sub3, fn_jpg_p1)
                           , plot=p1
                           , dpi=ppi, width=8, height=6, units="in")
         #}##IF~non-empty~END

         ## Logistic Regression (all comparator sites)
         
         # #~~~~~~~~~~~~~~~~~~~
         # (plot with all sites in cluster (comparators) not just by condition group)
         col.glm <- c(col.Bio, col.Bio.Deg, j)
         #df.comp.glm <- df.comp[complete.cases(df.comp[,col.glm]), col.glm]
         
         df.comp.glm <- df.comp[stats::complete.cases(df.comp[, col.glm]), col.glm] 
         
         # logr <- glm(df.comp.glm[,col.Bio.Deg] ~ df.comp.glm[,j], family=binomial)
         # plot(df.comp.glm[,j], df.comp.glm[, col.Bio.Deg], ylim=c(0,2))
         # myPredict <- stats::predict(logr, data.frame(df.comp.glm[,j]), type="response")
         # curve(myPredict, add=TRUE)
         #
         
         # create data frame with known column names
         df.plot <- df.comp.glm
         names(df.plot) <- c("y","Bio.Deg","x")
         # Confirm Levels (factors) as 1=No and 2=Yes
         df.plot$Bio.Deg <- factor(df.plot$Bio.Deg, c("No", "Yes"))
         # fix so so 0=No and 1=Yes
         df.plot$y.name <- as.numeric(df.plot$Bio.Deg)-1
         
         n_cc_df_plot <- sum(stats::complete.cases(df.plot[,c("x","y")]))
         #lab.sub <- paste0("All comparator sites with both ", col.Bio, " and ", j, " (n=", n_cc_df_plot, ")")
         
         # 20190416, comment out p2 and p3, moving to getBSR
         #  Stressor Response Curve
         if(sum(stats::complete.cases(df.plot))>0){##IF.complete.cases.START
           #
           fit <- stats::glm(y.name ~ x, data=df.plot, family=stats::binomial)
           # create data for curve
           newdat <- data.frame(x=seq(min(df.plot$x, na.rm=TRUE), max(df.plot$x, na.rm=TRUE), len=100))
           newdat$y.name <- stats::predict(fit, newdata=newdat, type="response") #se.fit=TRUE
           # type=response is for probabilities.

           # Scoring
           # j_values <- data.frame(x=df.i[,j])
           j_values <- data.frame(x= df.scores.i.n[, "Param_Value"])
           # sort values, 2019-05-20
           #j_values <-  data.frame(x= df.scores.i.n[order(df.scores.i.n[, "Param_Value"]), "Param_Value"])
           j_SR_predict <- stats::predict(fit, newdata=j_values, type="response")
           j_SR_score <- cut(j_SR_predict
                             , breaks=c(0, 0.2, 0.5, 1)
                             , labels=c(-1, 0, 1))

           # # Add scores df so can save
           df.scores.i.n[, "SR_pred_Deg"] <- j_SR_predict
           df.scores.i.n[, "Sc_SR"] <- j_SR_score

           #
           lab.sub <- paste0("All cluster sites with both ", col.Bio, " and ", j
                             , " (n=", n_cc_df_plot, ").\n Score = "
                             , paste(j_SR_score, collapse=", "),".")

           # plot2, ggplot ####
           p2 <- ggplot2::ggplot(df.plot, ggplot2::aes(x=x, y=y.name)) +
             ggplot2::geom_point(ggplot2::aes(color=Bio.Deg, shape=Bio.Deg, fill=Bio.Deg), alpha=0.5, size=2, na.rm = TRUE) +
             # ggplot2::geom_jitter(size=2, alpha=0.5
             #                      , ggplot2::aes(color=Bio.Deg, shape=Bio.Deg, fill=Bio.Deg), width=0, height=0.005) +
             ggplot2::scale_fill_manual(breaks=c("Yes", "No"), values=bio_col, drop=FALSE) +
             ggplot2::scale_color_manual(breaks=c("Yes", "No"), values=bio_col, drop=FALSE) +
             ggplot2::scale_shape_manual(breaks=c("Yes", "No"), values=bio_shp, drop=FALSE) +
             ggplot2::geom_vline(xintercept = df.i[,j], color=targ_line_col, lty=targ_line_lty, lwd=targ_line_lwd, na.rm = TRUE) +
             ggplot2::geom_hline(yintercept = c(0.2, 0.5), color="black", lty=2, na.rm = TRUE) +
             #ggplot2::geom_hline(yintercept = 0.5, color="black", lty=2) +
             ggplot2::labs(title=i, y="Relative Probability of Degraded Condition", x=j) +
             ggplot2::geom_line(ggplot2::aes(y=y.name, x=x), data=newdat, color="blue", lwd=1, na.rm = TRUE) +
             ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5), plot.subtitle = ggplot2::element_text(hjust=0.5)) +
             ggplot2::labs(title=i, caption=lab.sub)
           # p2
           # plots_jpg[[2]] <- grDevices::recordPlot()
           ggplot2::ggsave(filename=file.path(dir.plots, dir.sub2, dir.sub3, fn_jpg_p2)
                           , plot=p2
                           , dpi=ppi, width=8, height=6, units="in")

           # same colors
           #ggplot2::scale_fill_brewer(palette = "Set2", name=NULL, breaks=NULL, labels=NULL)

           # Save Plots
           #
           # PDF, p1 and p2
           #grDevices::pdf(file=file.path(wd, dir.sub, dir.sub2, fn.pdf), width=6, height=8)
           p3 <- gridExtra::grid.arrange(p1, p2, ncol=1, nrow=2 )
           #p3
           # Capture most recent plot to a list
           plots_pdf[[ij.num]] <- grDevices::recordPlot()
           # grDevices::dev.off()
           #
           # ggplot mods
           ## Size modifier - 4:3 isn't big enough for all of text on ggplots
           #size_mod <- 1.5
           #
           # # JPG, p1
           # grDevices::jpeg(filename = file.path(wd, dir.sub, dir.sub2, fn_jpg_p1)
           #                 , width = size_mod*4*ppi, height = size_mod*3*ppi, quality=100
           #                 , pointsize = 8
           #                 , res = ppi)
           #    grDevices::replayPlot(1)
           # grDevices::dev.off()
           #
           # JPG, p2
           # grDevices::jpeg(filename = file.path(wd, dir.sub, dir.sub2, fn_jpg_p2)
           #                 , width = size_mod*4*ppi, height = size_mod*3*ppi, quality=100
           #                 , pointsize=8
           #                 , res = ppi)
           #    grDevices::replayPlot(2)
           # grDevices::dev.off()
           #

         } else {
           #
           # Save Plots
           #
           # PDF, p1 only
           if(exists("p1")==TRUE & exists("p2")==FALSE){
             p3 <- gridExtra::grid.arrange(p1, ncol=1, nrow=2)
           }
           if(exists("p1")==FALSE & exists("p2")==TRUE){
             p3 <- gridExtra::grid.arrange(p2, ncol=1, nrow=2)
           }
           print(p3)
           # Capture most recent plot to a list
           plots_pdf[[ij.num]] <- grDevices::recordPlot()
           # grDevices::dev.off()
           #
           # ggplot mods
           ## Size modifier - 4:3 isn't big enough for all of text on ggplots
           #size_mod <- 1.5
           #
           # JPG, p1
           # grDevices::jpeg(filename = file.path(wd, dir.sub, dir.sub2, fn_jpg_p1)
           #                 , width = size_mod*4*ppi, height = size_mod*3*ppi, quality=100
           #                 , pointsize=8
           #                 , res = ppi)
           #    grDevices::replayPlot(1)
           # grDevices::dev.off()

         }##IF.complete.cases.END
          
         # add biocomm, 20190425
         df.scores.i.n[, "biocomm"] <- biocomm
         
          # Save tabular scores
          utils::write.table(df.scores.i.n, file=fn.scores
                      , col.names = FALSE, row.names=FALSE, sep="\t", append=TRUE)
          # Remove
          rm(df.scores.i.n)
         
         
       } else {
         # no data
         message(paste0("   All values NA for stressor (", j, ").\n"))
         utils::flush.console()
         # add data to scores table
         column_names <- c("Param_Name", "Param_Value", "n", "q25", "q50", "q75", "Sc_Box", "SR_pred_Deg", "Sc_SR")
         df.i.NA <- df.i[1,1:5]
         df.i.NA[, column_names] <- NA
         df.i.NA[, "Param_Name"] <- j
         # add biocomm, 20190425
         df.i.NA[, "biocomm"] <- biocomm
         utils::write.table(df.i.NA, file=fn.scores
                            , col.names = FALSE, row.names=FALSE, sep="\t", append=TRUE)
         
         
       }##IF.nrow.END
       #
       
       
     }##FOR.j.END
     #
 #    par(par.orig)
   #
    # PDF, new ####
    # Create PDF from list of recorded plots
    if(boo_DEBUG==FALSE){##IF.boo_DEBUG.START
      fn.pdf    <- paste0(i_TargetSiteID, ".CoOccurrence.ALL.pdf")
      grDevices::pdf(file=file.path(dir.plots, dir.sub2, dir.sub3, fn.pdf), width=6, height=8) #p3
     # grDevices::pdf(file=file.path(dir.plots, dir.sub2, fn.pdf), width=9, height=4) #p1 only
        #
        # Remove null items from plot list
        #plots_pdf_nonull <- plots_pdf[-which(sapply(plots_pdf, is.null))]
        ## already handled in loop with next
        for (p in plots_pdf){##FOR.gp.START
          #grDevices::replayPlot(g.plot)
          if(is.null(p)==TRUE) {next}
          grDevices::replayPlot(p)
        }##FOR.gp.END
        rm(plots_pdf)
        #
      grDevices::dev.off()
    }##IF.boo_DEBUG.END
    # PDF (ALL) (close for i)
    # grDevices::dev.off()
  
     #
  }##FOR.i.END 
  
  
}##FUNCTION.END

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Old Stuff
#~~~~~~~~~~~~
# # QC Check
# if(nrow(df.i.n)==0){##IF.nrow.START
#   break
# }##IF.nrow.END
#
#
# Save Scores
# utils::write.table(df.scores.i.n, file=fn.scores
#                    , col.names = FALSE, row.names=FALSE, sep="\t", append=TRUE)
# Remove
#rm(df.scores.i.n)

# # plot 2, R
# plot(y.name~x, data=df.plot, xlab=j, ylab="Relative Probability of Degraded Condition"
#      , main=i)
# lines(y.name~x, newdat)
# abline(v=df.i[,j], col="red", lty=2, lwd=2.5)
# abline(h=0.2, col="gray", lty=2)
# abline(h=0.5, col="gray", lty=2)


# # Confidence Limits
# ## http://www.stat.cmu.edu/~cshalizi/402/lectures/16-glm-practicals/lecture-16.pdf
# ## Note that calculating standard errors for predictions on the logit scale, and then
# # transforming, is better practice than getting standard errors directly on the
# # probability scale.
# pred <- stats::predict(fit, newdata=newdat, se.fit=TRUE)
# mySeq <- seq(min(df.plot$x, na.rm=TRUE), max(df.plot$x, na.rm=TRUE), len=100)
# library(car)
# lines(mySeq, logit(pred$fit), col="blue")
# lines(mySeq, logit(pred$fit+1.96*pred$se.fit), lty=2, col="green")
# lines(mySeq, logit(pred$fit-1.96*pred$se.fit), lty=2, col="red")
# 
# critval <- 1.96
# upr <- pred$fit + (critval * pred$se.fit)
# lwr <- pred$fit - (critval * pred$se.fit)
# 
# upr2 <- fit$family$linkinv(upr)
# lwr2 <- fit$family$linkinv(lwr)
# 
# # don't plot if use polygon
# #  lines(mySeq, upr2, col="gray", lty=2)
# #  lines(mySeq, lwr2, col="gray", lty=2)
# 
# # will need to replot some stuff
# polygon(c(mySeq, rev(mySeq)), c(upr2, rev(lwr2)), col="light gray", border=NA)
# 
# 
# #logr.conf <- confint(fit, )
# 
# # ci <- matrix(c())
# 
# #https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression
# 
# # score
# # Need to find out 20th and 50th to score
# lab.Score.Poor <- "Score = NAN"
# mtext(lab.Score.Poor, 3, 0.25)
# 
# y.lo <- 0.75
# xval.lo <- approx(x=newdat$y.name, y=newdat$x, xout=y.lo)$y
# y.hi <- 0.70
# 
# xval.hi <- approx(x=newdat$y.name, y=newdat$x, xout=y.hi)$y
# 
# points(c(xval.lo, xval.hi), c(y.lo, y.hi), col="blue", pch=19)
# 
# # #~~~~~~~~~~~~~~~~~~~
# 
# # logr <- glm(df.comp[,col.Bio.Deg] ~ df.comp[,j], family=binomial)
# # 
# # 
# # plot(df.comp[,j], df.comp[,col.Bio.Deg], ylim=c(0,2))
# # curve(stats::predict(logr, data.frame(df.comp[, j])=x,type="response"), add=TRUE)
# # 
# # curve(predict.glm(logr, newdata=df.comp[,j], type="response"), add=TRUE)
# # # predict fails
# # 
# # # 
# # logr <- glm(CSCI.Status ~ SpecCond_uf_µS_cm, data=df.comp, family=binomial)
# # plot(df.comp$SpecCond_uf_µS_cm, df.comp$CSCI.Status, ylim=c(0,2))
# # curve(stats::predict(logr, data.frame(SpecCond_uf_µS_cm[1:78]=x), type="response"), add=TRUE)
# # 
# # 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 
# # ggplot(df.comp, aes(x=TN_uf_mg_L, y=CSCI.Deg)) + geom_point() + 
# #   stat_smooth(method="glm", method.agrs=list(family="binomial"), se=FALSE)
# 
# # # # example, base
# # data(mtcars)
# # dat <- subset(mtcars, select=c(mpg, am, vs))
# # logr_vm <- glm(vs ~ mpg, data=dat, family=binomial)
# # plot(dat$mpg, dat$vs, xlab=j, main=i, ylab="Relative Probability of Degraded Condition")
# # curve(stats::predict(logr_vm, data.frame(mpg=x), type="response"), add=TRUE)
# # # ab line (fake)
# # Range.j <- max(df.comp[,j], na.rm=TRUE) - min(df.comp[,j], na.rm=TRUE)
# # PctRange.i <- (df.i[,j] - min(df.comp[,j], na.rm=TRUE)) / Range.j
# # Range.mtcars <- max(dat$mpg) - min(dat$mpg)
# # val.fake <- (PctRange.i * Range.mtcars) + min(dat$mpg)
# # abline(v=val.fake, col="red", lty=2, lwd=2.5)
# # # score
# # lab.Score.Poor <- "Score = NAN"
# # mtext(lab.Score.Poor, 3, 0.25)
# 
# # example, ggplot
# #library(ggplot2)
# # ggplot(dat, aes(x=mpg, y=vs)) + geom_point() + 
# #         stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


# mfrow not available with ggplot.
# use another package with named plots, so can only get 2 per page
# pdf set to width=6 and height=8 so 

#gridExtra::grid.arrange(p1, p2, ncol=1, nrow=2 )
