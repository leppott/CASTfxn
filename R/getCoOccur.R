# library(dplyr)
# library(replyr)
# library(ggplot2)
# library(gridExtra)
#
#' @title Co-Occurrence Plots
#' 
#' @description Generates a PDF with 2 plots per site for co-occurrence of 
#' measured chem and bio data
#' 
#' @details Derive evidence fo spatial/temporal co-occurrence.
#' 
#' Are higher levels of the stressor observed where and when the biological 
#' effect occurs?
#' 
#' Box plots are used to show the distribution of the stressor levels at compartor 
#' sites with better biological condition.  
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
#' Derive Evidence for Stressor-Response Relationships from Field Observational 
#' Studies.
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
#' Only a single biological measurement is used.  But multiple stressors can be
#'  used.
#' 
#' Uses the libraries dplyr, replyr, wrapr, ggplot2, and gridExtra.
#' 
#' @param df.data data frame with data.
#' @param ID.plot ID of station/sample to plot; can be single or multiple.  
#' Default is first entry in df.data[, col.ID]
#' @param col.ID df.data column with unique Station/Sample identifier.
#' @param col.Group df.data column with grouping variable.
#' @param col.Bio df.data column with biological numeric value.
#' @param col.Stressors df.data column(s) with stressor variable(s); can be 
#' single or multiple.
#' @param Bio.Nar.Brk Biological assessment narrative, cut function breaks. 
#' Default = c(-2, 0.62, 0.799, 0.919, 2)
#' @param Bio.Nar.Lab Biological assessment narrative, cut function labels. 
#' Default = c("very likely altered", "likely altered", "possibly altered ", 
#' "likely intact")
#' @param Bio.Deg.Brk Biological assessment degraded status, cut function breaks. 
#' Default = c(-2, 0.799, 2)
#' @param Bio.Deg.Lab Biological assessment degraded status, cut function labels. 
#' Default = c("Degraded", "Good")
#' @param dir.plots Directory to save plots.  Default = working directory
#'
#' @return Saves PDF of plots and a scores files (tab separated) to user defined
#'  directory.  A sub-directory is created for each SiteID in ID.plot.
#' 
#' @examples
#' #Load Data
#' df.data <- data_CoOccur
#' #
#' col.Group     <- "Group"
#' col.Bio       <- "CSCI"
#' col.Stressors <- c("DO_uf_mg_L", "SpecCond_uf_µS_cm", "TN_uf_mg_L", "TP_mg_L")
#' col.ID        <- c("StationID_Master")
#' #
#' Bio.Nar.Brk <- c(-2, 0.62, 0.799, 0.919, 2)
#' Bio.Nar.Lab <- c("very likely altered", "likely altered"
#'                 , "possibly altered ", "likely intact")
#' Bio.Deg.Brk <- c(-2, 0.799, 2)
#' Bio.Deg.Lab <- c("Degraded", "Good")
#' dir.plots <- file.path(getwd(), "Results")
#' #
#' ID.plot <- c("SMC08335", "901SJSJC9", "911TCAM01", "403STC004")
#' #
#' getCoOccur(df.data, ID.plot, col.ID, col.Group, col.Bio, col.Stressors
#'         , Bio.Nar.Brk, Bio.Nar.Lab, Bio.Deg.Brk, Bio.Deg.Lab 
#'         , dir.plots
#'         )
#~~~~~~~~~~~~~
# QC
# check for and create (if necessary) "Results" subdirectory of working directory
# wd <- getwd()
# dir.sub <- "Results"
# ifelse(!dir.exists(file.path(wd, dir.sub))==TRUE
#        , dir.create(file.path(wd, dir.sub))
#        , FALSE)
#~~~~~~~~~~~~~
#' @export
getCoOccur <- function(df.data, ID.plot=NULL
                    , col.ID, col.Group, col.Bio, col.Stressors
                    , Bio.Nar.Brk=c(-2, 0.62, 0.799, 0.919, 2)
                    , Bio.Nar.Lab=c("very likely altered", "likely altered"
                                    , "possibly altered ", "likely intact")
                    , Bio.Deg.Brk=c(-2, 0.799, 2)
                    , Bio.Deg.Lab=c("Degraded", "Good")
                    , dir.plots=getwd()
                    ) {##FUNCTION.START
  #
  # define pipe
  `%>%` <- dplyr::`%>%`
  
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
  
  # Add missing variable
  col.SiteTypeQuality <- col.Bio.Deg
  #
  # default sample ID
  if(is.null(ID.plot)){##IF.isnull.ID.START
    ID.plot <- as.character(sort(unique(df.data[,col.ID])))[1]
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
  
  # Add columns
  df.scores[, "Param_Name"] <- as.character(NA)
  df.scores[, "Param_Value"] <- as.numeric(NA)
  df.scores[, "n"]       <- as.character(NA)
  df.scores[, "q25"]     <- as.character(NA)
  df.scores[, "q50"]     <- as.character(NA)
  df.scores[, "q75"]     <- as.character(NA)
  df.scores[, "Sc_Comp"] <- as.character(NA)
  # Remove columns
  col.remove <- names(df.scores) %in% col.Stressors
  df.scores <- df.scores[, !col.remove]
  
  #
  # remove all rows
  df.scores <- df.scores[0, ]
  
  

  #par
#  par.orig <- par(no.readonly=TRUE)
  # reset with "par(par.orig)"
  
  
  # QC Test

  # num.ID        <- sum(ID.plot %in% df.data[,col.ID])
  # num.Stressors <- sum(col.Stressors %in% names(df.data))
  # num.Groups    <- length(unique(df.data[,col.Group]))
  # 
  # print(paste0("Items to process; Samples/Stations (n=",num.ID,")."))
  # print(paste0("Items to process; Stressors (n=",num.Stressors,")."))
  # print(paste0("Items to process; Groups (n=",num.Groups,")."))
  
 # i <- ID.plot[2]
  

  
  # Analysis for each "test" sample
  for (i in ID.plot){##FOR.i.START
    #
    TargetSiteID <- i
    #
    wd = getwd()
    dir.sub <- "Results"
    dir.sub2 <- TargetSiteID
    ifelse(!dir.exists(file.path(wd, dir.sub, dir.sub2))==TRUE
           , dir.create(file.path(wd, dir.sub, dir.sub2))
           , FALSE)
    #
    # Write to PDF
    fn.pdf <- paste0(TargetSiteID, "_CoOccurrence_", myDateTime,".pdf")
    grDevices::pdf(file=file.path(wd,dir.sub,dir.sub2,fn.pdf), width=6, height=8)
    #
    fn.scores <- file.path(wd,dir.sub,dir.sub2,paste0(TargetSiteID,".CoOccurrence_Scores_", myDateTime,".tsv"))
    write.table(df.scores, file=fn.scores
                , col.names = TRUE, row.names=FALSE, sep="\t")
    #
    i.num <- match(i, ID.plot)
    i.len <- length(ID.plot)
    #
    df.i <- df.data[df.data[,col.ID]==i, col.KEEP]
    i.Group <- df.i[,col.Group][1]
    i.Bio <- mean(df.data[df.data[,col.ID]==i, col.Bio], na.rm=TRUE)

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

    # j <- col.Stressors[2]

     #
 #    par(mfrow=c(3,2))
     #
     # Calculate quantiles on Comparator Sites
     for (j in col.Stressors){##FOR.j.START
       #
       j.num <- match(j, col.Stressors)
       j.len <- length(col.Stressors)
       #
       print(paste0("Items to process; ID, ", i.num, "/", i.len, ", ", i
                    , "; Stressors, ", j.num, "/", j.len, ", ", j, "."))
       #
       df.i[,paste0("n_",j)] <- sum(!is.na(df.comp[,j]))
       #df.i[,paste0("q20_",j)] <- stats::quantile(df.comp[,j], probs=0.20, na.rm=TRUE)
       df.i[,paste0("q25_",j)] <- stats::quantile(df.comp[,j], probs=0.25, na.rm=TRUE)
       df.i[,paste0("q50_",j)] <- stats::quantile(df.comp[,j], probs=0.50, na.rm=TRUE)
       df.i[,paste0("q75_",j)] <- stats::quantile(df.comp[,j], probs=0.75, na.rm=TRUE)
       # Comp Score
       df.i[,paste0("Sc_Comp_",j)] <- ifelse(df.i[,j] > df.i[,paste0("q75_",j)],1
                                             , ifelse(df.i[,j] < df.i[,paste0("q50_",j)],-1,0))
       df.i[is.na(df.i[,j]), paste0("Sc_Comp_",j)] <- NA
       
       # Plots
       # Need to filter df.i to get rid of NA for "j" (stressor)
       # order values by j then get multiple comp scores
       df.i.n <- df.i[!is.na(df.i[,j]), ]
       df.i.n <- df.i.n[order(df.i.n[,j]), ]
       
       if (nrow(df.i.n)!=0){##IF.nrow.START
         # Save to Score/Results file
         df.i.n[, "Param_Name"]  <- j
         df.i.n[, "Param_Value"] <- df.i.n[, j]
         df.i.n[, "n"]           <- df.i.n[, paste0("n_",j)]
         df.i.n[, "q25"]         <- df.i.n[, paste0("q25_",j)]
         df.i.n[, "q50"]         <- df.i.n[, paste0("q50_",j)]
         df.i.n[, "q75"]         <- df.i.n[, paste0("q75_",j)]
         df.i.n[, "Sc_Comp"]     <- df.i.n[, paste0("Sc_Comp_",j)]
         # df.i.n append to output (only keep matching columns)
         df.scores.i.n <- merge(df.scores, df.i.n[, (names(df.i.n) %in% names(df.scores))], all.y=TRUE)
         # Save
         write.table(df.scores.i.n, file=fn.scores
                     , col.names = FALSE, row.names=FALSE, sep="\t", append=TRUE)
         # Remove
         rm(df.scores.i.n)
       } else {
         # no data
       }##IF.nrow.END

       # # QC Check
       # if(nrow(df.i.n)==0){##IF.nrow.START
       #   break
       # }##IF.nrow.END

       ## Box Plot of Comparator Sites (with better bio)
       lab.Score <- paste0("Score = ", paste0(df.i.n[, paste0("Sc_Comp_", j)], collapse=", "))
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
       
       # plot, ggplot
       lab.sub <- paste0("Comparator sites with higher ", col.Bio, " scores (", lab.N, ") ; ", lab.Score)

      p1<- ggplot2::ggplot(df.comp, ggplot2::aes_string(y=j, x=col.Group, group=col.Group)) +
        ggplot2::geom_boxplot() +
        ggplot2::coord_flip() + 
        ggplot2::geom_jitter(size=1, alpha=0.5, ggplot2::aes_string(color=col.SiteTypeQuality)) +
        ggplot2::geom_hline(yintercept = df.i[,j], color="red", lty=2, lwd=1) + 
        ggplot2::scale_fill_brewer(palette = "Set2", name=NULL, breaks=NULL, labels=NULL) +
        ggplot2::scale_color_manual(values = c("black", "lightskyblue", "red", "darkgreen")) +
        ggplot2::labs(title=i, caption=lab.sub) + 
        ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5), plot.subtitle = ggplot2::element_text(hjust=0.5))
       
       ## Logistic Regression (all comparator sites)

       # #~~~~~~~~~~~~~~~~~~~
       # (plot with all sites not just by group)
       col.glm <- c(col.Bio, col.Bio.Deg, j)
       #df.comp.glm <- df.comp[complete.cases(df.comp[,col.glm]), col.glm]
       
       df.comp.glm <- df.data[stats::complete.cases(df.comp[,col.glm]), col.glm] 
       
       # logr <- glm(df.comp.glm[,col.Bio.Deg] ~ df.comp.glm[,j], family=binomial)
       # plot(df.comp.glm[,j], df.comp.glm[, col.Bio.Deg], ylim=c(0,2))
       # myPredict <- predict(logr, data.frame(df.comp.glm[,j]), type="response")
       # curve(myPredict, add=TRUE)
       #
       
       # create data frame with known column names
       df.plot <- df.comp.glm
       names(df.plot) <- c("y","y.name","x")
       # convert 1 "Degraded" and 2 "Good" to 0 and 1
       df.plot$y.name <- as.numeric(df.plot$y.name)-1
       #reverse so 1 is good and 0 is degraded
       df.plot$y.name <- 1-df.plot$y.name
       #
       
       # QC
       if(sum(stats::complete.cases(df.plot))>0){##IF.complete.cases.START
         #
         fit <- stats::glm(y.name ~ x, data=df.plot, family=stats::binomial)
         # create data for curve
         newdat <- data.frame(x=seq(min(df.plot$x, na.rm=TRUE), max(df.plot$x, na.rm=TRUE), len=100))
         newdat$y.name <- stats::predict(fit, newdata=newdat, type="response") #se.fit=TRUE
         # type=response is for probabilities.
         
         # plot2, ggplot
         p2 <- ggplot2::ggplot(df.plot, ggplot2::aes(x=x, y=y.name)) +
           ggplot2::geom_point() +
           ggplot2::geom_vline(xintercept = df.i[,j], color="red", lty=2, lwd=1) + 
           ggplot2::geom_hline(yintercept = 0.2, color="black", lty=2) +
           ggplot2::geom_hline(yintercept = 0.5, color="black", lty=2) + 
           ggplot2::labs(title=i, y="Relative Probability of Degraded Condition", x=j) + 
           ggplot2::geom_line(ggplot2::aes(y=y.name, x=x), data=newdat, color="blue", lwd=1) + 
           ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5), plot.subtitle = ggplot2::element_text(hjust=0.5))
         
           gridExtra::grid.arrange(p1, p2, ncol=1, nrow=2 )
         
       } else {  
        
         # no plot 2
         gridExtra::grid.arrange(p1, ncol=1, nrow=2 )
         
       }##IF.complete.cases.END

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
      
       #
     }##FOR.j.END
     #
 #    par(par.orig)
     #
    grDevices::dev.off()
  
     #
  }##FOR.i.END 
  

  #grDevices::dev.off()
  #
  
}##FUNCTION.END
