# SSD- generates plots of the proportion of species affected at different exposure levels in laboratory toxicity tests.
#Data= species data set name
#TaxaName= "column containing taxa names"
#ExposureAmount="column containing Exposure amount"
#EndPoint="column containing endpoint"
# library(psych)    # geometric.mean
# library(dplyr)    # mutate and pipe
# library(magrittr) # part of dplyr, don't need separately
# library(ggplot2)  # plot
# library(reshape2) # (no longer needed)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Species Sentivity Distribution (SSD) Plots
#' 
#' @description Generates plots of the proportion of species affected at different exposure levels in laboratory toxicity tests.
#' 
#' @details https://www.epa.gov/caddis-vol4/ssd-plots
#' 
#' @param Data species data set name
#' @param ResponseType column containing endpoint
#' @param Taxa column containing taxa names
#' @param Exposure column containing Exposure amount
#' 
#' @return An SSD plot.
#' 
#' @examples
#' # Example 1
#' # Define parameters
#' myDF   <- data_SSD
#' myRT   <- "ResponseType"
#' myTaxa <- "Taxa"
#' myExp  <- "Exposure_mgperL"
#' # Run function
#' p1 <- getSSDplot(myDF, myRT, myTaxa, myExp)
#' print(p1)
#' # can save output to a file
#' \dontrun{
#' library(ggplot2)
#' fn_p1 <- file.path(getwd(), "Results", "SSD_example1.pdf")
#' ggsave(fn_p1, p1)
#' }
#' 
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~
#' 
#' # Example 2
#  # Define parameters
#' myDF   <- data_SSD_permethrin
#' myRT   <- "ResponseType"
#' myTaxa <- "Taxa"
#' myExp  <- "Exposure"
#' # Add missing column
#' myDF[,myRT] <- "LC50"
#' # Run function
#' p2 <- getSSDplot(myDF, myRT, myTaxa, myExp)
#' print(p2)
#' #' # can save output to a file
#' \dontrun{
#' fn_p2 <- file.path(getwd(), "Results", "SSD_example2.pdf")
#' ggplot2::ggsave(fn_p2, p2)
#' }
#' 
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~
#' 
#' # Example 3 
#' # https://www.epa.gov/caddis-vol4/caddis-volume-4-data-analysis-download-software
#' # ssd_generator_v1.xlsm
#' myDF <- data_SSD_generator
#' myRT   <- "ResponseType"
#' myTaxa <- "Taxa"
#' myExp  <- "Exposure"
#' # Run function
#' p3 <- getSSDplot(myDF, myRT, myTaxa, myExp)
#' print(p3)
#' # 
#' # can save output to a file
#' \dontrun{
#' fn_p3 <- file.path(getwd(), "Results", "SSD_example3.pdf")
#' ggplot2::ggsave(fn_p3, p3)
#' }
#'  
#' #~~~~~~~~~~~~~~~~~~~~~~~~~~
#'  
#' # Example 2_mod 
#' # (same plot as #2 above)
#' library(ggplot2)
#' # create points
#' analyte <- c(0.05, 0.5, 5)
#' abund <- c(0, 1/3, 2/3)
#' taxon <- c("Baetis", "Cricotopus", "Simulium")
#' df.site <- data.frame(analyte, abund, taxon)
#' # add points to plot and add title (centered)
#' p2 + 
#'   geom_point(data=df.site, aes(x=analyte, y=abund), colour="blue", size = 2) + 
#'   geom_text(data=df.site, aes(x=analyte, y=abund, label=taxon)
#'                              , hjust="left", nudge_x=0.05, colour="blue") + 
#'   labs(title="Cluster SSD with site specific taxa") + theme(plot.title=element_text(hjust=0.5))
#'   
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QC
# library(CASTfxn)
# Data <- data_SSD_generator
# ResponseType <- "ResponseType"
# Taxa <- "Taxa"
# Exposure <- "Exposure"
#
#' @export
getSSDplot <- function(Data, ResponseType, Taxa, Exposure) {
  #
  
  # Define col names for dplyr
  names(Data)[names(Data)==ResponseType] <- "ResponseType"
  names(Data)[names(Data)==Taxa]         <- "Taxa"
  names(Data)[names(Data)==Exposure]     <- "Exposure"
  
  # define pipe
  `%>%` <- dplyr::`%>%`
  
  # Calculate geometric mean
  DF.TRIAL.P <- as.data.frame(Data %>% 
                                dplyr::group_by(Taxa, ResponseType) %>% 
                                dplyr::summarize(Conc_1_Mean_Standardized = psych::geometric.mean(Exposure)
                                                 , .groups = "drop_last"))
  
  #
  DF.TRIAL.P <- DF.TRIAL.P[order(DF.TRIAL.P$Conc_1_Mean_Standardized),] ## orders dataset by mean values
  DF.TRIAL.P <- DF.TRIAL.P[stats::complete.cases(DF.TRIAL.P), ]##removes na values
  
  #
  DF.TRIAL.P$LogMean    <- log10(DF.TRIAL.P$Conc_1_Mean_Standardized)
  DF.TRIAL.P$Rank       <- rank(DF.TRIAL.P$Conc_1_Mean_Standardized)
  # USEPA file has 0.5, Diana had .05
  DF.TRIAL.P$Proportion <- (DF.TRIAL.P$Rank-0.5)/length(DF.TRIAL.P$Taxa)
  DF.TRIAL.P$Probit     <- stats::qnorm(DF.TRIAL.P$Proportion, mean=5, sd=1)
  
  #
  sloperesult <- stats::lm(Probit ~ LogMean, data=DF.TRIAL.P)
  Slope       <- sloperesult$coefficients[2]
  Intercept   <- sloperesult$coefficients[1]
  # DF.TRIAL.P %>% dplyr::mutate(Log10CentralTendency=(Probit-Intercept)/Slope
  #                              , MSE=(Log10CentralTendency-LogMean)^2 )
  # duplicative of above but above not assigned
  DF.TRIAL.P <- dplyr::mutate(DF.TRIAL.P, Log10CentralTendency=(Probit-Intercept)/Slope)
  # (Log10mean(Obs) - Pred)^2  # number ok but not really MSE
  DF.TRIAL.P <- dplyr::mutate(DF.TRIAL.P, MSE=(Log10CentralTendency-LogMean)^2)
  SumofMSE <- sum(DF.TRIAL.P$MSE)/length(DF.TRIAL.P$Taxa)
  
  #
  DF.TRIAL.P         <- dplyr::mutate(DF.TRIAL.P,ProbitSquared=(Probit)^2)
  SumofProbitSquared <- sum(DF.TRIAL.P$ProbitSquared)
  AvgSumSquareProbit <- sum(DF.TRIAL.P$Probit)^2/length(DF.TRIAL.P$Taxa)
  cssq               <- SumofProbitSquared-AvgSumSquareProbit
  GrandMean          <- mean(DF.TRIAL.P$LogMean)
  df.Final.Product   <- dplyr::mutate(DF.TRIAL.P, PointError = 
                                      (SumofMSE/(Slope^2)) * 
                                      (1+(1/length(DF.TRIAL.P$Taxa)) + 
                                         ((Log10CentralTendency-GrandMean)^2)/cssq))
  df.Final.Product <- dplyr::mutate(df.Final.Product
                                    , log10UpperPI=(Log10CentralTendency) + ((2.02)*(sqrt(PointError))))
  df.Final.Product <- dplyr::mutate(df.Final.Product,log10LowerPI=(Log10CentralTendency) 
                                    - ((2.02)*(sqrt(PointError))))
  df.Final.Product <- dplyr::mutate(df.Final.Product,LowerPI=10^(log10LowerPI))
  df.Final.Product <- dplyr::mutate(df.Final.Product,UppererPI=10^(log10UpperPI))
  df.Final.Product <- dplyr::mutate(df.Final.Product,CentralTendency=10^(Log10CentralTendency))
  
  # plot 
  #taxalist <- unique(df.Final.Product$Taxa)  # not used
  
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_point(data = df.Final.Product
                        , ggplot2::aes(x = Conc_1_Mean_Standardized, y = Proportion)
                        , na.rm = TRUE) +
    ggplot2::geom_line(data = df.Final.Product
                       , na.rm = TRUE
                       , ggplot2::aes(x =  CentralTendency, y = Proportion), col = 'dark gray', lwd=0.75) +
    ggplot2::geom_line(data = df.Final.Product
                       , na.rm = TRUE
                       , ggplot2::aes(x = LowerPI, y = Proportion), linetype = 'dashed', col="light gray") + 
    ggplot2::geom_line(data = df.Final.Product
                       , na.rm = TRUE
                       , ggplot2::aes(x = UppererPI, y = Proportion), linetype = 'dashed', col="light gray") + 
    ggplot2::geom_text(data = df.Final.Product
                       , na.rm = TRUE
                       , ggplot2::aes(x = Conc_1_Mean_Standardized
                       , y = Proportion, label = Taxa)
                       , size = 3
                       , hjust="right"
                       , nudge_x = -0.05) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000)
                           , limits = c(0.003, max(df.Final.Product$Conc_1_Mean_Standardized))) +
    ggplot2::labs(x = expression(paste('Stressor Intensity')),y = 'Proportion Taxa')
  #
  return(p1)
}
