# SSD- generates plots of the proportion of species affected at different exposure levels in laboratory toxicity tests.
#Data= species data set name
#TaxaName= "column containing taxa names"
#ExposureAmount="column containing Exposure amount"
#EndPoint="column containing endpoint"
# library(psych)
# library(dplyr)
# library(magrittr) # part of dplyr, don't need
# library(ggplot2)
# library(reshape2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Species Sentivity Distribution (SSD) Plots
#' 
#' @description Generates plots of the proportion of species affected at different exposure levels in laboratory toxicity tests.
#' 
#' @details https://www.epa.gov/caddis-vol4/ssd-plots
#' 
#' @param Data species data set name
#' @param EndPoint column containing endpoint
#' @param TaxaName column containing taxa names
#' @param ExposureAmount column containing Exposure amount
#' @param Conc_1_Mean_Standardized column containing amount of taxa
#' @return ABC
#' @examples
#' #Load Data

#df.SSD <- read.delim(file.path(getwd(), "data-raw", "data_SSD_permethrin.txt"))




# df.SSD <- data_SSD
# myEndPoint <- "ResponseType"
# myTaxaName <- "Taxa"
# myExposureAmount <- "Exposure_mgperL"
# myConc <- "Conc_1_Mean_Standardized"
# 
# #~~~~~~~~~~~~~~~~~~~~~
# # QC
# Data <- df.SSD
# EndPoint <- myEndPoint
# TaxaName <- myTaxaName
# ExposureAmount <- myExposureAmount
# Conc_1_Mean_Standardized <- myConc
#
SSD <- function(Data, EndPoint, TaxaName, ExposureAmount, Conc_1_Mean_Standardized) {
  #TRIALPermethrin_JF= excel uploaded file name 
 # DF.TRIAL.P <- unique(Data[,c(TaxaName, ExposureAmount, EndPoint)])
  #DF.TRIAL.P <- aggregate(Data[,ExposureAmount], list(Data[,TaxaName]), list(Data[,EndPoint] ), (mean))
  # DF.TRIAL.P <- as.data.frame(Data %>% 
  #   dplyr::group_by(Taxa, ResponseType) %>% 
  #   dplyr::summarize(Conc_1_Mean_Standardized = mean(Exposure_mgperL)))
  
  
  # DF.TRIAL.P <- Data %>% 
  #   dplyr::group_by(.dots=list(TaxaName, EndPoint)) %>% 
  #   dplyr::summarize_(Conc_1_Mean_Standardized = mean(ExposureAmount))
  
  # Erik
  DF.TRIAL.P <- aggregate(Data[,ExposureAmount] ~ Data[,TaxaName] + Data[,EndPoint], data=Data, mean)
  names(DF.TRIAL.P) <- c(TaxaName, EndPoint, "Conc_1_Mean_Standardized")
  
  
  #names( DF.TRIAL.P)[names( DF.TRIAL.P) == 'Group.1'] <- 'Species' ###changes column name from group.1 to species
  #DF.TRIAL.P <- DF.TRIAL.P[order(DF.TRIAL.P[, ExposureAmount]),] ## orders dataset by mean values
  
  DF.TRIAL.P <- DF.TRIAL.P[order(DF.TRIAL.P[, "Conc_1_Mean_Standardized"]),]
  
  DF.TRIAL.P <- DF.TRIAL.P[complete.cases(DF.TRIAL.P), ]##removes na values
  
  DF.TRIAL.P <- DF.TRIAL.P %>% dplyr::mutate(LogMean = log10(Conc_1_Mean_Standardized)
                                             , Rank = rank(Conc_1_Mean_Standardized)
                                             , Proportion = (Rank-0.05)/length(Species)
                                             , Probit = qnorm(Proportion, mean=5, sd=1, conf=0.95) )
  
  sloperesult <- lm(Probit ~ LogMean, data=DF.TRIAL.P, conf=0.95)
  Slope <- sloperesult$coefficients[2]
  Intercept <- sloperesult$coefficients[1]
  DF.TRIAL.P %>% dplyr::mutate(Log10CentralTendency=(Probit-Intercept)/Slope
                               , MSE=(Log10CentralTendency-LogMean)^2 )
  DF.TRIAL.P <- dplyr::mutate(DF.TRIAL.P,Log10CentralTendency=(Probit-Intercept)/Slope)
  DF.TRIAL.P <- dplyr::mutate(DF.TRIAL.P,MSE=(Log10CentralTendency-LogMean)^2)
  SumofMSE <- sum(DF.TRIAL.P$MSE)/length(DF.TRIAL.P$Species)
  ####REMOVED ROWS 82 AND 48 CAUSE THEY HAD MISSING VALUES
  #DF.TRIAL.P <- DF.TRIAL.P[-82,]
  #DF.TRIAL.P <- DF.TRIAL.P[-48,]
  DF.TRIAL.P <- dplyr::mutate(DF.TRIAL.P,ProbitSquared=(Probit)^2)
  SumofProbitSquared <- sum(DF.TRIAL.P$ProbitSquared)
  AvgSumSquareProbit <- sum(DF.TRIAL.P$Probit)^2/length(DF.TRIAL.P$Species)
  cssq <- SumofProbitSquared-AvgSumSquareProbit
  GrandMean <- mean(DF.TRIAL.P$LogMean)
  df.Final.Product <- dplyr::mutate(DF.TRIAL.P, PointError = 
                                      (SumofMSE/(Slope^2)) * 
                                      (1+(1/length(DF.TRIAL.P$Species)) + 
                                         ((Log10CentralTendency-GrandMean)^2)/cssq))
  df.Final.Product <- dplyr::mutate(df.Final.Product
                                    , log10UpperPI=(Log10CentralTendency) + ((2.02)*(sqrt(PointError))))
  
  
  df.Final.Product <- dplyr::mutate(df.Final.Product,log10LowerPI=(Log10CentralTendency) 
                                    - ((2.02)*(sqrt(PointError))))
  df.Final.Product <- dplyr::mutate(df.Final.Product,LowerPI=10^(log10LowerPI))
  df.Final.Product <- dplyr::mutate(df.Final.Product,UppererPI=10^(log10UpperPI))
  df.Final.Product <- dplyr::mutate(df.Final.Product,CentralTendency=10^(Log10CentralTendency))
  
  
  ##### for grpahs 
  taxalist <- unique(df.Final.Product$Species)
  ggplot2::ggplot() +
    ggplot2::geom_point(data = df.Final.Product, aes(x = Conc_1_Mean_Standardized, y = Proportion)) +
    ggplot2::geom_line(data = df.Final.Product, aes(x =  CentralTendency, y = Proportion), col = 'red') +
    ggplot2::geom_line(data = df.Final.Product, aes(x = LowerPI, y = Proportion), linetype = 'dashed') + 
    ggplot2::geom_line(data = df.Final.Product, aes(x = UppererPI, y = Proportion), linetype = 'dashed') + 
    ggplot2::geom_text(data = df.Final.Product, aes(x = Conc_1_Mean_Standardized
                                                    , y = Proportion, label = Species), hjust = 0.5, size = 2) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), limits = c(0.003, max(df.Final.Product$Conc_1_Mean_Standardized))) +
    ggplot2::labs(x = expression(paste('Stressor Intensity')),y = 'Proportion Species')
}
