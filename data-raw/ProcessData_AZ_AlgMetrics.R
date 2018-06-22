# Prepare data for example for AZ, Benthic MacroInvertebrate Metrics
#
# Erik.Leppo@tetratech.com
# 20180611
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZBenthicMetricsFinal.tab"
df.bmi <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

#
df <- unique(df.bmi[,c("StationID", "BenCollDate")])
df$StationCode <- df$StationID
df$SampleDate <- as.Date(df$BenCollDate)
df <- df[,c(3,4)]
df$Algae.Metrics.SampID <- as.character(NA)
df$H20 <- as.character(NA)
df$D18 <- as.character(NA)
df$S2 <- as.character(NA)
df$StationID_Master <- df$StationCode

# Add elevation category (20180622)
## use Sites
ec <- data_Sites[, c("StationID_Master", "ElevCategory")]
dim(df)
df <- merge(df, ec, by="StationID_Master", all.x=TRUE)
dim(df)
table(df$ElevCategory, useNA="ifany")

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_AlgMetrics <- df
devtools::use_data(data_AlgMetrics, overwrite = TRUE)
