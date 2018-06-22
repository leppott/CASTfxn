# Prepare data for example for AZ, Benthic MacroInvertebrate Counts
#
# Erik.Leppo@tetratech.com
# 20180612
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZBenthicCountsFinal.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

# format
df$CollDate <- as.Date(df$CollDate)
# add columns
df$StationID_Master <- df$StationID
df$BMISampID        <- paste(df$StationID, df$CollDate, df$BenSampID, df$RepNum, sep="_")
df$BMI.Metrics.SampID <- df$BMISampID

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
data_BMIcounts <- df
devtools::use_data(data_BMIcounts, overwrite = TRUE)
