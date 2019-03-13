# Prepare data for example for AZ, Benthic Relative Abundance
#
# Erik.Leppo@tetratech.com
# 20180619
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZBenthicRelAbund.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile), stringsAsFactors = FALSE)

# Modify format
df$CollDate <- as.Date(df$CollDate, format="%m/%d/%Y")

names(df)

# Add columns
df$StationID_Master   <- df$StationID
df$Station_Date       <- df$CollDate
df$BMISampID          <- paste(df$StationID, df$CollDate, df$BenSampID, df$RepNum, sep="_")
df$BMI.Metrics.SampID <- df$BMISampID

dim(df)

# Add elevation category (20180622)
myFile <- "AZSitesFinal.tab"
df_sites <- read.delim(file.path(wd, "data-raw", "AZ", myFile))
## use Sites
ec <- df_sites[, c("StationID_Master", "ElevCategory")]
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
data_BMIRelAbund <- df
devtools::use_data(data_BMIRelAbund, overwrite = TRUE)
