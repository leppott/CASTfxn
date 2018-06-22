# Prepare data for example for AZ, Sample Summary
#
# Erik.Leppo@tetratech.com
# 20180611
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)
library(readxl)

# 1. Get data and process#####
# 1.1. Import Data
# myFile <- "SampSummary_Test.xlsx"
# df0 <- read_excel(file.path(wd, "data-raw", "AZ", myFile), sheet="data.SampSummary")
myFile <- "AZSiteSummary.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile), stringsAsFactors = FALSE)

# Modify format
df$CollDate <- as.Date(df$CollDate)

# Add columns
df$StationID_Master <- df$StationID
df$Station_Date <- df$CollDate


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
data_SampSummary <- df
devtools::use_data(data_SampSummary, overwrite = TRUE)
