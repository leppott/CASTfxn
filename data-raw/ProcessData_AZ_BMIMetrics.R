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
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

# QC names
myCol <- c("StationID_Master", "CollDate", "CSCI", "O_E", "MMI_Score")
myCol %in% names(df)
# add columns
df$StationID_Master   <- df$StationID
df$CollDate           <- as.Date(df$CollDate, format="%m/%d/%Y")
df$BenCollDate           <- as.Date(df$BenCollDate, format="%m/%d/%Y")
#df$BMISampID          <- paste(df$StationID, df$CollDate, df$BenSampID, df$RepNum, sep="_")
#df$BMI.Metrics.SampID <- df$BMISampID
df$CSCI               <- df$IBI
df$O_E                <- as.character(NA)
df$MMI_Score          <- df$IBI

# # Add elevation category (20180622)
# ## use Sites
# ec <- data_Sites[, c("StationID_Master", "ElevCategory")]
# dim(df)
# df <- merge(df, ec, by="StationID_Master", all.x=TRUE)
# dim(df)
# comment out 20190227
table(df$ElevCategory, useNA="ifany")

# df <- df[,c("StationID_Master","BMI.Metrics.SampID","ElevCategory","IBI", 
#             "TotalTaxSPL_Sc","DipTaxSPL_Sc","IntolTaxSPL_Sc","HBISPL_Sc",
#             "PlecoPct_Sc","ScrapPctSPL_Sc","ScrapTaxSPL_Sc","TrichTax_Sc", 
#             "EphemTax_Sc","EphemPct_Sc","Dom01PctSPL_Sc")]
# 20190228, remove, keep all columns


# 1.2. Process Data
View(head(df))
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_BMIMetrics <- df
devtools::use_data(data_BMIMetrics, overwrite = TRUE)
