# Prepare data for example for AZ, Sample Summary
#
# Erik.Leppo@tetratech.com
# 20180611
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)
#library(readxl)

# 1. Get data and process#####
# 1.1. Import Data
# myFile <- "SampSummary_Test.xlsx"
# df0 <- read_excel(file.path(wd, "data-raw", "AZ", myFile), sheet="data.SampSummary")
myFile <- "AZSiteSummary.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile), stringsAsFactors = FALSE)

# Modify format (2090227, add format)
df$CollDate <- as.Date(df$CollDate, format="%m/%d/%Y")

# Add columns
#df$StationID_Master <- df$StationID
df$Station_Date <- df$CollDate


# Add elevation category (20180622)
myFile <- "AZSitesFinal.tab"
data_Sites <- read.delim(file.path(wd, "data-raw", "AZ", myFile))
## use Sites
ec <- data_Sites[, c("StationID_Master", "ElevCategory")]
dim(df)
df <- merge(df, ec, by="StationID_Master", all.x=TRUE)
dim(df)
table(df$ElevCategory, useNA="ifany")





# Revise Alg SampID (20181217)

# df$Algae.Metrics.SampID <- NA # none are in Alg data as of Dec 2018
# myFile <- "AZAlgaeCountsFinal.tab"
# df_AlgCounts <- read.delim(file.path(wd, "data-raw", "AZ", myFile), stringsAsFactors = FALSE)
# df_AlgCounts$CollDate    <- as.Date(df_AlgCounts$CollDate)
# myCol <- c("StationID_Master", "CollDate")
# x <- merge(df, unique(df_AlgCounts[, c(myCol, "Alg.SampID")]), by=myCol, all.x=TRUE)

# # Remove _EMAP and _Multihabitat from end.
# re_EMAP <- "(_EMAP)$"
# re_MH  <- "(_Multihabitat)$"
# df$Algae.Metrics.SampID <- sub(re_EMAP, "", df$Algae.Metrics.SampID)
# df$Algae.Metrics.SampID <- sub(re_MH, "", df$Algae.Metrics.SampID)

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
