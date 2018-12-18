# Prepare data for example for AZ, Algae Metrics
#
# Erik.Leppo@tetratech.com
# 20180611
# 20181214
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZAlgaeMetrics.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

# Rename column to fit code (20181217)
colnames(df)[colnames(df)=="Alg.Metrics.SampID"] <- "Algae.Metrics.SampID"

#
# df <- unique(df.bmi[,c("StationID", "BenCollDate")])

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
