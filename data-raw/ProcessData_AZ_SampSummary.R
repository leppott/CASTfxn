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
myFile <- "SampSummary_Test.xlsx"
df <- read_excel(file.path(wd, "data-raw", "AZ", myFile), sheet="data.SampSummary")

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
