# Prepare data for example for AZ, Algae Counts
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
myFile <- "AZAlgaeCountsFinal.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

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
data_Algcounts <- df
devtools::use_data(data_Algcounts, overwrite = TRUE)
