# Prepare data for example for CoOccur()
#
# Erik.Leppo@tetratech.com
# 20180604
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZ/AZ_data_CoOccur.tsv"
df <- utils::read.delim(file.path(wd, "data-raw", myFile))

# no elevation as this example data is from San Diego not AZ

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_CoOccur <- df
devtools::use_data(data_CoOccur, overwrite = TRUE)
