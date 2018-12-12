# Prepare data for example for AZ, Benthic MacroInvertebrate Master Taxa
#
# Erik.Leppo@tetratech.com
# 20181211
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZBenthicMasterTaxa.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_BMIMasterTaxa <- df
devtools::use_data(data_BMIMasterTaxa, overwrite = TRUE)
