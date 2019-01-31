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
myFile <- "data_CoOccur.tsv"
df <- utils::read.delim(file.path(wd, "data-raw", myFile))

# no elevation as this example data is from San Diego not AZ

# AZ data
fn_AZ_Hi <- "AZCoOccurData_HI.tab"
df_AZ_Hi <- utils::read.delim(file.path(wd, "data-raw", "AZ", fn_AZ_Hi))

fn_AZ_Lo<- "AZCoOccurData_LO.tab"
df_AZ_Lo <- utils::read.delim(file.path(wd, "data-raw", "AZ", fn_AZ_Lo))


# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_CoOccur_CA <- df
devtools::use_data(data_CoOccur_CA, overwrite = TRUE)

data_CoOccur_AZ_Hi <- df_AZ_Hi
devtools::use_data(data_CoOccur_AZ_Hi, overwrite = TRUE)
