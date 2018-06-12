# Prepare data for example for AZ, Chem Info
#
# Erik.Leppo@tetratech.com
# 20180611
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZChemInfoFinal.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

# Modify Names to match existing code
df$Analyte <- df$StdParamName
df$GroupNum <- as.numeric(df$Category)

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_ChemInfo <- df
devtools::use_data(data_ChemInfo, overwrite = TRUE)

