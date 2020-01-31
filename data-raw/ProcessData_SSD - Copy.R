# Prepare data for example for SSD()
#
# Erik.Leppo@tetratech.com
# 20180601
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "data_SSD.csv"
df <- read.csv(file.path(wd, "data-raw", myFile))

fn.2 <- "data_SSD_permethrin.csv"
df.2 <- read.csv(file.path(wd, "data-raw", fn.2))

fn.3 <- "data_SSD_generator.csv"
df.3 <- read.csv(file.path(wd, "data-raw", fn.3))

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_SSD <- df
devtools::use_data(data_SSD, overwrite = TRUE)

data_SSD_permethrin <- df.2
devtools::use_data(data_SSD_permethrin, overwrite=TRUE)

data_SSD_generator <- df.3
devtools::use_data(data_SSD_generator, overwrite=TRUE)
