# Prepare data for example for AZ, 303d Listing
#
# Erik.Leppo@tetratech.com
# 20180611
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZSitesFinal.tab"
df.sites <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

# Create new dataset
df <- unique(df.sites[,c("COMID", "STATION_NAME")])
df$ComID <- df$COMID
df$WATER.BODY.NAME <- as.character(df$STATION_NAME)
df <- df[,c(3,4)]
df$POLLUTANT <- as.character(NA)
df$FINAL.LISTING.DECISION <- as.character(NA)


# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_303d <- df
devtools::use_data(data_303d, overwrite = TRUE)
