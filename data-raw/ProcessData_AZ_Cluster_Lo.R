# Prepare data for example for AZ, Clusters (Low Elevation)
#
# Erik.Leppo@tetratech.com
# 20180622
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 0. Prep####
wd <- getwd() # assume is package directory
#library(devtools)

# 1. Get data and process#####
# 1.1. Import Data
myFile <- "AZ_ClustersLowElev.txt"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

# Check for colnames
myCol <- c("COMID", "H6_noland", "H6_land", "ElevWs", "WsAreaSqKm", "PrecipWs"
           , "TmeanWs", "W___AGRIC", "W___URBAN", "W___FOREST")
myCol %in% names(df)
myCol[!(myCol %in% names(df))]
# add extra columns
df$H6_noland <- as.character(NA)
df$H6_land <- as.character(NA)
df$PrecipWs <- df$Precip08Ws
df$TmeanWs <- df$Tmean08Ws
df$W___AGRIC <- as.character(NA)
df$W___URBAN <- as.character(NA)
df$W___FOREST <- as.character(NA)

df$clust_noland <- df$clust
df$clust_land <- df$clust

# Add elevation category (20180622)
df$ElevCategory <- "LO"

myCol %in% names(df)

# QC check
length(myCol) == sum(myCol %in% names(df))

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_Cluster_Lo <- df
devtools::use_data(data_Cluster_Lo, overwrite = TRUE)
