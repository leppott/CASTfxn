# Prepare data for example for AZ, Sites
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
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

# Modify Names to match existing code
#df$StationID_Master <- df$STATION_CD
#df$FinalLatitude <- df$LATITUDE
#df$FinalLongitude <- df$LONGITUDE
#df$WaterbodyName <- df$STATION_NAME
#df$GIS_County <- df$COUNTY_NAME
#df$CARefSite_2017 <- tolower(df$ReferenceStatus)
#df$COMID_NHD2 <- df$COMID

#
#df$CARefSite_2017[df$CARefSite_2017=="reference"] <- 1

# # Add elevation category (20180622)
# ## use Hi/Lo cluster
# ec.hi.COMID <- data_Cluster_Hi$COMID
# ec.lo.COMID <- data_Cluster_Lo$COMID
# boo.hi <- df$COMID %in% ec.hi.COMID
# boo.lo <- df$COMID %in% ec.lo.COMID
# df$ElevCategory <- as.character(NA)
# df[boo.hi, "ElevCategory"] <- "HI"
# df[boo.lo, "ElevCategory"] <- "LO"
# comment out, 20190227

# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_Sites <- df
devtools::use_data(data_Sites, overwrite = TRUE)
