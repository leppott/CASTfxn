# Prepare data for example for AZ, Reach Modified Status
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

# Create subset based on COMID
df <- data.frame(COMID=sort(unique(df.sites$COMID)), ReachModStatus=NA, ModReason=NA)
# change data type
df$ReachModStatus <- as.character(NA)
df$ModReason <- as.character(NA)


# Add elevation category (20180622)
## use Hi/Lo cluster
dim(df)
ec.hi.COMID <- data_Cluster_Hi$COMID
ec.lo.COMID <- data_Cluster_Lo$COMID
boo.hi <- df$COMID %in% ec.hi.COMID
boo.lo <- df$COMID %in% ec.lo.COMID
df$ElevCategory <- as.character(NA)
df[boo.hi, "ElevCategory"] <- "HI"
df[boo.lo, "ElevCategory"] <- "LO"
dim(df)
table(df$ElevCategory, useNA="ifany")


# 1.2. Process Data
View(df)
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_ReachMod <- df
devtools::use_data(data_ReachMod, overwrite = TRUE)
