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
myFile <- "AZChemDataFinal.tab"
df <- read.delim(file.path(wd, "data-raw", "AZ", myFile))

myCols <- c("Analyte", "SampDate", "ChemSampleID", "StationID_Master", "ResultValue", "ConvertTo", "StdParamName"
            , "SITE_ID", "FinalResultValue")

myCols[!(myCols %in% names(df))]

# Modify Names to match existing code
df$Analyte <- df$StdParamName
#df$SampDate <- as.Date(df$SampDate)
df$SampDate <- as.Date(df$SampDate, "%m/%d/%Y")  # 20190227, fix dates
#df$Excel_DateDays <- df$SampDate - as.Date(format(ISOdate(1899,12,30), "%Y-%m-%d"))

df$SITE_ID <- df$StationID_Master 

df$ChemSampleID <- paste(df$SITE_ID, df$SampDate, sep="_")
#df$StationID_Master <- df$SITE_ID
#df$ResultValue <- df$FinalResultValue
#df$ConvertTo <- df$Analyte
df$FinalResultValue <- df$ResultValue


# # Add elevation category (20180622)
# ## use Sites
# ec <- data_Sites[, c("StationID_Master", "ElevCategory")]
# dim(df)
# df <- merge(df, ec, by="StationID_Master", all.x=TRUE)
# dim(df)
# Comment out, 20190227
table(df$ElevCategory, useNA="ifany")


myCols[!(myCols %in% names(df))]

# 1.2. Process Data
View(head(df))
# QC check
dim(df)
# structure
str(df)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Save as RDA for use in package####
#
data_Chem <- df
devtools::use_data(data_Chem, overwrite = TRUE)
