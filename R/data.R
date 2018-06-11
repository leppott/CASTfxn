#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title SSD example data
#' 
#' @description A dataset with example benthic macroinvertebrate data to be used with the SSD function.
#' 
#' @format A data frame with 16 rows and 28 variables:
#' \describe{
#'           \item{Taxa}{Taxa}
#'           \item{Exposure_mgperL}{Exposure mg/L}
#'           \item{ExposureRange}{ExposureRange}
#'           \item{OrganismStatus}{OrganismStatus}
#'           \item{ResponseType}{ResponseType}
#'           \item{StudyInfo}{StudyInfo}
#'           \item{Duration_days}{Duration_days}
#'           \item{Hardness_mgperL}{Hardness_mgperL}
#'           \item{pH}{pH}
#'           \item{Temperature_C}{Temperature_C}
#'           \item{Alkalinity_mgperL}{Alkalinity_mgperL}
#'           \item{OrganicCarbon_mgperL}{OrganicCarbon_mgperL}
#'           \item{DissolvedOxygen_mgperL}{DissolvedOxygen_mgperL}
#'           \item{Salinity_ppt}{Salinity_ppt}
#'           \item{Taxonomy}{Taxonomy}
#'           \item{Citation}{Citation}
#'           \item{Common_Name}{Common_Name}
#'           \item{ECOTOX_MED_Location}{ECOTOX_MED_Location}
#'           \item{Dataline}{Dataline}
#'           \item{SSD_Number}{SSD_Number}
#'           \item{SSD_Title}{SSD_Title}
#'           \item{CAS_Number}{CAS_Number}
#'           \item{ChemicalClass}{ChemicalClass}
#'           \item{ChemName}{ChemName}
#'           \item{Authors}{Authors}
#'           \item{Year}{Year}
#'           \item{Title}{Title}
#'           \item{Source}{Source}
#' }
#' @source example data
"data_SSD"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Co-Occurence example data
#' 
#' @description A dataset with example biological, chemical, habitat, and geo-physical parameters.
#' 
#' @format A data frame with 2,769 rows and 739 variables:
#' \describe{
#'           
#' }
#' @source example data
"data_CoOccur"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title SSD example data (permethrin)
#' 
#' @description A dataset with example benthic macroinvertebrate data for permethrin to be used with the SSD function.
#' 
#' @format A data frame with 48 rows and 2 variables:
#' \describe{
#'           \item{Taxa}{Taxa names}
#'           \item{Exposure}{Exposure mg/L}
#' }
#' @source example data
"data_SSD_permethrin"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title SSD example data (genorator file)
#' 
#' @description A dataset with example benthic macroinvertebrate data from 
#' USEPA ssd_generator_v1.xlsm file to be used with the SSD function.
#' 
#' https://www.epa.gov/caddis-vol4/caddis-volume-4-data-analysis-download-software
#' 
#' @format A data frame with 5 rows and 3 variables:
#' \describe{
#'           \item{Taxa}{Taxa names}
#'           \item{Exposure}{Exposure mg/L}
#'           \item{ResponseType}{Response type; LC50}
#' }
#' @source example data
"data_SSD_generator"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Sites example data
#' 
#' @description A dataset with example site information for use with the getSiteInfo function.
#' 
#' @format A data frame with 2,244 rows and 22 variables:
#' \describe{
#'           \item{STATION_CD}{Station Code}
#' }
#' @source example data
"data_Sites"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Reach modified status example data
#' 
#' @description A dataset with example reach modified status for use with the getSiteInfo function.
#' 
#' @format A data frame with 1,428 rows and 3 variables:
#' \describe{
#'           \item{COMID}{NHDplus COMID}
#'           \item{ReachModStatus}{Reach modified flow status}
#'           \item{ModReason}{Reach modified flow reason}
#' }
#' @source example data
"data_ReachMod"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title 303d listing example data
#' 
#' @description A dataset with example 303d listings for use with the getSiteInfo function.
#' 
#' @format A data frame with 2,244 rows and 4 variables:
#' \describe{
#'           \item{COMID}{NHDplus COMID}
#'           \item{WATER.BODY.NAME}{Waterbody Name}
#'           \item{POLLUTANT}{Pollutant}
#'           \item{FINAL.LISTING.DECISION}{Final listing decision}
#' }
#' @source example data
"data_303d"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title High elevation cluster example data
#' 
#' @description A dataset with example cluster data for use with the getSiteInfo function.
#' 
#' @format A data frame with 5,783 rows and 93 variables:
#' \describe{
#'           \item{COMID}{NHDplus COMID}
#' }
#' @source example data
"data_303d"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Benthic macroinvertebrate metrics example data
#' 
#' @description A dataset with example benthic macroinvertebrate (BMI) metric
#'  data for use with the getSiteInfo function.
#' 
#' @format A data frame with 957 rows and 29 variables:
#' \describe{
#'           \item{COMID}{NHDplus COMID}
#' }
#' @source example data
"data_BMIMetrics"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Algae metrics example data
#' 
#' @description A dataset with example algae metric data for use with the getSiteInfo function.
#' 
#' @format A data frame with 926 rows and 5 variables:
#' \describe{
#'           \item{StationCode}{Station ID}
#'           \item{SampleDate}{Station ID}
#'           \item{H20}{algae metric H20}
#'           \item{D18}{algae metric D18}
#'           \item{S2}{algae metric S2}
#' }
#' @source example data
"data_AlgMetrics"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Sample summary example data
#' 
#' @description A dataset with example sample summary for use with the getSiteInfo function.
#' 
#' @format A data frame with 3,577 rows and 7 variables:
#' \describe{
#'           \item{StationID_Master}{Station ID}
#'           \item{ColLDate}{Station ID}
#'           \item{Station_Date}{combined StationID and Date}
#'           \item{ChemCampID}{SampleID, Chem}
#'           \item{PhabSampID}{SampleID, Phab}
#'           \item{BMI.Metrics.SampID}{SampleID, BMI Metrics}
#'           \item{Algae.Metrics.SampID}{SampleID, Algae Metrics}
#' }
#' @source example data
"data_SampSummary"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~