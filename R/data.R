#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_303d ####
#' @title 303d listing example data
#' 
#' @description A dataset with example 303d listings for use with the getSiteInfo function.
#' 
#' @format A data frame with 1,536 rows and 5 variables:
#' \describe{
#'           \item{ComID}{NHDplus COMID}
#'           \item{WATER.BODY.NAME}{Waterbody Name}
#'           \item{POLLUTANT}{Pollutant}
#'           \item{FINAL.LISTING.DECISION}{Final listing decision}
#'           \item{ElevCategory}{Elevation Category}
#' }
#' @source example data
"data_303d"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_Algcounts ####
#' @title Algae Counts
#' 
#' @description Algae sample counts
#' 
#' @format A data frame with 4,882 observations on the following 14 variables
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{AlgalSampIdent}}{a numeric vector}
#'           \item{\code{RepNum}}{a numeric vector}
#'           \item{\code{CollDate}}{a factor with levels}
#'           \item{\code{CollMeth}}{a factor with levels \code{Alg-NS-ADEQ-multihabitat} \code{Alg-NS-EMAP-reachwide}}
#'           \item{\code{FinalID}}{a factor with levels }
#'           \item{\code{Individuals}}{a numeric vector}
#'           \item{\code{AdjFinalCount}}{a numeric vector}
#'           \item{\code{RelAbund}}{a numeric vector}
#'           \item{\code{Alg.SampID}}{a character vector}
#'           \item{\code{clust.hi}}{a character vector}
#'           \item{\code{clust.lo}}{a character vector}
#'           \item{\code{COMID_NHD2}}{a character vector}
#'           \item{\code{ElevCategory}}{a character vector}
#' }
#' 
#' @source example data
"data_Algcounts"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_AlgMasterTaxa ####
#' @title Algae Master Taxa
#' 
#' @description Algae Master Taxa
#' 
#' @format A data frame with 557 observations on the following 24 variables.
#' \describe{
#'            \item{\code{FinalID}}{a factor with levels }             
#'            \item{\code{GenusFinal}}{a factor with levels }
#'            \item{\code{Phylum}}{a factor with levels}
#'            \item{\code{Class}}{a factor with levels}
#'            \item{\code{Subclass}}{a factor with levels }
#'            \item{\code{Order}}{a factor with levels }
#'            \item{\code{Family}}{a factor with levels }
#'            \item{\code{Tribe}}{a logical vector}
#'            \item{\code{Genus}}{a factor with levels }
#'            \item{\code{Species}}{a factor with levels }
#'            \item{\code{Variety}}{a factor with levels }
#'            \item{\code{Poll_Tol_Class}}{a numeric vector}
#'            \item{\code{pH_class}}{a numeric vector}
#'            \item{\code{Salinity_class}}{a numeric vector}
#'            \item{\code{Nitrogen_Uptake_class}}{a numeric vector}
#'            \item{\code{Oxygen_class}}{a numeric vector}
#'            \item{\code{Saprobity_class}}{a numeric vector}
#'            \item{\code{Trophic_class}}{a numeric vector}
#'            \item{\code{Moisture_class}}{a numeric vector}
#'            \item{\code{Motility_class}}{a factor with levels \code{H} \code{M} \code{N} \code{V}}
#'            \item{\code{Phosphorus_class}}{a factor with levels \code{H} \code{L}}
#'            \item{\code{Nitrogen_class}}{a factor with levels \code{H} \code{L}}
#'            \item{\code{TaxaGroup}}{a logical vector}
#'            \item{\code{GroupName}}{a factor with levels }
#' }
#' 
#' @source example data
"data_AlgMasterTaxa"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# data_AlgMetrics ####
#' @title Algae metrics example data
#' 
#' @description A dataset with example algae metric data for use with the
#'  getSiteInfo and getAlgStressorResponses functions.
#' 
#' @format A data frame with 124 observations on the following 56 variables.
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{Algae.Metrics.SampID}}{a factor with levels }
#'           \item{\code{AlgalSampIdent}}{a numeric vector}
#'           \item{\code{RepNum}}{a numeric vector}
#'           \item{\code{CollDate}}{a factor with levels }
#'           \item{\code{clust.hi}}{a factor with levels }
#'           \item{\code{clust.lo}}{a factor with levels }
#'           \item{\code{COMID_NHD2}}{a character with vector }
#'           \item{\code{ElevCategory}}{a factor with levels }
#'           \item{\code{PollTolClass.1.tot}}{a numeric vector}
#'           \item{\code{PollTolClass.2.tot}}{a numeric vector}
#'           \item{\code{PollTolClass.3.tot}}{a numeric vector}
#'           \item{\code{PollTolClass.11.tot}}{a numeric vector}
#'           \item{\code{pHClass.2.tot}}{a numeric vector}
#'           \item{\code{pHClass.3.tot}}{a numeric vector}
#'           \item{\code{pHClass.4.tot}}{a numeric vector}
#'           \item{\code{pHClass.5.tot}}{a numeric vector}
#'           \item{\code{pHClass.6.tot}}{a numeric vector}
#'           \item{\code{SalinityClass.1.tot}}{a numeric vector}
#'           \item{\code{SalinityClass.2.tot}}{a numeric vector}
#'           \item{\code{SalinityClass.3.tot}}{a numeric vector}
#'           \item{\code{SalinityClass.4.tot}}{a numeric vector}
#'           \item{\code{N_UptakeClass.1.tot}}{a numeric vector}
#'           \item{\code{N_UptakeClass.2.tot}}{a numeric vector}
#'           \item{\code{N_UptakeClass.3.tot}}{a numeric vector}
#'           \item{\code{N_UptakeClass.4.tot}}{a numeric vector}
#'           \item{\code{OxygenClass.1.tot}}{a numeric vector}
#'           \item{\code{OxygenClass.2.tot}}{a numeric vector}
#'           \item{\code{OxygenClass.3.tot}}{a numeric vector}
#'           \item{\code{OxygenClass.4.tot}}{a numeric vector}
#'           \item{\code{OxygenClass.5.tot}}{a numeric vector}
#'           \item{\code{SaprobityClass.1.tot}}{a numeric vector}
#'           \item{\code{SaprobityClass.2.tot}}{a numeric vector}
#'           \item{\code{SaprobityClass.3.tot}}{a numeric vector}
#'           \item{\code{SaprobityClass.4.tot}}{a numeric vector}
#'           \item{\code{SaprobityClass.5.tot}}{a numeric vector}
#'           \item{\code{TrophicClass.1.tot}}{a numeric vector}
#'           \item{\code{TrophicClass.2.tot}}{a numeric vector}
#'           \item{\code{TrophicClass.3.tot}}{a numeric vector}
#'           \item{\code{TrophicClass.4.tot}}{a numeric vector}
#'           \item{\code{TrophicClass.5.tot}}{a numeric vector}
#'           \item{\code{TrophicClass.6.tot}}{a numeric vector}
#'           \item{\code{TrophicClass.7.tot}}{a numeric vector}
#'           \item{\code{MoistureClass.1.tot}}{a numeric vector}
#'           \item{\code{MoistureClass.2.tot}}{a numeric vector}
#'           \item{\code{MoistureClass.3.tot}}{a numeric vector}
#'           \item{\code{MoistureClass.4.tot}}{a numeric vector}
#'           \item{\code{MoistureClass.5.tot}}{a numeric vector}
#'           \item{\code{MotilityClass.H.tot}}{a numeric vector}
#'           \item{\code{MotilityClass.M.tot}}{a numeric vector}
#'           \item{\code{MotilityClass.N.tot}}{a numeric vector}
#'           \item{\code{MotilityClass.V.tot}}{a numeric vector}
#'           \item{\code{PhosphorusClass.H.tot}}{a numeric vector}
#'           \item{\code{PhosphorusClass.L.tot}}{a numeric vector}
#'           \item{\code{NitrogenClass.H.tot}}{a numeric vector}
#'           \item{\code{NitrogenClass.L.tot}}{a numeric vector}
#' }
#' @source example data
"data_AlgMetrics"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_BMIcounts ####
#' @title Benthic Macroinvertebrate counts example data
#' 
#' @description A dataset with example benthic macroinvertebrate (BMI) counts 
#' for use with the getStresssorSpecificRegressions function.
#' 
#' @format A data frame with 55,283 rows and 19 variables:
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{SampleID}}{Sample ID}
#'           \item{\code{Elevation}}{a numeric vector}
#'           \item{\code{InvertReg}}{a factor with levels \code{cold} \code{Cold} \code{warm} \code{Warm}}
#'           \item{\code{BenSampID}}{a numeric vector}
#'           \item{\code{RepNum}}{a numeric vector}
#'           \item{\code{CollDate}}{a Date}
#'           \item{\code{FieldGearID}}{a factor with levels \code{D-frame di}}
#'           \item{\code{Habitat}}{a factor with levels \code{Edge} \code{Multi-habitat} \code{Pool} \code{Riffle} \code{Run}}
#'           \item{\code{FinalID}}{a factor with levels }
#'           \item{\code{TotInds}}{a numeric vector}
#'           \item{\code{TotIndsCorr}}{a numeric vector}
#'           \item{\code{BMISampID}}{a character vector}
#'           \item{\code{BMI.Metrics.SampID}}{a character vector}
#'           \item{\code{ElevCategory}}{a character vector}
#'           \item{\code{clust.hi}}{a character vector}
#'           \item{\code{clust.lo}}{a character vector}
#'           \item{\code{COMID_NHD2}}{a character vector}
#'           \item{\code{RelAbundInds}}{a numeric vector}
#' }
#' @source example data
"data_BMIcounts"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_BMIMasterTaxa ####
#' @title data_BMIMasterTaxa
#' 
#' @description Benthic Macroinvertebrate, Master Taxa
#' 
#' @format A data frame with 824 observations on the following 60 variables.
#' \describe{
#'           \item{\code{GenusFinal}}{a factor with levels }
#'           \item{\code{STORET}}{a logical vector}
#'           \item{\code{BenTaxaID}}{a numeric vector}
#'           \item{\code{WQX_FinalID}}{a factor with levels }
#'           \item{\code{FinalID}}{a factor with levels }
#'           \item{\code{TaxaGroup}}{a factor with levels }
#'           \item{\code{Phylum}}{a factor with levels }
#'           \item{\code{Class}}{a factor with levels }
#'           \item{\code{Order}}{a factor with levels }
#'           \item{\code{Family}}{a factor with levels }
#'           \item{\code{Tribe}}{a factor with levels }
#'           \item{\code{Species}}{a factor with levels }
#'           \item{\code{Variety}}{a factor with levels \code{Type II}}
#'           \item{\code{TolVal}}{a numeric vector}
#'           \item{\code{TolValSource}}{a factor with levels }
#'           \item{\code{Fam.TV}}{a numeric vector}
#'           \item{\code{Fam.TV.reference}}{a factor with levels \code{EPA Draft} \code{Family tv} \code{Raw Data}}
#'           \item{\code{FFG}}{a factor with levels }
#'           \item{\code{FFGSource}}{a factor with levels }
#'           \item{\code{Habit}}{a factor with levels }
#'           \item{\code{HabitSource}}{a factor with levels \code{AZ DEQ}}
#'           \item{\code{InBenthics}}{a factor with levels \code{Y}}
#'           \item{\code{Fam.FFG}}{a factor with levels }
#'           \item{\code{Fam.FFG.reference}}{a factor with levels \code{EPA Draft} \code{MC3} \code{Raw Data} \code{Unclassified by EI}}
#'           \item{\code{Hilsenhoff.Biotic.Index}}{a numeric vector}
#'           \item{\code{Fine.Sediment.Biotic.Index}}{a numeric vector}
#'           \item{\code{Temperature.Preferance.Metric}}{a numeric vector}
#'           \item{\code{Metals.Tolerance.Index}}{a numeric vector}
#'           \item{\code{HT}}{a factor with levels \code{HT}}
#'           \item{\code{Tany}}{a factor with levels \code{TANY}}
#'           \item{\code{Voltinism}}{a factor with levels }
#'           \item{\code{LifeCycleSource}}{a factor with levels \code{AZ DEQ}}
#'           \item{\code{CharGroupID}}{a logical vector}
#'           \item{\code{RowID}}{a logical vector}
#'           \item{\code{Photo}}{a logical vector}
#'           \item{\code{AdminCheck}}{a logical vector}
#'           \item{\code{DateNameRevised}}{a factor with levels \code{2001-08-13 15:42:54} \code{2001-08-13 15:50:58} \code{2001-08-24 00:00:00}}
#'           \item{\code{EnterDate}}{a factor with levels }
#'           \item{\code{Comments}}{a factor with levels }
#'           \item{\code{Excluded.Taxa.}}{a logical vector}
#'           \item{\code{TIN}}{a numeric vector}
#'           \item{\code{NonBenthic}}{a logical vector}
#'           \item{\code{NeedsReview}}{a logical vector}
#'           \item{\code{STORET_CharName}}{a factor with levels }
#'           \item{\code{STORET_SpNum}}{a numeric vector}
#'           \item{\code{STORET_Comment}}{a factor with levels \code{(DH)} \code{(LH)}}
#'           \item{\code{OTU_Code}}{a numeric vector}
#'           \item{\code{OTU_Name}}{a factor with levels}
#'           \item{\code{TaxaCode}}{a factor with multiple levels }
#'           \item{\code{TSN}}{a factor with multiple levels }
#'           \item{\code{ParTSN}}{a numeric vector}
#'           \item{\code{uBio.Number}}{a factor with levels }
#'           \item{\code{Phylogenetic.Sort}}{a numeric vector}
#'           \item{\code{OLD.TolVal}}{a numeric vector}
#'           \item{\code{FFG_OLD}}{a factor with levels \code{CF} \code{CG} \code{Herbivores} \code{MH} \code{OM} \code{PA} \code{PH} \code{Piercer-Herbivores} \code{PR} \code{SC} \code{SH} \code{XY}}
#'           \item{\code{Invasive}}{a logical vector}
#'           \item{\code{FinesTolVal_hi}}{a numeric vector}
#'           \item{\code{FinesTolVal_low}}{a numeric vector}
#'           \item{\code{SpecCondTolVal}}{a numeric vector}
#'           \item{\code{WQX_UnidentifiedSpecies}}{a character vector}
#' }
#' 
#' @source example data, Arizona
"data_BMIMasterTaxa"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_BMIMetrics ####
#' @title Benthic macroinvertebrate metrics example data
#' 
#' @description A dataset with example benthic macroinvertebrate (BMI) metric
#'  data for use with the getSiteInfo function.
#' 
#' @format A data frame with 956 rows and 26 variables:
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{BenCollDate}}{a factor with levels }
#'           \item{\code{NarRat}}{a factor with levels \code{Inconclusive} \code{Meets} \code{Violates}}
#'           \item{\code{IBI}}{a numeric vector}
#'           \item{\code{TotalTaxSPL_Sc}}{a numeric vector}
#'           \item{\code{DipTaxSPL_Sc}}{a numeric vector}
#'           \item{\code{IntolTaxSPL_Sc}}{a numeric vector}
#'           \item{\code{HBISPL_Sc}}{a numeric vector}
#'           \item{\code{PlecoPct_Sc}}{a numeric vector}
#'           \item{\code{ScrapPctSPL_Sc}}{a numeric vector}
#'           \item{\code{ScrapTaxSPL_Sc}}{a numeric vector}
#'           \item{\code{TrichTax_Sc}}{a numeric vector}
#'           \item{\code{EphemTax_Sc}}{a numeric vector}
#'           \item{\code{EphemPct_Sc}}{a numeric vector}
#'           \item{\code{Dom01PctSPL_Sc}}{a numeric vector}
#'           \item{\code{CollDate}}{a Date}
#'           \item{\code{BMISampID}}{a character vector}
#'           \item{\code{BMI.Metrics.SampID}}{a character vector}
#'           \item{\code{CSCI}}{a numeric vector}
#'           \item{\code{O_E}}{a character vector}
#'           \item{\code{MMI_Score}}{a numeric vector}
#'           \item{\code{ElevCategory}}{a character vector}#'           
#'           \item{\code{clust.hi}}{a character vector}
#'           \item{\code{clust.lo}}{a character vector}
#'           \item{\code{COMID_NHD2}}{a character vector}
#'           \item{\code{SampYear}}{a character vector} 
#' }
#' @source example data
"data_BMIMetrics"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_BMIRelAbund ####
#' @title BMI Relative Abundance
#' 
#' @description Benthic Macroinvertebrate, Relative Abundances
#' 
#' @format A data frame with 55,319 rows and 12 variables
#' \describe{
#'           \item{StationID}{Station ID}
#'           \item{BenSampID}{BenSampID}
#'           \item{RepNum}{RepNum}
#'           \item{CollDate}{Sample collection date; YYYY-MM-DD}
#'           \item{SampleID}{SampleID}
#'           \item{FinalID}{FinalID}
#'           \item{RelAbundInds}{Relative abundance}
#'           \item{StationID_Master}{StationID_Master}
#'           \item{Station_Date}{Station_Date}
#'           \item{BMISampID}{BMISampID}
#'           \item{BMI.Metrics.SampID}{BMI.Metrics.SampID}
#'           \item{ElevCategory}{Elevation Category}
#' }
#' 
#' @source example data
"data_BMIRelAbund"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# data_Chem ####
#' @title Chem data
#' 
#' @description Chem data
#' 
#'  @format A data frame with 104,577 observations on the following 19 variables.
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{SITE_ID}}{a factor with levels }
#'           \item{\code{SampDate}}{a Date}
#'           \item{\code{SampTime}}{a numeric vector}
#'           \item{\code{SampleType}}{a factor with levels \code{A} \code{B} \code{C} \code{F} \code{G} \code{I} \code{M} \code{W} \code{Z}}
#'           \item{\code{FlowRegimeCode}}{a factor with levels}
#'           \item{\code{StdParamName}}{a factor with levels }
#'           \item{\code{ResultText}}{a factor with levels }
#'           \item{\code{FinalResultValue}}{a numeric vector}
#'           \item{\code{ResultHalfMDL}}{a numeric vector}
#'           \item{\code{Analyte}}{a factor with levels }
#'           \item{\code{ChemSampleID}}{a character vector}
#'           \item{\code{ResultValue}}{a numeric vector}
#'           \item{\code{ConvertTo}}{a factor with levels }
#'           \item{\code{ElevCategory}}{a character vector}
#'           \item{\code{SampYear}}{a numeric vector}
#'           \item{\code{clust.hi}}{a character vector}
#'           \item{\code{clust.lo}}{a character vector}
#'           \item{\code{COMID_NHD2}}{a character vector}
#' }
#' 
#' @source example data
"data_Chem"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_ChemInfo ####
#' @title Chem Parameters
#' 
#' @description Chem Parameters
#' 
#' @format A data frame with 299 observations on the following 16 variables.
#' \describe{
#'           \item{\code{ANALYSIS_TYPE}}{a factor with levels }
#'           \item{\code{FinalUnit}}{a factor with levels }
#'           \item{\code{LogTransf}}{a numeric vector}
#'           \item{\code{SSD}}{a numeric vector}
#'           \item{\code{SSTV}}{a factor with levels \code{} \code{SpecCondTolVal}}
#'           \item{\code{SensMin}}{a numeric vector}
#'           \item{\code{SensMax}}{a numeric vector}
#'           \item{\code{TolMin}}{a numeric vector}
#'           \item{\code{TolMax}}{a numeric vector}
#'           \item{\code{UseInStressorID}}{a numeric vector}
#'           \item{\code{Analyte}}{a factor with levels }
#'           \item{\code{GroupNum}}{a numeric vector}
#'           \item{\code{GroupName}}{a factor with levels \code{Bacteria} \code{Ions} \code{Metals_metalloids} \code{Nutrients} \code{Organic} \code{Organochlorine} \code{Organohalide} \code{Organophosphate} \code{PAHs & Phthalates} \code{Radiation} \code{Water quality}}
#'           \item{\code{CHEMICAL_NAME}}{a character vector}
#'           \item{\code{DirIncStress}}{a character vector}
#'           \item{\code{StdParamName}}{a character vector}
#' }
#'
#' @source example data
"data_ChemInfo"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_Cluster_Hi ####
#' @title High elevation cluster example data
#' 
#' @description A dataset with example cluster data for use with the getSiteInfo function.
#' 
#' @format A data frame with 5,929 rows and 95 variables:
#' \describe{
#'            \item{\code{COMID}}{NHD+ COMID}
#'            \item{\code{Al2O3Cat}}{a numeric vector}
#'            \item{\code{Al2O3Ws}}{a numeric vector}
#'            \item{\code{BFICat}}{a numeric vector}
#'            \item{\code{BFIWs}}{a numeric vector}
#'            \item{\code{CompStrgthCat}}{a numeric vector}
#'            \item{\code{CompStrgthWs}}{a numeric vector}
#'            \item{\code{KffactCat}}{a numeric vector}
#'            \item{\code{KffactWs}}{a numeric vector}
#'            \item{\code{Na2OWs}}{a numeric vector}
#'            \item{\code{PctSilicicWs}}{a numeric vector}
#'            \item{\code{SandCat}}{a numeric vector}
#'            \item{\code{SandWs}}{a numeric vector}
#'            \item{\code{SiO2Cat}}{a numeric vector}
#'            \item{\code{SiO2Ws}}{a numeric vector}
#'            \item{\code{Tmax8110Cat}}{a numeric vector}
#'            \item{\code{Tmax8110Ws}}{a numeric vector}
#'            \item{\code{Tmean08Cat}}{a numeric vector}
#'            \item{\code{Tmean08Ws}}{a numeric vector}
#'            \item{\code{Tmean09Cat}}{a numeric vector}
#'            \item{\code{Tmean09Ws}}{a numeric vector}
#'            \item{\code{Tmean8110Cat}}{a numeric vector}
#'            \item{\code{Tmean8110Ws}}{a numeric vector}
#'            \item{\code{Tmin8110Cat}}{a numeric vector}
#'            \item{\code{Tmin8110Ws}}{a numeric vector}
#'            \item{\code{WtDepCat}}{a numeric vector}
#'            \item{\code{WtDepWs}}{a numeric vector}
#'            \item{\code{CaOCat}}{a numeric vector}
#'            \item{\code{CaOWs}}{a numeric vector}
#'            \item{\code{CatAreaSqKm}}{a numeric vector}
#'            \item{\code{CatAreaSqKmRp100}}{a numeric vector}
#'            \item{\code{ClayCat}}{a numeric vector}
#'            \item{\code{ClayWs}}{a numeric vector}
#'            \item{\code{Fe2O3Cat}}{a numeric vector}
#'            \item{\code{Fe2O3Ws}}{a numeric vector}
#'            \item{\code{HydrlCondCat}}{a numeric vector}
#'            \item{\code{HydrlCondWs}}{a numeric vector}
#'            \item{\code{K2OCat}}{a numeric vector}
#'            \item{\code{MgOCat}}{a numeric vector}
#'            \item{\code{MgOWs}}{a numeric vector}
#'            \item{\code{Na2OCat}}{a numeric vector}
#'            \item{\code{OmCat}}{a numeric vector}
#'            \item{\code{OmWs}}{a numeric vector}
#'            \item{\code{PctNonCarbResidCat}}{a numeric vector}
#'            \item{\code{PctNonCarbResidWs}}{a numeric vector}
#'            \item{\code{PctSilicicCat}}{a numeric vector}
#'            \item{\code{PermCat}}{a numeric vector}
#'            \item{\code{PermWs}}{a numeric vector}
#'            \item{\code{Precip08Cat}}{a numeric vector}
#'            \item{\code{Precip08Ws}}{a numeric vector}
#'            \item{\code{Precip09Cat}}{a numeric vector}
#'            \item{\code{Precip09Ws}}{a numeric vector}
#'            \item{\code{Precip8110Cat}}{a numeric vector}
#'            \item{\code{Precip8110Ws}}{a numeric vector}
#'            \item{\code{QC_01}}{a numeric vector}
#'            \item{\code{QC_02}}{a numeric vector}
#'            \item{\code{QC_03}}{a numeric vector}
#'            \item{\code{QC_04}}{a numeric vector}
#'            \item{\code{QC_05}}{a numeric vector}
#'            \item{\code{QC_06}}{a numeric vector}
#'            \item{\code{QC_07}}{a numeric vector}
#'            \item{\code{QC_08}}{a numeric vector}
#'            \item{\code{QC_09}}{a numeric vector}
#'            \item{\code{QC_10}}{a numeric vector}
#'            \item{\code{QC_11}}{a numeric vector}
#'            \item{\code{QC_12}}{a numeric vector}
#'            \item{\code{QC_MA}}{a numeric vector}
#'            \item{\code{RckDepCat}}{a numeric vector}
#'            \item{\code{RckDepWs}}{a numeric vector}
#'            \item{\code{RunoffCat}}{a numeric vector}
#'            \item{\code{RunoffWs}}{a numeric vector}
#'            \item{\code{WetIndexCat}}{a numeric vector}
#'            \item{\code{WetIndexWs}}{a numeric vector}
#'            \item{\code{WsAreaSqKm}}{a numeric vector}
#'            \item{\code{WsAreaSqKmRp100}}{a numeric vector}
#'            \item{\code{ElevCat}}{a numeric vector}
#'            \item{\code{ElevWs}}{a numeric vector}
#'            \item{\code{K2OWs}}{a numeric vector}
#'            \item{\code{NCat}}{a numeric vector}
#'            \item{\code{NWs}}{a numeric vector}
#'            \item{\code{P2O5Cat}}{a numeric vector}
#'            \item{\code{P2O5Ws}}{a numeric vector}
#'            \item{\code{SCat}}{a numeric vector}
#'            \item{\code{SLOPE}}{a numeric vector}
#'            \item{\code{SWs}}{a numeric vector}
#'            \item{\code{clust}}{a numeric vector}
#'            \item{\code{H6_noland}}{a character vector}
#'            \item{\code{H6_land}}{a character vector}
#'            \item{\code{PrecipWs}}{a numeric vector}
#'            \item{\code{TmeanWs}}{a numeric vector}
#'            \item{\code{W___AGRIC}}{a character vector}
#'            \item{\code{W___URBAN}}{a character vector}
#'            \item{\code{W___FOREST}}{a character vector}
#'            \item{\code{clust_noland}}{a numeric vector}
#'            \item{\code{clust_land}}{a numeric vector}
#' }
#' @source example data
"data_Cluster_Hi"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_Cluster_Lo ####
#' @title Low  elevation cluster example data
#' 
#' @description A dataset with example cluster data for use with the getSiteInfo function.
#' 
#' @format A data frame with 10,707 rows and 97 variables:
#' \describe{
#'           \item{\code{COMID}}{a numeric vector}       
#'           \item{\code{Al2O3Cat}}{a numeric vector}
#'           \item{\code{Al2O3Ws}}{a numeric vector}
#'           \item{\code{BFICat}}{a numeric vector}
#'           \item{\code{BFIWs}}{a numeric vector}
#'           \item{\code{CompStrgthCat}}{a numeric vector}
#'           \item{\code{CompStrgthWs}}{a numeric vector}
#'           \item{\code{KffactCat}}{a numeric vector}
#'           \item{\code{KffactWs}}{a numeric vector}
#'           \item{\code{Na2OWs}}{a numeric vector}
#'           \item{\code{PctSilicicWs}}{a numeric vector}
#'           \item{\code{SandCat}}{a numeric vector}
#'           \item{\code{SandWs}}{a numeric vector}
#'           \item{\code{SiO2Cat}}{a numeric vector}
#'           \item{\code{SiO2Ws}}{a numeric vector}
#'           \item{\code{Tmax8110Cat}}{a numeric vector}
#'           \item{\code{Tmax8110Ws}}{a numeric vector}
#'           \item{\code{Tmean08Cat}}{a numeric vector}
#'           \item{\code{Tmean08Ws}}{a numeric vector}
#'           \item{\code{Tmean09Cat}}{a numeric vector}
#'           \item{\code{Tmean09Ws}}{a numeric vector}
#'           \item{\code{Tmean8110Cat}}{a numeric vector}
#'           \item{\code{Tmean8110Ws}}{a numeric vector}
#'           \item{\code{Tmin8110Cat}}{a numeric vector}
#'           \item{\code{Tmin8110Ws}}{a numeric vector}
#'           \item{\code{WtDepCat}}{a numeric vector}
#'           \item{\code{WtDepWs}}{a numeric vector}
#'           \item{\code{CaOCat}}{a numeric vector}
#'           \item{\code{CaOWs}}{a numeric vector}
#'           \item{\code{CatAreaSqKm}}{a numeric vector}
#'           \item{\code{CatAreaSqKmRp100}}{a numeric vector}
#'           \item{\code{ClayCat}}{a numeric vector}
#'           \item{\code{ClayWs}}{a numeric vector}
#'           \item{\code{Fe2O3Cat}}{a numeric vector}
#'           \item{\code{Fe2O3Ws}}{a numeric vector}
#'           \item{\code{HydrlCondCat}}{a numeric vector}
#'           \item{\code{HydrlCondWs}}{a numeric vector}
#'           \item{\code{K2OCat}}{a numeric vector}
#'           \item{\code{MgOCat}}{a numeric vector}
#'           \item{\code{MgOWs}}{a numeric vector}
#'           \item{\code{Na2OCat}}{a numeric vector}
#'           \item{\code{OmCat}}{a numeric vector}
#'           \item{\code{OmWs}}{a numeric vector}
#'           \item{\code{PctAlluvCoastCat}}{a numeric vector}
#'           \item{\code{PctAlluvCoastWs}}{a numeric vector}
#'           \item{\code{PctNonCarbResidCat}}{a numeric vector}
#'           \item{\code{PctNonCarbResidWs}}{a numeric vector}
#'           \item{\code{PctSilicicCat}}{a numeric vector}
#'           \item{\code{PermCat}}{a numeric vector}
#'           \item{\code{PermWs}}{a numeric vector}
#'           \item{\code{Precip08Cat}}{a numeric vector}
#'           \item{\code{Precip08Ws}}{a numeric vector}
#'           \item{\code{Precip09Cat}}{a numeric vector}
#'           \item{\code{Precip09Ws}}{a numeric vector}
#'           \item{\code{Precip8110Cat}}{a numeric vector}           
#'           \item{\code{Precip8110Ws}}{a numeric vector}
#'           \item{\code{QC_01}}{a numeric vector}
#'           \item{\code{QC_02}}{a numeric vector}
#'           \item{\code{QC_03}}{a numeric vector}
#'           \item{\code{QC_04}}{a numeric vector}
#'           \item{\code{QC_05}}{a numeric vector}
#'           \item{\code{QC_06}}{a numeric vector}
#'           \item{\code{QC_07}}{a numeric vector}
#'           \item{\code{QC_08}}{a numeric vector}
#'           \item{\code{QC_09}}{a numeric vector}
#'           \item{\code{QC_10}}{a numeric vector}
#'           \item{\code{QC_11}}{a numeric vector}
#'           \item{\code{QC_12}}{a numeric vector}
#'           \item{\code{QC_MA}}{a numeric vector}
#'           \item{\code{RckDepCat}}{a numeric vector}
#'           \item{\code{RckDepWs}}{a numeric vector}
#'           \item{\code{RunoffCat}}{a numeric vector}
#'           \item{\code{RunoffWs}}{a numeric vector}
#'           \item{\code{WetIndexCat}}{a numeric vector}
#'           \item{\code{WetIndexWs}}{a numeric vector}
#'           \item{\code{WsAreaSqKm}}{a numeric vector}
#'           \item{\code{WsAreaSqKmRp100}}{a numeric vector}
#'           \item{\code{ElevCat}}{a numeric vector}
#'           \item{\code{ElevWs}}{a numeric vector}
#'           \item{\code{K2OWs}}{a numeric vector}
#'           \item{\code{NCat}}{a numeric vector}
#'           \item{\code{NWs}}{a numeric vector}
#'           \item{\code{P2O5Cat}}{a numeric vector}
#'           \item{\code{P2O5Ws}}{a numeric vector}
#'           \item{\code{SCat}}{a numeric vector}
#'           \item{\code{SLOPE}}{a numeric vector}
#'           \item{\code{SWs}}{a numeric vector}
#'           \item{\code{clust}}{a numeric vector}
#'           \item{\code{H6_noland}}{a character vector}
#'           \item{\code{H6_land}}{a character vector}
#'           \item{\code{PrecipWs}}{a numeric vector}
#'           \item{\code{TmeanWs}}{a numeric vector}
#'           \item{\code{W___AGRIC}}{a character vector}
#'           \item{\code{W___URBAN}}{a character vector}
#'           \item{\code{W___FOREST}}{a character vector}
#'           \item{\code{clust_noland}}{a numeric vector}
#'           \item{\code{clust_land}}{a numeric vector}
#' }
#' @source example data
"data_Cluster_Lo"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_CoOccur_CA *fix*####
#' @title Co-Occurrence example data (CA)
#' 
#' @description A dataset from California with example biological, chemical, habitat, and geo-physical parameters.
#' 
#' @format A data frame with 2,769 rows and 739 variables:
#' \describe{
#'               \item{\code{StationID_Master}}{a character vector}
#'               \item{\code{SWAMP_Station_Code}}{a character vector}
#'               \item{\code{CanonicalStationID}}{a character vector}
#'               \item{\code{Stream_Name}}{a character vector}
#'               \item{\code{SampleDate}}{a character vector}
#'               \item{\code{County}}{a character vector}
#'               \item{\code{Latitude}}{a numeric vector}
#'               \item{\code{Longitude}}{a numeric vector}
#'               \item{\code{CSCI}}{a numeric vector}
#'               \item{\code{Group}}{a numeric vector}
#'               \item{\code{CollDate}}{a character vector}
#'               \item{\code{Acenaphthene_ng_g}}{a numeric vector}
#'               \item{\code{Acenaphthene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Acenaphthylene_ng_g}}{a numeric vector}
#'               \item{\code{Acenaphthylene_uf_ug_L}}{a numeric vector}
#'               \item{\code{AFDM_Algae_Particulate_g_m2}}{a numeric vector}
#'               \item{\code{Ag_f_ug_L}}{a numeric vector}
#'               \item{\code{Ag_uf_ug_L}}{a numeric vector}
#'               \item{\code{Ag_uf_mg_kg}}{a numeric vector}
#'               \item{\code{Al_f_ug_L}}{a numeric vector}
#'               \item{\code{Al_uf_ug_L}}{a numeric vector}
#'               \item{\code{Al_uf_mg_kg}}{a numeric vector}
#'               \item{\code{Aldrin_ng_g}}{a numeric vector}
#'               \item{\code{Aldrin_ppb}}{a numeric vector}
#'               \item{\code{Aldrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Alkalinity_mg_L}}{a numeric vector}
#'               \item{\code{AlkalinityCaCO3_f_mg_L}}{a numeric vector}
#'               \item{\code{AlkalinityCaCO3_uf_mg_L}}{a numeric vector}
#'               \item{\code{Allethrin_ng_g}}{a numeric vector}
#'               \item{\code{Allethrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Ametryn_uf_ug_L}}{a numeric vector}
#'               \item{\code{Ametryne_ppb}}{a logical vector}
#'               \item{\code{AnatoxinA_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{AnatoxinA_uf_ug_L}}{a numeric vector}
#'               \item{\code{ANC_uf_ueq_L}}{a numeric vector}
#'               \item{\code{Anthracene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Anthracene_uf_ng_g}}{a numeric vector}
#'               \item{\code{As_f_mg_L}}{a numeric vector}
#'               \item{\code{As_uf_mg_kg}}{a numeric vector}
#'               \item{\code{As_uf_mg_L}}{a numeric vector}
#'               \item{\code{Aspon_ppb}}{a logical vector}
#'               \item{\code{Aspon_uf_ug_L}}{a numeric vector}
#'               \item{\code{Atraton_ppb}}{a numeric vector}
#'               \item{\code{Atraton_uf_ug_L}}{a numeric vector}
#'               \item{\code{Atrazine_ppb}}{a numeric vector}
#'               \item{\code{Atrazine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Azinphos.ethyl_uf_ug_L}}{a numeric vector}
#'               \item{\code{Azinphos.methyl_uf_ug_L}}{a numeric vector}
#'               \item{\code{AzinphosEthyl_ppb}}{a logical vector}
#'               \item{\code{AzinphosMethyl_ppb}}{a logical vector}
#'               \item{\code{B_uf_mg_L}}{a numeric vector}
#'               \item{\code{Ba_uf_mg_kg}}{a numeric vector}
#'               \item{\code{Be_uf_mg_kg}}{a numeric vector}
#'               \item{\code{Benz.a.anthracene_ng_g}}{a numeric vector}
#'               \item{\code{Benz.a.anthracene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Benzene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Benzo.a.pyrene_ng_g}}{a numeric vector}
#'               \item{\code{Benzo.a.pyrene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Benzo.b.fluoranthene_ng_g}}{a numeric vector}
#'               \item{\code{Benzo.b.fluoranthene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Benzo.e.pyrene_ng_g}}{a numeric vector}
#'               \item{\code{Benzo.e.pyrene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Benzo.g.h.i.perylene_ng_g}}{a numeric vector}
#'               \item{\code{Benzo.g.h.i.perylene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Benzo.k.fluoranthene_ng_g}}{a numeric vector}
#'               \item{\code{Benzo.k.fluoranthene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Bicarbonate_HCO3_mg_L}}{a numeric vector}
#'               \item{\code{Bifenthrin_ng_g}}{a numeric vector}
#'               \item{\code{Bifenthrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Biphenyl_ng_g}}{a numeric vector}
#'               \item{\code{Biphenyl_uf_ug_L}}{a numeric vector}
#'               \item{\code{BOD_mg_L}}{a numeric vector}
#'               \item{\code{Bolstar_ng_g}}{a numeric vector}
#'               \item{\code{Bolstar_uf_ug_L}}{a numeric vector}
#'               \item{\code{Bromobenzene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Bromochloromethane_uf_ug_L}}{a numeric vector}
#'               \item{\code{Bromodichloromethane_uf_ug_L}}{a numeric vector}
#'               \item{\code{Bromoform_uf_ug_L}}{a numeric vector}
#'               \item{\code{Butylbenzene_n_uf_ug_L}}{a numeric vector}
#'               \item{\code{Butylbenzene_sec_uf_ug_L}}{a numeric vector}
#'               \item{\code{Butylbenzene_tert_uf_ug_L}}{a numeric vector}
#'               \item{\code{Ca_f_mg_L}}{a numeric vector}
#'               \item{\code{Ca_uf_mg_L}}{a numeric vector}
#'               \item{\code{Caffeine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Carbadox_uf_ug_L}}{a numeric vector}
#'               \item{\code{Carbamazepine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Carbonate_CaCO3_mg_L}}{a numeric vector}
#'               \item{\code{CarbonTetrachloride_uf_ug_L}}{a numeric vector}
#'               \item{\code{Carbophenothion_ppb}}{a numeric vector}
#'               \item{\code{Carbophenothion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Cd_ug_g}}{a numeric vector}
#'               \item{\code{Cd_f_mg_L}}{a numeric vector}
#'               \item{\code{Cd_uf_mg_L}}{a numeric vector}
#'               \item{\code{CHECK.THIS}}{a numeric vector}
#'               \item{\code{Chlor_a_mg_m2}}{a numeric vector}
#'               \item{\code{Chlor_a_mg_m3}}{a numeric vector}
#'               \item{\code{Chlor_a_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{Chlor_a_Particulate_mg_m2}}{a numeric vector}
#'               \item{\code{Chlordane_alpha_ng_g}}{a numeric vector}
#'               \item{\code{Chlordane_cis_ng_g}}{a numeric vector}
#'               \item{\code{Chlordane_cis_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlordane_trans_ng_g}}{a numeric vector}
#'               \item{\code{Chlordane_trans_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlordene_alpha_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlordene_cis_ng_g}}{a numeric vector}
#'               \item{\code{Chlordene_cis_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlordene_gamma_ng_g}}{a numeric vector}
#'               \item{\code{Chlordene_gamma_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlordene_trans_ng_g}}{a numeric vector}
#'               \item{\code{Chlordene_trans_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlorfenvinphos_ppb}}{a logical vector}
#'               \item{\code{Chlorfenvinphos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlorobenzene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chloroform_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlorotoluene_2_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlorotoluene_4_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlorpyrifos_ug_L}}{a numeric vector}
#'               \item{\code{Chlorpyrifos_methyl_ng_g}}{a numeric vector}
#'               \item{\code{Chlorpyrifos_methyl_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chlorpyrifos_ng_g}}{a numeric vector}
#'               \item{\code{Chlorpyrifos_uf_ug_L}}{a numeric vector}
#'               \item{\code{ChlorpyrifosMethyl_ppb}}{a logical vector}
#'               \item{\code{Chlortetracycline_uf_ug_L}}{a numeric vector}
#'               \item{\code{ChlorthalDimethyl_ppb}}{a logical vector}
#'               \item{\code{Chrysene_ng_g}}{a numeric vector}
#'               \item{\code{Chrysene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chrysenes_C1_ng_g}}{a numeric vector}
#'               \item{\code{Chrysenes_C1_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chrysenes_C2_ng_g}}{a numeric vector}
#'               \item{\code{Chrysenes_C2_uf_ug_L}}{a numeric vector}
#'               \item{\code{Chrysenes_C3_ng_g}}{a numeric vector}
#'               \item{\code{Chrysenes_C3_uf_ug_L}}{a numeric vector}
#'               \item{\code{Cinerin1_ng_g}}{a logical vector}
#'               \item{\code{Cinerin2_ng_g}}{a logical vector}
#'               \item{\code{Ciodrin_ppb}}{a logical vector}
#'               \item{\code{Ciodrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Cl_f_mg_L}}{a numeric vector}
#'               \item{\code{COD_mg_L}}{a numeric vector}
#'               \item{\code{Coliform_Fecal_MPN_100mL}}{a numeric vector}
#'               \item{\code{Coliform_Total_MPN_100mL}}{a numeric vector}
#'               \item{\code{Color_True_CU}}{a numeric vector}
#'               \item{\code{Coumaphos_ppb}}{a logical vector}
#'               \item{\code{Coumaphos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Cr_ug_g}}{a numeric vector}
#'               \item{\code{Cr_f_mg_L}}{a numeric vector}
#'               \item{\code{Cr_uf_mg_L}}{a numeric vector}
#'               \item{\code{Cu_ug_g}}{a numeric vector}
#'               \item{\code{Cu_f_mg_L}}{a numeric vector}
#'               \item{\code{Cu_uf_mg_L}}{a numeric vector}
#'               \item{\code{Cyfluthrin_ng_g}}{a numeric vector}
#'               \item{\code{Cyfluthrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Cyhalothrin_lambda_ng_g}}{a numeric vector}
#'               \item{\code{Cyhalothrin_lambda_ppb}}{a numeric vector}
#'               \item{\code{Cyhalothrin_lambda_uf_ug_L}}{a numeric vector}
#'               \item{\code{Cypermethrin_ng_g}}{a numeric vector}
#'               \item{\code{Cypermethrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dacthal_ng_g}}{a numeric vector}
#'               \item{\code{Dacthal_uf_ug_L}}{a numeric vector}
#'               \item{\code{Danitol_ng_g}}{a numeric vector}
#'               \item{\code{Danitol_uf_ug_L}}{a numeric vector}
#'               \item{\code{DCBP.p.p.._ng_g}}{a numeric vector}
#'               \item{\code{DDD.o.p.._ng_g}}{a numeric vector}
#'               \item{\code{DDD.o.p.._uf_ug_L}}{a numeric vector}
#'               \item{\code{DDD.p.p.._ng_g}}{a numeric vector}
#'               \item{\code{DDD.p.p.._uf_ug_L}}{a numeric vector}
#'               \item{\code{DDE.o.p.._ng_g}}{a numeric vector}
#'               \item{\code{DDE.o.p.._uf_ug_L}}{a numeric vector}
#'               \item{\code{DDE.p.p.._ng_g}}{a numeric vector}
#'               \item{\code{DDE.p.p.._uf_ug_L}}{a numeric vector}
#'               \item{\code{DDMU.p.p.._ng_g}}{a numeric vector}
#'               \item{\code{DDMU.p.p.._uf_ug_L}}{a numeric vector}
#'               \item{\code{DDT.o.p.._ng_g}}{a numeric vector}
#'               \item{\code{DDT.o.p.._uf_ug_L}}{a numeric vector}
#'               \item{\code{DDT.p.p.._ng_g}}{a numeric vector}
#'               \item{\code{DDT.p.p.._uf_ug_L}}{a numeric vector}
#'               \item{\code{DDTs_Total_ng_g}}{a numeric vector}
#'               \item{\code{DDVP_ppb}}{a logical vector}
#'               \item{\code{Deltamethrin_ng_g}}{a numeric vector}
#'               \item{\code{Deltamethrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{DeltamethrinTralomethrin_ng_g}}{a numeric vector}
#'               \item{\code{DeltamethrinTralomethrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Demeton_o_uf_ug_L}}{a numeric vector}
#'               \item{\code{Demeton_s_ng_g}}{a numeric vector}
#'               \item{\code{Demeton_s_uf_ug_L}}{a numeric vector}
#'               \item{\code{Demeton_ug_L}}{a logical vector}
#'               \item{\code{DesmethylLR_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{DesmethylLR_uf_ug_L}}{a numeric vector}
#'               \item{\code{DesmethylRR_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{DesmethylRR_uf_ug_L}}{a numeric vector}
#'               \item{\code{Diazinon_ng_g}}{a numeric vector}
#'               \item{\code{Diazinon_uf_ug_L}}{a numeric vector}
#'               \item{\code{Diazinon_ug_L}}{a numeric vector}
#'               \item{\code{Dibenz.a.h.anthracene_ng_g}}{a numeric vector}
#'               \item{\code{Dibenz.a.h.anthracene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibenzothiophene_ng_g}}{a numeric vector}
#'               \item{\code{Dibenzothiophene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibenzothiophenes_C1_ng_g}}{a numeric vector}
#'               \item{\code{Dibenzothiophenes_C1_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibenzothiophenes_C2_ng_g}}{a numeric vector}
#'               \item{\code{Dibenzothiophenes_C2_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibenzothiophenes_C3_ng_g}}{a numeric vector}
#'               \item{\code{Dibenzothiophenes_C3_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibromo.3.Chloropropane_1.2..DBCP._uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibromochloromethane_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibromoethane_12_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibromomethane_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dibutyltin_Sn_ng_g}}{a numeric vector}
#'               \item{\code{DIC_f_mg_L}}{a numeric vector}
#'               \item{\code{Dichlofenthion_ng_g}}{a numeric vector}
#'               \item{\code{Dichlofenthion_ppb}}{a logical vector}
#'               \item{\code{Dichlofenthion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichlorobenzene_12_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichlorobenzene_13_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichlorobenzene_14_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloroethane_11_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloroethane_12_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloroethylene_11_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloroethylene_cis_12_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloroethylene_trans_.12_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloropropane_12_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloropropane_13_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloropropane_22_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichloropropene_11_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dichlorvos_ng_g}}{a numeric vector}
#'               \item{\code{Dichlorvos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dicrotophos_ppb}}{a logical vector}
#'               \item{\code{Dicrotophos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dieldrin_ng_g}}{a numeric vector}
#'               \item{\code{Dieldrin_ppb}}{a logical vector}
#'               \item{\code{Dieldrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dimethoate_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dimethylnaphthalene_2.6_ng_g}}{a numeric vector}
#'               \item{\code{Dimethylnaphthalene_26_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dimethylphenanthrene_3.6_ng_g}}{a numeric vector}
#'               \item{\code{Dimethylphenanthrene_36_uf_ug_L}}{a numeric vector}
#'               \item{\code{Dioxathion._ng_g}}{a numeric vector}
#'               \item{\code{Dioxathion._uf_ug_L}}{a numeric vector}
#'               \item{\code{Dioxathion_ppb}}{a numeric vector}
#'               \item{\code{Disulfoton_uf_ug_L}}{a numeric vector}
#'               \item{\code{DO_uf_mg_L}}{a numeric vector}
#'               \item{\code{DOC_f_mg_L}}{a numeric vector}
#'               \item{\code{DomoicAcid_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{DomoicAcid_uf_ug_L}}{a numeric vector}
#'               \item{\code{Doxycycline_uf_ug_L}}{a numeric vector}
#'               \item{\code{DP_f_mg_L}}{a numeric vector}
#'               \item{\code{DP_P_mg_L}}{a numeric vector}
#'               \item{\code{E_coli_MPN_100_mL}}{a numeric vector}
#'               \item{\code{Endosulfan_I_ng_g}}{a numeric vector}
#'               \item{\code{Endosulfan_I_uf_ug_L}}{a numeric vector}
#'               \item{\code{Endosulfan_II_ng_g}}{a numeric vector}
#'               \item{\code{Endosulfan_II_ppb}}{a logical vector}
#'               \item{\code{Endosulfan_II_uf_ug_L}}{a numeric vector}
#'               \item{\code{Endosulfan_ppb}}{a logical vector}
#'               \item{\code{Endosulfan_sulfate_ng_g}}{a numeric vector}
#'               \item{\code{Endosulfan_Sulfate_ppb}}{a logical vector}
#'               \item{\code{Endosulfan_sulfate_uf_ug_L}}{a numeric vector}
#'               \item{\code{Endrin_Aldehyde_ng_g}}{a numeric vector}
#'               \item{\code{Endrin_Aldehyde_ppb}}{a numeric vector}
#'               \item{\code{Endrin_Aldehyde_uf_ug_L}}{a numeric vector}
#'               \item{\code{Endrin_Ketone_ng_g}}{a numeric vector}
#'               \item{\code{Endrin_Ketone_ppb}}{a logical vector}
#'               \item{\code{Endrin_Ketone_uf_ug_L}}{a numeric vector}
#'               \item{\code{Endrin_ppb}}{a logical vector}
#'               \item{\code{Endrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Endrin_uf_ng_g}}{a numeric vector}
#'               \item{\code{Enterococci_MPN_100mL}}{a numeric vector}
#'               \item{\code{Enterococcus_MPN_100_mL}}{a numeric vector}
#'               \item{\code{Erythromycin_H2O_uf_ug_L}}{a numeric vector}
#'               \item{\code{Esfenvalerate_ng_g}}{a numeric vector}
#'               \item{\code{Esfenvalerate_uf_ug_L}}{a numeric vector}
#'               \item{\code{EsfenvalerateFenvalerate_ng_g}}{a numeric vector}
#'               \item{\code{EsfenvalerateFenvalerate_uf_ug_L}}{a numeric vector}
#'               \item{\code{EsfenvalerateFenvalerate1_uf_ug_L}}{a logical vector}
#'               \item{\code{EsfenvalerateFenvalerate2_uf_ug_L}}{a logical vector}
#'               \item{\code{Estradiol_17beta_uf_ug_L}}{a numeric vector}
#'               \item{\code{Ethion_ng_g}}{a numeric vector}
#'               \item{\code{Ethion_ppb}}{a logical vector}
#'               \item{\code{Ethion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Ethoprop_ng_g}}{a numeric vector}
#'               \item{\code{Ethoprop_ppb}}{a logical vector}
#'               \item{\code{Ethoprop_uf_ug_L}}{a numeric vector}
#'               \item{\code{Ethylbenzene_uf_ug_L}}{a numeric vector}
#'               \item{\code{F_uf_mg_L}}{a numeric vector}
#'               \item{\code{Famphur_ppb}}{a logical vector}
#'               \item{\code{Famphur_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fe_ug_g}}{a numeric vector}
#'               \item{\code{Fe_f_ug_L}}{a numeric vector}
#'               \item{\code{Fe_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fenchlorphos_ng_g}}{a numeric vector}
#'               \item{\code{Fenchlorphos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fenitrothion_ng_g}}{a numeric vector}
#'               \item{\code{Fenitrothion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fenpropathrin_ppb}}{a logical vector}
#'               \item{\code{Fenpropathrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fensulfothion_ng_g}}{a numeric vector}
#'               \item{\code{Fensulfothion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fenthion_ng_g}}{a numeric vector}
#'               \item{\code{Fenthion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fenvalerate_ng_g}}{a numeric vector}
#'               \item{\code{Fenvalerate_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fipronil_ng_g}}{a numeric vector}
#'               \item{\code{FipronilDesulfinyl_ng_g}}{a numeric vector}
#'               \item{\code{FipronilSulfide_ng_g}}{a numeric vector}
#'               \item{\code{FipronilSulfone_ng_g}}{a numeric vector}
#'               \item{\code{Fluoranthene_ng_g}}{a numeric vector}
#'               \item{\code{Fluoranthene_uf_ug_L}}{a numeric vector}
#'               \item{\code{FluoranthenePyrenes_C1_ng_g}}{a numeric vector}
#'               \item{\code{FluoranthenePyrenes_C1_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fluorene_ng_g}}{a numeric vector}
#'               \item{\code{Fluorene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fluorenes_C1_ng_g}}{a numeric vector}
#'               \item{\code{Fluorenes_C1_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fluorenes_C2_ng_g}}{a numeric vector}
#'               \item{\code{Fluorenes_C2_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fluorenes_C3_ng_g}}{a numeric vector}
#'               \item{\code{Fluorenes_C3_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fluoxetine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Fluvalinate_ng_g}}{a numeric vector}
#'               \item{\code{Fluvalinate_uf_ng_L}}{a numeric vector}
#'               \item{\code{Fonofos_ng_g}}{a numeric vector}
#'               \item{\code{Fonofos_ppb}}{a logical vector}
#'               \item{\code{Fonofos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Gemfibrozil_uf_ug_L}}{a numeric vector}
#'               \item{\code{Hardness_CaCO3_f_mg_L}}{a numeric vector}
#'               \item{\code{Hardness_CaCO3_uf_mg_L}}{a numeric vector}
#'               \item{\code{HCH_alpha._ng_g}}{a numeric vector}
#'               \item{\code{HCH_alpha._uf_ug_L}}{a numeric vector}
#'               \item{\code{HCH_beta_ng_g}}{a numeric vector}
#'               \item{\code{HCH_beta_uf_ug_L}}{a numeric vector}
#'               \item{\code{HCH_delta_ng_g}}{a numeric vector}
#'               \item{\code{HCH_delta_uf_ug_L}}{a numeric vector}
#'               \item{\code{HCH_gamma_ng_g}}{a numeric vector}
#'               \item{\code{HCH_gamma_uf_ug_L}}{a numeric vector}
#'               \item{\code{Heptachlor_epoxide_ng_g}}{a numeric vector}
#'               \item{\code{Heptachlor_epoxide_uf_ug_L}}{a numeric vector}
#'               \item{\code{Heptachlor_ng_g}}{a numeric vector}
#'               \item{\code{Heptachlor_ppb}}{a logical vector}
#'               \item{\code{Heptachlor_uf_ug_L}}{a numeric vector}
#'               \item{\code{HeptachlorEpoxide_ppb}}{a logical vector}
#'               \item{\code{Hexachlorobenzene_ng_g}}{a numeric vector}
#'               \item{\code{Hexachlorobenzene_ppb}}{a logical vector}
#'               \item{\code{Hexachlorobenzene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Hexachlorobutadiene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Hg_ug_g}}{a numeric vector}
#'               \item{\code{Hg_f_ng_L}}{a numeric vector}
#'               \item{\code{Hg_uf_mg_kg_FishTissue}}{a numeric vector}
#'               \item{\code{Hg_uf_ng_L}}{a numeric vector}
#'               \item{\code{HydroxideAlk_CaCO3_mg_L}}{a numeric vector}
#'               \item{\code{Ibuprofen_uf_ug_L}}{a numeric vector}
#'               \item{\code{Indeno.1.2.3.c.d.pyrene_ng_g}}{a numeric vector}
#'               \item{\code{Indeno.123cd.pyrene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Isopropylbenzene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Isopropyltoluene_p_uf_ug_L}}{a numeric vector}
#'               \item{\code{Jasmoline1_ng_g}}{a logical vector}
#'               \item{\code{Jasmoline2_ng_g}}{a logical vector}
#'               \item{\code{K_f_mg_L}}{a numeric vector}
#'               \item{\code{K_uf_mg_L}}{a numeric vector}
#'               \item{\code{Leptophos_ppb}}{a logical vector}
#'               \item{\code{Leptophos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Lincomycin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Malathion_ng_g}}{a numeric vector}
#'               \item{\code{Malathion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Malathion_ug_L}}{a numeric vector}
#'               \item{\code{MBAS_mg_L}}{a numeric vector}
#'               \item{\code{MBAS_uf_mg_L}}{a numeric vector}
#'               \item{\code{MeanAlkalinity}}{a numeric vector}
#'               \item{\code{Merphos_ng_g}}{a numeric vector}
#'               \item{\code{Merphos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Methidathion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Methoxychlor_ng_g}}{a numeric vector}
#'               \item{\code{Methoxychlor_ppb}}{a logical vector}
#'               \item{\code{Methoxychlor_uf_ug_L}}{a numeric vector}
#'               \item{\code{Methyldibenzothiophene_4_ng_g}}{a numeric vector}
#'               \item{\code{Methyldibenzothiophene_4_uf_ug_L}}{a numeric vector}
#'               \item{\code{Methylfluoranthene_2_ng_g}}{a numeric vector}
#'               \item{\code{Methylfluoranthene_2_uf_ug_L}}{a numeric vector}
#'               \item{\code{Methylfluorene_1_ng_g}}{a numeric vector}
#'               \item{\code{Methylfluorene_1_uf_ug_L}}{a numeric vector}
#'               \item{\code{Methylnaphthalene_1_ng_g}}{a numeric vector}
#'               \item{\code{Methylnaphthalene_1_uf_ug_L}}{a numeric vector}
#'               \item{\code{Methylnaphthalene_2_ng_g}}{a numeric vector}
#'               \item{\code{Methylnaphthalene_2_uf_ug_L}}{a numeric vector}
#'               \item{\code{Methylphenanthrene_1_ng_g}}{a numeric vector}
#'               \item{\code{Methylphenanthrene_1_uf_ug_L}}{a numeric vector}
#'               \item{\code{Mevinphos_ng_g}}{a numeric vector}
#'               \item{\code{Mevinphos_ppb}}{a numeric vector}
#'               \item{\code{Mevinphos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Mg_f_mg_L}}{a numeric vector}
#'               \item{\code{Mg_uf_mg_L}}{a numeric vector}
#'               \item{\code{MicrocystinLA_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLA_uf_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLF_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLF_uf_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLR_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLR_uf_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLW_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLW_uf_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLY_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinLY_uf_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinRR_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinRR_uf_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinYR_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{MicrocystinYR_uf_ug_L}}{a numeric vector}
#'               \item{\code{Mirex_ng_g}}{a numeric vector}
#'               \item{\code{Mirex_ppb}}{a logical vector}
#'               \item{\code{Mirex_uf_ug_L}}{a numeric vector}
#'               \item{\code{Mn_ug_g}}{a numeric vector}
#'               \item{\code{Mn_f_ug_L}}{a numeric vector}
#'               \item{\code{Mn_uf_ug_L}}{a numeric vector}
#'               \item{\code{Molinate_ppb}}{a logical vector}
#'               \item{\code{Molinate_uf_ug_L}}{a numeric vector}
#'               \item{\code{Monobutyltin_Sn_ng_g}}{a numeric vector}
#'               \item{\code{MTBE_uf_ug_L}}{a numeric vector}
#'               \item{\code{Na_f_mg_L}}{a numeric vector}
#'               \item{\code{Na_uf_mg_L}}{a numeric vector}
#'               \item{\code{Naled_ppb}}{a logical vector}
#'               \item{\code{Naled_uf_ug_L}}{a numeric vector}
#'               \item{\code{Naphthalene_ng_g}}{a numeric vector}
#'               \item{\code{Naphthalene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Naphthalenes_C1_ng_g}}{a numeric vector}
#'               \item{\code{Naphthalenes_C1_uf_ug_L}}{a numeric vector}
#'               \item{\code{Naphthalenes_C2_ng_g}}{a numeric vector}
#'               \item{\code{Naphthalenes_C2_uf_ug_L}}{a numeric vector}
#'               \item{\code{Naphthalenes_C3_ng_g}}{a numeric vector}
#'               \item{\code{Naphthalenes_C3_uf_ug_L}}{a numeric vector}
#'               \item{\code{Naphthalenes_C4_ng_g}}{a numeric vector}
#'               \item{\code{Naphthalenes_C4_uf_ug_L}}{a numeric vector}
#'               \item{\code{NH3_N_f_mg_L}}{a numeric vector}
#'               \item{\code{NH3_N_mg_kg_ww}}{a numeric vector}
#'               \item{\code{NH3_N_uf_mg_L}}{a numeric vector}
#'               \item{\code{Ni_ug_g}}{a numeric vector}
#'               \item{\code{Ni_f_mg_L}}{a numeric vector}
#'               \item{\code{Ni_uf_mg_L}}{a numeric vector}
#'               \item{\code{NO2_N_f_mg_L}}{a numeric vector}
#'               \item{\code{NO2_N_uf_mg_L}}{a numeric vector}
#'               \item{\code{NO2NO3_N_f_mg_L}}{a numeric vector}
#'               \item{\code{NO2NO3_N_uf_mg_L}}{a numeric vector}
#'               \item{\code{NO3_N_f_mg_L}}{a numeric vector}
#'               \item{\code{NO3_N_uf_mg_L}}{a numeric vector}
#'               \item{\code{Nodularin_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{Nodularin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Nonachlor_cis_ng_g}}{a numeric vector}
#'               \item{\code{Nonachlor_cis_uf_ug_L}}{a numeric vector}
#'               \item{\code{Nonachlor_trans_ng_g}}{a numeric vector}
#'               \item{\code{Nonachlor_trans_uf_ug_L}}{a numeric vector}
#'               \item{\code{Nonylphenol_uf_ug_L}}{a numeric vector}
#'               \item{\code{Nonylphenolethoxylate_uf_ug_L}}{a numeric vector}
#'               \item{\code{O2Sat_uf_.}}{a numeric vector}
#'               \item{\code{Oil_Grease_mg_L}}{a numeric vector}
#'               \item{\code{OkadaicAcid_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{OkadaicAcid_uf_ug_L}}{a numeric vector}
#'               \item{\code{oPO4_P_f_mg_L}}{a numeric vector}
#'               \item{\code{oPO4_P_uf_mg_L}}{a numeric vector}
#'               \item{\code{Oxadiazon_ng_g}}{a numeric vector}
#'               \item{\code{Oxadiazon_ppb}}{a numeric vector}
#'               \item{\code{Oxadiazon_uf_ug_L}}{a numeric vector}
#'               \item{\code{Oxychlordane_ng_g}}{a numeric vector}
#'               \item{\code{Oxychlordane_uf_ug_L}}{a numeric vector}
#'               \item{\code{Oxytetracycline_uf_ug_L}}{a numeric vector}
#'               \item{\code{PAHs_ng_g}}{a numeric vector}
#'               \item{\code{Parathion_Ethyl_ng_g}}{a numeric vector}
#'               \item{\code{Parathion_Ethyl_uf_ug_L}}{a numeric vector}
#'               \item{\code{Parathion_Methyl_ng_g}}{a numeric vector}
#'               \item{\code{Parathion_Methyl_uf_ug_L}}{a numeric vector}
#'               \item{\code{Pb_ug_g}}{a numeric vector}
#'               \item{\code{Pb_f_mg_L}}{a numeric vector}
#'               \item{\code{Pb_uf_mg_L}}{a numeric vector}
#'               \item{\code{PBDE017_ng_g}}{a numeric vector}
#'               \item{\code{PBDE028_ng_g}}{a numeric vector}
#'               \item{\code{PBDE047_ng_g}}{a numeric vector}
#'               \item{\code{PBDE066_ng_g}}{a numeric vector}
#'               \item{\code{PBDE085_ng_g}}{a numeric vector}
#'               \item{\code{PBDE099_ng_g}}{a numeric vector}
#'               \item{\code{PBDE100_ng_g}}{a numeric vector}
#'               \item{\code{PBDE138_ng_g}}{a numeric vector}
#'               \item{\code{PBDE153_ng_g}}{a numeric vector}
#'               \item{\code{PBDE154_ng_g}}{a numeric vector}
#'               \item{\code{PBDE183_ng_g}}{a numeric vector}
#'               \item{\code{PBDE190_ng_g}}{a numeric vector}
#'               \item{\code{PBDE209_ng_g}}{a numeric vector}
#'               \item{\code{PCB_AROCLOR_1248_ng_g}}{a numeric vector}
#'               \item{\code{PCB_AROCLOR_1254_ng_g}}{a numeric vector}
#'               \item{\code{PCB_AROCLOR_1260_ng_g}}{a numeric vector}
#'               \item{\code{PCB003_ng_g}}{a numeric vector}
#'               \item{\code{PCB005_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB008_ng_g}}{a numeric vector}
#'               \item{\code{PCB008_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB015_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB018_ng_g}}{a numeric vector}
#'               \item{\code{PCB018_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB027_ng_g}}{a numeric vector}
#'               \item{\code{PCB027_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB028_ng_g}}{a numeric vector}
#'               \item{\code{PCB028_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB029_ng_g}}{a numeric vector}
#'               \item{\code{PCB029_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB031_ng_g}}{a numeric vector}
#'               \item{\code{PCB031_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB033_ng_g}}{a numeric vector}
#'               \item{\code{PCB033_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB037_ng_g}}{a numeric vector}
#'               \item{\code{PCB044_ng_g}}{a numeric vector}
#'               \item{\code{PCB044_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB049_ng_g}}{a numeric vector}
#'               \item{\code{PCB049_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB052_ng_g}}{a numeric vector}
#'               \item{\code{PCB052_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB056_ng_g}}{a numeric vector}
#'               \item{\code{PCB056_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB060_ng_g}}{a numeric vector}
#'               \item{\code{PCB060_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB064_ng_g}}{a numeric vector}
#'               \item{\code{PCB066_ng_g}}{a numeric vector}
#'               \item{\code{PCB066_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB070_ng_g}}{a numeric vector}
#'               \item{\code{PCB070_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB074_ng_g}}{a numeric vector}
#'               \item{\code{PCB074_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB077_ng_g}}{a numeric vector}
#'               \item{\code{PCB077_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB081_ng_g}}{a numeric vector}
#'               \item{\code{PCB087_ng_g}}{a numeric vector}
#'               \item{\code{PCB087_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB095_ng_g}}{a numeric vector}
#'               \item{\code{PCB095_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB097_ng_g}}{a numeric vector}
#'               \item{\code{PCB097_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB099_ng_g}}{a numeric vector}
#'               \item{\code{PCB099_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB101_ng_g}}{a numeric vector}
#'               \item{\code{PCB101_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB105_ng_g}}{a numeric vector}
#'               \item{\code{PCB105_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB110_ng_g}}{a numeric vector}
#'               \item{\code{PCB110_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB114_ng_g}}{a numeric vector}
#'               \item{\code{PCB114_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB118_ng_g}}{a numeric vector}
#'               \item{\code{PCB118_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB119_ng_g}}{a numeric vector}
#'               \item{\code{PCB123_ng_g}}{a numeric vector}
#'               \item{\code{PCB126_ng_g}}{a numeric vector}
#'               \item{\code{PCB126_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB128_ng_g}}{a numeric vector}
#'               \item{\code{PCB128_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB137_ng_g}}{a numeric vector}
#'               \item{\code{PCB137_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB138_ng_g}}{a numeric vector}
#'               \item{\code{PCB138_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB141_ng_g}}{a numeric vector}
#'               \item{\code{PCB141_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB146_ng_g}}{a numeric vector}
#'               \item{\code{PCB149_ng_g}}{a numeric vector}
#'               \item{\code{PCB149_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB151_ng_g}}{a numeric vector}
#'               \item{\code{PCB151_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB153_ng_g}}{a numeric vector}
#'               \item{\code{PCB153_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB156_ng_g}}{a numeric vector}
#'               \item{\code{PCB156_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB157_ng_g}}{a numeric vector}
#'               \item{\code{PCB157_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB158_ng_g}}{a numeric vector}
#'               \item{\code{PCB158_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB167_ng_g}}{a numeric vector}
#'               \item{\code{PCB168_132_ng_g}}{a numeric vector}
#'               \item{\code{PCB168_ng_g}}{a numeric vector}
#'               \item{\code{PCB169_ng_g}}{a numeric vector}
#'               \item{\code{PCB170_ng_g}}{a numeric vector}
#'               \item{\code{PCB170_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB174_ng_g}}{a numeric vector}
#'               \item{\code{PCB174_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB177_ng_g}}{a numeric vector}
#'               \item{\code{PCB177_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB180_ng_g}}{a numeric vector}
#'               \item{\code{PCB180_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB183_ng_g}}{a numeric vector}
#'               \item{\code{PCB183_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB187_ng_g}}{a numeric vector}
#'               \item{\code{PCB187_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB189_ng_g}}{a numeric vector}
#'               \item{\code{PCB189_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB194_ng_g}}{a numeric vector}
#'               \item{\code{PCB194_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB195_ng_g}}{a numeric vector}
#'               \item{\code{PCB195_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB198_199_ng_g}}{a numeric vector}
#'               \item{\code{PCB200_ng_g}}{a numeric vector}
#'               \item{\code{PCB200_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB201_ng_g}}{a numeric vector}
#'               \item{\code{PCB201_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB203_ng_g}}{a numeric vector}
#'               \item{\code{PCB203_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB206_ng_g}}{a numeric vector}
#'               \item{\code{PCB206_uf_ug_L}}{a numeric vector}
#'               \item{\code{PCB209_ng_g}}{a numeric vector}
#'               \item{\code{PCB209_uf_ug_L}}{a numeric vector}
#'               \item{\code{Permethrin_cis_ng_g}}{a numeric vector}
#'               \item{\code{Permethrin_cis_uf_ug_L}}{a numeric vector}
#'               \item{\code{Permethrin_ng_g}}{a numeric vector}
#'               \item{\code{Permethrin_trans_ng_g}}{a numeric vector}
#'               \item{\code{Permethrin_trans_uf_ug_L}}{a numeric vector}
#'               \item{\code{Permethrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Perthane_ng_g}}{a numeric vector}
#'               \item{\code{Perylene_ng_g}}{a numeric vector}
#'               \item{\code{Perylene_uf_ug_L}}{a numeric vector}
#'               \item{\code{pH}}{a numeric vector}
#'               \item{\code{Phenanthrene_ng_g}}{a numeric vector}
#'               \item{\code{Phenanthrene_uf_ug_L}}{a numeric vector}
#'               \item{\code{PhenanthreneAnthracene_C1_ng_g}}{a numeric vector}
#'               \item{\code{PhenanthreneAnthracene_C1_uf_ug_L}}{a numeric vector}
#'               \item{\code{PhenanthreneAnthracene_C2_ng_g}}{a numeric vector}
#'               \item{\code{PhenanthreneAnthracene_C2_uf_ug_L}}{a numeric vector}
#'               \item{\code{PhenanthreneAnthracene_C3_ng_g}}{a numeric vector}
#'               \item{\code{PhenanthreneAnthracene_C3_uf_ug_L}}{a numeric vector}
#'               \item{\code{PhenanthreneAnthracene_C4_ng_g}}{a numeric vector}
#'               \item{\code{PhenanthreneAnthracene_C4_uf_ug_L}}{a numeric vector}
#'               \item{\code{Pheo_a_Particulate_ug_L}}{a numeric vector}
#'               \item{\code{Phorate_ng_g}}{a numeric vector}
#'               \item{\code{Phorate_uf_ug_L}}{a numeric vector}
#'               \item{\code{Phosmet_uf_ug_L}}{a numeric vector}
#'               \item{\code{Phosphamidon_ng_g}}{a numeric vector}
#'               \item{\code{Phosphamidon_ppb}}{a logical vector}
#'               \item{\code{Phosphamidon_uf_ug_L}}{a numeric vector}
#'               \item{\code{PiperonylButoxide_ng_g}}{a numeric vector}
#'               \item{\code{PO4_P_mg_kg}}{a numeric vector}
#'               \item{\code{PO4_P_uf_mg_L}}{a numeric vector}
#'               \item{\code{Prallethrin_ng_g}}{a numeric vector}
#'               \item{\code{Prallethrin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Prometon_ppb}}{a numeric vector}
#'               \item{\code{Prometon_uf_ug_L}}{a numeric vector}
#'               \item{\code{Prometryn_ppb}}{a logical vector}
#'               \item{\code{Prometryn_uf_ug_L}}{a numeric vector}
#'               \item{\code{Propazine_ppb}}{a numeric vector}
#'               \item{\code{Propazine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Propylbenzene_n_uf_ug_L}}{a numeric vector}
#'               \item{\code{Prothiofos_ppb}}{a logical vector}
#'               \item{\code{Pyrene_ng_g}}{a numeric vector}
#'               \item{\code{Pyrene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Pyrethrin1_ng_g}}{a logical vector}
#'               \item{\code{Pyrethrin2_ng_g}}{a logical vector}
#'               \item{\code{Resmethrin_ng_g}}{a numeric vector}
#'               \item{\code{Ronnel_ppb}}{a logical vector}
#'               \item{\code{Roxithromycin_uf_ug_L}}{a numeric vector}
#'               \item{\code{S_mg_kg}}{a numeric vector}
#'               \item{\code{Salinity_uf_ppt}}{a numeric vector}
#'               \item{\code{Sb_ug_g}}{a numeric vector}
#'               \item{\code{Sb_f_mg_L}}{a numeric vector}
#'               \item{\code{Sb_uf_mg_L}}{a numeric vector}
#'               \item{\code{Se_ug_g}}{a numeric vector}
#'               \item{\code{Se_f_mg_L}}{a numeric vector}
#'               \item{\code{Se_uf_mg_L}}{a numeric vector}
#'               \item{\code{Secbumeton_ppb}}{a logical vector}
#'               \item{\code{Secbumeton_uf_ug_L}}{a numeric vector}
#'               \item{\code{Si_SiO2_f_mg_L}}{a numeric vector}
#'               \item{\code{Si_SiO2_uf_mg_L}}{a numeric vector}
#'               \item{\code{Simazine_ppb}}{a numeric vector}
#'               \item{\code{Simazine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Simetryn_ppb}}{a logical vector}
#'               \item{\code{Simetryn_uf_ug_L}}{a numeric vector}
#'               \item{\code{SO4_f_mg_L}}{a numeric vector}
#'               \item{\code{SO4_uf_mg_L}}{a numeric vector}
#'               \item{\code{Solids_.}}{a numeric vector}
#'               \item{\code{Solids_mg_L}}{a numeric vector}
#'               \item{\code{SpecCond_uf_uS_cm}}{a numeric vector}
#'               \item{\code{Sulfachloropyridazine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Sulfadimethoxine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Sulfamerazine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Sulfamethazine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Sulfamethizole_uf_ug_L}}{a numeric vector}
#'               \item{\code{Sulfamethoxazole_uf_ug_L}}{a numeric vector}
#'               \item{\code{Sulfathiazole_uf_ug_L}}{a numeric vector}
#'               \item{\code{Sulfotep_ng_g}}{a numeric vector}
#'               \item{\code{Sulfotep_ppb}}{a logical vector}
#'               \item{\code{Sulfotep_uf_ug_L}}{a numeric vector}
#'               \item{\code{Sulprofos_ppb}}{a logical vector}
#'               \item{\code{SuspSedConc_Particulate_mg_L}}{a numeric vector}
#'               \item{\code{TDS_calc_mg_L}}{a numeric vector}
#'               \item{\code{TDS_f_mg_L}}{a numeric vector}
#'               \item{\code{Tedion_ng_g}}{a numeric vector}
#'               \item{\code{Tedion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Temp_degC}}{a numeric vector}
#'               \item{\code{Temp_degF}}{a numeric vector}
#'               \item{\code{Terbufos_ppb}}{a logical vector}
#'               \item{\code{Terbufos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Terbuthylazine_ppb}}{a numeric vector}
#'               \item{\code{Terbuthylazine_uf_ug_L}}{a numeric vector}
#'               \item{\code{Terbutryn_ppb}}{a logical vector}
#'               \item{\code{Terbutryn_uf_ug_L}}{a numeric vector}
#'               \item{\code{Terphenyl_d14_Surrogate_uf_ug_L}}{a numeric vector}
#'               \item{\code{Tetrachloroethane_1112_uf_ug_L}}{a numeric vector}
#'               \item{\code{Tetrachloroethane_1122_uf_ug_L}}{a numeric vector}
#'               \item{\code{Tetrachloroethylene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Tetrachlorvinphos_ng_g}}{a numeric vector}
#'               \item{\code{Tetrachlorvinphos_ppb}}{a logical vector}
#'               \item{\code{Tetrachlorvinphos_uf_ug_L}}{a numeric vector}
#'               \item{\code{Tetracycline_uf_ug_L}}{a numeric vector}
#'               \item{\code{Tetradifon_ppb}}{a logical vector}
#'               \item{\code{Thiobencarb_ppb}}{a numeric vector}
#'               \item{\code{Thiobencarb_uf_ug_L}}{a numeric vector}
#'               \item{\code{Thionazin_ng_g}}{a numeric vector}
#'               \item{\code{Thionazin_ppb}}{a logical vector}
#'               \item{\code{Thionazin_uf_ug_L}}{a numeric vector}
#'               \item{\code{TKN_uf_mg_L}}{a numeric vector}
#'               \item{\code{TN_mg_kg}}{a numeric vector}
#'               \item{\code{TN_uf_mg_L}}{a numeric vector}
#'               \item{\code{TOC_.}}{a numeric vector}
#'               \item{\code{TOC_uf_mg_L}}{a numeric vector}
#'               \item{\code{Tokuthion_ng_g}}{a numeric vector}
#'               \item{\code{Tokuthion_uf_ug_L}}{a numeric vector}
#'               \item{\code{Toluene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Toxaphene_ng_g}}{a numeric vector}
#'               \item{\code{TP_mg_L}}{a numeric vector}
#'               \item{\code{TP_P_uf_mg_L}}{a numeric vector}
#'               \item{\code{Tributyltin_Sn_ng_g}}{a numeric vector}
#'               \item{\code{Trichlorfon_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trichlorobenzene_123_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trichlorobenzene_124_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trichloroethane_111_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trichloroethane_112_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trichloroethylene_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trichloronate_ng_g}}{a numeric vector}
#'               \item{\code{Trichloronate_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trichlorophon_ppb}}{a logical vector}
#'               \item{\code{Trichloropropane_123_uf_ug_L}}{a numeric vector}
#'               \item{\code{Triclosan_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trimethoprim_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trimethylbenzene_124_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trimethylbenzene_135_uf_ug_L}}{a numeric vector}
#'               \item{\code{Trimethylnaphthalene_2.3.5_ng_g}}{a numeric vector}
#'               \item{\code{Trimethylnaphthalene_235_uf_ug_L}}{a numeric vector}
#'               \item{\code{TSS_uf_mg_L}}{a numeric vector}
#'               \item{\code{Turb_NTU}}{a numeric vector}
#'               \item{\code{Tylosin_uf_ug_L}}{a numeric vector}
#'               \item{\code{Xylene_mp_uf_ug_L}}{a numeric vector}
#'               \item{\code{Xylene_o_uf_ug_L}}{a numeric vector}
#'               \item{\code{Zn_ug_g}}{a numeric vector}
#'               \item{\code{Zn_f_mg_L}}{a numeric vector}
#'               \item{\code{Zn_uf_mg_L}}{a numeric vector}
#' }
#' @source example data
"data_CoOccur_CA"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_CoOccur_AZ_Hi ####
#' @title Co-Occurrence example data (AZ hi)
#' 
#' @description A dataset from California with example biological, chemical, habitat, and geo-physical parameters.
#' 
#' @format A data frame with 336 rows and 307 variables:
#' \describe{
#'           \item{\code{StationID_Master}}{}
#'           \item{\code{Group}}{a numeric vector}
#'           \item{\code{SampYear}}{a numeric vector}
#'           \item{\code{ChemSampleID}}{}
#'           \item{\code{SampDate}}{}
#'           \item{\code{BenCollDate}}{}
#'           \item{\code{IBI}}{a numeric vector}
#'           \item{\code{X124Trichlorobenzene_uf_ug_L}}{a logical vector}
#'           \item{\code{X12Dichlorobenzene_uf_ug_L}}{a logical vector}
#'           \item{\code{X13Dichlorobenzene_uf_ug_L}}{a logical vector}
#'           \item{\code{X14Dichlorobenzene_uf_ug_L}}{a logical vector}
#'           \item{\code{X245Trichlorophenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X246Trichlorophenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X24Dichlorophenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X24Dimethylphenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X24Dinitrophenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X24Dinitrotoluene_uf_ug_L}}{a logical vector}
#'           \item{\code{X26Dinitrotoluene_uf_ug_L}}{a logical vector}
#'           \item{\code{X2Chlorophenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X2Methylnaphthalene_uf_ug_L}}{a logical vector}
#'           \item{\code{X2Methylphenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X2Nitroaniline_uf_ug_L}}{a logical vector}
#'           \item{\code{X2Nitrophenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X33primeDichlorobenzidine_uf_ug_L}}{a logical vector}
#'           \item{\code{X3Nitroaniline_uf_ug_L}}{a logical vector}
#'           \item{\code{X4Chlorophenylphenylether_uf_ug_L}}{a logical vector}
#'           \item{\code{X4Methylphenol_uf_ug_L}}{a logical vector}
#'           \item{\code{X4Nitroaniline_uf_ug_L}}{a logical vector}
#'           \item{\code{Acenaphthene_uf_ug_L}}{a logical vector}
#'           \item{\code{Acenaphthylene_uf_ug_L}}{a logical vector}
#'           \item{\code{Aldrin_uf_ug_L}}{a logical vector}
#'           \item{\code{Alkalinity_phenolphthalein_uf_mg_L}}{a numeric vector}
#'           \item{\code{Alpha_grossAsUNatural_calc_pCi_L}}{a logical vector}
#'           \item{\code{Alpha_grossAsUNatural_uf_pCi_L}}{a logical vector}
#'           \item{\code{alphaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
#'           \item{\code{Aluminum_f_mg_L}}{a logical vector}
#'           \item{\code{Aluminum_uf_ug_L}}{a numeric vector}
#'           \item{\code{Aniline_uf_ug_L}}{a logical vector}
#'           \item{\code{Anthracene_uf_ug_L}}{a logical vector}
#'           \item{\code{Antimony_f_mg_L}}{a numeric vector}
#'           \item{\code{Antimony_uf_mg_L}}{a numeric vector}
#'           \item{\code{Arsenic_inorg_calc_mg_kg}}{a logical vector}
#'           \item{\code{Arsenic_inorg_f_ug_L}}{a numeric vector}
#'           \item{\code{Arsenic_inorg_susp_ug_L}}{a logical vector}
#'           \item{\code{Arsenic_inorg_uf_ug_L}}{a numeric vector}
#'           \item{\code{Azobenzene_uf_ug_L}}{a logical vector}
#'           \item{\code{Barium_cmpds_calc_mg_kg}}{a logical vector}
#'           \item{\code{Barium_cmpds_f_ug_L}}{a numeric vector}
#'           \item{\code{Barium_cmpds_uf_pCi_L}}{a logical vector}
#'           \item{\code{Barium_cmpds_uf_ug_L}}{a numeric vector}
#'           \item{\code{BarometricPressure_mmHg}}{a numeric vector}
#'           \item{\code{Benzo_a_antracene_uf_ug_L}}{a logical vector}
#'           \item{\code{Benzo_a_pyrene_uf_ug_L}}{a logical vector}
#'           \item{\code{Benzo_b_flouranthene_uf_ug_L}}{a logical vector}
#'           \item{\code{Benzo_ghi_perylene_uf_ug_L}}{a logical vector}
#'           \item{\code{Benzo_k_fluoranthene_uf_ug_L}}{a logical vector}
#'           \item{\code{BenzoicAcid_uf_ug_L}}{a logical vector}
#'           \item{\code{BenzylAlcohol_uf_ug_L}}{a logical vector}
#'           \item{\code{Beryllium_cmpds_calc_mg_kg}}{a logical vector}
#'           \item{\code{Beryllium_cmpds_f_mg_L}}{a logical vector}
#'           \item{\code{Beryllium_cmpds_uf_mg_L}}{a numeric vector}
#'           \item{\code{Beta_radiation_calc_pCi_L}}{a logical vector}
#'           \item{\code{Beta_radiation_uf_pCi_L}}{a logical vector}
#'           \item{\code{betaChloronapthalene_uf_ug_L}}{a logical vector}
#'           \item{\code{betaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
#'           \item{\code{Bis_2chloroethoxy_methane_uf_ug_L}}{a logical vector}
#'           \item{\code{Bis_2chloroisopropyl_ether_uf_ug_L}}{a logical vector}
#'           \item{\code{Bis_chloroethyl_ether_uf_ug_L}}{a logical vector}
#'           \item{\code{Bismuth_f_pCi_L}}{a logical vector}
#'           \item{\code{BOD_uf_mg_L}}{a numeric vector}
#'           \item{\code{Bolstar_uf_ug_L}}{a logical vector}
#'           \item{\code{Boron_borates_f_mg_L}}{a logical vector}
#'           \item{\code{Boron_borates_uf_mg_L}}{a numeric vector}
#'           \item{\code{ButylBenzylPhthalate_uf_ug_L}}{a logical vector}
#'           \item{\code{CaCO3_calc_mg_L}}{a logical vector}
#'           \item{\code{CaCO3_f_mg_L}}{a logical vector}
#'           \item{\code{CaCO3_mg_L}}{a numeric vector}
#'           \item{\code{Cadmium_calc_mg_kg}}{a logical vector}
#'           \item{\code{Cadmium_f_mg_L}}{a numeric vector}
#'           \item{\code{Cadmium_uf_mg_L}}{a numeric vector}
#'           \item{\code{Caffeine_f_mg_L}}{a logical vector}
#'           \item{\code{Calcium_f_mg_L}}{a numeric vector}
#'           \item{\code{Calcium_uf_mg_L}}{a numeric vector}
#'           \item{\code{Carbazole_uf_ug_L}}{a logical vector}
#'           \item{\code{Carbon_calc_.}}{a logical vector}
#'           \item{\code{Carbon_f_mg_L}}{a logical vector}
#'           \item{\code{Carbon_uf_mg_L}}{a logical vector}
#'           \item{\code{Carbonate_uf_mg_L}}{a numeric vector}
#'           \item{\code{Cesium_134_uf_pCi_L}}{a logical vector}
#'           \item{\code{Cesium_137_uf_pCi_L}}{a logical vector}
#'           \item{\code{Chlordane_cis_uf_ug_L}}{a logical vector}
#'           \item{\code{Chlordane_gamma_uf_ug_L}}{a logical vector}
#'           \item{\code{Chlordane_uf_ug_L}}{a logical vector}
#'           \item{\code{Chloride_uf_mg_L}}{a numeric vector}
#'           \item{\code{Chlorine_other_mg_L}}{a logical vector}
#'           \item{\code{Chlorine_uf_mg_L}}{a numeric vector}
#'           \item{\code{Chlorophyll_a_uf_ug_L}}{a numeric vector}
#'           \item{\code{Chlorpyrifos_uf_ug_L}}{a logical vector}
#'           \item{\code{Chromium_calc_mg_kg}}{a logical vector}
#'           \item{\code{Chromium_f_mg_L}}{a logical vector}
#'           \item{\code{Chromium_mg_L}}{a logical vector}
#'           \item{\code{Chromium_uf_mg_L}}{a numeric vector}
#'           \item{\code{Chrysene_uf_ug_L}}{a logical vector}
#'           \item{\code{Cobalt_58_uf_pCi_L}}{a logical vector}
#'           \item{\code{Cobalt_60_uf_pCi_L}}{a logical vector}
#'           \item{\code{Cobalt_f_mg_L}}{a logical vector}
#'           \item{\code{Cobalt_uf_ug_L}}{a logical vector}
#'           \item{\code{Copper_f_ug_L}}{a numeric vector}
#'           \item{\code{Copper_uf_mg_kg}}{a logical vector}
#'           \item{\code{Copper_uf_ug_L}}{a numeric vector}
#'           \item{\code{Coumaphos_uf_ug_L}}{a logical vector}
#'           \item{\code{CrossSectionalArea_calc}}{a numeric vector}
#'           \item{\code{Cyanide_uf_mg_L}}{a logical vector}
#'           \item{\code{deltaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
#'           \item{\code{Demeton_o_uf_ug_L}}{a logical vector}
#'           \item{\code{Demeton_s_total_uf_ug_L}}{a logical vector}
#'           \item{\code{Depth_ft}}{a numeric vector}
#'           \item{\code{Di_2ethylhexylPhthalate_uf_ug_L}}{a logical vector}
#'           \item{\code{Di_n_octylPhthalate_uf_ug_L}}{a logical vector}
#'           \item{\code{Diazinon_uf_ug_L}}{a logical vector}
#'           \item{\code{Dibenz_ah_anthracene_uf_ug_L}}{a logical vector}
#'           \item{\code{Dibenzofuran_uf_ug_L}}{a logical vector}
#'           \item{\code{DibutylPhthalate_uf_ug_L}}{a logical vector}
#'           \item{\code{Dichlorvos_uf_ug_L}}{a logical vector}
#'           \item{\code{Dieldrin_uf_ug_L}}{a logical vector}
#'           \item{\code{DiethylPhthalate_uf_ug_L}}{a logical vector}
#'           \item{\code{Dimethoate_uf_ug_L}}{a logical vector}
#'           \item{\code{DimethylPhthalate_uf_ug_L}}{a logical vector}
#'           \item{\code{Dinitro_o_cresol_uf_ug_L}}{a logical vector}
#'           \item{\code{Disulfoton_uf_ug_L}}{a logical vector}
#'           \item{\code{DO_f_.}}{a numeric vector}
#'           \item{\code{DO_f_mg_L}}{a numeric vector}
#'           \item{\code{Ecoli_uf_CFU_100}}{a numeric vector}
#'           \item{\code{Ecoli_uf_mg_L}}{a logical vector}
#'           \item{\code{Endosulfan_alpha_uf_ug_L}}{a logical vector}
#'           \item{\code{Endosulfan_beta_uf_ug_L}}{a logical vector}
#'           \item{\code{EndosulfanSulfate_uf_ug_L}}{a logical vector}
#'           \item{\code{Endrin_uf_ug_L}}{a logical vector}
#'           \item{\code{EndrinAldehyde_uf_ug_L}}{a logical vector}
#'           \item{\code{EndrinKetone_uf_ug_L}}{a logical vector}
#'           \item{\code{Ethoprop_std_ug_L}}{a logical vector}
#'           \item{\code{Ethyl_p_nitrophenylPhenylphosphorothioate_uf_ug_L}}{a logical vector}
#'           \item{\code{Fensulfothion_uf_ug_L}}{a logical vector}
#'           \item{\code{Fenthion_uf_ug_L}}{a logical vector}
#'           \item{\code{Flow_cfs}}{a numeric vector}
#'           \item{\code{Fluoranthene_uf_ug_L}}{a logical vector}
#'           \item{\code{Fluorene_uf_ug_L}}{a logical vector}
#'           \item{\code{Fluoride_f_mg_L}}{a logical vector}
#'           \item{\code{Fluoride_uf_mg_L}}{a numeric vector}
#'           \item{\code{gammaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
#'           \item{\code{Hardness_CaCO3_MgCO3_calc_mg_L}}{a numeric vector}
#'           \item{\code{Hardness_CaCO3_MgCO3_f_mg_L}}{a logical vector}
#'           \item{\code{Heptachlor_uf_ug_L}}{a logical vector}
#'           \item{\code{HeptachlorEpoxide_uf_ug_L}}{a logical vector}
#'           \item{\code{Hexachlorobenzene_uf_ug_L}}{a logical vector}
#'           \item{\code{Hexachlorobutadiene_uf_ug_L}}{a logical vector}
#'           \item{\code{Hexachlorocyclopentadiene_uf_ug_L}}{a logical vector}
#'           \item{\code{Hexachloroethane_uf_ug_L}}{a logical vector}
#'           \item{\code{HydrogenCarbonate_uf_mg_L}}{a numeric vector}
#'           \item{\code{Indeno_123cd_pyrene_uf_ug_L}}{a logical vector}
#'           \item{\code{Iodine_131_uf_pCi_L}}{a logical vector}
#'           \item{\code{Iron_59_uf_pCi_L}}{a logical vector}
#'           \item{\code{Iron_f_mg_L}}{a logical vector}
#'           \item{\code{Iron_uf_ug_L}}{a numeric vector}
#'           \item{\code{Isophorone_uf_ug_L}}{a logical vector}
#'           \item{\code{Kjeldahl_N_calc_mg_kg}}{a logical vector}
#'           \item{\code{Kjeldahl_N_uf_mg_L}}{a numeric vector}
#'           \item{\code{Lanthanum_140_uf_pCi_L}}{a logical vector}
#'           \item{\code{Lead_214_uf_pCi_L}}{a logical vector}
#'           \item{\code{Lead_cmpdsInorg_calc_mg_kg}}{a logical vector}
#'           \item{\code{Lead_cmpdsInorg_f_mg_L}}{a numeric vector}
#'           \item{\code{Lead_cmpdsInorg_susp_mg_L}}{a logical vector}
#'           \item{\code{Lead_cmpdsInorg_uf_mg_L}}{a numeric vector}
#'           \item{\code{Magnesium_f_mg_L}}{a numeric vector}
#'           \item{\code{Magnesium_uf_mg_L}}{a numeric vector}
#'           \item{\code{Malathion_uf_ug_L}}{a logical vector}
#'           \item{\code{Manganese_54_uf_pCi_L}}{a logical vector}
#'           \item{\code{Manganese_f_ug_L}}{a numeric vector}
#'           \item{\code{Manganese_uf_ug_L}}{a numeric vector}
#'           \item{\code{Mercury_elemental_calc_mg_kg}}{a logical vector}
#'           \item{\code{Mercury_elemental_f_mg_L}}{a numeric vector}
#'           \item{\code{Mercury_elemental_susp_mg_kg}}{a logical vector}
#'           \item{\code{Mercury_elemental_uf_mg_L}}{a numeric vector}
#'           \item{\code{Mercury_elemental_uf_ng_g}}{a logical vector}
#'           \item{\code{Merphos_uf_ug_L}}{a logical vector}
#'           \item{\code{Methoxychlor_uf_ug_L}}{a logical vector}
#'           \item{\code{MethylAzinphos_uf_ug_L}}{a logical vector}
#'           \item{\code{Methylmercury_uf_ng_g}}{a logical vector}
#'           \item{\code{Methylmercury_uf_ng_L}}{a logical vector}
#'           \item{\code{MethylParathion_uf_ug_L}}{a logical vector}
#'           \item{\code{Mevinphos_uf_ug_L}}{a logical vector}
#'           \item{\code{Molybdenum_f_mg_L}}{a logical vector}
#'           \item{\code{Molybdenum_uf_ug_L}}{a logical vector}
#'           \item{\code{Monocrotophos_uf_ug_L}}{a logical vector}
#'           \item{\code{N_nitrosodi_N_propylamine_uf_ug_L}}{a logical vector}
#'           \item{\code{N_nitrosodiphenylamine_uf_ug_L}}{a logical vector}
#'           \item{\code{Naled_uf_ug_L}}{a logical vector}
#'           \item{\code{Naphthalene_uf_ug_L}}{a logical vector}
#'           \item{\code{Nickel_calc_mg_kg}}{a logical vector}
#'           \item{\code{Nickel_f_mg_L}}{a logical vector}
#'           \item{\code{Nickel_uf_ug_L}}{a logical vector}
#'           \item{\code{Niobium_95_uf_pCi_L}}{a logical vector}
#'           \item{\code{Nitrobenzene_uf_ug_L}}{a logical vector}
#'           \item{\code{Nitrogen_uf_mg_L}}{a logical vector}
#'           \item{\code{NO2_N_uf_mg_L}}{a logical vector}
#'           \item{\code{NO2NO3_calc_mg_kg}}{a logical vector}
#'           \item{\code{NO3_N_uf_mg_L}}{a logical vector}
#'           \item{\code{NO3NO2_uf_mg_L}}{a numeric vector}
#'           \item{\code{p_BromodiphenylEther_uf_ug_L}}{a logical vector}
#'           \item{\code{p_Chloroaniline_uf_ug_L}}{a logical vector}
#'           \item{\code{p_Nitrophenol_uf_ug_L}}{a logical vector}
#'           \item{\code{p_p_primeDichlorodiphenyldichloroethane_uf_ug_L}}{a logical vector}
#'           \item{\code{p_p_primeDichlorodiphenyldichloroethylene_uf_ug_L}}{a logical vector}
#'           \item{\code{p_p_primeDichlorodiphenyltrichloroethane_uf_ug_L}}{a logical vector}
#'           \item{\code{ParachlorometaCresol_uf_ug_L}}{a logical vector}
#'           \item{\code{Parathion_uf_ug_L}}{a logical vector}
#'           \item{\code{Pentachlorophenol_uf_ug_L}}{a logical vector}
#'           \item{\code{Perchlorate_uf_ug_L}}{a numeric vector}
#'           \item{\code{pH_SU}}{a numeric vector}
#'           \item{\code{Phenanthrene_uf_ug_L}}{a logical vector}
#'           \item{\code{Phenol_uf_ug_L}}{a logical vector}
#'           \item{\code{PheophytinA_uf_ug_L}}{a numeric vector}
#'           \item{\code{Phorate_uf_ug_L}}{a logical vector}
#'           \item{\code{Phosphorus_calc_mg_kg}}{a logical vector}
#'           \item{\code{Phosphorus_uf_mg_L}}{a numeric vector}
#'           \item{\code{Potassium_40_uf_pCi_L}}{a logical vector}
#'           \item{\code{Potassium_f_mg_L}}{a numeric vector}
#'           \item{\code{Potassium_uf_mg_L}}{a numeric vector}
#'           \item{\code{Pyrene_uf_ug_L}}{a logical vector}
#'           \item{\code{Radium_226_228_calc_pCi_L}}{a logical vector}
#'           \item{\code{Radium_226_228_uf_pCi_L}}{a logical vector}
#'           \item{\code{Radium_226_uf_pCi_L}}{a logical vector}
#'           \item{\code{Radium_228_f_pCi_L}}{a logical vector}
#'           \item{\code{Radium_228_uf_pCi_L}}{a logical vector}
#'           \item{\code{RedoxPotential_uf_mV}}{a numeric vector}
#'           \item{\code{Ronnel_uf_ug_L}}{a logical vector}
#'           \item{\code{Selenium_cmpds_calc_mg_kg}}{a logical vector}
#'           \item{\code{Selenium_cmpds_f_mg_L}}{a logical vector}
#'           \item{\code{Selenium_cmpds_uf_mg_L}}{a numeric vector}
#'           \item{\code{Silica_uf_mg_L}}{a logical vector}
#'           \item{\code{Silicon_uf_ug_L}}{a logical vector}
#'           \item{\code{Silver_calc_mg_kg}}{a logical vector}
#'           \item{\code{Silver_f_mg_L}}{a logical vector}
#'           \item{\code{Silver_uf_mg_L}}{a logical vector}
#'           \item{\code{Sodium_f_mg_L}}{a numeric vector}
#'           \item{\code{Sodium_uf_mg_L}}{a numeric vector}
#'           \item{\code{SolidsInSediment_calc_.}}{a logical vector}
#'           \item{\code{SpecCond_umhos_cm}}{a numeric vector}
#'           \item{\code{Strontium_f_mg_L}}{a logical vector}
#'           \item{\code{Sulfate_uf_mg_L}}{a numeric vector}
#'           \item{\code{TDS_calc_mg_L}}{a logical vector}
#'           \item{\code{TDS_f_mg_L}}{a numeric vector}
#'           \item{\code{Temp_degC}}{a numeric vector}
#'           \item{\code{Temp_degF}}{a logical vector}
#'           \item{\code{Tetrachlorovinphos_uf_ug_L}}{a logical vector}
#'           \item{\code{Tetraethyldithiopyrophosphate_uf_ug_L}}{a logical vector}
#'           \item{\code{Tetraethylpyrophosphate_uf_ug_L}}{a logical vector}
#'           \item{\code{Thallium_208_uf_pCi_L}}{a logical vector}
#'           \item{\code{Thallium_f_mg_L}}{a logical vector}
#'           \item{\code{Thallium_uf_mg_L}}{a logical vector}
#'           \item{\code{Tokuthion_uf_ug_L}}{a logical vector}
#'           \item{\code{Toxaphene_uf_ug_L}}{a logical vector}
#'           \item{\code{Trichloronate_uf_ug_L}}{a logical vector}
#'           \item{\code{TSS_coarse_susp_mg_L}}{a numeric vector}
#'           \item{\code{TSS_fine_susp_mg_L}}{a numeric vector}
#'           \item{\code{TSS_susp_mg_L}}{a numeric vector}
#'           \item{\code{Turbidity_NTU}}{a numeric vector}
#'           \item{\code{Uranium_natural_f_ug_L}}{a logical vector}
#'           \item{\code{Uranium_Natural_uf_pCi_L}}{a logical vector}
#'           \item{\code{Vanadium_f_mg_L}}{a logical vector}
#'           \item{\code{Vanadium_uf_mg_L}}{a logical vector}
#'           \item{\code{Velocity_ft_sec}}{a numeric vector}
#'           \item{\code{Zinc_65_uf_pCi_L}}{a logical vector}
#'           \item{\code{Zinc_calc_mg_kg}}{a logical vector}
#'           \item{\code{Zinc_f_mg_L}}{a numeric vector}
#'           \item{\code{Zinc_uf_mg_L}}{a numeric vector}
#'           \item{\code{Zirconium_95_uf_pCi_L}}{a logical vector}
#'           \item{\code{Alkalinity_bicarbonate_uf_mg_L}}{a numeric vector}
#'           \item{\code{Alkalinity_carbonate_uf_mg_L}}{a numeric vector}
#'           \item{\code{Alkalinity_hydroxide_uf_mg_L}}{a numeric vector}
#'           \item{\code{Alkalinity_total_uf_mg_L}}{a numeric vector}
#'           \item{\code{Barium_cmpds_f_mg_L}}{a numeric vector}
#'           \item{\code{CaCO3_uf_mg_L}}{a numeric vector}
#'           \item{\code{Chlorophyll_pheophytin_uf_ratio}}{a numeric vector}
#'           \item{\code{Depth_unk}}{a numeric vector}
#'           \item{\code{DO_f_unk}}{a numeric vector}
#'           \item{\code{DOSat_f_.}}{a numeric vector}
#'           \item{\code{DOSat_f_unk}}{a numeric vector}
#'           \item{\code{EventCond_none}}{a numeric vector}
#'           \item{\code{Fines_lt2mm_pct_Reach}}{a numeric vector}
#'           \item{\code{Fines_lt2mm_pct_Riffle}}{a numeric vector}
#'           \item{\code{Flow_unk}}{a numeric vector}
#'           \item{\code{Hardness_CaCO3_MgCO3_uf_mg_L}}{a numeric vector}
#'           \item{\code{NH3_N_calc_mg_kg}}{a numeric vector}
#'           \item{\code{NH3_N_uf_mg_L}}{a numeric vector}
#'           \item{\code{SpecCond_unk}}{a numeric vector}
#'           \item{\code{StreamCrossSectArea_unk}}{a numeric vector}
#'           \item{\code{StreamWidth_ft}}{a numeric vector}
#'           \item{\code{StreamWidth_unk}}{a numeric vector}
#'           \item{\code{TDS_f_unk}}{a numeric vector}
#'           \item{\code{TDS_uf_mg_L}}{a numeric vector}
#'           \item{\code{Temp_air_degC}}{a numeric vector}
#'           \item{\code{Temp_air_unk}}{a numeric vector}
#'           \item{\code{Temp_unk}}{a numeric vector}
#'           \item{\code{Turbidity_unk}}{a numeric vector}
#'           \item{\code{Uranium_natural_uf_ug_L}}{a numeric vector}
#'           \item{\code{Velocity_unk}}{a numeric vector}
#' }
#' @source example data
"data_CoOccur_AZ_Hi"           
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# data_CoOccur_AZ_Lo ####
#' @title Co-Occurrence example data (AZ lo)
#' 
#' @description A dataset from California with example biological, chemical, habitat, and geo-physical parameters.
#' 
#' @format A data frame with 356 observations on the following 310 variables:
#' \describe{
#'          \item{\code{StationID_Master}}{a factor with levels}
#'          \item{\code{Group}}{a numeric vector}
#'          \item{\code{SampYear}}{a numeric vector}
#'          \item{\code{ChemSampleID}}{a factor with levels }
#'          \item{\code{SampDate}}{a factor with levels }
#'          \item{\code{BenCollDate}}{a factor with levels }
#'          \item{\code{IBI}}{a numeric vector}
#'          \item{\code{X124Trichlorobenzene_uf_ug_L}}{a logical vector}
#'          \item{\code{X12Dichlorobenzene_uf_ug_L}}{a logical vector}
#'          \item{\code{X13Dichlorobenzene_uf_ug_L}}{a logical vector}
#'          \item{\code{X14Dichlorobenzene_uf_ug_L}}{a logical vector}
#'          \item{\code{X245Trichlorophenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X246Trichlorophenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X24Dichlorophenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X24Dimethylphenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X24Dinitrophenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X24Dinitrotoluene_uf_ug_L}}{a logical vector}
#'          \item{\code{X26Dinitrotoluene_uf_ug_L}}{a logical vector}
#'          \item{\code{X2Chlorophenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X2Methylnaphthalene_uf_ug_L}}{a logical vector}
#'          \item{\code{X2Methylphenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X2Nitroaniline_uf_ug_L}}{a logical vector}
#'          \item{\code{X2Nitrophenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X33primeDichlorobenzidine_uf_ug_L}}{a logical vector}
#'          \item{\code{X3Nitroaniline_uf_ug_L}}{a logical vector}
#'          \item{\code{X4Chlorophenylphenylether_uf_ug_L}}{a logical vector}
#'          \item{\code{X4Methylphenol_uf_ug_L}}{a logical vector}
#'          \item{\code{X4Nitroaniline_uf_ug_L}}{a logical vector}
#'          \item{\code{Acenaphthene_uf_ug_L}}{a logical vector}
#'          \item{\code{Acenaphthylene_uf_ug_L}}{a logical vector}
#'          \item{\code{Aldrin_uf_ug_L}}{a logical vector}
#'          \item{\code{Alkalinity_bicarbonate_uf_mg_L}}{a logical vector}
#'          \item{\code{Alkalinity_carbonate_uf_mg_L}}{a logical vector}
#'          \item{\code{Alkalinity_hydroxide_uf_mg_L}}{a logical vector}
#'          \item{\code{Alkalinity_phenolphthalein_uf_mg_L}}{a numeric vector}
#'          \item{\code{Alkalinity_total_uf_mg_L}}{a logical vector}
#'          \item{\code{Alpha_grossAsUNatural_calc_pCi_L}}{a logical vector}
#'          \item{\code{Alpha_grossAsUNatural_uf_pCi_L}}{a logical vector}
#'          \item{\code{alphaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
#'          \item{\code{Aluminum_f_mg_L}}{a logical vector}
#'          \item{\code{Aluminum_uf_ug_L}}{a logical vector}
#'          \item{\code{Aniline_uf_ug_L}}{a logical vector}
#'          \item{\code{Anthracene_uf_ug_L}}{a logical vector}
#'          \item{\code{Antimony_f_mg_L}}{a numeric vector}
#'          \item{\code{Antimony_uf_mg_L}}{a numeric vector}
#'          \item{\code{Arsenic_inorg_calc_mg_kg}}{a logical vector}
#'          \item{\code{Arsenic_inorg_f_mg_L}}{a logical vector}
#'          \item{\code{Arsenic_inorg_f_ug_L}}{a numeric vector}
#'          \item{\code{Arsenic_inorg_susp_ug_L}}{a logical vector}
#'          \item{\code{Arsenic_inorg_uf_mg_L}}{a logical vector}
#'          \item{\code{Arsenic_inorg_uf_ug_L}}{a numeric vector}
#'          \item{\code{Azobenzene_uf_ug_L}}{a logical vector}
#'          \item{\code{Barium_cmpds_calc_mg_kg}}{a logical vector}
#'          \item{\code{Barium_cmpds_f_mg_L}}{a logical vector}
#'          \item{\code{Barium_cmpds_f_ug_L}}{a logical vector}
#'          \item{\code{Barium_cmpds_uf_pCi_L}}{a logical vector}
#'          \item{\code{Barium_cmpds_uf_ug_L}}{a numeric vector}
#'          \item{\code{BarometricPressure_mmHg}}{a numeric vector}
#'          \item{\code{Benzo_a_antracene_uf_ug_L}}{a logical vector}
#'          \item{\code{Benzo_a_pyrene_uf_ug_L}}{a logical vector}
#'          \item{\code{Benzo_b_flouranthene_uf_ug_L}}{a logical vector}
#'          \item{\code{Benzo_ghi_perylene_uf_ug_L}}{a logical vector}
#'          \item{\code{Benzo_k_fluoranthene_uf_ug_L}}{a logical vector}
#'          \item{\code{BenzoicAcid_uf_ug_L}}{a logical vector}
#'          \item{\code{BenzylAlcohol_uf_ug_L}}{a logical vector}
#'          \item{\code{Beryllium_cmpds_calc_mg_kg}}{a logical vector}
#'          \item{\code{Beryllium_cmpds_f_mg_L}}{a logical vector}
#'          \item{\code{Beryllium_cmpds_uf_mg_L}}{a numeric vector}
#'          \item{\code{Beta_radiation_calc_pCi_L}}{a logical vector}
#'          \item{\code{Beta_radiation_uf_pCi_L}}{a logical vector}
#'          \item{\code{betaChloronapthalene_uf_ug_L}}{a logical vector}
#'          \item{\code{betaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
#'          \item{\code{Bis_2chloroethoxy_methane_uf_ug_L}}{a logical vector}
#'          \item{\code{Bis_2chloroisopropyl_ether_uf_ug_L}}{a logical vector}
#'          \item{\code{Bis_chloroethyl_ether_uf_ug_L}}{a logical vector}
#'          \item{\code{Bismuth_f_pCi_L}}{a logical vector}
#'          \item{\code{BOD_uf_mg_L}}{a numeric vector}
#'          \item{\code{Bolstar_uf_ug_L}}{a logical vector}
#'          \item{\code{Boron_borates_f_mg_L}}{a logical vector}
#'          \item{\code{Boron_borates_uf_mg_L}}{a numeric vector}
#'          \item{\code{ButylBenzylPhthalate_uf_ug_L}}{a logical vector}
#'          \item{\code{CaCO3_calc_mg_L}}{a logical vector}
#'          \item{\code{CaCO3_f_mg_L}}{a logical vector}
#'          \item{\code{CaCO3_mg_L}}{a numeric vector}
#'          \item{\code{CaCO3_uf_mg_L}}{a numeric vector}
#'          \item{\code{Cadmium_calc_mg_kg}}{a logical vector}
#'          \item{\code{Cadmium_f_mg_L}}{a numeric vector}
#'          \item{\code{Cadmium_uf_mg_L}}{a numeric vector}
#'          \item{\code{Caffeine_f_mg_L}}{a logical vector}
#'          \item{\code{Calcium_f_mg_L}}{a numeric vector}
#'          \item{\code{Calcium_uf_mg_L}}{a numeric vector}
#'          \item{\code{Carbazole_uf_ug_L}}{a logical vector}
#'          \item{\code{Carbon_calc_.}}{a logical vector}
#'          \item{\code{Carbon_f_mg_L}}{a logical vector}
#'          \item{\code{Carbon_uf_mg_L}}{a logical vector}
#'          \item{\code{Carbonate_uf_mg_L}}{a numeric vector}
#'          \item{\code{Cesium_134_uf_pCi_L}}{a logical vector}
#'          \item{\code{Cesium_137_uf_pCi_L}}{a logical vector}
#'          \item{\code{Chlordane_cis_uf_ug_L}}{a logical vector}
#'          \item{\code{Chlordane_gamma_uf_ug_L}}{a logical vector}
#'          \item{\code{Chlordane_uf_ug_L}}{a logical vector}
#'          \item{\code{Chloride_uf_mg_L}}{a numeric vector}
#'          \item{\code{Chlorine_other_mg_L}}{a logical vector}
#'          \item{\code{Chlorine_uf_mg_L}}{a numeric vector}
#'          \item{\code{Chlorophyll_a_uf_ug_L}}{a numeric vector}
#'          \item{\code{Chlorophyll_pheophytin_uf_ratio}}{a logical vector}
#'          \item{\code{Chlorpyrifos_uf_ug_L}}{a logical vector}
#'          \item{\code{Chromium_calc_mg_kg}}{a logical vector}
#'          \item{\code{Chromium_f_mg_L}}{a logical vector}
#'          \item{\code{Chromium_mg_L}}{a logical vector}
#'          \item{\code{Chromium_uf_mg_L}}{a numeric vector}
#'          \item{\code{Chrysene_uf_ug_L}}{a logical vector}
#'          \item{\code{Cobalt_58_uf_pCi_L}}{a logical vector}
#'          \item{\code{Cobalt_60_uf_pCi_L}}{a logical vector}
#'          \item{\code{Cobalt_f_mg_L}}{a logical vector}
#'          \item{\code{Cobalt_uf_ug_L}}{a logical vector}
#'          \item{\code{Copper_f_ug_L}}{a numeric vector}
#'          \item{\code{Copper_uf_mg_kg}}{a logical vector}
#'          \item{\code{Copper_uf_ug_L}}{a numeric vector}
#'          \item{\code{Coumaphos_uf_ug_L}}{a logical vector}
#'          \item{\code{CrossSectionalArea_calc}}{a numeric vector}
#'          \item{\code{Cyanide_uf_mg_L}}{a logical vector}
#'          \item{\code{deltaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
#'          \item{\code{Demeton_o_uf_ug_L}}{a logical vector}
#'          \item{\code{Demeton_s_total_uf_ug_L}}{a logical vector}
#'          \item{\code{Depth_ft}}{a numeric vector}
#'          \item{\code{Depth_unk}}{a logical vector}
#'          \item{\code{Di_2ethylhexylPhthalate_uf_ug_L}}{a logical vector}
#'          \item{\code{Di_n_octylPhthalate_uf_ug_L}}{a logical vector}
#'          \item{\code{Diazinon_uf_ug_L}}{a logical vector}
#'          \item{\code{Dibenz_ah_anthracene_uf_ug_L}}{a logical vector}
#'          \item{\code{Dibenzofuran_uf_ug_L}}{a logical vector}
#'          \item{\code{DibutylPhthalate_uf_ug_L}}{a logical vector}
#'          \item{\code{Dichlorvos_uf_ug_L}}{a logical vector}
#'          \item{\code{Dieldrin_uf_ug_L}}{a logical vector}
#'          \item{\code{DiethylPhthalate_uf_ug_L}}{a logical vector}
#'          \item{\code{Dimethoate_uf_ug_L}}{a logical vector}
#'          \item{\code{DimethylPhthalate_uf_ug_L}}{a logical vector}
#'          \item{\code{Dinitro_o_cresol_uf_ug_L}}{a logical vector}
#'          \item{\code{Disulfoton_uf_ug_L}}{a logical vector}
#'          \item{\code{DO_f_.}}{a numeric vector}
#'          \item{\code{DO_f_mg_L}}{a numeric vector}
#'          \item{\code{DO_f_unk}}{a logical vector}
#'          \item{\code{DOSat_f_.}}{a logical vector}
#'          \item{\code{DOSat_f_unk}}{a logical vector}
#'          \item{\code{Ecoli_uf_CFU_100}}{a numeric vector}
#'          \item{\code{Ecoli_uf_mg_L}}{a logical vector}
#'          \item{\code{Endosulfan_alpha_uf_ug_L}}{a logical vector}
#'          \item{\code{Endosulfan_beta_uf_ug_L}}{a logical vector}
#'          \item{\code{EndosulfanSulfate_uf_ug_L}}{a logical vector}
#'          \item{\code{Endrin_uf_ug_L}}{a logical vector}
#'          \item{\code{EndrinAldehyde_uf_ug_L}}{a logical vector}
#'          \item{\code{EndrinKetone_uf_ug_L}}{a logical vector}
#'          \item{\code{Ethoprop_std_ug_L}}{a logical vector}
#'          \item{\code{Ethyl_p_nitrophenylPhenylphosphorothioate_uf_ug_L}}{a logical vector}
#'          \item{\code{EventCond_none}}{a logical vector}
#'          \item{\code{Fensulfothion_uf_ug_L}}{a logical vector}
#'          \item{\code{Fenthion_uf_ug_L}}{a logical vector}
#'          \item{\code{Fines_lt2mm_pct_Reach}}{a numeric vector}
#'          \item{\code{Fines_lt2mm_pct_Riffle}}{a numeric vector}
#'          \item{\code{Flow_cfs}}{a numeric vector}
#'          \item{\code{Flow_unk}}{a logical vector}
#'          \item{\code{Fluoranthene_uf_ug_L}}{a logical vector}
#'          \item{\code{Fluorene_uf_ug_L}}{a logical vector}
#'          \item{\code{Fluoride_f_mg_L}}{a logical vector}
#'          \item{\code{Fluoride_uf_mg_L}}{a numeric vector}
#'          \item{\code{gammaHexachlorocyclohexane_uf_ug_L}}{a logical vector}
#'          \item{\code{Hardness_CaCO3_MgCO3_calc_mg_L}}{a numeric vector}
#'          \item{\code{Hardness_CaCO3_MgCO3_f_mg_L}}{a logical vector}
#'          \item{\code{Hardness_CaCO3_MgCO3_uf_mg_L}}{a logical vector}
#'          \item{\code{Heptachlor_uf_ug_L}}{a logical vector}
#'          \item{\code{HeptachlorEpoxide_uf_ug_L}}{a logical vector}
#'          \item{\code{Hexachlorobenzene_uf_ug_L}}{a logical vector}
#'          \item{\code{Hexachlorobutadiene_uf_ug_L}}{a logical vector}
#'          \item{\code{Hexachlorocyclopentadiene_uf_ug_L}}{a logical vector}
#'          \item{\code{Hexachloroethane_uf_ug_L}}{a logical vector}
#'          \item{\code{HydrogenCarbonate_uf_mg_L}}{a numeric vector}
#'          \item{\code{Indeno_123cd_pyrene_uf_ug_L}}{a logical vector}
#'          \item{\code{Iodine_131_uf_pCi_L}}{a logical vector}
#'          \item{\code{Iron_59_uf_pCi_L}}{a logical vector}
#'          \item{\code{Iron_f_mg_L}}{a logical vector}
#'          \item{\code{Iron_uf_ug_L}}{a logical vector}
#'          \item{\code{Isophorone_uf_ug_L}}{a logical vector}
#'          \item{\code{Kjeldahl_N_calc_mg_kg}}{a logical vector}
#'          \item{\code{Kjeldahl_N_uf_mg_L}}{a numeric vector}
#'          \item{\code{Lanthanum_140_uf_pCi_L}}{a logical vector}
#'          \item{\code{Lead_214_uf_pCi_L}}{a logical vector}
#'          \item{\code{Lead_cmpdsInorg_calc_mg_kg}}{a logical vector}
#'          \item{\code{Lead_cmpdsInorg_f_mg_L}}{a numeric vector}
#'          \item{\code{Lead_cmpdsInorg_susp_mg_L}}{a logical vector}
#'          \item{\code{Lead_cmpdsInorg_uf_mg_L}}{a numeric vector}
#'          \item{\code{Magnesium_f_mg_L}}{a numeric vector}
#'          \item{\code{Magnesium_uf_mg_L}}{a numeric vector}
#'          \item{\code{Malathion_uf_ug_L}}{a logical vector}
#'          \item{\code{Manganese_54_uf_pCi_L}}{a logical vector}
#'          \item{\code{Manganese_f_ug_L}}{a logical vector}
#'          \item{\code{Manganese_uf_ug_L}}{a numeric vector}
#'          \item{\code{Mercury_elemental_calc_mg_kg}}{a logical vector}
#'          \item{\code{Mercury_elemental_f_mg_L}}{a numeric vector}
#'          \item{\code{Mercury_elemental_susp_mg_kg}}{a logical vector}
#'          \item{\code{Mercury_elemental_uf_mg_L}}{a numeric vector}
#'          \item{\code{Mercury_elemental_uf_ng_g}}{a logical vector}
#'          \item{\code{Merphos_uf_ug_L}}{a logical vector}
#'          \item{\code{Methoxychlor_uf_ug_L}}{a logical vector}
#'          \item{\code{MethylAzinphos_uf_ug_L}}{a logical vector}
#'          \item{\code{Methylmercury_uf_ng_g}}{a logical vector}
#'          \item{\code{Methylmercury_uf_ng_L}}{a logical vector}
#'          \item{\code{MethylParathion_uf_ug_L}}{a logical vector}
#'          \item{\code{Mevinphos_uf_ug_L}}{a logical vector}
#'          \item{\code{Molybdenum_f_mg_L}}{a logical vector}
#'          \item{\code{Molybdenum_uf_ug_L}}{a logical vector}
#'          \item{\code{Monocrotophos_uf_ug_L}}{a logical vector}
#'          \item{\code{N_nitrosodi_N_propylamine_uf_ug_L}}{a logical vector}
#'          \item{\code{N_nitrosodiphenylamine_uf_ug_L}}{a logical vector}
#'          \item{\code{Naled_uf_ug_L}}{a logical vector}
#'          \item{\code{Naphthalene_uf_ug_L}}{a logical vector}
#'          \item{\code{NH3_N_calc_mg_kg}}{a logical vector}
#'          \item{\code{NH3_N_uf_mg_L}}{a numeric vector}
#'          \item{\code{Nickel_calc_mg_kg}}{a logical vector}
#'          \item{\code{Nickel_f_mg_L}}{a logical vector}
#'          \item{\code{Nickel_uf_ug_L}}{a logical vector}
#'          \item{\code{Niobium_95_uf_pCi_L}}{a logical vector}
#'          \item{\code{Nitrobenzene_uf_ug_L}}{a logical vector}
#'          \item{\code{Nitrogen_uf_mg_L}}{a logical vector}
#'          \item{\code{NO2_N_uf_mg_L}}{a logical vector}
#'          \item{\code{NO2NO3_calc_mg_kg}}{a logical vector}
#'          \item{\code{NO3_N_uf_mg_L}}{a logical vector}
#'          \item{\code{NO3NO2_uf_mg_L}}{a numeric vector}
#'          \item{\code{p_BromodiphenylEther_uf_ug_L}}{a logical vector}
#'          \item{\code{p_Chloroaniline_uf_ug_L}}{a logical vector}
#'          \item{\code{p_Nitrophenol_uf_ug_L}}{a logical vector}
#'          \item{\code{p_p_primeDichlorodiphenyldichloroethane_uf_ug_L}}{a logical vector}
#'          \item{\code{p_p_primeDichlorodiphenyldichloroethylene_uf_ug_L}}{a logical vector}
#'          \item{\code{p_p_primeDichlorodiphenyltrichloroethane_uf_ug_L}}{a logical vector}
#'          \item{\code{ParachlorometaCresol_uf_ug_L}}{a logical vector}
#'          \item{\code{Parathion_uf_ug_L}}{a logical vector}
#'          \item{\code{Pentachlorophenol_uf_ug_L}}{a logical vector}
#'          \item{\code{Perchlorate_uf_ug_L}}{a numeric vector}
#'          \item{\code{pH_SU}}{a numeric vector}
#'          \item{\code{Phenanthrene_uf_ug_L}}{a logical vector}
#'          \item{\code{Phenol_uf_ug_L}}{a logical vector}
#'          \item{\code{PheophytinA_uf_ug_L}}{a numeric vector}
#'          \item{\code{Phorate_uf_ug_L}}{a logical vector}
#'          \item{\code{Phosphorus_calc_mg_kg}}{a logical vector}
#'          \item{\code{Phosphorus_uf_mg_L}}{a numeric vector}
#'          \item{\code{Potassium_40_uf_pCi_L}}{a logical vector}
#'          \item{\code{Potassium_f_mg_L}}{a numeric vector}
#'          \item{\code{Potassium_uf_mg_L}}{a numeric vector}
#'          \item{\code{Pyrene_uf_ug_L}}{a logical vector}
#'          \item{\code{Radium_226_228_calc_pCi_L}}{a logical vector}
#'          \item{\code{Radium_226_228_uf_pCi_L}}{a logical vector}
#'          \item{\code{Radium_226_uf_pCi_L}}{a logical vector}
#'          \item{\code{Radium_228_f_pCi_L}}{a logical vector}
#'          \item{\code{Radium_228_uf_pCi_L}}{a logical vector}
#'          \item{\code{RedoxPotential_uf_mV}}{a numeric vector}
#'          \item{\code{Ronnel_uf_ug_L}}{a logical vector}
#'          \item{\code{Selenium_cmpds_calc_mg_kg}}{a logical vector}
#'          \item{\code{Selenium_cmpds_f_mg_L}}{a logical vector}
#'          \item{\code{Selenium_cmpds_uf_mg_L}}{a numeric vector}
#'          \item{\code{Silica_uf_mg_L}}{a logical vector}
#'          \item{\code{Silicon_uf_ug_L}}{a logical vector}
#'          \item{\code{Silver_calc_mg_kg}}{a logical vector}
#'          \item{\code{Silver_f_mg_L}}{a logical vector}
#'          \item{\code{Silver_uf_mg_L}}{a logical vector}
#'          \item{\code{Sodium_f_mg_L}}{a numeric vector}
#'          \item{\code{Sodium_uf_mg_L}}{a numeric vector}
#'          \item{\code{SolidsInSediment_calc_.}}{a logical vector}
#'          \item{\code{SpecCond_umhos_cm}}{a numeric vector}
#'          \item{\code{SpecCond_unk}}{a logical vector}
#'          \item{\code{StreamCrossSectArea_unk}}{a logical vector}
#'          \item{\code{StreamWidth_ft}}{a numeric vector}
#'          \item{\code{StreamWidth_unk}}{a logical vector}
#'          \item{\code{Strontium_f_mg_L}}{a logical vector}
#'          \item{\code{Sulfate_uf_mg_L}}{a numeric vector}
#'          \item{\code{TDS_calc_mg_L}}{a logical vector}
#'          \item{\code{TDS_f_mg_L}}{a numeric vector}
#'          \item{\code{TDS_f_unk}}{a logical vector}
#'          \item{\code{TDS_uf_mg_L}}{a logical vector}
#'          \item{\code{Temp_air_degC}}{a logical vector}
#'          \item{\code{Temp_air_unk}}{a logical vector}
#'          \item{\code{Temp_degC}}{a numeric vector}
#'          \item{\code{Temp_degF}}{a logical vector}
#'          \item{\code{Temp_unk}}{a logical vector}
#'          \item{\code{Tetrachlorovinphos_uf_ug_L}}{a logical vector}
#'          \item{\code{Tetraethyldithiopyrophosphate_uf_ug_L}}{a logical vector}
#'          \item{\code{Tetraethylpyrophosphate_uf_ug_L}}{a logical vector}
#'          \item{\code{Thallium_208_uf_pCi_L}}{a logical vector}
#'          \item{\code{Thallium_f_mg_L}}{a logical vector}
#'          \item{\code{Thallium_uf_mg_L}}{a logical vector}
#'          \item{\code{Tokuthion_uf_ug_L}}{a logical vector}
#'          \item{\code{Toxaphene_uf_ug_L}}{a logical vector}
#'          \item{\code{Trichloronate_uf_ug_L}}{a logical vector}
#'          \item{\code{TSS_coarse_susp_mg_L}}{a numeric vector}
#'          \item{\code{TSS_fine_susp_mg_L}}{a numeric vector}
#'          \item{\code{TSS_susp_mg_L}}{a numeric vector}
#'          \item{\code{Turbidity_NTU}}{a numeric vector}
#'          \item{\code{Turbidity_unk}}{a logical vector}
#'          \item{\code{Uranium_natural_f_ug_L}}{a logical vector}
#'          \item{\code{Uranium_natural_uf_mg_L}}{a logical vector}
#'          \item{\code{Uranium_Natural_uf_pCi_L}}{a logical vector}
#'          \item{\code{Uranium_natural_uf_ug_L}}{a logical vector}
#'          \item{\code{Vanadium_f_mg_L}}{a logical vector}
#'          \item{\code{Vanadium_uf_mg_L}}{a logical vector}
#'          \item{\code{Velocity_ft_sec}}{a numeric vector}
#'          \item{\code{Velocity_unk}}{a logical vector}
#'          \item{\code{Zinc_65_uf_pCi_L}}{a logical vector}
#'          \item{\code{Zinc_calc_mg_kg}}{a logical vector}
#'         \item{\code{Zinc_f_mg_L}}{a numeric vector}
#'          \item{\code{Zinc_uf_mg_L}}{a numeric vector}
#'          \item{\code{Zirconium_95_uf_pCi_L}}{a logical vector}
#' }
#' @source example data
"data_CoOccur_AZ_Lo"           
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   

# data_GIS_AZ_Outline ####
#' @title AZ state outline
#' 
#' @description A dataset with example GIS state outline of Arizona  
#' for use with the getSiteInfo function.  1 row with 51 columns.
#' 
#' @format Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
#' 
#' \describe{
#'..@ data       :'data.frame':	1 obs. of  51 variables:
#'  .. ..$ AREA      : num 113713
#'.. ..$ STATE_NAME: Factor w/ 1 level "Arizona": 1
#'.. ..$ STATE_FIPS: Factor w/ 1 level "04": 1
#'.. ..$ SUB_REGION: Factor w/ 1 level "Mtn": 1
#'.. ..$ STATE_ABBR: Factor w/ 1 level "AZ": 1
#'.. ..$ POP1990   : Factor w/ 1 level "3665228": 1
#'.. ..$ POP1999   : Factor w/ 1 level "4790311": 1
#'.. ..$ POP90_SQMI: int 32
#'.. ..$ HOUSEHOLDS: int 1368843
#'.. ..$ MALES     : int 1810691
#'.. ..$ FEMALES   : int 1854537
#'.. ..$ WHITE     : int 2963186
#'.. ..$ BLACK     : int 110524
#'.. ..$ AMERI_ES  : int 203527
#'.. ..$ ASIAN_PI  : int 55206
#'.. ..$ OTHER     : int 332785
#'.. ..$ HISPANIC  : int 688338
#'.. ..$ AGE_UNDER5: int 292859
#'.. ..$ AGE_5_17  : int 688260
#'.. ..$ AGE_18_29 : int 710728
#'.. ..$ AGE_30_49 : int 1038130
#'.. ..$ AGE_50_64 : int 456477
#'.. ..$ AGE_65_UP : int 478774
#'.. ..$ NEVERMARRY: int 723628
#'.. ..$ MARRIED   : int 1576632
#'.. ..$ SEPARATED : int 57025
#'.. ..$ WIDOWED   : int 182501
#'.. ..$ DIVORCED  : int 292486
#'.. ..$ HSEHLD_1_M: int 148181
#'.. ..$ HSEHLD_1_F: int 189500
#'.. ..$ MARHH_CHD : int 348392
#'.. ..$ MARHH_NO_C: int 399414
#'.. ..$ MHH_CHILD : int 29655
#'.. ..$ FHH_CHILD : int 99609
#'.. ..$ HSE_UNITS : int 1659430
#'
#'.. ..$ VACANT    : int 290587
#'. ..$ OWNER_OCC : int 878561
#'.. ..$ RENTER_OCC: int 490282
#'.. ..$ MEDIAN_VAL: int 80100
#'.. ..$ MEDIANRENT: int 370
#'.. ..$ UNITS_1DET: int 867884
#'.. ..$ UNITS_1ATT: int 109989
#'.. ..$ UNITS2    : int 28826
#'.. ..$ UNITS3_9  : int 120656
#'.. ..$ UNITS10_49: int 153822
#'.. ..$ UNITS50_UP: int 103386
#'.. ..$ MOBILEHOME: int 250597
#'.. ..$ NO_FARMS87: int 7669
#'.. ..$ AVG_SIZE87: int 4732
#'.. ..$ CROP_ACR87: int 1453852
#'.. ..$ AVG_SALE87: int 212354
#'..@ polygons   :List of 1
#'.. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
#'.. .. .. ..@ Polygons :List of 1
#'.. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
#'.. .. .. .. .. .. ..@ labpt  : num [1:2] -1363532 -559198
#'.. .. .. .. .. .. ..@ area   : num 2.95e+11
#'.. .. .. .. .. .. ..@ hole   : logi FALSE
#'.. .. .. .. .. .. ..@ ringDir: int 1
#'.. .. .. .. .. .. ..@ coords : num [1:153, 1:2] -1637983 -1641135 -1645795 -1647744 -1648474 ...
#'.. .. .. ..@ plotOrder: int 1
#'.. .. .. ..@ labpt    : num [1:2] -1363532 -559198
#'.. .. .. ..@ ID       : chr "0"
#'.. .. .. ..@ area     : num 2.95e+11
#'..@ plotOrder  : int 1
#'..@ bbox       : num [1:2, 1:2] -1676669 -931786 -1094035 -209591
#'.. ..- attr(*, "dimnames")=List of 2
#'.. .. ..$ : chr [1:2] "x" "y"
#'.. .. ..$ : chr [1:2] "min" "max"
#'..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#'.. .. ..@ projargs: chr "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#'}
#' 
#' @source example data
"data_GIS_AZ_Outline"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_GIS_Flow_HI ####
#' @title NHD+ flow line example data, AZ high gradient
#' 
#' @description A dataset with example GIS flow line data 
#' for use with the getSiteInfo function.  5,658 rows and 836 columns.
#' 
#' @format Formal class 'SpatialLinesDataFrame' [package "sp"] with 4 slots, 87.7 Mb:
#' \describe{
#'   ..@ data       :'data.frame': 5658 obs. of  836 variables:
#'   .. ..$ OBJECTID                      : int [1:5658] 2099383 2099384 2099420 2099421 2099422 2099423 2099424 2099425 2099426 2099427 ...
#'   .. ..$ COMID                         : int [1:5658] 3528899 3528611 3529549 3529547 3529555 3529571 3529587 3530021 3529589 3529593 ...
#'   .. ..$ FDATE                         : Factor w/ 39 levels "1999/06/28 00:00:00",..: 3 3 1 1 1 1 1 1 1 1 ...
#'   .. ..$ RESOLUTION                    : Factor w/ 1 level "Medium": 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ GNIS_ID                       : Factor w/ 483 levels " ","1005","10173",..: 435 435 457 457 457 457 457 457 457 457 ...
#'   .. ..$ GNIS_NAME                     : Factor w/ 410 levels " ","Alamo Wash",..: 190 190 245 245 245 245 245 245 245 245 ...
#'   .. ..$ LENGTHKM                      : num [1:5658] 9.713 8.276 3.013 0.141 1.091 ...
#'   .. ..$ REACHCODE                     : Factor w/ 4817 levels "14070006000016",..: 1 2 3 3 4 5 6 7 8 9 ...
#'   .. ..$ FLOWDIR                       : Factor w/ 1 level "With Digitized": 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ WBAREACOMI                    : int [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ FTYPE                         : Factor w/ 4 levels "ArtificialPath",..: 4 4 4 4 4 4 4 4 4 4 ...
#'   .. ..$ FCODE                         : int [1:5658] 46006 46003 46003 46006 46006 46006 46006 46006 46006 46006 ...
#'   .. ..$ Shape_Leng                    : num [1:5658] 0.09612 0.0793 0.03101 0.00158 0.01097 ...
#'   .. ..$ StreamLeve                    : int [1:5658] 6 6 5 5 5 5 5 5 5 5 ...
#'   .. ..$ StreamOrde                    : int [1:5658] 3 3 4 4 4 4 4 4 4 4 ...
#'   .. ..$ StreamCalc                    : int [1:5658] 3 3 4 4 4 4 4 4 4 4 ...
#'   .. ..$ FromNode                      : num [1:5658] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ ToNode                        : num [1:5658] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ Hydroseq                      : num [1:5658] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ LevelPathI                    : num [1:5658] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ Pathlength                    : num [1:5658] 1213 1223 1246 1249 1249 ...
#'   .. ..$ TerminalPa                    : num [1:5658] 7.2e+08 7.2e+08 7.2e+08 7.2e+08 7.2e+08 ...
#'   .. ..$ ArbolateSu                    : num [1:5658] 148 110 233 230 224 ...
#'   .. ..$ Divergence                    : int [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ StartFlag                     : int [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ TerminalFl                    : int [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ DnLevel                       : int [1:5658] 6 6 5 5 5 5 5 5 5 5 ...
#'   .. ..$ UpLevelPat                    : num [1:5658] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ UpHydroseq                    : num [1:5658] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ DnLevelPat                    : num [1:5658] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ DnMinorHyd                    : num [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ DnDrainCou                    : int [1:5658] 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ DnHydroseq                    : num [1:5658] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ FromMeas                      : num [1:5658] 0 0 0 95.5 0 ...
#'   .. ..$ ToMeas                        : num [1:5658] 100 100 95.5 100 100 ...
#'   .. ..$ RtnDiv                        : int [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VPUIn                         : int [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VPUOut                        : int [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ AreaSqKM                      : num [1:5658] 22.38 23.714 7.507 0.027 0.56 ...
#'   .. ..$ TotDASqKM                     : num [1:5658] 224 162 317 309 299 ...
#'   .. ..$ DivDASqKM                     : num [1:5658] 224 162 317 309 299 ...
#'   .. ..$ Tidal                         : int [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ TOTMA                         : num [1:5658] 0.2665 0.2521 0.085 0.0042 0.0419 ...
#'   .. ..$ WBAreaType                    : Factor w/ 4 levels " ","LakePond",..: 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ HWNodeSqKM                    : num [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ MAXELEVRAW                    : num [1:5658] -9998 -9998 -9998 -9998 -9998 ...
#'   .. ..$ MINELEVRAW                    : num [1:5658] 125108 152181 148679 153258 153252 ...
#'   .. ..$ MAXELEVSMO                    : num [1:5658] 154303 170223 153258 153400 153525 ...
#'   .. ..$ MINELEVSMO                    : num [1:5658] 125305 154303 148679 153258 153400 ...
#'   .. ..$ SLOPE                         : num [1:5658] 0.02985 0.01924 0.0152 0.01007 0.00115 ...
#'   .. ..$ ELEVFIXED                     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ HWTYPE                        : Factor w/ 3 levels " ","E","H": 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ SLOPELENKM                    : num [1:5658] 9.713 8.276 3.013 0.141 1.091 ...
#'   .. ..$ QA_MA                         : num [1:5658] 2.55 1.9 3.25 3.19 3.1 ...
#'   .. ..$ VA_MA                         : num [1:5658] 1.39 1.246 1.351 1.28 0.991 ...
#'   .. ..$ QC_MA                         : num [1:5658] 2.38 1.9 3.06 2.99 2.9 ...
#'   .. ..$ VC_MA                         : num [1:5658] 1.384 1.246 1.346 1.275 0.988 ...
#'   .. ..$ QE_MA                         : num [1:5658] 2.38 1.9 3.06 2.99 2.9 ...
#'   .. ..$ VE_MA                         : num [1:5658] 1.384 1.246 1.346 1.275 0.988 ...
#'   .. ..$ QA_01                         : num [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VA_01                         : num [1:5658] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QC_01                         : num [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VC_01                         : num [1:5658] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QE_01                         : num [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VE_01                         : num [1:5658] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QA_02                         : num [1:5658] 3.01 2.17 4.26 4.16 4.02 ...
#'   .. ..$ VA_02                         : num [1:5658] 1.49 1.32 1.51 1.43 1.09 ...
#'   .. ..$ QC_02                         : num [1:5658] 11.54 8.68 15.63 15.3 14.86 ...
#'   .. ..$ VC_02                         : num [1:5658] 2.79 2.41 2.78 2.61 1.93 ...
#'   .. ..$ QE_02                         : num [1:5658] 11.54 8.68 15.63 15.3 14.86 ...
#'   .. ..$ VE_02                         : num [1:5658] 2.79 2.41 2.78 2.61 1.93 ...
#'   .. ..$ QA_03                         : num [1:5658] 3.01 2.17 4.26 4.16 4.02 ...
#'   .. ..$ VA_03                         : num [1:5658] 1.49 1.32 1.51 1.43 1.09 ...
#'   .. ..$ QC_03                         : num [1:5658] 14.4 11.2 18.9 18.6 18.1 ...
#'   .. ..$ VC_03                         : num [1:5658] 3.11 2.72 3.04 2.86 2.1 ...
#'   .. ..$ QE_03                         : num [1:5658] 14.4 11.2 18.9 18.6 18.1 ...
#'   .. ..$ VE_03                         : num [1:5658] 3.11 2.72 3.04 2.86 2.1 ...
#'   .. ..$ QA_04                         : num [1:5658] 3.01 2.17 4.26 4.16 4.02 ...
#'   .. ..$ VA_04                         : num [1:5658] 1.49 1.32 1.51 1.43 1.09 ...
#'   .. ..$ QC_04                         : num [1:5658] 14 11.6 17.2 16.9 16.6 ...
#'   .. ..$ VC_04                         : num [1:5658] 3.07 2.76 2.9 2.74 2.02 ...
#'   .. ..$ QE_04                         : num [1:5658] 14 11.6 17.2 16.9 16.6 ...
#'   .. ..$ VE_04                         : num [1:5658] 3.07 2.76 2.9 2.74 2.02 ...
#'   .. ..$ QA_05                         : num [1:5658] 0.708 0.571 0.647 0.647 0.647 0.535 0.535 0.535 0.299 0.299 ...
#'   .. ..$ VA_05                         : num [1:5658] 0.856 0.804 0.751 0.725 0.606 ...
#'   .. ..$ QC_05                         : num [1:5658] 0.708 0.571 0.647 0.647 0.647 0.535 0.535 0.535 0.299 0.299 ...
#'   .. ..$ VC_05                         : num [1:5658] 0.873 0.804 0.764 0.737 0.614 ...
#'   .. ..$ QE_05                         : num [1:5658] 0.708 0.571 0.647 0.647 0.647 0.535 0.535 0.535 0.299 0.299 ...
#'   .. ..$ VE_05                         : num [1:5658] 0.873 0.804 0.764 0.737 0.614 ...
#'   .. ..$ QA_06                         : num [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VA_06                         : num [1:5658] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QC_06                         : num [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VC_06                         : num [1:5658] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QE_06                         : num [1:5658] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VE_06                         : num [1:5658] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QA_07                         : num [1:5658] 5.03 3.85 4.47 4.37 4.23 ...
#'   .. ..$ VA_07                         : num [1:5658] 1.86 1.67 1.54 1.46 1.11 ...
#'   .. ..$ QC_07                         : num [1:5658] 2.42 1.79 2.12 2.06 1.99 ...
#'   .. ..$ VC_07                         : num [1:5658] 1.392 1.218 1.163 1.103 0.865 ...
#'   .. .. [list output truncated]
#'   ..@ lines      :List of 5658
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:139, 1:2] -1271133 -1271152 -1271172 -1271234 -1271239 ...
#'   .. .. .. ..@ ID   : chr "1"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:111, 1:2] -1269802 -1269766 -1269704 -1269697 -1269735 ...
#'   .. .. .. ..@ ID   : chr "2"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:48, 1:2] -1246984 -1247040 -1247116 -1247147 -1247152 ...
#'   .. .. .. ..@ ID   : chr "3"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1246853 -1246927 -1246984 -280760 -280737 ...
#'   .. .. .. ..@ ID   : chr "4"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:20, 1:2] -1246761 -1246758 -1246786 -1246812 -1246847 ...
#'   .. .. .. ..@ ID   : chr "5"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:43, 1:2] -1247289 -1247286 -1247365 -1247385 -1247394 ...
#'   .. .. .. ..@ ID   : chr "6"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:32, 1:2] -1247675 -1247638 -1247630 -1247653 -1247700 ...
#'   .. .. .. ..@ ID   : chr "7"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:70, 1:2] -1248629 -1248604 -1248604 -1248631 -1248661 ...
#'   .. .. .. ..@ ID   : chr "8"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:56, 1:2] -1249675 -1249638 -1249576 -1249568 -1249634 ...
#'   .. .. .. ..@ ID   : chr "9"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1249815 -1249801 -1249720 -1249691 -1249675 ...
#'   .. .. .. ..@ ID   : chr "10"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:61, 1:2] -1249508 -1249518 -1249507 -1249426 -1249419 ...
#'   .. .. .. ..@ ID   : chr "11"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:64, 1:2] -1248302 -1248302 -1248289 -1248295 -1248294 ...
#'   .. .. .. ..@ ID   : chr "12"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:31, 1:2] -1248460 -1248434 -1248450 -1248481 -1248463 ...
#'   .. .. .. ..@ ID   : chr "13"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:53, 1:2] -1248593 -1248586 -1248598 -1248642 -1248655 ...
#'   .. .. .. ..@ ID   : chr "14"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:11, 1:2] -1248388 -1248399 -1248424 -1248444 -1248427 ...
#'   .. .. .. ..@ ID   : chr "15"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:25, 1:2] -1247395 -1247421 -1247484 -1247520 -1247598 ...
#'   .. .. .. ..@ ID   : chr "16"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:57, 1:2] -1247855 -1247846 -1247855 -1247854 -1247834 ...
#'   .. .. .. ..@ ID   : chr "17"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:109, 1:2] -1283581 -1283490 -1283393 -1283274 -1283233 ...
#'   .. .. .. ..@ ID   : chr "18"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:77, 1:2] -1256884 -1256865 -1256888 -1256880 -1256847 ...
#'   .. .. .. ..@ ID   : chr "19"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:46, 1:2] -1250845 -1250953 -1251091 -1251199 -1251275 ...
#'   .. .. .. ..@ ID   : chr "20"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:100, 1:2] -1252069 -1252105 -1252143 -1252153 -1252197 ...
#'   .. .. .. ..@ ID   : chr "21"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:102, 1:2] -1258104 -1258196 -1258250 -1258319 -1258353 ...
#'   .. .. .. ..@ ID   : chr "22"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:46, 1:2] -1259007 -1259100 -1259126 -1259171 -1259288 ...
#'   .. .. .. ..@ ID   : chr "23"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:40, 1:2] -1266352 -1266301 -1266180 -1265963 -1265884 ...
#'   .. .. .. ..@ ID   : chr "24"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:105, 1:2] -1257283 -1257290 -1257269 -1257255 -1257270 ...
#'   .. .. .. ..@ ID   : chr "25"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:88, 1:2] -1257014 -1256983 -1256951 -1256941 -1256902 ...
#'   .. .. .. ..@ ID   : chr "26"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:126, 1:2] -1254905 -1254894 -1254916 -1254939 -1254988 ...
#'   .. .. .. ..@ ID   : chr "27"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:83, 1:2] -1251671 -1251626 -1251585 -1251567 -1251560 ...
#'   .. .. .. ..@ ID   : chr "28"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:11, 1:2] -1250008 -1249970 -1249958 -1249944 -1249886 ...
#'   .. .. .. ..@ ID   : chr "29"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:37, 1:2] -1250766 -1250734 -1250710 -1250691 -1250695 ...
#'   .. .. .. ..@ ID   : chr "30"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:67, 1:2] -1251085 -1251022 -1251005 -1250986 -1250967 ...
#'   .. .. .. ..@ ID   : chr "31"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:21, 1:2] -1249841 -1249863 -1249858 -1249834 -1249802 ...
#'   .. .. .. ..@ ID   : chr "32"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:60, 1:2] -1250525 -1250440 -1250424 -1250392 -1250379 ...
#'   .. .. .. ..@ ID   : chr "33"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:54, 1:2] -1246468 -1246549 -1246637 -1246656 -1246727 ...
#'   .. .. .. ..@ ID   : chr "34"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:8, 1:2] -1247300 -1247349 -1247448 -1247475 -1247524 ...
#'   .. .. .. ..@ ID   : chr "35"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1246920 -1246930 -1246989 -296115 -296102 ...
#'   .. .. .. ..@ ID   : chr "36"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:12, 1:2] -1247441 -1247447 -1247463 -1247500 -1247497 ...
#'   .. .. .. ..@ ID   : chr "37"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:27, 1:2] -1246989 -1246995 -1246994 -1247039 -1247120 ...
#'   .. .. .. ..@ ID   : chr "38"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1246878 -1246904 -1246920 -296214 -296180 ...
#'   .. .. .. ..@ ID   : chr "39"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:61, 1:2] -1244773 -1244794 -1244819 -1244838 -1244852 ...
#'   .. .. .. ..@ ID   : chr "40"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:50, 1:2] -1246615 -1246687 -1246763 -1246754 -1246786 ...
#'   .. .. .. ..@ ID   : chr "41"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:86, 1:2] -1244285 -1244300 -1244298 -1244314 -1244351 ...
#'   .. .. .. ..@ ID   : chr "42"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1244134 -1244189 -1244285 -294754 -294699 ...
#'   .. .. .. ..@ ID   : chr "43"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:77, 1:2] -1240941 -1241002 -1241039 -1241066 -1241107 ...
#'   .. .. .. ..@ ID   : chr "44"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:95, 1:2] -1247922 -1247917 -1247926 -1247964 -1248011 ...
#'   .. .. .. ..@ ID   : chr "45"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:52, 1:2] -1245921 -1245952 -1245993 -1246083 -1246101 ...
#'   .. .. .. ..@ ID   : chr "46"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:36, 1:2] -1244257 -1244566 -1244620 -1244636 -1244656 ...
#'   .. .. .. ..@ ID   : chr "47"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:13, 1:2] -1243536 -1243557 -1243579 -1243682 -1243708 ...
#'   .. .. .. ..@ ID   : chr "48"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:36, 1:2] -1241933 -1241982 -1242045 -1242070 -1242088 ...
#'   .. .. .. ..@ ID   : chr "49"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:25, 1:2] -1244906 -1244915 -1244930 -1244956 -1245016 ...
#'   .. .. .. ..@ ID   : chr "50"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:57, 1:2] -1243742 -1243781 -1243807 -1243821 -1243852 ...
#'   .. .. .. ..@ ID   : chr "51"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:102, 1:2] -1237582 -1237634 -1237742 -1237781 -1237881 ...
#'   .. .. .. ..@ ID   : chr "52"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:9, 1:2] -1241439 -1241465 -1241522 -1241561 -1241629 ...
#'   .. .. .. ..@ ID   : chr "53"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:96, 1:2] -1244634 -1244676 -1244738 -1244811 -1244871 ...
#'   .. .. .. ..@ ID   : chr "54"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:127, 1:2] -1241962 -1241970 -1242004 -1241983 -1241987 ...
#'   .. .. .. ..@ ID   : chr "55"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:76, 1:2] -1243975 -1243988 -1244036 -1244062 -1244076 ...
#'   .. .. .. ..@ ID   : chr "56"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:110, 1:2] -1242839 -1242886 -1242931 -1242972 -1242993 ...
#'   .. .. .. ..@ ID   : chr "57"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:13, 1:2] -1244465 -1244539 -1244595 -1244683 -1244712 ...
#'   .. .. .. ..@ ID   : chr "58"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:31, 1:2] -1244280 -1244325 -1244377 -1244531 -1244560 ...
#'   .. .. .. ..@ ID   : chr "59"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:2, 1:2] -1244414 -1244465 -275329 -275517
#'   .. .. .. ..@ ID   : chr "60"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:9, 1:2] -1243971 -1244003 -1244068 -1244124 -1244232 ...
#'   .. .. .. ..@ ID   : chr "61"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:43, 1:2] -1251811 -1251820 -1251856 -1251867 -1251918 ...
#'   .. .. .. ..@ ID   : chr "62"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:43, 1:2] -1252998 -1253020 -1253010 -1253017 -1253037 ...
#'   .. .. .. ..@ ID   : chr "63"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:96, 1:2] -1253273 -1253237 -1253233 -1253213 -1253235 ...
#'   .. .. .. ..@ ID   : chr "64"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:174, 1:2] -1283511 -1283493 -1283498 -1283452 -1283397 ...
#'   .. .. .. ..@ ID   : chr "65"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:34, 1:2] -1277811 -1277684 -1277675 -1277667 -1277637 ...
#'   .. .. .. ..@ ID   : chr "66"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:23, 1:2] -1270107 -1270147 -1270177 -1270238 -1270262 ...
#'   .. .. .. ..@ ID   : chr "67"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:91, 1:2] -1268396 -1268416 -1268511 -1268670 -1268713 ...
#'   .. .. .. ..@ ID   : chr "68"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:85, 1:2] -1302528 -1302431 -1302346 -1302293 -1302213 ...
#'   .. .. .. ..@ ID   : chr "69"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:94, 1:2] -1305751 -1305758 -1305732 -1305716 -1305663 ...
#'   .. .. .. ..@ ID   : chr "70"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:31, 1:2] -1240034 -1240075 -1240135 -1240211 -1240254 ...
#'   .. .. .. ..@ ID   : chr "71"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:197, 1:2] -1234694 -1234746 -1234825 -1234858 -1234902 ...
#'   .. .. .. ..@ ID   : chr "72"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:33, 1:2] -1241169 -1241186 -1241224 -1241222 -1241243 ...
#'   .. .. .. ..@ ID   : chr "73"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:133, 1:2] -1244981 -1245050 -1245073 -1245119 -1245146 ...
#'   .. .. .. ..@ ID   : chr "74"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:38, 1:2] -1316597 -1316552 -1316525 -1316443 -1316440 ...
#'   .. .. .. ..@ ID   : chr "75"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:98, 1:2] -1315231 -1315123 -1314920 -1314753 -1314715 ...
#'   .. .. .. ..@ ID   : chr "76"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:83, 1:2] -1321181 -1321073 -1321015 -1320870 -1320728 ...
#'   .. .. .. ..@ ID   : chr "77"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:119, 1:2] -1322796 -1322800 -1322789 -1322793 -1322780 ...
#'   .. .. .. ..@ ID   : chr "78"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:66, 1:2] -1317612 -1317610 -1317633 -1317659 -1317699 ...
#'   .. .. .. ..@ ID   : chr "79"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:52, 1:2] -1329004 -1328910 -1328774 -1328725 -1328672 ...
#'   .. .. .. ..@ ID   : chr "80"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:89, 1:2] -1327672 -1327637 -1327535 -1327455 -1327417 ...
#'   .. .. .. ..@ ID   : chr "81"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:144, 1:2] -1327167 -1327128 -1327099 -1327086 -1327067 ...
#'   .. .. .. ..@ ID   : chr "82"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:88, 1:2] -1323388 -1323357 -1323355 -1323357 -1323369 ...
#'   .. .. .. ..@ ID   : chr "83"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:39, 1:2] -1313756 -1313606 -1313525 -1313422 -1313207 ...
#'   .. .. .. ..@ ID   : chr "84"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:26, 1:2] -1101815 -1101857 -1101854 -1101809 -1101803 ...
#'   .. .. .. ..@ ID   : chr "85"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:43, 1:2] -1103743 -1103567 -1103482 -1103377 -1103215 ...
#'   .. .. .. ..@ ID   : chr "86"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:74, 1:2] -1101647 -1101520 -1101507 -1101507 -1101479 ...
#'   .. .. .. ..@ ID   : chr "87"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:17, 1:2] -1102270 -1102263 -1102267 -1102226 -1102210 ...
#'   .. .. .. ..@ ID   : chr "88"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:20, 1:2] -1100861 -1100830 -1100815 -1100869 -1100889 ...
#'   .. .. .. ..@ ID   : chr "89"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:20, 1:2] -1101111 -1101085 -1101091 -1101086 -1101099 ...
#'   .. .. .. ..@ ID   : chr "90"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:29, 1:2] -1103039 -1102984 -1102886 -1102855 -1102737 ...
#'   .. .. .. ..@ ID   : chr "91"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:129, 1:2] -1108044 -1107986 -1107940 -1107947 -1108029 ...
#'   .. .. .. ..@ ID   : chr "92"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:6, 1:2] -1103305 -1103182 -1103181 -1103165 -1103043 ...
#'   .. .. .. ..@ ID   : chr "93"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:96, 1:2] -1098295 -1098301 -1098318 -1098355 -1098367 ...
#'   .. .. .. ..@ ID   : chr "94"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:48, 1:2] -1099541 -1099509 -1099506 -1099479 -1099471 ...
#'   .. .. .. ..@ ID   : chr "95"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:55, 1:2] -1100192 -1100186 -1100094 -1100054 -1100031 ...
#'   .. .. .. ..@ ID   : chr "96"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:24, 1:2] -1099370 -1099353 -1099349 -1099309 -1099319 ...
#'   .. .. .. ..@ ID   : chr "97"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:79, 1:2] -1103574 -1103477 -1103484 -1103479 -1103462 ...
#'   .. .. .. ..@ ID   : chr "98"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:78, 1:2] -1106098 -1106078 -1105781 -1105744 -1105696 ...
#'   .. .. .. ..@ ID   : chr "99"
#'   .. .. [list output truncated]
#'   ..@ bbox       : num [1:2, 1:2] -1529339 -912202 -1094965 -221728
#'   .. ..- attr(*, "dimnames")=List of 2
#'   .. .. ..$ : chr [1:2] "x" "y"
#'   .. .. ..$ : chr [1:2] "min" "max"
#'   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#'   .. .. ..@ projargs: chr "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' }
#' @source example data
"data_GIS_Flow_HI"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_GIS_Flow_LO ####
#' @title NHD+ flow line example data, AZ low gradient
#' 
#' @description A dataset with example GIS flow line data 
#' for use with the getSiteInfo function. 9,981 rows and 836 columns.
#' 
#' @format Formal class 'SpatialLinesDataFrame' [package "sp"] with 4 slots, 162.9 Mb:
#' \describe{
#'   ..@ data       :'data.frame':	9981 obs. of  836 variables:
#'   .. ..$ OBJECTID                      : int [1:9981] 2099370 2099376 2099377 2099378 2099379 2099380 2099381 2099382 2099401 2099402 ...
#'   .. ..$ COMID                         : int [1:9981] 3528375 3528435 3528439 3528457 3528471 3528479 3528487 3528507 3528429 3528425 ...
#'   .. ..$ FDATE                         : Factor w/ 48 levels "1999/06/28 00:00:00",..: 5 5 5 5 5 5 5 5 5 5 ...
#'   .. ..$ RESOLUTION                    : Factor w/ 1 level "Medium": 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ GNIS_ID                       : Factor w/ 655 levels " ","10202","10315",..: 568 564 564 564 564 564 564 564 623 623 ...
#'   .. ..$ GNIS_NAME                     : Factor w/ 549 levels " ","A D Wash",..: 9 256 256 256 256 256 256 256 318 318 ...
#'   .. ..$ LENGTHKM                      : num [1:9981] 1.1 0.957 0.314 2.899 4.541 ...
#'   .. ..$ REACHCODE                     : Factor w/ 7774 levels "14070006000001",..: 1 2 3 3 4 5 6 7 8 9 ...
#'   .. ..$ FLOWDIR                       : Factor w/ 1 level "With Digitized": 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ WBAREACOMI                    : int [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ FTYPE                         : Factor w/ 4 levels "ArtificialPath",..: 4 4 4 4 4 4 4 4 4 4 ...
#'   .. ..$ FCODE                         : int [1:9981] 46003 46006 46006 46006 46006 46006 46006 46006 46006 46006 ...
#'   .. ..$ Shape_Leng                    : num [1:9981] 0.01104 0.00906 0.00323 0.0279 0.04596 ...
#'   .. ..$ StreamLeve                    : int [1:9981] 5 6 6 6 6 6 6 6 5 5 ...
#'   .. ..$ StreamOrde                    : int [1:9981] 3 4 4 4 4 4 4 4 4 4 ...
#'   .. ..$ StreamCalc                    : int [1:9981] 3 4 4 4 4 4 4 4 4 4 ...
#'   .. ..$ FromNode                      : num [1:9981] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ ToNode                        : num [1:9981] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ Hydroseq                      : num [1:9981] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ LevelPathI                    : num [1:9981] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ Pathlength                    : num [1:9981] 1160 1199 1200 1200 1203 ...
#'   .. ..$ TerminalPa                    : num [1:9981] 7.2e+08 7.2e+08 7.2e+08 7.2e+08 7.2e+08 ...
#'   .. ..$ ArbolateSu                    : num [1:9981] 154 321 308 308 277 ...
#'   .. ..$ Divergence                    : int [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ StartFlag                     : int [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ TerminalFl                    : int [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ DnLevel                       : int [1:9981] 5 6 6 6 6 6 6 6 5 5 ...
#'   .. ..$ UpLevelPat                    : num [1:9981] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ UpHydroseq                    : num [1:9981] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ DnLevelPat                    : num [1:9981] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ DnMinorHyd                    : num [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ DnDrainCou                    : int [1:9981] 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ DnHydroseq                    : num [1:9981] 7.6e+08 7.6e+08 7.6e+08 7.6e+08 7.6e+08 ...
#'   .. ..$ FromMeas                      : num [1:9981] 0 0 0 9.83 0 ...
#'   .. ..$ ToMeas                        : num [1:9981] 100 100 9.83 100 100 ...
#'   .. ..$ RtnDiv                        : int [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VPUIn                         : int [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VPUOut                        : int [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ AreaSqKM                      : num [1:9981] 1.237 0.771 0.264 8.532 22.337 ...
#'   .. ..$ TotDASqKM                     : num [1:9981] 347 681 657 657 592 ...
#'   .. ..$ DivDASqKM                     : num [1:9981] 347 681 657 657 592 ...
#'   .. ..$ Tidal                         : int [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ TOTMA                         : num [1:9981] 0.03072 0.05226 0.00669 0.07436 0.12316 ...
#'   .. ..$ WBAreaType                    : Factor w/ 5 levels " ","LakePond",..: 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ HWNodeSqKM                    : num [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ MAXELEVRAW                    : num [1:9981] -9998 -9998 -9998 -9998 -9998 ...
#'   .. ..$ MINELEVRAW                    : num [1:9981] 112870 112869 112856 114454 116275 ...
#'   .. ..$ MAXELEVSMO                    : num [1:9981] 114481 112971 114454 117697 121456 ...
#'   .. ..$ MINELEVSMO                    : num [1:9981] 112870 112971 112971 114454 117697 ...
#'   .. ..$ SLOPE                         : num [1:9981] 0.01465 0.00001 0.04723 0.01119 0.00828 ...
#'   .. ..$ ELEVFIXED                     : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ HWTYPE                        : Factor w/ 3 levels " ","E","H": 1 1 1 1 1 1 1 1 1 1 ...
#'   .. ..$ SLOPELENKM                    : num [1:9981] 1.1 0.957 0.314 2.899 4.541 ...
#'   .. ..$ QA_MA                         : num [1:9981] 3.46 7.14 6.91 6.91 6.29 ...
#'   .. ..$ VA_MA                         : num [1:9981] 1.365 0.697 1.788 1.485 1.405 ...
#'   .. ..$ QC_MA                         : num [1:9981] 3.25 6.79 6.57 6.57 5.97 ...
#'   .. ..$ VC_MA                         : num [1:9981] 1.36 0.695 1.782 1.48 1.4 ...
#'   .. ..$ QE_MA                         : num [1:9981] 3.25 6.79 6.57 6.57 5.97 ...
#'   .. ..$ VE_MA                         : num [1:9981] 1.36 0.695 1.782 1.48 1.4 ...
#'   .. ..$ QA_01                         : num [1:9981] 0.001 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VA_01                         : num [1:9981] 0.322 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QC_01                         : num [1:9981] 0.001 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VC_01                         : num [1:9981] 0.323 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QE_01                         : num [1:9981] 0.001 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VE_01                         : num [1:9981] 0.323 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QA_02                         : num [1:9981] 2.98 8.15 7.98 7.98 7.61 ...
#'   .. ..$ VA_02                         : num [1:9981] 1.284 0.725 1.905 1.578 1.522 ...
#'   .. ..$ QC_02                         : num [1:9981] 11.4 27.6 27.1 27.1 26 ...
#'   .. ..$ VC_02                         : num [1:9981] 2.36 1.12 3.43 2.79 2.69 ...
#'   .. ..$ QE_02                         : num [1:9981] 11.4 27.6 27.1 27.1 26 ...
#'   .. ..$ VE_02                         : num [1:9981] 2.36 1.12 3.43 2.79 2.69 ...
#'   .. ..$ QA_03                         : num [1:9981] 4.67 9.16 8.83 8.83 7.96 ...
#'   .. ..$ VA_03                         : num [1:9981] 1.547 0.752 1.994 1.649 1.551 ...
#'   .. ..$ QC_03                         : num [1:9981] 20.3 34.2 33.3 33.3 30.7 ...
#'   .. ..$ VC_03                         : num [1:9981] 3.09 1.22 3.8 3.08 2.91 ...
#'   .. ..$ QE_03                         : num [1:9981] 20.3 34.2 33.3 33.3 30.7 ...
#'   .. ..$ VE_03                         : num [1:9981] 3.09 1.22 3.8 3.08 2.91 ...
#'   .. ..$ QA_04                         : num [1:9981] 4.67 9.16 8.83 8.83 7.96 ...
#'   .. ..$ VA_04                         : num [1:9981] 1.547 0.752 1.994 1.649 1.551 ...
#'   .. ..$ QC_04                         : num [1:9981] 18.1 26.8 26.3 26.3 24.7 ...
#'   .. ..$ VC_04                         : num [1:9981] 2.93 1.11 3.38 2.75 2.63 ...
#'   .. ..$ QE_04                         : num [1:9981] 18.1 26.8 26.3 26.3 24.7 ...
#'   .. ..$ VE_04                         : num [1:9981] 2.93 1.11 3.38 2.75 2.63 ...
#'   .. ..$ QA_05                         : num [1:9981] 4.67 3.5 3.18 3.17 2.31 ...
#'   .. ..$ VA_05                         : num [1:9981] 1.547 0.575 1.288 1.087 0.952 ...
#'   .. ..$ QC_05                         : num [1:9981] 4.67 3.5 3.18 3.17 2.31 ...
#'   .. ..$ VC_05                         : num [1:9981] 1.582 0.581 1.311 1.105 0.967 ...
#'   .. ..$ QE_05                         : num [1:9981] 4.67 3.5 3.18 3.17 2.31 ...
#'   .. ..$ VE_05                         : num [1:9981] 1.582 0.581 1.311 1.105 0.967 ...
#'   .. ..$ QA_06                         : num [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VA_06                         : num [1:9981] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QC_06                         : num [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VC_06                         : num [1:9981] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QE_06                         : num [1:9981] 0 0 0 0 0 0 0 0 0 0 ...
#'   .. ..$ VE_06                         : num [1:9981] 0.308 0.308 0.308 0.308 0.308 ...
#'   .. ..$ QA_07                         : num [1:9981] 4.67 11.84 11.51 11.51 10.64 ...
#'   .. ..$ VA_07                         : num [1:9981] 1.547 0.816 2.248 1.851 1.758 ...
#'   .. ..$ QC_07                         : num [1:9981] 2.23 6.33 6.14 6.14 5.62 ...
#'   .. ..$ VC_07                         : num [1:9981] 1.168 0.681 1.73 1.439 1.366 ...
#'   .. .. [list output truncated]
#'   ..@ lines      :List of 9981
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:24, 1:2] -1292671 -1292671 -1292691 -1292751 -1292773 ...
#'   .. .. .. ..@ ID   : chr "1"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:9, 1:2] -1277657 -1277566 -1277509 -1277423 -1277384 ...
#'   .. .. .. ..@ ID   : chr "2"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1277490 -1277524 -1277606 -1277660 -1277657 ...
#'   .. .. .. ..@ ID   : chr "3"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:38, 1:2] -1277906 -1277850 -1277864 -1277847 -1277826 ...
#'   .. .. .. ..@ ID   : chr "4"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:64, 1:2] -1275819 -1275820 -1275884 -1275914 -1275927 ...
#'   .. .. .. ..@ ID   : chr "5"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:13, 1:2] -1275603 -1275603 -1275585 -1275577 -1275607 ...
#'   .. .. .. ..@ ID   : chr "6"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:27, 1:2] -1274253 -1274311 -1274359 -1274445 -1274503 ...
#'   .. .. .. ..@ ID   : chr "7"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:44, 1:2] -1273518 -1273516 -1273543 -1273654 -1273686 ...
#'   .. .. .. ..@ ID   : chr "8"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:125, 1:2] -1269013 -1269087 -1269141 -1269206 -1269241 ...
#'   .. .. .. ..@ ID   : chr "9"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:13, 1:2] -1268327 -1268374 -1268483 -1268514 -1268571 ...
#'   .. .. .. ..@ ID   : chr "10"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:68, 1:2] -1265266 -1265344 -1265446 -1265530 -1265566 ...
#'   .. .. .. ..@ ID   : chr "11"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:25, 1:2] -1263905 -1263938 -1263959 -1263969 -1263987 ...
#'   .. .. .. ..@ ID   : chr "12"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:27, 1:2] -1262830 -1263049 -1263078 -1263096 -1263116 ...
#'   .. .. .. ..@ ID   : chr "13"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:78, 1:2] -1259451 -1259560 -1259628 -1259668 -1259681 ...
#'   .. .. .. ..@ ID   : chr "14"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:31, 1:2] -1257934 -1257941 -1258028 -1258065 -1258091 ...
#'   .. .. .. ..@ ID   : chr "15"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:9, 1:2] -1257229 -1257268 -1257354 -1257374 -1257528 ...
#'   .. .. .. ..@ ID   : chr "16"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:6, 1:2] -1256875 -1257015 -1257056 -1257099 -1257188 ...
#'   .. .. .. ..@ ID   : chr "17"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:15, 1:2] -1256323 -1256371 -1256416 -1256455 -1256496 ...
#'   .. .. .. ..@ ID   : chr "18"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:36, 1:2] -1254982 -1255107 -1255137 -1255135 -1255192 ...
#'   .. .. .. ..@ ID   : chr "19"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:8, 1:2] -1254772 -1254859 -1254871 -1254880 -1254915 ...
#'   .. .. .. ..@ ID   : chr "20"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:20, 1:2] -1254096 -1254080 -1254102 -1254135 -1254193 ...
#'   .. .. .. ..@ ID   : chr "21"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:6, 1:2] -1253670 -1253698 -1253783 -1253812 -1253821 ...
#'   .. .. .. ..@ ID   : chr "22"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:16, 1:2] -1253820 -1253789 -1253782 -1253823 -1253867 ...
#'   .. .. .. ..@ ID   : chr "23"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:35, 1:2] -1252127 -1252128 -1252136 -1252170 -1252257 ...
#'   .. .. .. ..@ ID   : chr "24"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:2, 1:2] -1299562 -1299589 -257272 -257302
#'   .. .. .. ..@ ID   : chr "25"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1299589 -1299578 -1299606 -1299623 -1299719 ...
#'   .. .. .. ..@ ID   : chr "26"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1300793 -1300833 -1301020 -1301368 -1301475 ...
#'   .. .. .. ..@ ID   : chr "27"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:4, 1:2] -1301475 -1301529 -1301636 -1301691 -258019 ...
#'   .. .. .. ..@ ID   : chr "28"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1299719 -1299774 -1299883 -1299991 -1300112 ...
#'   .. .. .. ..@ ID   : chr "29"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:6, 1:2] -1300112 -1300151 -1300205 -1300245 -1300391 ...
#'   .. .. .. ..@ ID   : chr "30"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:2, 1:2] -1297696 -1297698 -254242 -254325
#'   .. .. .. ..@ ID   : chr "31"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:4, 1:2] -1297526 -1297500 -1297502 -1297597 -255382 ...
#'   .. .. .. ..@ ID   : chr "32"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:8, 1:2] -1297597 -1297706 -1297949 -1298222 -1298559 ...
#'   .. .. .. ..@ ID   : chr "33"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1299042 -1299068 -1299296 -1299363 -1299411 ...
#'   .. .. .. ..@ ID   : chr "34"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:4, 1:2] -1299411 -1299513 -1299569 -1299562 -256545 ...
#'   .. .. .. ..@ ID   : chr "35"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:7, 1:2] -1297698 -1297699 -1297673 -1297675 -1297611 ...
#'   .. .. .. ..@ ID   : chr "36"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:2, 1:2] -1255623 -1255362 -253694 -253059
#'   .. .. .. ..@ ID   : chr "37"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:19, 1:2] -1255990 -1255927 -1255915 -1255925 -1255956 ...
#'   .. .. .. ..@ ID   : chr "38"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:47, 1:2] -1255940 -1255908 -1255900 -1255923 -1256048 ...
#'   .. .. .. ..@ ID   : chr "39"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:38, 1:2] -1255921 -1255911 -1255894 -1255884 -1255913 ...
#'   .. .. .. ..@ ID   : chr "40"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:23, 1:2] -1255268 -1255343 -1255392 -1255513 -1255648 ...
#'   .. .. .. ..@ ID   : chr "41"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:6, 1:2] -1255300 -1255295 -1255307 -1255277 -1255280 ...
#'   .. .. .. ..@ ID   : chr "42"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:25, 1:2] -1255063 -1255090 -1255131 -1255137 -1255125 ...
#'   .. .. .. ..@ ID   : chr "43"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:8, 1:2] -1255894 -1255944 -1256034 -1256059 -1256088 ...
#'   .. .. .. ..@ ID   : chr "44"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:25, 1:2] -1253467 -1253498 -1253569 -1253611 -1253674 ...
#'   .. .. .. ..@ ID   : chr "45"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:31, 1:2] -1254604 -1254634 -1254641 -1254662 -1254728 ...
#'   .. .. .. ..@ ID   : chr "46"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:53, 1:2] -1250214 -1250267 -1250474 -1250540 -1250569 ...
#'   .. .. .. ..@ ID   : chr "47"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:35, 1:2] -1307098 -1307079 -1307052 -1306963 -1306839 ...
#'   .. .. .. ..@ ID   : chr "48"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1306258 -1306262 -1306266 -264206 -264187 ...
#'   .. .. .. ..@ ID   : chr "49"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:34, 1:2] -1275445 -1275447 -1275511 -1275531 -1275519 ...
#'   .. .. .. ..@ ID   : chr "50"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:41, 1:2] -1272209 -1272216 -1272213 -1272248 -1272243 ...
#'   .. .. .. ..@ ID   : chr "51"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:72, 1:2] -1304596 -1304505 -1304439 -1304312 -1304215 ...
#'   .. .. .. ..@ ID   : chr "52"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1301738 -1301721 -1301708 -1301696 -1301691 ...
#'   .. .. .. ..@ ID   : chr "53"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:35, 1:2] -1299676 -1299830 -1299862 -1299895 -1299915 ...
#'   .. .. .. ..@ ID   : chr "54"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1301461 -1301463 -1301467 -1301471 -1301475 ...
#'   .. .. .. ..@ ID   : chr "55"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1299683 -1299706 -1299768 -257308 -257460 ...
#'   .. .. .. ..@ ID   : chr "56"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:7, 1:2] -1299589 -1299603 -1299623 -1299643 -1299643 ...
#'   .. .. .. ..@ ID   : chr "57"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:6, 1:2] -1299768 -1299759 -1299749 -1299739 -1299729 ...
#'   .. .. .. ..@ ID   : chr "58"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1297591 -1297628 -1297636 -255428 -255522 ...
#'   .. .. .. ..@ ID   : chr "59"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:7, 1:2] -1297636 -1297631 -1297624 -1297617 -1297611 ...
#'   .. .. .. ..@ ID   : chr "60"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:2, 1:2] -1297526 -1297591 -255382 -255428
#'   .. .. .. ..@ ID   : chr "61"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:2, 1:2] -1255200 -1255300 -259712 -259528
#'   .. .. .. ..@ ID   : chr "62"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:34, 1:2] -1253734 -1253762 -1253788 -1253825 -1253856 ...
#'   .. .. .. ..@ ID   : chr "63"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:2, 1:2] -1253717 -1253734 -259958 -259960
#'   .. .. .. ..@ ID   : chr "64"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:4, 1:2] -1253700 -1253708 -1253726 -1253734 -260235 ...
#'   .. .. .. ..@ ID   : chr "65"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:8, 1:2] -1253613 -1253664 -1253665 -1253632 -1253625 ...
#'   .. .. .. ..@ ID   : chr "66"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:23, 1:2] -1252871 -1252923 -1252916 -1252946 -1252978 ...
#'   .. .. .. ..@ ID   : chr "67"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:71, 1:2] -1252961 -1253032 -1253108 -1253237 -1253329 ...
#'   .. .. .. ..@ ID   : chr "68"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:16, 1:2] -1255146 -1255141 -1255156 -1255157 -1255166 ...
#'   .. .. .. ..@ ID   : chr "69"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:135, 1:2] -1262170 -1262204 -1262277 -1262393 -1262440 ...
#'   .. .. .. ..@ ID   : chr "70"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:15, 1:2] -1261898 -1262001 -1262053 -1262080 -1262066 ...
#'   .. .. .. ..@ ID   : chr "71"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:35, 1:2] -1263918 -1264013 -1264279 -1264378 -1264445 ...
#'   .. .. .. ..@ ID   : chr "72"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:27, 1:2] -1273919 -1273944 -1273932 -1273877 -1273846 ...
#'   .. .. .. ..@ ID   : chr "73"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:137, 1:2] -1270277 -1270316 -1270342 -1270397 -1270445 ...
#'   .. .. .. ..@ ID   : chr "74"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:58, 1:2] -1276377 -1276341 -1276315 -1276034 -1275987 ...
#'   .. .. .. ..@ ID   : chr "75"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:40, 1:2] -1276789 -1276801 -1276765 -1276758 -1276658 ...
#'   .. .. .. ..@ ID   : chr "76"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:144, 1:2] -1279034 -1278945 -1278920 -1278902 -1278843 ...
#'   .. .. .. ..@ ID   : chr "77"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:57, 1:2] -1276888 -1276879 -1276887 -1276936 -1276972 ...
#'   .. .. .. ..@ ID   : chr "78"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:39, 1:2] -1262128 -1262144 -1262182 -1262169 -1262175 ...
#'   .. .. .. ..@ ID   : chr "79"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:20, 1:2] -1250010 -1250004 -1250022 -1250065 -1250105 ...
#'   .. .. .. ..@ ID   : chr "80"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:111, 1:2] -1245235 -1245345 -1245383 -1245465 -1245529 ...
#'   .. .. .. ..@ ID   : chr "81"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:13, 1:2] -1249460 -1249527 -1249578 -1249615 -1249735 ...
#'   .. .. .. ..@ ID   : chr "82"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:12, 1:2] -1253141 -1253162 -1253182 -1253207 -1253239 ...
#'   .. .. .. ..@ ID   : chr "83"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:44, 1:2] -1251466 -1251508 -1251589 -1251594 -1251582 ...
#'   .. .. .. ..@ ID   : chr "84"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:32, 1:2] -1255616 -1255691 -1255721 -1255732 -1255749 ...
#'   .. .. .. ..@ ID   : chr "85"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:32, 1:2] -1258384 -1258394 -1258381 -1258382 -1258318 ...
#'   .. .. .. ..@ ID   : chr "86"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:14, 1:2] -1258028 -1258036 -1258068 -1258206 -1258240 ...
#'   .. .. .. ..@ ID   : chr "87"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1258033 -1258019 -1258028 -270082 -270207 ...
#'   .. .. .. ..@ ID   : chr "88"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:40, 1:2] -1256760 -1256826 -1256855 -1256885 -1256896 ...
#'   .. .. .. ..@ ID   : chr "89"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:56, 1:2] -1261907 -1261965 -1261980 -1262089 -1262173 ...
#'   .. .. .. ..@ ID   : chr "90"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:71, 1:2] -1264335 -1264340 -1264353 -1264383 -1264398 ...
#'   .. .. .. ..@ ID   : chr "91"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:63, 1:2] -1266041 -1266121 -1266149 -1266181 -1266282 ...
#'   .. .. .. ..@ ID   : chr "92"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:78, 1:2] -1279839 -1279808 -1279746 -1279725 -1279746 ...
#'   .. .. .. ..@ ID   : chr "93"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:2, 1:2] -1280977 -1280291 -263325 -263047
#'   .. .. .. ..@ ID   : chr "94"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:29, 1:2] -1286176 -1286107 -1286049 -1285928 -1285841 ...
#'   .. .. .. ..@ ID   : chr "95"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:210, 1:2] -1285127 -1285122 -1285134 -1285172 -1285183 ...
#'   .. .. .. ..@ ID   : chr "96"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:3, 1:2] -1299056 -1299188 -1299331 -256545 -256534 ...
#'   .. .. .. ..@ ID   : chr "97"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:5, 1:2] -1299042 -1299043 -1299047 -1299051 -1299056 ...
#'   .. .. .. ..@ ID   : chr "98"
#'   .. ..$ :Formal class 'Lines' [package "sp"] with 2 slots
#'   .. .. .. ..@ Lines:List of 1
#'   .. .. .. .. ..$ :Formal class 'Line' [package "sp"] with 1 slot
#'   .. .. .. .. .. .. ..@ coords: num [1:6, 1:2] -1299331 -1299343 -1299362 -1299381 -1299401 ...
#'   .. .. .. ..@ ID   : chr "99"
#'   .. .. [list output truncated]
#'   ..@ bbox       : num [1:2, 1:2] -1675797 -930219 -1096244 -210619
#'   .. ..- attr(*, "dimnames")=List of 2
#'   .. .. ..$ : chr [1:2] "x" "y"
#'   .. .. ..$ : chr [1:2] "min" "max"
#'   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
#'   .. .. ..@ projargs: chr "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' }
#' @source example data
"data_GIS_Flow_LO"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_ReachMod ####
#' @title Reach modified status example data
#' 
#' @description A dataset with example reach modified status for use with the getSiteInfo function.
#' 
#' @format A data frame with 1,419 rows and43 variables:
#' \describe{
#'           \item{COMID}{NHDplus COMID}
#'           \item{ElevCategory}{Elevation Category}
#'           \item{ReachModStatus}{Reach modified flow status}
#'           \item{ModReason}{Reach modified flow reason}
#' }
#' @source example data
"data_ReachMod"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_SampSummary ####
#' @title Sample summary example data
#' 
#' @description A dataset with example sample summary for use with the getSiteInfo function.
#' 
#' @format A data frame with 3,646 rows and 9 variables:
#' \describe{
#'           \item{StationID_Master}{Station ID}
#'           \item{CollDate}{Station ID}
#'           \item{Station_Date}{combined StationID and Date}
#'           \item{ChemSampleID}{SampleID, Chem}
#'           \item{PhabSampID}{SampleID, Phab}
#'           \item{BMI.Metrics.SampID}{SampleID, BMI Metrics}
#'           \item{Algae.Metrics.SampID}{SampleID, Algae Metrics}
#'           \item{ElevCategory}{Elevation Category}           
#'           \item{SampleYr}{Sample Year}
#' }
#' @source example data
"data_SampSummary"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_Sites ####
#' @title Sites example data
#' 
#' @description A dataset with example site information for use with the getSiteInfo function.
#' 
#' @format A data frame with 2,233 observations on the following 23 variables:
#' \describe{
#'           \item{StationRID}{a numeric vector}
#'           \item{StationType}{a vector}
#'           \item{clust.hi}{a vector}
#'           \item{clust.lo}{a vector}
#'           \item{HUC08}{8 digit Hydrologic Unit Code}
#'           \item{FLAG}{Flag}
#'           \item{InvRegCOMMENT}{Flag}
#'           \item{InvRegFINAL}{Flag}
#'           \item{UTM_EAST}{UTM Easting}
#'           \item{UTM_NORTH}{UTM Northing}
#'           \item{UTM_ZONE}{UTM Zone \code{11N} \code{12N}}
#'           \item{Elev}{Elevation}
#'           \item{ReferenceStatus}{Reference Status \code{reference} \code{Reference}}
#'           \item{Year}{Year}
#'           \item{StationID_Master}{StationID Master Code}
#'           \item{FinalLatitude}{Final Latitude}
#'           \item{FinalLongitude}{Final Longitude}
#'           \item{WaterbodyName}{Waterbody Name}
#'           \item{GIS_County}{County; derived from GIS }
#'           \item{CARefSite_2017}{a character vector}
#'           \item{COMID_NHD2}{a numeric vector}
#'           \item{ElevCategory}{Elevation Category; Hi or Lo, break point at 5,000 ft}
#'           \item{FlowRegime}{a vector}
#' }
#' @source example data
"data_Sites"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_SSD ####
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
# data_SSD_generator ####
#' @title SSD example data (generator file)
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
# data_SSD_permethrin ####
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
# df.sites.map ----
#' @title df.sites.map
#' 
#' @description example data
#' 
#' @format A data frame with 1011 observations on the following 9 variables.
#' \describe{
#'   \item{\code{StationID_Master}}{a factor with levels \code{402BA0031} \code{402BA0079} \code{402BA0095} \code{402BA0143} \code{402BA0335} \code{402M00002} \code{402M00005} \code{402M00009} \code{402MJCMHS} \code{402MTCUNF} \code{402PS0048} \code{402SNPMCR} \code{402WE0536} \code{402WE0803} \code{402WER001} \code{403BA0027} \code{403BA0047} \code{403BA0068} \code{403BA0136} \code{403BA0191} \code{403CE0156} \code{403CE0188} \code{403FCA022} \code{403LNCASC} \code{403M01505} \code{403M01506} \code{403M05757} \code{403M05758} \code{403M05771} \code{403PRCAFF} \code{403PRCAGH} \code{403PRCSFD} \code{403R4S117} \code{403R4S193} \code{403R4S211} \code{403S00064} \code{403S00640} \code{403S00772} \code{403S00831} \code{403S00875} \code{403S00960} \code{403S01136} \code{403S01163} \code{403S01195} \code{403S01272} \code{403S01536} \code{403S01707} \code{403S01728} \code{403S01784} \code{403S01883} \code{403S02363} \code{403S02764} \code{403S03320} \code{403S03643} \code{403S03832} \code{403S04600} \code{403S04868} \code{403S05247} \code{403S05291} \code{403S06139} \code{403S06283} \code{403S06315} \code{403S06660} \code{403S06904} \code{403S07024} \code{403S07227} \code{403S11084} \code{403S14156} \code{403S15608} \code{403S16332} \code{403S16493} \code{403S16978} \code{403S18093} \code{403S34646} \code{403S39062} \code{403SCVARC} \code{403SCVARD} \code{403SED076} \code{403SPCNWS} \code{403STC004} \code{403STC008} \code{403STC016} \code{403STC019} \code{403STC021} \code{403STC022} \code{403STC024} \code{403STC025} \code{403STC026} \code{403STC027} \code{403STC028} \code{403STC029} \code{403STC030} \code{403STC064} \code{403STC065} \code{403STC066} \code{403STC069} \code{403STC070} \code{403STC071} \code{403STC076} \code{403STC082} \code{403STC083} \code{403STC085} \code{403STC086} \code{403STC090} \code{403STC093} \code{403STCBQT} \code{403STCEST} \code{403STCNRB} \code{403STCPRU} \code{403STCSSP} \code{403STCSTP} \code{403WE0501} \code{403WE0534} \code{403WE0535} \code{403WE0540} \code{403WE0558} \code{403WE0560} \code{403WE0682} \code{403WE0683} \code{403WE0746} \code{403WE0795} \code{403WE0809} \code{403WE0891} \code{403WE0905} \code{403WE1021} \code{403WE1027} \code{404BA0084} \code{404BA0104} \code{404BA0616} \code{404BA0808} \code{404BA0852} \code{404BA0964} \code{404BA1144} \code{404BA1166} \code{404LVCALC} \code{404M04532} \code{404M07349} \code{404R4S015} \code{404S00808} \code{404S01128} \code{404S02920} \code{404S03048} \code{404S05992} \code{404S06456} \code{404S08040} \code{404S08616} \code{404S08846} \code{404S11406} \code{404S11880} \code{404S13160} \code{404S13416} \code{404S13672} \code{404S14952} \code{404S16168} \code{404S16232} \code{404S16516} \code{404S17016} \code{404S17266} \code{404S17664} \code{404S18250} \code{404S18666} \code{404S22464} \code{404S23297} \code{404S24066} \code{404S25298} \code{404S25668} \code{404S26670} \code{404S26868} \code{404S27470} \code{404S28068} \code{404S28270} \code{404S31468} \code{404S32468} \code{404S33670} \code{404S34120} \code{404S34230} \code{404S35270} \code{404S35418} \code{404S37670} \code{404S44210} \code{404S44532} \code{404S44642} \code{404S45622} \code{404S45745} \code{404S48200} \code{404SMB001} \code{404SMB011} \code{404SMB035} \code{404SMB043} \code{404SMB075} \code{404SMB41A} \code{404VCLSCU} \code{405BH2Axx} \code{405BH2Bxx} \code{405BH3Axx} \code{405BH3Bxx} \code{405BH3Cxx} \code{405BRCAMS} \code{405BRCASG} \code{405BRCSGR} \code{405CE0280} \code{405M10598} \code{405PS0030} \code{405PS0036} \code{405SGB003} \code{405SGB010} \code{405SGB011} \code{405SGB015} \code{405SGB022} \code{405SGB025} \code{405SGB027} \code{405SGEACC} \code{405SGEASC} \code{405SGRAAG} \code{405SMB048} \code{405WER318} \code{407CE0092} \code{407CE0220} \code{407CE0668} \code{407M01516} \code{408ACCR3x} \code{408BA0036} \code{408BA0268} \code{408BA0580} \code{408BA0660} \code{408BA0836} \code{408BA0916} \code{408BH5Axx} \code{408BH5Bxx} \code{408BH5Cxx} \code{408BH6Axx} \code{408BH6Bxx} \code{408BH6Cxx} \code{408CAL004} \code{408CAL005} \code{408CAL007} \code{408CAL008} \code{408CAL011} \code{408CAL012} \code{408CAL013} \code{408CALBWC} \code{408CGCS04} \code{408CGCS06} \code{408CGCS12} \code{408CGCS13} \code{408M03005} \code{408M03019} \code{408PS0032} \code{408WE0654} \code{408WE1039} \code{412ARSNPD} \code{412BH1Axx} \code{412BH1Bxx} \code{412BH1Cxx} \code{412BH4Axx} \code{412BH4Bxx} \code{412BH4Cxx} \code{412CE0104} \code{412CE0232} \code{412CE0616} \code{412CE0732} \code{412LAR007} \code{412LAR008} \code{412LAR013} \code{412LAR015} \code{412LAR019} \code{412LAR020} \code{412LAR023} \code{412LAR031} \code{412LARRHO} \code{412LARSCO} \code{412M08597} \code{412M08599} \code{412M08602} \code{412PS0020} \code{412PS0040} \code{412PS0052} \code{412WE0552} \code{412WE0563} \code{412WE0896} \code{481S05856} \code{801BNC530} \code{801BRC184} \code{801CCCCTT} \code{801CCWFAC} \code{801CE0152} \code{801CJW027} \code{801CJW041} \code{801CTCNHL} \code{801CYC114} \code{801CYC398} \code{801DRC025} \code{801EEWADD} \code{801ETC226} \code{801FC1089} \code{801FDCCCR} \code{801HBC050} \code{801HNC203} \code{801LCMFAS} \code{801LYC062} \code{801M12611} \code{801M12625} \code{801M15376} \code{801M16861} \code{801MFC100} \code{801MHC219} \code{801MIC034} \code{801MIC042} \code{801MIC272} \code{801MIC370} \code{801MLC057} \code{801MLC069} \code{801NLC105} \code{801PCW048} \code{801PCW171} \code{801PFB019} \code{801PLC362} \code{801PLC469} \code{801PNCEHL} \code{801PS0019} \code{801RB8167} \code{801RB8197} \code{801RB8207} \code{801RB8254} \code{801RB8262} \code{801RB8277} \code{801RB8289} \code{801RB8312} \code{801RB8327} \code{801RB8356} \code{801RB8361} \code{801RB8380} \code{801RB8478} \code{801RB8494} \code{801RB8501} \code{801RB8511} \code{801RB8559} \code{801RB8566} \code{801RB8567} \code{801RB8572} \code{801RB8594} \code{801RB8598} \code{801RB8613} \code{801S00791} \code{801S00903} \code{801S01367} \code{801S01523} \code{801S01559} \code{801S01671} \code{801S01783} \code{801S01805} \code{801S02123} \code{801S02464} \code{801S02567} \code{801S02749} \code{801S02947} \code{801S03111} \code{801S03488} \code{801S03971} \code{801S04078} \code{801S04471} \code{801S05127} \code{801S05383} \code{801S06231} \code{801S06679} \code{801S07485} \code{801S08183} \code{801S08727} \code{801S10259} \code{801S16169} \code{801S19286} \code{801S19399} \code{801S19486} \code{801SANT1x} \code{801SAR110} \code{801SAR151} \code{801SAR168} \code{801SAR351} \code{801SBCATC} \code{801SCASxx} \code{801SCLCRx} \code{801SDC180} \code{801SDC418} \code{801SDC504} \code{801SJR159} \code{801SNP001} \code{801SNP002} \code{801STC532} \code{801STW055} \code{801STW085} \code{801STW258} \code{801WCC247} \code{801WCC446} \code{801WCCAHS} \code{801WE0550} \code{801WE0669} \code{801WE0674} \code{801WE0806} \code{801WE0895} \code{801WE0989} \code{801WE1008} \code{801WE1020} \code{801WE1043} \code{801WE1127} \code{801XXX112} \code{801XXX305} \code{802FMCAIP} \code{802NJR147} \code{802NJR160} \code{802S03234} \code{802S09698} \code{802S10146} \code{802S11394} \code{802S25288} \code{802S25949} \code{802S33361} \code{802S33561} \code{802S37697} \code{802S45233} \code{802SJN851} \code{802SJR116} \code{802SJR587} \code{802SWC270} \code{802SWC419} \code{802SWC535} \code{802WE0658} \code{845CTC480} \code{845PS0011} \code{845RB8633} \code{901ACCCRx} \code{901ACPPDx} \code{901ATCAAS} \code{901ATCDOS} \code{901ATCTCx} \code{901BCCSRT} \code{901BELOLV} \code{901M14118} \code{901M14124} \code{901NP9HJC} \code{901NP9LCC} \code{901PS0057} \code{901S00313} \code{901S00469} \code{901S00531} \code{901S00997} \code{901S01705} \code{901S01811} \code{901S01849} \code{901S02702} \code{901S02873} \code{901S04309} \code{901S04409} \code{901S04565} \code{901S06030} \code{901S06798} \code{901S06851} \code{901S06969} \code{901S11685} \code{901S12942} \code{901S39498} \code{901S45253} \code{901SCCA74} \code{901SJC74x} \code{901SJMS1x} \code{901SJOF1x} \code{901SJSJC9} \code{901SJSMT2} \code{901SJSMT3} \code{901SMCSMR} \code{901TCSMP1} \code{902DLCDLM} \code{902M18864} \code{902MCGSxx} \code{902RCBWGR} \code{902RCWGRx} \code{902S00117} \code{902S00565} \code{902S01097} \code{902S02293} \code{902S02357} \code{902S05173} \code{902SCDLRx} \code{902SCSCRx} \code{902SMAS1x} \code{902SMRWGR} \code{902SMSND3} \code{902SMSTN1} \code{902SSMR05} \code{902TCI15x} \code{902WE0888} \code{903ACPCT1} \code{903CVPCT} \code{903GIR2xx} \code{903GJL000} \code{903M20124} \code{903M20153} \code{903NP9LWF} \code{903NP9SLR} \code{903NP9UAC} \code{903S06113} \code{903SLGRD2} \code{903SLKYS3} \code{903SLMSA2} \code{903SLRRFR} \code{903SLSLR3} \code{903SLSLR6} \code{903SLWVR1} \code{903WE0798} \code{903WE0900} \code{904AHC003} \code{904AHC004} \code{904CBAHC6} \code{904CBBVR4} \code{904CBESC5} \code{904CBESC6} \code{904CBESC8} \code{904CBSAM5} \code{904ECHRBx} \code{904ENCGVR} \code{904M21713} \code{904PS0034} \code{904S00537} \code{904S02201} \code{904S02585} \code{904S08089} \code{904S12185} \code{904WE1125} \code{904WE1131} \code{905BCC} \code{905BCN1xx} \code{905BMCCGx} \code{905CE0512} \code{905DGCC1x} \code{905DGSY1x} \code{905DGUT1x} \code{905KCCSDx} \code{905M21721} \code{905PS0026} \code{905S01953} \code{905S02561} \code{905S15201} \code{905SDBDN9} \code{905SDGVC2} \code{905SDYSA7} \code{905WE0679} \code{905WE1018} \code{906CCECRx} \code{906LPLPC4} \code{906LPRSC4} \code{906LPTEC3} \code{906M23318} \code{906S02246} \code{907BCT} \code{907CCCR02} \code{907CONECR} \code{907LCCHW8} \code{907S00577} \code{907S01418} \code{907S01434} \code{907S01610} \code{907S02774} \code{907S03210} \code{907S03786} \code{907S05514} \code{907S46499} \code{907SDALV3} \code{907SDB047} \code{907SDBOC2} \code{907SDCHC3} \code{907SDFRC2} \code{907SDR1xx} \code{907SDRMTx} \code{907SDRS2x} \code{907SDSDR8} \code{907SDSDR9} \code{907SDSVC3} \code{909CCCSPx} \code{909JPCH79} \code{909JQCASR} \code{909S00282} \code{909SHAR02} \code{909SLAW02} \code{909SSWR01} \code{909SSWR03} \code{909SSWR08} \code{909SWR94x} \code{909WE0662} \code{909WE0780} \code{909WE1014} \code{910DZRA03} \code{910OTJMC4} \code{910OTJMC5} \code{910S06570} \code{910S14762} \code{911CCH80x} \code{911LAP} \code{911MCCBML} \code{911NCPCR2} \code{911NCPCRx} \code{911PCH80x} \code{911S00538} \code{911S00858} \code{911S01142} \code{911S01818} \code{911S02058} \code{911S03354} \code{911S04086} \code{911S12262} \code{911TCCTCx} \code{911TJIND2} \code{911TJKC1x} \code{911TJKTC5} \code{911TJLAP4} \code{911TJLCC2} \code{911TJNPC2} \code{911TJPC2x} \code{911TJPVC1} \code{911TJWIL3} \code{911TTJR01} \code{AHCMLS} \code{BVCTWAS1} \code{CCNF54} \code{ESCMLS} \code{LACTWAS1} \code{LALT500} \code{LALT501} \code{LALT503} \code{LPCMLS} \code{LPCTWAS1} \code{LPCTWAS2} \code{MBTWAS1} \code{MBTWAS2} \code{ME-CC} \code{ORTWAS1} \code{PC1} \code{RBCI15} \code{RBCWGR} \code{REF-TCAS} \code{REFBC} \code{REFBCC} \code{REFCWC} \code{REFDC} \code{REFKC2} \code{REFKCR} \code{REFPC} \code{REFPC2} \code{REFSWGV} \code{REFSYC} \code{REFTC} \code{SDCMLS} \code{SDCTWAS1} \code{SDCTWAS2} \code{SDRMLS} \code{SDRTWAS1} \code{SDRTWAS2} \code{SDRTWAS3} \code{SGUR010} \code{SGUT501} \code{SGUT502} \code{SGUT503} \code{SGUT504} \code{SGUT505} \code{SLRMLS} \code{SLRTWAS1} \code{SLRTWAS2} \code{SMC00080} \code{SMC00096} \code{SMC00105} \code{SMC00153} \code{SMC00206} \code{SMC00208} \code{SMC00236} \code{SMC00271} \code{SMC00318} \code{SMC00345} \code{SMC00428} \code{SMC00436} \code{SMC00440} \code{SMC00457} \code{SMC00464} \code{SMC00479} \code{SMC00480} \code{SMC00520} \code{SMC00574} \code{SMC00665} \code{SMC00670} \code{SMC00684} \code{SMC00693} \code{SMC00702} \code{SMC00710} \code{SMC00729} \code{SMC00756} \code{SMC00766} \code{SMC00827} \code{SMC00830} \code{SMC00857} \code{SMC00873} \code{SMC00899} \code{SMC00911} \code{SMC00921} \code{SMC00924} \code{SMC00926} \code{SMC00957} \code{SMC00958} \code{SMC00963} \code{SMC01004} \code{SMC01013} \code{SMC01040} \code{SMC01046} \code{SMC01049} \code{SMC01096} \code{SMC01151} \code{SMC01155} \code{SMC01158} \code{SMC01161} \code{SMC01164} \code{SMC01172} \code{SMC01174} \code{SMC01196} \code{SMC01201} \code{SMC01208} \code{SMC01215} \code{SMC01257} \code{SMC01258} \code{SMC01278} \code{SMC01320} \code{SMC01341} \code{SMC01372} \code{SMC01384} \code{SMC01413} \code{SMC01424} \code{SMC01452} \code{SMC01464} \code{SMC01504} \code{SMC01512} \code{SMC01544} \code{SMC01550} \code{SMC01555} \code{SMC01567} \code{SMC01606} \code{SMC01640} \code{SMC01656} \code{SMC01676} \code{SMC01684} \code{SMC01689} \code{SMC01692} \code{SMC01694} \code{SMC01716} \code{SMC01717} \code{SMC01726} \code{SMC01748} \code{SMC01808} \code{SMC01824} \code{SMC01860} \code{SMC01881} \code{SMC01902} \code{SMC01909} \code{SMC01923} \code{SMC01934} \code{SMC01960} \code{SMC01962} \code{SMC01972} \code{SMC01979} \code{SMC01982} \code{SMC01987} \code{SMC01990} \code{SMC02006} \code{SMC02028} \code{SMC02075} \code{SMC02088} \code{SMC02092} \code{SMC02127} \code{SMC02152} \code{SMC02206} \code{SMC02228} \code{SMC02232} \code{SMC02270} \code{SMC02284} \code{SMC02302} \code{SMC02417} \code{SMC02436} \code{SMC02452} \code{SMC02457} \code{SMC02494} \code{SMC02536} \code{SMC02548} \code{SMC02563} \code{SMC02568} \code{SMC02591} \code{SMC02622} \code{SMC02644} \code{SMC02656} \code{SMC02680} \code{SMC02712} \code{SMC02718} \code{SMC02804} \code{SMC02884} \code{SMC02888} \code{SMC02905} \code{SMC02933} \code{SMC02976} \code{SMC02984} \code{SMC02988} \code{SMC02996} \code{SMC03011} \code{SMC03110} \code{SMC03216} \code{SMC03222} \code{SMC03268} \code{SMC03280} \code{SMC03304} \code{SMC03390} \code{SMC03401} \code{SMC03438} \code{SMC03510} \code{SMC03523} \code{SMC03646} \code{SMC03737} \code{SMC03929} \code{SMC03944} \code{SMC03984} \code{SMC03988} \code{SMC04000} \code{SMC04008} \code{SMC04047} \code{SMC04054} \code{SMC04121} \code{SMC04132} \code{SMC04134} \code{SMC04175} \code{SMC04239} \code{SMC04264} \code{SMC04294} \code{SMC04308} \code{SMC04383} \code{SMC04399} \code{SMC04426} \code{SMC04432} \code{SMC04441} \code{SMC04524} \code{SMC04532} \code{SMC04600} \code{SMC04661} \code{SMC04670} \code{SMC04682} \code{SMC04748} \code{SMC04749} \code{SMC04750} \code{SMC04756} \code{SMC04795} \code{SMC04806} \code{SMC04880} \code{SMC04932} \code{SMC04934} \code{SMC04956} \code{SMC04972} \code{SMC05017} \code{SMC05020} \code{SMC05109} \code{SMC05146} \code{SMC05165} \code{SMC05199} \code{SMC05230} \code{SMC05296} \code{SMC05332} \code{SMC05379} \code{SMC05402} \code{SMC05407} \code{SMC05423} \code{SMC05524} \code{SMC05567} \code{SMC05640} \code{SMC05694} \code{SMC05702} \code{SMC05759} \code{SMC05764} \code{SMC05848} \code{SMC05902} \code{SMC05956} \code{SMC05968} \code{SMC06019} \code{SMC06036} \code{SMC06044} \code{SMC06079} \code{SMC06188} \code{SMC06216} \code{SMC06252} \code{SMC06288} \code{SMC06298} \code{SMC06302} \code{SMC06356} \code{SMC06458} \code{SMC06467} \code{SMC06496} \code{SMC06612} \code{SMC06653} \code{SMC06714} \code{SMC06740} \code{SMC06794} \code{SMC06863} \code{SMC06904} \code{SMC06918} \code{SMC06926} \code{SMC07085} \code{SMC07126} \code{SMC07128} \code{SMC07828} \code{SMC08068} \code{SMC08094} \code{SMC08150} \code{SMC08157} \code{SMC08335} \code{SMC08414} \code{SMC08426} \code{SMC08540} \code{SMC08655} \code{SMC08660} \code{SMC08766} \code{SMC08845} \code{SMC09091} \code{SMC09118} \code{SMC09162} \code{SMC09174} \code{SMC09286} \code{SMC09325} \code{SMC09534} \code{SMC09564} \code{SMC10189} \code{SMC10198} \code{SMC10685} \code{SMC10756} \code{SMC11181} \code{SMC11343} \code{SMC11384} \code{SMC11581} \code{SMC11593} \code{SMC11727} \code{SMC12246} \code{SMC12814} \code{SMC12862} \code{SMC13062} \code{SMC13076} \code{SMC13187} \code{SMC13214} \code{SMC13391} \code{SMC13402} \code{SMC13599} \code{SMC13630} \code{SMC14099} \code{SMC14211} \code{SMC15464} \code{SMC15677} \code{SMC15678} \code{SMC16045} \code{SMC16169} \code{SMC16266} \code{SMC16446} \code{SMC16832} \code{SMC16892} \code{SMC16980} \code{SMC17056} \code{SMC17378} \code{SMC17432} \code{SMC17692} \code{SMC17918} \code{SMC18046} \code{SMC18116} \code{SMC18169} \code{SMC18545} \code{SMC18656} \code{SMC19228} \code{SMC19466} \code{SMC19552} \code{SMC19669} \code{SMC19697} \code{SMC19809} \code{SMC19945} \code{SMC20032} \code{SMC20092} \code{SMC20497} \code{SMC20994} \code{SMC21069} \code{SMC21371} \code{SMC21382} \code{SMC21796} \code{SMC21822} \code{SMC21921} \code{SMC22521} \code{SMC23495} \code{SMC24222} \code{SMC24921} \code{SMC26288} \code{SMC26694} \code{SMC29064} \code{SMC32718} \code{SMC32897} \code{SMC33179} \code{SMC34888} \code{SMC35697} \code{SMC35837} \code{SMC37632} \code{SMC40887} \code{SMRMLS2} \code{SMRSC} \code{SMRWGR} \code{SMTWAS1b} \code{SRTWAS1} \code{TCMLS} \code{TJRTWAS1} \code{VENTURA13} \code{VENTURA14}}
#'     \item{\code{COMID_NHD2}}{a numeric vector}
#'     \item{\code{WaterbodyName}}{a factor with levels \code{Calleguas watershed} \code{CentralSanDiego watershed} \code{LosAngeles watershed} \code{LowerSantaAna watershed} \code{MiddleSantaAna watershed} \code{MissionBaySanDiego watershed} \code{NorthernSanDiego watershed} \code{SanGabriel watershed} \code{SanJacinto watershed} \code{SanJuan watershed} \code{SantaClara watershed} \code{SantaMonica watershed} \code{SantaMonicaBay watershed} \code{SouthernSanDiego watershed} \code{UpperSantaAna watershed} \code{Ventura watershed}}
#'     \item{\code{GIS_County}}{a logical vector}
#'     \item{\code{FinalLatitude}}{a numeric vector}
#'     \item{\code{FinalLongitude}}{a numeric vector}
#'     \item{\code{CARefSite_2017}}{a numeric vector}
#'     \item{\code{ElevCategory}}{a logical vector}
#'     \item{\code{clust}}{a numeric vector}
#'   }
#' @source example data
"df.sites.map"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
