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
# data_CoOccur ####
#' @title Co-Occurence example data
#' 
#' @description A dataset with example biological, chemical, habitat, and geo-physical parameters.
#' 
#' @format A data frame with 2,769 rows and 739 variables:
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{SWAMP_Station_Code}}{a factor with levels }
#'           \item{\code{CanonicalStationID}}{a factor with levels }
#'           \item{\code{Stream_Name}}{a factor with levels }
#'           \item{\code{SampleDate}}{a factor with levels }
#'           \item{\code{County}}{a factor with levels }
#'           \item{\code{Latitude}}{a numeric vector}
#'           \item{\code{Longitude}}{a numeric vector}
#'           \item{\code{CSCI}}{a numeric vector}
#'           \item{\code{Group}}{a numeric vector}
#'           \item{\code{CollDate}}{a factor with levels }
#'           \item{\code{Acenaphthene_ng_g}}{a numeric vector}
#'           \item{\samp{Acenaphthene_uf__L}}{a numeric vector}
#'           \item{\code{Acenaphthylene_ng_g}}{a numeric vector}
#'           \item{\samp{Acenaphthylene_uf__L}}{a numeric vector}
#'           \item{\code{AFDM_Algae_Particulate_g_m2}}{a numeric vector}
#'           \item{\samp{Ag_f__L}}{a numeric vector}
#'           \item{\samp{Ag_uf__L}}{a numeric vector}
#'           \item{\code{Ag_uf_mg_kg}}{a numeric vector}
#'           \item{\samp{Al_f__L}}{a numeric vector}
#'           \item{\samp{Al_uf__L}}{a numeric vector}
#'           \item{\code{Al_uf_mg_kg}}{a numeric vector}
#'           \item{\code{Aldrin_ng_g}}{a numeric vector}
#'           \item{\code{Aldrin_ppb}}{a numeric vector}
#'           \item{\samp{Aldrin_uf__L}}{a numeric vector}
#'           \item{\code{Alkalinity_mg_L}}{a numeric vector}
#'           \item{\code{AlkalinityCaCO3_f_mg_L}}{a numeric vector}
#'           \item{\code{AlkalinityCaCO3_uf_mg_L}}{a numeric vector}
#'           \item{\code{Allethrin_ng_g}}{a numeric vector}
#'           \item{\samp{Allethrin_uf__L}}{a numeric vector}
#'           \item{\samp{Ametryn_uf__L}}{a numeric vector}
#'           \item{\code{Ametryne_ppb}}{a logical vector}
#'           \item{\samp{AnatoxinA_Particulate__L}}{a numeric vector}
#'           \item{\samp{AnatoxinA_uf__L}}{a numeric vector}
#'           \item{\samp{ANC_uf_q_L}}{a numeric vector}
#'           \item{\samp{Anthracene_uf__L}}{a numeric vector}
#'           \item{\code{Anthracene_uf_ng_g}}{a numeric vector}
#'           \item{\code{As_f_mg_L}}{a numeric vector}
#'           \item{\code{As_uf_mg_kg}}{a numeric vector}
#'           \item{\code{As_uf_mg_L}}{a numeric vector}
#'           \item{\code{Aspon_ppb}}{a logical vector}
#'           \item{\samp{Aspon_uf__L}}{a numeric vector}
#'           \item{\code{Atraton_ppb}}{a numeric vector}
#'           \item{\samp{Atraton_uf__L}}{a numeric vector}
#'           \item{\code{Atrazine_ppb}}{a numeric vector}
#'           \item{\samp{Atrazine_uf__L}}{a numeric vector}
#'           \item{\samp{Azinphos.ethyl_uf__L}}{a numeric vector}
#'           \item{\samp{Azinphos.methyl_uf__L}}{a numeric vector}
#'           \item{\code{AzinphosEthyl_ppb}}{a logical vector}
#'           \item{\code{AzinphosMethyl_ppb}}{a logical vector}
#'           \item{\code{B_uf_mg_L}}{a numeric vector}
#'           \item{\code{Ba_uf_mg_kg}}{a numeric vector}
#'           \item{\code{Be_uf_mg_kg}}{a numeric vector}
#'           \item{\code{Benz.a.anthracene_ng_g}}{a numeric vector}
#'           \item{\samp{Benz.a.anthracene_uf__L}}{a numeric vector}
#'           \item{\samp{Benzene_uf__L}}{a numeric vector}
#'           \item{\code{Benzo.a.pyrene_ng_g}}{a numeric vector}
#'           \item{\samp{Benzo.a.pyrene_uf__L}}{a numeric vector}
#'           \item{\code{Benzo.b.fluoranthene_ng_g}}{a numeric vector}
#'           \item{\samp{Benzo.b.fluoranthene_uf__L}}{a numeric vector}
#'           \item{\code{Benzo.e.pyrene_ng_g}}{a numeric vector}
#'           \item{\samp{Benzo.e.pyrene_uf__L}}{a numeric vector}
#'           \item{\code{Benzo.g.h.i.perylene_ng_g}}{a numeric vector}
#'           \item{\samp{Benzo.g.h.i.perylene_uf__L}}{a numeric vector}
#'           \item{\code{Benzo.k.fluoranthene_ng_g}}{a numeric vector}
#'           \item{\samp{Benzo.k.fluoranthene_uf__L}}{a numeric vector}
#'           \item{\code{Bicarbonate_HCO3_mg_L}}{a numeric vector}
#'           \item{\code{Bifenthrin_ng_g}}{a numeric vector}
#'           \item{\samp{Bifenthrin_uf__L}}{a numeric vector}
#'           \item{\code{Biphenyl_ng_g}}{a numeric vector}
#'           \item{\samp{Biphenyl_uf__L}}{a numeric vector}
#'           \item{\code{BOD_mg_L}}{a numeric vector}
#'           \item{\code{Bolstar_ng_g}}{a numeric vector}
#'           \item{\samp{Bolstar_uf__L}}{a numeric vector}
#'           \item{\samp{Bromobenzene_uf__L}}{a numeric vector}
#'           \item{\samp{Bromochloromethane_uf__L}}{a numeric vector}
#'           \item{\samp{Bromodichloromethane_uf__L}}{a numeric vector}
#'           \item{\samp{Bromoform_uf__L}}{a numeric vector}
#'           \item{\samp{Butylbenzene_n_uf__L}}{a numeric vector}
#'           \item{\samp{Butylbenzene_sec_uf__L}}{a numeric vector}
#'           \item{\samp{Butylbenzene_tert_uf__L}}{a numeric vector}
#'           \item{\code{Ca_f_mg_L}}{a numeric vector}
#'           \item{\code{Ca_uf_mg_L}}{a numeric vector}
#'           \item{\samp{Caffeine_uf__L}}{a numeric vector}
#'           \item{\samp{Carbadox_uf__L}}{a numeric vector}
#'           \item{\samp{Carbamazepine_uf__L}}{a numeric vector}
#'           \item{\code{Carbonate_CaCO3_mg_L}}{a numeric vector}
#'           \item{\samp{CarbonTetrachloride_uf__L}}{a numeric vector}
#'           \item{\code{Carbophenothion_ppb}}{a numeric vector}
#'           \item{\samp{Carbophenothion_uf__L}}{a numeric vector}
#'           \item{\samp{Cd__g}}{a numeric vector}
#'           \item{\code{Cd_f_mg_L}}{a numeric vector}
#'           \item{\code{Cd_uf_mg_L}}{a numeric vector}
#'           \item{\code{CHECK.THIS}}{a numeric vector}
#'           \item{\code{Chlor_a_mg_m2}}{a numeric vector}
#'           \item{\code{Chlor_a_mg_m3}}{a numeric vector}
#'           \item{\samp{Chlor_a_Particulate__L}}{a numeric vector}
#'           \item{\code{Chlor_a_Particulate_mg_m2}}{a numeric vector}
#'           \item{\code{Chlordane_alpha_ng_g}}{a numeric vector}
#'           \item{\code{Chlordane_cis_ng_g}}{a numeric vector}
#'           \item{\samp{Chlordane_cis_uf__L}}{a numeric vector}
#'           \item{\code{Chlordane_trans_ng_g}}{a numeric vector}
#'           \item{\samp{Chlordane_trans_uf__L}}{a numeric vector}
#'           \item{\samp{Chlordene_alpha_uf__L}}{a numeric vector}
#'           \item{\code{Chlordene_cis_ng_g}}{a numeric vector}
#'           \item{\samp{Chlordene_cis_uf__L}}{a numeric vector}
#'           \item{\code{Chlordene_gamma_ng_g}}{a numeric vector}
#'           \item{\samp{Chlordene_gamma_uf__L}}{a numeric vector}
#'           \item{\code{Chlordene_trans_ng_g}}{a numeric vector}
#'           \item{\samp{Chlordene_trans_uf__L}}{a numeric vector}
#'           \item{\code{Chlorfenvinphos_ppb}}{a logical vector}
#'           \item{\samp{Chlorfenvinphos_uf__L}}{a numeric vector}
#'           \item{\samp{Chlorobenzene_uf__L}}{a numeric vector}
#'           \item{\samp{Chloroform_uf__L}}{a numeric vector}
#'           \item{\samp{Chlorotoluene_2_uf__L}}{a numeric vector}
#'           \item{\samp{Chlorotoluene_4_uf__L}}{a numeric vector}
#'           \item{\samp{Chlorpyrifos__L}}{a numeric vector}
#'           \item{\code{Chlorpyrifos_methyl_ng_g}}{a numeric vector}
#'           \item{\samp{Chlorpyrifos_methyl_uf__L}}{a numeric vector}
#'           \item{\code{Chlorpyrifos_ng_g}}{a numeric vector}
#'           \item{\samp{Chlorpyrifos_uf__L}}{a numeric vector}
#'           \item{\code{ChlorpyrifosMethyl_ppb}}{a logical vector}
#'           \item{\samp{Chlortetracycline_uf__L}}{a numeric vector}
#'           \item{\code{ChlorthalDimethyl_ppb}}{a logical vector}
#'           \item{\code{Chrysene_ng_g}}{a numeric vector}
#'           \item{\samp{Chrysene_uf__L}}{a numeric vector}
#'           \item{\code{Chrysenes_C1_ng_g}}{a numeric vector}
#'           \item{\samp{Chrysenes_C1_uf__L}}{a numeric vector}
#'           \item{\code{Chrysenes_C2_ng_g}}{a numeric vector}
#'           \item{\samp{Chrysenes_C2_uf__L}}{a numeric vector}
#'           \item{\code{Chrysenes_C3_ng_g}}{a numeric vector}
#'           \item{\samp{Chrysenes_C3_uf__L}}{a numeric vector}
#'           \item{\code{Cinerin1_ng_g}}{a logical vector}
#'           \item{\code{Cinerin2_ng_g}}{a logical vector}
#'           \item{\code{Ciodrin_ppb}}{a logical vector}
#'           \item{\samp{Ciodrin_uf__L}}{a numeric vector}
#'           \item{\code{Cl_f_mg_L}}{a numeric vector}
#'           \item{\code{COD_mg_L}}{a numeric vector}
#'           \item{\code{Coliform_Fecal_MPN_100mL}}{a numeric vector}
#'           \item{\code{Coliform_Total_MPN_100mL}}{a numeric vector}
#'           \item{\code{Color_True_CU}}{a numeric vector}
#'           \item{\code{Coumaphos_ppb}}{a logical vector}
#'           \item{\samp{Coumaphos_uf__L}}{a numeric vector}
#'           \item{\samp{Cr__g}}{a numeric vector}
#'           \item{\code{Cr_f_mg_L}}{a numeric vector}
#'           \item{\code{Cr_uf_mg_L}}{a numeric vector}
#'           \item{\samp{Cu__g}}{a numeric vector}
#'           \item{\code{Cu_f_mg_L}}{a numeric vector}
#'           \item{\code{Cu_uf_mg_L}}{a numeric vector}
#'           \item{\code{Cyfluthrin_ng_g}}{a numeric vector}
#'           \item{\samp{Cyfluthrin_uf__L}}{a numeric vector}
#'           \item{\code{Cyhalothrin_lambda_ng_g}}{a numeric vector}
#'           \item{\code{Cyhalothrin_lambda_ppb}}{a numeric vector}
#'           \item{\samp{Cyhalothrin_lambda_uf__L}}{a numeric vector}
#'           \item{\code{Cypermethrin_ng_g}}{a numeric vector}
#'           \item{\samp{Cypermethrin_uf__L}}{a numeric vector}
#'           \item{\code{Dacthal_ng_g}}{a numeric vector}
#'           \item{\samp{Dacthal_uf__L}}{a numeric vector}
#'           \item{\code{Danitol_ng_g}}{a numeric vector}
#'           \item{\samp{Danitol_uf__L}}{a numeric vector}
#'           \item{\code{DCBP.p.p.._ng_g}}{a numeric vector}
#'           \item{\code{DDD.o.p.._ng_g}}{a numeric vector}
#'           \item{\samp{DDD.o.p.._uf__L}}{a numeric vector}
#'           \item{\code{DDD.p.p.._ng_g}}{a numeric vector}
#'           \item{\samp{DDD.p.p.._uf__L}}{a numeric vector}
#'           \item{\code{DDE.o.p.._ng_g}}{a numeric vector}
#'           \item{\samp{DDE.o.p.._uf__L}}{a numeric vector}
#'           \item{\code{DDE.p.p.._ng_g}}{a numeric vector}
#'           \item{\samp{DDE.p.p.._uf__L}}{a numeric vector}
#'           \item{\code{DDMU.p.p.._ng_g}}{a numeric vector}
#'           \item{\samp{DDMU.p.p.._uf__L}}{a numeric vector}
#'           \item{\code{DDT.o.p.._ng_g}}{a numeric vector}
#'           \item{\samp{DDT.o.p.._uf__L}}{a numeric vector}
#'           \item{\code{DDT.p.p.._ng_g}}{a numeric vector}
#'           \item{\samp{DDT.p.p.._uf__L}}{a numeric vector}
#'           \item{\code{DDTs_Total_ng_g}}{a numeric vector}
#'           \item{\code{DDVP_ppb}}{a logical vector}
#'           \item{\code{Deltamethrin_ng_g}}{a numeric vector}
#'           \item{\samp{Deltamethrin_uf__L}}{a numeric vector}
#'           \item{\code{DeltamethrinTralomethrin_ng_g}}{a numeric vector}
#'           \item{\samp{DeltamethrinTralomethrin_uf__L}}{a numeric vector}
#'           \item{\samp{Demeton_o_uf__L}}{a numeric vector}
#'           \item{\code{Demeton_s_ng_g}}{a numeric vector}
#'           \item{\samp{Demeton_s_uf__L}}{a numeric vector}
#'           \item{\code{Demeton_ug_L}}{a logical vector}
#'           \item{\samp{DesmethylLR_Particulate__L}}{a numeric vector}
#'           \item{\samp{DesmethylLR_uf__L}}{a numeric vector}
#'           \item{\samp{DesmethylRR_Particulate__L}}{a numeric vector}
#'           \item{\samp{DesmethylRR_uf__L}}{a numeric vector}
#'           \item{\code{Diazinon_ng_g}}{a numeric vector}
#'           \item{\samp{Diazinon_uf__L}}{a numeric vector}
#'           \item{\code{Diazinon_ug_L}}{a numeric vector}
#'           \item{\code{Dibenz.a.h.anthracene_ng_g}}{a numeric vector}
#'           \item{\samp{Dibenz.a.h.anthracene_uf__L}}{a numeric vector}
#'           \item{\code{Dibenzothiophene_ng_g}}{a numeric vector}
#'           \item{\samp{Dibenzothiophene_uf__L}}{a numeric vector}
#'           \item{\code{Dibenzothiophenes_C1_ng_g}}{a numeric vector}
#'           \item{\samp{Dibenzothiophenes_C1_uf__L}}{a numeric vector}
#'           \item{\code{Dibenzothiophenes_C2_ng_g}}{a numeric vector}
#'           \item{\samp{Dibenzothiophenes_C2_uf__L}}{a numeric vector}
#'           \item{\code{Dibenzothiophenes_C3_ng_g}}{a numeric vector}
#'           \item{\samp{Dibenzothiophenes_C3_uf__L}}{a numeric vector}
#'           \item{\samp{Dibromo.3.Chloropropane_1.2..DBCP._uf__L}}{a numeric vector}
#'           \item{\samp{Dibromochloromethane_uf__L}}{a numeric vector}
#'           \item{\samp{Dibromoethane_12_uf__L}}{a numeric vector}
#'           \item{\samp{Dibromomethane_uf__L}}{a numeric vector}
#'           \item{\code{Dibutyltin_Sn_ng_g}}{a numeric vector}
#'           \item{\code{DIC_f_mg_L}}{a numeric vector}
#'           \item{\code{Dichlofenthion_ng_g}}{a numeric vector}
#'           \item{\code{Dichlofenthion_ppb}}{a logical vector}
#'           \item{\samp{Dichlofenthion_uf__L}}{a numeric vector}
#'           \item{\samp{Dichlorobenzene_12_uf__L}}{a numeric vector}
#'           \item{\samp{Dichlorobenzene_13_uf__L}}{a numeric vector}
#'           \item{\samp{Dichlorobenzene_14_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloroethane_11_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloroethane_12_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloroethylene_11_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloroethylene_cis_12_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloroethylene_trans_.12_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloropropane_12_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloropropane_13_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloropropane_22_uf__L}}{a numeric vector}
#'           \item{\samp{Dichloropropene_11_uf__L}}{a numeric vector}
#'           \item{\code{Dichlorvos_ng_g}}{a numeric vector}
#'           \item{\samp{Dichlorvos_uf__L}}{a numeric vector}
#'           \item{\code{Dicrotophos_ppb}}{a logical vector}
#'           \item{\samp{Dicrotophos_uf__L}}{a numeric vector}
#'           \item{\code{Dieldrin_ng_g}}{a numeric vector}
#'           \item{\code{Dieldrin_ppb}}{a logical vector}
#'           \item{\samp{Dieldrin_uf__L}}{a numeric vector}
#'           \item{\samp{Dimethoate_uf__L}}{a numeric vector}
#'           \item{\code{Dimethylnaphthalene_2.6_ng_g}}{a numeric vector}
#'           \item{\samp{Dimethylnaphthalene_26_uf__L}}{a numeric vector}
#'           \item{\code{Dimethylphenanthrene_3.6_ng_g}}{a numeric vector}
#'           \item{\samp{Dimethylphenanthrene_36_uf__L}}{a numeric vector}
#'           \item{\code{Dioxathion._ng_g}}{a numeric vector}
#'           \item{\samp{Dioxathion._uf__L}}{a numeric vector}
#'           \item{\code{Dioxathion_ppb}}{a numeric vector}
#'           \item{\samp{Disulfoton_uf__L}}{a numeric vector}
#'           \item{\code{DO_uf_mg_L}}{a numeric vector}
#'           \item{\code{DOC_f_mg_L}}{a numeric vector}
#'           \item{\samp{DomoicAcid_Particulate__L}}{a numeric vector}
#'           \item{\samp{DomoicAcid_uf__L}}{a numeric vector}
#'           \item{\samp{Doxycycline_uf__L}}{a numeric vector}
#'           \item{\code{DP_f_mg_L}}{a numeric vector}
#'           \item{\code{DP_P_mg_L}}{a numeric vector}
#'           \item{\code{E_coli_MPN_100_mL}}{a numeric vector}
#'           \item{\code{Endosulfan_I_ng_g}}{a numeric vector}
#'           \item{\samp{Endosulfan_I_uf__L}}{a numeric vector}
#'           \item{\code{Endosulfan_II_ng_g}}{a numeric vector}
#'           \item{\code{Endosulfan_II_ppb}}{a logical vector}
#'           \item{\samp{Endosulfan_II_uf__L}}{a numeric vector}
#'           \item{\code{Endosulfan_ppb}}{a logical vector}
#'           \item{\code{Endosulfan_sulfate_ng_g}}{a numeric vector}
#'           \item{\code{Endosulfan_Sulfate_ppb}}{a logical vector}
#'           \item{\samp{Endosulfan_sulfate_uf__L}}{a numeric vector}
#'           \item{\code{Endrin_Aldehyde_ng_g}}{a numeric vector}
#'           \item{\code{Endrin_Aldehyde_ppb}}{a numeric vector}
#'           \item{\samp{Endrin_Aldehyde_uf__L}}{a numeric vector}
#'           \item{\code{Endrin_Ketone_ng_g}}{a numeric vector}
#'           \item{\code{Endrin_Ketone_ppb}}{a logical vector}
#'           \item{\samp{Endrin_Ketone_uf__L}}{a numeric vector}
#'           \item{\code{Endrin_ppb}}{a logical vector}
#'           \item{\samp{Endrin_uf__L}}{a numeric vector}
#'           \item{\code{Endrin_uf_ng_g}}{a numeric vector}
#'           \item{\code{Enterococci_MPN_100mL}}{a numeric vector}
#'           \item{\code{Enterococcus_MPN_100_mL}}{a numeric vector}
#'           \item{\samp{Erythromycin_H2O_uf__L}}{a numeric vector}
#'           \item{\code{Esfenvalerate_ng_g}}{a numeric vector}
#'           \item{\samp{Esfenvalerate_uf__L}}{a numeric vector}
#'           \item{\code{EsfenvalerateFenvalerate_ng_g}}{a numeric vector}
#'           \item{\samp{EsfenvalerateFenvalerate_uf__L}}{a numeric vector}
#'           \item{\samp{EsfenvalerateFenvalerate1_uf__L}}{a logical vector}
#'           \item{\samp{EsfenvalerateFenvalerate2_uf__L}}{a logical vector}
#'           \item{\samp{Estradiol_17beta_uf__L}}{a numeric vector}
#'           \item{\code{Ethion_ng_g}}{a numeric vector}
#'           \item{\code{Ethion_ppb}}{a logical vector}
#'           \item{\samp{Ethion_uf__L}}{a numeric vector}
#'           \item{\code{Ethoprop_ng_g}}{a numeric vector}
#'           \item{\code{Ethoprop_ppb}}{a logical vector}
#'           \item{\samp{Ethoprop_uf__L}}{a numeric vector}
#'           \item{\samp{Ethylbenzene_uf__L}}{a numeric vector}
#'           \item{\code{F_uf_mg_L}}{a numeric vector}
#'           \item{\code{Famphur_ppb}}{a logical vector}
#'           \item{\samp{Famphur_uf__L}}{a numeric vector}
#'           \item{\samp{Fe__g}}{a numeric vector}
#'           \item{\samp{Fe_f__L}}{a numeric vector}
#'           \item{\samp{Fe_uf__L}}{a numeric vector}
#'           \item{\code{Fenchlorphos_ng_g}}{a numeric vector}
#'           \item{\samp{Fenchlorphos_uf__L}}{a numeric vector}
#'           \item{\code{Fenitrothion_ng_g}}{a numeric vector}
#'           \item{\samp{Fenitrothion_uf__L}}{a numeric vector}
#'           \item{\code{Fenpropathrin_ppb}}{a logical vector}
#'           \item{\samp{Fenpropathrin_uf__L}}{a numeric vector}
#'           \item{\code{Fensulfothion_ng_g}}{a numeric vector}
#'           \item{\samp{Fensulfothion_uf__L}}{a numeric vector}
#'           \item{\code{Fenthion_ng_g}}{a numeric vector}
#'           \item{\samp{Fenthion_uf__L}}{a numeric vector}
#'           \item{\code{Fenvalerate_ng_g}}{a numeric vector}
#'           \item{\samp{Fenvalerate_uf__L}}{a numeric vector}
#'           \item{\code{Fipronil_ng_g}}{a numeric vector}
#'           \item{\code{FipronilDesulfinyl_ng_g}}{a numeric vector}
#'           \item{\code{FipronilSulfide_ng_g}}{a numeric vector}
#'           \item{\code{FipronilSulfone_ng_g}}{a numeric vector}
#'           \item{\code{Fluoranthene_ng_g}}{a numeric vector}
#'           \item{\samp{Fluoranthene_uf__L}}{a numeric vector}
#'           \item{\code{FluoranthenePyrenes_C1_ng_g}}{a numeric vector}
#'           \item{\samp{FluoranthenePyrenes_C1_uf__L}}{a numeric vector}
#'           \item{\code{Fluorene_ng_g}}{a numeric vector}
#'           \item{\samp{Fluorene_uf__L}}{a numeric vector}
#'           \item{\code{Fluorenes_C1_ng_g}}{a numeric vector}
#'           \item{\samp{Fluorenes_C1_uf__L}}{a numeric vector}
#'           \item{\code{Fluorenes_C2_ng_g}}{a numeric vector}
#'           \item{\samp{Fluorenes_C2_uf__L}}{a numeric vector}
#'           \item{\code{Fluorenes_C3_ng_g}}{a numeric vector}
#'           \item{\samp{Fluorenes_C3_uf__L}}{a numeric vector}
#'           \item{\samp{Fluoxetine_uf__L}}{a numeric vector}
#'           \item{\code{Fluvalinate_ng_g}}{a numeric vector}
#'           \item{\code{Fluvalinate_uf_ng_L}}{a numeric vector}
#'           \item{\code{Fonofos_ng_g}}{a numeric vector}
#'           \item{\code{Fonofos_ppb}}{a logical vector}
#'           \item{\samp{Fonofos_uf__L}}{a numeric vector}
#'           \item{\samp{Gemfibrozil_uf__L}}{a numeric vector}
#'           \item{\code{Hardness_CaCO3_f_mg_L}}{a numeric vector}
#'           \item{\code{Hardness_CaCO3_uf_mg_L}}{a numeric vector}
#'           \item{\code{HCH_alpha._ng_g}}{a numeric vector}
#'           \item{\samp{HCH_alpha._uf__L}}{a numeric vector}
#'           \item{\code{HCH_beta_ng_g}}{a numeric vector}
#'           \item{\samp{HCH_beta_uf__L}}{a numeric vector}
#'           \item{\code{HCH_delta_ng_g}}{a numeric vector}
#'           \item{\samp{HCH_delta_uf__L}}{a numeric vector}
#'           \item{\code{HCH_gamma_ng_g}}{a numeric vector}
#'           \item{\samp{HCH_gamma_uf__L}}{a numeric vector}
#'           \item{\code{Heptachlor_epoxide_ng_g}}{a numeric vector}
#'           \item{\samp{Heptachlor_epoxide_uf__L}}{a numeric vector}
#'           \item{\code{Heptachlor_ng_g}}{a numeric vector}
#'           \item{\code{Heptachlor_ppb}}{a logical vector}
#'           \item{\samp{Heptachlor_uf__L}}{a numeric vector}
#'           \item{\code{HeptachlorEpoxide_ppb}}{a logical vector}
#'           \item{\code{Hexachlorobenzene_ng_g}}{a numeric vector}
#'           \item{\code{Hexachlorobenzene_ppb}}{a logical vector}
#'           \item{\samp{Hexachlorobenzene_uf__L}}{a numeric vector}
#'           \item{\samp{Hexachlorobutadiene_uf__L}}{a numeric vector}
#'           \item{\samp{Hg__g}}{a numeric vector}
#'           \item{\code{Hg_f_ng_L}}{a numeric vector}
#'           \item{\code{Hg_uf_mg_kg_FishTissue}}{a numeric vector}
#'           \item{\code{Hg_uf_ng_L}}{a numeric vector}
#'           \item{\code{HydroxideAlk_CaCO3_mg_L}}{a numeric vector}
#'           \item{\samp{Ibuprofen_uf__L}}{a numeric vector}
#'           \item{\code{Indeno.1.2.3.c.d.pyrene_ng_g}}{a numeric vector}
#'           \item{\samp{Indeno.123cd.pyrene_uf__L}}{a numeric vector}
#'           \item{\samp{Isopropylbenzene_uf__L}}{a numeric vector}
#'           \item{\samp{Isopropyltoluene_p_uf__L}}{a numeric vector}
#'           \item{\code{Jasmoline1_ng_g}}{a logical vector}
#'           \item{\code{Jasmoline2_ng_g}}{a logical vector}
#'           \item{\code{K_f_mg_L}}{a numeric vector}
#'           \item{\code{K_uf_mg_L}}{a numeric vector}
#'           \item{\code{Leptophos_ppb}}{a logical vector}
#'           \item{\samp{Leptophos_uf__L}}{a numeric vector}
#'           \item{\samp{Lincomycin_uf__L}}{a numeric vector}
#'           \item{\code{Malathion_ng_g}}{a numeric vector}
#'           \item{\samp{Malathion_uf__L}}{a numeric vector}
#'           \item{\code{Malathion_ug_L}}{a numeric vector}
#'           \item{\code{MBAS_mg_L}}{a numeric vector}
#'           \item{\code{MBAS_uf_mg_L}}{a numeric vector}
#'           \item{\code{MeanAlkalinity}}{a numeric vector}
#'           \item{\code{Merphos_ng_g}}{a numeric vector}
#'           \item{\samp{Merphos_uf__L}}{a numeric vector}
#'           \item{\samp{Methidathion_uf__L}}{a numeric vector}
#'           \item{\code{Methoxychlor_ng_g}}{a numeric vector}
#'           \item{\code{Methoxychlor_ppb}}{a logical vector}
#'           \item{\samp{Methoxychlor_uf__L}}{a numeric vector}
#'           \item{\code{Methyldibenzothiophene_4_ng_g}}{a numeric vector}
#'           \item{\samp{Methyldibenzothiophene_4_uf__L}}{a numeric vector}
#'           \item{\code{Methylfluoranthene_2_ng_g}}{a numeric vector}
#'           \item{\samp{Methylfluoranthene_2_uf__L}}{a numeric vector}
#'           \item{\code{Methylfluorene_1_ng_g}}{a numeric vector}
#'           \item{\samp{Methylfluorene_1_uf__L}}{a numeric vector}
#'           \item{\code{Methylnaphthalene_1_ng_g}}{a numeric vector}
#'           \item{\samp{Methylnaphthalene_1_uf__L}}{a numeric vector}
#'           \item{\code{Methylnaphthalene_2_ng_g}}{a numeric vector}
#'           \item{\samp{Methylnaphthalene_2_uf__L}}{a numeric vector}
#'           \item{\code{Methylphenanthrene_1_ng_g}}{a numeric vector}
#'           \item{\samp{Methylphenanthrene_1_uf__L}}{a numeric vector}
#'           \item{\code{Mevinphos_ng_g}}{a numeric vector}
#'           \item{\code{Mevinphos_ppb}}{a numeric vector}
#'           \item{\samp{Mevinphos_uf__L}}{a numeric vector}
#'           \item{\code{Mg_f_mg_L}}{a numeric vector}
#'           \item{\code{Mg_uf_mg_L}}{a numeric vector}
#'           \item{\samp{MicrocystinLA_Particulate__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLA_uf__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLF_Particulate__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLF_uf__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLR_Particulate__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLR_uf__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLW_Particulate__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLW_uf__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLY_Particulate__L}}{a numeric vector}
#'           \item{\samp{MicrocystinLY_uf__L}}{a numeric vector}
#'           \item{\samp{MicrocystinRR_Particulate__L}}{a numeric vector}
#'           \item{\samp{MicrocystinRR_uf__L}}{a numeric vector}
#'           \item{\samp{MicrocystinYR_Particulate__L}}{a numeric vector}
#'           \item{\samp{MicrocystinYR_uf__L}}{a numeric vector}
#'           \item{\code{Mirex_ng_g}}{a numeric vector}
#'           \item{\code{Mirex_ppb}}{a logical vector}
#'           \item{\samp{Mirex_uf__L}}{a numeric vector}
#'           \item{\samp{Mn__g}}{a numeric vector}
#'           \item{\samp{Mn_f__L}}{a numeric vector}
#'           \item{\samp{Mn_uf__L}}{a numeric vector}
#'           \item{\code{Molinate_ppb}}{a logical vector}
#'           \item{\samp{Molinate_uf__L}}{a numeric vector}
#'           \item{\code{Monobutyltin_Sn_ng_g}}{a numeric vector}
#'           \item{\samp{MTBE_uf__L}}{a numeric vector}
#'           \item{\code{Na_f_mg_L}}{a numeric vector}
#'           \item{\code{Na_uf_mg_L}}{a numeric vector}
#'           \item{\code{Naled_ppb}}{a logical vector}
#'           \item{\samp{Naled_uf__L}}{a numeric vector}
#'           \item{\code{Naphthalene_ng_g}}{a numeric vector}
#'           \item{\samp{Naphthalene_uf__L}}{a numeric vector}
#'           \item{\code{Naphthalenes_C1_ng_g}}{a numeric vector}
#'           \item{\samp{Naphthalenes_C1_uf__L}}{a numeric vector}
#'           \item{\code{Naphthalenes_C2_ng_g}}{a numeric vector}
#'           \item{\samp{Naphthalenes_C2_uf__L}}{a numeric vector}
#'           \item{\code{Naphthalenes_C3_ng_g}}{a numeric vector}
#'           \item{\samp{Naphthalenes_C3_uf__L}}{a numeric vector}
#'           \item{\code{Naphthalenes_C4_ng_g}}{a numeric vector}
#'           \item{\samp{Naphthalenes_C4_uf__L}}{a numeric vector}
#'           \item{\code{NH3_N_f_mg_L}}{a numeric vector}
#'           \item{\code{NH3_N_mg_kg_ww}}{a numeric vector}
#'           \item{\code{NH3_N_uf_mg_L}}{a numeric vector}
#'           \item{\samp{Ni__g}}{a numeric vector}
#'           \item{\code{Ni_f_mg_L}}{a numeric vector}
#'           \item{\code{Ni_uf_mg_L}}{a numeric vector}
#'           \item{\code{NO2_N_f_mg_L}}{a numeric vector}
#'           \item{\code{NO2_N_uf_mg_L}}{a numeric vector}
#'           \item{\code{NO2NO3_N_f_mg_L}}{a numeric vector}
#'           \item{\code{NO2NO3_N_uf_mg_L}}{a numeric vector}
#'           \item{\code{NO3_N_f_mg_L}}{a numeric vector}
#'           \item{\code{NO3_N_uf_mg_L}}{a numeric vector}
#'           \item{\samp{Nodularin_Particulate__L}}{a numeric vector}
#'           \item{\samp{Nodularin_uf__L}}{a numeric vector}
#'           \item{\code{Nonachlor_cis_ng_g}}{a numeric vector}
#'           \item{\samp{Nonachlor_cis_uf__L}}{a numeric vector}
#'           \item{\code{Nonachlor_trans_ng_g}}{a numeric vector}
#'           \item{\samp{Nonachlor_trans_uf__L}}{a numeric vector}
#'           \item{\samp{Nonylphenol_uf__L}}{a numeric vector}
#'           \item{\samp{Nonylphenolethoxylate_uf__L}}{a numeric vector}
#'           \item{\code{O2Sat_uf_.}}{a numeric vector}
#'           \item{\code{Oil_Grease_mg_L}}{a numeric vector}
#'           \item{\samp{OkadaicAcid_Particulate__L}}{a numeric vector}
#'           \item{\samp{OkadaicAcid_uf__L}}{a numeric vector}
#'           \item{\code{oPO4_P_f_mg_L}}{a numeric vector}
#'           \item{\code{oPO4_P_uf_mg_L}}{a numeric vector}
#'           \item{\code{Oxadiazon_ng_g}}{a numeric vector}
#'           \item{\code{Oxadiazon_ppb}}{a numeric vector}
#'           \item{\samp{Oxadiazon_uf__L}}{a numeric vector}
#'           \item{\code{Oxychlordane_ng_g}}{a numeric vector}
#'           \item{\samp{Oxychlordane_uf__L}}{a numeric vector}
#'           \item{\samp{Oxytetracycline_uf__L}}{a numeric vector}
#'           \item{\code{PAHs_ng_g}}{a numeric vector}
#'           \item{\code{Parathion_Ethyl_ng_g}}{a numeric vector}
#'           \item{\samp{Parathion_Ethyl_uf__L}}{a numeric vector}
#'           \item{\code{Parathion_Methyl_ng_g}}{a numeric vector}
#'           \item{\samp{Parathion_Methyl_uf__L}}{a numeric vector}
#'           \item{\samp{Pb__g}}{a numeric vector}
#'           \item{\code{Pb_f_mg_L}}{a numeric vector}
#'           \item{\code{Pb_uf_mg_L}}{a numeric vector}
#'           \item{\code{PBDE017_ng_g}}{a numeric vector}
#'           \item{\code{PBDE028_ng_g}}{a numeric vector}
#'           \item{\code{PBDE047_ng_g}}{a numeric vector}
#'           \item{\code{PBDE066_ng_g}}{a numeric vector}
#'           \item{\code{PBDE085_ng_g}}{a numeric vector}
#'           \item{\code{PBDE099_ng_g}}{a numeric vector}
#'           \item{\code{PBDE100_ng_g}}{a numeric vector}
#'           \item{\code{PBDE138_ng_g}}{a numeric vector}
#'           \item{\code{PBDE153_ng_g}}{a numeric vector}
#'           \item{\code{PBDE154_ng_g}}{a numeric vector}
#'           \item{\code{PBDE183_ng_g}}{a numeric vector}
#'           \item{\code{PBDE190_ng_g}}{a numeric vector}
#'           \item{\code{PBDE209_ng_g}}{a numeric vector}
#'           \item{\code{PCB_AROCLOR_1248_ng_g}}{a numeric vector}
#'           \item{\code{PCB_AROCLOR_1254_ng_g}}{a numeric vector}
#'           \item{\code{PCB_AROCLOR_1260_ng_g}}{a numeric vector}
#'           \item{\code{PCB003_ng_g}}{a numeric vector}
#'           \item{\samp{PCB005_uf__L}}{a numeric vector}
#'           \item{\code{PCB008_ng_g}}{a numeric vector}
#'           \item{\samp{PCB008_uf__L}}{a numeric vector}
#'           \item{\samp{PCB015_uf__L}}{a numeric vector}
#'           \item{\code{PCB018_ng_g}}{a numeric vector}
#'           \item{\samp{PCB018_uf__L}}{a numeric vector}
#'           \item{\code{PCB027_ng_g}}{a numeric vector}
#'           \item{\samp{PCB027_uf__L}}{a numeric vector}
#'           \item{\code{PCB028_ng_g}}{a numeric vector}
#'           \item{\samp{PCB028_uf__L}}{a numeric vector}
#'           \item{\code{PCB029_ng_g}}{a numeric vector}
#'           \item{\samp{PCB029_uf__L}}{a numeric vector}
#'           \item{\code{PCB031_ng_g}}{a numeric vector}
#'           \item{\samp{PCB031_uf__L}}{a numeric vector}
#'           \item{\code{PCB033_ng_g}}{a numeric vector}
#'           \item{\samp{PCB033_uf__L}}{a numeric vector}
#'           \item{\code{PCB037_ng_g}}{a numeric vector}
#'           \item{\code{PCB044_ng_g}}{a numeric vector}
#'           \item{\samp{PCB044_uf__L}}{a numeric vector}
#'           \item{\code{PCB049_ng_g}}{a numeric vector}
#'           \item{\samp{PCB049_uf__L}}{a numeric vector}
#'           \item{\code{PCB052_ng_g}}{a numeric vector}
#'           \item{\samp{PCB052_uf__L}}{a numeric vector}
#'           \item{\code{PCB056_ng_g}}{a numeric vector}
#'           \item{\samp{PCB056_uf__L}}{a numeric vector}
#'           \item{\code{PCB060_ng_g}}{a numeric vector}
#'           \item{\samp{PCB060_uf__L}}{a numeric vector}
#'           \item{\code{PCB064_ng_g}}{a numeric vector}
#'           \item{\code{PCB066_ng_g}}{a numeric vector}
#'           \item{\samp{PCB066_uf__L}}{a numeric vector}
#'           \item{\code{PCB070_ng_g}}{a numeric vector}
#'           \item{\samp{PCB070_uf__L}}{a numeric vector}
#'           \item{\code{PCB074_ng_g}}{a numeric vector}
#'           \item{\samp{PCB074_uf__L}}{a numeric vector}
#'           \item{\code{PCB077_ng_g}}{a numeric vector}
#'           \item{\samp{PCB077_uf__L}}{a numeric vector}
#'           \item{\code{PCB081_ng_g}}{a numeric vector}
#'           \item{\code{PCB087_ng_g}}{a numeric vector}
#'           \item{\samp{PCB087_uf__L}}{a numeric vector}
#'           \item{\code{PCB095_ng_g}}{a numeric vector}
#'           \item{\samp{PCB095_uf__L}}{a numeric vector}
#'           \item{\code{PCB097_ng_g}}{a numeric vector}
#'           \item{\samp{PCB097_uf__L}}{a numeric vector}
#'           \item{\code{PCB099_ng_g}}{a numeric vector}
#'           \item{\samp{PCB099_uf__L}}{a numeric vector}
#'           \item{\code{PCB101_ng_g}}{a numeric vector}
#'           \item{\samp{PCB101_uf__L}}{a numeric vector}
#'           \item{\code{PCB105_ng_g}}{a numeric vector}
#'           \item{\samp{PCB105_uf__L}}{a numeric vector}
#'           \item{\code{PCB110_ng_g}}{a numeric vector}
#'           \item{\samp{PCB110_uf__L}}{a numeric vector}
#'           \item{\code{PCB114_ng_g}}{a numeric vector}
#'           \item{\samp{PCB114_uf__L}}{a numeric vector}
#'           \item{\code{PCB118_ng_g}}{a numeric vector}
#'           \item{\samp{PCB118_uf__L}}{a numeric vector}
#'           \item{\code{PCB119_ng_g}}{a numeric vector}
#'           \item{\code{PCB123_ng_g}}{a numeric vector}
#'           \item{\code{PCB126_ng_g}}{a numeric vector}
#'           \item{\samp{PCB126_uf__L}}{a numeric vector}
#'           \item{\code{PCB128_ng_g}}{a numeric vector}
#'           \item{\samp{PCB128_uf__L}}{a numeric vector}
#'           \item{\code{PCB137_ng_g}}{a numeric vector}
#'           \item{\samp{PCB137_uf__L}}{a numeric vector}
#'           \item{\code{PCB138_ng_g}}{a numeric vector}
#'           \item{\samp{PCB138_uf__L}}{a numeric vector}
#'           \item{\code{PCB141_ng_g}}{a numeric vector}
#'           \item{\samp{PCB141_uf__L}}{a numeric vector}
#'           \item{\code{PCB146_ng_g}}{a numeric vector}
#'           \item{\code{PCB149_ng_g}}{a numeric vector}
#'           \item{\samp{PCB149_uf__L}}{a numeric vector}
#'           \item{\code{PCB151_ng_g}}{a numeric vector}
#'           \item{\samp{PCB151_uf__L}}{a numeric vector}
#'           \item{\code{PCB153_ng_g}}{a numeric vector}
#'           \item{\samp{PCB153_uf__L}}{a numeric vector}
#'           \item{\code{PCB156_ng_g}}{a numeric vector}
#'           \item{\samp{PCB156_uf__L}}{a numeric vector}
#'           \item{\code{PCB157_ng_g}}{a numeric vector}
#'           \item{\samp{PCB157_uf__L}}{a numeric vector}
#'           \item{\code{PCB158_ng_g}}{a numeric vector}
#'           \item{\samp{PCB158_uf__L}}{a numeric vector}
#'           \item{\code{PCB167_ng_g}}{a numeric vector}
#'           \item{\code{PCB168_132_ng_g}}{a numeric vector}
#'           \item{\code{PCB168_ng_g}}{a numeric vector}
#'           \item{\code{PCB169_ng_g}}{a numeric vector}
#'           \item{\code{PCB170_ng_g}}{a numeric vector}
#'           \item{\samp{PCB170_uf__L}}{a numeric vector}
#'           \item{\code{PCB174_ng_g}}{a numeric vector}
#'           \item{\samp{PCB174_uf__L}}{a numeric vector}
#'           \item{\code{PCB177_ng_g}}{a numeric vector}
#'           \item{\samp{PCB177_uf__L}}{a numeric vector}
#'           \item{\code{PCB180_ng_g}}{a numeric vector}
#'           \item{\samp{PCB180_uf__L}}{a numeric vector}
#'           \item{\code{PCB183_ng_g}}{a numeric vector}
#'           \item{\samp{PCB183_uf__L}}{a numeric vector}
#'           \item{\code{PCB187_ng_g}}{a numeric vector}
#'           \item{\samp{PCB187_uf__L}}{a numeric vector}
#'           \item{\code{PCB189_ng_g}}{a numeric vector}
#'           \item{\samp{PCB189_uf__L}}{a numeric vector}
#'           \item{\code{PCB194_ng_g}}{a numeric vector}
#'           \item{\samp{PCB194_uf__L}}{a numeric vector}
#'           \item{\code{PCB195_ng_g}}{a numeric vector}
#'           \item{\samp{PCB195_uf__L}}{a numeric vector}
#'           \item{\code{PCB198_199_ng_g}}{a numeric vector}
#'           \item{\code{PCB200_ng_g}}{a numeric vector}
#'           \item{\samp{PCB200_uf__L}}{a numeric vector}
#'           \item{\code{PCB201_ng_g}}{a numeric vector}
#'           \item{\samp{PCB201_uf__L}}{a numeric vector}
#'           \item{\code{PCB203_ng_g}}{a numeric vector}
#'           \item{\samp{PCB203_uf__L}}{a numeric vector}
#'           \item{\code{PCB206_ng_g}}{a numeric vector}
#'           \item{\samp{PCB206_uf__L}}{a numeric vector}
#'           \item{\code{PCB209_ng_g}}{a numeric vector}
#'           \item{\samp{PCB209_uf__L}}{a numeric vector}
#'           \item{\code{Permethrin_cis_ng_g}}{a numeric vector}
#'           \item{\samp{Permethrin_cis_uf__L}}{a numeric vector}
#'           \item{\code{Permethrin_ng_g}}{a numeric vector}
#'           \item{\code{Permethrin_trans_ng_g}}{a numeric vector}
#'           \item{\samp{Permethrin_trans_uf__L}}{a numeric vector}
#'           \item{\samp{Permethrin_uf__L}}{a numeric vector}
#'           \item{\code{Perthane_ng_g}}{a numeric vector}
#'           \item{\code{Perylene_ng_g}}{a numeric vector}
#'           \item{\samp{Perylene_uf__L}}{a numeric vector}
#'           \item{\code{pH}}{a numeric vector}
#'           \item{\code{Phenanthrene_ng_g}}{a numeric vector}
#'           \item{\samp{Phenanthrene_uf__L}}{a numeric vector}
#'           \item{\code{PhenanthreneAnthracene_C1_ng_g}}{a numeric vector}
#'           \item{\samp{PhenanthreneAnthracene_C1_uf__L}}{a numeric vector}
#'           \item{\code{PhenanthreneAnthracene_C2_ng_g}}{a numeric vector}
#'           \item{\samp{PhenanthreneAnthracene_C2_uf__L}}{a numeric vector}
#'           \item{\code{PhenanthreneAnthracene_C3_ng_g}}{a numeric vector}
#'           \item{\samp{PhenanthreneAnthracene_C3_uf__L}}{a numeric vector}
#'           \item{\code{PhenanthreneAnthracene_C4_ng_g}}{a numeric vector}
#'           \item{\samp{PhenanthreneAnthracene_C4_uf__L}}{a numeric vector}
#'           \item{\samp{Pheo_a_Particulate__L}}{a numeric vector}
#'           \item{\code{Phorate_ng_g}}{a numeric vector}
#'           \item{\samp{Phorate_uf__L}}{a numeric vector}
#'           \item{\samp{Phosmet_uf__L}}{a numeric vector}
#'           \item{\code{Phosphamidon_ng_g}}{a numeric vector}
#'           \item{\code{Phosphamidon_ppb}}{a logical vector}
#'           \item{\samp{Phosphamidon_uf__L}}{a numeric vector}
#'           \item{\code{PiperonylButoxide_ng_g}}{a numeric vector}
#'           \item{\code{PO4_P_mg_kg}}{a numeric vector}
#'           \item{\code{PO4_P_uf_mg_L}}{a numeric vector}
#'           \item{\code{Prallethrin_ng_g}}{a numeric vector}
#'           \item{\samp{Prallethrin_uf__L}}{a numeric vector}
#'           \item{\code{Prometon_ppb}}{a numeric vector}
#'           \item{\samp{Prometon_uf__L}}{a numeric vector}
#'           \item{\code{Prometryn_ppb}}{a logical vector}
#'           \item{\samp{Prometryn_uf__L}}{a numeric vector}
#'           \item{\code{Propazine_ppb}}{a numeric vector}
#'           \item{\samp{Propazine_uf__L}}{a numeric vector}
#'           \item{\samp{Propylbenzene_n_uf__L}}{a numeric vector}
#'           \item{\code{Prothiofos_ppb}}{a logical vector}
#'           \item{\code{Pyrene_ng_g}}{a numeric vector}
#'           \item{\samp{Pyrene_uf__L}}{a numeric vector}
#'           \item{\code{Pyrethrin1_ng_g}}{a logical vector}
#'           \item{\code{Pyrethrin2_ng_g}}{a logical vector}
#'           \item{\code{Resmethrin_ng_g}}{a numeric vector}
#'           \item{\code{Ronnel_ppb}}{a logical vector}
#'           \item{\samp{Roxithromycin_uf__L}}{a numeric vector}
#'           \item{\code{S_mg_kg}}{a numeric vector}
#'           \item{\code{Salinity_uf_ppt}}{a numeric vector}
#'           \item{\samp{Sb__g}}{a numeric vector}
#'           \item{\code{Sb_f_mg_L}}{a numeric vector}
#'           \item{\code{Sb_uf_mg_L}}{a numeric vector}
#'           \item{\samp{Se__g}}{a numeric vector}
#'           \item{\code{Se_f_mg_L}}{a numeric vector}
#'           \item{\code{Se_uf_mg_L}}{a numeric vector}
#'           \item{\code{Secbumeton_ppb}}{a logical vector}
#'           \item{\samp{Secbumeton_uf__L}}{a numeric vector}
#'           \item{\code{Si_SiO2_f_mg_L}}{a numeric vector}
#'           \item{\code{Si_SiO2_uf_mg_L}}{a numeric vector}
#'           \item{\code{Simazine_ppb}}{a numeric vector}
#'           \item{\samp{Simazine_uf__L}}{a numeric vector}
#'           \item{\code{Simetryn_ppb}}{a logical vector}
#'           \item{\samp{Simetryn_uf__L}}{a numeric vector}
#'           \item{\code{SO4_f_mg_L}}{a numeric vector}
#'           \item{\code{SO4_uf_mg_L}}{a numeric vector}
#'           \item{\code{Solids_.}}{a numeric vector}
#'           \item{\code{Solids_mg_L}}{a numeric vector}
#'           \item{\samp{SpecCond_uf__cm}}{a numeric vector}
#'           \item{\samp{Sulfachloropyridazine_uf__L}}{a numeric vector}
#'           \item{\samp{Sulfadimethoxine_uf__L}}{a numeric vector}
#'           \item{\samp{Sulfamerazine_uf__L}}{a numeric vector}
#'           \item{\samp{Sulfamethazine_uf__L}}{a numeric vector}
#'           \item{\samp{Sulfamethizole_uf__L}}{a numeric vector}
#'           \item{\samp{Sulfamethoxazole_uf__L}}{a numeric vector}
#'           \item{\samp{Sulfathiazole_uf__L}}{a numeric vector}
#'           \item{\code{Sulfotep_ng_g}}{a numeric vector}
#'           \item{\code{Sulfotep_ppb}}{a logical vector}
#'           \item{\samp{Sulfotep_uf__L}}{a numeric vector}
#'           \item{\code{Sulprofos_ppb}}{a logical vector}
#'           \item{\code{SuspSedConc_Particulate_mg_L}}{a numeric vector}
#'           \item{\code{TDS_calc_mg_L}}{a numeric vector}
#'           \item{\code{TDS_f_mg_L}}{a numeric vector}
#'           \item{\code{Tedion_ng_g}}{a numeric vector}
#'           \item{\samp{Tedion_uf__L}}{a numeric vector}
#'           \item{\code{Temp_degC}}{a numeric vector}
#'           \item{\code{Temp_degF}}{a numeric vector}
#'           \item{\code{Terbufos_ppb}}{a logical vector}
#'           \item{\samp{Terbufos_uf__L}}{a numeric vector}
#'           \item{\code{Terbuthylazine_ppb}}{a numeric vector}
#'           \item{\samp{Terbuthylazine_uf__L}}{a numeric vector}
#'           \item{\code{Terbutryn_ppb}}{a logical vector}
#'           \item{\samp{Terbutryn_uf__L}}{a numeric vector}
#'           \item{\samp{Terphenyl_d14_Surrogate_uf__L}}{a numeric vector}
#'           \item{\samp{Tetrachloroethane_1112_uf__L}}{a numeric vector}
#'           \item{\samp{Tetrachloroethane_1122_uf__L}}{a numeric vector}
#'           \item{\samp{Tetrachloroethylene_uf__L}}{a numeric vector}
#'           \item{\code{Tetrachlorvinphos_ng_g}}{a numeric vector}
#'           \item{\code{Tetrachlorvinphos_ppb}}{a logical vector}
#'           \item{\samp{Tetrachlorvinphos_uf__L}}{a numeric vector}
#'           \item{\samp{Tetracycline_uf__L}}{a numeric vector}
#'           \item{\code{Tetradifon_ppb}}{a logical vector}
#'           \item{\code{Thiobencarb_ppb}}{a numeric vector}
#'           \item{\samp{Thiobencarb_uf__L}}{a numeric vector}
#'           \item{\code{Thionazin_ng_g}}{a numeric vector}
#'           \item{\code{Thionazin_ppb}}{a logical vector}
#'           \item{\samp{Thionazin_uf__L}}{a numeric vector}
#'           \item{\code{TKN_uf_mg_L}}{a numeric vector}
#'           \item{\code{TN_mg_kg}}{a numeric vector}
#'           \item{\code{TN_uf_mg_L}}{a numeric vector}
#'           \item{\code{TOC_.}}{a numeric vector}
#'           \item{\code{TOC_uf_mg_L}}{a numeric vector}
#'           \item{\code{Tokuthion_ng_g}}{a numeric vector}
#'           \item{\samp{Tokuthion_uf__L}}{a numeric vector}
#'           \item{\samp{Toluene_uf__L}}{a numeric vector}
#'           \item{\code{Toxaphene_ng_g}}{a numeric vector}
#'           \item{\code{TP_mg_L}}{a numeric vector}
#'           \item{\code{TP_P_uf_mg_L}}{a numeric vector}
#'           \item{\code{Tributyltin_Sn_ng_g}}{a numeric vector}
#'           \item{\samp{Trichlorfon_uf__L}}{a numeric vector}
#'           \item{\samp{Trichlorobenzene_123_uf__L}}{a numeric vector}
#'           \item{\samp{Trichlorobenzene_124_uf__L}}{a numeric vector}
#'           \item{\samp{Trichloroethane_111_uf__L}}{a numeric vector}
#'           \item{\samp{Trichloroethane_112_uf__L}}{a numeric vector}
#'           \item{\samp{Trichloroethylene_uf__L}}{a numeric vector}
#'           \item{\code{Trichloronate_ng_g}}{a numeric vector}
#'           \item{\samp{Trichloronate_uf__L}}{a numeric vector}
#'           \item{\code{Trichlorophon_ppb}}{a logical vector}
#'           \item{\samp{Trichloropropane_123_uf__L}}{a numeric vector}
#'           \item{\samp{Triclosan_uf__L}}{a numeric vector}
#'           \item{\samp{Trimethoprim_uf__L}}{a numeric vector}
#'           \item{\samp{Trimethylbenzene_124_uf__L}}{a numeric vector}
#'           \item{\samp{Trimethylbenzene_135_uf__L}}{a numeric vector}
#'           \item{\code{Trimethylnaphthalene_2.3.5_ng_g}}{a numeric vector}
#'           \item{\samp{Trimethylnaphthalene_235_uf__L}}{a numeric vector}
#'           \item{\code{TSS_uf_mg_L}}{a numeric vector}
#'           \item{\code{Turb_NTU}}{a numeric vector}
#'           \item{\samp{Tylosin_uf__L}}{a numeric vector}
#'           \item{\samp{Xylene_mp_uf__L}}{a numeric vector}
#'           \item{\samp{Xylene_o_uf__L}}{a numeric vector}
#'           \item{\samp{Zn__g}}{a numeric vector}
#'           \item{\code{Zn_f_mg_L}}{a numeric vector}
#'           \item{\code{Zn_uf_mg_L}}{a numeric vector}
#' }
#' @source example data
"data_CoOccur"
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
# data_Sites ####
#' @title Sites example data
#' 
#' @description A dataset with example site information for use with the getSiteInfo function.
#' 
#' @format A data frame with 2,244 observations on the following 23 variables:
#' \describe{
#'           \item{STATION_CD}{Station Code}
#'               \item{\code{STATION_CD}}{Station Code}
#'                \item{\code{STATION_NAME}}{Station Name}
#'                \item{\code{COMID}}{Associated NHD+v2 COMID}
#'                \item{\code{STATION_RID}}{a numeric vector}
#'                \item{\code{HUC12}}{12 digit Hydrologic Unit Code}
#'                \item{\code{COUNTY_NAME}}{County Name}
#'                \item{\code{STATION_TYPE}}{Station Type}
#'                \item{\code{LATITUDE}}{Latitude}
#'                \item{\code{LONGITUDE}}{Longitude}
#'                \item{\code{UTM_EAST}}{UTM Easting}
#'                \item{\code{UTM_NORTH}}{UTM Northing}
#'                \item{\code{UTM_ZONE}}{UTM Zone \code{11N} \code{12N}}
#'                \item{\code{Elevation}}{Elevation}
#'                \item{\code{ReferenceStatus}}{Reference Status \code{reference} \code{Reference}}
#'                \item{\code{Year}}{Year}
#'                \item{\code{StationID_Master}}{StationID Master Code}
#'                \item{\code{FinalLatitude}}{Final Latitude}
#'                \item{\code{FinalLongitude}}{Final Longitude}
#'                \item{\code{WaterbodyName}}{Waterbody Name}
#'                \item{\code{GIS_County}}{County; derived from GIS }
#'                \item{\code{CARefSite_2017}}{a character vector}
#'                \item{\code{COMID_NHD2}}{a numeric vector}
#'                \item{\code{ElevCategory}}{Elevation Category; Hi or Lo, break point at 5,000 ft}
#' }
#' @source example data
"data_Sites"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_ReachMod ####
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
# data_303d ####
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
# ******NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_Cluster_Hi ####
#' @title High elevation cluster example data
#' 
#' @description A dataset with example cluster data for use with the getSiteInfo function.
#' 
#' @format A data frame with 5,783 rows and 96 variables:
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
#'            \item{\code{ElevCategory}}{a character vector}
#' }
#' @source example data
"data_Cluster_Hi"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ******NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_Cluster_Lo ####
#' @title Low  elevation cluster example data
#' 
#' @description A dataset with example cluster data for use with the getSiteInfo function.
#' 
#' @format A data frame with 10,593 rows and 98 variables:
#' \describe{
#'           \item{COMID}{NHDplus COMID}
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
#'           \item{\code{ElevCategory}}{a character vector}
#' }
#' @source example data
"data_Cluster_Lo"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ******NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_BMIMetrics ####
#' @title Benthic macroinvertebrate metrics example data
#' 
#' @description A dataset with example benthic macroinvertebrate (BMI) metric
#'  data for use with the getSiteInfo function.
#' 
#' @format A data frame with 957 rows and 32 variables:
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{Year}}{a numeric vector}
#'           \item{\code{Index}}{a factor with levels \code{Sp} \code{Su}}
#'           \item{\code{BenSampID}}{a numeric vector}
#'           \item{\code{RepNum}}{a numeric vector}
#'           \item{\code{Lat_Dec}}{a numeric vector}
#'           \item{\code{Long_Dec}}{a numeric vector}
#'           \item{\code{InvertReg}}{a factor with levels \code{cold} \code{warm}}
#'           \item{\code{StreamType}}{a factor with levels \code{Effluent} \code{Perennial}}
#'           \item{\code{ActivityCategory}}{a factor with levels \code{Field Replicate} \code{Routine Sample}}
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
#'           \item{\code{ElevCategory}}{a character vector}
#' }
#' @source example data
"data_BMIMetrics"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_AlgMetrics ####
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
# data_SampSummary ####
#' @title Sample summary example data
#' 
#' @description A dataset with example sample summary for use with the getSiteInfo function.
#' 
#' @format A data frame with 3,577 rows and 7 variables:
#' \describe{
#'           \item{StationID_Master}{Station ID}
#'           \item{CollDate}{Station ID}
#'           \item{Station_Date}{combined StationID and Date}
#'           \item{ChemCampID}{SampleID, Chem}
#'           \item{PhabSampID}{SampleID, Phab}
#'           \item{BMI.Metrics.SampID}{SampleID, BMI Metrics}
#'           \item{Algae.Metrics.SampID}{SampleID, Algae Metrics}
#' }
#' @source example data
"data_SampSummary"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ******NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_BMIcounts ####
#' @title Benthic Macroinvertebrate counts example data
#' 
#' @description A dataset with example benthic macroinvertebrate (BMI) counts 
#' for use with the getStresssorSpecificRegressions function.
#' 
#' @format A data frame with 57,012 rows and 24 variables:
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{StationID}}{Station ID}
#'           \item{\code{Lat_Dec}}{Latitude, decimal degrees}
#'           \item{\code{Long_Dec}}{Longitude, decimal degrees}
#'           \item{\code{StreamType}}{a factor with levels \code{Effluent} \code{Perennial}}
#'           \item{\code{WBID}}{a factor with levels }
#'           \item{\code{Elevation}}{a numeric vector}
#'           \item{\code{InvertReg}}{a factor with levels \code{cold} \code{Cold} \code{warm} \code{Warm}}
#'           \item{\code{BenSampID}}{a numeric vector}
#'           \item{\code{RepNum}}{a numeric vector}
#'           \item{\code{CollDate}}{a Date}
#'           \item{\code{Comments}}{a factor with levels }
#'           \item{\code{CollMeth}}{a factor with levels \code{ADEQ Riffle bugs} \code{ADEQ Riffle Bugs} \code{EMAP Multihab bugs}}
#'           \item{\code{FieldGearID}}{a factor with levels \code{D-frame di}}
#'           \item{\code{Habitat}}{a factor with levels \code{Edge} \code{Multi-habitat} \code{Pool} \code{Riffle} \code{Run}}
#'           \item{\code{WQX_FinalID}}{a factor with levels }
#'           \item{\code{FinalID}}{a factor with levels }
#'           \item{\code{Individuals}}{a numeric vector}
#'           \item{\code{IndividualsCorrected}}{a numeric vector}
#'           \item{\code{LargeRare}}{a logical vector}
#'           \item{\code{Stage}}{a factor with levels \code{A} \code{ei} \code{eI} \code{i} \code{L} \code{p} \code{P} \code{x} \code{X}}
#'           \item{\code{BMISampID}}{a character vector}
#'           \item{\code{BMI.Metrics.SampID}}{a character vector}
#'           \item{\code{ElevCategory}}{a character vector}
#' }
#' @source example data
"data_BMIcounts"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_GIS_Flow_HI ####
#' @title NHD+ flow line example data, AZ high gradient
#' 
#' @description A dataset with example GIS flow line data 
#' for use with the getSiteInfo function.
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
#' for use with the getSiteInfo function.
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
# data_GIS_AZ_Outline ####
#' @title AZ state outline
#' 
#' @description A dataset with example GIS state outline of Arizona  
#' for use with the getSiteInfo function.
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
#  ******NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_Chem ####
#' @title Chem data
#' 
#' @description Chem data
#' 
#'  @format A data frame with 103,548 observations on the following 16 variables.
#' \describe{
#'           \item{\code{StationID_Master}}{a factor with levels }
#'           \item{\code{SITE_ID}}{a factor with levels }
#'           \item{\code{SITE_NAME}}{a factor with levels }
#'           \item{\code{SampDate}}{a Date}
#'           \item{\code{SAMPLE_TIME}}{a numeric vector}
#'           \item{\code{SAMPLE_TYPE}}{a factor with levels \code{A} \code{B} \code{C} \code{F} \code{G} \code{I} \code{M} \code{W} \code{Z}}
#'           \item{\code{FLOW_REGIME_CODE}}{a factor with levels \code{PER}}
#'           \item{\code{StdParamName}}{a factor with levels }
#'           \item{\code{FinalResultText}}{a factor with levels }
#'           \item{\code{FinalResultValue}}{a numeric vector}
#'           \item{\code{FinalResultHalfMDL}}{a numeric vector}
#'           \item{\code{Analyte}}{a factor with levels }
#'           \item{\code{ChemSampleID}}{a character vector}
#'           \item{\code{ResultValue}}{a numeric vector}
#'           \item{\code{ConvertTo}}{a factor with levels }
#'           \item{\code{ElevCategory}}{a character vector}
#' }
#' 
#' @source example data
"data_Chem"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ******NEEDS MORE ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_ChemInfo ####
#' @title Chem Parameters
#' 
#' @description Chem Parameters
#' 
#' @format A data frame with 284 observations on the following 16 variables.
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
#' }
#'
#' @source example data
"data_ChemInfo"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data_BMIRelAbund ####
#' @title BMI Relative Abundance
#' 
#' @description Benthic Macroinvertebrate, Relative Abundances
#' 
#' @format A data frame with 55,319 rows and 11 variables
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
#' }
#' 
#' @source example data
"data_BMIRelAbund"
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
#'           \item{\code{WQX_UnidentifiedSpecies}}{a factor with levels }
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
#' }
#' 
#' @source example data, Arizona
"data_BMIMasterTaxa"
