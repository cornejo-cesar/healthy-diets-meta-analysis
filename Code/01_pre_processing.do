********************************************************************************
* This Stata script processes data for the MH - HD meta-analysis by performing 
* various transformations (Cohen's d - Hedges' g - Pearson's r - Fisher's z) and
* calculations on variables related to depression (depr), anxiety (anx), and stress.
*
* program written by Cesar Cornejo
*
** Last revised: 2024.16.20
********************************************************************************

clear
set more off

*Define directory
********************************************************************************

gl DRIVE "" //set directory

***************************************************
* Data to merge                                   *
***************************************************

import excel "$DRIVE/Datasets/Excel files/RoB to stata.xlsx", firstrow 
keep if report_id !=.
save "$DRIVE/Datasets/Excel files/RoB to stata.dta", replace
clear 

import excel "$DRIVE/Datasets/Excel files/Dataset description.xlsx", firstrow 
drop author
keep if report_id !=.
save "$DRIVE/Datasets/Excel files/Dataset description.dta", replace
clear 

***************************************************
* Data Import and Cleaning                        *
***************************************************

import excel "$DRIVE/Datasets/Excel files/Raw data 1.xlsx", sheet("data_lmic") firstrow

* We keep the observations whose ID is different from missing
keep if report_id!=.

*Generate author_year

gen author_clean = author
replace author_clean = subinstr(author_clean, "et al.", "", .)

gen author_year = author_clean + " " + string(year) 
sort year author

* Genenerate exposure_group
gen exposure_group = ""
replace exposure_group = "G1- Adherence and adequacy" if exposure_specific == "DP: Adherence to diet recs" | exposure_specific == "DP: Nutrient adequacy"
replace exposure_group = "G2- Dietary pattern (NRCD reducing)" if exposure_specific == "DP: DASH" | exposure_specific == "DP: Dietary Inflammatory Index" | exposure_specific == "DP: Mediterranean diet" | exposure_specific == "DP: DASH & DP: Mediterranean diet"
replace exposure_group = "G3- Diet diversity indices" if exposure_specific == "DP: Dietary diversity (MDD, IDDS)" | exposure_specific == "DP: Dietary Variety Score"
replace exposure_group = "G4- Diet quality indices" if exposure_specific == "DP: DQI (all)" | exposure_specific == "DP: DQS" | exposure_specific == "DP: DQQ" | exposure_specific == "DP: HEI (all)" | exposure_specific == "DP: GDI"
replace exposure_group = "G5- Factor analysis and others" if exposure_specific == "DP: Healthy" | exposure_specific == "DP: Unhealthy"  | exposure_specific == "DP: Factor analysis (EFA, PCA)"

tab exposure_group, m



***************************************************
* Standard Error (SE) and Standard Deviation (SD) *
***************************************************

foreach var in depr stress anx {
    gen `var'_pv = .
    replace `var'_pv = real(`var'_p_value) if real(`var'_p_value) != .
}

* SE and SD are calculated for depression, anxiety, and stress if the type 
*  is β (type_new == 4), using confidence interval limits and population size.

foreach var in depr anx stress {
	
    gen `var'_se = (`var'_upper_ci - `var'_lower_ci) / (2 * 1.96) if statistic == "β"  & `var'_upper_ci!=.
	replace `var'_se = (`var') / ( abs(invnormal(`var'_pv/2))) if statistic == "β" & `var'_upper_ci==.
    gen `var'_sd = `var'_se * sqrt(n) if statistic == "β"
	replace `var'_mean_low_sd = `var'_mean_low_se * sqrt(n) if statistic == "mean" & `var'_mean_low_se!=.
	replace `var'_mean_high_sd = `var'_mean_high_se * sqrt(n) if statistic == "mean" & `var'_mean_high_se!=.
}

* Calculate the pooled standard deviation

foreach var in depr anx stress {
	
    gen `var'_sd_pooled = sqrt((`var'_mean_low_sd^2 + `var'_mean_high_sd^2) / 2) if statistic == "mean"
	
}

***************************************************
* Effect Size Calculations                        *
***************************************************


* Cohen's d 
foreach var in depr anx stress {
    display `var'
    gen `var'_d = .
    replace `var'_d = ((ln(`var'))/ _pi)*sqrt(3) if statistic == "AOR" | statistic == "OR" | statistic == "RR" |statistic == "PR" 
    replace `var'_d = `var' / `var'_sd if statistic == "β" & `var'_sd!=.
	replace `var'_d = (`var'_mean_low - `var'_mean_high) / `var'_sd_pooled if statistic == "mean"
	replace `var'_d = (`var'* 1.6 / _pi) * sqrt(3) if statistic == "HR"
}


* Hedges' g
foreach var in depr anx stress {
    display `var'
    gen `var'_g = (`var'_d * (1 - (3 / ((4 * n) - 9))))
}


* Pearson's r
foreach var in depr anx stress {

    gen `var'_r = `var'_d / sqrt(`var'_d^2 + 4) if `var'_d!=.
	replace `var'_r = `var' if statistic == "r"
}


* Complete r missing
foreach var in depr anx stress {
	replace `var'_d = 2*`var'_r/sqrt(1-`var'_r^2) if statistic == "r"
	replace `var'_g = (`var'_d * (1 - (3 / ((4 * n) - 9)))) if statistic == "r"
}


* Exposure_scale:If the study exposure scale is "Unhealthy (-)", the
* values for Pearson's r and Hedges' g are reversed.
foreach var in depr anx stress {

    replace `var'_r = `var'_r * (-1) if change_sign == "Yes"
	replace `var'_g = `var'_g * (-1) if change_sign == "Yes"
	
}


***************************************************
* Fisher's z Transformation                       *
***************************************************

*Fisher's z transformation is applied to Pearson's r for each variable.
foreach var in depr anx stress {

    gen `var'_z = 0.5 * ln((1 + `var'_r) / (1 - `var'_r))
}


*Keep the variables relevant and order them.
keep unique_report unique_statistic effect_size_id report_id title author author_year year journal literature  literature_group study_design claim exposure_scale change_sign relationship_sign excluded reason exposure fns_diets exposure_group exposure_specific outcome direction mh_population fns_population pop_type demographic n sample_size female female_percentage age_range age_group mean_age age_sd country region ses adjusted_ses type_ses income_level diet_scale mh_scale statistic depr_d depr_g depr_r depr_z anx_d anx_g anx_r  anx_z stress_d stress_g stress_r stress_z 

order unique_report unique_statistic report_id effect_size_id title author author_year year journal literature  literature_group study_design claim exposure_scale change_sign relationship_sign  excluded reason exposure fns_diets exposure_group exposure_specific outcome direction mh_population fns_population pop_type demographic n sample_size female female_percentage age_range age_group mean_age age_sd country region ses adjusted_ses type_ses income_level diet_scale mh_scale statistic depr_d depr_g depr_r depr_z anx_d anx_g anx_r  anx_z stress_d stress_g stress_r stress_z 

drop if excluded == "Yes"

tab unique_report
format depr_d depr_g depr_r depr_z anx_d anx_g anx_r anx_z stress_d stress_g stress_r stress_z %9.3f

export excel using "$DRIVE/Datasets/Excel files/Raw data 2.xlsx", firstrow(varlabels) replace
save "$DRIVE/Datasets/Excel files/Raw data 2.dta", replace


***************************************************
* Merge final dataset                             *
***************************************************


use "$DRIVE/Datasets/Excel files/Raw data 2.dta", clear

merge m:1 report_id using "$DRIVE/Datasets/Excel files/RoB to stata.dta"
drop if _merge==2
drop _merge

merge m:1 report_id using "$DRIVE/Datasets/Excel files/Dataset description.dta"
keep if _merge==3
drop _merge

* Gen n_final
gen n_final = .
bysort pop_cohort_dataset_id: egen max_n = max(n)
replace n_final = max_n if pop_cohort_dataset_id != 155
replace n_final = n if pop_cohort_dataset_id == 155
drop max_n

keep unique_report unique_statistic report_id pop_cohort_dataset_id effect_size_id title author author_year year journal literature literature_group study_design claim exposure_scale change_sign relationship_sign excluded reason exposure fns_diets exposure_group exposure_specific outcome direction mh_population fns_population pop_type demographic n n_final sample_size sample_label female female_percentage age_range age_group mean_age age_sd country region ses adjusted_ses type_ses income_level diet_scale mh_scale statistic depr_d depr_g depr_r depr_z anx_d anx_g anx_r anx_z stress_d stress_g stress_r stress_z cohort wave description_data_set participation attrition predictor_measurement outcome_measurement confounding analysis_and_reporting

order unique_report unique_statistic report_id pop_cohort_dataset_id effect_size_id title author author_year year journal literature literature_group study_design claim exposure_scale change_sign relationship_sign excluded reason exposure fns_diets exposure_group exposure_specific outcome direction mh_population fns_population pop_type demographic n n_final sample_size sample_label female female_percentage age_range age_group mean_age age_sd country region ses adjusted_ses type_ses income_level diet_scale mh_scale cohort wave description_data_set participation attrition predictor_measurement outcome_measurement confounding analysis_and_reporting statistic depr_d depr_g depr_r depr_z anx_d anx_g anx_r anx_z stress_d stress_g stress_r stress_z 

format depr_d depr_g depr_r depr_z anx_d anx_g anx_r anx_z stress_d stress_g stress_r stress_z %9.3f
export excel using "$DRIVE/Datasets/Excel files/Raw data 3.xlsx", firstrow(varlabels) replace

* Drop temporary files
erase "$DRIVE/Datasets/Excel files/RoB to stata.dta"
erase "$DRIVE/Datasets/Excel files/Dataset description.dta"
erase "$DRIVE/Datasets/Excel files/Raw data 2.dta"

