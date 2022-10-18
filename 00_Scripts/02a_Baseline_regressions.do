******************************
*** Sample code
*** Last updated: october 17, 2022
*** Details: this Do runs the baseline regression (effect of armed conflict on coffee production) and 
*** the baseline regression by farm size (heterogeneous impact of armed conflict on coffee production)
******************************

*-------------
*---  Preliminaries
*-------------

*** Preparing Stata
clear all
cap restore
set more off
set matsize 800

*** Working Directories
global pc "C:/Users/user/OneDrive - Universidad del Magdalena(1)/Eafit/Paper_agricultura/Munoz_2020_ReplicationsFiles"
global data_path "${pc}/01_Original_data"
global tables "${pc}/04_Tables"
cd "${pc}"

*** Adding Ado-files
adopath + "${data_path}/adofiles"

******************************
*---  01. Open SICA/Census data set  
******************************

use "${data_path}/Munoz_2020_farm_level.dta", clear

******************************
*---  02. Defining variables of interest (sample, treatment)
******************************

** Baseline sample (Coffee growers located at 20km or closer)
keep if sample_20km == 1

** Definition of treatment period and treatment group
gen pre_post = (year >= 2002 & year <= 2006)
gen int_20km= pre_post*d_treat_sample20km

** Generating municipality trend
gen mpal_trend = codmpio*year

** Removing observations with no cultivated area info
drop if area_cul ==.

** Only keep coffee farmers present in 1997
keep if year > 1996

** Include labels
include "00_Do-files/06a_Labels_1.do"

******************************
*---  03. Baseline results (Table 4):
** Difference-in-Difference estimates of effects of exogenous exposure to violence on coffee production
******************************

*-- 03a. Defining the sub-sample
preserve
keep if type_labor == 1

** Defining base year and municipality settings 
eststo clear
fvset base 1996 year
fvset base 19355 codmpio			

*-- 03b. Difference-in-Difference
*- Column I: No Fixed Effects
eststo: reg area_cul int_20km d_treat_sample20km pre_post, cluster(codmpio)
qui summ area_cul
estadd scalar ymean = r(mean) 

*- Column II: Year fixed effects 
eststo: reg area_cul int_20km d_treat_sample20km pre_post i.year, cluster(codmpio)
qui summ area_cul
estadd scalar ymean = r(mean) 

*- Column III: Year and municipality fixed effects
eststo: reg area_cul int_20km d_treat_sample20km pre_post i.year i.codmpio, cluster(codmpio)
qui summ area_cul
estadd scalar ymean = r(mean) 

*- Column IV: Year and municipality fixed effects, and municipality-specific trends
eststo: reg area_cul int_20km d_treat_sample20km pre_post mpal_trend i.year i.codmpio, cluster(codmpio)
qui summ area_cul
estadd scalar ymean = r(mean) 

*-- 03c. Export results
esttab using "${tables}/Table_4_baseline_results.tex", fragment star(* 0.10 ** 0.05 *** 0.01) mlabels(none) nonumber nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) keep(int_20km d_treat_sample20km pre_post) noobs posthead("\\") eqlabels(none) collabels(none) stats(ymean N, fmt(3 0 3) labels("Mean of Dependent Variable" "\textbf{Observations}")) 
restore

******************************
*---  04. Baseline results by farm size (Table 5):
** Difference-in-Difference estimates of effects of exogenous exposure to violence on coffee production
******************************

*-- 04a. Defining the sub-sample
preserve
keep if type_labor == 1

** Generating farm size variable
bys ccod_finca: egen farm_size = max(area_total)
gen type_farm = (farm_size > 25)

** Defining base year and municipality settings 
eststo clear
fvset base 1996 year
fvset base 19355 codmpio

*-- 04b. Difference-in-Difference
forvalue i = 0/1 {
	*- Column I: No fixed effects 
	eststo: reg area_cul int_20km d_treat_sample20km pre_post if type_farm == `i', cluster(codmpio)
	qui summ area_cul if type_farm == `i'
	estadd scalar ymean = r(mean) 

	*- Column II: Year fixed effects 
	eststo: reg area_cul int_20km d_treat_sample20km pre_post i.year if type_farm == `i', cluster(codmpio)
	qui summ area_cul if type_farm == `i'
	estadd scalar ymean = r(mean) 

	*- Column III: Year and municipality fixed effects
	eststo: reg area_cul int_20km d_treat_sample20km pre_post i.year i.codmpio if type_farm == `i', cluster(codmpio)
	qui summ area_cul if type_farm == `i'
	estadd scalar ymean = r(mean) 

	*- Column IV: Year and municipality fixed effects, and municipality-specific trend
	eststo: reg area_cul int_20km d_treat_sample20km pre_post mpal_trend i.year i.codmpio if type_farm == `i', cluster(codmpio)
	qui summ area_cul if type_farm == `i'
	estadd scalar ymean = r(mean) 
	}

*-- 04c. Export results
esttab using "${tables}/Table_5_baseline_farm_size.tex", fragment star(* 0.10 ** 0.05 *** 0.01) mlabels(none) nonumber nomtitles replace brackets label se compress nogaps depvars se(%9.3f) b(%9.3f) keep(int_20km d_treat_sample20km pre_post) noobs posthead("\\") eqlabels(none) collabels(none) stats(ymean N, fmt(3 0 3) labels("Mean of Dependent Variable" "\textbf{Observations}")) 
restore


