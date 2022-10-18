******************************
*** Sample code
*** Last edit: october 17, 2022
*** This Do file creates a panel dataset with information on extension services visits to coffee farms, by year and program. Source: Cenicafe
*** Author: Ana Pirela
******************************

*-------------
*---  Setting and paths
*-------------
  
clear all
set more off

global pc "C:/Users/anapi/OneDrive - Universidad EAFIT/Trabajo_grado/Transformacion_cafe"
global datos_ori "${pc}/01_Original_data"
global datos "${pc}/02_Data"
cd "${pc}"

******************************
** 1. Organizing annual Cenicafe data 
******************************

forvalues y = 2007/2013{
	
	dis `y'
	use "${datos_ori}/labedu_`y'.dta", clear

	** Fix ID numbers
	
	* Clean ID numbers 
	cap tostring documento, format("%15.0f") replace
	cap gen cedula = nro_doc_ca
	cap gen cedula = documento
	replace cedula  = "" if regexm(cedula, "SIN.*")
	drop if cedula == ""
	replace cedula = subinstr(cedula, " ", "", .)
	replace cedula = subinstr(cedula, ".", "", .)
	replace cedula = subinstr(cedula, "-", "", .)
	replace cedula = subinstr(cedula, "'", "", .)
	replace cedula = subinstr(cedula, "E", "", .)
	replace cedula = subinstr(cedula, "TI", "", .)
	replace cedula = subinstr(cedula, "EL", "", .)
	replace cedula = subinstr(cedula, "ERR", "", .)
	replace cedula = subinstr(cedula, "|", "", .)
	replace cedula = subinstr(cedula, "/", "", .)
	replace cedula = subinstr(cedula, "T", "", .)
	replace cedula = subinstr(cedula, "?", "", .)
	replace cedula = subinstr(cedula, "QEPD", "", .)
	replace cedula  = "@" if regexm(cedula, "SIN.*")
	drop if cedula == "@"
	drop if cedula == "Menor"
	destring cedula, replace ignore (" ") force

	* Drop misreported IDs
	drop if cedula == 0
	drop if cedula == 1234
	drop if cedula < 1000
	tostring cedula, gen(cedula2) format("%15.0f")
	drop if strlen(cedula2) < 7
	drop cedula2

	** Fix program names 
	replace programa  = "TRANSFERENCIA DE TECNOLOGIA" if regexm(programa, "TRANSFERENCIA.*")
	replace programa  = "ASESORIA CREDITO" if regexm(programa, "DITO.*")
	replace programa  = "ASESORIA CREDITO" if regexm(programa, "ASESOR.*")
	replace programa  = "GESTION EMPRESARIAL" if regexm(programa, "GESTI.*")
	replace programa  = "OTROS" if regexm(programa, "OTROS.*")
	replace programa  = "CEDULACION CAFETERA" if regexm(programa, "CEDULACI.*")

	* Create program IDs: id_program
	cap gen id_program = 0
	replace id_program = 22  if programa == "GESTION EMPRESARIAL"
	replace id_program = 23  if programa == "COMPETITIVIDAD"
	replace id_program = 24  if programa == "ASESORIA CREDITO"
	replace id_program = 25  if programa == "TRANSFERENCIA DE TECNOLOGIA"
	replace id_program = 26  if programa == "CAFES ESPECIALES"
	replace id_program = 27  if programa == "GREMIALES"
	replace id_program = 28  if programa == "LIDERAZGO"
	replace id_program = 29  if programa == "CONVIVENCIA CIUDADANA"
	replace id_program = 30  if programa == "CEDULACION CAFETERA"
	replace id_program = 32  if programa == "DESARROLLO SOCIAL"
	replace id_program = 32  if programa == "SEGURIDAD ALIMENTARIA"
	replace id_program = 33  if programa == "OTROS"
	replace id_program = 34  if programa == "MINISTERIO DEL TRABAJO"

	** Fix administrative unit names
	replace departamento = "CAQUETA" if regexm(departamento, "CAQUE.*")
	replace departamento = "CHOCO" if regexm(departamento, "CHOC.*")
	replace departamento = "NARINO" if regexm(departamento, "NARI.*")
	replace departamento = "NORTE SANTANDER" if regexm(departamento, "NORTE.*")
	replace departamento = "VALLE CAUCA" if regexm(departamento, "VALLE.*")

	** Coffee growers can receive multiple visits from a same program in a year; we collapse by nunmber of visits per program and year
	dummieslab id_program
	drop id_program
	
	** Type of credit visit (only available 2013)
	if `y' == 2013{
		gen psf = 1 if credito == "PSF"
		gen otro_credito = 1 if credito == "OTR"
		recode psf . = 0
		recode otro_credito . = 0	
	    collapse (sum) id_program* psf otro_credito, by(cedula departamento municipio vereda) 
	}
	
	if `y' != 2013 {
		collapse (sum) id_program*, by(cedula departamento municipio vereda) 
	}

	order departamento municipio vereda cedula id_program* 

	* Drop duplicates in IDs
	duplicates drop cedula, force

	** Create year variable and save data set
	gen year = `y'

	compress
	save "${datos}/cenicafe/labedu_`y'.dta", replace
	
	}
	
******************************
** 2. Extension services panel 
******************************

** We open the first year data set (2007) and append the 2008-2013 data
use "${datos}/cenicafe/labedu_2007.dta", clear

forvalues y = 2008/2013{
	dis `y'
	append using "${datos}/cenicafe/labedu_`y'.dta"
	}

* Organizing panel variables
order departamento municipio vereda cedula year id_program_22 id_program_23 id_program_24 id_program_25 id_program_26 id_program_27 id_program_28 id_program_29 id_program_30 id_program_32 id_program_33 id_program_34 psf otro_credito
	
* We replace missing values in programs with 0 (farms did not received any visits of the program)
foreach v in id_program_22 id_program_23 id_program_24 id_program_25 id_program_26 id_program_27 id_program_28 id_program_29 id_program_30 id_program_32 id_program_33 id_program_34 psf otro_credito {
	recode `v' . = 0
	}
	
sort departamento municipio vereda cedula year id_program_22 id_program_23 id_program_24 id_program_25 id_program_26 id_program_27 id_program_28 id_program_29 id_program_30 id_program_32 id_program_33 id_program_34 psf otro_credito

duplicates r departamento municipio vereda cedula year

** Save panel data
compress
save "${datos}/cenicafe/labedu_wide_2007_2013.dta", replace

******************************
** 3. Coffee farms panel
******************************

** We open each coffee farms data set and save the coffee growers IDs and the farm ID

forvalues y = 2006/2013{
	
	dis `y'
	* Open and organize data set
	use "${pc}/01_Datos_originales/infocul/infocul_`y'.dta", clear
	rename ccod_munic cod_mpio
	rename ccod_depto cod_dpto
	rename ccod_vered cod_vereda
	rename ccod_finca cod_finca
	
	* Only keep unique farms 
	keep cod_dpto cod_mpio cod_vereda cod_finca cedula
	duplicates drop cod_dpto cod_mpio cod_vereda cod_finca cedula, force
	
	* Organize data set and save
	order cod_dpto cod_mpio cod_vereda cod_finca cedula
	gen year = `y'
	
	compress
	save "${datos}/cenicafe/infocul_cod_`y'.dta", replace
	
	}

** We append each year and keep only unique farms; since this is a panel, farms can have data for multiple years
use "${datos}/cenicafe/infocul_cod_2006.dta", replace

forvalues y = 2007/2013{
	dis `y'
	append using "${datos}/cenicafe/infocul_cod_`y'.dta"
	}

sort cod_dpto cod_mpio cod_finca cod_vereda cedula year

** Fix IDs
gen cedula2 = cedula

replace cedula = subinstr(cedula, " ", "", .)
replace cedula = subinstr(cedula, ".", "", .)
replace cedula = subinstr(cedula, "E", "", .)
replace cedula = subinstr(cedula, "-", "", .)
replace cedula = subinstr(cedula, "'", "", .)
replace cedula = subinstr(cedula, "TI", "", .)
replace cedula = subinstr(cedula, "EL", "", .)
replace cedula = subinstr(cedula, "ERR", "", .)
replace cedula = subinstr(cedula, "|", "", .)
replace cedula = subinstr(cedula, "/", "", .)
replace cedula = subinstr(cedula, "T", "", .)
replace cedula = subinstr(cedula, "?", "", .)
replace cedula = subinstr(cedula, "QEPD", "", .)

replace cedula  = "@" if regexm(cedula, "SIN.*")
drop if cedula == "@"
drop if cedula == "Menor"
drop if strlen(cedula2) < 7

destring cedula, replace ignore (" ") force
drop if cedula == .
drop cedula2

duplicates r cod_dpto cod_mpio cod_vereda cod_finca year
duplicates r cod_finca cedula year

** Save coffee growers and farms ID data set
save "${datos}/cenicafe/panel_finca_cedula_infocul.dta", replace

** Only unique IDS 
preserve
drop year
duplicates drop cod_dpto cod_mpio cod_vereda cod_finca cedula, force

save "${datos}/cenicafe/codigos_finca_cedula_infocul.dta", replace
restore

******************************
** 4. Coffee farms and extension services panel
******************************

** We create a balanced panel of all the farms in coffee growers data; we will later use this to append extension services yearly data
use "${datos}/cenicafe/panel_finca_cedula_infocul.dta", clear

* Create farms ID
tostring cedula, gen(cedula2) format("%15.0f")
egen id = concat(cod_dpto cod_mpio cod_vereda cod_finca cedula2)
egen finca_id = group(id) 
duplicates tag finca_id year, gen(dup)

duplicates r finca_id year

* Balancing panel by ID and year
xtset finca_id year
tsfill, full

** Completing missing years data
drop cedula2 id dup
destring cod_dpto cod_mpio cod_vereda cod_finca, replace
bysort finca_id: egen cedula2 = max(cedula)
bysort finca_id: egen cod_finca2 = max(cod_finca)
bysort finca_id: egen cod_vereda2 = max(cod_vereda)
bysort finca_id: egen cod_mpio2 = max(cod_mpio)
bysort finca_id: egen cod_dpto2 = max(cod_dpto)

** Organizing panel and save
drop cod_dpto cod_mpio cod_vereda cod_finca cedula

rename cod_dpto2 cod_dpto
rename cod_mpio2 cod_mpio
rename cod_vereda2 cod_vereda
rename cod_finca2 cod_finca
rename cedula2 cedula

order cod_dpto cod_mpio cod_vereda cod_finca cedula finca_id year

save"${datos}/cenicafe/panel_finca_cedula_infocul_bal.dta", replace

** Append extension services panel
use"${datos}/cenicafe/panel_finca_cedula_infocul_bal.dta", clear
format cedula %20.0g
merge m:1 cedula year using "${datos}/cenicafe/labedu_wide_2007_2013.dta"

* Keep only farms with extension services data
keep if _merge == 3
drop _merge

order departamento cod_dpto municipio cod_mpio vereda cod_vereda cod_finca finca_id cedula 
sort departamento cod_dpto municipio cod_mpio vereda cod_vereda cod_finca finca_id cedula year

** Save final panel with data on yearly extension services visits by program
save"${datos}/cenicafe/panel_sica_cenicafe.dta", replace


