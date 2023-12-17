/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/****************/
/*** PROGRAMS ***/
/****************/

capture program drop get_coef ;
program get_coef ;
	version 10.1 ;
	syntax varlist [if] ;
	quietly { ;
		marksample touse ;
		regress `varlist' if `touse' ;
		matrix coef = e(b) ;
	} ;
end ;

capture program drop tse_boot ;
program tse_boot, eclass ;
	version 10.1 ;
	syntax varlist [if] ;
	quietly { ;
		use "data\ethnic.dta", clear ;
		preserve ;
		bsample 1, strata(country) ;
		regress adiv mdist ;
		restore ;

		use "data\country.dta", clear ;
		marksample touse ;
		preserve ;
		bsample if `touse' ;
		replace pdiv = _b[_cons] + _b[mdist] * mdist_addis ;
		replace pdiv_sqr = pdiv ^ 2 ;
		regress `varlist' ;
		restore ;
	} ;
end ;

/****************************************************************************************/
/*** TABLE D17: ROBUSTNESS TO ALTERNATIVE MEASURES OF ECONOMIC DEVELOPMENT IN 1500 CE ***/
/****************************************************************************************/

/* -------- */
/* COLUMN 1 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pop1500 pdiv pdiv_sqr if cleanpd1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pop1500 pdiv pdiv_sqr if cleanpd1500 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pop1500 pdiv pdiv_sqr if cleanpd1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD17", replace tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons) 
	sortvar(_b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_area_ar _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 2 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pop1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg if cleanpd1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pop1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg if cleanpd1500 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pop1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg if cleanpd1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD17", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons) 
	sortvar(_b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_area_ar _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 3 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pop1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pop1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
simulate _b, reps(1000) seed(54345):
tse_boot ln_pop1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD17", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_area_ar _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 4 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_ur1500 pdiv pdiv_sqr if cleanur1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_ur1500 pdiv pdiv_sqr if cleanur1500 == 1 ;
simulate _b, reps(1000) seed(32123):
tse_boot ln_ur1500 pdiv pdiv_sqr if cleanur1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD17", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons) 
	sortvar(_b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_area_ar _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 5 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_ur1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg if cleanur1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_ur1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg if cleanur1500 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_ur1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg if cleanur1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD17", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons) 
	sortvar(_b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_area_ar _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 6 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_ur1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg africa europe asia americas if cleanur1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_ur1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg africa europe asia americas if cleanur1500 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_ur1500 pdiv pdiv_sqr ln_yst ln_area_ar ln_abslat ln_suitavg africa europe asia americas if cleanur1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD17", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_area_ar _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

exit ;
