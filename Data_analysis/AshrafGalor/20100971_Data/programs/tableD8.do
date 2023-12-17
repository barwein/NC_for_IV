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

/****************************************************************************/
/*** TABLE D8: ROBUSTNESS TO THE MODE OF SUBSISTENCE PREVALENT IN 1000 CE ***/
/****************************************************************************/

/* -------- */
/* COLUMN 1 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var in adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia { ;
	rename `var' _b_`var' ;
} ;
reg ln_pd1500 _b_adiv _b_adiv_sqr _b_ln_yst _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_europe _b_asia if cleanlim == 1 & ln_submode1000 != ., robust ;
outreg2 using "results\tableD8", replace tex nocon 
	drop(_b_europe _b_asia) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_submode1000 _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
restore ;

/* -------- */
/* COLUMN 2 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var in adiv adiv_sqr ln_yst ln_submode1000 ln_arable ln_abslat ln_suitavg europe asia { ;
	rename `var' _b_`var' ;
} ;
reg ln_pd1500 _b_adiv _b_adiv_sqr _b_ln_yst _b_ln_submode1000 _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_europe _b_asia if cleanlim == 1 & _b_ln_submode1000 != ., robust ;
outreg2 using "results\tableD8", tex nocon 
	drop(_b_europe _b_asia) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_submode1000 _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
restore ;

/* -------- */
/* COLUMN 3 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 & ln_submode1000 != . ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 & ln_submode1000 != . ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 & ln_submode1000 != . ;
bstat, stat(coef) ;
outreg2 using "results\tableD8", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_submode1000 _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 4 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1500 pdiv pdiv_sqr ln_yst ln_submode1000 ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 & ln_submode1000 != . ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1500 pdiv pdiv_sqr ln_yst ln_submode1000 ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 & ln_submode1000 != . ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1500 pdiv pdiv_sqr ln_yst ln_submode1000 ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 & ln_submode1000 != . ;
bstat, stat(coef) ;
outreg2 using "results\tableD8", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_submode1000 _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 5 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1000 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 & ln_submode1000 != . ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1000 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 & ln_submode1000 != . ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1000 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 & ln_submode1000 != . ;
bstat, stat(coef) ;
outreg2 using "results\tableD8", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_submode1000 _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 6 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1000 pdiv pdiv_sqr ln_yst ln_submode1000 ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 & ln_submode1000 != . ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1000 pdiv pdiv_sqr ln_yst ln_submode1000 ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 & ln_submode1000 != . ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1000 pdiv pdiv_sqr ln_yst ln_submode1000 ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 & ln_submode1000 != . ;
bstat, stat(coef) ;
outreg2 using "results\tableD8", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst _b_ln_submode1000 _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

exit ;
