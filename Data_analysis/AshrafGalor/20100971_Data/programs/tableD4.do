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

		local divlist = "ln_pdiv ln_pdiv_sqr" ;
		local hasdivs : list divlist in varlist ;

		if `hasdivs' == 1 { ;
			replace pdiv = _b[_cons] + _b[mdist] * mdist_addis ;
			replace ln_pdiv = ln(1 + pdiv) ;
			replace ln_pdiv_sqr = ln_pdiv ^ 2 ;
		} ;
		else { ;
			replace pdiv_aa = _b[_cons] + _b[mdist] * mdist_addis_aa ;
			replace ln_pdiv_aa = ln(1 + pdiv_aa) ;
			replace ln_pdiv_aa_sqr = ln_pdiv_aa ^ 2 ;
		} ;
		regress `varlist' ;
		restore ;
	} ;
end ;

/******************************************************/
/*** TABLE D4: ROBUSTNESS TO LOG-LOG SPECIFICATIONS ***/
/******************************************************/

/* -------- */
/* COLUMN 1 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var in ln_adiv ln_adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg { ;
	rename `var' _b_`var' ;
} ;
reg ln_pd1500 _b_ln_adiv _b_ln_adiv_sqr _b_ln_yst _b_ln_arable _b_ln_abslat _b_ln_suitavg if cleanlim == 1, robust ;
outreg2 using "results\tableD4", replace tex nocon 
	sortvar(_b_ln_adiv _b_ln_adiv_sqr _b_ln_pdiv _b_ln_pdiv_sqr _b_ln_pdiv_aa _b_ln_pdiv_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
restore ;

/* -------- */
/* COLUMN 2 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var in ln_adiv ln_adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia { ;
	rename `var' _b_`var' ;
} ;
reg ln_pd1500 _b_ln_adiv _b_ln_adiv_sqr _b_ln_yst _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_africa _b_europe _b_asia if cleanlim == 1, robust ;
outreg2 using "results\tableD4", tex nocon 
	drop(_b_africa _b_europe _b_asia) 
	sortvar(_b_ln_adiv _b_ln_adiv_sqr _b_ln_pdiv _b_ln_pdiv_sqr _b_ln_pdiv_aa _b_ln_pdiv_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
restore ;

/* -------- */
/* COLUMN 3 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1500 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1500 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1500 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD4", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_ln_adiv _b_ln_adiv_sqr _b_ln_pdiv _b_ln_pdiv_sqr _b_ln_pdiv_aa _b_ln_pdiv_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 4 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1000 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1000 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1000 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD4", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_ln_adiv _b_ln_adiv_sqr _b_ln_pdiv _b_ln_pdiv_sqr _b_ln_pdiv_aa _b_ln_pdiv_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 5 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1 ln_pdiv ln_pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD4", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_ln_adiv _b_ln_adiv_sqr _b_ln_pdiv _b_ln_pdiv_sqr _b_ln_pdiv_aa _b_ln_pdiv_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 6 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 ln_pdiv_aa ln_pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 ln_pdiv_aa ln_pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_gdppc2000 ln_pdiv_aa ln_pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD4", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_ln_adiv _b_ln_adiv_sqr _b_ln_pdiv _b_ln_pdiv_sqr _b_ln_pdiv_aa _b_ln_pdiv_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

exit ;
