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
		regress adiv hmicost ;
		restore ;

		use "data\country.dta", clear ;
		marksample touse ;
		preserve ;
		bsample if `touse' ;

		local divlist = "pdivhmi pdivhmi_sqr" ;
		local hasdivs : list divlist in varlist ;

		if `hasdivs' == 1 { ;
			replace pdivhmi = _b[_cons] + _b[hmicost] * hmicost_addis ;
			replace pdivhmi_sqr = pdivhmi ^ 2 ;
		} ;
		else { ;
			replace pdivhmi_aa = _b[_cons] + _b[hmicost] * hmicost_addis_aa ;
			replace pdivhmi_aa_sqr = pdivhmi_aa ^ 2 ;
		} ;
		regress `varlist' ;
		restore ;
	} ;
end ;

/*********************************************************************************************/
/*** TABLE D5: ROBUSTNESS TO USING GENETIC DIVERSITY PREDICTED BY THE HUMAN MOBILITY INDEX ***/
/*********************************************************************************************/

/* -------- */
/* COLUMN 1 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1500 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1500 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1500 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1500 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD5", replace tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_europe _b_asia _b_americas) 
	sortvar(_b_pdivhmi _b_pdivhmi_sqr _b_pdivhmi_aa _b_pdivhmi_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 2 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1000 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1000 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1000 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1000 == 1 ;
simulate _b, reps(1000) seed(45154):
tse_boot ln_pd1000 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1000 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD5", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_europe _b_asia _b_americas) 
	sortvar(_b_pdivhmi _b_pdivhmi_sqr _b_pdivhmi_aa _b_pdivhmi_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 3 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1 == 1 ;
simulate _b, reps(1000) seed(41214):
tse_boot ln_pd1 pdivhmi pdivhmi_sqr ln_yst ln_arable ln_abslat ln_suitavg europe asia americas if cleanpd1 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD5", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_europe _b_asia _b_americas) 
	sortvar(_b_pdivhmi _b_pdivhmi_sqr _b_pdivhmi_aa _b_pdivhmi_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 4 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdivhmi_aa pdivhmi_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdivhmi_aa pdivhmi_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg europe asia americas if cleancomp == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdivhmi_aa pdivhmi_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg europe asia americas if cleancomp == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD5", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_europe _b_asia _b_americas) 
	sortvar(_b_pdivhmi _b_pdivhmi_sqr _b_pdivhmi_aa _b_pdivhmi_aa_sqr _b_ln_yst _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

exit ;
