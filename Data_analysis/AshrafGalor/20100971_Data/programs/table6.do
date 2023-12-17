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

		local divlist = "pdiv_aa pdiv_aa_sqr" ;
		local hasdivs : list divlist in varlist ;

		if `hasdivs' == 1 { ;
			wherext pdiv_aa pdiv_aa_sqr ;
		} ;
		else { ;
			wherext pdiv pdiv_sqr ;
		} ;
		matrix coef = (e(b), r(argext)) ;
	} ;
end ;

capture program drop tse_boot ;
program tse_boot, eclass ;
	version 10.1 ;
	syntax varlist [if] ;
	quietly { ;
		use "data\ethnic.dta", clear ;
		preserve ;
		bsample 1, strata(country);
		regress adiv mdist ;
		restore ;

		use "data\country.dta", clear ;
		marksample touse ;
		preserve ;
		bsample if `touse' ;

		local divlist = "pdiv_aa pdiv_aa_sqr" ;
		local hasdivs : list divlist in varlist ;

		if `hasdivs' == 1 { ;
			replace pdiv_aa = _b[_cons] + _b[mdist] * mdist_addis_aa ;
			replace pdiv_aa_sqr = pdiv_aa ^ 2 ;
		} ;
		else { ;
			replace pdiv = _b[_cons] + _b[mdist] * mdist_addis ;
			replace pdiv_sqr = pdiv ^ 2 ;
		} ;
		regress `varlist' ;
		if `hasdivs' == 1 { ;
			wherext pdiv_aa pdiv_aa_sqr ;
		} ;
		else { ;
			wherext pdiv pdiv_sqr ;
		} ;
		ereturn scalar eopt = r(argext) ;
		restore ;
	} ;
end ;

/**************************************************************************/
/*** TABLE 6: DIVERSITY AND ECONOMIC DEVELOPMENT IN 2000 CE AND 1500 CE ***/
/**************************************************************************/

/* -------- */
/* COLUMN 1 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
simulate _b opt = e(eopt), reps(1000) seed(54345):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
bstat, stat(coef) ;
outreg2 using "results\table6", replace tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst_aa _b_ln_yst _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_ln_pd1500 _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 2 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
simulate _b opt = e(eopt), reps(1000) seed(12321):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
bstat, stat(coef) ;
outreg2 using "results\table6", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst_aa _b_ln_yst _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_ln_pd1500 _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 3 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg ln_pd1500 africa europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg ln_pd1500 africa europe asia americas if cleancomp == 1 ;
simulate _b opt = e(eopt), reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg ln_pd1500 africa europe asia americas if cleancomp == 1 ;
bstat, stat(coef) ;
outreg2 using "results\table6", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst_aa _b_ln_yst _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_ln_pd1500 _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 4 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
simulate _b opt = e(eopt), reps(1000) seed(21212):
tse_boot ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;
bstat, stat(coef) ;
outreg2 using "results\table6", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _b_ln_yst_aa _b_ln_yst _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_ln_pd1500 _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

exit ;
