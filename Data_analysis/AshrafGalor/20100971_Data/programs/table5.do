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

		local divlist = "pdiv_aa pdiv_aa_sqr pdiv pdiv_sqr" ;
		local hasdivs : list divlist in varlist ;

		if `hasdivs' == 1 { ;
			test pdiv_aa pdiv_aa_sqr ;
			scalar fpv1 = r(p) ;
			test pdiv pdiv_sqr ;
			scalar fpv2 = r(p) ;
			matrix beta = (e(b), fpv1, fpv2) ;
		} ;
		else { ;
			matrix beta = e(b) ;
		} ;
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

		local divlst1 = "pdiv_aa pdiv_aa_sqr" ;
		local divlst2 = "pdiv pdiv_sqr" ;
		local divlst3 = "pdiv_aa pdiv_aa_sqr pdiv pdiv_sqr" ;
		local hasdiv1 : list divlst1 in varlist ;
		local hasdiv2 : list divlst2 in varlist ;
		local hasdiv3 : list divlst3 in varlist ;

		if `hasdiv1' == 1 { ;
			replace pdiv_aa = _b[_cons] + _b[mdist] * mdist_addis_aa ;
			replace pdiv_aa_sqr = pdiv_aa ^ 2 ;
		} ;
		if `hasdiv2' == 1 { ;
			replace pdiv = _b[_cons] + _b[mdist] * mdist_addis ;
			replace pdiv_sqr = pdiv ^ 2 ;
		} ;
		regress `varlist' ;
		if `hasdiv3' == 1 { ;
			test pdiv_aa pdiv_aa_sqr ;
			ereturn scalar efp1 = r(p) ;
			test pdiv pdiv_sqr ;
			ereturn scalar efp2 = r(p) ;
		} ;
		restore ;
	} ;
end ;

/*****************************************************/
/*** TABLE 5: ADJUSTED VERSUS UNADJUSTED DIVERSITY ***/
/*****************************************************/

/* -------- */
/* COLUMN 1 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr if cleancomp == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr if cleancomp == 1 ;
bstat, stat(beta) ;
outreg2 using "results\table5", replace tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _eq2_fpv1 _eq2_fpv2) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 2 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr africa europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr africa europe asia americas if cleancomp == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr africa europe asia americas if cleancomp == 1 ;
bstat, stat(beta) ;
outreg2 using "results\table5", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _eq2_fpv1 _eq2_fpv2) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 3 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv pdiv_sqr if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv pdiv_sqr if cleancomp == 1 ;
simulate _b, reps(1000) seed(12321):
tse_boot ln_gdppc2000 pdiv pdiv_sqr if cleancomp == 1 ;
bstat, stat(beta) ;
outreg2 using "results\table5", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _eq2_fpv1 _eq2_fpv2) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 4 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv pdiv_sqr africa europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv pdiv_sqr africa europe asia americas if cleancomp == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdiv pdiv_sqr africa europe asia americas if cleancomp == 1 ;
bstat, stat(beta) ;
outreg2 using "results\table5", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _eq2_fpv1 _eq2_fpv2) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 5 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr pdiv pdiv_sqr if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr pdiv pdiv_sqr if cleancomp == 1 ;
simulate _b fpv1 = e(efp1) fpv2 = e(efp2), reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr pdiv pdiv_sqr if cleancomp == 1 ;
bstat, stat(beta) ;
outreg2 using "results\table5", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _eq2_fpv1 _eq2_fpv2) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 6 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr pdiv pdiv_sqr africa europe asia americas if cleancomp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr pdiv pdiv_sqr africa europe asia americas if cleancomp == 1 ;
simulate _b fpv1 = e(efp1) fpv2 = e(efp2), reps(1000) seed(10101):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr pdiv pdiv_sqr africa europe asia americas if cleancomp == 1 ;
bstat, stat(beta) ;
outreg2 using "results\table5", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_pdiv _b_pdiv_sqr _eq2_fpv1 _eq2_fpv2) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

exit ;
