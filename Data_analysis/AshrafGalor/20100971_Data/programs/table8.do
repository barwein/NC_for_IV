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
		wherext pdiv_aa pdiv_aa_sqr ;
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
		replace pdiv_aa = _b[_cons] + _b[mdist] * mdist_addis_aa ;
		replace pdiv_aa_sqr = pdiv_aa ^ 2 ;
		regress `varlist' ;
		wherext pdiv_aa pdiv_aa_sqr ;
		ereturn scalar eopt = r(argext) ;
		restore ;
	} ;
end ;

/***********************************************************/
/*** TABLE 8: ADDRESSING ENDOGENOUS POST-1500 MIGRATIONS ***/
/***********************************************************/

/* -------- */
/* COLUMN 1 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 ;
simulate _b opt = e(eopt), reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 ;
bstat, stat(coef) ;
outreg2 using "results\table8", replace tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_opec _b_legor_uk _b_legor_fr _b_legor_ge _b_legor_so _b_pmuslim _b_pcatholic _b_pprotest _b_europe _b_asia _b_oceania _b_americas _b_wb_ssa) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_socinf _b_efrac _b_malfal _b_kgatr _b_distcr _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 2 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & oecd == 0 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & oecd == 0 ;
simulate _b opt = e(eopt), reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & oecd == 0 ;
bstat, stat(coef) ;
outreg2 using "results\table8", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_opec _b_legor_uk _b_legor_fr _b_pmuslim _b_pcatholic _b_pprotest _b_europe _b_asia _b_oceania _b_americas _b_wb_ssa) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_socinf _b_efrac _b_malfal _b_kgatr _b_distcr _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 3 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & western == 0 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & western == 0 ;
simulate _b opt = e(eopt), reps(1000) seed(12345):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & western == 0 ;
bstat, stat(coef) ;
outreg2 using "results\table8", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_opec _b_legor_uk _b_legor_fr _b_legor_ge _b_legor_so _b_pmuslim _b_pcatholic _b_pprotest _b_europe _b_asia _b_oceania _b_americas _b_wb_ssa) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_socinf _b_efrac _b_malfal _b_kgatr _b_distcr _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 4 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & wb_lac == 0 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & wb_lac == 0 ;
simulate _b opt = e(eopt), reps(1000) seed(12321):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 & wb_lac == 0 ;
bstat, stat(coef) ;
outreg2 using "results\table8", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_opec _b_legor_uk _b_legor_fr _b_legor_ge _b_legor_so _b_pmuslim _b_pcatholic _b_pprotest _b_europe _b_asia _b_oceania _b_americas _b_wb_ssa) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_socinf _b_efrac _b_malfal _b_kgatr _b_distcr _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 5 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest africa europe asia americas if cleangdp == 1 & wb_ssa == 0 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest africa europe asia americas if cleangdp == 1 & wb_ssa == 0 ;
simulate _b opt = e(eopt), reps(1000) seed(31213):
tse_boot ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest africa europe asia americas if cleangdp == 1 & wb_ssa == 0 ;
bstat, stat(coef) ;
outreg2 using "results\table8", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_opec _b_legor_uk _b_legor_fr _b_legor_ge _b_legor_so _b_pmuslim _b_pcatholic _b_pprotest _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_socinf _b_efrac _b_malfal _b_kgatr _b_distcr _eq2_opt) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 6 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var of varlist pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia wb_ssa { ;
	rename `var' _b_`var' ;
} ;
reg ln_gdppc2000 _b_pdiv_aa _b_pdiv_aa_sqr _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_socinf _b_efrac _b_malfal _b_kgatr _b_distcr _b_opec _b_legor_uk _b_legor_fr _b_legor_ge _b_legor_so _b_pmuslim _b_pcatholic _b_pprotest _b_europe _b_asia _b_wb_ssa if cleangdp == 1 & pnative > 0.97 ;
wherext _b_pdiv_aa _b_pdiv_aa_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\table8", tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	drop(_b_opec _b_legor_uk _b_legor_fr _b_legor_ge _b_legor_so _b_pmuslim _b_pcatholic _b_pprotest _b_europe _b_asia _b_wb_ssa) 
	sortvar(_b_pdiv_aa _b_pdiv_aa_sqr _b_ln_yst_aa _b_ln_arable _b_ln_abslat _b_socinf _b_efrac _b_malfal _b_kgatr _b_distcr _eq2_opt) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;
restore ;

exit ;
