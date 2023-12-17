/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/***********************************************************************/
/*** TABLE 1: OBSERVED DIVERSITY AND ECONOMIC DEVELOPMENT IN 1500 CE ***/
/***********************************************************************/

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

reg ln_pd1500 adiv adiv_sqr if cleanlim == 1, robust ;
wherext adiv adiv_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\table1", replace tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	sortvar(adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

/* -------- */
/* COLUMN 2 */
/* -------- */

reg ln_pd1500 ln_yst if cleanlim == 1, robust ;
outreg2 using "results\table1", tex nocon 
	sortvar(adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 3 */
/* -------- */

reg ln_pd1500 ln_arable ln_abslat ln_suitavg if cleanlim == 1, robust ;
outreg2 using "results\table1", tex nocon 
	sortvar(adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 4 */
/* -------- */

reg ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1, robust ;
wherext adiv adiv_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\table1", tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	sortvar(adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

/* -------- */
/* COLUMN 5 */
/* -------- */

reg ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe americas if cleanlim == 1, robust ;
wherext adiv adiv_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\table1", tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	drop(africa europe americas) 
	sortvar(adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

exit ;
