/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/****************************************************************************************************/
/*** TABLE D14: INTERPERSONAL TRUST, SCIENTIFIC PRODUCTIVITY, AND ECONOMIC DEVELOPMENT IN 2000 CE ***/
/****************************************************************************************************/

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

reg ln_gdppc2000 trust if cleangdptrust==1, robust ;
outreg2 using "results\tableD14", replace tex nocon 
	sortvar(trust articles ln_yst_aa ln_arable ln_abslat ln_suitavg xconst efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 2 */
/* -------- */

reg ln_gdppc2000 trust ln_yst_aa ln_arable ln_abslat ln_suitavg europe asia oceania americas wb_ssa if cleangdptrust==1, robust ;
outreg2 using "results\tableD14", tex nocon 
	drop(europe asia oceania americas wb_ssa) 
	sortvar(trust articles ln_yst_aa ln_arable ln_abslat ln_suitavg xconst efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 3 */
/* -------- */

reg ln_gdppc2000 trust ln_yst_aa ln_arable ln_abslat ln_suitavg xconst efrac malfal kgatr distcr opec legor_uk legor_fr europe asia oceania americas wb_ssa if cleangdptrust==1, robust ;
outreg2 using "results\tableD14", tex nocon 
	drop(opec legor_uk legor_fr europe asia oceania americas wb_ssa) 
	sortvar(trust articles ln_yst_aa ln_arable ln_abslat ln_suitavg xconst efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 4 */
/* -------- */

reg ln_gdppc2000 articles if cleangdparticles==1, robust ;
outreg2 using "results\tableD14", tex nocon 
	sortvar(trust articles ln_yst_aa ln_arable ln_abslat ln_suitavg xconst efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 5 */
/* -------- */

reg ln_gdppc2000 articles ln_yst_aa ln_arable ln_abslat ln_suitavg europe asia oceania americas wb_ssa if cleangdparticles==1, robust ;
outreg2 using "results\tableD14", tex nocon 
	drop(europe asia oceania americas wb_ssa) 
	sortvar(trust articles ln_yst_aa ln_arable ln_abslat ln_suitavg xconst efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 6 */
/* -------- */

reg ln_gdppc2000 articles ln_yst_aa ln_arable ln_abslat ln_suitavg xconst efrac malfal kgatr distcr opec legor_uk legor_fr europe asia oceania americas wb_ssa if cleangdparticles==1, robust ;
outreg2 using "results\tableD14", tex nocon 
	drop(opec legor_uk legor_fr europe asia oceania americas wb_ssa) 
	sortvar(trust articles ln_yst_aa ln_arable ln_abslat ln_suitavg xconst efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;

exit ;
