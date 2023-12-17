/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/****************************************************/
/*** TABLE 9: THE COSTS AND BENEFITS OF DIVERSITY ***/
/****************************************************/

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

reg trust pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal rough kgatr distcr opec legor_ge legor_sc europe asia oceania americas wb_ssa if cleantrust == 1, robust ;
outreg2 using "results\table9", replace tex nocon 
	drop(rough opec legor_ge legor_sc europe asia oceania americas wb_ssa) 
	sortvar(pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr school) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 2 */
/* -------- */

reg trust pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal rough kgatr distcr school opec legor_ge legor_sc europe asia oceania americas wb_ssa if cleantrust == 1, robust ;
outreg2 using "results\table9", tex nocon 
	drop(rough opec legor_ge legor_sc europe asia oceania americas wb_ssa) 
	sortvar(pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr school) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 3 */
/* -------- */

reg trust pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal rough kgatr distcr school opec legor_ge legor_sc europe asia oceania americas wb_ssa if cleantrust == 1 & cleanarticles == 1, robust ;
outreg2 using "results\table9", tex nocon 
	drop(rough opec legor_ge legor_sc europe asia oceania americas wb_ssa) 
	sortvar(pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr school) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 4 */
/* -------- */

reg articles pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal rough kgatr distcr opec legor_uk legor_fr europe asia oceania americas wb_ssa if cleanarticles == 1, robust ;
outreg2 using "results\table9", tex nocon 
	drop(rough opec legor_uk legor_fr europe asia oceania americas wb_ssa) 
	sortvar(pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr school) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 5 */
/* -------- */

reg articles pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal rough kgatr distcr school opec legor_uk legor_fr europe asia oceania americas wb_ssa if cleanarticles == 1, robust ;
outreg2 using "results\table9", tex nocon 
	drop(rough opec legor_uk legor_fr europe asia oceania americas wb_ssa) 
	sortvar(pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr school) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 6 */
/* -------- */

reg articles pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal rough kgatr distcr school opec legor_uk legor_fr europe asia oceania americas wb_ssa if cleanarticles == 1 & cleantrust == 1, robust ;
outreg2 using "results\table9", tex nocon 
	drop(rough opec legor_uk legor_fr europe asia oceania americas wb_ssa) 
	sortvar(pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr school) 
	bdec(3) tdec(3) rdec(3) ;

exit ;
