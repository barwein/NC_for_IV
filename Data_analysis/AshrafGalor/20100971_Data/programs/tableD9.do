/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/***********************************************************************************/
/*** TABLE D9: ANCESTRY-ADJUSTED MIGRATORY DISTANCE VERSUS ALTERNATIVE DISTANCES ***/
/***********************************************************************************/

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

reg ln_gdppc2000 mdist_addis_aa mdist_addis_aa_sqr if cleangdp == 1, rob ;
outreg2 using "results\tableD9", replace tex nocon 
	sortvar(mdist_addis_aa mdist_addis_aa_sqr mdist_addis mdist_addis_sqr aerial aerial_sqr aerial_aa aerial_aa_sqr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 2 */
/* -------- */

reg ln_gdppc2000 mdist_addis_aa mdist_addis_aa_sqr mdist_addis mdist_addis_sqr if cleangdp == 1, rob ;
outreg2 using "results\tableD9", tex nocon 
	sortvar(mdist_addis_aa mdist_addis_aa_sqr mdist_addis mdist_addis_sqr aerial aerial_sqr aerial_aa aerial_aa_sqr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 3 */
/* -------- */

reg ln_gdppc2000 mdist_addis_aa mdist_addis_aa_sqr aerial aerial_sqr if cleangdp == 1, rob ;
outreg2 using "results\tableD9", tex nocon 
	sortvar(mdist_addis_aa mdist_addis_aa_sqr mdist_addis mdist_addis_sqr aerial aerial_sqr aerial_aa aerial_aa_sqr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 4 */
/* -------- */

reg ln_gdppc2000 mdist_addis_aa mdist_addis_aa_sqr aerial_aa aerial_aa_sqr if cleangdp == 1, rob ;
outreg2 using "results\tableD9", tex nocon 
	sortvar(mdist_addis_aa mdist_addis_aa_sqr mdist_addis mdist_addis_sqr aerial aerial_sqr aerial_aa aerial_aa_sqr) 
	bdec(3) tdec(3) rdec(3) ;

exit ;
