/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/***********************************************************************************************************************************/
/*** TABLE D16 (PANEL A): ROBUSTNESS TO ALTERNATIVE DISTANCES UNDER (LOGGED) NEOLITHIC TRANSITION TIMING WITH RESPECT TO 1500 CE ***/
/***********************************************************************************************************************************/

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

reg ln_pd1500 mdist_addis mdist_addis_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg if cleanpd1500==1, robust ;
outreg2 using "results\tableD16-B", replace tex nocon 
	sortvar(mdist_addis mdist_addis_sqr aerial aerial_sqr mdist_london mdist_london_sqr mdist_tokyo mdist_tokyo_sqr mdist_mexico mdist_mexico_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 2 */
/* -------- */

reg ln_pd1500 aerial aerial_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg if cleanpd1500==1, robust ;
outreg2 using "results\tableD16-B", tex nocon 
	sortvar(mdist_addis mdist_addis_sqr aerial aerial_sqr mdist_london mdist_london_sqr mdist_tokyo mdist_tokyo_sqr mdist_mexico mdist_mexico_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 3 */
/* -------- */

reg ln_pd1500 mdist_london mdist_london_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg if cleanpd1500==1, robust ;
outreg2 using "results\tableD16-B", tex nocon 
	sortvar(mdist_addis mdist_addis_sqr aerial aerial_sqr mdist_london mdist_london_sqr mdist_tokyo mdist_tokyo_sqr mdist_mexico mdist_mexico_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 4 */
/* -------- */

reg ln_pd1500 mdist_tokyo mdist_tokyo_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg if cleanpd1500==1, robust ;
outreg2 using "results\tableD16-B", tex nocon 
	sortvar(mdist_addis mdist_addis_sqr aerial aerial_sqr mdist_london mdist_london_sqr mdist_tokyo mdist_tokyo_sqr mdist_mexico mdist_mexico_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 5 */
/* -------- */

reg ln_pd1500 mdist_mexico mdist_mexico_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg if cleanpd1500==1, robust ;
outreg2 using "results\tableD16-B", tex nocon 
	sortvar(mdist_addis mdist_addis_sqr aerial aerial_sqr mdist_london mdist_london_sqr mdist_tokyo mdist_tokyo_sqr mdist_mexico mdist_mexico_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

exit ;
