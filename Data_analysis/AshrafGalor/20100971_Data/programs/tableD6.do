/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/****************************************************************************************/
/*** TABLE D6: THE ZEROTH- AND FIRST-STAGE RESULTS OF THE 2SLS REGRESSIONS IN TABLE 2 ***/
/****************************************************************************************/

use "data\country.dta", clear ;

preserve ;

replace mdist_addis = mdist_addis / 100 ;

/* ----------- */
/* COLUMNS 1-3 */
/* ----------- */

reg adiv mdist_addis ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1, robust ;
outreg2 using "results\tableD6", replace tex nocon 
	sortvar(mdist_addis adivhat_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
predict adivhat if cleanlim == 1, xb ;
gen adivhat_sqr = adivhat ^ 2 ;
ivreg2 ln_pd1500 (adiv adiv_sqr = mdist_addis adivhat_sqr) ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1, robust savefirst ;
est restore _ivreg2_adiv ;
outreg2 using "results\tableD6", tex nocon 
	sortvar(mdist_addis adivhat_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
est restore _ivreg2_adiv_sqr ;
outreg2 using "results\tableD6", tex nocon 
	sortvar(mdist_addis adivhat_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
drop adivhat adivhat_sqr ;

/* ----------- */
/* COLUMNS 4-6 */
/* ----------- */

reg adiv mdist_addis ln_yst ln_arable ln_abslat ln_suitavg africa europe americas if cleanlim == 1, robust ;
outreg2 using "results\tableD6", tex nocon 
	drop(africa europe americas) 
	sortvar(mdist_addis adivhat_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
predict adivhat if cleanlim == 1, xb ;
gen adivhat_sqr = adivhat ^ 2 ;
ivreg2 ln_pd1500 (adiv adiv_sqr = mdist_addis adivhat_sqr) ln_yst ln_arable ln_abslat ln_suitavg africa europe americas if cleanlim == 1, robust savefirst ;
est restore _ivreg2_adiv ;
outreg2 using "results\tableD6", tex nocon 
	drop(africa europe americas) 
	sortvar(mdist_addis adivhat_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
est restore _ivreg2_adiv_sqr ;
outreg2 using "results\tableD6", tex nocon 
	drop(africa europe americas) 
	sortvar(mdist_addis adivhat_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
drop adivhat adivhat_sqr ;

restore ;

exit ;
