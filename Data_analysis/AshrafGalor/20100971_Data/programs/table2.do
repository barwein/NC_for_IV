/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/****************************************************************************************/
/*** TABLE 2: MIGRATORY DISTANCE FROM EAST AFRICA AND ECONOMIC DEVELOPMENT IN 1500 CE ***/
/****************************************************************************************/

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

reg ln_pd1500 mdist_hgdp mdist_hgdp_sqr if cleanlim == 1, robust ;
outreg2 using "results\table2", replace tex nocon 
	sortvar(adiv adiv_sqr mdist_hgdp mdist_hgdp_sqr hmicost_hgdp hmicost_hgdp_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 2 */
/* -------- */

reg ln_pd1500 adiv adiv_sqr mdist_hgdp mdist_hgdp_sqr if cleanlim == 1, robust ;
test adiv adiv_sqr ;
scalar divp = r(p) ;
test mdist_hgdp mdist_hgdp_sqr ;
scalar dstp = r(p) ;
outreg2 using "results\table2", tex nocon 
	addstat("P-val. for joint significance of adiv and adiv_sqr", divp, "P-val. for joint significance of mdist_hgdp and mdist_hgdp_sqr", dstp) 
	sortvar(adiv adiv_sqr mdist_hgdp mdist_hgdp_sqr hmicost_hgdp hmicost_hgdp_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

/* -------- */
/* COLUMN 3 */
/* -------- */

reg ln_pd1500 hmicost_hgdp hmicost_hgdp_sqr if cleanlim == 1, robust ;
outreg2 using "results\table2", tex nocon 
	sortvar(adiv adiv_sqr mdist_hgdp mdist_hgdp_sqr hmicost_hgdp hmicost_hgdp_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 4 */
/* -------- */

reg ln_pd1500 adiv adiv_sqr hmicost_hgdp hmicost_hgdp_sqr if cleanlim == 1, robust  ;
test adiv adiv_sqr ;
scalar divp = r(p) ;
test hmicost_hgdp hmicost_hgdp_sqr ;
scalar hmip = r(p) ;
outreg2 using "results\table2", tex nocon 
	addstat("P-val. for joint significance of adiv and adiv_sqr", divp, "P-val. for joint significance of hmicost_hgdp and hmicost_hgdp_sqr", hmip) 
	sortvar(adiv adiv_sqr mdist_hgdp mdist_hgdp_sqr hmicost_hgdp hmicost_hgdp_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

/* -------- */
/* COLUMN 5 */
/* -------- */

reg adiv mdist_addis ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1, robust ;
predict adivhat if cleanlim == 1, xb ;
gen adivhat_sqr = adivhat ^ 2 ;
ivreg2 ln_pd1500 (adiv adiv_sqr = mdist_addis adivhat_sqr) ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1, robust ;
outreg2 using "results\table2", tex nocon nor2 
	sortvar(adiv adiv_sqr mdist_hgdp mdist_hgdp_sqr hmicost_hgdp hmicost_hgdp_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
drop adivhat adivhat_sqr ;

/* -------- */
/* COLUMN 6 */
/* -------- */

reg adiv mdist_addis ln_yst ln_arable ln_abslat ln_suitavg africa europe americas if cleanlim == 1, robust ;
predict adivhat if cleanlim == 1, xb ;
gen adivhat_sqr = adivhat ^ 2 ;
ivreg2 ln_pd1500 (adiv adiv_sqr = mdist_addis adivhat_sqr) ln_yst ln_arable ln_abslat ln_suitavg africa europe americas if cleanlim == 1, robust ;
outreg2 using "results\table2", tex nocon nor2 
	drop(africa europe americas) 
	sortvar(adiv adiv_sqr mdist_hgdp mdist_hgdp_sqr hmicost_hgdp hmicost_hgdp_sqr ln_yst ln_arable ln_abslat ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
drop adivhat adivhat_sqr ;

exit ;
