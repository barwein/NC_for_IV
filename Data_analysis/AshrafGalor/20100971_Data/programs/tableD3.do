/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/*************************************************************************************/
/*** TABLE D3: THE RESULTS OF TABLE 2 WITH CORRECTIONS FOR SPATIAL AUTOCORRELATION ***/
/*************************************************************************************/

capture log close ;
log using "results\TableD3.log", replace ;

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 mdist_hgdp mdist_hgdp_sqr conley_const, xreg(3) coord(2) ;
restore ;

/* -------- */
/* COLUMN 2 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 adiv adiv_sqr mdist_hgdp mdist_hgdp_sqr conley_const, xreg(5) coord(2) ;
restore ;

/* -------- */
/* COLUMN 3 */
/* -------- */

preserve ;
drop if cleanlim == 0 | hmicost_hgdp == . ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 hmicost_hgdp hmicost_hgdp_sqr conley_const, xreg(3) coord(2) ;
restore ;

/* -------- */
/* COLUMN 4 */
/* -------- */

preserve ;
drop if cleanlim == 0 | hmicost_hgdp == . ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 adiv adiv_sqr hmicost_hgdp hmicost_hgdp_sqr conley_const, xreg(5) coord(2) ;
restore ;

/* -------- */
/* COLUMN 5 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
reg adiv mdist_addis ln_yst ln_arable ln_abslat ln_suitavg, robust ;
predict adivhat, xb ;
gen adivhat_sqr = adivhat ^ 2 ;
x_gmm2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg conley_const 
	   mdist_addis adivhat_sqr ln_yst ln_arable ln_abslat ln_suitavg conley_const, xreg(7) inst(7) coord(2) ;
restore ;

/* -------- */
/* COLUMN 6 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
reg adiv mdist_addis ln_yst ln_arable ln_abslat ln_suitavg africa europe americas, robust ;
predict adivhat, xb ;
gen adivhat_sqr = adivhat ^ 2 ;
x_gmm2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe americas conley_const 
	   mdist_addis adivhat_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe americas conley_const, xreg(10) inst(10) coord(2) ;
restore ;

log close ;

exit ;
