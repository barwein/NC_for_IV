/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/*************************************************************************************/
/*** TABLE D2: THE RESULTS OF TABLE 1 WITH CORRECTIONS FOR SPATIAL AUTOCORRELATION ***/
/*************************************************************************************/

capture log close ;
log using "results\TableD2.log", replace ;

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 adiv adiv_sqr conley_const, xreg(3) coord(2) ;
restore ;

/* -------- */
/* COLUMN 2 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 ln_yst conley_const, xreg(2) coord(2) ;
restore ;

/* -------- */
/* COLUMN 3 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 ln_arable ln_abslat ln_suitavg conley_const, xreg(4) coord(2) ;
restore ;

/* -------- */
/* COLUMN 4 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg conley_const, xreg(7) coord(2) ;
restore ;

/* -------- */
/* COLUMN 5 */
/* -------- */

preserve ;
drop if cleanlim == 0 ;
x_ols2 conley_coord1 conley_coord2 conley_cutoff1 conley_cutoff2 
       ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe americas conley_const, xreg(10) coord(2) ;
restore ;

log close ;

exit ;
