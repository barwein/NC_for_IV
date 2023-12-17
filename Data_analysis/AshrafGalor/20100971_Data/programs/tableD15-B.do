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
		matrix coef = e(b) ;
	} ;
end ;

capture program drop tse_boot ;
program tse_boot, eclass ;
	version 10.1 ;
	syntax varlist [if] ;
	quietly { ;
		use "data\ethnic.dta", clear ;
		preserve ;
		bsample 1, strata(country) ;
		regress adiv mdist ;
		restore ;

		use "data\country.dta", clear ;
		marksample touse ;
		preserve ;
		bsample if `touse' ;
		replace pdiv = _b[_cons] + _b[mdist] * mdist_addis ;
		replace pdiv_sqr = pdiv ^ 2 ;
		regress `varlist' ;
		restore ;
	} ;
end ;

/****************************************************************************************************************/
/*** TABLE D15 (PANEL B): ROBUSTNESS TO USING (NONLOGGED) NEOLITHIC TRANSITION TIMING WITH RESPECT TO 1500 CE ***/
/****************************************************************************************************************/

/* -------- */
/* COLUMN 1 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var in adiv adiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg { ;
	rename `var' _b_`var' ;
} ;
reg ln_pd1500 _b_adiv _b_adiv_sqr _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg if cleanlim == 1, robust ;
outreg2 using "results\tableD15-B", replace tex nocon 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
restore ;

/* -------- */
/* COLUMN 2 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var in adiv adiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg africa europe asia { ;
	rename `var' _b_`var' ;
} ;
reg ln_pd1500 _b_adiv _b_adiv_sqr _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_africa _b_europe _b_asia if cleanlim == 1, robust ;
outreg2 using "results\tableD15-B", tex nocon 
	drop(_b_africa _b_europe _b_asia) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
restore ;

/* -------- */
/* COLUMN 3 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var in adiv adiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg { ;
	rename `var' _b_`var' ;
} ;
reg _b_adiv mdist_addis _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg if cleanlim == 1, robust ;
predict adivhat if cleanlim == 1, xb ;
gen adivhat_sqr = adivhat ^ 2 ;
ivreg2 ln_pd1500 (_b_adiv _b_adiv_sqr = mdist_addis adivhat_sqr) _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg if cleanlim == 1, robust ;
outreg2 using "results\tableD15-B", tex nocon nor2 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
drop adivhat adivhat_sqr ;
restore ;

/* -------- */
/* COLUMN 4 */
/* -------- */

use "data\country.dta", clear ;
preserve ;
foreach var in adiv adiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg africa europe asia { ;
	rename `var' _b_`var' ;
} ;
reg _b_adiv mdist_addis _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_africa _b_europe _b_asia if cleanlim == 1, robust ;
predict adivhat if cleanlim == 1, xb ;
gen adivhat_sqr = adivhat ^ 2 ;
ivreg2 ln_pd1500 (_b_adiv _b_adiv_sqr = mdist_addis adivhat_sqr) _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg _b_africa _b_europe _b_asia if cleanlim == 1, robust ;
outreg2 using "results\tableD15-B", tex nocon nor2 
	drop(_b_africa _b_europe _b_asia) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) rdec(3) ;
drop adivhat adivhat_sqr ;
restore ;

/* -------- */
/* COLUMN 5 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1500 pdiv pdiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg if cleanpd1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1500 pdiv pdiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg if cleanpd1500 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1500 pdiv pdiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg if cleanpd1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD15-B", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

/* -------- */
/* COLUMN 6 */
/* -------- */

use "data\country.dta", clear ;
qui reg ln_pd1500 pdiv pdiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
scalar nobs = e(N) ;
scalar rsqd = e(r2) ;
get_coef ln_pd1500 pdiv pdiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
simulate _b, reps(1000) seed(12345):
tse_boot ln_pd1500 pdiv pdiv_sqr yst1500_scaled ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;
bstat, stat(coef) ;
outreg2 using "results\tableD15-B", tex 
	addstat("Observations", nobs, "R-squared", rsqd) nor2 noobs 
	drop(_b_cons _b_africa _b_europe _b_asia _b_americas) 
	sortvar(_b_adiv _b_adiv_sqr _b_pdiv _b_pdiv_sqr _b_yst1500_scaled _b_ln_arable _b_ln_abslat _b_ln_suitavg) 
	bdec(3) tdec(3) ;
matrix drop _all ;
scalar drop _all ;

exit ;
