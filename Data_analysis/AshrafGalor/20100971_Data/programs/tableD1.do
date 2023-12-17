/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/*******************************************************************************************/
/*** TABLE D1: ROBUSTNESS OF THE ROLE OF MIGRATORY DISTANCE IN THE SERIAL FOUNDER EFFECT ***/
/*******************************************************************************************/

use "data\country.dta", clear ;

preserve ;

gen adiv100 = adiv * 100 ;

/* -------- */
/* COLUMN 1 */
/* -------- */

reg adiv100 mdist_addis if cleanlim == 1, robust ;
outreg2 using "results\tableD1", replace tex nocon 
	sortvar(mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr) 
	bdec(3) tdec(3) rdec(3) ;

/* -------- */
/* COLUMN 2 */
/* -------- */

reg adiv100 mdist_addis abslat arable suitavg if cleanlim == 1, robust ;
est store results ;
reg adiv100 abslat arable suitavg if cleanlim == 1, robust ;
predict res1 if cleanlim == 1, resid ;
reg mdist_addis abslat arable suitavg if cleanlim == 1, robust ;
predict res2 if cleanlim == 1, resid ;
reg res1 res2 ;
scalar pr2 = e(r2) ;
outreg2 [results] using "results\tableD1", tex nocon 
	addstat("Partial R2", pr2) 
	sortvar(mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr) 
	bdec(3) tdec(3) rdec(3) ;
est drop _all ;
scalar drop _all ;
drop res1 res2 ;

/* -------- */
/* COLUMN 3 */
/* -------- */

reg adiv100 mdist_addis abslat arable suitavg suitrng suitgini if cleanlim == 1, robust ;
est store results ;
reg adiv100 abslat arable suitavg suitrng suitgini if cleanlim == 1, robust ;
predict res1 if cleanlim == 1, resid ;
reg mdist_addis abslat arable suitavg suitrng suitgini if cleanlim == 1, robust ;
predict res2 if cleanlim == 1, resid ;
reg res1 res2 ;
scalar pr2 = e(r2) ;
outreg2 [results] using "results\tableD1", tex nocon 
	addstat("Partial R2", pr2) 
	sortvar(mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr) 
	bdec(3) tdec(3) rdec(3) ;
est drop _all ;
scalar drop _all ;
drop res1 res2 ;

/* -------- */
/* COLUMN 4 */
/* -------- */

reg adiv100 mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd if cleanlim == 1, robust ;
est store results ;
reg adiv100 abslat arable suitavg suitrng suitgini elevavg2 elevstd if cleanlim == 1, robust ;
predict res1 if cleanlim == 1, resid ;
reg mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd if cleanlim == 1, robust ;
predict res2 if cleanlim == 1, resid ;
reg res1 res2 ;
scalar pr2 = e(r2) ;
outreg2 [results] using "results\tableD1", tex nocon 
	addstat("Partial R2", pr2) 
	sortvar(mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr) 
	bdec(3) tdec(3) rdec(3) ;
est drop _all ;
scalar drop _all ;
drop res1 res2 ;

/* -------- */
/* COLUMN 5 */
/* -------- */

reg adiv100 mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr if cleanlim == 1, robust ;
est store results ;
reg adiv100 abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr if cleanlim == 1, robust ;
predict res1 if cleanlim == 1, resid ;
reg mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr if cleanlim == 1, robust ;
predict res2 if cleanlim == 1, resid ;
reg res1 res2 ;
scalar pr2 = e(r2) ;
outreg2 [results] using "results\tableD1", tex nocon 
	addstat("Partial R2", pr2) 
	sortvar(mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr) 
	bdec(3) tdec(3) rdec(3) ;
est drop _all ;
scalar drop _all ;
drop res1 res2 ;

/* -------- */
/* COLUMN 6 */
/* -------- */

reg adiv100 mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr africa europe asia if cleanlim == 1, robust ;
est store results ;
reg adiv100 abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr africa europe asia if cleanlim == 1, robust ;
predict res1 if cleanlim == 1, resid ;
reg mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr africa europe asia if cleanlim == 1, robust ;
predict res2 if cleanlim == 1, resid ;
reg res1 res2 ;
scalar pr2 = e(r2) ;
outreg2 [results] using "results\tableD1", tex nocon 
	addstat("Partial R2", pr2) 
	drop(africa europe asia) 
	sortvar(mdist_addis abslat arable suitavg suitrng suitgini elevavg2 elevstd distcr) 
	bdec(3) tdec(3) rdec(3) ;
est drop _all ;
scalar drop _all ;
drop res1 res2 ;

restore ;

exit ;
