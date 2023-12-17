/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/************************************************************************************************/
/*** TABLE D13: STANDARDIZED BETA AND PARTIAL R2 COEFFICIENTS FOR THE BASELINE SPECIFICATIONS ***/
/************************************************************************************************/

capture log close ;
log using "results\TableD13.log", replace ;

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

/* STANDARDIZED BETAS -- PRINTED IN TEX/TXT FILES */
center ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1, prefix(z_) standardize ;
reg z_ln_pd1500 z_adiv z_adiv_sqr z_ln_yst z_ln_arable z_ln_abslat z_ln_suitavg if cleanlim == 1, robust ;
outreg2 using "results\tableD13", replace tex nose nocon 
	sortvar(z_adiv z_adiv_sqr z_pdiv z_pdiv_sqr z_pdiv_aa z_pdiv_aa_sqr z_ln_yst z_ln_yst_aa z_ln_arable z_ln_abslat z_ln_suitavg) 
	noaster bdec(3) tdec(3) rdec(3) ;
drop z_* ;

/* PARTIAL R2 COEFFICIENTS -- PRINTED IN LOG FILE */
pcorr2 ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1 ;

/* -------- */
/* COLUMN 2 */
/* -------- */

/* STANDARDIZED BETAS -- PRINTED IN TEX/TXT FILES */
center ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe americas if cleanlim == 1, prefix(z_) standardize ;
reg z_ln_pd1500 z_adiv z_adiv_sqr z_ln_yst z_ln_arable z_ln_abslat z_ln_suitavg z_africa z_europe z_americas if cleanlim == 1, robust ;
outreg2 using "results\tableD13", tex nose nocon 
	drop(z_africa z_europe z_americas) 
	sortvar(z_adiv z_adiv_sqr z_pdiv z_pdiv_sqr z_pdiv_aa z_pdiv_aa_sqr z_ln_yst z_ln_yst_aa z_ln_arable z_ln_abslat z_ln_suitavg) 
	noaster bdec(3) tdec(3) rdec(3) ;
drop z_* ;

/* PARTIAL R2 COEFFICIENTS -- PRINTED IN LOG FILE */
pcorr2 ln_pd1500 adiv adiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe americas if cleanlim == 1 ;

/* -------- */
/* COLUMN 3 */
/* -------- */

/* STANDARDIZED BETAS -- PRINTED IN TEX/TXT FILES */
center ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1, prefix(z_) standardize ;
reg z_ln_pd1500 z_pdiv z_pdiv_sqr z_ln_yst z_ln_arable z_ln_abslat z_ln_suitavg z_africa z_europe z_asia z_americas if cleanpd1500 == 1, robust ;
outreg2 using "results\tableD13", tex nose nocon 
	drop(z_africa z_europe z_asia z_americas) 
	sortvar(z_adiv z_adiv_sqr z_pdiv z_pdiv_sqr z_pdiv_aa z_pdiv_aa_sqr z_ln_yst z_ln_yst_aa z_ln_arable z_ln_abslat z_ln_suitavg) 
	noaster bdec(3) tdec(3) rdec(3) ;
drop z_* ;

/* PARTIAL R2 COEFFICIENTS -- PRINTED IN LOG FILE */
pcorr2 ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1 ;

/* -------- */
/* COLUMN 4 */
/* -------- */

/* STANDARDIZED BETAS -- PRINTED IN TEX/TXT FILES */
center ln_pd1000 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1, prefix(z_) standardize ;
reg z_ln_pd1000 z_pdiv z_pdiv_sqr z_ln_yst z_ln_arable z_ln_abslat z_ln_suitavg z_africa z_europe z_asia z_americas if cleanpd1000 == 1, robust ;
outreg2 using "results\tableD13", tex nose nocon 
	drop(z_africa z_europe z_asia z_americas) 
	sortvar(z_adiv z_adiv_sqr z_pdiv z_pdiv_sqr z_pdiv_aa z_pdiv_aa_sqr z_ln_yst z_ln_yst_aa z_ln_arable z_ln_abslat z_ln_suitavg) 
	noaster bdec(3) tdec(3) rdec(3) ;
drop z_* ;

/* PARTIAL R2 COEFFICIENTS -- PRINTED IN LOG FILE */
pcorr2 ln_pd1000 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1 ;

/* -------- */
/* COLUMN 5 */
/* -------- */

/* STANDARDIZED BETAS -- PRINTED IN TEX/TXT FILES */
center ln_pd1 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1 == 1, prefix(z_) standardize ;
reg z_ln_pd1 z_pdiv z_pdiv_sqr z_ln_yst z_ln_arable z_ln_abslat z_ln_suitavg z_africa z_europe z_asia z_americas if cleanpd1 == 1, robust ;
outreg2 using "results\tableD13", tex nose nocon 
	drop(z_africa z_europe z_asia z_americas) 
	sortvar(z_adiv z_adiv_sqr z_pdiv z_pdiv_sqr z_pdiv_aa z_pdiv_aa_sqr z_ln_yst z_ln_yst_aa z_ln_arable z_ln_abslat z_ln_suitavg) 
	noaster bdec(3) tdec(3) rdec(3) ;
drop z_* ;

/* PARTIAL R2 COEFFICIENTS -- PRINTED IN LOG FILE */
pcorr2 ln_pd1 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1 == 1 ;

/* -------- */
/* COLUMN 6 */
/* -------- */

/* STANDARDIZED BETAS -- PRINTED IN TEX/TXT FILES */
center ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1, prefix(z_) standardize ;
reg z_ln_gdppc2000 z_pdiv_aa z_pdiv_aa_sqr z_ln_yst_aa z_ln_arable z_ln_abslat z_ln_suitavg z_africa z_europe z_asia z_americas if cleancomp == 1, robust ;
outreg2 using "results\tableD13", tex nose nocon 
	drop(z_africa z_europe z_asia z_americas) 
	sortvar(z_adiv z_adiv_sqr z_pdiv z_pdiv_sqr z_pdiv_aa z_pdiv_aa_sqr z_ln_yst z_ln_yst_aa z_ln_arable z_ln_abslat z_ln_suitavg) 
	noaster bdec(3) tdec(3) rdec(3) ;
drop z_* ;

/* PARTIAL R2 COEFFICIENTS -- PRINTED IN LOG FILE */
pcorr2 ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas if cleancomp == 1 ;

log close ;

exit ;
