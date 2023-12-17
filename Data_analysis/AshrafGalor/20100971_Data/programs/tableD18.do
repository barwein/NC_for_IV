/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/********************************************************************************************************************************/
/*** TABLE D18: ROBUSTNESS TO ALLOWING FOR SPATIAL AUTOREGRESSION IN THE EXTENDED-SAMPLE HISTORICAL AND CONTEMPORARY ANALYSES ***/
/********************************************************************************************************************************/

use "data\ethnic.dta", clear ;

preserve ;

sort latitude longitude ;

drop if latitude[_n] == latitude[_n + 1] & longitude[_n] == longitude[_n + 1] ;

spmat idistance dobj longitude latitude, id(id) dfunction(dhaversine) ;

spreg ml adiv mdist, id(id) dlmat(dobj) ;
scalar beta_sar = _b[mdist] ;
scalar cons_sar = _b[_cons] ;

spreg ml adiv mdist, id(id) elmat(dobj) ;
scalar beta_sare = _b[mdist] ;
scalar cons_sare = _b[_cons] ;

spreg ml adiv mdist, id(id) dlmat(dobj) elmat(dobj) ;
scalar beta_sarar = _b[mdist] ;
scalar cons_sarar = _b[_cons] ;

clear mata ;

restore ;

/* ----------- */
/* COLUMNS 1-3 */
/* ----------- */

use "data\country.dta", clear ;

preserve ;
keep if cleanpd1500 == 1 ;

reg ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas, robust ;
outreg2 using "results\tableD18", replace tex bdec(3) tdec(3) rdec(3) cttop(OLS) nocons drop(africa europe asia americas) ;

spmat idistance dobj cenlong cenlat, id(id) dfunction(dhaversine) ;

replace pdiv = cons_sar + beta_sar * mdist_addis ;
replace pdiv_sqr = pdiv ^ 2 ;
spreg ml ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas, id(id) dlmat(dobj) ;
outreg2 using "results\tableD18", tex bdec(3) tdec(3) rdec(3) cttop(SAR) nocons drop(africa europe asia americas) eqkeep(ln_pd1500) ;

replace pdiv = cons_sare + beta_sare * mdist_addis ;
replace pdiv_sqr = pdiv ^ 2 ;
spreg ml ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas, id(id) elmat(dobj) ;
outreg2 using "results\tableD18", tex bdec(3) tdec(3) rdec(3) cttop(SARE) nocons drop(africa europe asia americas) eqkeep(ln_pd1500) ;

replace pdiv = cons_sarar + beta_sarar * mdist_addis ;
replace pdiv_sqr = pdiv ^ 2 ;
spreg gs2sls ln_pd1500 pdiv pdiv_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas, id(id) dlmat(dobj) elmat(dobj) ;
outreg2 using "results\tableD18", tex bdec(3) tdec(3) rdec(3) cttop(SARAR) nocons drop(africa europe asia americas) eqkeep(ln_pd1500) ;

clear mata ;

restore ;

/* ----------- */
/* COLUMNS 4-8 */
/* ----------- */

preserve ;
keep if cleancomp == 1 ;

reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas, robust ;
outreg2 using "results\tableD18", tex bdec(3) tdec(3) rdec(3) cttop(OLS) nocons drop(africa europe asia americas) 
	sortvar(pdiv pdiv_sqr pdiv_aa pdiv_aa_sqr ln_yst ln_yst_aa) ;

spmat idistance dobj cenlong cenlat, id(id) dfunction(dhaversine) ;

replace pdiv_aa = cons_sar + beta_sar * mdist_addis_aa ;
replace pdiv_aa_sqr = pdiv_aa ^ 2 ;
spreg ml ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas, id(id) dlmat(dobj) ;
outreg2 using "results\tableD18", tex bdec(3) tdec(3) rdec(3) cttop(SAR) nocons drop(africa europe asia americas) eqkeep(ln_gdppc2000) 
	sortvar(pdiv pdiv_sqr pdiv_aa pdiv_aa_sqr ln_yst ln_yst_aa) ;

replace pdiv_aa = cons_sare + beta_sare * mdist_addis_aa ;
replace pdiv_aa_sqr = pdiv_aa ^ 2 ;
spreg ml ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas, id(id) elmat(dobj) ;
outreg2 using "results\tableD18", tex bdec(3) tdec(3) rdec(3) cttop(SARE) nocons drop(africa europe asia americas) eqkeep(ln_gdppc2000) 
	sortvar(pdiv pdiv_sqr pdiv_aa pdiv_aa_sqr ln_yst ln_yst_aa) ;

replace pdiv_aa = cons_sarar + beta_sarar * mdist_addis_aa ;
replace pdiv_aa_sqr = pdiv_aa ^ 2 ;
spreg ml ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat ln_suitavg africa europe asia americas, id(id) dlmat(dobj) elmat(dobj) ;
outreg2 using "results\tableD18", tex bdec(3) tdec(3) rdec(3) cttop(SARAR) nocons drop(africa europe asia americas) eqkeep(ln_gdppc2000) 
	sortvar(pdiv pdiv_sqr pdiv_aa pdiv_aa_sqr ln_yst ln_yst_aa) ;

clear mata ;

restore ;

scalar drop _all ;
matrix drop _all ;

exit ;
