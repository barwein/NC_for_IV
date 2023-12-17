/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/******************************************************************************************/
/*** TABLE D11: ROBUSTNESS TO REGION FIXED EFFECTS AND SUBSAMPLING ON REGIONAL CLUSTERS ***/
/******************************************************************************************/

use "data\country.dta", clear ;

/* -------- */
/* COLUMN 1 */
/* -------- */

reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest wb_ssa wb_mena wb_eca wb_sas wb_eap wb_lac if cleangdp == 1, rob ;
wherext pdiv_aa pdiv_aa_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\tableD11", replace tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	drop(opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest wb_ssa wb_mena wb_eca wb_sas wb_eap wb_lac) 
	sortvar(pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

/* -------- */
/* COLUMN 2 */
/* -------- */

reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest wb_mena wb_eca wb_sas wb_eap wb_lac if cleangdp == 1 & wb_ssa == 0, rob ;
wherext pdiv_aa pdiv_aa_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\tableD11", tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	drop(opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest wb_mena wb_eca wb_sas wb_eap wb_lac) 
	sortvar(pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

/* -------- */
/* COLUMN 3 */
/* -------- */

reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest wb_ssa wb_mena wb_eca wb_sas wb_eap if cleangdp == 1 & wb_lac == 0, rob ;
wherext pdiv_aa pdiv_aa_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\tableD11", tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	drop(opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest wb_ssa wb_mena wb_eca wb_sas wb_eap) 
	sortvar(pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

/* -------- */
/* COLUMN 4 */
/* -------- */

reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest wb_mena wb_eca wb_sas wb_eap if cleangdp == 1 & wb_ssa == 0 & wb_lac == 0, rob ;
wherext pdiv_aa pdiv_aa_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\tableD11", tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	drop(opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest wb_mena wb_eca wb_sas wb_eap) 
	sortvar(pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

/* -------- */
/* COLUMN 5 */
/* -------- */

reg ln_gdppc2000 pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk pmuslim pcatholic pprotest wb_ssa if cleangdp == 1 & (wb_ssa == 1 | wb_lac == 1), rob ;
wherext pdiv_aa pdiv_aa_sqr ;
scalar opt = r(argext) ;
scalar ose = sqrt(r(Vargext)) ;
outreg2 using "results\tableD11", tex nocon 
	addstat("Optimum", opt, "Optimum S.E.", ose) 
	drop(opec legor_uk pmuslim pcatholic pprotest wb_ssa) 
	sortvar(pdiv_aa pdiv_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr) 
	bdec(3) tdec(3) rdec(3) ;
scalar drop _all ;

exit ;
