/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

use "data\country.dta", clear ;

/*************************************************************************/
/*** TABLE G1: SUMMARY STATISTICS FOR THE 21-COUNTRY HISTORICAL SAMPLE ***/
/*************************************************************************/

statsmat ln_pd1500 adiv mdist_hgdp hmicost_hgdp ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1, stat(n mean sd min max) mat(desc_lim) listwise ;
outtable using "results\tableG1", mat(desc_lim) replace format(%9.0g %9.3f %9.3f %9.3f %9.3f) ;
matrix drop _all ;

/****************************************************************************/
/*** TABLE G2: PAIRWISE CORRELATIONS FOR THE 21-COUNTRY HISTORICAL SAMPLE ***/
/****************************************************************************/

corrtex ln_pd1500 adiv mdist_hgdp hmicost_hgdp ln_yst ln_arable ln_abslat ln_suitavg if cleanlim == 1, file("results\tableG2") replace digits(3) nbobs ;

/**************************************************************************/
/*** TABLE G3: SUMMARY STATISTICS FOR THE 145-COUNTRY HISTORICAL SAMPLE ***/
/**************************************************************************/

statsmat ln_pd1500 ln_pd1000 ln_pd1 pdiv ln_yst ln_arable ln_abslat ln_suitavg ln_frontdist1500 ln_frontdist1000 ln_frontdist1 elevavg rough distcr land100cr climate axis size plants animals mdist_addis aerial mdist_london mdist_tokyo mdist_mexico if cleanpd1500 == 1, stat(n mean sd min max) mat(desc_hist) listwise ;
outtable using "results\tableG3", mat(desc_hist) replace format(%9.0g %9.3f %9.3f %9.3f %9.3f) ;
matrix drop _all ;

/*****************************************************************************/
/*** TABLE G4: PAIRWISE CORRELATIONS FOR THE 145-COUNTRY HISTORICAL SAMPLE ***/
/*****************************************************************************/

corrtex ln_pd1500 ln_pd1000 ln_pd1 pdiv ln_yst ln_arable ln_abslat ln_suitavg ln_frontdist1500 ln_frontdist1000 ln_frontdist1 elevavg rough distcr land100cr climate axis size plants animals mdist_addis aerial mdist_london mdist_tokyo mdist_mexico if cleanpd1500 == 1, file("results\tableG4") replace digits(3) nbobs ;

/****************************************************************************/
/*** TABLE G5: SUMMARY STATISTICS FOR THE 143-COUNTRY CONTEMPORARY SAMPLE ***/
/****************************************************************************/

statsmat ln_gdppc2000 ln_pd1500 pdiv pdiv_aa ln_yst ln_yst_aa ln_arable ln_abslat ln_suitavg if cleancomp == 1, stat(n mean sd min max) mat(desc_cont_base) listwise ;
outtable using "results\tableG5", mat(desc_cont_base) replace format(%9.0g %9.3f %9.3f %9.3f %9.3f) ;
matrix drop _all ;

/*******************************************************************************/
/*** TABLE G6: PAIRWISE CORRELATIONS FOR THE 143-COUNTRY CONTEMPORARY SAMPLE ***/
/*******************************************************************************/

corrtex ln_gdppc2000 ln_pd1500 pdiv pdiv_aa ln_yst ln_yst_aa ln_arable ln_abslat ln_suitavg if cleancomp == 1, file("results\tableG6") replace digits(3) nbobs ;

/****************************************************************************/
/*** TABLE G7: SUMMARY STATISTICS FOR THE 109-COUNTRY CONTEMPORARY SAMPLE ***/
/****************************************************************************/

statsmat ln_gdppc2000 pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr peuro school mdist_addis mdist_addis_aa aerial aerial_aa if cleangdp == 1, stat(n mean sd min max) mat(desc_cont_rob) listwise ;
outtable using "results\tableG7", mat(desc_cont_rob) replace format(%9.0g %9.3f %9.3f %9.3f %9.3f) ;
matrix drop _all ;

/*******************************************************************************/
/*** TABLE G8: PAIRWISE CORRELATIONS FOR THE 109-COUNTRY CONTEMPORARY SAMPLE ***/
/*******************************************************************************/

corrtex ln_gdppc2000 pdiv_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr peuro school mdist_addis mdist_addis_aa aerial aerial_aa if cleangdp == 1, file("results\tableG8") replace digits(3) nbobs ;

exit ;
