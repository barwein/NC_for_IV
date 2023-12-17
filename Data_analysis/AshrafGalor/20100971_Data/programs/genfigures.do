/***************************************************************************************************
 *** THE OUT-OF-AFRICA HYPOTHESIS, HUMAN GENETIC DIVERSITY, AND COMPARATIVE ECONOMIC DEVELOPMENT ***
 ***     AUTHORS: QUAMRUL ASHRAF AND ODED GALOR                                                  ***
 ***     VERSION: AER PUBLICATION                                                                ***
 ***************************************************************************************************/

# delimit ;
set more off ;

cd "C:\Research\AshrafGalor\paper\genetic\Final\empirics" ;

/*********************************************************************************/
/*** FIGURE 1: EXPECTED HETEROZYGOSITY AND MIGRATORY DISTANCE FROM EAST AFRICA ***/
/*********************************************************************************/

use "data\ethnic.dta", clear ;

twoway (scatter adiv mdist if africa  == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin)) || 
       (scatter adiv mdist if mideast == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin)) || 
       (scatter adiv mdist if europe  == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin)) || 
       (scatter adiv mdist if asia    == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin)) || 
       (scatter adiv mdist if oceania == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin)) || 
       (scatter adiv mdist if america == 1, msymbol(D) msize(medium) mfcolor("219 229 241") mlcolor(black) mlwidth(thin)) || 
       (lfit    adiv mdist, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("Expected heterozygosity") 
        xtitle("Migratory distance from East Africa (in thousand km)") 
        legend(order(1 2 3 4 5 6) rows(1) label(1 "Africa") label(2 "Middle East") label(3 "Europe") label(4 "Asia") label(5 "Oceania") label(6 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\Figure1.emf", replace ;

/******************************************************************************/
/*** FIGURE 3: OBSERVED GENETIC DIVERSITY AND POPULATION DENSITY IN 1500 CE ***/
/******************************************************************************/

use "data\country.dta", clear ;

twoway (lpolyci ln_pd1500 ahomo if cleanlim == 1, kernel(gaussian) bwidth(0.05) degree(2) pwidth(0.1) clwidth(medthick) clcolor("15 37 63") acolor(gs14)) || 
       (scatter ln_pd1500 ahomo if cleanlim == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_pd1500 ahomo if cleanlim == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_pd1500 ahomo if cleanlim == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_pd1500 ahomo if cleanlim == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_pd1500 ahomo if cleanlim == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (qfit    ln_pd1500 ahomo if cleanlim == 1, clpattern(dash) clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("Log population density in 1500 CE") 
        xtitle("(Observed) Genetic homogeneity") 
        legend(order(3 4 5 6 7 2 8) rows(1) label(3 "Africa") label(4 "Europe") label(5 "Asia") label(6 "Oceania") label(7 "Americas") label(2 "Non-parametric") label(8 "Quadratic") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\Figure3.emf", replace ;

/*******************************************************************************/
/*** FIGURE 4: PREDICTED GENETIC DIVERSITY AND POPULATION DENSITY IN 1500 CE ***/
/*******************************************************************************/

quietly reg ln_pd1500 phomo phomo_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1, robust ;
predict res if cleanpd1500 == 1, resid ;
gen acpr = _b[phomo] * phomo + _b[phomo_sqr] * phomo_sqr + res ;

twoway (scatter acpr phomo if cleanpd1500 == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1500 == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1500 == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1500 == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1500 == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (qfit    acpr phomo if cleanpd1500 == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Augmented component plus residuals)", size(vsmall)) 
        l2title("Log population density in 1500 CE") 
        b1title("", size(vsmall)) 
        b2title("(Predicted) Genetic homogeneity") 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\Figure4.emf", replace ;
drop res acpr ;

/**************************************************************************************/
/*** FIGURE 5: ANCESTRY-ADJUSTED GENETIC DIVERSITY AND INCOME PER CAPITA IN 2000 CE ***/
/**************************************************************************************/

quietly reg ln_gdppc2000 phomo_aa phomo_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 ;
predict res if cleangdp == 1, resid ;
gen acpr = _b[phomo_aa] * phomo_aa + _b[phomo_aa_sqr] * phomo_aa_sqr + res ;

twoway (scatter acpr phomo_aa if cleangdp == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo_aa if cleangdp == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo_aa if cleangdp == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo_aa if cleangdp == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo_aa if cleangdp == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (qfit    acpr phomo_aa if cleangdp == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Augmented component plus residuals)", size(vsmall)) 
        l2title("Log income per capita in 2000 CE") 
        b1title("", size(vsmall)) 
        b2title("(Predicted) Ancestry-adjusted genetic homogeneity") 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\Figure5.emf", replace ;
drop res acpr ;

/********************************************************************************/
/*** FIGURE A1: PREDICTED GENETIC DIVERSITY AND POPULATION DENSITY IN 1000 CE ***/
/********************************************************************************/

quietly reg ln_pd1000 phomo phomo_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1000 == 1, robust ;
predict res if cleanpd1000 == 1, resid ;
gen acpr = _b[phomo] * phomo + _b[phomo_sqr] * phomo_sqr + res ;

twoway (scatter acpr phomo if cleanpd1000 == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1000 == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1000 == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1000 == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1000 == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (qfit    acpr phomo if cleanpd1000 == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Augmented component plus residuals)", size(vsmall)) 
        l2title("Log population density in 1000 CE") 
        b1title("", size(vsmall)) 
        b2title("(Predicted) Genetic homogeneity") 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureA1.emf", replace ;
drop res acpr ;

/*****************************************************************************/
/*** FIGURE A2: PREDICTED GENETIC DIVERSITY AND POPULATION DENSITY IN 1 CE ***/
/*****************************************************************************/

quietly reg ln_pd1 phomo phomo_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1 == 1, robust ;
predict res if cleanpd1 == 1, resid ;
gen acpr = _b[phomo] * phomo + _b[phomo_sqr] * phomo_sqr + res ;

twoway (scatter acpr phomo if cleanpd1 == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1 == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1 == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1 == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter acpr phomo if cleanpd1 == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (qfit    acpr phomo if cleanpd1 == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Augmented component plus residuals)", size(vsmall)) 
        l2title("Log population density in 1 CE") 
        b1title("", size(vsmall)) 
        b2title("(Predicted) Genetic homogeneity") 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureA2.emf", replace ;
drop res acpr ;

/********************************************************************************/
/*** FIGURE B1: PAIRWISE Fst GENETIC DISTANCE AND PAIRWISE MIGRATORY DISTANCE ***/
/********************************************************************************/

use "data\ethnicpair.dta", clear ;

twoway (scatter gdist mdist, msize(medsmall) mfcolor("184 204 228") mlcolor(black) mlwidth(thin)) || (lfit gdist mdist, clwidth(medthick) clcolor("149 55 53")),
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("Pairwise Fst genetic distance") 
        xtitle("Pairwise migratory distance (in thousand km)") 
        legend(off) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureB1.emf", replace ;

/********************************************************************************************************************************************/
/*** FIGURE C1: OBSERVED GENETIC DIVERSITY AND POPULATION DENSITY IN 1500 CE – THE UNCONDITIONAL QUADRATIC AND CUBIC SPLINE RELATIONSHIPS ***/
/********************************************************************************************************************************************/

use "data\country.dta", clear ;

rcspline ln_pd1500 ahomo if cleanlim == 1, knots(0.28469774 0.3389765 0.39325526) scatter(msymbol(none)) ci(color(gs14)) clwidth(medthick) clcolor("15 37 63") 
         ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
         xlabel(, labsize(small)) 
         ytitle("Log population density in 1500 CE") 
         xtitle("(Observed) Genetic homogeneity") 
         note("") 
         legend(on order(4 5 6 7 8 3 9) rows(1) label(4 "Africa") label(5 "Europe") label(6 "Asia") label(7 "Oceania") label(8 "Americas") label(3 "Cubic Spline") label(9 "Quadratic") size(vsmall)) 
         graphregion(color(white)) 
         plotregion(color(white)) 
         bgcolor(white) 
         addplot((scatter ln_pd1500 ahomo if cleanlim == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_pd1500 ahomo if cleanlim == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_pd1500 ahomo if cleanlim == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_pd1500 ahomo if cleanlim == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_pd1500 ahomo if cleanlim == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (qfit    ln_pd1500 ahomo if cleanlim == 1, clpattern(dash) clwidth(medthick) clcolor("149 55 53"))) ;

graph export "results\FigureC1.emf", replace ;

/***********************************************************************************************************************************************/
/*** FIGURE C2a: PREDICTED GENETIC DIVERSITY AND POPULATION DENSITY IN 1500 CE – THE UNCONDITIONAL QUADRATIC AND NONPARAMETRIC RELATIONSHIPS ***/
/***********************************************************************************************************************************************/

twoway (lpolyci ln_pd1500 phomo if cleanpd1500 == 1, kernel(gaussian) bwidth(0.06) degree(2) pwidth(0.12) clwidth(medthick) clcolor("15 37 63") acolor(gs14)) || 
       (scatter ln_pd1500 phomo if cleanpd1500 == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_pd1500 phomo if cleanpd1500 == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_pd1500 phomo if cleanpd1500 == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_pd1500 phomo if cleanpd1500 == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_pd1500 phomo if cleanpd1500 == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (qfit    ln_pd1500 phomo if cleanpd1500 == 1, clpattern(dash) clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("Log population density in 1500 CE") 
        xtitle("(Predicted) Genetic homogeneity") 
        legend(order(3 4 5 6 7 2 8) rows(1) label(3 "Africa") label(4 "Europe") label(5 "Asia") label(6 "Oceania") label(7 "Americas") label(2 "Non-parametric") label(8 "Quadratic") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC2a.emf", replace ;

/**********************************************************************************************************************************************/
/*** FIGURE C2b: PREDICTED GENETIC DIVERSITY AND POPULATION DENSITY IN 1500 CE – THE UNCONDITIONAL QUADRATIC AND CUBIC SPLINE RELATIONSHIPS ***/
/**********************************************************************************************************************************************/

rcspline ln_pd1500 phomo if cleanpd1500 == 1, knots(0.27623983 0.32678077 0.37732171) scatter(msymbol(none)) ci(color(gs14)) clwidth(medthick) clcolor("15 37 63") 
         ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
         xlabel(, labsize(small)) 
         ytitle("Log population density in 1500 CE") 
         xtitle("(Predicted) Genetic homogeneity") 
         note("") 
         legend(on order(4 5 6 7 8 3 9) rows(1) label(4 "Africa") label(5 "Europe") label(6 "Asia") label(7 "Oceania") label(8 "Americas") label(3 "Cubic Spline") label(9 "Quadratic") size(vsmall)) 
         graphregion(color(white)) 
         plotregion(color(white)) 
         bgcolor(white) 
         addplot((scatter ln_pd1500 phomo if cleanpd1500 == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_pd1500 phomo if cleanpd1500 == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_pd1500 phomo if cleanpd1500 == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_pd1500 phomo if cleanpd1500 == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_pd1500 phomo if cleanpd1500 == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (qfit    ln_pd1500 phomo if cleanpd1500 == 1, clpattern(dash) clwidth(medthick) clcolor("149 55 53"))) ;

graph export "results\FigureC2b.emf", replace ;

/******************************************************************************************************************/
/*** FIGURE C3a: PREDICTED GENETIC DIVERSITY AND POPULATION DENSITY IN 1500 CE – THE FIRST-ORDER PARTIAL EFFECT ***/
/******************************************************************************************************************/

quietly reg ln_pd1500 phomo_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1, robust ;
predict res1 if cleanpd1500 == 1, resid ;
quietly reg phomo phomo_sqr ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1, robust ;
predict res2 if cleanpd1500 == 1, resid ;

twoway (scatter res1 res2 if cleanpd1500 == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanpd1500 == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanpd1500 == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanpd1500 == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanpd1500 == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (lfit    res1 res2 if cleanpd1500 == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Residuals)", size(vsmall)) 
        l2title("Log population density in 1500 CE") 
        b1title("(Residuals)", size(small)) 
        b2title("(Predicted) Genetic homogeneity") 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC3a.emf", replace ;
drop res1 res2 ;

/*******************************************************************************************************************/
/*** FIGURE C3b: PREDICTED GENETIC DIVERSITY AND POPULATION DENSITY IN 1500 CE – THE SECOND-ORDER PARTIAL EFFECT ***/
/*******************************************************************************************************************/

quietly reg ln_pd1500 phomo ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1, robust ;
predict res1 if cleanpd1500 == 1, resid ;
quietly reg phomo_sqr phomo ln_yst ln_arable ln_abslat ln_suitavg africa europe asia americas if cleanpd1500 == 1, robust ;
predict res2 if cleanpd1500 == 1, resid ;

twoway (scatter res1 res2 if cleanpd1500 == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanpd1500 == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanpd1500 == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanpd1500 == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanpd1500 == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (lfit    res1 res2 if cleanpd1500 == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Residuals)", size(vsmall)) 
        l2title("Log population density in 1500 CE") 
        b1title("(Residuals)", size(small)) 
        b2title("(Predicted) Genetic homogeneity square") 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC3b.emf", replace ;
drop res1 res2 ;

/******************************************************************************************************************************************************/
/*** FIGURE C4a: ANCESTRY-ADJUSTED GENETIC DIVERSITY AND INCOME PER CAPITA IN 2000 CE – THE UNCONDITIONAL QUADRATIC AND NONPARAMETRIC RELATIONSHIPS ***/
/******************************************************************************************************************************************************/

twoway (lpolyci ln_gdppc2000 phomo_aa if cleancomp == 1, kernel(gaussian) bwidth(0.06) degree(2) pwidth(0.12) clwidth(medthick) clcolor("15 37 63") acolor(gs14)) || 
       (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (qfit    ln_gdppc2000 phomo_aa if cleancomp == 1, clpattern(dash) clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("Log income per capita in 2000 CE") 
        xtitle("(Predicted) Ancestry-adjusted genetic homogeneity") 
        legend(order(3 4 5 6 7 2 8) rows(1) label(3 "Africa") label(4 "Europe") label(5 "Asia") label(6 "Oceania") label(7 "Americas") label(2 "Non-parametric") label(8 "Quadratic") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC4a.emf", replace ;

/*****************************************************************************************************************************************************/
/*** FIGURE C4b: ANCESTRY-ADJUSTED GENETIC DIVERSITY AND INCOME PER CAPITA IN 2000 CE – THE UNCONDITIONAL QUADRATIC AND CUBIC SPLINE RELATIONSHIPS ***/
/*****************************************************************************************************************************************************/

rcspline ln_gdppc2000 phomo_aa if cleancomp == 1, knots(0.26230253 0.29890618 0.33550982) scatter(msymbol(none)) ci(color(gs14)) clwidth(medthick) clcolor("15 37 63") 
         ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
         xlabel(, labsize(small)) 
         ytitle("Log income per capita in 2000 CE") 
         xtitle("(Predicted) Ancestry-adjusted genetic homogeneity") 
         note("") 
         legend(on order(4 5 6 7 8 3 9) rows(1) label(4 "Africa") label(5 "Europe") label(6 "Asia") label(7 "Oceania") label(8 "Americas") label(3 "Cubic Spline") label(9 "Quadratic") size(vsmall)) 
         graphregion(color(white)) 
         plotregion(color(white)) 
         bgcolor(white) 
         addplot((scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (scatter ln_gdppc2000 phomo_aa if cleancomp == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
                 (qfit    ln_gdppc2000 phomo_aa if cleancomp == 1, clpattern(dash) clwidth(medthick) clcolor("149 55 53"))) ;

graph export "results\FigureC4b.emf", replace ;

/*************************************************************************************************************************/
/*** FIGURE C5a: ANCESTRY-ADJUSTED GENETIC DIVERSITY AND INCOME PER CAPITA IN 2000 CE – THE FIRST-ORDER PARTIAL EFFECT ***/
/*************************************************************************************************************************/

quietly reg ln_gdppc2000 phomo_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 ;
predict res1 if cleangdp == 1, resid ;
quietly reg phomo_aa phomo_aa_sqr ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 ;
predict res2 if cleangdp == 1, resid ;

twoway (scatter res1 res2 if cleangdp == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleangdp == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleangdp == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleangdp == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleangdp == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (lfit    res1 res2 if cleangdp == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Residuals)", size(vsmall)) 
        l2title("Log income per capita in 2000 CE") 
        b1title("(Residuals)", size(vsmall)) 
        b2title("(Predicted) Ancestry-adjusted genetic homogeneity") 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC5a.emf", replace ;
drop res1 res2 ;

/**************************************************************************************************************************/
/*** FIGURE C5b: ANCESTRY-ADJUSTED GENETIC DIVERSITY AND INCOME PER CAPITA IN 2000 CE – THE SECOND-ORDER PARTIAL EFFECT ***/
/**************************************************************************************************************************/

quietly reg ln_gdppc2000 phomo_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 ;
predict res1 if cleangdp == 1, resid ;
quietly reg phomo_aa_sqr phomo_aa ln_yst_aa ln_arable ln_abslat socinf efrac malfal kgatr distcr opec legor_uk legor_fr legor_ge legor_so pmuslim pcatholic pprotest europe asia oceania americas wb_ssa if cleangdp == 1 ;
predict res2 if cleangdp == 1, resid ;

twoway (scatter res1 res2 if cleangdp == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleangdp == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleangdp == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleangdp == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleangdp == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (lfit    res1 res2 if cleangdp == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Residuals)", size(vsmall)) 
        l2title("Log income per capita in 2000 CE") 
        b1title("(Residuals)", size(vsmall)) 
        b2title("(Predicted) Ancestry-adjusted genetic homogeneity square") 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC5b.emf", replace ;
drop res1 res2 ;

/****************************************************************************************************************************************/
/*** FIGURE C6a: ANCESTRY-ADJUSTED MIGRATORY DISTANCE FROM EAST AFRICA AND MEAN SKIN REFLECTANCE IN CONTEMPORARY NATIONAL POPULATIONS ***/
/****************************************************************************************************************************************/

quietly reg skinreflectance uvdamage abslat arable kgatr kgatemp elevavg distcr land100cr europe asia oceania americas wb_ssa if cleanskin == 1 ;
predict res1 if cleanskin == 1, resid ;
quietly reg mdist_addis_aa uvdamage abslat arable kgatr kgatemp elevavg distcr land100cr europe asia oceania americas wb_ssa if cleanskin == 1 ;
predict res2 if cleanskin == 1, resid ;

twoway (scatter res1 res2 if cleanskin == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanskin == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanskin == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanskin == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanskin == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (lfit    res1 res2 if cleanskin == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Residuals)", size(vsmall)) 
        l2title("Average degree of skin reflectance") 
        b1title("(Residuals)", size(vsmall)) 
        b2title("Ancestry-adjusted migratory distance from East Africa") 
        caption("coef = 0.186, (robust) SE = 0.355, t = 0.52", size(medium)) 
        legend(order(1 2 3 4 5) rows(1) label(1 "Africa") label(2 "Europe") label(3 "Asia") label(4 "Oceania") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC6a.emf", replace ;
drop res1 res2 ;

/******************************************************************************************************************************/
/*** FIGURE C6b: ANCESTRY-ADJUSTED MIGRATORY DISTANCE FROM EAST AFRICA AND MEAN HEIGHT IN CONTEMPORARY NATIONAL POPULATIONS ***/
/******************************************************************************************************************************/

quietly reg height uvdamage abslat arable kgatr kgatemp elevavg distcr land100cr africa europe asia americas if cleanheight == 1 ;
predict res1 if cleanheight == 1, resid ;
quietly reg mdist_addis_aa uvdamage abslat arable kgatr kgatemp elevavg distcr land100cr africa europe asia americas if cleanheight == 1 ;
predict res2 if cleanheight == 1, resid ;

twoway (scatter res1 res2 if cleanheight == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanheight == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanheight == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanheight == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanheight == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (lfit    res1 res2 if cleanheight == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Residuals)", size(vsmall)) 
        l2title("Average height") 
        b1title("(Residuals)", size(vsmall)) 
        b2title("Ancestry-adjusted migratory distance from East Africa") 
        caption("coef = 0.014, (robust) SE = 0.178, t = 0.08", size(medium)) 
        legend(order(1 3 5) rows(1) label(1 "Africa") label(3 "Asia") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC6b.emf", replace ;
drop res1 res2 ;

/******************************************************************************************************************************/
/*** FIGURE C6c: ANCESTRY-ADJUSTED MIGRATORY DISTANCE FROM EAST AFRICA AND MEAN WEIGHT IN CONTEMPORARY NATIONAL POPULATIONS ***/
/******************************************************************************************************************************/

quietly reg weight uvdamage abslat arable kgatrstr kgatemp elevavg distcr land100cr africa europe asia americas if cleanweight == 1 ;
predict res1 if cleanweight == 1, resid ;
quietly reg mdist_addis_aa uvdamage abslat arable kgatrstr kgatemp elevavg distcr land100cr africa europe asia americas if cleanweight == 1 ;
predict res2 if cleanweight == 1, resid ;

twoway (scatter res1 res2 if cleanweight == 1 & africa   == 1, msymbol(O) msize(medium) mfcolor("23 55 93"   ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanweight == 1 & europe   == 1, msymbol(D) msize(medium) mfcolor("55 96 145"  ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanweight == 1 & asia     == 1, msymbol(S) msize(medium) mfcolor("79 129 189" ) mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanweight == 1 & oceania  == 1, msymbol(T) msize(medium) mfcolor("141 180 227") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (scatter res1 res2 if cleanweight == 1 & americas == 1, msymbol(O) msize(medium) mfcolor("184 204 228") mlcolor(black) mlwidth(thin) mlabel(code) mlabpos(6) mlabsize(1.75) mlabcolor(black)) || 
       (lfit    res1 res2 if cleanweight == 1, clwidth(medthick) clcolor("149 55 53")), 
        ylabel(, labsize(small) glcolor(gs14) glwidth(medthin)) 
        xlabel(, labsize(small)) 
        ytitle("") xtitle("") 
        l1title("(Residuals)", size(vsmall)) 
        l2title("Average weight") 
        b1title("(Residuals)", size(vsmall)) 
        b2title("Ancestry-adjusted migratory distance from East Africa") 
        caption("coef = -0.113, (robust) SE = 0.200, t = -0.57", size(medium)) 
        legend(order(1 3 5) rows(1) label(1 "Africa") label(3 "Asia") label(5 "Americas") size(vsmall)) 
        graphregion(color(white)) 
        plotregion(color(white)) 
        bgcolor(white) ;

graph export "results\FigureC6c.emf", replace ;
drop res1 res2 ;

exit ;
