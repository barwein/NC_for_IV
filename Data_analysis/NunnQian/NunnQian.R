# 
# Analysis of Nunn & Qian: "U.S. Food Aid and Civil Conflict".
# 

# Load libaries and sources -----------------------------------------------

library(haven)
library(dplyr)
library(fixest)
library(caret)
library(mltools)
library(data.table)
library(sandwich)
library(stringr)
library(mgcv)
library(kableExtra)
library(lmtest)


source("Aux_functions/Estimation_Functions.R")


# Read Data ---------------------------------------------------------------

Nunn.df <- read_dta("Data_analysis/NunnQian/in_sample.dta")

Nunn.df <- as.data.table(Nunn.df)

colnames.nunn <- colnames(Nunn.df)

# Define variables/controls --------------------------------------------------------

US_controls <-  c("oil_fadum_avg", "US_income_fadum_avg", "US_democ_pres_fadum_avg")

precip.vars <- colnames.nunn[str_detect(str = colnames.nunn,pattern = "all_Precip_?")]
temp.vars <- colnames.nunn[str_detect(str = colnames.nunn,pattern = "all_Temp_?")]
rcereal.vars <- colnames.nunn[str_detect(str = colnames.nunn,pattern = "rcereal_?")]
rimport.vars <- colnames.nunn[str_detect(str = colnames.nunn,pattern = "rimport_?")]
gdp.vars <- colnames.nunn[str_detect(str = colnames.nunn,pattern = "gdp_?")]
usmil.vars <- colnames.nunn[str_detect(str = colnames.nunn,pattern = "usmil_?")]
usec.vars <- colnames.nunn[str_detect(str = colnames.nunn,pattern = "usec_?")]

weather_controls <- c(precip.vars, temp.vars)
country_chars_controls <- c(gdp.vars, usmil.vars, usec.vars)
cereals_controls <- c(rcereal.vars, rimport.vars)
baseline_controls <- c(US_controls, country_chars_controls, weather_controls, cereals_controls) # full set of controls

# Year as factor

Nunn.df[,year.f := as.factor(year)]

# NCIV 

NCIV.vars <- colnames.nunn[str_detect(colnames.nunn, pattern = c("l_USprod_?"))]
# NCIV is NCIV.vars*fadum_avg

Nunn.df[,`:=`(l_USprod_Oranges = (l_USprod_Oranges/1000)*fadum_avg,
              l_USprod_Grapes = (l_USprod_Grapes/1000)*fadum_avg,
              l_USprod_Lettuce = (l_USprod_Lettuce/1000)*fadum_avg,
              l_USprod_Cotton_lint = (l_USprod_Cotton_lint/1000)*fadum_avg,
              l_USprod_Onions_dry = (l_USprod_Onions_dry/1000)*fadum_avg,
              l_USprod_Grapefruit = (l_USprod_Grapefruit/1000)*fadum_avg,
              l_USprod_Cabbages = (l_USprod_Cabbages/1000)*fadum_avg,
              l_USprod_Watermelons = (l_USprod_Watermelons/1000)*fadum_avg,
              l_USprod_Carrots_turnips = (l_USprod_Carrots_turnips/1000)*fadum_avg,
              l_USprod_Peaches_nectarines = (l_USprod_Peaches_nectarines/1000)*fadum_avg)]

Nunn.df[,intra_state := intra_state*1000]

# LM formulas --------------------------------------------------------------

reg.formula <- function(IV.name, is.NC, IV.adjusted){
  if(is.NC){ # If IV is NCIV
    if(IV.adjusted){ # Controls for original IV
      as.formula(paste("intra_state ~ ", IV.name, " + ",
                       "instrument"," + ",
                       paste(baseline_controls,collapse = " + "),
                       " + ", "risocode + year.f:wb_region"))  
    } else{
      as.formula(paste("intra_state ~ ", IV.name, " + ",
                       paste(baseline_controls,collapse = " + "),
                       " + ", "risocode + year.f:wb_region"))
    }
  } else{
  as.formula(paste("intra_state ~ ", IV.name, " + ",
                   paste(baseline_controls,collapse = " + "),
                   " + ", "risocode + year.f:wb_region"))
  }
}

# Run reduced form falsification regressions
compute.reduce.form.reg <- function(IV.name, is.NC, IV.adjusted = FALSE){
  cur.lm <- lm(reg.formula(IV.name,is.NC,IV.adjusted), data = Nunn.df)
  beta.hat <-  summary(cur.lm)$coef[2,1]
  robust.se <- sqrt(vcovCL(cur.lm, cluster = Nunn.df[,risocode])[2,2])
  t.stat.robust <- beta.hat / robust.se
  pval.robust <-  2*pt(abs(t.stat.robust), cur.lm$df.residual, lower.tail = FALSE)
  return(list(IV = IV.name, beta = beta.hat, robust.se = robust.se,
              t.stat = t.stat.robust, pval = pval.robust))
}


# Replicate Table 5 -------------------------------------------------------


Nunn.tab5.results <- data.table(IV = NA, beta = NA, robust.se = NA, t.stat = NA, pval = NA)

for (iv in c("instrument",NCIV.vars)) {
  is.NC = ifelse(iv=="instrument",FALSE,TRUE)
  curr.results <- compute.reduce.form.reg(IV.name = iv,is.NC =is.NC)
  Nunn.tab5.results <- rbindlist(list(Nunn.tab5.results, curr.results))
}

Nunn.tab5.results <- Nunn.tab5.results[-1,]

bonf.pval <- min(Nunn.tab5.results[2:11,pval])*length(NCIV.vars)
bonf.pval <- ifelse(bonf.pval<0.01,"<0.01",round(bonf.pval,3))

# Replicate Table 5 with control for IV -----------------------------------

Nunn.tab5.iv.adjusted.results <- data.table(IV = NA, beta = NA, robust.se = NA, t.stat = NA, pval = NA)

for (iv in NCIV.vars) {
  curr.results <- compute.reduce.form.reg(iv, is.NC=TRUE,IV.adjusted = TRUE)
  Nunn.tab5.iv.adjusted.results <- rbindlist(list(Nunn.tab5.iv.adjusted.results, curr.results))
}

Nunn.tab5.iv.adjusted.results <- Nunn.tab5.iv.adjusted.results[-1,]


bonf.pval.iv.adjusted <- min(Nunn.tab5.iv.adjusted.results[,pval])*length(NCIV.vars)
bonf.pval.iv.adjusted <- ifelse(bonf.pval.iv.adjusted<0.01,"<0.01",round(bonf.pval.iv.adjusted,3))


# Falsification test using all NCIV together -------------------------------

# F-test

full.formula.iv.adjusted <- as.formula(paste("intra_state ~ ", paste(NCIV.vars,collapse = " + "), " + ", 
                                               "instrument"," + ",
                                               paste(baseline_controls,collapse = " + "),
                                               " + ", "risocode + year.f:wb_region"))  
full.formula <- as.formula(paste("intra_state ~ ", paste(NCIV.vars ,collapse = " + "), " + ", 
                                               paste(baseline_controls,collapse = " + "),
                                               " + ", "risocode + year.f:wb_region"))  
null.formula.iv.adjusted <- as.formula(paste("intra_state ~ ", "instrument"," + ",
                                             paste(baseline_controls,collapse = " + "),
                                             " + ", "risocode + year.f:wb_region"))  

null.formula <- as.formula(paste("intra_state ~ ", paste(baseline_controls,collapse = " + "),
                                             " + ", "risocode + year.f:wb_region"))  

get.f.pval <- function(full.formula,null.formula, n.NC){
  lm.full <- lm(full.formula, Nunn.df)
  lm.null <- lm(null.formula, Nunn.df)
  
  sse_full <- sum((lm.full$residuals)^2)
  sigma_sq_full <- sse_full / lm.full$df.residual # denom of f-stat
  sse_no_nc <- sum((lm.null$residuals)^2)
  
  F_stat <- ((sse_no_nc - sse_full) / n.NC) / sigma_sq_full # Fstat of null beta_{NC} = 0
  
  return(pf(F_stat, n.NC, lm.full$df.residual,lower.tail = FALSE))
}


f.pval.iv.adj <- get.f.pval(full.formula = full.formula.iv.adjusted,
                            null.formula = null.formula.iv.adjusted,
                            n.NC = length(NCIV.vars))

f.pval <- get.f.pval(full.formula = full.formula,
                            null.formula = null.formula,
                            n.NC = length(NCIV.vars))

f.pval.iv.adj <- ifelse(f.pval.iv.adj<0.01,"<0.01",round(f.pval.iv.adj,3))
f.pval <- ifelse(f.pval<0.01,"<0.01",round(f.pval,3))

# Wald test (cluster robust se)

wald.robust.test <- function(full.formula, NC.names){
  lm.full <- lm(full.formula, Nunn.df)
  lm.coef <- coef(lm.full)
  robust.vcov <- vcovCL(lm.full, cluster = Nunn.df[,risocode])
  
  nc.vars <- which(names(lm.coef) %in% NC.names)
  
  lm.coef.nc <- lm.coef[nc.vars]
  robust.vcov.nc <- robust.vcov[nc.vars,nc.vars]
  
  wald.stat <- t(lm.coef.nc) %*% MASS::ginv(robust.vcov.nc) %*% lm.coef.nc
  wald.df <- length(NC.names)
  
  wald.pval <- pchisq(q = wald.stat, df = wald.df, lower.tail = FALSE)
  
  return(wald.pval)
}

wald.pval.iv.adj <- wald.robust.test(full.formula = full.formula.iv.adjusted,
                                     NC.names = NCIV.vars)

wald.pval <- wald.robust.test(full.formula = full.formula,
                                     NC.names = NCIV.vars)

wald.pval.iv.adj <- ifelse(wald.pval.iv.adj<0.01,"<0.01",round(wald.pval.iv.adj,3))
wald.pval <- ifelse(wald.pval<0.01,"<0.01",round(wald.pval,3))

# Ramsey's test


# Y ~ IV + NCI + C
basic.lm <- lm(formula = full.formula.iv.adjusted, data = Nunn.df)
ramsey.test <- resettest(basic.lm, type = "fitted", data = Nunn.df)
ramsey.test

# Y ~ IV + C
re.formula.no.nci <-  as.formula(paste("intra_state ~ ", 
                                       "instrument"," + ",
                                       paste(baseline_controls,collapse = " + "),
                                       " + ", "risocode + year.f:wb_region"))  
basic.lm.no.nci <- lm(formula = re.formula.no.nci, data = Nunn.df)
ramsey.test.no.nci <- resettest(basic.lm.no.nci, type = "fitted", data = Nunn.df)
ramsey.test.no.nci

# IV ~ C

re.formula.iv.on.c <- as.formula(paste("instrument", "~",
                                       paste(baseline_controls,collapse = " + "),
                                       " + ", "risocode + year.f:wb_region"))
basic.lm.iv.on.c <- lm(formula = re.formula.iv.on.c, data = Nunn.df)
ramsey.test.iv.on.c <- resettest(basic.lm.iv.on.c, type = "fitted", data = Nunn.df)
ramsey.test.iv.on.c

# GAM

GAM.full.formula.iv.adjusted <- as.formula(paste("intra_state ~ ",  "s(instrument)"," + ",
                                             paste(baseline_controls,collapse = " + "),
                                             " + ", "risocode + year.f:wb_region", " + ",
                                             paste(paste0("s(",NCIV.vars,")"),collapse = " + ")))

GAM.full.formula <- as.formula(paste("intra_state ~ ", paste(baseline_controls,collapse = " + "),
                                 " + ", "risocode + year.f:wb_region"," + ",
                                 paste(paste0("s(",NCIV.vars,")"),collapse = " + ")))  

get.GAM.pval <- function(GAM.full.formula, null.formula, IV.adjusted = FALSE){
  gam.null <- gam(formula = null.formula,
                  data = Nunn.df,
                  method = "ML")
  
  gam.full <- gam(formula = GAM.full.formula,
                  data = Nunn.df,
                  method = "ML")
  
  gam.coef <- coef(gam.full)
  robust.vcov <- vcovCL(gam.full, cluster = Nunn.df[,risocode])
  nc.coef.index <- which(str_detect(names(gam.coef),pattern = "s\\("))
  
  wald.stat <- t(gam.coef[nc.coef.index]) %*% 
                                    MASS::ginv(robust.vcov[nc.coef.index,nc.coef.index]) %*% 
                                    gam.coef[nc.coef.index]
  wald.df <- length(nc.coef.index)
  wald.pval <- pchisq(q = wald.stat, df = wald.df, lower.tail = FALSE)
  
  Anova.smooth.all <- anova.gam(gam.null, gam.full, test="Chisq")
  pval.anova.smooth <- Anova.smooth.all$`Pr(>Chi)`[2]
  
  return(list(anova.pval=pval.anova.smooth, wald.robust.pval = wald.pval))
}

GAM.pval.iv.adjusted <- get.GAM.pval(GAM.full.formula = GAM.full.formula.iv.adjusted,
                                null.formula = null.formula.iv.adjusted)

GAM.pval <- get.GAM.pval(GAM.full.formula = GAM.full.formula,
                                null.formula = null.formula)

GAM.wald.pval.iv.adjusted <- ifelse(GAM.pval.iv.adjusted$wald.robust.pval<0.01,
                                    "<0.01",round(GAM.pval.iv.adjusted$wald.robust.pval,3))
GAM.wald.pval <- ifelse(GAM.pval$wald.robust.pval<0.01,"<0.01",round(GAM.pval$wald.robust.pval,3))

GAM.anova.pval.iv.adjusted <- ifelse(GAM.pval.iv.adjusted$anova.pval<0.01,
                                     "<0.01",round(GAM.pval.iv.adjusted$anova.pval,3))
GAM.anova.pval <- ifelse(GAM.pval$anova.pval<0.01,"<0.01",round(GAM.pval$anova.pval,3))


# Save results ------------------------------------------------------------

results.df <- t(rbind(c(bonf.pval, f.pval, wald.pval , GAM.wald.pval, GAM.anova.pval),
                    c(bonf.pval.iv.adjusted, f.pval.iv.adj, wald.pval.iv.adj ,
                      GAM.wald.pval.iv.adjusted,GAM.anova.pval.iv.adjusted)))
rownames(results.df) <- c("Bonf.","F-test","Wald (CL robust)","GAM (Wald)","GAM (Anova)")
colnames(results.df) <- c("No IV adj.","With IV adj.")

kable(results.df, format = "simple", booktabs = T)


