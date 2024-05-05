# 
# Analysis of Ashraf & Galor.
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
library(MASS)
library(kableExtra)

source("Aux_functions/Estimation_Functions.R")


# Read Data ---------------------------------------------------------------

AG.df <- read_dta("Data_analysis/AshrafGalor/20100971_Data/data/country.dta")

AG.df <- as.data.table(AG.df)

AG.df <- AG.df[cleanpd1500==1,]

# Define variables/controls --------------------------------------------------------

outcome.name <- "ln_pd1500"
cntrls.names <- c("ln_yst", "ln_arable", "ln_abslat", "ln_suitavg")

IV.name <- "mdist_hgdp"
IV.sq.name <- "mdist_hgdp_sqr" 

NCIV.addis <- c("mdist_addis")
NCIV.london <- c("mdist_london")
NCIV.tokyo <- c("mdist_tokyo")
NCIV.mexico <- c("mdist_mexico")

NCIVs <- c(NCIV.addis, NCIV.london, NCIV.tokyo, NCIV.mexico)


# LM formulas --------------------------------------------------------------

reg.formula <- function(IV.name, NC.name, IV.adjusted){
  if(IV.adjusted){ # Controls for original IV
      as.formula(paste0(outcome.name," ~ ", NC.name, " + ",
                       "I(",NC.name,"^2)"," + ",
                       IV.name," + ",
                       "I(",IV.name,"^2)", " + ",
                       paste(cntrls.names,collapse = " + ")))  
    } else{
      as.formula(paste0(outcome.name," ~ ", NC.name, " + ",
                        "I(",NC.name,"^2)"," + ",
                       paste(cntrls.names,collapse = " + ")))
    }
}

# Run reduced form falsfication regressions
compute.reduce.form.reg <- function(IV.name, NC.name, IV.adjusted = FALSE){
  cur.lm <- lm(reg.formula(IV.name, NC.name, IV.adjusted), data = AG.df)
  beta.hat <-  summary(cur.lm)$coef[2,1]
  beta.sq.hat <-  summary(cur.lm)$coef[3,1]
  robust.vcov <- vcovHC(cur.lm)
  robust.se <- sqrt(robust.vcov[2,2])
  robust.se.sq <- sqrt(robust.vcov[3,3])
  t.stat.robust <- beta.hat / robust.se
  t.stat.robust.sq <- beta.sq.hat / robust.se.sq
  pval.robust <-  2*pt(abs(t.stat.robust), cur.lm$df.residual, lower.tail = FALSE)
  pval.robust.sq <-  2*pt(abs(t.stat.robust.sq), cur.lm$df.residual, lower.tail = FALSE)
  
  res.list <- list(NCIV = NC.name,
                   beta.n.se = paste0(round(beta.hat,3)," (",round(robust.se,3),")"),
                   # beta.n.se.sq = paste0(round(beta.sq.hat,3)," (",round(robust.se.sq,3),")"),
                   pval = pval.robust,
                   IV.adjusted = IV.adjusted)
                   # pval.sq = pval.robust.sq)
  
  res.list.sq <- list(NCIV = NC.name,
                   # beta.n.se = paste0(round(beta.hat,3)," (",round(robust.se,3),")"),
                   beta.n.se = paste0(round(beta.sq.hat,3)," (",round(robust.se.sq,3),")"),
                   # pval = pval.robust)
                   pval = pval.robust.sq,
                   IV.adjusted = IV.adjusted)
  
  return(rbindlist(list(res.list, res.list.sq)))
}



# Replicate Table 4 -------------------------------------------------------


AG.tab4.results <- data.table(NCIV = NA, beta.n.se = NA, pval = NA, IV.adjusted = NA)

for (nc in NCIVs) {
  results.no.adj <- compute.reduce.form.reg(IV.name = IV.name, 
                                            NC.name = nc,
                                            IV.adjusted = FALSE)
  results.w.adj <- compute.reduce.form.reg(IV.name = IV.name, 
                                            NC.name = nc,
                                            IV.adjusted = TRUE)
  AG.tab4.results <- rbindlist(list(AG.tab4.results, results.no.adj, results.w.adj))
}

AG.tab4.results <- AG.tab4.results[-1,]
AG.tab4.results[,pval := ifelse(pval<0.01,"<0.01",round(pval,3))]
AG.tab4.results[,method := "separate.coef"]
AG.tab4.results

# NCIV test using Wald test with robust se (robust F-test) -----------------------------------

# Wald test  (robust se)

wald.robust.test <- function(full.formula, NC.name, IV.adjusted){
  lm.full <- lm(full.formula, AG.df)
  lm.coef <- coef(lm.full)
  robust.vcov <- vcovHC(lm.full)
  nc.vars <- c(2,3)
  lm.coef.nc <- lm.coef[nc.vars]
  robust.vcov.nc <- robust.vcov[nc.vars,nc.vars]
  
  wald.stat <- t(lm.coef.nc) %*% ginv(robust.vcov.nc) %*% lm.coef.nc
  # wald.stat <- t(lm.coef.nc) %*% solve(robust.vcov.nc) %*% lm.coef.nc
  wald.df <- length(nc.vars)
  
  wald.pval <- pchisq(q = wald.stat, df = wald.df, lower.tail = FALSE)
  
  return(list(NCIV = NC.name, pval =wald.pval, IV.adjusted=IV.adjusted))
}


AG.Wald.results <- data.table(NCIV = NA, pval = NA, IV.adjusted = NA)

for (nc in NCIVs) {
  results.no.adj <- wald.robust.test(full.formula = reg.formula(IV.name = IV.name, NC.name = nc,IV.adjusted = FALSE),
                                     NC.name = nc,
                                     IV.adjusted = FALSE)
  results.w.adj <- wald.robust.test(full.formula = reg.formula(IV.name = IV.name, NC.name = nc,IV.adjusted = TRUE),
                                    NC.name = nc,
                                    IV.adjusted = TRUE)
  AG.Wald.results <- rbindlist(list(AG.Wald.results, results.no.adj, results.w.adj))
}

AG.Wald.results <- AG.Wald.results[-1,]
AG.Wald.results

AG.Wald.results[,pval := ifelse(pval<0.01,"<0.01",round(pval,3))]
AG.Wald.results[,method := "Wald (robust se)"]



# Analysis with GAM (smooth IV and NC) ---------------------------------------------------------------------

Gam.formula <- function(NC.name, IV.adjusted){
  if(IV.adjusted){
    gam_formula <- as.formula(paste0(outcome.name, " ~ ", "s(",NC.name,") + ",
                                     # "s(",IV.name,") + ",
                                     IV.name," + ",
                                     "I(",IV.name,"^2)", " + ",
                                        paste(cntrls.names,collapse = " + ")))
  } else{
    gam_formula <- as.formula(paste0(outcome.name, " ~ ", "s(",NC.name,") + ",
                                     paste(cntrls.names,collapse = " + ")))
  }
  return(gam_formula)
}

get.GAM.pval <- function(GAM.full.formula, NC.name, IV.adjusted = FALSE){
  
  fit.gam.full <- gam(formula = GAM.full.formula,
                  data = AG.df,
                  method = "ML")
  
  gam.coef <- coef(fit.gam.full)
  robust.vcov <- vcov.gam(fit.gam.full, sandwich = TRUE)
  # MASS::ginv(robust.vcov[nc.coef.index,nc.coef.index])
  # 
  # n.non.nc <- 1 + length(baseline_controls) + 2 + as.numeric(IV.adjusted)
  # 
  nc.coef.index <- which(str_detect(names(gam.coef),pattern = "s\\("))
  # 
  
  wald.stat <- t(gam.coef[nc.coef.index]) %*% 
                                    ginv(robust.vcov[nc.coef.index,nc.coef.index]) %*% 
                                    gam.coef[nc.coef.index]
  wald.df <- length(nc.coef.index)
  wald.pval <- pchisq(q = wald.stat, df = wald.df, lower.tail = FALSE)
  
  return(list(NCIV = NC.name, pval = wald.pval, IV.adjusted=IV.adjusted))
}


AG.GAM.results <- data.table(NCIV = NA, pval = NA, IV.adjusted = NA)

for (nc in NCIVs) {
  results.no.adj <- get.GAM.pval(GAM.full.formula = Gam.formula(NC.name = nc,IV.adjusted = FALSE),
                                     NC.name = nc,
                                     IV.adjusted = FALSE)
  results.w.adj <- get.GAM.pval(GAM.full.formula = Gam.formula(NC.name = nc,IV.adjusted = TRUE),
                                    NC.name = nc,
                                    IV.adjusted = TRUE)
  AG.GAM.results <- rbindlist(list(AG.GAM.results, results.no.adj, results.w.adj))
}

AG.GAM.results <- AG.GAM.results[-1,]
AG.GAM.results

AG.GAM.results[,pval := ifelse(pval<0.01,"<0.01",round(pval,3))]

AG.GAM.results[,method := "GAM (robust se)"]


# Save results ------------------------------------------------------------

AG.tab4.summarized <- AG.tab4.results[,.(pval = paste0(pval, collapse = ", ")), by = c("NCIV","IV.adjusted","method")]
AG.tab4.summarized <- AG.tab4.summarized[,.SD, .SDcols = c("NCIV","pval","IV.adjusted","method")]

results.df <- rbindlist(list(AG.tab4.summarized, AG.Wald.results, AG.GAM.results))

results.casted <- dcast(results.df, "NCIV ~ method + IV.adjusted", value.var = "pval")
results.casted <- results.casted[,c(1,6,7,4,5,2,3)]


results.casted$nciv.names <- c("Addis Ababa","London","Mexico","Tokyo")

kable(results.casted[,c(8,seq(2,7))], format = "simple",
      booktabs = T,
      col.names = c("",rep(c("FALSE","TRUE"),3))) %>% 
  add_header_above(c(" "=1, "IV Adj."=2,"IV Adj."=2,"IV Adj."=2)) %>% 
  add_header_above(c(" "=1, "Seperate (lin, quad)"=2,"Wald"=2,"GAM"=2)) 



# all NCI together (IV adj.) ------------------------------------------

nciv.sq.str <- paste0(NCIVs[2], " + ", "I(",NCIVs[2],"^2)")
for (nciv in NCIVs[3:4]){
  nciv.sq.str <- paste0(nciv.sq.str," + ", paste0(nciv, " + ", "I(", nciv,"^2)"))
}

lm.all.iv.adj.formula <- paste0(outcome.name, " ~ ", nciv.sq.str,
                                " + ", IV.name," + ",
                                "I(",IV.name,"^2)", " + ",
                                paste(cntrls.names,collapse = " + "))
# LM
lm.all.nciv <- lm(lm.all.iv.adj.formula, AG.df)
robust.vcov.all <- vcovHC(lm.all.nciv)
robust.t.stat <- coef(lm.all.nciv)[2:7] / sqrt(diag(robust.vcov.all))[2:7]
robust.p.vals <- 2*pt(abs(robust.t.stat), lm.all.nciv$df.residual, lower.tail = FALSE)

bonf.robust.pval.all <- min(robust.p.vals)*length(robust.p.vals)

# Wald (aka robust F-test)
all.wald.stat <- t(coef(lm.all.nciv)[2:7]) %*% ginv(robust.vcov.all[2:7,2:7]) %*% coef(lm.all.nciv)[2:7]
wald.df <- length(coef(lm.all.nciv)[2:7])
all.wald.pval <- pchisq(q = all.wald.stat, df = wald.df, lower.tail = FALSE)

# Gam not possible due to small sample size

