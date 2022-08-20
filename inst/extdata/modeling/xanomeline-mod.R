#' ---
#' title:  Integrate Xanomeline Analysis Dataset with Model Output
#' output: html_document
#' theme: united
#' highlight: tango
#' ---

library(haven)
library(magrittr)
library(dplyr)
library(tidyr)
library(wrangle)
library(ggplot2)
library(nlmixr)
library(yamlet)
library(datetime)
library(csv)

x <- io_csv('../xanomeline.csv', gz = TRUE)
x %>% head
x %<>% rename(ID = SUBJID)
x %>% decorations(-ID)
x %>% enumerate(ACTARM, EVID, DV == 0)
x %<>% filter(ACTARM > 0)
x %<>% filter(EVID == 1 | MDV == 0)
x %>% group_by(ID, TIME, EVID) %>% status
x %>% head
set.seed(0)
# mod <- nlmixr(
#   data = x,
#   est = 'saem',
#   object = function(){
#     ini({
#       lCl      <- log(200)   # log Cl (L/hr)
#       lVc      <- log(5000)  # log Vc (L)
#       lKA      <- log(2)     # log Ka (1/hr)
#       prop.err <- .01        # prop.err
#       eta.Vc   ~ 0.01         # BSV Vc
#     })
#     model({
#       Cl <- exp(lCl)
#       Vc <- exp(lVc + eta.Vc)
#       KA <- exp(lKA )
#       kel <- Cl / Vc
#       d/dt(depot) = - KA * depot
#       d/dt(centr) =   KA * depot - kel * centr
#       cp = centr / Vc
#       cp ~ prop(prop.err)
#     })
#   }
# )
# mod
# ── nlmixr SAEM(ODE); OBJF not calculated fit ──────────────────────────────────────────────────────────────────────────────────
# 
# Gaussian/Laplacian Likelihoods: AIC(mod) or mod$objf etc. 
# FOCEi CWRES & Likelihoods: addCwres(mod) 
# 
# ── Time (sec mod$time): ───────────────────────────────────────────────────────────────────────────────────────────────────────
# 
# saem setup table covariance other
# elapsed 204.94 0.278  0.14        0.1 1.052
# 
# ── Population Parameters (mod$parFixed or mod$parFixedDf): ────────────────────────────────────────────────────────────────────
# 
#              Parameter  Est.       SE    %RSE    Back-transformed(95%CI) BSV(CV%) Shrink(SD)%
# lCl      log Cl (L/hr)  5.23   0.0175   0.334             187 (180, 193)                     
# lVc         log Vc (L)  8.58  8.8e-05 0.00103 5.3e+03 (5.3e+03, 5.3e+03)     21.6      8.62% 
# lKA      log Ka (1/hr) 0.837 0.000544  0.0649          2.31 (2.31, 2.31)                     
# prop.err      prop.err 0.182                                       0.182                     
# 
# Covariance Type (mod$covMethod): |fim|
#   Fixed parameter correlations in mod$cor
# No correlations in between subject variability (BSV) matrix
# Full BSV covariance (mod$omega) or correlation (mod$omegaR; diagonals=SDs) 
# Distribution stats (mean/skewness/kurtosis/p-value) available in mod$shrink 
# 
# ── Fit Data (object mod is a modified tibble): ────────────────────────────────────────────────────────────────────────────────
# # A tibble: 3,116 × 18
# ID     TIME    DV  PRED    RES IPRED   IRES  IWRES eta.Vc    cp   depot  centr    Cl    Vc    KA    kel   tad dosenum
# <fct> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>  <dbl>  <dbl> <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>   <dbl>
#   1 1002      1  9.16  8.96  0.200  8.52  0.650  0.420 0.0527  8.52 5359.   47554.  187. 5585.  2.31 0.0334     1       1
# 2 1002      2  9.79  9.54  0.246  9.08  0.710  0.430 0.0527  9.08  532.   50710.  187. 5585.  2.31 0.0334     2       1
# 3 1002      4  8.33  8.99 -0.658  8.58 -0.252 -0.161 0.0527  8.58    5.24 47930.  187. 5585.  2.31 0.0334     4       1
# # … with 3,113 more rows
# 
# sapply(mod, class)
# mod %<>% mutate(across(where(is.numeric), signif, digits = 4))
# mod %>% as.data.frame %>% saveRDS('mod.Rds')
# mod %<>% decorate(x)
# mod %>% decorations(-ID)
# mod %<>% decorate('
# PRED: [Population Prediction, ng/mL]
# RES: [ Residual, ng/mL]
# IPRED: [Individual Prediction, ng/mL]
# IRES: [ Individual Residual, ng/mL]
# IWRES: [ Individual Weighted Residual, ng/mL]
# eta.Vc: [ Individual Random Effect on Vc ]
# cp: [ Plasma Concentration, ng/mL]
# depot: [Drug Amount in Depot, µg]
# centr: [Drug Amount in Central, µg]
# Cl: [ Apparent Clearance, L/h]
# Vc: [ Apparent Volume, L]
# KA: [ Absorption Rate Constant, 1/h]
# kel: [ Elimination Rate Constant, 1/h]
# tad: [Time Since Most Recent Dose, h]
# dosenum: Dose Number
# ')
# mod %>% io_csv('mod.csv', gz = TRUE)
mod <- io_csv('mod.csv', gz = TRUE)
head(x)
head(mod)
intersect(names(mod), names(x))
nrow(x)
mod %>% group_by(ID, TIME) %>% status
x   %>% group_by(ID, TIME, EVID) %>% status
mod$DV <- NULL
x %<>% left_join(mod)
nrow(x)
x %>% decorations(-ID)
x %>% head(2)
x %>% enumerate(EVID, is.na(IPRED), TIME == 0)
x %>% filter(is.na(IPRED), TIME != 0)
# x %>% filter(ID == 1254)
x$IPRED[x$TIME == 0 & is.na(x$IPRED)] <- 0
x %<>% filter(!is.na(IPRED))
x %>% enumerate(EVID)
x %>% head
# x %>%
#   ggready(parse = FALSE) %>% # decorations(IPRED, DV, ACTARM)
#   ggplot(aes(IPRED, DV, color = ACTARM)) + 
#   geom_point(alpha = 0.5) +
#   facet_wrap(~VISIT, ncol = 2) +
#   theme_bw() +
#   theme(aspect.ratio = 1) +
#   geom_abline(aes(slope = 1, intercept = 0))

x %>% io_csv('xanomeline-mod.csv', gz = TRUE)

sessionInfo()

