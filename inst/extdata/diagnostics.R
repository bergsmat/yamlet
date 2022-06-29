#' ---
#' title:  Diagnostic Plots for a PopPK Model
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


x <- io_csv('xanomeline.csv')
x %>% head
x %<>% rename(ID = SUBJID)
x %>% decorations(-ID)
x %>% enumerate(ACTARM, EVID, DV == 0)
x %<>% filter(ACTARM > 0)
x %<>% filter(EVID == 1 | MDV == 0)


# mod <- nlmixr(
#   data = x,
#   est = 'saem',
#   object = function(){
#     ini({
#       lCl      <- log(200)   # log Cl (L/hr)
#       lVc      <- log(5000)  # log Vc (L)
#       lKA      <- log(2)     # log Ka (1/hr)
#       prop.err <- .01        # prop.err
#       eta.Vc   ~ 0.1         # BSV Vc
#     })
#     model({
#       Cl <- exp(lCl)
#       Vc <- exp(lVc+ eta.Vc)
#       KA <- exp(lKA )
#       kel <- Cl / Vc
#       d/dt(depot) = - KA * depot
#       d/dt(centr) =   KA * depot - kel * centr
#       cp = centr / Vc
#       cp ~ prop(prop.err)
#     })
#   }
# )
# 
# mod %>% saveRDS('mod.Rds')
mod <- readRDS('mod.Rds')

head(x)
mod %<>% data.frame
head(mod)
intersect(names(mod), names(x))
mod$ID %<>% as.character %>% as.integer
nrow(x)
mod %>% group_by(ID, TIME) %>% status
mod$DV <- NULL
x %>% group_by(ID, TIME, -EVID) %>% status



x %<>% left_join(mod)
nrow(x)
x %>% head(2)


