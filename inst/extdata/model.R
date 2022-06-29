#' ---
#' title: Model Pharmacokinetics
#' output: html_document
#' theme: united
#' highlight: tango
#' ---

# dm.xpt, vs.xpt, ex.xpt downloaded from https://github.com/phuse-org/TestDataFactory/tree/main/Updated/TDF_SDTM on 2022-06-23.
library(haven)
library(magrittr)
library(dplyr)
library(tidyr)
library(wrangle)
library(ggplot2)
library(nlmixr)
library(yamlet)
library(datetime)


# https://patents.google.com/patent/US5980933A/en
# 
# EXAMPLE 2
# Transdermal Formulation without Polyethylene Glycol
# A 0.5 g sample of xanomeline free base was dissolved in 13.0 g of ethanol (200 proof). A 0.75 g sample of azone was added to the ethanol mixture with stirring. An 11.25 g sample of water was added to the mixture. The mixture was a clear solution. Finally, 0.75 g of Klucel was added to the mixture and stirred until the Klucel was dispersed. The mixture was allowed to stand for 24 hours. A 2.0 g sample of the formulation prepared as described herein was dispensed by syringe into a reservoir-type transdermal adhesive system.
# 
# ______________________________________                                    
# Time      Concentration in Dog                                            
# hours post                                                                
# ng/ml Plasma                                                    
# application                                                               
# 1         2      3       Mean + SEM                             
# ______________________________________                                    
# 0         0         0      0       0 ± 0                               
# 3 19 12 4 11.7 ± 5.3                                                 
# 6 24 17 6 15.7 ± 6.4                                                 
# 9 18 11 4 11.0 ± 4.9                                                 
# 12 12 9 5 8.7 ± 2.5                                                  
# 15 9 8 3 6.7 ± 2.3                                                   
# 24 6 5 3 4.7 ± 1.1                                                   
# 28 9 4 0 4.3 ± 3.2                                                   
# 32 7 3 0 3.3 ± 2.5                                                   
# 48 3 0 3 2.0 ± 1.2                                                   
# 54 0 0 0 0 ± 0                                                       
# 72 0 0 0 0 ± 0                                                       
# ______________________________________                                    
# 
# ______________________________________                                    
# Time      Concentration in Monkey                                         
# hours post                                                                
# ng/ml Plasma                                                    
# application                                                               
# 1         2      3       Mean + SEM                             
# ______________________________________                                    
# 0         0         0      0       0 ± 0                               
# 3 50 59 65 58 ± 5.3                                                  
# 6 44 50 59 51 ± 5.3                                                  
# 8 39 45 45 43 ± 2.5                                                  
# 12 25 42 41 36 ± 6.7                                                 
# 15 25 34 37 22 ± 11.8                                                
# 24 14 13 16 14.3 ± 1.1                                               
# 28 12 8 8 9.3 ± 1.6                                                  
# 32 9 7 7 7.7 ± 0.8                                                   
# 48 5 5 4 4.7 ± 0.4                                                   
# 54 2 2 2 2 ± 0                                                       
# 72 0 0 0 0 ± 0                                                       
# ______________________________________   



# https://bpspubs.onlinelibrary.wiley.com/doi/10.1111/bcp.14872
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6741650/pdf/CNS-9-159.pdf
# https://patentimages.storage.googleapis.com/4f/9c/c0/e3577f3c8dcabd/EP0723781A2.pdf

# A 0.5 g sample of xanomeline free base was dissolved in 
# 13.0 g of ethanol (200 proof). 
# A 0.75 g sample of azone was added to the ethanol mixture with stirring. 
# An 11.25 g sample of water was added to the mixture. 
# The mixture was a clear solution. 
# Finally, 0.75 g of Klucel was added to the mixture and stirred until the Klucel was dispersed. The mixture was allowed to stand for 24 hours. A 2.0 g sample of the formulation prepared as described herein was dispensed by syringe into a reservoir-type transdermal adhesive system.
2 * 0.5 / (0.5 + 13 + 0.75 + 11.25 + .75)
m <- data.frame(
  check.names = F,
#id = c(1, 2, 3 ),#      Mean + SEM
#______________________________________
`0` = c( 0, 0, 0),#       0 ± 0
`3` = c( 50, 59, 65 ), #, 58 ± 5.3
`6` = c(  44, 50, 59 ), #, 51 ± 5.3
`8` = c(  39, 45, 45 ), #, 43 ± 2.5
`12` = c(  25, 42, 41 ), #, 36 ± 6.7
`15` = c(  25, 34, 37 ), #, 22 ± 11.8
`24` = c(  14, 13, 16 ), #, 14.3 ± 1.1
`28` = c(  12, 8, 8 ), #, 9.3 ± 1.6
`32` = c(  9, 7, 7 ), #, 7.7 ± 0.8
`48` = c(  5, 5, 4 ), #, 4.7 ± 0.4
`54` = c(  2, 2, 2 ), #, 2 ± 0
`72` = c(  0, 0, 0 )#, 0 ± 0
)

m <- t(m)
m <- cbind(row.names(m), m)
m <- as.data.frame(m)
row.names(m) <- NULL
names(m) <- c('time',1,2,3)
m %<>% mutate(across(everything(), as.numeric))
m %<>% pivot_longer(`1`:`3`, names_to = 'id', values_to = 'dv')
m$id %<>% as.integer
m %<>% mutate(mdv = ifelse(dv == 0, 1, 0))
m %<>% mutate(evid = 0)
m$cmt <- 2
dose <- m %>% filter(time == 0)
dose$amt <- 0.038* 1E6 # g -> ug; ug/L ~ ng/mL
dose$evid <- 1
dose$mdv <- 1
dose$cmt <- 1
m %<>% bind_rows(dose)
m %<>% arrange(id, time, evid)
m %<>% mutate(across(where(is.numeric), replace_na, 0))
m %>% head
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4816996/

# male cyno ~ 8 kg, female ~ 5 kg

m %>% ggplot(aes(time, log(dv), color = factor(id))) + geom_point()

# initial conc is exp(4.5)~ 90 ng/mL = 90 ug/L for 3800 ug: 

m1 <- nlmixr(
  save = T,
  data = m,
  est = 'focei',
  outerOpt='bobyqa',
  object = function(){
    ini({
      lCl <- log(3/20)      #log Cl (L/hr)
      lVc <- log(400)   #log Vc (L)
      lKA <- 0.1      #log Ka (1/hr)
      prop.err <- c(0, 0.2, 1)
      eta.Cl ~ 0.1 ## BSV Cl
      #eta.Vc ~ 0.1 ## BSV Vc
      #eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl + eta.Cl)
      Vc = exp(lVc)# + eta.Vc)
      KA <- exp(lKA)# + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ prop(prop.err)
    })
  }
)

m1 %>%  ggplot(aes(PRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))
m1 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))

m2 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(37)      #log Cl (L/hr)
      lVc <- log(553)   #log Vc (L)
      lKA <- 0.849     #log Ka (1/hr)
      prop.err <- .228
      #eta.Cl ~ 0.1 ## BSV Cl
      eta.Vc ~ 0.1 ## BSV Vc
      #eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl)# + eta.Cl)
      Vc <- exp(lVc + eta.Vc)
      KA <- exp(lKA)# + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ prop(prop.err)
    })
  }
)
m2 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))
m3 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(37)      #log Cl (L/hr)
      lVc <- log(553)   #log Vc (L)
      lKA <- 0.849     #log Ka (1/hr)
      prop.err <- .228
      #eta.Cl ~ 0.1 ## BSV Cl
      # eta.Vc ~ 0.1 ## BSV Vc
      eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl)# + eta.Cl)
      Vc <- exp(lVc)# + eta.Vc)
      KA <- exp(lKA + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ prop(prop.err)
    })
  }
)
m3 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))
m4 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(37)      #log Cl (L/hr)
      lVc <- log(553)   #log Vc (L)
      lKA <- 0.849     #log Ka (1/hr)
      prop.err <- .228
      add.err <- 1
      #eta.Cl ~ 0.1 ## BSV Cl
      eta.Vc ~ 0.1 ## BSV Vc
      #eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl)# + eta.Cl)
      Vc <- exp(lVc + eta.Vc)
      KA <- exp(lKA)# + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ prop(prop.err) + add(add.err)
    })
  }
)
m4 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))

m5 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(37)      #log Cl (L/hr)
      lVc <- log(553)   #log Vc (L)
      lKA <- 0.849     #log Ka (1/hr)
      #prop.err <- .228
      add.err <- 1
      #eta.Cl ~ 0.1 ## BSV Cl
      eta.Vc ~ 0.1 ## BSV Vc
      #eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl)# + eta.Cl)
      Vc <- exp(lVc + eta.Vc)
      KA <- exp(lKA)# + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ add(add.err)
    })
  }
)
m5 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))

m6 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(37)      #log Cl (L/hr)
      lVc <- log(553)   #log Vc (L)
      lKA <- 0.849     #log Ka (1/hr)
      #prop.err <- .228
      add.err <- 1
      eta.Cl ~ 0.1 ## BSV Cl
      #eta.Vc ~ 0.1 ## BSV Vc
      #eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl + eta.Cl)
      Vc <- exp(lVc)# + eta.Vc)
      KA <- exp(lKA)# + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ add(add.err)
    })
  }
)
m6 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))

m7 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(37)      #log Cl (L/hr)
      lVc <- log(553)   #log Vc (L)
      lKA <- 0.849     #log Ka (1/hr)
      #prop.err <- .228
      add.err <- 1
      #eta.Cl ~ 0.1 ## BSV Cl
      #eta.Vc ~ 0.1 ## BSV Vc
      eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl)# + eta.Cl)
      Vc <- exp(lVc)# + eta.Vc)
      KA <- exp(lKA + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ add(add.err)
    })
  }
)
m7 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))
m8 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(34.2)      #log Cl (L/hr)
      lVc <- log(537)   #log Vc (L)
      lKA <- log(2.31)     #log Ka (1/hr)
      #prop.err <- .228
      add.err <- 4.52
      eta.Cl ~ 0.1 ## BSV Cl
      #eta.Vc ~ 0.1 ## BSV Vc
      eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl + eta.Cl)
      Vc <- exp(lVc)# + eta.Vc)
      KA <- exp(lKA + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ add(add.err)
    })
  }
)
m8 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))

m9 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(34.2)      #log Cl (L/hr)
      lVc <- log(537)   #log Vc (L)
      lKA <- log(2.31)     #log Ka (1/hr)
      prop.err <- .1
      add.err <- 4.52
      #eta.Cl ~ 0.1 ## BSV Cl
      #eta.Vc ~ 0.1 ## BSV Vc
      eta.KA ~ 0.1 ## BSV Ka
    })
    model({
      Cl <- exp(lCl)# + eta.Cl)
      Vc <- exp(lVc)# + eta.Vc)
      KA <- exp(lKA + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ add(add.err)+prop(prop.err)
    })
  }
)
m9 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))

# m2 has very good mvof
# m7 has very clean fit
# m5 has best DV ~ IPRED
# add Vc BSV (like m5) to m7
m10 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(34.2)      #log Cl (L/hr)
      lVc <- log(537)   #log Vc (L)
      lKA <- log(2.31)     #log Ka (1/hr)
      #prop.err <- .228
      add.err <- 4.52
      #eta.Cl ~ 0.1 ## BSV Cl
      eta.Vc ~ 0.1 ## BSV Vc
      eta.KA ~ 0.8 ## BSV Ka
    })
    model({
      Cl <- exp(lCl)# + eta.Cl)
      Vc <- exp(lVc + eta.Vc)
      KA <- exp(lKA + eta.KA)
      ## After the differential equations are defined
      kel <- Cl / Vc;
      d/dt(depot)    = -KA*depot;
      d/dt(centr)  =  KA*depot-kel*centr;
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ add(add.err)
    })
  }
)
m10 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))
m10 %>%  ggplot(aes(IPRED, CWRES)) + geom_point() + geom_smooth()
m10 %>%  ggplot(aes(TIME, CWRES)) + geom_point() + geom_smooth()
m10 %>% vpc_ui

m11 <- nlmixr(
  data = m,
  est = 'focei',
  object = function(){
    ini({
      lCl <- log(33.3)      #log Cl (L/hr)
      lVc <- log(538)   #log Vc (L)
      lKA <- log(2.3)     #log Ka (1/hr)
      lVp <- log(1)  # log Vp (L)
      lQ  <- log(10000) # log Q (1/hr)
      #prop.err <- .228
      add.err <- 3.91
      #eta.Cl ~ 0.1 ## BSV Cl
      eta.Vc ~ 0.157 ## BSV Vc
      eta.KA ~ 0.882 ## BSV Ka
    })
    model({
      Cl <- exp(lCl)# + eta.Cl)
      Vc <- exp(lVc + eta.Vc)
      KA <- exp(lKA + eta.KA)
      Vp <- exp(lVp)
      Q  <- exp(lQ)
      ## After the differential equations are defined
      kel <- Cl / Vc
      k23 <- Q / Vc
      k32 <- Q / Vp
      d/dt(depot)    = -KA*depot
      d/dt(centr)  =  KA*depot-kel*centr - k23 * centr + k32 * peri
      d/dt(peri)  =  k23 * centr - k32 * peri
      ## And the concentration is then calculated
      cp = centr / Vc;
      ## Last, nlmixr is told that the plasma concentration follows
      ## a proportional error (estimated by the parameter prop.err)
      cp ~ add(add.err)
    })
  }
)
m11 %>%  ggplot(aes(IPRED, DV)) + geom_point() + theme(aspect.ratio = 1) + geom_abline(aes(slope = 1, intercept = 0))
m11 %>%  ggplot(aes(IPRED, CWRES)) + geom_point() + geom_smooth()
m11 %>%  ggplot(aes(TIME, CWRES)) + geom_point() + geom_smooth()
m11 %>% vpc_ui

# extra compartment does not really help


# m10 is final:
#             Parameter  Est.     SE %RSE Back-transformed(95%CI) BSV(CV%) Shrink(SD)%
# lCl     log Cl (L/hr)  3.51 0.0437 1.25       33.3 (30.6, 36.3)                     
# lVc        log Vc (L)  6.29  0.107 1.69          538 (436, 662)     15.7    0.0499%<
# lKA     log Ka (1/hr) 0.834  0.338 40.5        2.3 (1.19, 4.47)     88.2      26.8%=
# add.err                3.91                                3.91   
# 

# 10-fold mass difference (4kg monkey, 80 kg human)
# clh = 33.3 * 10^.75 = 187 cl/f
# vch = 538 * 10^1 = 5380  vc/f
