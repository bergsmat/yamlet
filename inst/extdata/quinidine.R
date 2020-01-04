library(nlme)
# Pinheiro and Bates, 2000
# Mixed-Effects Models in S and S-Plus
fm1Quin.nlme <-
  nlme(
    conc ~ quinModel(
      Subject, time, conc, dose, interval,
      lV, lKa, lCl
    ),
   data = Quinidine,
   fixed = lV + lKa + lCl ~ 1,
   random = pdDiag(lV + lCl ~ 1),
   groups = ~ Subject,
   start = list(fixed = c(5, -0.3, 2)),
   na.action = na.pass,
   naPattern = ~ !is.na(conc)
  )

ranef <- ranef(fm1Quin.nlme)
ranef$Subject <- rownames(ranef)

dat <- Quinidine
dat$resid[!is.na(dat$conc)] <- resid(fm1Quin.nlme)
dat$Subject <- as.numeric(as.character(dat$Subject))
dat$Subject <- factor(dat$Subject, levels = unique(dat$Subject))
#dat %>% group_by(Subject, time) %>% status
ranef$Subject <- factor(ranef$Subject, levels = levels(dat$Subject))
dat <- merge(dat, ranef)
dat <- dat[order(dat$Subject, dat$time),]
dat <- dat[,c(1:14,16,17,15)]
dat %>% group_by(Subject) %>% slice(1) %>% metaplot(lCl, glyco,  ysmooth = T)


# https://stackoverflow.com/questions/53937854/trouble-with-convergence-in-non-linear-mixed-effects-model-from-pinheiro-and-bat
library(nlme)
fm1Quin.nlme <- nlme(conc ~ quinModel(Subject, time, conc, dose, interval, lV, lKa, lCl),
                     data = Quinidine,
                     fixed = lV + lKa + lCl ~ 1,
                     random = pdDiag(lV + lCl ~ 1),
                     groups = ~ Subject,
                     start = list(fixed = c(5, -0.3, 2)),
                     na.action = na.pass, # R does not have the function na.include
                     naPattern = ~ !is.na(conc))
fm1Quin.fix <- fixef(fm1Quin.nlme)
fm2Quin.nlme <- update(fm1Quin.nlme,
                       fixed = list(lCl ~ glyco, lKa + lV ~ 1),
                       start = c(fm1Quin.fix[3], 0, fm1Quin.fix[2:1]))
fm2Quin.fix <- fixef(fm2Quin.nlme)
fm3Quin.nlme <- update(fm2Quin.nlme,
                       fixed = list(lCl ~ glyco + Creatinine, lKa + lV ~ 1),
                       start = c(3.0291, -0.3631, 0.1503, -0.7458, 5.2893),
                       control = nlmeControl(pnlsTol = 0.0011))

data(Quinidine)
library(csv)
as.csv(Quinidine, 'quinidine.csv')
