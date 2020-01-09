library(nlme)
library(csv)
data(Phenobarb)
x <- Phenobarb
x$event  <- with(x, ifelse(is.na(dose), 'conc', 'dose'))
x$value <- with(x, ifelse(is.na(dose), conc, dose))
x$dose <- NULL
x$conc <- NULL
as.csv(x, 'phenobarb.csv')
