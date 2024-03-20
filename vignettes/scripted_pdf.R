## ----include = FALSE-------------------------------------------------
knitr::opts_chunk$set(dpi = 600, out.width = '50%')


## --------------------------------------------------------------------
library(magrittr)
library(ggplot2)
library(tablet)
library(yamlet)

## --------------------------------------------------------------------
x <- data.frame(time = 1:10, work = (1:10)^1.5)
x %<>% decorate('
 time: [ Time_cum.^alpha, h ]
 work: [ Work_total_obs, kg*m^2/s^2 ]
')
x %>% decorations

## --------------------------------------------------------------------
x %>% resolve %>% ggplot(aes(time, work)) + geom_point()

## --------------------------------------------------------------------
x %>% scripted %>% ggplot(aes(time, work)) + geom_point()

## --------------------------------------------------------------------
x %>% scripted %>% tablet %>% as_kable

