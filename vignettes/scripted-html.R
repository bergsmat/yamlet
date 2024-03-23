## ----include = FALSE---------------------------------------------------
knitr::opts_chunk$set(dpi = 600, out.width = '50%')

## ---- message = FALSE, warning = FALSE---------------------------------
library(magrittr)
library(ggplot2)
library(tablet)
library(yamlet)
library(dplyr)
library(kableExtra)

## ----------------------------------------------------------------------
x <- data.frame(
  time = 1:10, 
  work = (1:10)^1.5, 
  group = 1:2, 
  set = c(rep('delta',5), rep('gamma', 5))
)
x %<>% decorate('
 time: [ Time_cum.^alpha, h ]
 work: [ Work_total_obs, kg*m^2/s^2 ]
 group: [ Group, [ Second^\\*: 2, First^#: 1 ]]
 set: [ Set, [ gamma, delta ]]
')
x %>% decorations

## ---- fig.width = 3.73, fig.height = 2.52------------------------------
x %>% 
  resolve %>% 
  ggplot(aes(time, work, color = group, shape = set)) + 
  geom_point()

## ---- fig.width = 3.73, fig.height = 2.52------------------------------
x %>% 
  scripted %>% 
  ggplot(aes(time, work, color = group, shape = set)) + 
  geom_point()


## ----------------------------------------------------------------------
x %>% 
  scripted %>% 
  group_by(group, set) %>%
  tablet %>%
  as_kable

