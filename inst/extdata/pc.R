#' ---
#' title: Create PC Domain
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
ex <- 'ex.xpt.gz' %>% gzfile %>% read_xpt
vs <- 'dm.xpt.gz' %>% gzfile %>% read_xpt
dm <- 'dm.xpt.gz' %>% gzfile %>% read_xpt

dm %>% data.frame %>% head(3)
dm %>% constant
dm %<>% select(-(names(constant(dm))))
dm %>% decorations
dm %<>% select(USUBJID, SUBJID, RFSTDTC, AGE, SEX, RACE, ACTARM, ACTARMCD)           
dm %$% SUBJID %>% as.integer %>% is.na %>% any

dm %>% decorations(SUBJID)
dm %<>% mutate(SUBJID = SUBJID %>% as.integer) %>% decorate(dm)
dm %>% decorations(SUBJID)

dm %<>% mutate(SUBJID = SUBJID %>% mimic(USUBJID)) %>% select(-USUBJID)
dm %>% itemize(ACTARMCD, ACTARM)
dm %<>% mutate(
  ACTARM = ACTARM %>% 
    classified(
      levels = c(
        'Screen Failure',
        'Placebo',
        'Xanomeline Low Dose',
        'Xanomeline High Dose'
      )
    ) %>% 
    as.integer(-2)
) %>% select(-ACTARMCD)

dm %>% modify(SUBJID, guide = NULL) %>% decorations

dm %>% enumerate(SEX)
dm %<>% mutate(SEX = SEX %>% classified(labels = c('Female','Male')) %>% as.integer(-1)) %>% decorate(dm)
dm %>% decorations(SEX)
dm %>% enumerate(RACE)
dm %<>% mutate(
  RACE = RACE %>% classified(
    levels = c(
      'WHITE', 
      'BLACK OR AFRICAN AMERICAN', 
      'ASIAN', 
      'AMERICAN INDIAN OR ALASKA NATIVE'
    )
  ) %>% as.integer(-1)
) %>% decorate(dm)
dm %>% select(-SUBJID) %>% decorations
dm %<>% decorate('
RFSTDTC: [ guide: "%Y-%m-%d" ]
AGE:     [ guide: year ]
')
dm %>% head
dm %>% decorations(SEX, RACE)
dm %>% resolve %>% head
dm %>% resolve %>% resolve %>% head
dm %>% resolve %>% decorations(SEX, RACE)
dm %>% resolve %>% resolve %>% decorations(SEX, RACE)
dm %>% resolve %>% head
dm %>% resolve %>% desolve %>% head
dm %>% resolve(SEX) %>% head
dm %>% resolve(SEX) %>% resolve %>% head
dm %>% resolve(SEX) %>% resolve %>% resolve %>% head
dm %<>% head
dm %<>% select(SEX)



a <- 4:6
attr(a, 'guide') <- c('d','e','f')
a
classified(a)
class(classified(a))
str(classified(a))

b <- 4:6
attr(b, 'codelist') <- list(d = 4, e = 5, f = 6)
b
classified(b)
class(classified(b))
str(classified(b))






