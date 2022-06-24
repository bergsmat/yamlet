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

dm %>% head
dm %>% implicit_guide %>% resolve %>% head
dm %>% resolve %>% head

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


vs <- 'vs.xpt.gz' %>% gzfile %>% read_xpt
vs %>% data.frame %>% head
vs %>% constant
vs %<>% select(-(names(constant(vs))))
vs %>% enumerate(VSTESTCD, VSTEST, VSSTRESU, VSSTAT)
vs %<>% filter(VSSTAT == '') %>% select(-VSSTAT)
vs %>% enumerate(VSTESTCD, VSTEST, VSSTRESU, VSPOS)
vs %<>% filter(VSTESTCD %in% c('HEIGHT', 'WEIGHT'))
vs %>% constant
vs %<>% select(-(names(constant(vs))))
length(unique(vs$USUBJID))
length(unique(vs$USUBJID[vs$VSBLFL == 'Y']))
vs %>% group_by(USUBJID, VSTESTCD, VISITNUM) %>% status
vs %>% filter(VSTESTCD == 'HEIGHT') %>% nrow
vs %>% 
  filter(VSTESTCD == 'WEIGHT') %>% 
  group_by(USUBJID) %>% 
  filter(!any(VSBLFL == 'Y')) %>% data.frame
vs$VSBLFL[with(vs, USUBJID == '01-702-1082' & VSTESTCD == 'WEIGHT' & VISITNUM == 1)] <- 'Y'
vs %<>% filter(VSTESTCD %in% c('HEIGHT', 'WEIGHT'))
vs %>% enumerate(VSTESTCD, VSBLFL)
vs %<>% filter(VSTESTCD == 'HEIGHT' | VSBLFL == 'Y')
vs %>% enumerate(VSTESTCD)
vs %>% data.frame %>% head
vs %<>% select(USUBJID, VSTESTCD, VSSTRESN)
vs %<>% pivot_wider(names_from=VSTESTCD, values_from = VSSTRESN)
vs %<>% redecorate('
HEIGHT: [ Baseline Height, cm ]
WEIGHT: [ Baseline Weight, kg ]
')
vs %>% decorations

dm %>% head
vs %>% head
vs %>% decorations

dm %<>% mutate(USUBJID = SUBJID %>% resolve %>% as.character)
dm %<>% safe_join(vs) %>% select(-USUBJID)
dm %>% decorations(-SUBJID)
dm %>% head
