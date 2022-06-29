#' ---
#' title:  Assemble Xanomeline Dataset
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
dm %>% decorations

dm %>% resolve %>% head

ex <- 'ex.xpt.gz' %>% gzfile %>% read_xpt
ex %>% head %>% data.frame
ex %>% constant 
ex %<>% select(-(names(constant(ex))))
ex %>% enumerate(EXTRT)
ex %>% enumerate(USUBJID)
ex %>% enumerate(VISIT)
ex %>% enumerate(VISIT, EXDOSE)
ex %>% enumerate(VISITNUM, VISIT)
ex %<>% mutate(VISIT = VISITNUM %>% mimic(VISIT) %>% as_dvec)
ex %>% decorations(VISIT)
ex %<>% select(USUBJID, VISIT, EXDOSE, EXSTDTC)
ex %<>% safe_join(
  dm %>% 
    mutate(USUBJID = SUBJID) %>% 
    resolve(USUBJID) %>% 
    mutate(USUBJID = as.character(USUBJID)) %>%
    select(SUBJID, USUBJID, RFSTDTC)
)
ex %<>% mutate(
  TIME = 
    (
      as.date(as.character(EXSTDTC)) - 
      as.date(as.character(RFSTDTC))
    ) %>% as.second %>% as.hour %>% as.numeric
)
head(ex)
ex %<>% select(SUBJID, VISIT, TIME, AMT = EXDOSE)
# convert to micrograms
ex %<>% mutate(AMT = AMT * 1000)
ex %<>% mutate(
  DV = as_dvec(0), 
  MDV = as_dvec(1), 
  EVID = as_dvec(1), 
  CMT = as_dvec(1)
)
ex %>% decorations(-SUBJID)
ex %<>% decorate('
TIME: [ Time, h]
AMT: [guide: µg]
DV: [ Xanomeline Plasma Concentration, ng/mL]
EVID: [Event Type, [Dose: 1]]
CMT: [ Compartment, [ Depot: 1, Plasma: 2]]
MDV: [ Missing Dependent Value, [ DV not missing: 0, DV missing: 1]]
')

# pc simulated in pc.R
pc  <- io_csv('pc.csv')
pc %>% decorations(-SUBJID)

pc %<>% mutate(AMT = 0, MDV = 0, EVID = ifelse(DV == 0, 2, 0), CMT = 2)   
pc %<>% redecorate(ex)
pc %>% head
pc %<>% redecorate('EVID: [ Event Type, [Other: 2, Observation: 0]]')
is.integer(pc$VISIT)
is.double(ex$VISIT)
x <- bind_rows(
  pc, 
  ex
) %>% left_join(dm)
is.double(x$VISIT)

x %>% decorations(-SUBJID)
x %>% head %>% data.frame
x %<>% select(
  SUBJID, ACTARM, VISIT, TIME, TAD, 
  EVID, CMT, AMT, DV, MDV, 
  AGE, SEX, RACE, HEIGHT, WEIGHT
)

x$MDV[x$DV <= 0] <- 1

x %<>% arrange(SUBJID, TIME, EVID)
x %>% group_by(SUBJID, TIME, EVID) %>% status

x %>% io_csv('xanomeline.csv')
x %>% head
x %>% decorations(-SUBJID)

sessionInfo()
