#' ---
#' title:  Xanelomine Model Diagnostic Plots
#' author: Tim Bergsma
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   powerpoint_presentation:
#'     toc: FALSE
#'     keep_md: TRUE
#'     df_print: kable
#'     slide_level: 2
#'     reference_doc: template.pptx
#' ---
#+ setup, include = FALSE
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(error = FALSE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(dev = c('png','pdf'))
knitr::opts_chunk$set(dpi = 600)
library(knitr)
library(magrittr)
library(dplyr)
library(tidyr)
library(wrangle)
library(ggplot2)
library(yamlet)
library(metaplot)
library(plotscale)

x <- 'xanolemine-mod.csv.gz' %>% gzfile %>% read.csv(na.strings = '.')
x %<>% decorate( read_yamlet('xanolemine-mod.yaml'))
x %<>% filter(cp > 1)
x %<>% ggready(parse = F)
x %>% enumerate(DV == 0, IPRED == 0)

isoplot <- function(
    x, 
    endpoint, versus, 
    alpha = 0.5, 
    log = T, 
    trans = 'identity',
    ... 
  ){
  xrange <- x %>% select( {{ versus }}) %>% extract2(1) %>% range(na.rm = T)
  yrange <- x %>% select( {{ endpoint }}) %>% extract2(1) %>% range(na.rm = T)
  min <- min(xrange[[1]], yrange[[1]])
  max <- max(xrange[[2]], yrange[[2]])
  
  x %>%
    ggplot(aes(x = {{ versus }}, y = {{ endpoint }} )) +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(alpha = alpha) + 
    theme_bw() +
    theme(aspect.ratio = 1) + 
    geom_smooth(method = 'lm', formula = 'y ~ x') +
    scale_x_continuous(trans = trans, limits = c(min, max)) +
    scale_y_continuous(trans = trans, limits = c(min, max))
}


isopair <- function(
    x, 
    endpoint1, 
    versus1, 
    endpoint2, 
    versus2, 
    alpha = 0.5, # can be length 2
    trans = 'identity',
    ...
  ){
  alpha = rep(alpha, 2)
  trans = rep(trans, 2)
  multiplot(
    isoplot(x, {{ endpoint1 }}, {{versus1}}, alpha = alpha[[1]], trans = trans[[1]], ...),
    isoplot(x, {{ endpoint2 }}, {{versus2}}, alpha = alpha[[2]], trans = trans[[2]], ...) 
  )
}


trendplot <- function(
    x, 
    endpoint, versus, 
    alpha = 0.5, 
    log = T, 
    trans = 'identity',
    ref = 0,
    aspect = 1,
    ... 
){
  trans <- rep(trans, 2)
  x %>%
    ggplot(aes(x = {{ versus }}, y = {{ endpoint }} )) +
    geom_hline(yintercept = ref) +
    geom_point(alpha = alpha) + 
    theme_bw() +
    theme(aspect.ratio = aspect) + 
    geom_smooth(method = 'lm', formula = 'y ~ x') +
    scale_y_continuous(trans = trans[[1]]) +
    scale_x_continuous(trans = trans[[2]])
}

trendpair <- function(
    x, 
    endpoint1, 
    versus1, 
    endpoint2, 
    versus2, 
    alpha = 0.5, # can be length 2
    trans1 = 'identity',
    trans2 = 'identity',
    ref = 0,
    aspect = 1,
    ...
){
  alpha = rep(alpha, 2)
  trans1 = rep(trans1, 2)
  trans2 = rep(trans2, 2)
  ref = rep(ref, 2)
  aspect = rep(aspect, 2)
  multiplot(
    trendplot(x, {{ endpoint1 }}, {{versus1}}, alpha = alpha[[1]], trans = trans1, aspect = aspect[[1]], ref= ref[[1]], ...),
    trendplot(x, {{ endpoint2 }}, {{versus2}}, alpha = alpha[[2]], trans = trans2, aspect = aspect[[2]], ref= ref[[2]], ...) 
  )
}

#'
#' # Data Diagnostics
#' 

#'
#' ## DV vs IPRED by ACTARM per VISIT
#' 
#+ DV-IPRED-ACTARM-VISIT, fig.width = 9.69, fig.height = 4.3, fig.cap = 'DV-IPRED-ACTARM-VISIT.png'
#(
  x %>%
  ggplot(aes(IPRED, DV, color = ACTARM)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~VISIT, ncol = 3) +
  theme_bw() +
  theme(aspect.ratio = 1, legend.position = 'top', legend.title = element_blank()) +
  geom_abline(aes(slope = 1, intercept = 0))
  #) %>% devsize(3,3,verbose = T)
#'
#' ## DV vs IPRED
#' 
#+ DV-IPRED, fig.width = 3.65, fig.height = 3.52, fig.cap = "x %>% isoplot(DV, PRED)"
x %>% isoplot(DV, PRED)
#'
#' ## DV vs IPRED log-log
#+ DV-IPRED-LOG, fig.width = 3.65, fig.height = 3.52, fig.cap = "x %>% isoplot(DV, PRED, trans = 'log10')"
x %>% isoplot(DV, PRED, trans = 'log10')
#'
#' ## DV vs PRED and IPRED
#'
#+ DV-PRED-DV-IPRED, fig.width = 7.3, fig.height = 3.52, fig.cap = "x %>% isopair(DV, PRED, DV, IPRED)"
x %>% isopair(DV, PRED, DV, IPRED)
#'
#' ## DV vs IPRED, untransformed and log-log
#' 
#+ DV-IPRED-DV-IPRED-notrans-log, fig.width = 7.3, fig.height = 3.52, fig.cap = "x %>% isopair(DV, IPRED, DV, IPRED, trans = c('identity','log10'))"
x %>% isopair(DV, IPRED, DV, IPRED, trans = c('identity','log10'))
#'
#' ## IWRES vs TIME
#' 
#+ IWRES-TIME, fig.width = 3.65, fig.height = 3.52, fig.cap = "x %>% trendplot(IWRES, TIME)"
x %>% trendplot(IWRES, TIME) #%>% devsize(3,3,verbose = T)
#'
#' ## IWRES vs TIME, untransformed and log-log
#+ IWRES-TIME-IWRES-TIME-untrans-log, fig.width = 7.3, fig.height = 3.52, fig.cap = "x %>% trendpair(IWRES, TIME, IWRES, TIME, trans2 = c('identity', 'log10'))"
x %>% trendpair(IWRES, TIME, IWRES, TIME, trans2 = c('identity', 'log10'))
#'

