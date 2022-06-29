#' ---
#' title:  Xanelomine Model Diagnostic Plots
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
library(csv)
library(metaplot)

x <- 'xanelomine-mod.csv.gz' %>% gzfile %>% read.csv(na.strings = '.')
x %<>% decorate( read_yamlet('xanelomine-mod.yaml'))
x %>%
  ggready(parse = FALSE) %>% # decorations(IPRED, DV, ACTARM)
  ggplot(aes(IPRED, DV, color = ACTARM)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~VISIT, ncol = 2) +
  theme_bw() +
  theme(aspect.ratio = 1) +
  geom_abline(aes(slope = 1, intercept = 0))

isoplot <- function(x, endpoint, versus, alpha = 0.5, ... ){
  xrange <- x %>% select( {{ versus }}) %>% extract2(1) %>% range(na.rm = T)
  yrange <- x %>% select( {{ endpoint }}) %>% extract2(1) %>% range(na.rm = T)
  min <- min(xrange[[1]], yrange[[1]])
  max <- max(xrange[[2]], yrange[[2]])
  
  x %>%
    ggplot(aes(x = {{ versus }}, y = {{ endpoint }} )) +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(alpha = alpha) + 
    theme_bw() +
    ylim(min, max) +
    xlim(min, max) +
    theme(aspect.ratio = 1) + 
    geom_smooth(method = 'lm')
}

x %>% ggready(parse = F) %>% isoplot(DV, PRED)


isopair <- function(endpoint1, endpoint2, versus1, versus2 = versus1, ...){
  multiplot(
    isoplot(x, {{ endpoint1 }}, {{versus1}}, ...),
    isoplot(x, {{ enppoint2 }}, {{versus2}}, ...) 
  )
}

x %>% ggready(parse = F) %>% isopair(DV, DV, PRED, IPRED)
