---
title:  Xanelomine Model Diagnostic Plots
author: Tim Bergsma
date: "30 June, 2022"
output:
  powerpoint_presentation:
    toc: FALSE
    keep_md: TRUE
    df_print: kable
    slide_level: 2
    reference_doc: template.pptx
---




# Data Diagnostics


## DV vs IPRED by ACTARM per VISIT


![DV-IPRED-ACTARM-VISIT.png](diagnostics_files/figure-pptx/DV-IPRED-ACTARM-VISIT-1.png)


## DV vs IPRED


![x %>% isoplot(DV, PRED)](diagnostics_files/figure-pptx/DV-IPRED-1.png)


## DV vs IPRED log-log

![x %>% isoplot(DV, PRED, trans = 'log10')](diagnostics_files/figure-pptx/DV-IPRED-LOG-1.png)


## DV vs PRED and IPRED


![x %>% isopair(DV, PRED, DV, IPRED)](diagnostics_files/figure-pptx/DV-PRED-DV-IPRED-1.png)


## DV vs IPRED, untransformed and log-log


![x %>% isopair(DV, IPRED, DV, IPRED, trans = c('identity','log10'))](diagnostics_files/figure-pptx/DV-IPRED-DV-IPRED-notrans-log-1.png)


## IWRES vs TIME


![x %>% trendplot(IWRES, TIME)](diagnostics_files/figure-pptx/IWRES-TIME-1.png)


## IWRES vs TIME, untransformed and log-log

![x %>% trendpair(IWRES, TIME, IWRES, TIME, trans2 = c('identity', 'log10'))](diagnostics_files/figure-pptx/IWRES-TIME-IWRES-TIME-untrans-log-1.png)


