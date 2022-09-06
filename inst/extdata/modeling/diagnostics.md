---
title:  Xanomeline Model Diagnostic Plots
author: Tim Bergsma
date: "06 September, 2022"
output:
  powerpoint_presentation:
    toc: FALSE
    keep_md: TRUE
    df_print: kable
    slide_level: 2
    reference_doc: template.pptx
editor_options: 
  chunk_output_type: console
---








# Data Diagnostics

## Observations vs Individual Predictions by Arm, per Visit

![DV-IPRED-ACTARM-VISIT.png](diagnostics_files/figure-pptx/DV-IPRED-ACTARM-VISIT-1.png)

## Observations vs Individual Predictions by Arm and Visit, and for scales = 'free'

![DV-IPRED-ACTARM-VISIT-GRID.png](diagnostics_files/figure-pptx/DV-IPRED-ACTARM-VISIT-GRID-1.png)

## Observations vs Individual Predictions

![x %>% isoplot(DV, PRED)](diagnostics_files/figure-pptx/DV-PRED-1.png)

## Observations vs Individual Predictions, With/without Grids

![x %>% isoplot(DV, PRED)](diagnostics_files/figure-pptx/DV-PRED-grids-1.png)

## Observations vs Population Predictions, Log-log

![x %>% isoplot(DV, PRED, trans = 'log10')](diagnostics_files/figure-pptx/DV-PRED-LOG-1.png)

## Observations vs Population and Individual Predictions

![x %>% isopair(DV, PRED, DV, IPRED)](diagnostics_files/figure-pptx/DV-PRED-DV-IPRED-1.png)

## Observations vs Individual Predictions, Untransformed and Log-log

![x %>% isopair(DV, IPRED, DV, IPRED, trans = c('identity','log10'))](diagnostics_files/figure-pptx/DV-IPRED-DV-IPRED-notrans-log-1.png)

## Conditional Weighted Residuals vs Time

![x %>% trendplot(CWRES, TIME) + symmetric()](diagnostics_files/figure-pptx/CWRES-TIME-1.png)

## Conditional Weighted Residuals vs Time, Untransformed and Log-log

![x %>% trendpair(CWRES, TIME, CWRES, TIME, trans2 = c('identity', 'log10'), symmetric = TRUE)](diagnostics_files/figure-pptx/CWRES-TIME-CWRES-TIME-untrans-log-1.png)

## Conditional Weighted Residuals vs Time, Full Grid vs Horizontal Grid

![](diagnostics_files/figure-pptx/CWRES-TIME-CWRES-TIME-horizonal-grid-1.png)<!-- -->

## Conditional Weighted Residuals vs Time, Various Horizontal Reference Lines

![](diagnostics_files/figure-pptx/CWRES-TIME-CWRES-TIME-reference-lines-1.png)<!-- -->

## Individual Plots

![](diagnostics_files/figure-pptx/individuals-1.png)<!-- -->


## Individual Plots -- Detailed Panel Strip

![](diagnostics_files/figure-pptx/individuals-detail-1.png)<!-- -->


## Individual Plots -- Unscaled Panel Strip

![](diagnostics_files/figure-pptx/individuals-detail-unscaled-1.png)<!-- -->

## Individual Plots -- Scaled Panel Strip

![p + scale_striptext()](diagnostics_files/figure-pptx/individuals-detail-scaled-1.png)

