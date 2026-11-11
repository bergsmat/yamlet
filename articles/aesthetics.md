# Yamlet Plot Aesthetics

## Behavior

Plotting a decorated data.frame in ggplot() has special behavior for
aesthetics defined using yamlet decorations. If there is only one
dataset (typical), the aesthetics are coordinated with the (levels of
the) corresponding variable.

If one or more additional layers provide non-default data, a single set
of consensus decorations is accumulated, with each successive dataset
overriding all previous decorations where conflicts exist; then,
consensus decorations are enforced on all datasets.

## Example

In the example below, points are plotted using the observed data, then
summary data is plotted as a separate layer, supplying its own data. the
plot is rendered *without* decorations (left) and *with* decorations
(right).

``` r

library(magrittr)
library(ggplot2)
library(yamlet)
library(dplyr)
library(metaplot)

set.seed(10)
obs <- data.frame(
  time = 1:100,
  group = c('a','b'),
  concentration = rnorm(100)
)

# observed data
obs %<>% decorate('
  time: [ Time, h ]
  concentration: [ Concentration, ng/mL ]
  group: [ 
    Group, 
    [ a, b ], 
    color: [ green, yellow ] , 
    linetype: [ dashed, dotdash ]
  ]
')

# summary data
sum <- obs %>% group_by(group) %>% summarize(mean = mean(concentration))
sum %<>% undecorate %>% decorate('
  group: [ 
    Subset, 
    [ a, b ], 
    color: [ blue, magenta ], 
    shape: [ 15, 19 ]
  ]
')


q <- obs %>%
  undecorate %>%
  ggplot(aes(time, concentration)) +
  geom_point(aes(color = group, shape = group)) +
  geom_hline(
    data = undecorate(sum), 
    aes(
      yintercept = mean, 
      color = group, 
      linetype = group
    )
  ) + 
  theme(legend.position = "top") +
  labs(subtitle = 'without decorations') +
  symmetric()

p <- obs %>%
  resolve %>%
  ggplot(aes(time, concentration)) +
  geom_point(aes(color = group, shape = group)) +
  geom_hline(
    data = resolve(sum), 
    aes(
      yintercept = mean, 
      color = group, 
      linetype = group
    )
  ) + 
  theme(legend.position = "top") +
  labs(subtitle = 'with decorations') + 
  symmetric()


multiplot(q, p) # %>% devsize(2,2)
```

    ## Warning: mismatched label attributes: ignoring 'Group' in favor of 'Subset'

![](aesthetics_files/figure-html/Layered%20Aesthetics-1.png)

- Notice that the label (on the right) for color and shape is ‘Subset’,
  not ‘Groups’, because the last layer wins.
- Likewise, colors are blue and magenta, not green and yellow, because
  the summary layer was supplied last.
- The summary layer respects the linetype defined earlier, having no
  such decoration in its own data.
- The observations layer respects the shapes defined with the summary
  layer, which in fact are not even used by the summary layer.

This example is a bit contrived for heuristic value. As a best practice,
consider specifying all aesthetics in the default data only.

``` r

sessionInfo()
```

    ## R version 4.6.2 (2026-10-29)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.5 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] metaplot_0.8.4 dplyr_1.2.1    yamlet_1.5.1   ggplot2_4.0.3  magrittr_2.0.5
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyr_1.3.2         sass_0.4.10         generics_0.1.4     
    ##  [4] xml2_1.6.0          lattice_0.23-1      stringi_1.8.9      
    ##  [7] digest_0.6.39       evaluate_1.0.5      grid_4.6.2         
    ## [10] RColorBrewer_1.1-3  fastmap_1.2.0       jsonlite_2.0.0     
    ## [13] latexpdf_0.1.8      gridExtra_2.3.1     purrr_1.2.2        
    ## [16] viridisLite_0.4.3   scales_1.4.0        textshaping_1.0.5  
    ## [19] jquerylib_0.1.4     csv_0.6.2           cli_3.6.6          
    ## [22] rlang_1.3.0         units_1.0-1         withr_3.0.3        
    ## [25] cachem_1.1.0        yaml_2.3.12         otel_0.3.0         
    ## [28] tools_4.6.2         kableExtra_1.4.1    vctrs_0.7.3        
    ## [31] R6_2.6.1            png_0.1-9           spork_0.3.5        
    ## [34] lifecycle_1.0.5     stringr_1.6.0       fs_2.1.0           
    ## [37] htmlwidgets_1.6.4   ragg_1.5.2          pkgconfig_2.0.3    
    ## [40] desc_1.4.3          pkgdown_2.2.1       pillar_1.11.1      
    ## [43] bslib_0.12.0        gtable_0.3.6        Rcpp_1.1.2         
    ## [46] glue_1.8.1          data.table_1.18.6.1 systemfonts_1.3.2  
    ## [49] xfun_0.62           tibble_3.3.1        tidyselect_1.2.1   
    ## [52] rstudioapi_0.19.0   knitr_1.52          xtable_1.8-8       
    ## [55] farver_2.1.2        htmltools_0.5.9     labeling_0.4.3     
    ## [58] rmarkdown_2.32      svglite_2.2.2       compiler_4.6.2     
    ## [61] S7_0.2.2            encode_0.3.7
