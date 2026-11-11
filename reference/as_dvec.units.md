# Coerce Units to Decorated Vector

Coerces units to dvec.

## Usage

``` r
# S3 method for class 'units'
as_dvec(x, ...)
```

## Arguments

- x:

  units

- ...:

  passed arguments

## Examples

``` r
library(magrittr)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following object is masked from ‘package:kableExtra’:
#> 
#>     group_rows
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
a <- data.frame(id = 1:4, wt = c(70, 80, 70, 80), sex = c(0,1,0,1))
a %<>% decorate('wt: [ body weight, kg ]')
a %<>% decorate('sex: [ sex, [ female: 0, male: 1]]')
a %<>% decorate('id: identifier')
a %<>% resolve
a %<>% mutate(wt = as_units(wt))
a %<>% mutate(wt = as_dvec(wt))
str(a$wt)
#>  'dvec' num [1:4] 70 80 70 80
#>  - attr(*, "label")= chr "body weight"
#>  - attr(*, "units")= chr "kg"
```
