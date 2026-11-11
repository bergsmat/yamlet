# Reconcile Atttributes of List Members

Reconciles attributes of list members. Recursively arbitrates list
members pairwise, returning the accumulated attributes.

## Usage

``` r
# S3 method for class 'list'
reconcile(x, ...)
```

## Arguments

- x:

  list

- ...:

  passed arguments

## Value

list (of attributes)

## Examples

``` r
library(magrittr)
library(dplyr)
a <- data.frame(study = 1) %>% decorate('study: [Study, [A: 1]]')
b <- data.frame(study = 2) %>% decorate('study: [Study, [B: 2]]')
bind_rows(a, b) %>% decorations
#> - study
#>  - label: Study
#>  - guide
#>   - A: 1
#>   - B: 2
c(a$study, b$study)
#> [1] 1 2
#> attr(,"label")
#> [1] "Study"
#> attr(,"guide")
#> attr(,"guide")$A
#> [1] 1
#> 
#> attr(,"guide")$B
#> [1] 2
#> 
reconcile(list(a$study, b$study))
#> $label
#> [1] "Study"
#> 
#> $guide
#> $guide$A
#> [1] 1
#> 
#> $guide$B
#> [1] 2
#> 
#> 
```
