# Convert to Codes

Converts to Codes. Generic, with method
[`codify.default`](https://bergsmat.github.io/yamlet/reference/codify.default.md)

## Usage

``` r
codify(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

see methods

## Examples

``` r
example(codify.default)
#> 
#> cdfy.d> library(magrittr)
#> 
#> cdfy.d> # convert to decorated integer
#> cdfy.d> c('knife','fork','spoon') %>% codify
#> [1] 2 1 3
#> attr(,"guide")
#> attr(,"guide")$fork
#> [1] 1
#> 
#> attr(,"guide")$knife
#> [1] 2
#> 
#> attr(,"guide")$spoon
#> [1] 3
#> 
#> 
#> cdfy.d> # implement offset
#> cdfy.d> c('knife','fork','spoon') %>% codify(-1)
#> [1] 1 0 2
#> attr(,"guide")
#> attr(,"guide")$fork
#> [1] 0
#> 
#> attr(,"guide")$knife
#> [1] 1
#> 
#> attr(,"guide")$spoon
#> [1] 2
#> 
#> 
#> cdfy.d> # globally defeat the 'persistence' paradigm
#> cdfy.d> options(yamlet_persistence = FALSE)
#> 
#> cdfy.d> c('knife','fork','spoon') %>% 
#> cdfy.d+   codify %>% 
#> cdfy.d+   class # integer
#> [1] "integer"
#> 
#> cdfy.d> # remove option to restore default persistence paradigm
#> cdfy.d> options(yamlet_persistence = NULL)
#> 
#> cdfy.d> c('knife','fork','spoon') %>% 
#> cdfy.d+   codify %>%
#> cdfy.d+   class # dvec
#> [1] "dvec"
#> 
#> cdfy.d> # locally defeat persistence paradigm
#> cdfy.d> c('knife','fork','spoon') %>% 
#> cdfy.d+   codify(persistence = FALSE) %>% 
#> cdfy.d+   class # integer
#> [1] "integer"
#> 
#> cdfy.d> # pass levels
#> cdfy.d> c('knife','fork','spoon') %>%
#> cdfy.d+   codify(levels = c('spoon', 'knife', 'fork'))
#> [1] 2 3 1
#> attr(,"guide")
#> attr(,"guide")$spoon
#> [1] 1
#> 
#> attr(,"guide")$knife
#> [1] 2
#> 
#> attr(,"guide")$fork
#> [1] 3
#> 
```
