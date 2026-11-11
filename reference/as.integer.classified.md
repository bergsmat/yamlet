# Coerce Classified to Integer

Coerces classified to integer. Result is like
`as.integer(as.numeric(x)) + offset` but has a guide attribute: a list
of integers whose names are the original levels of x. If you need a
simple integer, consider coercing first to numeric.

## Usage

``` r
# S3 method for class 'classified'
as.integer(
  x,
  offset = 0L,
  ...,
  persistence = getOption("yamlet_persistence", TRUE)
)
```

## Arguments

- x:

  classified, see
  [`classified`](https://bergsmat.github.io/yamlet/reference/classified.md)

- offset:

  an integer value to add to intermediate result

- ...:

  passed to
  [`desolve`](https://bergsmat.github.io/yamlet/reference/desolve.md)

- persistence:

  whether to return 'dvec' (is.integer(): TRUE) or just integer.

## Value

integer (possibly of class dvec)

## See also

codify.default

Other classified:
[`[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-.classified.md),
`[<-.classified()`,
[`[[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.classified.md),
`[[<-.classified()`,
[`c.classified()`](https://bergsmat.github.io/yamlet/reference/c.classified.md),
[`classified()`](https://bergsmat.github.io/yamlet/reference/classified.md),
[`classified.classified()`](https://bergsmat.github.io/yamlet/reference/classified.classified.md),
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`classified.default()`](https://bergsmat.github.io/yamlet/reference/classified.default.md),
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
[`classified.factor()`](https://bergsmat.github.io/yamlet/reference/classified.factor.md),
[`codify.default()`](https://bergsmat.github.io/yamlet/reference/codify.default.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md),
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

## Examples

``` r
library(magrittr)

# create factor with codelist attribute
classified(c('knife','fork','spoon'))
#> [1] knife fork  spoon
#> attr(,"codelist")
#> attr(,"codelist")[[1]]
#> [1] fork
#> 
#> attr(,"codelist")[[2]]
#> [1] knife
#> 
#> attr(,"codelist")[[3]]
#> [1] spoon
#> 
#> Levels: fork knife spoon

# give back a simple numeric
classified(c('knife','fork','spoon')) %>% as.numeric
#> [1] 2 1 3

# intentionally preserve levels as 'guide' attribute
classified(c('knife','fork','spoon')) %>% as.integer
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

# implement offset
classified(c('knife','fork','spoon')) %>% as.integer(-1)
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

# globally defeat the 'persistence' paradigm
options(yamlet_persistence = FALSE)
c('knife','fork','spoon') %>% 
  classified %>%
  as.integer %>% 
  class # integer
#> [1] "integer"
  
# remove option to restore default persistence paradigm
options(yamlet_persistence = NULL)
c('knife','fork','spoon') %>% 
  classified %>%
  as.integer %>% 
  class # dvec
#> [1] "dvec"
  
# locally defeat persistence paradigm
c('knife','fork','spoon') %>% 
  classified %>%
  as.integer(persistence = FALSE) %>% 
  class # integer
#> [1] "integer"
  
```
