# Convert to Codes by Default

Reduces x integer codes with appropriate decorations. Result is like
`as.integer(as.numeric(x)) + offset` but has a guide attribute: a list
of integers whose names are the original levels of x. A wrapper for
`as.integer(classified(x)`.

## Usage

``` r
# Default S3 method
codify(
  x,
  offset = 0L,
  ...,
  persistence = getOption("yamlet_persistence", TRUE)
)
```

## Arguments

- x:

  anything for which there is a 'classified' method; see
  [`classified`](https://bergsmat.github.io/yamlet/reference/classified.md)

- offset:

  an integer value to add to intermediate result

- ...:

  passed to
  [`classified`](https://bergsmat.github.io/yamlet/reference/classified.md);
  for relevance, see
  [`classified.default`](https://bergsmat.github.io/yamlet/reference/classified.default.md)

- persistence:

  whether to return 'dvec' (is.integer(): TRUE) or just integer.

## Value

integer (possibly of class dvec)

## See also

as.integer.classified mimic

Other classified:
[`[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-.classified.md),
`[<-.classified()`,
[`[[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.classified.md),
`[[<-.classified()`,
[`as.integer.classified()`](https://bergsmat.github.io/yamlet/reference/as.integer.classified.md),
[`c.classified()`](https://bergsmat.github.io/yamlet/reference/c.classified.md),
[`classified()`](https://bergsmat.github.io/yamlet/reference/classified.md),
[`classified.classified()`](https://bergsmat.github.io/yamlet/reference/classified.classified.md),
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`classified.default()`](https://bergsmat.github.io/yamlet/reference/classified.default.md),
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
[`classified.factor()`](https://bergsmat.github.io/yamlet/reference/classified.factor.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md),
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

## Examples

``` r
library(magrittr)

# convert to decorated integer
c('knife','fork','spoon') %>% codify
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
c('knife','fork','spoon') %>% codify(-1)
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
  codify %>% 
  class # integer
#> [1] "integer"
  
# remove option to restore default persistence paradigm
options(yamlet_persistence = NULL)
c('knife','fork','spoon') %>% 
  codify %>%
  class # dvec
#> [1] "dvec"
  
# locally defeat persistence paradigm
c('knife','fork','spoon') %>% 
  codify(persistence = FALSE) %>% 
  class # integer
#> [1] "integer"
  
# pass levels
c('knife','fork','spoon') %>%
  codify(levels = c('spoon', 'knife', 'fork'))
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
