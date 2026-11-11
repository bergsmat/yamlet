# Create Classified by Default

Creates a factor of subclass 'classified', for which there are
attribute-preserving methods. In particular, classified has a codelist
attribute indicating the origin of its levels: it is constructed from
the codelist attribute of x if available, or from 'levels' and 'labels'
by default. Unlike the case for
[`factor`](https://rdrr.io/r/base/factor.html), length of labels cannot
be one (i.e., different from length of levels).

## Usage

``` r
# Default S3 method
classified(
  x = character(),
  levels,
  labels,
  exclude = NA,
  ordered = is.ordered(x),
  nmax = NA,
  token = character(0),
  ...
)
```

## Arguments

- x:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- levels:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- labels:

  see [`factor`](https://rdrr.io/r/base/factor.html), must have same
  length as levels

- exclude:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- ordered:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- nmax:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- token:

  informative label for messages

- ...:

  ignored

## Value

'classified' 'factor'

## See also

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
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
[`classified.factor()`](https://bergsmat.github.io/yamlet/reference/classified.factor.md),
[`codify.default()`](https://bergsmat.github.io/yamlet/reference/codify.default.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md),
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

## Examples

``` r

# classified creates a factor with a corresponding codelist attribute
classified(c('a','b','c'))
#> [1] a b c
#> attr(,"codelist")
#> attr(,"codelist")[[1]]
#> [1] a
#> 
#> attr(,"codelist")[[2]]
#> [1] b
#> 
#> attr(,"codelist")[[3]]
#> [1] c
#> 
#> Levels: a b c

# codelist 'remembers' the origins of levels
classified(c('a','b','c'), labels = c('A','B','C'))
#> [1] A B C
#> attr(,"codelist")
#> attr(,"codelist")$a
#> [1] A
#> 
#> attr(,"codelist")$b
#> [1] B
#> 
#> attr(,"codelist")$c
#> [1] C
#> 
#> Levels: A B C

# classified is 'reversible'
library(magrittr)
c('a','b','c') %>%
  classified(labels = c('A','B','C')) %>%
  unclassified
#> [1] "a" "b" "c"
#> attr(,"codelist")
#> attr(,"codelist")$A
#> [1] "a"
#> 
#> attr(,"codelist")$B
#> [1] "b"
#> 
#> attr(,"codelist")$C
#> [1] "c"
#> 
```
