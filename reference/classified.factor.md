# Create Classified from Factor

Creates classified from factor. Uses
[`classified.default`](https://bergsmat.github.io/yamlet/reference/classified.default.md),
but supplies existing levels by default.

## Usage

``` r
# S3 method for class 'factor'
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

  passed to
  [`classified.default`](https://bergsmat.github.io/yamlet/reference/classified.default.md);
  defaults to `levels(x)`

- labels:

  passed to
  [`classified.default`](https://bergsmat.github.io/yamlet/reference/classified.default.md);
  must be same length as levels(after removing values in `exclude`) and
  must not contain duplicates

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
[`classified.default()`](https://bergsmat.github.io/yamlet/reference/classified.default.md),
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
[`codify.default()`](https://bergsmat.github.io/yamlet/reference/codify.default.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md),
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

## Examples

``` r
a <- factor(c('c','b','a'))
levels(classified(a))
#> [1] "a" "b" "c"
attr(classified(a), 'codelist')
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> [1] "b"
#> 
#> [[3]]
#> [1] "c"
#> 
```
