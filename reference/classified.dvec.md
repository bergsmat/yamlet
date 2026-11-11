# Classify Decorated Vector

Coerces dvec to 'classified': a factor with a codelist attribute.
Results may differ if explicit_guide() is called first.

## Usage

``` r
# S3 method for class 'dvec'
classified(
  x,
  ...,
  exclude = NA,
  ordered = is.ordered(x),
  nmax = NA,
  token = character(0)
)
```

## Arguments

- x:

  dvec

- ...:

  un-named arguments ignored. Named arguments passed to
  [`classified.default`](https://bergsmat.github.io/yamlet/reference/classified.default.md)
  to modify behavior

- exclude:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- ordered:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- nmax:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- token:

  informative label for messages

## Value

classified

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
[`classified.factor()`](https://bergsmat.github.io/yamlet/reference/classified.factor.md),
[`codify.default()`](https://bergsmat.github.io/yamlet/reference/codify.default.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md),
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

Other dvec:
[`[.dvec()`](https://bergsmat.github.io/yamlet/reference/sub-.dvec.md),
`[<-.dvec()`,
[`[[.dvec()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.dvec.md),
`[[<-.dvec()`,
[`as.data.frame.dvec()`](https://bergsmat.github.io/yamlet/reference/as.data.frame.dvec.md),
[`as_dvec()`](https://bergsmat.github.io/yamlet/reference/as_dvec.md),
[`as_dvec.character()`](https://bergsmat.github.io/yamlet/reference/as_dvec.character.md),
[`as_dvec.complex()`](https://bergsmat.github.io/yamlet/reference/as_dvec.complex.md),
[`as_dvec.dvec()`](https://bergsmat.github.io/yamlet/reference/as_dvec.dvec.md),
[`as_dvec.integer()`](https://bergsmat.github.io/yamlet/reference/as_dvec.integer.md),
[`as_dvec.logical()`](https://bergsmat.github.io/yamlet/reference/as_dvec.logical.md),
[`as_dvec.numeric()`](https://bergsmat.github.io/yamlet/reference/as_dvec.numeric.md),
[`c.dvec()`](https://bergsmat.github.io/yamlet/reference/c.dvec.md),
[`desolve.dvec()`](https://bergsmat.github.io/yamlet/reference/desolve.dvec.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`format.dvec()`](https://bergsmat.github.io/yamlet/reference/format.dvec.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md),
[`length.dvec()`](https://bergsmat.github.io/yamlet/reference/length.dvec.md),
[`print.dvec()`](https://bergsmat.github.io/yamlet/reference/print.dvec.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md),
[`unclassified.dvec()`](https://bergsmat.github.io/yamlet/reference/unclassified.dvec.md)

## Examples

``` r
library(magrittr)
x <- as_dvec(1:3)
attr(x, 'guide') <- list(a = 1, b = 2, c = 3)
x %>% str
#>  'dvec' int [1:3] 1 2 3
#>  - attr(*, "guide")=List of 3
#>   ..$ a: num 1
#>   ..$ b: num 2
#>   ..$ c: num 3
x %>% classified %>% str
#>  Factor w/ 3 levels "1","2","3": 1 2 3
#>  - attr(*, "guide")=List of 3
#>   ..$ a: num 1
#>   ..$ b: num 2
#>   ..$ c: num 3
#>  - attr(*, "codelist")=List of 3
#>   ..$ : chr "1"
#>   ..$ : chr "2"
#>   ..$ : chr "3"
x %>% explicit_guide %>% classified %>% str
#>  Factor w/ 3 levels "a","b","c": 1 2 3
#>  - attr(*, "codelist")=List of 3
#>   ..$ 1: chr "a"
#>   ..$ 2: chr "b"
#>   ..$ 3: chr "c"
```
