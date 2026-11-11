# Desolve Guide for Decorated Vector

Un-resolves explicit usage of default key 'guide' to implicit usage for
class dvec. Calls
[`drop_title`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
[`unclassified`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
and
[`implicit_guide`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md).

## Usage

``` r
# S3 method for class 'dvec'
desolve(x, ...)
```

## Arguments

- x:

- ...:

  passed to
  [`drop_title`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
  [`unclassified`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
  and
  [`implicit_guide`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md)

## Value

dvec

## See also

Other resolve:
[`desolve()`](https://bergsmat.github.io/yamlet/reference/desolve.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`desolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/desolve.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`desolve.default()`](https://bergsmat.github.io/yamlet/reference/desolve.default.md),
[`enscript.data.frame()`](https://bergsmat.github.io/yamlet/reference/enscript.data.frame.md),
[`enscript.default()`](https://bergsmat.github.io/yamlet/reference/enscript.default.md),
[`resolve()`](https://bergsmat.github.io/yamlet/reference/resolve.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/resolve.data.frame.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`resolve.default()`](https://bergsmat.github.io/yamlet/reference/resolve.default.md),
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md)

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
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
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
x <- as_dvec(4:6)
attr(x, 'guide') <- 'kg'
x %>% str
#>  'dvec' int [1:3] 4 5 6
#>  - attr(*, "guide")= chr "kg"
x %>% resolve %>% str
#>  'dvec' int [1:3] 4 5 6
#>  - attr(*, "units")= chr "kg"
#>  - attr(*, "title")= chr " (kg)"
x %>% resolve %>% desolve %>% str
#>  'dvec' int [1:3] 4 5 6
#>  - attr(*, "guide")= chr "kg"
```
