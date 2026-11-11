# Resolve Guide for Decorated Vector

Resolves implicit usage of default key 'guide' to explicit usage for
class dvec. Simply calls
[`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md)
followed by
[`classified`](https://bergsmat.github.io/yamlet/reference/classified.md)
if x has a codelist attribute. If option `yamlet_with_title` is not
NULL, and if 'units' present, label and units will be concatenated by
default to create a title attribute.

## Usage

``` r
# S3 method for class 'dvec'
resolve(x, ...)
```

## Arguments

- x:

  dvec

- ...:

  passed to
  [`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
  [`classified`](https://bergsmat.github.io/yamlet/reference/classified.md),
  and
  [`make_title`](https://bergsmat.github.io/yamlet/reference/make_title.md)

## Value

dvec or classified

## See also

Other resolve:
[`desolve()`](https://bergsmat.github.io/yamlet/reference/desolve.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`desolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/desolve.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`desolve.default()`](https://bergsmat.github.io/yamlet/reference/desolve.default.md),
[`desolve.dvec()`](https://bergsmat.github.io/yamlet/reference/desolve.dvec.md),
[`enscript.data.frame()`](https://bergsmat.github.io/yamlet/reference/enscript.data.frame.md),
[`enscript.default()`](https://bergsmat.github.io/yamlet/reference/enscript.default.md),
[`resolve()`](https://bergsmat.github.io/yamlet/reference/resolve.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/resolve.data.frame.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`resolve.default()`](https://bergsmat.github.io/yamlet/reference/resolve.default.md),
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
[`desolve.dvec()`](https://bergsmat.github.io/yamlet/reference/desolve.dvec.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`format.dvec()`](https://bergsmat.github.io/yamlet/reference/format.dvec.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md),
[`length.dvec()`](https://bergsmat.github.io/yamlet/reference/length.dvec.md),
[`print.dvec()`](https://bergsmat.github.io/yamlet/reference/print.dvec.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md),
[`unclassified.dvec()`](https://bergsmat.github.io/yamlet/reference/unclassified.dvec.md)

## Examples

``` r
library(magrittr)
x <- as_dvec(1:3, guide = list(a = 1, b = 2, c = 3))
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
x %>% resolve %>% str
#>  Factor w/ 3 levels "a","b","c": 1 2 3
#>  - attr(*, "codelist")=List of 3
#>   ..$ 1: chr "a"
#>   ..$ 2: chr "b"
#>   ..$ 3: chr "c"
```
