# Desolve Guide for Classified

Un-resolves explicit usage of default key 'guide' to implicit usage for
class 'classified'. Calls
[`drop_title`](https://bergsmat.github.io/yamlet/reference/drop_title.md)
(a non-action by default),
[`unclassified`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
followed by
[`implicit_guide`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md).

## Usage

``` r
# S3 method for class 'classified'
desolve(x, ...)
```

## Arguments

- x:

  classified

- ...:

  passed to
  [`drop_title`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
  [`unclassified`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
  and
  [`unclassified`](https://bergsmat.github.io/yamlet/reference/unclassified.md)

## Value

dvec

## See also

Other resolve:
[`desolve()`](https://bergsmat.github.io/yamlet/reference/desolve.md),
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
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md)

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
[`codify.default()`](https://bergsmat.github.io/yamlet/reference/codify.default.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md),
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

## Examples

``` r
library(magrittr)
x <- as_dvec(
  4:6, 
  guide = list(a = 4L, b = 5L, c = 6L)
)

# untouched
x %>% str
#>  'dvec' int [1:3] 4 5 6
#>  - attr(*, "guide")=List of 3
#>   ..$ a: int 4
#>   ..$ b: int 5
#>   ..$ c: int 6

# resolved
x %>% resolve %>% str
#>  Factor w/ 3 levels "a","b","c": 1 2 3
#>  - attr(*, "codelist")=List of 3
#>   ..$ 4: chr "a"
#>   ..$ 5: chr "b"
#>   ..$ 6: chr "c"

# resolved and desolved
x %>% resolve %>% desolve %>% str
#>  'dvec' int [1:3] 4 5 6
#>  - attr(*, "guide")=List of 3
#>   ..$ a: int 4
#>   ..$ b: int 5
#>   ..$ c: int 6
```
