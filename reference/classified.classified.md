# Create Classified from Classified

See
[`classified.default`](https://bergsmat.github.io/yamlet/reference/classified.default.md).
Formerly (version 0.10.10), calling classified() on a classified object
was a non-operation. Currently we call factor(x, ...) and then try to
reconcile the codelist attribute with resulting levels.

## Usage

``` r
# S3 method for class 'classified'
classified(
  x,
  levels,
  labels,
  exclude = NULL,
  ordered = is.ordered(x),
  nmax = NA,
  drop = FALSE,
  ...
)
```

## Arguments

- x:

  classified

- levels:

  passed to [`factor`](https://rdrr.io/r/base/factor.html); defaults to
  `levels(x)`

- labels:

  passed to [`factor`](https://rdrr.io/r/base/factor.html); must be same
  length as levels(after removing values in `exclude` and unused levels
  if `drop` is TRUE) and must not contain duplicates

- exclude:

  passed to [`factor`](https://rdrr.io/r/base/factor.html)

- ordered:

  passed to [`factor`](https://rdrr.io/r/base/factor.html)

- nmax:

  passed to [`factor`](https://rdrr.io/r/base/factor.html)

- drop:

  whether to drop unused levels

- ...:

  ignored

## Value

'classified' 'factor'

## Details

By default classified is idempotent, such that classified(classified(x))
is the same as classified(x). In contrast, factor(factor(x)) will drop
unused levels (not shown). To drop unused levels, use
classified(classified(x), drop = TRUE).

## See also

Other classified:
[`[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-.classified.md),
`[<-.classified()`,
[`[[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.classified.md),
`[[<-.classified()`,
[`as.integer.classified()`](https://bergsmat.github.io/yamlet/reference/as.integer.classified.md),
[`c.classified()`](https://bergsmat.github.io/yamlet/reference/c.classified.md),
[`classified()`](https://bergsmat.github.io/yamlet/reference/classified.md),
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

a <- 4:6
attr(a, 'codelist') <- list(d = 4, e = 5, f = 6, g = 7)
b <- classified(a)
a
#> [1] 4 5 6
#> attr(,"codelist")
#> attr(,"codelist")$d
#> [1] 4
#> 
#> attr(,"codelist")$e
#> [1] 5
#> 
#> attr(,"codelist")$f
#> [1] 6
#> 
#> attr(,"codelist")$g
#> [1] 7
#> 
b
#> [1] d e f
#> attr(,"codelist")
#> attr(,"codelist")$`4`
#> [1] d
#> 
#> attr(,"codelist")$`5`
#> [1] e
#> 
#> attr(,"codelist")$`6`
#> [1] f
#> 
#> attr(,"codelist")$`7`
#> [1] g
#> 
#> Levels: d e f g
class(b)
#> [1] "classified" "factor"    
classified(b)
#> [1] d e f
#> attr(,"codelist")
#> attr(,"codelist")$`4`
#> [1] d
#> 
#> attr(,"codelist")$`5`
#> [1] e
#> 
#> attr(,"codelist")$`6`
#> [1] f
#> 
#> attr(,"codelist")$`7`
#> [1] g
#> 
#> Levels: d e f g
identical(b, classified(b))
#> [1] TRUE
```
