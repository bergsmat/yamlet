# Resolve Guide for Factor

Resolves implicit usage of default key 'guide' to explicit usage for
factor. Calls
[`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md)
followed by
[`classified`](https://bergsmat.github.io/yamlet/reference/classified.md).
If option `yamlet_with_title` is not NULL, and if 'units' present, label
and units will be concatenated by default to create a title attribute
(but this is generally unexpected).

## Usage

``` r
# S3 method for class 'factor'
resolve(x, ...)
```

## Arguments

- x:

  factor

- ...:

  passed to
  [`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
  [`classified`](https://bergsmat.github.io/yamlet/reference/classified.md),
  and
  [`make_title`](https://bergsmat.github.io/yamlet/reference/make_title.md)

## Value

classified

## Details

If decorating a factor with a guide, it is recommended to supply one
that resolves to a codelist. An even safer practice is to convert the
factor first to character, so that there are no factor levels to
disagree with the (somewhat equivalent) codelist.

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
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md)

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
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`unclassified.dvec()`](https://bergsmat.github.io/yamlet/reference/unclassified.dvec.md)

## Examples

``` r
library(magrittr)
x <- factor(1:3)
attr(x, 'guide') <- list(a = 1, b = 2, c = 3)
x
#> [1] 1 2 3
#> attr(,"guide")
#> attr(,"guide")$a
#> [1] 1
#> 
#> attr(,"guide")$b
#> [1] 2
#> 
#> attr(,"guide")$c
#> [1] 3
#> 
#> Levels: 1 2 3
resolve(x)
#> [1] a b c
#> attr(,"codelist")
#> attr(,"codelist")$`1`
#> [1] a
#> 
#> attr(,"codelist")$`2`
#> [1] b
#> 
#> attr(,"codelist")$`3`
#> [1] c
#> 
#> Levels: a b c
x <- data.frame(bar = x)
x
#>   bar
#> 1   1
#> 2   2
#> 3   3
resolve(x)
#>   bar
#> 1   a
#> 2   b
#> 3   c
```
