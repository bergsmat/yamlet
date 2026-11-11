# Coerce Factor Guide to Something More Implicit

Coerces factor guide-like attributes to 'guide'. The attribute 'guide'
generally suggests a guide to interpretation of a data item, such as
units, formats, codelists, and encodings. The idea here is to replace
these with 'guide': i.e., to undo the effects of
[`explicit_guide.dvec`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md).
If guide attribute is still present, the explicit attribute is removed.
Otherwise the explicit element is renamed.

## Usage

``` r
# S3 method for class 'factor'
implicit_guide(x, ...)
```

## Arguments

- x:

  factor

- ...:

  ignored

## Value

factor

## See also

Other explicit_guide:
[`explicit_guide()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`explicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md),
[`explicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.factor.md),
[`explicit_guide.yamlet()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md),
[`implicit_guide()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md),
[`implicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.data.frame.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`infer_guide()`](https://bergsmat.github.io/yamlet/reference/infer_guide.md)

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
[`length.dvec()`](https://bergsmat.github.io/yamlet/reference/length.dvec.md),
[`print.dvec()`](https://bergsmat.github.io/yamlet/reference/print.dvec.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md),
[`unclassified.dvec()`](https://bergsmat.github.io/yamlet/reference/unclassified.dvec.md)

## Examples

``` r
library(magrittr)
x <- data.frame(RACE = factor(1))
x %<>% modify(RACE, label = 'race', guide = list(white = 0, black = 1, asian = 2))
x %>% decorations
#> - RACE
#>  - label: race
#>  - guide
#>   - white: 0
#>   - black: 1
#>   - asian: 2
x %>% explicit_guide %>% decorations
#> - RACE
#>  - label: race
#>  - codelist
#>   - white: 0
#>   - black: 1
#>   - asian: 2
x %>% explicit_guide %>% implicit_guide %>% decorations
#> - RACE
#>  - label: race
#>  - guide
#>   - white: 0
#>   - black: 1
#>   - asian: 2
x %>% explicit_guide(simplify = FALSE) %>% decorations
#> - RACE
#>  - label: race
#>  - guide
#>   - white: 0
#>   - black: 1
#>   - asian: 2
#>  - codelist
#>   - white: 0
#>   - black: 1
#>   - asian: 2
x %>% explicit_guide(simplify = FALSE) %>% implicit_guide %>% decorations
#> - RACE
#>  - label: race
#>  - guide
#>   - white: 0
#>   - black: 1
#>   - asian: 2
x %<>% explicit_guide
a <- x$RACE
str(a)
#>  Factor w/ 1 level "1": 1
#>  - attr(*, "label")= chr "race"
#>  - attr(*, "codelist")=List of 3
#>   ..$ white: num 0
#>   ..$ black: num 1
#>   ..$ asian: num 2
str(a %>% implicit_guide)
#>  Factor w/ 1 level "1": 1
#>  - attr(*, "label")= chr "race"
#>  - attr(*, "guide")=List of 3
#>   ..$ white: num 0
#>   ..$ black: num 1
#>   ..$ asian: num 2
```
