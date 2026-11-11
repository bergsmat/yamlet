# Coerce Factor Guide to Something More Explicit

Coerces factor 'guide' attribute to something more explicit. The
attribute 'guide' generally suggests a guide to interpretation of a data
item, such as units, formats, codelists, and encodings. The idea here is
to replace 'guide' with something explicit in case required downstream.
Generally a codelist is expected, but not enforced.

## Usage

``` r
# S3 method for class 'factor'
explicit_guide(
  x,
  ...,
  overwrite = getOption("yamlet_explicit_guide_overwrite", TRUE),
  simplify = getOption("yamlet_explicit_guide_simplify", TRUE),
  expand = getOption("yamlet_expand_codelist", TRUE)
)
```

## Arguments

- x:

  factor

- ...:

  named arguments passed to
  [`as_yamlet`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
  [`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
  and
  [`decorate`](https://bergsmat.github.io/yamlet/reference/decorate.md);
  un-named arguments ignored

- overwrite:

  whether to overwrite attributes

- simplify:

  whether to remove guide attribute

- expand:

  whether to expand empty guide list using sorted unique values. NA
  likely excluded.

## Value

factor

## See also

Other explicit_guide:
[`explicit_guide()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`explicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md),
[`explicit_guide.yamlet()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md),
[`implicit_guide()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md),
[`implicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.data.frame.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md),
[`infer_guide()`](https://bergsmat.github.io/yamlet/reference/infer_guide.md)

## Examples

``` r
library(magrittr)
x <- data.frame(
 ID = as_dvec(1),
 SEX = factor(1)
)
x %<>% modify(SEX, label = 'sex', guide = list(female = 0, male = 1))
x %>% decorations
#> - ID: []
#> - SEX
#>  - label: sex
#>  - guide
#>   - female: 0
#>   - male: 1
x %>% explicit_guide %>% decorations
#> - ID: []
#> - SEX
#>  - label: sex
#>  - codelist
#>   - female: 0
#>   - male: 1
x %>% explicit_guide(SEX) %>% decorations # limit scope
#> - ID: []
#> - SEX
#>  - label: sex
#>  - codelist
#>   - female: 0
#>   - male: 1
x %$% SEX %>% explicit_guide
#> [1] 1
#> attr(,"label")
#> [1] sex
#> attr(,"codelist")
#> attr(,"codelist")$female
#> [1] 0
#> 
#> attr(,"codelist")$male
#> [1] 1
#> 
#> Levels: 1
```
