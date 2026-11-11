# Coerce Decorated Vector Guide to Something More Explicit

Coerces dvec 'guide' attribute to something more explicit. The attribute
'guide' generally suggests a guide to interpretation of a data item,
such as units, formats, codelists, and encodings. The idea here is to
replace 'guide' with something explicit in case required downstream.

## Usage

``` r
# S3 method for class 'dvec'
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

  dvec

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

dvec

## See also

Other explicit_guide:
[`explicit_guide()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`explicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.factor.md),
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
 CONC = as_dvec(1),
 RACE = as_dvec(1),
 SEX = as_dvec(1),
 DATE = as_dvec(1)
)
x %<>% modify(ID, label = 'subject identifier')
x %<>% modify(CONC, label = 'concentration', guide = 'ng/mL')
x %<>% modify(RACE, label = 'race', guide = list(white = 0, black = 1, asian = 2))
x %<>% modify(SEX, label = 'sex', guide = list(female = 0, male = 1))
x %<>% modify(DATE, label  = 'date', guide = '%Y-%m-%d')
x %>% decorations
#> - ID
#>  - label: subject identifier
#> - CONC
#>  - label: concentration
#>  - guide: ng/mL
#> - RACE
#>  - label: race
#>  - guide
#>   - white: 0
#>   - black: 1
#>   - asian: 2
#> - SEX
#>  - label: sex
#>  - guide
#>   - female: 0
#>   - male: 1
#> - DATE
#>  - label: date
#>  - guide: '%Y-%m-%d'
x %>% explicit_guide %>% decorations
#> - ID
#>  - label: subject identifier
#> - CONC
#>  - label: concentration
#>  - units: ng/mL
#> - RACE
#>  - label: race
#>  - codelist
#>   - white: 0
#>   - black: 1
#>   - asian: 2
#> - SEX
#>  - label: sex
#>  - codelist
#>   - female: 0
#>   - male: 1
#> - DATE
#>  - label: date
#>  - format: '%Y-%m-%d'
x %>% explicit_guide(DATE) %>% decorations # limit scope
#> - ID
#>  - label: subject identifier
#> - CONC
#>  - label: concentration
#>  - guide: ng/mL
#> - RACE
#>  - label: race
#>  - guide
#>   - white: 0
#>   - black: 1
#>   - asian: 2
#> - SEX
#>  - label: sex
#>  - guide
#>   - female: 0
#>   - male: 1
#> - DATE
#>  - label: date
#>  - format: '%Y-%m-%d'
x %$% DATE %>% explicit_guide
#> [1] 1
#> attr(,"label")
#> [1] "date"
#> attr(,"format")
#> [1] "%Y-%m-%d"
```
