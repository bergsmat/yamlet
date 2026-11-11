# Coerce Data Frame Guide to Something More Explicit

Coerces data.frame 'guide' attributes to something more explicit. The
attribute 'guide' generally suggests a guide to interpretation of a data
item, such as units, formats, codelists, and encodings. The idea here is
to replace 'guide' with something explicit in case required downstream.

## Usage

``` r
# S3 method for class 'data.frame'
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

  data.frame

- ...:

  named arguments passed to
  [`as_yamlet`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
  [`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
  and
  [`decorate`](https://bergsmat.github.io/yamlet/reference/decorate.md);
  un-named arguments limit scope

- overwrite:

  passed as TRUE

- simplify:

  whether to remove guide attribute

- expand:

  whether to expand empty guide list using sorted unique values. NA
  likely excluded.

## Value

data.frame

## Details

This method pulls the 'decorations' off of the data.frame, converts to
yamlet, applies
[`explicit_guide.yamlet`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md),
purges 'guide' attributes from the data.frame, and then re-decorates
using `overwrite = TRUE`.

## See also

Other explicit_guide:
[`explicit_guide()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`explicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md),
[`explicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.factor.md),
[`explicit_guide.yamlet()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md),
[`implicit_guide()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md),
[`implicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.data.frame.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md),
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
x <- data.frame(
 ID = 1,
 CONC = 1,
 RACE = 1,
 SEX = 1,
 DATE = 1
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
```
