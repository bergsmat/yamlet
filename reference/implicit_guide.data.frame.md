# Coerce Data Frame Guide to Something More Implicit

Coerces data.frame guide-like attributes to 'guide'. The attribute
'guide' generally suggests a guide to interpretation of a data item,
such as units, formats, codelists, and encodings. The idea here is to
replace these with 'guide': i.e., to undo the effects of
[`explicit_guide.data.frame`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md).
If guide attribute is still present, the explicit attribute is removed.
Otherwise the explicit element is renamed.

## Usage

``` r
# S3 method for class 'data.frame'
implicit_guide(x, ..., collapse = getOption("yamlet_collapse_codelist", Inf))
```

## Arguments

- x:

  data.frame

- ...:

  named arguments ignored; un-named arguments limit scope

- collapse:

  numeric: substitute empty codelist if number of levels exceeds this.
  Set to Inf (default) to ensure levels are always stored explicitly; if
  zero, empty codelist will always be substituted if codelist elements
  are un-named and exactly match `sort(unique(x))`.

## Value

data.frame

## See also

Other explicit_guide:
[`explicit_guide()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`explicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md),
[`explicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.factor.md),
[`explicit_guide.yamlet()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md),
[`implicit_guide()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md),
[`infer_guide()`](https://bergsmat.github.io/yamlet/reference/infer_guide.md)

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
x %>% explicit_guide %>% implicit_guide %>% decorations
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
x %>% explicit_guide %>% implicit_guide(DATE) %>% decorations # limit scope
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
#>  - guide: '%Y-%m-%d'
x %>% explicit_guide(simplify = FALSE) %>% decorations
#> - ID
#>  - label: subject identifier
#> - CONC
#>  - label: concentration
#>  - guide: ng/mL
#>  - units: ng/mL
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
#> - SEX
#>  - label: sex
#>  - guide
#>   - female: 0
#>   - male: 1
#>  - codelist
#>   - female: 0
#>   - male: 1
#> - DATE
#>  - label: date
#>  - guide: '%Y-%m-%d'
#>  - format: '%Y-%m-%d'
x %>% explicit_guide(simplify = FALSE) %>% implicit_guide %>% decorations
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
```
