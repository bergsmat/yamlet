# Coerce Yamlet Guide to Something More Explicit

Coerces yamlet 'guide' keys to something more explicit. The key 'guide'
generally suggests a guide to interpretation of a data item, such as
units, formats, codelists, and encodings. The idea here is to replace
'guide' with something explicit in case required downstream.

## Usage

``` r
# S3 method for class 'yamlet'
explicit_guide(
  x,
  ...,
  test = getOption("yamlet_infer_guide", yamlet::infer_guide),
  expand = getOption("yamlet_expand_codelist", TRUE),
  data = NULL
)
```

## Arguments

- x:

  yamlet

- ...:

  passed to
  [`select`](https://dplyr.tidyverse.org/reference/select.html) to limit
  scope

- test:

  function or function name; supply non-default or globally set
  `options(yamlet_infer_guide = )`.

- expand:

  whether to expand empty guide list using sorted unique values. NA
  likely excluded.

- data:

  optional data.frame for testing guides with length \> 1

## Value

yamlet

## Details

If `data` is supplied, guides that are lists are checked to see if they
evaluate to conditions in data context (see
[`isConditional.list`](https://bergsmat.github.io/yamlet/reference/isConditional.list.md)).
If so, inferences are based on the first guide element rather than the
guide as a whole.

This method iterates across the guide elements, renaming them as
specified by the value of `test`. (default:
[`infer_guide`](https://bergsmat.github.io/yamlet/reference/infer_guide.md)).
`test` should be a function (or name of one) that accepts x, data, and
dots. If a data.frame is passed to explicit_guide(), the relevant column
will be passed as data to `test`.

## See also

Other explicit_guide:
[`explicit_guide()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`explicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md),
[`explicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.factor.md),
[`implicit_guide()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md),
[`implicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.data.frame.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md),
[`infer_guide()`](https://bergsmat.github.io/yamlet/reference/infer_guide.md)

## Examples

``` r
library(magrittr)
'CONC: [ concentration, µg/mL ]' %>% as_yamlet %>% explicit_guide
#> - CONC
#>  - label: concentration
#>  - units: µg/mL
'RACE: [ subject race, [ Caucasian: 0, Latin: 1, Black: 2 ]]' %>% as_yamlet %>% explicit_guide
#> - RACE
#>  - label: subject race
#>  - codelist
#>   - Caucasian: 0
#>   - Latin: 1
#>   - Black: 2
'RACE: [ subject race, [ Caucasian, Latin, Black ]]' %>% as_yamlet %>% explicit_guide
#> - RACE
#>  - label: subject race
#>  - codelist: Caucasian, Latin, Black
'RACE: [ subject race, //0/Caucasian//1/Latin//2/Black// ]' %>% as_yamlet %>% explicit_guide
#> - RACE
#>  - label: subject race
#>  - encoding: //0/Caucasian//1/Latin//2/Black//
'DATE: [ date, "%Y-%m-%d" ]' %>% as_yamlet %>% explicit_guide
#> - DATE
#>  - label: date
#>  - format: '%Y-%m-%d'
'PRSE: [ standard error, "%" ]' %>% as_yamlet %>% explicit_guide
#> - PRSE
#>  - label: standard error
#>  - units: '%'
```
