# Classify Data Frame

Coerces items in data.frame with codelist attribute to 'classified': a
factor with a codelist attribute.

## Usage

``` r
# S3 method for class 'data.frame'
classified(x, ..., exclude = NA, ordered = is.ordered(x), nmax = NA)
```

## Arguments

- x:

  data.frame

- ...:

  passed to
  [`select`](https://dplyr.tidyverse.org/reference/select.html) to limit
  column scope

- exclude:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- ordered:

  see [`factor`](https://rdrr.io/r/base/factor.html)

- nmax:

  see [`factor`](https://rdrr.io/r/base/factor.html)

## Value

data.frame

## See also

Other classified:
[`[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-.classified.md),
`[<-.classified()`,
[`[[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.classified.md),
`[[<-.classified()`,
[`as.integer.classified()`](https://bergsmat.github.io/yamlet/reference/as.integer.classified.md),
[`c.classified()`](https://bergsmat.github.io/yamlet/reference/c.classified.md),
[`classified()`](https://bergsmat.github.io/yamlet/reference/classified.md),
[`classified.classified()`](https://bergsmat.github.io/yamlet/reference/classified.classified.md),
[`classified.default()`](https://bergsmat.github.io/yamlet/reference/classified.default.md),
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
[`classified.factor()`](https://bergsmat.github.io/yamlet/reference/classified.factor.md),
[`codify.default()`](https://bergsmat.github.io/yamlet/reference/codify.default.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md),
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

Other interface:
[`canonical.decorated()`](https://bergsmat.github.io/yamlet/reference/canonical.decorated.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`enscript.decorated()`](https://bergsmat.github.io/yamlet/reference/enscript.decorated.md),
[`ggplot.decorated()`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
[`io_table.character()`](https://bergsmat.github.io/yamlet/reference/io_table.character.md),
[`io_table.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_table.data.frame.md),
[`io_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.character.md),
[`io_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.data.frame.md),
[`is_parseable.default()`](https://bergsmat.github.io/yamlet/reference/is_parseable.default.md),
[`mimic.default()`](https://bergsmat.github.io/yamlet/reference/mimic.default.md),
[`modify.default()`](https://bergsmat.github.io/yamlet/reference/modify.default.md),
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md),
[`read_yamlet()`](https://bergsmat.github.io/yamlet/reference/read_yamlet.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md),
[`write_yamlet()`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)

## Examples

``` r
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)
x %>% explicit_guide %>% decorations(Age, Race, Heart:glyco)
#> - Age
#>  - label: baseline subject age
#>  - units: year
#> - Race
#>  - label: subject race
#>  - codelist: Caucasian, Latin, Black
#> - Heart
#>  - label: congestive heart failure
#>  - codelist: No/Mild, Moderate, Severe
#> - Creatinine
#>  - label: creatinine clearance
#>  - codelist
#>   - < 50 mg/min: < 50
#>   - '>= 50 mg/min': '>= 50'
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - units: mg/dL
x %>% explicit_guide %>% classified %>% decorations(Age, Race, Heart:glyco)
#> - Age
#>  - label: baseline subject age
#>  - units: year
#> - Race
#>  - label: subject race
#>  - codelist: Caucasian, Latin, Black
#> - Heart
#>  - label: congestive heart failure
#>  - codelist: No/Mild, Moderate, Severe
#> - Creatinine
#>  - label: creatinine clearance
#>  - codelist
#>   - < 50: < 50 mg/min
#>   - '>= 50': '>= 50 mg/min'
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - units: mg/dL
x %>% explicit_guide %>% classified(Heart:glyco) %>% decorations(Age, Race, Heart:glyco)
#> - Age
#>  - label: baseline subject age
#>  - units: year
#> - Race
#>  - label: subject race
#>  - codelist: Caucasian, Latin, Black
#> - Heart
#>  - label: congestive heart failure
#>  - codelist: No/Mild, Moderate, Severe
#> - Creatinine
#>  - label: creatinine clearance
#>  - codelist
#>   - < 50: < 50 mg/min
#>   - '>= 50': '>= 50 mg/min'
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - units: mg/dL
```
