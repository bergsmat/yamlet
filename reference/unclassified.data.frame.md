# Unclassify Data Frame

Unclassifies data.frame. Coerces 'classified' items to original values,
rebuilding codelist attribute.

## Usage

``` r
# S3 method for class 'data.frame'
unclassified(x, ...)
```

## Arguments

- x:

  data.frame

- ...:

  passed to
  [`select`](https://dplyr.tidyverse.org/reference/select.html) to limit
  scope

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
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`classified.default()`](https://bergsmat.github.io/yamlet/reference/classified.default.md),
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
[`classified.factor()`](https://bergsmat.github.io/yamlet/reference/classified.factor.md),
[`codify.default()`](https://bergsmat.github.io/yamlet/reference/codify.default.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md)

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
x %>% explicit_guide %>% classified %>% unclassified %>% decorations(Age, Race, Heart:glyco)
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
```
