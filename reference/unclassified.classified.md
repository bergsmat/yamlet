# Unclassify Classified

Unclassifies classified. Uses codelist attribute to restore original
values, preserving other attributes (and rebuilding codelist).

## Usage

``` r
# S3 method for class 'classified'
unclassified(x, ..., persistence = getOption("yamlet_persistence", TRUE))
```

## Arguments

- x:

  classified

- ...:

  ignored

- persistence:

  whether to reclass as dvec

## Value

vector

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
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

## Examples

``` r
example(unclassified.data.frame)
#> 
#> uncl..> library(magrittr)
#> 
#> uncl..> file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#> 
#> uncl..> x <- decorate(file)
#> 
#> uncl..> x %>% explicit_guide %>% decorations(Age, Race, Heart:glyco)
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
#> 
#> uncl..> x %>% explicit_guide %>% classified %>% unclassified %>% decorations(Age, Race, Heart:glyco)
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
