# Append Units for Data Frame

Appends units for data.frame. For finer control, consider applying
[`append_units.default`](https://bergsmat.github.io/yamlet/reference/append_units.default.md)
to individual columns.

## Usage

``` r
# S3 method for class 'data.frame'
append_units(x, ...)
```

## Arguments

- x:

  data.frame

- ...:

  named arguments passed to default method, un-named are columns to
  alter scope

## Value

data.frame

## See also

Other labels:
[`alias.data.frame()`](https://bergsmat.github.io/yamlet/reference/alias.data.frame.md),
[`append_units()`](https://bergsmat.github.io/yamlet/reference/append_units.md),
[`append_units.default()`](https://bergsmat.github.io/yamlet/reference/append_units.default.md),
[`drop_title()`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
[`drop_title.decorated()`](https://bergsmat.github.io/yamlet/reference/drop_title.decorated.md),
[`drop_title.default()`](https://bergsmat.github.io/yamlet/reference/drop_title.default.md),
[`drop_title.dvec()`](https://bergsmat.github.io/yamlet/reference/drop_title.dvec.md),
[`make_title()`](https://bergsmat.github.io/yamlet/reference/make_title.md),
[`make_title.decorated()`](https://bergsmat.github.io/yamlet/reference/make_title.decorated.md),
[`make_title.default()`](https://bergsmat.github.io/yamlet/reference/make_title.default.md),
[`make_title.dvec()`](https://bergsmat.github.io/yamlet/reference/make_title.dvec.md),
[`sub_units()`](https://bergsmat.github.io/yamlet/reference/sub_units.md)

## Examples

``` r
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
file %>% decorate %>% explicit_guide %>% append_units %>% decorations(Age, glyco)
#> - Age
#>  - label: baseline subject age (year)
#>  - units: year
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration (mg/dL)
#>  - units: mg/dL
file %>% decorate %>% explicit_guide %>% append_units(glyco) %>% decorations(Age, glyco)
#> - Age
#>  - label: baseline subject age
#>  - units: year
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration (mg/dL)
#>  - units: mg/dL
```
