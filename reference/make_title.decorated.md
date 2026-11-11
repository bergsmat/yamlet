# Make Title for Decorated

Make title for class 'decorated'. Limits scope to requested variables,
and then calls class-specific methods for each.

## Usage

``` r
# S3 method for class 'decorated'
make_title(x, ...)
```

## Arguments

- x:

  object

- ...:

  optional names of variables to limit scope

## Value

decorated

## See also

Other labels:
[`alias.data.frame()`](https://bergsmat.github.io/yamlet/reference/alias.data.frame.md),
[`append_units()`](https://bergsmat.github.io/yamlet/reference/append_units.md),
[`append_units.data.frame()`](https://bergsmat.github.io/yamlet/reference/append_units.data.frame.md),
[`append_units.default()`](https://bergsmat.github.io/yamlet/reference/append_units.default.md),
[`drop_title()`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
[`drop_title.decorated()`](https://bergsmat.github.io/yamlet/reference/drop_title.decorated.md),
[`drop_title.default()`](https://bergsmat.github.io/yamlet/reference/drop_title.default.md),
[`drop_title.dvec()`](https://bergsmat.github.io/yamlet/reference/drop_title.dvec.md),
[`make_title()`](https://bergsmat.github.io/yamlet/reference/make_title.md),
[`make_title.default()`](https://bergsmat.github.io/yamlet/reference/make_title.default.md),
[`make_title.dvec()`](https://bergsmat.github.io/yamlet/reference/make_title.dvec.md),
[`sub_units()`](https://bergsmat.github.io/yamlet/reference/sub_units.md)

## Examples

``` r
library(magrittr)
x <- data.frame(length = 1:10)
x %>% 
  decorate('length: [ Length, mm ]') %>%
  resolve %>%
  decorations
#> - length
#>  - label: Length
#>  - units: mm
#>  - title: Length (mm)
```
