# Coerce Symbolic Units to Spork

Coerces symbolic units to spork by coercing first to unit_string.

## Usage

``` r
# S3 method for class 'symbolic_units'
as_spork(x, canonical = TRUE, ...)
```

## Arguments

- x:

  symbolic_units; see
  [`as_units`](https://r-quantities.github.io/units/reference/units.html)

- ...:

  ignored arguments

## Value

spork

## See also

Other spork:
[`as_spork.unit_string()`](https://bergsmat.github.io/yamlet/reference/as_spork.unit_string.md),
[`as_spork.units()`](https://bergsmat.github.io/yamlet/reference/as_spork.units.md)

## Examples

``` r
library(units)
library(spork)
x <- as_units('kg.m/s^2')
names(attributes(x))
#> [1] "units" "class"
y <- attr(x,'units')
class(y)
#> [1] "symbolic_units"
as.character(y)
#> [1] "kg*m/s^2"
as.character(attr(x, 'units'))
#> [1] "kg*m/s^2"
as_spork(y)
#> [1] "kg*m/s^2."
#> attr(,"class")
#> [1] "spork"     "character"
library(magrittr)
'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_spork
#> [1] "kg*m^2./s^2."
#> attr(,"class")
#> [1] "spork"     "character"
'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_spork
#> [1] "kg*m^2./s^2."
#> attr(,"class")
#> [1] "spork"     "character"
'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_spork(FALSE)
#> [1] "kg m^2. s^-2."
#> attr(,"class")
#> [1] "spork"     "character"
'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_spork(FALSE)
#> [1] "kg m^2. s^-2."
#> attr(,"class")
#> [1] "spork"     "character"
```
