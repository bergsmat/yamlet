# Coerce Units to Spork

Coerces units to spork by coercing first to unit_string.

## Usage

``` r
# S3 method for class 'units'
as_spork(x, canonical = TRUE, ...)
```

## Arguments

- x:

  units; see
  [`as_units`](https://r-quantities.github.io/units/reference/units.html)

- ...:

  ignored arguments

## Value

spork

## See also

Other spork:
[`as_spork.symbolic_units()`](https://bergsmat.github.io/yamlet/reference/as_spork.symbolic_units.md),
[`as_spork.unit_string()`](https://bergsmat.github.io/yamlet/reference/as_spork.unit_string.md)

## Examples

``` r
library(units)
library(magrittr)
library(spork)
'kg.m^2/s^2' %>% as_units %>% as_spork
#> [1] "kg*m^2./s^2."
#> attr(,"class")
#> [1] "spork"     "character"
'kg.m2 s-2' %>% as_units %>% as_spork
#> [1] "kg*m^2./s^2."
#> attr(,"class")
#> [1] "spork"     "character"
'kg.m^2/s^2' %>% as_units %>% as_spork(FALSE)
#> [1] "kg m^2. s^-2."
#> attr(,"class")
#> [1] "spork"     "character"
'kg.m2 s-2' %>% as_units %>% as_spork(FALSE)
#> [1] "kg m^2. s^-2."
#> attr(,"class")
#> [1] "spork"     "character"
```
