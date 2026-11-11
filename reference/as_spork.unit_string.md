# Coerce Unit String to Spork

Coerces unit string to spork. A literal dot means different things in
spork vs. units, and there may be some other subtleties as well. Unit
string is character that
[`is_parseable`](https://bergsmat.github.io/yamlet/reference/is_parseable.md).

## Usage

``` r
# S3 method for class 'unit_string'
as_spork(x, ...)
```

## Arguments

- x:

  unit_string

- ...:

  ignored arguments

## Value

units

## See also

Other spork:
[`as_spork.symbolic_units()`](https://bergsmat.github.io/yamlet/reference/as_spork.symbolic_units.md),
[`as_spork.units()`](https://bergsmat.github.io/yamlet/reference/as_spork.units.md)

## Examples

``` r
library(magrittr)
library(spork)
'kg.m^2/s^2' %>% as_unit_string %>% as_spork
#> [1] "kg*m^2./s^2."
#> attr(,"class")
#> [1] "spork"     "character"
'kg.m2 s-2' %>% as_unit_string %>% as_spork
#> [1] "kg*m^2. s^-2."
#> attr(,"class")
#> [1] "spork"     "character"
```
