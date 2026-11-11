# Coerce Units to Unit String.

Coerces units to unit_string. Extracts units attribute (of
class(symbolic_units)) and converts.

## Usage

``` r
# S3 method for class 'units'
as_unit_string(x, canonical = TRUE, ...)
```

## Arguments

- x:

  units; see
  [`as_units`](https://r-quantities.github.io/units/reference/units.html)

- ...:

  ignored arguments

## Value

unit_string

## See also

Other unit_string:
[`[.unit_string()`](https://bergsmat.github.io/yamlet/reference/sub-.unit_string.md),
[`[[.unit_string()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.unit_string.md),
[`as_unit_string()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.md),
[`as_unit_string.character()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.character.md),
[`as_unit_string.factor()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.factor.md),
[`as_unit_string.symbolic_units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.symbolic_units.md)

## Examples

``` r
library(units)
x <- as_units('kg.m/s^2')
as_unit_string(x)
#> [1] "kg*m/s^2"
#> attr(,"class")
#> [1] "unit_string" "character"  
as_unit_string(x, canonical = FALSE)
#> [1] "kg m s-2"
#> attr(,"class")
#> [1] "unit_string" "character"  
```
