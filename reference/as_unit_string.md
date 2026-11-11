# Coerce to Unit String

Coerces to class 'unit_string'. Generic, with method
[`as_unit_string.character`](https://bergsmat.github.io/yamlet/reference/as_unit_string.character.md).
A unit string is character text suitable as input for
[`as_units`](https://r-quantities.github.io/units/reference/units.html).
See also
[`is_parseable`](https://bergsmat.github.io/yamlet/reference/is_parseable.md).

## Usage

``` r
as_unit_string(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

unit_string

## See also

Other unit_string:
[`[.unit_string()`](https://bergsmat.github.io/yamlet/reference/sub-.unit_string.md),
[`[[.unit_string()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.unit_string.md),
[`as_unit_string.character()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.character.md),
[`as_unit_string.factor()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.factor.md),
[`as_unit_string.symbolic_units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.symbolic_units.md),
[`as_unit_string.units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.units.md)

## Examples

``` r
as_unit_string('kg.m^2/s^2')
#> [1] "kg.m^2/s^2"
#> attr(,"class")
#> [1] "unit_string" "character"  
as_unit_string('kg.m2 s-2')
#> [1] "kg.m2 s-2"
#> attr(,"class")
#> [1] "unit_string" "character"  
```
