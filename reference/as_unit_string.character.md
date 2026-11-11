# Coerce Character to Unit String

Coerces character to class 'unit_string'. See description for
[`as_unit_string`](https://bergsmat.github.io/yamlet/reference/as_unit_string.md).

## Usage

``` r
# S3 method for class 'character'
as_unit_string(x, ...)
```

## Arguments

- x:

  character

- ...:

  ignored arguments

## Value

unit_string

## See also

Other unit_string:
[`[.unit_string()`](https://bergsmat.github.io/yamlet/reference/sub-.unit_string.md),
[`[[.unit_string()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.unit_string.md),
[`as_unit_string()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.md),
[`as_unit_string.factor()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.factor.md),
[`as_unit_string.symbolic_units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.symbolic_units.md),
[`as_unit_string.units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.units.md)

## Examples

``` r
as_unit_string('kg m2 s-2')
#> [1] "kg m2 s-2"
#> attr(,"class")
#> [1] "unit_string" "character"  
```
