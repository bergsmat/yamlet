# Coerce Factor to Unit String

Coerces factor to class 'unit_string' by converting to character and
calling
[`as_unit_string`](https://bergsmat.github.io/yamlet/reference/as_unit_string.md).

## Usage

``` r
# S3 method for class 'factor'
as_unit_string(x, ...)
```

## Arguments

- x:

  factor

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
[`as_unit_string.symbolic_units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.symbolic_units.md),
[`as_unit_string.units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.units.md)

## Examples

``` r
as_unit_string(as.factor('kg m2 s-2'))
#> [1] "kg m2 s-2"
#> attr(,"class")
#> [1] "unit_string" "character"  
```
