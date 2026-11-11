# Subset Unit String

Subsets unit_string, retaining class.

## Usage

``` r
# S3 method for class 'unit_string'
x[...]
```

## Arguments

- x:

  unit_string

- ...:

  passed to next method

## Value

unit_string

## See also

Other unit_string:
[`[[.unit_string()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.unit_string.md),
[`as_unit_string()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.md),
[`as_unit_string.character()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.character.md),
[`as_unit_string.factor()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.factor.md),
[`as_unit_string.symbolic_units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.symbolic_units.md),
[`as_unit_string.units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.units.md)

## Examples

``` r
x <- c(
  'm',
  's',
  'ng/mL'
)
x <- as_unit_string(x)
class(x)
#> [1] "unit_string" "character"  
class(x[1])
#> [1] "unit_string" "character"  
```
