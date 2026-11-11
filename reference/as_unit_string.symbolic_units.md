# Coerce Symbolic Units to Unit String.

Coerces symbolic units to unit_string.

## Usage

``` r
# S3 method for class 'symbolic_units'
as_unit_string(x, canonical = TRUE, ...)
```

## Arguments

- x:

  symbolic_units; see
  [`as_units`](https://r-quantities.github.io/units/reference/units.html)

- canonical:

  whether to return the form having all positive exponents

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
[`as_unit_string.units()`](https://bergsmat.github.io/yamlet/reference/as_unit_string.units.md)

## Examples

``` r
library(units)
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
as_unit_string(y)
#> [1] "kg*m/s^2"
#> attr(,"class")
#> [1] "unit_string" "character"  
library(magrittr)
'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_unit_string
#> [1] "kg*m^2/s^2"
#> attr(,"class")
#> [1] "unit_string" "character"  
'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_unit_string
#> [1] "kg*m^2/s^2"
#> attr(,"class")
#> [1] "unit_string" "character"  
'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_unit_string(FALSE)
#> [1] "kg m2 s-2"
#> attr(,"class")
#> [1] "unit_string" "character"  
'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_unit_string(FALSE)
#> [1] "kg m2 s-2"
#> attr(,"class")
#> [1] "unit_string" "character"  
```
