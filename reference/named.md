# Evaluate Named Arguments

Evaluates named arguments in `...`.

## Usage

``` r
named(...)
```

## Arguments

- ...:

  possibly a mix of named and un-named arguments.

## Value

named list

## See also

Other modify:
[`modify()`](https://bergsmat.github.io/yamlet/reference/modify.md),
[`modify.default()`](https://bergsmat.github.io/yamlet/reference/modify.default.md),
[`selected()`](https://bergsmat.github.io/yamlet/reference/selected.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md)

## Examples

``` r
named(a = 1, b = 2 + 3, 4, 'd')
#> $a
#> [1] 1
#> 
#> $b
#> [1] 5
#> 
```
