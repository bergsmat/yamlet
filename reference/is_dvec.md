# Test if Class is dvec

Tests whether x inherits 'dvec'.

## Usage

``` r
is_dvec(x)
```

## Arguments

- x:

  object

## Value

logical

## Examples

``` r
is_dvec(1L)
#> [1] FALSE
is_dvec(as_dvec(1L))
#> [1] TRUE
```
