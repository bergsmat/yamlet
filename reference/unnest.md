# Collapse Uninformative Levels

Each element of a list that is itself a list and does not have a name
but has exactly one element that DOES have a name should become that
element and have that name (recursively, from depth). Collapses
uninformative levels of nested lists. Formerly used in as_yam; now
superseded by
[`parsimonious.list`](https://bergsmat.github.io/yamlet/reference/parsimonious.list.md).

## Usage

``` r
unnest(x, ...)
```

## Arguments

- x:

  object

## Value

named list

## See also

Other unnest:
[`unnest.default()`](https://bergsmat.github.io/yamlet/reference/unnest.default.md),
[`unnest.list()`](https://bergsmat.github.io/yamlet/reference/unnest.list.md)

## Examples

``` r

# yaml.load reads this as a list of two un-named lists whose elements are named.
str(yaml::yaml.load('[foo: 1, bar: 3]'))
#> List of 2
#>  $ :List of 1
#>   ..$ foo: int 1
#>  $ :List of 1
#>   ..$ bar: int 3

# yamlet treats it as a list of two named integers.
str(unnest(yaml::yaml.load('[foo: 1, bar: 3]')))
#> Warning: `cols` is now required when using `unnest()`.
#> ℹ Please use `cols = c()`.
#> Error in UseMethod("unnest"): no applicable method for 'unnest' applied to an object of class "list"
```
