# Collapse Uninformative Levels by Default

The default unnest() method returns the unmodified object.

## Usage

``` r
# Default S3 method
unnest(x, ...)
```

## Arguments

- x:

  object

## Value

list

## See also

Other unnest:
[`unnest()`](https://bergsmat.github.io/yamlet/reference/unnest.md),
[`unnest.list()`](https://bergsmat.github.io/yamlet/reference/unnest.list.md)

## Examples

``` r
unnest(yaml::yaml.load('ITEM:'))
#> Warning: `cols` is now required when using `unnest()`.
#> ℹ Please use `cols = c()`.
#> Error in UseMethod("unnest"): no applicable method for 'unnest' applied to an object of class "list"
```
