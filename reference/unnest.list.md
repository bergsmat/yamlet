# Collapse Uninformative Levels of a List

The list method for unnest() recursively ascends a nested list, removing
uninformative levels.

## Usage

``` r
# S3 method for class 'list'
unnest(x, ...)
```

## Arguments

- x:

  list

## Value

list

## See also

Other unnest:
[`unnest()`](https://bergsmat.github.io/yamlet/reference/unnest.md),
[`unnest.default()`](https://bergsmat.github.io/yamlet/reference/unnest.default.md)

## Examples

``` r

a <- 'ITEM: [ label: sunshine, [foo: 1, bar: 3]]'

#  yaml.load() sees label nested one-deep, and foo nested two-deep:
yaml::yaml.load(a)
#> $ITEM
#> $ITEM[[1]]
#> $ITEM[[1]]$label
#> [1] "sunshine"
#> 
#> 
#> $ITEM[[2]]
#> $ITEM[[2]][[1]]
#> $ITEM[[2]][[1]]$foo
#> [1] 1
#> 
#> 
#> $ITEM[[2]][[2]]
#> $ITEM[[2]][[2]]$bar
#> [1] 3
#> 
#> 
#> 
#> 

# unnest() sees label nested zero-deep, and foo nested one-deep:
unnest(yaml::yaml.load(a))
#> Warning: `cols` is now required when using `unnest()`.
#> ℹ Please use `cols = c(ITEM)`.
#> Error in UseMethod("unnest"): no applicable method for 'unnest' applied to an object of class "list"

# as_yamlet() provides explicit name (default key) for second element of ITEM:
as_yamlet(a)
#> - ITEM
#>  - label: sunshine
#>  - guide
#>   - foo: 1
#>   - bar: 3
```
