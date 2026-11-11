# Subset Yamlet

Subsets yamlet. Preserves class, since a subset of yamlet is still
yamlet.

## Usage

``` r
# S3 method for class 'yamlet'
x[...]
```

## Arguments

- x:

  object to subset

- ...:

  passed to next method

## Value

yamlet

## See also

Other yamlet:
[`as.character.yamlet()`](https://bergsmat.github.io/yamlet/reference/as.character.yamlet.md),
[`as_yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
[`as_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md),
[`as_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.data.frame.md),
[`as_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.list.md),
[`as_yamlet.yam()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yam.md),
[`as_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yamlet.md),
[`print.yamlet()`](https://bergsmat.github.io/yamlet/reference/print.yamlet.md)

## Examples

``` r
meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
meta <- as_yamlet(meta)
class(meta)
#> [1] "yamlet"
stopifnot(inherits(meta[1:2],'yamlet'))
```
