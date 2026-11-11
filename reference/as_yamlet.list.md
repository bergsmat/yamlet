# Coerce List to Yamlet

Coerces list to yamlet. Assigns class 'yamlet'. Checks that list has
names.

## Usage

``` r
# S3 method for class 'list'
as_yamlet(x, ...)
```

## Arguments

- x:

  list

- ...:

  ignored

## Value

yamlet

## See also

Other yamlet:
[`[.yamlet()`](https://bergsmat.github.io/yamlet/reference/sub-.yamlet.md),
[`as.character.yamlet()`](https://bergsmat.github.io/yamlet/reference/as.character.yamlet.md),
[`as_yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
[`as_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md),
[`as_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.data.frame.md),
[`as_yamlet.yam()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yam.md),
[`as_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yamlet.md),
[`print.yamlet()`](https://bergsmat.github.io/yamlet/reference/print.yamlet.md)

## Examples

``` r
x <- list(a = 2, b = 3)
as_yamlet(x)
#> - a: 2
#> - b: 3
```
