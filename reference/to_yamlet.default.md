# Coerce Default to Yamlet Storage Format

Coerces to yamlet storage format by default conversion to character.

## Usage

``` r
# Default S3 method
to_yamlet(x, ...)
```

## Arguments

- x:

  object

- ...:

  ignored

## Value

length-one character

## See also

Other to_yamlet:
[`to_yamlet()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.md),
[`to_yamlet.NULL()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.NULL.md),
[`to_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.character.md),
[`to_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.list.md),
[`to_yamlet.numeric()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.numeric.md),
[`to_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.yamlet.md)

## Examples

``` r
to_yamlet(3)
#> [1] "3"
to_yamlet(c(a = '4',b = '5.8'))
#> [1] "[ a: 4, b: 5.8 ]"
to_yamlet(c(a = 4,b = 5.8))
#> [1] "[ a: 4, b: 5.8 ]"
to_yamlet(TRUE)
#> [1] "'TRUE'"
```
