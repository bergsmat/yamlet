# Coerce Yamlet to Yamlet Storage Format

Coerces yamlet to yamlet storage format by unclassing to list.

## Usage

``` r
# S3 method for class 'yamlet'
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
[`to_yamlet.default()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.default.md),
[`to_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.list.md),
[`to_yamlet.numeric()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.numeric.md)

## Examples

``` r
library(magrittr)
 'a: [[d: [-1, 0, 1, 2]]]' %>% as_yamlet %>% to_yamlet
#> [1] "[ a: [ label: [ d: [ -1, 0, 1, 2 ]]]]"
```
