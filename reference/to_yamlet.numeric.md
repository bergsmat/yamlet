# Coerce Numeric to Yamlet Storage Format

Coerces numeric to yamlet storage format. By default, numeric values
would be processed as character. But character values with a leading
dash are ordinarily quoted, since in character context a leading dash
could be mistaken for a yaml metacharacter. In the case of a numeric
value, however, we can be fairly certain that, despite appearances, the
dash if any is actually a negative sign. This method intends to leave
negative numerics unquoted, like their positive counterparts.

## Usage

``` r
# S3 method for class 'numeric'
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
[`to_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.yamlet.md)

## Examples

``` r
library(magrittr)
 'a: [[d: [-1, 0, 1, 2]]]' %>% as_yamlet %>% to_yamlet
#> [1] "[ a: [ label: [ d: [ -1, 0, 1, 2 ]]]]"
 to_yamlet(c(a = 4, b = 5.8))
#> [1] "[ a: 4, b: 5.8 ]"
 to_yamlet(list(a = 4, b = 5.8))
#> [1] "[ a: 4, b: 5.8 ]"
 to_yamlet(numeric(0))
#> [1] ""
```
