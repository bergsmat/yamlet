# Coerce list to yamlet Storage Format

Coerces list to yamlet storage format. Operates recursively on list
members.

## Usage

``` r
# S3 method for class 'list'
to_yamlet(x, ..., bracket_all = FALSE)
```

## Arguments

- x:

  object

- ...:

  ignored

- bracket_all:

  FALSE at top level, but TRUE recursively downward; supports 'ITEM:
  definition' which would otherwise be bracketted

## Value

length-one character

## See also

Other to_yamlet:
[`to_yamlet()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.md),
[`to_yamlet.NULL()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.NULL.md),
[`to_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.character.md),
[`to_yamlet.default()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.default.md),
[`to_yamlet.numeric()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.numeric.md),
[`to_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.yamlet.md)

## Examples

``` r
to_yamlet(list())
#> [1] ""
to_yamlet(list(a = 1, b = 2, c = NULL))
#> [1] "[ a: 1, b: 2, ? c ]"
to_yamlet(list(a = 1, b = list(c = 3, d = list(e = 4, f = 'g', 'h'))))
#> [1] "[ a: 1, b: [ c: 3, d: [ e: 4, f: g, h ]]]"
to_yamlet(setNames(1:3, c('a','b',NA)))
#> [1] "[ a: 1, b: 2, NA: 3 ]"
to_yamlet(setNames(c(1,2,NA), c('a','b','c')))
#> [1] "[ a: 1, b: 2, c: NA ]"
```
