# Coerce Null to Yamlet Storage Format

Coerces null to yamlet storage format (returns empty string).

## Usage

``` r
# S3 method for class '`NULL`'
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
[`to_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.character.md),
[`to_yamlet.default()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.default.md),
[`to_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.list.md),
[`to_yamlet.numeric()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.numeric.md),
[`to_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.yamlet.md)

## Examples

``` r
to_yamlet(NULL)
#> [1] ""
```
