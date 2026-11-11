# Coerce Character to Yamlet Storage Format

Coerces character to yamlet storage format. Named character is processed
as a named list. NA_character\_ is is stored as unquoted NA and read
back as NA by
[`as_yam.character`](https://bergsmat.github.io/yamlet/reference/as_yam.character.md).
Use quoted "NA" or 'NA' to store the literal string, which will be read
back as a string by
[`as_yam.character`](https://bergsmat.github.io/yamlet/reference/as_yam.character.md).

## Usage

``` r
# S3 method for class 'character'
to_yamlet(x, block = FALSE, ...)
```

## Arguments

- x:

  character

- block:

  whether to write block scalars

- ...:

  ignored

## Value

length-one character, never NA, no names

## Details

If block is TRUE, an attempt will be made to represent character strings
as literal block scalars if they contain newlines (experimental in
yamlet \>= 0.8).

## See also

Other to_yamlet:
[`to_yamlet()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.md),
[`to_yamlet.NULL()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.NULL.md),
[`to_yamlet.default()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.default.md),
[`to_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.list.md),
[`to_yamlet.numeric()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.numeric.md),
[`to_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/to_yamlet.yamlet.md)

## Examples

``` r
to_yamlet('foo')
#> [1] "foo"
to_yamlet(c('a','b'))
#> [1] "[ a, b ]"
to_yamlet(c(a = 'a',b = 'b'))
#> [1] "[ a: a, b: b ]"
to_yamlet(c(no = 'n', yes = 'y'))
#> [1] "[ 'no': 'n', 'yes': 'y' ]"
to_yamlet(NA)
#> [1] "NA"
to_yamlet("NA")
#> [1] "'NA'"
```
