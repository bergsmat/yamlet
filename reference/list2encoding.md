# Coerce List to Encoding

Tries to coerce a list to an encoding. Names are understood as decodes,
and list values as codes. On failure, the list is returned unchanged.

## Usage

``` r
list2encoding(x, ...)
```

## Arguments

- x:

  list

- ...:

  ignored

## Value

an encoding (length-one character), or original list if error occurs

## See also

Other encode:
[`encode.yamlet()`](https://bergsmat.github.io/yamlet/reference/encode.yamlet.md)

## Examples

``` r
meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
meta <- as_yamlet(meta)
list2encoding(meta$Creatinine$guide)
#> [1] "||< 50|< 50 mg/min||>= 50|>= 50 mg/min||"
```
