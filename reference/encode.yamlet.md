# Encode Yamlet

Encodes yamlet. Each 'guide' element with length \> 1 is converted to an
encoding, if possible. If `data` is supplied, conditional guides will be
ignored.

## Usage

``` r
# S3 method for class 'yamlet'
encode(x, ..., target = "guide", data = NULL)
```

## Arguments

- x:

  yamlet

- ...:

  ignored

- target:

  attribute to encode

- data:

  optional data.frame for guide context

## Value

yamlet, with guide elements possibly transformed to encodings

## See also

Other encode:
[`list2encoding()`](https://bergsmat.github.io/yamlet/reference/list2encoding.md)

## Examples

``` r
meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
meta <- as_yamlet(meta)
meta <- encode(meta)
```
