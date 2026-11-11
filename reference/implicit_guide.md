# Coerce Guide to Something More Implicit

Coerces 'guide' to something more implicit. Generic, with methods for
data.frame. The key 'guide' generally suggests a guide to interpretation
of a data item, such as units, formats, codelists, and encodings. The
idea here is to replace these with 'guide': i.e., to undo the effects of
[`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md).

## Usage

``` r
implicit_guide(x, ...)
```

## Arguments

- x:

  object of dispatch

- ...:

  passed arguments

## Value

see methods

## See also

Other explicit_guide:
[`explicit_guide()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`explicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md),
[`explicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.factor.md),
[`explicit_guide.yamlet()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md),
[`implicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.data.frame.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md),
[`infer_guide()`](https://bergsmat.github.io/yamlet/reference/infer_guide.md)
