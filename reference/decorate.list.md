# Decorate List

Decorates a list-like object. Takes metadata in yamlet format and loads
it onto corresponding list elements as attributes.

## Usage

``` r
# S3 method for class 'list'
decorate(
  x,
  meta = NULL,
  ...,
  ext = getOption("yamlet_extension", ".yaml"),
  persistence = getOption("yamlet_persistence", FALSE),
  overwrite = getOption("yamlet_overwrite", FALSE)
)
```

## Arguments

- x:

  object inheriting from `list`

- meta:

  file path for corresponding yaml metadata, or a yamlet or something
  coercible to yamlet; an attempt will be made to guess the file path if
  x has a 'source' attribute (as for
  [`as.csv`](https://rdrr.io/pkg/csv/man/as.csv.html))

- ...:

  passed to
  [`as_yamlet.character`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md)
  (by method dispatch)

- ext:

  file extension for metadata file, if relevant

- persistence:

  whether to coerce decorated items to 'dvec' where suitable method
  exists

- overwrite:

  whether to overwrite attributes that are already present (else give
  warning)

## Value

like x but with 'decorated' as first class element

## Details

As of v0.8.8, attribute persistence is supported by optionally coercing
decorated items to class 'dvec' where suitable methods exist.
`persistence` is false by default for the list method but true by
default for the data.frame method. See also
[`decorate.data.frame`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md).

## See also

Other decorate:
[`as_decorated()`](https://bergsmat.github.io/yamlet/reference/as_decorated.md),
[`as_decorated.default()`](https://bergsmat.github.io/yamlet/reference/as_decorated.default.md),
[`decorate()`](https://bergsmat.github.io/yamlet/reference/decorate.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
[`decorate_groups()`](https://bergsmat.github.io/yamlet/reference/decorate_groups.md),
[`decorate_groups.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate_groups.data.frame.md),
[`decorations()`](https://bergsmat.github.io/yamlet/reference/decorations.md),
[`decorations.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorations.data.frame.md),
[`decorations_groups()`](https://bergsmat.github.io/yamlet/reference/decorations_groups.md),
[`decorations_groups.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorations_groups.data.frame.md),
[`group_by_decorations()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.md),
[`group_by_decorations.data.frame()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.data.frame.md),
[`redecorate()`](https://bergsmat.github.io/yamlet/reference/redecorate.md),
[`undecorate()`](https://bergsmat.github.io/yamlet/reference/undecorate.md),
[`undecorate.default()`](https://bergsmat.github.io/yamlet/reference/undecorate.default.md)

## Examples

``` r
example(decorate.data.frame)
#> 
#> dcrt..> # find data path
#> dcrt..> library(csv)
#> 
#> dcrt..> file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#> 
#> dcrt..> file
#> [1] "/home/runner/work/_temp/Library/yamlet/extdata/quinidine.csv"
#> 
#> dcrt..> dat <- as.csv(file) # dat now has 'source' attribute
#> 
#> dcrt..> # use source attribute to find metadata 
#> dcrt..> a <- decorate(as.csv(file))
#> 
#> dcrt..> # supply metadata path (or something close) explicitly
#> dcrt..> b <- decorate(dat, meta = file)
#> 
#> dcrt..> # these are equivalent
#> dcrt..> stopifnot(identical(a, b))
```
