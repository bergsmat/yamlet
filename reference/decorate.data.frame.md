# Decorate Data Frame

Decorates a data.frame. Expects metadata in yamlet format, and loads it
onto columns as attributes.

## Usage

``` r
# S3 method for class 'data.frame'
decorate(
  x,
  meta = NULL,
  ...,
  persistence = getOption("yamlet_persistence", TRUE)
)
```

## Arguments

- x:

  data.frame

- meta:

  file path for corresponding yaml metadata, or a yamlet; an attempt
  will be made to guess the file path if x has a 'source' attribute

- ...:

  passed to
  [`decorate.list`](https://bergsmat.github.io/yamlet/reference/decorate.list.md)

- persistence:

  whether to coerce decorated columns to 'dvec' where suitable method
  exists

## Value

class 'decorated' 'data.frame'

## Details

As of v0.8.8, the data.frame method for decorate() coerces affected
columns using
[`as_dvec`](https://bergsmat.github.io/yamlet/reference/as_dvec.md) if
`persistence` is true and a suitable method exists. 'vctrs' methods are
implemented for class `dvec` to help attributes persist during tidyverse
operations. Details are described in
[`c.dvec`](https://bergsmat.github.io/yamlet/reference/c.dvec.md).
Disable this functionality with `options(yamlet_persistence = FALSE)`.

## See also

decorate.list

Other interface:
[`canonical.decorated()`](https://bergsmat.github.io/yamlet/reference/canonical.decorated.md),
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`enscript.decorated()`](https://bergsmat.github.io/yamlet/reference/enscript.decorated.md),
[`ggplot.decorated()`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
[`io_table.character()`](https://bergsmat.github.io/yamlet/reference/io_table.character.md),
[`io_table.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_table.data.frame.md),
[`io_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.character.md),
[`io_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.data.frame.md),
[`is_parseable.default()`](https://bergsmat.github.io/yamlet/reference/is_parseable.default.md),
[`mimic.default()`](https://bergsmat.github.io/yamlet/reference/mimic.default.md),
[`modify.default()`](https://bergsmat.github.io/yamlet/reference/modify.default.md),
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md),
[`read_yamlet()`](https://bergsmat.github.io/yamlet/reference/read_yamlet.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md),
[`write_yamlet()`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)

Other decorate:
[`as_decorated()`](https://bergsmat.github.io/yamlet/reference/as_decorated.md),
[`as_decorated.default()`](https://bergsmat.github.io/yamlet/reference/as_decorated.default.md),
[`decorate()`](https://bergsmat.github.io/yamlet/reference/decorate.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`decorate.list()`](https://bergsmat.github.io/yamlet/reference/decorate.list.md),
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

# find data path
library(csv)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
file
#> [1] "/home/runner/work/_temp/Library/yamlet/extdata/quinidine.csv"
dat <- as.csv(file) # dat now has 'source' attribute

# use source attribute to find metadata 
a <- decorate(as.csv(file))

# supply metadata path (or something close) explicitly
b <- decorate(dat, meta = file)

# these are equivalent
stopifnot(identical(a, b))
```
