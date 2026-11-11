# Decorate Character

Treats `x` as a file path. By default, metadata is sought from a file
with the same base but the 'yaml' extension.

## Usage

``` r
# S3 method for class 'character'
decorate(
  x,
  meta = NULL,
  ...,
  read = getOption("yamlet_import", as.csv),
  ext = getOption("yamlet_extension", ".yaml")
)
```

## Arguments

- x:

  file path for table data

- meta:

  file path for corresponding yamlet metadata, or a yamlet object

- ...:

  passed to `read` (if accepted) and to
  [`as_yamlet.character`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md)

- read:

  function or function name for reading x

- ext:

  file extension for metadata file, if relevant

## Value

class 'decorated' 'data.frame'

## See also

Other decorate:
[`as_decorated()`](https://bergsmat.github.io/yamlet/reference/as_decorated.md),
[`as_decorated.default()`](https://bergsmat.github.io/yamlet/reference/as_decorated.default.md),
[`decorate()`](https://bergsmat.github.io/yamlet/reference/decorate.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
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

Other interface:
[`canonical.decorated()`](https://bergsmat.github.io/yamlet/reference/canonical.decorated.md),
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
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

## Examples

``` r

# find data file
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
file
#> [1] "/home/runner/work/_temp/Library/yamlet/extdata/quinidine.csv"

# find metadata file
meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
meta
#> [1] "/home/runner/work/_temp/Library/yamlet/extdata/quinidine.yaml"

# decorate with explicit metadata reference
a <- decorate(file, meta)

# rely on default metadata path
b <- decorate(file)

# in this case: same
stopifnot(identical(a, b))
```
