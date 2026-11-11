# Sort Decorations

Enforces canonical attribute order for class 'decorated'. Set of
default_keys will be augmented with all observed attribute names and
will be expanded or reduced as necessary for each data item.

## Usage

``` r
# S3 method for class 'decorated'
canonical(
  x,
  default_keys = getOption("yamlet_default_keys", list("label", "guide")),
  ...
)
```

## Arguments

- x:

  decorated

- default_keys:

  attribute names in preferred order

- ...:

  ignored

## Value

decorated

## See also

Other canonical:
[`canonical()`](https://bergsmat.github.io/yamlet/reference/canonical.md),
[`canonical.yamlet()`](https://bergsmat.github.io/yamlet/reference/canonical.yamlet.md)

Other interface:
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
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
# make some decorated data
library(magrittr)
x <- data.frame(x = 1, y = 1, z = factor('a'))
x %<>% decorate('
x: [ guide: mm, desc: this, label: foo ]
"y": [ guide: bar, desc: other ]
')

# retrieve decorations: label not first!
decorations(x)
#> - x
#>  - guide: mm
#>  - desc: this
#>  - label: foo
#> - y
#>  - guide: bar
#>  - desc: other
#> - z: []

# sort label first by default
decorations(canonical(x))
#> - x
#>  - label: foo
#>  - guide: mm
#>  - desc: this
#> - y
#>  - guide: bar
#>  - desc: other
#> - z: []

# equivalent invocation
canonical(decorations(x))
#> - x
#>  - label: foo
#>  - guide: mm
#>  - desc: this
#> - y
#>  - guide: bar
#>  - desc: other
#> - z: []
```
