# Identify Selected Names by Default

Evaluates un-named arguments in `...` using
[`select`](https://dplyr.tidyverse.org/reference/select.html) rules,
returning explicit names in x. Returns all (non-empty) names by default
(if no dots supplied).

## Usage

``` r
# Default S3 method
selected(x, ..., expand = TRUE)
```

## Arguments

- x:

  object

- ...:

  to [`select`](https://dplyr.tidyverse.org/reference/select.html)

- expand:

  if true, selecting nothing expands to selecting everything

## Value

character: names in x

## See also

Other modify:
[`modify()`](https://bergsmat.github.io/yamlet/reference/modify.md),
[`modify.default()`](https://bergsmat.github.io/yamlet/reference/modify.default.md),
[`named()`](https://bergsmat.github.io/yamlet/reference/named.md),
[`selected()`](https://bergsmat.github.io/yamlet/reference/selected.md)

Other interface:
[`canonical.decorated()`](https://bergsmat.github.io/yamlet/reference/canonical.decorated.md),
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
[`write_yamlet()`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)

## Examples

``` r
library(magrittr)
list(a = 1, b = 1:10, c = letters) %>%
selected(b:c)
#> [1] "b" "c"
#> attr(,"class")
#> [1] "selected"  "character"
```
