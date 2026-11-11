# Export Resolved Table

Exports a documented table. "Desolves" attributes to standard form, then
writes data and metadata to storage. A short-cut for
`(io_csv(desolve(x))`.

## Usage

``` r
# S3 method for class 'decorated'
io_res(x, file = "", ...)
```

## Arguments

- x:

  decorated; passed to
  [`io_csv.data.frame`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md)

- file:

  passed to
  [`io_csv.data.frame`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md)

- ...:

  passed to
  [`io_csv.character`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md)
  and
  [`desolve.decorated`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md)

## Value

decorated (invisible)

## See also

Other io:
[`io_csv()`](https://bergsmat.github.io/yamlet/reference/io_csv.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res()`](https://bergsmat.github.io/yamlet/reference/io_res.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_table()`](https://bergsmat.github.io/yamlet/reference/io_table.md),
[`io_table.character()`](https://bergsmat.github.io/yamlet/reference/io_table.character.md),
[`io_table.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_table.data.frame.md),
[`io_yamlet()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.md),
[`io_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.character.md),
[`io_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.data.frame.md),
[`io_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.yamlet.md)

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
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- io_res(file)
tmp <- tempfile(fileext = '.csv')
io_res(x, tmp)
a <- io_csv(tmp, source = FALSE)
b <- io_csv(file, source = FALSE)
stopifnot(identical(a, b))
```
