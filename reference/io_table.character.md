# Import Documented Table

Imports a documented table. A wrapper for read.table() that also reads
associated yamlet metadata, if present, and applies it as attributes.

## Usage

``` r
# S3 method for class 'character'
io_table(x, ext = getOption("yamlet_extension", ".yaml"), meta = NULL, ...)
```

## Arguments

- x:

  character file path; passed to
  [`read.table`](https://rdrr.io/r/utils/read.table.html)

- ext:

  extension for metadata equivalent of x

- meta:

  explicit file path for metadata

- ...:

  passed to [`read.table`](https://rdrr.io/r/utils/read.table.html) (if
  accepted) and to
  [`decorate`](https://bergsmat.github.io/yamlet/reference/decorate.md)

## Value

data.frame

## See also

Other io:
[`io_csv()`](https://bergsmat.github.io/yamlet/reference/io_csv.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res()`](https://bergsmat.github.io/yamlet/reference/io_res.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
[`io_table()`](https://bergsmat.github.io/yamlet/reference/io_table.md),
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
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
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
example(io_table)
#> 
#> io_tbl> # generate some decorated data
#> io_tbl> file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#> 
#> io_tbl> x <- decorate(file)
#> 
#> io_tbl> # get a temporary filepath
#> io_tbl> out <- file.path(tempdir(), 'out.tab')
#> 
#> io_tbl> # save file using io_table (returns filepath)
#> io_tbl> foo <- io_table(x, out)
#> 
#> io_tbl> stopifnot(identical(out, foo))
#> 
#> io_tbl> # read using this filepath
#> io_tbl> y <- io_table(foo, as.is = TRUE)
#> 
#> io_tbl> # lossless round-trip
#> io_tbl> attr(x, 'source') <- NULL
#> 
#> io_tbl> rownames(x) <- NULL
#> 
#> io_tbl> rownames(y) <- NULL
#> 
#> io_tbl> stopifnot(identical(x, y))
```
