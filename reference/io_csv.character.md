# Import Documented Table as CSV

Imports a documented table as comma-separated variable. A wrapper for
[`as.csv.character`](https://rdrr.io/pkg/csv/man/as.csv.character.html)
that also reads associated yamlet metadata, if present, and applies it
as attributes.

## Usage

``` r
# S3 method for class 'character'
io_csv(
  x,
  ext = getOption("yamlet_extension", ".yaml"),
  meta = NULL,
  gz = NULL,
  ...
)
```

## Arguments

- x:

  character file path; passed to
  [`as.csv.character`](https://rdrr.io/pkg/csv/man/as.csv.character.html)
  (by method dispatch)

- ext:

  extension for metadata equivalent of x

- meta:

  explicit file path for metadata; if null, `ext` is appended to x after
  removing (final) extension, if any

- gz:

  logical; guessed by default from `x`; if TRUE, '.gz' extension
  enforced present for `x` and absent for default `meta`

- ...:

  passed to
  [`as.csv.character`](https://rdrr.io/pkg/csv/man/as.csv.character.html)
  and to
  [`decorate`](https://bergsmat.github.io/yamlet/reference/decorate.md)

## Value

data.frame

## See also

Other io:
[`io_csv()`](https://bergsmat.github.io/yamlet/reference/io_csv.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res()`](https://bergsmat.github.io/yamlet/reference/io_res.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
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
example(io_csv)
#> 
#> io_csv> # generate some decorated data
#> io_csv> file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#> 
#> io_csv> x <- decorate(file)
#> 
#> io_csv> # get a temporary filepath
#> io_csv> out <- file.path(tempdir(), 'out.csv')
#> 
#> io_csv> # save file using io_csv (returns filepath)
#> io_csv> foo <- io_csv(x, out)
#> 
#> io_csv> stopifnot(identical(out, foo))
#> 
#> io_csv> # read using this filepath
#> io_csv> y <- io_csv(foo)
#> 
#> io_csv> # lossless round-trip (ignoring source attribute)
#> io_csv> attr(x, 'source') <- NULL
#> 
#> io_csv> attr(y, 'source') <- NULL
#> 
#> io_csv> stopifnot(identical(x, y))
```
