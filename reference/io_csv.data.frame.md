# Export Documented Table as CSV

Exports a data.frame as comma-separated variable, as well as a yamlet
version of its decorations. A wrapper for
[`as.csv.data.frame`](https://rdrr.io/pkg/csv/man/as.csv.data.frame.html).

## Usage

``` r
# S3 method for class 'data.frame'
io_csv(
  x,
  file = "",
  ext = getOption("yamlet_extension", ".yaml"),
  meta = stdout(),
  gz = NULL,
  useBytes = FALSE,
  default_keys = getOption("yamlet_default_keys", list("label", "guide")),
  ...
)
```

## Arguments

- x:

  data.frame

- file:

  passed to
  [`as.csv.data.frame`](https://rdrr.io/pkg/csv/man/as.csv.data.frame.html)
  (by method dispatch)

- ext:

  = extension for metadata equivalent of x

- meta:

  passed as `con` to
  [`io_yamlet`](https://bergsmat.github.io/yamlet/reference/io_yamlet.md)

- gz:

  logical; guessed by default from `x`; if TRUE, '.gz' extension
  enforced present for (character) `file` and absent for default `meta`

- useBytes:

  passed to
  [`io_yamlet`](https://bergsmat.github.io/yamlet/reference/io_yamlet.md)

- default_keys:

  passed to
  [`io_yamlet`](https://bergsmat.github.io/yamlet/reference/io_yamlet.md)

- ...:

  passed to [`as.csv`](https://rdrr.io/pkg/csv/man/as.csv.html) and to
  [`io_yamlet`](https://bergsmat.github.io/yamlet/reference/io_yamlet.md)

## Value

invisible(file)

## Details

You should be able to supply exactly the connections you want for `file`
(the data file) and `meta` (the metadata file) if `gz` is FALSE. If `gz`
is NULL, it will be guessed from file (TRUE for character ending with
'.gz' or '.GZ'). If TRUE, character `file` will have '.gz' extension
enforced, but any '.gz' ('.GZ') will be stripped when calculating
`meta`.

## See also

Other io:
[`io_csv()`](https://bergsmat.github.io/yamlet/reference/io_csv.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
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
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
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
