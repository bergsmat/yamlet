# Import and Export Documented Tables as CSV

Imports or exports documented tables as comma-separated variable.
Generic, with methods that extend
[`as.csv`](https://rdrr.io/pkg/csv/man/as.csv.html).

## Usage

``` r
io_csv(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

See methods.

## See also

Other io:
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
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

## Examples

``` r
# generate some decorated data
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)

# get a temporary filepath
out <- file.path(tempdir(), 'out.csv')

# save file using io_csv (returns filepath)
foo <- io_csv(x, out)
stopifnot(identical(out, foo))

# read using this filepath
y <- io_csv(foo)

# lossless round-trip (ignoring source attribute)
attr(x, 'source') <- NULL
attr(y, 'source') <- NULL
stopifnot(identical(x, y))
```
