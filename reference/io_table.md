# Import and Export Documented Tables

Imports or exports documented tables. Generic, with methods that extend
[`read.table`](https://rdrr.io/r/utils/read.table.html) and
[`write.table`](https://rdrr.io/r/utils/write.table.html).

## Usage

``` r
io_table(x, ...)
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
[`io_csv()`](https://bergsmat.github.io/yamlet/reference/io_csv.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res()`](https://bergsmat.github.io/yamlet/reference/io_res.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
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
out <- file.path(tempdir(), 'out.tab')

# save file using io_table (returns filepath)
foo <- io_table(x, out)
stopifnot(identical(out, foo))

# read using this filepath
y <- io_table(foo, as.is = TRUE)

# lossless round-trip
attr(x, 'source') <- NULL
rownames(x) <- NULL
rownames(y) <- NULL
stopifnot(identical(x, y))
```
