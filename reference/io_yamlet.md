# Import and Export Yamlet

Imports and exports yamlet. Generic, with a read method
[`io_yamlet.character`](https://bergsmat.github.io/yamlet/reference/io_yamlet.character.md)
for character and a write method
[`io_yamlet.data.frame`](https://bergsmat.github.io/yamlet/reference/io_yamlet.data.frame.md)
for data.frame. See also
[`io_yamlet.yamlet`](https://bergsmat.github.io/yamlet/reference/io_yamlet.yamlet.md).

## Usage

``` r
io_yamlet(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

see methods

## See also

Other io:
[`io_csv()`](https://bergsmat.github.io/yamlet/reference/io_csv.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res()`](https://bergsmat.github.io/yamlet/reference/io_res.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
[`io_table()`](https://bergsmat.github.io/yamlet/reference/io_table.md),
[`io_table.character()`](https://bergsmat.github.io/yamlet/reference/io_table.character.md),
[`io_table.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_table.data.frame.md),
[`io_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.character.md),
[`io_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.data.frame.md),
[`io_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.yamlet.md)

## Examples

``` r
file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
x <- io_yamlet(file)
tmp <- tempdir()
out <- file.path(tmp, 'tmp.yaml')

# we can losslessly 'round-trip' x using to generic calls
identical(x, io_yamlet(io_yamlet(x, out)))
#> [1] TRUE
```
