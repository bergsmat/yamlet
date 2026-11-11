# Import and Export Resolved Tables

Inter-converts between tables as comma-separated variable and fully
resolved data frames. Generic, with character and data.frame methods
that extend
[`io_csv`](https://bergsmat.github.io/yamlet/reference/io_csv.md).

## Usage

``` r
io_res(x, ...)
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
example(io_res.character)
#> 
#> i_rs.c> library(magrittr)
#> 
#> i_rs.c> file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#> 
#> i_rs.c> x <- io_csv(file) %>% resolve
#> 
#> i_rs.c> y <- io_res(file)
#> 
#> i_rs.c> identical(x, y)
#> [1] TRUE
```
