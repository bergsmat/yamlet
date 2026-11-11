# Write Yamlet

Writes yamlet to file. Similar to
[`io_yamlet.yamlet`](https://bergsmat.github.io/yamlet/reference/io_yamlet.yamlet.md)
but returns invisible storage format instead of invisible storage
location.

## Usage

``` r
write_yamlet(
  x,
  con = stdout(),
  eol = "\n",
  useBytes = FALSE,
  default_keys = getOption("yamlet_default_keys", list("label", "guide")),
  fileEncoding = getOption("encoding"),
  block = FALSE,
  ...
)
```

## Arguments

- x:

  something that can be coerced to class 'yamlet', like a yamlet object
  or a decorated data.frame

- con:

  passed to [`writeLines`](https://rdrr.io/r/base/writeLines.html)

- eol:

  end-of-line; passed to
  [`writeLines`](https://rdrr.io/r/base/writeLines.html) as `sep`

- useBytes:

  passed to [`writeLines`](https://rdrr.io/r/base/writeLines.html)

- default_keys:

  character: default keys for the first n anonymous members of each
  element

- fileEncoding:

  if `con` is character, passed to
  [`file`](https://rdrr.io/r/base/connections.html) as `encoding`

- block:

  whether to write block scalars

- ...:

  passed to
  [`as_yamlet`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md)
  and to
  [`as.character.yamlet`](https://bergsmat.github.io/yamlet/reference/as.character.yamlet.md)

## Value

invisible character representation of yamlet (storage syntax)

## See also

[`decorate.list`](https://bergsmat.github.io/yamlet/reference/decorate.list.md)

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
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md)

## Examples

``` r
library(csv)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
x <- as.csv(file)
y <- read_yamlet(meta)
x <- decorate(x, meta = y)
identical(x, decorate(file))
#> [1] TRUE
tmp <- tempfile()
write_yamlet(x, tmp)
stopifnot(identical(read_yamlet(meta), read_yamlet(tmp)))
```
