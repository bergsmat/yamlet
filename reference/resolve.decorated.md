# Resolve Guide for Decorated

Resolves implicit usage of default key 'guide' to explicit usage for
decorated class. Calls
[`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`classified`](https://bergsmat.github.io/yamlet/reference/classified.md),
and
[`make_title`](https://bergsmat.github.io/yamlet/reference/make_title.md).

## Usage

``` r
# S3 method for class 'decorated'
resolve(x, ..., scripted = FALSE)
```

## Arguments

- x:

  decorated

- ...:

  passed to
  [`explicit_guide`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
  [`classified`](https://bergsmat.github.io/yamlet/reference/classified.md),
  and
  [`make_title`](https://bergsmat.github.io/yamlet/reference/make_title.md)

- scripted:

  logical; invokes
  [`enscript.decorated`](https://bergsmat.github.io/yamlet/reference/enscript.decorated.md)
  if TRUE

## Value

decorated

## See also

Other resolve:
[`desolve()`](https://bergsmat.github.io/yamlet/reference/desolve.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`desolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/desolve.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`desolve.default()`](https://bergsmat.github.io/yamlet/reference/desolve.default.md),
[`desolve.dvec()`](https://bergsmat.github.io/yamlet/reference/desolve.dvec.md),
[`enscript.data.frame()`](https://bergsmat.github.io/yamlet/reference/enscript.data.frame.md),
[`enscript.default()`](https://bergsmat.github.io/yamlet/reference/enscript.default.md),
[`resolve()`](https://bergsmat.github.io/yamlet/reference/resolve.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/resolve.data.frame.md),
[`resolve.default()`](https://bergsmat.github.io/yamlet/reference/resolve.default.md),
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md)

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
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md),
[`write_yamlet()`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)

## Examples

``` r
# generate some decorated data
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)
x %>% decorations(Age, glyco)
#> - Age
#>  - label: baseline subject age
#>  - guide: year
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - guide: mg/dL

# resolve everything, and show selected decorations
x %>% resolve %>% decorations(Age, glyco)
#> - Age
#>  - label: baseline subject age
#>  - units: year
#>  - title: baseline subject age (year)
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - units: mg/dL
#>  - title: alpha-1 acid glycoprotein concentration (mg/dL)

# resolve selectively, and show selected decorations
x %>% resolve(glyco) %>% decorations(Age, glyco)
#> - Age
#>  - label: baseline subject age
#>  - guide: year
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - units: mg/dL
#>  - title: alpha-1 acid glycoprotein concentration (mg/dL)
```
