# Coerce ggplot Mapping to Dataframe

Coerces ggplot mapping to 'data.frame'.

## Usage

``` r
# Default S3 method
mapframe(x, ...)
```

## Arguments

- x:

  mapping

- ...:

  ignored

## Value

data.frame

## Details

\* 'aesthetic' captures each aesthetic label as a string. \* 'map'
captures each map (typ. a column name) as a string.

## See also

Other decorated_ggplot:
[`data_context()`](https://bergsmat.github.io/yamlet/reference/data_context.md),
[`data_context.ggplot()`](https://bergsmat.github.io/yamlet/reference/data_context.ggplot.md),
[`ggplot.decorated()`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md),
[`ggplot_build.decorated_ggplot()`](https://bergsmat.github.io/yamlet/reference/ggplot_build.decorated_ggplot.md),
[`mapframe()`](https://bergsmat.github.io/yamlet/reference/mapframe.md),
[`mapframe.ggplot()`](https://bergsmat.github.io/yamlet/reference/mapframe.ggplot.md)

## Examples

``` r
library(ggplot2)
mapframe(ggplot(mapping = aes(conc, time, shape = factor(dose)))$mapping)
#>   aesthetic          map
#> 1         x         conc
#> 2         y         time
#> 3     shape factor(dose)
```
