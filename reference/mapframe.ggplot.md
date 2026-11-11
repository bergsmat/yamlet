# Coerce ggplot Mappings to Dataframe

Coerces ggplot mappings to 'data.frame'.

## Usage

``` r
# S3 method for class 'ggplot'
mapframe(x, ...)
```

## Arguments

- x:

  ggplot

- ...:

  ignored

## Value

data.frame

## Details

\* 'aesthetic' captures each aesthetic label as a string. \* 'map'
captures each map (typ. a column name) as a string. \* 'layer' is zero
for the default mapping, else the layer number. \* 'data' is NA for
default data if missing. Else zero for x\$data, zero for layers with no
data, and layer number for layers with data.

## See also

Other decorated_ggplot:
[`data_context()`](https://bergsmat.github.io/yamlet/reference/data_context.md),
[`data_context.ggplot()`](https://bergsmat.github.io/yamlet/reference/data_context.ggplot.md),
[`ggplot.decorated()`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md),
[`ggplot_build.decorated_ggplot()`](https://bergsmat.github.io/yamlet/reference/ggplot_build.decorated_ggplot.md),
[`mapframe()`](https://bergsmat.github.io/yamlet/reference/mapframe.md),
[`mapframe.default()`](https://bergsmat.github.io/yamlet/reference/mapframe.default.md)

## Examples

``` r
library(ggplot2)
mapframe(ggplot(mapping = aes(conc, time, shape = factor(dose))))
#>   aesthetic          map layer data
#> 1         x         conc     0   NA
#> 2         y         time     0   NA
#> 3     shape factor(dose)     0   NA
```
