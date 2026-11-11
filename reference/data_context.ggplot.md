# Retrieve Data Context of ggplot

Retrieves data context of ggplot as 'data.frame'. calls
[`mapframe.ggplot`](https://bergsmat.github.io/yamlet/reference/mapframe.ggplot.md),
and limits this to rows where 'data' is not NA and 'aesthetic' is not
duplicated. I.e., for each aesthetic, this function returns the first
mapping that has (at least default) data. Such mappings should be what
ggplot uses to train the corresponding scales.

## Usage

``` r
# S3 method for class 'ggplot'
data_context(x, ...)
```

## Arguments

- x:

  ggplot

- ...:

  ignored

## Value

data.frame

## See also

Other decorated_ggplot:
[`data_context()`](https://bergsmat.github.io/yamlet/reference/data_context.md),
[`ggplot.decorated()`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md),
[`ggplot_build.decorated_ggplot()`](https://bergsmat.github.io/yamlet/reference/ggplot_build.decorated_ggplot.md),
[`mapframe()`](https://bergsmat.github.io/yamlet/reference/mapframe.md),
[`mapframe.default()`](https://bergsmat.github.io/yamlet/reference/mapframe.default.md),
[`mapframe.ggplot()`](https://bergsmat.github.io/yamlet/reference/mapframe.ggplot.md)
