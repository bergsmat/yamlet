# Add Symmetry to Plot Object

Adds y axis symmetry to plot object.

## Usage

``` r
# S3 method for class 'ggplot_symmetric'
ggplot_add(object, plot, object_name, ...)
```

## Value

gg

## See also

symmetric

Other isometric:
[`ggplot_add.ggplot_isometric()`](https://bergsmat.github.io/yamlet/reference/ggplot_add.ggplot_isometric.md),
[`isometric()`](https://bergsmat.github.io/yamlet/reference/isometric.md),
[`symmetric()`](https://bergsmat.github.io/yamlet/reference/symmetric.md)

## Examples

``` r
example(symmetric)
#> 
#> symmtr> library(magrittr)
#> 
#> symmtr> library(ggplot2)
#> 
#> symmtr> data.frame(x = 1:10, y = c(-2, 5, 0, -1, 4, 0, 1, -3, 3, 0)) %>%
#> symmtr+ ggplot(aes(x, y)) + geom_point() + symmetric()
```
