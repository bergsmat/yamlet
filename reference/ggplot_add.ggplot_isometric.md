# Add Isometry to Plot Object

Adds isometry to plot object.

## Usage

``` r
# S3 method for class 'ggplot_isometric'
ggplot_add(object, plot, object_name, ...)
```

## Value

gg

## See also

isometric

Other isometric:
[`ggplot_add.ggplot_symmetric()`](https://bergsmat.github.io/yamlet/reference/ggplot_add.ggplot_symmetric.md),
[`isometric()`](https://bergsmat.github.io/yamlet/reference/isometric.md),
[`symmetric()`](https://bergsmat.github.io/yamlet/reference/symmetric.md)

## Examples

``` r
example(isometric)
#> 
#> ismtrc> library(magrittr)
#> 
#> ismtrc> library(ggplot2)
#> 
#> ismtrc> data.frame(x = 1:5, y = 3:7) %>%
#> ismtrc+ ggplot(aes(x, y)) + geom_point() + isometric()
```
