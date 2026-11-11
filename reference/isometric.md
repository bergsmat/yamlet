# Enforce Isometry

Enforces isometric plot design: aspect ratio of 1, identical ranges for
x and y axes. Can be used meaningfully with
`+ facet_wrap(scales = 'free' ...)`.

## Usage

``` r
isometric(expand = 0)
```

## Arguments

- expand:

  amount of scale expansion to allow, e.g. 0.05 (default 0 for best
  fidelity)

## Value

ggplot_isometric

## See also

ggplot_add.ggplot_isometric

Other isometric:
[`ggplot_add.ggplot_isometric()`](https://bergsmat.github.io/yamlet/reference/ggplot_add.ggplot_isometric.md),
[`ggplot_add.ggplot_symmetric()`](https://bergsmat.github.io/yamlet/reference/ggplot_add.ggplot_symmetric.md),
[`symmetric()`](https://bergsmat.github.io/yamlet/reference/symmetric.md)

## Examples

``` r
library(magrittr)
library(ggplot2)
data.frame(x = 1:5, y = 3:7) %>%
ggplot(aes(x, y)) + geom_point() + isometric()
```
