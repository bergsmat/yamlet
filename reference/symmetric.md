# Enforce Symmetry

Enforces symmetric plot design: y axis includes opposites of the range
of the data.

## Usage

``` r
symmetric(expand = c(top = FALSE, bottom = FALSE))
```

## Arguments

- expand:

  amount of scale expansion to allow, e.g. 0.05 (default 0 for best
  fidelity)

## Value

ggplot_symmetric

## See also

ggplot_add.ggplot_symmetric

Other isometric:
[`ggplot_add.ggplot_isometric()`](https://bergsmat.github.io/yamlet/reference/ggplot_add.ggplot_isometric.md),
[`ggplot_add.ggplot_symmetric()`](https://bergsmat.github.io/yamlet/reference/ggplot_add.ggplot_symmetric.md),
[`isometric()`](https://bergsmat.github.io/yamlet/reference/isometric.md)

## Examples

``` r
library(magrittr)
library(ggplot2)
data.frame(x = 1:10, y = c(-2, 5, 0, -1, 4, 0, 1, -3, 3, 0)) %>%
ggplot(aes(x, y)) + geom_point() + symmetric()

```
