# Substitute Expressions, Titles, Labels and Aesthetics in ggplots

Substitutes expressions, titles, labels and aesthetics in decorated
ggplots. Default labels (mappings for `x`, `y`,) will be replaced (if
possible) the first available of corresponding data attributes in
`get_option('yamlet_decorated_ggplot_search', c('expression', 'title', 'label'))`.
Likewise, if mappings for
`getOption('yamlet_decorated_ggplot_discrete',c('colour', 'fill', 'size', 'shape', 'linetype', 'linewidth', 'alpha'))`
indicate columns that have these defined as attributes, an attempt is
made to add a corresponding discrete scale if one does not exist
already. Values are recycled if necessary and are specific by ordinal
position to the corresponding level of the corresponding variable.
Levels are defined in increasing priority by \* `sort(unique(x))` (if
`getOption('yamlet_decorated_ggplot_drop', TRUE)`), \* any guide
attribute, \* any factor levels, \* any codelist attribute, or \* any
plotmath attribute.

## Usage

``` r
# S3 method for class 'decorated_ggplot'
ggplot_build(plot, ...)
```

## Arguments

- plot:

  class 'decorated_ggplot' from
  [`ggplot.decorated`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md)

- ...:

  ignored

## Value

see
[`ggplot_build`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)

## See also

Other decorated_ggplot:
[`data_context()`](https://bergsmat.github.io/yamlet/reference/data_context.md),
[`data_context.ggplot()`](https://bergsmat.github.io/yamlet/reference/data_context.ggplot.md),
[`ggplot.decorated()`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md),
[`mapframe()`](https://bergsmat.github.io/yamlet/reference/mapframe.md),
[`mapframe.default()`](https://bergsmat.github.io/yamlet/reference/mapframe.default.md),
[`mapframe.ggplot()`](https://bergsmat.github.io/yamlet/reference/mapframe.ggplot.md)
