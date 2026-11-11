# Retrieve Decorations for Data Frame

Retrieve the decorations of a data.frame; i.e., the metadata used to
decorate it. Returns a list with same names as the data.frame. By
default, 'class' and 'level' attributes are excluded from the result, as
you likely don't want to manipulate these independently.

## Usage

``` r
# S3 method for class 'data.frame'
decorations(
  x,
  ...,
  exclude_attr = getOption("yamlet_exclude_attr", c("class", "levels"))
)
```

## Arguments

- x:

  data.frame

- ...:

  optional unquoted column names to limit output (passed to
  [`select`](https://dplyr.tidyverse.org/reference/select.html))

- exclude_attr:

  attributes to remove from the result

## Value

named list of class 'yamlet'

## See also

Other decorate:
[`as_decorated()`](https://bergsmat.github.io/yamlet/reference/as_decorated.md),
[`as_decorated.default()`](https://bergsmat.github.io/yamlet/reference/as_decorated.default.md),
[`decorate()`](https://bergsmat.github.io/yamlet/reference/decorate.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
[`decorate.list()`](https://bergsmat.github.io/yamlet/reference/decorate.list.md),
[`decorate_groups()`](https://bergsmat.github.io/yamlet/reference/decorate_groups.md),
[`decorate_groups.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate_groups.data.frame.md),
[`decorations()`](https://bergsmat.github.io/yamlet/reference/decorations.md),
[`decorations_groups()`](https://bergsmat.github.io/yamlet/reference/decorations_groups.md),
[`decorations_groups.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorations_groups.data.frame.md),
[`group_by_decorations()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.md),
[`group_by_decorations.data.frame()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.data.frame.md),
[`redecorate()`](https://bergsmat.github.io/yamlet/reference/redecorate.md),
[`undecorate()`](https://bergsmat.github.io/yamlet/reference/undecorate.md),
[`undecorate.default()`](https://bergsmat.github.io/yamlet/reference/undecorate.default.md)

## Examples

``` r
# prepare a decorated data.frame
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)

# retrieve the decorations
decorations(x, Subject, time, conc)
#> - Subject
#>  - label: patient identifier
#> - time
#>  - label: time since start of study
#>  - guide: h
#> - conc
#>  - label: quinidine serum concentration
#>  - guide: mg/L
```
