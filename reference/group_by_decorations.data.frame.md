# Groups by Decorations for Data Frame

Invokes
[`group_by`](https://dplyr.tidyverse.org/reference/group_by.html) using
whatever groups are recovered by
[`decorations_groups`](https://bergsmat.github.io/yamlet/reference/decorations_groups.md).

## Usage

``` r
# S3 method for class 'data.frame'
group_by_decorations(x, ...)
```

## Arguments

- x:

  grouped_df

- ...:

  ignored

## Value

list of symbols

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
[`decorations.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorations.data.frame.md),
[`decorations_groups()`](https://bergsmat.github.io/yamlet/reference/decorations_groups.md),
[`decorations_groups.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorations_groups.data.frame.md),
[`group_by_decorations()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.md),
[`redecorate()`](https://bergsmat.github.io/yamlet/reference/redecorate.md),
[`undecorate()`](https://bergsmat.github.io/yamlet/reference/undecorate.md),
[`undecorate.default()`](https://bergsmat.github.io/yamlet/reference/undecorate.default.md)

## Examples

``` r
library(magrittr)
library(dplyr)
Theoph %>% group_vars # nothing!
#> character(0)
Theoph %<>% decorate_groups(Subject, Time) 
Theoph %<>% group_by_decorations
Theoph %>% group_vars # something
#> [1] "Subject" "Time"   
rm(Theoph)
```
