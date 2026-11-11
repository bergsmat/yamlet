# Recover Groups Decorations for Data Frame

Recovers groups decorations for class 'data.frame'. Seeks a
sequentially-valued integer attribute with name 'groups' for each
column, sorts these, and returns a character vector like
`group_vars(x)`.

## Usage

``` r
# S3 method for class 'data.frame'
decorations_groups(x, ...)
```

## Arguments

- x:

  data.frame

- ...:

  ignored

## Value

character: names of groups columns

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
[`group_by_decorations()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.md),
[`group_by_decorations.data.frame()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.data.frame.md),
[`redecorate()`](https://bergsmat.github.io/yamlet/reference/redecorate.md),
[`undecorate()`](https://bergsmat.github.io/yamlet/reference/undecorate.md),
[`undecorate.default()`](https://bergsmat.github.io/yamlet/reference/undecorate.default.md)

## Examples

``` r
library(magrittr)
library(dplyr)
Theoph %<>% group_by(Subject, Time)
Theoph %>% group_vars
#> [1] "Subject" "Time"   
Theoph %>% decorations_groups # nothing!
#> character(0)
Theoph %<>% decorate_groups
Theoph %>% decorations_groups # something!
#> [1] "Subject" "Time"   
Theoph %<>% ungroup
Theoph %>% group_vars # gone!
#> character(0)
Theoph %<>% group_by(across(all_of(decorations_groups(.))))
Theoph %>% group_vars # recovered!
#> [1] "Subject" "Time"   
Theoph %<>% group_by_decorations
Theoph %>% group_vars # same
#> [1] "Subject" "Time"   
rm(Theoph)
```
