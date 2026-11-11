# Capture Groups as Decorations for Data Frame

Captures groups as decorations for class 'data.frame'. Creates a
sequentially-valued integer attribute with name 'groups' for each
corresponding column (after clearing all such existing designations). It
is an error if not all such columns are present. Defaults to
`groups(x)`. If no columns are specified and x has no groups, x is
returned with any existing column-level 'groups' attributes removed.

## Usage

``` r
# S3 method for class 'data.frame'
decorate_groups(x, ...)
```

## Arguments

- x:

  data.frame

- ...:

  unquoted names of columns to assign as groups; defaults to `groups(x)`

## Value

same class as x

## See also

Other decorate:
[`as_decorated()`](https://bergsmat.github.io/yamlet/reference/as_decorated.md),
[`as_decorated.default()`](https://bergsmat.github.io/yamlet/reference/as_decorated.default.md),
[`decorate()`](https://bergsmat.github.io/yamlet/reference/decorate.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
[`decorate.list()`](https://bergsmat.github.io/yamlet/reference/decorate.list.md),
[`decorate_groups()`](https://bergsmat.github.io/yamlet/reference/decorate_groups.md),
[`decorations()`](https://bergsmat.github.io/yamlet/reference/decorations.md),
[`decorations.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorations.data.frame.md),
[`decorations_groups()`](https://bergsmat.github.io/yamlet/reference/decorations_groups.md),
[`decorations_groups.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorations_groups.data.frame.md),
[`group_by_decorations()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.md),
[`group_by_decorations.data.frame()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.data.frame.md),
[`redecorate()`](https://bergsmat.github.io/yamlet/reference/redecorate.md),
[`undecorate()`](https://bergsmat.github.io/yamlet/reference/undecorate.md),
[`undecorate.default()`](https://bergsmat.github.io/yamlet/reference/undecorate.default.md)

## Examples

``` r
library(magrittr)
library(dplyr)
Theoph %>% decorate_groups(Subject, Time) %>% groups # nothing!
#> list()
Theoph %>% decorate_groups(Subject, Time) %>% decorations # note well
#> - Subject
#>  - groups: 1
#> - Wt: 
#> - Dose: 
#> - Time
#>  - groups: 2
#> - conc: 
Theoph %>% group_by(Subject, Time) %>% decorate_groups %>% decorations # same
#> - Subject
#>  - groups: 1
#> - Wt: 
#> - Dose: 
#> - Time
#>  - groups: 2
#> - conc: 
```
