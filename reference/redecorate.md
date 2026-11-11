# Redecorate a List-like Object

Redecorates a list-like object. Equivalent to
`decorate( ..., overwrite = TRUE)`. If `meta` is not supplied, an
attempt will be made to redecorate with existing decorations, if any.

## Usage

``` r
redecorate(x, meta = NULL, ..., overwrite = TRUE)
```

## Arguments

- x:

  object

- meta:

  file path for corresponding yamlet metadata, or a yamlet object

- ...:

  passed arguments

- overwrite:

  passed to
  [`decorate`](https://bergsmat.github.io/yamlet/reference/decorate.md)

## Value

a list-like object, typically data.frame

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
[`group_by_decorations.data.frame()`](https://bergsmat.github.io/yamlet/reference/group_by_decorations.data.frame.md),
[`undecorate()`](https://bergsmat.github.io/yamlet/reference/undecorate.md),
[`undecorate.default()`](https://bergsmat.github.io/yamlet/reference/undecorate.default.md)

## Examples

``` r
library(dplyr)
library(magrittr)
library(csv)
library(haven)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(as.csv(file))
x %>% select(Subject) %>% decorations
#> - Subject: 
x %<>% redecorate('Subject: Patient Identifier')
x %>% select(Subject) %>% decorations
#> - Subject
#>  - label: Patient Identifier

# xpt may already have labels:

dm <- 'extdata/dm.xpt.gz' %>% 
  system.file(package = 'yamlet') %>% 
  gzfile %>% 
  read_xpt

dm %>% class  
#> [1] "tbl_df"     "tbl"        "data.frame"
dm %>% decorations(AGE, SEX, RACE)
#> - AGE
#>  - label: Age
#> - SEX
#>  - label: Sex
#> - RACE
#>  - label: Race

# but technically not decorated, and poor persistence:
bind_rows(dm, dm) %>% decorations(AGE, SEX, RACE)
#> - AGE: 
#> - SEX: 
#> - RACE: 

# self-redecorating helps:
dm %<>% redecorate
bind_rows(dm, dm) %>% decorations(AGE, SEX, RACE)
#> - AGE
#>  - label: Age
#> - SEX
#>  - label: Sex
#> - RACE
#>  - label: Race
```
