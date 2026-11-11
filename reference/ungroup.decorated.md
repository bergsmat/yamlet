# Ungroup Decorated

Preserves class when ungrouping decorated.

## Usage

``` r
# S3 method for class 'decorated'
ungroup(x, ...)
```

## Arguments

- x:

  A [`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html)

- ...:

  In
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  variables or computations to group by. Computations are always done on
  the ungrouped data frame. To perform computations on the grouped data,
  you need to use a separate
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) step
  before the
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  Computations are not allowed in
  [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html). In
  [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
  variables to remove from the grouping.

## See also

Other dplyr:
[`anti_join.decorated()`](https://bergsmat.github.io/yamlet/reference/anti_join.decorated.md),
[`arrange.decorated()`](https://bergsmat.github.io/yamlet/reference/arrange.decorated.md),
[`full_join.decorated()`](https://bergsmat.github.io/yamlet/reference/full_join.decorated.md),
[`group_by.decorated()`](https://bergsmat.github.io/yamlet/reference/group_by.decorated.md),
[`inner_join.decorated()`](https://bergsmat.github.io/yamlet/reference/inner_join.decorated.md),
[`left_join.decorated()`](https://bergsmat.github.io/yamlet/reference/left_join.decorated.md),
[`mutate.decorated()`](https://bergsmat.github.io/yamlet/reference/mutate.decorated.md),
[`right_join.decorated()`](https://bergsmat.github.io/yamlet/reference/right_join.decorated.md),
[`select.decorated()`](https://bergsmat.github.io/yamlet/reference/select.decorated.md),
[`semi_join.decorated()`](https://bergsmat.github.io/yamlet/reference/semi_join.decorated.md),
[`slice.decorated()`](https://bergsmat.github.io/yamlet/reference/slice.decorated.md),
[`summarise.decorated()`](https://bergsmat.github.io/yamlet/reference/summarise.decorated.md),
[`summarize.decorated()`](https://bergsmat.github.io/yamlet/reference/summarize.decorated.md)
