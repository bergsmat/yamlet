# Group_by Decorated

Preserves class when grouping decorated.

## Usage

``` r
# S3 method for class 'decorated'
group_by(.data, ..., add = FALSE, .drop = group_by_drop_default(.data))
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

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

- add:

  When `FALSE`, the default,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  will override existing groups. To add to the existing groups, use
  `.add = TRUE`.

  This argument was previously called `add`, but that prevented creating
  a new grouping variable called `add`, and conflicts with our naming
  conventions.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`. See
  [`group_by_drop_default()`](https://dplyr.tidyverse.org/reference/group_by_drop_default.html)
  for details.

## See also

Other dplyr:
[`anti_join.decorated()`](https://bergsmat.github.io/yamlet/reference/anti_join.decorated.md),
[`arrange.decorated()`](https://bergsmat.github.io/yamlet/reference/arrange.decorated.md),
[`full_join.decorated()`](https://bergsmat.github.io/yamlet/reference/full_join.decorated.md),
[`inner_join.decorated()`](https://bergsmat.github.io/yamlet/reference/inner_join.decorated.md),
[`left_join.decorated()`](https://bergsmat.github.io/yamlet/reference/left_join.decorated.md),
[`mutate.decorated()`](https://bergsmat.github.io/yamlet/reference/mutate.decorated.md),
[`right_join.decorated()`](https://bergsmat.github.io/yamlet/reference/right_join.decorated.md),
[`select.decorated()`](https://bergsmat.github.io/yamlet/reference/select.decorated.md),
[`semi_join.decorated()`](https://bergsmat.github.io/yamlet/reference/semi_join.decorated.md),
[`slice.decorated()`](https://bergsmat.github.io/yamlet/reference/slice.decorated.md),
[`summarise.decorated()`](https://bergsmat.github.io/yamlet/reference/summarise.decorated.md),
[`summarize.decorated()`](https://bergsmat.github.io/yamlet/reference/summarize.decorated.md),
[`ungroup.decorated()`](https://bergsmat.github.io/yamlet/reference/ungroup.decorated.md)
