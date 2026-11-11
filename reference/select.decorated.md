# Select Decorated

Preserves class when selecting decorated.

## Usage

``` r
# S3 method for class 'decorated'
select(.data, ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

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
[`semi_join.decorated()`](https://bergsmat.github.io/yamlet/reference/semi_join.decorated.md),
[`slice.decorated()`](https://bergsmat.github.io/yamlet/reference/slice.decorated.md),
[`summarise.decorated()`](https://bergsmat.github.io/yamlet/reference/summarise.decorated.md),
[`summarize.decorated()`](https://bergsmat.github.io/yamlet/reference/summarize.decorated.md),
[`ungroup.decorated()`](https://bergsmat.github.io/yamlet/reference/ungroup.decorated.md)
