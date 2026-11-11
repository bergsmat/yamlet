# Slice Decorated

Preserves class when slicing decorated.

## Usage

``` r
# S3 method for class 'decorated'
slice(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  For [`slice()`](https://dplyr.tidyverse.org/reference/slice.html):
  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Integer row values.

  Provide either positive values to keep, or negative values to drop.
  The values provided must be either all positive or all negative.
  Indices beyond the number of rows in the input are silently ignored.

  For `slice_*()`, these arguments are passed on to methods.

- .preserve:

  Relevant when the `.data` input is grouped. If `.preserve = FALSE`
  (the default), the grouping structure is recalculated based on the
  resulting data, otherwise the grouping is kept as is.

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
[`summarise.decorated()`](https://bergsmat.github.io/yamlet/reference/summarise.decorated.md),
[`summarize.decorated()`](https://bergsmat.github.io/yamlet/reference/summarize.decorated.md),
[`ungroup.decorated()`](https://bergsmat.github.io/yamlet/reference/ungroup.decorated.md)
