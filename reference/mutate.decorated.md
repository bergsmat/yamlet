# Mutate Decorated

Preserves class when mutating decorated.

## Usage

``` r
# S3 method for class 'decorated'
mutate(.data, ...)
```

## Arguments

- .data:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs. The name gives the name of the column in the output.

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

  - `NULL`, to remove the column.

  - A data frame or tibble, to create multiple columns in the output.

## See also

Other dplyr:
[`anti_join.decorated()`](https://bergsmat.github.io/yamlet/reference/anti_join.decorated.md),
[`arrange.decorated()`](https://bergsmat.github.io/yamlet/reference/arrange.decorated.md),
[`full_join.decorated()`](https://bergsmat.github.io/yamlet/reference/full_join.decorated.md),
[`group_by.decorated()`](https://bergsmat.github.io/yamlet/reference/group_by.decorated.md),
[`inner_join.decorated()`](https://bergsmat.github.io/yamlet/reference/inner_join.decorated.md),
[`left_join.decorated()`](https://bergsmat.github.io/yamlet/reference/left_join.decorated.md),
[`right_join.decorated()`](https://bergsmat.github.io/yamlet/reference/right_join.decorated.md),
[`select.decorated()`](https://bergsmat.github.io/yamlet/reference/select.decorated.md),
[`semi_join.decorated()`](https://bergsmat.github.io/yamlet/reference/semi_join.decorated.md),
[`slice.decorated()`](https://bergsmat.github.io/yamlet/reference/slice.decorated.md),
[`summarise.decorated()`](https://bergsmat.github.io/yamlet/reference/summarise.decorated.md),
[`summarize.decorated()`](https://bergsmat.github.io/yamlet/reference/summarize.decorated.md),
[`ungroup.decorated()`](https://bergsmat.github.io/yamlet/reference/ungroup.decorated.md)
