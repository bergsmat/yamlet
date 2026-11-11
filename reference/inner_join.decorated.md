# Inner_join Decorated

Preserves class when joining decorated.

## Usage

``` r
# S3 method for class 'decorated'
inner_join(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...)
```

## Arguments

- x, y:

  A pair of data frames, data frame extensions (e.g. a tibble), or lazy
  data frames (e.g. from dbplyr or dtplyr). See *Methods*, below, for
  more details.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html), or
  a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.html) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- ...:

  Other parameters passed onto methods.

## See also

Other dplyr:
[`anti_join.decorated()`](https://bergsmat.github.io/yamlet/reference/anti_join.decorated.md),
[`arrange.decorated()`](https://bergsmat.github.io/yamlet/reference/arrange.decorated.md),
[`full_join.decorated()`](https://bergsmat.github.io/yamlet/reference/full_join.decorated.md),
[`group_by.decorated()`](https://bergsmat.github.io/yamlet/reference/group_by.decorated.md),
[`left_join.decorated()`](https://bergsmat.github.io/yamlet/reference/left_join.decorated.md),
[`mutate.decorated()`](https://bergsmat.github.io/yamlet/reference/mutate.decorated.md),
[`right_join.decorated()`](https://bergsmat.github.io/yamlet/reference/right_join.decorated.md),
[`select.decorated()`](https://bergsmat.github.io/yamlet/reference/select.decorated.md),
[`semi_join.decorated()`](https://bergsmat.github.io/yamlet/reference/semi_join.decorated.md),
[`slice.decorated()`](https://bergsmat.github.io/yamlet/reference/slice.decorated.md),
[`summarise.decorated()`](https://bergsmat.github.io/yamlet/reference/summarise.decorated.md),
[`summarize.decorated()`](https://bergsmat.github.io/yamlet/reference/summarize.decorated.md),
[`ungroup.decorated()`](https://bergsmat.github.io/yamlet/reference/ungroup.decorated.md)
