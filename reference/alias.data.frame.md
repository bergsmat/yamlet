# Alias a Data Frame

Aliases a data.frame. Replaces column names with labels, where present.
Stores column name as 'alias' attribute.

## Usage

``` r
# S3 method for class 'data.frame'
alias(object, ...)
```

## Arguments

- object:

  data.frame

- ...:

  optional unquoted names of target columns

## Value

aliased data.frame

## See also

Other labels:
[`append_units()`](https://bergsmat.github.io/yamlet/reference/append_units.md),
[`append_units.data.frame()`](https://bergsmat.github.io/yamlet/reference/append_units.data.frame.md),
[`append_units.default()`](https://bergsmat.github.io/yamlet/reference/append_units.default.md),
[`drop_title()`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
[`drop_title.decorated()`](https://bergsmat.github.io/yamlet/reference/drop_title.decorated.md),
[`drop_title.default()`](https://bergsmat.github.io/yamlet/reference/drop_title.default.md),
[`drop_title.dvec()`](https://bergsmat.github.io/yamlet/reference/drop_title.dvec.md),
[`make_title()`](https://bergsmat.github.io/yamlet/reference/make_title.md),
[`make_title.decorated()`](https://bergsmat.github.io/yamlet/reference/make_title.decorated.md),
[`make_title.default()`](https://bergsmat.github.io/yamlet/reference/make_title.default.md),
[`make_title.dvec()`](https://bergsmat.github.io/yamlet/reference/make_title.dvec.md),
[`sub_units()`](https://bergsmat.github.io/yamlet/reference/sub_units.md)

Other deprecated:
[`append_units()`](https://bergsmat.github.io/yamlet/reference/append_units.md),
[`drop_title()`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
[`make_title()`](https://bergsmat.github.io/yamlet/reference/make_title.md)

## Examples

``` r
library(magrittr)
d <- data.frame(x = 1:10, y = 1:10, z = 1:10)
d %<>% modify(x, label = 'Independent Value')
d %<>% modify(y, label = 'Dependent Value')
d
#>     x  y  z
#> 1   1  1  1
#> 2   2  2  2
#> 3   3  3  3
#> 4   4  4  4
#> 5   5  5  5
#> 6   6  6  6
#> 7   7  7  7
#> 8   8  8  8
#> 9   9  9  9
#> 10 10 10 10
alias(d)
#>    Independent Value Dependent Value  z
#> 1                  1               1  1
#> 2                  2               2  2
#> 3                  3               3  3
#> 4                  4               4  4
#> 5                  5               5  5
#> 6                  6               6  6
#> 7                  7               7  7
#> 8                  8               8  8
#> 9                  9               9  9
#> 10                10              10 10
alias(d, y)
#>     x Dependent Value  z
#> 1   1               1  1
#> 2   2               2  2
#> 3   3               3  3
#> 4   4               4  4
#> 5   5               5  5
#> 6   6               6  6
#> 7   7               7  7
#> 8   8               8  8
#> 9   9               9  9
#> 10 10              10 10
```
