# Test Character Value is Levels

Tests whether character value is levels. Looks for any matches to
vector. Uses
[`intersect`](https://generics.r-lib.org/reference/setops.html), which
is fairly flexible respecting underlying data types (character 0 can
match integer 0, etc.).

## Usage

``` r
# S3 method for class 'character'
isLevels(x, table, ...)
```

## Arguments

- x:

  default

- table:

  lookup vector

- ...:

  passed arguments

## Value

logical

## See also

Other levels:
[`isLevels()`](https://bergsmat.github.io/yamlet/reference/isLevels.md),
[`isLevels.default()`](https://bergsmat.github.io/yamlet/reference/isLevels.default.md)
