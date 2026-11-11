# Test List is Conditional

Tests whether a list is conditional. Evaluates names of x on data and
looks for meaningful result. Returns TRUE if list has names and all
evaluate to logicals with length equal to number of rows in data.

## Usage

``` r
# S3 method for class 'list'
isConditional(x, data, ...)
```

## Arguments

- x:

  list

- data:

  environment for variable lookup

- ...:

  passed arguments

## Value

length-one logical

## See also

Other conditional:
[`isConditional()`](https://bergsmat.github.io/yamlet/reference/isConditional.md),
[`isConditional.default()`](https://bergsmat.github.io/yamlet/reference/isConditional.default.md)
