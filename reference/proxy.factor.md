# Calculate Substitute Values for Factor Levels

Calculates substitute values for factor levels. If x and y have same
length and there is a one-to-one correspondence of their elements, then
unique elements of y are returned in an order corresponding to
levels(x).

## Usage

``` r
# S3 method for class 'factor'
proxy(x, y = as.numeric(x), ...)
```

## Arguments

- x:

  factor

- y:

  factor or vector

- ...:

  ignored

## Value

same class as y

## See also

Other proxy:
[`proxy()`](https://bergsmat.github.io/yamlet/reference/proxy.md)

## Examples

``` r
proxy(factor(1:3), letters[1:3])
#> [1] "a" "b" "c"
proxy(factor(1:3), factor(letters[1:3]))
#> [1] a b c
#> Levels: a b c
proxy(factor(letters[4:6]))
#> [1] 1 2 3
foo <- classified(letters[1:5])
as_yamlet(attributes(foo))
#> - levels: a, b, c, d, e
#> - class: classified, factor
#> - codelist: a, b, c, d, e
foo <- classified(foo, labels = proxy(foo))
as_yamlet(attributes(foo))
#> - levels: 1, 2, 3, 4, 5
#> - class: classified, factor
#> - codelist: 1, 2, 3, 4, 5
```
