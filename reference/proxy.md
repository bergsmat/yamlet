# Calculate Substitute Values

Calculates substitute values. Generic, with method
[`proxy.factor`](https://bergsmat.github.io/yamlet/reference/proxy.factor.md)

## Usage

``` r
proxy(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

see methods

## See also

Other proxy:
[`proxy.factor()`](https://bergsmat.github.io/yamlet/reference/proxy.factor.md)

## Examples

``` r
example(proxy.factor)
#> 
#> prxy.f> proxy(factor(1:3), letters[1:3])
#> [1] "a" "b" "c"
#> 
#> prxy.f> proxy(factor(1:3), factor(letters[1:3]))
#> [1] a b c
#> Levels: a b c
#> 
#> prxy.f> proxy(factor(letters[4:6]))
#> [1] 1 2 3
#> 
#> prxy.f> foo <- classified(letters[1:5])
#> 
#> prxy.f> as_yamlet(attributes(foo))
#> - levels: a, b, c, d, e
#> - class: classified, factor
#> - codelist: a, b, c, d, e
#> 
#> prxy.f> foo <- classified(foo, labels = proxy(foo))
#> 
#> prxy.f> as_yamlet(attributes(foo))
#> - levels: 1, 2, 3, 4, 5
#> - class: classified, factor
#> - codelist: 1, 2, 3, 4, 5
```
