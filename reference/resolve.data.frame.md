# Resolve Data Frame

Resolves data.frame. Coerces first using as_decorated().

## Usage

``` r
# S3 method for class 'data.frame'
resolve(x, ...)
```

## Arguments

- x:

  data.frame

- ...:

  ignored

## Value

decorated

## See also

Other resolve:
[`desolve()`](https://bergsmat.github.io/yamlet/reference/desolve.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`desolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/desolve.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`desolve.default()`](https://bergsmat.github.io/yamlet/reference/desolve.default.md),
[`desolve.dvec()`](https://bergsmat.github.io/yamlet/reference/desolve.dvec.md),
[`enscript.data.frame()`](https://bergsmat.github.io/yamlet/reference/enscript.data.frame.md),
[`enscript.default()`](https://bergsmat.github.io/yamlet/reference/enscript.default.md),
[`resolve()`](https://bergsmat.github.io/yamlet/reference/resolve.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`resolve.default()`](https://bergsmat.github.io/yamlet/reference/resolve.default.md),
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md)

## Examples

``` r
head(resolve(Theoph))
#>   Subject   Wt Dose Time  conc
#> 1       1 79.6 4.02 0.00  0.74
#> 2       1 79.6 4.02 0.25  2.84
#> 3       1 79.6 4.02 0.57  6.57
#> 4       1 79.6 4.02 1.12 10.50
#> 5       1 79.6 4.02 2.02  9.66
#> 6       1 79.6 4.02 3.82  8.58
```
