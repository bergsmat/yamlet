# Subset Classified

Subsets classified factor, retaining attributes.

## Usage

``` r
# S3 method for class 'classified'
x[...]
```

## Arguments

- x:

  classified factor

- ...:

  passed to next method

## Value

class 'classified' 'factor'

## See also

Other classified: `[<-.classified()`,
[`[[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.classified.md),
`[[<-.classified()`,
[`as.integer.classified()`](https://bergsmat.github.io/yamlet/reference/as.integer.classified.md),
[`c.classified()`](https://bergsmat.github.io/yamlet/reference/c.classified.md),
[`classified()`](https://bergsmat.github.io/yamlet/reference/classified.md),
[`classified.classified()`](https://bergsmat.github.io/yamlet/reference/classified.classified.md),
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`classified.default()`](https://bergsmat.github.io/yamlet/reference/classified.default.md),
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
[`classified.factor()`](https://bergsmat.github.io/yamlet/reference/classified.factor.md),
[`codify.default()`](https://bergsmat.github.io/yamlet/reference/codify.default.md),
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`unclassified()`](https://bergsmat.github.io/yamlet/reference/unclassified.md),
[`unclassified.classified()`](https://bergsmat.github.io/yamlet/reference/unclassified.classified.md),
[`unclassified.data.frame()`](https://bergsmat.github.io/yamlet/reference/unclassified.data.frame.md)

## Examples

``` r
a <- classified(letters[1:3])
attr(a, 'label') <- 'foo'
a <- a[1:3]
attributes(a)
#> $levels
#> [1] "a" "b" "c"
#> 
#> $class
#> [1] "classified" "factor"    
#> 
#> $codelist
#> $codelist[[1]]
#> [1] "a"
#> 
#> $codelist[[2]]
#> [1] "b"
#> 
#> $codelist[[3]]
#> [1] "c"
#> 
#> 
#> $label
#> [1] "foo"
#> 
```
