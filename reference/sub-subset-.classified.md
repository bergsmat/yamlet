# Assign Element of Classified

Assigns element of classified factor, retaining attributes.

## Usage

``` r
# S3 method for class 'classified'
x[[...]] <- value
```

## Arguments

- x:

  classified factor

- ...:

  passed to next method

## Value

class 'classified' 'factor'

## See also

Other classified:
[`[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-.classified.md),
`[<-.classified()`,
[`[[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.classified.md),
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
a[[3]] <- 'a'
str(a)
#>  Factor w/ 3 levels "a","b","c": 1 2 1
#>  - attr(*, "codelist")=List of 3
#>   ..$ : chr "a"
#>   ..$ : chr "b"
#>   ..$ : chr "c"
class(a)
#> [1] "classified" "factor"    
```
