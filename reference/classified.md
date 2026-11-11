# Classify Something

Classifies something. Generic, with method
[`classified.default`](https://bergsmat.github.io/yamlet/reference/classified.default.md)

## Usage

``` r
classified(x, ...)
```

## Arguments

- x:

  object of dispatch

- ...:

  passed arguments

## Value

see methods

## See also

Other classified:
[`[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-.classified.md),
`[<-.classified()`,
[`[[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.classified.md),
`[[<-.classified()`,
[`as.integer.classified()`](https://bergsmat.github.io/yamlet/reference/as.integer.classified.md),
[`c.classified()`](https://bergsmat.github.io/yamlet/reference/c.classified.md),
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
example(classified.default)
#> 
#> clssf.> # classified creates a factor with a corresponding codelist attribute
#> clssf.> classified(c('a','b','c'))
#> [1] a b c
#> attr(,"codelist")
#> attr(,"codelist")[[1]]
#> [1] a
#> 
#> attr(,"codelist")[[2]]
#> [1] b
#> 
#> attr(,"codelist")[[3]]
#> [1] c
#> 
#> Levels: a b c
#> 
#> clssf.> # codelist 'remembers' the origins of levels
#> clssf.> classified(c('a','b','c'), labels = c('A','B','C'))
#> [1] A B C
#> attr(,"codelist")
#> attr(,"codelist")$a
#> [1] A
#> 
#> attr(,"codelist")$b
#> [1] B
#> 
#> attr(,"codelist")$c
#> [1] C
#> 
#> Levels: A B C
#> 
#> clssf.> # classified is 'reversible'
#> clssf.> library(magrittr)
#> 
#> clssf.> c('a','b','c') %>%
#> clssf.+   classified(labels = c('A','B','C')) %>%
#> clssf.+   unclassified
#> [1] "a" "b" "c"
#> attr(,"codelist")
#> attr(,"codelist")$A
#> [1] "a"
#> 
#> attr(,"codelist")$B
#> [1] "b"
#> 
#> attr(,"codelist")$C
#> [1] "c"
#> 
```
