# Assign Subset of Decorated

Assigns subset of decorated, retaining attributes.

## Usage

``` r
# S3 method for class 'decorated'
x[...] <- value
```

## Arguments

- x:

  decorated

- ...:

  passed to next method

## Value

decorated

## See also

Other decorated:
[`[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md),
[`[[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.decorated.md),
`[[<-.decorated()`,
[`as_categorical()`](https://bergsmat.github.io/yamlet/reference/as_categorical.md),
[`as_categorical.decorated()`](https://bergsmat.github.io/yamlet/reference/as_categorical.decorated.md),
[`merge.decorated()`](https://bergsmat.github.io/yamlet/reference/merge.decorated.md),
`names<-.decorated()`

## Examples

``` r
a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
a[2:3] <- 'a'
str(a)
#> List of 3
#>  $ A: chr "a"
#>  $ B: chr "a"
#>  $ C: chr "a"
#>  - attr(*, "class")= chr [1:2] "decorated" "list"
class(a)
#> [1] "decorated" "list"     
```
