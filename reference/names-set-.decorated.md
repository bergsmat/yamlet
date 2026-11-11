# Assign Names of Decorated

Assigns names of decorated, retaining attributes.

## Usage

``` r
# S3 method for class 'decorated'
names(x) <- value
```

## Arguments

- x:

  decorated

- value:

  passed to next method

## Value

decorated

## See also

Other decorated:
[`[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md),
`[<-.decorated()`,
[`[[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.decorated.md),
`[[<-.decorated()`,
[`as_categorical()`](https://bergsmat.github.io/yamlet/reference/as_categorical.md),
[`as_categorical.decorated()`](https://bergsmat.github.io/yamlet/reference/as_categorical.decorated.md),
[`merge.decorated()`](https://bergsmat.github.io/yamlet/reference/merge.decorated.md)

## Examples

``` r
a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
a[[2]]
#> [1] "b"
names(a[[2]]) <- 'c'
class(a)
#> [1] "decorated" "list"     
```
