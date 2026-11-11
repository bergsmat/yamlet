# Assign Subset of Decorated Vector

Assigns subset of decorated vector, retaining attributes.

## Usage

``` r
# S3 method for class 'dvec'
x[...] <- value
```

## Arguments

- x:

  decorated vector

- ...:

  passed to next method

## Value

dvec

## See also

Other dvec:
[`[.dvec()`](https://bergsmat.github.io/yamlet/reference/sub-.dvec.md),
[`[[.dvec()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.dvec.md),
`[[<-.dvec()`,
[`as.data.frame.dvec()`](https://bergsmat.github.io/yamlet/reference/as.data.frame.dvec.md),
[`as_dvec()`](https://bergsmat.github.io/yamlet/reference/as_dvec.md),
[`as_dvec.character()`](https://bergsmat.github.io/yamlet/reference/as_dvec.character.md),
[`as_dvec.complex()`](https://bergsmat.github.io/yamlet/reference/as_dvec.complex.md),
[`as_dvec.dvec()`](https://bergsmat.github.io/yamlet/reference/as_dvec.dvec.md),
[`as_dvec.integer()`](https://bergsmat.github.io/yamlet/reference/as_dvec.integer.md),
[`as_dvec.logical()`](https://bergsmat.github.io/yamlet/reference/as_dvec.logical.md),
[`as_dvec.numeric()`](https://bergsmat.github.io/yamlet/reference/as_dvec.numeric.md),
[`c.dvec()`](https://bergsmat.github.io/yamlet/reference/c.dvec.md),
[`classified.dvec()`](https://bergsmat.github.io/yamlet/reference/classified.dvec.md),
[`desolve.dvec()`](https://bergsmat.github.io/yamlet/reference/desolve.dvec.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`format.dvec()`](https://bergsmat.github.io/yamlet/reference/format.dvec.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md),
[`length.dvec()`](https://bergsmat.github.io/yamlet/reference/length.dvec.md),
[`print.dvec()`](https://bergsmat.github.io/yamlet/reference/print.dvec.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md),
[`unclassified.dvec()`](https://bergsmat.github.io/yamlet/reference/unclassified.dvec.md)

## Examples

``` r
a <- as_dvec(letters[1:3], label = 'foo')
a[2:3] <- 'a'
str(a)
#>  'dvec' chr [1:3] "a" "a" "a"
#>  - attr(*, "label")= chr "foo"
class(a)
#> [1] "dvec"
```
