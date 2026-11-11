# Combine Decorated Vector

Combines decorated vectors. Tries to preserve attributes by resolving
pairwise conflicts intelligently. The attributes of the first item are
reconciled with those of the second, then those of the third, etc. the
`class` attribute is untouched.

## Usage

``` r
# S3 method for class 'dvec'
c(...)
```

## Arguments

- ...:

  items to be combined, presumably all vectors

## Value

dvec

## Details

By default, the first version of any attribute is preserved, with
warning if the alternative differs. NULLs are largely ignored. If either
attribute is a list the other is coerced to a list with as.list(). If
either of two lists has names, then names are enforced for the other
(blank names if necessary). Lists are combined in forward order, and
elements that are duplicates in value and name (if present) are removed.
If the result has only blank names, these are removed as well.

## See also

Other dvec:
[`[.dvec()`](https://bergsmat.github.io/yamlet/reference/sub-.dvec.md),
`[<-.dvec()`,
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
a <- as_dvec(letters[1:3], label = 'foo', priority = 1)
b <- as_dvec(letters[3:5], label = 'foo', case = 'lower')
c <- c(a,b)
c
#> [1] "a" "b" "c" "c" "d" "e"
#> attr(,"label")
#> [1] "foo"
#> attr(,"priority")
#> [1] 1
#> attr(,"case")
#> [1] "lower"
class(c)
#> [1] "dvec"
```
