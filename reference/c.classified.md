# Combine Classified

Combines classified factor, retaining attributes. Attributes other than
levels and codelist are taken from the first argument if
`reconcile_attributes` is FALSE; otherwise remaining attributes are
reconciled (default as of 1.3.1). Attribute 'levels' is supplied by next
method. Attribute 'codelist' is the combined codelists in sequence of
all (dots) arguments, after silently removing exact duplicates. If any
names are duplicated, the entire codelist is replaced by the effective
levels, with optional warning.

## Usage

``` r
# S3 method for class 'classified'
c(
  ...,
  recursive = TRUE,
  warn_conflicted = getOption("yamlet_warn_conflicted", FALSE),
  reconcile_attributes = getOption("yamlet_reconcile_attributes", TRUE)
)
```

## Arguments

- ...:

  passed to next method

- recursive:

  passed to unlist() internally

- warn_conflicted:

  logical: warn if any duplicated codelist names?

- reconcile_attributes:

  logical: reconcile attribute lists (other than levels and attributes)
  if TRUE (default as of 1.3.1); else take attributes from first
  argument (former behavior)

## Value

class 'classified' 'factor'

## See also

Other classified:
[`[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-.classified.md),
`[<-.classified()`,
[`[[.classified()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.classified.md),
`[[<-.classified()`,
[`as.integer.classified()`](https://bergsmat.github.io/yamlet/reference/as.integer.classified.md),
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
b <- classified(letters[3:5])
c <- c(a,b)
c
#> [1] a b c c d e
#> attr(,"codelist")
#> [1] a b c d e
#> Levels: a b c d e
class(c)
#> [1] "factor"
```
