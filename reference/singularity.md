# Choose Singular Expression

For a list of expressions evaluated within data, this returns the index
of the one expression that evaluates to an all-true vector (after
coercing NA to FALSE). Returns 0 if no expressions succeed, and
NA_integer\_ if more than one succeed. Returns -1 if any expression does
not evaluate to logical or if list is empty.

## Usage

``` r
singularity(x, data, ...)
```

## Arguments

- x:

  list of expressions

- data:

  something coercible to a data environment (typically data.frame)

- ...:

  ignored

## Value

integer, possibly NA

## See also

Other promote:
[`[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md),
[`filter.decorated()`](https://bergsmat.github.io/yamlet/reference/filter.decorated.md),
[`promote()`](https://bergsmat.github.io/yamlet/reference/promote.md),
[`promote.data.frame()`](https://bergsmat.github.io/yamlet/reference/promote.data.frame.md),
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md)

## Examples

``` r
meta <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- read.csv(meta)
singularity(
  data = x,
  list(
    "event == 'conc'",
    "event == 'dose'",
    "event == 'metabolite'"
  )
)
#> [1] 0
singularity(
  data = x[x$event == 'dose',],
  list(
    "event == 'conc'",
    "event == 'dose'",
    "event == 'metabolite'"
  )
)
#> [1] 2
singularity(
  data = x[x$event == 'dose',],
  list(
    "time >= 0",
    "event == 'dose'"
  )
)
#> [1] NA
```
