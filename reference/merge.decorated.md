# Merge Decorated

Preserves class for 'decorated' during merge().

## Usage

``` r
# S3 method for class 'decorated'
merge(x, y, ...)
```

## Arguments

- x:

  decorated

- y:

  passed to [`merge`](https://rdrr.io/r/base/merge.html)

- ...:

  passed to [`merge`](https://rdrr.io/r/base/merge.html)

## Value

class 'decorated' 'data.frame'

## See also

Other decorated:
[`[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md),
`[<-.decorated()`,
[`[[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.decorated.md),
`[[<-.decorated()`,
[`as_categorical()`](https://bergsmat.github.io/yamlet/reference/as_categorical.md),
[`as_categorical.decorated()`](https://bergsmat.github.io/yamlet/reference/as_categorical.decorated.md),
`names<-.decorated()`

## Examples

``` r
library(magrittr)
library(dplyr)
x <- data.frame(foo = 1, bar = 2)
x %<>% decorate('foo: [distance, mm]')
x %<>% decorate('bar: [height, mm]')
class(merge(x,x))
#> [1] "decorated"  "data.frame"
```
