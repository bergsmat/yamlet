# Sort Yamlet

Enforces canonical attribute order for class 'yamlet'. Set of
default_keys will be augmented with all observed attribute names and
will be expanded or reduced as necessary for each data item.

## Usage

``` r
# S3 method for class 'yamlet'
canonical(
  x,
  default_keys = getOption("yamlet_default_keys", list("label", "guide")),
  ...
)
```

## Arguments

- x:

  yamlet

- default_keys:

  attribute names in preferred order

- ...:

  ignored

## Value

decorated

## See also

Other canonical:
[`canonical()`](https://bergsmat.github.io/yamlet/reference/canonical.md),
[`canonical.decorated()`](https://bergsmat.github.io/yamlet/reference/canonical.decorated.md)

## Examples

``` r
library(magrittr)
x <- data.frame(x = 1, y = 1, z = factor('a'))
x %<>% decorate('
x: [ guide: mm, desc: this, label: foo ]
"y": [ guide: bar, desc: other ]
')

decorations(x)
#> - x
#>  - guide: mm
#>  - desc: this
#>  - label: foo
#> - y
#>  - guide: bar
#>  - desc: other
#> - z: []
decorations(canonical(x))
#> - x
#>  - label: foo
#>  - guide: mm
#>  - desc: this
#> - y
#>  - guide: bar
#>  - desc: other
#> - z: []
canonical(decorations(x))
#> - x
#>  - label: foo
#>  - guide: mm
#>  - desc: this
#> - y
#>  - guide: bar
#>  - desc: other
#> - z: []
write_yamlet(x)
#> x: [ foo, mm, desc: this ]
#> 'y': [ guide: bar, desc: other ]
#> z: 
```
