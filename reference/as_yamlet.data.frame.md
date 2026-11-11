# Coerce Data Frame to Yamlet

Coerces data.frame to yamlet by calling
[`decorations`](https://bergsmat.github.io/yamlet/reference/decorations.md).

## Usage

``` r
# S3 method for class 'data.frame'
as_yamlet(x, ...)
```

## Arguments

- x:

  data.frame

- ...:

  passed to
  [`decorations`](https://bergsmat.github.io/yamlet/reference/decorations.md)

## Value

yamlet

## See also

Other yamlet:
[`[.yamlet()`](https://bergsmat.github.io/yamlet/reference/sub-.yamlet.md),
[`as.character.yamlet()`](https://bergsmat.github.io/yamlet/reference/as.character.yamlet.md),
[`as_yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
[`as_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md),
[`as_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.list.md),
[`as_yamlet.yam()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yam.md),
[`as_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yamlet.md),
[`print.yamlet()`](https://bergsmat.github.io/yamlet/reference/print.yamlet.md)

## Examples

``` r
library(csv)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(as.csv(file))
as_yamlet(x)
#> - Subject: 
#> - time: 
#> - conc: 
#> - dose: 
#> - interval: 
#> - Age: 
#> - Height: 
#> - Weight: 
#> - Race: 
#> - Smoke: 
#> - Ethanol: 
#> - Heart: 
#> - Creatinine: 
#> - glyco: 
```
