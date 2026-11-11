# Desolve Guide

Un-resolves explicit versions of 'guide' to implicit usage. Generic,
with methods
[`desolve.decorated`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`desolve.classified`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
and
[`desolve.dvec`](https://bergsmat.github.io/yamlet/reference/desolve.dvec.md).

## Usage

``` r
desolve(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

see methods

## See also

Other resolve:
[`desolve.classified()`](https://bergsmat.github.io/yamlet/reference/desolve.classified.md),
[`desolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/desolve.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`desolve.default()`](https://bergsmat.github.io/yamlet/reference/desolve.default.md),
[`desolve.dvec()`](https://bergsmat.github.io/yamlet/reference/desolve.dvec.md),
[`enscript.data.frame()`](https://bergsmat.github.io/yamlet/reference/enscript.data.frame.md),
[`enscript.default()`](https://bergsmat.github.io/yamlet/reference/enscript.default.md),
[`resolve()`](https://bergsmat.github.io/yamlet/reference/resolve.md),
[`resolve.classified()`](https://bergsmat.github.io/yamlet/reference/resolve.classified.md),
[`resolve.data.frame()`](https://bergsmat.github.io/yamlet/reference/resolve.data.frame.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`resolve.default()`](https://bergsmat.github.io/yamlet/reference/resolve.default.md),
[`resolve.dvec()`](https://bergsmat.github.io/yamlet/reference/resolve.dvec.md),
[`resolve.factor()`](https://bergsmat.github.io/yamlet/reference/resolve.factor.md)

## Examples

``` r
example(resolve.decorated)
#> 
#> rslv.d> # generate some decorated data
#> rslv.d> library(magrittr)
#> 
#> rslv.d> file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#> 
#> rslv.d> x <- decorate(file)
#> 
#> rslv.d> x %>% decorations(Age, glyco)
#> - Age
#>  - label: baseline subject age
#>  - guide: year
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - guide: mg/dL
#> 
#> rslv.d> # resolve everything, and show selected decorations
#> rslv.d> x %>% resolve %>% decorations(Age, glyco)
#> - Age
#>  - label: baseline subject age
#>  - units: year
#>  - title: baseline subject age (year)
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - units: mg/dL
#>  - title: alpha-1 acid glycoprotein concentration (mg/dL)
#> 
#> rslv.d> # resolve selectively, and show selected decorations
#> rslv.d> x %>% resolve(glyco) %>% decorations(Age, glyco)
#> - Age
#>  - label: baseline subject age
#>  - guide: year
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - units: mg/dL
#>  - title: alpha-1 acid glycoprotein concentration (mg/dL)
```
