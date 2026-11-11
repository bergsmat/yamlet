# Coerce Decorated Vector to Units

Coerces dvec to units. If x has a units attribute, it is used to create
class 'units'. It is an error if x has no units attribute.

## Usage

``` r
# S3 method for class 'dvec'
as_units(x, ..., preserve = getOption("yamlet_as_units_preserve", "label"))
```

## Arguments

- x:

  dvec

- ...:

  ignored

- preserve:

  attributes to preserve; just label by default (class and units are
  handled implicitly)

## Examples

``` r
library(magrittr)
a <- data.frame(id = 1:4, wt = c(70, 80, 70, 80), sex = c(0,1,0,1))
a %<>% decorate('wt: [ body weight, kg ]')
a %<>% decorate('sex: [ sex, [ female: 0, male: 1]]')
a %<>% decorate('id: identifier')
a %<>% resolve
a$wt %>% as_units
#> Units: [kg]
#> [1] 70 80 70 80
#> attr(,"label")
#> [1] "body weight"
```
