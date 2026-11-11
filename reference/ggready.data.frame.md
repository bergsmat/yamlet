# Prepare Data Frame for GGplot

Prepares data.frame for ggplot. Appends units to label using
[`append_units`](https://bergsmat.github.io/yamlet/reference/append_units.md)
(passing `style = 'plotmath'` if `parse` is true, else
`style = 'plain'`). Enforces classes 'decorated' and 'ggready'.

## Usage

``` r
# S3 method for class 'data.frame'
ggready(x, ..., parse = getOption("yamlet_ggready_parse", TRUE))
```

## Arguments

- x:

  object

- ...:

  passed to
  [`append_units`](https://bergsmat.github.io/yamlet/reference/append_units.md);
  may include unquoted column names

- parse:

  passed to
  [`append_units`](https://bergsmat.github.io/yamlet/reference/append_units.md)

## Value

ggready decorated

## See also

Other ggready:
[`ggready()`](https://bergsmat.github.io/yamlet/reference/ggready.md),
[`ggready.decorated()`](https://bergsmat.github.io/yamlet/reference/ggready.decorated.md)

## Examples

``` r
example(ggready)
#> 
#> ggredy> file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#> 
#> ggredy> x <- decorate(file)
#> 
#> ggredy> decorations(x, Weight)
#> - Weight
#>  - label: subject body weight
#>  - guide: kg
#> 
#> ggredy> decorations(as.data.frame(x), Weight) # downgrade still has attributes
#> - Weight
#>  - label: subject body weight
#>  - guide: kg
#> 
#> ggredy> class(x)
#> [1] "decorated"  "data.frame"
#> 
#> ggredy> class(ggready(as.data.frame(x)))
#> [1] "ggready"    "decorated"  "data.frame"
#> 
#> ggredy> class(ggready(x))
#> [1] "ggready"    "decorated"  "data.frame"
#> 
#> ggredy> class(ggready(resolve(x)))
#> [1] "ggready"    "decorated"  "data.frame"
#> 
#> ggredy> x <- ggready(x)
#> 
#> ggredy> library(magrittr)
#> 
#> ggredy> library(ggplot2)
#> 
#> ggredy> # Here we filter on-the-fly
#> ggredy> # without loss of attributes.
#> ggredy> # Notice mg/L rendering; this is actually part of an expression.
#> ggredy> file %>%
#> ggredy+  decorate %>%
#> ggredy+  filter(!is.na(conc)) %>%
#> ggredy+  ggready %>%
#> ggredy+  ggplot(aes(x = time, y = conc, color = Heart)) +
#> ggredy+  geom_point()

#> 
#> ggredy> # By default ggready resolves everything that is decorated.
#> ggredy> # But we can intervene to resolve selectively,
#> ggredy> # And further intervene to 'ggready' selectively.
#> ggredy> #
#> ggredy> x <- file %>% decorate %>% filter(!is.na(conc))
#> 
#> ggredy> x %>%
#> ggredy+ resolve(conc, time) %>%   # Heart left unresolved!
#> ggredy+ ggready(conc, Heart, resolve = FALSE) %>%  # time left unreadied!
#> ggredy+ ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()

#> 
#> ggredy> # Still, all the labels were actually expressions:
#> ggredy> x %>%
#> ggredy+ resolve(conc, time) %>%
#> ggredy+ ggready(conc, Heart, resolve = FALSE) %>%
#> ggredy+ decorations(conc, time, Heart)
#> - conc
#>  - label: [ '*', [ '*', "quinidine" * " " * "serum" * " " * "concentration",   ], (mg/L) ]
#>  - units: mg/L
#>  - title: quinidine serum concentration (mg/L)
#> - time
#>  - label: time since start of study
#>  - units: h
#>  - title: time since start of study (h)
#> - Heart
#>  - label: [ '*', [ '*', "congestive" * " " * "heart",   ], failure ]
#>  - guide: No/Mild, Moderate, Severe
```
