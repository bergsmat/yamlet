# Filter Decorated

Filters a decorated data.frame. After a filter operation, promote() is
called to see if ambiguous conditional attributes can be improved.

## Usage

``` r
# S3 method for class 'decorated'
filter(
  .data,
  ...,
  .preserve = FALSE,
  .promote = getOption("yamlet_promote", TRUE)
)
```

## Arguments

- .data:

  passed to
  [`filter`](https://dplyr.tidyverse.org/reference/filter.html)

- ...:

  passed to
  [`filter`](https://dplyr.tidyverse.org/reference/filter.html)

- .preserve:

  passed to
  [`filter`](https://dplyr.tidyverse.org/reference/filter.html)

- .promote:

  whether to auto-promote plural attributes

## Value

decorated

## See also

Other promote:
[`[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md),
[`promote()`](https://bergsmat.github.io/yamlet/reference/promote.md),
[`promote.data.frame()`](https://bergsmat.github.io/yamlet/reference/promote.data.frame.md),
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md),
[`singularity()`](https://bergsmat.github.io/yamlet/reference/singularity.md)

## Examples

``` r
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- file %>% decorate

# Note that there are two elements each for value label and value guide.
x %>% decorations(event, value)
#> - event
#>  - label: type of event
#>  - guide: dose, conc
#> - value
#>  - label
#>   - event == 'dose': dose of drug administered
#>   - event == 'conc': serum phenobarbital concentration
#>  - guide
#>   - event == 'dose': µg/kg
#>   - event == 'conc': µg/L

# Filtering promotes the relevant conditional attributes automatically.
x %>% filter(event == 'dose') %>% decorations(value)
#> - value
#>  - label: dose of drug administered
#>  - guide: µg/kg
x %>% filter(event == 'conc') %>% decorations(value)
#> - value
#>  - label: serum phenobarbital concentration
#>  - guide: µg/L
```
