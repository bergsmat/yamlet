# Promote Something

Promotes something. Generic, with default method.

## Usage

``` r
promote(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

see methods

## See also

Other promote:
[`[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md),
[`filter.decorated()`](https://bergsmat.github.io/yamlet/reference/filter.decorated.md),
[`promote.data.frame()`](https://bergsmat.github.io/yamlet/reference/promote.data.frame.md),
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md),
[`singularity()`](https://bergsmat.github.io/yamlet/reference/singularity.md)

## Examples

``` r
example(promote.data.frame)
#> 
#> prmt..> example(promote.list)
#> 
#> prmt.l> library(magrittr)
#> 
#> prmt.l> file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#> 
#> prmt.l> x <- file %>% decorate
#> 
#> prmt.l> # Note that there are two elements each for value label and value guide.
#> prmt.l> x %>% decorations(event, value)
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
#> 
#> prmt.l> # After filtering, only one set is relevant.
#> prmt.l> # promote() identifies and retains such.
#> prmt.l> x %>% dplyr:::filter.data.frame(event == 'dose') %>% decorations(value)
#> - value
#>  - label
#>   - event == 'dose': dose of drug administered
#>   - event == 'conc': serum phenobarbital concentration
#>  - guide
#>   - event == 'dose': µg/kg
#>   - event == 'conc': µg/L
#> 
#> prmt.l> x %>% dplyr:::filter.data.frame(event == 'dose') %>% promote %>% decorations(value)
#> - value
#>  - label: dose of drug administered
#>  - guide: µg/kg
#> 
#> prmt.l> # If for some reason we do a partial promote, value attributes are unaffected.
#> prmt.l> # Nonsense example:
#> prmt.l> x %>% dplyr:::filter.data.frame(event == 'dose') %>% promote(event) %>% decorations(value)
#> - value
#>  - label
#>   - event == 'dose': dose of drug administered
#>   - event == 'conc': serum phenobarbital concentration
#>  - guide
#>   - event == 'dose': µg/kg
#>   - event == 'conc': µg/L
#> 
#> prmt.l> # However, the 'decorated' method for filter() calls promote() internally.
#> prmt.l> x %>% filter(event == 'dose') %>% decorations(value)
#> - value
#>  - label: dose of drug administered
#>  - guide: µg/kg
```
