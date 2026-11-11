# a \<- as_decorated(as.list(setNames(letters\[1:3\], LETTERS\[1:3\]))) attr(a\$B, 'label') \<- 'foo' a \<- a\[1:3\] attributes(a) Subset Decorated

Subsets decorated. Calls
[`promote`](https://bergsmat.github.io/yamlet/reference/promote.md)
internally to improve ambiguous conditional attributes where possible.

## Usage

``` r
# S3 method for class 'decorated'
x[..., .promote = getOption("yamlet_promote", TRUE)]
```

## Arguments

- x:

  decorated

- ...:

  passed to next method

- .promote:

  whether to auto-promote plural attributes

## Value

decorated (unless dimension is dropped)

## See also

Other promote:
[`filter.decorated()`](https://bergsmat.github.io/yamlet/reference/filter.decorated.md),
[`promote()`](https://bergsmat.github.io/yamlet/reference/promote.md),
[`promote.data.frame()`](https://bergsmat.github.io/yamlet/reference/promote.data.frame.md),
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md),
[`singularity()`](https://bergsmat.github.io/yamlet/reference/singularity.md)

Other decorated: `[<-.decorated()`,
[`[[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.decorated.md),
`[[<-.decorated()`,
[`as_categorical()`](https://bergsmat.github.io/yamlet/reference/as_categorical.md),
[`as_categorical.decorated()`](https://bergsmat.github.io/yamlet/reference/as_categorical.decorated.md),
[`merge.decorated()`](https://bergsmat.github.io/yamlet/reference/merge.decorated.md),
`names<-.decorated()`

## Examples

``` r
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- file %>% decorate
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

# Subsetting promotes automatically.
x[x$event == 'dose',] %>% decorations(event, value)
#> - event
#>  - label: type of event
#>  - guide: dose, conc
#> - value
#>  - label: dose of drug administered
#>  - guide: µg/kg
x[x$event == 'conc',] %>% decorations(event, value)
#> - event
#>  - label: type of event
#>  - guide: dose, conc
#> - value
#>  - label: serum phenobarbital concentration
#>  - guide: µg/L

# Dimension may be dropped
x[1,1]
#> [1] 1
#> attr(,"label")
#> [1] "infant identifier"

# Conventional subsetting
a <- as_decorated(as.list(setNames(letters[1:3], LETTERS[1:3])))
attr(a$B, 'label') <- 'foo'
a <- a[1:3]
attributes(a)
#> $names
#> [1] "A" "B" "C"
#> 
#> $class
#> [1] "decorated" "list"     
#> 
```
