# Modify Attributes of Indicated Components

Modifies the attributes of indicated components. Generic, with method
[`modify.default`](https://bergsmat.github.io/yamlet/reference/modify.default.md).

## Usage

``` r
modify(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

see methods

## See also

Other modify:
[`modify.default()`](https://bergsmat.github.io/yamlet/reference/modify.default.md),
[`named()`](https://bergsmat.github.io/yamlet/reference/named.md),
[`selected()`](https://bergsmat.github.io/yamlet/reference/selected.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md)

## Examples

``` r
example(modify.default)
#> 
#> mdfy.d> library(magrittr)
#> 
#> mdfy.d> library(dplyr)
#> 
#> mdfy.d> file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
#> 
#> mdfy.d> x <- decorate(file)
#> 
#> mdfy.d> # modify selected columns
#> mdfy.d> x %<>% modify(title = paste(label, '(', guide, ')'), time)
#> 
#> mdfy.d> x %>% select(time, conc) %>% decorations
#> - time
#>  - label: time since start of study
#>  - guide: h
#>  - title: time since start of study ( h )
#> - conc
#>  - label: quinidine serum concentration
#>  - guide: mg/L
#> 
#> mdfy.d> # modify (almost) all columns
#> mdfy.d> x %<>% modify(title = paste(label, '(', guide, ')'), -Subject)
#> 
#> mdfy.d> x %>% select(time, conc) %>% decorations
#> - time
#>  - label: time since start of study
#>  - guide: h
#>  - title: time since start of study ( h )
#> - conc
#>  - label: quinidine serum concentration
#>  - guide: mg/L
#>  - title: quinidine serum concentration ( mg/L )
#> 
#> mdfy.d> # use column itself
#> mdfy.d> x %<>% modify(`defined values` = sum(!is.na(.)))
#> 
#> mdfy.d> x %>% select(time) %>% decorations
#> - time
#>  - label: time since start of study
#>  - guide: h
#>  - title: time since start of study ( h )
#>  - defined values: 1471
#> 
#> mdfy.d> # rename column
#> mdfy.d> x %<>% modify(time, name = label)
#> 
#> mdfy.d> names(x)
#>  [1] "Subject"                   "time since start of study"
#>  [3] "conc"                      "dose"                     
#>  [5] "interval"                  "Age"                      
#>  [7] "Height"                    "Weight"                   
#>  [9] "Race"                      "Smoke"                    
#> [11] "Ethanol"                   "Heart"                    
#> [13] "Creatinine"                "glyco"                    
#> 
#> mdfy.d> # warn if assignment fails
#> mdfy.d> ## Not run: 
#> mdfy.d> ##D \donttest{
#> mdfy.d> ##D x %<>% modify(title = foo, time)
#> mdfy.d> ##D }
#> mdfy.d> ## End(Not run)
#> mdfy.d> 
#> mdfy.d> # support lists
#> mdfy.d> list(a = 1, b = 1:10, c = letters) %>%
#> mdfy.d+ modify(length = length(.), b:c)
#> $a
#> [1] 1
#> 
#> $b
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"length")
#> [1] 10
#> 
#> $c
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t" "u" "v" "w" "x" "y" "z"
#> attr(,"length")
#> [1] 26
#> 
#> 
#> mdfy.d> x %<>% select(Subject) %>% modify(label = NULL, `defined values` = NULL)
#> 
#> mdfy.d> # distinguish data and environment
#> mdfy.d> location <- 'environment'
#> 
#> mdfy.d> x %>% modify(where = location) %>% decorations
#> - Subject
#>  - where: environment
#> 
#> mdfy.d> x %>% modify(where = .env$location) %>% decorations
#> - Subject
#>  - where: environment
#> 
#> mdfy.d> ## Not run: 
#> mdfy.d> ##D \donttest{
#> mdfy.d> ##D x%>% modify(where = .data$location) %>% decorations
#> mdfy.d> ##D }
#> mdfy.d> ## End(Not run)
#> mdfy.d> x %>% modify(location = 'attributes', where = location) %>% decorations
#> - Subject
#>  - location: attributes
#>  - where: attributes
#> 
#> mdfy.d> x %>% modify(location = 'attributes', where = .data$location) %>% decorations
#> - Subject
#>  - location: attributes
#>  - where: attributes
```
