# Conditionalize Attributes

Conditionalizes attributes of something. Generic, with method for
data.frame.

## Usage

``` r
conditionalize(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

see methods

## See also

Other conditionalize:
[`conditionalize.data.frame()`](https://bergsmat.github.io/yamlet/reference/conditionalize.data.frame.md)

## Examples

``` r
example(conditionalize.data.frame)
#> 
#> cndt..> library(magrittr)
#> 
#> cndt..> library(dplyr)
#> 
#> cndt..> library(csv)
#> 
#> cndt..> file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
#> 
#> cndt..> x <- as.csv(file)
#> 
#> cndt..> head(x,3)
#>   Subject  Wt Apgar ApgarInd time event value
#> 1       1 1.4     7     >= 5  0.0  dose  25.0
#> 2       1 1.4     7     >= 5  2.0  conc  17.3
#> 3       1 1.4     7     >= 5 12.5  dose   3.5
#> 
#> cndt..> # suppose we have an event label stored as a column:
#> cndt..> 
#> cndt..> x %<>% mutate(evid = ifelse(
#> cndt..+   event == 'dose',
#> cndt..+   'dose of drug administered',
#> cndt..+   'serum phenobarbital concentration'
#> cndt..+  )
#> cndt..+ )
#> 
#> cndt..> # We can define a conditional label for 'value'
#> cndt..> # by mapping evid to event:
#> cndt..> 
#> cndt..> x %<>% conditionalize(value, label, event, evid)
#> 
#> cndt..> x %>% as_yamlet
#> - Subject: 
#> - Wt: 
#> - Apgar: 
#> - ApgarInd: 
#> - time: 
#> - event: 
#> - value
#>  - label
#>   - event == 'dose': dose of drug administered
#>   - event == 'conc': serum phenobarbital concentration
#> - evid: 
#> 
#> cndt..> x %>% write_yamlet
#> Subject: 
#> Wt: 
#> Apgar: 
#> ApgarInd: 
#> time: 
#> event: 
#> value: [[ event == 'dose': dose of drug administered, event == 'conc': serum phenobarbital concentration ]]
#> evid: 
```
