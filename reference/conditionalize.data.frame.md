# Conditionalize Attributes of Data Frame

Conditionalizes attributes of data.frame. Creates a conditional
`attribute` definition for `column` by mapping `value` to `test`. Only
considers records where both `test` and `value` are defined, and gives
an error if there is not one-to-one mapping. Can be used with write
methods as an alternative to hand-coding conditional metadata.

## Usage

``` r
# S3 method for class 'data.frame'
conditionalize(x, column, attribute, test, value, ...)
```

## Arguments

- x:

  data.frame

- column:

  unquoted name of column to conditionalize

- attribute:

  unquoted name of attribute to create for column

- test:

  unquoted name of column to test

- value:

  unquoted name of column supplying attribute value

- ...:

  ignored arguments

## Value

class 'decorated' 'data.frame'

## Details

If the test column is character, individual elements should not contain
both single and double quotes. For the conditional expressions, these
values will be single-quoted by default, or double-quoted if they
contain single quotes.

## See also

Other conditionalize:
[`conditionalize()`](https://bergsmat.github.io/yamlet/reference/conditionalize.md)

## Examples

``` r
library(magrittr)
library(dplyr)
library(csv)
file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- as.csv(file)
head(x,3)
#>   Subject  Wt Apgar ApgarInd time event value
#> 1       1 1.4     7     >= 5  0.0  dose  25.0
#> 2       1 1.4     7     >= 5  2.0  conc  17.3
#> 3       1 1.4     7     >= 5 12.5  dose   3.5

# suppose we have an event label stored as a column:

x %<>% mutate(evid = ifelse(
  event == 'dose',
  'dose of drug administered',
  'serum phenobarbital concentration'
 )
)

# We can define a conditional label for 'value'
# by mapping evid to event:

x %<>% conditionalize(value, label, event, evid)

x %>% as_yamlet
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
x %>% write_yamlet
#> Subject: 
#> Wt: 
#> Apgar: 
#> ApgarInd: 
#> time: 
#> event: 
#> value: [[ event == 'dose': dose of drug administered, event == 'conc': serum phenobarbital concentration ]]
#> evid: 
```
