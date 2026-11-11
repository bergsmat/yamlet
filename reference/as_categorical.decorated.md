# Coerce decorated to Categorical

Coerces (elements of) 'decorated' to categorical. See example(s).
Briefly, a continuous variable with units (and a small set of existing
values) is converted to a factor-ready variable.

## Usage

``` r
# S3 method for class 'decorated'
as_categorical(x, ...)
```

## Arguments

- x:

  decorated

- ...:

  unquoted names of columns to be converted

## Value

decorated

## See also

Other decorated:
[`[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md),
`[<-.decorated()`,
[`[[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-sub-.decorated.md),
`[[<-.decorated()`,
[`as_categorical()`](https://bergsmat.github.io/yamlet/reference/as_categorical.md),
[`merge.decorated()`](https://bergsmat.github.io/yamlet/reference/merge.decorated.md),
`names<-.decorated()`

## Examples

``` r
library(magrittr)
library(tablet)
library(kableExtra)
#> 
#> Attaching package: ‘kableExtra’
#> The following object is masked from ‘package:tablet’:
#> 
#>     kbl
#> The following object is masked from ‘package:yamlet’:
#> 
#>     footnote
library(yamlet)
x <- 
  data.frame(DOSE = c(12, 1.2, 2.4, 6, 12, 1.2)) %>% 
  decorate('DOSE: [ Dose, mg ]')
x %>% 
  as_categorical(DOSE) %>%
  resolve %>% 
  tablet %>% 
  as_kable %>%
  kable_classic
#> <table class=" lightable-classic" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; margin-left: auto; margin-right: auto;'>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;">   </th>
#>    <th style="text-align:left;"> All<br>(N = 6) </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr grouplength="4"><td colspan="2" style="border-bottom: 1px solid;"><strong>Dose</strong></td></tr>
#> <tr>
#>    <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 1.2 mg </td>
#>    <td style="text-align:left;"> 2 (33.3%) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 2.4 mg </td>
#>    <td style="text-align:left;"> 1 (16.7%) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 6 mg </td>
#>    <td style="text-align:left;"> 1 (16.7%) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;padding-left: 2em;" indentlevel="1"> 12 mg </td>
#>    <td style="text-align:left;"> 2 (33.3%) </td>
#>   </tr>
#> </tbody>
#> </table>
  
```
