# Gather a Decorated Data Frame

Gathers a decorated data.frame. I.e. a gather method for class
'decorated'. Invokes tidyr::gather(), converting gathered labels to the
guide attribute of `key`, and converting gathered guides ... if all the
same ... to the guide attribute of `value`. Somewhat experimental!

## Usage

``` r
# S3 method for class 'decorated'
gather(
  data,
  key = "key",
  value = "value",
  ...,
  na.rm = FALSE,
  convert = FALSE,
  factor_key = FALSE
)
```

## Arguments

- data:

  see [`gather`](https://tidyr.tidyverse.org/reference/gather.html)

- key:

  see [`gather`](https://tidyr.tidyverse.org/reference/gather.html)

- value:

  see [`gather`](https://tidyr.tidyverse.org/reference/gather.html)

- ...:

  see [`gather`](https://tidyr.tidyverse.org/reference/gather.html)

- na.rm:

  see [`gather`](https://tidyr.tidyverse.org/reference/gather.html)

- convert:

  see [`gather`](https://tidyr.tidyverse.org/reference/gather.html)

- factor_key:

  see [`gather`](https://tidyr.tidyverse.org/reference/gather.html)

## Value

decorated

## Examples

``` r
library(magrittr)
library(tidyr)
#> 
#> Attaching package: ‘tidyr’
#> The following object is masked from ‘package:magrittr’:
#> 
#>     extract
#> The following object is masked from ‘package:yamlet’:
#> 
#>     unnest
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)
x %>% gather('key', 'value', time, interval) %>% decorations
#> Warning: attributes are not identical across measure variables; they will be dropped
#> - Subject
#>  - label: patient identifier
#> - conc
#>  - label: quinidine serum concentration
#>  - guide: mg/L
#> - dose
#>  - label: quinidine administered dose
#>  - guide: mg
#> - Age
#>  - label: baseline subject age
#>  - guide: year
#> - Height
#>  - label: baseline subject height
#>  - guide: in
#> - Weight
#>  - label: subject body weight
#>  - guide: kg
#> - Race
#>  - label: subject race
#>  - guide: Caucasian, Latin, Black
#> - Smoke
#>  - label: smoking status
#>  - guide: 'no', 'yes'
#> - Ethanol
#>  - label: ethanol abuse status
#>  - guide: none, current, former
#> - Heart
#>  - label: congestive heart failure
#>  - guide: No/Mild, Moderate, Severe
#> - Creatinine
#>  - label: creatinine clearance
#>  - guide
#>   - < 50 mg/min: < 50
#>   - '>= 50 mg/min': '>= 50'
#> - glyco
#>  - label: alpha-1 acid glycoprotein concentration
#>  - guide: mg/dL
#> - key: 
#> - value: 
```
