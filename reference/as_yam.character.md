# Coerce Character to Yam

Coerces character to yam. Length-one character can be a filepath,
otherwise treated as data. Proceeds by importing the data and
determining the default keys.

## Usage

``` r
# S3 method for class 'character'
as_yam(
  x,
  as.named.list,
  handlers = list(seq = parsimonious, map = function(x) lapply(x, unclass), str =
    function(x) {
     if (identical(x, "yamlet_NA_literal_")) {
         return("NA")
  
      }
     if (identical(x, "NA")) {
         return(NA_character_)
     }
    
    return(x)
 }),
  ...
)
```

## Arguments

- x:

  length-one filepath or actual data

- as.named.list:

  enforced as TRUE

- ...:

  passed to
  [`read_yaml`](https://yaml.r-lib.org/reference/read_yaml.html) and
  [`yaml.load`](https://yaml.r-lib.org/reference/yaml.load.html) if
  supported

## Value

a named list

## See also

Other yam:
[`as.character.yam()`](https://bergsmat.github.io/yamlet/reference/as.character.yam.md),
[`as_yam()`](https://bergsmat.github.io/yamlet/reference/as_yam.md),
[`as_yam.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yam.yamlet.md)

## Examples

``` r

# Read sample data from file.
file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
file
#> [1] "/home/runner/work/_temp/Library/yamlet/extdata/quinidine.yaml"
as_yam(file)
#> $Subject
#> $Subject[[1]]
#> [1] "patient identifier"
#> 
#> 
#> $time
#> $time[[1]]
#> [1] "time since start of study"
#> 
#> $time[[2]]
#> [1] "h"
#> 
#> 
#> $conc
#> $conc[[1]]
#> [1] "quinidine serum concentration"
#> 
#> $conc[[2]]
#> [1] "mg/L"
#> 
#> 
#> $dose
#> $dose[[1]]
#> [1] "quinidine administered dose"
#> 
#> $dose[[2]]
#> [1] "mg"
#> 
#> 
#> $interval
#> $interval[[1]]
#> [1] "inter-dose interval"
#> 
#> $interval[[2]]
#> [1] "h"
#> 
#> 
#> $Age
#> $Age[[1]]
#> [1] "baseline subject age"
#> 
#> $Age[[2]]
#> [1] "year"
#> 
#> 
#> $Height
#> $Height[[1]]
#> [1] "baseline subject height"
#> 
#> $Height[[2]]
#> [1] "in"
#> 
#> 
#> $Weight
#> $Weight[[1]]
#> [1] "subject body weight"
#> 
#> $Weight[[2]]
#> [1] "kg"
#> 
#> 
#> $Race
#> $Race[[1]]
#> [1] "subject race"
#> 
#> $Race[[2]]
#> $Race[[2]][[1]]
#> [1] "Caucasian"
#> 
#> $Race[[2]][[2]]
#> [1] "Latin"
#> 
#> $Race[[2]][[3]]
#> [1] "Black"
#> 
#> 
#> 
#> $Smoke
#> $Smoke[[1]]
#> [1] "smoking status"
#> 
#> $Smoke[[2]]
#> $Smoke[[2]][[1]]
#> [1] "no"
#> 
#> $Smoke[[2]][[2]]
#> [1] "yes"
#> 
#> 
#> 
#> $Ethanol
#> $Ethanol[[1]]
#> [1] "ethanol abuse status"
#> 
#> $Ethanol[[2]]
#> $Ethanol[[2]][[1]]
#> [1] "none"
#> 
#> $Ethanol[[2]][[2]]
#> [1] "current"
#> 
#> $Ethanol[[2]][[3]]
#> [1] "former"
#> 
#> 
#> 
#> $Heart
#> $Heart[[1]]
#> [1] "congestive heart failure"
#> 
#> $Heart[[2]]
#> $Heart[[2]][[1]]
#> [1] "No/Mild"
#> 
#> $Heart[[2]][[2]]
#> [1] "Moderate"
#> 
#> $Heart[[2]][[3]]
#> [1] "Severe"
#> 
#> 
#> 
#> $Creatinine
#> $Creatinine[[1]]
#> [1] "creatinine clearance"
#> 
#> $Creatinine[[2]]
#> $Creatinine[[2]]$`< 50 mg/min`
#> [1] "< 50"
#> 
#> $Creatinine[[2]]$`>= 50 mg/min`
#> [1] ">= 50"
#> 
#> 
#> 
#> $glyco
#> $glyco[[1]]
#> [1] "alpha-1 acid glycoprotein concentration"
#> 
#> $glyco[[2]]
#> [1] "mg/dL"
#> 
#> 
#> attr(,"class")
#> [1] "yam"

# Read yamlet directly from character vector.
as_yam(c('ID:','TIME:'))
#> $ID
#> list()
#> 
#> $TIME
#> list()
#> 
#> attr(,"class")
#> [1] "yam"

# Read from length-one character (same result).
as_yam('ID:\nTIME:')
#> $ID
#> list()
#> 
#> $TIME
#> list()
#> 
#> attr(,"class")
#> [1] "yam"
```
