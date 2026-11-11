# Coerce Yamlet to Character

Coerces yamlet to character. See also
[`as_yamlet.character`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md).

## Usage

``` r
# S3 method for class 'yamlet'
as.character(x, sort = TRUE, ...)
```

## Arguments

- x:

  yamlet

- sort:

  whether to coerce attribute order using
  [`canonical.yamlet`](https://bergsmat.github.io/yamlet/reference/canonical.yamlet.md)

- ...:

  passed to
  [`as.character.yam`](https://bergsmat.github.io/yamlet/reference/as.character.yam.md)
  and
  [`as_yam.yamlet`](https://bergsmat.github.io/yamlet/reference/as_yam.yamlet.md)

## Value

character

## See also

Other yamlet:
[`[.yamlet()`](https://bergsmat.github.io/yamlet/reference/sub-.yamlet.md),
[`as_yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
[`as_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md),
[`as_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.data.frame.md),
[`as_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.list.md),
[`as_yamlet.yam()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yam.md),
[`as_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yamlet.md),
[`print.yamlet()`](https://bergsmat.github.io/yamlet/reference/print.yamlet.md)

## Examples

``` r

as.character(as_yamlet('ID: subject identifier'))
#> [1] "ID: subject identifier"
as.character(as_yamlet(c('id: subject','amt: dose')))
#> [1] "id: subject" "amt: dose"  
as.character(as_yamlet(c('id: subject\namt: dose')))
#> [1] "id: subject" "amt: dose"  
foo <- as_yamlet(system.file(package = 'yamlet', 'extdata','quinidine.yaml'))
class(foo)
#> [1] "yamlet"
writeLines(as.character(foo))
#> Subject: patient identifier
#> time: [ time since start of study, h ]
#> conc: [ quinidine serum concentration, mg/L ]
#> dose: [ quinidine administered dose, mg ]
#> interval: [ inter-dose interval, h ]
#> Age: [ baseline subject age, year ]
#> Height: [ baseline subject height, in ]
#> Weight: [ subject body weight, kg ]
#> Race: [ subject race, [ Caucasian, Latin, Black ]]
#> Smoke: [ smoking status, [ 'no', 'yes' ]]
#> Ethanol: [ ethanol abuse status, [ none, current, former ]]
#> Heart: [ congestive heart failure, [ No/Mild, Moderate, Severe ]]
#> Creatinine: [ creatinine clearance, [ < 50 mg/min: < 50, '>= 50 mg/min': '>= 50' ]]
#> glyco: [ alpha-1 acid glycoprotein concentration, mg/dL ]
identical(foo, as_yamlet(as.character(foo)))
#> [1] TRUE
identical(as.character(foo), as.character(as_yamlet(as.character(foo))))
#> [1] TRUE
file <- system.file(package = 'yamlet','extdata','quinidine.csv')
file
#> [1] "/home/runner/work/_temp/Library/yamlet/extdata/quinidine.csv"
foo <- resolve(decorate(file))
as.character(as_yamlet(foo))
#>  [1] "Subject: patient identifier"                                                                                             
#>  [2] "time: [ time since start of study, units: h, title: time since start of study (h) ]"                                     
#>  [3] "conc: [ quinidine serum concentration, units: mg/L, title: quinidine serum concentration (mg/L) ]"                       
#>  [4] "dose: [ quinidine administered dose, units: mg, title: quinidine administered dose (mg) ]"                               
#>  [5] "interval: [ inter-dose interval, units: h, title: inter-dose interval (h) ]"                                             
#>  [6] "Age: [ baseline subject age, units: year, title: baseline subject age (year) ]"                                          
#>  [7] "Height: [ baseline subject height, units: in, title: baseline subject height (in) ]"                                     
#>  [8] "Weight: [ subject body weight, units: kg, title: subject body weight (kg) ]"                                             
#>  [9] "Race: [ subject race, codelist: [ Caucasian, Latin, Black ]]"                                                            
#> [10] "Smoke: [ smoking status, codelist: [ 'no', 'yes' ]]"                                                                     
#> [11] "Ethanol: [ ethanol abuse status, codelist: [ none, current, former ]]"                                                   
#> [12] "Heart: [ congestive heart failure, codelist: [ No/Mild, Moderate, Severe ]]"                                             
#> [13] "Creatinine: [ creatinine clearance, codelist: [ < 50: < 50 mg/min, '>= 50': '>= 50 mg/min' ]]"                           
#> [14] "glyco: [ alpha-1 acid glycoprotein concentration, units: mg/dL, title: alpha-1 acid glycoprotein concentration (mg/dL) ]"
as.character(as_yamlet(foo, exclude_attr = 'class'))
#>  [1] "Subject: patient identifier"                                                                                                           
#>  [2] "time: [ time since start of study, units: h, title: time since start of study (h) ]"                                                   
#>  [3] "conc: [ quinidine serum concentration, units: mg/L, title: quinidine serum concentration (mg/L) ]"                                     
#>  [4] "dose: [ quinidine administered dose, units: mg, title: quinidine administered dose (mg) ]"                                             
#>  [5] "interval: [ inter-dose interval, units: h, title: inter-dose interval (h) ]"                                                           
#>  [6] "Age: [ baseline subject age, units: year, title: baseline subject age (year) ]"                                                        
#>  [7] "Height: [ baseline subject height, units: in, title: baseline subject height (in) ]"                                                   
#>  [8] "Weight: [ subject body weight, units: kg, title: subject body weight (kg) ]"                                                           
#>  [9] "Race: [ subject race, levels: [ Caucasian, Latin, Black ], codelist: [ Caucasian, Latin, Black ]]"                                     
#> [10] "Smoke: [ smoking status, levels: [ 'no', 'yes' ], codelist: [ 'no', 'yes' ]]"                                                          
#> [11] "Ethanol: [ ethanol abuse status, levels: [ none, current, former ], codelist: [ none, current, former ]]"                              
#> [12] "Heart: [ congestive heart failure, levels: [ No/Mild, Moderate, Severe ], codelist: [ No/Mild, Moderate, Severe ]]"                    
#> [13] "Creatinine: [ creatinine clearance, levels: [ < 50 mg/min, '>= 50 mg/min' ], codelist: [ < 50: < 50 mg/min, '>= 50': '>= 50 mg/min' ]]"
#> [14] "glyco: [ alpha-1 acid glycoprotein concentration, units: mg/dL, title: alpha-1 acid glycoprotein concentration (mg/dL) ]"              
```
