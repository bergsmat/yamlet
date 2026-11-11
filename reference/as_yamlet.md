# Coerce to Yamlet

Coerces something to yamlet format. If the object or user specifies
default keys, these are applied. See
[`as_yamlet.character`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md).

## Usage

``` r
as_yamlet(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

a named list

## See also

Other yamlet:
[`[.yamlet()`](https://bergsmat.github.io/yamlet/reference/sub-.yamlet.md),
[`as.character.yamlet()`](https://bergsmat.github.io/yamlet/reference/as.character.yamlet.md),
[`as_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md),
[`as_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.data.frame.md),
[`as_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.list.md),
[`as_yamlet.yam()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yam.md),
[`as_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yamlet.md),
[`print.yamlet()`](https://bergsmat.github.io/yamlet/reference/print.yamlet.md)

## Examples

``` r
file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
file
#> [1] "/home/runner/work/_temp/Library/yamlet/extdata/quinidine.yaml"
identical(as_yamlet(as_yam(file)), as_yamlet(file))
#> [1] TRUE

# Read yamlet from storage and apply default keys.
as_yamlet(file)
#> - Subject
#>  - label: patient identifier
#> - time
#>  - label: time since start of study
#>  - guide: h
#> - conc
#>  - label: quinidine serum concentration
#>  - guide: mg/L
#> - dose
#>  - label: quinidine administered dose
#>  - guide: mg
#> - interval
#>  - label: inter-dose interval
#>  - guide: h
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
```
