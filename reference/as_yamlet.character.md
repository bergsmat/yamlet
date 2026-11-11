# Coerce Character To Yamlet Format

Coerces character to yamlet format. Length-one character is understood
as a file path if the file exists. Otherwise, it is treated as data. The
file is a mapping of (nested) sequences, where map keys are data item
names, and sequences represent data item attributes. Attributes may be
named or anonymous.

## Usage

``` r
# S3 method for class 'character'
as_yamlet(
  x,
  default_keys = getOption("yamlet_default_keys", list("label", "guide")),
  ...
)
```

## Arguments

- x:

  length-one filepath or actual data

- default_keys:

  character: default keys for the first n anonymous members of each
  element

- ...:

  passed to
  [`as_yam.character`](https://bergsmat.github.io/yamlet/reference/as_yam.character.md)
  and
  [`as_yamlet.yam`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yam.md)

## Value

yamlet: a named list with default keys applied

## Details

If an attribute is anonymous, an attempt is made to name it using
available defaults. A special item named '\_keys' if present identifies
a sequence of key names that over-ride `default_keys`. Attribute names
are sought first in the explicit yaml, then in the special item named
'\_keys', then in the `default_keys` argument passed to
[`as_yamlet`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
then in `options()$yamlet_default_keys`, then in the defaults for
argument `default_keys`.

## See also

Other yamlet:
[`[.yamlet()`](https://bergsmat.github.io/yamlet/reference/sub-.yamlet.md),
[`as.character.yamlet()`](https://bergsmat.github.io/yamlet/reference/as.character.yamlet.md),
[`as_yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
[`as_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.data.frame.md),
[`as_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.list.md),
[`as_yamlet.yam()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yam.md),
[`as_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yamlet.md),
[`print.yamlet()`](https://bergsmat.github.io/yamlet/reference/print.yamlet.md)

## Examples

``` r
file <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
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
as_yamlet('ID: subject identifier')
#> - ID
#>  - label: subject identifier
as_yamlet(c('id: subject','amt: dose'))
#> - id
#>  - label: subject
#> - amt
#>  - label: dose
as_yamlet(c('id: subject\namt: dose'))
#> - id
#>  - label: subject
#> - amt
#>  - label: dose
```
