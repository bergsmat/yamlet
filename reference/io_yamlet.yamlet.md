# Export Yamlet

Exports yamlet. The archetype method for
[`io_yamlet`](https://bergsmat.github.io/yamlet/reference/io_yamlet.md).
Similar to
[`write_yamlet`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)
but returns (description of) `con`.

## Usage

``` r
# S3 method for class 'yamlet'
io_yamlet(
  x,
  con = stdout(),
  eol = "\n",
  useBytes = FALSE,
  default_keys = getOption("yamlet_default_keys", list("label", "guide")),
  fileEncoding = getOption("encoding"),
  ...
)
```

## Arguments

- x:

  yamlet

- con:

  passed to [`writeLines`](https://rdrr.io/r/base/writeLines.html)

- eol:

  end-of-line; passed to
  [`writeLines`](https://rdrr.io/r/base/writeLines.html) as `sep`

- useBytes:

  passed to [`writeLines`](https://rdrr.io/r/base/writeLines.html)

- default_keys:

  character: default keys for the first n anonymous members of each
  element

- fileEncoding:

  if `con` is character, passed to
  [`file`](https://rdrr.io/r/base/connections.html) as `encoding`

- ...:

  passed to
  [`as.character.yamlet`](https://bergsmat.github.io/yamlet/reference/as.character.yamlet.md)

## Value

invisible description of con: i.e., a file path

## See also

Other io:
[`io_csv()`](https://bergsmat.github.io/yamlet/reference/io_csv.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res()`](https://bergsmat.github.io/yamlet/reference/io_res.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
[`io_table()`](https://bergsmat.github.io/yamlet/reference/io_table.md),
[`io_table.character()`](https://bergsmat.github.io/yamlet/reference/io_table.character.md),
[`io_table.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_table.data.frame.md),
[`io_yamlet()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.md),
[`io_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.character.md),
[`io_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.data.frame.md)

## Examples

``` r
library(csv)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)
out <- file.path(tempdir(), 'out.yamlet')
io_yamlet(as_yamlet(x), out)
io_yamlet(out)
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
