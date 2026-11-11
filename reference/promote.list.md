# Promote a List

Promotes attributes of list-like objects. For the plural attributes of
each element, any singularity is promoted to the sole attribute.
Reserved attributes are untouched. Methods
[`filter.decorated`](https://bergsmat.github.io/yamlet/reference/filter.decorated.md)
and
[`[.decorated`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md)
automatically attempt to promote attributes for all elements.

## Usage

``` r
# S3 method for class 'list'
promote(
  x,
  ...,
  .reserved = getOption("yamlet_promote_reserved", c("class", "levels", "labels",
    "names"))
)
```

## Arguments

- x:

  list, or list-like

- ...:

  indicated elements

- .reserved:

  attributes to leave untouched

## Value

same class as x

## See also

filter.decorated \[.decorated

Other promote:
[`[.decorated()`](https://bergsmat.github.io/yamlet/reference/sub-.decorated.md),
[`filter.decorated()`](https://bergsmat.github.io/yamlet/reference/filter.decorated.md),
[`promote()`](https://bergsmat.github.io/yamlet/reference/promote.md),
[`promote.data.frame()`](https://bergsmat.github.io/yamlet/reference/promote.data.frame.md),
[`singularity()`](https://bergsmat.github.io/yamlet/reference/singularity.md)

Other interface:
[`canonical.decorated()`](https://bergsmat.github.io/yamlet/reference/canonical.decorated.md),
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
[`enscript.decorated()`](https://bergsmat.github.io/yamlet/reference/enscript.decorated.md),
[`ggplot.decorated()`](https://bergsmat.github.io/yamlet/reference/ggplot.decorated.md),
[`io_csv.character()`](https://bergsmat.github.io/yamlet/reference/io_csv.character.md),
[`io_csv.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_csv.data.frame.md),
[`io_res.character()`](https://bergsmat.github.io/yamlet/reference/io_res.character.md),
[`io_res.decorated()`](https://bergsmat.github.io/yamlet/reference/io_res.decorated.md),
[`io_table.character()`](https://bergsmat.github.io/yamlet/reference/io_table.character.md),
[`io_table.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_table.data.frame.md),
[`io_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.character.md),
[`io_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/io_yamlet.data.frame.md),
[`is_parseable.default()`](https://bergsmat.github.io/yamlet/reference/is_parseable.default.md),
[`mimic.default()`](https://bergsmat.github.io/yamlet/reference/mimic.default.md),
[`modify.default()`](https://bergsmat.github.io/yamlet/reference/modify.default.md),
[`read_yamlet()`](https://bergsmat.github.io/yamlet/reference/read_yamlet.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md),
[`write_yamlet()`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)

## Examples

``` r
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- file %>% decorate

# Note that there are two elements each for value label and value guide.
x %>% decorations(event, value)
#> - event
#>  - label: type of event
#>  - guide: dose, conc
#> - value
#>  - label
#>   - event == 'dose': dose of drug administered
#>   - event == 'conc': serum phenobarbital concentration
#>  - guide
#>   - event == 'dose': µg/kg
#>   - event == 'conc': µg/L

# After filtering, only one set is relevant.
# promote() identifies and retains such.
x %>% dplyr:::filter.data.frame(event == 'dose') %>% decorations(value)
#> - value
#>  - label
#>   - event == 'dose': dose of drug administered
#>   - event == 'conc': serum phenobarbital concentration
#>  - guide
#>   - event == 'dose': µg/kg
#>   - event == 'conc': µg/L
x %>% dplyr:::filter.data.frame(event == 'dose') %>% promote %>% decorations(value)
#> - value
#>  - label: dose of drug administered
#>  - guide: µg/kg

# If for some reason we do a partial promote, value attributes are unaffected.
# Nonsense example:
x %>% dplyr:::filter.data.frame(event == 'dose') %>% promote(event) %>% decorations(value)
#> - value
#>  - label
#>   - event == 'dose': dose of drug administered
#>   - event == 'conc': serum phenobarbital concentration
#>  - guide
#>   - event == 'dose': µg/kg
#>   - event == 'conc': µg/L

# However, the 'decorated' method for filter() calls promote() internally.
x %>% filter(event == 'dose') %>% decorations(value)
#> - value
#>  - label: dose of drug administered
#>  - guide: µg/kg
```
