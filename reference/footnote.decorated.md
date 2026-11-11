# Footnote Decorated

Footnotes a decorated data.frame. Generates a text string that defines
column names using label and unit attributes.

## Usage

``` r
# S3 method for class 'decorated'
footnote(x, ..., equal = ":", collapse = "; ")
```

## Arguments

- x:

  decorated

- ...:

  passed to
  [`append_units`](https://bergsmat.github.io/yamlet/reference/append_units.md)

- equal:

  character: a symbol suggesting equality between a name and its note

- collapse:

  used to [`paste`](https://rdrr.io/r/base/paste.html) column-wise
  footnotes

## Value

character

## See also

Other footnote:
[`footnote()`](https://bergsmat.github.io/yamlet/reference/footnote.md)

## Examples

``` r
library(magrittr)
set.seed(0)
x <- data.frame(
 auc = rnorm(100, mean = 2400, sd = 200),
 bmi = rnorm(100, mean = 20, sd = 5),
 gen = 0:1
)
x %<>% decorate('auc: [AUC_0-24, ng*h/mL]')
x %<>% decorate('bmi: [Body Mass Index, kg/m^2]')
x %<>% decorate('gen: [Gender, [Male: 1, Female: 0]]')
x %<>% resolve
footnote(x)
#> Error in if (kable_format %in% c("pipe", "markdown")) {    kable_input <- md_table_parser(kable_input)    kable_format <- attr(kable_input, "format")}: argument is of length zero
footnote(x, auc)
#> Error in if (kable_format %in% c("pipe", "markdown")) {    kable_input <- md_table_parser(kable_input)    kable_format <- attr(kable_input, "format")}: argument is of length zero
```
