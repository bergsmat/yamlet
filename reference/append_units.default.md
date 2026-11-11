# Append Units By Default

Units attribute is wrapped in `open` and `close`, and appended to label.
Result is assigned to `target` attribute (default: 'label'). If style is
'latex' or 'plotmath', all elements are treated as spork
([`as_spork`](https://rdrr.io/pkg/spork/man/as_spork.html)) and coerced
to canonical form before concatenation.

## Usage

``` r
# Default S3 method
append_units(
  x,
  ...,
  open = getOption("yamlet_append_units_open", " ("),
  close = getOption("yamlet_append_units_close", ")"),
  style = getOption("yamlet_append_units_style", "plain"),
  target = getOption("yamlet_append_units_target", "label")
)
```

## Arguments

- x:

  object

- ...:

  passed to [`as_latex`](https://rdrr.io/pkg/spork/man/as_latex.html),
  [`as_plotmath`](https://rdrr.io/pkg/spork/man/as_plotmath.html)

- open:

  character to precede units

- close:

  character to follow units

- style:

  one of 'plain', 'latex', or 'plotmath'

- target:

  attribute name for appended result

## Value

same class as x, with sub-class 'latex' or 'plotmath' depending on
`style`

## See also

Other labels:
[`alias.data.frame()`](https://bergsmat.github.io/yamlet/reference/alias.data.frame.md),
[`append_units()`](https://bergsmat.github.io/yamlet/reference/append_units.md),
[`append_units.data.frame()`](https://bergsmat.github.io/yamlet/reference/append_units.data.frame.md),
[`drop_title()`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
[`drop_title.decorated()`](https://bergsmat.github.io/yamlet/reference/drop_title.decorated.md),
[`drop_title.default()`](https://bergsmat.github.io/yamlet/reference/drop_title.default.md),
[`drop_title.dvec()`](https://bergsmat.github.io/yamlet/reference/drop_title.dvec.md),
[`make_title()`](https://bergsmat.github.io/yamlet/reference/make_title.md),
[`make_title.decorated()`](https://bergsmat.github.io/yamlet/reference/make_title.decorated.md),
[`make_title.default()`](https://bergsmat.github.io/yamlet/reference/make_title.default.md),
[`make_title.dvec()`](https://bergsmat.github.io/yamlet/reference/make_title.dvec.md),
[`sub_units()`](https://bergsmat.github.io/yamlet/reference/sub_units.md)

## Examples

``` r
library(units)
#> udunits database from /usr/share/xml/udunits/udunits2.xml
library(magrittr)
x <- 1:10
attr(x, 'label') <- 'acceleration'
units(x) <- 'm/s^2'
y <- as_units('kg')
x %>% attr('label')
#> [1] "acceleration"
x %>% append_units %>% attr('label')
#> [1] "acceleration (m/s^2)"
y %>% attr('label')
#> NULL
y %>% append_units %>% attr('label')
#> [1] " (kg)"
x %>% append_units(style = 'plain')
#> Units: [m/s^2]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"label")
#> [1] "acceleration (m/s^2)"
x %>% append_units(style = 'plotmath')
#> Units: [m/s^2]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"label")
#> expression("acceleration" * " " * "(m/s"^{
#>     "2"
#> } * ")")
x %>% append_units(style = 'latex')
#> Units: [m/s^2]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"label")
#> [1] "\\(\\mathrm{\\textrm{acceleration} \\textrm{ } \\textrm{(m/s}{}^{\\textrm{\\scriptsize 2}} \\textrm{)}}\\)"
#> attr(,"class")
#> [1] "latex"     "character"

```
