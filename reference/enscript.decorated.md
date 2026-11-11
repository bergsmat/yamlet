# Render Scripted Attributes of Indicated Components by Default

Modifies specific attributes of each indicated element (all elements by
default).

## Usage

``` r
# S3 method for class 'decorated'
enscript(
  x,
  ...,
  open = getOption("yamlet_append_units_open", " ("),
  close = getOption("yamlet_append_units_close", ")"),
  format = getOption("yamlet_format", ifelse(knitr::is_latex_output(), "latex", "html")),
  scripted = FALSE
)
```

## Arguments

- x:

  object

- ...:

  indicated columns, or name-value pairs; passed to
  [`resolve`](https://bergsmat.github.io/yamlet/reference/resolve.md)
  and
  [`selected`](https://bergsmat.github.io/yamlet/reference/selected.md)

- open:

  character to precede units

- close:

  character to follow units

- format:

  one of 'latex' or 'html'

- scripted:

  logical; always passed to
  [`resolve.decorated`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md)
  as FALSE (avoids circularity)

## Value

'enscript', a superclass of x

## Details

The goal here is to render labels and units (where present) in a way
that supports subscripts and superscripts for both plots and tables in
either html or latex contexts.

The current implementation writes an 'expression' attribute to support
figure labels and a 'title' attribute to support tables.
[`ggplot_build.decorated_ggplot`](https://bergsmat.github.io/yamlet/reference/ggplot_build.decorated_ggplot.md)
will attempt to honor the expression attribute if it exists.
[`tablet.data.frame`](https://rdrr.io/pkg/tablet/man/tablet.data.frame.html)
will attempt to honor the title attribute if it exists (see Details
there). An attempt is made to guess the output format (html or latex).

In addition to the 'title' and 'expression' attributes, enscript()
writes a 'plotmath' attribute to store plotmath versions of factor
levels, where present. By default, factor levels are converted to their
latex or html equivalents. However,
[`ggplot_build.decorated_ggplot`](https://bergsmat.github.io/yamlet/reference/ggplot_build.decorated_ggplot.md)
will use the plotmath versions of factor labels for legends and facet
labels. If a 'plotmath' attribute already exists, it is not overwritten,
preventing the same variable from being accidentally transformed twice.

To flexibly support latex, html, and plotmath, this function expects
column labels and units to be encoded in "spork" syntax. See
[`as_spork`](https://rdrr.io/pkg/spork/man/as_spork.html) for details
and examples. Briefly, "\_" precedes a subscript, "^" precedes a
superscript, and "." is used to force the termination of either a
superscript or a subscript where necessary. For best results, units
should be written using \*, /, and ^; e.g. "kg\*m^2/s^2" not "kg m2 s-2"
(although both are valid: see
[`is_parseable`](https://bergsmat.github.io/yamlet/reference/is_parseable.md)).
A literal backslash followed by "n" represents a newline. Greek letters
are represented by their names, except where names are enclosed in
backticks.

[`enscript()`](https://bergsmat.github.io/yamlet/reference/enscript.md)
always calls
[`resolve()`](https://bergsmat.github.io/yamlet/reference/resolve.md)
for the indicated columns, to make units present where appropriate.

## See also

Other enscript:
[`enscript()`](https://bergsmat.github.io/yamlet/reference/enscript.md)

Other interface:
[`canonical.decorated()`](https://bergsmat.github.io/yamlet/reference/canonical.decorated.md),
[`classified.data.frame()`](https://bergsmat.github.io/yamlet/reference/classified.data.frame.md),
[`decorate.character()`](https://bergsmat.github.io/yamlet/reference/decorate.character.md),
[`decorate.data.frame()`](https://bergsmat.github.io/yamlet/reference/decorate.data.frame.md),
[`desolve.decorated()`](https://bergsmat.github.io/yamlet/reference/desolve.decorated.md),
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
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md),
[`read_yamlet()`](https://bergsmat.github.io/yamlet/reference/read_yamlet.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md),
[`write_yamlet()`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)

## Examples

``` r
library(magrittr)
library(ggplot2)
x <- data.frame(time = 1:10, work = (1:10)^1.5)
x %<>% decorate('
  time: [ Time_elapsed, h ]
  work: [ Work_total_observed, kg*m^2/s^2 ]
')

x %>% decorations
#> - time
#>  - label: Time_elapsed
#>  - guide: h
#> - work
#>  - label: Work_total_observed
#>  - guide: kg*m^2/s^2
x %>% ggplot(aes(time, work)) + geom_point()

x %>% enscript %>% ggplot(aes(time, work)) + geom_point()

x %>% enscript(format = 'html') %$% work %>% attr('title')
#> Work<sub>total<sub>observed</sub></sub> (kg&#183;m<sup>2</sup>/s<sup>2</sup>)
testthat::expect_equal(enscript(x), enscript(enscript(x)))
```
