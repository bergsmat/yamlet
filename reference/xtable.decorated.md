# Create Export Table for Decorated

Creates an export table for decorated data.frame by adding a footnote
attribute.

## Usage

``` r
# S3 method for class 'decorated'
xtable(x, ..., label = NULL, style = "latex")
```

## Arguments

- x:

  decorated

- ...:

  passed to
  [`footnote`](https://bergsmat.github.io/yamlet/reference/footnote.md)
  and (if named) [`xtable`](https://rdrr.io/pkg/xtable/man/xtable.html)

- label:

  passed to [`xtable`](https://rdrr.io/pkg/xtable/man/xtable.html)

- style:

  passed to
  [`footnote`](https://bergsmat.github.io/yamlet/reference/footnote.md)

## Value

class 'decorated_xtable','xtable', 'data.frame'

## Examples

``` r
library(magrittr)
library(xtable)
set.seed(0)
x <- data.frame(
 auc = rnorm(100, mean = 2400, sd = 200),
 bmi = rnorm(100, mean = 20, sd = 5),
 gen = 0:1
)
x %<>% decorate('auc: [AUC_0-24, ng*h/mL]')
x %<>% decorate('bmi: [Body Mass Index, kg/m^2]')
x %<>% decorate('gen: [Gender, [Male: 1, Female: 0]]')
y <- xtable(x)
attr(y, 'footnote')
#> [1] "auc:\\(\\mathrm{\\textrm{AUC}{}_{\\textrm{\\scriptsize 0-24}}}\\); bmi:\\(\\mathrm{\\textrm{Body} \\textrm{ } \\textrm{Mass} \\textrm{ } \\textrm{Index}}\\); gen:\\(\\mathrm{\\textrm{Gender}}\\)"
y <- xtable(x, auc:bmi)
attr(y, 'footnote')
#> [1] "auc:\\(\\mathrm{\\textrm{AUC}{}_{\\textrm{\\scriptsize 0-24}}}\\); bmi:\\(\\mathrm{\\textrm{Body} \\textrm{ } \\textrm{Mass} \\textrm{ } \\textrm{Index}}\\)"
```
