# Drop Title for Decorated Vector

Drops title for decorated vectors. If option `with_title` is TRUE and x
has a 'units' attribute, it removes the title attribute. See also
[`make_title`](https://bergsmat.github.io/yamlet/reference/make_title.md)
for coordinated use.

## Usage

``` r
# S3 method for class 'dvec'
drop_title(x, ..., with_title = getOption("yamlet_with_title", TRUE))
```

## Arguments

- x:

  dvec

- ...:

  ignored arguments

- with_title:

  whether to drop title

## Value

dvec

## See also

Other labels:
[`alias.data.frame()`](https://bergsmat.github.io/yamlet/reference/alias.data.frame.md),
[`append_units()`](https://bergsmat.github.io/yamlet/reference/append_units.md),
[`append_units.data.frame()`](https://bergsmat.github.io/yamlet/reference/append_units.data.frame.md),
[`append_units.default()`](https://bergsmat.github.io/yamlet/reference/append_units.default.md),
[`drop_title()`](https://bergsmat.github.io/yamlet/reference/drop_title.md),
[`drop_title.decorated()`](https://bergsmat.github.io/yamlet/reference/drop_title.decorated.md),
[`drop_title.default()`](https://bergsmat.github.io/yamlet/reference/drop_title.default.md),
[`make_title()`](https://bergsmat.github.io/yamlet/reference/make_title.md),
[`make_title.decorated()`](https://bergsmat.github.io/yamlet/reference/make_title.decorated.md),
[`make_title.default()`](https://bergsmat.github.io/yamlet/reference/make_title.default.md),
[`make_title.dvec()`](https://bergsmat.github.io/yamlet/reference/make_title.dvec.md),
[`sub_units()`](https://bergsmat.github.io/yamlet/reference/sub_units.md)

## Examples

``` r
library(magrittr)
1 %>% 
as_dvec(label = 'length', guide = 'mm') %>%
resolve %>%
desolve
#> [1] 1
#> attr(,"label")
#> [1] "length"
#> attr(,"guide")
#> [1] "mm"
```
