# Infer Type of Guide

Infers type of guide. Default mapping function for
[`explicit_guide.yamlet`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md)
where it replaces the key 'guide' with the return value.

## Usage

``` r
infer_guide(x, data = NULL, default = "guide", token = "data", ...)
```

## Arguments

- x:

  character or list

- data:

  atomic

- default:

  value for unrecognized guides

- token:

  character: discriptive term for 'data' used in warning

- ...:

  ignored

## Value

length-one character

## Details

\* If x is a list, the result is 'codelist'.

\* If x otherwise has length greater than 1, result is the default
value.

\* If x
[`is_parseable`](https://bergsmat.github.io/yamlet/reference/is_parseable.md),
result is 'units'. Use
[`install_unit`](https://r-quantities.github.io/units/reference/install_unit.html)
to register a non-default unit.

\* If x contains two or more percent signs, result is 'format' (i.e. a
'format' string for a date or time class).

\* If x is ([`encoded`](https://rdrr.io/pkg/encode/man/encoded.html)),
result is 'encoding'.

\* A length-one value of x not otherwise recognized is assumed to be an
attempt to provide a length-one 'codelist'.

If data is supplied (not NULL), a warning is issued for a codelist with
elements not present.

## See also

Other explicit_guide:
[`explicit_guide()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.md),
[`explicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.data.frame.md),
[`explicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.dvec.md),
[`explicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.factor.md),
[`explicit_guide.yamlet()`](https://bergsmat.github.io/yamlet/reference/explicit_guide.yamlet.md),
[`implicit_guide()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.md),
[`implicit_guide.data.frame()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.data.frame.md),
[`implicit_guide.dvec()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.dvec.md),
[`implicit_guide.factor()`](https://bergsmat.github.io/yamlet/reference/implicit_guide.factor.md)

## Examples

``` r
infer_guide('a') # recognized unit
#> [1] "units"
infer_guide('z') # unrecognized as unit, evaluates to guide
#> [1] "guide"
if (FALSE) { # \dontrun{
# evaluates to codelist but data suggests otherwise (warning)
infer_guide(as.list(letters), data = LETTERS)
} # }
infer_guide(c(1,2,3))                   # guide
#> [1] "guide"
infer_guide(list('a','b','c'))          # codelist
#> [1] "codelist"
infer_guide(list(a = 1, b = 2, c = 3))  # codelist
#> [1] "codelist"
infer_guide(list(a = 1))                # codelist
#> [1] "codelist"
infer_guide('kg/m^2')                   # units
#> [1] "units"
infer_guide('%')                        # units
#> [1] "units"
infer_guide('%Y-%m-%d')                 # format
#> [1] "format"
infer_guide('//a/1//b/2//c/3//')        # encoding
#> [1] "encoding"
```
