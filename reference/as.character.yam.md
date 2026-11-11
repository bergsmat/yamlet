# Coerce Yam to Character

Coerces class yam to character. Forms the basis for a yamlet emitter.

## Usage

``` r
# S3 method for class 'yam'
as.character(x, ...)
```

## Arguments

- x:

  yam

- ...:

  ignored; keys is an attribute of yam

## Value

character

## See also

Other yam:
[`as_yam()`](https://bergsmat.github.io/yamlet/reference/as_yam.md),
[`as_yam.character()`](https://bergsmat.github.io/yamlet/reference/as_yam.character.md),
[`as_yam.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yam.yamlet.md)

## Examples

``` r
foo <- as_yamlet(c('id: subject','amt: dose'))
class(foo)
#> [1] "yamlet"
bar <- as_yam(foo)
class(bar)
#> [1] "yam"
as.character(bar)
#> [1] "id: subject" "amt: dose"  
as.character(
as_yam(
as_yamlet(
"race: [label: race, guide: [ white: 0, black: 1, asian: 2 ], multiple: ['yes': 1, 'no': 0]]"
)))
#> [1] "race: [ race, [ white: 0, black: 1, asian: 2 ], multiple: [ 'yes': 1, 'no': 0 ]]"
```
