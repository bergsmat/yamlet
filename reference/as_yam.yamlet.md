# Coerce Yamlet to Yam

Coerces class yamlet to yam, negotiating the default keys. For each
member of x, names of sub-members will be dropped if all previous such
have been dropped. I.e., attribute order is preserved, and 'guide' (by
default) will not be made implicit unless 'label' has already been
encountered (and made implicit). Default keys are attached as the 'keys'
attribute of the result.

## Usage

``` r
# S3 method for class 'yamlet'
as_yam(
  x,
  ...,
  default_keys = getOption("yamlet_default_keys", list("label", "guide"))
)
```

## Arguments

- x:

  yamlet

- ...:

  ignored

- default_keys:

  names that may be omitted in left subsets

## Value

yam

## See also

Other yam:
[`as.character.yam()`](https://bergsmat.github.io/yamlet/reference/as.character.yam.md),
[`as_yam()`](https://bergsmat.github.io/yamlet/reference/as_yam.md),
[`as_yam.character()`](https://bergsmat.github.io/yamlet/reference/as_yam.character.md)

## Examples

``` r
as_yam(as_yamlet(c('id: subject','amt: dose')))
#> $id
#> $id[[1]]
#> [1] "subject"
#> 
#> 
#> $amt
#> $amt[[1]]
#> [1] "dose"
#> 
#> 
#> attr(,"class")
#> [1] "yam"
#> attr(,"keys")
#> attr(,"keys")[[1]]
#> [1] "label"
#> 
#> attr(,"keys")[[2]]
#> [1] "guide"
#> 
as_yam(as_yamlet(c('amt: [ dose, mg ]')))
#> $amt
#> $amt[[1]]
#> [1] "dose"
#> 
#> $amt[[2]]
#> [1] "mg"
#> 
#> 
#> attr(,"class")
#> [1] "yam"
#> attr(,"keys")
#> attr(,"keys")[[1]]
#> [1] "label"
#> 
#> attr(,"keys")[[2]]
#> [1] "guide"
#> 
as_yam(as_yamlet(c('amt: [ guide: mg, label: dose ]')))
#> $amt
#> $amt$guide
#> [1] "mg"
#> 
#> $amt[[2]]
#> [1] "dose"
#> 
#> 
#> attr(,"class")
#> [1] "yam"
#> attr(,"keys")
#> attr(,"keys")[[1]]
#> [1] "label"
#> 
#> attr(,"keys")[[2]]
#> [1] "guide"
#> 
```
