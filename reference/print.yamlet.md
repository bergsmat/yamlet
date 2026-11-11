# Print a Yamlet

Prints a yamlet object for interactive inspection. By default, lists
with no names (recursively) that unlist to identical length are
displayed in one line for compactness. If this seems misleading, you can
turn it of with `options(yamlet_print_simplify = FALSE)`.

## Usage

``` r
# S3 method for class 'yamlet'
print(x, ..., simplify = getOption("yamlet_print_simplify", TRUE))
```

## Arguments

- x:

  yamlet

- ...:

  ignored

- simplify:

  whether to collapse the display of very simple lists into one line of
  output

## Value

invisible(x)

## See also

Other yamlet:
[`[.yamlet()`](https://bergsmat.github.io/yamlet/reference/sub-.yamlet.md),
[`as.character.yamlet()`](https://bergsmat.github.io/yamlet/reference/as.character.yamlet.md),
[`as_yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.md),
[`as_yamlet.character()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.character.md),
[`as_yamlet.data.frame()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.data.frame.md),
[`as_yamlet.list()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.list.md),
[`as_yamlet.yam()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yam.md),
[`as_yamlet.yamlet()`](https://bergsmat.github.io/yamlet/reference/as_yamlet.yamlet.md)

## Examples

``` r
as_yamlet('mpg: [efficiency, mi/gallon]\nvs: [Engine, [V-shaped: 0, straight: 1]]')
#> - mpg
#>  - label: efficiency
#>  - guide: mi/gallon
#> - vs
#>  - label: Engine
#>  - guide
#>   - V-shaped: 0
#>   - straight: 1
```
