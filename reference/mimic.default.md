# Try To Look Like Another Equal-length Variable

Tries to mimic another vector or factor. If meaningful and possible, x
acquires a guide attribute with labels from corresponding values in y.
Any codelist attribute is removed. No guide is created for zero-length
x. If x is a factor, unused levels are removed.

## Usage

``` r
# Default S3 method
mimic(x, y = x, ...)
```

## Arguments

- x:

  vector-like

- y:

  vector-like, same length as x

- ...:

  passed to `link{factor}`

## Value

same class as x

## See also

Other mimic:
[`mimic()`](https://bergsmat.github.io/yamlet/reference/mimic.md),
[`mimic.classified()`](https://bergsmat.github.io/yamlet/reference/mimic.classified.md)

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
[`modify.default()`](https://bergsmat.github.io/yamlet/reference/modify.default.md),
[`promote.list()`](https://bergsmat.github.io/yamlet/reference/promote.list.md),
[`read_yamlet()`](https://bergsmat.github.io/yamlet/reference/read_yamlet.md),
[`resolve.decorated()`](https://bergsmat.github.io/yamlet/reference/resolve.decorated.md),
[`selected.default()`](https://bergsmat.github.io/yamlet/reference/selected.default.md),
[`write_yamlet()`](https://bergsmat.github.io/yamlet/reference/write_yamlet.md)

## Examples

``` r
library(magrittr)
library(dplyr)
let <- letters[1:5]
LET <- LETTERS[1:5]
int <- 0L:4L
num <- as.numeric(int)
fac <- factor(let)
css <- classified(let)

# any of these can mimic any other
str(mimic(LET, let))
#>  chr [1:5] "A" "B" "C" "D" "E"
#>  - attr(*, "guide")=List of 5
#>   ..$ a: chr "A"
#>   ..$ b: chr "B"
#>   ..$ c: chr "C"
#>   ..$ d: chr "D"
#>   ..$ e: chr "E"
str(mimic(num, let))
#>  num [1:5] 0 1 2 3 4
#>  - attr(*, "guide")=List of 5
#>   ..$ a: num 0
#>   ..$ b: num 1
#>   ..$ c: num 2
#>   ..$ d: num 3
#>   ..$ e: num 4
str(mimic(let, num))
#>  chr [1:5] "a" "b" "c" "d" "e"
#>  - attr(*, "guide")=List of 5
#>   ..$ 0: chr "a"
#>   ..$ 1: chr "b"
#>   ..$ 2: chr "c"
#>   ..$ 3: chr "d"
#>   ..$ 4: chr "e"

# factors get a guide and classifieds get a named codelist
str(mimic(fac, int))
#>  Factor w/ 5 levels "a","b","c","d",..: 1 2 3 4 5
#>  - attr(*, "guide")=List of 5
#>   ..$ 0: chr "a"
#>   ..$ 1: chr "b"
#>   ..$ 2: chr "c"
#>   ..$ 3: chr "d"
#>   ..$ 4: chr "e"
str(mimic(css, int))
#>  Factor w/ 5 levels "a","b","c","d",..: 1 2 3 4 5
#>  - attr(*, "codelist")=List of 5
#>   ..$ 0: chr "a"
#>   ..$ 1: chr "b"
#>   ..$ 2: chr "c"
#>   ..$ 3: chr "d"
#>   ..$ 4: chr "e"

# int can 'pick up' the factor levels as guide names
str(mimic(int, css))
#>  int [1:5] 0 1 2 3 4
#>  - attr(*, "guide")=List of 5
#>   ..$ a: int 0
#>   ..$ b: int 1
#>   ..$ c: int 2
#>   ..$ d: int 3
#>   ..$ e: int 4

# if two variables mean essentially the same thing,
# mimic lets you save space
x <- data.frame(id = 1:2, ID = c('A','B'))
x
#>   id ID
#> 1  1  A
#> 2  2  B
x %<>% mutate(id = mimic(id, ID)) %>% select(-ID)
x
#>   id
#> 1  1
#> 2  2
# ID still available, in principle:
x %>% as_decorated %>% resolve
#>   id
#> 1  A
#> 2  B
```
