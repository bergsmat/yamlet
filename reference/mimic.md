# Try To Look Like Something Else

Tries to make an object look like something else. Generic, with method
[`mimic.default`](https://bergsmat.github.io/yamlet/reference/mimic.default.md)

## Usage

``` r
mimic(x, ...)
```

## Arguments

- x:

  object

- ...:

  passed arguments

## Value

see methods

## See also

Other mimic:
[`mimic.classified()`](https://bergsmat.github.io/yamlet/reference/mimic.classified.md),
[`mimic.default()`](https://bergsmat.github.io/yamlet/reference/mimic.default.md)

## Examples

``` r
example(mimic.default)
#> 
#> mmc.df> library(magrittr)
#> 
#> mmc.df> library(dplyr)
#> 
#> mmc.df> let <- letters[1:5]
#> 
#> mmc.df> LET <- LETTERS[1:5]
#> 
#> mmc.df> int <- 0L:4L
#> 
#> mmc.df> num <- as.numeric(int)
#> 
#> mmc.df> fac <- factor(let)
#> 
#> mmc.df> css <- classified(let)
#> 
#> mmc.df> # any of these can mimic any other
#> mmc.df> str(mimic(LET, let))
#>  chr [1:5] "A" "B" "C" "D" "E"
#>  - attr(*, "guide")=List of 5
#>   ..$ a: chr "A"
#>   ..$ b: chr "B"
#>   ..$ c: chr "C"
#>   ..$ d: chr "D"
#>   ..$ e: chr "E"
#> 
#> mmc.df> str(mimic(num, let))
#>  num [1:5] 0 1 2 3 4
#>  - attr(*, "guide")=List of 5
#>   ..$ a: num 0
#>   ..$ b: num 1
#>   ..$ c: num 2
#>   ..$ d: num 3
#>   ..$ e: num 4
#> 
#> mmc.df> str(mimic(let, num))
#>  chr [1:5] "a" "b" "c" "d" "e"
#>  - attr(*, "guide")=List of 5
#>   ..$ 0: chr "a"
#>   ..$ 1: chr "b"
#>   ..$ 2: chr "c"
#>   ..$ 3: chr "d"
#>   ..$ 4: chr "e"
#> 
#> mmc.df> # factors get a guide and classifieds get a named codelist
#> mmc.df> str(mimic(fac, int))
#>  Factor w/ 5 levels "a","b","c","d",..: 1 2 3 4 5
#>  - attr(*, "guide")=List of 5
#>   ..$ 0: chr "a"
#>   ..$ 1: chr "b"
#>   ..$ 2: chr "c"
#>   ..$ 3: chr "d"
#>   ..$ 4: chr "e"
#> 
#> mmc.df> str(mimic(css, int))
#>  Factor w/ 5 levels "a","b","c","d",..: 1 2 3 4 5
#>  - attr(*, "codelist")=List of 5
#>   ..$ 0: chr "a"
#>   ..$ 1: chr "b"
#>   ..$ 2: chr "c"
#>   ..$ 3: chr "d"
#>   ..$ 4: chr "e"
#> 
#> mmc.df> # int can 'pick up' the factor levels as guide names
#> mmc.df> str(mimic(int, css))
#>  int [1:5] 0 1 2 3 4
#>  - attr(*, "guide")=List of 5
#>   ..$ a: int 0
#>   ..$ b: int 1
#>   ..$ c: int 2
#>   ..$ d: int 3
#>   ..$ e: int 4
#> 
#> mmc.df> # if two variables mean essentially the same thing,
#> mmc.df> # mimic lets you save space
#> mmc.df> x <- data.frame(id = 1:2, ID = c('A','B'))
#> 
#> mmc.df> x
#>   id ID
#> 1  1  A
#> 2  2  B
#> 
#> mmc.df> x %<>% mutate(id = mimic(id, ID)) %>% select(-ID)
#> 
#> mmc.df> x
#>   id
#> 1  1
#> 2  2
#> 
#> mmc.df> # ID still available, in principle:
#> mmc.df> x %>% as_decorated %>% resolve
#>   id
#> 1  A
#> 2  B
```
