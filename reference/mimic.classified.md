# Try To Make Classified Look Like Another Equal-length Variable

Tries to mimic another vector or factor for 'classified'. See
[`classified.default`](https://bergsmat.github.io/yamlet/reference/classified.default.md).
If meaningful and possible, x updates its codelist attribute with labels
from corresponding values in y. Codes that don't occur (i.e. unused
levels) are removed from the codelist.

## Usage

``` r
# S3 method for class 'classified'
mimic(x, y = x, ...)
```

## Arguments

- x:

  classified

- y:

  vector-like, same length as x

- ...:

  ignored arguments

## Value

classified

## See also

Other mimic:
[`mimic()`](https://bergsmat.github.io/yamlet/reference/mimic.md),
[`mimic.default()`](https://bergsmat.github.io/yamlet/reference/mimic.default.md)

## Examples

``` r
let <- letters[1:5]
LET <- LETTERS[1:5]
int <- 0L:4L
num <- as.numeric(int)
fac <- factor(let)
css <- classified(let)

mimic(LET)
#> [1] "A" "B" "C" "D" "E"
#> attr(,"guide")
#> attr(,"guide")[[1]]
#> [1] "A"
#> 
#> attr(,"guide")[[2]]
#> [1] "B"
#> 
#> attr(,"guide")[[3]]
#> [1] "C"
#> 
#> attr(,"guide")[[4]]
#> [1] "D"
#> 
#> attr(,"guide")[[5]]
#> [1] "E"
#> 
mimic(LET, let)
#> [1] "A" "B" "C" "D" "E"
#> attr(,"guide")
#> attr(,"guide")$a
#> [1] "A"
#> 
#> attr(,"guide")$b
#> [1] "B"
#> 
#> attr(,"guide")$c
#> [1] "C"
#> 
#> attr(,"guide")$d
#> [1] "D"
#> 
#> attr(,"guide")$e
#> [1] "E"
#> 
mimic(let, let)
#> [1] "a" "b" "c" "d" "e"
#> attr(,"guide")
#> attr(,"guide")[[1]]
#> [1] "a"
#> 
#> attr(,"guide")[[2]]
#> [1] "b"
#> 
#> attr(,"guide")[[3]]
#> [1] "c"
#> 
#> attr(,"guide")[[4]]
#> [1] "d"
#> 
#> attr(,"guide")[[5]]
#> [1] "e"
#> 
mimic(num, let)
#> [1] 0 1 2 3 4
#> attr(,"guide")
#> attr(,"guide")$a
#> [1] 0
#> 
#> attr(,"guide")$b
#> [1] 1
#> 
#> attr(,"guide")$c
#> [1] 2
#> 
#> attr(,"guide")$d
#> [1] 3
#> 
#> attr(,"guide")$e
#> [1] 4
#> 
mimic(int, let)
#> [1] 0 1 2 3 4
#> attr(,"guide")
#> attr(,"guide")$a
#> [1] 0
#> 
#> attr(,"guide")$b
#> [1] 1
#> 
#> attr(,"guide")$c
#> [1] 2
#> 
#> attr(,"guide")$d
#> [1] 3
#> 
#> attr(,"guide")$e
#> [1] 4
#> 
mimic(fac, let)
#> [1] a b c d e
#> attr(,"guide")
#> attr(,"guide")[[1]]
#> [1] a
#> 
#> attr(,"guide")[[2]]
#> [1] b
#> 
#> attr(,"guide")[[3]]
#> [1] c
#> 
#> attr(,"guide")[[4]]
#> [1] d
#> 
#> attr(,"guide")[[5]]
#> [1] e
#> 
#> Levels: a b c d e
mimic(css, let)
#> [1] a b c d e
#> attr(,"codelist")
#> attr(,"codelist")[[1]]
#> [1] a
#> 
#> attr(,"codelist")[[2]]
#> [1] b
#> 
#> attr(,"codelist")[[3]]
#> [1] c
#> 
#> attr(,"codelist")[[4]]
#> [1] d
#> 
#> attr(,"codelist")[[5]]
#> [1] e
#> 
#> Levels: a b c d e
mimic(character(0))
#> character(0)
mimic(numeric(0))
#> numeric(0)
mimic(let, num)
#> [1] "a" "b" "c" "d" "e"
#> attr(,"guide")
#> attr(,"guide")$`0`
#> [1] "a"
#> 
#> attr(,"guide")$`1`
#> [1] "b"
#> 
#> attr(,"guide")$`2`
#> [1] "c"
#> 
#> attr(,"guide")$`3`
#> [1] "d"
#> 
#> attr(,"guide")$`4`
#> [1] "e"
#> 
mimic(fac, num)
#> [1] a b c d e
#> attr(,"guide")
#> attr(,"guide")$`0`
#> [1] a
#> 
#> attr(,"guide")$`1`
#> [1] b
#> 
#> attr(,"guide")$`2`
#> [1] c
#> 
#> attr(,"guide")$`3`
#> [1] d
#> 
#> attr(,"guide")$`4`
#> [1] e
#> 
#> Levels: a b c d e
mimic(css, num)
#> [1] a b c d e
#> attr(,"codelist")
#> attr(,"codelist")$`0`
#> [1] a
#> 
#> attr(,"codelist")$`1`
#> [1] b
#> 
#> attr(,"codelist")$`2`
#> [1] c
#> 
#> attr(,"codelist")$`3`
#> [1] d
#> 
#> attr(,"codelist")$`4`
#> [1] e
#> 
#> Levels: a b c d e
mimic(num, css)
#> [1] 0 1 2 3 4
#> attr(,"guide")
#> attr(,"guide")$a
#> [1] 0
#> 
#> attr(,"guide")$b
#> [1] 1
#> 
#> attr(,"guide")$c
#> [1] 2
#> 
#> attr(,"guide")$d
#> [1] 3
#> 
#> attr(,"guide")$e
#> [1] 4
#> 
mimic(let, css)
#> [1] "a" "b" "c" "d" "e"
#> attr(,"guide")
#> attr(,"guide")[[1]]
#> [1] "a"
#> 
#> attr(,"guide")[[2]]
#> [1] "b"
#> 
#> attr(,"guide")[[3]]
#> [1] "c"
#> 
#> attr(,"guide")[[4]]
#> [1] "d"
#> 
#> attr(,"guide")[[5]]
#> [1] "e"
#> 

util <- c('knife','fork','spoon')
util
#> [1] "knife" "fork"  "spoon"
factor(util)
#> [1] knife fork  spoon
#> Levels: fork knife spoon
classified(util)
#> [1] knife fork  spoon
#> attr(,"codelist")
#> attr(,"codelist")[[1]]
#> [1] fork
#> 
#> attr(,"codelist")[[2]]
#> [1] knife
#> 
#> attr(,"codelist")[[3]]
#> [1] spoon
#> 
#> Levels: fork knife spoon
mimic(util)
#> [1] "knife" "fork"  "spoon"
#> attr(,"guide")
#> attr(,"guide")[[1]]
#> [1] "fork"
#> 
#> attr(,"guide")[[2]]
#> [1] "knife"
#> 
#> attr(,"guide")[[3]]
#> [1] "spoon"
#> 
mimic(factor(util))
#> [1] knife fork  spoon
#> attr(,"guide")
#> attr(,"guide")[[1]]
#> [1] fork
#> 
#> attr(,"guide")[[2]]
#> [1] knife
#> 
#> attr(,"guide")[[3]]
#> [1] spoon
#> 
#> Levels: fork knife spoon
mimic(classified(util))
#> [1] knife fork  spoon
#> attr(,"codelist")
#> attr(,"codelist")[[1]]
#> [1] fork
#> 
#> attr(,"codelist")[[2]]
#> [1] knife
#> 
#> attr(,"codelist")[[3]]
#> [1] spoon
#> 
#> Levels: fork knife spoon

x <- data.frame(let, LET)
library(dplyr)
library(magrittr)
x %<>% mutate(let = mimic(let, LET), LET = mimic(LET))
str(x)
#> 'data.frame':    5 obs. of  2 variables:
#>  $ let: chr  "a" "b" "c" "d" ...
#>   ..- attr(*, "guide")=List of 5
#>   .. ..$ A: chr "a"
#>   .. ..$ B: chr "b"
#>   .. ..$ C: chr "c"
#>   .. ..$ D: chr "d"
#>   .. ..$ E: chr "e"
#>  $ LET: chr  "A" "B" "C" "D" ...
#>   ..- attr(*, "guide")=List of 5
#>   .. ..$ : chr "A"
#>   .. ..$ : chr "B"
#>   .. ..$ : chr "C"
#>   .. ..$ : chr "D"
#>   .. ..$ : chr "E"
```
