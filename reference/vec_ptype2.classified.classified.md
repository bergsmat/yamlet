# Find Common Type for classified, classified

Find common type for classified, classified.

## Usage

``` r
# S3 method for class 'classified.classified'
vec_ptype2(x, y, ...)
```

## Arguments

- x:

  classified

- y:

  classified

- ...:

  ignored

## Examples

``` r
library(vctrs)
#> 
#> Attaching package: ‘vctrs’
#> The following object is masked from ‘package:dplyr’:
#> 
#>     data_frame
library(magrittr)
c1 <- classified('a', levels = c('a','b')) %>% structure(label = 'c1')
c2 <- classified('b', levels = c('a','b')) %>% structure(label = 'c2')
c3 <- classified('c', levels = c('a','c')) %>% structure(label = 'c3')
f1 <- factor('a', levels = c('a','b')) %>% structure(label = 'f1')
f2 <- factor('b', levels = c('a','b')) %>% structure(label = 'f2')
f3 <- factor('c', levels = c('a','c')) %>% structure(label = 'f3')

vec_c(c1, c1) # combined data, same codelist
#> [1] a a
#> attr(,"label")
#> [1] c1
#> attr(,"codelist")
#> [1] a b
#> Levels: a b
vec_c(c1, c2) # combined data, same codelist
#> Warning: mismatched label attributes: ignoring 'c2' in favor of 'c1'
#> [1] a b
#> attr(,"label")
#> [1] c1
#> attr(,"codelist")
#> [1] a b
#> Levels: a b
vec_c(c2, c1) # reversed data, same codelist
#> Warning: mismatched label attributes: ignoring 'c1' in favor of 'c2'
#> [1] b a
#> attr(,"label")
#> [1] c2
#> attr(,"codelist")
#> [1] a b
#> Levels: a b

vec_c(c1, c3) # combined data, combined codelist
#> Warning: mismatched label attributes: ignoring 'c3' in favor of 'c1'
#> [1] a c
#> attr(,"label")
#> [1] c1
#> attr(,"codelist")
#> [1] a b c
#> Levels: a b c
vec_c(c3, c1) # reversed data, revised codelist
#> Warning: mismatched label attributes: ignoring 'c1' in favor of 'c3'
#> [1] c a
#> attr(,"label")
#> [1] c3
#> attr(,"codelist")
#> [1] a c b
#> Levels: a c b
vec_c(c1, f1) # matching levels: return classified
#> Warning: mismatched label attributes: ignoring 'f1' in favor of 'c1'
#> [1] a a
#> attr(,"label")
#> [1] c1
#> attr(,"codelist")
#> [1] a b
#> Levels: a b
vec_c(f1, c1) # matching levels: return classified
#> Warning: mismatched label attributes: ignoring 'c1' in favor of 'f1'
#> [1] a a
#> attr(,"label")
#> [1] f1
#> attr(,"codelist")
#> [1] a b
#> Levels: a b

vec_c(c1, f2) # matching levels: return classified
#> Warning: mismatched label attributes: ignoring 'f2' in favor of 'c1'
#> [1] a b
#> attr(,"label")
#> [1] c1
#> attr(,"codelist")
#> [1] a b
#> Levels: a b
vec_c(f2, c1) # matching levels: return classified
#> Warning: mismatched label attributes: ignoring 'c1' in favor of 'f2'
#> [1] b a
#> attr(,"label")
#> [1] f2
#> attr(,"codelist")
#> [1] a b
#> Levels: a b

vec_c(c1, f3) # mismatched levels: return factor
#> Warning: mismatched label attributes: ignoring 'f3' in favor of 'c1'
#> [1] a c
#> attr(,"label")
#> [1] c1
#> Levels: a b c
vec_c(f3, c1) # mismatched levels: return factor
#> Warning: mismatched label attributes: ignoring 'c1' in favor of 'f3'
#> [1] c a
#> attr(,"label")
#> [1] f3
#> attr(,"codelist")
#> [1] a b
#> Levels: a c b
```
