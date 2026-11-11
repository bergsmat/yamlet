# Reduce Something to its Simplest Sufficient Version

Reduces something to its simplest sufficient version. Generic, with
method
[`parsimonious.list`](https://bergsmat.github.io/yamlet/reference/parsimonious.list.md).

## Usage

``` r
parsimonious(x, ...)
```

## Arguments

- x:

  object of dispatch

- ...:

  ignored

## See also

Other parsimonious:
[`parsimonious.default()`](https://bergsmat.github.io/yamlet/reference/parsimonious.default.md),
[`parsimonious.list()`](https://bergsmat.github.io/yamlet/reference/parsimonious.list.md)

## Examples

``` r
example(parsimonious.list)
#> 
#> prsmn.> library(magrittr)
#> 
#> prsmn.> library(yaml)
#> 
#> prsmn.> # Parsimonious:
#> prsmn.> '[a: 1, b: 2]' %>% yaml.load
#> [[1]]
#> [[1]]$a
#> [1] 1
#> 
#> 
#> [[2]]
#> [[2]]$b
#> [1] 2
#> 
#> 
#> 
#> prsmn.> '[a: 1, b: 2]' %>% yaml.load(handlers = list(seq = parsimonious))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        
#> 
#> prsmn.> # No effect on vector types:
#> prsmn.> '[1, 2]' %>% yaml.load
#> [1] 1 2
#> 
#> prsmn.> '[1, 2]' %>% yaml.load(handlers = list(seq = parsimonious))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        
#> 
#> prsmn.> # Respects mixed-length vector types:
#> prsmn.> 'RACE: [ race, [white, black, asian ]]' %>% yaml.load
#> $RACE
#> $RACE[[1]]
#> [1] "race"
#> 
#> $RACE[[2]]
#> [1] "white" "black" "asian"
#> 
#> 
#> 
#> prsmn.> 'RACE: [ race, [white, black, asian ]]' %>% yaml.load(handlers = list(seq = parsimonious))
#> $RACE
#> [[1]]
#> [1] "race"
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] "white"
#> 
#> [[2]][[2]]
#> [1] "black"
#> 
#> [[2]][[3]]
#> [1] "asian"
#> 
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        
#> 
#> 
#> prsmn.> # Anonymous elements get a blank name:
#> prsmn.> '[a: 1, 2]' %>% yaml.load %>% sapply(names)
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> NULL
#> 
#> 
#> prsmn.> '[a: 1, 2]' %>% yaml.load(handlers = list(seq = parsimonious)) %>% names
#> [1] "a" "" 
#> 
#> prsmn.> # Also works for sequence of length one:
#> prsmn.> '[a: 1]' %>% yaml.load
#> [[1]]
#> [[1]]$a
#> [1] 1
#> 
#> 
#> 
#> prsmn.> '[a: 1]' %>% yaml.load(handlers = list(seq = parsimonious))
#> $a
#> [1] 1
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        
#> 
#> prsmn.> # Works for NULL:
#> prsmn.> yaml.load('-')
#> [[1]]
#> NULL
#> 
#> 
#> prsmn.> yaml.load('-', handlers = list(seq = parsimonious))
#> [[1]]
#> NULL
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        
#> 
#> prsmn.> # And for empty list:
#> prsmn.> yaml.load('[]')
#> list()
#> 
#> prsmn.> yaml.load('[]', handlers = list(seq = parsimonious))
#> list()
#> attr(,"class")
#> [1] "parsimonious" "list"        
#> 
#> prsmn.> # Limited to first (most deeply nested) encounter:
#> prsmn.> '[[[a: 1]]]' %>% yaml.load
#> [[1]]
#> [[1]][[1]]
#> [[1]][[1]][[1]]
#> [[1]][[1]][[1]]$a
#> [1] 1
#> 
#> 
#> 
#> 
#> 
#> prsmn.> '[[[a: 1]]]' %>% yaml.load(handlers = list(seq = parsimonious))
#> [[1]]
#> [[1]][[1]]
#> [[1]][[1]]$a
#> [1] 1
#> 
#> 
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        
#> 
#> prsmn.> # Works for mixed-depth nesting:
#> prsmn.> 'ITEM: [ label: item, [ foo: bar, hey: baz ]]' %>% yaml.load
#> $ITEM
#> $ITEM[[1]]
#> $ITEM[[1]]$label
#> [1] "item"
#> 
#> 
#> $ITEM[[2]]
#> $ITEM[[2]][[1]]
#> $ITEM[[2]][[1]]$foo
#> [1] "bar"
#> 
#> 
#> $ITEM[[2]][[2]]
#> $ITEM[[2]][[2]]$hey
#> [1] "baz"
#> 
#> 
#> 
#> 
#> 
#> prsmn.> 'ITEM: [ label: item, [ foo: bar, hey: baz ]]' %>% yaml.load(handlers = list(seq = parsimonious))
#> $ITEM
#> $label
#> [1] "item"
#> 
#> [[2]]
#> [[2]]$foo
#> [1] "bar"
#> 
#> [[2]]$hey
#> [1] "baz"
#> 
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        
#> 
```
