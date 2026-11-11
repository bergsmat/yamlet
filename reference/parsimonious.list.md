# Reduce A List to its Simplest Sufficient Version

Reduces a list to its simplest sufficient version. Used internally with
[`yaml.load`](https://yaml.r-lib.org/reference/yaml.load.html) as a
custom handler for objects of type 'seq'. Consider:
`str(yaml.load('[a: 1, b: 2]'))`. The result is technically correct. By
default, the parser returns a sequence of two maps. Not reducible to a
base type, The sequence is an anonymous list. The maps themselves are
named lists. In the special case that all elements are of length one,
this structure can be collapsed without semantic loss to a named list.
More generally, if an anonymous list consists entirely of length one
members, those members which are lists (but not already parsimonious
lists) can be replaced with their first elements; the list becomes named
if any of those elements has a name. In that case, any elements without
names get the name ” (empty string).

## Usage

``` r
# S3 method for class 'list'
parsimonious(x, ...)
```

## Arguments

- x:

  object of dispatch

- ...:

  passed arguments

## Value

list

## See also

Other parsimonious:
[`parsimonious()`](https://bergsmat.github.io/yamlet/reference/parsimonious.md),
[`parsimonious.default()`](https://bergsmat.github.io/yamlet/reference/parsimonious.default.md)

## Examples

``` r
library(magrittr)
library(yaml)
# Parsimonious:
'[a: 1, b: 2]' %>% yaml.load
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
'[a: 1, b: 2]' %>% yaml.load(handlers = list(seq = parsimonious))
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        

# No effect on vector types:
'[1, 2]' %>% yaml.load
#> [1] 1 2
'[1, 2]' %>% yaml.load(handlers = list(seq = parsimonious))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        

# Respects mixed-length vector types:
'RACE: [ race, [white, black, asian ]]' %>% yaml.load
#> $RACE
#> $RACE[[1]]
#> [1] "race"
#> 
#> $RACE[[2]]
#> [1] "white" "black" "asian"
#> 
#> 
'RACE: [ race, [white, black, asian ]]' %>% yaml.load(handlers = list(seq = parsimonious))
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

# Anonymous elements get a blank name:
'[a: 1, 2]' %>% yaml.load %>% sapply(names)
#> [[1]]
#> [1] "a"
#> 
#> [[2]]
#> NULL
#> 
'[a: 1, 2]' %>% yaml.load(handlers = list(seq = parsimonious)) %>% names
#> [1] "a" "" 

# Also works for sequence of length one:
'[a: 1]' %>% yaml.load
#> [[1]]
#> [[1]]$a
#> [1] 1
#> 
#> 
'[a: 1]' %>% yaml.load(handlers = list(seq = parsimonious))
#> $a
#> [1] 1
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        

# Works for NULL:
yaml.load('-')
#> [[1]]
#> NULL
#> 
yaml.load('-', handlers = list(seq = parsimonious))
#> [[1]]
#> NULL
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        

# And for empty list:
yaml.load('[]')
#> list()
yaml.load('[]', handlers = list(seq = parsimonious))
#> list()
#> attr(,"class")
#> [1] "parsimonious" "list"        

# Limited to first (most deeply nested) encounter:
'[[[a: 1]]]' %>% yaml.load
#> [[1]]
#> [[1]][[1]]
#> [[1]][[1]][[1]]
#> [[1]][[1]][[1]]$a
#> [1] 1
#> 
#> 
#> 
#> 
'[[[a: 1]]]' %>% yaml.load(handlers = list(seq = parsimonious))
#> [[1]]
#> [[1]][[1]]
#> [[1]][[1]]$a
#> [1] 1
#> 
#> 
#> 
#> attr(,"class")
#> [1] "parsimonious" "list"        

# Works for mixed-depth nesting:
'ITEM: [ label: item, [ foo: bar, hey: baz ]]' %>% yaml.load
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
'ITEM: [ label: item, [ foo: bar, hey: baz ]]' %>% yaml.load(handlers = list(seq = parsimonious))
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
