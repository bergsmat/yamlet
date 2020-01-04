# library(yaml)
# yaml.load('RACE: [ race, [ foo: bar, hey: baz ]]')
# yaml.load('RACE: [ race, [ {foo: bar}, {hey: baz} ]]')
# yaml.load('RACE: [ race, [ {foo: bar}, hey: baz ]]')
# yaml.load('RACE: [ race, [ {foo: bar}, ? baz ]]')
# yaml.load('RACE: [ race, [ {foo: bar},  baz: ]]')
# yaml.load('RACE: [ race, [ {foo: bar}, hey: ]]')
# yaml.load('RACE: [ race, [ bar, baz ]]')
# yaml.load('RACE: [ race, [ {foo: bar} ]]')
# yaml.load('RACE:')
# yaml.load('RACE')
# yaml.load('? RACE')
# yaml.load('[{RACE: }, ID: ]')
# yaml.load('[? RACE, ? ID]')
# yaml.load('[RACE: , ID: ]')
# yaml.load('[ID: ]')
# yaml.load('ID: ')
# yaml.load('[  baz: ]')
# yaml.load('[ baz: ]')
# as_yamlet('RACE: [white: 0, 1 ]') # surprising, but correct.
# as_yamlet('RACE: [race, [white: 0, 1 ]]')
# as_yamlet('RACE: [ race, [ foo: bar ]]')
# as_yamlet('RACE: [ label: race, [ foo: bar ]]') # must not be label, label; must not drop foo
# as_yamlet('RACE: [ label: race, [ foo: bar, hey: baz ]]')
# yaml.load('RACE: [ label: race, [ foo: bar ]]')
# yaml.load('RACE: [ label: race, [ foo: bar, hey: baz ]]')
# yaml.load('RACE: [ label: race, [ foo: bar, baz ]]')
# # each element of a list
# #  that is itself a list
# #  and does not have a name
# #  but has exactly one element
# #  that DOES have a name
# # should be that element
# # and have that name
# # recursively bottom-up
# yaml.load('1') # a length-one vector
# yaml.load('a') # a length-one vector
# yaml.load('a:') # a length-one named list
# yaml.load('a: ') # a length-one named list
# yaml.load('? a') # a length-one named list
# yaml.load('[ 0]') # a length-one sequence, represented as a vector
# yaml.load('[ 0, 1]') # a sequence, represented as a vector
# yaml.load('a: 0') # a length-one mapping, represented as a length-one named list
# yaml.load('[a: 0]')  # a list of named list * recursive
# yaml.load('[a: 0, b: 1]') # a list of named lists *
# yaml.load('[a: [0,1,2], b: 1]') # a list of lists *
# yaml.load('[a: [0,1,2], 5 ]') # a list of one list and one int
# yaml.load('[ [ [ [d: [0, 1, 2]]]]]') # a list of named list * recursive
# library(magrittr)
# yaml.load('1') %>% unnest %>% to_yamlet
# yaml.load('a') %>% unnest %>% to_yamlet
# yaml.load('a:') %>% unnest %>% to_yamlet
# yaml.load('a: ') %>% unnest %>% to_yamlet
# yaml.load('? a') %>% unnest %>% to_yamlet
# yaml.load('[ 0]') %>% unnest %>% to_yamlet
# yaml.load('[ 0, 1]') %>% unnest %>% to_yamlet
# yaml.load('a: 0') %>% unnest %>% to_yamlet
# yaml.load('[a: 0]') %>% unnest %>% to_yamlet
# yaml.load('[a: 0, b: 1]') %>% unnest %>% to_yamlet
# yaml.load('[a: [0,1,2], b: 1]') %>% unnest %>% to_yamlet
# yaml.load('[a: [0,1,2], 5 ]')  %>% unnest %>% to_yamlet
# yaml.load('[ [ [ [d: [0, 1, 2]]]]]') %>% unnest %>% to_yamlet
