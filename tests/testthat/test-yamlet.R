test_that('yaml package result is stable',{
  expect_equal_to_reference(file = '001.rds', yaml::yaml.load('[ID: ]'))
  expect_equal_to_reference(file = '002.rds', yaml::yaml.load('ID: '))
  expect_equal_to_reference(file = '003.rds', yaml::yaml.load('[  ID: ]'))
  expect_equal_to_reference(file = '004.rds', yaml::yaml.load('[ ID: ]'))
  expect_equal_to_reference(file = '005.rds', yaml::yaml.load('RACE'))
  expect_equal_to_reference(file = '006.rds', yaml::yaml.load('RACE:'))
  expect_equal_to_reference(file = '007.rds', yaml::yaml.load('? RACE'))
  expect_equal_to_reference(file = '008.rds', yaml::yaml.load('[{RACE: }, ID: ]'))
  expect_equal_to_reference(file = '009.rds', yaml::yaml.load('[? RACE, ? ID]'))
  expect_equal_to_reference(file = '010.rds', yaml::yaml.load('[RACE: , ID: ]'))
  expect_equal_to_reference(file = '011.rds', yaml::yaml.load('RACE: [ race, [ foo: bar, hey: baz ]]'))
  expect_equal_to_reference(file = '012.rds', yaml::yaml.load('RACE: [ race, [ {foo: bar}, {hey: baz} ]]'))
  expect_equal_to_reference(file = '013.rds', yaml::yaml.load('RACE: [ race, [ {foo: bar}, hey: baz ]]'))
  expect_equal_to_reference(file = '014.rds', yaml::yaml.load('RACE: [ race, [ {foo: bar}, ? baz ]]'))
  expect_equal_to_reference(file = '015.rds', yaml::yaml.load('RACE: [ race, [ {foo: bar},  baz: ]]'))
  expect_equal_to_reference(file = '016.rds', yaml::yaml.load('RACE: [ race, [ {foo: bar}, hey: ]]'))
  expect_equal_to_reference(file = '017.rds', yaml::yaml.load('RACE: [ race, [ bar, baz ]]'))
  expect_equal_to_reference(file = '018.rds', yaml::yaml.load('RACE: [ race, [ {foo: bar} ]]'))
  expect_equal_to_reference(file = '019.rds', yaml::yaml.load('RACE: [ label: race, [ foo: bar ]]'))
  expect_equal_to_reference(file = '020.rds', yaml::yaml.load('RACE: [ label: race, [ foo: bar, hey: baz ]]'))
  expect_equal_to_reference(file = '021.rds', yaml::yaml.load('RACE: [ label: race, [ foo: bar, baz ]]'))
  expect_equal_to_reference(file = '022.rds', yaml::yaml.load('1'))                       # a length-one vector
  expect_equal_to_reference(file = '023.rds', yaml::yaml.load('a'))                       # a length-one vector
  expect_equal_to_reference(file = '024.rds', yaml::yaml.load('a:'))                      # a length-one named list
  expect_equal_to_reference(file = '025.rds', yaml::yaml.load('a: '))                     # a length-one named list
  expect_equal_to_reference(file = '026.rds', yaml::yaml.load('? a'))                     # a length-one named list
  expect_equal_to_reference(file = '027.rds', yaml::yaml.load('[ 0]'))                    # a length-one sequence, represented as a vector
  expect_equal_to_reference(file = '028.rds', yaml::yaml.load('[ 0, 1]'))                 # a sequence, represented as a vector
  expect_equal_to_reference(file = '029.rds', yaml::yaml.load('a: 0'))                    # a length-one mapping, represented as a length-one named list
  expect_equal_to_reference(file = '030.rds', yaml::yaml.load('[a: 0]'))                  # a list of named list * recursive
  expect_equal_to_reference(file = '031.rds', yaml::yaml.load('[a: 0, b: 1]'))            # a list of named lists *
  expect_equal_to_reference(file = '032.rds', yaml::yaml.load('[a: [0,1,2], b: 1]'))      # a list of lists *
  expect_equal_to_reference(file = '033.rds', yaml::yaml.load('[a: [0,1,2], 5 ]'))        # a list of one list and one int
  expect_equal_to_reference(file = '034.rds', yaml::yaml.load('[ [ [ [d: [0, 1, 2]]]]]')) # a list of named list * recursive
})

test_that('as_yamlet result is stable',{
  expect_equal_to_reference(file = '035.rds', as_yamlet('RACE: [white: 0, 1 ]'))         # surprising, but correct.
  expect_equal_to_reference(file = '036.rds', as_yamlet('RACE: [race, [white: 0, 1 ]]'))
  expect_equal_to_reference(file = '037.rds', as_yamlet('RACE: [ race, [ foo: bar ]]'))
  expect_equal_to_reference(file = '038.rds', ('RACE: [ label: race, [ foo: bar ]]'))    # must not be label, label; must not drop foo
  expect_equal_to_reference(file = '039.rds', as_yamlet('RACE: [ label: race, [ foo: bar, hey: baz ]]'))
})

test_that('unnest result is stable',{
# each element of a list
#  that is itself a list
#  and does not have a name
#  but has exactly one element
#  that DOES have a name
# should BE that element
# and HAVE that name
# recursively, starting at depth
expect_equal_to_reference(file = '040.rds', to_yamlet(unnest(yaml::yaml.load('1'))))
expect_equal_to_reference(file = '041.rds', to_yamlet(unnest(yaml::yaml.load('a'))))
expect_equal_to_reference(file = '042.rds', to_yamlet(unnest(yaml::yaml.load('a:'))))
expect_equal_to_reference(file = '043.rds', to_yamlet(unnest(yaml::yaml.load('a: '))))
expect_equal_to_reference(file = '044.rds', to_yamlet(unnest(yaml::yaml.load('? a'))))
expect_equal_to_reference(file = '045.rds', to_yamlet(unnest(yaml::yaml.load('[ 0]'))))
expect_equal_to_reference(file = '046.rds', to_yamlet(unnest(yaml::yaml.load('[ 0, 1]'))))
expect_equal_to_reference(file = '047.rds', to_yamlet(unnest(yaml::yaml.load('a: 0'))))
expect_equal_to_reference(file = '048.rds', to_yamlet(unnest(yaml::yaml.load('[a: 0]'))))
expect_equal_to_reference(file = '049.rds', to_yamlet(unnest(yaml::yaml.load('[a: 0, b: 1]'))))
expect_equal_to_reference(file = '050.rds', to_yamlet(unnest(yaml::yaml.load('[a: [0,1,2], b: 1]'))))
expect_equal_to_reference(file = '051.rds', to_yamlet(unnest(yaml::yaml.load('[a: [0,1,2], 5 ]') )))
expect_equal_to_reference(file = '052.rds', to_yamlet(unnest(yaml::yaml.load('[ [ [ [d: [0, 1, 2]]]]]'))))
})

test_that('more elements than keys gives warning',{
  expect_warning(as_yamlet('RACE: [label: race, guide: [white: 0, black: 1 ], categorical, 0]\nID: [1, 2, 3]'))
})

test_that('yamlet reads length-one character equivalently to vector',{
  expect_identical(as_yam(c('ID:','TIME:')),as_yam('ID:\nTIME:'))
})
test_that('uninformative nesting is removed',{
  expect_identical(names(unnest(yaml::yaml.load('[foo: 1, bar: 3]'))), c('foo','bar'))
})

test_that('key priority by source is explicit > object > argument > option > default',{
  expect_identical(names(as_yamlet('a: value')$a), 'label')
  old <- getOption('yamlet_default_keys')
  options(yamlet_default_keys = 'option')
  expect_identical(names(as_yamlet('a: value')$a), 'option')
  expect_identical(names(as_yamlet('a: value', default_keys = 'argument')$a), 'argument')
  expect_identical(names(as_yamlet('a: value\n_keys: object', default_keys = 'argument')$a), 'object')
  expect_identical(names(as_yamlet('a: [explicit: value]\n_keys: object', default_keys = 'argument')$a), 'explicit')
  options(yamlet_default_keys = old)
})

test_that('default decorations are equivalent to explicit requests',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
  expect_identical(decorate(file),decorate(file, meta = meta))
  expect_identical(decorate(file, meta = as_yamlet(meta)),decorate(file, meta = meta))
  expect_identical(decorate(file),decorate(file, meta = meta))
})

test_that('non-default import is equivalent',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  b <- decorate(file, coerce = TRUE)
  attr(b, 'source') <- NULL
  c <- decorate(
    file,
    fun = read.table,
    quote = "",
    as.is = TRUE,
    sep = ',',
    header = TRUE,
    na.strings = c('', '\\s', '.','NA'),
    strip.white = TRUE,
    check.names = FALSE,
    coerce = TRUE
  )
  expect_identical(b, c)
})

test_that('interconversion to and from storage is conservative',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
  expect_identical(
    readLines(meta),
    as.character(as_yamlet(decorate(file)))
  )
})

test_that('coersion to storage format is stable',{
  expect_equal_to_reference(file = '053.rds', to_yamlet(3))
  expect_equal_to_reference(file = '054.rds', to_yamlet(c(a = '4',b = '5.8')))
  expect_equal_to_reference(file = '055.rds', to_yamlet(c(a = 4,b = 5.8)))
  expect_equal_to_reference(file = '056.rds', to_yamlet(TRUE))
  expect_equal_to_reference(file = '057.rds', to_yamlet('foo'))
  expect_equal_to_reference(file = '058.rds', to_yamlet(c('a','b')))
  expect_equal_to_reference(file = '059.rds', to_yamlet(c(a = 'a',b = 'b')))
  expect_identical(to_yamlet(c(no = 'n', yes = 'y')),"[ 'no': 'n', 'yes': 'y' ]")

})

test_that('as.character.yamlet and as_yamlet.character are exactly inverse',{
  foo <- as_yamlet(system.file(package = 'yamlet', 'extdata','quinidine.yaml'))
  expect_identical(foo, as_yamlet(as.character(foo)))
  expect_identical(as.character(foo), as.character(as_yamlet(as.character(foo))))
})

