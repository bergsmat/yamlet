# retired tests
test_that('unnest result is stable',{
  # each element of a list
  #  that is itself a list
  #  and does not have a name
  #  but has exactly one element
  #  that DOES have a name
  # should BE that element
  # and HAVE that name
  # recursively, starting at depth
  library(magrittr)
  'a: [[d: [0, 1, 2]]]' %>% as_yamlet %>% to_yamlet # correct "[ a: [ label: [ d: [ 0, 1, 2 ]]]]"

  '[ [ [ [d: [0, 1, 2]]]]]' %>% yaml.load # expected
  '[ [ [ [d: [0, 1, 2]]]]]' %>% yaml.load %>% unnest # runaway parsimony
  '[ [ [ [d: [0, 1, 2]]]]]' %>% yaml.load %>% unnest %>% to_yamlet # runaway parsimony
  '[ [ [ [d: [0, 1, 2]]]]]' %>%
    yaml.load(
      TRUE,
      list(
        seq = parsimonious,
        map = function(x)lapply(x, unclass)
      )
    ) # restrained parsimony

  '[ [ [ [d: [0, 1, 2]]]]]' %>%
    yaml.load(
      TRUE,
      list(
        seq = parsimonious,
        map = function(x)lapply(x, unclass)
      )
    ) %>%
    to_yamlet # not collapsed if all singlets get brackets

  'a:  [[d: [0, 1, 2]]]' %>% as_yamlet %>% to_yamlet # correct "[ a: [ label: [ d: [ 0, 1, 2 ]]]]"
  'a: [[[d: [0, 1, 2]]]]' %>% as_yamlet %>% to_yamlet # correct "[ a: [ label: [ [ d: [ 0, 1, 2 ]]]]]"






  # consider:
  '[ [ [ [d: [0, 1, 2]]]]]' %>% yaml.load %>% unnest # runaway parsimony

  # this is runaway parsimony.
  # As of 0.7.8, we want to assume that the user had good reason for deep nesting.
  # We replace unnest() with parsimonious, which only operates at depth.
  # As a test, to_yamlet should give back the original, or something close to it.
  'RACE: [ label: race, [ foo: bar ]]' %>% as_yamlet
  'a: [ [ [ [d: [0, 1, 2]]]]]' %>% as_yamlet
  'a: [ [ [ [d: [0, 1, 2]]]]]' %>% as_yamlet %>% to_yamlet

  expect_identical(
    '1' %>% yaml.load %>% unnest,
    '1' %>% yaml.load(handlers = list(seq = parsimonious))
  )
  h <- list(seq = parsimonious, map = function(x)lapply(x, unclass))
  expect_equal_to_reference(file = '040.rds', '1' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '040.rds', '1' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '041.rds', 'a' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '041.rds', 'a' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '042.rds', 'a:' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '042.rds', 'a:' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '043.rds', 'a: ' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '043.rds', 'a: ' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '044.rds', '? a' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '044.rds', '? a' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '045.rds', '[ 0]' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '045.rds', '[ 0]' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '046.rds', '[ 0, 1]' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '046.rds', '[ 0, 1]' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '047.rds', 'a: 0' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '047.rds', 'a: 0' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '048.rds', '[a: 0]' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '048.rds', '[a: 0]' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '049.rds', '[a: 0, b: 1]' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '049.rds', '[a: 0, b: 1]' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '050.rds', '[a: [0,1,2], b: 1]' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '050.rds', '[a: [0,1,2], b: 1]' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  expect_equal_to_reference(file = '051.rds', '[a: [0,1,2], 5 ]' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '051.rds', '[a: [0,1,2], 5 ]' %>% yaml.load( TRUE, h ) %>% to_yamlet)

  # here we see runaway parsimony using unnested, but restrained parsimony using parsimonious:
  expect_equal_to_reference(file = '052.rds',  '[ [ [ [d: [0, 1, 2]]]]]' %>% yaml.load %>% unnest %>% to_yamlet)
  expect_equal_to_reference(file = '052b.rds', '[ [ [ [d: [0, 1, 2]]]]]' %>% yaml.load( TRUE, h ) %>% to_yamlet)

})

test_that('uninformative nesting is removed',{
  expect_identical(names(unnest(yaml.load('[foo: 1, bar: 3]'))), c('foo','bar'))
})

# test_that('user can specify unit instead of units',{
#   a <- 'CONC: [ concentration, ng/mL ]' %>% as_yamlet %>% explicit_guide(default = 'unit')
#   expect_identical(names(a$CONC), c('label','unit'))
# })

test_that('labels parsed and unparsed, with and without units, display correctly',{
  library(magrittr)
  library(ggplot2)
  Theoph %<>% as.data.frame
  Theoph %<>% as_decorated
  options(yamlet_enclose = c('[',']'))
  Theoph$conc %<>% structure(label = 'CO[2] concentration', units = 'µg/m^2')
  Theoph$Time %<>% structure(label = 'time since administration', units = 'h')
  options(yamlet_label_parse = FALSE) # no longer used anywhere
  ggplot(data = Theoph, aes(x = Time, y = conc)) + geom_point()
  options(yamlet_label_parse = TRUE)
  ggplot(data = Theoph, aes(x = Time, y = conc)) + geom_point()
})
