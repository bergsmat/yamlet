library(testthat)
library(magrittr)
library(yaml)
test_that('yaml package result is stable',{
  expect_equal_to_reference(file = '001.rds', yaml.load('[ID: ]'))
  expect_equal_to_reference(file = '002.rds', yaml.load('ID: '))
  expect_equal_to_reference(file = '003.rds', yaml.load('[  ID: ]'))
  expect_equal_to_reference(file = '004.rds', yaml.load('[ ID: ]'))
  expect_equal_to_reference(file = '005.rds', yaml.load('RACE'))
  expect_equal_to_reference(file = '006.rds', yaml.load('RACE:'))
  expect_equal_to_reference(file = '007.rds', yaml.load('? RACE'))
  expect_equal_to_reference(file = '008.rds', yaml.load('[{RACE: }, ID: ]'))
  expect_equal_to_reference(file = '009.rds', yaml.load('[? RACE, ? ID]'))
  expect_equal_to_reference(file = '010.rds', yaml.load('[RACE: , ID: ]'))
  expect_equal_to_reference(file = '011.rds', yaml.load('RACE: [ race, [ foo: bar, hey: baz ]]'))
  expect_equal_to_reference(file = '012.rds', yaml.load('RACE: [ race, [ {foo: bar}, {hey: baz} ]]'))
  expect_equal_to_reference(file = '013.rds', yaml.load('RACE: [ race, [ {foo: bar}, hey: baz ]]'))
  expect_equal_to_reference(file = '014.rds', yaml.load('RACE: [ race, [ {foo: bar}, ? baz ]]'))
  expect_equal_to_reference(file = '015.rds', yaml.load('RACE: [ race, [ {foo: bar},  baz: ]]'))
  expect_equal_to_reference(file = '016.rds', yaml.load('RACE: [ race, [ {foo: bar}, hey: ]]'))
  expect_equal_to_reference(file = '017.rds', yaml.load('RACE: [ race, [ bar, baz ]]'))
  expect_equal_to_reference(file = '018.rds', yaml.load('RACE: [ race, [ {foo: bar} ]]'))
  expect_equal_to_reference(file = '019.rds', yaml.load('RACE: [ label: race, [ foo: bar ]]'))
  expect_equal_to_reference(file = '020.rds', yaml.load('RACE: [ label: race, [ foo: bar, hey: baz ]]'))
  expect_equal_to_reference(file = '021.rds', yaml.load('RACE: [ label: race, [ foo: bar, baz ]]'))
  expect_equal_to_reference(file = '022.rds', yaml.load('1'))                       # a length-one vector
  expect_equal_to_reference(file = '023.rds', yaml.load('a'))                       # a length-one vector
  expect_equal_to_reference(file = '024.rds', yaml.load('a:'))                      # a length-one named list
  expect_equal_to_reference(file = '025.rds', yaml.load('a: '))                     # a length-one named list
  expect_equal_to_reference(file = '026.rds', yaml.load('? a'))                     # a length-one named list
  expect_equal_to_reference(file = '027.rds', yaml.load('[ 0]'))                    # a length-one sequence, represented as a vector
  expect_equal_to_reference(file = '028.rds', yaml.load('[ 0, 1]'))                 # a sequence, represented as a vector
  expect_equal_to_reference(file = '029.rds', yaml.load('a: 0'))                    # a length-one mapping, represented as a length-one named list
  expect_equal_to_reference(file = '030.rds', yaml.load('[a: 0]'))                  # a list of named list * recursive
  expect_equal_to_reference(file = '031.rds', yaml.load('[a: 0, b: 1]'))            # a list of named lists *
  expect_equal_to_reference(file = '032.rds', yaml.load('[a: [0,1,2], b: 1]'))      # a list of lists *
  expect_equal_to_reference(file = '033.rds', yaml.load('[a: [0,1,2], 5 ]'))        # a list of one list and one int
  expect_equal_to_reference(file = '034.rds', yaml.load('[ [ [ [d: [0, 1, 2]]]]]')) # a list of named list * recursive
})

test_that('as_yamlet result is stable',{
  expect_equal_to_reference(file = '035.rds', as_yamlet('RACE: [white: 0, 1 ]'))         # surprising, but correct.
  expect_equal_to_reference(file = '036.rds', as_yamlet('RACE: [race, [white: 0, 1 ]]'))
  expect_equal_to_reference(
    file = '037.rds', # @ 0.7.7 this was runaway parsimony
    as_yamlet(
      'RACE: [ race, [ foo: bar ]]' # @ 0.7.8 we see (correctly) label, guide not label, foo
    )
  )
})

test_that('as_yamlet result is still stable',{

  yaml.load('foo: bar')
  yaml.load('foo: bar', handlers = list(seq = parsimonious))
  yaml.load('[foo: bar]')
  yaml.load('[foo: bar]', handlers = list(seq = parsimonious))
  yaml.load('RACE: [ label: race, [ foo: bar ]]')
  yaml.load('RACE: [ label: race, [ foo: bar ]]', handlers = list(seq = parsimonious))
  as_yamlet('RACE: [ label: race, [ foo: bar ]]')
  as_yamlet('RACE: [ label: race, [ foo: bar, hey: baz ]]')
  expect_equal_to_reference(file = '038.rds',as_yamlet('RACE: [ label: race, [ foo: bar ]]'))    # must not be label, label; must not drop foo
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

test_that('more elements than keys gives warning',{
  expect_warning(as_yamlet('RACE: [label: race, guide: [white: 0, black: 1 ], categorical, 0]\nID: [1, 2, 3]'))
})

test_that('yamlet reads length-one character equivalently to vector',{
  expect_identical(as_yam(c('ID:','TIME:')),as_yam('ID:\nTIME:'))
})

test_that('uninformative nesting is removed',{
  expect_identical(names(unnest(yaml.load('[foo: 1, bar: 3]'))), c('foo','bar'))
})

test_that('key priority by source is explicit > object > argument > option > default',{
  expect_identical(names(as_yamlet('a: value')$a), 'label')
  old <- getOption('yamlet_default_keys')
  options(yamlet_default_keys = 'option')
  expect_identical(names(as_yamlet('a: value')$a), 'option')
  expect_identical(names(as_yamlet('a: value', default_keys = 'argument')$a), 'argument')



  as_yamlet('a: ')

  expect_identical(
    names(as_yamlet('a: value\n_keys: object', default_keys = 'argument')$a),
    'object'
  )







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
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  b <- decorate(file, source = FALSE) %>% resolve
  c <- decorate(
    file,
    read = read.table,
    quote = "",
    as.is = TRUE,
    sep = ',',
    header = TRUE,
    na.strings = c('', '\\s', '.','NA'),
    strip.white = TRUE,
    check.names = FALSE
  ) %>% resolve
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

test_that('as.character.yamlet and as_yamlet.character are reciprocal',{
  foo <- as_yamlet(system.file(package = 'yamlet', 'extdata','quinidine.yaml'))
  expect_identical(foo, as_yamlet(as.character(foo)))
  expect_identical(as.character(foo), as.character(as_yamlet(as.character(foo))))
})

test_that('read_yamlet and write_yamlet are reciprocal',{
  foo <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
  file <- tempfile()
  write_yamlet(read_yamlet(foo), con = file)
  expect_identical(readLines(file), readLines(foo))
})

test_that('decorate will not overwrite existing attributes',{
  foo <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(foo)
  expect_warning(y <- decorate(x))
  expect_identical(x, y)
})

test_that('decorate ignores anonymous attributes',{
  foo <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  library(csv)
  x <- as.csv(foo)
  expect_warning(meta <- as_yamlet('time: [ a, b, c]'))
  expect_warning(decorate(x, meta = meta))
})

test_that('io_yamlet methods are reciprocal with default or modified arguments',{
  foo <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
  out <- file.path(tempdir(), 'out.yaml')
  x <- io_yamlet(foo)
  expect_identical(x,io_yamlet(io_yamlet(x, out)))
  expect_identical(
    x,
    io_yamlet(
      fileEncoding = 'UTF-8', # read
      eol = '\n',
      #default_keys = c('foo','bar'),
      io_yamlet(
        fileEncoding = 'UTF-8', # write
        eol = '\r',
        default_keys = c('foo','bar'),
        x,
        out
      )
    )
  )
})

test_that('io_table methods are reciprocal with default or modified arguments',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  out <- file.path(tempdir(), 'out.tab')
  foo <- io_table(x, out)
  expect_identical(out, foo)
  y <- io_table(foo, as.is = TRUE)
  attr(x, 'source') <- NULL
  rownames(x) <- NULL
  rownames(y) <- NULL
  expect_identical(x, y) # lossless 'round-trip'

  io_table(x, out, sep = ',' , na = '.') #, fileEncoding = 'UTF-16')
  y <- io_table(out, as.is = TRUE, sep = ',', na.strings = '.') #, fileEncoding = 'UTF-16')
  rownames(y) <- NULL
  expect_identical(x, y) # lossless 'round-trip'
})

test_that('io_csv methods are reciprocal with default or modified arguments',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  attr(x, 'source') <- NULL
  out <- file.path(tempdir(), 'out.tab')
  foo <- io_csv(x, out)
  expect_identical(out, foo)
  expect_identical(readLines(file), readLines(out))
  y <- io_csv(out)
  rownames(x) <- NULL
  rownames(y) <- NULL
  attr(x, 'source') <- NULL
  attr(y, 'source') <- NULL
  expect_identical(x, y) # lossless 'round-trip'

  io_csv(x, out, quote = TRUE, na = 'NA', eol = '\r') #, fileEncoding = 'UTF-16')
  y <- io_csv(out, na.strings = 'NA') # , fileEncoding = 'UTF-16')
  attr(y, 'source') <- NULL
  expect_identical(x, y) # lossless 'round-trip'

})

test_that('class attributes are excluded from storage by default',{
  expect_false('class' %in% decorations(Theoph)$Subject)
})

test_that('yamlet package writes proper yaml with non-default keys',{
  out <- file.path(tempdir(), 'out.yaml')
  expect_silent(write_yamlet('ID: identifier', con = out))
  expect_silent(write_yamlet('ID: identifier', con = out, default_keys = c('foo','bar')))
  expect_warning(write_yamlet('ID: identifier', con = out, default_keys = character(0)))

  foo <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')
  x <- io_yamlet(foo) # read
  y <- as_yam(x[1], default_keys = c('foo','bar'))
  io_yamlet(x, out, default_keys = c('foo','bar')) # write
  expect_silent(io_yamlet(out)) # read

  z <- as_yamlet('ID: identifier')
  expect_identical(as.character(z), 'ID: identifier')
  expect_identical(
    as.character(z, default_keys = c('foo','bar'))[[1]],
    'ID: [ label: identifier ]'
  )
})

test_that('dplyr filter does not drop attributes',{
 # not okay in 3.6.1: filter drops label on factors
  library(dplyr)
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- file %>% decorate %>% resolve
  x %$% Heart %>% attributes %>% names
  expect_true(
    setequal(
      x %>% filter(!is.na(conc)) %$% Heart %>% attributes %>% names,
      c('levels','class','label','codelist')
    )
  )
})

test_that('print.dg treats variable as categorical if guide has length > 1',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  library(ggplot2)
  library(dplyr)
  library(magrittr)
  file %>% decorate %>% filter(!is.na(conc)) %>%
  ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()
})

test_that('print.dg uses conditional labels and guides',{
  file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
  file %>% decorate %>%
  filter(event == 'conc') %>%
  ggplot(aes(x = time, y = value, color = ApgarInd)) + geom_point()
})

test_that('io_table accepts nuisance arguments without error',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  out <- file.path(tempdir(), 'out.tab')
  expect_silent(foo <- io_table(x, out, foo = 'bar'))
  expect_silent(y <- io_table(foo, as.is = TRUE, foo = 'bar'))
})

test_that('io_csv accepts nuisance arguments without error',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  out <- file.path(tempdir(), 'out.csv')
  expect_silent(foo <- io_csv(x, out, foo = 'bar'))
  expect_silent(y <- io_csv(foo, as.is = TRUE, foo = 'bar'))
})

test_that('explicit_guide recognizes encodings, units, formats, and codelists',{
  library(magrittr)
  a <- 'CONC: [ concentration, ng/mL ]' %>% as_yamlet %>% explicit_guide
  b <- 'RACE: [ subject race, [ Caucasian, Latin, Black ]]' %>% as_yamlet %>% explicit_guide
  c <- 'RACE: [ subject race, //Caucasian//Latin//Black// ]' %>% as_yamlet %>% explicit_guide
  d <- 'DATE: [ date, "%Y-%m-%d" ]' %>% as_yamlet %>% explicit_guide
  e <- c(
    names(a[[1]])[[2]],
    names(b[[1]])[[2]],
    names(c[[1]])[[2]],
    names(d[[1]])[[2]]
  )
  expect_identical(e, c('units','codelist','encoding','format'))

 x <- data.frame(
  ID = 1,
  CONC = 1,
  RACE = 1,
  SEX = 1,
  DATE = 1
 )

 x$ID   %<>% structure(label = 'subject identifier')
 x$CONC %<>% structure(label = 'concentration', guide = 'ng/mL')
 x$RACE %<>% structure(label = 'race', guide = list(white = 0, black = 1, asian = 2))
 x$SEX  %<>% structure(label = 'sex', guide = list(female = 0, male = 1))
 x$DATE %<>% structure(label = 'date', guide = '%Y-%m-%d')
 expect_identical(
   x %>% explicit_guide %>% as_yamlet %>% lapply(names) %>% unlist %>% as.character,
   c('label','label','units','label','codelist','label','codelist','label','format')
 )
})

test_that('classified() creates class factor and removes attribute codelist',{
 library(magrittr)
 file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
 x <- decorate(file)
 x %<>% explicit_guide %>% classified %>% as_yamlet(exclude_attr = NULL)
 expect_identical(
   x$Creatinine %>% names,
   c('levels','class','label','codelist')
 )
 expect_true('factor' %in% x$Heart$class)
})

# test_that('user can specify unit instead of units',{
#   a <- 'CONC: [ concentration, ng/mL ]' %>% as_yamlet %>% explicit_guide(default = 'unit')
#   expect_identical(names(a$CONC), c('label','unit'))
# })

test_that('resolve correctly classifies conditional elements',{
  skip_if_not( l10n_info()$`UTF-8` )
  skip_if(
    .Platform$OS.type == "unix" && Encoding(enc2native("\U00B5")) != "UTF-8",
    "Skipping non-ASCII path tests on UTF-8 Unix system"
  )
  library(magrittr)
  library(dplyr)
  file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
  x <- decorate(file)
  x %>% as_yamlet
  x %>% explicit_guide %>% as_yamlet
  x %>% select(value) %>% explicit_guide %>% as_yamlet
  x %>% explicit_guide %>% classified %>% as_yamlet
  a <- x %>% resolve %>% as_yamlet
  expect_true(setequal(names(a$value), c('label','units')))
})

test_that('resolve correctly classifies factors',{
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  expect_true(file %>% decorate %>% resolve %$% Heart %>% inherits('factor'))
})

test_that('filter, select, mutate, group_by, arrange, summarize and [ do not drop subclass decorated',{
  library(dplyr)
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_identical('decorated', x %>% class %>% `[[`(1))
  expect_identical('decorated', x %>% filter(!is.na(conc)) %>% class %>% `[[`(1))
  expect_identical(
    'decorated',
    x %>%
      group_by(Subject) %>%
      mutate(mxt = max(time)) %>%
      class %>% `[[`(1)
  )
  expect_identical(
    'decorated',
    x %>%
      select(Subject:interval) %>%
      class %>% `[[`(1)
  )
  expect_identical(
    'decorated',
    x %>%
      summarize(mx = max(time)) %>%
      class %>% `[[`(1)
  )
  expect_identical(
    'decorated',
    x %>%
      arrange(Subject,time) %>%
      class %>% `[[`(1)
  )
  expect_identical(
    'decorated',
    class(x[1:5, 1:3])[[1]]
  )

})

test_that('conditionalize errors on mixed quotes',{
  library(dplyr)
  library(magrittr)
  x <- data.frame(column = 'foo', test = "can't\"", value = 1)
  expect_error(
    x %>% conditionalize(column, label, test, value) %>% as_yamlet
  )
})

test_that('conditionalize alternates single and double quotes',{
  library(dplyr)
  library(magrittr)
  x <- data.frame(
    stringsAsFactors = FALSE,
    column = 'foo',
    test = c('"cant"',"can't"),
    value = 1
  )
  expect_identical(
    x %>% conditionalize(column, label, test, value) %>%
      as_yamlet %$% column %$% label %>% names,
    c( "test == '\"cant\"'", "test == \"can't\"")
  )
})

test_that('conditionalize does not quote numerics',{
  library(dplyr)
  library(magrittr)
  x <- data.frame(
    column = 1,
    test = 2,
    value = 3
  )
  expect_identical(
    x %>% conditionalize(column, label, test, value) %>%
      as_yamlet %$% column %$% label %>% names,
    "test == 2"
  )
})

test_that('conitionalize handles factors like character',{
  library(dplyr)
  library(magrittr)
  x <- data.frame(
    stringsAsFactors = TRUE,
    column = 'foo',
    test = c('"cant"',"can't"),
    value = 1
  )
  expect_identical(
    x %>% conditionalize(column, label, test, value) %>%
      as_yamlet %$% column %$% label %>% names,
    c( "test == '\"cant\"'", "test == \"can't\"")
  )
})

test_that('subset classified does not drop label', {
 a <- classified(factor(letters))
 attr(a, 'label') <- 'foo'
 a <- a[1:3]
 expect_identical(attr(a,'label'), 'foo')
})

test_that('is_parseable distinguishes udunits from non-udunits',{
  expect_identical(
    is_parseable(c('kg/m2','kg/m^2','foo','kg.m/s2')),
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that('is_pareseable is vectorized',{
  expect_identical(
    is_parseable(c('kg/m2','kg/m^2','foo','kg.m/s2')),
    c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that('micro symbol is_pareseable',{
  # https://github.com/rstudio/httpuv/issues/264
  # https://github.com/rstudio/httpuv/commit/32ba7d34e9d0895552db8346cea8acbed7a74022
  skip_if_not( l10n_info()$`UTF-8` )
  skip_if(
    .Platform$OS.type == "unix" && Encoding(enc2native("\U00B5")) != "UTF-8",
    "Skipping non-ASCII path tests on UTF-8 Unix system"
  )
  expect_true(is_parseable('µg/L'))
})

test_that('is_parseable respects locally-defined units',{
  library(units)
  expect_false(is_parseable('foo'))
  install_unit('foo')
  expect_true(is_parseable('foo'))
  remove_unit('foo')
  expect_false(is_parseable('foo'))
})

test_that('labels parsed and unparsed, with and without units, display correctly',{
  library(magrittr)
  library(ggplot2)
  Theoph %<>% as.data.frame
  Theoph %<>% as_decorated
  options(yamlet_enclose = c('[',']'))
  Theoph$conc %<>% structure(label = 'CO[2] concentration', units = 'µg/m^2')
  Theoph$Time %<>% structure(label = 'time since administration', units = 'h')
  ggplot(data = Theoph, aes(x = Time, y = conc)) + geom_point()
  options(yamlet_label_parse = TRUE)
  ggplot(data = Theoph, aes(x = Time, y = conc)) + geom_point()
})

test_that('all valid spork print as axis label',{
  library(magrittr)
  library(dplyr)
  library(ggplot2)
  library(spork)
  expect_silent(
  data.frame(y=1:10, x=1:10) %>%
  decorate("x: 1 joule^\\*. ~1 kg m^2./s^2") %>%
  #decorate("x: ''") %>%
  mutate(x = structure(x, label = x %>% attr('label') %>%
  as_spork %>%
  as_plotmath %>%
  as.expression)) %>%
  ggplot(aes(x, y))
  )
  expect_silent(
  data.frame(y=1:10, x=1:10) %>%
    decorate("x: gravitational force \\\\ (kg\\.m/s^2.)") %>%
    mutate(x = structure(x, label = x %>% attr('label') %>%
  as_spork %>%
  as_plotmath %>%
  as.expression)) %>%
  ggplot(aes(x, y))
  )
})

test_that('R reserved words survive in print.dg labels',{
  library(magrittr)
  library(dplyr)
  library(ggplot2)
  library(testthat)
  expect_silent(
  data.frame(y=1:10, x=1:10) %>%
   decorate("x: for NaN% joule^\\*. ~1 kg m^2./s^2. %") %>%
   #decorate("x: ''") %>%
    mutate(x = structure(x, label = x %>% attr('label') %>%
  as_spork %>%
  as_plotmath %>%
  as.expression)) %>%
  ggplot(aes(x, y))
  )
})

test_that('ggplot.decorated works with multiple layers',{
  library(yamlet)
  library(ggplot2)
  library(magrittr)
  library(csv)
  a <- io_csv(system.file(package = 'yamlet', 'extdata','phenobarb.csv'))
  b <- io_csv(system.file(package = 'yamlet', 'extdata','quinidine.csv'))
  c <- as.csv(system.file(package = 'yamlet', 'extdata','phenobarb.csv'))
  d <- as.csv(system.file(package = 'yamlet', 'extdata','quinidine.csv'))

  x <-
    a %>% filter(event == 'conc') %>%
    ggplot(aes(x = time, y = value, color = ApgarInd)) + geom_point() +
    b %>% filter(!is.na(conc)) %>%
    geom_point(data = ., aes(x = time/10, y = conc*10, color = Heart))
  y <-
    c %>% filter(event == 'conc') %>%
    ggplot2:::ggplot.default(aes(x = time, y = value, color = ApgarInd)) + geom_point() +
    d %>% filter(!is.na(conc)) %>%
    geom_point(data = ., aes(x = time/10, y = conc*10, color = Heart))

})

test_that('column attributes with metacharacters are quoted or escaped on write',{
  library(magrittr)
  library(dplyr)
  library(ggplot2)
  library(testthat)

  datum <- "x: [ 'AUC , [0-24]', ng*h/mL ]"
  x <- data.frame(x=1:10) %>% decorate(datum)
  path <- tempdir()
  file <- file.path(path,'foo.csv')
  x %>% io_csv(file)
  y <- readLines(sub('csv','yaml',file))
  expect_identical(y, datum)
})

test_that('ggready supports axis label line breaks',{
  library(yamlet)
  library(ggplot2)
  library(magrittr)
  library(dplyr)
  library(encode)
  data(mtcars)
  mtcars %>%
    select(mpg, vs, am) %>%
    data.frame %>%
    mutate(
      plotgroup = case_when(
        vs == 0 & am == 0 ~ 'v-shaped\nautomatic',
        vs == 0 & am == 1 ~ 'v-shaped\nmanual',
        vs == 1 & am == 0 ~ 'straight\nautomatic',
        vs == 1 & am == 1 ~ 'straight\nmanual'
      )
    ) %>%
    redecorate("
mpg: [ milage, mi/gal ]
plotgroup: [ engine\\ntransmission, [v-shaped\n\nautomatic,v-shaped\n\nmanual,straight\n\nautomatic,straight\n\nmanual]]
") %>%
    ggready %>%
    ggplot(aes(x = plotgroup, y = mpg)) +
    geom_boxplot()
})

test_that('for each named column, or all if none named, the data.frame method for modify() assigns a value in the attributes environment',{
  library(magrittr)
  library(dplyr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  x %<>% modify(title = paste(label, '(', guide, ')'), time)
  x %>% select(time, conc) %>% as_yamlet
  expect_identical(attr(x$time,'title'), 'time since start of study ( h )')
  expect_identical(attr(x$conc,'title'), NULL)


  # modify (almost) all columns
  x %<>% modify(title = paste(label, '(', guide, ')'), -Subject)
  x %>% select(time, conc) %>% as_yamlet
  expect_identical(attr(x$time,'title'), 'time since start of study ( h )')
  expect_identical(attr(x$conc,'title'),  'quinidine serum concentration ( mg/L )')
  expect_identical(attr(x$Subject,'title'),  NULL)


})

test_that('modify() makes the underlying object available as an argument',{
    file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
    x <- decorate(file)
    x %<>% modify(`defined values` = sum(!is.na(.)))
    x %>% select(time) %>% as_yamlet
    expect_identical(attr(x$time,'defined values'), 1471L)

  })

test_that('modify() makes the object name available for use and assignment',{
    file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
    x <- decorate(file)
    x %<>% modify(time, name = label)
    expect_identical(names(x)[[2]], 'time since start of study')
  })

test_that('the data.frame method for modify() gives a warning if the assignment target is reserved (i.e, class, levels, labels, names)',{
    file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
    x <- decorate(file)
    expect_warning(x %<>% modify(class = 'numeric', Subject))
})

test_that('the data.frame method for modify() fails gracefully if assignment cannot be made',{
    file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
    x <- decorate(file)
    expect_warning(x %<>% modify(title = foo, time))

})

test_that('the default method for modify() supports lists',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  a <- list(a = 1, b = 1:10, c = letters) %>% modify(length = length(.), b:c)
  expect_identical(attr(a[['a']],'length'), NULL)
  expect_identical(attr(a[['c']],'length'), 26L)

  })

test_that('modifier verbs are limited to dots scope',{
    library(magrittr)
    file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
    x <- decorate(file)
    # as_yamlet
    expect_equal_to_reference(file = '060.rds', as_yamlet(x, Height))
    # decorations
    expect_equal_to_reference(file = '061.rds', decorations(x, Height))
    # modify
    expect_equal_to_reference(
      file = '062.rds',
      modify(x, Height, label = 'Height (cm)') %>% as_yamlet(Height,Weight)
    )
    # resolve
    as_yamlet(x, Height, Weight)
    expect_equal_to_reference(
      file = '063.rds',
      resolve(x, Height) %>% as_yamlet(Height, Weight)
    )
  })

test_that('io_res resolves guide ambiguity on read',{
    library(magrittr)
    file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
    x <- io_csv(file) %>% resolve
    y <- io_res(file)
    expect_identical(x, y)
  })

test_that('output of as_decorated inherits class decorated',{
  x <- as_decorated(list())
  expect_true(inherits(x,'decorated'))
})

test_that('xtable.decorated is stable',{
library(magrittr)
library(xtable)
set.seed(0)
x <- data.frame(
 auc = rnorm(100, mean = 2400, sd = 200),
 bmi = rnorm(100, mean = 20, sd = 5),
 gen = 0:1
)
x %<>% decorate('auc: [AUC_0-24, ng*h/mL]')
x %<>% decorate('bmi: [Body Mass Index, kg/m^2]')
x %<>% decorate('gen: [Gender, [Male: 1, Female: 0]]')
y <- xtable(x)
expect_equal_to_reference(file = '064.rds', y)

y <- xtable(x, auc:bmi)
expect_equal_to_reference(file = '065.rds', y)
expect_equal_to_reference(file = '066.rds', resolve(x))
expect_equal_to_reference(file = '067.rds', xtable(resolve(x)))
})

test_that('promote is stable',{
library(magrittr)
meta <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- read.csv(meta)
singularity(
  data = x,
  list(
    "event == 'conc'",
    "event == 'dose'",
    "event == 'metabolite'"
  )
) %>% expect_identical(0L)
singularity(
  data = x[x$event == 'dose',],
  list(
    "event == 'conc'",
    "event == 'dose'",
    "event == 'metabolite'"
  )
) %>% expect_identical(2L)
singularity(
  data = x[x$event == 'dose',],
  list(
    "time >= 0",
    "event == 'dose'"
  )
) %>% expect_identical(NA_integer_)

file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- file %>% decorate
expect_equal_to_reference(
  file = '068.rds',
  x %>% dplyr:::filter.data.frame(event == 'dose') %>% promote(event) %>% decorations(value)
)
expect_equal_to_reference(
  file = '069.rds',
  x %>% filter(event == 'dose') %>% decorations(value)
)
expect_equal_to_reference(
  file = '070.rds',
  x %>% filter(event == 'dose') %>% decorations(value)
)
expect_equal_to_reference(
  file = '071.rds',
  x %>% filter(event == 'conc') %>% decorations(value)
)
expect_equal_to_reference(
  file = '072.rds',
  x[x$event == 'dose',] %>% decorations(event, value)
)
expect_equal_to_reference(
  file = '073.rds',
  x[x$event == 'conc',] %>% decorations(event, value)
)

})

test_that('ggready is stable',{
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)
expect_equal_to_reference(file = '074.rds',decorations(x, Weight))
expect_equal_to_reference(file = '075.rds',decorations(as.data.frame(x), Weight))
expect_equal_to_reference(file = '076.rds',class(x))
expect_equal_to_reference(file = '078.rds',class(ggready(as.data.frame(x))))
expect_equal_to_reference(file = '079.rds',class(ggready(x)))
expect_equal_to_reference(file = '080.rds',class(ggready(resolve(x))))
x <- ggready(x)
library(magrittr)
library(ggplot2)

# Here we filter on-the-fly
# without loss of attributes.
# Notice mg/L rendering; this is actually part of an expression.
file %>%
 decorate %>%
 filter(!is.na(conc)) %>%
 ggready %>%
 ggplot(aes(x = time, y = conc, color = Heart)) +
 geom_point()

# By default ggready resolves everything decorated.
# But we can intervene to resolve selectively,
# And further intervene to 'ggready' selectively.
#
x <- file %>% decorate %>% filter(!is.na(conc))
x %>%
resolve(conc, time) %>%   # Heart left unresolved!
ggready(conc, Heart) %>%  # time left unreadied!
ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()

# Still, all the labels were actually expressions:
expect_equal_to_reference(file = '081.rds',x %>%
resolve(conc, time) %>%
ggready(conc, Heart) %$% conc %>% attributes %$% label %>% class
)
})

test_that('ggplot.resolved is stable',{
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
library(ggplot2)
library(dplyr)
library(magrittr)
x <- decorate(file)
x %<>% filter(!is.na(conc))
expect_equal_to_reference(file = '082.rds',class(x))
expect_equal_to_reference(file = '083.rds',class(data.frame(x)))
expect_equal_to_reference(file = '084.rds',class(as_decorated(data.frame(x))))

# The bare data.frame gives boring labels and unordered groups.
map <- aes(x = time, y = conc, color = Heart)
data.frame(x) %>% ggplot(map) + geom_point()

# Decorated data.frame uses supplied labels.
# Notice CHF levels are still not ordered.
x %>% ggplot(map) + geom_point()

# We can resolve guide for a chance to enrich the output with units.
# Notice CHF levels are now ordered.
x %<>% resolve
suppressWarnings( # because this complains for columns with no units
  x <- modify(x, title = paste0(label, '\n(', units, ')'))
)
x %>% ggplot(map) + geom_point()

# Or something fancier.
x %<>% modify(conc, title = 'conc_serum. (mg*L^-1.)')
x %>% ggplot(map) + geom_point()

# The y-axis title is deliberately given in spork syntax for elegant coercion:
library(spork)
x %<>% modify(conc, expression = as.expression(as_plotmath(as_spork(title))))
x %>% ggplot(map) + geom_point()

# Add a fancier label for Heart, and facet by a factor:
x %<>% modify(Heart, expression = as.expression(as_plotmath(as_spork('CHF^\\*'))))
x %>% ggplot(map) + geom_point() + facet_wrap(~Creatinine)

# ggready handles the units and plotmath implicitly for a 'standard' display:
x %>% ggready %>% ggplot(map) + geom_point() + facet_wrap(~Creatinine)

# Here we try a dataset with conditional labels and units.
file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
x <- file %>% decorate %>% resolve
# Note that value has two elements for label and guide.
expect_equal_to_reference(file = '085.rds',x %>% decorations(value))
  #'
# The print method defaults to the first, with warning.
map <- aes(x = time, y = value, color = event)
expect_warning(print(x %>% ggplot(map) + geom_point()))

# If we subset appropriately, the relevant value is substituted.
expect_silent(print(x %>% filter(event == 'conc') %>% ggplot(map) + geom_point()))

})

test_that('unclassified is the inverse of classified',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  x %<>% explicit_guide
  y <- classified(x)
  z <- unclassified(y)
  x %>% decorations(Creatinine)
  y %>% decorations(Creatinine)
  z %>% decorations(Creatinine)
  attr(y$Creatinine, 'codelist')
  identical(
  attr(x$Creatinine, 'codelist'),
  attr(z$Creatinine, 'codelist')
  )
  str(attr(x$Creatinine,'codelist'))
  str(attr(z$Creatinine,'codelist'))

  names(names(attr(x$Creatinine,'codelist')))
  names(names(attr(z$Creatinine,'codelist')))

  expect_identical(x, z)
})

test_that('implicit_guide is the inverse of explicit_guide',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_identical(x, implicit_guide(explicit_guide(x)))
})

test_that('desolve is the inverse of resolve',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_identical(x, desolve(resolve(x)))
})

test_that('resolve and desolve retain class',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_true(inherits(resolve(x), 'decorated'))
  expect_true(inherits(desolve(resolve(x)), 'decorated'))
})

test_that('labels and guide elements with colon-space are quoted',{
  foo <- data.frame(x = 1)
  attr(foo$x,'label') <- 'foo: x'
  guide <- list(1)
  names(guide) <- 'H: M'
  attr(foo$x, 'guide') <- guide
  dir <- tempdir()
  file <- file.path(dir, 'foo.csv')
  foo %>% io_csv(file)
  expect_silent(io_csv(file))
})

test_that('classified methods do not lose attributes',{
foo <- classified(letters[1:5])
bar <- classified(LETTERS[1:5])
attr(foo, 'label') <- 'letters'
attr(bar, 'label') <- 'LETTERS'
foo[2:3] <- c('a','b')
foo[[4]] <- 'c'
expect_true(
  setequal(
    names(attributes(c(foo,bar))),
    c('levels','class','codelist','label')
  )
)
expect_true(
  setequal(
    names(attributes(foo[1:2])),
    c('levels','class','codelist','label')
  )
)
expect_true(
  setequal(
    names(attributes(foo[[2]])),
    c('levels','class','codelist','label')
  )
)

})

test_that('unclassified methods do not lose attributes',{
  foo <- classified(letters[1:5])
  attr(foo, 'label') <- 'letters'
  foo <- unclassified(foo)
  expect_true(
    setequal(
      names(attributes(foo)),
      c('codelist','label')
    )
  )
})

test_that('classified() works the same on character and factor',{
expect_identical(classified(LETTERS), classified(factor(LETTERS)))
})

test_that('as_yamlet does not capture levels of classified by default',{
  library(magrittr)
  library(dplyr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  y <- x
  y$Heart %<>% classified
  expect_true(
    setequal(
      names(decorations(y, Heart)[[1]]),
      c('label','guide','codelist')
    )
  )
  decorations(x, Heart)
})

test_that('decorations() does not print colon for un-named list',{})

test_that('filter.decorated retains class', {
  library(dplyr)
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_true(inherits(x %>% filter(Subject == 1), 'decorated'))

})

test_that('promote() retains class decorated', {
  library(dplyr)
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
  x <- decorate(file)
  x %<>% filter(event == 'dose')
  decorations(x)
  expect_true(inherits(x, 'decorated'))
})

test_that('decorations() treats factor levels the same for factor and classified',{
  library(dplyr)
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_false('levels' %in% (x %>% decorations(Race) %>% `[[`(1) %>% names))
  expect_false('levels' %in% (x %>% resolve %>% decorations(Race) %>% `[[`(1) %>% names))
})

test_that('mimic() is stable',{
  library(dplyr)
  library(magrittr)
  let <- letters[1:5]
  LET <- LETTERS[1:5]
  int <- 0L:4L
  num <- as.numeric(int)
  fac <- factor(let)
  css <- classified(let)

  expect_equal_to_reference(mimic(let, let), '086.rds')
  expect_equal_to_reference(mimic(LET, let), '087.rds')
  expect_equal_to_reference(mimic(int, let), '088.rds')
  expect_equal_to_reference(mimic(num, let), '089.rds')
  expect_equal_to_reference(mimic(fac, let), '090.rds')
  expect_equal_to_reference(mimic(css, let), '091.rds')
  expect_equal_to_reference(mimic(character(0)), '092.rds')
  expect_equal_to_reference(mimic(numeric(0)), '093.rds')
  expect_equal_to_reference(mimic(LET), '094.rds')
  x <- data.frame(let, LET)
  x %<>% mutate(let = mimic(let, LET), LET = mimic(LET))
  expect_equal_to_reference(str(x), '095.rds')

})

test_that('subset retains class for decorated inheriting grouped_df',{
  library(dplyr)
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  x %<>% group_by(Subject)
  attr(x[['time']], 'foo') <- 'bar'
  expect_true(inherits(x, 'decorated'))
  #  also for names<-
})

test_that('classified may contain NA',{
  expect_silent(
    classified(c(1,NA))
  )
  expect_silent(
    classified(c(1,NA), exclude = NULL)
  )
})

test_that('bind_rows() works for grouped_df containing classified factors',{
  library(magrittr)
  library(dplyr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  b <- decorate(file) %>% resolve
  b %<>% group_by(Subject)
  expect_silent(bind_rows(b,b))

  a <- data.frame(i = c(1,1,2), x = classified(1:3))
  b <- data.frame(i = c(2,2,3), x = classified(3:5))
  a %<>% group_by(i)
  b %<>% group_by(i)
  # str(a)
  # str(b)
  bind_rows(a,b) %$% x %>% attributes
  expect_silent(bind_rows(a,b))
  expect_identical(bind_rows(a,b) %$% x %>% attributes %$% codelist, as.character(1:5))
})

test_that('gather.decorated respects supplied key and value',{
  library(magrittr)
  library(tidyr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  # x %>% gather('key', 'value', time, interval) %>% decorations
  # x %>% gather('key', 'value', time, interval) %>% names()
  # x %>% gather(key = 'key', value = 'value', time, interval) %>% names()
  suppressWarnings(nms <- names(
    gather(
      x,
      key = 'source',
      value = 'widgets',
      time,
      interval
    )
  ))
  expect_true(all(c('source','widgets') %in% nms))

})

test_that('gather.decorated with no arguments is a non-operation',{
  library(magrittr)
  library(tidyr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_identical(x, gather(x, key = 'source', value = 'widgets'))
  expect_identical(x, gather(x, key = 'source', value = 'widgets', !!!character(0)))
})

test_that('mimic is stable',{
  let <- letters[1:5]
  LET <- LETTERS[1:5]
  int <- 0L:4L
  num <- as.numeric(int)
  fac <- factor(let)
  css <- classified(let)
  expect_equal_to_reference(file = '096.rds',
    list(
  mimic(LET, let),
  mimic(let, let),
  mimic(num, let),
  mimic(int, let),
  mimic(fac, let),
  mimic(css, let),
  mimic(character(0)),
  mimic(numeric(0)),
  mimic(LET),
  mimic(let),
  mimic(num),
  mimic(int),
  mimic(fac),
  mimic(css)
))
})

test_that('factor and character can mimic numeric',{
  let <- letters[1:5]
  LET <- LETTERS[1:5]
  int <- 0L:4L
  num <- as.numeric(int)
  fac <- factor(let)
  css <- classified(let)

  expect_silent(mimic(let, num))
  expect_silent(mimic(fac, num))
  expect_silent(mimic(css, num))

  mimic(css, num)
  unclassified(mimic(css, num))
  expect_true(is.numeric(unclassified(mimic(css, num))))

  mimic(css, as.character(num))
  unclassified(mimic(css, as.character(num)))
  expect_true(is.numeric(unclassified(mimic(css, as.character(num)))))


  expect_true(
    inherits(
      unclassified(
        mimic(css, as.numeric(css))
      ),'integer')
  )
  expect_error(mimic(css, as.integer(css)))
})

test_that('as.integer.classified() returns integer with codelist',{
  css <- classified(letters[1:3], labels = LETTERS[1:3])
  int <- as.integer(css)
  expect_true('codelist' %in% names(attributes(int)))
  expect_true(inherits(int, 'integer'))
})

test_that('as.integer.classified() is equivalent to as.numeric.classified()',{
  css <- classified(c('knife','fork','spoon'))
  expect_true(all(as.integer(css) == as.numeric(css)))
})

test_that('modify() does not search for assignment targets beyond data scope',{
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  suppressWarnings(x %<>% modify(time, SORT = .data$sort))
  expect_false('sort' %in% names(attributes(x$time)))
})

test_that('print.yamlet handles unexpected objects nicely',{
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  x %<>% modify(time, SORT = sort)
  print(decorations(x,time))
  expect_equal_to_reference(file = '097.rds', decorations(x, time))
})

test_that('subset decorated succeeds when dimensions are dropped',{
  library(magrittr)
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_silent(x[1,1])
})

test_that('NA names and values in lists can be converted to yamlet',{
  expect_silent(to_yamlet(setNames(1:3, c('a','b',NA))))
  expect_silent(to_yamlet(setNames(c(1,2,NA), c('a','b','c'))))
})

test_that('subplots respect metadata assignments',{

  library(ggplot2)
  library(magrittr)
  library(dplyr)
  library(gridExtra)
  a <- io_csv(system.file(package = 'yamlet', 'extdata','phenobarb.csv'))
  b <- io_csv(system.file(package = 'yamlet', 'extdata','quinidine.csv'))
  c <- as.csv(system.file(package = 'yamlet', 'extdata','phenobarb.csv'))
  d <- as.csv(system.file(package = 'yamlet', 'extdata','quinidine.csv'))

  x <-
    a %>% filter(event == 'conc') %>%
    ggplot(aes(x = time, y = value, color = ApgarInd)) + geom_point() +
    b %>% filter(!is.na(conc)) %>%
    geom_point(data = ., aes(x = time/10, y = conc*10, color = Heart))

  y <-
    a %>% filter(event == 'conc') %>%
    ggplot2:::ggplot.default(aes(x = time, y = value, color = ApgarInd)) + geom_point() +
    d %>% filter(!is.na(conc)) %>%
    geom_point(data = ., aes(x = time/10, y = conc*10, color = Heart))

  grid.arrange(x, y)

  p <- x %>% ggplot_build
  q <- p %>% ggplot_gtable
  plot(q)
  expect_equal_to_reference(file = '098.rds', p)

})

test_that('a length one sequence resolves parsimoniously',{
  y <- as_yamlet('RACE: [ race, [ Asian: 1 ]]')
  expect_equal(names(y[[1]][2]), 'guide')
})


test_that('resolve has no effect on resolved item',{

})

test_that('read_yamlet and write_yamlet reproduce block quote',{

})


test_that('gather.decorated works with unquoted argument names',{

})

test_that('class "guided" or similar supports concatenation of guides',{

})

test_that('variables with units support unit math',{

})

test_that('write_yamlet uses canonical attribute order by default',{

})

test_that('class "decorated" persists after merges, joins, enumerations',{

})

test_that('moot redecorate warnings are suppressed',{

})
