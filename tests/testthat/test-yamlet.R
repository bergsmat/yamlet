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

  # yaml.load('foo: bar')
  # yaml.load('foo: bar', handlers = list(seq = parsimonious))
  # yaml.load('[foo: bar]')
  # yaml.load('[foo: bar]', handlers = list(seq = parsimonious))
  #
  # yaml.load('RACE: [ label: race, [ foo: bar ]]')
  # yaml.load('RACE: [ label: race, [ foo: bar ]]', handlers = list(seq = parsimonious))
  #
  # yaml.load('RACE: [ label: race, [ foo: bar, hey: baz ]]')
  # yaml.load('RACE: [ label: race, [ foo: bar, hey: baz ]]', handlers = list(seq = parsimonious))
  #
  # as_yamlet('RACE: [ label: race, [ foo: bar ]]')
  # as_yamlet('RACE: [ label: race, [ foo: bar, hey: baz ]]')
  #
  # as_yam('RACE: [ label: race, [ foo: bar ]]') %>% str
  # as_yam('RACE: [ label: race, [ foo: bar, hey: baz ]]') %>% str

  expect_equal_to_reference(file = '038.rds',as_yamlet('RACE: [ label: race, [ foo: bar ]]'))    # must not be label, label; must not drop foo
  expect_equal_to_reference(file = '039.rds', as_yamlet('RACE: [ label: race, [ foo: bar, hey: baz ]]')) # 'label: race' must reduce in presence of plural list

})

test_that('more elements than keys gives warning',{
  expect_warning(as_yamlet('RACE: [label: race, guide: [white: 0, black: 1 ], categorical, 0]\nID: [1, 2, 3]'))
})

test_that('yamlet reads length-one character equivalently to vector',{
  expect_identical(as_yam(c('ID:','TIME:')),as_yam('ID:\nTIME:'))
})

test_that('key priority by source is explicit > object > argument > option > default',{
  expect_identical(names(as_yamlet('a: value')$a), 'label') # default
  old <- getOption('yamlet_default_keys')
  options(yamlet_default_keys = 'option')
  expect_identical(names(as_yamlet('a: value')$a), 'option') # option
  expect_identical(names(as_yamlet('a: value', default_keys = 'argument')$a), 'argument') # argument
  expect_identical(names(as_yamlet('a: value\n_keys: object', default_keys = 'argument')$a),'object')
  expect_identical(names(as_yamlet('a: [explicit: value]\n_keys: object', default_keys = 'argument')$a),'explicit')
  options(yamlet_default_keys = old)
})

test_that('mixed-length vector types are respected',{
  expect_equal_to_reference(
    file = '099.rds',
    as_yamlet('RACE: [ race, [white, black, asian ]]')
  )
})

test_that('mixed-depth nesting is supported',{
  expect_equal_to_reference(file = '100.rds', as_yamlet('ITEM: [ label: item, [ foo: bar, hey: baz ]]'))
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
  expect_equal_to_reference(file = '054.rds', to_yamlet(c(a = '4', b = '5.8')))
  expect_equal_to_reference(file = '055.rds', to_yamlet(c(a = 4, b = 5.8)))
  expect_equal_to_reference(file = '056.rds', to_yamlet(TRUE))
  expect_equal_to_reference(file = '057.rds', to_yamlet('foo'))
  expect_equal_to_reference(file = '058.rds', to_yamlet(c('a','b')))
  expect_equal_to_reference(file = '059.rds', to_yamlet(c(a = 'a',b = 'b')))
  expect_identical(to_yamlet(c(no = 'n', yes = 'y')),"[ 'no': 'n', 'yes': 'y' ]")
  expect_identical(to_yamlet(c('no' = 'n', 'yes' = 'y')),"[ 'no': 'n', 'yes': 'y' ]")
  expect_identical(to_yamlet(c('No' = 'n', 'Yes' = 'y')),"[ 'No': 'n', 'Yes': 'y' ]")
  
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
  expect_warning(y <- decorate(x, 'Subject: subject identifier'))
  expect_silent(y <- redecorate(x, 'Subject: subject identifier'))

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
  expect_warning( x %>% select(value) %>% explicit_guide %>% as_yamlet) # value looks like codelist because event not available to signal conditional
  x %>% explicit_guide %>% classified %>% as_yamlet
  a <- x %>%  resolve %>% as_yamlet
  expect_true(setequal(names(a$value), c('label','units', 'title')))
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
    if(exists('foo'))rm(foo)
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
options(yamlet_persistence = FALSE)
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
options(yamlet_persistence = TRUE)
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
})

test_that('ggplot.resolved is stable',{
skip_if_not( l10n_info()$`UTF-8` )
skip_if(
  .Platform$OS.type == "unix" && Encoding(enc2native("\U00B5")) != "UTF-8",
  "Skipping non-ASCII tests on UTF-8 Unix system"
)

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
  # x %>% decorations(Creatinine)
  # y %>% decorations(Creatinine)
  # z %>% decorations(Creatinine)
  # attr(y$Creatinine, 'codelist')
  # identical(
  # attr(x$Creatinine, 'codelist'),
  # attr(z$Creatinine, 'codelist')
  # )
  # str(attr(x$Creatinine,'codelist'))
  # str(attr(z$Creatinine,'codelist'))
  #
  # names(names(attr(x$Creatinine,'codelist')))
  # names(names(attr(z$Creatinine,'codelist')))

  expect_identical(x, z)
})

test_that('desolve is the inverse of resolve',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_identical(x, desolve(resolve(x)))
})

test_that('implicit_guide is the inverse of explicit_guide',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  x <- decorate(file)
  expect_identical(x, implicit_guide(explicit_guide(x)))
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
      c('codelist','label','class')
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
  # x %<>% mutate(let = mimic(let, LET), LET = mimic(LET))
  expect_equal_to_reference(x, '095.rds')

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
    is.integer(
      unclassified(
        mimic(css, as.numeric(css))
      )
    )
  )
  # expect_error(mimic(css, as.integer(css))) # don't know why this should be an error
  expect_silent(mimic(css, as.integer(css)))
})

test_that('as.integer.classified() returns integer with guide',{
  css <- classified(letters[1:3], labels = LETTERS[1:3])
  int <- as.integer(css)
  expect_true('guide' %in% names(attributes(int)))
  expect_true(is.integer(int))
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
  # print(decorations(x,time))
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

test_that('a length one sequence resolves parsimoniously',{
  y <- as_yamlet('RACE: [ race, [ Asian: 1 ]]')
  expect_equal(names(y[[1]][2]), 'guide')
})

test_that('column named *n* can be decorated',{
  library(magrittr)
  library(dplyr)
  x <- data.frame(n = 1)
  x %<>% decorate('"n": count')
  expect_identical('count',decorations(x)$n$label)
})

test_that('column named *scenario* can have label *Scenario* even if there is a column with this name',{
  library(magrittr)
  library(dplyr)
  x <- data.frame(scenario = 1, Scenario = 1)
  x %<>% decorate('scenario: Scenario')
  x %<>% decorate('Scenario: scenario')
  expect_identical('Scenario', decorations(x)$scenario$label)
})

test_that('write_yamlet uses canonical attribute order by default',{
  x <- data.frame(x = 1, y = 1, z = factor('a'))
  x %<>% decorate('
  x: [ guide: mm, desc: this, label: foo ]
  "y": [ guide: bar, desc: other ]
  ')
  expect_equal_to_reference(file = '101.rds', capture.output(write_yamlet(x)))
})

test_that('moot redecorate warnings are suppressed',{
  x <- data.frame(x = 1, y = 1, z = factor('a'))
  x %<>% decorate('
  x: [ guide: mm, desc: this, label: foo ]
  "y": [ guide: bar, desc: other ]
  ')
  expect_silent(x %>% decorate(decorations(x)))
  expect_warning(x %>% decorate('x: bar'))
  expect_silent(x %>% decorate('x: bar', overwrite = TRUE))

})

test_that('class "decorated" persists after merges, joins, enumerations',{
  library(magrittr)
  library(dplyr)
  x <- data.frame(foo = 1, bar = 2)
  x %<>% decorate('foo: [distance, mm]')
  x %<>% decorate('bar: [height, mm]')
  expect_true(inherits(x, 'decorated'))
  expect_true(inherits(full_join(x,x), 'decorated'))
  expect_true(inherits(left_join(x,x), 'decorated'))
  expect_true(inherits(left_join(x, as.data.frame(x)), 'decorated'))
  expect_false(inherits(full_join(as.data.frame(x), x), 'decorated'))
  expect_true(inherits(what = 'decorated', merge(x,x)))
  # if(require(wrangle)){
  #   expect_true(inherits(what = 'decorated', enumerate(x, foo, bar)))
  # } # check gives warning
})

test_that('decorations() does not print colon for un-named list',{
  x <- data.frame(foo = c('a','b','c'))
  x %<>% decorate('foo: [title, [[1,2],[2,3]]]')
  foo <- capture_output(decorations(x), print = TRUE)
  foo <- sub(':','', foo) # remove the only expected colon
  expect_false(grepl(':', foo))
})

test_that('read_yamlet and write_yamlet reproduce block quote',{
  x <- read_yamlet('
    x:
      background: |
        x is so happy
        a variable of note
        it wants to help you
      sentence: >
        Sometimes we don\'t really
        care where the line breaks
        are.')
  expect_equal_to_reference(
    file = '102.rds',
    capture_output(
      print = TRUE,
      write_yamlet(x)
    )
  )
  expect_equal_to_reference(
    file = '103.rds',
    capture_output(
      print = TRUE,
      write_yamlet(x, block = TRUE)
    )
  )
})

test_that('decorated retains class when ungrouped', {
  expect_true(inherits(ungroup(as_decorated(group_by(Theoph, Subject))), 'decorated'))
})

test_that('length-one codelists are not confused with units',{
  x <- data.frame(race = 2, sex = 'M', conc = 1, time = 0)
  x %<>% decorate('
    race: [ Race, [ Asian: 2 ]]
    sex: [ Sex,  [M]  ]
  ')
  #x %>% resolve %>% decorations
  expect_identical(
    'codelist',
    x %>% resolve %>% decorations %$% race %>% names %>% extract2(2)
  )
  expect_identical(
    'codelist',
    x %>% resolve %>% decorations %$% sex %>% names %>% extract2(2)
  )
  expect_equal_to_reference(
    file = '104.rds',
    'sex: [ Sex,  M  ]' %>% yaml.load(handlers = list(seq = parsimonious))
  )
  expect_equal_to_reference(
    file = '105.rds',
    'sex: [ Sex,[ M ]]' %>% yaml.load(handlers = list(seq = parsimonious))
  )
  expect_equal_to_reference(
    file = '106.rds',
    'sex: [ Sex,[ M, F ]]' %>% yaml.load(handlers = list(seq = parsimonious))
  )
})

test_that('un-named codelists are back-transformed consistently',{

  expect_identical(
    'sex: Sex',
    capture.output(write_yamlet(as_yamlet('sex: Sex')))
  )
  expect_identical(
    'sex: [ Sex, M ]',
    capture.output(write_yamlet(as_yamlet('sex: [ Sex, M ]')))
  )
  expect_identical(
    'sex: [ Sex, [ M ]]',
    capture.output(write_yamlet(as_yamlet('sex: [ Sex, [ M ]]')))
  )
  expect_identical(
    'sex: [ Sex, [ M, F ]]',
    capture.output(write_yamlet(as_yamlet('sex: [ Sex, [ M, F ]]')))
  )
})

test_that('named codelists are back-transformed consistently',{

  expect_identical(
    'sex: [ Sex, [ Male: M ]]',
    capture.output(write_yamlet(as_yamlet('sex: [ Sex, [ Male: M ]]')))
  )

  expect_identical(
    'sex: [ Sex, [ Male: M, Female: F ]]',
    capture.output(write_yamlet(as_yamlet('sex: [ Sex, [ Male: M, Female: F ]]')))
  )
})

test_that('class "guided" or similar supports concatenation of guides',{
  # Use vctrs to achieve consistent attribute treatment.
  # see test-dvec.R
})

test_that('variables with units support unit math',{
  # write converters for guided -> units and back
  # see test-dvec.R
})

test_that('classified.data.frame passes exclude = NULL to member factors',{
  x <- data.frame(letters = c('a','b','c','d', NA))
  x %<>% decorate('letters: [Letters, [ a, b, c, d, NA ]]')
  x %>% decorations
  x %<>% explicit_guide
  x %>% decorations
  x %<>% classified(exclude = NULL)
  expect_true(NA %in% attr(x$letters, 'codelist'))
  expect_true(NA %in% levels(x$letters))
})

test_that('codelist can contain literal NA if quoted',{
  x <- data.frame(letters = c('a','b','c','d', 'NA'))
  x %<>% decorate('letters: [Letters, [ a, b, c, d, "NA" ]]')
  x %>% decorations
  x %<>% explicit_guide
  x %>% decorations
  x %<>% classified
  expect_true('NA' %in% attr(x$letters, 'codelist'))
  expect_true('NA' %in% levels(x$letters))
})

test_that('when two different decodes have the same code, classified levels match classified codelist values',{
  x <- data.frame(letters = c('a','a','b'))
  x %<>% decorate('letters: [Letters, [ TRT1: a, TRT2: a, TRT3: b ]]')
  x
  expect_warning({
  x %>% resolve
  x %>% resolve %>% desolve
  x %>% resolve %>% desolve %>% resolve
  x %<>% resolve
    
})
  expect_identical(
    levels(x$letters), 
    unlist(as.character(attr(x$letters, 'codelist')))
  )
})

test_that('when two different codes have the same decode, classified levels match unique classified codelist values',{
  x <- data.frame(letters = c('a','b','c'))
  x %<>% decorate('letters: [Letters, [ TRT1: a, TRT2: b, TRT2: c ]]')
  x
  expect_warning({
  x %>% resolve
  x %>% resolve %>% desolve 
  x %>% resolve %>% desolve  %>% decorations
  x %>% resolve %>% desolve %>% resolve
  x %<>% resolve
   
  })
  levels(x$letters)
  as.character(attr(x$letters, 'codelist'))
  expect_identical(
    x %$% letters %>% levels, 
    x %$% letters %>% attr('codelist') %>% 
      as.character %>% unlist %>% unique
  )
})

test_that('ggplot succeeds for class decorated that has no labels',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  library(ggplot2)
  library(dplyr)
  library(magrittr)
  expect_silent(a <- file %>% as.csv %>% filter(!is.na(conc)) %>% as_decorated %>%
    ggplot(aes(x = time, y = conc, color = Heart)) + geom_point())
  # look for legend: congestive heart failure (mod/no/sev)
  
})

test_that('classified does not re-classify',{
  # avoid alternating states
  x <- data.frame(
    age = c(53, 58, 60),
    sex = c(0, 1, 1),
    race = c(1, 1, 2)
  )
  x %<>% decorate('
  age: [ Age, year ]
  sex: [ Sex, [ Female: 0, Male: 1 ]]
  race: [ Race, [White: 1, Asian: 2 ]]
  ')
  x
  x %>% resolve
  x %>% resolve(sex)
  x %>% resolve(sex) %>% resolve
  x %>% resolve %$% sex %>% attributes
  x %>% resolve(sex) %>% resolve %$% sex %>% attributes
  x %>% resolve(sex) %$% sex %>% attributes
  
  foo <- x %>% resolve(sex)
  # this drops sex label:
  foo %>% resolve %>% decorations
  
  expect_identical(
    x %>% resolve,
    x %>% resolve(sex) %>% resolve
  )
  expect_identical(
    x %>% resolve,
    x %>% resolve(race) %>% resolve
  )
  expect_identical(
    x %>% resolve,
    x %>% resolve(age) %>% resolve
  )
})

test_that('io_csv.character allows user to over-ride meta',{
  # issue 3
  expect_silent(
    x <- io_csv(
      system.file(package = 'yamlet','extdata/phenobarb.csv'),
      meta = system.file(package = 'yamlet','extdata/quinidine.yaml')
    )
  )
  expect_identical(NULL, attr(x$Wt, 'label'))
})

test_that('[ can be the first character of a code or decode',{
  # issue 2
  x <- data.frame(range = '[min,max]')
  expect_silent(x %<>% decorate('range: [ Range, [ "[minimum,maximum]": "[min,max]" ]]'))
  expect_silent(decorations(x))
  where <- tempdir()
  x %>% io_csv(file.path(where, 'bracket.csv'))
  y <- io_csv(file.path(where, 'bracket.csv'), source = FALSE)
  expect_identical(x,y)
})

test_that('Quoted Yes and No survive parsing verbatim',{

  x <- "blq: [ LOQ Y/N, [ 'No': 0, 'Yes': 1 ]]"
  y <- x %>% write_yamlet %>% as.character
  x
  y
  expect_identical(x, y)
})

test_that('append_units() supports specific target',{
  x <- as_dvec(1:10, label = 'acceleration', units = 'm/s^2')
  x %<>% append_units(target = 'title')
  expect_true('title' %in% names(attributes(x)))
})

test_that('make_title() honors pass-through arguments for append_units()',{
  x <- as_dvec(1:10, label = 'acceleration', units = 'm/s^2')
  x %<>% make_title(open = '[', close = ']')
  attr(x, 'title')
  expect_identical(attr(x, 'title'), "acceleration[m/s^2]")
})

test_that('make_title() / drop_title() active on resolve() and desolve()',{
  x <- data.frame(
    age = c(53, 58, 60),
    sex = c(0, 1, 1),
    race = c(1, 1, 2)
  )
  x %<>% decorate('
  age: [ Age, year ]
  sex: [ Sex, [ Female: 0, Male: 1 ]]
  race: [ Race, [White: 1, Asian: 2 ]]
  ')
  x
  x %<>% resolve
  expect_true('title' %in% names(attributes(x$age)))
  x %<>% desolve
  expect_false('title' %in% names(attributes(x$age)))
  
})

test_that('make_title() can be globally defeated',{
  options(yamlet_with_title = FALSE)
  x <- data.frame(
    age = c(53, 58, 60),
    sex = c(0, 1, 1),
    race = c(1, 1, 2)
  )
  x %<>% decorate('
  age: [ Age, year ]
  sex: [ Sex, [ Female: 0, Male: 1 ]]
  race: [ Race, [White: 1, Asian: 2 ]]
  ')
  x
  x %<>% resolve
  expect_false('title' %in% names(attributes(x$age)))
  options(yamlet_with_title = NULL)
  
})

test_that('yamlet_options() displays globally-configurable options',{
  options(yamlet_with_title = FALSE)
  expect_true('yamlet_with_title' %in% names(yamlet_options()))
  options(yamlet_with_title = NULL)
  expect_false('yamlet_with_title' %in% names(yamlet_options()))
})

test_that('make_title() behaves as expected for class dvec',{
  x <- as_dvec(1:10, label = 'length', guide = 'mm')
  expect_false('title' %in% (x %>% make_title %>% attributes %>% names))
  expect_true('title' %in% (x %>% resolve %>% make_title %>% attributes %>% names))
})

test_that('add_title() and drop_title() have no effect on (undecorated) data.frame',{
  x <- data.frame(
    age = c(53, 58, 60),
    height = c(155, 130, 145),
    race = c(1, 2, 2)
  )
  x %<>% decorate('
  age: [ Age, year ]
  height: [ Height, cm, title: "Subject Height [cm]" ]
  race: [ Race, [White: 1, Asian: 2 ]]
  ')
  x %>% decorations
  x %<>% as.data.frame
  x %>% decorations # still has decorations, but is just a data.frame
  x %<>% make_title
  x %>% decorations
  expect_false('title' %in% names(attributes(x$age)))
  x %<>% drop_title
  x %>% decorations
  expect_true('title' %in% names(attributes(x$height)))
})

test_that('row_bind of supported table types returns consistent class and functional metadata',{
  library(dplyr)
  
  a <- data.frame(study = 1) %>% decorate('study: [Study, [A: 1]]', persistence = FALSE)
  class(a) <- 'data.frame'
  b <- data.frame(study = 2) %>% decorate('study: [Study, [B: 2]]') # decorated data.frame
  c <-     tibble(study = 3) %>% decorate('study: [Study, [C: 3]]') # decorated tbl_df
  c %<>% group_by(study) # decorated grouped_df
  
  # per ?bind_rows: bind_rows() returns the same type as the first input,
  # either a data.frame, tbl_df, or grouped_df
  # or by extension, decorated data.frame, decorated tbl_df, decorated grouped_df
  
  # bind_rows(a, a) %>% str # no magic, attributes dropped, not surprising
  # bind_rows(b, b) %>% str # magic
  # bind_rows(c, c) %>% str # magic
  # bind_rows(a, b) %>% str # magic @ 0.10.7
  # bind_rows(b, a) %>% str # magic
  # bind_rows(a, c) %>% str # magic @ 0.10.7
  # bind_rows(c, a) %>% str # magic
  # bind_rows(b, c) %>% str # returns decorated data.frame, not surprising
  # bind_rows(c, b) %>% str # magic
  
  expect_equal_to_reference(file = '108.rds', decorations(bind_rows(a, a)))
  expect_equal_to_reference(file = '109.rds', decorations(bind_rows(b, b)))
  expect_equal_to_reference(file = '110.rds', decorations(bind_rows(c, c)))
  expect_equal_to_reference(file = '111.rds', decorations(bind_rows(a, b)))
  expect_equal_to_reference(file = '112.rds', decorations(bind_rows(b, a)))
  expect_equal_to_reference(file = '113.rds', decorations(bind_rows(a, c)))
  expect_equal_to_reference(file = '114.rds', decorations(bind_rows(c, a)))
  expect_equal_to_reference(file = '115.rds', decorations(bind_rows(b, c)))
  expect_equal_to_reference(file = '116.rds', decorations(bind_rows(c, b)))
  
  # Conclusions:
  # * Without any additional intervention, 'decorated'
  #   always appears first before 'tbl_df' or 'grouped_df'
  # * Without additional intervention, bind_rows() always
  #   returns the data type of first argument.
  # * bind_rows() drops meta if returning data.frame.
  # * bind_rows() respects 'vestigal' meta on data.frames otherwise.
  
})

test_that('yamlet warns if codelist not one-to-one',{
  x <- data.frame(
    age = c(53, 58, 60),
    height = c(155, 130, 145),
    race = c(1, 1, 1),
    ethnicity = c(0, 0, 1)
  )
  x %<>% decorate('
    age: [ Age, year ]
    height: [ Height, cm, title: "Subject Height [cm]" ]
    race: [ Race, [White: 1, White: 1, Asian: 1 ]]
    ethnicity: [ Ethnicity, [ Hispanic: 0, Hispanic: 1]]
  ')
  expect_warning(x %>% resolve %>% decorations)
  expect_warning(x %>% resolve)

})

test_that('yamlet warns if row_bind gives overlapping codelist',{
  library(dplyr)
  x <- data.frame(
    race = c(1, 2, 2)
  )
  x %<>% decorate('
    race: [ Race, [White: 1, Asian: 2 ]]
  ')
  y <- data.frame(
    race = c(1, 2, 2)
  )
  y %<>% decorate('
    race: [ Race, [Asian: 1, White: 2, Black: 3]]
  ')
  expect_warning(bind_rows(x, y) %>% resolve)
  expect_warning(bind_rows(x, y))
})

test_that('print.decorated_ggplot() warns if label has length > 1',{
  library(magrittr)
  library(ggplot2)
  library(yamlet)
  
  a <- Theoph %>%
    as.data.frame %>%
    decorate('
    conc: Concentration
    Time: Time
    ') %>%
    mutate(source = 'Theoph')

  b <- a %>%
    ggplot(
      aes(
        x = Time, 
        y = conc
      )
    ) + 
    geom_point() +
    ggtitle(a$source)
  b$labels
  expect_warning(print(b))

})

test_that('yamlet can decorate n and N', {
  x <- data.frame(a = 0, n = 0, N = 0)
  x %<>% decorate('a: test')
  x %<>% decorate('"n": number')
  x %<>% decorate('"N": [ newtons, kg*m*s-2 ]')
  expect_identical(attr(x$N, 'label'), 'newtons')
})

test_that('decorations for "n" etc. survive trip to storage', {
  x <- data.frame(n = 0, sam = 0, wt = 0)
  x %<>% decorate('
  "n": [ Number, [a: 0, b: 1, c: -1], sort: 1]
  sam: Number of Samples
  wt: [ Body Weight, kg ]
  ')
  y <- x %>% io_yamlet(tempfile()) %>% io_yamlet
  expect_identical(decorations(x), y)
  
})

test_that('decorating with guide element -1 survives trip to storage as integer',{
  expect_identical(to_yamlet(-1L), "-1")
})

test_that('classified.classified() drops unused levels',{
  a <- factor(c('knife','fork'), levels = c('knife','fork','spoon'))
  levels(a) # three levels
  levels(factor(a)) # two levels
  b <- classified(a)
  levels(b) # three levels
  levels(classified(b))
  levels(classified(b, drop = TRUE))
  expect_identical(levels(classified(b, drop = TRUE)), c('knife', 'fork'))
  b
  classified(b)
  expect_silent(classified(b, levels = 'knife'))
  expect_error(classified(b, labels = 'knife'))
  expect_silent(classified(b, labels = c('Knife','Fork','Spoon')))
  expect_error(classified(b, labels = c('Knife','Fork','Spoon'), exclude = 'fork'))
  expect_silent(classified(b, drop = TRUE, labels = c('Knife','Fork')))
})

test_that('classified() supports NA values',{
  a <- factor(c('knife','fork','spoon'), levels = c('knife','fork'))
  b <- classified(a)
  expect_true(any(is.na(b)))
  expect_false(any(is.na(attr(b, 'codelist'))))
  expect_identical(levels(a), levels(b))
})

test_that('classified() supports NA levels',{
  a <- factor(c('knife','fork', NA), levels = c('knife','fork',NA), exclude = NULL)
  expect_true(any(is.na(levels(a))))
  b <- classified(a, exclude = NULL)
  expect_false(any(is.na(b)))
  expect_true(any(is.na(attr(b, 'codelist'))))
  levels(a)
  levels(b)
  expect_identical(levels(a), levels(b))
})

test_that('as.integer.classified() supports NA values and levels',{
  a <- classified(
    factor(
      c('knife','fork','spoon'), 
      levels = c('knife','fork')
    )
  )
  b <- classified(
    factor(
      c('knife','fork',NA), 
      levels = c('knife','fork',NA), 
      exclude = NULL
    ), 
    exclude = NULL
  )
  ai <- as.integer(a, -1)
  expect_identical(as.integer(ai), c(0L, 1L, NA))
  expect_identical(as.integer(attr(ai, 'guide')), c(0L, 1L))
  expect_identical(names(attr(ai, 'guide')), c('knife','fork'))
 
  bi <- as.integer(b, -1, exclude = NULL)
  expect_false(any(is.na(bi)))
  expect_true(any(is.na(names(attr(bi, 'guide')))))
})

test_that('classified() handles multiple new levels appropriately',{
  a <- structure(1:3, codelist = list(knife = 1, fork = 2, spoon = 3))
  b <- classified(a)
  expect_identical(b, classified(b))
  expect_warning(
    c <- classified(
      b, 
      levels = c('knife','fork','spoon','chopstix','ladel'),
      labels = c('Knife','Fork','Spoon','Chopstix','Ladel')
    )
  )
})

test_that('literal NA and NA_character_ survive round-trip',{
  a <- 'letters: [ Letters, [ a, b, c, NA ]]'
  x <- data.frame(letters = c('a','b','c', NA))
  x %<>% decorate(a)
  b <- write_yamlet(x)
  expect_identical(a, b)
  
  a <- "letters: [ Letters, [ a, b, c, 'NA' ]]"
  x <- data.frame(letters = c('a','b','c', 'NA'))
  x %<>% decorate(a)
  b <- write_yamlet(x)
  expect_identical(a, b)
})

test_that('yamlet names can be true NA or NA string',{
  a <- "letters: [ Letters, [ A: a, B: b, C: c, 'NA': 'NA', NA: NA ]]"
  x <- data.frame(letters = c('a','b','c', 'NA', NA ))
  x %<>% decorate(a)
  foo <- capture.output(b <- write_yamlet(x))
  expect_identical(a, b)
  c <- attr(x$letters, 'guide')
  expect_true(any(is.na(names(c))))
  expect_true(any(is.na(c)))
  expect_equal_to_reference(capture.output(decorations(x)), file = '118.rds')
  x %<>% redecorate("letters: [ Letters, [ a, b, c, 'NA', NA ]]")
  expect_equal_to_reference(capture.output(decorations(x)), file = '119.rds')
})

test_that('as.integer.classified gives fully-recoverable result when NA is a level',{
  library(dplyr)
  x <- data.frame(letters = c('a','b','c',NA))
  x %<>% decorate('letters: [ Letters, [NA, a, b, c ]]')
  x %<>% resolve(exclude = NULL)
  x %<>% mutate(letters = classified(letters, exclude = NULL))
  x %<>% mutate(letters = as.integer(letters, -1, exclude = NULL))
  expect_silent(x %>% resolve(letters))
  x %<>% resolve
  expect_equal_to_reference(x, file = '120.rds')
})

test_that('mimic supports solitary NA',{
  expect_silent(
    {
      mimic(NA, NA)
      mimic(factor(NA), NA)
      mimic(factor(NA, exclude = NULL), NA)
      mimic(NA, 1)
      mimic(factor(NA), 1)
      mimic(factor(NA, exclude = NULL), 1)
      mimic(factor(NA, levels = c(NA, 'foo')), 1)
      mimic(factor(NA, levels = NA, exclude = NULL), 1)
    }
  )
  
  expect_silent(
    {
      mimic(
        factor(
          NA, 
          levels = NA, 
          exclude = NULL
        ), 
        1, 
        exclude = NULL
      )
    }
  )
  
 
})