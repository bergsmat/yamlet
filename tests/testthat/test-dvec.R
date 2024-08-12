library(testthat)
library(magrittr)
library(dplyr)
library(tidyr)
test_that('this file gets run on check',{
 expect_true(TRUE)
})

test_that('dvec subset and element-select preserve attributes',{
  a <- as_dvec(letters)
  attr(a, 'label') <- 'letters'
  attr(a, 'guide') <- list('a','b','c')
  expect_identical('letters', attr(a[[2]], 'label'))
  expect_identical('letters', attr(a[2:3], 'label'))
})

test_that('dvec subset assign and element assign preserve attributes',{
  a <- as_dvec(letters)
  attr(a, 'label') <- 'letters'
  attr(a, 'guide') <- list('a','b','c')
  a[[3]] <- '1'
  expect_identical('letters', attr(a, 'label'))
  a[2:3] <- '1'
  expect_identical('letters', attr(a, 'label'))
})

test_that('dvec subset assign and element assign respect class coercion',{
  a <- as_dvec(1:10)
  attr(a, 'label') <- 'numbers'
  attr(a, 'guide') <- 'kg'
  a[[3]] <- 'a'
  expect_true(is.character(a))
})

test_that('reconciliation of attributes is comprehensive',{
  a <- 1:10
  b <- letters[1:10]
  c <- 11:20

  attr(a,'label') <- 'numbers'
  attr(a, 'guide') <- 'kg'
  attr(b, 'label') <- 'letters'
  attr(c, 'units') <- 'mg'
  attr(c, 'label') <- 'other'
  a <- as_dvec(a)
  expect_warning(d <- c(a, b, c))
  expect_true(is.character(d))
  expect_true(attr(d, 'label') == 'numbers')
  expect_true(attr(d, 'guide') == 'kg')
  expect_true(attr(d, 'units') == 'mg')
  expect_true(attr(d, 'label') == 'numbers')
})

test_that('c.dvec fails informatively for factor input',{
  a <- as_dvec(letters[1:3])
  b <- factor(letters[3:5])
  expect_error(c(a, b))
})

test_that('bind_rows() reconciles attributes',{
  a <- data.frame(head(Theoph, 2))
  a$Subject %<>% classified
  a %<>% decorate('
    Subject: subject
    Wt: [Weight, kg]
    Dose: [Dose, mg]
    Time: [Time, h]
    conc: [Concentration, ng/mL]
  ')
  a %<>% mutate(across(-Subject, as_dvec))
  b <- bind_rows(a, rev(a))
  decorations(b)
  expect_identical(attr(b$Subject, 'label'), 'subject')
  expect_identical(attr(b$Dose, 'label'), 'Dose')
  expect_identical(attr(b$conc, 'guide'), 'ng/mL')
  b <- a %>% redecorate('
    Subject: SUBJECT
    Wt: WEIGHT
  ')
  # undebug(bind_rows)
  # undebug(yamlet:::reconcile.list)
  # undebug(yamlet:::arbitrate.default)
  a %<>% select(Wt)
  b %<>% select(Wt)
  c <- data.frame(Wt = as_dvec(c(79.6, 79.6), label = 'Weight'))
  d <- data.frame(Wt = as_dvec(c(79.6, 79.6), label = 'WEIGHT'))
  expect_warning({
  bind_rows(a, b) %>% decorations # one warning
  bind_rows(c, d) %>% decorations # one warning
  bind_rows(c, b) %>% decorations # one warning
  bind_rows(a, d) %>% decorations # one warning

  bind_rows(as.data.frame(a), as.data.frame(b)) # one warning
  bind_rows(as_decorated(c), as_decorated(d)) # one warning

  c <- bind_rows(a, b)
  d <- bind_rows(b, a)
})  
  
  decorations(c)
  decorations(d)
  # expect_identical(attr(c$Subject, 'label'), 'subject')
  expect_identical(attr(c$Wt, 'label'), 'Weight')
  expect_identical(attr(d$Wt, 'label'), 'WEIGHT')
  expect_identical(attr(c$Wt, 'guide'), 'kg')

  # vec_rbind(
  #   as_dvec(numeric(1), label = 'foo'),
  #   as_dvec(numeric(1), label = 'FOO')
  # )
expect_warning({ 
  bind_rows(
    data.frame(a = as_dvec(numeric(1), label = 'foo')),
    data.frame(a = as_dvec(numeric(1), label = 'FOO'))
  ) %>% decorations
  bind_rows(
    data.frame(a = as_dvec(numeric(0), label = 'foo')),
    data.frame(a = as_dvec(numeric(0), label = 'FOO'))
  ) %>% decorations
})
})

test_that('bind_rows respects column type of first argument', {
  library(haven)
  library(dplyr)
  library(magrittr)
  dm <- 'extdata/dm.xpt.gz' %>% 
    system.file(package = 'yamlet') %>% 
    gzfile %>% 
    read_xpt
  dm %<>% select(RACE) %>% slice(1:2)
  dm2 <- redecorate(dm, 'RACE: foo')
  dm %>% decorations
  dm2 %>% decorations
  expect_warning({
    bind_rows(dm, dm2) %>% str
    c(dm2$RACE, dm$RACE)
   vctrs::vec_c(dm2$RACE, dm$RACE)
  vctrs::vec_c(dm$RACE, dm2$RACE)
  vctrs::vec_rbind(dm, dm2)
  vctrs::vec_rbind(dm2, dm)
  bind_rows(dm2, dm) %>% decorations # one warning
  bind_rows(dm2, as_decorated(dm)) %>% decorations # one warning
  bind_rows(as_decorated(dm), dm2) %>% decorations
  dm3 <- bind_rows(as_decorated(dm), dm2)
   
  })
  expect_identical(attr(dm3$RACE, 'label'), 'Race')
  
  # In this (next) very interesting example,
  # EVEN THOUGH dm has decorations,
  # EVEN IF dm is coerced to decorated,
  # EVEN THOUGH dm$RACE has a label,
  # EVEN THOUGH dm2$RACE is dvec,
  # dm$RACE is still character.
  # But as of 0.10.7, c(dm$RACE, dm2$RACE)
  # promotes dm$RACE to dvec (see vec_ptype2.character.dvec )
  # so attributes are preserved.
 
  expect_warning(
  bind_rows(dm %>% redecorate(persistence = F), dm2) %>% decorations
  )
  
  # here also attributes are preserved
  expect_warning({
  bind_rows(dm %>% redecorate(persistence = T), dm2) %>% decorations
  bind_rows(dm %>% redecorate, dm2) %>% decorations
    
  })

  # Columns of an xpt, bearing labels, can be coerced to dvec by
  # self-redecorating with persistence turned on (default).
  
  dm %<>% redecorate
  expect_warning(dm %<>% bind_rows(dm2))
  expect_identical(attr(dm$RACE, 'label'), 'Race')
  
})

test_that('pivot_longer() reconciles attributes',{
  a <- data.frame(head(Theoph))
  a$Subject %<>% classified
  a %<>% decorate('
    Subject: subject
    Wt: [Weight, kg]
    Dose: [Dose, mg]
    Time: [Time, h]
    conc: [Concentration, ng/mL]
  ')
  a %<>% mutate(across(-Subject, as_dvec))
  as_tibble(a)
  expect_warning(pivot_longer(a, Wt:conc))

  l = as_dvec(letters[1:3], label = 'letters')
  L = as_dvec(LETTERS[1:3], label = 'Letters')
  x <- data.frame(l, L)
  expect_warning(c(l,L))
  expect_warning( out <- pivot_longer(x, l:L))
  expect_true(inherits(out$value, 'dvec'))
  expect_identical(attr(out$value, 'label'), 'letters')

})

test_that('dplyr verbs preserve attributes',{
  a <- data.frame(head(Theoph))
  a$Subject %<>% classified
  a %<>% decorate('
    Subject: subject
    Wt: [Weight, kg]
    Dose: [Dose, mg]
    Time: [Time, h]
    conc: [Concentration, ng/mL]
  ')
  a %<>% mutate(across(-Subject, as_dvec))
  # select, filter, mutate, summarize, arrange, left_join

  expect_identical('kg', a %>% select(Wt) %$% Wt %>% attr('guide'))
  expect_identical('kg', a %>% filter(Time == 0.25) %>% select(Wt) %$% Wt %>% attr('guide'))
  expect_identical('kg', a %>% arrange(conc) %>% select(Wt) %$% Wt %>% attr('guide'))
  expect_identical(
    'kg',
    a %>%
      select(-Wt) %>%
      left_join(a %>% select(Subject, Wt) %>% unique) %$%
      Wt %>%
      attr('guide')
  )
  expect_identical('kg', a %>% mutate(Wt = Wt * 2) %$% Wt %>% attr('guide'))
  expect_identical(
    'kg',
    a %>%
      group_by(Subject) %>%
      mutate(Wt = Wt * 2) %$% Wt %>% attr('guide'))

})

test_that('mutate preserves attributes on direct assigment',{
  a <- data.frame(wt = 70)
  a %<>% decorate('wt: [ body weight, kg ]')
  a %>% decorations
  a %<>% mutate(WT = wt/2.2)
  a %>% decorations
  expect_identical('kg', attr(a$WT, 'guide'))
})

test_that('mutate forwards attributes of RHS',{
  a <- data.frame(wt = 70)
  a %<>% decorate('wt: [ body weight, kg ]')
  a %>% decorations
  a %<>% mutate(WT = 70 * 2.2)
  a %>% decorations
  expect_identical(NULL, attr(a$WT, 'guide'))
})

test_that('mutate preserves attributes for ifelse()',{
  a <- data.frame(wt = c(70, 80), sex = c(0,1))
  a %<>% decorate('wt: [ body weight, kg ]')
  a %<>% decorate('sex: [ sex, [ female: 0, male: 1]]')
  a %>% decorations
  a %<>% mutate(WT = ifelse(sex, wt, wt * 1.1))
  a %<>% mutate(wt = ifelse(sex, wt, wt * 1.1))
  a %>% decorations
  expect_identical('sex', attr(a$WT, 'label'))
})

test_that('subsetting dvec returns dvec',{
  a <- as_dvec(1:10)
  expect_true(inherits(a[1], 'dvec'))
  expect_true(inherits(a[1:3], 'dvec'))
})

test_that('pivot_wider preserves attributes and class',{
  a <- data.frame(id = 1:4, wt = c(70, 80, 70, 80), sex = c(0,1,0,1))
  a %<>% decorate('wt: [ body weight, kg ]')
  a %<>% decorate('sex: [ sex, [ female: 0, male: 1]]')
  a %<>% decorate('id: identifier')
  a %<>% mutate(across(everything(), as_dvec))
  as_tibble(a)
  a %>% decorations
  a %<>% resolve(sex)
  a %<>% pivot_wider(names_from = sex, values_from = wt )
  a %>% decorations
  expect_identical(attributes(a$female), attributes(a$male))
  a %<>% pivot_longer(cols = female:male)
  a %>% decorations
  expect_true(inherits(a$value, 'dvec'))
})

test_that('decorate() can class its targets as dvec',{
  a <- data.frame(a = 1:2, b = 3:4)
  b <- data.frame(a = 5:6, b = 7:8)
  a %<>% decorate('
   a: [this, [one: 1, two: 2]]
   b: [that, [three: 3, four: 4]]
  ')
  b %<>% decorate('
   a: [this, [five: 5, six: 6]]
   b: [that, [seven: 7, eight: 8]]
  ')
  decorations(bind_rows(a,b))
  expect_equal_to_reference(file = '107.rds', decorations(bind_rows(a,b)))
})

test_that('bind_rows combines guides and codelists',{
  a <- data.frame(a = 1:2, b = 3:4)
  b <- data.frame(a = 5:6, b = 7:8)
  a %<>% decorate('
   a: [this, [one: 1, two: 2]]
   b: [that, [three: 3, four: 4]]
  ')
  b %<>% decorate('
   a: [this, [five: 5, six: 6]]
   b: [that, [seven: 7, eight: 8]]
  ')
  decorations(bind_rows(a,b))
  expect_equal_to_reference(file = '107.rds', decorations(bind_rows(a,b)))
})

test_that('yamlet persistence can be disabled',{
  a <- data.frame(a = 1:2, b = 3:4)
  options(yamlet_persistence = FALSE)
  a %<>% decorate('
   a: [this, [one: 1, two: 2]]
   b: [that, [three: 3, four: 4]]
  ')
  options(yamlet_persistence = TRUE)
  expect_false(inherits(a$a, 'dvec'))
})

test_that('dvec and units are inter-changeable',{
  library(magrittr)
  library(dplyr)
  a <- data.frame(id = 1:4, wt = c(70, 80, 70, 80), sex = c(0,1,0,1))
  a %<>% decorate('wt: [ body weight, kg ]')
  a %<>% decorate('sex: [ sex, [ female: 0, male: 1]]')
  a %<>% decorate('id: identifier')
  a %<>% resolve
  expect_true(inherits(a$wt,'dvec'))
  a %<>% mutate(wt = as_units(wt))
  expect_true(inherits(a$wt,'units'))
  a %<>% mutate(wt = as_dvec(wt))
  expect_true(inherits(a$wt,'dvec'))
  expect_identical('body weight', attr(a$wt, 'label'))
})

test_that('both fundamental types can be resolved/desolved',{
  a <- data.frame(id = 1:4, wt = c(70, 80, 70, 80), sex = c(0L,1L,0L,1L))
  a %<>% decorate('wt: [ body weight, kg ]')
  a %<>% decorate('sex: [ sex, [ female: 0, male: 1]]')
  a %<>% decorate('id: identifier')
  a
  b <- desolve(resolve(a))
  identical(decorations(a), decorations(b))
  identical(attributes(a), attributes(b))
  identical(names(a), names(b))
  identical(a[[1]], b[[1]])
  identical(a[[2]], b[[2]])
  identical(a[[3]], b[[3]])
  str(a[[3]])
  str(b[[3]])
  expect_identical(
    a,
    a %>% resolve %>% desolve
  )
})

test_that('as.integer.classified() respects yamlet_persistence',{
  options(yamlet_persistence = FALSE)
  expect_identical(
    c('knife','fork','spoon') %>%
      classified %>%
      as.integer %>%
      class,
    'integer'
  )

  options(yamlet_persistence = NULL)
  expect_identical(
    c('knife','fork','spoon') %>%
      classified %>%
      as.integer %>%
      class,
    'dvec'
  )
  
  expect_identical(
    c('knife','fork','spoon') %>%
      classified %>%
      as.integer(persistence = FALSE) %>%
      class,
    'integer'
  )
})

test_that('left_join.decorated works for y; tibble, and data.frame, and decorated',{
  x <- data.frame(
    id = c(1,1,1,2,2,2)
  )
  y <- data.frame(
    id = c(1,2),
    sex = c(0,1)
  )
  expect_identical(3, x %>% left_join(y) %$% sex %>% sum)
  x %<>% decorate('id: subject')
  expect_identical(3, x %>% left_join(y) %$% sex %>% sum)
  expect_identical(3, x %>% left_join(as_tibble(y)) %$% sex %>% sum)
  expect_identical(3, as_tibble(x) %>% left_join(y) %$% sex %>% sum)
  y %<>% decorate('id: subject')
  y %<>% decorate('sex: sex')
  expect_identical(3, x %>% left_join(y) %$% sex %>% sum)
})

test_that('dvec int and double are coerced compatibly during merge',{
  library(vctrs)
  library(yamlet)
  library(dplyr)
  # https://github.com/r-lib/vctrs/issues/1669
  # <dvec<int>> + <dbl> = <dvec<dbl>>
  ptype <- vec_ptype2(as_dvec(1L), 1)
  str(ptype)
  #>  'dvec' num(0)
  expect_true(is.double(ptype))
  
  # try casting both inputs to the <dvec<int>> common type:
  # - <dvec<int>> -> <dvec<dbl>>
  # - <dbl> -> <dvec<dbl>> 
  str(vec_cast_common(as_dvec(1L), 1, .to = ptype))
  #> List of 2
  #>  $ : 'dvec' num 1
  #>  $ : 'dvec' num 1
  

  a <- left_join( # ok
    data.frame(ID = as_dvec(1)),
    data.frame(ID = 1, TIME = 0)
  )
  
  b <- left_join( # ok
    data.frame(ID = as_dvec(1L)),
    data.frame(ID = 1L, TIME = 0)
  )
  
  c <- left_join( # ok!  calls vec_ptype2.dvec.double()
    data.frame(ID = as_dvec(1L)),
    data.frame(ID = 1, TIME = 0)
  )
  
  d <- left_join( # no match, calls vec_ptype2.dvec.integer()
    data.frame(ID = as_dvec(1)),
    data.frame(ID = 1L, TIME = 0)
  )
  
  expect_false(any(is.na(a$TIME)))
  expect_false(any(is.na(b$TIME)))
  expect_false(any(is.na(c$TIME)))
  expect_false(any(is.na(d$TIME)))
  
  a <- left_join( # ok
    data.frame(ID = 1),
    data.frame(ID = as_dvec(1), TIME = 0)
  )
  
  b <- left_join( # ok
    data.frame(ID = 1L),
    data.frame(ID = as_dvec(1L), TIME = 0)
  )
  
  c <- left_join( # ok!  calls vec_ptype2.dvec.double()
    data.frame(ID = 1),
    data.frame(ID = as_dvec(1L), TIME = 0)
  )
  
  d <- left_join( # no match, calls vec_ptype2.dvec.integer()
    data.frame(ID = 1L),
    data.frame(ID = as_dvec(1), TIME = 0)
  )
  
  expect_false(any(is.na(a$TIME)))
  expect_false(any(is.na(b$TIME)))
  expect_false(any(is.na(c$TIME)))
  expect_false(any(is.na(d$TIME)))
  
  # character not automatically coerced to numeric or vice versa
  expect_error(
    left_join(
      data.frame(ID = 1),
      data.frame(ID = '1', TIME = 0)
    )
  )
})

test_that('as.integer.classified preserves tangential attributes',{
  library(magrittr)
  a <- as_dvec(c('a','b','c'), label = 'Letters', title = 'Sample Letters')
  a %<>% classified
  expect_true(all(c('label','title') %in% names(attributes(a))))
})

test_that('yamlet_as_units_preserve functions as expected',{
  library(magrittr)
  a <- as_dvec(1, label = 'height', units = 'cm', title = 'Height (cm)')
  expect_true(
    setequal(
      a %>% as_units %>% attributes %>% names, 
      c('label','units','class')
    )
  )
  expect_true(
    setequal(
      a %>% as_units(preserve = character(0)) %>% attributes %>% names, 
      c('units','class')
    )
  )
  options(yamlet_as_units_preserve = character(0))
  expect_true(
    setequal(
      a %>% as_units(preserve = character(0)) %>% attributes %>% names, 
      c('units','class')
    )
  )
  options(yamlet_as_units_preserve = NULL)
  expect_true(
    setequal(
      a %>% as_units %>% attributes %>% names, 
      c('label','units','class')
    )
  )
  
})

test_that('arbitrated namedList prints correctly',{
  pc <- data.frame(MDV = 0)
  ex <- data.frame(MDV = 1)
  pc %<>% decorate('MDV: [mdv, [Missing: 1, Not Missing: 0 ]]')
  ex %<>% decorate('MDV: mdv')
  out <- capture.output(pc %>% bind_rows(ex) %>% decorations(MDV))
  expect_equal_to_reference(out, '117.rds')
})

test_that('vec_ptype2() conserves attributes if possible',{
  library(vctrs)
  dvc <- as_dvec(1, label = 'yin')
  num <- structure(2, label = 'yang')
  expect_warning({
  a <- c(dvc, num)
  b <- c(num, dvc)
  
  c <- vec_c(dvc, num)
  d <- vec_c(num, dvc)
    
  })
  
  expect_identical(attr(a, 'label'), 'yin')
  expect_identical(attr(b, 'label'), NULL )
  expect_identical(attr(c, 'label'), 'yin')
  expect_identical(attr(d, 'label'), 'yang')
  

})

test_that('casting to dvec always gives dvec',{
  
  expect_true(inherits(vec_cast(TRUE, as_dvec(TRUE)), 'dvec'))
  expect_true(inherits(vec_cast(TRUE, as_dvec(1L)), 'dvec'))
  expect_true(inherits(vec_cast(TRUE, as_dvec(1)), 'dvec'))
  expect_true(inherits(vec_cast(TRUE, as_dvec(1+0i)), 'dvec'))
  expect_error(inherits(vec_cast(TRUE, as_dvec('1')), 'dvec'))
  
  expect_true(inherits(vec_cast(1L, as_dvec(TRUE)), 'dvec'))
  expect_true(inherits(vec_cast(1L, as_dvec(1L)), 'dvec'))
  expect_true(inherits(vec_cast(1L, as_dvec(1)), 'dvec'))
  expect_true(inherits(vec_cast(1L, as_dvec(1+0i)), 'dvec'))
  expect_error(inherits(vec_cast(1L, as_dvec('1')), 'dvec'))
  
  expect_true(inherits(vec_cast(1, as_dvec(TRUE)), 'dvec'))
  expect_true(inherits(vec_cast(1, as_dvec(1L)), 'dvec'))
  expect_true(inherits(vec_cast(1, as_dvec(1)), 'dvec'))
  expect_true(inherits(vec_cast(1, as_dvec(1+0i)), 'dvec'))
  expect_error(inherits(vec_cast(1, as_dvec('1')), 'dvec'))
  
  expect_error(inherits(vec_cast(1+0i, as_dvec(TRUE)), 'dvec'))
  expect_error(inherits(vec_cast(1+0i, as_dvec(1L)), 'dvec'))
  expect_error(inherits(vec_cast(1+0i, as_dvec(1)), 'dvec'))
  expect_true(inherits(vec_cast(1+0i, as_dvec(1+0i)), 'dvec'))
  expect_error(inherits(vec_cast(1+0i, as_dvec('1')), 'dvec'))
  
  expect_error(inherits(vec_cast('1', as_dvec(TRUE)), 'dvec'))
  expect_error(inherits(vec_cast('1', as_dvec(1L)), 'dvec'))
  expect_error(inherits(vec_cast('1', as_dvec(1)), 'dvec'))
  expect_error(inherits(vec_cast('1', as_dvec(1+0i)), 'dvec'))
  expect_true(inherits(vec_cast('1', as_dvec('1')), 'dvec'))
  
})

test_that('casting from dvec never gives dvec',{
  
  expect_false(inherits(vec_cast(as_dvec(TRUE), TRUE), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1L), TRUE), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1), TRUE), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1+0i), TRUE), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec('1'), TRUE), 'dvec'))
  
  expect_false(inherits(vec_cast(as_dvec(TRUE), 1L), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1L), 1L), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1), 1L), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1+0i), 1L), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec('1'), 1L), 'dvec'))
  
  expect_false(inherits(vec_cast(as_dvec(TRUE), 1), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1L), 1), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1), 1), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1+0i), 1), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec('1'), 1), 'dvec'))
  
  expect_false(inherits(vec_cast(as_dvec(TRUE), 1+0i), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1L), 1+0i), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1), 1+0i), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1+0i), 1+0i), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec('1'), 1+0i), 'dvec'))
  
  expect_false(inherits(vec_cast(as_dvec(TRUE), '1'), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1L), '1'), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1), '1'), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec(1+0i), '1'), 'dvec'))
  expect_false(inherits(vec_cast(as_dvec('1'), '1'), 'dvec'))
  
})

test_that('casting from dvec gives expected class',{
  
  expect_true(inherits(vec_cast(as_dvec(TRUE), TRUE), 'logical'))
  expect_true(inherits(vec_cast(as_dvec(1L), TRUE), 'logical'))
  expect_true(inherits(vec_cast(as_dvec(1), TRUE), 'logical'))
  expect_true(inherits(vec_cast(as_dvec(1+0i), TRUE), 'logical'))
  expect_true(inherits(vec_cast(as_dvec('1'), TRUE), 'logical'))
  
  expect_true(inherits(vec_cast(as_dvec(TRUE), 1L), 'integer'))
  expect_true(inherits(vec_cast(as_dvec(1L), 1L), 'integer'))
  expect_true(inherits(vec_cast(as_dvec(1), 1L), 'integer'))
  expect_true(inherits(vec_cast(as_dvec(1+0i), 1L), 'integer'))
  expect_true(inherits(vec_cast(as_dvec('1'), 1L), 'integer'))
  
  expect_true(inherits(vec_cast(as_dvec(TRUE), 1), 'numeric'))
  expect_true(inherits(vec_cast(as_dvec(1L), 1), 'numeric'))
  expect_true(inherits(vec_cast(as_dvec(1), 1), 'numeric'))
  expect_true(inherits(vec_cast(as_dvec(1+0i), 1), 'numeric'))
  expect_true(inherits(vec_cast(as_dvec('1'), 1), 'numeric'))
  
  expect_true(inherits(vec_cast(as_dvec(TRUE), 1+0i), 'complex'))
  expect_true(inherits(vec_cast(as_dvec(1L), 1+0i), 'complex'))
  expect_true(inherits(vec_cast(as_dvec(1), 1+0i), 'complex'))
  expect_true(inherits(vec_cast(as_dvec(1+0i), 1+0i), 'complex'))
  expect_true(inherits(vec_cast(as_dvec('1'), 1+0i), 'complex'))
  
  expect_true(inherits(vec_cast(as_dvec(TRUE), '1'), 'character'))
  expect_true(inherits(vec_cast(as_dvec(1L), '1'), 'character'))
  expect_true(inherits(vec_cast(as_dvec(1), '1'), 'character'))
  expect_true(inherits(vec_cast(as_dvec(1+0i), '1'), 'character'))
  expect_true(inherits(vec_cast(as_dvec('1'), '1'), 'character'))
  
})

test_that('resolve is idempotent on classifiable dvec',{
  a <- as_dvec(c('a','b','c'), guide = as.list(c('a','b','c')))
  a <- resolve(a)
  expect_silent(a <- resolve(a))
})

test_that('[ can be part of a code or decode', {
  library(magrittr)
  x <- data.frame(col = '[')
  x %<>% decorate( 'col: [ column, ["[foo]": "[" ]]')
  tmp <- tempfile(fileext = '.csv')
  x %>% io_csv(tmp)
  tmp %>% io_csv %>% resolve
  tmp %>% sub('csv$','yaml', .) %>% readLines -> foo
  expect_identical(foo, "col: [ column, [ '[foo]': '[' ]]")
  
})

test_that('dvec can be combined with character, etc.', {
  library(magrittr)
  library(tidyr)
  x <- as_dvec('a', label = 'foo')
  y <- 'b'
  str(c(x, y))
  str(c(y, x)) # could this do better?
  str(vctrs::vec_c(y, x))
  
  # what happens when we pivot_longer on a mixed table?
  x <- data.frame(
    ID = c(1,1,2,2),
    OBS1 = c(3,4,5,6),
    OBS2 = c(7,8,9,10)
  )
  x %<>% decorate('OBS2: foo')
  x %<>% pivot_longer(c(OBS1, OBS2)) # works great
  x %>% decorations # combined column is dvec! Even if OBS1 is decorated instead.
  x <- data.frame(
    ID = c(1,1,2,2),
    OBS1 = c(3,4,5,6),
    OBS2 = c('a','b','c','d')
  )
  #x %<>% decorate('OBS1: foo')
  x %<>% pivot_longer(c(OBS1, OBS2)) # does not work, with or w/o the decoration
  

})

### errors at dplyr 1.0.10, but not for dev version

# test_that('ifelse() returns dvec if true or false is dvec',{
#   # https://vctrs.r-lib.org/articles/s3-vector.html :
#   # Unfortunately there’s no way to fix this problem with the current design of c().
#   x <- data.frame(EVID = c(1,0,1,0), MDV = c(0,0,0,0))
#   x %<>% decorate('
#   EVID: [ Event Identifier, [ Dose: 1, Observation: 0]]
#   MDV: [ Missing Dependent Value, [ DV Not Missing: 0, DV Missing: 1]]
#   ')
#   x %>% decorations
#   class(x$MDV)
#   c(x$MDV, 1) # magic
#   c(1, x$MDV) # no magic
#   c(as_dvec(1), x$MDV) # magic
#   # can't rescue ifelse() by coercing to dvec
#   y <- x %>% mutate(MDV =  ifelse(EVID == 1, as_dvec(1, label = 'foo'), MDV))
#   expect_false(inherits(y$MDV, 'dvec'))
#   # CAN rescue if_else() by coercing to dvec
#   
#   # expect_warning( does not warn under dplyr, 1.0.10, did warn for dev version
#     z <- x %>% mutate(MDV = if_else(EVID == 1, as_dvec(1, label = 'foo'), MDV))
#   # )
#   expect_true(inherits(z$MDV, 'dvec'))
#   
#   # as of 0.10.7, coercion is implicit:
#   # gives error in dplyr 1.0.10: `false` must have class `numeric`, not class `dvec`.
#   expect_warning(
#     z <- x %>% mutate(MDV = if_else(EVID == 1, structure(1, label = 'foo'), MDV))
#   )
#   expect_true(inherits(z$MDV, 'dvec'))
#   
#   # as of 0.10.7, case_when no longer requires dvec in both positions:
#   # gives error in dplyr 1.0.10
#   expect_identical(
#     x %>% mutate(
#       MDV = case_when(
#         EVID == 1 ~ 1,
#         TRUE ~ MDV
#       )
#     ) %$% MDV %>% attr('label'), 'Missing Dependent Value'
#   )
#   
#  # case_when preserves first attributes
#   expect_warning(
#     expect_identical(
#       x %>% mutate(
#         MDV = case_when(
#           EVID == 1 ~ structure(1, label = 'foo'),
#           TRUE ~ MDV
#         )
#       ) %$% MDV %>% attr('label'), 'foo'
#     ))
#   
#   # case_when preserves first attributes
#   # gives error in dplyr 1.0.10
#   expect_identical(
#     x %>% mutate(
#       MDV = case_when(
#         EVID != 1 ~ MDV,
#         TRUE ~ 1
#       )
#     ) %$% MDV %>% attr('label'), 'Missing Dependent Value')
#   
# })
# 
# test_that('case_when type mismatch gives meaningful error',{
#   library(yamlet)
#   library(magrittr)
#   library(dplyr)
#   a <- data.frame(
#     EVID = c(1, 0, 0),
#     MDV = c(0, 0, 0)
#   )
#   a %<>% decorate('
#     EVID: [ Event ID, [ Observation: 0, Dose: 1]]
#     MDV: [ Missing DV, [ Not Missing: 0, Missing: 1]]
#   ')
#   
#   
#   a %>% mutate(MDV = case_when(EVID == 0 ~        MDV, TRUE ~ 1   )) %>% str # magic
#   a %>% mutate(MDV = case_when(EVID == 1 ~          1, TRUE ~ MDV )) %>% str # magic as of 0.10.7
#   a %>% mutate(MDV = case_when(EVID == 1 ~ as_dvec(1), TRUE ~ MDV )) %>% str # magic
#   a %>% mutate(MDV =    ifelse(EVID == 1,           1, MDV        )) %>% str # no magic
#   a %>% mutate(MDV =    ifelse(EVID == 0,         MDV, 1          )) %>% str # no magic
#   a %>% mutate(MDV =   if_else(EVID == 0,         MDV, 1          )) %>% str # magic
#   a %>% mutate(MDV =   if_else(EVID == 1,           1, MDV        )) %>% str # magic as of 0.10.7
#   a %>% mutate(MDV =   if_else(EVID == 1,  as_dvec(1), MDV        )) %>% str # magic
# 
#   c(1, a$MDV) %>% str # no magic
#   c(a$MDV, 1) %>% str # magic
#   vctrs:::vec_c(1, a$MDV) %>% str # magic as of 0.10.7
#   vctrs:::vec_c(a$MDV, 1) %>% str # magic
#   
#   b <- case_when(c(TRUE, FALSE) == TRUE ~ 1, TRUE  ~ as_dvec(1, label = 'foo'))
#   c <- case_when(c(TRUE, FALSE) == TRUE ~ as_dvec(1, label = 'foo'), TRUE  ~ 1)
#   d <- case_when(c(TRUE, FALSE) == TRUE ~ as_dvec(1), TRUE  ~ as_dvec(1, label = 'foo'))
#   
#   # With dplyr_1.0.99.9000, at 0.10.7
#   # case_when(num, dvec) returns decorated dvec
#   # case_when(dvec, num) returns decorated dvec
#   # case_when(dvec, decorated dvec) returns decorated dvec
#   # perfect!
#   
#   expect_identical(b,c)
#   expect_identical(b,d)
#   
#   # case_when does automatic type coercion
#   expect_identical(
#   case_when(c(TRUE, FALSE) == TRUE ~ 1L, TRUE  ~ as_dvec(1, label = 'foo')),
#   case_when(c(TRUE, FALSE) == TRUE ~ 1, TRUE  ~ as_dvec(1L, label = 'foo'))
#   )
#   
#   
#   c(2L, as_dvec(1L)) %>% str # no magic
#   c(as_dvec(1L, label = 'foo'), 2L) %>% str # magic
#   ifelse(FALSE, as_dvec(1L, label = 'foo'),2L) %>% str # no magic
#   if_else(FALSE, as_dvec(1L, label = 'foo'),2L) %>% str # magic
#   
#   # with dev version of dplyr, no longer seeing "dvec must have class dvec" error
# 
# })
# 
# test_that('if_else and case_when conserve decorations',{
#   library(yamlet)
#   library(magrittr)
#   library(dplyr)
#   a <- data.frame(
#     EVID = c(0L, 1L, 0L),
#     MDV = c(0L, 0L, 0L)
#   )
#   a %<>% decorate('
#     EVID: [ Event ID, [ Observation: 0, Dose: 1]]
#     MDV: [ Missing DV, [ Not Missing: 0, Missing: 1]]
#   ')
# 
#  a %>% decorations
#  a 
#  # The programmatic goal is to set MDV to 1 where EVID is 1.
#  # One problem is that the user is likely to assign 'double' to this integer.
#  # Another problem is that decorations historically have been destroyed here.
#  
#  # Most intuitive code is this. Note MDV becomes num and decorations are preserved as of 0.10.7.
#  expect_identical(
#   a %>% mutate(MDV = if_else(EVID == 1, 1, MDV))  %$% MDV %>% attr('label'), 'Missing DV'
#  )
#  
# 
#  # This version is less intuitive.  MDV becomes num and decorations are preserved.
#  expect_identical(
#    a %>% mutate(MDV = if_else(EVID != 1, MDV, 1))  %$% MDV %>% attr('label'), 'Missing DV'
#  )
#  
#  # This version is easy to understand, MDV stays int, decorations are preserved.
#  expect_identical(
#  a %>% mutate(MDV = MDV %>% replace(EVID == 1, 1)) %$% MDV %>% attr('label'), 'Missing DV'
# )
#  # This version is intuitive, but like if_else coerces to dvec num and drops decorations.
#  expect_identical(
#  a %>% mutate(MDV = case_when(EVID == 1 ~ 1, TRUE ~ MDV)) %$% MDV %>% attr('label'), 'Missing DV'
# )
#  # This version less intuitive, but like if_else coerces to dvec num and preserves decor.
#  expect_identical(
#  a %>% mutate(
#    MDV = case_when(
#      EVID != 1 ~ MDV, 
#      EVID == 1 ~ 1
#    )
#  )  %$% MDV %>% attr('label'), 'Missing DV'
#  )
#  
#  # something similar going on with vec_c, i.e. order independence at 0.10.7
#  expect_identical(
#  vctrs::vec_c(1, as_dvec(1L, label = 'foo')),
#  vctrs::vec_c(as_dvec(1L, label = 'foo'), 1)
#  )
#  
#  # of course, coercing to dvec gets it right
#  expect_identical(
#  vctrs::vec_c(as_dvec(1), as_dvec(1L, label = 'foo')),
#  vctrs::vec_c(as_dvec(1L, label = 'foo'), as_dvec(1))
#    
#  )
#  
#  # is replace() na-safe?  Does an NA in the condition destroy good data?
#  b <- data.frame(
#    EVID = c(0L, 1L, NA),
#    MDV = c(0L, 0L, 0L)
#  )
#  
#  expect_false(
#  b %>% mutate(MDV = MDV %>% replace(EVID == 1, 1)) %$% MDV %>% is.na %>% any
#    
#  )
#  # yes!  MDV not disturbed where EVID == 1 returns NA.
#  
# })

