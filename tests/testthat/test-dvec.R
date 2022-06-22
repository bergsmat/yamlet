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
  b <- bind_rows(a, rev(a))
  decorations(b)
  expect_identical(attr(b$Subject, 'label'), 'subject')
  expect_identical(attr(b$Dose, 'label'), 'Dose')
  expect_identical(attr(b$conc, 'guide'), 'ng/mL')
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

test_that('dvec of both fundamental types can be resolved/desolved',{
  
})


test_that('easy to compare data to guide levels',{})
test_that('easy to add NA to guide levels',{})








