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

test_that('reconciliation of attributes is as comprehensive',{
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
  pivot_longer(a, Wt:conc)

})
test_that('dplyr verbs preserve attributes',{})
test_that('mutate preserves attributes on direct assigment',{})
test_that('mutate preserves attributes on both sides of ifelse()',{})










