test_that('classified factors can be combined with each other and with vanilla factors',{
  set.seed(0)
  library(magrittr)
  library(dplyr)
  library(yamlet)
  library(tidyr)
  library(ggplot2)
  library(vctrs)
  library(testthat)
  x <- data.frame(id = 1:4)
  x %<>% mutate(wt  = 78:81)
  x %<>% mutate(age = 18:21)
  x %<>% mutate(sex = c(0, 1, 0, 1))
  x %<>% mutate(coh = c(1, 2, 3, 2))
  x %<>% mutate(CL  = rnorm(id, 10, 15) %>% signif)
  x %<>% mutate(V   = rnorm(id, 3, 0.25) %>% signif)
  
  x %<>% decorate('
    id: Subject ID
    wt:  [ Body Weight, kg]
    age: [ Age, year]
    sex: [ Sex, [ Female: 0, Male: 1 ]]
    coh: [ Cohort, [ Cohort 1: 1, Cohort 2: 2, Cohort 3: 3]]
    CL:  [ CL/F, L/h]
    V:   [ V/F, L ]
')

  # works, but gives appropriate warnings:
  expect_warning(x %>% pivot_longer(c(coh, sex)))

  # should succeed by casting to factor if attr not compatible:
  x %>% resolve %>% pivot_longer(c(coh, sex))
  x %>% resolve %>% pivot_longer(c(coh, sex)) %>% decorations
  
  # don't handle:
  # x %>% resolve(coh) %>% pivot_longer(c(coh, sex))
  
  x %<>% resolve
  y <- x
  y$coh %<>% factor
  y$sex %<>% factor
  
  # attributes on right missing, attributes on left dropped:
  rbind(x, y)

  # classified~factor: If factor levels match exactly, promote factor to classified and keep attributes of classified
  bind_rows(x, y) %>% decorations
  bind_rows(y, x) %>% decorations
  
  vec_c(x$sex, y$sex)
  vec_c(y$sex, x$sex) # no label for y$sex; new behavior in 1.3.1
  
  
  z <- y %>% rename(coh = sex, sex = coh)
  
  # attributes on right missing, attributes on left dropped:
  rbind(x, z)

  
  # classified~factor: if factor levels don't match, demote classified to factor and adopt default behaviors
  bind_rows(x, z) # labels preserved
  
  a <- x %>% rename(coh = sex, sex = coh)
  
  # classified~classified: demotes both sides to factor
  rbind(x, a)  
  
  # classified-classified: if factor levels don't match drop codelist names
  bind_rows(x, a)
  
  c1 <- classified('a', levels = c('a','b')) %>% structure(label = 'c1')
  c2 <- classified('b', levels = c('a','b')) %>% structure(label = 'c2')
  c3 <- classified('c', levels = c('a','c')) %>% structure(label = 'c3')
  f1 <- factor('a', levels = c('a','b')) %>% structure(label = 'f1')
  f2 <- factor('b', levels = c('a','b')) %>% structure(label = 'f2')
  f3 <- factor('c', levels = c('a','c')) %>% structure(label = 'f3')
 
  expect_equal_to_reference(file = '122.rds', vec_c(c1, c1))
  expect_equal_to_reference(file = '123.rds', vec_c(c1, c2))
  expect_equal_to_reference(file = '124.rds', vec_c(c2, c1))
  expect_equal_to_reference(file = '125.rds', vec_c(c1, c3))
  expect_equal_to_reference(file = '126.rds', vec_c(c3, c1))
  expect_equal_to_reference(file = '127.rds', vec_c(c1, f1))
  expect_equal_to_reference(file = '128.rds', vec_c(f1, c1))
  expect_equal_to_reference(file = '129.rds', vec_c(c1, f2))
  expect_equal_to_reference(file = '130.rds', vec_c(f2, c1))
  expect_equal_to_reference(file = '131.rds', vec_c(c1, f3))
  expect_equal_to_reference(file = '132.rds', vec_c(f3, c1))
})

test_that('items with an empty list as guide resolve to classified',{
  library(magrittr)
  library(dplyr)
  library(yamlet)
  library(testthat)
  x <- data.frame(ID = 1:3)
  x %<>% redecorate('ID: [ Identifier, []]')
  x %<>% resolve
  expect_true(is.factor(x$ID))
  x %<>% desolve(collapse = 0)
  expect_false(is.factor(x$ID))
  expect_true(length(attr(x$ID, 'guide')) == 0)
  decorations(x)
})
test_that('decorated factor honors codelist',{
  library(magrittr)
  x <- factor(1:3)
  attr(x, 'guide') <- list(a = 1, b = 2, c = 3)
  expect_identical(type.convert(x, as.is=TRUE), 1:3)
  expect_identical(type.convert(resolve(x), as.is=TRUE), c('a','b','c'))
  x <- data.frame(bar = x)
  x
  resolve(x)
  expect_identical(
    c('a','b','c'),
    x %>% resolve %$% bar %>% type.convert(as.is=TRUE)
  )
  
})

test_that('by default, combining classified reconciles attributes',{
  a <- classified(letters[1:3])
  b <- classified(letters[3:5])
  attr(a, 'priority') <- 1
  attr(b, 'priority') <- 2
  attr(b, 'case') <- 'lower'
  c <- c(a,b)
  expect_equal_to_reference(file = '136.rds', c)
  
})

