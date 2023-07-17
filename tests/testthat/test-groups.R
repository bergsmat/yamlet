library(testthat)
library(magrittr)
library(dplyr)

test_that('decorate_groups clears existing specifications',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% decorate_groups(Subject, Time)
  expect_identical(decorations_groups(test), c('Subject','Time'))
  test %<>% decorate_groups
  expect_identical(decorations_groups(test), character(0))
})
test_that('decorate_groups sets decorations sequentially',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% decorate_groups(Subject, Time)
  expect_true(attr(test$Time, 'groups') == 2) 
})
test_that('decorate_groups honors defaults',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% group_by(Subject, Time)
  test %<>% decorate_groups
  expect_identical(decorations_groups(test), c('Subject','Time'))
  })
test_that('decorate_groups errors if column not present',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  expect_error(decorate_groups(test, Foo, Time))
})

test_that('decorations_groups recovers groups decorations as character',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% decorate_groups(Subject, Time)
  expect_true(is.character(decorations_groups(test)))
})
test_that('decorations_groups sorts groups decorations',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% decorate_groups(Subject, Time)
  test %<>% select(Time, everything()) 
  expect_identical(decorations_groups(test), c('Subject','Time'))
})
test_that('decorations_groups warns missing indices',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test$foo <- 1
  test %<>% decorate_groups(Subject, foo, Time)
  test$foo <- NULL
  expect_warning(decorations_groups(test))
})
test_that('decorations_groups errors on non-integer',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% modify(Subject, Time, groups = 2.5)
  expect_error(decorations_groups(test))
  
})
test_that('decorations_groups warns duplicated indices',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% modify(Subject, Time, groups = 2L)
  expect_warning(decorations_groups(test))
})

test_that('group_by_decorations returns a tbl_df',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  expect_false(inherits(test, 'tbl_df'))
  test %<>% group_by_decorations
  expect_true(inherits(test, 'tbl_df'))
  test %<>% decorate_groups(Subject, Time)
  test %<>% group_by_decorations
  expect_true(inherits(test, 'tbl_df'))
})
test_that('group_by_decorations honors NULL decorations',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% group_by_decorations
  expect_identical(groups(test), list())
  
})
test_that('group_by_decorations honors group decorations',{
  test <- data.frame(lapply(Theoph, type.convert, as.is = TRUE))
  test %<>% decorate_groups(Subject, Time)
  test %<>% group_by_decorations
  expect_identical(group_vars(test), c('Subject', 'Time'))
})

