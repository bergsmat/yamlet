library(testthat)
library(magrittr)
library(yaml)

test_that('io_csv methods support gzip compression seamlessly',{
  options(csv_source = FALSE)
  x <- Theoph %>% head %>% data.frame
  x$Subject %<>% as.character %>% as.integer
  x %<>% decorate('
  Subject: Subject
  Wt: [ Weight, kg ]
  Dose: [Dose, mg/kg ]
  Time: [Time, h]
  conc: [Plasma Concentration, ng/mL]
  ')
  # first three bytes of a gzipped file
  gzipped <- function(file){
    bytes <- charToRaw('\037\x8b\b')
    first <- readBin(file, 'raw', n = 3L)
    identical(first, bytes)
  }

  # the usual case
  file <- tempfile(fileext = '.csv')
  y <- x %>% io_csv(file) %>% io_csv
  expect_identical(x, y)
  expect_false(gzipped(file))
  
  # signal gz
  file <- tempfile(fileext = '.csv.gz')
  y <- x %>% io_csv(file) %>% io_csv
  expect_identical(x, y)
  expect_true(gzipped(file))
  
  # suppress compression
  file <- tempfile(fileext = '.csv.gz')
  z <- io_csv(x, file, gz = FALSE)
  expect_false(gzipped(file))
  y <- io_csv(z, gz = FALSE)
  expect_identical(x, y)

  # manual compression
  file <- tempfile()
  meta <- tempfile()
  con1 <- gzfile(file)
  z <- io_csv(x, file = con1, meta = meta, gz = TRUE)
  expect_true(gzipped(file))
  y <- io_csv(file, meta = meta)
  expect_identical(x, y)
  
  # explicit compression
  file <- tempfile(fileext = '.csv')
  z <- io_csv(x, file, gz = TRUE)
  expect_true(gzipped(z))
  y <- io_csv(z, gz = TRUE)
  expect_identical(x, y)
  y <- io_csv(z)
  expect_identical(x, y)
  
  # explicit non-standard meta file
  file <- tempfile(fileext = '.gz')
  meta <- tempfile()
  z <- io_csv(x, file, meta = meta, gz = TRUE)
  expect_identical(file, z)
  expect_true(file.exists(file))
  expect_true(file.exists(meta))
  expect_true(gzipped(z))
  expect_false(gzipped(meta))
  y <- io_csv(z, meta = meta)
  expect_identical(x, y)

})

