library(testthat)
library(yamlet)
library(dplyr)
library(magrittr)
library(ggplot2)
test_that('print.dg treats variable as categorical if guide has length > 1',{
  file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
  library(ggplot2)
  library(dplyr)
  library(magrittr)
  file %>% decorate %>% filter(!is.na(conc)) %>%
    ggplot(aes(x = time, y = conc, color = Heart)) + geom_point()
  # look for legend: congestive heart failure (mod/no/sev)
})

test_that('print.dg uses conditional labels and guides',{
  file <- system.file(package = 'yamlet', 'extdata','phenobarb.csv')
  file %>% decorate %>%
    filter(event == 'conc') %>%
    ggplot(aes(x = time, y = value, color = ApgarInd)) + geom_point()
  # look for y axis: serum phenobarbital concentration (only true if event == conc)
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
  # nonsensical, but shows injection of new layer with categories
  y <-
    c %>% filter(event == 'conc') %>%
    ggplot2:::ggplot.default(aes(x = time, y = value, color = ApgarInd)) + geom_point() +
    d %>% filter(!is.na(conc)) %>%
    geom_point(data = ., aes(x = time/10, y = conc*10, color = Heart))
  # as above, without the benefit of metadata (see reduced axis labels)
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
  # note that x axis labels are 2-line, as is x-axis category labels
})

test_that('subplots respect metadata assignments',{
  library(ggplot2)
  library(magrittr)
  library(dplyr)
  library(gridExtra)
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
    a %>% filter(event == 'conc') %>%
    ggplot2:::ggplot.default(aes(x = time, y = value, color = ApgarInd)) + geom_point() +
    d %>% filter(!is.na(conc)) %>%
    geom_point(data = ., aes(x = time/10, y = conc*10, color = Heart))

  grid.arrange(x, y)
  # note informative axis labels in first panel
  
  p <- x %>% ggplot_build
  q <- p %>% ggplot_gtable
  plot(q)
  expect_equal_to_reference(file = '098.rds', p)
  
  foo <- ggplot_build(x)
  bar <- print(x)

})

test_that('print method for decorated_ggplot supports colour, fill, size, shape, linetype, alpha',{
  x <- data.frame(x = c(1:6, 3:8), y = c(1:6,1:6), z = letters[c(1:6,1:6)])
  x %<>% decorate('z: [color: ["red", "blue", "green", "gold", "black", "magenta"]]')
  x %<>% decorate('z: [fill: ["red", "blue", "green", "gold", "black", "magenta"]]')
  x %<>% decorate('z: [shape: [20, 21, 22, 23, 24, 25]]')
  x %<>% decorate('z: [linetype: [6, 5, 4, 3, 2, 1]]')
  x %<>% decorate('z: [alpha: [ .9, .8, .7, .6, .5, .4]]')
  x %<>% decorate('z: [size: [1, 1.5, 2, 2.5, 3, 3.5]]')

  # undebug(yamlet:::print.decorated_ggplot)
  
  x %>% ggplot(aes(
    x, y,
    color = z,
    fill = z,
    shape = z,
    linetype = z, 
    alpha = z,
    size = z,
  )) + 
    geom_point() +
    geom_line(size = 1)
  })
# notice that all aesthetics are supported.  Seems like under certain circumstances,
# there is a warning not to use discrete scale for continuous vars.

test_that('print method for decorate_ggplot respects aesthetics with assignment priority of sort-unique, guide, factor levels, codelist',{
  
})
