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
  # 2025-02-18 as of ggplot2_3.5.1.9000, we see informative axis labels also in y.
  # this is expected, because updated ggplot honors label attributes.
  
  p <- x %>% ggplot_build
  q <- p %>% ggplot_gtable
  plot(q)
  file <- '098.rds'
  if(yamlet:::gg_new()) file <- '098.1.rds'
  expect_equal_to_reference(file = file, p)
  
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
    size = z,
    alpha = z
  )) + 
    geom_point() +
    geom_line(linewidth = 1)
  
# notice that all aesthetics are supported.  Seems like under certain circumstances,
# there is a warning not to use discrete scale for continuous vars.
# 2025-02-18 As of ggplot2_3.5.1.9000 it appears to be working.
  
})

test_that('print method for decorate_ggplot respects aesthetics with assignment priority of sort-unique, guide, factor levels, codelist',{
  
})

test_that('print.decorated_ggplot correctly handles spork for x axis, y axis, facet labels, and legends',{
  library(magrittr)
  library(ggplot2)
  library(tablet)
  library(yamlet)
  library(dplyr)
 
  x <- data.frame(
    time = 1:10, 
    work = (1:10)^1.5, 
    group = 1:2, 
    set = c(rep('delta',5), rep('gamma', 5))
  )
  x %<>% decorate('
 time: [ Time_cum.^alpha, h ]
 work: [ Work_total_obs\\n, kg*m^2/s^2 ]
 group: [ Group, [ Second\\nGroup^\\*: 2, First\\nGroup^#: 1 ]]
 set: [ Set, [ gamma, delta ]]
')
  
  # note the special headers and group levels
  x %>% 
    enscript %>% 
    group_by(group, set) %>%
    tablet %>%
    as_kable
  
  # undebug(ggplot2:::ggplot.default)
  # undebug(yamlet:::ggplot.decorated)
  # undebug(ggplot2:::ggplot_build.ggplot)
  # undebug(yamlet:::ggplot_build.decorated_ggplot)
  # undebug(yamlet:::.decorated_ggplot)
  # undebug(ggplot2:::print.ggplot)
  # undebug(yamlet:::print.decorated_ggplot)
  
  # note column names (3.5.1) or labels (later) as axis titles
  x %>% 
    ggplot(aes(time, work, color = group, shape = set)) + 
    geom_point()

  # note the literal axes and legends
  x %>% 
    resolve %>% 
    ggplot(aes(time, work, color = group, shape = set)) + 
    geom_point()
  
  # note the special axes and legends
  x %>% 
    enscript %>%
    ggplot(aes(time, work, color = group, shape = set)) + 
    geom_point()

  # must work for facet_wrap and facet_grid
  x %>% 
    enscript %>%
    ggplot(aes(time, work, color = group, shape = set)) + 
    geom_point() +
    facet_grid(set~group) 
  x %>% 
    enscript %>% 
    ggplot(aes(time, work, color = group, shape = set)) + 
    geom_point() +
    facet_wrap(~ set + group)
  
})
