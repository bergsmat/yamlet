## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
meta <- system.file(package = 'yamlet', 'extdata','quinidine.yaml')


## -----------------------------------------------------------------------------
x <- data.frame(
  ID = 1, 
  CONC = 1,
  RACE = 1
)
library(dplyr)
library(magrittr)
library(yamlet)

x$ID %<>% structure(label = 'subject identifier')
x$CONC %<>% structure(label = 'concentration', guide = 'ng/mL')
x$RACE %<>% structure(label = 'race', guide = list(white = 0, black = 1, asian = 2))

x %>% as_yamlet %>% as.character %>% writeLines

# or

x %>% as_yamlet %>% as.character %>% writeLines(file.path(tempdir(), 'drug.yaml'))


## -----------------------------------------------------------------------------
meta <- read_yamlet(file.path(tempdir(), 'drug.yaml'))
meta

## -----------------------------------------------------------------------------
x <- data.frame(ID = 1, CONC = 1, RACE = 1)
x <- decorate(x, meta)
str(x)

## -----------------------------------------------------------------------------
decorations(x) # just a list
as_yamlet(x) # a list with class 'yamlet' (special print method)

## -----------------------------------------------------------------------------
file <- file.path(tempdir(), 'out.yaml')
write_yamlet(x, file )
file %>% readLines %>% writeLines

## -----------------------------------------------------------------------------
library(csv)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
a <- decorate(file)
as_yamlet(a)

## -----------------------------------------------------------------------------
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)
out <- file.path(tempdir(), 'out.csv')
io_csv(x, out)
y <- io_csv(out)
attr(x, 'source') <- NULL
attr(y, 'source') <- NULL
identical(x, y) # lossless 'round-trip'
file.exists(out)
meta <- sub('csv','yaml', out)
file.exists(meta)
meta %>% readLines %>% head %>% writeLines

## ---- fig.width = 5.46, fig.height = 3.52, fig.cap = 'Automatic axis labels and legends using curated metadata as column attributes'----
library(ggplot2)
library(dplyr)
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')

(file %>% 
  decorate %>% 
  filter(!is.na(conc)) %>%
  agplot(aes(x = time, y = conc, color = Heart)) + 
  geom_point()) 


