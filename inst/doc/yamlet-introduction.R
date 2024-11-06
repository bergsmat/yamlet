## ----setup, include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(package.startup.message = FALSE)

## ---- package.startup.message = FALSE-------------------------------------------
suppressMessages(library(dplyr))
library(magrittr)
library(yamlet)
x <- data.frame(
  ID = 1, 
  CONC = 1,
  RACE = 1
)

x$ID %<>% structure(label = 'subject identifier')
x$CONC %<>% structure(label = 'concentration', guide = 'ng/mL')
x$RACE %<>% structure(label = 'race', guide = list(white = 0, black = 1, asian = 2))

x %>% as_yamlet %>% as.character %>% writeLines

# or

x %>% as_yamlet %>% as.character %>% writeLines(file.path(tempdir(), 'drug.yaml'))


## -------------------------------------------------------------------------------
meta <- read_yamlet(file.path(tempdir(), 'drug.yaml'))
meta

## -------------------------------------------------------------------------------
x <- data.frame(ID = 1, CONC = 1, RACE = 1)
x <- decorate(x, meta = meta)
decorations(x)

## -------------------------------------------------------------------------------
x <- data.frame(ID = 1, CONC = 1, RACE = 1)
x <- decorate(x,'
ID: subject identifier
CONC: [ concentration, ng/mL ]
RACE: [ race, [white: 0, black: 1, asian: 2 ]]
')
decorations(x)

## -------------------------------------------------------------------------------
decorations(x)

## -------------------------------------------------------------------------------
file <- file.path(tempdir(), 'out.yaml')
write_yamlet(x, con = file )
file %>% readLines %>% writeLines

## -------------------------------------------------------------------------------
library(csv)
# see ?Quinidine in package nlme
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
a <- decorate(file)
as_yamlet(a)[1:3]

## -------------------------------------------------------------------------------
options(csv_source = FALSE) # see ?as.csv
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')
x <- decorate(file)
out <- file.path(tempdir(), 'out.csv')
io_csv(x, out)
y <- io_csv(out)
identical(x, y) # lossless 'round-trip'
file.exists(out)
meta <- sub('csv','yaml', out)
file.exists(meta)
meta %>% readLines %>% head %>% writeLines
options(csv_source = TRUE) # restore

## ---- fig.width = 5.46, fig.height = 3.52, fig.cap = 'Automatic axis labels and legends using curated metadata as column attributes.'----
suppressWarnings(library(ggplot2))
library(dplyr)
library(magrittr)
file <- system.file(package = 'yamlet', 'extdata','quinidine.csv')

file %>% 
  decorate %>% 
  filter(!is.na(conc)) %>% 
  resolve %>%
  ggplot(aes(x = time, y = conc, color = Heart)) + 
  geom_point()


## -------------------------------------------------------------------------------
suppressMessages(library(table1))
file %>%
  decorate %>% 
  resolve %>% 
  group_by(Subject) %>%
  slice(1) %>%
  table1(~ Age + Weight + Race | Heart, .)

