## ----message = FALSE----------------------------------------------------------
library(magrittr)
library(ggplot2)
library(yamlet)
library(dplyr)
library(metaplot)

set.seed(10)
obs <- data.frame(
  time = 1:100,
  group = c('a','b'),
  concentration = rnorm(100)
)

# observed data
obs %<>% decorate('
  time: [ Time, h ]
  concentration: [ Concentration, ng/mL ]
  group: [ 
    Group, 
    [ a, b ], 
    color: [ green, yellow ] , 
    linetype: [ dashed, dotdash ]
  ]
')

# summary data
sum <- obs %>% group_by(group) %>% summarize(mean = mean(concentration))
sum %<>% undecorate %>% decorate('
  group: [ 
    Subset, 
    [ a, b ], 
    color: [ blue, magenta ], 
    shape: [ 15, 19 ]
  ]
')


q <- obs %>%
  undecorate %>%
  ggplot(aes(time, concentration)) +
  geom_point(aes(color = group, shape = group)) +
  geom_hline(
    data = undecorate(sum), 
    aes(
      yintercept = mean, 
      color = group, 
      linetype = group
    )
  ) + 
  theme(legend.position = "top") +
  labs(subtitle = 'without decorations') +
  symmetric()

p <- obs %>%
  resolve %>%
  ggplot(aes(time, concentration)) +
  geom_point(aes(color = group, shape = group)) +
  geom_hline(
    data = resolve(sum), 
    aes(
      yintercept = mean, 
      color = group, 
      linetype = group
    )
  ) + 
  theme(legend.position = "top") +
  labs(subtitle = 'with decorations') + 
  symmetric()


multiplot(q, p) # %>% devsize(2,2)

## -----------------------------------------------------------------------------
sessionInfo()

