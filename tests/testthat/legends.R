# demonstration of native ggplot legend behavior 
library(magrittr)
library(dplyr)
library(ggplot2)
set.seed(10)
obs <- data.frame(
  time = 1:100,
  group = c('a','b'),
  static = 'c',
  concentration = rnorm(100)
)


# summary data
sum <- obs %>% group_by(group, static) %>% summarize(mean = mean(concentration))
sum$class <- 'summary'

head(obs)
sum

attr(obs$time, 'label') <- 'Time'
attr(obs$concentration, 'label') <- 'Concentration'
attr(obs$static, 'label') <- 'Static'
attr(obs$group, 'label') <- 'Group'

attr(sum$class, 'label') <- 'The Class'
attr(sum$mean, 'label') <- 'The Concentration'
attr(sum$static, 'label') <- 'The Static'
attr(sum$group, 'label') <- 'The Group'

# def layer provides time, conc; sum layer provides new y
# separate legends for separate columns group (Group) and static (The Static).
obs %>%
  ggplot(aes(time, concentration, color = group, shape = group)) +
  geom_point() +
  geom_hline(
    data = sum, 
    aes(
      yintercept = mean, 
      color = group, 
      linetype = static
    )
  ) 

# separate legends for group (Group) and group (The Group).
obs %>%
  ggplot(aes(time, concentration, color = group, shape = group)) +
  geom_point() +
  geom_hline(
    data = sum, 
    aes(
      yintercept = mean, 
      color = group, 
      linetype = group
    )
  ) 

# no default data, same result as no layer 1 data
ggplot(mapping = aes(time, concentration, color = group, shape = group)) +
  geom_point(data = obs) +
  geom_hline(
    data = sum, 
    aes(
      yintercept = mean, 
      color = group, 
      linetype = group
    )
  ) 

# no default data, three legends; 
# shape: group (Group), 
# color: group (Group), static (The Static)
# linetype: group (The Group)
ggplot(mapping = aes(time, concentration, color = group, shape = group)) +
  geom_point(data = obs) +
  geom_hline(
    data = sum, 
    aes(
      yintercept = mean, 
      color = static, 
      linetype = group
    )
  ) 

# no default data, two legends
# color, shape: group (Group)
# linetype: group (The Group)
obs %>%
  ggplot(mapping = aes(time, concentration, color = group, shape = group)) +
  geom_point() +
  geom_hline(
    data = sum, 
    aes(
      yintercept = mean, 
      color = group, 
      linetype = group
    )
  ) 

# no default data, one legend
# color, group, linetype: group (Group)
obs %>%
  ggplot(mapping = aes(time, concentration, color = group, shape = group, linetype = group)) +
  geom_point() +
  geom_hline(
    data = sum, 
    aes(
      yintercept = mean, 
      color = group,
      linetype = group
    )
  ) 




