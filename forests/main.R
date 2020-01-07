library(readr)
library(dplyr)
library(ggplot2)

setwd("~/Code/r_projects/forests")
forest_fires = read_csv('forestfires.csv')

forest_fires <- forest_fires %>% 
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun",
                                          "jul", "aug", "sep", "oct", "nov", "dec")))
forest_fires <- forest_fires %>% 
  mutate(day = factor(day, levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun")))

fires_perday <- forest_fires %>%
  group_by(day) %>%
  summarise(total_fires = n())


ggplot(data = fires_perday) +
  aes(x = day, y = total_fires) +
  geom_bar(stat = 'identity')

fires_permonth <- forest_fires %>%
  group_by(month) %>%
  summarise(total_fires = n())

ggplot(data = fires_permonth) +
  aes(x = month, y = total_fires) +
  geom_bar(stat = 'identity')

