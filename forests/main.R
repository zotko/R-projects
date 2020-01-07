library(readr)
library(dplyr)
library(ggplot2)

install.packages('ggplot2')

# read file
forestfires = read_csv('forestfires.csv')

fires_perday <- forestfires %>%
  group_by(day) %>%
  summarise(total_fires = n())

ggplot(data = fires_perday) +
  aes(x = day, y = total_fires) +
  geom_bar(stat = 'identity')

fires_permonth <- forestfires %>%
  group_by(month) %>%
  summarise(total_fires = n())

ggplot(data = fires_permonth) +
  aes(x = month, y = total_fires) +
  geom_bar(stat = 'identity')

