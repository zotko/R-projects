# Load the packages
library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

# Import the data file
forest_fires = read_csv('forestfires.csv')

# Change the data type of month to factor
forest_fires <- forest_fires %>%
  mutate(month = factor(
    month,
    levels = c(
      "jan",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec"
    )
  ))

# Change the data type of daty to factor
forest_fires <- forest_fires %>%
  mutate(day = factor(day, levels = c(
    "mon", "tue", "wed", "thu", "fri", "sat", "sun"
  )))

# Create bar chart showing the number of forest fires occuring on each day of the week
fires_perday <- forest_fires %>%
  group_by(day) %>%
  summarise(total_fires = n())


ggplot(data = fires_perday) +
  aes(x = day, y = total_fires) +
  geom_bar(stat = 'identity') +
  theme(panel.background = element_rect(fill = 'white'))

# Create bar chart showing the number of forest fires occuring in each month
fires_permonth <- forest_fires %>%
  group_by(month) %>%
  summarise(total_fires = n())

ggplot(data = fires_permonth) +
  aes(x = month, y = total_fires) +
  geom_bar(stat = 'identity') +
  theme(panel.background = element_rect(fill = 'white'))


create_boxplot <- function(x, y) {
  ggplot(data = forest_fires) +
    aes_string(x = x, y = y) +
    geom_boxplot() +
    theme(panel.background = element_rect(fill = 'white'))
}

vals <- c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain")

# Create boxplots
map2('month', vals, create_boxplot)
map2('day', vals, create_boxplot)

ggplot(data = forest_fires) +
  aes(x = area) +
  geom_histogram()

# Filter out data where area is equal to zero or greater than 300
filter_area <- forest_fires %>%
  filter(area > 0 & area < 300)

# Create boxplots with filtered data
create_scatter <- function(x, y) {
  ggplot(data = filter_area) +
    aes_string(x = x, y = y) +
    geom_point() +
    theme(panel.background = element_rect(fill = 'white'))
}

map2(vals, 'area', create_scatter)
