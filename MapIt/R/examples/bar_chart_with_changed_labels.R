library(here)
library(rnaturalearth)
source(here("R/add_lines.R"))
source(here("R/add_bar_charts.R"))
source(here("R/choropleth.R"))

data <- ne_countries(returnclass = "sf", scale = 10, continent = "africa")

width <- 6
height <- 6
small_country_area <- 15

data <- modify_label_positions(data = data,
                               small_country_area = small_country_area,
                               width = width, height = height)
map <- choropleth(data = data, fill = data$pop_rank,
                  legend_title = "Population rank") +
  add_lines_to_labels(data = data, width = width, height = height) +
  add_bar_charts(df = data, width = width, height = height,
                 attributes = c("pop_rank", "name_len"))

print(map)