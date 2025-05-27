library(here)
library(rnaturalearth)
source(here("MapIt/R/add_lines.R"))
source(here("MapIt/R/add_pie_charts.R"))
source(here("MapIt/R/choropleth.R"))

data <- ne_countries(returnclass = "sf", scale = 10,
                     continent = "south america")

width <- 6
height <- 6
small_country_area <- 15
pie_scale <- 1.5

data <- modify_label_positions(data, small_country_area, width, height)
map <- choropleth(data, data$pop_rank, "Population rank") +
  add_lines_to_labels(data, width, height) +
  add_pie_charts(data, "label_x", "label_y", c("name_len", "pop_rank"),
                 "Name vs Pop", pie_scale)


print(map)