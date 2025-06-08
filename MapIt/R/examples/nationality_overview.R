library(here)
library(rnaturalearth)
source(here("R/add_lines.R"))
source(here("R/add_bar_charts.R"))
source(here("R/choropleth.R"))
source(here("R/add_pie_charts.R"))
source(here("R/add_stars.R"))
source(here("R/add_points.R"))
source(here("R/merge_data.R"))


csv_data <-
  read.csv(here("R/examples/2025_03_14_nationality_overview.csv"))

country_data <- ne_countries(scale = 10, type = "countries")

data <- merge_data_with_country_matching(country_data,
                                         csv_data, "name", "nationality")

map <- choropleth(data, no, "Number of people") +
  add_points(data, "label_x", "label_y", "avg_tmua", 10, "Average tmua score")



ggsave("plot_output.svg", plot = map, device = "svg")

print(map)