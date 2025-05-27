library(rnaturalearth)
library(rvest)
library(here)
source(here("R/add_lines.R"))
source(here("R/add_bar_charts.R"))
source(here("R/choropleth.R"))
source(here("R/add_pie_charts.R"))
source(here("R/merge_data.R"))


url <-
  "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_sector_composition"
webpage <- read_html(url)
table_nodes <- html_nodes(webpage, "table.wikitable")
gdp_data <- html_table(table_nodes[[2]], fill = TRUE)

country_data <- ne_countries(scale = 10, type = "countries",
                             continent = "africa",
                             returnclass = "sf")

data <- merge_data_with_country_matching(country_data, gdp_data,
                                         "name", "Country/Economy")
data <- convert_columns_to_number(data, c("Agricultural (%)", "Industrial (%)",
                                          "Service (%)"), c("%"))

width <- 6
height <- 6
small_country_area <- 15
pie_scale <- 1.2


data <- modify_label_positions(data, small_country_area, width, height)
map <- choropleth(data, data$pop_rank, "Population rank") +
  add_lines_to_labels(data = data, width = width, height = height) +
  add_pie_charts(data, "label_x", "label_y",
                 c("Agricultural (%)", "Industrial (%)", "Service (%)"),
                 "Gdp by sector", pie_scale)
print(map)
