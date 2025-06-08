library(here)
library(rnaturalearth)
library(rvest)
source(here("R/add_lines.R"))
source(here("R/add_bar_charts.R"))
source(here("R/choropleth.R"))
source(here("R/merge_data.R"))

# data <- ne_countries(returnclass = "sf", scale = 10, continent = "africa")

width <- 6
height <- 6
small_country_area <- 15

url <-
  "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_sector_composition"
webpage <- read_html(url)
table_nodes <- html_nodes(webpage, "table.wikitable")
gdp_data <- html_table(table_nodes[[2]], fill = TRUE)

country_data <- ne_countries(scale = 10, type = "countries",
                             continent = "africa",
                             returnclass = "sf")


data <- merge_data_with_ui(country_data, gdp_data,
                           "name", "Country/Economy")
data <- convert_columns_to_number(data, c("Agricultural (%)", "Industrial (%)",
                                          "Service (%)"), c("%"))



data <- modify_label_positions(data = data,
                               small_country_area = small_country_area,
                               width = width, height = height)

map <- choropleth(data = data, fill = pop_est,
                  legend_title = "Population") +
  add_lines_to_labels(data = data, width = width, height = height) +
  add_bar_charts(data, width, height,
                 c("Agricultural (%)", "Industrial (%)", "Service (%)"),
                 "GDP by sector")


print(map)
ggsave("plot.png", plot = map, width = 10, height = 7, units = "in", dpi = 300)