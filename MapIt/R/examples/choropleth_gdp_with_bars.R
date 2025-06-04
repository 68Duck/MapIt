library(here)
library(rnaturalearth)
library(rvest)

source(here("R/merge_data.R"))
source(here("R/choropleth.R"))
source(here("R/add_bar_charts.R"))
source(here("R/add_points.R"))

url <-
  "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_sector_composition"
webpage <- read_html(url)
table_nodes <- html_nodes(webpage, "table.wikitable")
gdp_data <- html_table(table_nodes[[2]], fill = TRUE)

country_data <- ne_countries(scale = 10, type = "countries",
                             continent = "south america",
                             returnclass = "sf")

width <- 5
height <- 5

data <- merge_data_with_ui(country_data, gdp_data,
                                         "name", "Country/Economy")
data <- convert_columns_to_number(data, c("Agricultural (%)", "Industrial (%)",
                                          "Service (%)"), c("%"))


map <- choropleth(data, pop_est, "Population") +
  add_bar_charts(data, width, height,
                 c("Agricultural (%)", "Industrial (%)", "Service (%)"))



print(map)
ggsave("plot.png", plot = map, width = 10, height = 7, units = "in", dpi = 300)
