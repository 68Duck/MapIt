library(here)
library(rnaturalearth)
library(rvest)
source(here("R/add_lines.R"))
source(here("R/add_pie_charts.R"))
source(here("R/choropleth.R"))

url <-
  "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_sector_composition"
webpage <- read_html(url)
table_nodes <- html_nodes(webpage, "table.wikitable")
gdp_data <- html_table(table_nodes[[2]], fill = TRUE)

country_data <- ne_countries(scale = 10, type = "countries",
                             continent = "south america",
                             returnclass = "sf")

width <- 10
height <- 10
pie_scale <- 3

data <- merge_data_with_ui(country_data, gdp_data,
                           "name", "Country/Economy")

data <- convert_columns_to_number(data, c("Agricultural (%)", "Industrial (%)",
                                          "Service (%)"), c("%"))

map <- choropleth(data, pop_est, "Population") +
       add_pie_charts(data, "label_x", "label_y", 
         c("Agricultural (%)", "Industrial (%)", "Service (%)"),
         pie_scale = pie_scale, legend_title = "Types of industry")


print(map)
ggsave("plot.png", plot = map, width = 10, height = 7, units = "in", dpi = 300)