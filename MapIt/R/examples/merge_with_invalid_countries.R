library(here)
library(rnaturalearth)
library(rvest)

source(here("MapIt/MapIt/R/merge_data.R"))

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
