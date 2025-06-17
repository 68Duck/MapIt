library(here)
library(rnaturalearth)
library(rvest)

source(here("R/merge_data.R"))
source(here("R/choropleth.R"))
source(here("R/add_pie_charts.R"))

url <-
  "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_sector_composition"
webpage <- read_html(url)
table_nodes <- html_nodes(webpage, "table.wikitable")
gdp_data <- html_table(table_nodes[[2]], fill = TRUE)

country_data <- ne_countries(scale = 10, type = "countries",
                             continent = "south america",
                             returnclass = "sf")


data <- merge_data_with_ui(country_data, gdp_data,
                   "name", "Country/Economy", "test.csv", 0)


data$`Total GDP (US$MM)` <- as.numeric(gsub(",", "", data$`Total GDP (US$MM)`))

map <- choropleth(data, `Total GDP (US$MM)`)

for (i in 1:nrow(data)) {
  cat(
    data$region_number[i], " | ",
    data$name[i], " | ",
    data[["Country/Economy"]][i], 
    data$`Total GDP (US$MM)`[i],
    "\n"
  )
}
print(map)
