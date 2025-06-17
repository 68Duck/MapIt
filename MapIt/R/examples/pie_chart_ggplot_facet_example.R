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


data <- merge_data_with_edit_distance(country_data, gdp_data,
                   "name", "Country/Economy", 0)

data <- convert_columns_to_number(data, c("Agricultural (%)", "Industrial (%)",
                                          "Service (%)"), c("%"))

print(colnames(data))

# for (i in 1:nrow(data)) {
#   cat(
#     data$region_number[i], " | ",
#     data$name[i], " | ",
#     data[["Agricultural (%)"]][i], " | ",
#     data[["Industrial (%)"]][i], " | ",
#     data[["Service (%)"]][i], "\n"
#   )
# }
data <- data %>%
  pivot_longer(
    cols = c("Agricultural (%)", "Industrial (%)", "Service (%)"),
    names_to = "sector",
    values_to = "value"
  )



map <- ggplot(data, aes(x = "", y = value, fill = sector)) +
       geom_bar(stat = "identity", width = 1) +
       coord_polar(theta = "y") +
       facet_wrap(~ name) +
       theme_void()


print(map)
ggsave("plot.png", plot = map, width = 10, height = 7, units = "in", dpi = 300)