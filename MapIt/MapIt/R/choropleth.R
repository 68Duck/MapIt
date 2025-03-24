library(ggplot2)
library(rnaturalearth)
library(scales)

choropleth <- function(data, fill, legend_title) {
#' Create a Choropleth Map
#' 
#' Generates a choropleth map using ggplot2
#' 
#' @param data A `sf` (simple features) object containing the data to be plotted.
#' @param fill The variable in `data` to be used for filling the regions.
#' @param legend_title The title of the legend.
#' 
#' @return A `ggplot` object representing the choropleth map.
#' 
#' @examples
#' choropleth(sf_data, fill = "population_density", legend_title = "Population Density")
#' 
#' @import ggplot2
#' @import sf
graph <- ggplot() + geom_sf(data=data, aes(fill = fill)) +
    theme_void() + 
    scale_color_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) 
graph$labels$fill <- legend_title
return(graph)
}

test <- ne_countries(scale=10, type="countries", continent = "africa")
map <- choropleth(test, test$pop_rank, "Population rank")
source("C:\\Users\\Joshu\\Documents\\MapIt\\MapIt\\MapIt\\R\\add_stars.R")
# print(map)
map <- map + add_stars(test, "pop_rank", 10, 10, 3)

print(map)