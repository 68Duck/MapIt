library(ggplot2)
library(rnaturalearth)
library(scales)

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
#' choropleth(sf_data, fill = "population_density",
#'  legend_title = "Population Density")
#' 
#' @import ggplot2
#' @import sf
#' @export
choropleth <- function(data, fill, legend_title) {
    graph <- ggplot() + geom_sf(data=data, aes(fill = fill)) +
        theme_void() +
        scale_color_gradient2(low = "red", high = "blue",
        mid = "white", midpoint = 0) 
    graph$labels$fill <- legend_title
    graph
}