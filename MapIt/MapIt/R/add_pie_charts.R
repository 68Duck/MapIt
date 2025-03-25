library(ggplot2)
library(scatterpie)
library(ggnewscale)


#' Add Pie Charts to a Plot
#'
#' Generates a pie chart layer for a ggplot object
#'
#' @param data A data frame containing the data to be plotted.
#' @param x A  string indicating the name of the column in `data` that contains
#'   the x-coordinate for positioning the pie charts.
#' @param y A string indicating the name of the column in `data` that contains
#'   the y-coordinate for positioning the pie charts.
#' @param attributes A list of column names in `data` that will be used to
#'   fill the pie charts.
#' @param legend_title A string to be used as the title of the legend.
#' @param pie_scale The size of the pie charts
#' 
#' @return A `ggplot` object containing the pie layer
#' 
#' @examples
#' data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6),
#'  A = c(30, 50, 20), B = c(70, 50, 80))
#' map <- map + add_pie_charts(data, x = "x", y = "y",
#'  attributes = c("A", "B"), legend_title = "Category", pie_scale = 0.1)
#' 
#' @import ggplot2
#' @import scatterpie
#' @import ggnewscale
add_pie_charts <- function(data, x, y, attributes, legend_title, pie_scale) {
  list(
    new_scale("fill"),
    geom_scatterpie(aes_string(x = x, y = y),
                     data = as.data.frame(data),
                     cols = attributes, 
                     pie_scale = pie_scale),
    coord_sf(),
    labs("fill" = legend_title))
}
