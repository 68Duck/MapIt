library(ggplot2)
library(dplyr)

#' Generates a point layer based off an attribute
#'
#' @param data A data frame containing the data to be plotted.
#' @param x The name of the column in `data` that contains the x-coordinates for
#'   the points.
#' @param y The name of the column in `data` that contains the y-coordinates for
#'   the points.
#' @param attribute The name of the column in `data` that contains the variable
#'  to determine the size and color of the points.
#' @param legend_title The title of the legend.
#' 
#' @return A layer of points that can be added to a ggplot object.
#' 
#' @examples
#' df <- data.frame(x = rnorm(100), y = rnorm(100), measure = rnorm(100))
#' plot <- ggplot() + add_points(df, x = "x", y = "y",
#'  attribute = "attribute", legend_title = "Attribute")
#'
#' @import ggplot2
#' @import dplyr
#' @export
add_points <- function(data, x, y, attribute, legend_title) {
  data <- data[!is.na(data[[attribute]]), ]
  data[[attribute]] <- as.numeric(data[[attribute]])
  list(
    new_scale("colour"),
    new_scale("size"),
    geom_point(aes_string(x = data[[x]], y = data[[y]],
                          size = data[[attribute]],
                          colour = data[[attribute]])),
    scale_size_continuous(range = c(1, 12)),
    scale_color_viridis_c(trans = "log"),
    labs("size" = legend_title),
    labs("colour" = legend_title)
  )
}
