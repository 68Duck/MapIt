library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggnewscale)
library(ggstar)

add_stars <- function(df, attribute, width, height, star_size,
 label_x = "label_x", label_y = "label_y") {
#' Add Star Ratings to a ggplot object
#'
#' Generates a geom object which can be added to a ggplot object containing
#' a layer of stars which are found using an attribute from the dataframe
#'
#' @param df A data frame containing the data to be processed.
#' @param attribute A string indicating the name of the column in `df`
#'   that contains the numeric values to be used for star ratings.
#' @param width The width of the stars object.
#' @param height The height of the stars object.
#' @param star_size The size of each individual star
#' @param label_x The name of the column in `df` that contains the x-coordinate
#' for positioning the star ratings. Default is "label_x".
#' @param label_y The name of the column in `df` that contains the y-coordinate
#' for positioning the star ratings. Default is "label_y".
#' 
#' @return The star layer as a list of geom objects
#' 
#' @examples
#' df <- data.frame(id = 1:5, rating = c(5, 4, 3, 2, 1))
#' map <- map + add_stars(df, attribute = "rating",
#'  width = 100, height = 50, star_size = 10)
#' 
  map_elements <- list()
  df <- df[!is.na(df[[attribute]]), ]
  max_value <- max(df[[attribute]], na.rm = TRUE)
  no_stars <- 5
  for (i in 1:nrow(df)) {
    value <- round(df[[attribute]][i] / max_value * no_stars, digits = 0)
    data <- data.frame(
        x = c(1, 2, 3, 4, 5),
        y = c(1, 1, 1, 1, 1),
        colours = c(rep("yellow", each = value),
         rep("grey", each = (no_stars - value)))
    )
    map_elements[[i]] <- build_star_layer(df = df[i, , drop = FALSE],
     data = data, width = width, height = height, star_size = star_size)
  }
  map_elements
}


build_star_layer <- function(df, data, width, height,
 star_size, label_x, label_y) {
#' Builds an individual star layer
#'
#' @param df A data frame containing the coordinates for positioning the
#'  star layer
#' @param data A data frame with columns `x`, `y`, and `colours` which represent
#' the positions and color of the stars (yellow for filled, grey for empty).
#' @param width The width of the star layer
#' @param height The height of the star layer
#' @param star_size The size of each individual star
#' @param label_x The name of the column in `df` that contains the x-coordinate
#' for positioning the star ratings.
#' @param label_y The name of the column in `df` that contains the y-coordinate
#' for positioning the star ratings.
#' 
#' @return The star layer as a geom object
#' 
#' @examples
#' df <- data.frame(label_x = 5, label_y = 5)
#' data <- data.frame(x = 1:5, y = rep(1, 5),
#'  colours = c("yellow", "yellow", "yellow", "grey", "grey"))
#' build_star_layer(df = df, data = data,
#'  width = 100, height = 50, star_size = 10, label_x = "label_x",
#'  label_y = "label_y")
#' 
  points <- ggplot(data, aes(x = x, y = y, color = colours, fill = colours)) +
    geom_star(stat = "identity", size=star_size) +
    scale_fill_manual(values = c("yellow" = "yellow", "grey" = "grey")) +
    scale_color_manual(values = c("yellow" = "yellow", "grey" = "grey")) +
    theme_minimal() +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())

  point_grob <- ggplotGrob(points)

  annotation_custom(grob = point_grob, xmin = df$label_x - width / 2,
   xmax = df$label_x + width / 2, ymin = df$label_y - height / 2,
   ymax = df$label_y + height / 2)
}
