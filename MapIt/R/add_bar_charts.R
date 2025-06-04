library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggnewscale)
library(cowplot)

#' Add a Bar Chart Layer to a ggplot layer
#'
#'
#' @param df A data frame containing the data to be plotted.
#' @param width The width of the bar charts
#' @param height The height of the bar charts
#' @param attributes A list of column names in `df` that will be used to
#'   generate the bars.
#' @param label_x The name of the column in `df` that contains the
#'   x-coordinate for positioning the bar charts. Default is `"label_x"`.
#' @param label_y The name of the column in `df` that contains the
#'   y-coordinate for positioning the bar charts. Default is `"label_y"`.
#' 
#' @return A layer of barcharts which can be added to a ggplot object
#' 
#' @examples
#' df <- data.frame(id = 1:5, value1 = c(5, 4, 3, NA, 2),
#'  value2 = c(2, 3, 4, 5, 6), label_x = 1:5, label_y = rep(1, 5))
#' map <- map + add_bar_charts(df, width = 100,
#'  height = 50, attributes = c("value1", "value2"))
#'
#' @import ggplot2
#' @import rnaturalearth
#' @import sf
#' @import ggnewscale
#' 
#' @export
add_bar_charts <- function(df, width, height, attributes,
                           label_x = "label_x", label_y = "label_y") {

  map_elements <- list()

  df_no_na <- df[apply(df[attributes], 1, function(row) all(!is.na(row))), ]
  for (i in 1:nrow(df_no_na)) {
    map_elements[[i]] <- build_layer(df = df_no_na[i, , drop = FALSE],
      width = width, height = height, attributes = attributes,
      label_x = label_x, label_y = label_y)
  }

  dummy_data <- data.frame(
    Category = attributes,
    Value = sapply(attributes, function(attr) df[[attr]][1])
  )

  dummy_legend <- geom_bar(stat = "identity", data = dummy_data, mapping = aes(
    x = Category, y = Value, fill = Category)) 
  list(
  map_elements,
  new_scale("fill"),
  dummy_legend,
  scale_fill_brewer(palette = "Set2"),
  coord_sf(lims_method = "geometry_bbox")
  )
}

create_legend <- function(df, attributes) {
  data <- data.frame(
    Category = attributes,
    Value = sapply(attributes, function(attr) df[[attr]][1])
  )

  plot <- ggplot(data, aes(x = Category, y = Value, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Set2")

  plot
}

#' Builds a bar chart
#'
#' @param df A data frame containing the coordinates for positioning the
#'  bar chart.
#' @param data A data frame with the bar chart information.
#' @param width The width of the bar chart.
#' @param height The height of the bar chart.
#' @param label_x The name of the column in `df` that contains the x-coordinate
#' for positioning the bar chart.
#' @param label_y The name of the column in `df` that contains the y-coordinate
#' for positioning the bar chart.
#' 
#' @return The bar chart as a geom object
#' 
#' @examples
#' # Sample data frame
#' df <- data.frame(label_x = 5, label_y = 5)
#' attributes <- c("value1", "value2")
#' df_data <- data.frame(value1 = 10, value2 = 20)
#' bar_layer <- build_layer(df = df_data,
#'                          width = 100,
#'                          height = 50,
#'                          attributes = attributes,
#'                          label_x = "label_x",
#'                          label_y = "label_y")
#'
#'
build_layer <- function(df, width, height, attributes, label_x, label_y) {
  data <- data.frame(
    Category = attributes,
    Value = sapply(attributes, function(attr) df[[attr]])
  )

  bar_chart <- ggplot(data, aes(x = Category, y = Value, fill = Category)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(axis.title = element_blank(),
     axis.text = element_blank(), axis.ticks = element_blank(),
     legend.position = "none")

  bar_grob <- ggplotGrob(bar_chart)
  annotation_custom(grob = bar_grob, xmin = df$label_x - width / 2,
   xmax = df$label_x + width / 2, ymin = df$label_y - height / 2,
   ymax = df$label_y + height / 2)
}
