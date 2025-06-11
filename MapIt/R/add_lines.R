library(rnaturalearth)
library(sf)
library(dplyr)
library(ggplot2)
library(clue)

#' Calculates the Euclidean distance between two points.
#'
#' This function computes the Euclidean distance between two points.
#' The points are represented as coordinate pairs (longitude, latitude).
#'
#' @param point1 A vector representing the first point (longitude, latitude).
#' @param point2 A vector representing the second point (longitude, latitude).
#'
#' @return The Euclidean distance between the two points
#' @examples
#' point1 <- c(-75, 40)
#' point2 <- c(-80, 45)
#' distance <- euclidean_distance(point1, point2)
#' print(distance)
euclidean_distance <- function(point1, point2) {
  lon_diff <- point1[1] - point2[1]
  lat_diff <- point1[2] - point2[2]
  sqrt(lon_diff^2 + lat_diff^2)
}

#' Finds the closest rectangle in free space for a small countries
#' information to be sotred in.
#'
#' @param data The map data which should include simple features information.
#' @param small_country_area A value representing the threshold area below which
#'                   countries are considered small.
#' @param width The width of the free space the small countries
#'              should be assigned
#' @param height The height of the free space the small countries
#'              should be assigned
#'
#' @return A data frame containing the closest rectangle for each small country.
#'
#' @examples
#' # Assuming `world_data` is a valid `sf` object containing world map data:
#' closest_rects <- find_closest_rects(world_data,
#'  small_country_area = 100000, width = 1, height = 1)
#'
#' @import ggplot2
#' @import sf
#' @import dplyr
#' @import purrr
#' @import clue
find_closest_rects <- function(data, small_country_area, width, height) {
  country_bboxes <- data %>%
    mutate(
      bbox = purrr::map(geometry, st_bbox),
      xmin = purrr::map_dbl(bbox, 1),
      ymin = purrr::map_dbl(bbox, 2),
      xmax = purrr::map_dbl(bbox, 3),
      ymax = purrr::map_dbl(bbox, 4)
    ) %>%
    select(name, xmin, ymin, xmax, ymax)

  country_bboxes$area <- (country_bboxes$xmax - country_bboxes$xmin) *
    (country_bboxes$ymax - country_bboxes$ymin)

  bounding_box <- st_bbox(data)
  bbox_polygon <- st_as_sfc(bounding_box)
  world_valid <- st_make_valid(data)
  world_polygon <- st_union(world_valid)
  non_intersecting_geometry <- st_difference(bbox_polygon, world_polygon)
  non_intersecting_bbox <- st_bbox(non_intersecting_geometry)

  xmin_values <- seq(non_intersecting_bbox["xmin"],
                     non_intersecting_bbox["xmax"] - width, by = width)
  ymin_values <- seq(non_intersecting_bbox["ymin"],
                     non_intersecting_bbox["ymax"] - height, by = height)

  rectangles <- expand.grid(xmin = xmin_values, ymin = ymin_values)
  rectangles <- rectangles %>%
    mutate(xmax = xmin + width, ymax = ymin + height)

  rectangles_sf <- st_sfc(lapply(1:nrow(rectangles), function(i) {
    st_polygon(list(matrix(c(
      rectangles$xmin[i], rectangles$ymin[i],
      rectangles$xmin[i] + width, rectangles$ymin[i],
      rectangles$xmin[i] + width, rectangles$ymin[i] + height,
      rectangles$xmin[i], rectangles$ymin[i] + height,
      rectangles$xmin[i], rectangles$ymin[i]
    ), ncol = 2, byrow = TRUE)))
  }))

  rectangles_sf <- st_set_crs(rectangles_sf, st_crs(data))

  rectangles_intersects_world <- st_intersects(rectangles_sf, world_polygon,
                                               sparse = FALSE)
  valid_rectangles_sf <- rectangles_sf[!apply(rectangles_intersects_world,
                                              1, any)]
  rectangles_centroids <- st_centroid(valid_rectangles_sf)
  rectangles_centroids <- st_set_crs(rectangles_centroids, st_crs(data))

  small_countries <- country_bboxes[country_bboxes$area < small_country_area, ]

  distances_matrix <- matrix(0, nrow(small_countries),
                             length(rectangles_centroids))

  for (i in 1:nrow(small_countries)) {
    selected_country <- small_countries[i, ]
    selected_country_centroid <- st_centroid(selected_country)
    selected_country_centroid <- st_set_crs(selected_country_centroid,
                                            st_crs(data))
    country_point <- st_coordinates(selected_country_centroid)

    for (j in 1:length(rectangles_centroids)) {
      rect_point <- st_coordinates(rectangles_centroids[j])
      distance <- euclidean_distance(rect_point, country_point)
      distances_matrix[i, j] <- distance
    }
  }

  n_countries <- nrow(distances_matrix)
  n_rects <- ncol(distances_matrix)

  if (n_countries > n_rects) {
    return(NULL)
  }

  optimal_assignment <- solve_LSAP(distances_matrix)

  closest_rectangles_df <- data.frame(
    country_name = character(),
    closest_rectangle_sf = I(list()),
    country_point = I(list()),
    rectangle_point = I(list()),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(small_countries)) {
    selected_country <- small_countries[i, ]
    selected_country_centroid <- st_centroid(selected_country)
    selected_country_centroid <- st_set_crs(selected_country_centroid,
                                            st_crs(data))
    country_point <- st_coordinates(selected_country_centroid)

    closest_rectangle_index <- optimal_assignment[i]
    closest_rectangle <- valid_rectangles_sf[closest_rectangle_index]
    closest_rectangle_centroid <- st_centroid(closest_rectangle)
    closest_rectangle_point <- st_coordinates(closest_rectangle_centroid)

    closest_rectangles_df <- rbind(closest_rectangles_df, data.frame(
      country_name = selected_country$name,
      country_point = country_point,
      closest_rectangle_sf = closest_rectangle,
      rectangle_point = closest_rectangle_point
    ))
  }
  closest_rectangles_df
}


#' Modifies Label Positions Based on the closest rectangles if the countries
#' area is too small
#'
#' @param data The map data which should include simple features information.
#' @param small_country_area A value representing the threshold area below which
#'                   countries are considered small.
#' @param width The width of the free space the small countries
#'              should be assigned
#' @param height The height of the free space the small countries
#'              should be assigned
#' @param label_x The column name for the x-coordinate of country labels.
#' @param label_y The column name for the y-coordinate of country labels.
#'
#' @return A data frame containing the original
#'         data along with updated label positions.
#'
#' @examples
#' # Assuming `world_data` is a valid `sf` object containing world map data:
#' modified_labels <- modify_label_positions(world_data,
#'                                           small_country_area = 100000,
#'                                           width = 1, height = 1)
#'
#' @import ggplot2
#' @import sf
#' @import dplyr
#' @export
modify_label_positions <- function(data, small_country_area, width, height,
                                   label_x = "label_x", label_y = "label_y") {

  df <- find_closest_rects(data = data, small_country_area = small_country_area,
                           width = width, height = height)

  new_data <- data %>%
    mutate(
      label_x = ifelse(!is.na(rectangle_point.X),
                       rectangle_point.X, label_x),
      label_y = ifelse(!is.na(rectangle_point.Y),
                       rectangle_point.Y, label_y)
    )
  new_data
}


#' Adds lines connecting small countries to their closest rectangles
#'
#'
#' @param width The width of the free space the small countries
#'              should be assigned
#' @param height The height of the free space the small countries
#'              should be assigned
#' @param data The map data which should include simple features information.
#'
#' @return A geom_segment which contains the lines between countries and
#'         the closest rectangles
#'
#' @examples
#' # Assuming `world_data` is a valid `sf` object containing world map data:
#' world_data <- modify_label_positions(world_data,
#'                                      small_country_area = 100000,
#'                                      width = 1, height = 1) +
#' plot_with_lines <- ggplot() +
#'   geom_sf(data = world_data) +
#'   add_lines_to_labels(data = world_data, width = 1, height = 1)
#'
#' @import ggplot2
#' @import sf
#' @export
add_lines_to_labels <- function(data, width, height) {
  if ("country_point" %in% colnames(data) &&
      "rectangle_point" %in% colnames(data)) {
    geom_segment(data = data, aes(x = country_point.X,
                                  y = country_point.Y,
                                  xend = rectangle_point.X,
                                  yend = rectangle_point.Y),
                color = "blue", size = 1)
  } else {
    stop(paste("country_point and rectangle_point must be colums in the data.",
               "Please call modify_label_positions()",
               "first to ensure they exist."))
  }
}