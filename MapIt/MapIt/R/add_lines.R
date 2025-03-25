library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(dplyr)
library(ggplot2)


find_closest_rects <- function(data, small_country_area, width, height) {
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
  rectangles_intersects_world <- st_intersects(rectangles_sf,
                                               world_polygon, sparse = FALSE)
  valid_rectangles_sf <- rectangles_sf[!apply(rectangles_intersects_world,
                                              1, any)]
  rectangles_centroids <- st_centroid(valid_rectangles_sf)

  euclidean_distance <- function(point1, point2) {
    lon_diff <- point1[1] - point2[1]
    lat_diff <- point1[2] - point2[2]
    sqrt(lon_diff^2 + lat_diff^2)
  }

  small_countries <- country_bboxes[country_bboxes$area < small_country_area, ]

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
    rectangles_centroids <- st_set_crs(rectangles_centroids, st_crs(data))
    selected_country_centroid <- st_set_crs(
      selected_country_centroid, st_crs(data)
    )
    distances_df <- data.frame()
    country_point <- st_coordinates(selected_country_centroid)

    for (i in 1:length(rectangles_centroids)) {
      rect_point <- st_coordinates(rectangles_centroids[i])
      distance <- euclidean_distance(rect_point, country_point)
      distances_df <- rbind(
        distances_df, data.frame(rectangle_id = i, distance = distance)
      )
    }

    closest_rectangle_index <- which.min(distances_df$distance)
    closest_rectangle <- valid_rectangles_sf[closest_rectangle_index]
    closest_rectangle_centroid <- st_centroid(closest_rectangle)
    closest_rectangle_point <- st_coordinates(closest_rectangle_centroid)

    closest_rectangles_df <- rbind(closest_rectangles_df, data.frame(
      country_name = selected_country$name,
      country_point = country_point,
      closest_rectangle_sf = closest_rectangle,
      rectangle_point = closest_rectangle_point
    ))

    valid_rectangles_sf <- valid_rectangles_sf[-closest_rectangle_index]
    rectangles_centroids <- rectangles_centroids[-closest_rectangle_index]
  }

  closest_rectangles_df
} 

modify_label_positions <- function(data, small_country_area, width, height,
                                   label_x = "label_x", label_y = "label_y") {
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

  df <- find_closest_rects(data = data, small_country_area = small_country_area,
                           width = width, height = height)

  new_data <- data %>%
    left_join(df, by = c("name" = "country_name")) %>%
    mutate(
      label_x = ifelse(!is.na(df$rectangle_point.X),
                       df$rectangle_point.X, label_x),
      label_y = ifelse(!is.na(df$rectangle_point.Y),
                       df$rectangle_point.Y, label_y)
    )
  new_data
}


add_lines_to_labels <- function(data, width, height) {
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
#' plot_with_lines <- ggplot() +
#'   geom_sf(data = world_data) +
#'   add_lines_to_labels(small_country_area = 100000,
#'                       width = 1, height = 1, data = world_data)
#'
#' @import ggplot2
#' @import sf

  geom_segment(data = data, aes(x = data$country_point.X,
                                y = data$country_point.Y,
                                xend = data$rectangle_point.X,
                                yend = data$rectangle_point.Y),
               color = "blue", size = 1)
}