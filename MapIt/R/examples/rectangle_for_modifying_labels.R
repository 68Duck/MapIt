library(here)
library(rnaturalearth)
library(dplyr)
library(purrr)
library(sf)
library(clue)
library(ggplot2)

euclidean_distance <- function(point1, point2) {
  lon_diff <- point1[1] - point2[1]
  lat_diff <- point1[2] - point2[2]
  sqrt(lon_diff^2 + lat_diff^2)
}

data <- ne_countries(returnclass = "sf", scale = 10, continent = "africa")
width <- 5
height <- 5
small_country_area <- 10

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

country_bboxes_polygons <- country_bboxes %>%
  rowwise() %>%
  mutate(
    geometry = list(st_polygon(list(matrix(c(
      xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin
    ), ncol = 2, byrow = TRUE))))
  ) %>%
  ungroup() %>%
  st_as_sf() %>%                     
  st_set_crs(st_crs(data))     

map <- ggplot() +
    geom_sf(data = data, fill = "white", color = "black") +  # Plot the world map
    # geom_sf(data = rectangles_sf, fill = NA, color = "red", size = 1) +  # Plot the rectangles
    # geom_sf(data = valid_rectangles_sf, fill = NA, color = "red", size = 1) +  # Plot the rectangles
    # geom_sf(data = country_bboxes_polygons[country_bboxes$area < small_country_area, ], fill = NA, color = "red", size = 1) +  # Plot the rectangles
    geom_sf(data = country_bboxes[country_bboxes$area < small_country_area, ], fill = "yellow", color = "red", size = 2) + 
    geom_sf(data = closest_rectangles_df$geometry, fill = "green", color = "black", size = 2) +  # Plot the closest rectangle
    geom_segment(data = closest_rectangles_df, aes(x = country_point.X, y = country_point.Y, xend = rectangle_point.X, yend = rectangle_point.Y), 
               color = "blue", size = 1) +
    coord_sf() +
    theme_void()
print(map)
