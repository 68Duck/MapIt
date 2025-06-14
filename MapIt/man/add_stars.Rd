% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_stars.R
\name{add_stars}
\alias{add_stars}
\title{Add Star Ratings to a ggplot object}
\usage{
add_stars(
  df,
  attribute,
  width,
  height,
  star_size,
  legend_title,
  label_x = "label_x",
  label_y = "label_y"
)
}
\arguments{
\item{df}{A data frame containing the data to be processed.}

\item{attribute}{A string indicating the name of the column in \code{df}
that contains the numeric values to be used for star ratings.}

\item{width}{The width of the stars object.}

\item{height}{The height of the stars object.}

\item{star_size}{The size of each individual star}

\item{label_x}{The name of the column in \code{df} that contains the x-coordinate
for positioning the star ratings. Default is "label_x".}

\item{label_y}{The name of the column in \code{df} that contains the y-coordinate
for positioning the star ratings. Default is "label_y".}
}
\value{
The star layer as a list of geom objects
}
\description{
Generates a geom object which can be added to a ggplot object containing
a layer of stars which are found using an attribute from the dataframe
}
\examples{
df <- data.frame(id = 1:5, rating = c(5, 4, 3, 2, 1))
map <- map + add_stars(df, attribute = "rating",
 width = 100, height = 50, star_size = 10)

}
