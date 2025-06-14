% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choropleth.R
\name{choropleth}
\alias{choropleth}
\title{Create a Choropleth Map}
\usage{
choropleth(data, fill, legend_title = NULL)
}
\arguments{
\item{data}{A \code{sf} (simple features) object containing the
data to be plotted.}

\item{fill}{The variable in \code{data} to be used for filling the regions.}

\item{legend_title}{The title of the legend.}
}
\value{
A \code{ggplot} object representing the choropleth map.
}
\description{
Generates a choropleth map using ggplot2
}
\examples{
choropleth(sf_data, fill = "population_density",
 legend_title = "Population Density")

}
