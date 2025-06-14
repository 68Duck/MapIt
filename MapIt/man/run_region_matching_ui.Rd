% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_data.R
\name{run_region_matching_ui}
\alias{run_region_matching_ui}
\title{Iterates through each region and checks whether it has been allocated
a number automatically. If not, it prompts the user to input a name or
whether the region should be removed.}
\usage{
run_region_matching_ui(
  region_data,
  region_name,
  csv_path,
  distance = 3,
  method = "lv"
)
}
\arguments{
\item{region_data}{A data frame containing the region data.}

\item{region_name}{A string specifying the column name in \code{region_data}
that contains the region names.
#' @param csv_path A string specifying the path of the csv which has
alternative names for regins in its rows}
}
\value{
An \code{sf} data frame containing new region numbers
}
\description{
Iterates through each region and checks whether it has been allocated
a number automatically. If not, it prompts the user to input a name or
whether the region should be removed.
}
\examples{
country_data <- run_region_matching_ui(country_data, "country")
}
