% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_matching.R
\name{distance_lesser_than}
\alias{distance_lesser_than}
\title{Calculates the distance between two strings and checks
if it is less than a threshold.}
\usage{
distance_lesser_than(str1, str2, x, method = "lv")
}
\arguments{
\item{str1}{A string representing the first name.}

\item{str2}{A string representing the second name.}

\item{x}{A numeric threshold for the distance.}

\item{method}{A string representing the edit distance method}
}
\value{
TRUE if the distance is less than \code{x}; otherwise, FALSE.
}
\description{
Calculates the distance between two strings and checks
if it is less than a threshold.
}
\examples{
is_closer <- distance_lesser_than("germany", "germnay", 3)
}
