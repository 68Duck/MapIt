library(stringdist)
library(purrr)

csv_data <<- read.csv("countryNames.csv", header = FALSE)

#' Creates a mapping from country names to numbers
#'
#' @return A map of country names to numbers
#' @examples
#' country_map <- create_country_map()
create_country_map <- function() {
  data <- csv_data
  country_map <- list()
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      country_name <- tolower(data[i, j])
      country_map[[country_name]] <- i
    }
  }
  country_map
}


#' Returns the country number based off the levenstein distance being
#' less than x
#'
#' This function searches for the country in the CSV data by calculating the
#' Levenshtein distance between the input `country` and the country names in
#' the data. If the distance is less than a threshold `x`, it returns the
#' index of the matching row which is the country number.
#'
#' @param country A string representing the country name to search for.
#' @param x A numeric threshold for the Levenshtein distance.
#'
#' @return The row index of the closest matching country
#'         if found; otherwise, -1.
#' @examples
#' country_number <- get_country_number_with_levenshtein_distance("germany", 3)
get_country_number_with_levenshtein_distance <- function(country, x) {
  data <- csv_data
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    for (j in 1:length(row)) {
      value <- row[j]
      if (length(value) == 0) {
        break
      }
      if (levenshtein_distance_lesser_than(toString(value),
                                           tolower(country), x)) {
        print(i)
        return(i)
      }
    }
  }
  -1
}


#' Returns the country number based off the csv file
#'
#' This function first checks if a cached `country_map` exists and uses it for
#' fast lookup. If not, it will compute the mapping by calling 
#' `create_country_map`.
#' The function tries to match the input `country` with names in the map. 
#' It also attempts to clean the country name by removing common 
#' words like "and", "of", or "the".
#' If a direct match isn't found, it uses the Levenshtein distance to search for
#' a close match.
#'
#' @param country A string representing the country name to search for.
#' @param x A numeric threshold for the Levenshtein distance.
#'
#' @return The row index of the closest matching country 
#'          if found; otherwise, -1.
#' @examples
#' country_number <- get_country_number("germany", 3)
get_country_number <- function(country, x) {
  if (!exists("country_map", envir = .GlobalEnv)) {
    country_map <<- create_country_map()
  }
  country <- tolower(country)
  if (country %in% names(country_map)) {
    return (country_map[[country]])
  }
  country <- gsub("\\b(and|of|the)\\b", "", country, ignore.case = TRUE)
  if (country %in% names(country_map)) {
    return (country_map[[country]])
  }
  data <- csv_data
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    for (j in 1:length(row)) {
      value <- row[j]
      if (length(value) == 0) {
        break
      }
      value <- toString(value)
      value <- gsub("\\b(and|of|the)\\b", "", value, ignore.case = TRUE)
      if (levenshtein_distance_lesser_than(value,
                                           country, x)) {
        return(i)
      }
      if (abbreviated(country, value)) {
        return(i)
      }
    }
  }
  -1
}


#' Calculates the Levenshtein distance between two strings and checks
#'  if it is less than a threshold.
#'
#' @param str1 A string representing the first country name.
#' @param str2 A string representing the second country name.
#' @param x A numeric threshold for the Levenshtein distance.
#'
#' @return TRUE if the Levenshtein distance is less than `x`; otherwise, FALSE.
#' @examples
#' is_closer <- levenshtein_distance_lesser_than("germany", "germnay", 3)
levenshtein_distance_lesser_than <- function(str1, str2, x) {
  dist <- stringdist(str1, str2, method = "lv")
  dist < x
}

#' Checks if two country names are abbreviations of each other.
#'
#' @param str1 A string representing the first country name.
#' @param str2 A string representing the second country name.
#'
#' @return TRUE if the two country names are abbreviations of each other;
#'         otherwise, FALSE.
#' @examples
#' is_abbreviation <- abbreviated("us", "united states")
abbreviated <- function(str1, str2) {
  while (nchar(str1) > 0 && nchar(str2) > 0) {
    if (substring(str1, 0, 1) == ".") {
      str1 <- substring(str1, 3)
      str2 <- sub("^\\S+\\s*", "", str2)
    } else if (substring(str1, 0, 1) == substring(str2, 0, 1)) {
      str1 <- substring(str1, 2)
      str2 <- substring(str2, 2)
    } else {
      return(FALSE)
    }
  }
  str1 == str2
}

#' Compares if two countries are the same based on their Levenshtein distance.
#'
#' @param country1 A string representing the first country name.
#' @param country2 A string representing the second country name.
#' @param max_distance A numeric threshold for the maximum allowed
#'                     Levenshtein distance.
#'
#' @return TRUE if both countries are the same based on the
#'  Levenshtein distance; otherwise, FALSE.
#' @examples
#' are_same <- compare_nearest_country("germany", "germnay", 3)
compare_nearest_country <- function(country1, country2, max_distance) {
  number1 <- get_country_number_with_levenshtein_distance(country1,
                                                          max_distance)
  number2 <- get_country_number_with_levenshtein_distance(country2,
                                                          max_distance)
  number1 == number2
}


#' Returns the indexes of countries from a list of country names in a data frame
#'
#' @param countries A vector of country names to search for.
#' @param data_frame_countries A vector of country names in the data frame.
#' @param max_distance A numeric threshold for the maximum allowed
#'                     Levenshtein distance.
#'
#' @return A list of indexes where the countries in `countries`
#'         are found in the data frame.
#' @examples
#' country_indexes <- get_country_indexes_from_dataframe(
#'  countries = c("Germany", "France"),
#'  data_frame_countries = csv_data$V1, max_distance = 3)
get_country_indexes_from_dataframe <- function(countries, data_frame_countries,
                                               max_distance) {
  numbers <- map(countries,
                 \(x) get_country_number_with_levenshtein_distance(
                   x,
                   max_distance
                ))
  indexes <- list()
  country_numbers <- map(data_frame_countries, \(x) get_country_number(x))
  for (number in numbers) {
    if (number %in% country_numbers) {
      for (i in 1:length(country_numbers)) {
        if (country_numbers[i] == number) {
          indexes <- append(indexes, i)
        }
      }
    }
  }
  indexes
}
