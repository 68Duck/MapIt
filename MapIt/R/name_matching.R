library(stringdist)
library(purrr)
library(here)

# csv_data <<- read.csv(here("countryNames.csv"), header = FALSE)

# #' Creates a mapping from country names to numbers
# #'
# #' @return A map of country names to numbers
# #' @import stringdist
# #' @examples
# #' country_map <- create_country_map()
# create_region_map <- function() {
#   data <- csv_data
#   region_map <- list()
#   for (i in 1:nrow(data)) {
#     for (j in 1:ncol(data)) {
#       country_name <- tolower(data[i, j])
#       country_map[[country_name]] <- i
#     }
#   }
#   country_map
# }


#' Returns the country number based off the levenstein distance being
#' less than x
#'
#' This function searches for the country in the CSV data by calculating the
#' Levenshtein distance between the input `country` and the country names in
#' the data. If the distance is less than a threshold `x`, it returns the
#' index of the matching row which is the country number.
#'
#' @param region A string representing the region name to search for.
#' @param x A numeric threshold for the Levenshtein distance.
#'
#' @return The row index of the closest matching country
#'         if found; otherwise, -1.
#' @examples
#' country_number <- get_country_number_with_levenshtein_distance("germany", 3)
#' @export
get_region_number_with_levenshtein_distance <- function(country,
                                                        distance, csv_path) {
  data <- read.csv(here(csv_path), header = FALSE)
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    for (j in 1:length(row)) {
      value <- row[j]
      if (length(value) == 0) {
        break
      }
      if (levenshtein_distance_lesser_than(toString(value),
                                           tolower(country), distance)) {
        print(i)
        return(i)
      }
    }
  }
  -1
}


#' Returns the country number based off the csv file
#'
#' The function tries to match the input region with names in the csv.
#' It also attempts to clean the country name by removing common
#' words like "and", "of", or "the".
#' If a direct match isn't found, it uses the Levenshtein distance to search for
#' a close match.
#'
#' @param region A string representing the region name to search for.
#' @param x A numeric threshold for the Levenshtein distance.
#' @param csv_path A string specifying the path of the csv which has
#'                 alternative names for regins in its rows
#' @param basic A boolean which sets whether levenshtein distance is used or not
#'
#' @return The row index of the closest matching region
#'          if found; otherwise, -1.
#' @examples
#' country_number <- get_region_number("germany", csv_path, 3)
#' @export
get_region_number <- function(region, csv_path, distance = 3, basic = FALSE) {
  region <- gsub("\\b(and|of|the)\\b", "", region, ignore.case = TRUE)
  region <- tolower(region)
  data <- read.csv(here(csv_path), header = FALSE)
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    for (j in 1:length(row)) {
      value <- row[j]
      if (length(value) == 0 || is.na(value)) {
        break
      }
      value <- toString(value)
      value <- gsub("\\b(and|of|the)\\b", "", value, ignore.case = TRUE)
      value <- tolower(value)
      if (basic) {
        if (value == region) {
          return(i)
        }
      } else {
        if (levenshtein_distance_lesser_than(value,
                                             region, distance)) {
          return(i)
        }
      }
      if (abbreviated(region, value)) {
        return(i)
      }
    }
  }
  -1
}

get_region_from_map <- function(region, csv_path) {
  if (!exists("region_map", envir = .GlobalEnv)) {
    region_map <<- create_region_map(csv_path)
  }
  region <- tolower(region)
  if (region %in% names(region_map)) {
    return(region_map[[region]])
  }
  region <- gsub("\\b(and|of|the)\\b", "", region, ignore.case = TRUE)
  if (region %in% names(region_map)) {
    return(region_map[[region]])
  }
  -1
}



#' Calculates the Levenshtein distance between two strings and checks
#'  if it is less than a threshold.
#'
#' @param str1 A string representing the first name.
#' @param str2 A string representing the second name.
#' @param x A numeric threshold for the Levenshtein distance.
#'
#' @return TRUE if the Levenshtein distance is less than `x`; otherwise, FALSE.
#' @examples
#' is_closer <- levenshtein_distance_lesser_than("germany", "germnay", 3)
#' @export
levenshtein_distance_lesser_than <- function(str1, str2, x) {
  dist <- stringdist(str1, str2, method = "lv")
  dist < x
}


#' Checks if a name is an acronym of another.
#'
#' @param str1 A string representing the first name.
#' @param str2 A string representing the second name.
#'
#' @return TRUE if the first string is an acronym of the second;
#'         otherwise, FALSE.
#' @examples
#' acronym <- is_acronym("us", "united states")
is_acronym <- function(str1, str2) {
  str1 <- gsub(" ", "", str1)
  words <- unlist(strsplit(tolower(str2), "\\s+"))
  if (nchar(str1) != length(words)) {
    return(FALSE)
  }
  for (i in 1:nchar(str1)) {
    if (substr(str1, i, i) != substr(words[i], 1, 1)){
      return(FALSE)
    }
  }
  return (TRUE)
}

#' Checks if a name is a shortened version of another using a . to represent 
#' a shortened string.
#'
#' @param str1 A string representing the first name.
#' @param str2 A string representing the second name.
#'
#' @return TRUE if the first string is an shortened version of the second;
#'         otherwise, FALSE.
#' @examples
#' shortened <- is_shortened("un.", "united")
is_shortened <- function(str1, str2) {
  words1 <- unlist(strsplit(tolower(str1), "\\s+"))
  words2 <- unlist(strsplit(tolower(str2), "\\s+"))
  if (length(words1) != length(words2)) {
    return(FALSE)
  }

  for (i in 1:length(words1)) {
    if (words1[i] != words2[i]) {
      for (j in 1:nchar(words1[i])) {
        if (substr(words1[i], j, j) == ".") {
          if (j != nchar(words1[i])) {
            return(FALSE)
          } 
        } else {
          if (substr(words1[i], j, j) != substr(words2[i], j, j)) {
            return(FALSE)
          }
          if (j == nchar(words1[i])) {
            return(FALSE)
          }
        }
      }
    }
  }
  return(TRUE)
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
  str1 <- tolower(str1)
  str2 <- tolower(str2)

  return(
    is_acronym(str1, str2) ||
    is_acronym(str2, str1) || 
    is_shortened(str1, str2) || 
    is_shortened(str2, str1)
  )

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
#' @export
compare_nearest_country <- function(country1, country2, max_distance, csv_path) {
  number1 <- get_region_number_with_levenshtein_distance(country1,
                                                          max_distance,
                                                          csv_path)
  number2 <- get_region_number_with_levenshtein_distance(country2,
                                                          max_distance,
                                                          csv_path)
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
#' @export
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
