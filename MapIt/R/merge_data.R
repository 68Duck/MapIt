library(dplyr)
library(here)
library(sf)
library(shiny)

source(here("R/name_matching.R"))


#' Merges two datasets based on country names.
#'
#' @param country_data A data frame containing the country data.
#' @param auxiliary_data A data frame containing the auxiliary data to merge.
#' @param country_name A string specifying the column name in `country_data`
#'                     that contains the country names.
#' @param auxiliary_country_name A string specifying the column name in
#'                              `auxiliary_data` that contains the country names
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data(country_data, auxiliary_data,
#'                           "country", "country_name")
#' @export
merge_data <- function(country_data, auxiliary_data,
                       country_name, auxiliary_country_name) {
  original_countries <- country_data[[country_name]]
  data <- merge(country_data, auxiliary_data,
                by.x = country_name, by.y = auxiliary_country_name)
  merged_countries <- data[[country_name]]
  missing_countries <- setdiff(original_countries, merged_countries)
  if (length(missing_countries) > 0) {
    warning(paste("Not all countries were found in the auxiliary data. 
    The following countries were not merged: ",
                  paste(missing_countries, collapse = ", ")))
  }
  data <- st_as_sf(data)
  data
}


#' Merges two datasets based on country names using levenshtein distance.
#'
#' @param country_data A data frame containing the country data.
#' @param auxiliary_data A data frame containing the auxiliary data to merge.
#' @param country_name A string specifying the column name in `country_data`
#'                     that contains the country names.
#' @param auxiliary_country_name A string specifying the column name in
#'                              `auxiliary_data` that contains the country names
#' @param distance An integer representing the maximum levenshtein distance
#'                 between two strings which can be matched
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data_with_levenshtein_distance(country_data,
#'                                                     auxiliary_data,
#'                                                     "country",
#'                                                     "country_name", 3)
#' @export
merge_data_with_levenshtein_distance <- function(region_data, auxiliary_data,
                                                 region_name,
                                                 auxiliary_region_name,
                                                 distance = 3) {
  region_data$region_number <- seq_len(nrow(region_data))
  original_regions <- region_data[[region_name]]
  match_indices <- match(auxiliary_data[[auxiliary_region_name]],
                         region_data[[region_name]])
  auxiliary_data$region_number <- region_data$region_number[match_indices]
  print(auxiliary_data$region_number)

  for (i in seq_len(nrow(auxiliary_data))) {
    if (is.na(auxiliary_data$region_number[i])) {
      auxiliary_name <- auxiliary_data[[auxiliary_region_name]][i]
      for (j in seq_len(nrow(region_data))) {
        if (!j %in% auxiliary_data$region_number) {
          if (levenshtein_distance_lesser_than(region_data[[region_name]][j],
                                               auxiliary_name, distance)) {
            auxiliary_data$region_number[i] <- j
          }
        }
      }
    }
  }
  data <- merge(region_data, auxiliary_data,
                by.x = "region_number", by.y = "region_number")
  merged_regions <- data[[region_name]]

  missing_regions <- setdiff(original_regions, merged_regions)

  if (length(missing_regions) > 0) {
    warning(paste("Not all regions were matched to the auxiliary data. 
    The following regions were not merged: ",
                  paste(missing_regions, collapse = ", ")))

  }
  data
}


#' Merges two datasets based on country names, using name matching.
#'
#' @param country_data A data frame containing the country data.
#' @param auxiliary_data A data frame containing the auxiliary data to merge.
#' @param country_name A string specifying the column name in `country_data`
#'                     that contains the country names.
#' @param auxiliary_country_name A string specifying the column name in
#'                              `auxiliary_data` that contains the country names
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data_with_country_matching(country_data, auxiliary_data,
#'                                                 "country", "country_name")
#' @export
merge_data_with_CSV <- function(region_data, auxiliary_data,
                                             region_name,
                                             auxiliary_region_name,
                                             csv_path) {
  region_data$region_number <-
    apply(region_data, 1,
          function(row) get_region_number(row[region_name], 3))
  auxiliary_data$region_number <-
    apply(auxiliary_data, 1,
          function(row) get_region_number(row[auxiliary_region_name], 3))

  data <- merge(region_data, auxiliary_data,
                by = "region_number", all.x = TRUE)
  data <- st_as_sf(data)
  data
}

#' Merges two datasets based on country names, using name matching,
#' and prompts user input if the countries cannot be allocated automatically.
#'
#' @param country_data A data frame containing the country data.
#' @param auxiliary_data A data frame containing the auxiliary data to merge.
#' @param country_name A string specifying the column name in `country_data`
#'                     that contains the country names.
#' @param auxiliary_country_name A string specifying the column name in
#'                              `auxiliary_data` that contains the country names
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data_with_ui(country_data, auxiliary_data,
#'                                   "country", "country_name")
#' @export
merge_data_with_ui <- function(country_data, auxiliary_data,
                               country_name,
                               auxiliary_country_name) {
  country_data$country_number <-
    apply(country_data, 1,
          function(row) get_country_number_basic(row[country_name]))
  auxiliary_data$country_number <-
    apply(auxiliary_data, 1,
          function(row) get_country_number_basic(row[auxiliary_country_name]))

  country_data <- run_country_matching_ui(country_data, country_name)
  auxiliary_data <- run_country_matching_ui(auxiliary_data,
                                            auxiliary_country_name)

  data <- merge(country_data, auxiliary_data,
                by = "country_number", all.x = TRUE)
  data <- st_as_sf(data)
  data
}

#' Iterates through each country and checks whether it has been allocated
#' a number automatically. If not, it prompts the user to input a name or
#' whether the country should be removed.
#'
#' @param country_data A data frame containing the country data.
#' @param country_name A string specifying the column name in `country_data`
#'                     that contains the country names.
#'
#' @return An `sf` data frame containing new country numbers
#' @examples
#' country_data <- run_country_matching_ui(country_data, "country")
run_country_matching_ui <- function(country_data, country_name) {
  used_country_numbers <- country_data$country_number
  countries_to_remove <- c()
  for (i in 1:nrow(country_data)) {
    country_name_value <- country_data[i, country_name]
    country_number <- country_data[["country_number"]][i]
    remove_country <- FALSE
    while (country_number == -1) {
      new_country_name <-
        readline(prompt = paste(
                                "Please enter the country name for: ",
                                country_name_value,
                                " If you would like to delete this
                                country please enter -1"))
      if (new_country_name == -1 || new_country_name == "") {
        countries_to_remove <- c(countries_to_remove, i)
        remove_country <- TRUE
        break
      }
      country_number <- get_country_number_basic(new_country_name)
      if (country_number == -1) {
        print(paste("The country: ", new_country_name,
                    " is not a valid country. Please try again."))
      } else if (country_number %in% country_data$country_number) {
        print(paste("The country: ", new_country_name,
                    " is already in the data. Please pick a different country"))
        country_number <- -1
      }
    }
    if (!remove_country) {
      used_country_numbers <- c(used_country_numbers, country_number)
      country_data[i, "country_number"] <- country_number
    }
  }
  country_data <- country_data[-countries_to_remove, ]
  country_data
}


#' Converts specified columns to numeric values after removing
#'  unwanted characters.
#'
#' @param data A data frame containing the columns to convert.
#' @param columns A vector of column names to be converted to a number.
#' @param characters_to_remove A vector of characters to remove from the
#' columns before conversion to a number.
#'
#' @return A data frame with the specified columns converted to numeric values.
#' @examples
#' converted_data <- convert_columns_to_number(data,
#'  columns = c("population", "gdp"), characters_to_remove = c("$", "%"))
convert_columns_to_number <- function(data, columns, characters_to_remove) {
  for (column in columns) {
    data[[column]] <- gsub(",", "", data[[column]])
    for (character in characters_to_remove) {
      data[[column]] <- gsub(character, "", data[[column]])
    }
    data[[column]] <- as.numeric(data[[column]])
  }
  data
}