library(dplyr)
library(here)
source(here("MapIt/MapIt/R/name_matching.R"))


#' Merges two datasets based on country names.
#'
#' @param country_data A data frame containing the country data.
#' @param auxillary_data A data frame containing the auxiliary data to merge.
#' @param country_name A string specifying the column name in `country_data`
#'                     that contains the country names.
#' @param auxillary_country_name A string specifying the column name in
#'                              `auxillary_data` that contains the country names
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data(country_data, auxillary_data,
#'                           "country", "country_name")
merge_data <- function(country_data, auxillary_data,
                       country_name, auxillary_country_name) {
  data <- merge(country_data, auxillary_data,
                by.x = country_name, by.y = auxillary_country_name)
  data <- st_as_sf(data)
  data
}


#' Merges two datasets based on country names, using name matching.
#'
#' @param country_data A data frame containing the country data.
#' @param auxillary_data A data frame containing the auxiliary data to merge.
#' @param country_name A string specifying the column name in `country_data`
#'                     that contains the country names.
#' @param auxillary_country_name A string specifying the column name in
#'                              `auxillary_data` that contains the country names
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data_with_country_matching(country_data, auxillary_data,
#'                                                 "country", "country_name")
merge_data_with_country_matching <- function(country_data, auxillary_data,
                                             country_name,
                                             auxillary_country_name) {
  country_data$country_number <-
    apply(country_data, 1,
          function(row) get_country_number(row[country_name], 3))
  auxillary_data$country_number <-
    apply(auxillary_data, 1,
          function(row) get_country_number(row[auxillary_country_name], 3))
  data <- merge(country_data, auxillary_data,
                by = "country_number", all.x = TRUE)
  data <- st_as_sf(data)
  data
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