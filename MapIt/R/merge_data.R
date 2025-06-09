library(dplyr)
library(here)
library(sf)
library(shiny)

source(here("R/name_matching.R"))


#' Merges two datasets based on region names.
#'
#' @param region_data A data frame containing the region data.
#' @param auxiliary_data A data frame containing the auxiliary data to merge.
#' @param region_name A string specifying the column name in `region`
#'                     that contains the region names.
#' @param auxiliary_region_name A string specifying the column name in
#'                              `auxiliary_data` that contains the region names
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data(country_data, auxiliary_data,
#'                           "country", "country_name")
#' @export
merge_data <- function(region_data, auxiliary_data,
                       region_name, auxiliary_region_name) {
  original_regions <- region_data[[region_name]]
  data <- merge(region_data, auxiliary_data,
                by.x = region_name, by.y = auxiliary_region_name)
  merged_regions <- data[[region_name]]
  missing_regions <- setdiff(original_regions, merged_regions)
  if (length(missing_regions) > 0) {
    warning(paste("Not all regions were found in the auxiliary data. 
    The following regions were not merged: ",
                  paste(missing_regions, collapse = ", ")))
  }
  data <- st_as_sf(data)
  data
}


#' Merges two datasets based on region names using levenshtein distance.
#'
#' @param region_data A data frame containing the region data.
#' @param auxiliary_data A data frame containing the auxiliary data to merge.
#' @param region_name A string specifying the column name in `region_data`
#'                     that contains the region names.
#' @param auxiliary_region_name A string specifying the column name in
#'                              `auxiliary_data` that contains the region names
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


#' Merges two datasets based on values in a csv.
#'
#' @param region_data A data frame containing the region data.
#' @param auxiliary_data A data frame containing the auxiliary data to merge.
#' @param region_name A string specifying the column name in `region_data`
#'                     that contains the region names.
#' @param auxiliary_region_name A string specifying the column name in
#'                              `auxiliary_data` that contains the region names
#' @param csv_path A string specifying the path of the csv which has
#'                 alternative names for regins in its rows
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data_with_csv(country_data, auxiliary_data,
#'                                    "country", "country_name", csv_path)
#' @export
merge_data_with_csv <- function(region_data, auxiliary_data,
                                             region_name,
                                             auxiliary_region_name,
                                             csv_path,
                                             basic = FALSE) {
  region_data$region_number <-
    apply(region_data, 1,
          function(row) get_region_number(row[region_name], csv_path, 3, basic))
  auxiliary_data$region_number <-
    apply(auxiliary_data, 1,
          function(row) get_region_number(row[auxiliary_region_name], csv_path,
                                          3, basic))

  data <- merge(region_data, auxiliary_data,
                by = "region_number", all.x = TRUE)
  data <- st_as_sf(data)
  data
}

#' Merges two datasets based on region names, using name matching,
#' and prompts user input if the regions cannot be allocated automatically.
#'
#' @param region_data A data frame containing the region data.
#' @param auxiliary_data A data frame containing the auxiliary data to merge.
#' @param region_name A string specifying the column name in `region_data`
#'                     that contains the region names.
#' @param auxiliary_region_name A string specifying the column name in
#'                              `auxiliary_data` that contains the region names
#'
#' @return An `sf` data frame containing the merged data.
#' @examples
#' merged_data <- merge_data_with_ui(country_data, auxiliary_data,
#'                                   "country", "country_name")
#' @export
merge_data_with_ui <- function(region_data, auxiliary_data,
                               region_name,
                               auxiliary_region_name,
                               csv_path) {
  region_data$region_number <-
    apply(region_data, 1,
          function(row) get_region_number(row[region_name],
           csv_path, 3))
  auxiliary_data$region_number <-
    apply(auxiliary_data, 1,
          function(row) get_region_number(row[auxiliary_region_name],
           csv_path, 3))

  region_data <- run_region_matching_ui(region_data, region_name, csv_path)
  auxiliary_data <- run_region_matching_ui(auxiliary_data,
                                           auxiliary_region_name, csv_path)

  data <- merge(region_data, auxiliary_data,
                by = "region_number", all.x = TRUE)
  data <- st_as_sf(data)
  data
}

#' Iterates through each region and checks whether it has been allocated
#' a number automatically. If not, it prompts the user to input a name or
#' whether the region should be removed.
#'
#' @param region_data A data frame containing the region data.
#' @param region_name A string specifying the column name in `region_data`
#'                     that contains the region names.
#' #' @param csv_path A string specifying the path of the csv which has
#'                 alternative names for regins in its rows
#'
#' @return An `sf` data frame containing new region numbers
#' @examples
#' country_data <- run_region_matching_ui(country_data, "country")
run_region_matching_ui <- function(region_data, region_name, csv_path) {
  used_region_numbers <- region_data$region_number
  regions_to_remove <- c()
  for (i in 1:nrow(region_data)) {
    region_name_value <- region_data[i, region_name]
    region_number <- region_data[["region_number"]][i]
    remove_region <- FALSE
    while (region_number == -1) {
      new_region_name <-
        readline(prompt = paste(
                                "Please enter the region name for: ",
                                region_name_value,
                                " If you would like to delete this
                                region please enter -1"))
      if (new_region_name == -1 || new_region_name == "") {
        regions_to_remove <- c(regions_to_remove, i)
        remove_region <- TRUE
        break
      }
      region_number <- get_region_number(new_region_name, csv_path, 3)
      if (region_number == -1) {
        print(paste("The region: ", new_region_name,
                    " is not a valid region Please try again."))
      } else if (region_number %in% region_data$region_number) {
        print(paste("The region: ", new_region_name,
                    " is already in the data. Please pick a different region"))
        region_number <- -1
      }
    }
    if (!remove_region) {
      used_region_numbers <- c(used_region_numbers, region_number)
      region_data[i, "region_number"] <- region_number
    }
  }
  if (length(regions_to_remove) > 0) {
    region_data <- region_data[-regions_to_remove, ]
  }
  region_data
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