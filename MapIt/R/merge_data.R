library(dplyr)
library(here)
library(sf)
library(shiny)

source(here("R/name_matching.R"))


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
#' @export
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
#' @export
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

merge_data_with_ui <- function(country_data, auxillary_data,
                               country_name,
                               auxillary_country_name) {
  country_data$country_number <-
    apply(country_data, 1,
          function(row) get_country_number_basic(row[country_name]))
  auxillary_data$country_number <-
    apply(auxillary_data, 1,
          function(row) get_country_number_basic(row[auxillary_country_name]))

  invalid_countries <- list()

  for (i in 1:nrow(country_data)) {
    if (country_data$country_number[i] == -1) {
      invalid_countries[[length(invalid_countries) + 1]] <-
        country_data[[country_name]][i]
    }
  }

  print(invalid_countries)

  if (length(invalid_countries) > 0) {
    run_country_matching_ui(invalid_countries)
  } else {
    data <- merge(country_data, auxillary_data,
                  by = "country_number", all.x = TRUE)
    data <- st_as_sf(data)
    data
  }

}

# run_country_matching_ui <- function(invalid_countries) {

# }



# run_country_matching_ui <- function(invalid_countries) {
#   print("test")
#   # Define the UI for the Shiny app
#   ui <- fluidPage(
#     titlePanel("Assign Country Numbers to Invalid Countries"),

#     sidebarLayout(
#       sidebarPanel(
#         h3("Invalid Countries"),
#         # Create inputs for each invalid country
#         uiOutput("country_inputs")  # Dynamic UI for country inputs
#       ),

#       mainPanel(
#         actionButton("submit", "Submit Country Numbers")  # Button to submit
#       )
#     )
#   )

#   # Define the server logic
#   server <- function(input, output, session) {
    
#     # Dynamically generate input fields for each invalid country
#     output$country_inputs <- renderUI({
#       # Create a list of numeric inputs for each invalid country
#       lapply(seq_along(invalid_countries), function(i) {
#         tagList(
#           textOutput(paste("country_name", i)),
#           numericInput(paste("country_number", i), 
#                        label = paste("Assign a country number to", invalid_countries[i]), 
#                        value = NA, 
#                        min = -1)
#         )
#       })
#     })
    
#     # Store country numbers when submit button is clicked
#     observeEvent(input$submit, {
#       # Create a list to store assigned country numbers
#       assigned_numbers <- sapply(seq_along(invalid_countries), function(i) {
#         input[[paste("country_number", i)]]
#       })
      
#       # Print assigned country numbers (you can replace this with actual logic)
#       print(data.frame(country = invalid_countries, assigned_country_number = assigned_numbers))
      
#       # You can perform any additional action here (e.g., store results in a database)
#     })
#   }

#   # Run the Shiny app
#   shinyApp(ui = ui, server = server)
# }


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