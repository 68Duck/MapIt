library(shiny)
library(ggplot2)
library(dplyr)

source(here("R/add_lines.R"))
source(here("R/add_bar_charts.R"))
source(here("R/choropleth.R"))
source(here("R/add_pie_charts.R"))
source(here("R/add_stars.R"))
source(here("R/add_points.R"))
source(here("R/merge_data.R"))

world_data <- ne_countries(returnclass = "sf", scale = 10)

continent_data <- list(
  africa = world_data[world_data$continent == "Africa", ],
  europe = world_data[world_data$continent == "Europe", ],
  `south america` = world_data[world_data$continent == "South America", ],
  world = world_data
)

ui <- fluidPage(
  titlePanel("Test"),
  sidebarLayout(
    sidebarPanel(
      selectInput("chart_type", "Choose chart type:",
                  choices = c("Bar Chart", "Pie Chart", "Star Plot")),
      selectInput("continent_choice", "Choose continent or whole world:",
                  choices = c("africa", "europe", "world", "south america")),
      conditionalPanel(
        condition = "input.chart_type == 'Bar Chart'",
        numericInput("size_choice", "Choose the size of the bar chart:",
                    value = 6, min = 2, max = 15)
      ),

      conditionalPanel(
        condition = "input.chart_type == 'Pie Chart'",
        numericInput("size_choice", "Choose the size of the pie chart:",
                    value = 6, min = 2, max = 15)
      )
    ),
    mainPanel(
      plotOutput("ggplot")
    )
  )
)

server <- function(input, output) {
  output$ggplot <- renderPlot({
    data <- continent_data[[input$continent_choice]]
    # if (input$continent_choice == "world") {
    #   data <- ne_countries(returnclass = "sf", scale = 10)
    # } else {
    #   data <- ne_countries(returnclass = "sf", scale = 10,
    #                        continent = input$continent_choice)

    # }

    if (input$chart_type == "Bar Chart") {
        width <- input$size_choice
        height <- input$size_choice
        small_country_area <- input$size_choice * 15 / 6
        data <- modify_label_positions(data = data,
                                    small_country_area = small_country_area,
                                    width = width, height = height)

        validate(need(!is.null(data),
                      "Bar chart size is too large to fit on the map"))
        
        map <- choropleth(data = data, fill = data$pop_rank,
                        legend_title = "Population rank") +
            add_lines_to_labels(data = data, width = width, height = height) +
            add_bar_charts(df = data, width = width, height = height,
                            attributes = c("pop_rank", "name_len"))
        return (map)
    } else if (input$chart_type == "Pie Chart") {
        width <- input$size_choice / 2
        height <- input$size_choice / 2
        pie_scale <- input$size_choice / 4
        data <- modify_label_positions(data = data,
                                    small_country_area = small_country_area,
                                    width = width, height = height)
        map <- choropleth(data = data, fill = data$pop_rank,
                        legend_title = "Population rank") +
            add_lines_to_labels(data = data, width = width, height = height) +
            add_pie_charts(data, "label_x", "label_y", c("name_len", "pop_rank"),
                            "Name vs Pop", pie_scale)
        return (map)
    } else if (input$chart_type == "Star Plot") {

    }
  })
}

shinyApp(ui = ui, server = server)
