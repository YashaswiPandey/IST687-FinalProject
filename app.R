library(tidyverse)
#weather_data <- read_csv("weatherES (1).xlsx")
#view(weather_data)
library(readxl)

# Replace "file_path.xlsx" with the path to your Excel file
data <- read_csv("weatherESTestPred.csv")
library(shiny)
#install.packages("shinydashboard")
library(rsconnect)
library(shinydashboard)

library(RCurl)

library(jsonlite)

library(tidyverse)

library(ggmap)

library(dplyr)
library(DT)
library(shiny)
library(ggplot2)
library(dplyr)
#view(data)
library(shiny)
library(shinydashboard)
library(DT)  # For interactive tables (optional)
library(ggplot2)  # For plotting (optional)
str(data)
# Your previous code...

# Assuming 'data' contains your dataset

default_selection <- unique(data$in.county)[1]  # Set the default selection to the first unique value

ui <- dashboardPage(
  dashboardHeader(title = "Weather Data Dashboard"),
  dashboardSidebar(
    selectInput("filter_county", "Select County ID", 
                choices = unique(data$in.county), 
                selected = default_selection),  # Set default selection
    actionButton("max_pred_button", "Show Maximum Total Predicted")  # Button to display maximum total predicted
  ),
  dashboardBody(
    fluidRow(
      box(
        verbatimTextOutput('max_pred_info')  # Output for maximum total predicted information
      )
    ),
    fluidRow(
      box(
        DTOutput('weather_table', width = '100%')  # Output for weather data table
      )
    )
  )
)

server <- function(input, output) {
  # Render weather data table based on selected County ID
  output$weather_table <- renderDT({
    filtered_data <- data[data$in.county == input$filter_county, ]  # Filter by selected County ID
    datatable(filtered_data)
  })
  
  # Calculate and display max total predicted information
  observeEvent(input$max_pred_button, {
    filtered_max_data <- subset(data, in.county == input$filter_county)
    max_pred_value <- max(filtered_max_data$totalPredicted)
    max_time <- filtered_max_data$time[which.max(filtered_max_data$totalPredicted)]
    output$max_pred_info <- renderPrint({
      paste("Maximum Total Predicted for", input$filter_county, "is", max_pred_value,
            "at time:", max_time)
    })
  })
}

shinyApp(ui = ui, server = server)

