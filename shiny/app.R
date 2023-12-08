wd<-"/Users/harsh/Downloads"
setwd(wd)
getwd()
library(tidyverse)
#weather_data <- read_csv("weatherES (1).xlsx")
#view(weather_data)
library(readxl)
data1 <- read_csv("weatherESTestPred.csv")
head(data1)
library(shiny)
library(dplyr)

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
#library(DT)  # For interactive tables 
library(ggplot2) 
# Selecting necessary columns 
data <- data1 %>% 
  select(in.county, time, totalPredicted,`Dry Bulb Temperature [°C]`)

# Create the user interface (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Weather Data Dashboard"),
  dashboardSidebar(
    selectInput("filter_county", "Select County ID", 
                choices = unique(data$in.county), 
                selected = unique(data$in.county)[1])  # Set default selection
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


# Define the server logic
server <- function(input, output) {
  # Render weather data table based on selected County ID
  output$weather_table <- renderDT({
    filtered_data <- data[data$in.county == input$filter_county, ]  # Filter by selected County ID
    datatable(filtered_data)
  })
  
  # Calculate and display max total predicted information automatically
  output$max_pred_info <- renderPrint({
    filtered_max_data <- data[data$in.county == input$filter_county, ]
    max_pred_value <- max(filtered_max_data$totalPredicted)
    max_time <- filtered_max_data$time[which.max(filtered_max_data$totalPredicted)]
    paste("Maximum Total Predicted for", input$filter_county, "is", max_pred_value,
          "at time:", max_time)
  })
  
  # Create a ggplot for Dry Bulb Temperature vs Total Predicted
  output$drybulb_plot <- renderPlot({
    filtered_plot_data <- data[data$in.county == input$filter_county, ]  # Filter by selected County ID
    ggplot(filtered_plot_data, aes(x = `Dry Bulb Temperature [°C]`, y = totalPredicted)) +
      geom_line() +  # Use geom_line to create a line graph
      labs(x = "Dry Bulb Temperature [°C]", y = "Total Predicted") +
      ggtitle("Dry Bulb Temperature vs Total Predicted")
  })
}

# the dashboardBody to include the plot
ui <- dashboardPage(
  dashboardHeader(title = "Weather Data Dashboard"),
  dashboardSidebar(
    selectInput("filter_county", "Select County ID", 
                choices = unique(data$in.county), 
                selected = unique(data$in.county)[1])  # Set default selection
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
    ),
    fluidRow(
      box(
        plotOutput('drybulb_plot')  # Output for Dry Bulb Temperature vs Total Predicted plot
      )
    )
  )
)


# Run the application
shinyApp(ui = ui, server = server)
