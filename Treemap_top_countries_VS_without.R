devtools::install_github("timelyportfolio/d3treeR")

library(shiny)
library(dplyr)
library(treemap)
library(lubridate)
library(d3treeR)
library(readr)
library(here)  # Assuming 'here' is already installed and loaded

# Load the data
processed_data <- read_delim("./Data&Shiny/2024.03_Data_Booking.com_VD_processed_joint.csv", 
                             delim = ";", trim_ws = TRUE)

# Prepare the data
processed_data <- processed_data %>%
  mutate(
    Date = dmy(Date),
    Year = year(Date),
    Month = month(Date, label = TRUE, abbr = FALSE)
  )

# Function to get top N countries by count for each year
get_top_countries <- function(data, N = 10) {
  data %>%
    group_by(Pays) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    top_n(N, Count) %>%
    pull(Pays)
}

# Calculate top countries
top_countries <- get_top_countries(processed_data, N = 10)

# UI definition
ui <- fluidPage(
  titlePanel("Treemap Visualization Based on Country Selection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countrySelection", "Choose Country Set:",
                  choices = c("Top Countries" = "top_countries", "Without Top Countries" = "without_top_countries"),
                  selected = "top_countries")
    ),
    mainPanel(
      d3tree2Output("treemap")
    )
  )
)

# Server logic
server <- function(input, output) {
  output$treemap <- renderD3tree2({
    # Filter data based on the selected option
    if (input$countrySelection == "top_countries") {
      data <- processed_data %>%
        filter(Pays %in% top_countries)
    } else {
      data <- processed_data %>%
        filter(!(Pays %in% top_countries))
    }
    
    # Aggregate data by Destination, Type, and Pays
    aggregated_data <- data %>%
      group_by(Destination, Type, Pays) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(Total = sum(count),  # Calculate total for each group
             Percentage = (count / Total) * 100)  # Calculate percentage of each group
    
    # Create treemap
    treemap_data <- treemap(
      aggregated_data,
      index = c("Destination", "Type", "Pays"),
      vSize = "count",
      vColor = "Percentage",  # Color by percentage
      palette = "Set3",
      title = paste("Treemap of Destinations, Type, and Pays -", input$countrySelection)
    )
    
    # Convert to interactive d3tree
    d3tree2(treemap_data, rootname = "General")
  })
}

# Run the application
shinyApp(ui, server)
